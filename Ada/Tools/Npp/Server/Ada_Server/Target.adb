-- *********************************************************************************************************************
-- *                       (c) 2013 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada_95.Project;
with Ada_95.Token;
with Directory;
with File;
with Log;
with Os.Process;
with Project.Gpr;
with Promotion;
with Server;
with Text;

package body Target is

  procedure Log_Execution (Item : String) is
  begin
    Log.Write ("    -> " & Item);
  end Log_Execution;


  procedure Check_For_Parser_Errors_In (Filename : String) is

    The_Error_Token : Ada_95.Token.Handle;

    function Error_Image return String is
    begin
      return Text.Legible_Of (Ada_95.Project.Error_Kind'image(Ada_95.Project.Error_Kind_Of(The_Error_Token)));
    end Error_Image;

    use type Ada_95.Token.Handle;

  begin -- Check_For_Parser_Errors_In
    The_Error_Token := Ada_95.Project.First_Error_Of (Filename);
    if The_Error_Token /= null then
      Promotion.Set_Error (Item      => Error_Image,
                           File      => Ada_95.Token.Filename_Of (The_Error_Token),
                           At_Line   => Ada_95.Token.Line_Number_Of (The_Error_Token),
                           At_Column => Ada_95.Token.Column_Position_Of (The_Error_Token));
    end if;
  end Check_For_Parser_Errors_In;


  procedure Gpr_Execute (Parameters : String) is

    Gpr_Build : constant String := Project.Tools_Folder & "gprbuild";

    Product_Location : constant String := Project.Product_Directory & Project.Product_Sub_Path;

    Gpr_Parameters : constant String := " -j0 -P" & Project.Gpr.Filename &
                                        " -XBinary_Root=" & Project.Binary_Folder &
                                        " -XProduct_Location=" & Product_Location &
                                        " -XSource_Root=" & Project.Source_Folder &
                                        " " & Parameters;

    End_Of_Text : constant String := "" & Ascii.Nul;

  begin -- Gpr_Execute
    if not Project.Tools_Defined then
      Promotion.Set_Error ("GNAT tools directory undefined");
    end if;
    Log_Execution (Gpr_Build & Gpr_Parameters);
    Directory.Create (Product_Location);
    declare
      Result : constant String := Os.Process.Execution_Of (Executable     => Gpr_Build,
                                                           Parameters     => Gpr_Parameters,
                                                           Environment    => Project.Environment_Variables,
                                                           Current_Folder => Project.Created_Target_Folder);
      Result_Text : constant String := Text.Trimmed (Result);
    begin
      if Result_Text = "" then
        return;
      end if;
      Log.Write ("   OUTPUT: <" & Result_Text & ">");
      if Text.Location_Of ("Memory region", Result_Text, Result_Text'first) = Result_Text'first then
        return;
      end if;
      declare
        First_Index : Natural := Result_Text'first;
        The_Index   : Natural := Result_Text'first;

        function Next_String_Until (Separator : String) return String is
        begin
          if Separator = End_Of_Text then
            The_Index := Result_Text'last + 1;
          else
            The_Index := Text.Location_Of (Separator, Result_Text, First_Index);
          end if;
          declare
            The_Text : constant String := Result_Text(First_Index .. The_Index - 1);
          begin
            First_Index := The_Index + Separator'length;
            return The_Text;
          end;
        end Next_String_Until;

        Separator : constant String := ":";
        Filename  : constant String := Project.Full_Name_Of (Next_String_Until (Separator));
      begin
        if File.Exists (Filename) then
          declare
            The_Line : Natural  := Natural'value(Next_String_Until (Separator));
            Column   : constant Server.Column_Range := Server.Column_Range'value(Next_String_Until (Separator));
          begin
            if The_Line > Natural(Server.Line_Number'last) then
              The_Line := Natural(Server.Line_Number'last);
            end if;
            Promotion.Set_Error (Item      => Next_String_Until (End_Of_Text),
                                 File      => Filename,
                                 At_Line   => Server.Line_Number(The_Line),
                                 At_Column => Column);
          end;
        else
          Promotion.Set_Error (Result_Text);
        end if;
      exception
      when Promotion.Error =>
        raise;
      when Item: others =>
        Log.Write ("!!! Gnat.Gpr_Execute - Error Parser", Item);
      end;
    end;
  exception
  when Promotion.Error =>
    raise;
  when Os.Process.Execution_Failed =>
    Promotion.Set_Error ("GNAT GPR not executed");
  when Item: others =>
    Log.Write ("Gnat.Gpr_Execute", Item);
    Promotion.Set_Error ("GNAT GPR execution failed");
  end Gpr_Execute;


  procedure Compile (Filename : String) is
  begin
    if Text.Lowercase_Of(Filename(Filename'last - 2 .. Filename'last)) = "ads" then
      Promotion.Set_Message ("Check " & Filename);
      --TEST------------------------------------
      Log_Execution ("Check: " & Filename);
      ------------------------------------------
      Gpr_Execute ("-ws -c -gnatc -u -q -eS " & Filename);
      Check_For_Parser_Errors_In (Filename);
      Promotion.Set_Message ("Semantic check successfully completed");
    else
      Promotion.Set_Message ("Compile " & Filename);
      --TEST--------------------------------------
      Log_Execution ("Compile: " & Filename);
      --------------------------------------------
      Gpr_Execute ("-ws -c -u -q -eS " & Filename);
      Check_For_Parser_Errors_In (Filename);
    end if;
  end Compile;


  procedure Build (Filename : String) is

  begin
    Promotion.Set_Message ("Build " & Project.Tools_Kind & Filename);
    Check_For_Parser_Errors_In (Filename);
    Gpr_Execute ("-XLIBRARY_TYPE=static -eS -q");
  exception
  when Promotion.Error =>
    raise;
  when Os.Process.Execution_Failed =>
    Promotion.Set_Error ("Gnat Builder not executed");
  when Item: others =>
    Log.Write ("Gnat.Build", Item);
    Promotion.Set_Error ("Unknown Build result", Filename);
  end Build;


  procedure Modifier_Handling is
    Modifier : constant String := Project.Modifier_Tool;
  begin
    if Modifier /= "" then
      if Project.Modifier_Success = "" then
        Promotion.Set_Message ("Modify " & Project.Modifier_Parameters);
      end if;
      declare
        Result : constant String
          := Text.Trimmed (Os.Process.Execution_Of (Executable    => Modifier,
                                                    Parameters    => Project.Modifier_Parameters,
                                                    Handle_Output => False));
      begin
        if Result = "" then
          if Project.Modifier_Success /= "" then
            Promotion.Set_Message (Project.Modifier_Success);
          end if;
        else
          Promotion.Set_Error (Result);
        end if;
      end;
    end if;
  end Modifier_Handling;


  procedure Installation_Handling is
    Destinations : constant Text.List := Project.Installation_Destinations;
  begin
    for Destination of Destinations loop
      declare
        Items     : constant Text.Strings := Text.Strings_Of (Destination, Separator => '*');
        Folder    : constant String       := Items(1);
        Tool_Kind : constant String       := (if Items.Count = 2 then Items(2) else "");
      begin
        if Tool_Kind = Project.Tools_Kind then
          File.Copy (Project.Product, Folder, "mode=overwrite,preserve=timestamps");
          Promotion.Set_Message ("Successfully installed " & Project.Name & " in " & Folder);
        end if;
      end;
    end loop;
  exception
  when Occurance: others =>
    Log.Write ("Target.Installation_Handling:", Occurance);
    Promotion.Set_Error ("Installation of " & Project.Name & " failed");
  end Installation_Handling;


  procedure Promote (Filename : String) is

    procedure Promote (Is_Second_Compilation : Boolean := False) is
    begin
      if Project.Is_Program_Unit (Filename) then
        if Project.Environment_Defined (Is_Second_Compilation) then
          Project.Generate_New_Resource;
          Build (Filename);
          Modifier_Handling;
          Installation_Handling;
        else
          Promotion.Set_Error ("Build pragma undefined!", Filename);
        end if;
      else
        if Project.Is_Maching (Filename) then
          Compile (Filename);
        end if;
      end if;
    end Promote;

  begin
    --TEST--------------------------------
    Log.Write ("&&& Promote " & Filename);
    --------------------------------------
    Promote;
    if Project.Has_Second_Compiler then
      Promote (Is_Second_Compilation => True);
      Project.Set_Back_To_First_Compiler;
    end if;
    if Project.Is_Program_Unit (Filename) then
      Promotion.Define_Next_Message_Color (Promotion.Green);
    end if;
  end Promote;


  function Run (Executable : String;
                Parameters : String) return Boolean is
  begin
    --TEST---------------------------------------------------
    Log.Write ("&&& Run " & Executable & " " & Parameters);
    ---------------------------------------------------------
    Os.Process.Create (Executable => Executable,
                       Parameters => Parameters,
                       Console    => Os.Process.Invisible);
    return True;
  exception
  when others =>
    return False;
  end Run;

end Target;
