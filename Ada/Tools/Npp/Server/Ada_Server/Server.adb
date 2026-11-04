-- *********************************************************************************************************************
-- *                       (c) 2008 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Definite_Doubly_Linked_Lists;
with Ada.Characters.Handling;
with Ada_95.Library;
with Ada_95.Name;
with Ada_95.Project;
with Ada_95.Source.File;
with Ada_95.Token.Data;
with Log;
with Project;
with Promotion;

package body Server is

  subtype Token_Handle is Ada_95.Token.Handle;

  type Action is (Reference, Promote);

  Last_Action : Action;


  function Is_In_Project (Name : String) return Boolean is
  begin
    return Project.Is_Source (Name);
  end Is_In_Project;


  function Project_Opened (Name : String) return Boolean is
  begin
    if Project.Initialized (Name) then
      Log.Write ("&&& Project opened: " & Name);
      return True;
    else
      Log.Write ("&&& Project not opened: " & Name);
      return False;
    end if;
  end Project_Opened;


  procedure Close_Project is
  begin
    Project.Finalize;
    Ada_95.Project.Finalize;
  end Close_Project;


  function Edge_Column return Server.Column_Position is
  begin
    return Ada_95.Max_Line_Length;
  end Edge_Column;


  function Known_Extensions return String is
  begin
    return "|ads|adb|";
  end Known_Extensions;


  --TEST-----------------------------------------------------
  --function From_Buffer (Is_True : Boolean) return String is
  --begin
  --  if Is_True then
  --    return "";
  --  else
  --    return " (FROM BUFFER)";
  --  end if;
  --end From_Buffer;
  -----------------------------------------------------------


  The_Case_Data  : Case_Data(1 .. Source_Buffer'size / Case_Info'size);
  The_Case_Index : Positive;

  function Case_Updates return Case_Data is
  begin
    --TEST--------------------------
    --Log.Write ("&&& Case_Update");
    --------------------------------
    return The_Case_Data(The_Case_Data'first .. The_Case_Index - 1);
  end Case_Updates;


  function Updates_For (The_Filename : String;
                        First_Line   : Line_Number;
                        Last_Line    : Line_Number;
                        Content      : String) return Tokens is

    The_Token      : Token_Handle;
    The_Line       : Line_Number := Line_Number'first;
    The_Results    : Tokens(1 .. 100000);
    Result_Index   : Positive := The_Results'first;
    Area_Cleared   : Boolean := False;
    After_Error    : Boolean := False;


    procedure Append_Black_Area (From_Line   : Line_Number;
                                 To_Line     : Line_Number;
                                 From_Column : Column_Range := Column_Range'first;
                                 To_Column   : Column_Range := Column_Range'last) is
    begin
      --TEST---------------------------------------------------------------
      --Log.Write ("&&& BLACK MARK INDEX:" & Natural'image(Result_Index));
      --Log.Write ("    First_Line      :" & Line_Number'image(From_Line));
      --Log.Write ("    Last_Line       :" & Line_Number'image(To_Line));
      ---------------------------------------------------------------------
      if Result_Index <= The_Results'last then
        The_Results(Result_Index) := (First_Line   => From_Line,
                                      Last_Line    => To_Line,
                                      First_Column => From_Column,
                                      Last_Column  => To_Column,
                                      Kind         => Is_Others);
        Result_Index := Result_Index + 1;
      end if;
    end Append_Black_Area;


    procedure Handle_Visible_Token (Area : Ada_95.Token.Area) is

      Kind : constant Token_Kind := Token_Kind(Ada_95.Project.Token_Kind_Of (The_Token));

      procedure Append_Token is
        Last_Column : Column_Range := Column_Range'first;
      begin
        if Result_Index <= The_Results'last then
          if Area.Last_Column > Last_Column then
            Last_Column := Area.Last_Column;
          end if;
          --TEST----------------------------------------------------------
          --Log.Write ("&&& RESULT INDEX:" & Natural'image(Result_Index));
          ----------------------------------------------------------------
          The_Results(Result_Index) := (First_Line   => The_Line,
                                        Last_Line    => The_Line + Area.Last_Column_Line_Offset,
                                        First_Column => Area.First_Column,
                                        Last_Column  => Last_Column,
                                        Kind         => Kind);
          Result_Index := Result_Index + 1;
        end if;
      end Append_Token;

      procedure Append_Case_Data (Image, Expected : String) is
        The_Mask   : Case_Mask := 0;
        The_Count  : Natural := 0;
        The_Column : Column_Range := Area.First_Column;
      begin
        for Index in Image'range loop
          if The_Count = 32 then
            if The_Mask /= 0 then
              The_Case_Data (The_Case_Index) := (The_Line, The_Column, The_Mask);
              The_Case_Index := The_Case_Index + 1;
            end if;
            The_Column := The_Column + 32;
            The_Count := 0;
            The_Mask := 0;
          end if;
          if Image(Index) /= Expected(Index) then
            The_Mask := The_Mask + Case_Mask(2 ** The_Count);
          end if;
          The_Count := The_Count + 1;
        end loop;
        if The_Mask /= 0 then
          The_Case_Data (The_Case_Index) := (The_Line, The_Column, The_Mask);
          The_Case_Index := The_Case_Index + 1;
        end if;
      end Append_Case_Data;

      procedure Handle_Case (Change_Identifiers : Boolean := True) is
      begin
        case Ada_95.Token.Kind_Of (The_Token.all) is
        when Ada_95.Token.Is_Reserved_Word | Ada_95.Token.Is_Attribute=>
          declare
            Id : constant Ada_95.Name.Handle := Ada_95.Token.Name_Handle(The_Token).Id;
          begin
            if not Ada_95.Name.Is_Lower (Id) then
              declare
                Image : constant String := Ada_95.Name.Image_Of (Id);
              begin
                Append_Case_Data (Image, Ada.Characters.Handling.To_Lower (Image));
              end;
            end if;
          end;
        when Ada_95.Token.Is_Aspect | Ada_95.Token.Is_Pragma_Identifier =>
          declare
            Id : constant Ada_95.Name.Handle := Ada_95.Token.Name_Handle(The_Token).Id;
          begin
            if not Ada_95.Name.Is_Capitalized_Or_Upper (Id) then
              declare
                Image     : constant String := Ada_95.Name.Image_Of (Id);
                New_Image : String := Image;
                After_Gap : Boolean := True;
              begin
                for The_Character of New_Image loop
                  if The_Character = '_' then
                    After_Gap := True;
                  elsif After_Gap then
                    The_Character := Ada.Characters.Handling.To_Upper (The_Character);
                    After_Gap := False;
                  else
                    The_Character := Ada.Characters.Handling.To_Lower (The_Character);
                  end if;
                end loop;
                Append_Case_Data (Image, New_Image);
              end;
            end if;
          end;
        when Ada_95.Token.Is_Identifier =>
          if Change_Identifiers then
            declare
              The_Reference : constant Ada_95.Token.Handle := Ada_95.Token.Reference_Of (The_Token);
              use type Ada_95.Token.Handle;
            begin
              if The_Reference /= null then
                declare
                  Id          : constant Ada_95.Name.Handle := Ada_95.Token.Name_Handle(The_Token).Id;
                  Expected_Id : constant Ada_95.Name.Handle := Ada_95.Token.Name_Handle(The_Reference).Id;
                begin
                  if not Ada_95.Name.Is_Equivalent (Id, Expected_Id) then
                    Append_Case_Data (Ada_95.Name.Image_Of (Id), Ada_95.Name.Image_Of (Expected_Id));
                  end if;
                end;
              end if;
            end;
          end if;
        when others =>
          null;
        end case;
      exception
      when Item: others =>
        Log.Write (Item);
      end Handle_Case;

    begin -- Handle_Visible_Token
      --TEST--------------------------------------------------------------------------------------
      --Log.Write ("&&& Token Kind         : "& Token_Kind'image (Kind));
      --Log.Write ("    At Line            :" & Line_Number'image(The_Line));
      --Log.Write ("    Area.First_Column  :" & Column_Range'image (Area.First_Column));
      --Log.Write ("    Area.Last_Column   :" & Natural'image (Natural(Area.Last_Column)));
      --Log.Write ("    Area.Line_Increment:" & Line_Number'image (Area.Last_Column_Line_Offset));
      --------------------------------------------------------------------------------------------
      case Kind is
      when Is_Style_Error | Is_Semantic_Error | Is_Syntax_Error =>
        After_Error := True;
        Append_Token;
      when Is_Unknown_Identifier =>
        if not After_Error then
          Append_Token;
        end if;
      when Is_Others =>
        null;
      when others =>
        if The_Line in First_Line .. Last_Line then
          if not Area_Cleared then
            Append_Black_Area (From_Line   => The_Line,
                               To_Line     => Last_Line,
                               From_Column => Area.First_Column);
            Area_Cleared := True;
          end if;
          Append_Token;
        end if;
      end case;
      if Content'length > 0 then
        case Project.Case_Handling_Style is
        when Project.Change_All =>
          Handle_Case;
        when Project.Keywords =>
          Handle_Case (Change_Identifiers => False);
        when Project.No_Change =>
          null;
        end case;
      end if;
    end Handle_Visible_Token;

    use type Token_Handle;

  begin -- Update_For
    --TEST-------------------------------------------------------------
    --Log.Write ("&&& Update " & The_Filename &
    --                " lines" & Server.Line_Number'image(First_Line) &
    --                   " .." & Server.Line_Number'image(Last_Line));
    -------------------------------------------------------------------
    The_Case_Index := The_Case_Data'first;
    if (First_Line <= Last_Line) then
      The_Token := Ada_95.Project.Token_Handle_Of (Filename => The_Filename,
                                                   Content  => Content);
      if The_Token /= null then
        --TEST--------------------------------------
        --Log.Write ("&&& Updated " & The_Filename);
        --------------------------------------------
        while The_Token /= null loop
          case Ada_95.Token.Kind_Of (The_Token.all) is
          when Ada_95.Token.Is_Invisible =>
            null;
          when others =>
            Handle_Visible_Token (Ada_95.Token.Area_Of (The_Token.all));
          end case;
          if The_Token.all in Ada_95.Token.Line_Object'class then
            if The_Line > Line_Number'last - Ada_95.Token.Line_Handle (The_Token).Count then
              return No_Tokens; -- file too big
            end if;
            The_Line := The_Line + Ada_95.Token.Line_Handle (The_Token).Count;
          end if;
          The_Token := The_Token.Next;
        end loop;
      end if;
    end if;
    return The_Results(The_Results'first .. Result_Index - 1);
  end Updates_For;


  The_Reference : Token_Handle;

  function Referenced (The_Filename : String;
                       At_Column    : Column_Range;
                       At_Line      : Line_Number;
                       Content      : String) return Boolean is

    use type Token_Handle;

  begin
    --TEST----------------------------------------------------------------------------------------------
    --Log.Write ("<== Referenced " & The_Filename &
    --                  " at line" & Server.Line_Number'image(At_Line) &
    --                  ", column" & Server.Column_Range'image(At_Column) & From_Buffer (Content = ""));
    ----------------------------------------------------------------------------------------------------
    if not Project.Is_Defined then
      return False;
    end if;
    The_Reference := Ada_95.Project.Reference_Of (Filename => The_Filename,
                                                  Line     => At_Line,
                                                  Column   => At_Column,
                                                  Content  => Content);
    if The_Reference = null then
      The_Reference := Ada_95.Project.First_Error_Of (The_Filename, Content);
    end if;
    Last_Action := Reference;
    return The_Reference /= null;
  end Referenced;


  package Reference_List is new Definite_Doubly_Linked_Lists (File_Reference);


  function Usage (The_Filename : String;
                  At_Column    : Column_Range;
                  At_Line      : Line_Number;
                  Content      : String) return Reference_Data is

    Used_Token : constant Token_Handle := Ada_95.Project.Token_Handle_Of (The_Filename, At_Line, At_Column, Content);

    Used_Token_Image : constant String := Ada_95.Project.Full_Name_Of (Used_Token);

    The_Filenames   : Text.List;
    The_Line_Images : Text.List;
    The_References  : Reference_List.Item;

    use type Reference_List.Item;


    procedure Get_Usages_For (Unit : Ada_95.Library.Handle) is

      The_Token : Token_Handle;
      use type Token_Handle;

      Filename_Added : Boolean:= False;

    begin -- Get_Usages_For
      The_Token := Ada_95.Token.Data.Resource(Unit.all).Tokens.First;
      while The_Token /= null loop
        if The_Token /= Used_Token and then The_Token.all in Ada_95.Token.Identifier'class then
          if Ada_95.Token.Declaration_Of (The_Token) = Used_Token then
            if not Filename_Added then
              The_Filenames.Append (Ada_95.Token.Data.Resource(Unit.all).Attributes.Handle.Id);
              Filename_Added := True;
            end if;
            declare
              Line_Image : constant String := Ada_95.Token.Line_Image_Of (The_Token);
            begin
              if The_Line_Images.Is_Empty or else The_Line_Images.Last_Element /= Line_Image then
                The_Line_Images.Append (Line_Image);
              end if;
            end;
            The_References := The_References +
              File_Reference'(Cursor      => (Line   => Ada_95.Token.Line_Number_Of (The_Token),
                                              Column => Ada_95.Token.Column_Position_Of (The_Token)),
                              File_Index  => The_Filenames.Count,
                              Image_Index => The_Line_Images.Count);
          end if;
        end if;
        The_Token := The_Token.Next;
      end loop;
    end Get_Usages_For;


    procedure Get_Actual_Usages is

      procedure Get_For (Element : Ada_95.Library.Element_Cursor) is

        use type Ada_95.Library.Handle;

      begin
        Get_Usages_For (Unit => Element.Unit);
        if Element.Unit.all in Ada_95.Token.Data.Unit_Declaration'class then
          declare
            Unit : constant Ada_95.Library.Handle
              := Ada_95.Library.Handle(Ada_95.Token.Data.Unit_Declaration_Handle(Element.Unit).Implementation);
          begin
            if Unit /= null then
              Get_Usages_For (Unit => Unit);
            end if;
          end;
        end if;
      end Get_For;

      procedure Get_All_Usages is new Ada_95.Library.Iterator (Get_For);

    begin -- Get_Actual_Usages
      Get_All_Usages;
    end Get_Actual_Usages;


    procedure Get_Usages is

      Program : constant String := Project.Program_Unit;

      procedure Get_For (Used_Unit : Ada_95.Library.Handle) is
      begin
        Get_Usages_For (Unit => Used_Unit);
      end Get_For;

      procedure Get_All_Usages_For is new Ada_95.Library.Iterator_With (Get_For);

      The_Program : Ada_95.Library.Handle;

      use type Ada_95.Library.Handle;

    begin -- Get_Usages
      if Program /= "" then
        The_Program := Ada_95.Library.Added (Ada_95.Source.File.New_With (Program));
        if The_Program /= null then
          Get_All_Usages_For (The_Program, Visit_Body => True);
        end if;
      else
        Get_Actual_Usages;
      end if;
    end Get_Usages;

    use type Ada_95.Library.Handle;

    function S return String is
    begin
      if The_References.Count = 1 then
        return "";
      else
        return "s";
      end if;
    end S;

    function Usage_Message return String is
    begin
      if Ada_95.Project.Local_Unit /= null then
        Get_Usages_For (Unit => Ada_95.Project.Local_Unit);
        if The_References.Count = 0 then
          return "%rNo local usage";
        else
          return "%bFound" & The_References.Count'img & " local reference" & S;
        end if;
      else
        Get_Usages;
        if The_References.Count = 0 then
          return "%rNo global usage";
        else
          return "%bFound" & The_References.Count'img & " global reference" & S;
        end if;
      end if;
    end Usage_Message;

  begin -- Usage
    --TEST----------------------------------------------------------
    --Log.Write ("<== Usage of " & The_Filename &
    --           " at line" & Server.Line_Number'image(At_Line) &
    --           ", column" & Server.Column_Range'image(At_Column));
    ----------------------------------------------------------------
    if not Project.Is_Defined then
      return No_Reference_Data;
    elsif not Ada_95.Token.Is_Used (Used_Token) and then Used_Token_Image = "" then
      return No_Reference_Data;
    end if;
    declare
      Message     : constant String := Usage_Message;
      Filenames   : constant String := Names_Of (The_Filenames);
      Line_Images : constant String := Names_Of (The_Line_Images);
    begin
      return Reference_Data'(Message_Length     => Message'length,
                             Filenames_Length   => Filenames'length,
                             Line_Images_Length => Line_Images'length,
                             Message            => Message,
                             Reference_Count    => Ada_95.Token.Counter(The_References.Count),
                             Filenames          => Filenames,
                             Line_Images        => Line_Images,
                             Locations          => File_References(The_References.Elements));
    end;
  exception
  when Occurence: others=>
    Log.Write ("!!! Server.Used", Occurence);
    return No_Reference_Data;
  end Usage;


  function Usage (The_Filename : String;
                  At_Column    : Column_Range;
                  At_Line      : Line_Number;
                  Content      : String) return References is
  begin
    raise Program_Error;
    return null;
  end Usage;


  function Unused return Reference_Data is

    The_Filenames   : Text.List;
    The_Line_Images : Text.List;
    The_References  : Reference_List.Item;

    use type Reference_List.Item;

    procedure Get_Unused_For (Unit : Ada_95.Library.Handle) is

      The_Token : Token_Handle;
      use type Token_Handle;
      use type Ada_95.Token.Data_Handle;

      Filename_Added : Boolean:= False;

      Max_Unused_Tokens : constant := 500;
      Max_Line_Count    : constant := 1000;

      The_Token_Count : Natural := 0;

    begin -- Get_Unused_For
      The_Token := Ada_95.Token.Data.Resource(Unit.all).Tokens.First;
      while The_Token /= null loop
        if The_Token.all in Ada_95.Token.Identifier'class then
          if Ada_95.Token.Is_Unused_Declaration (The_Token) then
            if not (Unit.all in Ada_95.Token.Data.Package_Specification'class
                              | Ada_95.Token.Data.Library_Subprogram_Declaration'class
                              | Ada_95.Token.Data.Generic_Library_Subprogram_Declaration'class)
            then
              if not Filename_Added then
                The_Filenames.Append (Ada_95.Token.Data.Resource(Unit.all).Attributes.Handle.Id);
                Filename_Added := True;
              end if;
              The_Line_Images.Append (Ada_95.Token.Line_Image_Of (The_Token));
              The_Token_Count := The_Token_Count + 1;
              The_References := The_References +
                File_Reference'(Cursor      => (Line   => Ada_95.Token.Line_Number_Of (The_Token),
                                                Column => Ada_95.Token.Column_Position_Of (The_Token)),
                                File_Index  => The_Filenames.Count,
                                Image_Index => The_Line_Images.Count);
            end if;
          elsif Ada_95.Token.Identifier_Handle(The_Token).Data = null then
            Log.Write ("___ No reference in file " & Ada_95.Token.Data.Resource(Unit.all).Attributes.Handle.Id &
                       " at line" & Line_Number'image(Ada_95.Token.Line_Number_Of (The_Token)));
          end if;
        end if;
        The_Token := The_Token.Next;
        if The_Token_Count >= Max_Unused_Tokens then
          Log.Write ("%%% too many unused tokens");
          exit;
        end if;
        if The_Line_Images.Count >= Max_Line_Count then
          Log.Write ("%%% too many unused token lines");
          exit;
        end if;
      end loop;
    end Get_Unused_For;


    Program : constant String := Project.Program_Unit;

    function Unused_Declarations return String is

      The_Program : Ada_95.Library.Handle;

      procedure Load (Unit : Ada_95.Library.Handle) is
        use type Ada_95.Library.Handle;
      begin
        if Unit = null then
          raise Program_Error;
        end if;
      end Load;

      procedure Get_For (Used_Module : Ada_95.Library.Handle) is
      begin
        if not Project.Is_In_Library_Area (Ada_95.Token.Data.Resource(Used_Module.all).Attributes.Handle.Id) then
          Get_Unused_For (Unit => Used_Module);
        end if;
      end Get_For;

      procedure Load_All_For is new Ada_95.Library.Iterator_With (Load);

      procedure Get_All_Unused_For is new Ada_95.Library.Iterator_With (Get_For);

      use type Ada_95.Library.Handle;

      function S return String is
      begin
        if The_References.Count = 1 then
          return "";
        else
          return "s";
        end if;
      end S;

    begin -- Unused_Declarations
      The_Program := Ada_95.Library.Added (Ada_95.Source.File.New_With (Program));
      if The_Program /= null then
        Load_All_For (The_Program, Visit_Body => True);
        Get_All_Unused_For (The_Program, Visit_Body => True);
        if The_References.Count = 0 then
          return "%bNo unused declarations";
        else
          return "%rFound" & The_References.Count'img & " unused declaration" & S;
        end if;
      else
        return "%rProgram unit unknown";
      end if;
    end Unused_Declarations;

  begin -- Unused
    --TEST---------------------
    --Log.Write ("<== Unused");
    ---------------------------
    if not Project.Is_Defined then
      return No_Reference_Data;
    end if;
    declare
      Message     : constant String := Unused_Declarations;
      Filenames   : constant String := Names_Of (The_Filenames);
      Line_Images : constant String := Names_Of (The_Line_Images);
    begin
      return (Message_Length     => Message'length,
              Filenames_Length   => Filenames'length,
              Line_Images_Length => Line_Images'length,
              Message            => Message,
              Reference_Count    => Ada_95.Token.Counter(The_References.Count),
              Filenames          => Filenames,
              Line_Images        => Line_Images,
              Locations          => File_References(The_References.Elements));
    end;
  exception
  when Occurence: others =>
    Log.Write ("!!! Server.Unused", Occurence);
    return No_Reference_Data;
  end Unused;


  function Unused return References is
  begin
    raise Program_Error;
    return null;
  end Unused;


  procedure Promote (Name : String;
                     Kind : Promotion_Kind := Normal) is
  begin
    --TEST------------------------------
    --Log.Write ("<== Promote " & Name);
    ------------------------------------
    Last_Action := Promote;
    Promotion.Start (Name, Kind);
  end Promote;


  function Has_Promotion_Message return Boolean is
  begin
    return Promotion.Message_Ready;
  end Has_Promotion_Message;


  function Has_Promotion_Error return Boolean is
  begin
    return Promotion.Error_Message_Ready;
  end Has_Promotion_Error;


  function Message return String is
    Item : constant String := Promotion.Message;
  begin
    --TEST-------------------------------
    --Log.Write ("==> Message: " & Item);
    -------------------------------------
    return Item;
  end Message;


  function Filename return String is
  begin
    case Last_Action is
    when Reference =>
      declare
        Item : constant String := Ada_95.Token.Filename_Of (The_Reference);
      begin
        --TEST-------------------------------------------
        --Log.Write ("==> Referenced Filename: " & Item);
        -------------------------------------------------
        return Item;
      end;
    when Promote =>
      return Promotion.Filename;
    end case;
  end Filename;


  function Line return Line_Number is
  begin
    case Last_Action is
    when Reference =>
      return Ada_95.Token.Line_Number_Of (The_Reference);
    when Promote =>
      return Promotion.Line;
    end case;
  end Line;


  function Column return Column_Range is
  begin
    case Last_Action is
    when Reference =>
      return Ada_95.Token.Column_Position_Of (The_Reference);
    when Promote =>
      return Promotion.Column;
    end case;
  end Column;


  function Names_Of (Item : Text.List) return String is
  begin
    return Item.To_Data (Names_Separator);
  end Names_Of;

end Server;
