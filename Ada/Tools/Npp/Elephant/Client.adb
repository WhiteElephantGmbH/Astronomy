-- *********************************************************************************************************************
-- *                       (c) 2014 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *                                                                                                                   *
-- *    This program is free software; you can redistribute it and/or modify it under the terms of the GNU General     *
-- *    Public License as published by the Free Software Foundation; either version 2 of the License, or               *
-- *    (at your option) any later version.                                                                            *
-- *                                                                                                                   *
-- *    This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the     *
-- *    implied warranty of MERCHANTABILITY or FITNESS for A PARTICULAR PURPOSE. See the GNU General Public License    *
-- *    for more details.                                                                                              *
-- *                                                                                                                   *
-- *    You should have received a copy of the GNU General Public License along with this program; if not, write to    *
-- *    the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.                *
-- *********************************************************************************************************************
pragma Style_Astronomy;

with Ada.Containers.Indefinite_Vectors;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with File;
with Indefinite_Doubly_Linked_Lists;
with Log;
with Npp.Buffer;
with Npp.Message;
with Npp.Plugin;
with Npp.Tree_View;
with Rt;
with Scintilla;
with Text;
with System;
with Win;

package body Client is

  subtype Column_Range is Server.Column_Range;
  subtype Line_Number  is Server.Line_Number;

  Project_Is_Open : Boolean := False;


  function Buffer_Name return String is
  begin
    return Npp.Buffer.Name;
  end Buffer_Name;


  procedure Show (Message : String) is
    The_Color : Win.COLORREF;
  begin
    if Message'length > 1 and then Message(Message'first) = '%' then
      case Message(Message'first + 1) is
      when 'g' =>
        The_Color := Win.RGB (Red => 0, Green => 192, Blue => 0);
      when 'b' =>
        The_Color := Win.RGB (Red => 0, Green => 0, Blue => 192);
      when 'o' =>
        The_Color := Win.RGB (Red => 255, Green => 128, Blue => 0);
      when 'r' =>
        The_Color := Win.RGB (Red => 192, Green => 0, Blue => 0);
      when others =>
        null;
      end case;
      Npp.Message.Write_Line (Message(Message'first + 2 .. Message'last), The_Color);
    else
      Npp.Message.Write_Line (Message);
    end if;
  end Show;


  procedure Show_Error (Message : String) is
  begin
    Npp.Plugin.Show_Header;
    Show ("%r" & Message);
  end Show_Error;


  function All_Saved return Boolean is
  begin
    if Npp.Plugin.All_Files_Saved then
      Rt.Wait (1.0); -- wait for files to be written
    end if;
    return True;
  end All_Saved;


  function No_Buffer return Boolean is
    Buffer_Filename : constant String := Text.Trimmed (Buffer_Name);
  begin
    if Buffer_Filename(Buffer_Filename'first .. Buffer_Filename'first + 2) = "new" then
      return True;
    elsif Buffer_Filename = "" then
      Show_Error ("No buffer selected");
      return True;
    end if;
    return False;
  end No_Buffer;


  function No_Project return Boolean is
  begin
    if Project_Is_Open then
      return False;
    end if;
    Show_Error ("No project open");
    return True;
  end No_Project;


  function Buffer_Content (Handle : Scintilla.Object) return String is
  begin
    if Scintilla.Buffer_Is_Modified (Handle) then
      return Scintilla.Buffer_Content (Handle);
    end if;
    return "";
  end Buffer_Content;


  --------------
  -- Updating --
  --------------

  procedure Update_Style (Filename : String;
                          Handle   : Scintilla.Object) is

    function Token_Style_For (Token_Kind : Server.Token_Kind) return Scintilla.Style is
    begin
      case Token_Kind is
      when Server.Is_Attribute =>
        return Scintilla.Attributes;
      when Server.Is_Comment =>
        return Scintilla.Comment;
      when Server.Is_Special_Comment =>
        return Scintilla.Special_Comment;
      when Server.Is_Directive =>
        return Scintilla.Directive;
      when Server.Is_Reserved_Word =>
        return Scintilla.Reserved_Word;
      when Server.Is_Character_Literal =>
        return Scintilla.Character_Literal;
      when Server.Is_Numeric_Literal =>
        return Scintilla.Numeric_Literal;
      when Server.Is_String_Literal =>
        return Scintilla.String_Literal;
      when Server.Is_Type =>
        return Scintilla.Types;
      when Server.Is_Unused_Declaration =>
        return Scintilla.Unused_Declaration;
      when Server.Is_Unused_Type_Declaration =>
        return Scintilla.Unused_Type_Declaration;
      when Server.Is_Unknown_Identifier =>
        return Scintilla.Unknown_Identifier;
      when Server.Is_Style_Error =>
        return Scintilla.Style_Error;
      when Server.Is_Semantic_Error =>
        return Scintilla.Semantic_Error;
      when Server.Is_Syntax_Error =>
        return Scintilla.Syntax_Error;
      when Server.Is_Compilation_Error =>
        return Scintilla.Compilation_Error;
      when Server.Is_Others =>
        return Scintilla.Default;
      end case;
    end Token_Style_For;

    procedure Set_Style_For (Info : Server.Token_Info) is
    begin
      Scintilla.Set (Handle       => Handle,
                     First_Line   => Positive(Info.First_Line),
                     Last_Line    => Positive(Info.Last_Line),
                     First_Column => Positive(Info.First_Column),
                     Last_Column  => Positive(Info.Last_Column),
                     To_Style     => Token_Style_For (Info.Kind));
    end Set_Style_For;

    procedure Change_Case_For (Info : Server.Case_Info) is
    begin
      --TEST--------------------------------------------------------------
      --Log.Write ("&&& Change Case");
      --Log.Write ("      Line   :" & Line_Number'image (Info.Line));
      --Log.Write ("      Column :" & Column_Range'image (Info.Column));
      --Log.Write ("      Mask   :" & Server.Case_Mask'image (Info.Mask));
      --------------------------------------------------------------------
      declare
        The_Column : Long_Integer     := Long_Integer(Info.Column);
        The_Mask   : Server.Case_Mask := Info.Mask;
        use type Server.Case_Mask;
      begin
        loop
          if (The_Mask and 1) = 1 then
            Scintilla.Change_Case (Handle => Handle,
                                   Line   => Positive(Info.Line),
                                   Column => Positive(The_Column));
          end if;
          The_Column := The_Column + 1;
          The_Mask := The_Mask / 2;
          exit when The_Mask = 0;
        end loop;
      end;
    end Change_Case_For;

  begin -- Update_Style
    declare
      Results : constant Server.Tokens := Server.Updates_For (The_Filename => Filename,
                                                              First_Line   => Line_Number'first,
                                                              Last_Line    => Line_Number'last,
                                                              Content      => Buffer_Content(Handle));
    begin
      for Result of Results loop
        Set_Style_For (Result);
      end loop;
    end;
    declare
      Case_Data : constant Server.Case_Data := Server.Case_Updates;
    begin
      for Case_Info of Case_Data loop
        Change_Case_For (Case_Info);
      end loop;
    end;
  end Update_Style;


  ---------------
  -- Promoting --
  ---------------

  task type Control is

    entry Promotion (Message : String);

    entry Termination;

  end Control;

  Error_Is_Set : Boolean := False;
  The_Control  : access Control;


  procedure Promoting (Message : String := "") is
    Editor : Scintilla.Object;
  begin
    if Message /= "" then
      Show (Message);
      while Server.Has_Promotion_Message loop
        Show (Server.Message);
      end loop;
    end if;
    if Server.Has_Promotion_Error then
      Show_Error (Server.Message);
      if Server.Filename /= "" then
        if Npp.Plugin.Opened (Server.Filename) then
          Scintilla.Create (Editor, Npp.Plugin.Edit_View);
          Scintilla.Set_Cursor (Handle => Editor,
                                Line   => Positive(Server.Line),
                                Column => Positive(Server.Column));
          Error_Is_Set := True;
          Scintilla.Set (Handle     => Editor,
                         First_Line => Positive(Server.Line),
                         Last_Line  => Positive(Server.Line),
                         To_Style   => Scintilla.Compilation_Error);
        else
          Show_Error (Server.Filename & " - Column:" & Long_Integer(Server.Column)'img);
        end if;
      end if;
    else
      Npp.Plugin.Show_Header;
    end if;
  exception
  when Item: others =>
    Log.Write ("Client.Promoting", Item);
    Show_Error ("Promoting failed");
  end Promoting;


  task body Control is
    The_Message : Text.String;
  begin
    Log.Write ("Client.Control.Started");
    loop
      select
        accept Promotion (Message : String) do
          The_Message := [Message];
        end Promotion;
        Promoting (The_Message.S);
      or
        accept Termination;
        exit;
      end select;
    end loop;
    Log.Write ("Client.Control.Terminated");
  exception
  when Item: others =>
    Log.Write ("Client.Control", Item);
  end Control;


  procedure Termination is
  begin
    Npp.Plugin.Show_Header;
    The_Control.Termination;
    The_Control := null;
  exception
  when Item: others =>
    Log.Write ("Client.Termination", Item);
  end Termination;


  procedure Controlled_Promoting (Message : String) is
  begin
    Npp.Plugin.Hide_Header;
    if The_Control = null then
      The_Control := new Control;
      Npp.Plugin.Install_Termination (Termination'access);
    end if;
    The_Control.Promotion (Message);
  end Controlled_Promoting;


  procedure Promote (Kind : Server.Promotion_Kind := Server.Normal) is

     use type Server.Promotion_Kind;
  begin
    Npp.Message.Clear;
    if No_Project or else not All_Saved then
      return;
    end if;
    Server.Promote (Buffer_Name, Kind);
    while Server.Has_Promotion_Message loop
      declare
        Message : constant String := Server.Message;
        function Found (Item : String) return Boolean is
          (Text.Location_Of (Item, Text.Lowercase_Of (Message)) /= Text.Not_Found);
      begin
        if Kind = Server.All_Projects or else (Found ("build") or Found ("link") or Found ("archive")) then
          Controlled_Promoting (Message);
          return;
        else
          Show (Message);
        end if;
      end;
    end loop;
    Promoting;
  end Promote;


  -----------------
  -- Referencing --
  -----------------

  type Location (Length : Natural) is record
    Filename : String (1..Length);
    Line     : Line_Number;
    Column   : Column_Range;
  end record;

  package Location_Stack is new Indefinite_Doubly_Linked_Lists (Location);

  The_Location_Stack : Location_Stack.Item;

  use type Location_Stack.Item;

  package Lists is new Ada.Containers.Indefinite_Vectors (Element_Type => String, Index_Type => Positive);

  subtype List is Lists.Vector;

  subtype Filenames is List;
  type Locations is access Server.File_References;

  The_Filenames : Filenames;
  The_Locations : Locations;


  procedure Reference (Filename         : String;
                       At_Location      : Server.Location;
                       Show_Cursor_Line : Boolean := False) is
    Editor : Scintilla.Object;
  begin
    if Npp.Plugin.Opened (Filename) then
      Scintilla.Create (Editor, Npp.Plugin.Edit_View);
      if Show_Cursor_Line then
        Scintilla.Set_Cursor (Handle => Editor,
                              Line   => Positive(At_Location.Line),
                              Column => Positive(Column_Range'first));
        Scintilla.Show_Cursor_Line (Editor);
      else
        Scintilla.Set_Cursor (Handle => Editor,
                              Line   => Positive(At_Location.Line),
                              Column => Positive(At_Location.Column));
      end if;
    else
      Show_Error (Filename &
                  " (" & Line_Number'image(At_Location.Line) &
                  ", " & Column_Range'image(At_Location.Column) & ")");
    end if;
  end Reference;


  procedure Show_Reference is
    Editor : Scintilla.Object;
  begin
    Npp.Message.Clear;
    if No_Project or else No_Buffer then
      return;
    end if;
    Scintilla.Create (Editor, Npp.Plugin.Edit_View);
    declare
      Current_Location : constant Location := (Length   => Buffer_Name'length,
                                               Filename => Buffer_Name,
                                               Line     => Line_Number(Scintilla.Current_Line(Editor)),
                                               Column   => Column_Range(Scintilla.Current_Column(Editor)));
    begin
      if Server.Referenced (The_Filename => Buffer_Name,
                            At_Line      => Current_Location.Line,
                            At_Column    => Current_Location.Column,
                            Content      => Buffer_Content (Editor))
      then
        The_Location_Stack := The_Location_Stack + Current_Location;
        Reference (Server.Filename, Server.Location'(Line => Server.Line, Column => Server.Column));
      else
        Show_Error ("No reference");
      end if;
    end;
  end Show_Reference;


  procedure Undo_Reference is
    Editor : Scintilla.Object;
  begin
    Npp.Message.Clear;
    if not The_Location_Stack.Is_Empty then
      declare
        Last_Location : constant Location := The_Location_Stack.Last_Element;
      begin
        The_Location_Stack.Delete_Last;
        if Npp.Plugin.Opened (Last_Location.Filename) then
          Scintilla.Create (Editor, Npp.Plugin.Edit_View);
          Scintilla.Set_Cursor (Handle => Editor,
                                Line   => Positive(Last_Location.Line),
                                Column => Positive(Last_Location.Column));
        else
          Show_Error ("Reference obsolete");
          The_Location_Stack.Clear;
        end if;
      end;
    else
      Show_Error ("Nothing to undo");
    end if;
  end Undo_Reference;


  procedure Show_References (References : Server.References;
                             Expand     : Boolean := False) is

    function List_Of (Names : String) return List is
      The_List  : List;
      The_First : Natural := Names'first;
    begin
      for Index in Names'range loop
        if Names(Index) = Ascii.Nul then
          The_List.Append (Names(The_First .. Index - 1));
          The_First := Index + 1;
        end if;
      end loop;
      if The_First <= Names'last then
        The_List.Append (Names(The_First .. Names'last));
      end if;
      return The_List;
    end List_Of;

    The_Line_Images : constant List := List_Of (References.Line_Images);
    The_File_Index  : Natural := 0;

    use type Server.References;

    Filename_Item : Npp.Tree_View.Item := Npp.Tree_View.Root;
    Unused        : Npp.Tree_View.Item;
    The_Line      : Text.String;

    procedure Dispose is new Ada.Unchecked_Deallocation (Server.File_References, Locations);

  begin -- Show_References
    The_Filenames := List_Of (References.Filenames);
    if The_Locations /= null then
      Dispose (The_Locations);
    end if;
    The_Locations := new Server.File_References'(References.Locations);
    Npp.Tree_View.Clear;
    Show (References.Message);
    if The_Locations'length /= 0 then
      for Index in The_Locations'range loop
        declare
          The_Location : Server.File_Reference renames The_Locations(Index);
        begin
          if The_Location.File_Index /= The_File_Index then
            if Expand and (The_File_Index /= 0) then
              Npp.Tree_View.Expand (Filename_Item);
            end if;
            The_File_Index := The_File_Index + 1;
            Filename_Item := Npp.Tree_View.Add (The_Filenames(The_File_Index));
          end if;
          declare
            Cursor_Mark   : constant Character := Character'val(149);
            Line          : constant String := The_Line_Images (The_Location.Image_Index);
            At_Position   : constant Natural := Line'first + Positive(The_Location.Cursor.Column) - 1;
            At_Line       : constant String := Text.Trimmed (The_Location.Cursor.Line'image);
            Last_Position : Natural := Line'last;
            use type Server.Line_Counter;
          begin
            if Index /= The_Locations'last then
              declare
                Next_Location : Server.File_Reference renames The_Locations(Index + 1);
              begin
                if (Next_Location.Cursor.Line = The_Location.Cursor.Line) and
                  (Next_Location.File_Index = The_Location.File_Index)
                then
                  Last_Position := Line'first + Positive(Next_Location.Cursor.Column) - 2;
                end if;
              end;
            end if;
            if Text.Is_Null (The_Line) then
              The_Line := [(At_Line & ' ' & Text.Trimmed (Line(Line'first .. At_Position - 1)))];
            end if;
            The_Line.Append (Cursor_Mark & Line(At_Position .. Last_Position));
            if Last_Position = Line'last then
              Unused := Npp.Tree_View.Add (Parent => Filename_Item,
                                           Data   => The_Location'address,
                                           Title  => The_Line.S);
              Text.Clear (The_Line);
            end if;
          end;
        end;
      end loop;
      if Expand then
        Npp.Tree_View.Expand (Filename_Item);
      end if;
    end if;
  end Show_References;


  procedure Show_Usage is
    Editor : Scintilla.Object;
  begin
    Npp.Message.Clear;
    if No_Project or else No_Buffer then
      return;
    end if;
    Scintilla.Create (Editor, Npp.Plugin.Edit_View);
    Show_References (References => Server.Usage (The_Filename => Buffer_Name,
                                                 At_Line      => Line_Number(Scintilla.Current_Line(Editor)),
                                                 At_Column    => Column_Range(Scintilla.Current_Column(Editor)),
                                                 Content      => Buffer_Content (Editor)),
                     Expand => True);
  exception
  when Item: others =>
    Log.Write ("Show_Usage", Item);
  end Show_Usage;


  procedure Show_Unused is
  begin
    Npp.Message.Clear;
    if No_Project or No_Buffer then
      return;
    end if;
    Show_References (Server.Unused);
  end Show_Unused;


  ------------------
  -- Notifications
  ------------------

  The_Known_Extensions : Text.String;

  procedure Open_Project is
  begin
    if No_Buffer then
      return;
    end if;
    Project_Is_Open := Server.Project_Opened (Buffer_Name);
    if Project_Is_Open then
      Show (Server.Message);
      The_Known_Extensions := [Server.Known_Extensions];
      declare
        Editor : Scintilla.Object;
      begin
        Scintilla.Create (Editor, Npp.Plugin.Edit_View);
        Scintilla.Define(Editor, Natural(Server.Edge_Column));
      end;
    else
      Show_Error (Server.Message);
    end if;
  end Open_Project;


  procedure Change_Project is
  begin
    if Project_Is_Open then
      if Server.Is_In_Project (Buffer_Name) then
        return;
      end if;
      Project_Is_Open := False;
      Text.Clear (The_Known_Extensions);
      Server.Close_Project;
      Show ("Project closed");
    end if;
    Open_Project;
  end Change_Project;


  procedure Update is
  begin
    if Project_Is_Open then
      if Error_Is_Set then
        Error_Is_Set := False;
      else
        declare
          Editor    : Scintilla.Object;
          Filename  : constant String := Buffer_Name;
          Extension : constant String := "|" & Text.File_Extension_Of (Filename) & "|";
        begin
          if The_Known_Extensions.Index_Of (Extension) /= Text.Not_Found then
            Scintilla.Create (Editor, Npp.Plugin.Edit_View);
            Scintilla.Define_Styles (Editor);
            Update_Style (Filename, Editor);
          end if;
        end;
      end if;
    end if;
  end Update;


  procedure Trim_Trailing_Spaces is
    package IO renames Ada.Text_IO;
  begin
    if Project_Is_Open then
      declare
        Filename           : constant String := Buffer_Name;
        Temporary_Filename : constant String := Buffer_Name & ".tmp";
        The_Input_File     : IO.File_Type;
        The_Output_File    : IO.File_Type;
        Is_Modified        : Boolean := False;
      begin
        if Server.Is_In_Project (Filename) then
          if File.Exists (Temporary_Filename) then
            Log.Write ("Trim_Trailing_Spaces - Exists: " & Temporary_Filename);
            File.Delete (Temporary_Filename);
          end if;
          IO.Open (File => The_Input_File,
                   Mode => IO.In_File,
                   Name => Filename);
          begin
            IO.Create (File => The_Output_File,
                       Mode => IO.Out_File,
                       Name => Temporary_Filename);
            begin
              while not IO.End_Of_File (The_Input_File) loop
                declare
                  Input_Line  : constant String := IO.Get_Line (The_Input_File);
                  Output_Line : constant String := Text.Trimmed_Trailing (Input_Line);
                begin
                  if Input_Line'length /= Output_Line'length then
                    Is_Modified := True;
                  end if;
                  IO.Put_Line (The_Output_File, Output_Line);
                end;
              end loop;
            exception
            when others =>
              IO.Close (The_Output_File);
              File.Delete (Temporary_Filename);
              raise;
            end;
            IO.Close (The_Output_File);
            IO.Close (The_Input_File);
          exception
          when others =>
            IO.Close (The_Input_File);
            raise;
          end;
          if Is_Modified then
            File.Delete (Filename);
            File.Rename (Old_Name => Temporary_Filename,
                         New_Name => Filename);
          else
            File.Delete (Temporary_Filename);
          end if;
        end if;
      end;
    end if;
  exception
  when Item: others =>
    Log.Write ("Trim_Trailing_Spaces", Item);
  end Trim_Trailing_Spaces;


  type File_Reference_Access is access all Server.File_Reference;

  function Convert is new Ada.Unchecked_Conversion (System.Address, File_Reference_Access);


  procedure Tree_View_Location_Handler (Data : System.Address) is
  begin
    declare
      At_Location : constant File_Reference_Access := Convert(Data);
    begin
      declare
        Filename : constant String := The_Filenames(At_Location.File_Index);
      begin
        Reference (Filename, At_Location.Cursor, Show_Cursor_Line => True);
      end;
    end;
  exception
  when others =>
    null;
  end Tree_View_Location_Handler;


  procedure Focus_Lost_Handler (Unused : System.Address) is
    Editor : Scintilla.Object;
  begin
    Scintilla.Create (Editor, Npp.Plugin.Edit_View);
    Scintilla.Hide_Cursor_Line (Editor);
  end Focus_Lost_Handler;

begin
  Npp.Tree_View.Install (Double_Click => Tree_View_Location_Handler'access,
                         Focus_Lost   => Focus_Lost_Handler'access);
  Npp.Message.Install;
end Client;
