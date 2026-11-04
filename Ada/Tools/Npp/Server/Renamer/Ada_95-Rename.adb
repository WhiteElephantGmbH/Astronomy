-- *********************************************************************************************************************
-- *                       (c) 2007 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Directories;
with Ada.IO_Exceptions;
with Ada_95.File;
with Ada_95.Lexical;
with Ada_95.Name;
with Ada_95.Reader;
with Ada_95.Token;
with Display;
with Log;
with Text;

package body Ada_95.Rename is

  procedure Check_Unit_Name (Element : Reader.Element) is

    use type Token.Lexical_Handle;
    use type Lexical.Element;
    use type File.Kind;

    Max_Units : constant := 32;

    Filename : constant String := Token.File_Begin (Element.Tokens.First.all).Name;

    File_Information : constant File.Information := File.Information_For (Filename);

    File_Unit_Name : File.Unit_Name renames File_Information.Id;

    The_File_Kind : File.Kind := File_Information.Extension;

    Syntax_Error : exception;
    End_Error    : exception;

    The_Token : Token.Lexical_Handle := Token.Lexical_After (Element.Tokens.First);

    The_Actual_Unit_Name : File.Unit_Name_Handle;
    The_Parent_Unit_Name : File.Unit_Name_Handle;


    procedure Get_Next_Token with Inline is
    begin
      if The_Token = null then
        raise End_Error;
      else
        The_Token := Token.Lexical_After (The_Token);
        if The_Token = null then
          raise End_Error;
        end if;
      end if;
    end Get_Next_Token;


    function Next_Token return Token.Lexical_Handle with Inline is
    begin
      Get_Next_Token;
      return The_Token;
    end Next_Token;


    function Next_If (The_Element : Lexical.Element) return Boolean with Inline is
      Current_Token : constant Token.Lexical_Handle := The_Token;
    begin
      Get_Next_Token;
      if The_Token.Element = The_Element then
        return True;
      else
        The_Token := Current_Token;
        return False;
      end if;
    end Next_If;


    procedure Find (The_Element : Lexical.Element) with Inline is
    begin
      while The_Token.Element /= The_Element loop
        Get_Next_Token;
      end loop;
    end Find;


    procedure Check (The_Element : Lexical.Element) with Inline is
    begin
      if The_Token.Element /= The_Element then
        raise Syntax_Error;
      end if;
    end Check;


    procedure Next_Check (The_Element : Lexical.Element) with Inline is
    begin
      Get_Next_Token;
      Check (The_Element);
    end Next_Check;


    procedure Check_Identifier with Inline is
    begin
      Check (Lexical.Identifier);
    end Check_Identifier;


    function Identifier return Name.Handle with Inline is
      The_Name : Name.Handle;
    begin
      Check_Identifier;
      The_Name := Token.Name_Handle(The_Token).Id;
      Get_Next_Token;
      return The_Name;
    end Identifier;


    function Next_Identifier return Name.Handle with Inline is
    begin
      Get_Next_Token;
      return Identifier;
    end Next_Identifier;


    function Unit_Name return File.Unit_Name_Handle with Inline is
      The_Name : File.Unit_Name (1..Max_Units);
      The_Last : Positive := The_Name'first;
    begin
      The_Name(The_Last) := Identifier;
      while The_Token.Element = Lexical.Period loop
        The_Last := The_Last + 1;
        The_Name(The_Last) := Next_Identifier;
      end loop;
      return new File.Unit_Name'(The_Name(The_Name'first..The_Last));
    end Unit_Name;


    function Next_Unit_Name return File.Unit_Name_Handle with Inline is
    begin
      Get_Next_Token;
      return Unit_Name;
    end Next_Unit_Name;


    procedure Rename_Unit (Name          : String;
                           New_Unit_Name : String) is

      New_Name_With_Extension : constant String := New_Unit_Name & File.Image_Of (The_File_Kind);
      New_Name                : constant String := File.Path_Of (Name) & "\" & New_Name_With_Extension;

    begin
      if Text.Matches (Name, New_Name) then
        Ada.Directories.Rename (Name, New_Name & '_');
        Ada.Directories.Rename (New_Name & '_', New_Name);
      else
        Ada.Directories.Rename (Name, New_Name);
      end if;
      Display.Rename (File.Name_And_Extension_Of (Name), New_Name_With_Extension);
    exception
    when Ada.IO_Exceptions.Use_Error =>
      Display.Error ("Rename failed for " & Name & " - new name " & New_Name & " already exists");
    when Occurrence: others =>
      Log.Write (Occurrence);
      Display.Error ("Rename failed from " & Name & " to " & New_Name);
    end Rename_Unit;


    procedure Check_Unit (The_Kind : File.Kind) with Inline is
    begin
      The_Actual_Unit_Name := Next_Unit_Name;
      if (Name.Image_Of (The_Actual_Unit_Name.all) /= Name.Image_Of (File_Unit_Name)) or
         (The_Kind /= The_File_Kind)
      then
        The_File_Kind := The_Kind;
        Rename_Unit (Filename, File.Unit_Name_Of(The_Actual_Unit_Name.all));
      end if;
    end Check_Unit;


    procedure Check_Unit_Implementation with Inline is
    begin
      Check_Unit (File.Implementation);
    end Check_Unit_Implementation;


    procedure Check_Unit_Specification with Inline is
    begin
      Check_Unit (File.Specification);
    end Check_Unit_Specification;


    procedure Get_Parent_Unit_Name with Inline is
    begin
      The_Parent_Unit_Name := Next_Unit_Name;
    end Get_Parent_Unit_Name;


    procedure Check_Subunit with Inline is
      Subunit_Name : constant String := File.Subunit_Name_Of (The_Parent_Unit_Name.all, Next_Unit_Name.all);
    begin
      if Subunit_Name /= File.Unit_Name_Of (File_Unit_Name) then
        Rename_Unit (Filename, Subunit_Name);
      end if;
    end Check_Subunit;


    procedure Check_For_Multiple_Unit is
      use type File.Unit_Name;
    begin
      The_Token := Token.Lexical_Before(Element.Tokens.Last);
      while The_Token /= null loop
        case The_Token.Element is
        when Lexical.Is_End =>
          if Next_Token.Element = Lexical.Identifier and then Unit_Name.all /= The_Actual_Unit_Name.all then
            Display.Error ("Multiple Units detected in " & Filename);
          end if;
          return;
        when Lexical.Is_Package | Lexical.Is_Procedure | Lexical.Is_Function | Lexical.Is_Generic =>
          Display.Error ("Multiple Units detected in " & Filename);
          return;
        when others =>
          null;
        end case;
        The_Token := Token.Lexical_Before (The_Token);
      end loop;
      Display.Error ("End not found in " & Filename);
    end Check_For_Multiple_Unit;

  begin -- Check_Unit_Name
    if The_Token = null then
      raise Syntax_Error;
    end if;
    if The_File_Kind = File.Unknown then
      Display.Error ("Unknown file type for " & Filename);
      return;
    end if;
    loop
      case The_Token.Element is
      when Lexical.Is_Function | Lexical.Is_Procedure =>
        Check_Unit (The_File_Kind);
        return;
      when Lexical.Is_Package =>
        if Next_If (Lexical.Is_Body) then
          Check_Unit_Implementation;
        else
          Check_Unit (The_File_Kind);
          if The_Token.Element = Lexical.Is_Is and then not Next_If (Lexical.Is_New) then
            Check_For_Multiple_Unit;
          end if;
        end if;
        return;
      when Lexical.Is_Private =>
        Get_Next_Token;
      when Lexical.Is_Generic =>
        loop
          case Next_Token.Element is
          when Lexical.Identifier | Lexical.Is_Type | Lexical.Is_With | Lexical.Is_Use | Lexical.Is_Pragma =>
            -- skip generic_formal_parameter_declaration or use_clause or pragma
            Find (Lexical.Semicolon);
          when Lexical.Is_Function | Lexical.Is_Procedure | Lexical.Is_Package =>
            Check_Unit_Specification;
            begin
              Find (Lexical.Is_With);
              if Next_Token.Element = Lexical.Identifier then
                Display.Error ("Multiple Units detected in " & Filename);
              end if;
            exception
            when others =>
              null;
            end;
            return;
          when others =>
            raise Syntax_Error;
          end case;
        end loop;
      when Lexical.Is_Separate =>
        Next_Check (Lexical.Left_Parenthesis);
        Get_Parent_Unit_Name;
        Check (Lexical.Right_Parenthesis);
        case Next_Token.Element is
        when Lexical.Is_Function | Lexical.Is_Procedure =>
          Check_Subunit;
          return;
        when Lexical.Is_Package | Lexical.Is_Task | Lexical.Is_Protected =>
          Next_Check (Lexical.Is_Body);
          Check_Subunit;
          return;
        when others =>
          raise Syntax_Error;
        end case;
      when Lexical.Is_With | Lexical.Is_Use | Lexical.Is_Pragma =>
        -- skip context_clause or pragma
        Find (Lexical.Semicolon);
        Get_Next_Token;
      when others =>
        raise Syntax_Error;
      end case;
    end loop;
  exception
  when End_Error =>
    Display.Error ("Incomplete Ada Source in " & Filename);
  when Syntax_Error =>
    Display.Error ("Incorrect Ada Source in " & Filename);
  when Item : others =>
    Log.Write (Filename, Item);
    Display.Error ("Internal error by processing " & Filename);
  end Check_Unit_Name;


  procedure Start is
  begin
    Reader.Start;
  end Start;


  procedure Stop is
  begin
    Reader.Finalize;
    Log.Write ("Renaming Stopped");
  end Stop;


  procedure Add (Filename : String) is
  begin
    Log.Write ("Rename.Add " & Filename);
    Reader.Add (Filename);
  end Add;


  procedure Finalize is

    procedure Check_All is new Reader.Iterator (Check_Unit_Name);

  begin
    Check_All;
    Reader.Finalize;
    Display.Show_Result;
    Log.Write ("Renaming Complete");
  end Finalize;

end Ada_95.Rename;
