-- *********************************************************************************************************************
-- *                       (c) 2007 .. 2022 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada_95.Library;
with Ada_95.File;
with Log;

package body Ada_95.Token.Pre_Parser is

  function Process (Resource : Data.Resource_Handle) return Data.Unit_Handle is

    Start_Token : constant Handle := Resource.Tokens.First;

    Filename : constant String := Token.File_Begin (Start_Token.all).Name;

    File_Information : constant File.Information := File.Information_For (Filename);

    File_Unit_Name : File.Unit_Name renames File_Information.Id;
    File_Kind      : File.Kind      renames File_Information.Extension;

    Reported_Error : exception;

    The_Library_Unit : Data.Unit_Handle;

    The_Token : Lexical_Handle;


    procedure Report_Error (Item     : Error.Kind;
                            At_Token : Lexical_Handle) is
    begin
      Token.Mark_Error (Item, At_Token, Resource.Tokens);
      raise Reported_Error;
    end Report_Error;


    procedure Syntax_Error is
    begin
      Report_Error (Error.Syntax_Error, The_Token);
    end Syntax_Error;


    procedure Not_Implemented (Message : String) is
    begin
      Log.Write ("!!! PREPARSER: " & Message);
      Report_Error (Error.Not_Implemented, The_Token);
    end Not_Implemented;


    procedure End_Of_File_Error is
    begin
      Report_Error (Error.End_Of_File_Error, null);
    end End_Of_File_Error;


    function No_Parent (Name : File.Unit_Name) return Boolean is
      The_Unit : Library.Handle;
      use type Library.Handle;
    begin
      The_Unit := Library.Unit_Of (Name);
      return The_Unit = null or else Is_Null (The_Unit.Location);
    end No_Parent;


    procedure Check_Unit (Item     : Identifier_List;
                          The_Kind : File.Kind) is
      use type Name.Handle;
      use type File.Kind;
    begin
      if The_Kind /= File_Kind then
        Report_Error (Error.Unit_Kind_Error, Lexical_Handle(Item(Item'last)));
      else
        for Index in File_Unit_Name'range loop
          if Item'last < Index then
            Report_Error (Error.Unit_Name_Error, The_Token);
          elsif Item(Index).Id /= File_Unit_Name(Index) then
            Report_Error (Error.Unit_Name_Error, Lexical_Handle(Item(Index)));
          elsif Index < File_Unit_Name'last then
            if No_Parent (File_Unit_Name(File_Unit_Name'first .. Index)) then
              Report_Error (Error.Parent_Unit_Name_Error, Lexical_Handle(Item(Index)));
            end if;
          end if;
        end loop;
        if File_Unit_Name'last < Item'last then
          Report_Error (Error.Unit_Name_Error, Lexical_Handle(Item(File_Unit_Name'last + 1)));
        end if;
      end if;
    end Check_Unit;


    procedure Check_Unit_Implementation (Id : Identifier_List) with Inline is
    begin
      Check_Unit (Id, File.Implementation);
    end Check_Unit_Implementation;


    procedure Check_Unit_Specification (Id : Identifier_List) with Inline is
    begin
      Check_Unit (Id, File.Specification);
    end Check_Unit_Specification;


    procedure Check_No_End_Of_File with Inline is
    begin
      if The_Token = null then
        End_Of_File_Error;
        Report_Error (Error.End_Of_File_Error, null);
      end if;
    end Check_No_End_Of_File;


    procedure Get_Next_Token with Inline is
    begin
      Check_No_End_Of_File;
      The_Token := Lexical_After (The_Token);
    end Get_Next_Token;


    function Next_Token return Lexical_Handle with Inline is
    begin
      Get_Next_Token;
      return The_Token;
    end Next_Token;


    procedure Check (Element : Lexical.Element) with Inline is
      use type Lexical.Element;
    begin
      Check_No_End_Of_File;
      if The_Token.Element /= Element then
        Syntax_Error;
      end if;
    end Check;


    procedure Get_Element (Item : Lexical.Element) with Inline is
    begin
      Check (Item);
      Get_Next_Token;
    end Get_Element;


    procedure Get_Next_Element (Item : Lexical.Element) with Inline is
    begin
      Get_Next_Token;
      Get_Element (Item);
    end Get_Next_Element;


    function Element_Is (Element : Lexical.Element) return Boolean with Inline is
     use type Lexical.Element;
    begin
      if The_Token.Element = Element then
        Get_Next_Token;
        return True;
      else
        return False;
      end if;
    end Element_Is;


    function Next_Element_Is (Element : Lexical.Element) return Boolean with Inline is
    begin
      Get_Next_Token;
      return Element_Is (Element);
    end Next_Element_Is;


    function Actual_Identifier return Identifier_Handle with Inline is
      The_Identifier : Identifier_Handle;
      use type Lexical.Element;
    begin
      if The_Token.Element /= Lexical.Identifier then
        Syntax_Error;
      end if;
      The_Identifier := Identifier_Handle(The_Token);
      Get_Next_Token;
      return The_Identifier;
    end Actual_Identifier;


    function Unit_Name return Identifiers is
      Unit_Token : constant Lexical_Handle := The_Token;
      The_Length : Natural := 1;
    begin
      loop
        Get_Next_Token;
        exit when not Element_Is (Lexical.Period);
        The_Length := The_Length + 1;
      end loop;
      declare
        The_List  : Identifiers(1..The_Length);
        The_Index : Positive := The_List'first;
      begin
        The_Token := Unit_Token;
        loop
          The_List(The_Index) := Actual_Identifier;
          exit when The_Index = The_List'last;
          Get_Next_Token;
          The_Index := Positive'succ(The_Index);
        end loop;
        return The_List;
      end;
    end Unit_Name;


    function Next_Unit_Name return Identifiers with Inline is
    begin
      Get_Next_Token;
      return Unit_Name;
    end Next_Unit_Name;


    procedure Find (Element : Lexical.Element) with Inline is
      use type Lexical.Element;
    begin
      while The_Token.Element /= Element loop
        Get_Next_Token;
      end loop;
    end Find;


    procedure Pragma_Call with Inline is
      use type Lexical.Element;
    begin
      Get_Next_Token;
      case Next_Token.Element is
      when Lexical.Left_Parenthesis =>
        Find (Lexical.Semicolon);
      when Lexical.Semicolon =>
        null;
      when others =>
        Syntax_Error;
      end case;
    end Pragma_Call;


    procedure Get_Unit_Names is

      procedure Get_Name is
      begin
        loop
          Get_Element (Lexical.Identifier);
          exit when not Element_Is (Lexical.Period);
        end loop;
      end Get_Name;

    begin -- Get_Unit_Names
      Get_Name;
      while Element_Is (Lexical.Comma) loop
        Get_Name;
      end loop;
      Check (Lexical.Semicolon);
    end Get_Unit_Names;


    procedure Generic_Package is
      Id : constant Identifier_List := new Identifiers'(Next_Unit_Name);
    begin
      case The_Token.Element is
      when Lexical.Is_Is | Lexical.Is_With =>
        Check_Unit_Specification (Id);
        The_Library_Unit := Data.New_Generic_Library_Package_Declaration (Id, Resource);
      when Lexical.Is_Renames =>
        Check_Unit_Specification (Id);
        The_Library_Unit := Data.New_Generic_Library_Package_Renaming (Id, Resource);
      when others =>
        Syntax_Error;
      end case;
    end Generic_Package;


    procedure Generic_Subprogram is
      Id : constant Identifier_List := new Identifiers'(Next_Unit_Name);
    begin
      case The_Token.Element is
      when Lexical.Is_Renames =>
        Check_Unit_Specification (Id);
        The_Library_Unit := Data.New_Generic_Library_Subprogram_Renaming (Id, Resource);
      when others =>
        Check_Unit_Specification (Id);
        The_Library_Unit := Data.New_Generic_Library_Subprogram_Declaration (Id, Resource);
      end case;
    end Generic_Subprogram;


    procedure Library_Generic is
    begin
      loop
        case Next_Token.Element is
        when Lexical.Identifier | Lexical.Is_Type | Lexical.Is_With | Lexical.Is_Use | Lexical.Is_Pragma =>
          -- skip generic_formal_parameter_declaration or use_clause or pragma
          Find (Lexical.Semicolon);
        when Lexical.Is_Function
           | Lexical.Is_Procedure
        =>
          Generic_Subprogram;
          exit;
        when Lexical.Is_Package =>
          Generic_Package;
          exit;
        when others =>
          Syntax_Error;
        end case;
      end loop;
    end Library_Generic;


    procedure Library_Subprogram with Inline is

      Id : constant Identifier_List := new Identifiers'(Next_Unit_Name);

      The_Count : Natural := 0;

    begin
      loop
        case The_Token.Element is
        when Lexical.Left_Parenthesis =>
          The_Count := The_Count + 1;
        when Lexical.Right_Parenthesis =>
          if The_Count = 0 then
            Syntax_Error;
          end if;
          The_Count := The_Count - 1;
        when Lexical.Is_Is =>
          if The_Count /= 0 then
            Syntax_Error;
          end if;
          Get_Next_Token;
          if Element_Is (Lexical.Is_New) then
            Check_Unit_Specification (Id);
            The_Library_Unit := Data.New_Library_Subprogram_Instantiation (Id, Resource);
          else
            Check_Unit_Implementation (Id);
            The_Library_Unit := Data.New_Library_Subprogram_Body (Id, Resource);
          end if;
          exit;
        when Lexical.Semicolon =>
          if The_Count = 0 then
            Check_Unit_Specification (Id);
            The_Library_Unit := Data.New_Library_Subprogram_Declaration (Id, Resource);
            exit;
          end if;
        when Lexical.Is_Renames =>
          if The_Count = 0 then
            Check_Unit_Specification (Id);
            The_Library_Unit := Data.New_Library_Subprogram_Renaming (Id, Resource);
            exit;
          end if;
        when others =>
          null;
        end case;
        Get_Next_Token;
      end loop;
    end Library_Subprogram;


    procedure Library_Package (Is_Private : Boolean := False) with Inline is

      The_Id : Identifier_List;

    begin
      if Next_Element_Is (Lexical.Is_Body) then
        The_Id := new Identifiers'(Unit_Name);
        Check_Unit_Implementation (The_Id);
        The_Library_Unit := Data.New_Library_Package_Body (The_Id, Resource);
      else
        The_Id := new Identifiers'(Unit_Name);
        case The_Token.Element is
        when Lexical.Is_Is =>
          if Next_Element_Is (Lexical.Is_New) then
            The_Library_Unit := Data.New_Library_Package_Instantiation (The_Id, Resource);
          else
            Check_Unit_Specification (The_Id);
            The_Library_Unit := Data.New_Library_Package_Specification (The_Id, Resource, Is_Private);
          end if;
        when Lexical.Is_With =>
          Check_Unit_Specification (The_Id);
          The_Library_Unit := Data.New_Library_Package_Specification (The_Id, Resource, Is_Private);
        when Lexical.Is_Renames =>
          Check_Unit_Specification (The_Id);
          The_Library_Unit := Data.New_Library_Package_Renaming (The_Id, Resource);
        when others =>
          Syntax_Error;
        end case;
      end if;
    end Library_Package;


    procedure Subunit is
      The_Id : Identifier_List;
    begin
      Get_Next_Element (Lexical.Left_Parenthesis);
      declare
        Id : constant Identifiers := Unit_Name;
      begin
        Get_Element (Lexical.Right_Parenthesis);
        case The_Token.Element is
        when Lexical.Is_Procedure
           | Lexical.Is_Function
        =>
          Get_Next_Token;
          The_Id := new Identifiers'(Id & Actual_Identifier);
          Check_Unit_Implementation (The_Id);
          The_Library_Unit := Data.New_Subprogram_Subunit (The_Id, Resource);
        when Lexical.Is_Package =>
          Get_Next_Element (Lexical.Is_Body);
          The_Id := new Identifiers'(Id & Actual_Identifier);
          Check_Unit_Implementation (The_Id);
          The_Library_Unit := Data.New_Package_Subunit (The_Id, Resource);
        when Lexical.Is_Task =>
          Not_Implemented ("Task Subunit");
        when others =>
          Syntax_Error;
        end case;
      end;
    end Subunit;

  begin -- Process
    The_Token := Lexical_After (Start_Token);
    while The_Token /= null loop
      case The_Token.Element is
      when Lexical.Is_Limited =>
        case Next_Token.Element is
        when Lexical.Is_Private =>
          Get_Next_Element (Lexical.Is_With);
          Find (Lexical.Semicolon);
        when Lexical.Is_With =>
          Find (Lexical.Semicolon);
        when others =>
          Syntax_Error;
        end case;
      when Lexical.Is_Private =>
        case Next_Token.Element is
        when Lexical.Is_Procedure
           | Lexical.Is_Function
        =>
          Library_Subprogram;
          exit;
        when Lexical.Is_Package =>
          Library_Package (Is_Private => True);
          exit;
        when Lexical.Is_Generic =>
          Library_Generic;
          exit;
        when Lexical.Is_With =>
          Find (Lexical.Semicolon);
        when others =>
          Syntax_Error;
        end case;
      when Lexical.Is_Procedure
         | Lexical.Is_Function
      =>
        Library_Subprogram;
        exit;
      when Lexical.Is_Package =>
        Library_Package;
        exit;
      when Lexical.Is_Generic =>
        Library_Generic;
        exit;
      when Lexical.Is_Separate =>
        Subunit;
        exit;
      when Lexical.Is_Pragma =>
        Pragma_Call;
      when Lexical.Is_Use =>
        Get_Next_Token;
        if Element_Is (Lexical.Is_Type) then
          Find (Lexical.Semicolon);
        else
          Get_Unit_Names;
        end if;
      when Lexical.Is_With =>
        Get_Next_Token;
        Get_Unit_Names;
      when others =>
        Syntax_Error;
      end case;
      Get_Next_Token;
    end loop;
    return The_Library_Unit;
  exception
  when Reported_Error =>
    return null;
  when Occurrence: others =>
    Log.Write ("!!! PRE-PARSER EXCEPTION", Occurrence);
    return null;
  end Process;

end Ada_95.Token.Pre_Parser;
