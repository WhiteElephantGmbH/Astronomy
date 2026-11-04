-- *********************************************************************************************************************
-- *                   (c) 2008 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada_95.File;
with Ada_95.Source.File;
with Ada_95.Source.Buffer;
with Ada_95.Token.Data;
with Container.List;
with Log;

package body Ada_95.Project is

  procedure Initialize (Work_Path : Text.List) is
  begin
    File.Define_Work_Path (Work_Path);
    Library.Start;
  end Initialize;


  procedure Finalize renames Library.Finalize;


  function Unit_Of (Filename : String;
                    Content  : String) return Library.Handle is
  begin
    if Content = "" then
      return Library.Added (Source.File.New_With (Filename));
    else
      Source.Buffer.Initialize (Content);
      return Library.Added (Source.Buffer.New_With (Filename));
    end if;
  end Unit_Of;


  function Resource (Unit : Library.Handle) return Token.Data.Resource_Handle with Inline is
  begin
    return Token.Data.Resource(Unit.all);
  end Resource;


  function Token_Handle_Of (Filename : String;
                            Content  : String := "") return Token.Handle is

    Unit : constant Library.Handle := Unit_Of (Filename, Content);

    use type Library.Handle;

  begin
    if Unit /= null then
      return Resource(Unit).Tokens.First;
    end if;
    return null;
  end Token_Handle_Of;


  function Token_Handle_Of (Filename : String;
                            Line     : Token.Line_Number;
                            Column   : Token.Column_Position;
                            Content  : String) return Token.Handle is

    Unit : constant Library.Handle := Unit_Of (Filename, Content);

    use type Library.Handle;

  begin
    if Unit /= null then
      return Token.Handle_Of (Item   => Token.Data.Resource(Unit.all).Tokens,
                              Line   => Line,
                              Column => Column);
    end if;
    return null;
  end Token_Handle_Of;


  The_Local_Unit : Library.Handle;

  function Full_Name_Of (Item : Token.Handle) return String is
    use type Token.Handle;
  begin
    The_Local_Unit := null;
    if Item /= null and then Item.all in Token.Identifier'class then
      declare
        Id         : constant Token.Identifier_Handle := Token.Identifier_Handle(Item);
        The_Parent : Library.Handle;
        use type Token.Data_Handle;
        use type Library.Handle;
      begin
        if Id.Data /= null then
          if Id.Data.all in Token.Data.Unit_Body'class and then Token.Data.Is_Library (Library.Handle(Id.Data)) then
            The_Local_Unit := Library.Handle(Id.Data);
          elsif Id.Data.all in Token.Data.Declaration_Type'class then
            The_Parent := Token.Data.Declaration_Handle(Id.Data).Parent;
            while The_Parent /= null loop
              if The_Parent.all in Token.Data.Unit_Body'class and then Token.Data.Is_Library (The_Parent) then
                The_Local_Unit := The_Parent;
                exit;
              end if;
              The_Parent := The_Parent.Parent;
            end loop;
          end if;
          return Token.Data.Full_Name_Of (Id.Data);
        end if;
      end;
    end if;
    return "";
  end Full_Name_Of;


  function Local_Unit return Library.Handle is
  begin
    return The_Local_Unit;
  end Local_Unit;


  function Error_Token_Of (Unit : Library.Handle) return Token.Handle is
  begin
    return Token.Handle(Token.Data.Resource(Unit.all).Tokens.Error_Token);
  end Error_Token_Of;


  function Reference_Of (Filename : String;
                         Line     : Token.Line_Number;
                         Column   : Token.Column_Position;
                         Content  : String := "") return Token.Handle is

    Unit : constant Library.Handle := Unit_Of (Filename, Content);

    The_Body      : Library.Handle;
    The_Token     : Token.Handle;
    The_Reference : Token.Handle;

    use type Token.Data.Unit_Handle;
    use type Token.Handle;
    use type Token.Column_Position;

  begin
    if Unit /= null then
      The_Token := Token.Handle_Of (Item   => Token.Data.Resource(Unit.all).Tokens,
                                    Line   => Line,
                                    Column => Column);
      if not (The_Token.all in Token.Identifier'class) and then (Column > Token.Column_Position'first) then
        The_Token := Token.Handle_Of (Item   => Token.Data.Resource(Unit.all).Tokens,
                                      Line   => Line,
                                      Column => Column - 1);
      end if;
      The_Reference := Token.Reference_Of (The_Token);
      if The_Reference /= null then
        if The_Reference = The_Token then
          if Token.Data.Is_Library (Unit) and Unit.all in Token.Data.Unit_Declaration'class then
            The_Body := Library.Body_Of (Unit);
            if The_Body /= null then
              The_Reference := Token.Data.Resource(The_Body.all).Tokens.First;
              while The_Reference /= null loop
                if The_Reference.all in Token.Identifier'class then
                  if Token.Reference_Of (The_Reference) = The_Token then
                    case Token.Data_Kind_Of (Token.Identifier_Handle(The_Reference).Data.all) is
                    when Is_Package_Body
                       | Is_Package_Renaming
                       | Is_Generic_Package_Renaming
                       | Is_Protected_Body
                       | Is_Subprogram_Body
                       | Is_Subprogram_Renaming
                       | Is_Generic_Subprogram_Renaming
                       | Is_Task_Body
                       | Is_Incomplete_Type
                    =>
                      exit;
                    when others =>
                      null;
                    end case;
                  end if;
                end if;
                The_Reference := The_Reference.Next;
              end loop;
              if The_Reference = null then
                The_Reference := Error_Token_Of (The_Body);
                if The_Reference = null then
                  The_Reference := The_Token;
                end if;
              end if;
            end if;
          end if;
        end if;
      end if;
    end if;
    return The_Reference;
  end Reference_Of;


  function Token_Kind_Of (Item : Token.Handle) return Token_Kind is
    use type Token.Handle;
    use all type Token.Kind;
    use all type Token_Kind;
  begin
    case Token.Kind_Of (Item.all) is
    when Is_Attribute | Is_Aspect =>
      return Is_Attribute;
    when Is_Pragma_Identifier =>
      if not Token.Pragma_Identifier_Handle(Item).Is_Used then
        return Is_Unused_Declaration;
      end if;
      return Is_Attribute;
    when Is_Comment =>
      return Is_Comment;
    when Is_Special_Comment =>
      if Item.all in Token.Special_Comment'class then
        if not Token.Special_Comment_Handle(Item).Is_Used then
          return Is_Unused_Declaration;
        end if;
      end if;
      return Is_Special_Comment;
    when Is_Character_Literal =>
      case Token.Data.Kind_Of (Item) is
      when Token.Data.Is_Known =>
        return Is_Character_Literal;
      when others =>
        return Is_Unknown_Identifier;
      end case;
    when Is_Numeric_Literal =>
      return Is_Numeric_Literal;
    when Is_Reserved_Word =>
      return Is_Reserved_Word;
    when Is_String_Literal =>
      return Is_String_Literal;
    when Is_Identifier =>
      case Token.Data.Kind_Of (Item) is
      when Token.Data.Is_Type =>
        return Is_Type;
      when Token.Data.Is_Unused_Declaration =>
        return Is_Unused_Declaration;
      when Token.Data.Is_Unused_Type_Declaration =>
        return Is_Unused_Type_Declaration;
      when Token.Data.Is_Known =>
        return Is_Others;
      when Token.Data.Is_Unknown =>
        return Is_Unknown_Identifier;
      end case;
    when Is_Error =>
      if Token.Error_Handle(Item).Message in Error.Incorrect_Style then
        return Is_Style_Error;
      elsif Token.Error_Handle(Item).Message in Error.In_Semantic then
        return Is_Semantic_Error;
      else
        return Is_Syntax_Error;
      end if;
    when others =>
      return Is_Others;
    end case;
  end Token_Kind_Of;


  use type Token.Handle;
  package Error_List is new Container.List (Element => Token.Handle);

  The_Error_List : Error_List.Item;


  procedure Append_Error_For (Unit : Library.Handle) is
    Handle : constant Token.Handle := Error_Token_Of (Unit);
  begin
    if Handle /= null then
      The_Error_List.Append (Handle);
    end if;
  end Append_Error_For;


  The_Error_Cursor : Error_List.Cursor;

  function First_Error_Of (Filename : String;
                           Content  : String := "") return Token.Handle is

    Unit : constant Library.Handle := Unit_Of (Filename, Content);

    use type Library.Handle;

    procedure Append_Errors (Element : Library.Element_Cursor) is

      procedure Handle_Module (Library_Module : Library.Handle) is
      begin
        if Library_Module /= null and then Library_Module /= Unit then
          Append_Error_For (Library_Module);
        end if;
      end Handle_Module;

    begin
      if Element.Unit /= null then
        Handle_Module (Element.Unit);
      end if;
    end Append_Errors;

    procedure Append_Other_Errors is new Library.Iterator (Append_Errors);

  begin
    The_Error_Cursor := Error_List.No_Element;
    Error_List.Clear (The_Error_List);
    if Unit /= null then
      Append_Error_For (Unit);
      Append_Other_Errors;
      if not Error_List.Is_Empty (The_Error_List) then
        The_Error_Cursor := The_Error_List.First;
        return Error_List.Element_At (The_Error_Cursor);
      end if;
    end if;
    return null;
  exception
  when Id: others =>
    Log.Write ("First_Error_Of", Id);
    return null;
  end First_Error_Of;


  function Next_Error return Token.Handle is
    use type Error_List.Cursor;
  begin
    Error_List.Next (The_Error_Cursor);
    if The_Error_Cursor /= Error_List.No_Element then
      return Error_List.Element_At (The_Error_Cursor);
    end if;
    return null;
  end Next_Error;


  function Error_Kind_Of (Item : Token.Handle) return Error_Kind is
  begin
    return Error_Kind(Token.Error_Handle(Item).Message);
  end Error_Kind_Of;

end Ada_95.Project;
