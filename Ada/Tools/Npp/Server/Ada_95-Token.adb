-- *********************************************************************************************************************
-- *                       (c) 2007 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Characters.Handling;
with Ada.Unchecked_Conversion;
with Ada_95.Library;
with Ada_95.Symbol;
with Ada_95.Token.Data;
with Log;
with System;

package body Ada_95.Token is

  ----------------------------------------------------------------------------------------------------------------------
  -- Logging
  ----------------------------------------------------------------------------------------------------------------------

  Blanks : constant String := "                                                ";

  Log_Indent : constant := 2;

  The_Log_Indent : Natural := 0;

  procedure Increment_Log_Indent is
  begin
    The_Log_Indent := The_Log_Indent + Log_Indent;
  end Increment_Log_Indent;

  procedure Decrement_Log_Indent is
  begin
    The_Log_Indent := The_Log_Indent - Log_Indent;
  end Decrement_Log_Indent;

  procedure Write_Log (Message : String) is
  begin
    Log.Write (Blanks(1..The_Log_Indent) & Message);
  exception
  when others =>
    Log.Write (">>" & Blanks & Message);
  end Write_Log;


  ----------------------------------------------------------------------------------------------------------------------
  -- Types
  ----------------------------------------------------------------------------------------------------------------------

  use type Column_Position;

  function "<" (Left, Right : Handle) return Boolean is
    pragma Unreferenced (Left, Right);
  begin
    return False; -- not used for this type
  end "<";


  function Is_Null (Item : Identifier_Handle) return Boolean is
    function System_Address is new Ada.Unchecked_Conversion (Identifier_Handle, System.Address);
    use type System.Address;
  begin
    return System_Address(Item) = System.Null_Address;
  end Is_Null;


  function "=" (Left, Right : Identifier_Handle) return Boolean is
    use type Lexical.Name_Key;
  begin
    return Left.Id.Key = Right.Id.Key;
  end "=";


  function "<" (Left, Right : Identifier_Handle) return Boolean is
    use type Lexical.Name_Key;
  begin
    return Left.Id.Key < Right.Id.Key;
  end "<";


  function Comparison (Left, Right : Identifiers) return Result is
    use type Lexical.Name_Key;
  begin
    for The_Index in Left'range loop
      if Right'length < The_Index then
        return After;
      elsif Left(The_Index).Id.Key < Right(The_Index).Id.Key then
        return Before;
      elsif Left(The_Index).Id.Key /= Right(The_Index).Id.Key then
        return After;
      end if;
    end loop;
    if Right'length > Left'length then
      return Before;
    else
      return Equal;
    end if;
  end Comparison;


  function "=" (Left, Right : Identifiers) return Boolean is
    use type Lexical.Name_Key;
  begin
    if Left'length = Right'length then
      for The_Index in Left'range loop
        if Left(The_Index).Id.Key /= Right(The_Index).Id.Key then
          return False;
        end if;
      end loop;
      return True;
    end if;
    return False;
  end "=";


  function "<" (Left, Right : Identifiers) return Boolean is
  begin
    raise Program_Error; -- not implemented
    return False;
  end "<";


  function Comparison (Left, Right : Identifier_List) return Result is
  begin
    return Comparison (Left.all, Right.all);
  end Comparison;


  function "=" (Left, Right : Identifier_List) return Boolean is
  begin
    return Left.all = Right.all;
  end "=";


  function "<" (Left, Right : Identifier_List) return Boolean is
  begin
    return Left.all < Right.all;
  end "<";


  ----------------------------------------------------------------------------------------------------------------------
  -- Selectors
  ----------------------------------------------------------------------------------------------------------------------

  Special_Comments_Allowed : Boolean;

  function Handle_Of (Item : Sequence;
                      Line : Line_Number) return Handle is
    The_Count : Counter := Line_Count_Of (Item);
    The_Line  : Line_Handle := Item.Last_Line;
    use type Counter;
  begin
    if Line = Line_Number'first then
      return Item.First;
    end if;
    loop
      if The_Line = null then
        return Item.First;
      end if;
      The_Count := The_Count - The_Line.Count;
      exit when The_Count < Line;
      The_Line := The_Line.Previous_Line;
    end loop;
    if The_Line.Previous_Line = null then
      return Item.First;
    else
      return The_Line.Previous_Line.Next;
    end if;
  end Handle_Of;


  function Handle_Of (Item   : Sequence;
                      Line   : Line_Number;
                      Column : Column_Position) return Handle is
    The_Handle : Handle := Handle_Of (Item, Line);
  begin
    while The_Handle /= null loop
      if The_Handle.all in Lexical_Object'class then
        if Lexical_Handle(The_Handle).Column > Column then
          return The_Handle.Previous;
        end if;
      end if;
      The_Handle := The_Handle.Next;
      if The_Handle /= null and then The_Handle.all in Line_Object'class then
        return The_Handle.Previous;
      end if;
    end loop;
    return null;
  end Handle_Of;


  function Is_Declaration (Item : Handle) return Boolean is
    The_Data : Data_Handle;
  begin
    if Item /= null and then Item.all in Identifier'class then
      The_Data := Identifier_Handle(Item).Data;
      return The_Data /= null and then Handle(The_Data.Location) = Item;
    end if;
    return False;
  end Is_Declaration;


  function Declaration_Of (Item : Handle) return Handle is
    The_Data : Data_Handle;
  begin
    if Item /= null and then Item.all in Identifier'class then
      The_Data := Identifier_Handle(Item).Data;
      if The_Data /= null then
        if Handle(The_Data.Location) /= Item then
          if not (The_Data.all in Data.End_Identifier'class) then
            return Handle(The_Data.Location);
          end if;
        else
          if The_Data.all in Data.With_Declaration'class then
            return Handle(Data.With_Handle(The_Data).Unit.Location);
          end if;
        end if;
      end if;
    end if;
    return null;
  end Declaration_Of;


  function Is_Used (Item : Handle) return Boolean is
    The_Data : Data_Handle;
  begin
    if Item /= null and then Item.all in Identifier'class then
      The_Data := Identifier_Handle(Item).Data;
      if The_Data /= null and then The_Data.all in Data.Declaration_Type'class then
        return Data.Declaration_Handle(The_Data).Is_Used;
      end if;
    end if;
    return False;
  end Is_Used;


  function Is_Unused_Declaration (Item : Handle) return Boolean is
  begin
    if Item /= null and then Item.all in Identifier'class then
      return Data.Is_Unused (Item);
    end if;
    return False;
  end Is_Unused_Declaration;


  function Reference_Of (Item : Handle) return Handle is
    The_Data : Data_Handle;
  begin
    if Item /= null and then Item.all in Identifier'class then
      The_Data := Identifier_Handle(Item).Data;
      if The_Data /= null then
        if Handle(The_Data.Location) = Item then
          if The_Data.all in Data.Unit_Body'class then
            if The_Data.all in Data.Separate_Package_Body'class | Data.Separate_Subprogram_Body'class then
              declare
                Separate_Unit_Name : constant String := Data.Full_Name_Of (The_Data);
                Separate_Unit      : constant Data.Unit_Handle := Library.Unit_Of (Name.Id_Of (Separate_Unit_Name));
                use type Data.Unit_Handle;
              begin
                if Separate_Unit /= null then
                  return Handle(Separate_Unit.Location);
                end if;
              end;
            else
              declare
                Specification : constant Data.Unit_Declaration_Handle := Data.Unit_Body_Handle(The_Data).Specification;
                use type Data.Unit_Declaration_Handle;
              begin
                if Specification /= null then
                  return Handle(Specification.Location);
                end if;
              end;
            end if;
          elsif The_Data.all in Data.Unit_Declaration'class then
            declare
              Implementation : constant Data.Unit_Body_Handle := Data.Unit_Declaration_Handle(The_Data).Implementation;
              use type Data.Unit_Body_Handle;
            begin
              if Implementation /= null then
                return Handle(Implementation.Location);
              end if;
            end;
          elsif The_Data.all in Data.Incomplete_Object'class then
            declare
              Complete_Data : constant Data.Complete_Object_Handle
                := Data.Incomplete_Object_Handle(The_Data).Complete_Data;
              use type Data.Complete_Object_Handle;
            begin
              if Complete_Data /= null then
                return Handle(Complete_Data.Location);
              end if;
            end;
          elsif The_Data.all in Data.Complete_Object'class then
            declare
              Incomplete_Data : constant Data.Incomplete_Object_Handle
                := Data.Complete_Object_Handle(The_Data).Incomplete_Data;
              use type Data.Incomplete_Object_Handle;
            begin
              if Incomplete_Data /= null then
                return Handle(Incomplete_Data.Location);
              end if;
            end;
          end if;
        end if;
        if The_Data.all in Data.With_Declaration'class then
          return Handle(Data.With_Handle(The_Data).Unit.Location);
        end if;
        return Handle(The_Data.Location);
      end if;
    end if;
    return null;
  end Reference_Of;


  function Line_Count_Of (Item : Sequence) return Counter is
    The_Line  : Line_Handle := Item.Last_Line;
    The_Count : Counter := 0;
    use type Counter;
  begin
    while The_Line /= null loop
      The_Count := The_Count + The_Line.Count;
      The_Line := The_Line.Previous_Line;
    end loop;
    return The_Count;
  end Line_Count_Of;


  function Filename_Of (Item : Handle) return String is
    The_Handle      : Handle := Item;
    The_Line_Handle : Line_Handle;
  begin
    if The_Handle.all in Line_Object'class then
      The_Line_Handle := Line_Handle(The_Handle);
    else
      loop
        if The_Handle.all in File_Begin then
          return File_Begin(The_Handle.all).Name;
        end if;
        exit when The_Handle.all in Line_Object'class;
        The_Handle := The_Handle.Previous;
      end loop;
      The_Line_Handle := Line_Handle(The_Handle);
    end if;
    while The_Line_Handle.Previous_Line /= null loop
      The_Line_Handle := The_Line_Handle.Previous_Line;
    end loop;
    The_Handle := Handle(The_Line_Handle);
    loop
      if The_Handle.all in File_Begin then
        return File_Begin(The_Handle.all).Name;
      end if;
      The_Handle := The_Handle.Previous;
    end loop;
  end Filename_Of;


  function Line_Image_Of (Item : Handle) return String is

    function "+" (Left  : String;
                  Right : Handle) return String is

      Spaces : constant String(1 .. Natural(Area_Of(Right.all).First_Column) - Left'length - 1) := [others => ' '];

      Image : constant String := Left & Spaces & Image_Of (Right.all);

    begin -- "+"
      if Right /= null then
        if Right.all in Line_End_Comment'class then
          return Image;
        elsif (Right.Next.all in Lexical_Object'class) or else (Right.Next.all in Line_End_Comment'class)  then
          return Image + Right.Next;
        end if;
      end if;
      return Image;
    end "+";

    The_Handle : Handle := Item;

  begin -- Line_Image_Of
    if The_Handle = null then
      return "";
    elsif not (The_Handle.all in Special_Comment) then
      loop
        exit when not (The_Handle.Previous.all in Lexical_Object'class);
        The_Handle := The_Handle.Previous;
      end loop;
    end if;
    return "" + The_Handle;
  end Line_Image_Of;


  function Line_Number_Of (Item : Handle) return Line_Number is
    The_Handle      : Handle := Item;
    The_Line_Handle : Line_Handle;
    The_Line_Number : Line_Number := Line_Number'first;
    use type Counter;
  begin
    if The_Handle.all in Line_Object'class then
      The_Line_Handle := Line_Handle(The_Handle).Previous_Line;
    else
      loop
        if The_Handle = null then
          return Line_Number'first;
        end if;
        exit when The_Handle.all in Line_Object'class;
        The_Handle := The_Handle.Previous;
      end loop;
      The_Line_Handle := Line_Handle(The_Handle);
    end if;
    while The_Line_Handle /= null loop
      if The_Line_Number > Line_Number'last - The_Line_Handle.Count then
        return Line_Number'last;
      end if;
      The_Line_Number := The_Line_Number + The_Line_Handle.Count;
      The_Line_Handle := The_Line_Handle.Previous_Line;
    end loop;
    return The_Line_Number;
  end Line_Number_Of;


  function Column_Position_Of (Item : Handle) return Column_Position is
  begin
    return Area_Of (Item.all).First_Column;
  end Column_Position_Of;


  function Lexical_After (Item : Handle) return Lexical_Handle is
    The_Handle : Handle := Item.Next;
  begin
    while The_Handle /= null loop
      exit when The_Handle.all in Lexical_Object'class;
      The_Handle := The_Handle.Next;
    end loop;
    return Lexical_Handle(The_Handle);
  end Lexical_After;


  function Lexical_After (Item : Lexical_Handle) return Lexical_Handle is
  begin
    return Lexical_After (Handle(Item));
  end Lexical_After;


  function Lexical_Before (Item : Handle) return Lexical_Handle is
    The_Handle : Handle := Item.Previous;
  begin
    while The_Handle /= null loop
      exit when The_Handle.all in Lexical_Object'class;
      The_Handle := The_Handle.Previous;
    end loop;
    return Lexical_Handle(The_Handle);
  end Lexical_Before;


  function Lexical_Before (Item : Lexical_Handle) return Lexical_Handle is
  begin
    return Lexical_Before (Handle(Item));
  end Lexical_Before;


  function Next_Special_Comment (Item : Handle) return Special_Comment_Handle is
    The_Handle : Handle := Item.Next;
  begin
    while The_Handle /= null loop
      if The_Handle.all in Special_Comment'class then
        return Special_Comment_Handle(The_Handle);
      end if;
      exit when The_Handle.all in Lexical_Object'class;
      The_Handle := The_Handle.Next;
    end loop;
    return null;
  end Next_Special_Comment;


  function Line_End_Special_Comment (Item : Handle) return Special_Comment_Handle is
    The_Handle : Handle := Item.Next;
  begin
    while The_Handle /= null loop
      if The_Handle.all in Line_Object'class then
        if The_Handle.all in Special_Line_End_Comment'class then
          return Special_Comment_Handle(The_Handle);
        else
          return null;
        end if;
      end if;
      The_Handle := The_Handle.Next;
    end loop;
    return null;
  end Line_End_Special_Comment;


  function Name_List_Of (Item : Identifiers) return Name.List is
    The_Name_List : Name.List (1..Item'length);
  begin
    for Index in Item'range loop
      The_Name_List(The_Name_List'first + Index - Item'first) := Item(Index).Id;
    end loop;
    return The_Name_List;
  end Name_List_Of;


  function Image_Of (Item      : Identifiers;
                     Separator : Character := '.') return String is

    The_Index : Natural := Item'first;

    function Prepended (Image : String) return String is
    begin
      if The_Index <= Item'last then
        declare
          Next_Image : constant String := Image_Of (Item (The_Index).all);
        begin
          The_Index := The_Index + 1;
          if Image = "" then
            return Prepended(Next_Image);
          else
            return Prepended(Image & Separator & Next_Image);
          end if;
        end;
      else
        return Image;
      end if;
    end Prepended;

  begin
    return Prepended("");
  end Image_Of;


  function Annotation_Text (Mark       : String;
                            Annotation : Special_Comment_Handle) return String is
  begin
    if Annotation /= null then
      declare
        The_Text  : constant String := Comment.Text_Of (Annotation.Handle);
        The_Index : Natural := The_Text'first + 1; -- skip '>'

        procedure Skip_Spaces is
        begin
          while The_Index <= The_Text'last and then The_Text(The_Index) = ' ' loop
            The_Index := The_Index + 1;
          end loop;
        end Skip_Spaces;

        function Next_Is (Item : String) return Boolean is
          The_First : Natural;
        begin
          Skip_Spaces;
          The_First := The_Index;
          The_Index := The_Index + Item'length;
          return The_Text'last >= The_Index and then
            Ada.Characters.Handling.To_Upper (The_Text(The_First .. The_Index - 1)) = Item;
        end Next_Is;

        function Next_Token return String is
        begin
          Skip_Spaces;
          return The_Text(The_Index .. The_Text'last);
        end Next_Token;

      begin
        if Next_Is (Mark) and then Next_Is (":") then
          return Next_Token;
        end if;
      end;
    end if;
    return "";
  end Annotation_Text;


  function Next_Style (Item          :     Handle;
                       Style_Defined : out Boolean) return Lexical.Style_Pragma is
    use all type Lexical.Style_Pragma;
    Annotation : constant Special_Comment_Handle := Next_Special_Comment (Item);
  begin
    Style_Defined := False;
    if Annotation = null then
      return Is_Style_None;
    end if;
    declare
      Style_Name : constant String := "Is_Style_" & Annotation_Text ("STYLE", Annotation);
      The_Style  : Lexical.Style_Pragma;
    begin
      The_Style := Lexical.Style_Pragma'value(Style_Name);
      Annotation.Is_Used := True;
      Style_Defined := True;
      return The_Style;
    end;
  exception
  when others =>
    return Is_Style_None;
  end Next_Style;


  function Is_Marked (Mark       : String;
                      Annotation : Special_Comment_Handle) return Boolean is
  begin
    if Annotation_Text (Mark, Annotation) /= "" then
      Annotation.Is_Used := True;
      return True;
    end if;
    return False;
  end Is_Marked;


  ----------------------------------------------------------------------------------------------------------------------
  -- Objects
  ----------------------------------------------------------------------------------------------------------------------

  function Image_Of (Item : Object) return String is
    pragma Unreferenced (Item);
  begin
    return "";
  end Image_Of;

  ----------------------------------------------------------------------------------------------------------------------

  function Area_Of (Item : File_Begin) return Area is
    pragma Unreferenced (Item);
  begin
    return Area'(First_Column   => 0,
                 Last_Column    => 0,
                 Last_Column_Line_Offset => 0);
  end Area_Of;

  function Kind_Of (Item : File_Begin) return Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Invisible;
  end Kind_Of;

  ----------------------------------------------------------------------------------------------------------------------

  function Area_Of (Item : File_End) return Area is
    pragma Unreferenced (Item);
  begin
    return Area'(First_Column   => 0,
                 Last_Column    => 0,
                 Last_Column_Line_Offset => 0);
  end Area_Of;

  function Kind_Of (Item : File_End) return Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Invisible;
  end Kind_Of;

  ----------------------------------------------------------------------------------------------------------------------

  function Area_Of (Item : Line_End) return Area is
  begin
    return Area'(First_Column   => Item.Column,
                 Last_Column    => 0,
                 Last_Column_Line_Offset => 1);
  end Area_Of;

  function Kind_Of (Item : Line_End) return Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Invisible;
  end Kind_Of;

  ----------------------------------------------------------------------------------------------------------------------

  function Area_Of (Item : Lines) return Area is
  begin
    return Area'(First_Column   => Item.Column,
                 Last_Column    => 0,
                 Last_Column_Line_Offset => Item.Count);
  end Area_Of;

  function Kind_Of (Item : Lines) return Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Invisible;
  end Kind_Of;

  ----------------------------------------------------------------------------------------------------------------------

  function Area_Of (Item : Line_End_Comment) return Area is
  begin
    return Area'(First_Column => Item.Column,
                 Last_Column  => Item.Column + Column_Position(Comment.Length_Of (Item.Handle)) - 1,
                 Last_Column_Line_Offset => 0);
  end Area_Of;

  function Image_Of (Item : Line_End_Comment) return String is
  begin
    return Comment.Image_Of (Item.Handle);
  end Image_Of;

  function Kind_Of (Item : Line_End_Comment) return Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Comment;
  end Kind_Of;

  ----------------------------------------------------------------------------------------------------------------------

  function Area_Of (Item : Comment_Lines) return Area is
    use type Counter;
  begin
    return Area'(First_Column => Item.Column,
                 Last_Column  => Item.Column + Column_Position(Comment.Length_Of (Item.List(Item.Length))) - 1,
                 Last_Column_Line_Offset => Item.Length - 1);
  end Area_Of;

  function Kind_Of (Item : Comment_Lines) return Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Comment;
  end Kind_Of;

  ----------------------------------------------------------------------------------------------------------------------

  function Area_Of (Item : Special_Comment) return Area is
  begin
    return Area'(First_Column => Item.Column,
                 Last_Column  => Item.Column + Column_Position(Comment.Length_Of (Item.Handle)) - 1,
                 Last_Column_Line_Offset => 0);
  end Area_Of;

  function Kind_Of (Item : Special_Comment) return Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Special_Comment;
  end Kind_Of;

  ----------------------------------------------------------------------------------------------------------------------

  function Area_Of (Item : Name_Object) return Area is
  begin
    return Area'(First_Column   => Item.Column,
                 Last_Column    => Item.Column + Column_Position(Name.Length_Of (Item.Id)) - 1,
                 Last_Column_Line_Offset => 0);
  end Area_Of;

  function Image_Of (Item : Name_Object) return String is
  begin
    return Name.Image_Of (Item.Id);
  end Image_Of;

  ----------------------------------------------------------------------------------------------------------------------

  function Kind_Of (Item : Reserved_Word) return Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Reserved_Word;
  end Kind_Of;

  ----------------------------------------------------------------------------------------------------------------------

  function Kind_Of (Item : Aspect) return Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Aspect;
  end Kind_Of;

  ----------------------------------------------------------------------------------------------------------------------

  function Kind_Of (Item : Attribute) return Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Attribute;
  end Kind_Of;

  ----------------------------------------------------------------------------------------------------------------------

  overriding
  function Kind_Of (Item : Pragma_Identifier) return Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Pragma_Identifier;
  end Kind_Of;

  ----------------------------------------------------------------------------------------------------------------------

  function Kind_Of (Item : Identifier) return Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Identifier;
  end Kind_Of;

  ----------------------------------------------------------------------------------------------------------------------

  function Area_Of (Item : Operator_Symbol) return Area is
  begin
    return Area'(First_Column   => Item.Column,
                 Last_Column    => Item.Column + Column_Position(Name.Length_Of (Item.Id)),
                 Last_Column_Line_Offset => 0);
  end Area_Of;

  function Image_Of (Item : Operator_Symbol) return String is
  begin
    return """" & Name.Image_Of (Item.Id); -- closing " is stored in name image
  end Image_Of;

  ----------------------------------------------------------------------------------------------------------------------

  function Area_Of (Item : Character_Literal) return Area is
  begin
    return Area'(First_Column   => Item.Column,
                 Last_Column    => Item.Column + Column_Position(Name.Length_Of (Item.Id)),
                 Last_Column_Line_Offset => 0);
  end Area_Of;

  function Image_Of (Item : Character_Literal) return String is
    Image : constant String := Name.Image_Of (Item.Id);
  begin
    return ''' & Image(Image'last) & ''';
  end Image_Of;

  function Kind_Of (Item : Character_Literal) return Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Character_Literal;
  end Kind_Of;

  ----------------------------------------------------------------------------------------------------------------------

  function Area_Of (Item : String_Literal) return Area is
  begin
    return Area'(First_Column   => Item.First_Column,
                 Last_Column    => Item.Last_Column,
                 Last_Column_Line_Offset => 0);
  end Area_Of;

  function Image_Of (Item : String_Literal) return String is
  begin
    return Texts.Image_Of (Item.Handle);
  end Image_Of;

  function Kind_Of (Item : String_Literal) return Kind is
    pragma Unreferenced (Item);
  begin
    return Is_String_Literal;
  end Kind_Of;

  ----------------------------------------------------------------------------------------------------------------------

  function Area_Of (Item : Numeric_Literal) return Area is
  begin
    return Area'(First_Column   => Item.Column,
                 Last_Column    => Item.Column + Column_Position(Number.Length_Of(Item.Handle)) - 1,
                 Last_Column_Line_Offset => 0);
  end Area_Of;

  function Image_Of (Item : Numeric_Literal) return String is
  begin
    return Number.Image_Of (Item.Handle);
  end Image_Of;

  function Kind_Of (Item : Numeric_Literal) return Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Numeric_Literal;
  end Kind_Of;

  ----------------------------------------------------------------------------------------------------------------------

  function Area_Of (Item : Delimiter) return Area is
  begin
    return Area'(First_Column   => Item.Column,
                 Last_Column    => Item.Column + Column_Position(Symbol.Length_Of (Item.Element)) - 1,
                 Last_Column_Line_Offset => 0);
  end Area_Of;

  function Image_Of (Item : Delimiter) return String is
  begin
    return Symbol.Image_Of (Item.Element);
  end Image_Of;

  function Kind_Of (Item : Delimiter) return Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Delimiter;
  end Kind_Of;

  ----------------------------------------------------------------------------------------------------------------------

  function Area_Of (Item : Error_Object) return Area is
  begin
    return Area'(First_Column   => Item.Column,
                 Last_Column    => Item.Column + Column_Position(Error.Length_Of (Item.Handle)) - 1,
                 Last_Column_Line_Offset => 0);
  end Area_Of;

  function Image_Of (Item : Error_Object) return String is
  begin
    return Error.Image_Of (Item.Handle);
  end Image_Of;

  function Kind_Of (Item : Error_Object) return Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Error;
  end Kind_Of;


  ----------------------------------------------------------------------------------------------------------------------
  -- Constructors
  ----------------------------------------------------------------------------------------------------------------------

  Max_Comment_List_Length : constant := Counter'last;

  Has_Tokens             : Boolean;
  Is_In_Unit             : Boolean;
  Is_Generic_Formal_Type : Boolean;
  Aspect_Enabled         : Boolean;
  Aspect_Allowed         : Boolean;
  Is_In_Iterable_Aspect  : Boolean;
  Last_Was_Apostrophe    : Boolean;
  Next_Is_Identifier     : Boolean;
  In_Pragma_Call         : Boolean;
  The_Last_String        : String_Handle;
  The_Position           : Column_Position;
  Last_Position          : Column_Position;
  The_Line_Count         : Counter;
  The_Comment_Count      : Counter;
  The_Comment_List       : Comment_List (1 .. Max_Comment_List_Length);

  The_Nesting_Level : Integer;

  The_List : Sequence;


  function Actual_List return Sequence is
  begin
    return The_List;
  end Actual_List;


  procedure Append (Item : Object'class) with Inline is
  begin
    The_List.Last.Next := new Object'class'(Item);
    The_List.Last := The_List.Last.Next;
  end Append;


  procedure Append_Line_Object (Item : Line_Object'class) with Inline is
  begin
    Append (Item);
    The_List.Last_Line := Line_Handle(The_List.Last);
  end Append_Line_Object;


  procedure Append_Comments with Inline is
  begin
    Append_Line_Object (Comment_Lines'(Length        => The_Comment_Count,
                                       List          => The_Comment_List(1 .. The_Comment_Count),
                                       Column        => Last_Position,
                                       Count         => The_Comment_Count,
                                       Previous_Line => The_List.Last_Line,
                                       Next          => null,
                                       Previous      => The_List.Last));
    The_Comment_Count := 0;
  end Append_Comments;


  procedure Append_Lines with Inline is
  begin
    Append_Line_Object (Lines'(Column        => Last_Position,
                               Count         => The_Line_Count,
                               Previous_Line => The_List.Last_Line,
                               Next          => null,
                               Previous      => The_List.Last));
    The_Line_Count := 0;
  end Append_Lines;


  procedure Append_Comments_Or_Lines with Inline is
    use type Counter;
  begin
    if The_Comment_Count > 0 then
      Append_Comments;
    elsif The_Line_Count > 0 then
      Append_Lines;
    end if;
  end Append_Comments_Or_Lines;


  procedure Append_Object (Item : Object'class) with Inline is
  begin
    Append (Item);
    Has_Tokens := True;
    if not Is_In_Iterable_Aspect then
      Aspect_Allowed := False;
    end if;
    Last_Was_Apostrophe := False;
    Next_Is_Identifier := False;
    The_Last_String := null;
  end Append_Object;


  procedure Create_List (Filename : String) is

    Start_Object : constant Handle := new File_Begin'(Length   => Filename'length,
                                                      Name     => Filename,
                                                      Next     => null,
                                                      Previous => null);
  begin
    Is_In_Unit := False;
    Is_Generic_Formal_Type := False;
    Has_Tokens := False;
    Last_Was_Apostrophe := False;
    Aspect_Allowed := False;
    Aspect_Enabled := False;
    Is_In_Iterable_Aspect := False;
    Next_Is_Identifier := False;
    In_Pragma_Call := False;
    The_Last_String := null;
    The_Position := 1;
    Last_Position := 1;
    The_Line_Count := 0;
    The_Comment_Count := 0;
    The_Nesting_Level := 0;
    Special_Comments_Allowed := True;
    The_List := Sequence'(First       => Start_Object,
                          Last        => Start_Object,
                          Last_Line   => null,
                          Error_Token => null);
  end Create_List;


  procedure End_List is
  begin
    Append_Comments_Or_Lines;
    Append_Object (File_End'(Next     => null,
                             Previous => The_List.Last));
  end End_List;


  procedure Set_Position (Column : Column_Position) is
  begin
    The_Position := Column;
  end Set_Position;


  function Position return Column_Position is
  begin
    return The_Position;
  end Position;


  procedure New_Line is
    use type Counter;
  begin
    if The_Comment_Count > 0 then
      Append_Comments;
    end if;
    if Has_Tokens then
      Append_Line_Object (Line_End'(Column        => The_Position,
                                    Count         => 1,
                                    Previous_Line => The_List.Last_Line,
                                    Next          => null,
                                    Previous      => The_List.Last));
      Has_Tokens := False;
    elsif The_Line_Count = 0 then
      The_Line_Count := 1;
      Last_Position := The_Position;
    elsif Last_Position = The_Position then
      The_Line_Count := The_Line_Count + 1;
    else
      Append_Lines;
      The_Line_Count := 1;
      Last_Position := The_Position;
    end if;
  end New_Line;


  procedure Append_Comment (Item : Comment.Handle) is
    use type Counter;
  begin -- Append_Comment
    if Has_Tokens then
      if Special_Comments_Allowed and then Comment.Is_Special (Item) then
        Append_Line_Object (Special_Line_End_Comment'(Handle        => Item,
                                                      Is_Used       => False,
                                                      Column        => The_Position,
                                                      Count         => 1,
                                                      Previous_Line => The_List.Last_Line,
                                                      Next          => null,
                                                      Previous      => The_List.Last));
      else
        Append_Line_Object (Line_End_Comment'(Handle        => Item,
                                              Column        => The_Position,
                                              Count         => 1,
                                              Previous_Line => The_List.Last_Line,
                                              Next          => null,
                                              Previous      => The_List.Last));
      end if;
      Has_Tokens := False;
    else
      if The_Line_Count > 0 then
        Append_Lines;
      end if;
      if Special_Comments_Allowed and Comment.Is_Special (Item) then
        if The_Comment_Count > 0 then
          Append_Comments;
        end if;
        Append_Line_Object (Special_Comment'(Handle        => Item,
                                             Is_Used       => False,
                                             Column        => The_Position,
                                             Count         => 1,
                                             Previous_Line => The_List.Last_Line,
                                             Next          => null,
                                             Previous      => The_List.Last));
      elsif The_Comment_Count = 0 then
        The_Comment_Count := 1;
        The_Comment_List (The_Comment_Count) := Item;
        Last_Position := The_Position;
      elsif Last_Position = The_Position then
        The_Comment_Count := The_Comment_Count + 1;
        The_Comment_List (The_Comment_Count) := Item;
      else
        Append_Comments;
        The_Comment_Count := 1;
        The_Comment_List (The_Comment_Count) := Item;
        Last_Position := The_Position;
      end if;
    end if;
  end Append_Comment;


  procedure Append_Name (Item : Name.Handle) is
  begin
    Append_Comments_Or_Lines;
    if Last_Was_Apostrophe and then Lexical.Is_Attribute (Item.Key) then
      Append_Object (Attribute'(Id         => Item,
                                Next       => null,
                                Previous   => The_List.Last,
                                Column     => The_Position,
                                Element    => Lexical.Attribute,
                                Designator => Lexical.Attribute_Of (Item.Key)));
    elsif Lexical.Is_Reserved_Word (Item.Key) then
      declare
        Keyword : constant Lexical.Element := Lexical.Reserved_Word_Of (Item.Key);
      begin
        Append_Object (Reserved_Word'(Id       => Item,
                                      Next     => null,
                                      Previous => The_List.Last,
                                      Column   => The_Position,
                                      Element  => Keyword));
        case Keyword is
        when Lexical.Is_Generic =>
          Is_In_Unit := False;
        when Lexical.Is_Package =>
          Is_In_Unit := not Aspect_Allowed;
        when Lexical.Is_Type =>
          Is_Generic_Formal_Type := not Is_In_Unit;
        when Lexical.Is_Function | Lexical.Is_Procedure =>
          Next_Is_Identifier := True;
          Is_In_Unit := not Aspect_Allowed;
        when Lexical.Is_End | Lexical.Is_Is | Lexical.Is_Renames =>
          Next_Is_Identifier := True;
        when Lexical.Is_Pragma =>
          In_Pragma_Call := True;
        when Lexical.Is_With =>
          if (Is_In_Unit or Is_Generic_Formal_Type) and The_Nesting_Level = 0 then
            Aspect_Allowed := True;
            Aspect_Enabled := True;
          end if;
        when Lexical.Is_Record =>
          Aspect_Allowed := False;
          Aspect_Enabled := False;
        when others =>
          null;
        end case;
      end;
    elsif Aspect_Allowed and then Lexical.Is_Aspect (Item.Key) then
      declare
        Aspect_Item : constant Lexical.Aspect_Id := Lexical.Aspect_Of (Item.Key);
        use type Lexical.Aspect_Id;
      begin
        Append_Object (Aspect'(Id         => Item,
                               Next       => null,
                               Previous   => The_List.Last,
                               Column     => The_Position,
                               Element    => Lexical.Aspect,
                               Designator => Aspect_Item));
        if Aspect_Item in Lexical.Is_Aggregate | Lexical.Is_Iterable then
          Is_In_Iterable_Aspect := True;
          Aspect_Allowed := True;
        else
          Aspect_Allowed := False;
        end if;
      end;
    elsif In_Pragma_Call and then Lexical.Is_Pragma_Id (Item.Key) then
      Append_Object (Pragma_Identifier'(Id         => Item,
                                        Next       => null,
                                        Previous   => The_List.Last,
                                        Column     => The_Position,
                                        Element    => Lexical.Pragma_Identifier,
                                        Designator => Lexical.Pragma_Id_Of (Item.Key),
                                        Is_Used    => False));
    else
      Append_Object (Identifier'(Id       => Item,
                                 Next     => null,
                                 Previous => The_List.Last,
                                 Column   => The_Position,
                                 Element  => Lexical.Identifier,
                                 Data     => null));
    end if;
  end Append_Name;


  procedure Append_Literal (Item : Character) is
    Literal_Id : String := ['`', Item]; -- mark not uppercase
  begin
    if Ada.Characters.Handling.Is_Upper (Item) then
      Literal_Id(Literal_Id'first) := '~'; -- mark uppercase
    end if;
    Append_Comments_Or_Lines;
    Append_Object (Character_Literal'(Id       => Name.Handle_Of (Literal_Id),
                                      Next     => null,
                                      Previous => The_List.Last,
                                      Column   => The_Position,
                                      Element  => Lexical.Identifier,
                                      Data     => null));
  end Append_Literal;


  function Operator_Id_Of (Item : String) return Name.Handle with Inline is
  begin
    return Name.Handle_Of(Item & """"); -- no " at front to allow style detection
  end Operator_Id_Of;


  procedure Append_Literal (Item         : Texts.Handle;
                            First_Column : Column_Position;
                            Last_Column  : Column_Position) is
  begin
    Append_Comments_Or_Lines;
    if Next_Is_Identifier or In_Pragma_Call then
      declare
        Designator : constant String := Texts.Operator_Of (Item);
      begin
        if Designator /= "" then
          if (Designator = "=") or (Designator = "/=") then
            Append_Object (Equality_Operator'(Id       => Operator_Id_Of (Designator),
                                              Next     => null,
                                              Previous => The_List.Last,
                                              Column   => The_Position,
                                              Element  => Lexical.Identifier,
                                              Data     => null));
          else
            Append_Object (Operator_Symbol'(Id       => Operator_Id_Of (Designator),
                                            Next     => null,
                                            Previous => The_List.Last,
                                            Column   => The_Position,
                                            Element  => Lexical.Identifier,
                                            Data     => null));
          end if;
          return;
        end if;
      end;
    end if;
    Append_Object (String_Literal'(Handle       => Item,
                                   First_Column => First_Column,
                                   Last_Column  => Last_Column,
                                   Next         => null,
                                   Previous     => The_List.Last,
                                   Column       => The_Position,
                                   Element      => Lexical.String_Literal));
    The_Last_String := String_Handle(The_List.Last);
  end Append_Literal;


  procedure Append_Literal (Item           : Number.Handle;
                            Is_Real_Number : Boolean) is
  begin
    Append_Comments_Or_Lines;
    if Is_Real_Number then
      Append_Object (Real_Literal'(Handle   => Item,
                                   Next     => null,
                                   Previous => The_List.Last,
                                   Column   => The_Position,
                                   Element  => Lexical.Real_Literal));
    else
      Append_Object (Integer_Literal'(Handle   => Item,
                                      Next     => null,
                                      Previous => The_List.Last,
                                      Column   => The_Position,
                                      Element  => Lexical.Integer_Literal));
    end if;
  end Append_Literal;


  procedure Replace_String_With_Operator (Designator  : String;
                                          Last_String : String_Handle) with Inline is
  begin
    Last_String.Previous.Next := new Operator_Symbol'(Id       => Operator_Id_Of (Designator), -- no " at front to allow
                                                      Next     => Last_String.Next,            -- style detection
                                                      Previous => Last_String.Previous,
                                                      Column   => Last_String.Column,
                                                      Element  => Lexical.Identifier,
                                                      Data     => null);
    Last_String.Next.Previous := Last_String.Previous.Next;
  end Replace_String_With_Operator;


  procedure Append_Symbol (Item : Lexical.Delimiter) is
    Last_String : constant String_Handle := The_Last_String;
    use type Lexical.Element;
  begin
    Append_Comments_Or_Lines;
    Append_Object (Delimiter'(Next     => null,
                              Previous => The_List.Last,
                              Column   => The_Position,
                              Element  => Item));
    case Item is
    when Lexical.Left_Bracket | Lexical.Left_Parenthesis | Lexical.Association | Lexical.Apostrophe =>
      In_Pragma_Call := False;
      if Item in Lexical.Left_Bracket | Lexical.Left_Parenthesis then
        The_Nesting_Level := The_Nesting_Level + 1;
      end if;
      Last_Was_Apostrophe := Item = Lexical.Apostrophe;
      if Last_String /= null then
        declare
          Designator : constant String := Texts.Operator_Of (Last_String.Handle);
        begin
          if Designator /= "" then
            Replace_String_With_Operator (Designator, Last_String);
            if Item = Lexical.Association then
              Next_Is_Identifier := True;
            end if;
          end if;
        end;
      end if;
    when Lexical.Right_Bracket | Lexical.Right_Parenthesis =>
      The_Nesting_Level := The_Nesting_Level - 1;
    when Lexical.Comma =>
      if Aspect_Enabled and (The_Nesting_Level = 0 or Is_In_Iterable_Aspect) then
        Aspect_Allowed := True;
      end if;
    when Lexical.Semicolon =>
      if The_Nesting_Level = 0 then
        Aspect_Enabled := False;
        Is_In_Iterable_Aspect := False;
      end if;
      In_Pragma_Call := False;
      if Is_Generic_Formal_Type then
        Is_Generic_Formal_Type := False;
        Aspect_Allowed := False;
      end if;
    when Lexical.Period =>
      Next_Is_Identifier := True;
    when others =>
      null;
    end case;
  end Append_Symbol;


  procedure Append_Error (Message    : Error.Kind;
                          Item       : Error.Handle;
                          The_Column : Column_Range) is
  begin
    Append_Lines;
    --TEST-------------------------------------------------------------------------
    Log.Write ("!!! " & Error.Kind'image(Message) & ": " & Error.Image_Of(Item));
    Log.Write ("    at" & Column_Range'image(The_Column));
    -------------------------------------------------------------------------------
    Append_Object (Error_Object'(Next     => null,
                                 Previous => The_List.Last,
                                 Column   => The_Column,
                                 Handle   => Item,
                                 Element  => Lexical.Error,
                                 Message  => Message));
    if The_List.Error_Token = null then
      The_List.Error_Token := Error_Handle(The_List.Last);
    end if;
  end Append_Error;


  procedure Mark_Error (Message : Error.Kind;
                        Item    : Lexical_Handle;
                        Tokens  : in out Sequence) is

    The_Item        : Lexical_Handle := Item;
    The_Error_Token : Error_Handle;

  begin
    if Item = null then
      --TEST-------------------------------------------
      Log.Write ("!!! " & Error.Kind'image(Message));
      -------------------------------------------------
      The_Item := Lexical_Before (Tokens.Last);
      if The_Item = null then
        null;
      end if;
      The_Error_Token := new Error_Object'(Next       => The_Item.Next,
                                           Previous   => Handle(The_Item),
                                           Column     => Area_Of(The_Item.all).Last_Column,
                                           Element    => Lexical.Error,
                                           Message    => Message,
                                           Handle     => Error.Handle_Of(" "));
      The_Item.Next.Previous := Handle(The_Error_Token);
      The_Item.Next := Handle(The_Error_Token);
    elsif Item.all in Error_Object'class then
      return;
    else
      --TEST-------------------------------------------------------------------------------
      Log.Write ("!!! " & Error.Kind'image(Message) & ": MARK -> " & Image_Of(Item.all));
      -------------------------------------------------------------------------------------
      The_Error_Token := new Error_Object'(Next       => Item.Next,
                                           Previous   => Item.Previous,
                                           Column     => Item.Column,
                                           Element    => Lexical.Error,
                                           Message    => Message,
                                           Handle     => Error.Handle_Of(Image_Of(Item.all)));
      The_Item.Previous.Next := Handle(The_Error_Token);
      The_Item.Next.Previous := Handle(The_Error_Token);
    end if;
    if Tokens.Error_Token = null then
      Tokens.Error_Token := The_Error_Token;
    end if;
  end Mark_Error;

end Ada_95.Token;
