-- *********************************************************************************************************************
-- *                           (c) 2010 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant

with Ada_95.Comment;
with Ada_95.Lexical;

package body Ada_95.Format is

  The_Token         : Token.Handle;
  The_Kind          : Format_Kind := Is_Unknown;
  The_Data_Kind     : Data_Kind := Is_Unknown;
  The_Position      : Natural := 0;
  The_Line_Count    : Natural := 0;
  The_Comment       : Comment.Handle;
  The_Comment_Count : Token.Counter;
  The_Comment_Index : Token.Counter;


  procedure Initialize (Item : Token.Handle) is
  begin
    The_Token := Item;
    The_Kind := Is_Unknown;
    The_Data_Kind := Is_Unknown;
    The_Position := 0;
    The_Line_Count := 0;
    The_Comment_Count := 0;
    Comment.Clear (The_Comment);
  end Initialize;


  procedure Next is
    use type Token.Handle;
    use type Token.Counter;
  begin
    Comment.Clear (The_Comment);
    if The_Line_Count > 0 then
      The_Line_Count := The_Line_Count - 1;
      The_Kind := Is_New_Line;
    elsif The_Comment_Count > 0 then
      The_Comment_Count := The_Comment_Count - 1;
      The_Comment_Index := The_Comment_Index + 1;
      The_Comment := Token.Comment_Lines_Handle(The_Token).List(The_Comment_Index);
      The_Kind := Is_Comment;
      The_Line_Count := 1;
    elsif The_Token.Next /= null then
      The_Token := The_Token.Next;
      The_Position := Natural(Token.Area_Of (The_Token.all).First_Column);
      The_Line_Count := 0;
      The_Data_Kind := Is_Unknown;
      if The_Token.all in Token.Lexical_Object'class then
        case Token.Lexical_Handle(The_Token).Element is
        when Lexical.Is_Interface =>
          The_Kind := Is_Interface;
        when Lexical.Is_Overriding =>
          The_Kind := Is_Overriding;
        when Lexical.Is_Synchronized =>
          The_Kind := Is_Synchronized;
        when Lexical.Is_In =>
          The_Kind := Is_In;
        when Lexical.Is_End =>
          The_Kind := Is_End;
        when Lexical.Is_Is =>
          The_Kind :=  Is_Is;
        when Lexical.Is_If =>
          The_Kind := Is_If;
        when Lexical.Is_Return =>
          The_Kind := Is_Return;
        when Lexical.Is_When =>
          The_Kind := Is_When;
        when Lexical.Is_Out =>
          The_Kind := Is_Out;
        when Lexical.Is_Procedure =>
          The_Kind := Is_Procedure;
        when Lexical.Is_Then =>
          The_Kind := Is_Then;
        when Lexical.Is_Begin =>
          The_Kind := Is_Begin;
        when Lexical.Is_With =>
          The_Kind := Is_With;
        when Lexical.Is_Function =>
          The_Kind := Is_Function;
        when Lexical.Is_Raise =>
          The_Kind := Is_Raise;
        when Lexical.Is_Renames =>
          The_Kind := Is_Renames;
        when Lexical.Is_Others =>
          The_Kind := Is_Others;
        when Lexical.Is_Package =>
          The_Kind := Is_Package;
        when Lexical.Is_Exception =>
          The_Kind := Is_Exception;
        when Lexical.Is_Constant =>
          The_Kind := Is_Constant;
        when Lexical.Is_Loop =>
          The_Kind := Is_Loop;
        when Lexical.Is_Type =>
          The_Kind := Is_Type;
        when Lexical.Is_New =>
          The_Kind := Is_New;
        when Lexical.Is_Record =>
          The_Kind := Is_Record;
        when Lexical.Is_Else =>
          The_Kind := Is_Else;
        when Lexical.Is_Null =>
          The_Kind := Is_Null;
        when Lexical.Is_Case =>
          The_Kind := Is_Case;
        when Lexical.Is_Or =>
          The_Kind := Is_Or;
        when Lexical.Is_For =>
          The_Kind := Is_For;
        when Lexical.Is_Not =>
          The_Kind := Is_Not;
        when Lexical.Is_And =>
          The_Kind := Is_And;
        when Lexical.Is_All =>
          The_Kind := Is_All;
        when Lexical.Is_Private =>
          The_Kind := Is_Private;
        when Lexical.Is_Subtype =>
          The_Kind := Is_Subtype;
        when Lexical.Is_Elsif =>
          The_Kind := Is_Elsif;
        when Lexical.Is_Body =>
          The_Kind := Is_Body;
        when Lexical.Is_Declare =>
          The_Kind := Is_Declare;
        when Lexical.Is_Use =>
          The_Kind := Is_Use;
        when Lexical.Is_While =>
          The_Kind := Is_While;
        when Lexical.Is_Array =>
          The_Kind := Is_Array;
        when Lexical.Is_Of =>
          The_Kind := Is_Of;
        when Lexical.Is_Exit =>
          The_Kind := Is_Exit;
        when Lexical.Is_Accept =>
          The_Kind := Is_Accept;
        when Lexical.Is_At =>
          The_Kind := Is_At;
        when Lexical.Is_Pragma =>
          The_Kind := Is_Pragma;
        when Lexical.Is_Entry =>
          The_Kind := Is_Entry;
        when Lexical.Is_Do =>
          The_Kind := Is_Do;
        when Lexical.Is_Select =>
          The_Kind := Is_Select;
        when Lexical.Is_Abstract =>
          The_Kind := Is_Abstract;
        when Lexical.Is_Generic =>
          The_Kind := Is_Generic;
        when Lexical.Is_Task =>
          The_Kind := Is_Task;
        when Lexical.Is_Delay =>
          The_Kind := Is_Delay;
        when Lexical.Is_Aliased =>
          The_Kind := Is_Aliased;
        when Lexical.Is_Limited =>
          The_Kind := Is_Limited;
        when Lexical.Is_Mod =>
          The_Kind := Is_Mod;
        when Lexical.Is_Xor =>
          The_Kind := Is_Xor;
        when Lexical.Is_Terminate =>
          The_Kind := Is_Terminate;
        when Lexical.Is_Reverse =>
          The_Kind := Is_Reverse;
        when Lexical.Is_Protected =>
          The_Kind := Is_Protected;
        when Lexical.Is_Abs =>
          The_Kind := Is_Abs;
        when Lexical.Is_Rem =>
          The_Kind := Is_Rem;
        when Lexical.Is_Separate =>
          The_Kind := Is_Separate;
        when Lexical.Is_Tagged =>
          The_Kind := Is_Tagged;
        when Lexical.Is_Abort =>
          The_Kind := Is_Abort;
        when Lexical.Is_Requeue =>
          The_Kind := Is_Requeue;
        when Lexical.Is_Until =>
          The_Kind := Is_Until;
        when Lexical.Is_Goto =>
          The_Kind := Is_Goto;
        when Lexical.Is_Range =>
          The_Kind := Is_Range;
        when Lexical.Is_Access =>
          The_Kind := Is_Access;
        when Lexical.Is_Delta =>
          The_Kind :=  Is_Delta;
        when Lexical.Is_Digits =>
          The_Kind := Is_Digits;
        when Lexical.Semicolon =>
          The_Kind := Is_Semicolon;
        when Lexical.Period =>
          The_Kind := Is_Period;
        when Lexical.Comma =>
          The_Kind := Is_Comma;
        when Lexical.Assignment =>
          The_Kind := Is_Assignment;
        when Lexical.Association =>
          The_Kind := Is_Association;
        when Lexical.Equal =>
          The_Kind := Is_Equal;
        when Lexical.Not_Equal =>
          The_Kind := Is_Not_Equal;
        when Lexical.Greater =>
          The_Kind := Is_Greater;
        when Lexical.Greater_Or_Equal =>
          The_Kind := Is_Greater_Or_Equal;
        when Lexical.Less =>
          The_Kind := Is_Less;
        when Lexical.Less_Or_Equal =>
          The_Kind := Is_Less_Or_Equal;
        when Lexical.Ampersand =>
          The_Kind := Is_Ampersand;
        when Lexical.Plus =>
          The_Kind := Is_Plus;
        when Lexical.Minus =>
          The_Kind := Is_Minus;
        when Lexical.Asterisk =>
          The_Kind := Is_Asterisk;
        when Lexical.Slash =>
          The_Kind := Is_Slash;
        when Lexical.Left_Parenthesis =>
          The_Kind := Is_Left_Parenthesis;
        when Lexical.Right_Parenthesis =>
          The_Kind := Is_Right_Parenthesis;
        when Lexical.Exponentiation =>
          The_Kind := Is_Exponentiation;
        when Lexical.Colon =>
          The_Kind := Is_Colon;
        when Lexical.Apostrophe =>
          The_Kind := Is_Apostrophe;
        when Lexical.Vertical_Line =>
          The_Kind := Is_Vertical_Line;
        when Lexical.Quote =>
          The_Kind := Is_Quote;
        when Lexical.Range_Delimiter =>
          The_Kind := Is_Range_Delimiter;
        when Lexical.Unconstrained =>
          The_Kind := Is_Unconstrained;
        when Lexical.Start_Label =>
          The_Kind := Is_Start_Label;
        when Lexical.End_Label =>
          The_Kind := Is_End_Label;
        when Lexical.Special_Id =>
          The_Kind := Is_Special_Id;
        when Lexical.Attribute =>
          The_Kind := Is_Attribute;
        when Lexical.Identifier =>
          declare
            Handle : constant Token.Data_Handle := Token.Identifier_Handle(The_Token).Data;
            use type Token.Data_Handle;
          begin
            if Handle = null then
              The_Data_Kind := Is_Unknown;
            else
              The_Data_Kind := Token.Data_Kind_Of (Handle.all);
            end if;
          end;
          if The_Token.all in Token.Character_Literal'class then
            The_Kind := Is_Character_Literal;
          else
            if Token.Is_Declaration (The_Token) then
              The_Kind := Is_Declaring_Identifier;
            else
              The_Kind := Is_Identifier;
            end if;
          end if;
        when Lexical.Integer_Literal =>
          The_Kind := Is_Integer_Literal;
        when Lexical.Real_Literal =>
          The_Kind := Is_Real_Literal;
        when Lexical.String_Literal =>
          The_Kind := Is_String_Literal;
        when Lexical.Error =>
          The_Kind := Is_Error;
        end case;
      elsif The_Token.all in Token.File_End then
        The_Kind := Is_End_Of_File;
      elsif The_Token.all in Token.Line_End then
        The_Kind := Is_New_Line;
      elsif The_Token.all in Token.Lines then
        The_Kind := Is_New_Line;
        The_Line_Count := Natural(Token.Area_Of (The_Token.all).Last_Column_Line_Offset) - 1;
      elsif The_Token.all in Token.Comment_Lines then
        The_Comment_Count := Token.Comment_Lines_Handle(The_Token).Length - 1;
        The_Comment_Index := Token.Comment_Lines_Handle(The_Token).List'first;
        The_Comment := Token.Comment_Lines_Handle(The_Token).List(The_Comment_Index);
        The_Kind := Is_Comment;
        The_Line_Count := 1;
      elsif The_Token.all in Token.Line_End_Comment then
        The_Kind := Is_Comment;
        The_Comment := Token.Line_End_Comment_Handle(The_Token).Handle;
        The_Line_Count := 1;
      elsif The_Token.all in Token.Special_Comment'class then
        The_Kind := Is_Special_Comment;
        The_Comment := Token.Special_Comment_Handle(The_Token).Handle;
        The_Line_Count := 1;
      end if;
    end if;
  end Next;


  function Actual_Image return String is
    use type Comment.Handle;
    use type Token.Handle;
  begin
    if not Comment.Is_Null (The_Comment) then
      return Comment.Image_Of (The_Comment);
    elsif The_Token /= null then
      if The_Token.all in Token.Identifier then
        declare
          Reference : constant Token.Handle := Token.Reference_Of (The_Token);
        begin
          if Reference /= null then
            return Token.Image_Of (Token.Lexical_Handle(Reference).all);
          end if;
        end;
      end if;
      if The_Token.all in Token.Lexical_Object'class then
        return Token.Image_Of (Token.Lexical_Handle(The_Token).all);
      end if;
    end if;
    return "";
  end Actual_Image;


  function Actual_Kind return Format_Kind is
  begin
    return The_Kind;
  end Actual_Kind;


  function Actual_Data_Kind return Data_Kind is
  begin
    return The_Data_Kind;
  end Actual_Data_Kind;


  function Actual_Position return Natural is
  begin
    return The_Position;
  end Actual_Position;


end Ada_95.Format;
