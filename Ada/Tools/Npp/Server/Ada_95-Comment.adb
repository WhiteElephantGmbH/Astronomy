-- *********************************************************************************************************************
-- *                       (c) 2007 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada_95.Token;

package body Ada_95.Comment is

  function Is_Null (Item : Handle) return Boolean is
  begin
    return Item = null;
  end Is_Null;


  procedure Clear (The_Item : in out Handle) is
  begin
    The_Item := null;
  end Clear;


  procedure Add (Item : String) is
  begin
    Token.Append_Comment (Handle'(new String'(Item)));
  end Add;


  function Is_Special (The_Handle : Handle) return Boolean is
  begin
    if The_Handle'length > 0 and then The_Handle(The_Handle'first) = '>' then
      declare
        The_Index : Positive := The_Handle'first + 1;

        function Found (Item : String) return Boolean is
          Last : constant Positive := The_Index + Item'length - 1;
        begin
          return Last <= The_Handle'last and then Item = The_Handle (The_Index .. Last);
        end Found;

      begin
        while The_Index <= The_Handle'last loop
          if The_Handle(The_Index) /= ' ' then
            return Found ("Style") or Found ("UP") or Found ("UD");
          end if;
          The_Index := The_Index + 1;
        end loop;
      end;
    end if;
    return False;
  end Is_Special;


  function Text_Of (The_Handle : Handle) return String is
  begin
    return The_Handle.all;
  end Text_Of;


  function Image_Of (The_Handle : Handle) return String is
  begin
    return "--" & The_Handle.all;
  end Image_Of;


  function Length_Of (The_Handle : Handle) return Natural is
  begin
    return The_Handle'length + 2;
  end Length_Of;

end Ada_95.Comment;
