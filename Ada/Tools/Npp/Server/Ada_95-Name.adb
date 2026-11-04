-- *********************************************************************************************************************
-- *                       (c) 2007 .. 2020 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Unchecked_Conversion;
with Ada_95.Token;

package body Ada_95.Name is

  ----------------
  -- Name Handling  (Storage: Global_Pool)
  ----------------

  package Id is

    procedure Add (Image : Word_List) with Inline;

    function Handle_Of (Image : Word_List) return Handle with Inline;

    procedure Do_Finalize;

  end Id;


  package Key is

    function Item_Of (Image : Word_List) return Lexical.Name_Key;

    procedure Do_Finalize;

  end Key;


  subtype Length_Range is Positive range 1 .. Max_Length;

  The_Item : Word_List (Length_Range);

  The_Word_Index : Natural := 0;


  package body Id is

    type Name_Tree;

    type Tree_Access is access Name_Tree;
    for Tree_Access'storage_pool use Memory.Global_Pool.all;

    type Name_Tree is record
      Name  : Handle;
      Left  : Tree_Access;
      Right : Tree_Access;
    end record;


    subtype Hash_Key is Lexical.Name_Key range 0..255;

    type Length_Table is array (Length_Range) of Tree_Access;
    type Table is array (Hash_Key) of Length_Table;

    The_Table : Table;

    The_Handle : Handle;


    function Comparison (Left  : Word_List;
                         Right : Word_List) return Result with Inline is
      Max_Int_Bit : constant := Standard'address_size - 1;
      type Int is range -2 ** (Max_Int_Bit) .. 2 ** Max_Int_Bit - 1;
      function Convert is new Ada.Unchecked_Conversion (Word.Handle, Int);
    begin
      for Index in Left'range loop
        declare
          Left_Word  : constant Int := Convert(Left(Index));
          Right_Word : constant Int := Convert(Right(Index + Right'first - Left'first));
        begin
          if Left_Word > Right_Word then
            return After;
          elsif Left_Word < Right_Word then
            return Before;
          end if;
        end;
      end loop;
      return Equal;
    end Comparison;


    procedure Create (Tree  : out Tree_Access;
                      Image :     Word_List) with Inline is
    begin
      The_Handle := new Item'(Length => Image'length,
                              List   => Image,
                              Key    => Key.Item_Of (Image));
      Tree := new Name_Tree'(Name => The_Handle,
                             Left  => null,
                             Right => null);
    end Create;


    procedure Add (Image :        Word_List;
                   Tree  : in out Tree_Access) is
    begin
      if Tree = null then
        Create (Tree, Image);
      else
        case Comparison (Image, Tree.Name.List) is
        when Equal =>
          The_Handle := Tree.Name;
          return;
        when Before =>
          Add (Image, Tree.Left);
        when After =>
          Add (Image, Tree.Right);
        end case;
      end if;
    end Add;


    procedure Add (Image : Word_List) is
      use type Lexical.Name_Key;
      Tree : Tree_Access renames The_Table(Image(Image'last).Key mod (Hash_Key'last + 1))(Image'length);
    begin
      if Tree = null then
        Create (Tree, Image);
      else
        case Comparison (Image, Tree.Name.List) is
        when Equal =>
          The_Handle := Tree.Name;
        when Before =>
          Add (Image, Tree.Left);
        when After =>
          Add (Image, Tree.Right);
        end case;
      end if;
    end Add;


    function Handle_Of (Image : Word_List) return Handle is
    begin
      Add (Image);
      return The_Handle;
    end Handle_Of;


    procedure Do_Finalize is
    begin
      for The_Key in Hash_Key loop
        for Length in Length_Range loop
          The_Table(The_Key)(Length) := null;
        end loop;
      end loop;
    end Do_Finalize;

  end Id;


  package body Key is

    type Name_Tree;

    type Tree_Access is access Name_Tree;
    for Tree_Access'storage_pool use Memory.Global_Pool.all;

    type Name_Tree (Length : Positive) is record
      Image : Word_List (1 .. Length);
      Key   : Lexical.Name_Key;
      Left  : Tree_Access;
      Right : Tree_Access;
    end record;


    type Table is array (Length_Range) of Tree_Access;

    The_Table : Table;

    The_Key : Lexical.Name_Key;


    function Comparison (Left  : Word_List;
                         Right : Word_List) return Result with Inline is
    begin
      for Index in Left'range loop
        declare
          use type Lexical.Name_Key;
          Left_Item  : constant Lexical.Name_Key := Left(Index).Key;
          Right_Item : constant Lexical.Name_Key := Right(Index + Right'first - Left'first).Key;
        begin
          if Left_Item /= Right_Item then
            if Left_Item > Right_Item then
              return After;
            else
              return Before;
            end if;
          end if;
        end;
      end loop;
      return Equal;
    end Comparison;


    procedure Create (Tree  : out Tree_Access;
                      Image :     Word_List) with Inline is
    begin
      if Image'length = 1 then
        The_Key := Image(Image'first).Key;
      else
        The_Key := Lexical.Next_Name_Key;
      end if;
      Tree := new Name_Tree'(Length => Image'length,
                             Image  => Image,
                             Key    => The_Key,
                             Left   => null,
                             Right  => null);
    end Create;


    procedure Add (Image :        Word_List;
                   Tree  : in out Tree_Access) is
    begin
      if Tree = null then
        Create (Tree, Image);
      else
        case Comparison (Image, Tree.Image) is
        when Equal =>
          The_Key := Tree.Key;
          return;
        when Before =>
          Add (Image, Tree.Left);
        when After =>
          Add (Image, Tree.Right);
        end case;
      end if;
    end Add;


    function Item_Of (Image : Word_List) return Lexical.Name_Key is
      Tree : Tree_Access renames The_Table(Image'length);
    begin
      if Tree = null then
        Create (Tree, Image);
      else
        case Comparison (Image, Tree.Image) is
        when Equal =>
          return Tree.Key;
        when Before =>
          Add (Image, Tree.Left);
        when After =>
          Add (Image, Tree.Right);
        end case;
      end if;
      return The_Key;
    end Item_Of;


    procedure Do_Finalize is
    begin
      for Length in Length_Range loop
        The_Table(Length) := null;
      end loop;
    end Do_Finalize;

  end Key;


  function "=" (Left, Right : Handle) return Boolean is
    use type Lexical.Name_Key;
  begin
    return Left.Key = Right.Key;
  end "=";


  function "<" (Left, Right : Handle) return Boolean is
    use type Lexical.Name_Key;
  begin
    return Left.Key < Right.Key;
  end "<";


  function Comparison (Left, Right : Handle) return Result is
    use type Lexical.Name_Key;
  begin
    if Left.Key < Right.Key then
      return Before;
    elsif Left.Key > Right.Key then
      return After;
    else
      return Equal;
    end if;
  end Comparison;


  function Is_Equivalent (Left, Right : Handle) return Boolean is
    use type Lexical.Name_Key;
    use type Word.Case_Style;
  begin
    if Left.Key /= Right.Key then
      return False;
    else
      for Index in Left.List'range loop
        if Left.List(Index).Style /= Right.List(Index).Style then
          return False;
        end if;
      end loop;
      return True;
    end if;
  end Is_Equivalent;


  function Is_Capitalized_Or_Upper (The_Name : Handle) return Boolean is
    use type Word.Case_Style;
  begin
    for Index in The_Name.List'range loop
      case The_Name.List(Index).Style is
      when Word.Capitalized | Word.Upper_Case | Word.Undefined_Case =>
        null;
      when others =>
        return False;
      end case;
    end loop;
    return True;
  end Is_Capitalized_Or_Upper;


  function Is_Lower (The_Name : Handle) return Boolean is
    use type Word.Case_Style;
  begin
    for Index in The_Name.List'range loop
      case The_Name.List(Index).Style is
      when Word.Lower_Case | Word.Undefined_Case =>
        null;
      when others =>
        return False;
      end case;
    end loop;
    return True;
  end Is_Lower;


  procedure Add (Part : String) is
  begin
    The_Word_Index := The_Word_Index + 1;
    The_Item(The_Word_Index) := Word.Handle_Of (Part);
  end Add;


  procedure Complete_Compound is
  begin
    Id.Add (The_Item(1 .. The_Word_Index));
    The_Word_Index := 0;
  end Complete_Compound;


  procedure Clear is
  begin
    The_Word_Index := 0;
  end Clear;


  procedure Complete is
  begin
    Token.Append_Name (Id.Handle_Of(The_Item(1 .. The_Word_Index)));
    The_Word_Index := 0;
  end Complete;


  function Image_Of (The_Name  : Word_List) return String is
  begin
    if The_Name'length = 1 then
      return The_Name(The_Name'first).Image;
    else
      return The_Name (The_Name'first).Image & "_" & Image_Of (The_Name(The_Name'first + 1 .. The_Name'last));
    end if;
  end Image_Of;


  function Length_Of (The_Name  : Word_List) return Natural with Inline is
    The_Length : Natural := The_Name(The_Name'first).Length;
  begin
    for Index in The_Name'first + 1 .. The_Name'last loop
      The_Length := The_Length + 1 + The_Name (Index).Length;
    end loop;
    return The_Length;
  end Length_Of;


  function Handle_Of (Image : String) return Handle is
    Start_Index : Natural := Image'first;
  begin
    for Index in Image'range loop
      if Image(Index) = '_' then
        Add (Image(Start_Index .. Index - 1));
        Start_Index := Index + 1;
      end if;
    end loop;
    declare
      The_Handle  : Handle;
    begin
      Add (Image(Start_Index .. Image'last));
      The_Handle := Id.Handle_Of(The_Item(1 .. The_Word_Index));
      The_Word_Index := 0;
      return The_Handle;
    end;
  end Handle_Of;


  function Image_Of (The_Name : Handle) return String is
  begin
    return Image_Of (The_Name.List);
  end Image_Of;


  function Length_Of (The_Name : Handle) return Natural is
  begin
    return Length_Of (The_Name.List);
  end Length_Of;


  procedure Finalize is
  begin
    Word.Finalize;
    Id.Do_Finalize;
    Key.Do_Finalize;
    The_Word_Index := 0;
  end Finalize;


  ---------------------
  -- Name List Handling  (Storage: Pool)
  ---------------------

  function Id_Of (Image     : String;
                  Separator : Character := '.') return List is
    The_List    : List_Elements;
    The_Last    : Positive := List_Elements'first;
    Start_Index : Natural := Image'first;
  begin
    for Index in Image'range loop
      if Image(Index) = Separator then
        The_List(The_Last) := Handle_Of(Image(Start_Index .. Index - 1));
        The_Last := The_Last + 1;
        Start_Index := Index + 1;
      end if;
    end loop;
    The_List(The_Last) := Handle_Of(Image(Start_Index .. Image'last));
    return The_List(List_Elements'first..The_Last);
  end Id_Of;


  function Image_Of (The_Name  : List;
                     Separator : Character := '.') return String is

    The_Index : Natural := The_Name'first;

    function Prepended (Image : String) return String is
    begin
      if The_Index <= The_Name'last then
        declare
          Next_Image : constant String := Image_Of (The_Name (The_Index));
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


  function "=" (Left, Right : List) return Boolean is
    use type Lexical.Name_Key;
  begin
    if Left'length = Right'length then
      for The_Index in Left'range loop
        if Left(The_Index).Key /= Right(The_Index).Key then
          return False;
        end if;
      end loop;
      return True;
    end if;
    return False;
  end "=";


  function "<" (Left, Right : List) return Boolean is
  begin
    raise Program_Error; -- not implemented
    return False;
  end "<";


  function Comparison (Left, Right : List) return Result is
    use type Lexical.Name_Key;
  begin
    for The_Index in Left'range loop
      if Right'length < The_Index then
        return After;
      elsif Left(The_Index).Key < Right(The_Index).Key then
        return Before;
      elsif Left(The_Index).Key /= Right(The_Index).Key then
        return After;
      end if;
    end loop;
    if Right'length > Left'length then
      return Before;
    else
      return Equal;
    end if;
  end Comparison;

end Ada_95.Name;
