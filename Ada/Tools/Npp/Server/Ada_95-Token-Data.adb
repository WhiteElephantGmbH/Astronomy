-- *********************************************************************************************************************
-- *                       (c) 2007 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Tags;
with Ada_95.Library;
with Ada.Unchecked_Conversion;
with Log;
with Text;
with System;

package body Ada_95.Token.Data is

  function Comparison (Left, Right : Library_Element) return Result is
    use type Name.List_Handle;
  begin
    return Name.Comparison (Left.Id.all, Right.Id.all);
  end Comparison;

  function Id_Comparison (Id         : File.Unit_Name;
                          In_Element : Library_Element) return Result is
    use type Name.List_Handle;
  begin
    return Name.Comparison (Id, In_Element.Id.all);
  end Id_Comparison;

  function Element_With (Id : File.Unit_Name) return Library_Element is
  begin
    return Library_Element'(Id          => new File.Unit_Name'(Id),
                            Is_Standard => File.Is_Standard (Id),
                            Is_Marked   => False,
                            Is_Visited  => False,
                            Unit        => null);
  end Element_With;

  ----------------------------------------------------------------------------------------------------------------------

  function Comparison (Left, Right : Data_Handle) return Result is
  begin
    return Name.Comparison (Left.Location.Id, Right.Location.Id);
  end Comparison;

  function Id_Comparison (Id         : Name.Handle;
                          In_Element : Data_Handle) return Result is
  begin
    return Name.Comparison (Id, In_Element.Location.Id);
  end Id_Comparison;

  function Element_With (Id : Name.Handle) return Data_Handle is
    pragma Unreferenced (Id);
  begin
    return null;
  end Element_With;

  function Is_Equal (Left, Right : Data_Handle) return Boolean is
    use type Name.Handle;
  begin
    return Left.Location.Id = Right.Location.Id;
  end Is_Equal;

  function Is_Less_Than (Left, Right : Data_Handle) return Boolean is
    use type Name.Handle;
  begin
    return Left.Location.Id < Right.Location.Id;
  end Is_Less_Than;


  function Is_Null (Item : Data_Handle) return Boolean is

    function Address_Of is new Ada.Unchecked_Conversion (Data_Handle, System.Address);

    use type System.Address;

  begin
    return Address_Of(Item) = System.Null_Address;
  end Is_Null;


  function Full_Name_Of (Item : Data_Handle) return String is

    function Complete_Name (The_Name : String;
                            The_Data : Data_Handle) return String is
    begin
      if not Is_Null (The_Data) then
        if The_Data.all in Declaration_Type'class then
          declare
            Parent_Data : constant Data_Handle := Data_Handle(Declaration_Handle(The_Data).Parent);
          begin
            if not Is_Null (Parent_Data) then
              if Is_Null (Parent_Data.Location) or else Declaration_Handle(Parent_Data).Parent = null then
                return Complete_Name (The_Name, Parent_Data);
              else
                return Complete_Name (Image_Of (Parent_Data.Location.all) & "." & The_Name, Parent_Data);
              end if;
            end if;
          end;
        end if;
      end if;
      return The_Name;
    end Complete_Name;

  begin
    if not Is_Null (Item) and then not Is_Null (Item.Location) then
      return Complete_Name (Image_Of (Item.Location.all), Item);
    end if;
    return "";
  end Full_Name_Of;

  ----------------------------------------------------------------------------------------------------------------------

  function Parent_Library_Of (Item : Declaration_Handle) return Unit_Handle with Inline is
    The_Item : Unit_Handle;
  begin
    if Item = null then
      return null;
    end if;
    The_Item := Item.Parent;
    while not Is_Library (The_Item) loop
      The_Item := The_Item.Parent;
    end loop;
    return The_Item;
  end Parent_Library_Of;


  function In_Same_Library (Left, Right : Declaration_Handle) return Boolean with Inline is
  begin
    return Parent_Library_Of (Left) = Parent_Library_Of (Right);
  end In_Same_Library;

  ----------------------------------------------------------------------------------------------------------------------

  function Has_Key (Id         : Name.Handle;
                    In_Element : Data_Handle) return Boolean is
    use type Name.Handle;
  begin
    return Id = In_Element.Location.Id;
  end Has_Key;

  function Data_With is new List.Element_With (null, Name.Handle, Has_Key);


  function Data_With (Id   : Name.Handle;
                      From : List.Elements) return Data_Handle with Inline is
    use type Name.Handle;
  begin
    for Index in From'range loop
      if From(Index).Location.Id = Id then
        return From(Index);
      end if;
    end loop;
    return null;
  end Data_With;

  ----------------------------------------------------------------------------------------------------------------------

  function Aspect_For (Mark    : Lexical.Aspect_Id;
                       Aspects : Aspect_Specification) return Identifier_Handle is
     use type Lexical.Aspect_Id;
  begin
    for The_Aspect of Aspects loop
      if Mark = The_Aspect.Mark then
        return The_Aspect.Definition;
      end if;
    end loop;
    return null;
  end Aspect_For;


  ----------------------------------------------------------------------------------------------------------------------
  -- Global Data
  ----------------------------------------------------------------------------------------------------------------------

  The_Ada_Tag              : Data_Handle;
  The_Asm_Input            : Data_Handle;
  The_Asm_Output           : Data_Handle;
  The_Exception_Occurrence : Data_Handle;
  The_System_Address       : Data_Handle;


  procedure Initialize_Global_Data is
  begin
    The_Ada_Tag := null;
    The_Asm_Input := null;
    The_Asm_Output := null;
    The_Exception_Occurrence := null;
    The_System_Address := null;
  end Initialize_Global_Data;


  ----------------------------------------------------------------------------------------------------------------------
  -- Objects
  ----------------------------------------------------------------------------------------------------------------------

  procedure Set_Used (Item : Data_Handle) is
  begin
    if Item /= null and then Item.all in Declaration_Type'class then
      Declaration_Handle(Item).Is_Used := True;
    end if;
  end Set_Used;

  procedure Set_Used (Item : List.Item) is
  begin
    for Element of Item loop
      if Element.all in Declaration_Type'class then
        Declaration_Handle(Element).Is_Used:= True;
      end if;
    end loop;
  end Set_Used;


  ----------------------------------------------------------------------------------------------------------------------

  procedure Declare_Item (To   : in out Block_Type;
                          Item :        Data_Handle) is
  begin
    Tree.Put (Item, To => To.Declarations);
  end Declare_Item;

  procedure Declare_Item (To         : in out Block_Type;
                          Item       :        Data_Handle;
                          The_Cursor : out    Tree.Cursor) is
  begin
    Tree.Add (Data        => Item,
              To          => To.Declarations,
              Data_Cursor => The_Cursor);
  end Declare_Item;

  function Declaration_Of (Id   : Identifier_Handle;
                           From : Block_Type) return Data_Handle is
    The_Cursor : Tree.Cursor;
    use type Tree.Cursor;
  begin
    Tree.Get (Id          => Id.Id,
              From        => From.Declarations,
              Data_Cursor => The_Cursor);
    if The_Cursor /= null then
      return The_Cursor.all;
    end if;
    return null;
  end Declaration_Of;

  function Inner_Declaration_Of (Id   : Identifier_Handle;
                                 From : Block_Type) return Data_Handle renames Declaration_Of;


  ----------------------------------------------------------------------------------------------------------------------

  function Data_Kind_Of (Item : End_Identifier) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_End_Identifier;
  end Data_Kind_Of;

  ----------------------------------------------------------------------------------------------------------------------

  function Data_Kind_Of (Item : Any_Type_Declaration) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Unknown;
  end Data_Kind_Of;

  function Data_Kind_Of (Item : Array_Type) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Array_Type;
  end Data_Kind_Of;

  function Data_Kind_Of (Item : Record_Type) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Record_Type;
  end Data_Kind_Of;

  function Data_Kind_Of (Item : Interface_Type) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Interface_Type;
  end Data_Kind_Of;

  function Data_Kind_Of (Item : Root_Integer) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Integer_Type;
  end Data_Kind_Of;

  function Data_Kind_Of (Item : Discrete_Type) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Discrete_Type;
  end Data_Kind_Of;

  function Data_Kind_Of (Item : Integer_Type) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Integer_Type;
  end Data_Kind_Of;

  function Data_Kind_Of (Item : Root_Real) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Real_Type;
  end Data_Kind_Of;

  function Data_Kind_Of (Item : Real_Type) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Real_Type;
  end Data_Kind_Of;

  function Data_Kind_Of (Item : Enumeration_Type) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Enumeration_Type;
  end Data_Kind_Of;

  function Data_Kind_Of (Item : Sub_Type) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Subtype;
  end Data_Kind_Of;

  function Data_Kind_Of (Item : Access_Type) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Access_Type;
  end Data_Kind_Of;

  function Data_Kind_Of (Item : Subprogram_Access_Type) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Subprogram_Access_Type;
  end Data_Kind_Of;

  function Data_Kind_Of (Item : Root_Access) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Access_Type;
  end Data_Kind_Of;

  function Data_Kind_Of (Item : Derived_Type) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Derived_Type;
  end Data_Kind_Of;

  function Data_Kind_Of (Item : Private_Extension_Type) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Private_Extension_Type;
  end Data_Kind_Of;

  function Data_Kind_Of (Item : Incomplete_Type) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Incomplete_Type;
  end Data_Kind_Of;

  function Data_Kind_Of (Item : Private_Type) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Private_Type;
  end Data_Kind_Of;

  function Data_Kind_Of (Item : Protected_Type) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Protected_Type;
  end Data_Kind_Of;

  function Data_Kind_Of (Item : Task_Type) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Task_Type;
  end Data_Kind_Of;

  function Data_Kind_Of (Item : Synchronized_Interface_Type) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Interface_Type;
  end Data_Kind_Of;

  ----------------------------------------------------------------------------------------------------------------------

  function Data_Kind_Of (Item : Formal_Object) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Formal_Object;
  end Data_Kind_Of;

  function Data_Kind_Of (Item : Formal_Package) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Formal_Package;
  end Data_Kind_Of;

  function Data_Kind_Of (Item : Formal_Subprogram) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Formal_Subprogram;
  end Data_Kind_Of;

  function Data_Kind_Of (Item : Formal_Type) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Formal_Type;
  end Data_Kind_Of;

  ----------------------------------------------------------------------------------------------------------------------

  function Data_Kind_Of (Item : Instantiated_Item) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Instantiation;
  end Data_Kind_Of;

  ----------------------------------------------------------------------------------------------------------------------

  function Data_Kind_Of (Item : Unknown_Library) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Unknown;
  end Data_Kind_Of;

  function Resource (Item : Unknown_Library) return Resource_Handle is
  begin
    return Item.Resource;
  end Resource;

  ----------------------------------------------------------------------------------------------------------------------

  function Data_Kind_Of (Item : Block) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Block;
  end Data_Kind_Of;

  function Data_Kind_Of (Item : Private_Block) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Private_Block;
  end Data_Kind_Of;

  function Inner_Declaration_Of (Id   : Identifier_Handle;
                                 From : Private_Block) return Data_Handle is
    The_Handle : Data_Handle;
  begin
    The_Handle := Inner_Declaration_Of (Id, Block_Type(From));
    if The_Handle = null and then From.Parent /= null then
      The_Handle := Inner_Declaration_Of (Id, From.Parent.all);
    end if;
    return The_Handle;
  end Inner_Declaration_Of;

  function Data_Kind_Of (Item : Formal_Block) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Formal_Block;
  end Data_Kind_Of;

  ----------------------------------------------------------------------------------------------------------------------

  function Data_Kind_Of (Item : Package_Specification) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Package_Specification;
  end Data_Kind_Of;

  procedure Declare_Item (To   : in out Package_Specification;
                          Item :        Data_Handle) is
  begin
    Tree.Add (Item, To => To.Declarations);
  end Declare_Item;

  procedure Declare_Item (To         : in out Package_Specification;
                          Item       :        Data_Handle;
                          The_Cursor : out    Tree.Cursor) is
  begin
    Tree.Add (Data        => Item,
              To          => To.Declarations,
              Data_Cursor => The_Cursor);
  end Declare_Item;

  function Declaration_Of (Id   : Identifier_Handle;
                           From : Package_Specification) return Data_Handle is
    The_Cursor : Tree.Cursor;
    use type Tree.Cursor;
  begin
    Tree.Get (Id          => Id.Id,
              From        => From.Declarations,
              Data_Cursor => The_Cursor);
    if The_Cursor /= null then
      return The_Cursor.all;
    end if;
    return null;
  end Declaration_Of;

  function Inner_Declaration_Of (Id   : Identifier_Handle;
                                 From : Package_Specification) return Data_Handle is
    The_Cursor : Tree.Cursor;
    use type Tree.Cursor;
  begin
    if From.Private_Part = null then
      Tree.Get (Id          => Id.Id,
                From        => From.Declarations,
                Data_Cursor => The_Cursor);
    else
      Tree.Get (Id          => Id.Id,
                From        => From.Private_Part.Declarations,
                Data_Cursor => The_Cursor);
      if The_Cursor = null then
        Tree.Get (Id          => Id.Id,
                  From        => From.Declarations,
                  Data_Cursor => The_Cursor);
      end if;
    end if;
    if The_Cursor /= null then
      return The_Cursor.all;
    end if;
    return null;
  end Inner_Declaration_Of;


  function Data_Kind_Of (Item : Generic_Package_Declaration) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Generic_Package_Declaration;
  end Data_Kind_Of;

  function Declaration_Of (Id   : Identifier_Handle;
                           From : Generic_Package_Declaration) return Data_Handle is
    The_Cursor : Tree.Cursor;
    use type Tree.Cursor;
  begin
    Tree.Get (Id          => Id.Id,
              From        => From.Declarations,
              Data_Cursor => The_Cursor);
    if The_Cursor /= null then
      return The_Cursor.all;
    elsif From.Generic_Parameters /= null then
      return Declaration_Of (Id, From.Generic_Parameters.all);
    end if;
    return null;
  end Declaration_Of;

  function Inner_Declaration_Of (Id   : Identifier_Handle;
                                 From : Generic_Package_Declaration) return Data_Handle is
    The_Cursor : Tree.Cursor;
    use type Tree.Cursor;
  begin
    if From.Private_Part = null then
      Tree.Get (Id          => Id.Id,
                From        => From.Declarations,
                Data_Cursor => The_Cursor);
    else
      Tree.Get (Id          => Id.Id,
                From        => From.Private_Part.Declarations,
                Data_Cursor => The_Cursor);
      if The_Cursor = null then
        Tree.Get (Id          => Id.Id,
                  From        => From.Declarations,
                  Data_Cursor => The_Cursor);
      end if;
    end if;
    if The_Cursor /= null then
      return The_Cursor.all;
    elsif From.Generic_Parameters /= null then
      return Declaration_Of (Id, From.Generic_Parameters.all);
    end if;
    return null;
  end Inner_Declaration_Of;


  function Data_Kind_Of (Item : Package_Body) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Package_Body;
  end Data_Kind_Of;

  function Declaration_Of (Id   : Identifier_Handle;
                           From : Package_Body) return Data_Handle is
    The_Cursor : Tree.Cursor;
    use type Tree.Cursor;
  begin
    Tree.Get (Id          => Id.Id,
              From        => From.Declarations,
              Data_Cursor => The_Cursor);
    if The_Cursor /= null then
      return The_Cursor.all;
    elsif From.Specification /= null then
      return Inner_Declaration_Of (Id, From.Specification.all);
    end if;
    return null;
  end Declaration_Of;

  function Inner_Declaration_Of (Id   : Identifier_Handle;
                                 From : Package_Body) return Data_Handle renames Declaration_Of;

  ----------------------------------------------------------------------------------------------------------------------

  function Data_Kind_Of (Item : Subprogram_Declaration) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Subprogram_Declaration;
  end Data_Kind_Of;

  function Declaration_Of (Id   : Identifier_Handle;
                           From : Subprogram_Declaration) return Data_Handle is
    The_Handle : Data_Handle;
    use type List.Elements_Access;
  begin
    if From.Profile.Parameters /= null then
      The_Handle := Data_With (Id.Id, From.Profile.Parameters.all);
      if not Is_Null (The_Handle) then
        return The_Handle;
      end if;
    end if;
    return null;
  end Declaration_Of;

  function Inner_Declaration_Of (Id   : Identifier_Handle;
                                 From : Subprogram_Declaration) return Data_Handle renames Declaration_Of;


  function Data_Kind_Of (Item : Generic_Subprogram_Declaration) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Generic_Subprogram_Declaration;
  end Data_Kind_Of;

  function Data_Kind_Of (Item : Used_Subprogram) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Used_Subprogram;
  end Data_Kind_Of;


  function Data_Kind_Of (Item : Used_Generic_Subprogram) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Used_Generic_Subprogram;
  end Data_Kind_Of;


  function Data_Kind_Of (Item : Subprogram_Body) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Subprogram_Body;
  end Data_Kind_Of;

  function Declaration_Of (Id   : Identifier_Handle;
                           From : Subprogram_Body) return Data_Handle is
    The_Cursor : Tree.Cursor;
    The_Handle : Data_Handle;
    Use_Handle : Data_Handle;
    use type Tree.Cursor;
    use type List.Elements_Access;
  begin
    Tree.Get (Id          => Id.Id,
              From        => From.Declarations,
              Data_Cursor => The_Cursor);
    if The_Cursor /= null then
      if The_Cursor.all.all in Used_Subprogram'class then
        Use_Handle := The_Cursor.all;
      else
        return The_Cursor.all;
      end if;
    end if;
    if From.Profile.Parameters /= null then
      The_Handle := Data_With (Id.Id, From.Profile.Parameters.all);
      if not Is_Null (The_Handle) then
        return The_Handle;
      end if;
    end if;
    if From.Specification /= null and then From.Specification.all in Generic_Subprogram_Declaration'class then
      declare
        Formal_Part : constant Formal_Block_Handle := Generic_Subprogram_Handle(From.Specification).Generic_Parameters;
      begin
        if Formal_Part /= null then
          Tree.Get (Id          => Id.Id,
                    From        => Formal_Part.Declarations,
                    Data_Cursor => The_Cursor);
          if The_Cursor /= null then
            return The_Cursor.all;
          end if;
        end if;
      end;
    end if;
    return Use_Handle;
  end Declaration_Of;

  function Inner_Declaration_Of (Id   : Identifier_Handle;
                                 From : Subprogram_Body) return Data_Handle renames Declaration_Of;

  ----------------------------------------------------------------------------------------------------------------------

  function Data_Kind_Of (Item : Package_Instantiation) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Package_Instantiation;
  end Data_Kind_Of;

  ----------------------------------------------------------------------------------------------------------------------

  function Data_Kind_Of (Item : Package_Renaming) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Package_Renaming;
  end Data_Kind_Of;

  function Declaration_Of (Id   : Identifier_Handle;
                           From : Package_Renaming) return Data_Handle is
  begin
    if not Is_Null (From.Renamed_Item) and then From.Renamed_Item.all in Unit_Declaration'class then
      return Declaration_Of (Id, Unit_Handle(From.Renamed_Item).all);
    end if;
    return null;
  end Declaration_Of;

  function Inner_Declaration_Of (Id   : Identifier_Handle;
                                 From : Package_Renaming) return Data_Handle is
  begin
    if not Is_Null (From.Renamed_Item) and then From.Renamed_Item.all in Unit_Declaration'class then
      return Inner_Declaration_Of (Id, Unit_Handle(From.Renamed_Item).all);
    end if;
    return null;
  end Inner_Declaration_Of;

  ----------------------------------------------------------------------------------------------------------------------

  function Data_Kind_Of (Item : Generic_Package_Renaming) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Generic_Package_Renaming;
  end Data_Kind_Of;

  function Data_Kind_Of (Item : Subprogram_Renaming) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Subprogram_Renaming;
  end Data_Kind_Of;

  function Data_Kind_Of (Item : Generic_Subprogram_Renaming) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Generic_Subprogram_Renaming;
  end Data_Kind_Of;

  ----------------------------------------------------------------------------------------------------------------------

  function Resource (Item : Library_Package_Specification) return Resource_Handle is
  begin
    return Item.Resource;
  end Resource;

  function Resource (Item : Generic_Library_Package_Declaration) return Resource_Handle is
  begin
    return Item.Resource;
  end Resource;

  function Resource (Item : Library_Package_Body) return Resource_Handle is
  begin
    return Item.Resource;
  end Resource;

  function Resource (Item : Package_Subunit) return Resource_Handle is
  begin
    return Item.Resource;
  end Resource;

  function Resource (Item : Library_Package_Instantiation) return Resource_Handle is
  begin
    return Item.Resource;
  end Resource;

  function Resource (Item : Library_Package_Renaming) return Resource_Handle is
  begin
    return Item.Resource;
  end Resource;

  function Resource (Item : Generic_Library_Package_Renaming) return Resource_Handle is
  begin
    return Item.Resource;
  end Resource;

  function Resource (Item : Library_Subprogram_Declaration) return Resource_Handle is
  begin
    return Item.Resource;
  end Resource;

  function Resource (Item : Generic_Library_Subprogram_Declaration) return Resource_Handle is
  begin
    return Item.Resource;
  end Resource;

  function Resource (Item : Library_Subprogram_Body) return Resource_Handle is
  begin
    return Item.Resource;
  end Resource;

  function Resource (Item : Subprogram_Subunit) return Resource_Handle is
  begin
    return Item.Resource;
  end Resource;

  function Resource (Item : Library_Subprogram_Renaming) return Resource_Handle is
  begin
    return Item.Resource;
  end Resource;

  function Resource (Item : Generic_Library_Subprogram_Renaming) return Resource_Handle is
  begin
    return Item.Resource;
  end Resource;

  ----------------------------------------------------------------------------------------------------------------------

  function Data_Kind_Of (Item : With_Declaration) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_With_Declaration;
  end Data_Kind_Of;

  ----------------------------------------------------------------------------------------------------------------------

  procedure Declare_Item (To   : in out Active_Declaration;
                          Item :        Data_Handle) is
  begin
    Tree.Add (Item, To => To.Declarations);
  end Declare_Item;

  procedure Declare_Item (To         : in out Active_Declaration;
                          Item       :        Data_Handle;
                          The_Cursor : out    Tree.Cursor) is
  begin
    Tree.Add (Data        => Item,
              To          => To.Declarations,
              Data_Cursor => The_Cursor);
  end Declare_Item;

  function Declaration_Of (Id   : Identifier_Handle;
                           From : Active_Declaration) return Data_Handle is
    The_Cursor : Tree.Cursor;
    use type Tree.Cursor;
  begin
    Tree.Get (Id          => Id.Id,
              From        => From.Declarations,
              Data_Cursor => The_Cursor);
    if The_Cursor /= null then
      return The_Cursor.all;
    end if;
    return null;
  end Declaration_Of;

  function Inner_Declaration_Of (Id   : Identifier_Handle;
                                 From : Active_Declaration) return Data_Handle renames Declaration_Of;


  function Declaration_Of (Id   : Identifier_Handle;
                           From : Active_Body) return Data_Handle is
    The_Cursor : Tree.Cursor;
    use type Tree.Cursor;
  begin
    Tree.Get (Id          => Id.Id,
              From        => From.Declarations,
              Data_Cursor => The_Cursor);
    if The_Cursor /= null then
      return The_Cursor.all;
    elsif From.Specification /= null then
      return Inner_Declaration_Of (Id, From.Specification.all);
    end if;
    return null;
  end Declaration_Of;

  function Inner_Declaration_Of (Id   : Identifier_Handle;
                                 From : Active_Body) return Data_Handle renames Declaration_Of;


  function Data_Kind_Of (Item : Protected_Declaration) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Protected_Declaration;
  end Data_Kind_Of;

  function Data_Kind_Of (Item : Protected_Body) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Protected_Body;
  end Data_Kind_Of;


  function Data_Kind_Of (Item : Task_Declaration) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Task_Declaration;
  end Data_Kind_Of;

  function Data_Kind_Of (Item : Task_Body) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Task_Body;
  end Data_Kind_Of;


  function Data_Kind_Of (Item : Entry_Declaration) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Entry_Declaration;
  end Data_Kind_Of;

  function Data_Kind_Of (Item : Entry_Body) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Entry_Body;
  end Data_Kind_Of;


  function Data_Kind_Of (Item : Accept_Declaration) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Accept_Declaration;
  end Data_Kind_Of;

  function Declaration_Of (Id   : Identifier_Handle;
                           From : Accept_Declaration) return Data_Handle is
    The_Handle : Data_Handle;
    use type List.Elements_Access;
  begin
    if From.Profile.Parameters /= null then
      The_Handle := Data_With (Id.Id, From.Profile.Parameters.all);
      if not Is_Null (The_Handle) then
        return The_Handle;
      end if;
    end if;
    return null;
  end Declaration_Of;

  function Inner_Declaration_Of (Id   : Identifier_Handle;
                                 From : Accept_Declaration) return Data_Handle renames Declaration_Of;

  ----------------------------------------------------------------------------------------------------------------------

  function Data_Kind_Of (Item : Exception_Object) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Exception;
  end Data_Kind_Of;

  function Data_Kind_Of (Item : Enumeration_Value) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Enumeration_Value;
  end Data_Kind_Of;

  function Data_Kind_Of (Item : Data_Object) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Object;
  end Data_Kind_Of;

  function Data_Kind_Of (Item : Label) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Label;
  end Data_Kind_Of;

  ----------------------------------------------------------------------------------------------------------------------

  function Data_Kind_Of (Item : Predefined_Operator_Type) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Predefined_Operator_Type;
  end Data_Kind_Of;


  function Data_Kind_Of (Item : Predefined_Name_Id_Type) return Data_Kind is
    pragma Unreferenced (Item);
  begin
    return Is_Predefined_Pragma_Argument_Id_Type;
  end Data_Kind_Of;


  ----------------------------------------------------------------------------------------------------------------------
  -- Predefined Types
  ----------------------------------------------------------------------------------------------------------------------

  package Predefined is

    Any_Types         : Data_Handle;
    Boolean_Type      : Data_Handle;
    Operator_Call     : Data_Handle;
    Name_Id_Type      : Data_Handle;
    Root_Access_Type  : Data_Handle;
    Root_Integer_Type : Data_Handle;
    Root_Real_Type    : Data_Handle;
    Root_String_Type  : Data_Handle;

    procedure Do_Initialize;

    procedure Initialize (Standard_Library : Unit_Handle);

  end Predefined;

  function Predefined_Boolean return Data_Handle is
  begin
    return Predefined.Boolean_Type;
  end Predefined_Boolean;

  function Predefined_Root_Access return Data_Handle is
  begin
    return Predefined.Root_Access_Type;
  end Predefined_Root_Access;

  function Predefined_Root_Integer return Data_Handle is
  begin
    return Predefined.Root_Integer_Type;
  end Predefined_Root_Integer;

  function Predefined_Root_Real return Data_Handle is
  begin
    return Predefined.Root_Real_Type;
  end Predefined_Root_Real;

  function Predefined_Root_String return Data_Handle is
  begin
    return Predefined.Root_String_Type;
  end Predefined_Root_String;

  function Predefined_Operator return Data_Handle is
  begin
    return Predefined.Operator_Call;
  end Predefined_Operator;

  function Predefined_Name return Data_Handle is
  begin
    return Predefined.Name_Id_Type;
  end Predefined_Name;


  package body Predefined is

    procedure Do_Initialize is
    begin
      Operator_Call := new Predefined_Operator_Type'(Location => null);
      Name_Id_Type := new Predefined_Name_Id_Type'(Location => null);
      Any_Types := new Any_Type_Declaration'
        (Location    => null,
         Is_Used     => True,
         Parent      => null,
         Parent_Type => null);
      Root_Access_Type := new Root_Access'
        (Location    => null,
         Is_Used     => False,
         Parent      => null,
         Parent_Type => null);
      Root_Integer_Type := new Root_Integer'
        (Location    => null,
         Is_Used     => False,
         Parent      => null,
         Parent_Type => null);
      Root_Real_Type := new Root_Real'
        (Location    => null,
         Is_Used     => False,
         Parent      => null,
         Parent_Type => null);
      Root_String_Type := new Root_String'
        (Location    => null,
         Is_Used     => False,
         Parent      => null,
         Parent_Type => null,
         Definition  => new Array_Definition'(Dimension      => 1,
                                              Index_Subtypes => [Root_Integer_Type],
                                              Component_Type => Boolean_Type));
    end Do_Initialize;

    procedure Initialize (Standard_Library : Unit_Handle) is

      function Type_Handle_Of (Type_Name : String) return Data_Handle is

        The_Cursor : Tree.Cursor;

        use type Tree.Cursor;

      begin
        Tree.Get (Id          => Name.Handle_Of (Type_Name),
                  From        => Package_Specification_Handle(Standard_Library).Declarations,
                  Data_Cursor => The_Cursor);
        if The_Cursor = null then
          return null;
        else
          return The_Cursor.all;
        end if;
      end Type_Handle_Of;

    begin
      Boolean_Type := Type_Handle_Of ("Boolean");
    end Initialize;

  end Predefined;


  function Is_Root_Type (Item : Data_Handle) return Boolean with Inline is
  begin
    return Item.all in Root_Type'class | Root_String;
  end Is_Root_Type;


  function Next_Abstract_Class_Of (Item : Type_Handle) return Type_Handle;


  function Is_In_Abstract_Class (Item  : Type_Handle;
                                 Class : Type_Handle) return Boolean is

    The_Abstract_Class : Type_Handle := Next_Abstract_Class_Of (Class);

  begin
    --TEST-----------------------------------------------------------------------------------------------------
    --begin
    --  Write_Log ("Is_In_Abstract_Class - Item: " & Name.Image_Of (Item.Location.Id)
    --                               & " - Class: " & Name.Image_Of (Class.Location.Id));
    --  Write_Log ("                     - Item Type: " & Ada.Tags.External_Tag (Item.all'tag)
    --                               & " - Class Type: " & Ada.Tags.External_Tag (The_Abstract_Class.all'tag));
    --  Write_Log ("                     - Class Item: " & Name.Image_Of (The_Abstract_Class.Location.Id));
    --exception
    --when others =>
    --  null;
    --end;
    -----------------------------------------------------------------------------------------------------------
    while The_Abstract_Class /= null loop
      if The_Abstract_Class = Item then
        return True;
      end if;
      The_Abstract_Class := Next_Abstract_Class_Of (The_Abstract_Class);
    end loop;
    return False;
  end Is_In_Abstract_Class;


  function "=" (Left, Right : Data_Handle) return Boolean is

    function Address_Of is new Ada.Unchecked_Conversion (Data_Handle, System.Address);

    use type System.Address;

  begin
    if Address_Of(Left) = Address_Of(Right) then
      return True;
    elsif Is_Null (Left) or Is_Null (Right) then
      return False;
    elsif Is_Root_Type (Right) then
      if Right.all in Root_Access then
        return Left.all in Access_Type'class;
      else
        return Address_Of(Right) = Address_Of(Root_Type_Of (Left));
      end if;
    elsif Is_Root_Type (Left) then
      if Left.all in Root_Access then
        return Right.all in Access_Type'class;
      else
        return Address_Of(Left) = Address_Of(Root_Type_Of (Right));
      end if;
    elsif Left.all in Access_Type'class and Right.all in Access_Type'class then
      if Is_Null (Left.Location) or Is_Null (Right.Location) then -- at least one anonyme
        return Type_Handle(Left).Parent_Type = Type_Handle(Right).Parent_Type;
      end if;
    end if;
    if Left.all in Class_Access_Type'class and Right.all in Type_Declaration'class then
      return Is_In_Abstract_Class (Type_Handle(Right), Type_Handle(Left));
    elsif Right.all in Class_Access_Type'class and Left.all in Type_Declaration'class then
      return Is_In_Abstract_Class (Type_Handle(Left), Type_Handle(Right));
    elsif Left.all in Private_Type'class | Private_Extension_Type'class then
      return Address_Of(Right) = Address_Of(Left.Location.Data);
    elsif Right.all in Private_Type'class | Private_Extension_Type'class then
      return Address_Of(Left) = Address_Of(Right.Location.Data);
    elsif Left.all in Instantiated_Item'class and Right.all in Instantiated_Item'class then
      return Address_Of(Item_Instantiation(Left).Item) = Address_Of(Item_Instantiation(Right).Item) and then
             Item_Instantiation(Left).Instantiation = Item_Instantiation(Right).Instantiation;
    elsif Left.all in Instantiated_Item'class then
      return Item_Instantiation(Left).Item = Right;
    elsif Right.all in Instantiated_Item'class then
      return Item_Instantiation(Right).Item = Left;
    end if;
    return False;
  end "=";


  function Any_Type (Id : Identifier_Handle) return Data_Handle is
  begin
    Id.Data := Predefined.Any_Types;
    return Predefined.Any_Types;
  end Any_Type;


  function Ada_Tag return Data_Handle is

    The_Cursor : Tree.Cursor;

    use type Tree.Cursor;

  begin
    if Is_Null (The_Ada_Tag) then
      Tree.Get (Id          => Name.Handle_Of ("Tag"),
                From        => Package_Specification_Handle(Library.Ada_Tags_Unit).Declarations,
                Data_Cursor => The_Cursor);
      if The_Cursor = null then
        return null;
      else
        The_Ada_Tag := The_Cursor.all;
      end if;
    end if;
    return The_Ada_Tag;
  end Ada_Tag;


  function Asm_Input return Data_Handle is

    The_Cursor : Tree.Cursor;

    use type Tree.Cursor;

  begin
    if Is_Null (The_Asm_Input) then
      Tree.Get (Id          => Name.Handle_Of ("Asm_Input_Operand"),
                From        => Package_Specification_Handle(Library.System_Machine_Code_Unit).Declarations,
                Data_Cursor => The_Cursor);
      if The_Cursor = null then
        return null;
      else
        The_Asm_Input := The_Cursor.all;
      end if;
    end if;
    return The_Asm_Input;
  end Asm_Input;


  function Asm_Output return Data_Handle is

    The_Cursor : Tree.Cursor;

    use type Tree.Cursor;

  begin
    if Is_Null (The_Asm_Output) then
      Tree.Get (Id          => Name.Handle_Of ("Asm_Output_Operand"),
                From        => Package_Specification_Handle(Library.System_Machine_Code_Unit).Declarations,
                Data_Cursor => The_Cursor);
      if The_Cursor = null then
        return null;
      else
        The_Asm_Output := The_Cursor.all;
      end if;
    end if;
    return The_Asm_Output;
  end Asm_Output;


  function Exception_Occurrence return Data_Handle is

    The_Cursor : Tree.Cursor;

    use type Tree.Cursor;

  begin
    if Is_Null (The_Exception_Occurrence) then
      Tree.Get (Id          => Name.Handle_Of ("Exception_Occurrence"),
                From        => Package_Specification_Handle(Library.Ada_Exceptions_Unit).Declarations,
                Data_Cursor => The_Cursor);
      if The_Cursor = null then
        return null;
      else
        The_Exception_Occurrence := The_Cursor.all;
      end if;
    end if;
    return The_Exception_Occurrence;
  end Exception_Occurrence;


  function System_Address return Data_Handle is

    The_Cursor : Tree.Cursor;

    use type Tree.Cursor;

  begin
    if Is_Null (The_System_Address) then
      Tree.Get (Id          => Name.Handle_Of ("Address"),
                From        => Package_Specification_Handle(Library.System_Unit).Declarations,
                Data_Cursor => The_Cursor);
      if The_Cursor = null then
        return null;
      else
        The_System_Address := The_Cursor.all;
      end if;
    end if;
    return The_System_Address;
  end System_Address;


  ----------------------------------------------------------------------------------------------------------------------
  -- Constructors
  ----------------------------------------------------------------------------------------------------------------------

  procedure Initialize is
  begin
    Initialize_Global_Data;
    Predefined.Do_Initialize;
  end Initialize;


  procedure Add_Import (Item : Unit_Handle;
                        To   : Unit_Handle) is
  begin
    Library_Tree.Add (Resource(Item.all).Element.all, Resource(To.all).Imports);
  end Add_Import;


  procedure Add_Imports (From : Unit_Handle;
                         To   : Unit_Handle) is
  begin
    Library_Tree.Add (Resource(From.all).Imports, Resource(To.all).Imports);
  end Add_Imports;


  procedure New_Used_Subprogram (Item     : Declaration_Handle;
                                 Scope    : Unit_Handle;
                                 Instance : Instantiation_Handle);

  procedure Add_Use (Item : Data_Handle;
                     To   : Unit_Handle) is

    The_Instantiation : Instantiation_Handle;

    procedure Visit (Cursor : Tree.Cursor) is
    begin
      if Is_Subprogram (Cursor.all) then
        declare
          The_Subprogram : Declaration_Handle := Declaration_Handle(Cursor.all);
        begin
          while The_Subprogram /= null loop
            if not (The_Subprogram.all in Used_Subprogram'class) then
              declare
                The_Item : constant Data_Handle := Declaration_From (To, The_Subprogram.Location);
              begin
                if not Is_Null (The_Item) and then not (The_Item.all in Unit_Type'class) then
                  Log.Write ("%%% no override of " & Image_Of (The_Subprogram.Location.all)
                           & " by use " & Full_Name_Of (Item));
                  exit;
                else
                  New_Used_Subprogram (The_Subprogram, To, The_Instantiation);
                end if;
              end;
            end if;
            The_Subprogram := Overload_Of (The_Subprogram);
          end loop;
        end;
      end if;
    end Visit;

    procedure Add_Subprograms is new Tree.Iterator (Visit);

  begin -- Add_Use
    if To.all in Data.Library_Subprogram_Declaration'class | Data.Generic_Library_Subprogram_Declaration'class then
      Log.Write ("%%% Use package not supported for subprogram " & Full_Name_Of (Data_Handle(To)));
      return;
    end if;
    if To.Used_Packages = null then
      To.Used_Packages := new List.Item;
    end if;
    To.Used_Packages.Append (Item);
    if Item.all in Package_Specification'class then
      Add_Subprograms (Package_Specification_Handle(Item).Declarations);
    elsif Item.all in Instantiated_Item'class then
       declare
        Instance : constant Item_Instantiation := Item_Instantiation(Item);
        Unit     : constant Data_Handle := Instance.Item;
      begin
        if Unit.all in Package_Specification'class then
          The_Instantiation := Instance.Instantiation;
          Add_Subprograms (Package_Specification_Handle(Unit).Declarations);
        end if;
      end;
    end if;
  end Add_Use;


  procedure Add_Standard (Unit : Unit_Handle) is
  begin
    Predefined.Initialize (Unit);
  end Add_Standard;


  procedure Add_Unit (Self : Unit_Handle) is

    Self_Resource : constant Resource_Handle := Resource(Self.all);

    function Generation_Count return Natural is
    begin
      if Self.Parent /= null then
        declare
          The_Handle : constant Resource_Handle := Resource (Self.Parent.all);
        begin
          if The_Handle /= null and then The_Handle.Generations /= null then
            return The_Handle.Generations'length + 1;
          end if;
        end;
      end if;
      return 1;
    end Generation_Count;

    procedure Copy_Tree (From : Tree.Item;
                         To   : in out Tree.Item) is

      function Copy_Of (From_Tree : Tree.Item) return Tree.Item is
        The_Tree : Tree.Item;
      begin
        Copy_Tree (From_Tree, The_Tree);
        return The_Tree;
      end Copy_Of;

      procedure Visit_Element (Item : Tree.Cursor) is
        Element : constant With_Handle := With_Handle(Item.all);
        Handle  : constant With_Handle := new With_Declaration'(Location   => Element.Location,
                                                                Parent     => Self,
                                                                Is_Used    => False,
                                                                Unit       => Element.Unit,
                                                                Children   => Copy_Of (Element.Children),
                                                                Copy_Of    => Element,
                                                                Is_Private => Element.Is_Private);
      begin
        --TEST------------------------------------------------------------------------------------
        --Write_Log ("*** new copy of WITH - Import Unit " & Image_Of (Handle.Unit.Location.all));
        ------------------------------------------------------------------------------------------
        Tree.Add (Data_Handle(Handle), To);
      end Visit_Element;

      procedure Visit_Generations is new Tree.Iterator (Visit_Element);

    begin -- Copy_Tree
      Visit_Generations (From);
    end Copy_Tree;

    procedure Initialize (The_Generations : Generation_Access) is
    begin
      if The_Generations'length > 1 then
        declare
          Parent_Generations : constant Generation_Access := Resource (Self.Parent.all).Generations;
        begin
          for Index in The_Generations'first .. The_Generations'last - 1 loop
            The_Generations(Index).Unit := Parent_Generations(Index).Unit;
            --TEST------------------------------------------------------------------------------------------------------
            --Write_Log ("~~~" & Natural'image(Index) & " unit " & Image_Of (The_Generations(Index).Unit.Location.all));
            ------------------------------------------------------------------------------------------------------------
            Copy_Tree (Parent_Generations(Index).Children, The_Generations(Index).Children);
          end loop;
        end;
      end if;
      The_Generations(The_Generations'last) := Generation'(Unit     => Self,
                                                           Children => Tree.Empty);
    end Initialize;

    procedure Add_Instantiation with Inline is
    begin
      Initialize (Self_Resource.Generations);
      Self_Resource.Element.Unit := Self;
    end Add_Instantiation;

    procedure Add_Specification with Inline is
      Implementation : constant Unit_Body_Handle := Unit_Body_Handle(Self_Resource.Element.Unit);
    begin
      Initialize (Self_Resource.Generations);
      if Implementation /= null then
        Implementation.Specification := Unit_Declaration_Handle(Self);
        Unit_Declaration_Handle(Self).Implementation := Implementation;
      end if;
      Self_Resource.Element.Unit := Self;
    end Add_Specification;

    procedure Add_Implementation with Inline is
      The_Specification : Unit_Declaration_Handle := Unit_Declaration_Handle(Self_Resource.Element.Unit);
      use type File.Unit_Name_Handle;
    begin
      if The_Specification = null then -- no specification
        if Self.all in Subprogram_Renaming'class then
          Initialize (Self_Resource.Generations);
          Self_Resource.Element.Unit := Self; -- only implementation
          return;
        end if;
        if Self_Resource.Element.Id /= null then
          declare
            Specification_Unit : constant Unit_Handle := Library.Spec_Of (Self_Resource.Element.Id.all);
          begin
            if Specification_Unit = null then
              Initialize (Self_Resource.Generations);
              Self_Resource.Element.Unit := Self; -- only implementation
              return;
            elsif Specification_Unit.all in Unknown_Library'class then
              raise Unknown_Specification;
            end if;
            The_Specification := Unit_Declaration_Handle(Specification_Unit);
          end;
        end if;
        Self_Resource.Element.Unit := Unit_Handle(The_Specification);
      end if;
      Unit_Body_Handle(Self).Specification := The_Specification;
      The_Specification.Implementation := Unit_Body_Handle(Self);
      declare
        The_Unit : Unit_Handle := Unit_Handle(The_Specification);
      begin
        loop
          Add_Imports (From => The_Unit, To => Self);
          exit when The_Unit.Parent = null;
          The_Unit := The_Unit.Parent;
        end loop;
      end;
      declare
        Body_Generations : constant Generation_Access := Self_Resource.Generations;
        Spec_Generations : constant Generation_Access := Resource (The_Specification.all).Generations;
      begin
        for Index in Body_Generations'range loop
          --TEST------------------------------------------------------------------------------------------------------
          --Write_Log ("~~~" & Natural'image(Index) & " get " & Image_Of (Spec_Generations(Index).Unit.Location.all));
          ------------------------------------------------------------------------------------------------------------
          Body_Generations(Index) := Generation'(Unit     => Spec_Generations(Index).Unit,
                                                 Children => Tree.Empty);
          Copy_Tree (Spec_Generations(Index).Children, Body_Generations(Index).Children);
        end loop;
        Body_Generations(Body_Generations'last).Unit := Self;
      end;
      if The_Specification.Used_Packages /= null then
        if Self.Used_Packages = null then
          Self.Used_Packages := new List.Item;
        end if;
        Self.Used_Packages.Append (List.Copy_Of(The_Specification.Used_Packages.all));
      end if;
      if The_Specification.all in Package_Specification'class then
        declare
          Private_Part : constant Block_Handle := Package_Specification_Handle(The_Specification).Private_Part;
        begin
          if Private_Part /= null and then Private_Part.Used_Packages /= null then
            if Self.Used_Packages = null then
              Self.Used_Packages := new List.Item;
            end if;
            Self.Used_Packages.Append (List.Copy_Of(Private_Part.Used_Packages.all));
          end if;
        end;
      end if;
    end Add_Implementation;

    procedure Add_Subunit with Inline is
      The_Unit : Unit_Handle;
    begin
      Initialize (Self_Resource.Generations);
      Self_Resource.Element.Unit := Self; -- only implementation
      if Self.Parent /= null and then Self.Parent.all in Unit_Declaration'class then
        The_Unit := Unit_Handle(Unit_Declaration_Handle(Self.Parent).Implementation);
        if The_Unit = null then
          The_Unit := Library.Body_Of (Self.Parent);
        end if;
      else
        The_Unit := Self.Parent;
      end if;
      if The_Unit = null then
        Log.Write ("%%% no parent found for subunit: " & Full_Name_Of (Data_Handle(Self)));
      else
        Self.Parent := The_Unit;
        --TEST--------------------------------------------------------------------------------
        --Log.Write ("~~~ new parent found for subunit: " & Full_Name_Of (Data_Handle(Self)));
        --------------------------------------------------------------------------------------
      end if;
      while The_Unit /= null loop
        Add_Imports (From => The_Unit, To => Self);
        The_Unit := The_Unit.Parent;
      end loop;
    end Add_Subunit;

  begin -- Add_Unit
    Self_Resource.Generations := new Generations(1..Generation_Count);
    if Self.all in Unit_Body'class then
      if Self.all in Subprogram_Subunit'class | Package_Subunit'class then
        --TEST-------------------------------------------------------------------
        --Write_Log ("~~~ Add_Unit subunit " & Full_Name_Of (Data_Handle(Self)));
        -------------------------------------------------------------------------
        Add_Subunit;
        --TEST------------------------------------------------------------------------
        --Write_Log ("~~~ Add_Unit subunit done " & Full_Name_Of (Data_Handle(Self)));
        ------------------------------------------------------------------------------
      else
        --TEST----------------------------------------------------------------
        --Write_Log ("~~~ Add_Unit body " & Full_Name_Of (Data_Handle(Self)));
        ----------------------------------------------------------------------
        Add_Implementation;
        --TEST---------------------------------------------------------------------
        --Write_Log ("~~~ Add_Unit body done " & Full_Name_Of (Data_Handle(Self)));
        ---------------------------------------------------------------------------
      end if;
    elsif Self.all in Package_Instantiation'class then
      --TEST----------------------------------------------------------------
      --Write_Log ("~~~ Add_Unit inst " & Full_Name_Of (Data_Handle(Self)));
      ----------------------------------------------------------------------
      Add_Instantiation;
      --TEST---------------------------------------------------------------------
      --Write_Log ("~~~ Add_Unit inst done " & Full_Name_Of (Data_Handle(Self)));
      ---------------------------------------------------------------------------
    else
      --TEST----------------------------------------------------------------
      --Write_Log ("~~~ Add_Unit spec " & Full_Name_Of (Data_Handle(Self)));
      ----------------------------------------------------------------------
      Add_Specification;
      --TEST---------------------------------------------------------------------
      --Write_Log ("~~~ Add_Unit spec done " & Full_Name_Of (Data_Handle(Self)));
      ---------------------------------------------------------------------------
    end if;
  end Add_Unit;


  procedure Import (Id         : Identifiers;
                    Self       : Unit_Handle;
                    Is_Private : Boolean) is

    Self_Resource   : constant Resource_Handle := Resource (Self.all);
    The_Generations : constant Generation_Access := Self_Resource.Generations;

    No_Library : exception;

    The_Handle : With_Handle;

    Last_Generation : Positive := Positive'first;

  begin --Import
    --TEST---------------------------------------
    --Write_Log ("*** Import: " & Image_Of (Id));
    ---------------------------------------------
    for Index in Id'range loop
      declare

        The_Library : Unit_Handle;

        function Is_Generation return Boolean with Inline is
          use type Name.Handle;
        begin
          if The_Generations'last < (Index + 1) then
            return False;
          else
            The_Library := The_Generations(Index + 1).Unit;
            return Id(Index).Id = The_Library.Location.Id;
          end if;
        end Is_Generation;

        function Found_In (The_Data : Tree.Item) return Boolean with Inline is
          The_Cursor : Tree.Cursor;
          use type Tree.Cursor;
        begin
          Tree.Get (Id          => Id(Index).Id,
                    From        => The_Data,
                    Data_Cursor => The_Cursor);
          if The_Cursor /= null then
            The_Handle := With_Handle(The_Cursor.all);
            The_Handle.Location := Id(Index);
            The_Handle.Copy_Of := null; -- use local import
            Id(Index).Data := Data_Handle(The_Handle.Unit);
            return True;
          end if;
          return False;
        end Found_In;

        procedure Add_Library_To (The_Tree : in out Tree.Item) with Inline is
        begin
          The_Library := Library.Unit_Of (Name_List_Of (Id(1 .. Index)));
          if The_Library = null then
            raise No_Library;
          end if;
          --TEST--------------------------------------------------
          --Write_Log ("*** new WITH " & Image_Of (Id(1..Index)));
          --------------------------------------------------------
          The_Handle := new With_Declaration'(Location   => Id(Index),
                                              Parent     => Self,
                                              Is_Used    => False,
                                              Unit       => The_Library,
                                              Children   => Tree.Empty,
                                              Copy_Of    => null,
                                              Is_Private => Is_Private);
          The_Handle.Location.Data := Data_Handle(The_Handle);
          Tree.Add (Data => Data_Handle(The_Handle),
                    To   => The_Tree);
          Add_Import (Item => The_Library, To => Self); -- Legacy use for library dependancy
        end Add_Library_To;

      begin
        if The_Handle /= null then
          if not Found_In (The_Handle.Children) then
            --TEST-------------------------------------------------------------------------------
            --Write_Log ("~~~ Add to Child " & Image_Of (The_Handle.Unit.Location.all) &
            --           " unit " & Image_Of (Id(1..Index)));
            --Write_Log ("    PARENT TYPE " & Ada.Tags.External_Tag (The_Handle.Parent.all'tag));
            -------------------------------------------------------------------------------------
            Add_Library_To (The_Handle.Children);
          --TEST--------------------------------------------------------------------------
          --else
          --  Write_Log ("~~~ Found in Child " & Image_Of (The_Handle.Unit.Location.all) &
          --             " unit " & Image_Of (Id(Index).all));
          --------------------------------------------------------------------------------
          end if;
        elsif Is_Generation then
          Id(Index).Data := Data_Handle(The_Library);
          Last_Generation := Index + 1;
          --TEST-------------------------------------------------------------------
          --Write_Log ("~~~ Is Generation " & Image_Of (The_Library.Location.all) &
          --           " at" & Natural'image(Last_Generation));
          -------------------------------------------------------------------------
          Add_Import (Item => The_Library, To => Self); -- %%% Legacy
        else
          if Index = Id'last then
            --TEST------------------------------------------------------------------------------------------------------
            --Write_Log ("~~~ Add last to Generation " & Image_Of (The_Generations(Last_Generation).Unit.Location.all) &
            --           " unit " & Image_Of (Id(Index).all));
            ------------------------------------------------------------------------------------------------------------
            Add_Library_To (The_Generations(Last_Generation).Children);
          elsif not Found_In (The_Generations(Last_Generation).Children) then
            --TEST-------------------------------------------------------------------------------------------------
            --Write_Log ("~~~ Add to Generation " & Image_Of (The_Generations(Last_Generation).Unit.Location.all) &
            --           " unit " & Image_Of (Id(Index).all));
            -------------------------------------------------------------------------------------------------------
            Add_Library_To (The_Generations(Last_Generation).Children);
          --TEST-----------------------------------------------------------------------------------------------
          --else
          --  Write_Log ("~~~ In Generation " & Image_Of (The_Generations(Last_Generation).Unit.Location.all));
          -----------------------------------------------------------------------------------------------------
          end if;
        end if;
      end;
    end loop;
    ------------------------------------
    --Write_Log ("*** Import complete");
    ------------------------------------
  exception
  when No_Library =>
    null;
  end Import;


  procedure Use_Package (Item  : Data_Handle;
                         Scope : Unit_Handle) is
  begin
    if Item /= null then
      if Item.all in Package_Specification'class or else Item.all in Instantiated_Item'class then
        --TEST------------------------------------------------
        --Write_Log (">>> Use_Package " & Full_Name_Of(Item));
        ------------------------------------------------------
        Add_Use (Item, To => Scope);
      --TEST-----------------------------------------------------------------------------------------------
      --else
      --  Write_Log (">>> Use " & Full_Name_Of(Item) & " - type: " & Ada.Tags.External_Tag (Item.all'tag));
      -----------------------------------------------------------------------------------------------------
      end if;
    end if;
  end Use_Package;


  procedure Use_Type (Item    : Data_Handle;
                      Scope   : Unit_Handle;
                      Use_All : Boolean) is

    procedure Visit (Cursor : Tree.Cursor) is
      Enumeration : constant Enumeration_Value_Handle := Enumeration_Value_Handle(Cursor.all);
    begin
      New_Enumeration_Value (Enumeration.Location, Scope, Item);
    end Visit;

    procedure Add_Subprograms is new Tree.Iterator (Visit);

  begin
    if Item /= null then
      --TEST---------------------------------------------
      --Write_Log ("*** Use_Type " & Full_Name_Of(Item));
      ---------------------------------------------------
      if Use_All then
        if Item.all in Enumeration_Type'class then
          Add_Subprograms (Enumeration_Handle(Item).Values);
        end if;
      end if;
    end if;
  end Use_Type;

  ----------------------------------------------------------------------------------------------------------------------

  function New_Unknown_Library (Resource : Resource_Handle) return Unit_Handle is
  begin
    return new Unknown_Library'(Location      => null,
                                Is_Used       => False,
                                Parent        => null,
                                Resource      => Resource,
                                Used_Packages => null);
  end New_Unknown_Library;

  ----------------------------------------------------------------------------------------------------------------------

  function Parent_Of (Id : Identifiers) return Unit_Handle is
  begin
    if Id'length > 1 then
      declare
        Parent_Id      : constant Identifiers := Id(1..Id'last-1);
        Parent_Library : constant Unit_Handle := Library.Unit_Of (Name_List_Of (Parent_Id));
        Parent_Handle  : Unit_Handle := Parent_Library;
      begin
        if Parent_Handle = null then
          return null;
        end if;
        for Index in reverse Parent_Id'range loop
          Id(Index).Data := Data_Handle(Parent_Handle);
          Parent_Handle := Parent_Handle.Parent;
        end loop;
        return Parent_Library;
      end;
    elsif Name.Image_Of (Id(1).Id) = File.Standard_Name then
      return null;
    else
      return Library.Standard_Unit;
    end if;
  end Parent_Of;


  function "=" (Left, Right : Subprogram_Profile) return Boolean is
    use type List.Elements_Access;
  begin
    if Type_Of (Left.Result_Type) /= Type_Of (Right.Result_Type) then
      --TEST----------------------------------------
      --Write_Log ("- LEFT RESULT <> RIGHT RESULT");
      ----------------------------------------------
      return False;
    elsif (Left.Parameters = null) and (Right.Parameters = null) then
      return True;
    elsif (Left.Parameters = null) or (Right.Parameters = null) then
      --TEST---------------------------------
      --Write_Log ("- LEFT OR RIGHT : NULL");
      ---------------------------------------
      return False;
    elsif Left.Parameters'length /= Right.Parameters'length then
      --TEST----------------------------------------
      --Write_Log ("- LEFT LENGTH <> RIGHT LENGTH");
      ----------------------------------------------
      return False;
    end if;
    for Index in Left.Parameters'range loop
      declare
        Left_Type  : constant Data_Handle := Object_Handle(Left.Parameters(Index)).Object_Type;
        Right_Type : constant Data_Handle := Object_Handle(Right.Parameters(Index)).Object_Type;
      begin
        --TEST------------------------------------------------------------------------
        --if Is_Null (Left_Type) then
        --  Write_Log ("- LEFT  : UNKNOWN TYPE");
        --else
        --  if Is_Null (Left_Type.Location) then
        --    Write_Log ("- LEFT : NO NAME");
        --  else
        --    Write_Log ("- LEFT : " & Image_Of (Left_Type.Location.all));
        --  end if;
        --  Write_Log ("       TYPE : " & Ada.Tags.External_Tag (Left_Type.all'tag));
        --end if;
        --if Is_Null (Right_Type) then
        --  Write_Log ("- RIGHT  : UNKNOWN TYPE");
        --else
        --  if Is_Null (Right_Type.Location) then
        --    Write_Log ("- RIGHT  : UNKNOWN NAME");
        --  else
        --    Write_Log ("- RIGHT  : " & Image_Of (Right_Type.Location.all));
        --  end if;
        --  Write_Log ("       TYPE : " & Ada.Tags.External_Tag (Right_Type.all'tag));
        --end if;
        ------------------------------------------------------------------------------
        if Type_Of(Left_Type) /= Type_Of(Right_Type) then
          return False;
        end if;
      end;
    end loop;
    return True;
  end "=";


  procedure Set_Parameters_Used (Profile : Subprogram_Profile)  is
    use type List.Elements_Access;
  begin
    if Profile.Parameters /= null then
      for Index in Profile.Parameters'range loop
        declare
          The_Parameter : constant Data_Handle := Profile.Parameters(Index);
        begin
          if not Is_Null (The_Parameter) and then The_Parameter.all in Declaration_Type'class then
            Declaration_Handle(Profile.Parameters(Index)).Is_Used:= True;
          end if;
        end;
      end loop;
    end if;
  end Set_Parameters_Used;


  function Used (Profile : Subprogram_Profile) return Subprogram_Profile is
  begin
    Set_Parameters_Used (Profile);
    return Profile;
  end Used;


  procedure Set_Profile_Used (Unit : Unit_Handle) is
    Profile : constant Subprogram_Profile := Profile_Of (Declaration_Handle(Unit));
  begin
    Set_Parameters_Used (Profile);
  end Set_Profile_Used;


  procedure Handle_Aspects (Id : Identifier_Handle) is
    The_Object : constant Object_Handle := Object_Handle(Id.Data);
    The_Handle : constant Data_Handle := The_Object.Object_Type;
    Aggregate  : Aggregate_Aspect_Handle;
  begin
    if The_Handle /= null then
      if The_Handle.all in Private_Type'class then
        declare
          Aspects : constant Private_Aspect_Handle := Private_Type_Handle(The_Handle).Aspects;
        begin
          if Aspects /= null then
            Aggregate := Aspects.Aggregate;
          end if;
        end;
      elsif The_Handle /= null and then The_Handle.all in Derived_Type'class then
        Aggregate := Derived_Type_Handle(The_Handle).Aggregate;
      end if;
      if Aggregate /= null and then not Is_Null (Aggregate.Empty) and then Aggregate.Empty = The_Object.Location then
        Aggregate.Empty.Data := Id.Data;
        The_Object.Is_Used := True;
      end if;
    end if;
  end Handle_Aspects;


  procedure Handle_Aspects (Profile : Subprogram_Profile;
                            Unit    : Unit_Handle) is

    procedure Handle_Aggregate (Aggregate : Aggregate_Aspect_Handle) is
    begin
      if Aggregate /= null then
        if not Is_Null (Aggregate.Add_Named) and then
          Aggregate.Add_Named = Unit.Location
        then
          Aggregate.Add_Named.Data := Data_Handle(Unit);
          Unit.Is_Used := True;
        elsif not Is_Null (Aggregate.Add_Unnamed) and then
          Aggregate.Add_Unnamed = Unit.Location
        then
          Aggregate.Add_Unnamed.Data := Data_Handle(Unit);
          Unit.Is_Used := True;
        elsif not Is_Null (Aggregate.Assign_Indexed) and then
          Aggregate.Assign_Indexed = Unit.Location
        then
          Aggregate.Assign_Indexed.Data := Data_Handle(Unit);
          Unit.Is_Used := True;
        end if;
      end if;
    end Handle_Aggregate;

    procedure Handle_Aggregate_Result (Aggregate : Aggregate_Aspect_Handle) is
    begin
      if Aggregate /= null then
        if not Is_Null (Aggregate.Empty) and then
          Aggregate.Empty = Unit.Location
        then
          Aggregate.Empty.Data := Data_Handle(Unit);
          Unit.Is_Used := True;
        elsif not Is_Null (Aggregate.New_Indexed) and then
          Aggregate.New_Indexed = Unit.Location
        then
          Aggregate.New_Indexed.Data := Data_Handle(Unit);
          Unit.Is_Used := True;
        end if;
      end if;
    end Handle_Aggregate_Result;

    procedure Handle_Iterable (Aspects : Iterable_Aspect_Handle) is
    begin
      if Aspects /= null then
        if not Is_Null (Aspects.Element) and then Aspects.Element = Unit.Location then
          Aspects.Element.Data := Data_Handle(Unit);
          Unit.Is_Used := True;
        elsif not Is_Null (Aspects.Has_Element) and then Aspects.Has_Element = Unit.Location then
          Aspects.Has_Element.Data := Data_Handle(Unit);
          Unit.Is_Used := True;
        elsif not Is_Null (Aspects.First) and then Aspects.First = Unit.Location then
          Aspects.First.Data := Data_Handle(Unit);
          Unit.Is_Used := True;
        elsif not Is_Null (Aspects.Next) and then Aspects.Next = Unit.Location then
          Aspects.Next.Data := Data_Handle(Unit);
          Unit.Is_Used := True;
        end if;
      end if;
    end Handle_Iterable;

    use type List.Elements_Access;

  begin -- Handle_Aspects
    if Profile.Parameters /= null then
      declare
        First_Type : constant Data_Handle := Object_Handle(Profile.Parameters(Profile.Parameters'first)).Object_Type;
      begin
        if not Is_Null (First_Type) then
          if First_Type.all in Private_Type'class then
            declare
              Handle  : constant Private_Type_Handle := Private_Type_Handle(First_Type);
              Aspects : constant Private_Aspect_Handle := Handle.Aspects;
            begin
              if Aspects /= null then
                if not Is_Null (Aspects.Iterator.Constant_Indexing) and then
                  Aspects.Iterator.Constant_Indexing = Unit.Location
                then
                  Aspects.Iterator.Constant_Indexing.Data := Data_Handle(Unit);
                  Unit.Is_Used := True;
                elsif not Is_Null (Aspects.Iterator.Variable_Indexing) and then
                  Aspects.Iterator.Variable_Indexing  = Unit.Location
                then
                  Aspects.Iterator.Variable_Indexing.Data := Data_Handle(Unit);
                  Unit.Is_Used := True;
                elsif not Is_Null (Aspects.Iterator.Default_Iterator) and then
                  Aspects.Iterator.Default_Iterator = Unit.Location
                then
                  Aspects.Iterator.Default_Iterator.Data := Data_Handle(Unit);
                  Unit.Is_Used := True;
                else
                  Handle_Aggregate (Aspects.Aggregate);
                end if;
                Handle_Iterable (Aspects.Iterable);
              end if;
            end;
          elsif First_Type.all in Derived_Type'class then
            Handle_Aggregate (Derived_Type_Handle(First_Type).Aggregate);
          elsif First_Type.all in Record_Type'class then
            Handle_Iterable (Record_Handle(First_Type).Aspects);
          end if;
        end if;
      end;
    elsif Profile.Result_Type /= null then
      declare
        Result_Type : constant Data_Handle := Profile.Result_Type;
      begin
        if Result_Type.all in Private_Type'class then
          declare
            Handle  : constant Private_Type_Handle := Private_Type_Handle(Result_Type);
          begin
            if Handle.Aspects /= null then
              Handle_Aggregate_Result (Handle.Aspects.Aggregate);
            end if;
          end;
        elsif Result_Type.all in Derived_Type'class then
          Handle_Aggregate_Result (Derived_Type_Handle(Result_Type).Aggregate);
        end if;
      end;
    end if;
  end Handle_Aspects;


  procedure Handle_Methods (Profile : Subprogram_Profile;
                            Unit    : Unit_Handle) is

    The_Type : Data_Handle;

    procedure Prepend_Unit_To (The_Methods : in out List.Item) is
    begin
      --TEST---------------------------------------------------------
      --Write_Log ("*** add Method " & Image_Of (Unit.Location.all));
      ---------------------------------------------------------------
      if Unit.Parent = Data.Type_Handle(The_Type).Parent then
        The_Methods.Prepend (Data_Handle(Unit));
        Unit.Is_Used := True;
      elsif Unit.Parent.Data_Kind_Of = Is_Private_Block then
        if Unit.Parent.Parent = Data.Type_Handle(The_Type).Parent then
          The_Methods.Prepend (Data_Handle(Unit));
        end if;
      end if;
    end Prepend_Unit_To;

    use type List.Elements_Access;

  begin -- Handle_Methods
    if Profile.Parameters /= null and then Profile.Parameters'length > 0 then
      declare
        The_Parameter : constant Data_Handle := Profile.Parameters(Profile.Parameters'first);
      begin
        if The_Parameter /= null and then The_Parameter.all in Data_Object'class then
          The_Type := Object_Handle(The_Parameter).Object_Type;
          if The_Type /= null then
            if The_Type.all in Access_Type'class then
              The_Type := Data.Type_Handle(The_Type).Parent_Type;
              if The_Type = null then
                return;
              end if;
            end if;
            if The_Type.all in Tagged_Record_Type'class then
              Prepend_Unit_To (Tagged_Record_Handle(The_Type).Methods);
            elsif The_Type.all in Tagged_Private_Type'class then
              Prepend_Unit_To (Tagged_Private_Handle(The_Type).Methods);
            elsif The_Type.all in Interface_Type'class then
              Prepend_Unit_To (Interface_Handle(The_Type).Methods);
            end if;
          end if;
        end if;
      end;
    end if;
  end Handle_Methods;


  procedure Update_Reference_Of (Subprogram : Unit_Handle;
                                 Profile    : Subprogram_Profile;
                                 Renamed_Id : Identifier_Handle) is
    The_Subprogram : Declaration_Handle := Declaration_Handle(Subprogram);
  begin
    while The_Subprogram /= null loop
      if Matches (Profile, Profile_Of (The_Subprogram), Instantiation_Handle(Renamed_Id.Data)) then
        Renamed_Id.Data := Data_Handle(The_Subprogram);
        The_Subprogram.Is_Used := True;
        return;
      end if;
      The_Subprogram := Overload_Of (The_Subprogram);
    end loop;
    if Renamed_Id.all in Operator_Symbol'class then --%%%
      Renamed_Id.Data := Data.Predefined_Operator;
    else
      Renamed_Id.Data := null;
    end if;
  end Update_Reference_Of;


  function Updated_Subprogram_Body (Item : Subprogram_Body_Handle;
                                    From : Generic_Subprogram_Handle) return Boolean with Inline is
    use type Lexical.Element;
    use type List.Elements_Access;
    The_Token : Lexical_Handle := Lexical_Handle(Item.Location);
  begin
    if (Item.Profile.Parameters /= null) and then (From.Profile.Parameters /= null) and then
      Item.Profile.Parameters'length = From.Profile.Parameters'length
    then
      for The_Index in Item.Profile.Parameters'range loop
        declare
          Item_Parameter : constant Object_Handle := Object_Handle(Item.Profile.Parameters(The_Index));
          From_Parameter : constant Object_Handle := Object_Handle(From.Profile.Parameters(The_Index));
        begin
          if Item_Parameter.Object_Type /= From_Parameter.Object_Type then
            if Item_Parameter.Location = From_Parameter.Location then -- search parameter type
              The_Token := Lexical_Handle(Item_Parameter.Location);
              loop
                The_Token := Lexical_After (The_Token);
                exit when The_Token.Element = Lexical.Colon;
              end loop;
              loop
                The_Token := Lexical_After (The_Token);
                exit when The_Token.Element = Lexical.Identifier;
              end loop;
              declare
                The_Type : Data_Handle := From_Parameter.Object_Type;
              begin
                if not Is_Null (The_Type) then
                  if The_Type.all in Access_Type'class then
                    The_Type := Type_Handle(The_Type).Parent_Type;
                  end if;
                  if not Is_Null (The_Type) and then not Is_Null (The_Type.Location) then
                    if The_Type.Location = Identifier_Handle(The_Token) then
                      Item_Parameter.Object_Type := From_Parameter.Object_Type;
                      Identifier_Handle(The_Token).Data := The_Type;
                    end if;
                  end if;
                end if;
              end;
            else
              return False; -- no match
            end if;
          end if;
        end;
      end loop;
    elsif (Item.Profile.Parameters /= null) or (From.Profile.Parameters /= null) then
      return False;
    end if;
    if Item.Profile.Result_Type = From.Profile.Result_Type then
      return True;
    else
      loop -- search return type
        The_Token := Lexical_After (The_Token);
        exit when The_Token.Element = Lexical.Is_Return;
        if The_Token.Element = Lexical.Is_Is then
          return True; -- is procedure
        end if;
      end loop;
      The_Token := Lexical_After (The_Token);
      if not Is_Null (From.Profile.Result_Type) and then
        (Is_Null (From.Profile.Result_Type.Location) or else
         From.Profile.Result_Type.Location = Identifier_Handle(The_Token))
      then
        Item.Profile.Result_Type := From.Profile.Result_Type;
        Identifier_Handle(The_Token).Data := From.Profile.Result_Type;
        return True;
      end if;
    end if;
    return False;
  end Updated_Subprogram_Body;

  ----------------------------------------------------------------------------------------------------------------------

  function New_Block (Id     : Identifier_Handle;
                      Parent : Unit_Handle) return Unit_Handle is

    Unit : constant Unit_Handle := new Block'(Location      => Id,
                                              Is_Used       => False,
                                              Parent        => Parent,
                                              Used_Packages => null,
                                              Declarations  => Tree.Empty);
  begin
    if not Is_Null (Id) then
      --TEST---------------------------------------------------------
      --Write_Log ("*** new Block_Declaration " & Image_Of (Id.all));
      ---------------------------------------------------------------
      Id.Data := Data_Handle(Unit);
    end if;
    return Unit;
  end New_Block;


  function New_Exception_Block (Id     : Identifier_Handle;
                                Parent : Unit_Handle) return Unit_Handle is

    Unit : constant Unit_Handle := new Block'(Location      => null,
                                              Is_Used       => False,
                                              Parent        => Parent,
                                              Used_Packages => null,
                                              Declarations  => Tree.Empty);
  begin
    --TEST-------------------------------------------------------------------
    --Write_Log ("*** new Exception_Block_Declaration " & Image_Of (Id.all));
    -------------------------------------------------------------------------
    New_Object (Id            => Id,
                Subtype_Mark  => Exception_Occurrence,
                Is_Class_Wide => False,
                Has_Default   => False,
                Parent        => Unit);
    return Unit;
  end New_Exception_Block;


  function New_Private_Part (Visible_Part : Unit_Handle) return Unit_Handle is

    The_Block : constant Block_Handle := new Private_Block'(Location      => Visible_Part.Location,
                                                            Is_Used       => False,
                                                            Parent        => Visible_Part,
                                                            Used_Packages => null,
                                                            Declarations  => Tree.Empty);
  begin
    --TEST-----------------------------------------------------------------------
    --Write_Log ("*** new Private_Part " & Image_Of (Visible_Part.Location.all));
    -----------------------------------------------------------------------------
    Package_Specification_Handle(Visible_Part).Private_Part := The_Block;
    return Unit_Handle(The_Block);
  end New_Private_Part;


  function New_Formal_Part (Parent : Unit_Handle) return Formal_Block_Handle is
  begin
    --TEST------------------------------
    --Write_Log ("*** new Formal_Part");
    ------------------------------------
    return new Formal_Block'(Location      => null,
                             Is_Used       => False,
                             Parent        => Parent,
                             Used_Packages => null,
                             Declarations  => Tree.Empty,
                             Last_Position => 0);
  end New_Formal_Part;

  ----------------------------------------------------------------------------------------------------------------------

  function New_Generic_Library_Package_Declaration (Id       : Identifier_List;
                                                    Resource : Resource_Handle) return Unit_Handle is
  begin
    --TEST---------------------------------------------------------------------------
      Write_Log ("*** new Generic_Library_Package_Declaration " & Image_Of (Id.all));
    ---------------------------------------------------------------------------------
    return new Generic_Library_Package_Declaration'(Location           => Id(Id'last),
                                                    Resource           => Resource,
                                                    Is_Used            => False,
                                                    Parent             => Parent_Of (Id.all),
                                                    Used_Packages      => null,
                                                    Declarations       => Tree.Empty,
                                                    Implementation     => null,
                                                    Private_Part       => null,
                                                    Generic_Parameters => null);
  end New_Generic_Library_Package_Declaration;


  procedure Add_Generic_Library_Package_Declaration (Self               : Unit_Handle;
                                                     Generic_Parameters : Formal_Block_Handle) is
  begin
    --TEST--------------------------------------------------------------------------------------
      Write_Log ("*** add Generic_Library_Package_Declaration " & Image_Of (Self.Location.all));
    --------------------------------------------------------------------------------------------
    Generic_Package_Handle(Self).Generic_Parameters := Generic_Parameters;
    Self.Location.Data := Data_Handle(Self);
  end Add_Generic_Library_Package_Declaration;


  function New_Library_Package_Specification (Id         : Identifier_List;
                                              Resource   : Resource_Handle;
                                              Is_Private : Boolean) return Unit_Handle is
  begin
    --TEST---------------------------------------------------------------------
      Write_Log ("*** new Library_Package_Specification " & Image_Of (Id.all));
    ---------------------------------------------------------------------------
    return new Library_Package_Specification'(Location       => Id(Id'last),
                                              Resource       => Resource,
                                              Is_Private     => Is_Private,
                                              Is_Used        => False,
                                              Parent         => Parent_Of (Id.all),
                                              Used_Packages  => null,
                                              Declarations   => Tree.Empty,
                                              Implementation => null,
                                              Private_Part   => null);
  end New_Library_Package_Specification;


  procedure Add_Library_Package_Specification (Self : Unit_Handle) is
  begin
    --TEST--------------------------------------------------------------------------------
      Write_Log ("*** add Library_Package_Specification " & Image_Of (Self.Location.all));
    --------------------------------------------------------------------------------------
    Self.Location.Data := Data_Handle(Self);
  end Add_Library_Package_Specification;


  function New_Library_Package_Body (Id       : Identifier_List;
                                     Resource : Resource_Handle) return Unit_Handle is
  begin
    --TEST------------------------------------------------------------
      Write_Log ("*** new Library_Package_Body " & Image_Of (Id.all));
    ------------------------------------------------------------------
    return new Library_Package_Body'(Location      => Id(Id'last),
                                     Resource      => Resource,
                                     Is_Used       => True,
                                     Parent        => Parent_Of (Id.all),
                                     Used_Packages => null,
                                     Declarations  => Tree.Empty,
                                     Specification => null);
  end New_Library_Package_Body;


  function New_Package_Subunit (Id       : Identifier_List;
                                Resource : Resource_Handle) return Unit_Handle is
  begin
    --TEST-------------------------------------------------------
      Write_Log ("*** new Package_Subunit " & Image_Of (Id.all));
    -------------------------------------------------------------
    return new Package_Subunit'(Location      => Id(Id'last),
                                Resource      => Resource,
                                Is_Used       => True,
                                Parent        => Parent_Of (Id.all),
                                Used_Packages => null,
                                Declarations  => Tree.Empty,
                                Specification => null);
  end New_Package_Subunit;


  procedure Add_Library_Package_Body (Self : Unit_Handle) is
    The_Specification : Unit_Declaration_Handle;
  begin
    Self.Location.Data := Data_Handle(Self);
    if Self.all in Package_Subunit'class then
      --TEST------------------------------------------------------------------
        Write_Log ("*** add Package_Subunit " & Image_Of (Self.Location.all));
      ------------------------------------------------------------------------
    else
      --TEST-----------------------------------------------------------------------
        Write_Log ("*** add Library_Package_Body " & Image_Of (Self.Location.all));
      -----------------------------------------------------------------------------
      if Library_Body_Handle(Self).Specification /= null then
        The_Specification := Library_Body_Handle(Self).Specification;
        while The_Specification /= null loop
          declare
            Private_Part : constant Block_Handle := Package_Specification_Handle(The_Specification).Private_Part;
          begin
            if Private_Part /= null and then Private_Part.Used_Packages /= null then
              if Self.Used_Packages = null then
                Self.Used_Packages := new List.Item;
              end if;
              Self.Used_Packages.Append (List.Copy_Of(Private_Part.Used_Packages.all));
            end if;
          end;
          if The_Specification.Parent /= null and then The_Specification.Parent.all in Package_Specification'class then
            The_Specification := Unit_Declaration_Handle(The_Specification.Parent);
          else
            exit;
          end if;
        end loop;
      end if;
    end if;
  end Add_Library_Package_Body;


  function New_Generic_Library_Subprogram_Declaration (Id       : Identifier_List;
                                                       Resource : Resource_Handle) return Unit_Handle is
  begin
    --TEST------------------------------------------------------------------------------
      Write_Log ("*** new Generic_Library_Subprogram_Declaration " & Image_Of (Id.all));
    ------------------------------------------------------------------------------------
    return new Generic_Library_Subprogram_Declaration'(Location           => Id(Id'last),
                                                       Resource           => Resource,
                                                       Is_Used            => False,
                                                       Parent             => Parent_Of (Id.all),
                                                       Used_Packages      => null,
                                                       Implementation     => null,
                                                       Profile            => (null, null),
                                                       Overload           => null,
                                                       Generic_Parameters => null);
  end New_Generic_Library_Subprogram_Declaration;


  procedure Add_Generic_Library_Subprogram_Declaration (Self               : Unit_Handle;
                                                        Generic_Parameters : Formal_Block_Handle;
                                                        Profile            : Subprogram_Profile) is
  begin
    --TEST-----------------------------------------------------------------------------------------
      Write_Log ("*** add Generic_Library_Subprogram_Declaration " & Image_Of (Self.Location.all));
    -----------------------------------------------------------------------------------------------
    Generic_Subprogram_Handle(Self).Generic_Parameters := Generic_Parameters;
    Generic_Subprogram_Handle(Self).Profile := Profile;
    Self.Location.Data := Data_Handle(Self);
  end Add_Generic_Library_Subprogram_Declaration;


  function New_Library_Subprogram_Declaration (Id       : Identifier_List;
                                               Resource : Resource_Handle) return Unit_Handle is
  begin
    --TEST----------------------------------------------------------------------
      Write_Log ("*** new Library_Subprogram_Declaration " & Image_Of (Id.all));
    ----------------------------------------------------------------------------
    return new Library_Subprogram_Declaration'(Location       => Id(Id'last),
                                               Resource       => Resource,
                                               Is_Used        => False,
                                               Parent         => Parent_Of (Id.all),
                                               Used_Packages  => null,
                                               Implementation => null,
                                               Profile        => (null, null),
                                               Overload       => null);
  end New_Library_Subprogram_Declaration;


  procedure Add_Library_Subprogram_Declaration (Self    : Unit_Handle;
                                                Profile : Subprogram_Profile) is
  begin
    --TEST---------------------------------------------------------------------------------
      Write_Log ("*** add Library_Subprogram_Declaration " & Image_Of (Self.Location.all));
    ---------------------------------------------------------------------------------------
    Subprogram_Declaration_Handle(Self).Profile := Profile;
    Self.Location.Data := Data_Handle(Self);
  end Add_Library_Subprogram_Declaration;


  function New_Library_Subprogram_Body (Id       : Identifier_List;
                                        Resource : Resource_Handle) return Unit_Handle is
  begin
    --TEST---------------------------------------------------------------
      Write_Log ("*** new Library_Subprogram_Body " & Image_Of (Id.all));
    ---------------------------------------------------------------------
    return new Library_Subprogram_Body'(Location      => Id(Id'last),
                                        Resource      => Resource,
                                        Is_Used       => True,
                                        Parent        => Parent_Of (Id.all),
                                        Used_Packages => null,
                                        Specification => null,
                                        Profile       => (null, null),
                                        Overload      => null,
                                        Declarations  => Tree.Empty);
  end New_Library_Subprogram_Body;


  function New_Subprogram_Subunit (Id       : Identifier_List;
                                   Resource : Resource_Handle) return Unit_Handle is
  begin
    --TEST----------------------------------------------------------
      Write_Log ("*** new Subprogram_Subunit " & Image_Of (Id.all));
    ----------------------------------------------------------------
    return new Subprogram_Subunit'(Location      => Id(Id'last),
                                   Resource      => Resource,
                                   Is_Used       => True,
                                   Parent        => Parent_Of (Id.all),
                                   Used_Packages => null,
                                   Specification => null,
                                   Profile       => (null, null),
                                   Overload      => null,
                                   Declarations  => Tree.Empty);
  end New_Subprogram_Subunit;


  procedure Add_Library_Subprogram_Body (Self    : Unit_Handle;
                                         Profile : Subprogram_Profile) is
    The_Handle : Data_Handle;
  begin
    Subprogram_Body_Handle(Self).Profile := Profile;
    if Self.all in Subprogram_Subunit'class then
      --TEST---------------------------------------------------------------------
        Write_Log ("*** add Subprogram_Subunit " & Image_Of (Self.Location.all));
      ---------------------------------------------------------------------------
      The_Handle := Inner_Declaration_Of (Self.Location, Self.Parent.all);
      if Is_Subprogram (The_Handle) then
        Self.Location.Data := The_Handle;
      end if;
    else
      --TEST--------------------------------------------------------------------------
        Write_Log ("*** add Library_Subprogram_Body " & Image_Of (Self.Location.all));
      --------------------------------------------------------------------------------
      Self.Location.Data := Data_Handle(Self);
      declare
        Specification : constant Unit_Declaration_Handle := Subprogram_Body_Handle(Self).Specification;
      begin
        if Specification /= null and then Specification.all in Generic_Subprogram_Declaration'class
          and then Updated_Subprogram_Body (Subprogram_Body_Handle(Self), Generic_Subprogram_Handle(Specification))
        then
          return;
        end if;
      end;
    end if;
  end Add_Library_Subprogram_Body;

  ----------------------------------------------------------------------------------------------------------------------

  function New_Library_Package_Instantiation (Id       : Identifier_List;
                                              Resource : Resource_Handle) return Unit_Handle is
  begin
    --TEST---------------------------------------------------------------------
      Write_Log ("*** new Library_Package_Instantiation " & Image_Of (Id.all));
    ---------------------------------------------------------------------------
    return new Library_Package_Instantiation'(Location             => Id(Id'last),
                                              Resource             => Resource,
                                              Is_Used              => False,
                                              Parent               => Parent_Of (Id.all),
                                              Used_Packages        => null,
                                              Generic_Package      => null,
                                              Parent_Instantiation => null,
                                              Actual_Part          => null);
  end New_Library_Package_Instantiation;


  procedure Add_Library_Package_Instantiation (Self                 : Unit_Handle;
                                               Generic_Package      : Unit_Handle;
                                               Parent_Instantiation : Instantiation_Handle;
                                               Actual_Part          : List.Elements) is
  begin
    --TEST------------------------------------------------------------------------------------------------
      Write_Log ("*** add Library_Package_Instantiation " & Image_Of (Self.Location.all));
      if Parent_Instantiation /= null then
        Write_Log ("        Parent Instantiation: " & Full_Name_Of (Data_Handle(Parent_Instantiation)));
      end if;
    ------------------------------------------------------------------------------------------------------
    Instantiation_Handle(Self).Generic_Package := Generic_Package;
    Instantiation_Handle(Self).Parent_Instantiation := Parent_Instantiation;
    Instantiation_Handle(Self).Actual_Part := new List.Elements'(Actual_Part);
    Self.Location.Data := Data_Handle(Self);
  end Add_Library_Package_Instantiation;


  function New_Library_Subprogram_Instantiation (Id       : Identifier_List;
                                                 Resource : Resource_Handle) return Unit_Handle is
  begin
    --TEST----------------------------------------------------------------------------
      Write_Log ("*** new New_Library_Subprogram_Instantiation " & Image_Of (Id.all));
    ----------------------------------------------------------------------------------
    return new Library_Subprogram_Instantiation'(Location       => Id(Id'last),
                                                 Resource       => Resource,
                                                 Is_Used        => False,
                                                 Parent         => Parent_Of (Id.all),
                                                 Used_Packages  => null,
                                                 Implementation => null,
                                                 Profile        => (null, null),
                                                 Overload       => null);
  end New_Library_Subprogram_Instantiation;


  procedure Add_Library_Subprogram_Instantiation (Self    : Unit_Handle;
                                                  Profile : Subprogram_Profile) is
  begin
    --TEST---------------------------------------------------------------------------------------
      Write_Log ("*** add Add_Library_Subprogram_Instantiation " & Image_Of (Self.Location.all));
    ---------------------------------------------------------------------------------------------
    Self.Location.Data := Data_Handle(Self);
    Library_Subprogram_Instantiation_Handle(Self).Profile := Profile;
  end Add_Library_Subprogram_Instantiation;


  function New_Library_Package_Renaming (Id       : Identifier_List;
                                         Resource : Resource_Handle) return Unit_Handle is
  begin
    --TEST----------------------------------------------------------------
      Write_Log ("*** new Library_Package_Renaming " & Image_Of (Id.all));
    ----------------------------------------------------------------------
    return new Library_Package_Renaming'(Location      => Id(Id'last),
                                         Resource      => Resource,
                                         Is_Used       => False,
                                         Parent        => Parent_Of (Id.all),
                                         Used_Packages => null,
                                         Renamed_Item  => null);
  end New_Library_Package_Renaming;


  procedure Add_Library_Package_Renaming (Self            : Unit_Handle;
                                          Renamed_Package : Data_Handle) is
  begin
    --TEST---------------------------------------------------------------------------
      Write_Log ("*** add Library_Package_Renaming " & Image_Of (Self.Location.all));
    ---------------------------------------------------------------------------------
    Package_Renaming_Handle(Self).Renamed_Item := Renamed_Package;
    Self.Location.Data := Data_Handle(Self);
  end Add_Library_Package_Renaming;


  function New_Library_Subprogram_Renaming (Id       : Identifier_List;
                                            Resource : Resource_Handle) return Unit_Handle is
  begin
    --TEST-------------------------------------------------------------------
      Write_Log ("*** new Library_Subprogram_Renaming " & Image_Of (Id.all));
    -------------------------------------------------------------------------
    return new Library_Subprogram_Renaming'(Location      => Id(Id'last),
                                            Resource      => Resource,
                                            Is_Used       => False,
                                            Parent        => Parent_Of (Id.all),
                                            Used_Packages => null,
                                            Specification => null,
                                            Profile       => (null, null),
                                            Overload      => null,
                                            Declarations  => Tree.Empty,
                                            Renamed_Unit  => null);
  end New_Library_Subprogram_Renaming;


  procedure Add_Library_Subprogram_Renaming (Self               : Unit_Handle;
                                             Renamed_Subprogram : Unit_Handle) is
  begin
    --TEST------------------------------------------------------------------------------
      Write_Log ("*** add Library_Subprogram_Renaming " & Image_Of (Self.Location.all));
    ------------------------------------------------------------------------------------
    Subprogram_Renaming_Handle(Self).Renamed_Unit := Renamed_Subprogram;
    Self.Location.Data := Data_Handle(Self);
  end Add_Library_Subprogram_Renaming;


  function New_Generic_Library_Package_Renaming (Id       : Identifier_List;
                                                 Resource : Resource_Handle) return Unit_Handle is
  begin
    --TEST------------------------------------------------------------------------
      Write_Log ("*** new Generic_Library_Package_Renaming " & Image_Of (Id.all));
    ------------------------------------------------------------------------------
    return new Generic_Library_Package_Renaming'(Location      => Id(Id'last),
                                                 Resource      => Resource,
                                                 Is_Used       => False,
                                                 Parent        => Parent_Of (Id.all),
                                                 Used_Packages => null,
                                                 Renamed_Item  => null);
  end New_Generic_Library_Package_Renaming;


  procedure Add_Generic_Library_Package_Renaming (Self            : Unit_Handle;
                                                  Renamed_Package : Unit_Handle) is
  begin
    --TEST-----------------------------------------------------------------------------------
      Write_Log ("*** add Generic_Library_Package_Renaming " & Image_Of (Self.Location.all));
    -----------------------------------------------------------------------------------------
    Package_Renaming_Handle(Self).Renamed_Item := Data_Handle(Renamed_Package);
    Self.Location.Data := Data_Handle(Self);
  end Add_Generic_Library_Package_Renaming;


  function New_Generic_Library_Subprogram_Renaming (Id       : Identifier_List;
                                                    Resource : Resource_Handle) return Unit_Handle is
  begin
    --TEST---------------------------------------------------------------------------
      Write_Log ("*** new Generic_Library_Subprogram_Renaming " & Image_Of (Id.all));
    ---------------------------------------------------------------------------------
    return new Generic_Library_Subprogram_Renaming'(Location      => Id(Id'last),
                                                    Resource      => Resource,
                                                    Is_Used       => False,
                                                    Parent        => Parent_Of (Id.all),
                                                    Used_Packages => null,
                                                    Specification => null,
                                                    Profile       => (null, null),
                                                    Overload      => null,
                                                    Declarations  => Tree.Empty,
                                                    Renamed_Unit  => null);
  end New_Generic_Library_Subprogram_Renaming;


  procedure Add_Generic_Library_Subprogram_Renaming (Self               : Unit_Handle;
                                                     Renamed_Subprogram : Unit_Handle) is
  begin
    --TEST--------------------------------------------------------------------------------------
      Write_Log ("*** add Generic_Library_Subprogram_Renaming " & Image_Of (Self.Location.all));
    --------------------------------------------------------------------------------------------
    Subprogram_Renaming_Handle(Self).Renamed_Unit := Renamed_Subprogram;
    Self.Location.Data := Data_Handle(Self);
  end Add_Generic_Library_Subprogram_Renaming;

  ----------------------------------------------------------------------------------------------------------------------

  function Declared (Id   : Identifier_Handle;
                     To   : Unit_Handle;
                     Item : Data_Handle) return Unit_Handle with Inline is
  begin
    Declare_Item (To.all, Item);
    Id.Data := Item;
    return Unit_Handle(Item);
  end Declared;


  function Declared_Body (Id   : Identifier_Handle;
                          To   : Unit_Handle;
                          Item : Unit_Body_Handle) return Unit_Handle is

    The_Declaration : Data_Handle := Declaration_Of (Id, To.all);

  begin
    Id.Data := Data_Handle(Item);
    if Is_Null (The_Declaration) then
      Declare_Item (To.all, Data_Handle(Item));
    else
      if The_Declaration.all in Active_Type'class then
        if Item.all in Active_Body'class then
          Active_Body_Handle(Item).Type_Link := The_Declaration;
        end if;
        if In_Same_Library (Declaration_Handle(Declaration_Handle(The_Declaration).Parent),
                            Declaration_Handle(Item))
        then
          The_Declaration.Location := Item.Location;
        end if;
        The_Declaration := Data_Handle(Active_Type_Handle(The_Declaration).Object);
      end if;
      if The_Declaration.all in Unit_Declaration'class then
        Item.Specification := Unit_Declaration_Handle (The_Declaration);
        if In_Same_Library (Declaration_Handle(The_Declaration), Declaration_Handle(Item)) then
          Unit_Declaration_Handle(The_Declaration).Implementation := Item;
          Item.Is_Used := True;
          if The_Declaration.all in Task_Declaration'class then
            Unit_Declaration_Handle(The_Declaration).Is_Used := True;
          end if;
        else
          Declare_Item (To.all, Data_Handle(Item));
        end if;
      else
        Declare_Item (To.all, Data_Handle(Item));
      end if;
    end if;
    return Unit_Handle(Item);
  end Declared_Body;

  ----------------------------------------------------------------------------------------------------------------------


  function New_Generic_Package_Declaration (Id                 : Identifier_Handle;
                                            Parent             : Unit_Handle;
                                            Generic_Parameters : Formal_Block_Handle) return Unit_Handle is
  begin
    --TEST-------------------------------------------------------------------
    --Write_Log ("*** new Generic_Package_Declaration " & Image_Of (Id.all));
    -------------------------------------------------------------------------
    return Declared (Id   => Id,
                     To   => Parent,
                     Item => new Generic_Package_Declaration'(Location           => Id,
                                                              Is_Used            => False,
                                                              Parent             => Parent,
                                                              Used_Packages      => null,
                                                              Declarations       => Tree.Empty,
                                                              Implementation     => null,
                                                              Private_Part       => null,
                                                              Generic_Parameters => Generic_Parameters));
  end New_Generic_Package_Declaration;


  function New_Package_Specification (Id     : Identifier_Handle;
                                      Parent : Unit_Handle) return Unit_Handle is
  begin
    --TEST-------------------------------------------------------------
    --Write_Log ("*** new Package_Specification " & Image_Of (Id.all));
    -------------------------------------------------------------------
    return Declared (Id   => Id,
                     To   => Parent,
                     Item => new Package_Specification'(Location       => Id,
                                                        Is_Used        => False,
                                                        Parent         => Parent,
                                                        Used_Packages  => null,
                                                        Declarations   => Tree.Empty,
                                                        Implementation => null,
                                                        Private_Part   => null));
  end New_Package_Specification;


  function New_Package_Body (Id          : Identifier_Handle;
                             Parent      : Unit_Handle;
                             Is_Separate : Boolean := False) return Unit_Handle is
    Self              : Unit_Body_Handle;
    The_Body          : Unit_Handle;
    The_Specification : Unit_Declaration_Handle;
  begin
    if Is_Separate then
      --TEST-------------------------------------------------------------
      --Write_Log ("*** new Separate_Package_Body " & Image_Of (Id.all));
      -------------------------------------------------------------------
      Self := new Separate_Package_Body'(Location      => Id,
                                         Is_Used       => True,
                                         Parent        => Parent,
                                         Used_Packages => null,
                                         Specification => null,
                                         Declarations  => Tree.Empty);
    else
      --TEST----------------------------------------------------
      Write_Log ("*** new Package_Body " & Image_Of (Id.all));
      ----------------------------------------------------------
      Self := new Package_Body'(Location      => Id,
                                Is_Used       => True,
                                Parent        => Parent,
                                Used_Packages => null,
                                Specification => null,
                                Declarations  => Tree.Empty);
    end if;
    The_Body := Declared_Body (Id   => Id,
                               To   => Parent,
                               Item => Self);
    The_Specification := Self.Specification;
    while The_Specification /= null loop
      if The_Specification.Used_Packages /= null then
        if Self.Used_Packages = null then
          Self.Used_Packages := new List.Item;
        end if;
        Self.Used_Packages.Append (List.Copy_Of(The_Specification.Used_Packages.all));
      end if;
      declare
        Private_Part : constant Block_Handle := Package_Specification_Handle(The_Specification).Private_Part;
      begin
        if Private_Part /= null and then Private_Part.Used_Packages /= null then
          if Self.Used_Packages = null then
            Self.Used_Packages := new List.Item;
          end if;
          Self.Used_Packages.Append (List.Copy_Of(Private_Part.Used_Packages.all));
        end if;
      end;
      if The_Specification.Parent /= null and then The_Specification.Parent.all in Unit_Declaration'class then
        The_Specification := Unit_Declaration_Handle(The_Specification.Parent);
      else
        exit;
      end if;
    end loop;
    return The_Body;
  end New_Package_Body;

  ----------------------------------------------------------------------------------------------------------------------

  procedure Declare_Item (Id         :     Identifier_Handle;
                          To         :     Unit_Handle;
                          Item       :     Data_Handle;
                          The_Cursor : out Tree.Cursor) with Inline is
  begin
    Declare_Item (To.all, Item, The_Cursor);
    if Is_Null (Id.Data) then
      Id.Data := Item; -- only base type for reused locations (eg. derived enumerations)
    end if;
  end Declare_Item;


  procedure Declare_Item (Id   : Identifier_Handle;
                          To   : Unit_Handle;
                          Item : Data_Handle) with Inline is
  begin
    Declare_Item (To.all, Item);
    Id.Data := Item;
  end Declare_Item;


  function Is_Subprogram (Item : Data_Handle) return Boolean is
  begin
    if Is_Null (Item) then
      return False;
    end if;
    case Data_Kind_Of (Item.all) is
    when Is_In_Subprogram
       | Is_Used_Subprogram
       | Is_Used_Generic_Subprogram
       | Is_Generic_Subprogram_Declaration
       | Is_Subprogram_Renaming
       | Is_Entry_Declaration
       | Is_Entry_Body
       | Is_Enumeration_Value
    =>
      return True;
    when others =>
      return False;
    end case;
  end Is_Subprogram;


  procedure Declare_Subprogram (Id   : Identifier_Handle;
                                To   : Unit_Handle;
                                Item : Declaration_Handle) is

    procedure Link_Item_To (Specification : Subprogram_Declaration_Handle) with Inline is
    begin
      Item.Is_Used := True;
      if In_Same_Library (Declaration_Handle(Specification), Item) then
        Specification.Implementation := Unit_Body_Handle(Item);
      end if;
    end Link_Item_To;

    function Is_Own_Specification (Specification : Declaration_Handle) return Boolean with Inline is
    begin
      if Specification.all in Subprogram_Declaration'class then
        if Profile_Of (Item) = Profile_Of (Specification) then
          Unit_Body_Handle(Item).Specification := Unit_Declaration_Handle(Specification);
          Link_Item_To (Subprogram_Declaration_Handle(Specification));
          return True;
        elsif Specification.all in Generic_Subprogram_Declaration'class and then Item.all in Subprogram_Body'class then
          if Updated_Subprogram_Body (Subprogram_Body_Handle(Item), Generic_Subprogram_Handle(Specification)) then
            Unit_Body_Handle(Item).Specification := Unit_Declaration_Handle(Specification);
            Link_Item_To (Subprogram_Declaration_Handle(Specification));
            return True;
          end if;
        end if;
      end if;
      return False;
    end Is_Own_Specification;

    procedure Search_Own_Specification_From (Specification : Declaration_Handle) with Inline is
      The_Specification : Declaration_Handle := Specification;
    begin
      while The_Specification /= null loop
        if Is_Own_Specification (The_Specification) then
          return;
        end if;
        The_Specification := Overload_Of (The_Specification);
      end loop;
    end Search_Own_Specification_From;

    function Nested_Subprogram return Declaration_Handle is
      The_Scope  : Unit_Handle := To;
      The_Handle : Data_Handle;
      use type Name.Handle;
    begin
      while The_Scope /= null loop
        if The_Scope.all in Package_Body'class | Protected_Body'class then
          -- Lookup own Specification
          The_Scope := Unit_Handle(Unit_Body_Handle(The_Scope).Specification);
          exit when The_Scope = null;
          The_Handle := Inner_Declaration_Of (Id, The_Scope.all);
        else
          if Is_Subprogram (Data_Handle(The_Scope)) and then The_Scope.Location.Id = Id.Id then
            return Declaration_Handle(The_Scope);
          end if;
          The_Scope := The_Scope.Parent;
          exit when The_Scope = null;
          The_Handle := Inner_Declaration_Of (Id, The_Scope.all);
        end if;
        if Is_Subprogram (The_Handle) then
          return Declaration_Handle(The_Handle);
        end if;
      end loop;
      return null;
    end Nested_Subprogram;

    The_Cursor : Tree.Cursor;
    The_Handle : Data_Handle;

  begin -- Declare_Subprogram
    if Id.all in Operator_Symbol'class then
      Data.Set_Used (Data_Handle(Item));
    end if;
    Declare_Item (Id, To, Data_Handle(Item), The_Cursor);
    if not Is_Subprogram (The_Cursor.all) then
      return;
    end if;
    declare
      Unit : constant Declaration_Handle :=  Declaration_Handle(The_Cursor.all);
    begin
      if Item.all in Subprogram_Body'class then
        if Unit /= null and then Unit /= Item then -- local overload
          if Is_Own_Specification (Unit) then
            return; -- overload already resolved in specification
          else
            Search_Own_Specification_From (Unit);
            Subprogram_Body_Handle(Item).Overload := Unit;
            The_Cursor.all := Data_Handle(Item);
          end if;
        else -- no local overload
          declare
            Subprogram : constant Declaration_Handle := Nested_Subprogram;
          begin
            if Subprogram /= null and then Subprogram /= Item then -- not found self
              if Is_Own_Specification (Subprogram) then
                Chain_In (Subprogram);
                if Is_Overloaded (Subprogram) then
                  Overload (Subprogram, To => Item);
                end if;
              else
                Chain_In (Subprogram);
                Overload (Subprogram, To => Item);
                Search_Own_Specification_From (Subprogram);
              end if;
            end if;
          end;
        end if;
      else -- specification
        if Unit /= Item then -- local overload
          Overload (Unit, To => Item);
          The_Cursor.all := Data_Handle(Item);
        else -- no local overload
          if To.all in Private_Block'class then
            The_Handle := Declaration_Of (Id, To.Parent.all);
            if Is_Subprogram (The_Handle) then
              Overload (Declaration_Handle(The_Handle), To => Item);
              return;
            end if;
          end if;
          declare
            Subprogram : constant Declaration_Handle := Nested_Subprogram;
          begin
            if Subprogram /= null and then Subprogram /= Item then -- not found self
              Overload (Subprogram, To => Item);
            end if;
          end;
        end if;
      end if;
    end;
  end Declare_Subprogram;


  procedure Chain_In (Item : Declaration_Handle) is

    Inner_Scope     : constant Unit_Handle := Item.Parent;
    The_Outer_Scope : Unit_Handle;

  begin
    if Inner_Scope.all in Unit_Declaration'class then
      The_Outer_Scope := Inner_Scope.Parent;
      if The_Outer_Scope /= null and then The_Outer_Scope.all in Unit_Body'class then
        declare
          Outer_Item   : constant Data_Handle := Declaration_Of (Item.Location, The_Outer_Scope.all);
          The_Start    : Declaration_Handle := Item;
          The_Overload : Declaration_Handle := Overload_Of (The_Start);
        begin
          if Is_Subprogram (Outer_Item) then
            --TEST-------------------------------------------------------------------------
            --Write_Log ("Chain_In => " & Full_Name_Of (Data_Handle(Subprogram)));
            --Write_Log ("   Inner_Scope: " & Full_Name_Of (Data_Handle(Inner_Scope)));
            --Write_Log ("   Outer Scope: " & Full_Name_Of (Data_Handle(The_Outer_Scope)));
            --Write_Log ("   Outer Item : " & Full_Name_Of (Outer_Item));
            -------------------------------------------------------------------------------
            while The_Overload /= null and then The_Overload.Parent = Inner_Scope loop
              if The_Overload = Declaration_Handle(Outer_Item) then
                --TEST-------------------------------
                --Write_Log ("<<ALREADY IN CHAIN>>");
                -------------------------------------
                return;
              elsif Overload_Of (Declaration_Handle(Outer_Item)) = The_Overload then
                exit;
              end if;
              The_Start := The_Overload;
              The_Overload := Overload_Of (The_Overload);
            end loop;
            Overload (Declaration_Handle(Outer_Item), To => The_Start);
            --TEST------------------------------------------------------------------------------------------------------
            --Write_Log ("<<CHAINED IN>> " & Full_Name_Of (Outer_Item) & " - " & Full_Name_Of (Data_Handle(The_Start)));
            ------------------------------------------------------------------------------------------------------------
          end if;
        end;
      end if;
    end if;
  end Chain_In;


  function Declared_Subprogram (Id   : Identifier_Handle;
                                To   : Unit_Handle;
                                Item : Unit_Handle) return Unit_Handle with Inline is
  begin
    Declare_Subprogram (Id, To, Declaration_Handle(Item));
    return Item;
  end Declared_Subprogram;


  procedure New_Generic_Subprogram_Declaration (Id                 : Identifier_Handle;
                                                Parent             : Unit_Handle;
                                                Generic_Parameters : Formal_Block_Handle;
                                                Profile            : Subprogram_Profile) is
  begin
    --TEST----------------------------------------------------------------------
    --Write_Log ("*** new Generic_Subprogram_Declaration " & Image_Of (Id.all));
    ----------------------------------------------------------------------------
    Declare_Subprogram
      (Id   => Id,
       To   => Parent,
       Item => new Generic_Subprogram_Declaration'(Location           => Id,
                                                   Is_Used            => False,
                                                   Parent             => Parent,
                                                   Used_Packages      => null,
                                                   Implementation     => null,
                                                   Profile            => Used (Profile),
                                                   Overload           => null,
                                                   Generic_Parameters => Generic_Parameters));
  end New_Generic_Subprogram_Declaration;


  function New_Function_Expression_Declaration (Id      : Identifier_Handle;
                                                Parent  : Unit_Handle;
                                                Profile : Subprogram_Profile) return Unit_Handle is
    The_Unit : Unit_Handle;

  begin
    --TEST-----------------------------------------------------------------------
    --Write_Log ("*** new Function_Expression_Declaration " & Image_Of (Id.all));
    -----------------------------------------------------------------------------
    The_Unit := Declared_Subprogram (Id   => Id,
                                     To   => Parent,
                                     Item => new Subprogram_Body'(Location      => Id,
                                                                  Is_Used       => False,
                                                                  Parent        => Parent,
                                                                  Used_Packages => null,
                                                                  Specification => null,
                                                                  Profile       => Profile,
                                                                  Overload      => null,
                                                                  Declarations  => Tree.Empty));
    Handle_Aspects (Profile, The_Unit);
    Handle_Methods (Profile, The_Unit);
    return The_Unit;
  end New_Function_Expression_Declaration;


  function New_Subprogram_Declaration (Id      : Identifier_Handle;
                                       Parent  : Unit_Handle;
                                       Profile : Subprogram_Profile) return Unit_Handle is
    The_Unit : Unit_Handle;

  begin
    if Parent.Parent = null and then Id.all in Equality_Operator'class then
      --TEST---------------------------------------------------------------------
      --Write_Log ("*** new Predefined_Equality_Operation " & Image_Of (Id.all));
      ---------------------------------------------------------------------------
      The_Unit := Declared_Subprogram (Id   => Id,
                                       To   => Parent,
                                       Item => new Predefined_Equality_Operation'(Location       => Id,
                                                                                  Is_Used        => False,
                                                                                  Parent         => Parent,
                                                                                  Used_Packages  => null,
                                                                                  Implementation => null,
                                                                                  Profile        => Used (Profile),
                                                                                  Overload       => null));
    else
      --TEST--------------------------------------------------------------
      --Write_Log ("*** new Subprogram_Declaration " & Image_Of (Id.all));
      --------------------------------------------------------------------
      The_Unit := Declared_Subprogram (Id   => Id,
                                       To   => Parent,
                                       Item => new Subprogram_Declaration'(Location       => Id,
                                                                           Is_Used        => False,
                                                                           Parent         => Parent,
                                                                           Used_Packages  => null,
                                                                           Implementation => null,
                                                                           Profile        => Used (Profile),
                                                                           Overload       => null));
      Handle_Methods (Profile, The_Unit);
    end if;
    Handle_Aspects (Profile, The_Unit);
    return The_Unit;
  end New_Subprogram_Declaration;


  function Actual_Profile_Of (Profile  : Subprogram_Profile;
                              Instance : Instantiation_Handle) return Subprogram_Profile is

    The_Position     : Natural;
    The_Parameters   : List.Item;
    The_Component    : Data_Handle;
    The_Profile      : Subprogram_Profile;

    use type List.Elements_Access;

  begin
    if Profile.Parameters = null then
      The_Profile := New_Profile;
    else
      for Index in Profile.Parameters'range loop
        if Profile.Parameters(Index).all in Data_Object'class then
          declare
            The_Parameter : constant Object_Handle := Object_Handle(Profile.Parameters(Index));
          begin
            if The_Parameter /= null and then not Is_Null (The_Parameter.Object_Type) and then
              The_Parameter.Object_Type.all in Formal_Type'class
            then
              --TEST--------------------------------------------------------------------------
              --Write_Log ("*** new Actual Component " & Image_Of (The_Parameter.Location.all)
              --         & " for " & Image_Of (The_Parameter.Object_Type.Location.all));
              --------------------------------------------------------------------------------
              declare
                Formal_Parameter : constant Formal_Handle := Formal_Handle(The_Parameter.Object_Type);
              begin
                The_Position := Formal_Parameter.Position;
                The_Component := new Data_Object'(Location      => The_Parameter.Location,
                                                  Is_Used       => False,
                                                  Parent        => null,
                                                  Object_Type   => Instance.Actual_Part(The_Position),
                                                  Is_Class_Wide => The_Parameter.Is_Class_Wide,
                                                  Has_Default   => The_Parameter.Has_Default);
                --TEST------------------------------------------------------------------------------------
                --Write_Log ("Type: " & Image_Of (Object_Handle(The_Component).Object_Type.Location.all));
                ------------------------------------------------------------------------------------------
              exception
              when others =>
                Write_Log ("*** new Actual Component " & Image_Of (The_Parameter.Location.all)
                           & " for " & Image_Of (The_Parameter.Object_Type.Location.all));
                Write_Log ("%%% Formal parameter type not found");
                The_Component := Data_Handle(The_Parameter);
              end;
            else
              The_Component := Data_Handle(The_Parameter);
            end if;
            The_Parameters.Append (The_Component);
          end;
        end if;
      end loop;
      The_Profile := New_Profile (The_Parameters);
    end if;
    The_Profile.Result_Type := Profile.Result_Type;
    if not Is_Null (Profile.Result_Type) and then Profile.Result_Type.all in Formal_Type'class then
      --TEST-----------------------------------------------------------------------------------
      --Write_Log ("*** new Actual Result for " & Image_Of (Profile.Result_Type.Location.all));
      -----------------------------------------------------------------------------------------
      declare
        Formal_Result : constant Formal_Handle := Formal_Handle(Profile.Result_Type);
      begin
        The_Position := Formal_Result.Position;
        The_Profile.Result_Type := Data.Type_Of(Instance.Actual_Part(The_Position));
        --TEST-------------------------------------------------------------------------
        --Write_Log ("Result_Type " & Image_Of (The_Profile.Result_Type.Location.all));
        -------------------------------------------------------------------------------
      exception
      when others =>
        Write_Log ("*** new Actual Result for " & Image_Of (Profile.Result_Type.Location.all));
        Write_Log ("%%% Formal result type not found");
      end;
    end if;
    return The_Profile;
  end Actual_Profile_Of;


  procedure New_Used_Subprogram (Item     : Declaration_Handle;
                                 Scope    : Unit_Handle;
                                 Instance : Instantiation_Handle) is

    The_Scope      : Unit_Handle := Scope;
    The_Subprogram : Declaration_Handle;
    The_Cursor     : Tree.Cursor;
    The_Handle     : Data_Handle;

    use type Tree.Cursor;
    use type Name.Handle;

  begin
    if Item.all in Generic_Subprogram_Declaration'class then
      --TEST--------------------------------------------------------------------------
      --Write_Log ("*** new Used_Generic_Subprogram " & Image_Of (Item.Location.all));
      --------------------------------------------------------------------------------
      The_Subprogram
        := new Used_Generic_Subprogram'(Location           => Item.Location,
                                        Is_Used            => False,
                                        Parent             => Item.Parent,
                                        Used_Packages      => null,
                                        Profile            => Profile_Of (Item),
                                        Overload           => null,
                                        Generic_Parameters => Generic_Subprogram_Handle(Item).Generic_Parameters);
    else
      --TEST------------------------------------------------------------------
      --Write_Log ("*** new Used_Subprogram " & Image_Of (Item.Location.all));
      ------------------------------------------------------------------------
      if Instance = null then
        The_Subprogram := new Used_Subprogram'(Location       => Item.Location,
                                               Is_Used        => False,
                                               Parent         => Item.Parent,
                                               Used_Packages  => null,
                                               Profile        => Profile_Of (Item),
                                               Overload       => null);
      else
        The_Subprogram := new Used_Subprogram'(Location       => Item.Location,
                                               Is_Used        => False,
                                               Parent         => Item.Parent,
                                               Used_Packages  => null,
                                               Profile        => Actual_Profile_Of (Profile_Of (Item), Instance),
                                               Overload       => null);
      end if;
    end if;
    Declare_Item (To         => The_Scope.all,
                  Item       => Data_Handle(The_Subprogram),
                  The_Cursor => The_Cursor);
    if The_Cursor = null or else not Is_Subprogram (The_Cursor.all) then
      return;
    end if;
    if Declaration_Handle(The_Cursor.all) /= The_Subprogram then
      Overload (Declaration_Handle(The_Cursor.all), To => The_Subprogram);
      The_Cursor.all := Data_Handle(The_Subprogram);
    else
      if Scope.all in Private_Block'class then
        The_Handle := Declaration_Of (Item.Location, Scope.Parent.all);
        if Is_Null (The_Handle) then
          The_Scope := The_Scope.Parent;
        else
          if Is_Subprogram (The_Handle) then
            Overload (Declaration_Handle(The_Handle), To => The_Subprogram);
          end if;
          return;
        end if;
      end if;
      loop
        if The_Scope.all in Package_Body'class | Protected_Body'class then
          -- Lookup own Specification
          The_Scope := Unit_Handle(Unit_Body_Handle(The_Scope).Specification);
          if The_Scope = null then
            return;
          end if;
          The_Handle := Inner_Declaration_Of (Item.Location, The_Scope.all);
        else
          if Is_Subprogram (Data_Handle(The_Scope)) and then The_Scope.Location.Id = Item.Location.Id then
            The_Handle := Data_Handle(The_Scope);
            exit;
          end if;
          The_Scope := The_Scope.Parent;
          if The_Scope = null then
            return;
          end if;
          The_Handle := Inner_Declaration_Of (Item.Location, The_Scope.all);
        end if;
        exit when Is_Subprogram (The_Handle);
      end loop;
      if Declaration_Handle(The_Handle) /= Item then -- not self
        Overload (Declaration_Handle(The_Handle), To => The_Subprogram);
      end if;
    end if;
  end New_Used_Subprogram;


  function New_Subprogram_Body (Id          : Identifier_Handle;
                                Parent      : Unit_Handle;
                                Profile     : Subprogram_Profile;
                                Is_Separate : Boolean := False) return Unit_Handle is
    Self : Unit_Handle;
  begin
    if Is_Separate then
      --TEST----------------------------------------------------------------
      --Write_Log ("*** new Separate_Subprogram_Body " & Image_Of (Id.all));
      ----------------------------------------------------------------------
      Self := new Separate_Subprogram_Body'(Location      => Id,
                                            Is_Used       => False,
                                            Parent        => Parent,
                                            Used_Packages => null,
                                            Specification => null,
                                            Profile       => Profile,
                                            Overload      => null,
                                            Declarations  => Tree.Empty);
    else
      --TEST-------------------------------------------------------
      --Write_Log ("*** new Subprogram_Body " & Image_Of (Id.all));
      -------------------------------------------------------------
      Self := new Subprogram_Body'(Location      => Id,
                                   Is_Used       => False,
                                   Parent        => Parent,
                                   Used_Packages => null,
                                   Specification => null,
                                   Profile       => Profile,
                                   Overload      => null,
                                   Declarations  => Tree.Empty);
    end if;
    Self := Declared_Subprogram (Id   => Id,
                                 To   => Parent,
                                 Item => Self);
    if Subprogram_Body_Handle(Self).Specification = null then
      Handle_Methods (Profile, Self);
    end if;
    return Self;
  end New_Subprogram_Body;

  ----------------------------------------------------------------------------------------------------------------------

  procedure New_Package_Instantiation (Id                   : Identifier_Handle;
                                       Parent               : Unit_Handle;
                                       Generic_Package      : Unit_Handle;
                                       Parent_Instantiation : Instantiation_Handle;
                                       Actual_Part          : List.Elements) is
  begin
    --TEST----------------------------------------------------------------------------------------------
    --Write_Log ("*** new Package_Instantiation " & Image_Of (Id.all));
    --if Parent_Instantiation /= null then
    --  Write_Log ("        Parent Instantiation: " & Full_Name_Of (Data_Handle(Parent_Instantiation)));
    --end if;
    ----------------------------------------------------------------------------------------------------
    Declare_Item (Id   => Id,
                  To   => Parent,
                  Item => new Package_Instantiation'(Location             => Id,
                                                     Is_Used              => False,
                                                     Parent               => Parent,
                                                     Used_Packages        => null,
                                                     Generic_Package      => Generic_Package,
                                                     Parent_Instantiation => Parent_Instantiation,
                                                     Actual_Part          => new List.Elements'(Actual_Part)));
  end New_Package_Instantiation;


  procedure New_Package_Renaming (Id              : Identifier_Handle;
                                  Parent          : Unit_Handle;
                                  Renamed_Package : Data_Handle) is
  begin
    --TEST--------------------------------------------------------
    --Write_Log ("*** new Package_Renaming " & Image_Of (Id.all));
    --------------------------------------------------------------
    Set_Used (Renamed_Package);
    Declare_Item (Id   => Id,
                  To   => Parent,
                  Item => new Package_Renaming'(Location      => Id,
                                                Is_Used       => False,
                                                Parent        => Parent,
                                                Used_Packages => null,
                                                Renamed_Item  => Renamed_Package));
  end New_Package_Renaming;


  procedure New_Subprogram_Renaming (Id                 : Identifier_Handle;
                                     Parent             : Unit_Handle;
                                     Profile            : Subprogram_Profile;
                                     Renamed_Subprogram : Unit_Handle;
                                     Renamed_Id         : Identifier_Handle) is
  begin
    --TEST-----------------------------------------------------------
    --Write_Log ("*** new Subprogram_Renaming " & Image_Of (Id.all));
    -----------------------------------------------------------------
    if not Is_Null (Renamed_Id) then
      Update_Reference_Of (Renamed_Subprogram, Profile, Renamed_Id);
    end if;
    Declare_Subprogram (Id   => Id,
                        To   => Parent,
                        Item => new Subprogram_Renaming'(Location      => Id,
                                                         Is_Used       => False,
                                                         Parent        => Parent,
                                                         Used_Packages => null,
                                                         Specification => null,
                                                         Profile       => Used (Profile),
                                                         Overload      => null,
                                                         Declarations  => Tree.Empty,
                                                         Renamed_Unit  => Renamed_Subprogram));
  end New_Subprogram_Renaming;


  procedure New_Generic_Subprogram_Renaming (Id                 : Identifier_Handle;
                                             Parent             : Unit_Handle;
                                             Renamed_Subprogram : Unit_Handle) is
  begin
    --TEST-------------------------------------------------------------------
    --Write_Log ("*** new Generic_Subprogram_Renaming " & Image_Of (Id.all));
    -------------------------------------------------------------------------
    Set_Used (Data_Handle(Renamed_Subprogram));
    Declare_Subprogram (Id   => Id,
                        To   => Parent,
                        Item => new Generic_Subprogram_Renaming'(Location      => Id,
                                                                 Is_Used       => False,
                                                                 Parent        => Parent,
                                                                 Used_Packages => null,
                                                                 Specification => null,
                                                                 Profile       => (null, null),
                                                                 Overload      => null,
                                                                 Declarations  => Tree.Empty,
                                                                 Renamed_Unit  => Renamed_Subprogram));
  end New_Generic_Subprogram_Renaming;

  ----------------------------------------------------------------------------------------------------------------------

  function New_Protected_Declaration (Id     : Identifier_Handle;
                                      Parent : Unit_Handle) return Unit_Handle is
  begin
    --TEST-------------------------------------------------------------
    --Write_Log ("*** new Protected_Declaration " & Image_Of (Id.all));
    -------------------------------------------------------------------
    return Declared (Id   => Id,
                     To   => Parent,
                     Item => new Protected_Declaration'(Location       => Id,
                                                        Is_Used        => False,
                                                        Parent         => Parent,
                                                        Used_Packages  => null,
                                                        Implementation => null,
                                                        Declarations   => Tree.Empty));
  end New_Protected_Declaration;


  function New_Protected_Body (Id     : Identifier_Handle;
                               Parent : Unit_Handle) return Unit_Handle is
  begin
    --TEST------------------------------------------------------
    --Write_Log ("*** new Protected_Body " & Image_Of (Id.all));
    ------------------------------------------------------------
    return Declared_Body (Id   => Id,
                          To   => Parent,
                          Item => new Protected_Body'(Location      => Id,
                                                      Is_Used       => True,
                                                      Parent        => Parent,
                                                      Used_Packages => null,
                                                      Specification => null,
                                                      Type_Link     => null,
                                                      Declarations  => Tree.Empty));
  end New_Protected_Body;


  function New_Task_Declaration (Id     : Identifier_Handle;
                                 Parent : Unit_Handle) return Unit_Handle is
  begin
    --TEST--------------------------------------------------------
    --Write_Log ("*** new Task_Declaration " & Image_Of (Id.all));
    --------------------------------------------------------------
    return Declared (Id   => Id,
                     To   => Parent,
                     Item => new Task_Declaration'(Location       => Id,
                                                   Is_Used        => False,
                                                   Parent         => Parent,
                                                   Used_Packages  => null,
                                                   Implementation => null,
                                                   Declarations   => Tree.Empty));
  end New_Task_Declaration;


  function New_Task_Body (Id     : Identifier_Handle;
                          Parent : Unit_Handle) return Unit_Handle is
  begin
    --TEST-------------------------------------------------
    --Write_Log ("*** new Task_Body " & Image_Of (Id.all));
    -------------------------------------------------------
    return Declared_Body (Id   => Id,
                          To   => Parent,
                          Item => new Task_Body'(Location      => Id,
                                                 Is_Used       => True,
                                                 Parent        => Parent,
                                                 Used_Packages => null,
                                                 Specification => null,
                                                 Type_Link     => null,
                                                 Declarations  => Tree.Empty));
  end New_Task_Body;


  procedure New_Entry_Declaration (Id         : Identifier_Handle;
                                   Parent     : Unit_Handle;
                                   Profile    : Subprogram_Profile;
                                   Index_Type : Data_Handle) is
  begin
    --TEST---------------------------------------------------------
    --Write_Log ("*** new Entry_Declaration " & Image_Of (Id.all));
    ---------------------------------------------------------------
    Declare_Subprogram (Id   => Id,
                        To   => Parent,
                        Item => new Entry_Declaration'(Location       => Id,
                                                       Is_Used        => False,
                                                       Parent         => Parent,
                                                       Used_Packages  => null,
                                                       Implementation => null,
                                                       Profile        => Used (Profile),
                                                       Overload       => null,
                                                       Index_Type     => Index_Type));
  end New_Entry_Declaration;


  function New_Entry_Body (Id         : Identifier_Handle;
                           Parent     : Unit_Handle;
                           Profile    : Subprogram_Profile;
                           Index_Type : Data_Handle) return Unit_Handle is
  begin
    --TEST--------------------------------------------------
    --Write_Log ("*** new Entry_Body " & Image_Of (Id.all));
    --------------------------------------------------------
    return Declared_Subprogram (Id   => Id,
                                To   => Parent,
                                Item => new Entry_Body'(Location      => Id,
                                                        Is_Used       => True,
                                                        Parent        => Parent,
                                                        Used_Packages => null,
                                                        Specification => null,
                                                        Profile       => Profile,
                                                        Overload      => null,
                                                        Declarations  => Tree.Empty,
                                                        Index_Type    => Index_Type));
  end New_Entry_Body;


  function New_Accept_Declaration (Id      : Identifier_Handle;
                                   Parent  : Unit_Handle;
                                   Profile : Subprogram_Profile) return Unit_Handle is

    Item : constant Data_Handle := new Accept_Declaration'(Location      => Id,
                                                           Is_Used       => False,
                                                           Parent        => Parent,
                                                           Used_Packages => null,
                                                           Declarations  => Tree.Empty,
                                                           Profile       => Profile);
  begin
    --TEST----------------------------------------------------------
    --Write_Log ("*** new Accept_Declaration " & Image_Of (Id.all));
    ----------------------------------------------------------------
    return Unit_Handle(Item);
  end New_Accept_Declaration;

  ----------------------------------------------------------------------------------------------------------------------

  procedure Update_Private_Type (Id   : Identifier_Handle;
                                 To   : Unit_Handle;
                                 Item : Data_Handle) is

    Specification : constant Unit_Handle := To.Parent;

    The_Data : constant Data_Handle := Declaration_Of (Id, Specification.all);

  begin
    if not Is_Null (The_Data) then
      if The_Data.all in Private_Type'class | Private_Extension_Type'class then
        The_Data.Location.Data := Item;
        Set_Used (Item);
        Id.Data := The_Data;
        Set_Used (The_Data);
      end if;
    end if;
  end Update_Private_Type;


  procedure Complete_Type (Id         : Identifier_Handle;
                           Item       : Data_Handle;
                           The_Cursor : Tree.Cursor) is

    The_Data : constant Data_Handle := The_Cursor.all;

  begin
    if The_Data.all in Incomplete_Type'class then
      --TEST------------------------
      --Write_Log ("Complete Type");
      ------------------------------
      The_Cursor.all := Item;
      The_Data.Location.Data := Item;
      Set_Used (Item);
      Id.Data := The_Data;
      Set_Used (The_Data);
    end if;
  end Complete_Type;


  procedure Link_To_Incomplete_Type (Id : Identifier_Handle;
                                     To : Unit_Handle) is
    The_Scope  : Unit_Handle := To;
    The_Handle : Data_Handle;
  begin
    if The_Scope.all in Package_Body'class then -- Lookup own Specification
      The_Scope := Unit_Handle(Unit_Body_Handle(The_Scope).Specification);
      if The_Scope /= null then
        The_Handle := Inner_Declaration_Of (Id, The_Scope.all);
        if not Is_Null (The_Handle) then
          if The_Handle.all in Incomplete_Type'class then
            Id.Data := The_Handle;
            Set_Used (The_Handle);
          end if;
        end if;
      end if;
    end if;
  end Link_To_Incomplete_Type;


  procedure Declare_Type (Id   : Identifier_Handle;
                          To   : Unit_Handle;
                          Item : Data_Handle) with Inline is
    The_Cursor : Tree.Cursor;
  begin
    Declare_Item (Id, To, Item, The_Cursor);
    if The_Cursor.all /= Item then
      if The_Cursor.all.all in Used_Subprogram'class then
        The_Cursor.all := Item;
        Log.Write ("%%% the used subprogram " & Image_Of (Id.all) & " is hidden in " & Full_Name_Of (Data_Handle(To)));
      else
        Complete_Type (Id, Item, The_Cursor);
      end if;
    elsif To.all in Private_Block then
      Update_Private_Type (Id, To, Item);
    elsif Item.all in Record_Type'class then
      Link_To_Incomplete_Type (Id, To);
    end if;
  end Declare_Type;


  function Declared_Type (Id   : Identifier_Handle;
                          To   : Unit_Handle;
                          Item : Data_Handle) return Data_Handle with Inline is
  begin
    if not Is_Null (Id) then
      Declare_Type (Id, To, Item);
    end if;
    return Item;
  end Declared_Type;


  procedure New_Subtype (Id            : Identifier_Handle;
                         Parent        : Unit_Handle;
                         From_Type     : Data_Handle;
                         Is_Class_Wide : Boolean) is
  begin
    --TEST-----------------------------------------------
    --Write_Log ("*** new Subtype " & Image_Of (Id.all));
    -----------------------------------------------------
    Declare_Type (Id   => Id,
                  To   => Parent,
                  Item => new Sub_Type'(Location      => Id,
                                        Is_Used       => False,
                                        Parent        => Parent,
                                        Parent_Type   => From_Type,
                                        Is_Class_Wide => Is_Class_Wide));
  end New_Subtype;


  function New_Enumeration_Type (Id     : Identifier_Handle;
                                 Parent : Unit_Handle) return Data_Handle is
  begin
    --TEST--------------------------------------------------------
    --Write_Log ("*** new Enumeration_Type " & Image_Of (Id.all));
    --------------------------------------------------------------
    return Declared_Type (Id   => Id,
                          To   => Parent,
                          Item => new Enumeration_Type'(Location      => Id,
                                                        Is_Used       => False,
                                                        Parent        => Parent,
                                                        Parent_Type   => null,
                                                        Values        => Tree.Empty,
                                                        Last_Position => 0));
  end New_Enumeration_Type;


  function New_Signed_Integer_Type (Id     : Identifier_Handle;
                                    Parent : Unit_Handle) return Data_Handle is
  begin
    --TEST-----------------------------------------------------------
    --Write_Log ("*** new Signed_Integer_Type " & Image_Of (Id.all));
    -----------------------------------------------------------------
    return Declared_Type (Id   => Id,
                          To   => Parent,
                          Item => new Signed_Integer_Type'(Location    => Id,
                                                           Is_Used     => False,
                                                           Parent      => Parent,
                                                           Parent_Type => Predefined_Root_Integer));
  end New_Signed_Integer_Type;


  function New_Modular_Integer_Type (Id     : Identifier_Handle;
                                     Parent : Unit_Handle) return Data_Handle is
  begin
    --TEST------------------------------------------------------------
    --Write_Log ("*** new Modular_Integer_Type " & Image_Of (Id.all));
    ------------------------------------------------------------------
    return Declared_Type (Id   => Id,
                          To   => Parent,
                          Item => new Modular_Integer_Type'(Location    => Id,
                                                            Is_Used     => False,
                                                            Parent      => Parent,
                                                            Parent_Type => Predefined_Root_Integer));
  end New_Modular_Integer_Type;


  function New_Floating_Point_Type (Id     : Identifier_Handle;
                                    Parent : Unit_Handle) return Data_Handle is
  begin
    --TEST-----------------------------------------------------------
    --Write_Log ("*** new Floating_Point_Type " & Image_Of (Id.all));
    -----------------------------------------------------------------
    return Declared_Type (Id   => Id,
                          To   => Parent,
                          Item => new Floating_Point_Type'(Location    => Id,
                                                           Is_Used     => False,
                                                           Parent      => Parent,
                                                           Parent_Type => Predefined_Root_Real));
  end New_Floating_Point_Type;


  function New_Fixed_Point_Type (Id     : Identifier_Handle;
                                 Parent : Unit_Handle) return Data_Handle is
  begin
    --TEST--------------------------------------------------------
    --Write_Log ("*** new Fixed_Point_Type " & Image_Of (Id.all));
    --------------------------------------------------------------
    return Declared_Type (Id   => Id,
                          To   => Parent,
                          Item => new Fixed_Point_Type'(Location    => Id,
                                                        Is_Used     => False,
                                                        Parent      => Parent,
                                                        Parent_Type => Predefined_Root_Real));
  end New_Fixed_Point_Type;


  function New_Record_Type (Id            : Identifier_Handle;
                            Is_Abstract   : Boolean;
                            Is_Tagged     : Boolean;
                            Parent        : Unit_Handle;
                            Ancestor      : Data_Handle;
                            Discriminants : List.Item;
                            Interfaces    : List.Item) return Data_Handle is
    The_Item : Data_Handle;
  begin
    if Is_Abstract then
      if Is_Tagged or else Is_Tagged_Type (Ancestor) then
        --TEST-------------------------------------------------------------------
        --Write_Log ("*** new Abstract_Tagged_Record_Type " & Image_Of (Id.all));
        -------------------------------------------------------------------------
        The_Item := new Abstract_Tagged_Record_Type'(Location      => Id,
                                                     Is_Used       => False,
                                                     Parent        => Parent,
                                                     Parent_Type   => Ancestor,
                                                     Discriminants => Discriminants,
                                                     Components    => List.Empty,
                                                     Methods       => List.Empty,
                                                     Interfaces    => Interfaces,
                                                     Aspects       => null);
      else
        --TEST------------------------------------------------------------
        --Write_Log ("*** new Abstract_Record_Type " & Image_Of (Id.all));
        ------------------------------------------------------------------
        The_Item := new Abstract_Record_Type'(Location      => Id,
                                              Is_Used       => False,
                                              Parent        => Parent,
                                              Parent_Type   => Ancestor,
                                              Discriminants => Discriminants,
                                              Components    => List.Empty,
                                              Aspects       => null);
      end if;
    else
      if Is_Tagged or else Is_Tagged_Type (Ancestor)  then
        --TEST----------------------------------------------------------
        --Write_Log ("*** new Tagged_Record_Type " & Image_Of (Id.all));
        ----------------------------------------------------------------
        The_Item := new Tagged_Record_Type'(Location      => Id,
                                            Is_Used       => False,
                                            Parent        => Parent,
                                            Parent_Type   => Ancestor,
                                            Discriminants => Discriminants,
                                            Components    => List.Empty,
                                            Methods       => List.Empty,
                                            Interfaces    => Interfaces,
                                            Aspects       => null);
      else
        --TEST---------------------------------------------------
        --Write_Log ("*** new Record_Type " & Image_Of (Id.all));
        ---------------------------------------------------------
        The_Item := new Record_Type'(Location      => Id,
                                     Is_Used       => False,
                                     Parent        => Parent,
                                     Parent_Type   => Ancestor,
                                     Discriminants => Discriminants,
                                     Components    => List.Empty,
                                     Aspects       => null);
      end if;
    end if;
    return Declared_Type (Id   => Id,
                          To   => Parent,
                          Item => The_Item);
  end New_Record_Type;


  function New_Array_Type (Id         : Identifier_Handle;
                           Parent     : Unit_Handle;
                           Definition : Array_Definition_Handle) return Data_Handle is

    Parent_Type : Data_Handle;

  begin
    --TEST--------------------------------------------------------------------------------------------
    --if Id /= null then
    --  Write_Log ("*** new Array_Type " & Image_Of (Id.all));
    --  Write_Log ("        Component: " & Ada.Tags.External_Tag (Definition.Component_Type.all'Tag));
    --end if;
    --------------------------------------------------------------------------------------------------
    if not Is_Null (Definition.Component_Type) and then
      Definition.Dimension = 1 and then Root_Type_Of (Definition.Component_Type).all in Enumeration_Type'class
    then
      Parent_Type := Predefined_Root_String;
    end if;
    return Declared_Type (Id   => Id,
                          To   => Parent,
                          Item => new Array_Type'(Location    => Id,
                                                  Is_Used     => False,
                                                  Parent      => Parent,
                                                  Parent_Type => Parent_Type,
                                                  Definition  => Definition));
  end New_Array_Type;


  function New_Subprogram_Access_Type (Id      : Identifier_Handle;
                                       Parent  : Unit_Handle;
                                       Profile : Subprogram_Profile) return Data_Handle is
  begin
    --TEST--------------------------------------------------------------
    --Write_Log ("*** new Subprogram_Access_Type " & Image_Of (Id.all));
    --------------------------------------------------------------------
    return Declared_Type (Id   => Id,
                          To   => Parent,
                          Item => new Subprogram_Access_Type'(Location    => Id,
                                                              Is_Used     => False,
                                                              Parent      => Parent,
                                                              Parent_Type => null,
                                                              Profile     => Profile));
  end New_Subprogram_Access_Type;


  function New_Protected_Subprogram_Access_Type (Id      : Identifier_Handle;
                                                 Parent  : Unit_Handle;
                                                 Profile : Subprogram_Profile) return Data_Handle is
  begin
    --TEST------------------------------------------------------------------------
    --Write_Log ("*** new Protected_Subprogram_Access_Type " & Image_Of (Id.all));
    ------------------------------------------------------------------------------
    return Declared_Type (Id   => Id,
                          To   => Parent,
                          Item => new Protected_Subprogram_Access_Type'(Location    => Id,
                                                                        Is_Used     => False,
                                                                        Parent      => Parent,
                                                                        Parent_Type => null,
                                                                        Profile     => Profile));
  end New_Protected_Subprogram_Access_Type;


  function New_Access_Type (Id      : Identifier_Handle;
                            Parent  : Unit_Handle;
                            To_Type : Data_Handle) return Data_Handle is
  begin
    --TEST----------------------------------------------------=------------------------
    --if Is_Null (Id) then
    --  Write_Log ("*** new Access_Type ACCESS TO " & Image_Of (To_Type.Location.all));
    --else
    --  Write_Log ("*** new Access_Type " & Image_Of (Id.all));
    --end if;
    --------------------------------------------------------=--------------------------
    return Declared_Type (Id   => Id,
                          To   => Parent,
                          Item => new Access_Type'(Location    => Id,
                                                   Is_Used     => False,
                                                   Parent      => Parent,
                                                   Parent_Type => To_Type));
  end New_Access_Type;


  function New_Class_Access_Type (Id      : Identifier_Handle;
                                  Parent  : Unit_Handle;
                                  To_Type : Data_Handle) return Data_Handle is
  begin
    --TEST---------------------------------------------------------
    --Write_Log ("*** new Class_Access_Type " & Image_Of (Id.all));
    ---------------------------------------------------------------
    return Declared_Type (Id   => Id,
                          To   => Parent,
                          Item => new Class_Access_Type'(Location    => Id,
                                                         Is_Used     => False,
                                                         Parent      => Parent,
                                                         Parent_Type => To_Type));
  end New_Class_Access_Type;


  function New_Derived_Type (Id        : Identifier_Handle;
                             Parent    : Unit_Handle;
                             From_Type : Data_Handle) return Data_Handle is

    The_Item : Data_Handle;

    procedure Visit (Cursor : Tree.Cursor) is
      Enumeration : constant Enumeration_Value_Handle := Enumeration_Value_Handle(Cursor.all);
    begin
      New_Enumeration_Value (Enumeration.Location, Parent, The_Item);
    end Visit;

    procedure Add_Subprograms is new Tree.Iterator (Visit);

  begin
    --TEST----------------------------------------------------
    --Write_Log ("*** new Derived_Type " & Image_Of (Id.all));
    ----------------------------------------------------------
    if Is_Null (From_Type) then
      Write_Log ("%%% new Derived_Type (FROM NULL)" & Image_Of (Id.all));
      return null;
    end if;
    if From_Type.all in Enumeration_Type'class then
      declare
        The_Enumeration : Enumeration_Handle := Enumeration_Handle(From_Type);
      begin
        while The_Enumeration.Parent_Type /= null and then
          The_Enumeration.Parent_Type.all in Enumeration_Type'class
        loop
          The_Enumeration := Enumeration_Handle(The_Enumeration.Parent_Type);
        end loop;
        The_Item := new Enumeration_Type'(Location      => Id,
                                          Is_Used       => False,
                                          Parent        => Parent,
                                          Parent_Type   => From_Type,
                                          Values        => Tree.Empty,
                                          Last_Position => The_Enumeration.Last_Position);
        Add_Subprograms (The_Enumeration.Values);
      end;
    else
      The_Item := new Derived_Type'(Location    => Id,
                                    Is_Used     => False,
                                    Parent      => Parent,
                                    Parent_Type => From_Type,
                                    Aggregate   => null);
    end if;
    return Declared_Type (Id   => Id,
                          To   => Parent,
                          Item => The_Item);
  end New_Derived_Type;


  function New_Private_Extension_Type (Id            : Identifier_Handle;
                                       Is_Abstract   : Boolean;
                                       Parent        : Unit_Handle;
                                       From_Type     : Data_Handle;
                                       Discriminants : List.Item;
                                       Interfaces    : List.Item) return Data_Handle is
    The_Item : Data_Handle;
  begin
    if Is_Abstract then
      --TEST-----------------------------------------------------------------------
      --Write_Log ("*** new Abstract_Private_Extension_Type " & Image_Of (Id.all));
      -----------------------------------------------------------------------------
      The_Item := new Abstract_Private_Extension_Type'(Location      => Id,
                                                       Is_Used       => False,
                                                       Parent        => Parent,
                                                       Parent_Type   => From_Type,
                                                       Discriminants => Discriminants,
                                                       Aspects       => null,
                                                       Methods       => List.Empty,
                                                       Interfaces    => Interfaces);
    else
      --TEST--------------------------------------------------------------
      --Write_Log ("*** new Private_Extension_Type " & Image_Of (Id.all));
      --------------------------------------------------------------------
      The_Item := new Private_Extension_Type'(Location      => Id,
                                              Is_Used       => False,
                                              Parent        => Parent,
                                              Parent_Type   => From_Type,
                                              Discriminants => Discriminants,
                                              Aspects       => null,
                                              Methods       => List.Empty,
                                              Interfaces    => Interfaces);
    end if;
    return Declared_Type (Id   => Id,
                          To   => Parent,
                          Item => The_Item);
  end New_Private_Extension_Type;


  function New_Incomplete_Type (Id     : Identifier_Handle;
                                Parent : Unit_Handle) return Data_Handle is
  begin
    --TEST-------------------------------------------------------
    --Write_Log ("*** new Incomplete_Type " & Image_Of (Id.all));
    -------------------------------------------------------------
    return Declared_Type (Id   => Id,
                          To   => Parent,
                          Item => new Incomplete_Type'(Location    => Id,
                                                       Is_Used     => False,
                                                       Parent      => Parent,
                                                       Parent_Type => null));
  end New_Incomplete_Type;


  function New_Private_Type (Id            : Identifier_Handle;
                             Is_Tagged     : Boolean;
                             Parent        : Unit_Handle;
                             Discriminants : List.Item) return Data_Handle is
    The_Item : Data_Handle;
  begin
    if Is_Tagged then
      --TEST-----------------------------------------------------------
      --Write_Log ("*** new Tagged_Private_Type " & Image_Of (Id.all));
      -----------------------------------------------------------------
      The_Item := new Tagged_Private_Type'(Location      => Id,
                                           Is_Used       => False,
                                           Parent        => Parent,
                                           Parent_Type   => null,
                                           Discriminants => Discriminants,
                                           Aspects       => null,
                                           Methods       => List.Empty,
                                           Interfaces    => List.Empty);
    else
      --TEST----------------------------------------------------
      --Write_Log ("*** new Private_Type " & Image_Of (Id.all));
      ----------------------------------------------------------
      The_Item := new Private_Type'(Location      => Id,
                                    Is_Used       => False,
                                    Parent        => Parent,
                                    Parent_Type   => null,
                                    Aspects       => null,
                                    Discriminants => Discriminants);
    end if;
    return Declared_Type (Id   => Id,
                          To   => Parent,
                          Item => The_Item);
  end New_Private_Type;


  procedure Add_Iterator_Aspects (To      : Data_Handle;
                                  Aspects : Aspect_Specification) is

    function New_Iterable_Aspects return Iterable_Aspect_Handle is
    begin
      return new Iterable_Aspects'(Element     => Aspect_For (Lexical.Is_Element, Aspects),
                                   Has_Element => Aspect_For (Lexical.Is_Has_Element, Aspects),
                                   First       => Aspect_For (Lexical.Is_First, Aspects),
                                   Next        => Aspect_For (Lexical.Is_Next, Aspects));
    end New_Iterable_Aspects;

    function New_Aggregate_Aspects return Aggregate_Aspect_Handle is
    begin
      return new Aggregate_Aspects'(Empty          => Aspect_For (Lexical.Is_Empty, Aspects),
                                    Add_Named      => Aspect_For (Lexical.Is_Add_Named, Aspects),
                                    Add_Unnamed    => Aspect_For (Lexical.Is_Add_Unnamed, Aspects),
                                    New_Indexed    => Aspect_For (Lexical.Is_New_Indexed, Aspects),
                                    Assign_Indexed => Aspect_For (Lexical.Is_Assign_Indexed, Aspects));
    end New_Aggregate_Aspects;

    procedure Handle_Inherited_Aspect (Subprogram       : Identifier_Handle;
                                       Parameters_Count : Natural) is
      The_Type : Data_Handle :=  Private_Type_Handle(To).Parent_Type;
    begin
      if not Is_Null (Subprogram) and not Is_Null (The_Type) then
        if The_Type.all in Instantiated_Type'class then
          The_Type := Data.Item_Instantiation(The_Type).Item;
          if not Is_Null (The_Type) then
            if The_Type.all in Tagged_Private_Type'class then
              declare
                Unit : constant Tagged_Private_Handle := Tagged_Private_Handle(The_Type);
              begin
                for The_Method of Unit.Methods loop
                  if The_Method.all in Data.Subprogram_Declaration'class then
                    if The_Method.Location = Subprogram then
                      if Subprogram_Declaration_Handle(The_Method).Profile.Parameters'length = Parameters_Count then
                        Subprogram.Data := The_Method;
                        Declaration_Handle(The_Method).Is_Used := True;
                      end if;
                    end if;
                  end if;
                end loop;
              end;
            end if;
          end if;
        end if;
      end if;
    end Handle_Inherited_Aspect;

  begin -- Add_Iterator_Aspects
    if To /= null then
      --TEST--------------------------------------------------------------------
      --Write_Log ("*** add Iterator_Aspects to " & Image_Of (To.Location.all));
      --------------------------------------------------------------------------
      if To.all in Private_Type'class then
        if Aspects /= No_Aspects then
          declare
            Item_Handle : constant Private_Type_Handle := Private_Type_Handle(To);
          begin
            Item_Handle.Aspects := new Private_Aspects'
              (Aggregate => New_Aggregate_Aspects,
               Iterator => (Element           => Aspect_For (Lexical.Is_Iterator_Element, Aspects),
                            Constant_Indexing => Aspect_For (Lexical.Is_Constant_Indexing, Aspects),
                            Variable_Indexing => Aspect_For (Lexical.Is_Variable_Indexing, Aspects),
                            Default_Iterator  => Aspect_For (Lexical.Is_Default_Iterator, Aspects)),
               Iterable => New_Iterable_Aspects);
            Handle_Inherited_Aspect (Item_Handle.Aspects.Aggregate.Add_Unnamed, Parameters_Count => 2);
          end;
        end if;
      elsif To.all in Derived_Type'class then
        if Aspects /= No_Aspects then
          Derived_Type_Handle(To).Aggregate := New_Aggregate_Aspects;
        end if;
      elsif To.all in Record_Type'class then
        if Aspects /= No_Aspects then
          Record_Handle(To).Aspects := New_Iterable_Aspects;
        end if;
      end if;
    end if;
  end Add_Iterator_Aspects;


  function New_Protected_Type (Id            : Identifier_Handle;
                               Parent        : Unit_Handle;
                               Discriminants : List.Item) return Unit_Handle is

    Self : constant Active_Declaration_Handle := new Protected_Declaration'(Location       => Id,
                                                                            Is_Used        => False,
                                                                            Parent         => Parent,
                                                                            Used_Packages  => null,
                                                                            Implementation => null,
                                                                            Declarations   => Tree.Empty);

    Declarations : constant List.Elements := List.Elements_Of (Discriminants);

  begin
    --TEST------------------------------------------------------
    --Write_Log ("*** new Protected_Type " & Image_Of (Id.all));
    ------------------------------------------------------------
    for Index in Declarations'range loop
      Tree.Add (Declarations(Index), To => Self.Declarations);
    end loop;
    Declare_Type (Id   => Id,
                  To   => Parent,
                  Item => new Protected_Type'(Location      => Id,
                                              Is_Used       => False,
                                              Parent        => Parent,
                                              Parent_Type   => null,
                                              Discriminants => Discriminants,
                                              Object        => Unit_Handle(Self)));
    return Unit_Handle(Self);
  end New_Protected_Type;


  function New_Task_Type (Id            : Identifier_Handle;
                          Parent        : Unit_Handle;
                          Discriminants : List.Item) return Unit_Handle is

    Self : constant Active_Declaration_Handle := new Task_Declaration'(Location       => Id,
                                                                       Is_Used        => False,
                                                                       Parent         => Parent,
                                                                       Used_Packages  => null,
                                                                       Implementation => null,
                                                                       Declarations   => Tree.Empty);

    Declarations : constant List.Elements := List.Elements_Of (Discriminants);

  begin
    --TEST-------------------------------------------------
    --Write_Log ("*** new Task_Type " & Image_Of (Id.all));
    -------------------------------------------------------
    for Index in Declarations'range loop
      Tree.Add (Declarations(Index), To => Self.Declarations);
    end loop;
    Declare_Type (Id   => Id,
                  To   => Parent,
                  Item => new Task_Type'(Location      => Id,
                                         Is_Used       => False,
                                         Parent        => Parent,
                                         Parent_Type   => null,
                                         Discriminants => Discriminants,
                                         Object        => Unit_Handle(Self)));
    return Unit_Handle(Self);
  end New_Task_Type;

  ----------------------------------------------------------------------------------------------------------------------

  function New_Interface_Type (Id         : Identifier_Handle;
                               Parent     : Unit_Handle;
                               Interfaces : List.Item) return Data_Handle is
  begin
    --TEST------------------------------------------------------
    --Write_Log ("*** new Interface_Type " & Image_Of (Id.all));
    ------------------------------------------------------------
    return Declared_Type (Id   => Id,
                          To   => Parent,
                          Item => new Interface_Type'(Location    => Id,
                                                      Is_Used     => False,
                                                      Parent      => Parent,
                                                      Parent_Type => null,
                                                      Methods     => List.Empty,
                                                      Progenitors => Interfaces));
  end New_Interface_Type;

  ----------------------------------------------------------------------------------------------------------------------

  function New_Instantiation (Item          : Data_Handle;
                              Instantiation : Instantiation_Handle) return Data_Handle is
  begin
    if Is_Subtype (Item) then
      --TEST--------------------------------------------------------------------
      --Write_Log ("*** new Instantiated_Type " & Image_Of (Item.Location.all));
      --------------------------------------------------------------------------
       return new Instantiated_Type'(Location      => Item.Location,
                                     Item          => Item,
                                     Instantiation => Instantiation);
    else
      --TEST--------------------------------------------------------------------
      --Write_Log ("*** new Instantiated_Item " & Image_Of (Item.Location.all));
      --------------------------------------------------------------------------
       return new Instantiated_Item'(Location      => Item.Location,
                                     Item          => Item,
                                     Instantiation => Instantiation);
    end if;
  end New_Instantiation;

  ----------------------------------------------------------------------------------------------------------------------

  procedure New_End_Identifier (Id   : Identifier_Handle;
                                Self : Data_Handle) is
  begin
    --TEST---------------------------------------------------
    --Write_Log (">>> new End_Identifier: " & Image_Of (Id));
    ---------------------------------------------------------
    if not Is_Null(Self) then
      Id.Data := new End_Identifier'(Location => Self.Location);
    end if;
  end New_End_Identifier;

  ----------------------------------------------------------------------------------------------------------------------

  function New_Component_List (Component_Names : Identifiers;
                               Subtype_Mark    : Data_Handle;
                               Is_Class_Wide   : Boolean;
                               Has_Default     : Boolean;
                               Parent          : Unit_Handle) return List.Item is
    The_List   : List.Item;
    The_Object : Data_Handle;

  begin
    --TEST------------------------------------------------------------------------------------------
    --Write_Log ("*** new List of Discriminants or Components: " & Image_Of (Component_Names, ','));
    --if Subtype_Mark = null then
    --  Write_Log ("    component type = NULL");
    --else
    --  Write_Log ("    component type = " & Ada.Tags.External_Tag (Subtype_Mark.all'tag));
    --end if;
    ------------------------------------------------------------------------------------------------
    for The_Index in Component_Names'range loop
      The_Object := new Data_Object'(Location      => Component_Names(The_Index),
                                     Is_Used       => False,
                                     Parent        => Parent,
                                     Object_Type   => Subtype_Mark,
                                     Is_Class_Wide => Is_Class_Wide,
                                     Has_Default   => Has_Default);
      Component_Names(The_Index).Data := The_Object;
      The_List.Append (The_Object);
    end loop;
    return The_List;
  end New_Component_List;

  function New_Parameter_List (Parameter_Names : Identifiers;
                               Subtype_Mark    : Data_Handle;
                               Is_Class_Wide   : Boolean;
                               Has_Default     : Boolean;
                               Parent          : Unit_Handle) return List.Item is
    The_List   : List.Item;
    The_Object : Data_Handle;

  begin
    --TEST---------------------------------------------------------------------------------
    --Write_Log ("*** new List of Parameters: " & Image_Of (Parameter_Names, ','));
    --if Subtype_Mark = null then
    --  Write_Log ("    component type = NULL");
    --else
    --  Write_Log ("    component type = " & Ada.Tags.External_Tag (Subtype_Mark.all'tag));
    --end if;
    ---------------------------------------------------------------------------------------
    for The_Index in Parameter_Names'range loop
      The_Object := new Parameter'(Location      => Parameter_Names(The_Index),
                                   Is_Used       => False,
                                   Parent        => Parent,
                                   Object_Type   => Subtype_Mark,
                                   Is_Class_Wide => Is_Class_Wide,
                                   Has_Default   => Has_Default);
      Parameter_Names(The_Index).Data := The_Object;
      The_List.Append (The_Object);
    end loop;
    return The_List;
  end New_Parameter_List;

  ----------------------------------------------------------------------------------------------------------------------

  procedure New_Enumeration_Value (Id           : Identifier_Handle;
                                   Parent       : Unit_Handle;
                                   Subtype_Mark : Data_Handle) is

    Enumeration : constant Enumeration_Handle := Enumeration_Handle(Subtype_Mark);

    Next_Position : constant Integer := Enumeration.Last_Position + 1;

  begin
    --TEST---------------------------------------------------------
    --Write_Log ("*** new Enumeration_Value " & Image_Of (Id.all));
    ---------------------------------------------------------------
    Declare_Subprogram (Id   => Id,
                        To   => Parent,
                        Item => new Enumeration_Value'(Location => Id,
                                                       Is_Used  => True,
                                                       Parent   => Parent,
                                                       Profile  => Subprogram_Profile'(Parameters  => null,
                                                                                       Result_Type => Subtype_Mark),
                                                       Position => Next_Position,
                                                       Overload => null));
    Tree.Add (Id.Data, To => Enumeration.Values);
    Enumeration.Last_Position := Next_Position;
  end New_Enumeration_Value;


  procedure New_Exceptions (Names  : Identifiers;
                            Parent : Unit_Handle) is
  begin
    --TEST-------------------------------------------------------
    --Write_Log ("*** new Exceptions: " & Image_Of (Names, ','));
    -------------------------------------------------------------
    for The_Index in Names'range loop
      Declare_Item (Id   => Names(The_Index),
                    To   => Parent,
                    Item => new Exception_Object'(Location => Names(The_Index),
                                                  Is_Used  => False,
                                                  Parent   => Parent));
    end loop;
  end New_Exceptions;


  procedure New_Label (Id     : Identifier_Handle;
                       Parent : Unit_Handle) is
  begin
    --TEST----------------------------------------------
    --Write_Log ("*** new Label: " & Image_Of (Id.all));
    ----------------------------------------------------
    Declare_Item (Id   => Id,
                  To   => Parent,
                  Item => new Label'(Location      => Id,
                                     Is_Used       => True,
                                     Parent        => Parent));
  end New_Label;


  procedure New_Object (Id            : Identifier_Handle;
                        Subtype_Mark  : Data_Handle;
                        Is_Class_Wide : Boolean;
                        Has_Default   : Boolean;
                        Parent        : Unit_Handle) is
  begin
    --TEST----------------------------------------------
    --Write_Log ("*** new Object " & Image_Of (Id.all));
    ----------------------------------------------------
    Declare_Item (Id   => Id,
                  To   => Parent,
                  Item => new Data_Object'(Location      => Id,
                                           Is_Used       => False,
                                           Parent        => Parent,
                                           Object_Type   => Subtype_Mark,
                                           Is_Class_Wide => Is_Class_Wide,
                                           Has_Default   => Has_Default));
  end New_Object;


  procedure New_Objects (Names         : Identifiers;
                         Subtype_Mark  : Data_Handle;
                         Is_Class_Wide : Boolean;
                         Has_Default   : Boolean;
                         Parent        : Unit_Handle) is
  begin
    --TEST----------------------------------------------------
    --Write_Log ("*** new Objects: " & Image_Of (Names, ','));
    ----------------------------------------------------------
    for The_Index in Names'range loop
      Declare_Item (Id   => Names(The_Index),
                    To   => Parent,
                    Item => new Data_Object'(Location      => Names(The_Index),
                                             Is_Used       => False,
                                             Parent        => Parent,
                                             Object_Type   => Subtype_Mark,
                                             Is_Class_Wide => Is_Class_Wide,
                                             Has_Default   => Has_Default));
    end loop;
  end New_Objects;


  procedure New_Constants (Names         : Identifiers;
                           Subtype_Mark  : Data_Handle;
                           Is_Class_Wide : Boolean;
                           Has_Default   : Boolean;
                           Parent        : Unit_Handle) is

    function Incomplete_Data_For (Id : Identifier_Handle) return Incomplete_Object_Handle is
    begin
      if Parent.all in Private_Block'class then
        declare
          The_Data : constant Data_Handle := Declaration_Of (Id, Parent.Parent.all);
        begin
          if not Is_Null (The_Data) and then The_Data.all in Incomplete_Object'class then
            return Incomplete_Object_Handle(The_Data);
          end if;
        end;
      end if;
      return null;
    end Incomplete_Data_For;

  begin --New_Constants
    --TEST------------------------------------------------------
    --Write_Log ("*** new Constants: " & Image_Of (Names, ','));
    ------------------------------------------------------------
    for The_Index in Names'range loop
      if Has_Default then
        declare
          Complete_Data : constant Complete_Object_Handle
            :=  new Complete_Object'(Location        => Names(The_Index),
                                     Is_Used         => False,
                                     Parent          => Parent,
                                     Object_Type     => Subtype_Mark,
                                     Is_Class_Wide   => Is_Class_Wide,
                                     Has_Default     => Has_Default,
                                     Incomplete_Data => Incomplete_Data_For (Names(The_Index)));
        begin
          Declare_Item (Id   => Names(The_Index),
                        To   => Parent,
                        Item => Data_Handle(Complete_Data));
          if Complete_Data.Incomplete_Data /= null then
            Complete_Data.Is_Used := True;
            Complete_Data.Incomplete_Data.Complete_Data := Complete_Data;
          end if;
        end;
      else
        Declare_Item (Id   => Names(The_Index),
                      To   => Parent,
                      Item => new Incomplete_Object'(Location      => Names(The_Index),
                                                     Is_Used       => False,
                                                     Parent        => Parent,
                                                     Object_Type   => Subtype_Mark,
                                                     Is_Class_Wide => Is_Class_Wide,
                                                     Has_Default   => Has_Default,
                                                     Complete_Data => null));
        Handle_Aspects (Names(The_Index));
      end if;
    end loop;
  end New_Constants;

  ----------------------------------------------------------------------------------------------------------------------

  procedure Declare_Formal (Item : Formal_Handle;
                            To   : Formal_Block_Handle) with Inline is
  begin
    To.Last_Position := To.Last_Position + 1;
    Item.Position := To.Last_Position;
    Declare_Item (Id   => Item.Location,
                  To   => Unit_Handle(To),
                  Item => Data_Handle(Item));
  end Declare_Formal;


  procedure Declare_Formal (Item : Formal_Subprogram_Handle;
                            To   : Formal_Block_Handle) with Inline is
    The_Cursor : Tree.Cursor;
  begin
    To.Last_Position := To.Last_Position + 1;
    Item.Position := To.Last_Position;
    Declare_Item (Id         => Item.Location,
                  To         => Unit_Handle(To),
                  Item       => Data_Handle(Item),
                  The_Cursor => The_Cursor);
    declare
      The_Parameter : constant Formal_Handle := Formal_Handle(The_Cursor.all);
    begin
      if The_Parameter /= null and then The_Parameter /= Formal_Handle(Item) then -- overload
        if The_Parameter.all in Formal_Subprogram'class then
          The_Cursor.all := Data_Handle(Item);
          Item.Overload := Formal_Subprogram_Handle(The_Parameter);
        end if;
      end if;
    end;
  end Declare_Formal;


  procedure Declare_Formal_Type (Item : Data_Handle;
                                 To   : Formal_Block_Handle) with Inline is
  begin
    Declare_Formal (To   => To,
                    Item => new Formal_Type'(Location    => Item.Location,
                                             Is_Used     => False,
                                             Parent      => Unit_Handle(To),
                                             Declaration => Item,
                                             Position    => Positive'last)); -- undefined
  end Declare_Formal_Type;


  procedure New_Formal_Decimal_Fixed_Point_Type (Id         : Identifier_Handle;
                                                 Parameters : Formal_Block_Handle) is

  begin
    --TEST-----------------------------------------------------------------------
    --Write_Log ("*** new Formal_Decimal_Fixed_Point_Type " & Image_Of (Id.all));
    -----------------------------------------------------------------------------
    Declare_Formal_Type (To   => Parameters,
                         Item => new Fixed_Point_Type'(Location    => Id,
                                                       Is_Used     => False,
                                                       Parent      => Unit_Handle(Parameters),
                                                       Parent_Type => null));
  end New_Formal_Decimal_Fixed_Point_Type;


  procedure New_Formal_Discrete_Type (Id         : Identifier_Handle;
                                      Parameters : Formal_Block_Handle) is
  begin
    --TEST------------------------------------------------------------
    --Write_Log ("*** new Formal_Discrete_Type " & Image_Of (Id.all));
    ------------------------------------------------------------------
    Declare_Formal_Type (To   => Parameters,
                         Item => new Discrete_Type'(Location    => Id,
                                                    Is_Used     => False,
                                                    Parent      => Unit_Handle(Parameters),
                                                    Parent_Type => null));
  end New_Formal_Discrete_Type;


  procedure New_Formal_Floating_Point_Type (Id         : Identifier_Handle;
                                            Parameters : Formal_Block_Handle) is
  begin
    --TEST------------------------------------------------------------------
    --Write_Log ("*** new Formal_Floating_Point_Type " & Image_Of (Id.all));
    ------------------------------------------------------------------------
    Declare_Formal_Type (To   => Parameters,
                         Item => new Floating_Point_Type'(Location    => Id,
                                                          Is_Used     => False,
                                                          Parent      => Unit_Handle(Parameters),
                                                          Parent_Type => null));
  end New_Formal_Floating_Point_Type;


  procedure New_Formal_Modular_Type (Id         : Identifier_Handle;
                                     Parameters : Formal_Block_Handle) is
  begin
    --TEST-----------------------------------------------------------
    --Write_Log ("*** new Formal_Modular_Type " & Image_Of (Id.all));
    -----------------------------------------------------------------
    Declare_Formal_Type (To   => Parameters,
                         Item => new Modular_Integer_Type'(Location    => Id,
                                                           Is_Used     => False,
                                                           Parent      => Unit_Handle(Parameters),
                                                           Parent_Type => null));
  end New_Formal_Modular_Type;


  procedure New_Formal_Ordinary_Fixed_Point_Type (Id         : Identifier_Handle;
                                                  Parameters : Formal_Block_Handle) is
  begin
    --TEST------------------------------------------------------------------------
    --Write_Log ("*** new Formal_Ordinary_Fixed_Point_Type " & Image_Of (Id.all));
    ------------------------------------------------------------------------------
    Declare_Formal_Type (To   => Parameters,
                         Item => new Fixed_Point_Type'(Location    => Id,
                                                       Is_Used     => False,
                                                       Parent      => Unit_Handle(Parameters),
                                                       Parent_Type => null));
  end New_Formal_Ordinary_Fixed_Point_Type;


  procedure New_Formal_Signed_Integer_Type (Id         : Identifier_Handle;
                                            Parameters : Formal_Block_Handle) is
  begin
    --TEST------------------------------------------------------------------
    --Write_Log ("*** new Formal_Signed_Integer_Type " & Image_Of (Id.all));
    ------------------------------------------------------------------------
    Declare_Formal_Type (To   => Parameters,
                         Item => new Signed_Integer_Type'(Location    => Id,
                                                          Is_Used     => False,
                                                          Parent      => Unit_Handle(Parameters),
                                                          Parent_Type => null));
  end New_Formal_Signed_Integer_Type;


  procedure New_Formal_Private_Type (Id         : Identifier_Handle;
                                     Parameters : Formal_Block_Handle) is
  begin
    --TEST-----------------------------------------------------------
    --Write_Log ("*** new Formal_Private_Type " & Image_Of (Id.all));
    -----------------------------------------------------------------
    Declare_Formal_Type (To   => Parameters,
                         Item => new Private_Type'(Location      => Id,
                                                   Is_Used       => False,
                                                   Parent        => Unit_Handle(Parameters),
                                                   Parent_Type   => null,
                                                   Aspects       => null,
                                                   Discriminants => List.Empty));
  end New_Formal_Private_Type;


  procedure New_Formal_Incomplete_Type (Id         : Identifier_Handle;
                                        Parameters : Formal_Block_Handle) is
  begin
    --TEST------------------------------------------------------------------
    --Write_Log ("*** new New_Formal_Incomplete_Type " & Image_Of (Id.all));
    ------------------------------------------------------------------------
    Declare_Formal_Type (To   => Parameters,
                         Item => new Incomplete_Type'(Location      => Id,
                                                      Is_Used       => False,
                                                      Parent        => Unit_Handle(Parameters),
                                                      Parent_Type   => null));
  end New_Formal_Incomplete_Type;


  procedure New_Formal_Derived_Type (Id         : Identifier_Handle;
                                     From_Type  : Data_Handle;
                                     Parameters : Formal_Block_Handle) is
  begin
    --TEST-----------------------------------------------------------
    --Write_Log ("*** new Formal_Derived_Type " & Image_Of (Id.all));
    -----------------------------------------------------------------
    Declare_Formal_Type (To   => Parameters,
                         Item => new Derived_Type'(Location    => Id,
                                                   Is_Used     => False,
                                                   Parent      => Unit_Handle(Parameters),
                                                   Parent_Type => From_Type,
                                                   Aggregate   => null));
  end New_Formal_Derived_Type;

  procedure New_Formal_Private_Extension_Type (Id         : Identifier_Handle;
                                               From_Type  : Data_Handle;
                                               Parameters : Formal_Block_Handle) is
  begin
    --TEST---------------------------------------------------------------------
    --Write_Log ("*** new Formal_Private_Extension_Type " & Image_Of (Id.all));
    ---------------------------------------------------------------------------
    Declare_Formal_Type (To   => Parameters,
                         Item => new Private_Extension_Type'(Location      => Id,
                                                             Is_Used       => False,
                                                             Parent        => Unit_Handle(Parameters),
                                                             Parent_Type   => From_Type,
                                                             Discriminants => List.Empty,
                                                             Aspects       => null,
                                                             Methods       => List.Empty,
                                                             Interfaces    => List.Empty));
  end New_Formal_Private_Extension_Type;


  procedure New_Formal_Abstract_Private_Extension_Type (Id         : Identifier_Handle;
                                                        From_Type  : Data_Handle;
                                                        Parameters : Formal_Block_Handle) is
  begin
    --TEST------------------------------------------------------------------------------
    --Write_Log ("*** new Formal_Abstract_Private_Extension_Type " & Image_Of (Id.all));
    ------------------------------------------------------------------------------------
    Declare_Formal_Type (To   => Parameters,
                         Item => new Abstract_Private_Extension_Type'(Location      => Id,
                                                                      Is_Used       => False,
                                                                      Parent        => Unit_Handle(Parameters),
                                                                      Parent_Type   => From_Type,
                                                                      Discriminants => List.Empty,
                                                                      Aspects       => null,
                                                                      Methods       => List.Empty,
                                                                      Interfaces    => List.Empty));
  end New_Formal_Abstract_Private_Extension_Type;


  procedure New_Formal_Limited_Private_Type (Id         : Identifier_Handle;
                                             Parameters : Formal_Block_Handle) is
  begin
    --TEST-------------------------------------------------------------------
    --Write_Log ("*** new Formal_Limited_Private_Type " & Image_Of (Id.all));
    -------------------------------------------------------------------------
    Declare_Formal_Type (To   => Parameters,
                         Item => new Private_Type'(Location      => Id,
                                                   Is_Used       => False,
                                                   Parent        => Unit_Handle(Parameters),
                                                   Parent_Type   => null,
                                                   Aspects       => null,
                                                   Discriminants => List.Empty));
  end New_Formal_Limited_Private_Type;


  procedure New_Formal_Access_Type (Id         : Identifier_Handle;
                                    To_Type    : Data_Handle;
                                    Parameters : Formal_Block_Handle) is
  begin
    --TEST----------------------------------------------------------
    --Write_Log ("*** new Formal_Access_Type " & Image_Of (Id.all));
    ----------------------------------------------------------------
    Declare_Formal_Type (To   => Parameters,
                         Item => new Access_Type'(Location    => Id,
                                                  Is_Used     => False,
                                                  Parent      => Unit_Handle(Parameters),
                                                  Parent_Type => To_Type));
  end New_Formal_Access_Type;


  procedure New_Formal_Array_Type (Id         : Identifier_Handle;
                                   Definition : Array_Definition_Handle;
                                   Parameters : Formal_Block_Handle) is
  begin
    --TEST---------------------------------------------------------
    --Write_Log ("*** new Formal_Array_Type " & Image_Of (Id.all));
    ---------------------------------------------------------------
    Declare_Formal_Type (To   => Parameters,
                         Item => new Array_Type'(Location    => Id,
                                                 Is_Used     => False,
                                                 Parent      => Unit_Handle(Parameters),
                                                 Parent_Type => null,
                                                 Definition  => Definition));
  end New_Formal_Array_Type;

  ----------------------------------------------------------------------------------------------------------------------

  procedure New_Formal_Objects (Formal_Names : Identifiers;
                                Subtype_Mark : Data_Handle;
                                Has_Default  : Boolean;
                                Parameters   : Formal_Block_Handle) is
  begin
    --TEST------------------------------------------------------------------
    --Write_Log ("*** new Formal_Objects: " & Image_Of (Formal_Names, ','));
    ------------------------------------------------------------------------
    for The_Index in Formal_Names'range loop
      Declare_Formal (To   => Parameters,
                      Item => new Formal_Object'(Location    => Formal_Names(The_Index),
                                                 Is_Used     => False,
                                                 Parent      => Unit_Handle(Parameters),
                                                 Declaration => Subtype_Mark,
                                                 Position    => Positive'last, -- undefined
                                                 Has_Default => Has_Default));
    end loop;
  end New_Formal_Objects;

  ----------------------------------------------------------------------------------------------------------------------

  procedure New_Formal_Subprogram_Declaration (Id           : Identifier_Handle;
                                               Default_Name : Identifier_Handle;
                                               Profile      : Subprogram_Profile;
                                               Parameters   : Formal_Block_Handle) is

    Item : constant Formal_Subprogram_Handle
      := new Formal_Subprogram'(Location     => Id,
                                Default_Name => Default_Name,
                                Is_Used      => False,
                                Parent       => Unit_Handle(Parameters),
                                Declaration  => null,
                                Profile      => Profile,
                                Overload     => null,
                                Position     => Positive'last); -- undefined
  begin
    --TEST---------------------------------------------------------
    --Write_Log ("*** new Formal_Subprogram " & Image_Of (Id.all));
    ---------------------------------------------------------------
    Item.Declaration := Data_Handle(Item);
    if not Is_Null (Default_Name) and then Is_Null (Default_Name.Data) then
      Default_Name.Data := Data_Handle(Item);
    end if;
    Declare_Formal (To   => Parameters,
                    Item => Item);
  end New_Formal_Subprogram_Declaration;

  ----------------------------------------------------------------------------------------------------------------------

  procedure New_Formal_Package_Declaration (Id              : Identifier_Handle;
                                            Generic_Package : Unit_Handle;
                                            Actual_Part     : List.Elements_Access;
                                            Parameters      : Formal_Block_Handle) is

    Item : constant Formal_Package_Handle
      := new Formal_Package'(Location        => Id,
                             Is_Used         => False,
                             Parent          => Unit_Handle(Parameters),
                             Generic_Package => Generic_Package,
                             Actual_Part     => Actual_Part,
                             Declaration     => null,
                             Position        => Positive'last); -- undefined
  begin
    --TEST------------------------------------------------------
    --Write_Log ("*** new Formal_Package " & Image_Of (Id.all));
    ------------------------------------------------------------
    Item.Declaration := Data_Handle(Item);
    Declare_Formal (To   => Parameters,
                    Item => Formal_Handle(Item));
  end New_Formal_Package_Declaration;

  ----------------------------------------------------------------------------------------------------------------------

  function New_Profile return Subprogram_Profile is
  begin
    return Subprogram_Profile'(Parameters  => null,
                               Result_Type => null);
  end New_Profile;


  function New_Profile (Parameters : List.Item) return Subprogram_Profile is
  begin
    return Subprogram_Profile'(Parameters  => new List.Elements'(List.Elements_Of (Parameters)),
                               Result_Type => null);
  end New_Profile;


  function New_Array_Definition (Index_Subtypes : List.Item;
                                 Component_Type : Data_Handle) return Array_Definition_Handle is
  begin
    return new Array_Definition'(Dimension      => Index_Subtypes.Length,
                                 Index_Subtypes => List.Elements_Of (Index_Subtypes),
                                 Component_Type => Component_Type);
  end New_Array_Definition;


  ----------------------------------------------------------------------------------------------------------------------
  -- Overload association
  ----------------------------------------------------------------------------------------------------------------------

  procedure Overload (Item : Declaration_Handle;
                      To   : Declaration_Handle) is
  begin
    if Item = To then
      Write_Log ("%%% Overload Self");
      return;
    end if;
    case Data_Kind_Of (To.all) is
    when Is_Subprogram_Declaration
       | Is_Generic_Subprogram_Declaration
       | Is_Entry_Declaration
    =>
      Subprogram_Declaration_Handle(To).Overload := Item;
    when Is_Subprogram_Body
       | Is_Subprogram_Renaming
       | Is_Entry_Body
    =>
      Subprogram_Body_Handle(To).Overload := Item;
    when Is_Used_Subprogram =>
      Used_Subprogram_Handle(To).Overload := Item;
    when Is_Used_Generic_Subprogram =>
      Used_Generic_Subprogram_Handle(To).Overload := Item;
    when Is_Enumeration_Value =>
      Enumeration_Value_Handle(To).Overload := Item;
    when others =>
      raise Program_Error;
    end case;
  end Overload;


  function Is_Overloaded (Item : Declaration_Handle) return Boolean is
  begin
    return Overload_Of (Item) /= null;
  end Is_Overloaded;


  function Overload_Of (Item : Declaration_Handle) return Declaration_Handle is
  begin
    case Data_Kind_Of (Item.all) is
    when Is_Subprogram_Declaration
       | Is_Generic_Subprogram_Declaration
       | Is_Entry_Declaration
    =>
      return Subprogram_Declaration_Handle(Item).Overload;
    when Is_Subprogram_Body
       | Is_Subprogram_Renaming
       | Is_Entry_Body
    =>
      return Subprogram_Body_Handle(Item).Overload;
    when Is_Used_Subprogram =>
      return Used_Subprogram_Handle(Item).Overload;
    when Is_Used_Generic_Subprogram =>
      return Used_Generic_Subprogram_Handle(Item).Overload;
    when Is_Enumeration_Value =>
      return Enumeration_Value_Handle(Item).Overload;
    when others =>
      return null;
    end case;
  end Overload_Of;


  function Profile_Of (Item : Declaration_Handle) return Subprogram_Profile is
    The_Item : Declaration_Handle := Item;
  begin
    while The_Item /= null loop
      case Data_Kind_Of (The_Item.all) is
      when Is_Subprogram_Declaration
         | Is_Generic_Subprogram_Declaration
         | Is_Entry_Declaration
      -- | Is_Formal_Subprogram %%%
      =>
        return Subprogram_Declaration_Handle(The_Item).Profile;
      when Is_Subprogram_Body
         | Is_Subprogram_Renaming
         | Is_Entry_Body
      =>
        return Subprogram_Body_Handle(The_Item).Profile;
      when Is_Subprogram_Access_Type =>
        return Subprogram_Access_Handle(The_Item).Profile;
      when Is_Accept_Declaration =>
        return Accept_Handle(The_Item).Profile;
      when Is_Used_Subprogram =>
        return Used_Subprogram_Handle(The_Item).Profile;
      when Is_Used_Generic_Subprogram =>
        return Used_Generic_Subprogram_Handle(The_Item).Profile;
      when Is_Enumeration_Value =>
        return Enumeration_Value_Handle(Item).Profile;
      when others =>
        if The_Item.all in Declaration_Type'class then
          The_Item := Declaration_Handle(The_Item.Parent);
        else
          exit;
        end if;
      end case;
    end loop;
    return (null, null);
  end Profile_Of;


  function Parameter_Index_Of (Profile  : Subprogram_Profile;
                               Selector : Identifier_Handle) return Natural is
  begin
    for The_Index in Profile.Parameters'range loop
      declare
        Profile_Parameter : constant Object_Handle := Object_Handle(Profile.Parameters(The_Index));
      begin
        if Selector = Profile_Parameter.Location then
          Selector.Data := Data_Handle(Profile_Parameter);
          return The_Index;
        end if;
      end;
    end loop;
    return Parameter_Not_Found;
  exception
  when others =>
    return Parameter_Not_Found;
  end Parameter_Index_Of;


  function Has_Default_Parameter (Profile      : Subprogram_Profile;
                                  The_Position : Positive) return Boolean is
  begin
    return Object_Handle(Profile.Parameters(The_Position)).Has_Default;
  exception
  when others =>
    return False;
  end Has_Default_Parameter;


  function Parameter_Object_Of (Profile      : Subprogram_Profile;
                                The_Position : Positive) return Object_Handle is
  begin
    if The_Position <= Profile.Parameters'last then
      declare
        The_Parameter : constant Data_Handle := Profile.Parameters(The_Position);
      begin
        if not Is_Null (The_Parameter) then
          if The_Parameter.all in Data_Object'class then
            return Object_Handle(Profile.Parameters(The_Position));
          else
            Write_Log ("%%% Parameter type: " & Ada.Tags.External_Tag (The_Parameter.all'tag));
          end if;
        end if;
      end;
    end if;
    return null;
  exception
  when others =>
    return null;
  end Parameter_Object_Of;


  function Actual_Type_Of (Formal_Parameter : Formal_Handle;
                           Instantiation    : Instantiation_Handle) return Data_Handle is

    The_Position : constant Natural := Formal_Parameter.Position;

  begin
    if The_Position <= Instantiation.Actual_Part'length then
      return Instantiation.Actual_Part (The_Position);
    else
      return null;
    end if;
  exception
  when Item: others =>
    Log.Write ("!!! Data.Actual_Type_Of ", Item);
    return null;
  end Actual_Type_Of;


  function Is_Associated (Parameter_Type   : Data_Handle;
                          Formal_Parameter : Formal_Handle;
                          Instantiation    : Instantiation_Handle) return Boolean is
    The_Position : constant Natural := Formal_Parameter.Position;
    The_Handle   : Instantiation_Handle := Instantiation;
  begin
    while The_Handle /= null loop
      --TEST-----------------------------------------------------------------------------------
      --Write_Log ("Is_Associated - Instantiation: " & Name.Image_Of (The_Handle.Location.Id));
      --Write_Log ("              - Position     :" & Natural'image (Position));
      -----------------------------------------------------------------------------------------
      if The_Position <= The_Handle.Actual_Part'length then
        if Type_Of (Parameter_Type) = Type_Of (The_Handle.Actual_Part (The_Position)) then
          --TEST-----------------------
          --Write_Log ("- ASSOCIATED");
          -----------------------------
          return True;
        end if;
      end if;
      The_Handle := The_Handle.Parent_Instantiation;
    end loop;
    --TEST---------------------------
    --Write_Log ("- NOT ASSOCIATED");
    ---------------------------------
    return False;
  exception
  when Item: others =>
    Log.Write ("!!! Data.Is_Associated ", Item);
    return False;
  end Is_Associated;


  function Matches (Left, Right   : Data_Handle;
                    Instantiation : Instantiation_Handle) return Boolean is
    The_Left  : Data_Handle := Left;
    The_Right : Data_Handle := Right;
  begin
    --TEST--------------------------------------------------------
    --Write_Log ("Matches - Left  : " & Full_Name_Of (The_Left));
    --Write_Log ("          Right : " & Full_Name_Of (The_Right));
    --------------------------------------------------------------
    if Base_Type_Of (The_Left) = Base_Type_Of (The_Right) then
      return True;
    else
      if ((not Is_Null (The_Left)) and then (The_Left.all in Class_Access_Type'class)) and
         ((not Is_Null (The_Right)) and then (The_Right.all in Class_Access_Type'class))
      then
        The_Left := Type_Handle(The_Left).Parent_Type;
        The_Right := Type_Handle(The_Right).Parent_Type;
        if ((not Is_Null (The_Left)) and then (The_Left.all in Tagged_Record_Type'class)) and
           ((not Is_Null (The_Right)) and then (The_Right.all in Private_Extension_Type'class))
        then
          The_Right := Tagged_Private_Handle(The_Right).Parent_Type;
          if The_Left = The_Right then
            return True;
          end if;
        end if;
      end if;
      if Instantiation /= null then
        if not Is_Null (The_Left) and then The_Left.all in Formal_Type'class then
          --TEST--------------------------------
          --Write_Log ("LEFT FORMAL PARAMETER");
          --------------------------------------
          if Is_Associated (Parameter_Type   => The_Right,
                            Formal_Parameter => Formal_Handle(The_Left),
                            Instantiation    => Instantiation)
          then
            return True;
          elsif not Is_Null (The_Right) and then The_Right.all in Formal_Type'class then
            --TEST-------------------------------------
            --Write_Log ("AND RIGHT FORMAL PARAMETER");
            -------------------------------------------
            return True;
          end if;
        elsif not Is_Null (The_Right) and then The_Right.all in Formal_Type'class then
          --TEST---------------------------------
          --Write_Log ("RIGHT FORMAL PARAMETER");
          ---------------------------------------
          if Is_Associated (Parameter_Type   => The_Left,
                            Formal_Parameter => Formal_Handle(The_Right),
                            Instantiation    => Instantiation)
          then
            return True;
          elsif not Is_Null (The_Left) and then The_Left.all in Formal_Type'class then
            --TEST------------------------------------
            --Write_Log ("AND LEFT FORMAL PARAMETER");
            ------------------------------------------
            return True;
          end if;
        end if;
      end if;
    end if;
    --TEST---------------------------------------------------------------------------------------
    --if Is_Null (The_Left) then
    --  Write_Log ("LEFT TYPE NULL");
    --elsif Is_Null (The_Right) then
    --  Write_Log ("RIGHT TYPE NULL");
    --else
    --  Write_Log ("LEFT TYPE (" & Ada.Tags.External_Tag (The_Left.all'tag) & ") <> RIGHT TYPE ("
    --                           & Ada.Tags.External_Tag (The_Right.all'tag) & ")");
    --end if;
    ---------------------------------------------------------------------------------------------
    return False;
  end Matches;


  function Matches (Left, Right   : Subprogram_Profile;
                    Instantiation : Instantiation_Handle) return Boolean is
    use type List.Elements_Access;
  begin
    --TEST--------------------------------------
    --Write_Log ("Matches Subprogram Profile ");
    --------------------------------------------
    if not Matches (Left.Result_Type, Right.Result_Type, Instantiation) then
      --TEST----------------------------
      --Write_Log ("- NO RESULT MATCH");
      ----------------------------------
      return False;
    end if;
    if (Left.Parameters = null) and (Right.Parameters = null) then
      return True;
    elsif (Left.Parameters = null) or (Right.Parameters = null) then
      return False;
    end if;
    if Left.Parameters'length /= Right.Parameters'length then
      --TEST----------------------------------------
      --Write_Log ("- LEFT'length /= RIGHT'length");
      ----------------------------------------------
      return False;
    end if;
    for Index in Left.Parameters'range loop
      declare
        Left_Type  : constant Data_Handle := Object_Handle(Left.Parameters(Index)).Object_Type;
        Right_Type : constant Data_Handle := Object_Handle(Right.Parameters(Index)).Object_Type;
      begin
        --TEST------------------------------------------------------------------------
        --if Is_Null (Left_Type.Location) then
        --  Write_Log ("- LEFT : NO NAME");
        --else
        --  Write_Log ("- LEFT : " & Image_Of (Left_Type.Location.all));
        --end if;
        --Write_Log ("       TYPE : " & Ada.Tags.External_Tag (Left_Type.all'tag));
        --if Is_Null (Right_Type) then
        --  Write_Log ("- RIGHT  : UNKNOWN TYPE");
        --else
        --  if Is_Null (Right_Type.Location) then
        --    Write_Log ("- RIGHT  : NO NAME");
        --  else
        --    Write_Log ("- RIGHT  : " & Image_Of (Right_Type.Location.all));
        --  end if;
        --  Write_Log ("       TYPE : " & Ada.Tags.External_Tag (Right_Type.all'tag));
        --end if;
        ------------------------------------------------------------------------------
        if not Matches (Left_Type, Right_Type, Instantiation) then
          --TEST-------------------------------
          --Write_Log ("- NO PARAMETER MATCH");
          -------------------------------------
          return False;
        end if;
      end;
    end loop;
    --TEST------------------
    --Write_Log ("MATCHED");
    ------------------------
    return True;
  end Matches;


  function Matches (The_Parameter     : Data_Handle;
                    Profile_Parameter : Object_Handle;
                    Instantiation     : Instantiation_Handle) return Boolean is

    The_Parameter_Type         : Data_Handle := The_Parameter;
    The_Profile_Parameter_Type : Data_Handle := Profile_Parameter.Object_Type;
    The_Instantiation          : Instantiation_Handle := Instantiation;

  begin
    --TEST-----------------------------------------------------------------------------------------
    --Write_Log ("Matches - Parameter         : " & Full_Name_Of (The_Parameter));
    --Write_Log ("          Profile_Parameter : " & Full_Name_Of (Data_Handle(Profile_Parameter)));
    -----------------------------------------------------------------------------------------------
    if Is_Null (The_Parameter) then
      --TEST---------------------------
      --Write_Log ("- TYPE : UNKNOWN");
      ---------------------------------
      return False;
    end if;
    --TEST----------------------------------------------------------------------------------------
    --if Is_Null (The_Parameter.Location) then
    --  Write_Log ("- PARAMETER : NO NAME");
    --else
    --  Write_Log ("- PARAMETER : " & Image_Of (The_Parameter.Location.all));
    --end if;
    --Write_Log ("       TYPE : " & Ada.Tags.External_Tag (The_Parameter.all'tag));
    --if Is_Null (The_Profile_Parameter_Type) then
    --  Write_Log ("- EXPECTED  : UNKNOWN TYPE");
    --else
    --  if Is_Null (The_Profile_Parameter_Type.Location) then
    --    Write_Log ("- EXPECTED  : UNKNOWN NAME");
    --  else
    --    Write_Log ("- EXPECTED  : " & Image_Of (The_Profile_Parameter_Type.Location.all));
    --  end if;
    --  Write_Log ("       TYPE : " & Ada.Tags.External_Tag (The_Profile_Parameter_Type.all'tag));
    --end if;
    ----------------------------------------------------------------------------------------------
    if ((not Is_Null (The_Parameter_Type)) and then (The_Parameter_Type.all in Access_Type'class)) and
       ((not Is_Null (The_Profile_Parameter_Type)) and then (The_Profile_Parameter_Type.all in Class_Access_Type'class))
    then
      The_Parameter_Type := Type_Handle(The_Parameter_Type).Parent_Type;
      The_Profile_Parameter_Type := Type_Handle(The_Profile_Parameter_Type).Parent_Type;
    end if;
    if not Is_Null (The_Parameter_Type) and then The_Parameter_Type.all in Instantiated_Type'class then
      The_Parameter_Type := Item_Instantiation(The_Parameter_Type).Item;
    end if;
    if not Is_Null (The_Profile_Parameter_Type) and then The_Profile_Parameter_Type.all in Instantiated_Type'class then
      The_Instantiation := Item_Instantiation(The_Profile_Parameter_Type).Instantiation;
      The_Profile_Parameter_Type := Item_Instantiation(The_Profile_Parameter_Type).Item;
    end if;
    declare
      Profile_Parameter_Type : constant Data_Handle := Type_Of (The_Profile_Parameter_Type);
      Parameter_Type         : constant Data_Handle := Type_Of (The_Parameter_Type);
    begin
      if Is_Null (Profile_Parameter_Type) or Is_Null (Parameter_Type) then
        --TEST------------------
        --Write_Log ("NO TYPE");
        ------------------------
        return False;
      elsif Parameter_Type = Profile_Parameter_Type then
        --TEST------------------
        --Write_Log ("MATCHED");
        ------------------------
        return True;
      else
        if Parameter_Type.all in Enumeration_Type'class then --%%%
          declare
            Base_Type : constant Data_Handle := Base_Type_Of (Profile_Parameter_Type);
          begin
            if Base_Type.all in Enumeration_Type'class and then Parameter_Type = Base_Type then
              --TEST------------------------------
              --Write_Log ("ENUMERATION MATCHED");
              ------------------------------------
              return True;
            end if;
          end;
        end if;
        if (The_Instantiation /= null) and then Profile_Parameter_Type.all in Formal_Type'class then
          --TEST-----------------------------------------------------------------------------
          --Write_Log ("-> FORMAL PARAMETER = " & Image_Of (Profile_Parameter.Location.all));
          -----------------------------------------------------------------------------------
          if Is_Associated (Parameter_Type   => Parameter_Type,
                            Formal_Parameter => Formal_Handle(Profile_Parameter_Type),
                            Instantiation    => The_Instantiation)
          then
            --TEST------------------
            --Write_Log ("MATCHED");
            ------------------------
            return True;
          end if;
        elsif Parameter_Type.all in Formal_Type'class and then Profile_Parameter_Type.all in Formal_Type'class then
          --TEST-------------------------
          --Write_Log ("MATCHED Formal");
          -------------------------------
          return True ;
        else
          if Profile_Parameter.Is_Class_Wide then
            --TEST---------------------------------------------------------------------------------
            --Write_Log ("-> CLASS WIDE PARAMETER = " & Image_Of (Profile_Parameter.Location.all));
            ---------------------------------------------------------------------------------------
            if Is_In (Profile_Parameter_Type, The_Parameter) then
              --TEST------------------
              --Write_Log ("MATCHED");
              ------------------------
              return True;
            end if;
          elsif Is_Tagged_Type (Parameter_Type) and then Is_Tagged_Type (Profile_Parameter_Type) then
            if Is_In (Profile_Parameter_Type, Parameter_Type) then
              --TEST------------------------------
              --Write_Log ("MATCHED TAGGED TYPE");
              ------------------------------------
              return True;
            end if;
          end if;
        end if;
      end if;
    end;
    --TEST----------------------
    --Write_Log ("NOT MATCHED");
    ----------------------------
    return False;
  end Matches;


  procedure Associate_Entry (Id         : Identifier_Handle;
                             With_Entry : Unit_Handle;
                             Profile    : Subprogram_Profile) is
    The_Entry : Declaration_Handle;
  begin
    if With_Entry /= null and then With_Entry.all in Entry_Declaration then
      The_Entry := Declaration_Handle(With_Entry);
      loop
        if Profile_Of (The_Entry) = Profile then
          Id.Data := Data_Handle(The_Entry);
          exit;
        end if;
        The_Entry := Overload_Of (The_Entry);
        exit when The_Entry = null;
      end loop;
    end if;
  end Associate_Entry;


  ----------------------------------------------------------------------------------------------------------------------
  -- Selectors
  ----------------------------------------------------------------------------------------------------------------------

  function Is_Marked_Unused (Id : Handle) return Boolean is
    The_Data   : constant Data_Handle := Identifier_Handle(Id).Data;
    Image      : constant String := Token.Image_Of(Identifier_Handle(Id).all);
    Annotation : constant Special_Comment_Handle := Line_End_Special_Comment (Id);
  begin
    if Text.Location_Of ("Unused", Image) /= Text.Not_Found then
      return True;
    elsif Text.Location_Of ("Ignored", Image) /= Text.Not_Found then
      return True;
    elsif Text.Location_Of ("Dummy", Image) /= Text.Not_Found then
      return True;
    elsif The_Data /= null and then The_Data.all in Parameter'class then
      return Is_Marked ("UP", Annotation);
    else
      return Is_Marked ("UD", Annotation);
    end if;
  end Is_Marked_Unused;


  function Is_Unused (Id : Handle) return Boolean is
    The_Data : constant Data_Handle := Identifier_Handle(Id).Data;
  begin
    if not Is_Null (The_Data) and then Handle(The_Data.Location) = Id then -- is declaration
      declare
        The_Declaration : Declaration_Handle := Declaration_Handle(The_Data);
      begin
        if not The_Declaration.Is_Used then
          if The_Declaration.all in With_Declaration'class then
            return not Is_Marked_Unused (Id);
          end if;
          while The_Declaration.Parent /= null loop
            if The_Declaration.Parent.all in Unit_Body'class then
              return not Is_Marked_Unused (Id);
            end if;
            if The_Declaration.Parent.all in Declaration_Type'class then
              The_Declaration := Declaration_Handle(The_Declaration.Parent);
            else
              return False;
            end if;
          end loop;
        end if;
      end;
    end if;
    return False;
  end Is_Unused;


  function Kind_Of (Id : Handle) return Declaration_Kind is
  begin
    if Id.all in Identifier'class then
      declare
        The_Data : constant Data_Handle := Identifier_Handle(Id).Data;
      begin -- Kind_Of
        if not Is_Null (The_Data) then
          if The_Data.all in Type_Declaration'class | Formal_Type'class then
            if Is_Unused (Id) then
              return Is_Unused_Type_Declaration;
            end if;
            return Is_Type;
          else
            if Is_Unused (Id) then
              return Is_Unused_Declaration;
            end if;
            return Is_Known;
          end if;
        end if;
      end;
    end if;
    return Is_Unknown;
  end Kind_Of;


  function Completion_Of (The_Type : Type_Handle) return Data_Handle is

    The_Unit : Unit_Body_Handle;

  begin
    if The_Type.Parent /= null and then The_Type.Parent.all in Private_Block'class then
      The_Unit := Unit_Declaration_Handle(The_Type.Parent.Parent).Implementation;
      if The_Unit /= null then
        return Declaration_Of (The_Type.Location, From => The_Unit.all);
      else
        if The_Type.Parent.Parent.Parent /= null then
          The_Unit := Unit_Declaration_Handle(The_Type.Parent.Parent.Parent).Implementation;
          if The_Unit /= null then
            declare
              Unit : constant Data_Handle := Declaration_Of (The_Type.Parent.Parent.Location, From => The_Unit.all);
            begin
              if Unit /= null then
                if Unit.all in Unit_Body'class then
                  return Declaration_Of (The_Type.Location, From => Unit_Body_Handle(Unit).all);
                end if;
              end if;
            end;
          end if;
        end if;
      end if;
    end if;
    return null;
  end Completion_Of;


  function Type_Of (Item : Data_Handle) return Data_Handle is
    The_Type : Data_Handle := Item;
  begin
    while not Is_Null (The_Type) loop
      if The_Type.all in Incomplete_Type'class then
        The_Type := The_Type.Location.Data;
        if The_Type.all in Incomplete_Type'class then
          The_Type := Completion_Of (Type_Handle(The_Type));
          if Is_Null (The_Type) then
            return null;
          end if;
        end if;
      end if;
      exit when not (The_Type.all in Sub_Type'class);
      The_Type := Type_Handle(The_Type).Parent_Type;
    end loop;
    return The_Type;
  end Type_Of;


  function Base_Type_Of (Item : Data_Handle) return Data_Handle is
    The_Type : Data_Handle := Item;
  begin
    if not Is_Null (The_Type) and then The_Type.all in Instantiated_Type'class then
      The_Type := Item_Instantiation(The_Type).Item;
    end if;
    while not Is_Null (The_Type) loop
      if The_Type.all in Incomplete_Type'class then
        The_Type := The_Type.Location.Data;
        if The_Type.all in Incomplete_Type'class then
          The_Type := Completion_Of (Type_Handle(The_Type));
          if Is_Null (The_Type) then
            return null;
          end if;
        end if;
      end if;
      exit when not (The_Type.all in Sub_Type'class or The_Type.all in Derived_Type'class);
      The_Type := Type_Handle(The_Type).Parent_Type;
    end loop;
    return The_Type;
  end Base_Type_Of;


  function Parent_Type_Of (Item              :     Data_Handle;
                           The_Instantiation : out Instantiation_Handle) return Type_Handle is
    The_Type : Data_Handle := Item;
  begin
    The_Instantiation := null;
    if not Is_Null (The_Type) then
      if The_Type.all in Instantiated_Item'class then
        The_Instantiation := Item_Instantiation(The_Type).Instantiation;
        The_Type := Item_Instantiation(The_Type).Item;
      end if;
      loop
        if The_Type.all in Incomplete_Type'class then
          The_Type := The_Type.Location.Data;
          if The_Type.all in Incomplete_Type'class then
            The_Type := Completion_Of (Type_Handle(The_Type));
            if Is_Null (The_Type) then
              return null;
            end if;
          end if;
        end if;
        if not (The_Type.all in Type_Declaration'class) then
          return null;
        end if;
        The_Type := Type_Handle(The_Type).Parent_Type;
        if Is_Null (The_Type) then
          return null;
        end if;
        if The_Type.all in Instantiated_Item'class then
          if The_Instantiation = null then
            The_Instantiation := Item_Instantiation(The_Type).Instantiation;
          end if;
          The_Type := Item_Instantiation(The_Type).Item;
        end if;
        exit when not (The_Type.all in Sub_Type'class);
      end loop;
      if The_Type.all in Type_Declaration'class and then not Is_Root_Type (The_Type) then
        return Type_Handle(The_Type);
      end if;
    end if;
    return null;
  end Parent_Type_Of;


  function Parent_Type_Of (Item : Data_Handle) return Type_Handle is
    The_Instantiation : Instantiation_Handle;
  begin
    return Parent_Type_Of (Item, The_Instantiation);
  end Parent_Type_Of;


  function Is_Tagged_Type (Item : Data_Handle) return Boolean is
  begin
    if Is_Null (Item) then
      return False;
    elsif Item.all in Tagged_Record_Type'class then
      return True;
    elsif Item.all in Tagged_Private_Type'class then
      return True;
    elsif Item.all in Private_Extension_Type'class then
      return True;
    else
      return False;
    end if;
  end Is_Tagged_Type;


  function Is_Subtype (Item : Data_Handle) return Boolean is
  begin
    return Item /= null and then
             ((Item.all in Data.Type_Declaration'class) or
              (Item.all in Data.Formal_Type'class) or
              (Item.all in Data.Instantiated_Type'class));
  end Is_Subtype;


  function Is_In (Parent_Class : Data_Handle;
                  Item         : Data_Handle) return Boolean is

    function Is_In (Interfaces : List.Item) return Boolean is
    begin
      for The_Interface of Interfaces loop
        if The_Interface = Parent_Class then
          return True;
        elsif Is_In (Interface_Handle(The_Interface).Progenitors) then
          return True;
        end if;
      end loop;
      return False;
    end Is_In;

    The_Item : Data_Handle := Item;

  begin -- Is_In
    while not Is_Null (The_Item) loop
      if The_Item.all in Instantiated_Type'class then
        The_Item := Item_Instantiation(The_Item).Item;
      end if;
      if The_Item.all in Incomplete_Type'class then
        The_Item := The_Item.Location.Data;
        if The_Item.all in Incomplete_Type'class then
          The_Item := Completion_Of (Type_Handle(The_Item));
          if Is_Null (The_Item) then
            return False;
          end if;
        end if;
      end if;
      if The_Item = Parent_Class then
        return True;
      elsif Parent_Class.all in Interface_Type'class then
        --TEST-----------------------------------------------------------------------------------------
        --Write_Log ("Is_In Interface " & Full_Name_Of (Parent_Class) & " " & Full_Name_Of (The_Item));
        -----------------------------------------------------------------------------------------------
        if The_Item.all in Tagged_Record_Type'class then
          if Is_In (Tagged_Record_Handle(The_Item).Interfaces) then
            return True;
          end if;
        elsif The_Item.all in Tagged_Private_Type'class then
          if Is_In (Tagged_Private_Handle(The_Item).Interfaces) then
            return True;
          end if;
        end if;
      end if;
      if not (The_Item.all in Type_Declaration'class) then
        return False;
      end if;
      The_Item := Type_Handle(The_Item).Parent_Type;
    end loop;
    return False;
  end Is_In;


  function Next_Abstract_Class_Of (Item : Type_Handle) return Type_Handle is
    The_Type : Data_Handle := Item.Parent_Type;
  begin
    while not Is_Null (The_Type) and then The_Type.all in Type_Declaration'class loop
      if The_Type.all in Abstract_Tagged_Record_Type'class then
        return Type_Handle(The_Type);
      elsif The_Type.all in Abstract_Private_Extension_Type'class then
        return Type_Handle(The_Type);
      elsif The_Type.all in Incomplete_Type'class then
        The_Type := The_Type.Location.Data;
        if The_Type.all in Incomplete_Type'class then
          The_Type := Completion_Of (Type_Handle(The_Type));
          if Is_Null (The_Type) then
            return null;
          end if;
        end if;
        if The_Type.all in Abstract_Tagged_Record_Type'class then
          return Type_Handle(The_Type);
        elsif The_Type.all in Abstract_Private_Extension_Type'class then
          return Type_Handle(The_Type);
        end if;
      end if;
      if Is_Null (Type_Handle(The_Type).Parent_Type) then
        return Type_Handle(The_Type);
      end if;
      The_Type := Type_Handle(The_Type).Parent_Type;
    end loop;
    return null;
  end Next_Abstract_Class_Of;


  function Root_Type_Of (Item : Data_Handle) return Data_Handle is
    The_Type : Data_Handle := Item;
  begin
    if Item /= null then
      if Item.all in Instantiated_Item'class then
        The_Type := Item_Instantiation(The_Type).Item;
      else
        The_Type := Item;
      end if;
      if The_Type.all in Type_Declaration'class then
        while not Is_Null (Type_Handle(The_Type).Parent_Type) loop
          The_Type := Type_Handle(The_Type).Parent_Type;
          if not (The_Type.all in Type_Declaration'class) then
            return Item;
          end if;
        end loop;
        return The_Type;
      end if;
    end if;
    return Item;
  end Root_Type_Of;


  function Object_Type_Of (Item : Data_Handle) return Data_Handle is
  begin
    if not Is_Null (Item) and then Item.all in Data_Object'class then
      return Object_Handle(Item).Object_Type;
    else
      return null;
    end if;
  end Object_Type_Of;


  function Range_Type_Of (Item : Data_Handle) return Data_Handle is
  begin
    if not Is_Null (Item) and then Item.all in Array_Type'class then
      return Array_Handle(Item).Definition.Index_Subtypes(1); -- dimension must be 1
    else
      return Item;
    end if;
  end Range_Type_Of;


  function Iterable_Type_Of (Item : Data_Handle) return Data_Handle is
    The_Type      : Data_Handle := Item;
    Aspect_Found  : Boolean := False;
    Element_Found : Boolean := False;
    The_Instantiations      : array (1..16) of Instantiation_Handle;
    The_Index : Natural := 0;
  begin
    while not Is_Null (The_Type) loop
      if The_Type.all in Instantiated_Type'class then
        if Item_Instantiation(The_Type).Instantiation /= null then
          The_Index := @ + 1;
          The_Instantiations(The_Index) := Item_Instantiation(The_Type).Instantiation;
        end if;
        The_Type := Item_Instantiation(The_Type).Item;
        exit when Is_Null (The_Type);
      end if;
      --TEST---------------------------------------------------------------------
      --if Is_Null (The_Type.Location) then
      --  Log.Write ("Iterable Name: NONE");
      --else
      --  Log.Write ("Iterable Name: " & Image_Of(The_Type.Location.all));
      --end if;
      --Log.Write ("Iterable Type: " & Ada.Tags.External_Tag (The_Type.all'tag));
      ---------------------------------------------------------------------------
      if The_Type.all in Private_Type'class then
        if The_Type.all in Tagged_Private_Type'class then
          declare
            Aspects : constant Private_Aspect_Handle := Private_Type_Handle(The_Type).Aspects;
          begin
            if Aspects = null then
              The_Type := Tagged_Private_Handle(The_Type).Parent_Type;
            elsif not Is_Null (Aspects.Iterator.Element) then
              The_Type := Aspects.Iterator.Element.Data;
              Aspect_Found := True;
            elsif Aspects.Iterable /= null and then not Is_Null (Aspects.Iterable.Element) then
              The_Type := Aspects.Iterable.Element.Data;
              Aspect_Found := True;
            else
              The_Type := Tagged_Private_Handle(The_Type).Parent_Type;
           end if;
          end;
        else -- not tagged private
          declare
            Aspects : constant Private_Aspect_Handle := Private_Type_Handle(The_Type).Aspects;
          begin
            if Aspects = null then
              The_Type := Private_Type_Handle(The_Type).Parent_Type;
            elsif Is_Null (Aspects.Iterable.Element) then
              exit;
            else
              The_Type := Aspects.Iterable.Element.Data;
              Aspect_Found := True;
            end if;
          end;
        end if;
      elsif The_Type.all in Record_Type'class then
        declare
          Aspects  : constant Iterable_Aspect_Handle := Record_Handle(The_Type).Aspects;
          Old_Type : Data_Handle;
          function Address_Of is new Ada.Unchecked_Conversion (Data_Handle, System.Address);
          use type System.Address;
        begin
          if Aspects /= null and then not Is_Null (Aspects.Element) then
            The_Type := Aspects.Element.Data;
            Aspect_Found := True;
          else
            if not Is_Null (The_Type.Location) then
              Old_Type := The_Type;
              The_Type := The_Type.Location.Data; -- type from public part
              if Address_Of (Old_Type) = Address_Of (The_Type) or else The_Type.all in Incomplete_Type'class then
                -- if self or incomplete type
                The_Type := Old_Type;
                if not Is_Null (Record_Handle(The_Type).Parent_Type) then
                  The_Type := Record_Handle(The_Type).Parent_Type;
                else
                  exit;
                end if;
              end if;
            elsif not Is_Null (Record_Handle(The_Type).Parent_Type) then
              The_Type := Record_Handle(The_Type).Parent_Type;
            else
              exit;
            end if;
          end if;
        end;
      elsif The_Type.all in Array_Type'class then
        The_Type := Array_Handle(The_Type).Definition.Component_Type;
        Element_Found := True;
      elsif The_Type.all in Derived_Type'class then
        The_Type := Type_Handle(The_Type).Parent_Type;
      elsif The_Type.all in Formal_Type'class then
        if Item.all in Instantiated_Item'class then
          The_Type := Actual_Declaration_Of (Formal_Handle(The_Type),
                                             Item_Instantiation(Item).Instantiation);
        else
          The_Type := Formal_Handle(The_Type).Declaration;
        end if;
      else
        Write_Log ("%%% Iterable " & Image_Of (Item.Location.all) & ": " & Ada.Tags.External_Tag (The_Type.all'tag));
        exit;
      end if;
      exit when Is_Null (The_Type);
      if Element_Found then
        return The_Type;
      elsif Aspect_Found then
        while The_Index > 0 loop
          if The_Type.all in Formal_Type'class then
            The_Type := Actual_Declaration_Of (Formal_Handle(The_Type),
                                               The_Instantiations(The_Index));
          end if;
          The_Index := @ - 1;
        end loop;
        declare
          Result_Type : constant Data_Handle := Data.Profile_Of (Data.Declaration_Handle(The_Type)).Result_Type;
        begin
          if not Is_Null (Result_Type) then
            The_Type := Result_Type;
          end if;
        end;
        return The_Type;
      end if;
    end loop;
    Write_Log ("%%% Iterable - no iterator element type");
    return Item;
  end Iterable_Type_Of;


  function Component_Choice_Of (Item     : Identifier_Handle;
                                The_Type : Record_Handle) return Data_Handle is
    The_Handle : Data_Handle;
  begin
    if not Is_Null (Item) then
      declare
        The_Record_Handle : Record_Handle := The_Type;
      begin
        loop
          The_Handle := Data_With (Item.Id, From => The_Record_Handle.Components);
          if not Is_Null (The_Handle) then
            Item.Data := The_Handle;
            Set_Used (The_Handle);
            return Object_Type_Of (The_Handle);
          else
            The_Handle := Data_With (Item.Id, From => The_Record_Handle.Discriminants);
            if not Is_Null (The_Handle) then
              Item.Data := The_Handle;
              Set_Used (The_Handle);
              return Object_Type_Of (The_Handle);
            end if;
          end if;
          exit when Is_Null (The_Record_Handle.Parent_Type)
                    or else not (The_Record_Handle.Parent_Type.all in Record_Type'class);
          The_Record_Handle := Record_Handle(The_Record_Handle.Parent_Type);
        end loop;
      end;
    end if;
    return null;
  end Component_Choice_Of;


  function Component_Choice_Of (The_Position : Positive;
                                The_Type     : Record_Handle) return Data_Handle is

    Discriminant_Count : constant Natural := The_Type.Discriminants.Length;

    The_Object : Data_Handle;

  begin
    if The_Position > Discriminant_Count then
      The_Object := The_Type.Components(The_Position - Discriminant_Count);
    else
      The_Object := The_Type.Discriminants(The_Position);
    end if;
    return Object_Type_Of (The_Object);
  exception
  when Constraint_Error =>
    return null;
  end Component_Choice_Of;


  function Formal_Position_Of (Item  : Identifier_Handle;
                               Scope : Formal_Block_Handle) return Natural is
    The_Cursor : Tree.Cursor;

    use type Tree.Cursor;

  begin
    if (not Is_Null (Item)) and (Scope /= null) then
      Tree.Get (Id          => Item.Id,
                From        => Scope.Declarations,
                Data_Cursor => The_Cursor);
      if The_Cursor /= null then
        Item.Data := The_Cursor.all;
        Set_Used (The_Cursor.all);
        return Formal_Handle(The_Cursor.all).Position;
      end if;
    end if;
    return List.Not_Found;
  end Formal_Position_Of;


  function Formal_Type_At (The_Position : Positive;
                           Scope        : Formal_Block_Handle) return Data_Handle is

    The_Type : Data_Handle;

    Found : exception;

  begin
    declare

      procedure Get_Type (Cursor : Tree.Cursor) is
        Item : constant Formal_Handle := Formal_Handle(Cursor.all);
      begin
        if Item.Position = The_Position then
          Set_Used (Data_Handle(Item));
          The_Type := Item.Declaration;
          raise Found;
        elsif Item.all in Formal_Subprogram'class then
          declare
            The_Item : Formal_Subprogram_Handle := Formal_Subprogram_Handle(Item).Overload;
          begin
            while The_Item /= null loop
              if The_Item.Position = The_Position then
                Set_Used (Data_Handle(The_Item));
                The_Type := The_Item.Declaration;
                raise Found;
              end if;
              The_Item := The_Item.Overload;
            end loop;
          end;
        end if;
      end Get_Type;

      procedure Find_Type is new Tree.Iterator (Get_Type);

    begin
      if Scope /= null then
        Find_Type (Scope.Declarations);
      end if;
    end;
    return null;
  exception
  when Found =>
    return The_Type;
  end Formal_Type_At;


  function Generic_Parameters_Of (Unit : Unit_Handle) return Formal_Block_Handle is
  begin
    if Unit /= null then
      case Data_Kind_Of (Unit.all) is
      when Is_Generic_Subprogram_Declaration =>
        return Generic_Subprogram_Handle(Unit).Generic_Parameters;
      when Is_Used_Generic_Subprogram =>
        return Used_Generic_Subprogram_Handle(Unit).Generic_Parameters;
      when Is_Generic_Package_Declaration =>
        return Generic_Package_Handle(Unit).Generic_Parameters;
      when Is_Subprogram_Body =>
        declare
          Specification : constant Unit_Declaration_Handle := Unit_Body_Handle(Unit).Specification;
        begin
          if Specification /= null and then Specification.all in Generic_Subprogram_Declaration'class then
            return Generic_Subprogram_Handle(Specification).Generic_Parameters;
          end if;
        end;
      when Is_Package_Body =>
        declare
          Specification : constant Unit_Declaration_Handle := Unit_Body_Handle(Unit).Specification;
        begin
          if Specification /= null and then Specification.all in Generic_Package_Declaration'class then
            return Generic_Package_Handle(Specification).Generic_Parameters;
          end if;
        end;
      when others =>
        null;
      end case;
    end if;
    return null;
  end Generic_Parameters_Of;


  procedure Get_Inner_Generic_Parameters_Of (The_Unit       : in out Unit_Handle;
                                             The_Parameters :    out Formal_Block_Handle) is
    The_Specification : Unit_Declaration_Handle;
  begin
    while The_Unit /= null loop
      if The_Unit.all in Unit_Declaration'class then
        The_Specification := Unit_Declaration_Handle(The_Unit);
      elsif The_Unit.all in Unit_Body'class then
        The_Specification := Unit_Body_Handle(The_Unit).Specification;
      end if;
      if The_Specification /= null then
        if The_Specification.all in Generic_Package_Declaration'class then
          The_Parameters := Generic_Package_Handle(The_Specification).Generic_Parameters;
          return;
        elsif The_Specification.all in Generic_Subprogram_Declaration'class then
          The_Parameters := Generic_Subprogram_Handle(The_Specification).Generic_Parameters;
          return;
        end if;
      end if;
      The_Unit := The_Unit.Parent;
    end loop;
    The_Parameters := null;
  end Get_Inner_Generic_Parameters_Of;


  function Actual_Declaration_Of (Formal        : Formal_Handle;
                                  Instantiation : Instantiation_Handle) return Data_Handle is
    The_Handle : Instantiation_Handle := Instantiation;
  begin
    --TEST----------------------------------------------------------------------------------------
    --Write_Log ("Actual_Declaration - Instantiation: " & Name.Image_Of (The_Handle.Location.Id));
    ----------------------------------------------------------------------------------------------
    loop
      if The_Handle.Actual_Part'length >= Formal.Position then
        declare
          Actual_Parameter : constant Data_Handle := The_Handle.Actual_Part(Formal.Position);
        begin
          if not Is_Null (Actual_Parameter) then
            --TEST-----------------------------------------------------------------------------------------
            --Write_Log ("Actual_Declaration - Type: " & Ada.Tags.External_Tag (Actual_Parameter.all'tag));
            -----------------------------------------------------------------------------------------------
             if The_Handle.Parent_Instantiation = null then
               return Actual_Parameter;
             elsif The_Handle.Parent_Instantiation.Actual_Part'length < Formal.Position then
               return Actual_Parameter;
             elsif not Is_Null (Actual_Parameter.Location) then -- cheet for unnamed types
               --TEST-------------------------------------------------------------------------------------
               --Write_Log ("Actual_Declaration - Name: " & Name.Image_Of (Actual_Parameter.Location.Id));
               -------------------------------------------------------------------------------------------
               return Actual_Parameter;
             end if;
          end if;
        end;
      end if;
      The_Handle := The_Handle.Parent_Instantiation;
      exit when The_Handle = null;
      --TEST-----------------------------------------------------------------------------------------------
      --Write_Log ("Actual_Declaration - Parent Instantiation: " & Name.Image_Of (The_Handle.Location.Id));
      -----------------------------------------------------------------------------------------------------
    end loop;
    --TEST-------------------------------------------
    --Write_Log ("Actual_Declaration - %%% Unknown");
    -------------------------------------------------
    return null;
  exception
  when Occurence: others =>
    Log.Write ("!!! Data.Actual_Handle_Of", Occurence);
    return null;
  end Actual_Declaration_Of;


  function Index_Subtype_Of (Definition    : Array_Definition_Handle;
                             Index         : Positive;
                             Instantiation : Instantiation_Handle) return Data_Handle is
    The_Type : Data_Handle := Definition.Index_Subtypes(Index);
  begin
    if Instantiation /= null and then not Is_Null (The_Type) and then The_Type.all in Formal_Type'class then
      The_Type := Actual_Declaration_Of (Formal_Handle(The_Type), Instantiation);
    end if;
    return The_Type;
  end Index_Subtype_Of;


  function Actual_Profile_For (Generic_Unit : Unit_Handle;
                               Subprogram   : Identifier_Handle;
                               Actual_Part  : List.Elements) return Subprogram_Profile is

    Profile : constant Subprogram_Profile := Profile_Of (Declaration_Handle(Generic_Unit));

    The_Position     : Natural;
    The_Parameters   : List.Item;
    The_Component    : Data_Handle;
    The_Profile      : Subprogram_Profile;
    The_Instatiation : Instantiation_Handle;

    use type List.Elements_Access;

  begin
    if not Is_Null (Subprogram) then
      if not Is_Null (Subprogram.Data) then
        if Subprogram.Data.all in Package_Instantiation'class then
          The_Instatiation := Instantiation_Handle(Subprogram.Data);
          --TEST------------------------------------------------------------------------------------------
          --Write_Log ("Actual_Profile_For - Instantiation: " & Image_Of (The_Instatiation.Location.all));
          ------------------------------------------------------------------------------------------------
        end if;
      end if;
      Subprogram.Data := Data_Handle(Generic_Unit);
      Generic_Unit.Is_Used := True;
    end if;
    if Profile.Parameters = null then
      The_Profile := New_Profile;
    else
      for Index in Profile.Parameters'range loop
        if Profile.Parameters(Index).all in Data_Object'class then
          declare
            The_Parameter : constant Object_Handle := Object_Handle(Profile.Parameters(Index));
          begin
            if The_Parameter /= null and then not Is_Null (The_Parameter.Object_Type) and then
              The_Parameter.Object_Type.all in Formal_Type'class
            then
              --TEST--------------------------------------------------------------------------
              --Write_Log ("*** new Actual Component " & Image_Of (The_Parameter.Location.all)
              --         & " for " & Image_Of (The_Parameter.Object_Type.Location.all));
              --------------------------------------------------------------------------------
              declare
                Formal_Parameter : constant Formal_Handle := Formal_Handle(The_Parameter.Object_Type);
              begin
                The_Position := Formal_Parameter.Position;
                if Unit_Handle(Generic_Parameters_Of (Generic_Unit)) /= Formal_Parameter.Parent then
                  if The_Instatiation /= null then
                    The_Component := new Data_Object'(Location      => The_Parameter.Location,
                                                      Is_Used       => False,
                                                      Parent        => null,
                                                      Object_Type   => The_Instatiation.Actual_Part(The_Position),
                                                      Is_Class_Wide => The_Parameter.Is_Class_Wide,
                                                      Has_Default   => The_Parameter.Has_Default);
                  else
                    The_Component := Data_Handle(The_Parameter);
                  end if;
                else
                  The_Component := new Data_Object'(Location      => The_Parameter.Location,
                                                    Is_Used       => False,
                                                    Parent        => null,
                                                    Object_Type   => Actual_Part(The_Position),
                                                    Is_Class_Wide => The_Parameter.Is_Class_Wide,
                                                    Has_Default   => The_Parameter.Has_Default);
                end if;
                --TEST------------------------------------------------------------------------------------
                --Write_Log ("Type: " & Image_Of (Object_Handle(The_Component).Object_Type.Location.all));
                ------------------------------------------------------------------------------------------
              exception
              when others =>
                Write_Log ("*** new Actual Component " & Image_Of (The_Parameter.Location.all)
                           & " for " & Image_Of (The_Parameter.Object_Type.Location.all));
                Write_Log ("%%% Formal parameter type not found");
                The_Component := Data_Handle(The_Parameter);
              end;
            else
              The_Component := Data_Handle(The_Parameter);
            end if;
            The_Parameters.Append (The_Component);
          end;
        end if;
      end loop;
      The_Profile := New_Profile (The_Parameters);
    end if;
    The_Profile.Result_Type := Profile.Result_Type;
    if not Is_Null (Profile.Result_Type) and then Profile.Result_Type.all in Formal_Type'class then
      --TEST-----------------------------------------------------------------------------------
      --Write_Log ("*** new Actual Result for " & Image_Of (Profile.Result_Type.Location.all));
      -----------------------------------------------------------------------------------------
      declare
        Formal_Result : constant Formal_Handle := Formal_Handle(Profile.Result_Type);
      begin
        The_Position := Formal_Result.Position;
        if Unit_Handle(Generic_Parameters_Of (Generic_Unit)) /= Formal_Result.Parent then
          if The_Instatiation /= null then
            The_Profile.Result_Type := Data.Type_Of(The_Instatiation.Actual_Part(The_Position));
          end if;
        else
          The_Profile.Result_Type := Data.Type_Of(Actual_Part(The_Position));
        end if;
        --TEST-------------------------------------------------------------------------
        --Write_Log ("Result_Type " & Image_Of (The_Profile.Result_Type.Location.all));
        -------------------------------------------------------------------------------
      exception
      when others =>
        Write_Log ("*** new Actual Result for " & Image_Of (Profile.Result_Type.Location.all));
        Write_Log ("%%% Formal result type not found");
      end;
    end if;
    return The_Profile;
  end Actual_Profile_For;


  function Class_Of (Profile : Subprogram_Profile) return Type_Handle is
  begin
    for The_Index in Profile.Parameters'range loop
      declare
        Parameter_Type : constant Data_Handle := Object_Handle(Profile.Parameters(The_Index)).Object_Type;
      begin
        if Parent_Type_Of (Parameter_Type) /= null then
          return Type_Handle(Parameter_Type);
        end if;
      end;
    end loop;
    if Parent_Type_Of (Profile.Result_Type) /= null then
      return Type_Handle(Profile.Result_Type);
    end if;
    return null;
  exception
  when others =>
    return null;
  end Class_Of;


  function Discriminants_From (The_Type : Data_Handle) return List.Item is
    The_Discriminants : List.Item;
    The_Handle        : Data_Handle := The_Type;
    use type List.Item;
  begin
    loop
      if The_Handle.all in Record_Type'class then
        The_Discriminants := Record_Handle(The_Handle).Discriminants;
        exit when The_Discriminants /= List.Empty;
        The_Handle := Type_Handle(The_Handle).Parent_Type;
        exit when The_Handle = null;
      elsif The_Handle.all in Private_Type'class then
        The_Discriminants := Private_Type_Handle(The_Handle).Discriminants;
        exit;
      elsif The_Handle.all in Active_Type'class then
        The_Discriminants := Active_Type_Handle(The_Handle).Discriminants;
        exit;
      elsif The_Type.all in Active_Body'class then
        declare
          Type_Link : constant Data_Handle := Active_Body_Handle(The_Handle).Type_Link;
        begin
          if Type_Link /= null and then Type_Link.all in Active_Type'class then
            The_Discriminants := Active_Type_Handle(Type_Link).Discriminants;
          end if;
        end;
        exit;
      else
        exit;
      end if;
    end loop;
    return The_Discriminants;
  end Discriminants_From;


  function Discriminant_Of (Item     : Identifier_Handle;
                            The_Type : Data_Handle) return Data_Handle is
  begin
    if not Is_Null (The_Type) then
      Item.Data := Data_With (Item.Id, From => Discriminants_From (The_Type));
    end if;
    return Item.Data;
  end Discriminant_Of;


  function Discriminant_Type_Of (The_Position : Positive;
                                 The_Type     : Data_Handle) return Data_Handle is
  begin
    declare
      The_Discriminant : constant Data_Handle := Discriminants_From(The_Type)(The_Position);
    begin
      Set_Used (The_Discriminant);
      return Object_Type_Of (The_Discriminant);
    end;
  exception
  when Constraint_Error =>
    return null;
  end Discriminant_Type_Of;


  function Has_Discriminats (The_Type : Data_Handle) return Boolean is
    use type List.Item;
  begin
    return not Is_Null (The_Type) and then Discriminants_From (The_Type) /= List.Empty;
  end Has_Discriminats;


  function Entry_With (Item  : Identifier_Handle;
                       Scope : Unit_Handle) return Data_Handle is

    The_Handle        : Data_Handle;
    The_Specification : Unit_Declaration_Handle;

  begin
    if Scope.all in Unit_Body'class then
      The_Handle := Data_Handle(Unit_Body_Handle(Scope).Specification);
    end if;
    if Is_Null (The_Handle) then
      return null;
    else
      if The_Handle.all in Unit_Declaration'class then
        The_Specification := Unit_Declaration_Handle(The_Handle);
      elsif The_Handle.all in Active_Type'class then
        The_Specification := Unit_Declaration_Handle(Active_Type_Handle(The_Handle).Object);
      else
        return null;
      end if;
      return Declaration_Of (Item, From => The_Specification.all);
    end if;
  end Entry_With;


  function Scope_Of_Body (From : Unit_Handle) return Unit_Handle with Inline is
    The_Scope : Unit_Handle := From;
  begin
    loop
      if The_Scope = null then
        return null;
      elsif The_Scope.all in Unit_Body'class then
        return The_Scope;
      end if;
      The_Scope := The_Scope.Parent;
    end loop;
  end Scope_Of_Body;


  function Entry_Of (Item  : Identifier_Handle;
                     Scope : Unit_Handle) return Unit_Handle is
    Body_Scope : constant Unit_Handle := Scope_Of_Body (Scope);
    The_Handle : Unit_Handle;
  begin
    if Body_Scope /= null then
      The_Handle := Unit_Handle(Entry_With (Item, Body_Scope));
      if The_Handle /= null then
        Item.Data := Data_Handle(The_Handle);
        The_Handle.Is_Used := True;
        return The_Handle;
      end if;
    end if;
    return null;
  end Entry_Of;


  function Index_Type_Of (Item : Data_Handle) return Data_Handle is
  begin
    if Item.all in Entry_Body'class then
      return Entry_Body_Handle(Item).Index_Type;
    elsif Item.all in Entry_Declaration'class then
      return Entry_Declaration_Handle(Item).Index_Type;
    else
      return null;
    end if;
  end Index_Type_Of;


  function Used_Declaration_Of (Declaration   : Data_Handle;
                                Item          : Identifier_Handle;
                                Scope         : Unit_Handle;
                                Suppress_Used : Boolean) return Data_Handle with Inline is
  begin
    if not Is_Null (Declaration) then
      --TEST--------------------------------------------------------------------
      --Write_Log ("Used_Declaration_Of: " & Full_Name_Of (Declaration)
      --           & " - Type: " & Ada.Tags.External_Tag (Declaration.all'tag));
      --------------------------------------------------------------------------
      case Data_Kind_Of (Declaration.all) is
      when Is_In_Subprogram
         | Is_Generic_Subprogram_Declaration
         | Is_Subprogram_Renaming
         | Is_Entry_Declaration
         | Is_Entry_Body
      =>
        return Declaration;
      when Is_Used_Subprogram =>
        if Suppress_Used then
          --TEST--------------------------------------------------------------------
          --Write_Log ("Suppressed Used Subprogram: " & Full_Name_Of (Declaration));
          --------------------------------------------------------------------------
          declare
            The_Overload : Declaration_Handle := Used_Subprogram_Handle(Declaration).Overload;
          begin
            while The_Overload /= null and then The_Overload.all in Used_Subprogram'class loop
              The_Overload := Used_Subprogram_Handle(The_Overload).Overload;
            end loop;
            if The_Overload = null or else The_Overload.Parent /= Scope then
              return null;
            end if;
            return Declaration;
          end;
        else
          return Declaration;
        end if;
      when others =>
        Item.Data := Declaration;
        Declaration_Handle(Declaration).Is_Used := True;
      end case;
    end if;
    return Declaration;
  end Used_Declaration_Of;


  function Declaration_From (Scope         : Unit_Handle;
                             Item          : Identifier_Handle;
                             Suppress_Used : Boolean := False) return Data_Handle is
  begin
    if Scope = null then
      return null;
    end if;
    return Used_Declaration_Of (Declaration_Of (Item, Scope.all), Item, Scope, Suppress_Used);
  end Declaration_From;


  function Inner_Declaration_From (Scope         : Unit_Handle;
                                   Item          : Identifier_Handle;
                                   Suppress_Used : Boolean := False) return Data_Handle is
  begin
    if Scope = null then
      return null;
    end if;
    return Used_Declaration_Of (Inner_Declaration_Of (Item, Scope.all), Item, Scope, Suppress_Used);
  end Inner_Declaration_From;


  function Declaration_From (The_Generations : Generation_Access;
                             Index           : Positive;
                             Item            : Identifier_Handle) return With_Handle is

    The_Cursor : Tree.Cursor;
    use type Tree.Cursor;

  begin --Declaration_From
    Tree.Get (Id          => Item.Id,
              From        => The_Generations(Index).Children,
              Data_Cursor => The_Cursor);
    if The_Cursor /= null then
      --TEST--------------------------------------------------------------------------------------------------------
      --Write_Log ("~~~ Declaration_From Generations: found at" & Natural'image(Index) & " " & Image_Of (Item.all));
      --Write_Log ("~~~   TYPE " & Ada.Tags.External_Tag (Generations(Generations'last).Unit.all'tag));
      --------------------------------------------------------------------------------------------------------------
      declare
        The_Handle : With_Handle := With_Handle(The_Cursor.all);
      begin
        --TEST---------------------------------------------
        --Write_Log ("~~~   found " & Image_Of (Item.all));
        ---------------------------------------------------
        while The_Handle.Copy_Of /= null loop
          --TEST------------------------------
          --Write_Log ("~~~   back one copy");
          ------------------------------------
          The_Handle := The_Handle.Copy_Of;
        end loop;
        Item.Data := Data_Handle(The_Handle);
        The_Handle.Is_Used := True;
        return The_Handle;
      end;
    end if;
    return null;
  exception
  when others =>
    return null;
  end Declaration_From;


  function Declaration_From (The_Import : With_Handle;
                             Item       : Identifier_Handle) return With_Handle is
    The_Cursor : Tree.Cursor;
    use type Tree.Cursor;
  begin
    Tree.Get (Id          => Item.Id,
              From        => The_Import.Children,
              Data_Cursor => The_Cursor);
    if The_Cursor /= null then
      declare
        The_Handle : With_Handle := With_Handle(The_Cursor.all);
      begin
        --TEST---------------------------------------------
        --Write_Log ("~~~   found " & Image_Of (Item.all));
        ---------------------------------------------------
        while The_Handle.Copy_Of /= null loop
          --TEST------------------------------
          --Write_Log ("~~~   back one copy");
          ------------------------------------
          The_Handle := The_Handle.Copy_Of;
        end loop;
        Item.Data := Data_Handle(The_Handle);
        The_Handle.Is_Used := True;
        return The_Handle;
      end;
    end if;
    return null;
  end Declaration_From;


  function Declaration_From (The_Generations : Generation_Access;
                             Unit            : Unit_Handle;
                             Item            : Identifier_Handle) return With_Handle is

    The_Declaration : With_Handle;

    procedure Search_In (Children : Tree.Item) is

      Found : exception;

      procedure Visit_Element (The_Data : Tree.Cursor);

      procedure Visit_Generations is new Tree.Iterator (Visit_Element);

      procedure Visit_Element (The_Data : Tree.Cursor) is
        Element : constant With_Handle := With_Handle(The_Data.all);
      begin
        --TEST------------------------------------------------------------
        --Write_Log ("~~~ Visit " & Image_Of (Element.Unit.Location.all));
        --TEST------------------------------------------------------------
        if Element.Unit = Unit then
          --TEST--------------------------------
          --Write_Log ("~~~ Visit Found Unit ");
          --TEST--------------------------------
          The_Declaration := Declaration_From (Element, Item);
          raise Found;
        else
          Visit_Generations (Element.Children);
        end if;
      end Visit_Element;

    begin -- Search_In
      Visit_Generations (Children);
    exception
    when Found =>
      null;
    end Search_In;

  begin -- Declaration_From
    if The_Generations /= null then
      for Index in The_Generations'range loop
        if The_Generations(Index).Unit = Unit then
          The_Declaration := Declaration_From (The_Generations, Index, Item);
        else
          Search_In (The_Generations(Index).Children);
        end if;
        if The_Declaration /= null then
          --TEST-------------------------------------------------------------------------------
          --Write_Log ("~~~ Declaration_From found at" & Natural'image(Index) &
          --           " in " & Image_Of (Unit.Location.all) & " item " & Image_Of (Item.all));
          -------------------------------------------------------------------------------------
          return The_Declaration;
        end if;
      end loop;
    end if;
    return null;
  end Declaration_From;


  function Declaration_From (Used_Packages     : Use_List_Handle;
                             The_Generations   : Generation_Access;
                             Item              : Identifier_Handle;
                             The_Instantiation : out Instantiation_Handle) return Data_Handle is

    The_Declaration : Data_Handle;

  begin
    The_Instantiation := null;
    for The_Element of Used_Packages.all loop
      The_Declaration := The_Element;
      if The_Declaration.all in Instantiated_Item'class then
        declare
          Unit : constant Item_Instantiation := Item_Instantiation(The_Declaration);
        begin
          The_Declaration := Unit.Item;
          The_Instantiation := Unit.Instantiation;
          if not (The_Declaration.all in Package_Specification'class) then
            return null;
          end if;
        end;
      end if;
      The_Declaration := Declaration_From (Unit_Handle(The_Declaration), Item);
      if The_Declaration /= null then
        --TEST--------------------------------------------------
        --Write_Log ("Used: " & Full_Name_Of (The_Declaration));
        --------------------------------------------------------
        return The_Declaration;
      elsif The_Element.all in Library_Package_Specification'class then
        declare
          Unit       : constant Unit_Handle := Unit_Handle(The_Element);
          The_Handle : With_Handle;
        begin
          The_Handle := Declaration_From (The_Generations, The_Generations'first, Unit.Location);
          if The_Handle = null and then Unit.Parent /= null then
            The_Handle := Declaration_From (The_Generations, The_Generations'first, Unit.Parent.Location);
            if The_Handle /= null then
              The_Handle := Declaration_From (The_Handle, Unit.Location);
            end if;
          end if;
          if The_Handle /= null then
            --TEST------------------------------------------------------------------
            --Write_Log ("Used in: " & Full_Name_Of (Data_Handle(The_Handle.Unit)));
            ------------------------------------------------------------------------
            The_Handle := Declaration_From (The_Handle, Item);
            if The_Handle /= null then
              --TEST---------------------------------------------------------------
              --Write_Log ("Used: " & Full_Name_Of (Data_Handle(The_Handle.Unit)));
              ---------------------------------------------------------------------
              return Data_Handle (The_Handle);
            end if;
          end if;
        end;
      end if;
    end loop;
    return null;
  end Declaration_From;


  Imported : exception;

  procedure Check_Imported (Unit   : Unit_Handle;
                            Cursor : Library_Tree.Cursor) is
  begin
    if Unit = Cursor.all.Unit then
      raise Imported;
    end if;
  end Check_Imported;

  procedure Check is new Library_Tree.Iterator_With (Unit_Handle, Check_Imported);


  function Is_Library (Item : Unit_Handle) return Boolean is
  begin
    return Resource(Item.all) /= null;
  end Is_Library;


  function Library_Id (Item : Unit_Handle) return Identifiers is
    The_Unit  : Unit_Handle := Item;
    The_Count : Positive := Positive'first;
  begin
    while The_Unit.Parent /= null loop
      The_Count := The_Count + 1;
      The_Unit := The_Unit.Parent;
    end loop;
    declare
      Id : Identifiers(1..The_Count);
    begin
      The_Unit := Item;
      for The_Id of reverse Id loop
        The_Id := The_Unit.Location;
        if not Is_Null (The_Id) then
          The_Id.Data := Data_Handle(The_Unit);
        end if;
        The_Unit := The_Unit.Parent;
      end loop;
      return Id;
    end;
  end Library_Id;


  function Is_Used (Unit : Unit_Handle;
                    By   : Unit_Handle) return Boolean is
  begin
    if By.Parent = Unit then
      return True;
    end if;
    Check (Unit, Resource(By.all).Imports);
    return False;
  exception
  when Imported =>
    return True;
  end Is_Used;

end Ada_95.Token.Data;
