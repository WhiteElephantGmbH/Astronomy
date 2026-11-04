-- *********************************************************************************************************************
-- *                       (c) 2007 .. 2022 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Characters.Handling;
with Ada_95.Word;
with Ada_95.Name;

package body Ada_95.Lexical is

  Prefix_Size : constant := 3; -- length of "Is_"

  Shared_Single_Aspects   : constant Name_Key := Aspect_Id'pos(Single_Aspect'first);
  Shared_Compound_Aspects : constant Name_Key := Aspect_Id'pos(Compound_Aspect'first) - 1
                                                 - Aspect_Id'pos(Single_Aspect'last);

  Shared_Single_Pragmas   : constant Name_Key := Pragma_Id'pos(Single_Pragma'first);
  Shared_Compound_Pragmas : constant Name_Key := Pragma_Id'pos(Compound_Pragma'first) - 1
                                                 - Pragma_Id'pos(Single_Pragma'last);

  First_Reserved_Key       : constant Name_Key := 1;
  Last_Reserved_Key        : constant Name_Key := First_Reserved_Key + Reserved_Word'pos(Reserved_Word'last);
  First_Single_Attribute   : constant Name_Key := Last_Reserved_Key + 1 - Attribute_Id'pos(Single_Attribute'first);
  Last_Single_Attribute    : constant Name_Key := First_Single_Attribute + Attribute_Id'pos(Single_Attribute'last);
  First_Single_Aspect      : constant Name_Key := Last_Single_Attribute + 1 - Shared_Single_Aspects;
  Last_Single_Aspect       : constant Name_Key := First_Single_Aspect + Aspect_Id'pos(Single_Aspect'last);
  First_Single_Pragma      : constant Name_Key := Last_Single_Aspect + 1 - Shared_Single_Pragmas;
  Last_Single_Pragma       : constant Name_Key := First_Single_Pragma + Pragma_Id'pos(Single_Pragma'last);
  First_Compound_Attribute : constant Name_Key := Last_Single_Pragma + 1;
  Last_Compound_Attribute  : Name_Key;
  First_Compound_Aspect    : Name_Key;
  Last_Compound_Aspect     : Name_Key;
  First_Compound_Pragma    : Name_Key;
  Last_Compound_Pragma     : Name_Key;

  Preelaborable_Initialization_Key : Name_Key; -- used in attributes, aspects and pragmas

  The_Name_Key : Name_Key;
  The_Base_Key : Name_Key;


  type Crc_Value is mod 2 ** 24;

  Crc_24 : constant Crc_Value
    := 2**0 + 2**2 + 2**3 + 2**4 + 2**5 + 2**6 + 2**9 + 2**10 + 2**13 + 2**16 + 2**17 + 2**22 + 2**23;

  The_Crc_Value : Crc_Value;


  procedure Update_Crc with Inline is
  begin
    if The_Crc_Value >= 2**23 then
      The_Crc_Value := (The_Crc_Value xor Crc_24) * 2 + 1;
    else
      The_Crc_Value := The_Crc_Value * 2;
    end if;
  end Update_Crc;


  procedure Define is

    function To_Lower (Item : String) return String renames Ada.Characters.Handling.To_Lower;

    The_Next_Name_Key          : Name_Key;
    The_Compound_Key : Name_Key;

  begin
    The_Name_Key := Null_Key;
    The_Base_Key := 0; -- no random keys
    for Id in Reserved_Word'range loop
      declare
        Image : constant String := Reserved_Word'image (Id);
      begin
        Word.Add (To_Lower (Image(Image'first + Prefix_Size .. Image'last)));
      end;
    end loop;

    for Id in Single_Attribute'range loop
      declare
        Image : constant String := Single_Attribute'image (Id);
      begin
        Word.Add (To_Lower (Image(Image'first + Prefix_Size .. Image'last)));
      end;
    end loop;

    for Id in Single_Aspect'range loop
      declare
        Image : constant String := Single_Aspect'image (Id);
      begin
        Word.Add (To_Lower (Image(Image'first + Prefix_Size .. Image'last)));
      end;
    end loop;

    for Id in Single_Pragma'range loop
      declare
        Image : constant String := Single_Pragma'image (Id);
      begin
        Word.Add (To_Lower (Image(Image'first + Prefix_Size .. Image'last)));
      end;
    end loop;

    The_Compound_Key := The_Name_Key;
    The_Name_Key := The_Name_Key + Name_Key(Compound_Attribute'pos(Compound_Attribute'last)
                                          - Compound_Attribute'pos(Compound_Attribute'first) + 1);
    Last_Compound_Attribute  := The_Name_Key;
    First_Compound_Aspect := Last_Compound_Attribute + 1 - Shared_Compound_Aspects;
    The_Name_Key := The_Name_Key + Name_Key(Compound_Aspect'pos(Compound_Aspect'last)
                                          - Compound_Aspect'pos(Compound_Aspect'first) + 1);
    Last_Compound_Aspect  := The_Name_Key;
    First_Compound_Pragma := Last_Compound_Aspect + 1 - Shared_Compound_Pragmas;
    The_Name_Key := The_Name_Key + Name_Key(Compound_Pragma'pos(Compound_Pragma'last)
                                          - Compound_Pragma'pos(Compound_Pragma'first) + 1);
    Last_Compound_Pragma  := The_Name_Key;

    for Id in Compound_Attribute'range loop
      declare
        Image : constant String := Compound_Attribute'image (Id);
        First : Natural := Image'first + Prefix_Size;
      begin
        for Index in First .. Image'last loop
          if Image (Index) = '_' then
            Name.Add (To_Lower(Image (First .. Index - 1)));
            First := Index + 1;
          end if;
        end loop;
        Name.Add (To_Lower(Image (First .. Image'last)));
        The_Next_Name_Key := The_Name_Key;
        The_Name_Key := The_Compound_Key;
        Name.Complete_Compound;
        The_Compound_Key := The_Name_Key;
        The_Name_Key := The_Next_Name_Key;
        if Id = Is_Preelaborable_Initialization then
          Preelaborable_Initialization_Key := The_Compound_Key;
        end if;
      end;
    end loop;

    for Id in Compound_Aspect'range loop
      declare
        Image : constant String := Compound_Aspect'image (Id);
        First : Natural := Image'first + Prefix_Size;
      begin
        for Index in First .. Image'last loop
          if Image (Index) = '_' then
            Name.Add (To_Lower(Image (First .. Index - 1)));
            First := Index + 1;
          end if;
        end loop;
        Name.Add (To_Lower(Image (First .. Image'last)));
        The_Next_Name_Key := The_Name_Key;
        The_Name_Key := The_Compound_Key;
        Name.Complete_Compound;
        The_Compound_Key := The_Name_Key;
        The_Name_Key := The_Next_Name_Key;
      end;
    end loop;

    for Id in Compound_Pragma'range loop
      declare
        Image : constant String := Compound_Pragma'image (Id);
        First : Natural := Image'first + Prefix_Size;
      begin
        for Index in First .. Image'last loop
          if Image (Index) = '_' then
            Name.Add (To_Lower(Image (First .. Index - 1)));
            First := Index + 1;
          end if;
        end loop;
        Name.Add (To_Lower(Image (First .. Image'last)));
        The_Next_Name_Key := The_Name_Key;
        The_Name_Key := The_Compound_Key;
        Name.Complete_Compound;
        The_Compound_Key := The_Name_Key;
        The_Name_Key := The_Next_Name_Key;
      end;
    end loop;

    The_Base_Key := The_Name_Key; -- start random key generator
    The_Crc_Value := 1;
  end Define;


  function Next_Name_Key return Name_Key is
  begin
    if The_Base_Key = 0 then
      The_Name_Key := The_Name_Key + 1;
    else
      Update_Crc;
      The_Name_Key := The_Base_Key + Name_Key(The_Crc_Value);
    end if;
    return The_Name_Key;
  end Next_Name_Key;


  function Is_Reserved_Word (Key : Name_Key) return Boolean is
  begin
    return Key <= Last_Reserved_Key;
  end Is_Reserved_Word;


  function Reserved_Word_Of (Key : Name_Key) return Reserved_Word is
  begin
    return Reserved_Word'val (Key - First_Reserved_Key);
  end Reserved_Word_Of;


  function Is_Attribute (Key : Name_Key) return Boolean is
  begin
    if Key >= First_Single_Attribute and Key <= Last_Single_Attribute then
      return True;
    elsif Key >= First_Compound_Attribute and Key <= Last_Compound_Attribute then
      return True;
    end if;
    return False;
  end Is_Attribute;


  function Attribute_Of (Key : Name_Key) return Attribute_Id is
  begin
    if Key <= Last_Single_Attribute then
      return Attribute_Id'val (Key - First_Single_Attribute);
    else
      return Attribute_Id'val (Key - First_Compound_Attribute + Compound_Attribute'pos(Compound_Attribute'first));
    end if;
  end Attribute_Of;


  function Is_Aspect (Key : Name_Key) return Boolean is
  begin
    if Key >= First_Single_Aspect and Key <= Last_Single_Aspect then
      return True;
    elsif Key >= First_Compound_Aspect and Key <= Last_Compound_Aspect then
      return True;
    end if;
    return False;
  end Is_Aspect;


  function Aspect_Of (Key : Name_Key) return Aspect_Id is
  begin
    if Key <= Last_Single_Aspect then
      return Aspect_Id'val (Key - First_Single_Aspect);
    else
      return Aspect_Id'val (Key - First_Compound_Aspect + Single_Aspect'pos(Single_Aspect'last) + 1);
    end if;
  end Aspect_Of;


  function Is_Pragma_Id (Key : Name_Key) return Boolean  is
  begin
    if Key >= First_Single_Pragma and Key <= Last_Single_Pragma then
      return True;
    elsif Key >= First_Compound_Pragma and Key <= Last_Compound_Pragma then
      return True;
    elsif Key = Preelaborable_Initialization_Key then
      return True;
    end if;
    return False;
  end Is_Pragma_Id;


  function Pragma_Id_Of (Key : Name_Key) return Pragma_Id  is
  begin
    if Key = Preelaborable_Initialization_Key then
      return Is_Preelaborable_Initialization;
    elsif Key <= Last_Single_Pragma then
      return Pragma_Id'val (Key - First_Single_Pragma);
    else
      return Pragma_Id'val (Key - First_Compound_Pragma + Single_Pragma'pos(Single_Pragma'last) + 1);
    end if;
  end Pragma_Id_Of;

end Ada_95.Lexical;
