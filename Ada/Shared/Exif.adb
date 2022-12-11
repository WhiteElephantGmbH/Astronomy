-- *********************************************************************************************************************
-- *                               (c) 2021 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
pragma Style_White_Elephant;

with GNAT.OS_Lib;
with System;
with Strings;
with Traces;

package body Exif is

  package Log is new Traces ("Exif");

  use type Strings.Element;

  The_Orientation : Image_Orientation := Undefined;

  The_Date_Time_Digitized : Strings.Element;

  The_Image_Width  : Size := Undefined_Size;
  The_Image_Height : Size := Undefined_Size;

  The_Altitude_Ref : See_Level := Undefined;
  The_Altitude     : Height    := Undefined_Height;

  The_Latitude_Ref : Reference := Undefined_Ref;
  The_Latitude     : Values    := Undefined_Values;

  The_Longitude_Ref : Reference := Undefined_Ref;
  The_Longitude     : Values    := Undefined_Values;

  The_Time_Stamp : Values := Undefined_Values;
  The_Date_Stamp : Date   := Undefined_Date;


  function Orientation return Image_Orientation is
  begin
    return The_Orientation;
  end Orientation;


  function Image_Height return Size is
  begin
    return The_Image_Height;
  end Image_Height;


  function Image_Width return Size is
  begin
    return The_Image_Width;
  end Image_Width;


  function Altitude_Ref return See_Level is
  begin
    return The_Altitude_Ref;
  end Altitude_Ref;


  function Altitude return Height is
  begin
    return The_Altitude;
  end Altitude;


  function Latitude_Ref return Reference is
  begin
    return The_Latitude_Ref;
  end Latitude_Ref;


  function Latitude return Values is
  begin
    return The_Latitude;
  end Latitude;


  function Longitude_Ref return Reference is
  begin
    return The_Longitude_Ref;
  end Longitude_Ref;


  function Longitude return Values is
  begin
    return The_Longitude;
  end Longitude;


  function Time_Stamp return Values is
  begin
    return The_Time_Stamp;
  end Time_Stamp;


  function Date_Stamp return Date is
  begin
    return The_Date_Stamp;
  end Date_Stamp;


  function Date_Time_Digitized return String is
  begin
    return +The_Date_Time_Digitized;
  end Date_Time_Digitized;


  package OS renames GNAT.OS_Lib;

  type Marker is new Unsigned.Word;

  Marker_Size : constant := Marker'size / System.Storage_Unit;

  function Marker_Of (Item : Unsigned.Big_Endian_Word) return Marker is
  begin
    return Marker(Unsigned.Swap (Item));
  end Marker_Of;

  Tiff_Intel_Byte_Align    : constant String := "II";
  Tiff_Motorola_Byte_Align : constant String := "MM";
  Exif_Id                  : constant String := "Exif";
  Tiff_Tag_Mark            : constant Marker := Marker_Of (16#2A00#);

  SOI_Marker  : constant Marker := Marker_Of (16#FFD8#);
  JFIF_Marker : constant Marker := Marker_Of (16#FFE0#);
  APP1_Marker : constant Marker := Marker_Of (16#FFE1#);

  TIFF_II_Marker : constant Marker := Word_Of (Unsigned.String_Of (Tiff_Intel_Byte_Align));
  TIFF_MM_Marker : constant Marker := Word_Of (Unsigned.String_Of (Tiff_Motorola_Byte_Align));

  -- Main Tags
  Orientation_Tag : constant := 16#0112#;
  Exif_Offset_Tag : constant := 16#8769#;
  Gps_Info_Tag    : constant := 16#8825#;

  -- Sub Tags
  Date_Time_Digitized_Tag : constant := 16#9004#;
  Exif_Image_Width_Tag    : constant := 16#A002#;
  Exif_Image_Height_Tag   : constant := 16#A003#;

  -- Gps Tags
  Gps_Latitude_Ref_Tag  : constant := 16#01#;
  Gps_Latitude_Tag      : constant := 16#02#;
  Gps_Longitude_Ref_Tag : constant := 16#03#;
  Gps_Longitude_Tag     : constant := 16#04#;
  Gps_Altitude_Ref_Tag  : constant := 16#05#;
  Gps_Altitude_Tag      : constant := 16#06#;
  Gps_Time_Stamp_Tag    : constant := 16#07#;
  Gps_Date_Stamp_Tag    : constant := 16#1D#;

  type Format is (Unused_0, Unsigned_Byte, Ascii_String, Unsigned_Short, Unused_Unsigned_Long, Rational, Unused_6,
                  Unused_Undefined, Unused_8, Unused_Signed_Long, Unused_Signed_Rational, Unused_11, Unused_12);

  type Word is new Unsigned.Word;

  Word_Size : constant := Word'size / System.Storage_Unit;

  subtype Words is Unsigned.Word_String;

  Longword_Size : constant := Longword'size / System.Storage_Unit;

  type IFD_Entry is record
    Tag   : Word;
    Kind  : Format;
    Count : Natural;
    Data  : Longword;
  end record;


  procedure Read (Filename : String) is

    File : constant OS.File_Descriptor := OS.Open_Read (Name  => Filename,
                                                        Fmode => OS.Binary);

    type File_Position is new Long_Integer;

    The_Position   : File_Position := 0; -- Start of File
    The_Tiff_Start : File_Position;
    Is_Big_Endian  : Boolean;


    procedure Read (Item     : System.Address;
                    The_Size : Positive) is
      The_Count : Integer;
    begin
      The_Count := OS.Read (FD => File,
                            A  => Item,
                            N  => The_Size);
      if The_Count /= The_Size then
        raise Invalid_File;
      end if;
      The_Position := The_Position + File_Position(The_Size);
    end Read;


    function Actual_Position return File_Position is
    begin
      return The_Position;
    end Actual_Position;


    procedure Set (Position : File_Position) is
    begin
      The_Position := Position;
      OS.Lseek (FD     => File,
                offset => Long_Integer(Position),
                origin => OS.Seek_Set);
    end Set;


    procedure Set (Offset : Longword) is
    begin
      Set (The_Tiff_Start + File_Position(Offset));
    end Set;


    function Get (The_Size : Natural) return String is
      Item : aliased String(1..The_Size);
    begin
      Read (Item'address, The_Size);
      return Item;
    end Get;


    function Get return Word is
      The_Item : aliased Word;
    begin
      Read (The_Item'address, Word_Size);
      if Is_Big_Endian then
        Swap (The_Item);
      end if;
      return The_Item;
    end Get;


    procedure Check (Item : Word) is
    begin
      if Word'(Get) /= Item then
        raise Invalid_File;
      end if;
    end Check;


    procedure Check (Item : Marker) is
    begin
      Check (Word(Item));
    end Check;


    function Get return Longword is
      The_Item : aliased Longword;
    begin
      Read (The_Item'address, Longword_Size);
      if Is_Big_Endian then
        Swap (The_Item);
      end if;
      return The_Item;
    end Get;


    function Get_Entry return IFD_Entry is
      The_Entry : IFD_Entry;
    begin
      The_Entry.Tag := Get;
      The_Entry.Kind := Format'val(Word'(Get));
      The_Entry.Count := Natural(Longword'(Get));
      The_Entry.Data := Get;
      return The_Entry;
    end Get_Entry;


    function String_Of (Item : IFD_Entry) return String is
    begin
      if Item.Kind /= Ascii_String then
        Log.Error ("String expected");
        raise Invalid_File;
      end if;
      if Item.Count <= 4 then
        declare
          Image : String (1..4);
          for Image'address use Item.Data'address;
        begin
          return Image(Image'first .. Image'first + Item.Count - 2);
        end;
      end if;
      Set (Offset => Item.Data);
      declare
        Image    : constant String := Get (Item.Count);
        The_Last : Natural := Image'last;
      begin
        while Image(The_Last) = Ascii.Nul loop
          The_Last := The_Last - 1;
          exit when The_Last = Image'first - 1;
        end loop;
        return Image(Image'first .. The_Last);
      end;
    end String_Of;


    function Rational_Of (Item : IFD_Entry) return Rational_Values is
      The_Values : Rational_Values(1 .. Item.Count);
    begin
      if Item.Kind /= Rational then
        Log.Error ("Rational values expected");
        raise Invalid_File;
      end if;
      Set (Item.Data);
      for The_Value of The_Values loop
        The_Value.Nominator := Get;
        The_Value.Denominator := Get;
      end loop;
      return The_Values;
    end Rational_Of;


    function Unsigned_Bytes_Of (Item : IFD_Entry) return Unsigned.Byte_String is
      The_Count : Natural := Item.Count;
    begin
      if Item.Kind /= Unsigned_Byte then
        Log.Error ("Unsigned_Byte values expected");
        raise Invalid_File;
      end if;
      if The_Count > 4 then
        Log.Error ("<Unsigned_Byte count > 4 not supported (actual count = " & The_Count'image & ")>");
        The_Count := 4;
      end if;
      declare
        The_Values : Unsigned.Byte_String(1 .. The_Count);
        for The_Values'address use Item.Data'address;
      begin
        return The_Values;
      end;
    end Unsigned_Bytes_Of;


    function Unsigned_Shorts_Of (Item  : IFD_Entry) return Words is
    begin
      if Item.Kind /= Unsigned_Short then
        Log.Error ("Unsigned_Short values expected");
        raise Invalid_File;
      end if;
      if Item.Count > 2 then
        declare
          The_Values : Words(1..Item.Count);
        begin
          Set (Offset => Item.Data);
          for The_Value of The_Values loop
            The_Value := Unsigned.Word(Word'(Get));
          end loop;
          return The_Values;
        end;
      else
        declare
          The_Values : Words(1 .. Item.Count);
          for The_Values'address use Item.Data'address;
        begin
          return The_Values;
        end;
      end if;
    end Unsigned_Shorts_Of;


    function Trimmed (Item : String) return String renames Strings.Trimmed;

    function Image_Of (Value : Rational_Value) return String is
    begin
      return Trimmed (Value.Nominator'image) & '/' & Trimmed (Value.Denominator'image);
    end Image_Of;


    function Image_Of (The_Values : Rational_Values) return String is

      function Image_Of (I : String;
                         V : Rational_Values) return String is
      begin
        if V'length = 0 then
          return I;
        else
          return Image_Of (I => I & ' ' & Image_Of (V(V'first)),
                           V => V(V'first + 1 .. V'last));
        end if;
      end Image_Of;

    begin
      return Trimmed (Image_Of ("", The_Values));
    end Image_Of;

    procedure Get_Sub_Image_File_Directory is
      Entry_Count : constant Natural := Natural(Word'(Get));
    begin
      for Unused in 1 .. Entry_Count loop
        declare
          Sub_Entry : constant IFD_Entry := Get_Entry;
          Next_IFD  : constant File_Position := Actual_Position;

          function Read_Size return Size is
            The_Values : constant Words := Unsigned_Shorts_Of (Sub_Entry);
          begin
            if The_Values'length = 1 then
              return Size(The_Values(The_Values'first));
            end if;
            return Undefined_Size;
          exception
          when others =>
            return Undefined_Size;
          end Read_Size;

        begin
          case Sub_Entry.Tag is
          when Date_Time_Digitized_Tag =>
            declare
              Date_Time : constant String := String_Of (Sub_Entry);
            begin
              The_Date_Time_Digitized := [Date_Time];
              Log.Write ("Date Time    : " & Date_Time);
            end;
          when Exif_Image_Width_Tag =>
            The_Image_Width := Read_Size;
            Log.Write ("Image Width  :" & The_Image_Width'image);
          when Exif_Image_Height_Tag =>
            The_Image_Height := Read_Size;
            Log.Write ("Image Height :" & The_Image_Height'image);
          when Gps_Altitude_Ref_Tag =>
            null;
          when others =>
            null;
          end case;
          Set (Position => Next_IFD);
        end;
      end loop;
    end Get_Sub_Image_File_Directory;


    procedure GPS_Info is
      Entry_Count : constant Natural := Natural(Word'(Get));
    begin
      for Unused in 1 .. Entry_Count loop
        declare
          Gps_Entry : constant IFD_Entry := Get_Entry;
          Next_IFD  : constant File_Position := Actual_Position;

          function Ref return Reference is
            Image : constant String :=  String_Of (Gps_Entry);
          begin
            if Image'length = 1 then
              return Reference(Image(Image'first));
            end if;
            return Undefined_Ref;
          end Ref;

          function Level return See_Level is
            The_Values : constant Unsigned.Byte_String :=  Unsigned_Bytes_Of (Gps_Entry);
          begin
            if The_Values'length = 1 then
              declare
                Value : constant Unsigned.Byte := The_Values(The_Values'first);
                use type Unsigned.Byte;
              begin
                if Value = 0 then
                  return Above;
                elsif Value = 1 then
                  return Below;
                end if;
              end;
            end if;
            return Undefined;
          end Level;

        begin
          case Gps_Entry.Tag is
          when Gps_Altitude_Ref_Tag =>
            The_Altitude_Ref := Level;
            Log.Write ("Gps_Altitude_Ref  : " & Altitude_Ref'image);
          when Gps_Altitude_Tag =>
            The_Altitude := Rational_Of (Gps_Entry)(1);
            Log.Write ("Gps_Altitude      : " & Image_Of (The_Altitude));
          when Gps_Latitude_Ref_Tag =>
            The_Latitude_Ref := Ref;
            Log.Write ("Gps_Latitude_Ref  : " & Latitude_Ref);
          when Gps_Latitude_Tag =>
            The_Latitude := Rational_Of (Gps_Entry);
            Log.Write ("Gps_Latitude      : " & Image_Of (Latitude));
          when Gps_Longitude_Ref_Tag =>
            The_Longitude_Ref := Ref;
            Log.Write ("Gps_Longitude_Ref : " & Longitude_Ref);
          when Gps_Longitude_Tag =>
            The_Longitude := Rational_Of (Gps_Entry);
            Log.Write ("Gps_Longitude     : " & Image_Of (Longitude));
          when Gps_Time_Stamp_Tag =>
            The_Time_Stamp := Rational_Of (Gps_Entry);
            Log.Write ("Gps_Time_Stamp    : " & Image_Of (Time_Stamp));
          when Gps_Date_Stamp_Tag =>
            The_Date_Stamp := String_Of (Gps_Entry);
            Log.Write ("Gps_Date_Stamp    : " & Date_Stamp);
          when others =>
            null;
          end case;
          Set (Position => Next_IFD);
        end;
      end loop;
    end GPS_Info;


    procedure Get_Main_Image_File_Directory is
      Entry_Count     : constant Natural := Natural(Word'(Get));
      The_Exif_Offset : Longword := 0;
      The_Gps_Offset  : Longword := 0;
    begin
      for Unused in 1 .. Entry_Count loop
        declare
          Main_Entry : constant IFD_Entry := Get_Entry;
          Next_IFD   : constant File_Position := Actual_Position;

          function Read_Orientation return Image_Orientation is
            The_Values : constant Words := Unsigned_Shorts_Of (Main_Entry);
          begin
            if The_Values'length = 1 then
              return Image_Orientation'val(The_Values(The_Values'first));
            end if;
            return Undefined;
          exception
          when others =>
            return Undefined;
          end Read_Orientation;

        begin
          case Main_Entry.Tag is
          when Orientation_Tag =>
            The_Orientation := Read_Orientation;
            Log.Write ("Orientation  : " & Orientation'image);
          when Exif_Offset_Tag =>
            The_Exif_Offset := Main_Entry.Data;
          when Gps_Info_Tag =>
            The_Gps_Offset := Main_Entry.Data;
          when others =>
            null;
          end case;
          Set (Position => Next_IFD);
        end;
      end loop;
      if The_Exif_Offset /= 0 then
        Set (Offset => The_Exif_Offset);
        Get_Sub_Image_File_Directory;
      end if;
      if The_Gps_Offset /= 0 then
        Set (Offset => The_Gps_Offset);
        GPS_Info;
      end if;
    end Get_Main_Image_File_Directory;


    procedure Cleanup is
    begin
      OS.Close (File);
    end Cleanup;


    function Read_File_Mark return Marker is
      The_Marker : aliased Marker;
      use type OS.File_Descriptor;
    begin
      The_Orientation := Undefined;
      The_Image_Height := Undefined_Size;
      The_Image_Width := Undefined_Size;
      The_Altitude_Ref := Undefined;
      The_Altitude := Undefined_Height;
      The_Latitude_Ref := Undefined_Ref;
      The_Latitude := Undefined_Values;
      The_Longitude_Ref := Undefined_Ref;
      The_Longitude := Undefined_Values;
      The_Time_Stamp := Undefined_Values;
      The_Date_Stamp := Undefined_Date;
      Strings.Clear (The_Date_Time_Digitized);
      if File = OS.Invalid_FD then
        raise File_Not_Found;
      end if;
      Read (The_Marker'address, Marker_Size);
      return The_Marker;
    end Read_File_Mark;


    function Read_Exif_Header return Marker is

      type Header is record
        APP_Mark  : Marker;
        APP_Size  : Unsigned.Big_Endian_Word;
      end record with Pack;

      Header_Size : constant Integer := Header'size / System.Storage_Unit;

      type Exif_Header is record
        Exif_Id   : String(1..4);
        Zero      : Word;
        Tiff_Mark : Marker;
      end record with Pack;

      Exif_Header_Size : constant Integer := Exif_Header'size / System.Storage_Unit;

      The_Header      : aliased Header;
      The_Exif_Header : aliased Exif_Header;
      The_Count       : Integer;

    begin -- Read_Exif_Header
      loop
        Read (The_Header'address, Header_Size);
        The_Count := Integer(Unsigned.Swap(The_Header.APP_Size)) - Word_Size;
        if The_Header.APP_Mark = APP1_Marker then
          Read (The_Exif_Header'address, Exif_Header_Size);
          if The_Exif_Header.Exif_Id /= Exif_Id and The_Exif_Header.Zero /= 0 then
            Log.Error ("Invalid file header");
            raise Invalid_File;
          end if;
          return The_Exif_Header.Tiff_Mark;
        elsif The_Header.APP_Mark = JFIF_Marker then
          declare
            Data_Size  : constant Natural := The_Count;
            The_Buffer : aliased String(1 .. The_Count);
          begin
            Read (The_Buffer'address, Data_Size);
            if The_Count /= Data_Size then
              Log.Error ("Incorrect JFIF data");
              raise Invalid_File;
            end if;
          end;
        else
          Log.Error ("Unknown File Mark: " & The_Header.APP_Mark'image);
          raise Invalid_File;
        end if;
      end loop;
    end Read_Exif_Header;

    The_Marker : Marker := Read_File_Mark;

  begin -- Read
    if The_Marker = SOI_Marker then
      The_Marker := Read_Exif_Header;
    end if;
    if The_Marker = TIFF_II_Marker then
      Is_Big_Endian := False;
    elsif The_Marker = TIFF_MM_Marker then
      Is_Big_Endian := True;
    else
      Log.Error ("Unknown file header");
      raise Invalid_File;
    end if;
    The_Tiff_Start := The_Position - Marker_Size;
    Check (Tiff_Tag_Mark);
    Set (Offset => Get);
    Get_Main_Image_File_Directory;
    Cleanup;
  exception
  when others =>
    Cleanup;
    raise;
  end Read;

end Exif;
