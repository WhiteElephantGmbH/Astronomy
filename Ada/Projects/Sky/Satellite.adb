-- *********************************************************************************************************************
-- *                       (c) 2013 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
pragma Style_Astronomy;

with Ada.Containers.Ordered_Maps;
with AWS.Client;
with AWS.Messages;
with AWS.Response;
with Error;
with Persistent;
with Traces;
with Text;
with Time;

package body Satellite is

  package Log is new Traces (Id);

  use type Number;

  type Data is record
    Name    : Text.String;
    Element : Tle;
  end record;

  package Tle_Data is new Ada.Containers.Ordered_Maps (Key_Type     => Number,
                                                       Element_Type => Data);

  type Items is record
    Last_Update   : Time.Calendar_Value;
    Actual_Groups : Groups.Set;
    Tle_Map       : Tle_Data.Map;
  end record;

  package Persistent_Storage is new Persistent (Items, Name => Id);

  Persistent_Items : Persistent_Storage.Data;

  Tle_Map       : Tle_Data.Map renames Persistent_Items.Storage.Tle_Map;
  Actual_Groups : Groups.Set renames  Persistent_Items.Storage.Actual_Groups;
  Last_Update   : Time.Calendar_Value renames Persistent_Items.Storage.Last_Update;

  Tle_Map_Save : Tle_Data.Map;

  Update_Failed : exception;


  procedure Read (Selection : String;
                  Target    : String) is

    Url : constant String := "https://celestrak.org/NORAD/elements/gp.php?" & Selection & "=" & Target & "&FORMAT=TLE";

  begin
    Log.Write ("URL: " & Url);
    declare
      Response : constant AWS.Response.Data := AWS.Client.Get (Url);
      Status   : constant AWS.Messages.Status_Code := AWS.Response.Status_Code (Response);
      use type AWS.Messages.Status_Code;
    begin
      if Status = AWS.Messages.S200 then
        declare
          Result : constant String := AWS.Response.Message_Body (Response);
          Last   : Natural := Result'first;

          function Next_Line return String is
            First : constant Natural := Last;
          begin
            while Last <= Result'last and then not (Result(Last) in Ascii.Cr | Ascii.Lf) loop
              Last := @ + 1;
            end loop;
            return Dummy : constant String := Result(First .. Last - 1) do
              Last := @ + 1;
              if Last <= Result'last and then Result(Last) in Ascii.Cr | Ascii.Lf then
                Last := @ + 1;
              end if;
            end return;
          end Next_Line;

        begin
          while Last <= Result'last loop
            declare
              Name : constant String := Text.Trimmed (Next_Line);
              Tle1 : constant String := Next_Line;
              Tle2 : constant String := Next_Line;
            begin
              declare
                Item : constant Data := (Name    => [Name],
                                         Element => [1 => Tle1,
                                                     2 => Tle2]);
                Key : constant Number := Norad.Number_Of (Item.Element);
              begin
                if not Tle_Map.Contains (Key) then
                  if Norad.Is_In_Deep_Space (Item.Element) then
                    Log.Warning (Name & " is in deep space");
                  else
                    Log.Write ("Name: " & Name);
                    Log.Write ("Tle1: " & Tle1);
                    Log.Write ("Tle2: " & Tle2);
                    Tle_Map.Insert (Key, Item);
                  end if;
                end if;
              end;
            end;
          end loop;
        end;
      else
        Log.Warning ("Data update failed for " & Target);
        raise Update_Failed;
      end if;
    end;
  end Read;


  function Enum_Style_Of (Image : String) return String is
    The_Image : String := Image;
  begin
    for The_Character of The_Image loop
      if The_Character in ' ' | '-' then
        The_Character := '_';
      end if;
    end loop;
    return The_Image;
  end Enum_Style_Of;


  function Group_Style_Of (Image : String) return String is
    The_Image : String := Image;
  begin
    for The_Character of The_Image loop
      if The_Character = '_' then
        The_Character := '-';
      else
        The_Character := Text.Lowercase_Of (@);
      end if;
    end loop;
    return The_Image;
  end Group_Style_Of;


  function Image_Of (Item : Group) return String is
  begin
    return Group_Style_Of (Item'image);
  end Image_Of;


  function Image_Of (Value : Groups.Set) return String is
    Image : constant String := Group_Style_Of (Value'image);
  begin
    return Image(Image'first + 1 .. Image'last - 1);
  end Image_Of;


  procedure Set_Groups (Image : String) is
    Images    : constant Text.Strings := Text.Strings_Of (Image, Separator => ',');
    The_Group : Group;
    use type Groups.Set;
  begin
    The_Groups := [];
    for Group_Image of Images loop
      begin
        The_Group := Group'value(Enum_Style_Of (Group_Image));
      exception
      when others =>
        Error.Raise_With (Id & " group " & Group_Image & " unknown");
      end;
      if The_Group < The_Groups then
        Error.Raise_With (Id & " group " & Group_Image & " already defined");
      end if;
      The_Groups := @ + The_Group;
    end loop;
    Log.Write ("Groups: " & Image_Of (The_Groups));
  end Set_Groups;


  procedure Read_Group (From : Group) is
    Group_Name : constant String := Image_Of (From);
  begin
    Read ("GROUP", Group_Name);
  end Read_Group;


  The_Objects : Numbers.Set := [];

  procedure Add_Object (Item : Number) is
    use type Numbers.Set;
  begin
    The_Objects := @ + Item;
  end Add_Object;


  procedure Initialize_Objects is
  begin
    The_Objects := [];
    for Item of Tle_Map loop
      Add_Object (Norad.Number_Of (Item.Element));
    end loop;
  end Initialize_Objects;


  function Read return Boolean is
  begin
    if not Tle_Map.Is_Empty then
      declare
        subtype Hours is Duration delta 0.1;
        Maximum_Data_Age : constant Hours := 6.0;
        Age_Of_Data      : constant Hours := Time.Duration_Since (Last_Update) / Time.One_Hour;
        use type Groups.Set;
      begin
        Log.Write ("Age of data =" & Age_Of_Data'image & " hours");
        if Age_Of_Data < Maximum_Data_Age and then Actual_Groups = The_Groups then
          Initialize_Objects;
          return True;
        end if;
      end;
      Tle_Map_Save := Tle_Map;
      Tle_Map.Clear;
    end if;
    for The_Group of The_Groups loop
      Read_Group (The_Group);
    end loop;
    Log.Write ("Number of visible satellites:" & Tle_Map.Length'image);
    Actual_Groups := The_Groups;
    Last_Update := Time.Calendar_Now;
    Initialize_Objects;
    return True;
  exception
  when Update_Failed =>
    Tle_Map := Tle_Map_Save;
    Initialize_Objects;
    if Tle_Map.Is_Empty then
      Actual_Groups := [];
      Log.Warning ("update failed - no data");
      return False;
    else
      Log.Warning ("update failed - old data in use");
      return True;
    end if;
  when Item: others =>
    Log.Termination (Item);
    return False;
  end Read;


  function Read (Object : Number) return Boolean is
    use type Numbers.Set;
  begin
    if not (Object < The_Objects) then
      Read ("CATNR", Text.Trimmed(Object'image));
      Add_Object (Object);
      return True;
    end if;
    return False;
  end Read;


  function Objects return Numbers.Set is
  begin
    return The_Objects;
  end Objects;


  function Tle_Of (Object : Number) return Tle is
  begin
    return Tle_Map.Element (Object).Element;
  end Tle_Of;


  function Tle_Name_Of (Object : Number) return String is
  begin
    return Tle_Map.Element(Object).Name.S;
  end Tle_Name_Of;


  function Name_Of (Object : Number) return String is
    Id : constant String := "   " & Object'image;
  begin
    return Id(Id'last - 4 .. Id'last) & " " & (Tle_Name_Of (Object));
  end Name_Of;

end Satellite;
