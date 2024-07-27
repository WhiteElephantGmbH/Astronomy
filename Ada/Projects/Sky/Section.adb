-- *********************************************************************************************************************
-- *                           (c) 2024 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

with Error;
with File;
with Text;

package body Section is

  The_Section : Configuration.Section_Handle;

  procedure Set (Handle : Configuration.Section_Handle) is
  begin
    The_Section := Handle;
  end Set;


  function Angles_Of (Key     : String;
                      Maximum : Natural; -- in degrees
                      Unit    : String := "") return Angles.List is

    function Image_Of (The_Ordinal : Positive) return String is
      Image : constant String := Text.Trimmed (The_Ordinal'img);
    begin
      if not (The_Ordinal in 4 .. 20) then
        case The_Ordinal mod 10 is
        when 1 =>
          return Image & "st";
        when 2 =>
          return Image & "nd";
        when 3 =>
          return Image & "rd";
        when others =>
          null;
        end case;
      end if;
      return Image & "th";
    end Image_Of;

    Item     : constant String := String_Of (Key);
    Images   : constant Text.Strings := Text.Strings_Of (Item, ',');
    The_List : Angles.List;

  begin -- Angles_Of
    for Index in Text.First_Index .. Images.Count loop
      begin
        declare
          Image : constant String := Image_Of (Images(Index), Unit);
          Value : constant Angle.Value := Angle.Value_Of (Image);
        begin
          if Value < (Angle.Value'(+Angle.Degrees(Maximum)) + Angle.Epsilon) then
            The_List.Append (Value);
          else
            Error.Raise_With ("value greater than" & Maximum'img & "°" & Unit);
          end if;
        end;
      exception
      when others =>
        Error.Raise_With ("Incorrect " & Image_Of (Index) & " value of " & Key & ": <" & Item & ">");
      end;
    end loop;
    return The_List;
  end Angles_Of;


  function Degrees_Of (Key     : String;
                       Maximum : Angle.Degrees;
                       Minimum : Angle.Degrees := 0.0) return Angle.Degrees is
    Item : constant String := String_Of (Key);
  begin
    return Angle.Degrees_Of (Item, Lower_Limit => Minimum, Upper_Limit => Maximum);
  exception
  when others =>
    Error.Raise_With ("Incorrect value of " & Key & ": <" & Item & ">");
  end Degrees_Of;


  function Direction_Of (Key : String) return Earth.Direction is
    Image : constant String := String_Value_Of (Key);
  begin
    if Image = "" then
      return Earth.Unknown_Direction;
    end if;
    begin
      declare
        Az : constant Angle.Degrees := Degrees_Of (Key, 360.0);
      begin
        return Earth.Direction_Of (Az=> +Az, Alt => +Angle.Degrees(5.0));
      end;
    exception
    when others =>
      Error.Raise_With ("Incorrect value for " & Key & ": <" & Image & ">");
    end;
  end Direction_Of;


  function Duration_Of (Key         : String;
                        Lower_Limit : Duration := 0.0;
                        Upper_Limit : Duration) return Duration is
    Image : constant String := String_Value_Of (Key);

    function Image_Of (Item : Duration) return String is
      type Value is delta 0.1 range Duration'first .. Duration'last;
    begin
      return Text.Trimmed (Value(Item)'image) & 's';
    end Image_Of;

  begin -- Duration_Of
    if Image = "" then
      return 0.0;
    end if;
    if Image(Image'last) /= 's' then
      Error.Raise_With ("Unit s (seconds) missing at end of value for " & Key & ": <" & Image & ">");
    end if;
    begin
      declare
        Value : constant Duration := Duration'value(Image(Image'first .. Image'last - 1));
      begin
        if Value >= Lower_Limit and Value <= Upper_Limit then
          return Value;
        else
          Error.Raise_With ("value not in range " & Image_Of (Lower_Limit) & " .. " & Image_Of (Upper_Limit));
        end if;
      end;
    exception
    when others =>
      Error.Raise_With ("Incorrect value for " & Key & ": <" & Image & ">");
    end;
  end Duration_Of;


  function Filename_Of (Key  : String;
                        Name : String := "") return String is
    Filename : constant String := String_Of (Key, Name);
  begin
    if not File.Exists (Filename) then
      Error.Raise_With ("Filename " & Filename & " not found for " & Key);
    end if;
    return Filename;
  end Filename_Of;


  function Image_Of (Item : String;
                     Unit : String := "") return String is
  begin
    if Unit /= "" then
      if Item(Item'last - Unit'length + 1 .. Item'last) /= Unit then
        raise Error.Occurred;
      end if;
    end if;
    return Item(Item'first .. Item'last - Unit'length);
  end Image_Of;


  function Ip_Address_For (Name : String) return Network.Ip_Address is
    Server      : constant String := String_Of (Ip_Address_Key, Name);
    The_Address : Network.Ip_Address;
  begin
    begin
      The_Address := Network.Ip_Address_Of (Server);
    exception
    when others =>
      The_Address := Network.Ip_Address_Of_Host (Server);
    end;
    return The_Address;
  exception
  when others =>
    Error.Raise_With ("Incorrect " & Name & " IP address " & Server);
  end Ip_Address_For;


  function Port_For (Name : String) return Network.Port_Number is
    Value : constant Integer := Value_Of (Port_Key, Name);
  begin
    return Network.Port_Number (Value);
  exception
  when others =>
    Error.Raise_With (Name & " port number" & Value'image & " out of range");
  end Port_For;


  function String_Of (Key  : String;
                      Name : String := "") return String is
    Image : constant String := String_Value_Of (Key);
  begin
    if Image = "" then
      Error.Raise_With ("Parameter <" & Key & (if Name = "" then "" else "> for <") & Name & "> not defined");
    end if;
    return Image;
  end String_Of;


  function String_Value_Of (Key : String) return String is
  begin
    return Configuration.Value_Of (The_Section, Key);
  exception
  when others =>
    return "";
  end String_Value_Of;


  function Value_Of (Key  : String;
                     Name : String := "") return Integer is
    Item : constant String := String_Of (Key, Name);
  begin
    return Integer'value(Image_Of(Item));
  exception
  when others =>
    Error.Raise_With ("Incorrect " & (if Name = "" then "" else Name & " ") & Key & ": <" & Item & ">");
  end Value_Of;

end Section;
