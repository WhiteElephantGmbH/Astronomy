-- *********************************************************************************************************************
-- *                               (c) 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Direct_IO;
with Ada.IO_Exceptions;
with Exceptions;
with Text;
with Traces;

package body Fits is

  package Log is new Traces ("Fits");

  subtype Card is String(1 .. 80);

  package Fits_Io is new Ada.Direct_IO (Card);

  Undefined : constant := 0;

  The_Axis_Elements_1 : Natural := Undefined;
  The_Axis_Elements_2 : Natural := Undefined;

  Undefined_Date : constant Time.JD := Time.JD'first;

  The_Mid_Exposer_Date : Time.JD := Undefined_Date;

  function Read_Header (Filename : String) return Boolean is

    The_File   : Fits_Io.File_Type;
    The_Card   : Card;
    Has_Header : Boolean := False;

    function Opened_File return Boolean is
    begin
      for Unused_Count in 1 .. 3 loop
        begin
          Fits_Io.Open (The_File, Fits_Io.In_File, Filename);
         return True;
        exception
        when Ada.IO_Exceptions.Use_Error =>
          Log.Write ("Retry opening " & Filename);
          delay 0.3;
        when Item: others =>
          Log.Error ("Opening " & Filename & " failed with " & Exceptions.Name_Of (Item));
          return False;
        end;
      end loop;
      Log.Warning ("Opening " & Filename & " failed");
      return False;
    end Opened_File;

    Id_Size    : constant := 8;
    Value_Size : constant := 20;

  begin -- Read_Header
    The_Axis_Elements_1 := Undefined;
    The_Axis_Elements_2 := Undefined;
    The_Mid_Exposer_Date := Undefined_Date;
    if not Opened_File then
      return False;
    end if;
    Header:
    loop
      Fits_Io.Read (The_File, The_Card);
      declare
        Id : constant String := Text.Trimmed (The_Card(The_Card'first .. The_Card'first + Id_Size - 1));
      begin
        if Id = "END" then
          exit Header;
        elsif The_Card(The_Card'first + Id_Size) /= '=' then
          if Has_Header then
            Log.Error ("Missing '=' after " & Id);
            Has_Header := False;
          end if;
          exit Header;
        else
          for Character of Id loop
            if not (Text.Lowercase_Of (Character) in ' ' | '-' | '_' | '0' .. '9' | 'a' .. 'z') then
              if Has_Header then
                Log.Error ("Incorrect Id: " & Id);
                Has_Header := False;
              end if;
              exit Header;
            end if;
          end loop;
          declare
            First : constant Positive := The_Card'first + Id_Size + 2;

            function Last return Positive is
              Index : Positive := First + 1;
            begin
              while The_Card(Index) /= ''' loop
                Index := @ + 1;
              end loop;
              return Index;
            end Last;

            Value : constant String := (if The_Card(First) = '''
                                        then The_Card(First + 1 .. Last - 1)
                                        else Text.Trimmed (The_Card(First .. First + Value_Size - 1)));
          begin
            if not Has_Header then
              if Id = "SIMPLE" and then Value = "T" then
                Log.Write ("Standard header detected");
                Has_Header := True;
              else
                exit Header;
              end if;
            elsif Id = "INSTRUME" then
              Log.Write ("Instrument: " & Value);
            elsif Id = "NAXIS1" then
              The_Axis_Elements_1 := Positive'value(Value);
              Log.Write ("Axis 1 elements:" & The_Axis_Elements_1'image);
            elsif Id = "NAXIS2" then
              The_Axis_Elements_2 := Positive'value(Value);
              Log.Write ("Axis 2 elements:" & The_Axis_Elements_2'image);
            elsif Id = "JD_UTC" then
              The_Mid_Exposer_Date := Time.JD'value(Value);
              Log.Write ("Mid exposure time: " & Time.Image_Of (Time.Ut_Of (The_Mid_Exposer_Date)));
            elsif Id = "EXPTIME" then
              Log.Write ("Exposure Time: " & Value);
            elsif Id = "GAIN" then
              Log.Write ("Gain: " & Value);
            elsif Id = "BLKLEVEL" then
              Log.Write ("Black Level (Offset): " & Value);
            elsif Id = "CCD-TEMP" then
              Log.Write ("CCD Temperature: " & Value);
            elsif Id = "SWCREATE" then
              Log.Write ("Created by: " & Value);
            else
              null;
            end if;
          end;
        end if;
      end;
    end loop Header;
    Fits_Io.Close (The_File);
    return Has_Header;
  exception
  when Item: others =>
    Log.Termination (Item);
    Fits_Io.Close (The_File);
    return False;
  end Read_Header;


  function Is_Landscape return Boolean is
  begin
    if The_Axis_Elements_1 /= Undefined and The_Axis_Elements_2 /= Undefined then
      return The_Axis_Elements_1 > The_Axis_Elements_2;
    end if;
    raise Undefined_Value;
  end Is_Landscape;


  function Mid_Exposer_Date return Time.JD is
    use type Time.JD;
  begin
    if The_Mid_Exposer_Date /= Undefined_Date then
      return The_Mid_Exposer_Date;
    end if;
    raise Undefined_Value;
  end Mid_Exposer_Date;

end Fits;
