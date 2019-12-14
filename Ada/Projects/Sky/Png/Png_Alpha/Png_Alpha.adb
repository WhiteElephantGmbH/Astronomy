-- *********************************************************************************************************************
-- *                           (c) 2019 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

with Ada.Command_Line;
with Ada.Text_IO;
with Exceptions;
with Png;

procedure Png_Alpha is

  pragma Linker_Options ("-mconsole");

  Syntax_Error : constant String := "Syntax: png_alpha <filename>.png ";

  procedure Put (Item : String) is
  begin
    Ada.Text_IO.Put_Line (Item);
  end Put;

begin -- Png_Alpha

  if Ada.Command_Line.Argument_Count /= 1 then
    Put (Syntax_Error);
    return;
  end if;

  declare
    Filename : constant String  := Ada.Command_Line.Argument(1);
    The_File : Png.File;
  begin
    begin
      Png.Open (The_File, Filename);
    exception
    when Occurence : others =>
      Put ("Failed to open file: " & Filename);
      Put (Exceptions.Information_Of (Occurence));
      return;
    end;

    declare
      W : constant Png.Dimension        := Png.Width (The_File);
      H : constant Png.Dimension        := Png.Height (The_File);
      D : constant Png.Depth            := Png.Bit_Depth (The_File);
      T : constant Png.Colour_Type_Code := Png.Colour_Type (The_File);
    begin
      Put ("Image information from file: " & Filename);
      Put ("  Width  :"  & W'img);
      Put ("  Height :"  & H'img);
      Put ("  Depth  : " & D'img);
      Put ("  Type   : " & T'img);
      Put ("Limits");
      for Column in 0 .. W-1 loop
        for Row in 0 .. H-1 loop
          declare
            A : constant Natural := Png.Alpha_Value (The_File, R => Row, C => Column);
          begin
            if A = 255 then
              Put(" " & Column'img & " :" & Row'img);
              exit;
            end if;
          end;
        end loop;
      end loop;
    end;

    Png.Close(The_File);
  end;
exception
when Item: others =>
  Put ("Program terminated by exception.");
  Put (Exceptions.Information_Of (Item));
end Png_Alpha;
