-- *********************************************************************************************************************
-- *                           (c) 2025 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

with Ada.Calendar;
with Ada.Streams.Stream_IO;
with Ada.Numerics.Generic_Elementary_Functions;
with GID;
with Traces;
with Unsigned;

package body Focus is

  package Log is new Traces ("Focus");

  package Numeric is new Ada.Numerics.Generic_Elementary_Functions (Float);


  function Half_Flux_Diameter (Filename : String) return Diameter is

    GID_Image : GID.Image_descriptor;

    The_Diameter : Diameter;

    procedure Evaluate_Diameter is

      Width  : constant Positive := GID.Pixel_width (GID_Image);
      Height : constant Positive := GID.Pixel_height (GID_Image);
      Bits   : constant Positive := GID.Bits_per_pixel (GID_Image);

      type Value is new Unsigned.Byte;

      Center_X    : constant Natural := Width / 2;
      Center_Y    : constant Natural := Height / 2;
      Center_Area : constant Natural := Height / 8;

      type Column is new Natural range Center_X - Center_Area .. Center_X + Center_Area - 1;
      type Row    is new Natural range Center_Y - Center_Area .. Center_Y + Center_Area - 1;

      type Image is array (Column, Row) of Value with Pack;

      The_Image : Image;


      procedure Set_Image is

        type Color_Range is mod 2**8;

        Actual_X     : Natural;
        Actual_Y     : Natural;
        Is_Set       : Boolean := False;
        New_Position : Boolean := False;

        procedure Set_X_Y (X, Y: Natural) is
        begin
          Actual_X := X;
          Actual_Y := Y;
          New_Position := True;
          Is_Set := True;
        end Set_X_Y;

        procedure Put_Pixel (Red    : Color_Range;
                             Green  : Color_Range;
                             Blue   : Color_Range;
                             Unused : Color_Range) is
        begin
          if not Is_Set then
            raise Program_Error;
          end if;
          if not New_Position then
            Actual_X := Actual_X + 1;
          end if;
          if Actual_X >= Natural(Column'first) and Actual_X <= Natural(Column'last) and
             Actual_Y >= Natural(Row'first) and Actual_Y <= Natural(Row'last)
          then
            The_Image(Column(Actual_X), Row(Actual_Y)) := Value((Natural(Red) + Natural(Green) + Natural(Blue)) / 3);
          end if;
          New_Position := False;
        end Put_Pixel;

        procedure Feedback (Percents : Natural) is
        begin
          null;
        end Feedback;

        procedure Load_Image is new GID.Load_image_contents (Primary_color_range => Color_Range,
                                                             Set_X_Y             => Set_X_Y,
                                                             Put_Pixel           => Put_Pixel,
                                                             Feedback            => Feedback,
                                                             mode                => GID.fast);
        Next_Frame : Ada.Calendar.Day_Duration;

      begin -- Set_Image
        Load_Image (image      => GID_Image,
                    next_frame => Next_Frame);
      end Set_Image;


      Max_Value   : Value := 0;
      The_Average : Value;

      procedure Find_Average_And_Maximum is
        The_Total  : Long_Long_Integer := 0;
        The_Value  : Value;
      begin
        for The_Column in Column loop
          for The_Row in Row loop
            The_Value := The_Image(The_Column, The_Row);
            if The_Value > Max_Value then
              Max_Value := The_Value;
            end if;
            The_Total := The_Total + Long_Long_Integer(The_Value);
          end loop;
        end loop;
        The_Average := Value(The_Total / Long_Long_Integer(Center_Area * 2) ** 2);
        Log.Write ("Average:" & The_Average'image);
        Log.Write ("Maximum:" & Max_Value'image);
      end Find_Average_And_Maximum;


      Center_Column : Column;
      Center_Row    : Row;
      Max_Count     : Natural := 0;

      procedure Find_Largest_Object is
        The_Count : Natural := 0;
        Limit     : constant Value := Max_Value - (Max_Value - The_Average) / 2;
      begin
        Log.Write ("Limit:" & Limit'image);
        for The_Column in Column loop
          for The_Row in Row loop
            if The_Image(The_Column, The_Row) > Limit then
              The_Count := The_Count + 1;
            else
              if The_Count > Max_Count then
                Max_Count := The_Count;
                Center_Column := The_Column;
                Center_Row := The_Row;
              end if;
              The_Count := 0;
            end if;
          end loop;
        end loop;
        if Max_Count < 2 then
          raise No_Object_Found;
        end if;
        Center_Row := Row(Natural(Center_Row) - Max_Count / 2);
        Log.Write ("Column:" & Center_Column'image);
        Log.Write ("Row:" & Natural'(Height - Natural(Center_Row))'image);
        Log.Write ("Size:" & Max_Count'image);
      end Find_Largest_Object;


      The_Pixel_Count  : Natural := 0;

      procedure Count_Object_Pixels is
        The_First_Column : Column  := Center_Column;
        The_Last_Column  : Column  := Center_Column;
        The_First_Row    : Row     := Center_Row;
        The_Last_Row     : Row     := Center_Row;
      begin
        loop
          exit when The_First_Column = Column'first;
          The_First_Column := The_First_Column - 1;
          exit when The_Image(The_First_Column, Center_Row) < The_Average;
        end loop;
        Log.Write ("First Column:" & The_First_Column'image);
        loop
          exit when The_Last_Column = Column'last;
          The_Last_Column := The_Last_Column + 1;
          exit when The_Image(The_Last_Column, Center_Row) < The_Average;
        end loop;
        Log.Write ("Last Column:" & The_Last_Column'image);
        loop
          exit when The_First_Row = Row'first;
          The_First_Row := The_First_Row - 1;
          exit when The_Image(Center_Column, The_First_Row) < The_Average;
        end loop;
        Log.Write ("First Row:" & The_First_Row'image);
        loop
          exit when The_Last_Row = Row'last;
          The_Last_Row := The_Last_Row + 1;
          exit when The_Image(Center_Column, The_Last_Row) < The_Average;
        end loop;
        Log.Write ("Last Row:" & The_Last_Row'image);
        for The_Column in The_First_Column .. The_Last_Column loop
          for The_Row in The_First_Row .. The_Last_Row loop
            if The_Image(The_Column, The_Row) > The_Average then
              The_Pixel_Count := The_Pixel_Count + 1;
            end if;
          end loop;
        end loop;
        Log.Write ("Pixel Count:" & The_Pixel_Count'image);
      end Count_Object_Pixels;

    begin -- Evaluated_Diameter
      if not (Bits = 32 or Bits = 24) then
        Log.Error ("not supported number of bits:" & Bits'image);
        raise Unknown_File;
      end if;
      Set_Image;
      Find_Average_And_Maximum;
      Find_Largest_Object;
      Count_Object_Pixels;
      The_Diameter := Diameter(Numeric.Sqrt (Float(The_Pixel_Count)));
    end Evaluate_Diameter;

    The_File  : Ada.Streams.Stream_IO.File_Type;

  begin
    begin
      Ada.Streams.Stream_IO.Open (File => The_File,
                                  Mode => Ada.Streams.Stream_IO.In_File,
                                  Name => Filename);
    exception
    when others =>
      raise File_Not_Found;
    end;
    declare
    begin
      GID.Load_image_header (image => GID_Image,
                             from  => Ada.Streams.Stream_IO.Stream(The_File).all);
      Evaluate_Diameter;
      Ada.Streams.Stream_IO.Close (The_File);
      return The_Diameter;
    exception
    when Item: others =>
      Log.Termination (Item);
    end;
    Ada.Streams.Stream_IO.Close (The_File);
    raise Unknown_File;
  end Half_Flux_Diameter;

end Focus;
