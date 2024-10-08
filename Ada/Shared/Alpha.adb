-- *********************************************************************************************************************
-- *                       (c) 2020 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
with GID;

package body Alpha is

  The_Last_Row : Row := 0;

  function Last_Row return Row is
  begin
    return The_Last_Row;
  end Last_Row;


  function Limits_Of (Filename : String) return Limits is

    The_Image : GID.Image_descriptor;
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
      GID.Load_image_header (image => The_Image,
                             from  => Ada.Streams.Stream_IO.Stream(The_File).all);
      declare
        Width     : constant Positive := GID.Pixel_width (The_Image);
        Height    : constant Positive := GID.Pixel_height (The_Image);
        Bits      : constant Positive := GID.Bits_per_pixel (The_Image);
        Has_Alpha : constant Boolean  := GID.Expect_transparency (The_Image);

        subtype Actual_Column is Column range 0 .. Column(Width) - 1;

        subtype Actual_Limits is Limits(Actual_Column);

        type Color_Range is mod 2**8;

        Row_Offset    : constant Row := Row(Height) / 2;
        Undefined_Row : constant Row := Row'last;
        Undefined     : constant Limit := (Undefined_Row, Undefined_Row);

        New_Position : Boolean;
        The_Column   : Column;
        The_Row      : Row;
        The_Limits   : Actual_Limits := (others => Undefined);

        procedure Set_X_Y (X, Y: Natural) is
        begin
          The_Column := Column(X);
          The_Row := Row(Y);
          New_Position := True;
        end Set_X_Y;

        procedure Put_Pixel (Unused_Red   : Color_Range;
                             Unused_Green : Color_Range;
                             Unused_Blue  : Color_Range;
                             Alpha_Value  : Color_Range) is
        begin
          if not New_Position then
            The_Column := The_Column + 1;
          end if;
          if The_Row < Row_Offset then -- Below zero horizon
            if The_Limits(The_Column).Lower = Undefined_Row then
              The_Limits(The_Column).Lower := 0; -- force to zero
            end if;
            if The_Limits(The_Column).Upper = Undefined_Row then
              The_Limits(The_Column).Upper := 0; -- force to zero
            end if;
          else
            if Alpha_Value < Color_Range'last - 10 then
              The_Limits(The_Column).Lower := Undefined_Row;
            else
              if The_Limits(The_Column).Lower = Undefined_Row then
                The_Limits(The_Column).Lower := The_Row - Row_Offset + 1;
              end if;
              if The_Limits(The_Column).Upper = Undefined_Row then
                The_Limits(The_Column).Upper := The_Row - Row_Offset + 1;
              end if;
            end if;
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

      begin
        if Bits /= 32 or else not Has_Alpha then
          raise Unknown_File;
        end if;
        Load_Image (image      => The_Image,
                    next_frame => Next_Frame);
        Ada.Streams.Stream_IO.Close (The_File);
        The_Last_Row := Row_Offset - 1;
        return The_Limits;
      end;
    exception
    when others =>
      null;
    end;
    Ada.Streams.Stream_IO.Close (The_File);
    raise Unknown_File;
  end Limits_Of;

end Alpha;
