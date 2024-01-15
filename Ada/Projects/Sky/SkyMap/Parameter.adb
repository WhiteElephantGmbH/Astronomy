-- *********************************************************************************************************************
-- *                       (c) 2023 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Text_IO;
with Application;
with Configuration;
with Error;
with File;
with Strings;
with Traces;

package body Parameter is

  package Log is new Traces ("Parameter");

  Filename : constant String := Application.Composure (Application.Name, "ini");

  Color_Key : constant String := "Color";
  Size_Key  : constant String := "Size";

  Sky_Id          : constant String := "Sky";
  Day_Color_Key   : constant String := "Day Color";
  Night_Color_Key : constant String := "Night Color";

  Earth_Id : constant String := "Earth";

  Moon_Id          : constant String := "Moon";
  Dark_Color_Key   : constant String := "Dark_Color";
  Bright_Color_Key : constant String := "Bright_Color";

  Sun_Id : constant String := "Sun";

  Stars_Id          : constant String := "Stars";
  Min_Size_Key      : constant String := "Min Size";
  Max_Size_Key      : constant String := "Max Size";
  Min_Magnitude_Key : constant String := "Min Magnitude";
  Max_Magnitude_Key : constant String := "Max Magnitude";

  Constellations_Id : constant String := "Constellations";

  Planets_Id : constant String := "Planets";

  The_Section : Configuration.Section_Handle;

  -- Sky
  The_Night_Sky_Color : Eps.Color;
  The_Day_Sky_Color   : Eps.Color;

  -- Earth
  The_Earth_Color : Eps.Color;

  -- Moon
  The_Bright_Moon_Color : Eps.Color;
  The_Dark_Moon_Color   : Eps.Color;
  The_Moon_Size         : Eps.Value;

  -- Sun
  The_Sun_Color : Eps.Color;
  The_Sun_Size  : Eps.Value;

  -- Stars
  The_Star_Colors    : Star.Colors;
  The_Stars_Min_Size : Eps.Value;
  The_Stars_Max_Size : Eps.Value;
  The_Min_Magnitude  : Star.Magnitude;
  The_Max_Magnitude  : Star.Magnitude;

  -- Constellations
  The_Line_Color : Eps.Color;
  The_Line_Size  : Eps.Value;

  -- Planets

  type Planet_Color is array (Solar.Planet) of Eps.Color;
  type Planet_Size is array (Solar.Planet) of Eps.Value;

  The_Planet_Color : Planet_Color;
  The_Planet_Size  : Planet_Size;


  function Color_Key_Of (Item : Solar.Planet) return String is (Solar.Image_Of (Item) & " Color");

  function Size_Key_Of (Item : Solar.Planet) return String is (Solar.Image_Of (Item) & " Size");


  function Star_Color_Key (Item : Star.Color_Range) return String is

    Class : constant Star.Spectral_Class := Star.Spectral_Class'val(Natural(Item) - Natural(Star.Color_Range'first));
    Image : constant String := Class'image & ' ';
  begin
    return Image(Image'first .. Image'first + 1) & " Class Color";
  end Star_Color_Key;


  function Adjusted (Item   : String;
                     Within : String) return String is
    Result : String := Within;
  begin
    Result(Result'first .. Result'first + Item'length - 1) := Item;
    return Result;
  end Adjusted;


  procedure Set (Section : Configuration.Section_Handle) is
  begin
    The_Section := Section;
  end Set;


  function String_Value_Of (Key : String) return String is
  begin
    return Configuration.Value_Of (The_Section, Key);
  exception
  when others =>
    return "";
  end String_Value_Of;


  function String_Of (Key : String) return String is
    Image : constant String := String_Value_Of (Key);
  begin
    if Image = "" then
      Error.Raise_With ("Parameter <" & Key & "> not defined");
    end if;
    return Image;
  end String_Of;


  function Value_Of (Key  : String) return Eps.Value is
    Item : constant String := String_Of (Key);
  begin
    return Eps.Value'value(Item);
  exception
  when others =>
    Error.Raise_With ("Incorrect " & Key & ": <" & Item & ">");
  end Value_Of;


  function Value_Of (Key  : String) return Star.Magnitude is
    Item : constant String := String_Of (Key);
  begin
    return Star.Magnitude'value(Item);
  exception
  when others =>
    Error.Raise_With ("Incorrect " & Key & ": <" & Item & ">");
  end Value_Of;


  function Value_Of (Key : String) return Eps.Color is

    Line  : constant String := String_Of (Key);
    Parts : constant Strings.Item := Strings.Item_Of (Line, Separator => '|');

    The_Color : Eps.Color := (others => 0.0);

  begin
    if Parts.Count = 0 then
      Error.Raise_With ("No color parts in " & Key);
    elsif Parts.Count > 4 then
      Error.Raise_With ("Too many color parts in " & Key & ": <" & Line & ">");
    end if;
    for Part of Parts loop
      declare
        Items : constant Strings.Item := Strings.Item_Of (Part, Separator => ' ');

        function Value return Eps.Color_Value is
        begin
          return Eps.Color_Value'value(Items(2));
        exception
        when others =>
          Error.Raise_With ("Incorrect color value in " & Key & ": <" & Part & ">");
        end Value;

      begin
        if Items.Count /= 2 or else Items(1)'length /= 1 then
          Error.Raise_With ("Incorrect color part in " & Key & ": <" & Part & ">");
        end if;
        declare
          Id : constant String := Items(1);
        begin
          case Id(Id'first) is
          when 'C' => The_Color.C := Value;
          when 'M' => The_Color.M := Value;
          when 'Y' => The_Color.Y := Value;
          when 'K' => The_Color.K := Value;
          when others =>
            Error.Raise_With ("Unknown color id '" & Id & "' in " & Key & ": <" & Part & ">");
          end case;
        end;
      end;
    end loop;
    return The_Color;
  end Value_Of;


  function Color_Value_Of (The_Planet : Solar.Planet) return Eps.Color is
  begin
    return Value_Of (Color_Key_Of (The_Planet));
  end Color_Value_Of;


  function Size_Value_Of (The_Planet : Solar.Planet) return Eps.Value is
  begin
    return Value_Of (Size_Key_Of (The_Planet));
  end Size_Value_Of;


  function Image_Of (Item : Eps.Color) return String is
  begin
    return " C" & Item.C'image & " | M" & Item.M'image & " | Y" & Item.Y'image & " | K" & Item.K'image;
  end Image_Of;


  procedure Read is

    procedure Create_Default_Parameters is

      function Color_Image_Of (The_Class : Star.Color_Range) return String is

        Default_Star_Color : constant Star.Colors := [
          (C => 0.67, M => 0.52, Y => 0.00, K => 0.00), -- O
          (C => 0.46, M => 0.35, Y => 0.00, K => 0.00), -- B
          (C => 0.18, M => 0.15, Y => 0.00, K => 0.00), -- A
          (C => 0.00, M => 0.00, Y => 0.00, K => 0.00), -- F
          (C => 0.02, M => 0.00, Y => 0.69, K => 0.00), -- G
          (C => 0.02, M => 0.37, Y => 0.72, K => 0.00), -- K
          (C => 0.02, M => 0.50, Y => 0.72, K => 0.00), -- L
          (C => 0.02, M => 0.68, Y => 0.71, K => 0.00), -- M
          (C => 0.00, M => 0.00, Y => 0.00, K => 1.00), -- (R)
          (C => 0.33, M => 0.52, Y => 0.99, K => 0.15), -- S
          (C => 0.33, M => 0.60, Y => 0.99, K => 0.15), -- T
          (C => 0.29, M => 0.87, Y => 0.99, K => 0.32), -- N
          (C => 0.09, M => 0.67, Y => 0.66, K => 0.00), -- C
          (C => 0.05, M => 0.04, Y => 0.00, K => 0.00), -- DA
          (C => 0.00, M => 0.00, Y => 0.00, K => 1.00), -- (DB)
          (C => 0.00, M => 0.00, Y => 0.00, K => 1.00), -- (DC)
          (C => 0.00, M => 0.00, Y => 0.00, K => 1.00), -- (DF)
          (C => 0.00, M => 0.00, Y => 0.00, K => 1.00), -- (DG)
          (C => 0.00, M => 0.00, Y => 0.00, K => 1.00), -- (DO)
          (C => 0.00, M => 0.00, Y => 0.00, K => 1.00), -- (DQ)
          (C => 0.00, M => 0.00, Y => 0.00, K => 1.00), -- (DZ)
          (C => 0.40, M => 0.00, Y => 0.00, K => 0.00), -- (WC)
          (C => 0.40, M => 0.00, Y => 0.00, K => 0.00), -- (WO)
          (C => 0.32, M => 0.15, Y => 0.03, K => 0.00), -- WN
          (C => 0.00, M => 0.00, Y => 0.00, K => 1.00)  -- (WR)
        ];

      begin
        return Image_Of (Default_Star_Color (The_Class));
      end Color_Image_Of;


      function Color_Image_Of (The_Planet : Solar.Planet) return String is

        use all type Solar.Planet;

        Default_Planet_Color : constant Planet_Color := [
          Mercury => (C => 0.44, M => 0.44, Y => 0.52, K => 0.08),
          Venus   => (C => 0.00, M => 0.00, Y => 0.10, K => 0.00),
          Mars    => (C => 0.00, M => 0.78, Y => 0.75, K => 0.00),
          Jupiter => (C => 0.00, M => 0.38, Y => 0.89, K => 0.00),
          Saturn  => (C => 0.17, M => 0.17, Y => 0.76, K => 0.00),
          Uranus  => (C => 0.65, M => 0.12, Y => 0.23, K => 0.00),
          Neptune => (C => 0.79, M => 0.65, Y => 0.00, K => 0.00),
          Pluto   => (C => 0.40, M => 0.61, Y => 0.47, K => 0.11)];

      begin
        return Image_Of (Default_Planet_Color (The_Planet));
      end Color_Image_Of;


      function Size_Image_Of (The_Planet : Solar.Planet) return String is

        use all type Solar.Planet;

        Default_Planet_Size : constant Planet_Size := [
          Mercury => 2.0,
          Venus   => 6.0,
          Mars    => 5.0,
          Jupiter => 7.0,
          Saturn  => 6.0,
          Uranus  => 3.0,
          Neptune => 2.0,
          Pluto   => 1.0];

      begin
        return Default_Planet_Size (The_Planet)'image;
      end Size_Image_Of;

      The_File : Ada.Text_IO.File_Type;

      procedure Put (Line : String) is
      begin
        Ada.Text_IO.Put_Line (The_File, Line);
      end Put;

    begin -- Create_Default_Parameters
      begin
        Ada.Text_IO.Create (The_File, Name => Filename);
      exception
      when others =>
        Error.Raise_With ("Can't create " & Filename);
      end;
      Put (Strings.Bom_8 & "[" & Sky_Id & "]");
      Put (Day_Color_Key & "   = C 0.80 | M 0.40 | Y 0.00 | K 0.00");
      Put (Night_Color_Key & " = C 0.00 | M 0.00 | Y 0.00 | K 1.00");
      Put ("");
      Put ("[" & Earth_Id & "]");
      Put (Color_Key & " = C 0.42 | M 0.00 | Y 0.82 | K 0.00");
      Put ("");
      Put ("[" & Moon_Id & "]");
      Put (Bright_Color_Key & " = C 0.00 | M 0.00 | Y 0.10 | K 0.10");
      Put (Dark_Color_Key & "   = C 0.00 | M 0.00 | Y 0.00 | K 0.90");
      Put (Size_Key & "         = 12.0");
      Put ("");
      Put ("[" & Sun_Id & "]");
      Put (Color_Key & " = C 0.00 | M 0.00 | Y 1.00 | K 0.00");
      Put (Size_Key & "  = 12.0");
      Put ("");
      Put ("[" & Stars_Id & "]");
      for The_Class in Star.Color_Range loop
        Put (Star_Color_Key (The_Class) & " = " & Color_Image_Of (The_Class));
      end loop;
      Put (Min_Size_Key & "      = 0.2");
      Put (Max_Size_Key & "      = 2.5");
      Put (Min_Magnitude_Key & " =-1.0");
      Put (Max_Magnitude_Key & " = 8.0");
      Put ("");
      Put ("[" & Planets_Id & "]");
      for The_Planet in Solar.Planet'range loop
        Put (Adjusted (Color_Key_Of (The_Planet), "              = ") & Color_Image_Of (The_Planet));
      end loop;
      for The_Planet in Solar.Planet'range loop
        Put (Adjusted (Size_Key_Of (The_Planet), "              = ") & Size_Image_Of (The_Planet));
      end loop;
      Put ("");
      Put ("[" & Constellations_Id & "]");
      Put (Color_Key & " = C 0.00 | M 1.00 | Y 0.00 | K 0.00");
      Put (Size_Key & "  = 0.5");
      Ada.Text_IO.Close (The_File);
    exception
    when Error.Occurred =>
      raise;
    when Item: others =>
      Log.Termination (Item);
      Ada.Text_IO.Delete (The_File);
      Error.Raise_With ("Internal Error - creating default parameters");
    end Create_Default_Parameters;


    procedure Read_Values is

      Handle         : constant Configuration.File_Handle    := Configuration.Handle_For (Filename);
      Sky            : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, Sky_Id);
      Earth          : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, Earth_Id);
      Moon           : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, Moon_Id);
      Sun            : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, Sun_Id);
      Stars          : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, Stars_Id);
      Planets        : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, Planets_Id);
      Constellations : constant Configuration.Section_Handle := Configuration.Handle_For (Handle, Constellations_Id);

    begin -- Read_Values
      Set (Sky);
      Log.Write ("Sky");
      The_Day_Sky_Color := Value_Of (Day_Color_Key);
      Log.Write ("  Day Color   :" & Image_Of (The_Day_Sky_Color));
      The_Night_Sky_Color := Value_Of (Night_Color_Key);
      Log.Write ("  Night Color :" & Image_Of (The_Night_Sky_Color));

      Set (Earth);
      Log.Write ("Earth");
      The_Earth_Color := Value_Of (Color_Key);
      Log.Write ("  Color :" & Image_Of (The_Earth_Color));

      Set (Moon);
      Log.Write ("Moon");
      The_Bright_Moon_Color := Value_Of (Bright_Color_Key);
      Log.Write ("  Bright Color :" & Image_Of (The_Dark_Moon_Color));
      The_Dark_Moon_Color := Value_Of (Dark_Color_Key);
      Log.Write ("  Dark Color   :" & Image_Of (The_Dark_Moon_Color));
      The_Moon_Size := Value_Of (Size_Key);
      Log.Write ("  Size         :" & The_Moon_Size'image);

      Set (Sun);
      Log.Write ("Sun");
      The_Sun_Color := Value_Of (Color_Key);
      Log.Write ("  Color :" & Image_Of (The_Sun_Color));
      The_Sun_Size := Value_Of (Size_Key);
      Log.Write ("  Size  :" & The_Sun_Size'image);

      Set (Stars);
      Log.Write ("Stars");
      for The_Class in Star.Color_Range loop
        The_Star_Colors(The_Class) := Value_Of (Star_Color_Key (The_Class));
        Log.Write ("  Line " & Star_Color_Key (The_Class) & " :" & Image_Of (The_Star_Colors(The_Class)));
      end loop;
      The_Stars_Min_Size := Value_Of (Min_Size_Key);
      Log.Write ("  Min Size       :" & The_Stars_Min_Size'image);
      The_Stars_Max_Size := Value_Of (Max_Size_Key);
      Log.Write ("  Max Size       :" & The_Stars_Max_Size'image);
      The_Min_Magnitude := Value_Of (Min_Magnitude_Key);
      Log.Write ("  Min Magnitude  :" & The_Min_Magnitude'image);
      The_Max_Magnitude := Value_Of (Max_Magnitude_Key);
      Log.Write ("  Max Magnitude  :" & The_Max_Magnitude'image);

      Set (Planets);
      for The_Planet in Solar.Planet'range loop
        The_Planet_Color(The_Planet) := Color_Value_Of (The_Planet);
      end loop;
      for The_Planet in Solar.Planet'range loop
        The_Planet_Size(The_Planet) := Size_Value_Of (The_Planet);
      end loop;

      Set (Constellations);
      Log.Write ("Constellation");
      The_Line_Color := Value_Of (Color_Key);
      Log.Write ("  Line Color :" & Image_Of (The_Line_Color));
      The_Line_Size := Value_Of (Size_Key);
      Log.Write ("  Line Size  :" & The_Line_Size'image);
    end Read_Values;

   begin -- Read
    Log.Write ("Read " & Filename);
    if not File.Exists (Filename) then
      Create_Default_Parameters;
    end if;
    Read_Values;
  end Read;

  function Earth_Color return Eps.Color is (The_Earth_Color);

  function Day_Sky_Color return Eps.Color is (The_Day_Sky_Color);

  function Night_Sky_Color return Eps.Color is (The_Night_Sky_Color);

  function Line_Color return Eps.Color is (The_Line_Color);

  function Line_Size return Eps.Value is (The_Line_Size);

  function Star_Colors return Star.Colors is (The_Star_Colors);

  function Star_Min return Eps.Value is (The_Stars_Min_Size);

  function Star_Max return Eps.Value is (The_Stars_Max_Size);

  function Magnitude_Min return Star.Magnitude is (The_Min_Magnitude);

  function Magnitude_Max return Star.Magnitude is (The_Max_Magnitude);

  function Sun_Color return Eps.Color is (The_Sun_Color);

  function Sun_Size return Eps.Value is (The_Sun_Size);

  function Bright_Moon_Color return Eps.Color is (The_Bright_Moon_Color);

  function Dark_Moon_Color return Eps.Color is (The_Dark_Moon_Color);

  function Moon_Size return Eps.Value is (The_Moon_Size);

  function Color_Of (The_Planet : Solar.Planet) return Eps.Color is (The_Planet_Color (The_Planet));

  function Size_Of (The_Planet : Solar.Planet) return Eps.Value is (The_Planet_Size (The_Planet));

end Parameter;
