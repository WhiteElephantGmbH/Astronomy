-- *********************************************************************************************************************
-- *                           (c) 2021 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Strings;

package body Lx200 is

  Command_Start : constant Character := ':';


  function Command_For (Item : String) return String is
  begin
    return Command_Start & Item & Terminator;
  end Command_For;


  function String_Of (Item      : Command;
                      Parameter : String := "") return String is
  begin
    case Item is
    when Get_Alignment_Status =>
      return Command_For ("GW");
    when Get_Product_Name =>
      return Command_For ("GVP");
    when Get_Firmware_Date =>
      return Command_For ("GVD");
    when Get_Firmware_Number =>
      return Command_For ("GVN");
    when Get_Latitude =>
      return Command_For ("Gt");
    when Get_Longitude =>
      return Command_For ("Gg");
    when Get_Sideral_Time =>
      return Command_For ("GS");
    when Get_Altitude =>
      return Command_For ("GA");
    when Get_Azimuth =>
      return Command_For ("GZ");
    when Get_Right_Ascension =>
      return Command_For ("GR");
    when Get_Declination =>
      return Command_For ("GD");
    when Set_Alt_Az_Alignment =>
      return Command_For ("AA");
    when Set_Polar_Alignment =>
      return Command_For ("AP");
    when Set_Latitude =>
      return Command_For ("St" & Parameter);
    when Set_Longitude =>
      return Command_For ("Sg" & Parameter);
    when Set_Local_Time =>
      return Command_For ("SL" & Parameter);
    when Set_Time_Offset =>
      return Command_For ("SG" & Parameter);
    when Set_Altitude =>
      return Command_For ("Sa" & Parameter);
    when Set_Azimuth =>
      return Command_For ("Sz" & Parameter);
    when Set_Right_Ascension =>
      return Command_For ("Sr" & Parameter);
    when Set_Declination =>
      return Command_For ("Sd" & Parameter);
    when Slew =>
      return Command_For ("MS");
    when Synchronize =>
      return Command_For ("CM");
    when Move_East =>
      return Command_For ("Me");
    when Move_North =>
      return Command_For ("Mn");
    when Move_South =>
      return Command_For ("Ms");
    when Move_West =>
      return Command_For ("Mw");
    when Quit_Move_East =>
      return Command_For ("Qe");
    when Quit_Move_North =>
      return Command_For ("Qn");
    when Quit_Move_South =>
      return Command_For ("Qs");
    when Quit_Move_West =>
      return Command_For ("Qw");
    when Quit_Move =>
      return Command_For ("Q");
    when Set_Centering_Rate =>
      return Command_For ("RC");
    when Set_Guiding_Rate =>
      return Command_For ("RG");
    when Set_Finding_Rate =>
      return Command_For ("RM");
    when Set_Slewing_Rate =>
      return Command_For ("RS");
    end case;
  end String_Of;


  function Angle_Image_Of (Data       : String;
                           U1, U2, U3 : Character) return String is
                                     --             1  2  3
    The_Image : String := Data & U3; -- format: "sddUddUddU
  begin
    The_Image(The_Image'last - 6) := U1;
    The_Image(The_Image'last - 3) := U2;
    return The_Image;
  end Angle_Image_Of;



  function Signed_Degrees_Of (Item         : Angle.Value;
                              Front_Digits : Natural := 2) return String is

    use type Angle.Units;

    The_Image : String := Strings.Ansi_Of (Angle.Image_Of (The_Value   => Item,
                                                           Unit        => Angle.In_Degrees,
                                                           Decimals    => 0,
                                                           Show_Signed => True));
    Image_Template : constant String := "sdDDmDDs";

    Digits_To_Insert : constant Natural := Image_Template'length + Front_Digits - The_Image'length;

    Leading_Zeros : constant String(1..Digits_To_Insert) := (others => '0');

  begin -- Signed_Degrees_Of
    The_Image(The_Image'last - 6) := '*'; -- replace d
    The_Image(The_Image'last - 3) := ':'; -- replace m
    return The_Image(The_Image'first) & Leading_Zeros & The_Image(The_Image'first + 1 .. The_Image'last - 1);
  end Signed_Degrees_Of;


  function Signed_Degrees_Of (Item : String) return Angle.Value is
  begin
    return Angle.Value_Of (Angle_Image_Of (Item, 'd', ''', '"'), With_Units => Angle.In_Degrees);
  end Signed_Degrees_Of;


  function Hours_Of (Item : Angle.Value) return String is

    use type Angle.Units;

    The_Image : String := Strings.Ansi_Of (Angle.Image_Of (The_Value   => Item,
                                                           Unit        => Angle.In_Hours,
                                                           Decimals    => 0,
                                                           Show_Signed => False));
    Image_Template : constant String := "DDhDDmDDs";

    Digits_To_Insert : constant Natural := Image_Template'length - The_Image'length;

    Leading_Zeros : constant String(1..Digits_To_Insert) := (others => '0');

  begin -- Hours_Of
    The_Image(The_Image'last - 6) := ':'; -- replace h
    The_Image(The_Image'last - 3) := ':'; -- replace m
    return Leading_Zeros & The_Image(The_Image'first .. The_Image'last - 1);
  end Hours_Of;


  function Hours_Of (Item : String) return Angle.Value is
  begin
    return Angle.Value_Of (Angle_Image_Of (Item, 'h', 'm', 's'), With_Units => Angle.In_Hours);
  end Hours_Of;

end Lx200;
