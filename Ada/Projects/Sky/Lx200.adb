-- *********************************************************************************************************************
-- *                       (c) 2021 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Strings;
with Traces;

package body Lx200 is

  package Log is new Traces ("Lx200");

  Command_Start : constant Character := ':';

  Has_Ultra_Precision : Boolean := False;


  function Command_For (Item : String) return String is
  begin
    return Command_Start & Item & Terminator;
  end Command_For;


  function String_Of (Item      : Extended_Command;
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
      return Command_For ("RC" & Parameter);
    when Set_Guiding_Rate =>
      return Command_For ("RG" & Parameter);
    when Set_Finding_Rate =>
      return Command_For ("RM");
    when Set_Slewing_Rate =>
      return Command_For ("RS" & Parameter);

    -- extended commands

    when Get_Status =>
      return Command_For ("Gstat");
    when Get_Transit_Status =>
      return Command_For ("TLESCK");
    when Get_Axis_RA_Position =>
      return Command_For ("GaXa");
    when Get_Axis_Dec_Position =>
      return Command_For ("GaXb");
    when Set_Axis_RA_Position =>
      return Command_For ("SaXa" & Parameter);
    when Set_Axis_Dec_Position =>
      return Command_For ("SaXb" & Parameter);
    when Get_Number_Of_Alignment_Stars =>
      return Command_For ("getalst");
    when Get_Alignment_Information =>
      return Command_For ("getain");
    when Get_Pointing_State =>
      return Command_For ("pS");
    when New_Alignment_Start =>
      return Command_For ("newalig");
    when New_Alignment_Point =>
      return Command_For ("newalpt" & Parameter);
     when New_Alignment_End =>
      return Command_For ("endalig");
    when Get_Air_Pressure =>
      return Command_For ("GRPRS");
    when Get_Temperature =>
      return Command_For ("GRTMP");
    when Get_Julian_Date =>
      return Command_For ("GJD1");
    when Set_Air_Pressure =>
      return Command_For ("SRPRS" & Parameter);
    when Set_Temperature =>
      return Command_For ("SRTMP" & Parameter);
    when Set_Julian_Date =>
      return Command_For ("SJD" & Parameter);
    when Set_Centering_Rate_Factor =>
      return Command_For ("Rc" & Parameter);
    when Set_Slewing_Rate_Factor =>
      return Command_For ("Rs" & Parameter);
    when Slew_To_Axis_Position =>
      return Command_For ("MaX");
    when Slew_To_Park_Position =>
      return Command_For ("KA");
    when Set_Ultra_Precision_Mode =>
      Has_Ultra_Precision := True;
      return Command_For ("U2");
    when Stop =>
      return Command_For ("STOP");
    when Tle_Load_Satellite =>
      return Command_For ("TLEL0" & Parameter);
    when Tle_Precalculate =>
      return Command_For ("TLEP" & Parameter);
    when Tle_Slew =>
      return Command_For ("TLES");
    when Unpark =>
      return Command_For ("PO");
    end case;
  end String_Of;


  function Angle_Image_Of (Data       : String;
                           U1, U2, U3 : Character) return String is
  begin
    declare
      -- Extended     6543210
      -- format: "s*ddUddUddU
      -- Ultra        876543210
      -- format: "sdddUddUdd.dU
      -- Ultra       9876543210
      -- format: "sddUddUdd.ddU
      U1_Offset : constant Natural := (if Has_Ultra_Precision then (if Data(Data'last - 1) = '.' then 8 else 9) else 6);
      U2_Offset : constant Natural := U1_Offset - 3;
      The_Image : String := Data & U3;
    begin
      The_Image(The_Image'last - U1_Offset) := U1;
      The_Image(The_Image'last - U2_Offset) := U2;
      return The_Image;
    end;
  exception
  when others =>
    Log.Error ("Angle_Image_Of failed with " & Data);
    raise Protocol_Error;
  end Angle_Image_Of;


  function Signed_Degrees_Of (Item         : Angle.Value;
                              Front_Digits : Natural := 2) return String is

    use type Angle.Units;

    Decimals : constant Angle.Decimal_Places := (if Has_Ultra_Precision then 1 else 0);

    The_Image : String := Strings.Ansi_Of (Angle.Image_Of (The_Value   => Item,
                                                           Unit        => Angle.In_Degrees,
                                                           Decimals    => Decimals,
                                                           Show_Signed => True));

    Image_Template : constant String := (if Has_Ultra_Precision then "sdDDmDD.Ds" else "sdDDmDDs");

    D_Offset : constant Natural := (if Has_Ultra_Precision then 8 else 6);
    M_Offset : constant Natural := D_Offset - 3;

    D_Replace : constant Character := (if Has_Ultra_Precision then ':' else '*');
    M_Replace : constant Character := ':';

    Digits_To_Insert : constant Natural := Image_Template'length + Front_Digits - The_Image'length;

    Leading_Zeros : constant String(1..Digits_To_Insert) := [others => '0'];

  begin -- Signed_Degrees_Of
    The_Image(The_Image'last - D_Offset) := D_Replace;
    The_Image(The_Image'last - M_Offset) := M_Replace;
    return The_Image(The_Image'first) & Leading_Zeros & The_Image(The_Image'first + 1 .. The_Image'last - 1);
  end Signed_Degrees_Of;


  function Signed_Degrees_Of (Item : String) return Angle.Value is
  begin
    return Angle.Value_Of (Angle_Image_Of (Item, 'd', ''', '"'), With_Units => Angle.In_Degrees);
  exception
  when others =>
    Log.Error ("Signed_Degrees_Of failed with " & Item);
    raise Protocol_Error;
  end Signed_Degrees_Of;


  function Hours_Of (Item : Angle.Value) return String is

    use type Angle.Units;

    Decimals : constant Angle.Decimal_Places := (if Has_Ultra_Precision then 2 else 0);

    The_Image : String := Strings.Ansi_Of (Angle.Image_Of (The_Value   => Item,
                                                           Unit        => Angle.In_Hours,
                                                           Decimals    => Decimals,
                                                           Show_Signed => False));

    Image_Template : constant String := (if Has_Ultra_Precision then "DDhDDmDD.DDs" else "DDhDDmDDs");

    H_Offset : constant Natural := (if Has_Ultra_Precision then 9 else 6);
    M_Offset : constant Natural := H_Offset - 3;

    H_Replace : constant Character := ':';
    M_Replace : constant Character := ':';

    Digits_To_Insert : constant Natural := Image_Template'length - The_Image'length;

    Leading_Zeros : constant String(1..Digits_To_Insert) := [others => '0'];

  begin -- Hours_Of
    The_Image(The_Image'last - H_Offset) := H_Replace;
    The_Image(The_Image'last - M_Offset) := M_Replace;
    return Leading_Zeros & The_Image(The_Image'first .. The_Image'last - 1);
  end Hours_Of;


  function Hours_Of (Item : String) return Angle.Value is
  begin
    return Angle.Value_Of (Angle_Image_Of (Item, 'h', 'm', 's'), With_Units => Angle.In_Hours);
  exception
  when others =>
    Log.Error ("Hours_Of failed with " & Item);
    raise Protocol_Error;
  end Hours_Of;


  function Position_Of (Item : Angle.Value) return String is
    use type Angle.Value;
    use type Angle.Degrees;
    The_Value : Angle.Degrees := +Item;
  begin
    if The_Value > 180.0 then
      The_Value := The_Value - 360.0;
    end if;
    declare
      type Degrees is delta 0.0001 range -180.0 .. 180.0;
      Image : constant String := Strings.Trimmed (Degrees'image(Degrees(abs The_Value)));
      Sign  : constant Character := (if The_Value >= 0.0 then '+' else '-');
      Zeros : constant String := "00";
    begin
      return Sign & Zeros(Zeros'first .. Zeros'first + 7 - Image'length) & Image;
    end;
  end Position_Of;


  function Position_Of (Item : String) return Angle.Value is
    use type Angle.Value;
  begin
    return +Angle.Degrees'value(Item);
  exception
  when others =>
    Log.Error ("Position_Of failed with " & Item);
    raise Protocol_Error;
  end Position_Of;


  function Air_Pressure_Of (Item : Refraction.Hectopascal) return String is
    Image : constant String := "000" & Strings.Trimmed (Item'image);
  begin
    return Image(Image'last - 5 .. Image'last);
  end Air_Pressure_Of;


  function Temperature_Of (Item : Refraction.Celsius) return String is
    use type Refraction.Celsius;
    Value : constant Refraction.Celsius := abs Item;
    Image : constant String := "00" & Strings.Trimmed (Value'image);
  begin
    return (if Item < 0.0 then '-' else '+') & Image (Image'last - 4 .. Image'last);
  end Temperature_Of;


  function Julian_Date_Of (Item : Time.JD) return String is
    Jd_Delta : constant := 10.0**(-8);
    type Julian_Date is delta Jd_Delta range 0.0 .. ((2 ** 64 - 1) * Jd_Delta) with Small => Jd_Delta, Size => 64;
  begin
    return Strings.Trimmed (Julian_Date(Item)'image);
  end Julian_Date_Of;


  function Time_Offset_Of (Item : Duration) return String is
    type Offset is delta 0.1 range -28.0 .. 28.0;
    Image : constant String := "0" & Strings.Trimmed (Offset'(abs Item / 3600.0)'image);
  begin
    return (if Item > 0.0 then '-' else '+') & Image(Image'last - 3 .. Image'last);
  end Time_Offset_Of;

end Lx200;
