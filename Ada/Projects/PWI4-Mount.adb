-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with PWI4.Protocol;
with Strings;

package body PWI4.Mount is

  procedure Execute (Command_Name : String;
                     Parameters   : String := "") is
  begin
    Execute (Device     => "mount",
             Command    => Command_Name,
             Parameters => Parameters);
  end Execute;


  Is_Homing : Boolean := False;
  Is_Homed  : Boolean := False;

  Is_Leaving : Boolean := False;

  Last_Axis0_Position : Degrees;
  Last_Axis1_Position : Degrees;

  function Info return Information is

    Data  : constant Protocol.Mount_Info := Protocol.Mount.Info;
    Flags : Protocol.Mount_Flag renames Data.Flags;


    function Status return State is
      Delta_Axis0 : Degrees;
      Delta_Axis1 : Degrees;
    begin
      if not Flags.Is_Connected then
        Is_Homing := False;
        Is_Homed := False;
        Is_Leaving := False;
        Last_Axis0_Position := Degrees'last;
        Last_Axis1_Position := Degrees'last;
        return Disconnected;
      elsif not (Flags.Axis1_Is_Enabled and Flags.Axis1_Is_Enabled) then
        Last_Axis0_Position := Degrees'last;
        Last_Axis1_Position := Degrees'last;
        Is_Homing := False;
        Is_Homed := False;
        Is_Leaving := False;
        return Connected;
      elsif not Is_Homed then
        if Is_Homing then
          Delta_Axis0 := Data.Axis0.Position - Last_Axis0_Position;
          Delta_Axis1 := Data.Axis1.Position - Last_Axis1_Position;
          if abs(Delta_Axis0) < 0.001 and abs(Delta_Axis1) < 0.001 then
            Is_Homed := True;
            Is_Homing := False;
            return Stopped;
          end if;
          Last_Axis0_Position := Data.Axis0.Position;
          Last_Axis1_Position := Data.Axis1.Position;
        end if;
        return Enabled;
      elsif not (Flags.Is_Tracking or Flags.Is_Slewing) then
        if Is_Leaving then
          return Approaching;
        else
          return Stopped;
        end if;
      elsif Flags.Is_Slewing then
        return Approaching;
      elsif Is_Leaving then
        return Approaching;
      else
        return Tracking;
      end if;
    end Status;

  begin
    return (Status    => Status,
            Ra        => Data.Ra,
            Dec       => Data.Dec,
            Ra_J2000  => Data.Ra_J2000,
            Dec_J2000 => Data.Dec_J2000,
            Az        => Data.Azimuth,
            Alt       => Data.Altitude,
            Az_Axis   => Data.Axis0,
            Alt_Axis  => Data.Axis1);
  end Info;


  procedure Connect is
  begin
    Execute ("connect");
  end Connect;


  procedure Disconnect is
  begin
    Execute ("disconnect");
  end Disconnect;


  procedure Enable is
  begin
    Execute (Command_Name => "enable",
             Parameters   => "axis=0");
    Execute (Command_Name => "enable",
             Parameters   => "axis=1");
  end Enable;


  procedure Disable is
  begin
    Execute (Command_Name => "disable",
             Parameters   => "axis=0");
    Execute (Command_Name => "disable",
             Parameters   => "axis=1");
  end Disable;


  procedure Find_Home is
  begin
    Execute ("find_home");
    Is_Homing := True;
  end Find_Home;


  procedure Goto_Ra_Dec (With_Ra    : Hours;
                         With_Dec   : Degrees;
                         From_J2000 : Boolean := False) is

    Ra_Image  : constant String := Image_Of (With_Ra);
    Dec_Image : constant String := Image_Of (With_Dec);

  begin
    Is_Leaving := True;
    if From_J2000 then
      Execute (Command_Name => "goto_ra_dec_j2000",
               Parameters   => "ra_hours" & "=" & Ra_Image & "&dec_degs" & "=" & Dec_Image);
    else
      Execute (Command_Name => "goto_ra_dec_apparent",
               Parameters   => "ra_hours" & "=" & Ra_Image & "&dec_degs" & "=" & Dec_Image);
    end if;
  end Goto_Ra_Dec;


  procedure Goto_Alt_Az (Alt : Degrees;
                         Az  : Degrees) is

    Alt_Image : constant String := Image_Of (Alt);
    Az_Image  : constant String := Image_Of (Az);

  begin
    Is_Leaving := True;
    Execute (Command_Name => "goto_alt_az",
             Parameters   => "alt_degs=" & Alt_Image & "&az_degs=" & Az_Image);
  end Goto_Alt_Az;


  function Command_For (Axis : Offset_Axis;
                        Command : Offset_Command) return String is
  begin
    return Strings.Lowercase_Of (Axis'image) & "_" & Strings.Lowercase_Of (Command'image);
  end Command_For;


  procedure Set_Offset (Axis    : Offset_Axis;
                        Command : Offset_Command;
                        Item    : Arc_Second) is

    Item_Image : constant String := Image_Of (Item);
    Parameter  : constant String := Command_For (Axis, Command) & "=" & Item_Image;

  begin
    Execute (Command_Name => "offset",
             Parameters   => Parameter);
  end Set_Offset;


  procedure Stop_Rates is
    Stop_Ra_Rate  : constant String := Command_For (Ra, Stop_Rate) & "=0";
    Stop_Dec_Rate : constant String := Command_For (Dec, Stop_Rate) & "=0";
  begin
    Execute (Command_Name => "offset",
             Parameters   => Stop_Ra_Rate & "&" & Stop_Dec_Rate);
  end Stop_Rates;


  procedure Set_Gradual_Offsets (Delta_Ra  : Arc_Second;
                                 Delta_Dec : Arc_Second) is

    function Parameter_For (Axis : Offset_Axis;
                            Item : Arc_Second) return String is

      Over_A_Second : constant String := Command_For (Axis, Gradual_Offset_Seconds) & "=1";

    begin
      return Command_For (Axis, Add_Gradual_Offset_Arcsec) & "=" & Image_Of (Item) & "&" & Over_A_Second;
    end Parameter_For;

  begin
    Execute (Command_Name => "offset",
             Parameters   => Parameter_For (Ra, Delta_Ra) & "&" & Parameter_For (Dec, Delta_Dec));
  end Set_Gradual_Offsets;


  procedure Confirm_Goto is
  begin
    Is_Leaving := False;
  end Confirm_Goto;


  procedure Stop is
  begin
    Is_Leaving := False;
    Execute ("stop");
  end Stop;

end PWI4.Mount;
