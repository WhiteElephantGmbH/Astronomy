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


  procedure Goto_Ra_Dec (Ra         : Hours;
                         Dec        : Degrees;
                         From_J2000 : Boolean := False) is

    Ra_Image  : constant String := Strings.Trimmed (Ra'img);
    Dec_Image : constant String := Strings.Trimmed (Dec'img);

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

    Alt_Image : constant String := Strings.Trimmed (Alt'img);
    Az_Image  : constant String := Strings.Trimmed (Az'img);

  begin
    Is_Leaving := True;
    Execute (Command_Name => "goto_alt_az",
             Parameters   => "alt_degs=" & Alt_Image & "&az_degs=" & Az_Image);
  end Goto_Alt_Az;


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
