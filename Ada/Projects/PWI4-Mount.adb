-- *********************************************************************************************************************
-- *                       (c) 2023 .. 2025 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with PWI4.Protocol;
with Text;

package body PWI4.Mount is

  The_Enable_Delay_Count : Natural := 0;

  procedure Set_Powerup (Enable_Delay : Duration) is
  begin
    The_Enable_Delay_Count := Request_Rate * Natural(Enable_Delay);
  end Set_Powerup;


  procedure Execute (Command_Name : String;
                     Parameters   : Parameter := "") is
  begin
    Execute (Device     => "mount",
             Command    => Command_Name,
             Parameters => Parameters);
  end Execute;


  Is_Homing : Boolean := False;
  Is_Homed  : Boolean := False;

  Is_Leaving : Boolean := False;

  Homing_Check_Count : constant Natural := 3;
  Homing_Counter     : Natural := Homing_Check_Count;

  Last_Axis0_Position : Degrees := 0.0;
  Last_Axis1_Position : Degrees := 0.0;

  Spiral_Offsets : PWI4.Protocol.Spiral_Data;


  procedure Initialize_Homing is
  begin
    Homing_Counter := Homing_Check_Count;
    Is_Homing := False;
    Is_Homed := False;
    Is_Leaving := False;
    Last_Axis0_Position := 0.0;
    Last_Axis1_Position := 0.0;
  end Initialize_Homing;


  function Info return Information is

    Data  : constant Protocol.Mount_Info := Protocol.Mount.Info;
    Flags : Protocol.Mount_Flag renames Data.Flags;

    function Status return State is
      Delta_Axis0 : Degrees;
      Delta_Axis1 : Degrees;
    begin
      if not Flags.Is_Connected or Data.Count = 0 then
        Initialize_Homing;
        if Flags.Has_Error then
          return Error;
        end if;
        return Disconnected;
      elsif not (Flags.Axis_Is_Enabled(0) and Flags.Axis_Is_Enabled(1)) then
        Initialize_Homing;
        return Connected;
      elsif not Is_Homed then
        if Is_Homing then
          Delta_Axis0 := Data.Axis(0).Position - Last_Axis0_Position;
          Delta_Axis1 := Data.Axis(1).Position - Last_Axis1_Position;
          if abs(Delta_Axis0) < 0.0002 and abs(Delta_Axis1) < 0.0002 then
            Homing_Counter := @ - 1;
            if Homing_Counter = 0 then
              Homing_Counter := Homing_Check_Count;
              Is_Homed := True;
              Is_Homing := False;
              return Stopped;
            end if;
          else
            Homing_Counter := Homing_Check_Count;
          end if;
        end if;
        Last_Axis0_Position := Data.Axis(0).Position;
        Last_Axis1_Position := Data.Axis(1).Position;
        if The_Enable_Delay_Count > 0 then
          The_Enable_Delay_Count := @ - 1;
          return Connected;
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

  begin -- Info
    Spiral_Offsets := Data.Spiral_Offsets;
    return (Status    => Status,
            Ra        => Data.Ra,
            Dec       => Data.Dec,
            Ra_J2000  => Data.Ra_J2000,
            Dec_J2000 => Data.Dec_J2000,
            Az        => Data.Azimuth,
            Alt       => Data.Altitude,
            Az_Axis   => Data.Axis(0),
            Alt_Axis  => Data.Axis(1),
            Wrap_Min  => Data.Wrap_Range_Min,
            Model     => Data.Model);
  end Info;


  function Is_Updating return Boolean is
  begin
    return Protocol.Mount.Info.Count > 0;
  end Is_Updating;


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
               Parameters   => "ra_hours" / Ra_Image + "dec_degs" / Dec_Image);
    else
      Execute (Command_Name => "goto_ra_dec_apparent",
               Parameters   => "ra_hours" / Ra_Image + "dec_degs" / Dec_Image);
    end if;
  end Goto_Ra_Dec;


  procedure Goto_Alt_Az (Alt : Degrees;
                         Az  : Degrees) is

    Alt_Image : constant String := Image_Of (Alt);
    Az_Image  : constant String := Image_Of (Az);

  begin
    Is_Leaving := True;
    Execute (Command_Name => "goto_alt_az",
             Parameters   => "alt_degs" / Alt_Image + "az_degs" / Az_Image);
  end Goto_Alt_Az;


  function Command_For (Axis    : Offset_Axis;
                        Command : Offset_Command) return String is
  begin
    return Text.Lowercase_Of (Axis'image) & "_" & Text.Lowercase_Of (Command'image);
  end Command_For;


  procedure Set_Offset (Axis    : Offset_Axis;
                        Command : Offset_Command;
                        Item    : Arc_Second) is
    Offset_Parameter : constant Parameter := Command_For (Axis, Command) / Image_Of (Item);
  begin
    Execute (Command_Name => "offset",
             Parameters   => Offset_Parameter);
  end Set_Offset;


  procedure Stop_Rates is
    Stop_Axis0_Rate      : constant Parameter := Command_For (Axis0, Stop_Rate) / "0";
    Stop_Axis1_Rate      : constant Parameter := Command_For (Axis1, Stop_Rate) / "0";
    Stop_Dec_Rate        : constant Parameter := Command_For (Dec, Stop_Rate) / "0";
    Stop_Ra_Rate         : constant Parameter := Command_For (Ra, Stop_Rate) / "0";
    Stop_Path_Rate       : constant Parameter := Command_For (Path, Stop_Rate) / "0";
    Stop_Transverse_Rate : constant Parameter := Command_For (Transverse, Stop_Rate) / "0";
  begin
    Execute (Command_Name => "offset",
             Parameters   => Stop_Axis0_Rate + Stop_Axis1_Rate +
                             Stop_Dec_Rate + Stop_Ra_Rate +
                             Stop_Path_Rate + Stop_Transverse_Rate);
  end Stop_Rates;


  procedure Set_Moving (Alt_Speed : Arc_Second;
                        Az_Speed  : Arc_Second) is
    Move_Axis0 : constant Parameter := Command_For (Axis0, Set_Rate_Arcsec_Per_Sec) / Image_Of (Az_Speed);
    Move_Axis1 : constant Parameter := Command_For (Axis1, Set_Rate_Arcsec_Per_Sec) / Image_Of (Alt_Speed);
  begin
    Execute (Command_Name => "tracking_on");
    Execute (Command_Name => "offset",
             Parameters   => Move_Axis0 + Move_Axis1);
  end Set_Moving;


  procedure Spiral_Offset_Center is
    X_Step : constant String := Image_Of (Spiral_Offsets.X_Step);
    Y_Step : constant String := Image_Of (Spiral_Offsets.Y_Step);
  begin
    Execute (Command_Name => "spiral_offset/new",
             Parameters   => "x_step_arcsec" / X_Step + "y_step_arcsec" / Y_Step);
  end Spiral_Offset_Center;


  procedure Spiral_Offset_Next is
  begin
    Execute (Command_Name => "spiral_offset/next");
  end Spiral_Offset_Next;


  procedure Spiral_Offset_Previous is
  begin
    Execute (Command_Name => "spiral_offset/previous");
  end Spiral_Offset_Previous;


  procedure Reset_Moving_Target is
    Path_Reset       : constant Parameter := Command_For (Path, Reset) / "0";
    Transverse_Reset : constant Parameter := Command_For (Transverse, Reset) / "0";
  begin
    Execute (Command_Name => "offset",
             Parameters   => Path_Reset + Transverse_Reset);
  end Reset_Moving_Target;


  procedure Set_Gradual_Offsets (Delta_Ra  : Arc_Second;
                                 Delta_Dec : Arc_Second) is

    function Parameter_For (Axis : Offset_Axis;
                            Item : Arc_Second) return Parameter is

      Over_A_Second : constant Parameter := Command_For (Axis, Gradual_Offset_Seconds) / "1";

    begin
      return Command_For (Axis, Add_Gradual_Offset_Arcsec) / Image_Of (Item) + Over_A_Second;
    end Parameter_For;

  begin
    Execute (Command_Name => "offset",
             Parameters   => Parameter_For (Ra, Delta_Ra) + Parameter_For (Dec, Delta_Dec));
  end Set_Gradual_Offsets;


  procedure Set_Axis0_Wrap (Range_Min : Degrees) is
    Minimum_Image : constant String := Image_Of (Range_Min);
  begin
    Execute (Command_Name => "set_axis0_wrap_range_min",
             Parameters   => "degs" / Minimum_Image);
  end Set_Axis0_Wrap;


  procedure Add_Point (Ra_J2000  : Hours;
                       Dec_J2000 : Degrees) is

    Ra_Image  : constant String := Image_Of (Ra_J2000);
    Dec_Image : constant String := Image_Of (Dec_J2000);

  begin
    Execute (Command_Name => "model/add_point",
             Parameters   => "ra_j2000_hours" / Ra_Image + "dec_j2000_degs" / Dec_Image);
  end Add_Point;


  procedure Confirm_Goto is
  begin
    Is_Leaving := False;
  end Confirm_Goto;


  procedure Follow_Tle (Line_1 : String;
                        Line_2 : String;
                        Line_3 : String) is
  begin
    Execute (Command_Name => "follow_tle",
             Parameters   => "line1" / Line_1 + "line2" / Line_2 + "line3" / Line_3);
  end Follow_Tle;


  procedure Stop is
  begin
    Is_Leaving := False;
    Execute ("stop");
  end Stop;

end PWI4.Mount;
