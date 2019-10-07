-- *********************************************************************************************************************
-- *                           (c) 2019 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Numerics;
with PWI.XML;
with Strings;

package body PWI.Mount is

  procedure Execute (Command_Name : String;
                     Parameters   : String := "") is
  begin
    Execute (Device     => "mount",
             Command    => Command_Name,
             Parameters => Parameters);
  end Execute;


  Is_Simulation : Boolean := False;

  function Status_Of (Flags : XML.Mount_Flag) return State is
  begin
    if Is_Simulation then
      if Flags.On_Target then
        return Tracking;
      elsif Flags.Tracking then
        return Approaching;
      end if;
      return Disconnected;
    elsif not Flags.Connected then
      return Disconnected;
    elsif not (Flags.Azm_Enabled and Flags.Alt_Enabled) then
      return Connected;
    elsif Flags.Is_Finding_Home then
      return Homing;
    elsif not Flags.Encoders_Have_Been_Set then
      return Enabled;
    elsif not Flags.Pointing_Model_Set then
      return Synchronised;
    elsif not Flags.Tracking then
      return Stopped;
    elsif not Flags.On_Target then
      return Approaching;
    else
      return Tracking;
    end if;
  end Status_Of;


  function Value_Of (Item : XML.Right_Ascension) return Hours is

    Milli_Seconds : constant Natural
      := ((Natural(Item.Hours) * 60 + Natural(Item.Minutes)) * 60 + Natural(Item.Seconds)) * 1000
         + Natural(Item.Milli_Seconds);
  begin
    return Hours(Long_Float(Milli_Seconds) / 3600000.0);
  end Value_Of;


  function Value_Of (Item : XML.Declination) return Degrees is

    Centi_Seconds : Integer := ((Natural(Item.Degrees) * 60 + Natural(Item.Minutes)) * 60 + Natural(Item.Seconds)) * 100
                               + Natural(Item.Centi_Seconds);
  begin
    if not Item.Is_Positive then
      Centi_Seconds := - Centi_Seconds;
    end if;
    return Degrees(Long_Float(Centi_Seconds) / 360000.0);
  end Value_Of;


  function Value_Of (Item : XML.Radian) return Degrees is
  begin
    return Degrees(Long_Float(Item) * 360.0 / (Ada.Numerics.Pi * 2.0));
  end Value_Of;


  function Info return Information is
    Data : constant XML.Mount_Info := XML.Mount.Info;
  begin
    return (Status         => Status_Of (Data.Flags),
            Ra             => Value_Of (Data.Ra),
            Dec            => Value_Of (Data.Dec),
            Ra_2000        => Value_Of (Data.Ra_2000),
            Dec_2000       => Value_Of (Data.Dec_2000),
            Azm            => Value_Of (Data.Azm_Radian),
            Alt            => Value_Of (Data.Alt_Radian),
            Field_Rotation => Data.Field_Rotation,
            Azm_Encoder    => Data.Azm_Encoder,
            Alt_Encoder    => Data.Alt_Encoder);
  end Info;


  procedure Define_Pointing_Model (Filename : String) is
  begin
    XML.Mount.Define_Pointing_Model (Filename);
  end Define_Pointing_Model;


  function Defined_Pointing_Model return String is
  begin
    return XML.Mount.Defined_Pointing_Model;
  end Defined_Pointing_Model;


  procedure Set_Simulation_Mode is
  begin
    Is_Simulation := True;
  end Set_Simulation_Mode;


  function Status return State is
  begin
    return Status_Of (XML.Mount.Flags);
  end Status;


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
    Execute ("enable");
  end Enable;


  procedure Disable is
  begin
    Execute ("disable");
  end Disable;


  procedure Find_Home is
  begin
    Execute ("findhome");
  end Find_Home;


  procedure Set_Pointing_Model is
  begin
    Execute (Command_Name => "setmodel",
             Parameters   => "filename=" & PWI.Mount.Defined_Pointing_Model);
  end Set_Pointing_Model;


  procedure Move (Ra         : Hours;
                  Dec        : Degrees;
                  From_J2000 : Boolean := False) is

    Kind      : constant String := (if From_J2000 then "2000" else "");
    Ra_Image  : constant String := Strings.Trimmed (Ra'img);
    Dec_Image : constant String := Strings.Trimmed (Dec'img);

  begin
    Execute (Command_Name => "move",
             Parameters   => "ra" & Kind & "=" & Ra_Image & "&dec" & Kind & "=" & Dec_Image);
  end Move;


  procedure Move (Ra         : Hours;
                  Dec        : Degrees;
                  Ra_Rate    : Speed;
                  Dec_Rate   : Speed;
                  From_J2000 : Boolean := False) is

    Kind           : constant String := (if From_J2000 then "2000" else "");
    Ra_Image       : constant String := Strings.Trimmed (Ra'img);
    Dec_Image      : constant String := Strings.Trimmed (Dec'img);
    Ra_Rate_Image  : constant String := Strings.Trimmed (Ra_Rate'img);
    Dec_Rate_Image : constant String := Strings.Trimmed (Dec_Rate'img);

  begin
    Execute (Command_Name => "move",
             Parameters   => "ra" & Kind & "=" & Ra_Image & "&dec" & Kind & "=" & Dec_Image &
                             "&rarate=" & Ra_Rate_Image & "&decrate=" & Dec_Rate_Image);
  end Move;


  procedure Move (Alt : Degrees;
                  Azm : Degrees) is

    Alt_Image : constant String := Strings.Trimmed (Alt'img);
    Azm_Image : constant String := Strings.Trimmed (Azm'img);

  begin
    Execute (Command_Name => "move",
             Parameters   => "alt=" & Alt_Image & "&azm=" & Azm_Image & "&rarate=15.041&decrate=0");
  end Move;


  procedure Jog (Alt_Rate : Axis_Rate;
                 Azm_Rate : Axis_Rate) is

    Alt_Image : constant String := Strings.Trimmed (Alt_Rate'img);
    Azm_Image : constant String := Strings.Trimmed (Azm_Rate'img);

  begin
    Execute (Command_Name => "jog",
             Parameters   => "axis1rate=" & Azm_Image & "&axis2rate=" & Alt_Image);
  end Jog;


  procedure Stop is
  begin
    Execute ("stop");
  end Stop;

end PWI.Mount;
