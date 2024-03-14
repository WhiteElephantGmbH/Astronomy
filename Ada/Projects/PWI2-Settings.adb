-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Numerics;
with Ada.Text_IO;
with Angle;
with Site;
with Text;

package body PWI2.Settings is

  package IO renames Ada.Text_IO;

  Longitude_Id : constant String := "longitudeDegrees";
  Latitude_Id  : constant String := "latitudeDegrees";
  Elevation_Id : constant String := "elevationMeters";

  Undefined_Degrees : constant Angle.Degrees := Angle.Degrees'last;
  Undefined_Meters  : constant Integer := Integer'last;

  The_Longitude : Angle.Degrees := Undefined_Degrees;
  The_Latitude  : Angle.Degrees := Undefined_Degrees;
  The_Elevation : Integer       := Undefined_Meters;

  Lower_Azm_Goto_Limit_Id : constant String := "azmEncCcwGotoLimitRad";
  Upper_Azm_Goto_Limit_Id : constant String := "azmEncCwGotoLimitRad";
  Lower_Alt_Goto_Limit_Id : constant String := "altEncLowerGotoLimitRad";
  Upper_Alt_Goto_Limit_Id : constant String := "altEncUpperGotoLimitRad";

  Undefined_Limit : constant Encoder_Degrees := 0.0;

  The_Lower_Azm_Goto_Limit : Encoder_Degrees := Undefined_Limit;
  The_Upper_Azm_Goto_Limit : Encoder_Degrees := Undefined_Limit;
  The_Lower_Alt_Goto_Limit : Encoder_Degrees := Undefined_Limit;
  The_Upper_Alt_Goto_Limit : Encoder_Degrees := Undefined_Limit;


  procedure Read (Filename : String) is

    The_File : IO.File_Type;

    function Next_Token (Mark : String) return String is
      Start_Mark : constant String := '<' & Mark & '>';
      End_Mark   : constant String := "</" & Mark & '>';
    begin
      while not IO.End_Of_File (The_File) loop
        declare
          Line : constant String := Text.Trimmed (IO.Get_Line (The_File));
        begin
          if Line(Line'first .. Line'first + Start_Mark'length - 1) = Start_Mark and then
             Line(Line'last - End_Mark'length + 1 .. Line'last) = End_Mark
          then
            return Line(Line'first + Start_Mark'length .. Line'last - End_Mark'length);
          end if;
        exception
        when others =>
          null;
        end;
      end loop;
      return "";
    exception
    when others =>
      return "";
    end Next_Token;

    function Next_Limit return Encoder_Degrees is
      type Radian is delta 0.000_000_000_000_01 range -6.0 * Ada.Numerics.Pi .. 6.0 * Ada.Numerics.Pi;
    begin
      declare
        Item : constant Radian := Radian'value(Next_Token ("value"));
      begin
        return Encoder_Degrees(Long_Float(Item) * 360.0 / (Ada.Numerics.Pi * 2.0));
      end;
    exception
    when others =>
      return Undefined_Limit;
    end Next_Limit;

    use type Angle.Degrees;
    use type Angle.Value;

  begin -- Read
    begin
      IO.Open (File => The_File,
               Mode => IO.In_File,
               Name => Filename);
    exception
    when others =>
      raise File_Not_Found;
    end;
    while not IO.End_Of_File (The_File) loop
      declare
        Item : constant String := Next_Token ("title");
      begin
        if Item = Longitude_Id then
          The_Longitude := Angle.Degrees'value(Next_Token ("value"));
        elsif Item = Latitude_Id then
          The_Latitude := Angle.Degrees'value(Next_Token ("value"));
        elsif Item = Elevation_Id then
          The_Elevation := Integer'value(Next_Token ("value"));
        elsif Item = Lower_Azm_Goto_Limit_Id then
          The_Lower_Azm_Goto_Limit := Next_Limit;
        elsif Item = Upper_Azm_Goto_Limit_Id then
          The_Upper_Azm_Goto_Limit := Next_Limit;
        elsif Item = Lower_Alt_Goto_Limit_Id then
          The_Lower_Alt_Goto_Limit := Next_Limit;
        elsif Item = Upper_Alt_Goto_Limit_Id then
          The_Upper_Alt_Goto_Limit := Next_Limit;
        end if;
      end;
    end loop;
    IO.Close (The_File);
    if The_Longitude = Undefined_Degrees then
      raise Missing_Longitude;
    elsif The_Latitude = Undefined_Degrees then
      raise Missing_Latitude;
    elsif The_Elevation = Undefined_Meters then
      raise Missing_Elevation;
    elsif The_Lower_Azm_Goto_Limit = Undefined_Limit then
      raise Missing_Lower_Azm_Goto_Limit;
    elsif The_Upper_Azm_Goto_Limit = Undefined_Limit then
      raise Missing_Upper_Azm_Goto_Limit;
    elsif The_Lower_Alt_Goto_Limit = Undefined_Limit then
      raise Missing_Lower_Alt_Goto_Limit;
    elsif The_Upper_Alt_Goto_Limit = Undefined_Limit then
      raise Missing_Upper_Alt_Goto_Limit;
    end if;
    Site.Define ((Latitude  => +The_Latitude,
                  Longitude => +The_Longitude,
                  Elevation => The_Elevation));
  end Read;


  function Lower_Azm_Goto_Limit return Encoder_Degrees is
  begin
    return The_Lower_Azm_Goto_Limit;
  end Lower_Azm_Goto_Limit;


  function Upper_Azm_Goto_Limit return Encoder_Degrees is
  begin
    return The_Upper_Azm_Goto_Limit;
  end Upper_Azm_Goto_Limit;


  function Lower_Alt_Goto_Limit return Encoder_Degrees is
  begin
    return The_Lower_Alt_Goto_Limit;
  end Lower_Alt_Goto_Limit;


  function Upper_Alt_Goto_Limit return Encoder_Degrees is
  begin
    return The_Upper_Alt_Goto_Limit;
  end Upper_Alt_Goto_Limit;

end PWI2.Settings;
