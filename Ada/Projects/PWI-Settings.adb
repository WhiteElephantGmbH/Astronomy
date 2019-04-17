-- *********************************************************************************************************************
-- *                           (c) 2019 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Text_IO;
with Strings;
with Traces;

package body PWI.Settings is

  package Log is new Traces ("Settings");

  package IO renames Ada.Text_IO;

  Longitude_Id : constant String := "longitudeDegrees";
  Latitude_Id  : constant String := "latitudeDegrees";
  Elevation_Id : constant String := "elevationMeters";


  Undefined_Degrees : constant Angle.Degrees := Angle.Degrees'last;
  Undefined_Meters  : constant Integer := Integer'last;

  The_Longitude : Angle.Degrees := Undefined_Degrees;
  The_Latitude  : Angle.Degrees := Undefined_Degrees;
  The_Elevation : Integer       := Undefined_Meters;


  procedure Read (Filename : String) is

    The_File : IO.File_Type;

    function Next_Token (Mark : String) return String is
      Start_Mark : constant String := '<' & Mark & '>';
      End_Mark   : constant String := "</" & Mark & '>';
    begin
      while not IO.End_Of_File (The_File) loop
        declare
          Line : constant String := Strings.Trimmed (IO.Get_Line (The_File));
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

    use type Angle.Degrees;

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
          Log.Write ("Longitude" & The_Longitude'img);
        elsif Item = Latitude_Id then
          The_Latitude := Angle.Degrees'value(Next_Token ("value"));
          Log.Write ("Latitude" & The_Latitude'img);
        elsif Item = Elevation_Id then
          The_Elevation := Integer'value(Next_Token ("value"));
          Log.Write ("Elevation" & The_Elevation'img);
          exit;
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
    end if;
  end Read;


  function Latitude return Angle.Value is
    use type Angle.Value;
  begin
    return +The_Latitude;
  end Latitude;


  function Longitude return Angle.Value is
    use type Angle.Value;
  begin
    return +The_Longitude;
  end Longitude;


  function Elevation return Integer is
  begin
    return The_Elevation;
  end Elevation;

end PWI.Settings;
