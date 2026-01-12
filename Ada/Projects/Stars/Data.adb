-- *********************************************************************************************************************
-- *                       (c) 2010 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
-->Style: White_Elephant

with Ada.IO_Exceptions;
with Ada.Text_IO;
with Text;

package body Data is

  Max_Number_Of_Stars : constant := 9999;

  type Star is record
    Ra   : Coordinate.Right_Ascension;
    Dec  : Coordinate.Declination;
    Mag  : Magnitude;
    Name : Text.String;
    Show : Boolean := False;
  end record;


  The_Stars : array (Positive range 1..Max_Number_Of_Stars) of Star;
  HR_Number : Natural;

  procedure Reset is
  begin
    HR_Number := 0;
  end Reset;


  function Next return Boolean is
  begin
    while HR_Number < The_Stars'last loop
      HR_Number := HR_Number + 1;
      if The_Stars (HR_Number).Show then
        return True;
      end if;
    end loop;
    return False;
  end Next;


  function HR_Id return Natural is
  begin
    return HR_Number;
  end HR_Id;


  function HR_Image return String is
    Image     : constant String := Natural'image(HR_Number);
    The_Image : String := "0000";
  begin
    The_Image(The_Image'first + The_Image'length + 1 - Image'length .. The_Image'last)
      := Image(Image'first + 1 .. Image'last);
    return The_Image;
  end HR_Image;


  function Name return String is
  begin
    return The_Stars(HR_Number).Name.S;
  end Name;


  function Location return Coordinate.Polar is
  begin
    return (Coordinate.Location_Of (Ra  => The_Stars(HR_Number).Ra,
                                    Dec => The_Stars(HR_Number).Dec));
  end Location;


  function Point return Coordinate.Cartesian is
  begin
    return (Coordinate.Point_Of (Ra  => The_Stars(HR_Number).Ra,
                                 Dec => The_Stars(HR_Number).Dec));
  end Point;


  function Point_Of (HR : Natural) return Coordinate.Cartesian is
  begin
    return (Coordinate.Point_Of (Ra  => The_Stars(HR).Ra,
                                 Dec => The_Stars(HR).Dec));
  end Point_Of;


  function Dec_Of (HR : Natural) return Coordinate.Declination is
  begin
    return The_Stars(HR).Dec;
  end Dec_Of;


  function Mag return Magnitude is
  begin
    return The_Stars(HR_Number).Mag;
  end Mag;


  function Hr_Of (Loc : Coordinate.Polar) return Natural is
  begin
    for Index in The_Stars'range loop
      if The_Stars(Index).Show then
        declare
          Star_Loc : constant Coordinate.Polar := Coordinate.Location_Of (Ra  => The_Stars(Index).Ra,
                                                                          Dec => The_Stars(Index).Dec);
          use type Coordinate.Position;
        begin
          if (abs(Coordinate.X_Of (Loc) - Coordinate.X_Of (Star_Loc)) +
              abs(Coordinate.Y_Of (Loc) - Coordinate.Y_Of (Star_Loc)))
             < 0.002
          then
            return Index;
          end if;
        end;
      end if;
    end loop;
    return 0;
  end Hr_Of;


  function Image_Of (HR : Natural) return String is
  begin
    if HR > 0 then
      return Text.Trimmed (Natural'image(HR));
    end if;
    return "";
  end Image_Of;


  function Name_Of (HR : Natural) return String is
  begin
    if HR > 0 then
      return The_Stars (HR).Name.S;
    end if;
    return "";
  end Name_Of;


  function Name_Of (Loc : Coordinate.Polar) return String is
  begin
    return Name_Of (Hr_Of (Loc));
  end Name_Of;


  procedure Read is

    procedure Store (Line : String) is

      The_Index : Natural := Line'first;

      function Next return String is
        First    : constant Natural := The_Index;
        The_Last : Natural := 0;
      begin
        while The_Index <= Line'last loop
          if Line(The_Index) = ',' then
            The_Index := The_Index + 1;
            exit;
          end if;
          The_Last := The_Index;
          The_Index := The_Index + 1;
        end loop;
        return Text.Trimmed(Line(First .. The_Last));
      end Next;

      The_Star : Star;

    begin -- Store
      if Text.Trimmed (Line) /= "" then
        The_Star.Ra := Coordinate.Ra_Of (Next);
        The_Star.Dec := Coordinate.Dec_Of (Next);
        The_Star.Mag := Magnitude'value(Next);
        HR_Number := Natural'value(Next);
        The_Star.Name := Text.String_Of (Next);
        The_Star.Show := True;
        if HR_Number > The_Stars'last then
          Error ("HR Nummer zu gross:" & Line);
        elsif The_Stars(HR_Number).Show then
          Error ("Doppelt definiert: " & Line);
        else
          The_Stars(HR_Number) := The_Star;
        end if;
      end if;
    end Store;

    File       : Ada.Text_IO.File_Type;
    The_Line   : String (1..256);
    The_Length : Natural;

  begin -- Read
    Ada.Text_IO.Open (File, Ada.Text_IO.In_File, "Stars.csv");
    for Index in The_Stars'range loop
      The_Stars(Index).Show := False;
    end loop;
    while not Ada.Text_IO.End_Of_File (File) loop
      Ada.Text_IO.Get_Line (File, The_Line, The_Length);
      Store (The_Line(1..The_Length));
    end loop;
    Ada.Text_IO.Close (File);
    Reset;
  exception
  when Ada.IO_Exceptions.Name_Error =>
    Error ("Stars.csv not Found");
  end Read;

end Data;
