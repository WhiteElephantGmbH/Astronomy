-- *********************************************************************************************************************
-- *                       (c) 2023 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Command_Line;
with Ada.Text_IO;
with Angle;
with Astap;
with Sky.Catalog;
with File;
with Os.System;
with Text;
with Traces;

package body Nebulae is

  package Log is new Traces ("Request");

  package IO   renames Ada.Text_IO;
  package Cmd  renames Ada.Command_Line;


  procedure Error (Item : String) is
  begin
    IO.Put_Line ("<<< " & Item & " >>>");
  end Error;


  procedure Evaluate (Name           : String;
                      Picture_Height : Angle.Degrees) is

    File_Extension : constant String := "png";

    function Object_Name_Of (Item : String) return String is
    begin
      declare
        Extension : constant String := File.Extension_Of (Item);
        Base_Name : constant String := File.Base_Name_Of (Item);
      begin
        if Extension in File_Extension | "" then
          return Base_Name;
        end if;
        return "";
      end;
    exception
    when others =>
      return "";
    end Object_Name_Of;

    Object_Name : constant String := Object_Name_Of (Name);
    Filename    : constant String := Object_Name & '.' & File_Extension;

    procedure Write_Textures (C00, C01, C10, C11 : Astap.Location) is

      function "+" (Item : String) return String is
      begin
        return '"' & Item & '"';
      end "+";


      function "+" (Left, Right : String) return String is
      begin
        return Left & ", " & Right;
      end "+";


      function Field_Of (Item    : String;
                         Image   : String;
                         Postfix : String := "") return String is
      begin
        return + Item & " : " & Image & Postfix;
      end Field_Of;


      function Image_Credits return String is
      begin
        return '{' & Field_Of ("short", + "Urs Maurer") & '}';
      end Image_Credits;


      function World_Coords return String is

        function Corners return String is

          function Corner (Corner : Astap.Location) return String is

            function Image_Of (Item : Angle.Degrees) return String is
              type Degrees is delta 10.0**(-5) range -360.0 .. 360.0;
            begin
              return Text.Trimmed (Degrees(Item)'image);
            end Image_Of;

          begin -- Corner
            return '[' & Image_Of (Corner(1)) + Image_Of (Corner(2)) & ']';
          end Corner;

        begin -- Corners
          return '[' & Corner (C00) + Corner (C10) + Corner (C11) + Corner (C01) & ']';
        end Corners;

      begin -- World_Coords
        return '[' & Corners & ']';
      end World_Coords;


      Output : IO.File_Type;

      Tab : constant Character := Ascii.Ht;

      procedure Put_Line (Item : String) is
      begin
        IO.Put_Line (Output, Tab & Item);
      end Put_Line;

      procedure Put_Field (Item    : String;
                           Image   : String;
                           Postfix : String := ",") is
      begin
        Put_Line (Tab & Field_Of (Item, Image, Postfix));
      end Put_Field;

    begin -- Write_Textures;
      IO.Create (Output, Name => Object_Name & ".json");
      Put_Line ("{");
      Put_Field ("imageCredits", Image_Credits);
      Put_Field ("imageUrl", + Text.Lowercase_Of (Filename));
      Put_Field ("worldCoords", World_Coords);
      Put_Field ("textureCoords", "[[[0,0], [1,0], [1,1], [0,1]]]");
      Put_Field ("minResolution", "1.0");
      Put_Field ("maxBrightness", "14.0", Postfix => "");
      Put_Line ("},");
    end Write_Textures;

    CRVAL : Astap.Location;
    CD    : Astap.Matrix;
    Size  : Astap.Pixels;
    Ra    : constant := Astap.Ra;
    Dec   : constant := Astap.Dec;
    Id    : constant Sky.Object := Sky.Catalog.Object_Of (Object_Name);

    use type Angle.Degrees;
    use type Sky.Object;

  begin -- Evaluate
    if Object_Name = "" then
      Error ("File " & Name & " is not a *.png file");
      return;
    elsif Id = Sky.Undefined then
      Error ("Object " & Object_Name & " unknown");
      return;
    end if;
    Astap.Define (Os.System.Program_Files_Folder & "astap\astap.exe");
    Astap.Solve (Filename => Filename,
                 Height   => Picture_Height,
                 Start    => [Sky.Catalog.Ra_J2000_Of(Id), Sky.Catalog.Dec_J2000_Of(Id)]);
    for Unused in 1 .. 10 loop
      delay 1.0;
      if Astap.Solved (CRVAL, CD, Size) then
        IO.Put_Line ("Solved RA =" & CRVAL(Ra)'image & " - DEC =" & CRVAL(Dec)'image);
        IO.Put_Line ("  Size RA =" & Size(Ra)'image & " - DEC =" & Size(Dec)'image);
        declare
          use type Astap.Location;
          D_1 : constant Angle.Degrees := Angle.Degrees(Size(Ra) / 2);
          D_2 : constant Angle.Degrees := Angle.Degrees(Size(Dec) / 2);
        begin
          Write_Textures (C00 => [-D_1, -D_2] * CD + CRVAL,
                          C01 => [-D_1, +D_2] * CD + CRVAL,
                          C10 => [+D_1, -D_2] * CD + CRVAL,
                          C11 => [+D_1, +D_2] * CD + CRVAL);
        end;
        return;
      end if;
    end loop;
    IO.Put_Line ("Not Solved (Timeout)");
    Astap.Stop;
  exception
  when Astap.Not_Solved =>
    IO.Put_Line ("Not Solved");
  end Evaluate;


  procedure Solve is
    Argument_Count : constant Natural := Cmd.Argument_Count;
    Picture_Height : Angle.Degrees := 1.0; -- default
  begin
    if Argument_Count = 0 then
      Error ("Filename parameter missing");
    elsif Argument_Count > 2 then
      Error ("Too many parameters");
    else
      if Argument_Count = 2 then
        begin
          Picture_Height := Angle.Degrees'value(Cmd.Argument(2));
        exception
        when others =>
          Error ("Invalid picture height");
          return;
        end;
      end if;
      Evaluate (Cmd.Argument(1), Picture_Height);
    end if;
  exception
  when Occurrence: others =>
    IO.Put_Line ("Failed");
    Log.Termination (Occurrence);
  end Solve;

end Nebulae;
