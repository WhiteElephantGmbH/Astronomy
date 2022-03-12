-- *********************************************************************************************************************
-- *                           (c) 2022 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Calendar;
with Ada.Command_Line;
with Ada.Text_IO;
with Angle;
with File;
with Os.Horizon;
with Site;
with Stellarium; pragma Unreferenced (Stellarium); -- imported to define site location
with Strings;
with Text;
with Traces;

package body Request is

  package Log is new Traces ("Request");

  package IO   renames Ada.Text_IO;
  package Date renames Ada.Calendar;
  package Cmd  renames Ada.Command_Line;

  One_Day : constant Duration := Date.Day_Duration'last;

  subtype Days is Natural range 1 .. 90;

  type Minutes is range 1 .. 60;


  function Image_Of (Time : Date.Time) return String is

    Day   : constant String := Date.Day (Time)'image;
    Month : constant String := Date.Month (Time)'image;
    Year  : constant String := Strings.Trimmed (Date.Year (Time)'image);

    function Part_Of (Item : String) return String is
      Image : constant String := '0' & Strings.Trimmed (Item);
    begin
      return Image(Image'last-1 .. Image'last);
    end Part_Of;

  begin -- Image_Of
    return Year & "-" & Part_Of (Month) & "-" &  Part_Of (Day);
  end Image_Of;


  function Site_Location return String is

    Default : constant String := "8.60986388888,47.70550277777,0.54"; -- Sternwarte Schaffhausen

    function Image_Of (Item : Angle.Degrees) return String is
      type Value is delta 10.0**(-9) range -((2 ** 63 - 1) * 10.0**(-9)) .. +((2 ** 63 - 1) * 10.0**(-9));
    begin
      return Strings.Trimmed (Value(Item)'image);
    end Image_Of;

  begin -- Site_Location
    if not Site.Is_Defined then
      return Default;
    end if;
    declare
      use type Angle.Value;
      Longitude : constant Angle.Degrees := +Site.Longitude;
      Latitude  : constant Angle.Degrees := +Site.Latitude;
      Elevation : constant String        := Strings.Trimmed(Site.Elevation'image);
    begin
      return Image_Of (Longitude) & ',' & Image_Of (Latitude) & ',' & Elevation;
    end;
  end Site_Location;


  procedure Error (Item : String) is
  begin
    IO.Put_Line ("<<< " & Item & " >>>");
  end Error;


  procedure Generate (The_Days  : Days;
                      The_Steps : Minutes) is

    use type Date.Time;

    Home   : constant String := Site_Location;
    Start  : constant String := Image_Of (Date.Clock);
    Stop   : constant String := Image_Of (Date.Clock + (The_Days * One_Day));
    Steps  : constant String := Strings.Trimmed (The_Steps'image) & 'm';

    New_Target : Boolean := True;


    procedure Handle (Target : String) is

      procedure Evaluate (Data : String) is

        The_Index : Natural := Data'first;
        The_First : Natural;

        function Next_Line return String is
        begin
          while The_Index < Data'last loop
            if Data(The_Index .. The_Index + 1) = "\n" then
              The_Index := The_Index + 2;
              return Data(The_First .. The_Index - 3);
            end if;
            The_Index := The_Index + 1;
          end loop;
          The_Index := Data'last + 1;
          return Data(The_First..Data'last);
        end Next_Line;

        The_Target_Name : Text.String;

        procedure Evaluate_Target_Name (Item : String) is
          Is_Natural : Boolean := False;
        begin
          Text.Clear (The_Target_Name);
          for The_Character of Item loop
            case The_Character is
            when '0' .. '9' =>
              Is_Natural := True;
              Text.Append_To (The_Target_Name, The_Character);
            when ' ' =>
              if Is_Natural then
                Is_Natural := False;
                Text.Clear (The_Target_Name);
              else
                exit;
              end if;
            when '/' =>
              Is_Natural := False;
              Text.Append_To (The_Target_Name, '-');
            when others =>
              Is_Natural := False;
              Text.Append_To (The_Target_Name, The_Character);
            end case;
          end loop;
        end Evaluate_Target_Name;

        function Filename return String is
        begin
          return Text.String_Of (The_Target_Name) & ".sssb";
        end Filename;

        The_File : IO.File_Type;
        To_File  : Boolean := False;

      begin -- Evaluate
        if Strings.Location_Of ("Traceback", Data) = Data'first then
          Error ("No Connection to Nasa Horizon");
          return;
        end if;
        The_Index := Strings.Location_Of ("****************", Data);
        if The_Index = Strings.Not_Found then
          Error ("Unknown Answer from Nasa Horizon");
          return;
        end if;
        File.Delete (Filename);
        while The_Index <= Data'last loop
          The_First := The_Index;
          declare
            Name_Tag : constant String := "Target body name:";
            Line     : constant String := Next_Line;
          begin
            exit when Strings.Location_Of ("""}", Line) = Line'first;
            if Strings.Location_Of ("$$SOE", Line) = Line'first then
              IO.Create (The_File, Name => Filename, Mode =>IO.Out_File);
              To_File := True;
            end if;
            if To_File then
              IO.Put_Line (File => The_File,
                           Item => Line);
              exit when Strings.Location_Of ("$$EOE", Line) = Line'first;
            else
              if Strings.Location_Of (Name_Tag, Line) = Line'first then
                Evaluate_Target_Name (Strings.Trimmed (Line(Line'first + Name_Tag'length .. Line'last)));
              end if;
              IO.Put_Line (Line);
            end if;
          end;
        end loop;
        if To_File then
          IO.Close (The_File);
          IO.Put_Line ("Ephemerides generated in " & Filename);
        end if;
      end Evaluate;

    begin -- Handle
      New_Target := Target /= "";
      if not New_Target then
        return;
      end if;
      declare
        use type Os.Horizon.Arguments;
        Result : constant String := Os.Horizon.Result_Of_Get_With (Target + Home + Start + Stop + Steps );
      begin
        Evaluate (Result);
      end;
    exception
    when Occurrence: others =>
      IO.Put_Line ("Failed");
      Log.Termination (Occurrence);
    end Handle;

  begin -- Generate
    IO.Put_Line ("Start Time : " & Start);
    IO.Put_Line ("Stop  Time : " & Stop);
    IO.Put_Line ("Location   : " & Site_Location);
    IO.Put_Line ("Steps      : " & Steps);
    while New_Target loop
      IO.Put ("Target> ");
      Handle (IO.Get_Line);
    end loop;
  end Generate;


  procedure Work is
    The_Days       : Days := 28;
    The_Steps      : Minutes := 60;
    Argument_Count : constant Natural := Cmd.Argument_Count;
  begin
    if Argument_Count > 0 then
      if Argument_Count > 2 then
        Error ("Too many parameters");
        return;
      else
        begin
          The_Days := Days'value(Cmd.Argument(1));
        exception
        when others =>
          Error ("Number of days not in range 1 .. 90");
          return;
        end;
        if Argument_Count = 2 then
          begin
            The_Steps := Minutes'value(Cmd.Argument(2));
          exception
          when others =>
            Error ("Steps in minutes not in the range 1 .. 60");
            return;
          end;
        end if;
      end if;
    end if;
    Generate (The_Days, The_Steps);
  exception
  when Occurrence: others =>
    IO.Put_Line ("Failed");
    Log.Termination (Occurrence);
  end Work;

end Request;
