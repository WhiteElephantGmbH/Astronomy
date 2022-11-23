-- *********************************************************************************************************************
-- *                       (c) 2015 .. 2022 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Directories;
with Ada.Real_Time;
with Ada.Text_IO;
with Alignment.Calculations;
with Application;
with Error;
with File;
with Matrix;
with Strings;
with Traces;

package body Alignment is

  package Log is new Traces ("Alignment");

  Text_Extension : constant String := "txt";

  Alignment_Directory    : constant String := Application.Composure ("Alignment");
  Alignment_Filename     : constant String := File.Composure (Alignment_Directory, "Data", Text_Extension);
  New_Alignment_Filename : constant String := File.Composure (Alignment_Directory, "New_Data", Text_Extension);

  Invers_Id : constant String := "I";
  Normal_Id : constant String := "N";


  protected Control is

    procedure Set_Actual (Direction : Earth.Direction;
                          Offset    : Earth.Direction);

    function Has_Offsets return Boolean;

    procedure Store_Correction;

    procedure Add_Correction;

    function Has_Corrections return Boolean;

    function Correction_Data_Ready return Boolean;

    procedure Apply_Corrections;

    procedure Get_Values (Annotation  :     String;
                          Alt_Actual  : out Angle.Degrees;
                          Az_Actual   : out Angle.Degrees;
                          Alt_Nominal : out Angle.Degrees;
                          Az_Nominal  : out Angle.Degrees);

    procedure Set_Cone_Error (Item : Angle.Value);

    procedure Set_Pole_Offsets (Item : Earth.Direction);

    procedure Set_Offsets (For_Ra  : Angle.Degrees;
                           For_Dec : Angle.Degrees);

    procedure Set (The_Pole_Height_Offset : Angle.Degrees;
                   The_Pole_Az_Offset     : Angle.Degrees;
                   The_Ra_Rotation        : Angle.Degrees;
                   The_Dec_Rotation       : Angle.Degrees);

    function Actual_Cone_Error return Angle.Value;

    function Actual_Pole_Offsets return Earth.Direction;

    function Three_Star_Rotations return Space.Direction;

    function Three_Star_Corrections return Space.Direction;

    function Star_Synchronized_Offsets return Offsets;

    function Number_Of_Stars_Added return Natural;

    procedure Clear_All_Corrections;

  private
    File          : Ada.Text_IO.File_Type;
    The_Direction : Earth.Direction;
    The_Offset    : Earth.Direction;

    The_Correction_Data : Matrix.Data_Lists;

    The_Alt_Actual  : Angle.Degrees;
    The_Az_Actual   : Angle.Degrees;
    The_Alt_Nominal : Angle.Degrees;
    The_Az_Nominal  : Angle.Degrees;

    The_Ra_Correction  : Angle.Degrees := 0.0;
    The_Dec_Correction : Angle.Degrees := 0.0;

    The_Cone_Error   : Angle.Value := Angle.Zero;
    The_Pole_Offsets : Earth.Direction;
    The_Rotations    : Space.Direction;
    The_Corrections  : Space.Direction;
  end Control;


  procedure Set (Direction : Earth.Direction;
                 Offset    : Earth.Direction) is
  begin
    Control.Set_Actual (Direction, Offset);
  end Set;


  Has_One_Star_Alignment : Boolean := False;

  function Has_One_Star_Offsets return Boolean is
  begin
    Has_One_Star_Alignment := Control.Has_Offsets;
    return Control.Has_Offsets;
  end Has_One_Star_Offsets;


  function Is_One_Star return Boolean is
  begin
    return Has_One_Star_Alignment;
  end Is_One_Star;


  procedure Store is
  begin
    Control.Store_Correction;
  end Store;


  procedure Add is
  begin
    Control.Add_Correction;
  end Add;


  procedure Apply is
  begin
    Control.Apply_Corrections;
  end Apply;


  function Image_Of (The_Value : Angle.Degrees) return String is
    Epsilon : constant := 360.0 / 2.0**32;
    type Degrees is delta Epsilon range -(360.0 - Epsilon) .. 360.0;
  begin
    return Degrees'image(Degrees(The_Value));
  end Image_Of;


  procedure Set (The_Cone_Error : Angle.Value) is
  begin
    Control.Set_Cone_Error (The_Cone_Error);
  end Set;


  procedure Set (The_Pole_Offsets : Earth.Direction) is
  begin
    Control.Set_Pole_Offsets (The_Pole_Offsets);
  end Set;


  procedure Set (Ra_Offset  : Angle.Degrees;
                 Dec_Offset : Angle.Degrees) is
  begin
    Control.Set_Offsets (For_Ra  => Ra_Offset,
                         For_Dec => Dec_Offset);
  end Set;


  function Cone_Error return Angle.Value is
  begin
    return Control.Actual_Cone_Error;
  end Cone_Error;


  function Pole_Offsets return Earth.Direction is
  begin
    return Control.Actual_Pole_Offsets;
  end Pole_Offsets;


  function Rotations return Space.Direction is
  begin
    return Control.Three_Star_Rotations;
  end Rotations;


  The_System_Error : Angle.Degrees := 0.0;

  function System_Error return Angle.Value is
    use type Angle.Value;
  begin
    return +The_System_Error;
  end System_Error;


  function Corrections return Space.Direction is
  begin
    return Control.Three_Star_Corrections;
  end Corrections;


  function Synchronized_Offsets return Offsets is
    The_Offsets : constant Offsets := Control.Star_Synchronized_Offsets;
  begin
    Control.Clear_All_Corrections;
    Log.Write ("Synchronized_Offsets Ra:" & The_Offsets(Ra)'image & " - Dec:" & The_Offsets(Dec)'image);
    return The_Offsets;
  end Synchronized_Offsets;


  function Has_Correction return Boolean is
  begin
    return Control.Has_Corrections;
  end Has_Correction;


  function Has_Correction_Data return Boolean is
  begin
    return Control.Correction_Data_Ready;
  end Has_Correction_Data;


  function Number_Of_Aligned_Stars return Natural is
  begin
    return Control.Number_Of_Stars_Added;
  end Number_Of_Aligned_Stars;


  procedure Clear_Corrections is
  begin
    Control.Clear_All_Corrections;
  end Clear_Corrections;


  procedure Calculate (The_Data_Lists : in out Matrix.Data_Lists) is
    Start_Time   : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
    The_Duration : Duration;
    use type Ada.Real_Time.Time;
  begin
    Matrix.Calculate (The_Data_Lists);
    The_Duration := Ada.Real_Time.To_Duration (Ada.Real_Time.Clock - Start_Time);
    Log.Write ("calculation time =>" & The_Duration'img & " seconds");
  end Calculate;


  protected body Control is

    procedure Set_Actual (Direction : Earth.Direction;
                          Offset    : Earth.Direction) is
    begin
      The_Direction := Direction;
      The_Offset := Offset;
    end Set_Actual;


    function Has_Offsets return Boolean is
      use type Earth.Direction;
    begin
      return The_Offset /= Earth.Zero_Direction;
    end Has_Offsets;


    procedure Store_Correction is

      use type Angle.Value;

      Alt_Offset : constant Angle.Signed := +Earth.Alt_Of (The_Offset);
      Az_Offset  : constant Angle.Signed := +Earth.Az_Of (The_Offset);

      Alt : constant Angle.Unsigned := Earth.Alt_Of (The_Direction) - Alt_Offset;
      Az  : constant Angle.Unsigned := Earth.Az_Of (The_Direction) - Az_Offset;
      Inv : constant Boolean        := Earth.Direction_Is_Inverse (The_Direction);

      function Image_Of (Item : Angle.Signed) return String is
      begin
        return ' ' & Strings.Trimmed (Item'img);
      end Image_Of;

      function Image_Of (Is_Inverse : Boolean) return String is
      begin
        if Is_Inverse then
          return Invers_Id;
        else
          return Normal_Id;
        end if;
      end Image_Of;

    begin -- Add_Correction
      begin
        Ada.Text_IO.Open (File, Name => New_Alignment_Filename, Mode => Ada.Text_IO.Append_File);
      exception
      when others =>
        Ada.Directories.Create_Path (Alignment_Directory);
        Ada.Text_IO.Create (File, Name => New_Alignment_Filename);
      end;
      Ada.Text_IO.Put_Line (File, Image_Of (Inv) & Alt'img & Az'img & Image_Of (Alt_Offset) & Image_Of (Az_Offset));
      Ada.Text_IO.Close (File);
    exception
    when Occurrence: others =>
      Log.Termination (Occurrence);
    end Store_Correction;


    procedure Add_Correction is

      use type Angle.Signed;
      use type Matrix.List.Item;

      Offsets : constant Matrix.Offsets := (Alt => +Earth.Alt_Of (The_Offset),
                                            Az  => +Earth.Az_Of (The_Offset));

      Index : constant Boolean := Earth.Direction_Is_Inverse (The_Direction);

    begin
      The_Correction_Data(Index) := The_Correction_Data(Index)
                                    + Matrix.Data'(Alt    => Earth.Alt_Of (The_Direction) - Offsets.Alt,
                                                   Az     => Earth.Az_Of (The_Direction) - Offsets.Az,
                                                   Offset => Offsets);
    end Add_Correction;


    function Has_Corrections return Boolean is
    begin
      return not Matrix.Is_Empty (The_Correction_Data) or Matrix.Is_Available;
    end Has_Corrections;


    function Correction_Data_Ready return Boolean is
    begin
      return Matrix.Is_Ready (The_Correction_Data);
    end Correction_Data_Ready;


    procedure Apply_Corrections is
    begin
      Calculate (The_Correction_Data);
    end Apply_Corrections;


    function Number_Of_Stars_Added return Natural is
    begin
      return Natural(The_Correction_Data(False).Length) + Natural(The_Correction_Data(True).Length);
    end Number_Of_Stars_Added;


    procedure Clear_All_Corrections is
    begin
      Matrix.Clear (The_Correction_Data);
      The_Pole_Offsets := Earth.Unknown_Direction;
      The_Rotations := Space.Unknown_Direction;
      The_Corrections := Space.Unknown_Direction;
      The_System_Error := 0.0;
      The_Ra_Correction := 0.0;
      The_Dec_Correction := 0.0;
    end Clear_All_Corrections;


    procedure Get_Values (Annotation  :     String;
                          Alt_Actual  : out Angle.Degrees;
                          Az_Actual   : out Angle.Degrees;
                          Alt_Nominal : out Angle.Degrees;
                          Az_Nominal  : out Angle.Degrees) is

      use type Angle.Unsigned;
      use type Angle.Value;

      Alt_Offset : constant Angle.Signed := +Earth.Alt_Of (The_Offset);
      Az_Offset  : constant Angle.Signed := +Earth.Az_Of (The_Offset);

    begin
      Has_One_Star_Alignment := False;
      The_Alt_Actual := +Earth.Alt_Of (The_Direction);
      The_Az_Actual := +Earth.Az_Of (The_Direction);
      The_Alt_Nominal := +Angle.Unsigned'(Earth.Alt_Of (The_Direction) - Alt_Offset);
      The_Az_Nominal := +Angle.Unsigned'(Earth.Az_Of (The_Direction) - Az_Offset);
      Alt_Actual := The_Alt_Actual;
      Az_Actual := The_Az_Actual;
      Alt_Nominal := The_Alt_Nominal;
      Az_Nominal := The_Az_Nominal;
      Log.Write (Annotation);
      Log.Write ("  Is Inverse = " & Earth.Direction_Is_Inverse (The_Direction)'img);
      Log.Write ("  Altitude   = " & Earth.Alt_Image_Of (The_Direction));
      Log.Write ("  Azimuth    = " & Earth.Az_Image_Of (The_Direction));
      Log.Write ("  Alt Offset = " & Earth.Alt_Offset_Image_Of (The_Offset));
      Log.Write ("  Az Offset  = " & Earth.Az_Offset_Image_Of (The_Offset));
    end Get_Values;


    procedure Set_Cone_Error (Item : Angle.Value) is
    begin
      The_Cone_Error := Item;
    end Set_Cone_Error;


    procedure Set_Pole_Offsets (Item : Earth.Direction) is
    begin
      The_Pole_Offsets := Item;
    end Set_Pole_Offsets;


    procedure Set_Offsets (For_Ra  : Angle.Degrees;
                           For_Dec : Angle.Degrees) is
    begin
      The_Ra_Correction  := For_Ra;
      The_Dec_Correction := For_Dec;
    end Set_Offsets;


    procedure Set (The_Pole_Height_Offset : Angle.Degrees;
                   The_Pole_Az_Offset     : Angle.Degrees;
                   The_Ra_Rotation        : Angle.Degrees;
                   The_Dec_Rotation       : Angle.Degrees) is

      use type Angle.Degrees;
      use type Angle.Value;

    begin
      The_Pole_Offsets := Earth.Direction_Of (Az  => +The_Pole_Az_Offset,
                                              Alt => +The_Pole_Height_Offset);

      The_Rotations := Space.Direction_Of (Ra  => The_Ra_Rotation,
                                           Dec => The_Dec_Rotation);

      Calculations.Evaluate_Tree_Star_Corrections (Nominal_Alt        => The_Alt_Nominal,
                                                   Nominal_Az         => The_Az_Nominal,
                                                   Actual_Alt         => The_Alt_Actual,
                                                   Actual_Az          => The_Az_Actual,
                                                   The_Ra_Correction  => The_Ra_Correction,
                                                   The_Dec_Correction => The_Dec_Correction);

      The_Ra_Correction := The_Ra_Rotation - The_Ra_Correction;
      The_Dec_Correction := The_Dec_Rotation - The_Dec_Correction;

      Log.Write ("Third star Star_Ra_Correction  =" & Image_Of (The_Ra_Correction));
      Log.Write ("Third star Star_Dec_Correction =" & Image_Of (The_Dec_Correction));

      The_Corrections :=  Space.Direction_Of (Ra  => The_Ra_Correction,
                                              Dec => The_Dec_Correction);
    end Set;


    function Actual_Cone_Error return Angle.Value is
    begin
      return The_Cone_Error;
    end Actual_Cone_Error;


    function Actual_Pole_Offsets return Earth.Direction is
    begin
      return The_Pole_Offsets;
    end Actual_Pole_Offsets;


    function Three_Star_Rotations return Space.Direction is
    begin
      return The_Rotations;
    end Three_Star_Rotations;


    function Three_Star_Corrections return Space.Direction is
    begin
      return The_Corrections;
    end Three_Star_Corrections;


    function Star_Synchronized_Offsets return Offsets is
    begin
      return [Ra => The_Ra_Correction, Dec => The_Dec_Correction];
    end Star_Synchronized_Offsets;

  end Control;


  Alt_1_Actual  : Angle.Degrees;
  Az_1_Actual   : Angle.Degrees;
  Alt_1_Nominal : Angle.Degrees;
  Az_1_Nominal  : Angle.Degrees;
  Alt_2_Actual  : Angle.Degrees;
  Az_2_Actual   : Angle.Degrees;
  Alt_2_Nominal : Angle.Degrees;
  Az_2_Nominal  : Angle.Degrees;

  procedure Add_First is
  begin
    Control.Get_Values ("Add first value",
                        Alt_Actual  => Alt_1_Actual,
                        Az_Actual   => Az_1_Actual,
                        Alt_Nominal => Alt_1_Nominal,
                        Az_Nominal  => Az_1_Nominal);
  end Add_First;


  procedure Add_Second is
  begin
    Control.Get_Values ("Add second value",
                        Alt_Actual  => Alt_2_Actual,
                        Az_Actual   => Az_2_Actual,
                        Alt_Nominal => Alt_2_Nominal,
                        Az_Nominal  => Az_2_Nominal);
  end Add_Second;


  --TEST 1 ONLY--------------------------------------------------
  --procedure Set_Test_Value (Dec_Nominal :     Angle.Degrees;
  --                          Tau_Nominal :     Angle.Degrees;
  --                          Dec_Actual  :     Angle.Degrees;
  --                          Tau_Actual  :     Angle.Degrees;
  --                          Alt_Nominal : out Angle.Degrees;
  --                          Az_Nominal  : out Angle.Degrees;
  --                          Alt_Actual  : out Angle.Degrees;
  --                          Az_Actual   : out Angle.Degrees) is
  --  use Astro;
  --  use SPHLIB;
  --  use type Angle.Value;
  --begin
  --  EQUHOR (DEC => Dec_Nominal,
  --          TAU => Tau_Nominal,
  --          PHI => +Parameter.Pole_Height,
  --          H   => Alt_Nominal,
  --          AZ  => Az_Nominal);
  --          Az_Nominal := Az_Nominal + 180.0;
  --  EQUHOR (DEC => Dec_Actual,
  --          TAU => Tau_Actual,
  --          PHI => +Parameter.Pole_Height,
  --          H   => Alt_Actual,
  --          AZ  => Az_Actual);
  --          Az_Actual := Az_Actual + 180.0;
  --end Set_Test_Value;
  ---------------------------------------------------------------

  --TEST 2 ONLY--------------------------------------------------
  --procedure Set_Test_Value (Number      :     Character;
  --                          Alt_Image   :     String;
  --                          Az_Image    :     String;
  --                          Alt_Offset  :     String;
  --                          Az_Offset   :     String;
  --                          Alt_Nominal : out Angle.Degrees;
  --                          Az_Nominal  : out Angle.Degrees;
  --                          Alt_Actual  : out Angle.Degrees;
  --                          Az_Actual   : out Angle.Degrees) is
  --  use Astro;
  --  use SPHLIB;
  --  use type Angle.Value;
  --  The_Nominal_Dec : REAL;
  --  The_Nominal_Tau : REAL;
  --begin
  --  Alt_Nominal := +Angle.Value_Of (Alt_Image);
  --  Az_Nominal := +Angle.Value_Of (Az_Image);
  --  Alt_Actual := +(Angle.Value_Of (Alt_Offset) + Alt_Nominal);
  --  Az_Actual := +(Angle.Value_Of (Az_Offset) + Az_Nominal);
  --  HOREQU (H   => Alt_Nominal,
  --          AZ  => Az_Nominal,
  --          PHI => +Parameter.Pole_Height,
  --          DEC => The_Nominal_Dec,
  --          TAU => The_Nominal_Tau);
  --  Log.Write ("Alt " & Number & " Nominal = " & Angle.Image_Of (+Alt_Nominal, Show_Signed => True));
  --  Log.Write ("Az  " & Number & " Nominal = " & Angle.Image_Of (+Az_Nominal));
  --  Log.Write ("Dec " & Number & " Nominal = " & Angle.Image_Of (+The_Nominal_Dec));
  --  Log.Write ("Tau " & Number & " Nominal = " & Angle.Image_Of (+The_Nominal_Tau));
  --  Log.Write ("Alt " & Number & " Actual  = " & Angle.Image_Of (+Alt_Actual, Show_Signed => True));
  --  Log.Write ("Az  " & Number & " Actual  = " & Angle.Image_Of (+Az_Actual));
  --end Set_Test_Value;
  ---------------------------------------------------------------


  procedure Add_Third is

    Alt_3_Actual  : Angle.Degrees;
    Az_3_Actual   : Angle.Degrees;
    Alt_3_Nominal : Angle.Degrees;
    Az_3_Nominal  : Angle.Degrees;

    The_Pole_Height_Offset : Angle.Degrees;
    The_Pole_Az_Offset     : Angle.Degrees;
    The_Ra_Rotation        : Angle.Degrees;
    The_Dec_Rotation       : Angle.Degrees;

  begin
    Control.Get_Values ("Add third value",
                        Alt_Actual  => Alt_3_Actual,
                        Az_Actual   => Az_3_Actual,
                        Alt_Nominal => Alt_3_Nominal,
                        Az_Nominal  => Az_3_Nominal);

    --TEST 1 ONLY----------------------------------------------
    --Set_Test_Value (Dec_Nominal => 40.0,
    --                Tau_Nominal => 107.0,
    --                Dec_Actual  => 40.0,
    --                Tau_Actual  => 107.52,
    --                Alt_Nominal => Alt_1_Nominal,
    --                Az_Nominal  => Az_1_Nominal,
    --                Alt_Actual  => Alt_1_Actual,
    --                Az_Actual   => Az_1_Actual);
    --Log.Write ("Alt_1_Nominal =" & Image_Of (Alt_1_Nominal));
    --Log.Write ("Az_1_Nominal  =" & Image_Of (Az_1_Nominal));
    --Log.Write ("Alt_1_Actual  =" & Image_Of (Alt_1_Actual));
    --Log.Write ("Az_1_Actual   =" & Image_Of (Az_1_Actual));
    --
    --Set_Test_Value (Dec_Nominal => 21.0,
    --                Tau_Nominal => 62.0,
    --                Dec_Actual  => 21.0,
    --                Tau_Actual  => 62.49,
    --                Alt_Nominal => Alt_2_Nominal,
    --                Az_Nominal  => Az_2_Nominal,
    --                Alt_Actual  => Alt_2_Actual,
    --                Az_Actual   => Az_2_Actual);
    --Log.Write ("Alt_2_Nominal =" & Image_Of (Alt_2_Nominal));
    --Log.Write ("Az_2_Nominal  =" & Image_Of (Az_2_Nominal));
    --Log.Write ("Alt_2_Actual  =" & Image_Of (Alt_2_Actual));
    --Log.Write ("Az_2_Actual   =" & Image_Of (Az_2_Actual));
    --
    --Set_Test_Value (Dec_Nominal => 20.0,
    --                Tau_Nominal => 85.0,
    --                Dec_Actual  => 52.0,
    --                Tau_Actual  => 85.51,
    --                Alt_Nominal => Alt_3_Nominal,
    --                Az_Nominal  => Az_3_Nominal,
    --                Alt_Actual  => Alt_3_Actual,
    --                Az_Actual   => Az_3_Actual);
    --Log.Write ("Alt_3_Nominal =" & Image_Of (Alt_3_Nominal));
    --Log.Write ("Az_3_Nominal  =" & Image_Of (Az_3_Nominal));
    --Log.Write ("Alt_3_Actual  =" & Image_Of (Alt_3_Actual));
    --Log.Write ("Az_3_Actual   =" & Image_Of (Az_3_Actual));
    -----------------------------------------------------------

    --TEST 2 ONLY------------------------------------
    --Set_Test_Value (Number     => '1',
    --                Alt_Image  => "+18°41'43.9""",
    --                Az_Image   => "129°27'22.0""",
    --                Alt_Offset => "+0°07'55.201""",
    --                Az_Offset  => "+0°13'26.397""",
    --                Alt_Actual  => Alt_1_Actual,
    --                Az_Actual   => Az_1_Actual,
    --                Alt_Nominal => Alt_1_Nominal,
    --                Az_Nominal  => Az_1_Nominal);
    --
    --Set_Test_Value (Number     => '2',
    --                Alt_Image  => "+34°21'28.1""",
    --                Az_Image   => "84°37'34.0""",
    --                Alt_Offset => "+0°11'22.561""",
    --                Az_Offset  => "+0°11'47.512""",
    --                Alt_Actual  => Alt_2_Actual,
    --                Az_Actual   => Az_2_Actual,
    --                Alt_Nominal => Alt_2_Nominal,
    --                Az_Nominal  => Az_2_Nominal);
    --
    --Set_Test_Value (Number     => '3',
    --                Alt_Image  => "+38°06'25.5""",
    --                Az_Image   => "129°36'09.1""",
    --                Alt_Offset => "+0°07'49.442""",
    --                Az_Offset  => "+0°16'10.522""",
    --                Alt_Actual  => Alt_3_Actual,
    --                Az_Actual   => Az_3_Actual,
    --                Alt_Nominal => Alt_3_Nominal,
    --                Az_Nominal  => Az_3_Nominal);
    -------------------------------------------------

    Calculations.Evaluate_Tree_Star_Rotations (Nominal_Alt_1          => Alt_1_Nominal,
                                               Nominal_Az_1           => Az_1_Nominal,
                                               Nominal_Alt_2          => Alt_2_Nominal,
                                               Nominal_Az_2           => Az_2_Nominal,
                                               Nominal_Alt_3          => Alt_3_Nominal,
                                               Nominal_Az_3           => Az_3_Nominal,
                                               Actual_Alt_1           => Alt_1_Actual,
                                               Actual_Az_1            => Az_1_Actual,
                                               Actual_Alt_2           => Alt_2_Actual,
                                               Actual_Az_2            => Az_2_Actual,
                                               Actual_Alt_3           => Alt_3_Actual,
                                               Actual_Az_3            => Az_3_Actual,
                                               The_Pole_Height_Offset => The_Pole_Height_Offset,
                                               The_Pole_Az_Offset     => The_Pole_Az_Offset,
                                               The_Ra_Rotation        => The_Ra_Rotation,
                                               The_Dec_Rotation       => The_Dec_Rotation,
                                               The_System_Error       => The_System_Error);

    Log.Write ("The_Pole_Height_Offset =" & Image_Of (The_Pole_Height_Offset));
    Log.Write ("The_Pole_Az_Offset     =" & Image_Of (The_Pole_Az_Offset));
    Log.Write ("The_Ra_Rotation        =" & Image_Of (The_Ra_Rotation));
    Log.Write ("The_Dec_Rotation       =" & Image_Of (The_Dec_Rotation));
    Log.Write ("The_System_Error       =" & Image_Of (The_System_Error));

    Control.Set (The_Pole_Height_Offset,
                 The_Pole_Az_Offset,
                 The_Ra_Rotation,
                 The_Dec_Rotation);
  exception
  when Alignment.Calculations.Failed =>
    raise Failure;
  end Add_Third;


  procedure Read is

    The_Data_Lists : Matrix.Data_Lists;

    The_File : Ada.Text_IO.File_Type;


  begin
    --TEST ONLY--------------------
    --Control.Add_Third_Correction;
    -------------------------------
    if Matrix.Is_Available then
      return;
    end if;
    begin
      Ada.Text_IO.Open (The_File, Name => Alignment_Filename, Mode => Ada.Text_IO.In_File);
    exception
    when others =>
      return; -- no alignment
    end;
    while not Ada.Text_IO.End_Of_File (The_File) loop
      declare
        Line  : constant String := Strings.Trimmed (Ada.Text_IO.Get_Line (The_File));
        Parts : constant Strings.Item := Strings.Item_Of (Line, ' ');

        type Element is (Kind, Altitude, Azimuth, Altitude_Offset, Azimuth_Offset);

        function Image_Of (Item : Element) return String is
        begin
          return Strings.Trimmed (Parts(Strings.First_Index + Element'pos(Item)));
        end Image_Of;

        function Is_Inverse (Item : Element) return Boolean is
        begin
          declare
            Kind_Image : constant String := Image_Of (Kind);
          begin
            if Kind_Image = Invers_Id then
              return True;
            elsif Kind_Image = Normal_Id then
              return False;
            else
              raise Constraint_Error;
            end if;
          end;
        exception
        when others =>
          Error.Raise_With ("Incorrect Value for " & Item'img & " in line <" & Line & "> of " & Alignment_Filename);
        end Is_Inverse;

        function Value_Of (Item : Element) return Angle.Unsigned is
        begin
          return Angle.Unsigned'value(Image_Of (Item));
        exception
        when others =>
          Error.Raise_With ("Incorrect Value for " & Item'img & " in line <" & Line & "> of " & Alignment_Filename);
        end Value_Of;

        function Value_Of (Item : Element) return Angle.Signed is
        begin
          return Angle.Signed'value(Image_Of (Item));
        exception
        when others =>
          Error.Raise_With ("Incorrect Offset for " & Item'img & " in line <" & Line & "> of " & Alignment_Filename);
        end Value_Of;

        use type Matrix.List.Item;

      begin
        if Parts.Count /= (Element'pos(Element'last) + 1) then
          Error.Raise_With ("Incorrect Line <" & Line & "> in " & Alignment_Filename);
        end if;
        declare
          Matrix_Correction : constant Matrix.Data := (Alt    => Value_Of (Altitude),
                                                       Az     => Value_Of (Azimuth),
                                                       Offset => (Alt => Value_Of (Altitude_Offset),
                                                                  Az  => Value_Of (Azimuth_Offset)));
          Index : constant Boolean := Is_Inverse (Kind);
        begin
          The_Data_Lists(Index) := The_Data_Lists(Index) + Matrix_Correction;
        end;
      end;
    end loop;
    Ada.Text_IO.Close (The_File);
    Calculate (The_Data_Lists);
  exception
  when Error.Occurred =>
    raise;
  when Occurrence: others =>
    Log.Termination (Occurrence);
    Ada.Text_IO.Close (The_File);
  end Read;

end Alignment;
