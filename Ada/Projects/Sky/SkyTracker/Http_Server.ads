-- *********************************************************************************************************************
-- *                       (c) 2023 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Angle;

private with Network;
private with Text;
private with Traces;

package Http_Server is

  type Degrees is delta 0.00001 range -999.0 .. 999.0;

  subtype Microns is Integer;

  type Mount_Data is record
    Exists       : Boolean := False;
    Axis0        : Degrees := 0.0;
    Axis1        : Degrees := 0.0;
    Model_Points : Natural := 0;
  end record;

  type Mirror_Position is (Unknown, Between, Ocular, Camera);

  type Focuser_Data is record
    Exists       : Boolean := False;
    Moving       : Boolean := False;
    Max_Position : Microns := 0;
    Position     : Microns := 0;
    Set_Position : access procedure (Item : Microns);
  end record;

  type Rotator_Data is record
    Exists             : Boolean := False;
    Moving             : Boolean := False;
    Slewing            : Boolean := False;
    Mech_Position      : Degrees := 0.0;
    Field_Angle        : Degrees := 0.0;
    Goto_Field_Angle   : access procedure (Item : Degrees);
    Goto_Mech_Position : access procedure (Item : Degrees);
    Goto_Offset        : access procedure (Item : Degrees);
  end record;

  procedure Start;

  procedure Set_State (Image : String);

  procedure Set_Moving (Speed : Angle.Value); -- per second

  procedure Set (Data : Mount_Data);

  procedure Set (Position : Mirror_Position);

  procedure Set (Data : Focuser_Data);

  procedure Set (Data : Rotator_Data);

  procedure Shutdown;

private

  Id : constant String := "Http_Server";

  package Log is new Traces (Id);

  The_Server_Port     : Network.Port_Number;
  The_Client_Filename : Text.String;

end Http_Server;
