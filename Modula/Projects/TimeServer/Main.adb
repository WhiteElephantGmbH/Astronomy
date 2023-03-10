-- *********************************************************************************************************************
-- *                           (c) 2023 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Clock;
with Kernel.Death;
with State;

package body Main is

  use Kernel;

  Loop_Time : constant := 500 * Clock.One_Millisecond;

  type Attribute_Body is record
    Control : State.Control;
    Work    : State.Item;
    Timer   : Clock.Event;
  end record;

  type Attributes is not null access Attribute_Body;

  The_Attribute_Body : Attribute_Body;

  -----------------------------------------------------------------------------*
  --                                  States                                   *
  -----------------------------------------------------------------------------*)

  ----------
  -- Work --
  ----------

  procedure Work_Entry (A : Attributes) is
  begin
    Clock.Arm (A.Timer, Loop_Time);
  end Work_Entry;

  procedure Work (A : Attributes) is
  begin
    if Clock.Has_Occurred (A.Timer) then
      Clock.Arm (A.Timer, Loop_Time);
      Death.By (Death.Peripheral_Error); -- Show P for Perfect
    else
      CHECK;
    end if;
  end Work;

  procedure Initialize is
  begin
    null;
  end Initialize;

  A : Attribute_Body renames The_Attribute_Body;

begin
  State.Initialize_High (A.Control, A'address, Secondary_Stack_Size => 256);
  State.Define (A.Work, A.Control, Work_Entry'address, Work'address);
  Clock.Initialize (A.Timer);
end Main;
