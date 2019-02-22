-- *********************************************************************************************************************
-- *                       (c) 2011 .. 2019 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Angle;
with Device;
with Parameter;
with Os.Pipe;
with Sky_Line;
with Time;
with Traces;
with User.Input;

package body Os.Ascom is

  package Log is new Traces ("Ascom");

  protected type Position is
    procedure Clear;
    procedure Set_Direction (The_Value : Space.Direction);
    function Ra_Image return String;
    function Dec_Image return String;
    function Object return Space.Direction;
  private
    Is_Set     : Boolean := False;
    The_Object : Space.Direction;
  end Position;


  Not_Set : exception;

  protected body Position is

    procedure Clear is
    begin
      Is_Set := False;
    end Clear;

    procedure Set_Direction (The_Value : Space.Direction) is
    begin
      if Space.Direction_Is_Known (The_Value) then
        Is_Set := True;
        The_Object := The_Value;
      else
        Is_Set := False;
      end if;
    end Set_Direction;

    function Object return Space.Direction is
    begin
      if Is_Set then
        return The_Object;
      else
        raise Not_Set;
      end if;
    end Object;

    function Ra_Image return String is
      type Hours  is delta 0.000001 range 0.0 .. 24.0;
      use type Angle.Value;
    begin
      if Is_Set then
        return Hours(Angle.Hours'(+Space.Ra_Of (The_Object)))'img;
      else
        raise Not_Set;
      end if;
    exception
    when others =>
      raise Not_Set;
    end Ra_Image;

    function Dec_Image return String is
      type Degrees is delta 0.000001 range -90.0 .. 90.0;
      use type Angle.Signed;
    begin
      if Is_Set then
        return Degrees(Angle.Degrees'(+Angle.Signed'(+Space.Dec_Of (The_Object))))'img;
      else
        raise Not_Set;
      end if;
    exception
    when others =>
      raise Not_Set;
    end Dec_Image;

  end Position;


  Telescope : Position;
  Target    : Position;

  task type Control;

  The_Control   : access Control with Unreferenced;
  Goto_Location  : Handler;
  Synch_Location : Handler;
  Is_Slewing     : Boolean := False;


  procedure Start is
  begin
    The_Control := new Control;
  end Start;


  procedure Define_Handlers (Goto_Handler  : Handler;
                             Synch_Handler : Handler) is
  begin
    Goto_Location := Goto_Handler;
    Synch_Location := Synch_Handler;
  end Define_Handlers;


  procedure Set (The_Location : Space.Direction) is
  begin
    Telescope.Set_Direction (The_Location);
  end Set;


  procedure Set (Is_Approaching : Boolean) is
  begin
    Is_Slewing := Is_Approaching;
  end Set;


  The_Pipe  : Os.Pipe.Handle;
  Is_Active : Boolean := True;


  procedure Close is
  begin
    Is_Active := False;
    Os.Pipe.Close (The_Pipe);
  end Close;


  task body Control is

    The_Data : aliased String (1..256);

    function Read return String is
      The_Length : Natural;
    begin
      Os.Pipe.Read (From_Pipe => The_Pipe,
                    Data      => The_Data'address,
                    Length    => The_Length);
      return The_Data(The_Data'first .. The_Length);
    end Read;

    procedure Write (Item : String) is
    begin
      The_Data(1..Item'length) := Item;
      Os.Pipe.Write (To_Pipe => The_Pipe,
                     Data    => The_Data'address,
                     Length  => Item'length);
    end Write;

    function Image_Of (The_Value : Angle.Value) return String is
      type Degrees is delta 0.000001 range 0.0 .. 360.0;
      use type Angle.Value;
    begin
      return Degrees(Angle.Degrees'(+The_Value))'img;
    end Image_Of;

    The_Ra  : Angle.Hours;
    The_Dec : Angle.Degrees;

    use type Angle.Value;

  begin -- Control
    Log.Write ("Started");
    while Is_Active loop
      begin
        Target.Clear;
        Os.Pipe.Open (The_Pipe  => The_Pipe,
                      Name      => "ASCOM_STD_1.00",
                      Kind      => Os.Pipe.Server,
                      Mode      => Os.Pipe.Duplex,
                      Size      => The_Data'length,
                      Wait_Time => 1.0);
        Log.Write ("Pipe opened");
        loop
          declare
            Command : constant String := Read;
          begin
            if Command'length > 0 then
              case Command (Command'first) is
              when 'r' =>
                begin
                  declare
                    Image : constant String := Telescope.Ra_Image;
                  begin
                    Write (Image);
                  end;
                exception
                when Not_Set =>
                  Write ("");
                end;
              when 'd' =>
                begin
                  declare
                    Image : constant String := Telescope.Dec_Image;
                  begin
                    Write (Image);
                  end;
                exception
                when Not_Set =>
                  Write ("");
                end;
              when 'i' =>
                if Is_Slewing then
                  Write ("true");
                else
                  Write ("false");
                end if;
              when 'l' =>
                if Command'length > 1 then
                  case Command (Command'first + 1) is
                  when 'o' =>
                    Log.Write ("Longitude");
                    Write (Image_Of (Parameter.Longitude));
                  when 'a' =>
                    Log.Write ("Latitude");
                    Write (Image_Of (Parameter.Latitude));
                  when others =>
                    Log.Write ("Unhandled Command: " & Command);
                  end case;
                else
                  Log.Write ("Unhandled Command: " & Command);
                end if;
              when 'A' =>
                Log.Write ("AbortSlew");
                User.Input.Put (Device.Stop, User.Input.Ascom);
              when 'R' =>
                The_Ra := Angle.Hours'value(Command(Command'first + 3 .. Command'last));
              when 'D' =>
                The_Dec := Angle.Degrees'value(Command(Command'first + 4 .. Command'last));
              when 'S' =>
                Target.Set_Direction (Space.Direction_Of (Dec => Angle.Value'(+The_Dec),
                                                          Ra  => Angle.Value'(+The_Ra)));
                case Command (Command'first + 1) is
                when 'e' => -- Set
                  Log.Write ("!!!Set - Command: " & Command);
                when 'l' => -- Slew
                  if Goto_Location /= null then
                    begin
                      if Sky_Line.Is_Above (Target.Object, Time.Lmst) then
                        Goto_Location (Target.Object);
                        Is_Slewing := True;
                      end if;
                    exception
                    when Not_Set =>
                      Log.Write ("!!!Unknown Target");
                    end;
                  end if;
                when 'y' => -- Sync
                  if Synch_Location /= null then
                    begin
                      if Sky_Line.Is_Above (Target.Object, Time.Lmst) then
                        Synch_Location (Target.Object);
                        Is_Slewing := False;
                      end if;
                    exception
                    when Not_Set =>
                      Log.Write ("!!!Unknown Target");
                    end;
                  end if;
                when others =>
                  Log.Write ("Unhandled Command: " & Command);
                end case;
              when others =>
                Log.Write ("Unhandled Command: " & Command);
              end case;
            end if;
          end;
        end loop;
      exception
      when Os.Pipe.Broken =>
        Os.Pipe.Close (The_Pipe);
        Log.Write ("Pipe closed - broken");
      when Os.Pipe.No_Server =>
        Os.Pipe.Close (The_Pipe);
        Log.Write ("Pipe closed - no server");
      when Os.Pipe.Timeout =>
        null;
      when Item: others =>
        Log.Termination (Item);
      end;
    end loop;
    Log.Write ("Terminated");
  exception
  when Item: others =>
    Log.Termination (Item);
  end Control;

end Os.Ascom;
