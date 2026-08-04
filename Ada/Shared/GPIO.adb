-- *********************************************************************************************************************
-- *                           (c) 2026 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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
pragma Style_Astronomy;

with Ada.Finalization;
with Gpiod_Interface;
with Standard_C_Interface;
with Terminator;
with Traces;

package body GPIO is

  package Log is new Traces ("GPIO");

  package GI renames Gpiod_Interface;
  package CI renames Standard_C_Interface;

  Device   : aliased constant String := "/dev/gpiochip0" & Ascii.Nul;
  Consumer : aliased constant String := "GPIO" & Ascii.Nul;

  First_Line : constant := 2;

  type Line_Kind is (Static_Input, Dynamic_Input, Output);


  protected type Line_State is

    entry Request (The_Kind : Line_Kind);

    entry Release (The_Kind : out Line_Kind);

    procedure Finish_Release_Dynamic_Input;

    procedure Release_Done;

    procedure Set_Active (The_Kind : Line_Kind);

    procedure Set_Inactive;

    function Is_In_Use return Boolean;

  private
    Is_Active     : Boolean := False;
    Is_Releasing  : Boolean := False;
    Is_Used       : Boolean := False;
    The_Line_Kind : Line_Kind;
  end Line_State;


  protected body Line_State is

    entry Request (The_Kind : Line_Kind) when not Is_Active is
    begin
      if Is_Used then
        raise In_Use;
      end if;
      The_Line_Kind := The_Kind;
      Is_Active := True;
      Is_Releasing := False;
      Is_Used := True;
    end Request;


    entry Release (The_Kind : out Line_Kind) when not Is_Active is
    begin
      if not Is_Used then
        raise Not_In_Use;
      end if;
      The_Kind := The_Line_Kind;
      Is_Releasing := The_Kind = Dynamic_Input;
      Is_Active := True;
    end Release;


    procedure Finish_Release_Dynamic_Input is
    begin
      Is_Releasing := False;
      Is_Used := False;
    end Finish_Release_Dynamic_Input;


    procedure Release_Done is
    begin
      if not Is_Releasing then
        Is_Used := False;
      end if;
      Is_Active := False;
    end Release_Done;


    procedure Set_Active (The_Kind : Line_Kind) is
    begin
      if not Is_Used then
        raise Not_In_Use;
      elsif The_Line_Kind /= The_Kind then
        raise Usage_Error;
      end if;
      if Is_Releasing then -- Finish releasing dynamic input
        Is_Releasing := False;
        Is_Used := False;
        raise Released;
      end if;
      Is_Active := True;
    end Set_Active;


    procedure Set_Inactive is
    begin
      Is_Active := False;
    end Set_Inactive;


    function Is_In_Use return Boolean is (Is_Used);

  end Line_State;


  procedure Raise_Error (Error_Message : String) with No_Return is
  begin
    Log.Error (Error_Message);
    raise Device_Error;
  end Raise_Error;


  procedure Check (Is_True        : Boolean;
                   Error_Message  : String) is
  begin
    if not Is_True then
      Raise_Error (Error_Message);
    end if;
  end Check;


  procedure Check (Return_Code   : CI.Return_Code;
                   Error_Message : String) is
    use type CI.Return_Code;
  begin
    Check (Return_Code = CI.Success, Error_Message);
  end Check;


  type Line_Configuration is record
    State          : Line_State;
    Settings       : access GI.Line_Settings;
    Config         : access GI.Line_Config;
    Request_Config : access GI.Request_Config;
    Request        : access GI.Line_Request;
    Event_Buffer   : access GI.Edge_Event_Buffer;
    Trigger        : Terminator.Trigger;
  end record;

  type Line_Configurations is array (Line) of Line_Configuration;

  type Chip is new Ada.Finalization.Limited_Controlled with record
    Device         : access GI.Chip;
    Configurations : Line_Configurations;
  end record;


  overriding
  procedure Finalize (The_Chip : in out Chip);


  procedure Initialize (The_Chip : in out Chip) is
  begin
    Log.Write ("Initialize " & Device);
    The_Chip.Device := GI.Open (Device'address);
    Check (The_Chip.Device /= null,
           Error_Message => "Unable to open chip");
  end Initialize;


  procedure Finalize (The_Chip : in out Chip) is
  begin
    Log.Write ("Finalize");
    if The_Chip.Device /= null then
      for The_Line in Line loop
        if The_Chip.Configurations (The_Line).State.Is_In_Use then
          Release (The_Line);
        end if;
      end loop;
      GI.Close (The_Chip.Device);
    end if;
  end Finalize;


  The_Chip : Chip;


  procedure Request (Item         : Line;
                     Kind         : Line_Kind;
                     Direction    : GI.Line_Direction;
                     Output_Value : GI.Line_Value := GI.Error) is
    Data   : Line_Configuration renames The_Chip.Configurations (Item);
    Offset : aliased GI.T_Unsigned := GI.T_Unsigned (Line'pos (Item) + First_Line);
  begin
    Data.State.Request (Kind);
    Data.Settings := GI.New_Line_Settings;
    Check (Data.Settings /= null,
           Error_Message => "Unable to create line settings");
    Check (GI.Set_Direction (Data.Settings, Direction),
           Error_Message => "Set_Direction failed");
    case Kind is
    when Output =>
      Check (GI.Set_Output_Value (Data.Settings, Output_Value),
             Error_Message => "Set_Output_Value failed");
    when Dynamic_Input =>
      Data.Event_Buffer := GI.New_Edge_Event_Buffer (1);
      Check (Data.Event_Buffer /= null,
             Error_Message => "Unable to create edge event buffer");
      Check (GI.Set_Edge_Detection (Data.Settings, GI.Both),
             Error_Message => "Set_Edge_Detection failed");
    when others =>
      null;
    end case;
    Data.Config := GI.New_Line_Config;
    Check (Data.Config /= null,
           Error_Message => "Unable to create line config");
    Check (GI.Add_Line_Settings (Data.Config, Offset'access, 1, Data.Settings),
           Error_Message => "Add_Line_Settings failed");
    Data.Request_Config := GI.New_Request_Config;
    Check (Data.Request_Config /= null,
            Error_Message => "Unable to create request config");
    GI.Set_Consumer (Data.Request_Config, Consumer'address);
    Data.Request := GI.Request_Lines (The_Chip.Device, Data.Request_Config, Data.Config);
    Check (Data.Request /= null,
           Error_Message => "Request failed");
    Data.State.Set_Inactive;
  exception
  when others =>
    Data.State.Set_Inactive;
    raise;
  end Request;


  procedure Request_Static_Input (Item : Line) is
  begin
    Log.Write ("Request static input " & Item'image);
    Request (Item, Static_Input, GI.Input);
  end Request_Static_Input;


  function Level_Of (Item : Line) return Level is

    Data   : Line_Configuration renames The_Chip.Configurations (Item);
    Offset : constant GI.T_Unsigned := GI.T_Unsigned (Line'pos (Item) + First_Line);

    use type GI.Line_Value;

  begin
    Data.State.Set_Active (Static_Input);
    declare
      The_Level : Level;
    begin
      case GI.Get_Value (Data.Request, Offset) is
      when GI.Inactive =>
        The_Level := Low;
      when GI.Active =>
        The_Level := High;
      when GI.Error =>
        Raise_Error ("Level of " & Item'image & " failed");
      end case;
      Log.Write ("Level of " & Item'image & " " & The_Level'image);
      Data.State.Set_Inactive;
      return The_Level;
    end;
  exception
  when others =>
    Data.State.Set_Inactive;
    raise;
  end Level_Of;


  procedure Request_Dynamic_Input (Item : Line) is
  begin
    Log.Write ("Request dynamic input " & Item'image);
    Request (Item, Dynamic_Input, GI.Input);
  end Request_Dynamic_Input;


  procedure Await_Change (On : Line;
                          To : Level) is

    Data     : Line_Configuration renames The_Chip.Configurations (On);
    Count    : CI.Return_Count;
    Event    : access GI.Edge_Event;
    Expected : constant GI.Edge_Event_Type := (if To = High then GI.Rising_Edge else GI.Falling_Edge);

    use type GI.Edge_Event_Type;
    use type CI.Return_Count;

  begin
    Data.State.Set_Active (Dynamic_Input);
    Log.Write ("Await change to " & To'image & " on " & On'image);

    --  Discard all pending events. We are interested only in
    --  changes occurring after this call.
    loop
      Count := GI.Wait_Edge_Events (Data.Request, 0);
      exit when Count = 0;
      Check (Count = 1,
             Error_Message => "Wait_Edge_Events failed");
      Count := GI.Read_Edge_Events (Data.Request, Data.Event_Buffer, 2);
      Check (Count > 0,
             Error_Message => "Read_Edge_Events failed");
    end loop;

    declare
      use type CI.File_Descriptor;

      Gpio_Fd    : constant CI.File_Descriptor := GI.Get_Fd (Data.Request);
      Aborter_Fd : constant CI.File_Descriptor := Data.Trigger.Read_Fd;
      N_Fds      : constant CI.Fd_Number := CI.Fd_Number'max (Gpio_Fd, Aborter_Fd) + 1;

      Result : CI.Return_Count;
      Fd_Set : aliased CI.Fd_Set;
    begin
      Wait_For_Change:
      loop
        Fd_Set := [others => False];
        Fd_Set (Gpio_Fd) := True;
        Fd_Set (Aborter_Fd) := True;
        Data.State.Set_Inactive;
        Result := CI.Wait_Select (N_Fds, Fd_Set'access);
        Check (Result /= CI.Failed,
               Error_Message => "Wait select failed");
        Check (Result /= CI.Timed_Out,
               Error_Message => "Unexpected timeout");
        if Fd_Set (Aborter_Fd) then
          Data.Trigger.Clear;
          Data.State.Finish_Release_Dynamic_Input;
          raise Released;
        elsif Fd_Set (Gpio_Fd) then
          Data.State.Set_Active (Dynamic_Input);
          Count := GI.Read_Edge_Events (Data.Request, Data.Event_Buffer, 2);
          Check (Count > 0,
                 Error_Message => "Read edge events failed");
          for Index in 0 .. GI.T_Unsigned_Long (Count - 1) loop
            Event := GI.Get_Event (Data.Event_Buffer, Index);
            Check (Event /= null,
                   Error_Message => "Get event failed");
            exit Wait_For_Change when GI.Get_Event_Type (Event) = Expected;
          end loop;
        else
          Raise_Error ("Unexpected wait select result");
        end if;
      end loop Wait_For_Change;
    end;
    Log.Write ("Change to " & To'image & " on " & On'image);
    Data.State.Set_Inactive;
  exception
  when others =>
    Data.State.Set_Inactive;
    raise;
  end Await_Change;


  procedure Await_Change_To_High (On : Line) is
  begin
    Await_Change (On, To => High);
  end Await_Change_To_High;


  procedure Await_Change_To_Low (On : Line) is
  begin
    Await_Change (On, To => Low);
  end Await_Change_To_Low;


  procedure Request_Output (Item          : Line;
                            Initial_Value : Level := Low) is
    Line_Value : constant GI.Line_Value := (if Initial_Value = Low then GI.Inactive else GI.Active);
  begin
    Log.Write ("Request output " & Item'image & " - initial value: " & Initial_Value'image);
    Request (Item, Output, GI.Output, Line_Value);
  end Request_Output;


  procedure Set (Item : Line;
                 To   : Level) is
    Data   : Line_Configuration renames The_Chip.Configurations(Item);
    Offset : constant GI.T_Unsigned := GI.T_Unsigned(Line'pos(Item) + First_Line);
  begin
    Data.State.Set_Active (Output);
    Check (GI.Set_Value (Data.Request, Offset, (if To = High then GI.Active else GI.Inactive)),
           Error_Message => "Set " & Item'image & " " & To'image & " failed");
    Data.State.Set_Inactive;
  exception
  when others =>
    Data.State.Set_Inactive;
    raise;
  end Set;


  procedure Release (Item : Line) is
    Data : Line_Configuration renames The_Chip.Configurations(Item);
    The_Kind : Line_Kind;
  begin
    Data.State.Release (The_Kind);
    case The_Kind is
    when Output =>
      Log.Write ("Release output " & Item'image);
    when Static_Input =>
      Log.Write ("Release static input " & Item'image);
    when Dynamic_Input =>
      Log.Write ("Release dynamic input " & Item'image);
      Data.Trigger.Signal;
    end case;
    if Data.Request /= null then
      GI.Release (Data.Request);
    end if;
    if Data.Request_Config /= null then
      GI.Free (Data.Request_Config);
    end if;
    if Data.Config /= null then
      GI.Free (Data.Config);
    end if;
    if Data.Settings /= null then
      GI.Free (Data.Settings);
    end if;
    if Data.Event_Buffer /= null then
      GI.Free (Data.Event_Buffer);
    end if;
    Data.State.Release_Done;
    if The_Kind = Output then
      Request_Static_Input (Item);
      Release (Item);
    end if;
  end Release;

end GPIO;
