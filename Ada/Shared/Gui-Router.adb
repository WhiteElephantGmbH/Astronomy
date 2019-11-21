-- *********************************************************************************************************************
-- *                       (c) 2015 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Exceptions;
with Ada.Task_Identification;
with Definite_Doubly_Linked_Lists;
with Gtk.Handlers;
with Gtk.Main;
with Indefinite_Doubly_Linked_Lists;
with Log;

package body Gui.Router is

  type Request_Data_Ptr is access all Request_Data'class;

  type State is (Inactive, Active, Closing);

  The_State : State := Inactive;

  type Gateway_State is (Idle, Failed, Ready, Busy, Killed);
  subtype Completed is Gateway_State range Idle..Ready;

  type Callback is (Action, Click, Key);

  type Callback_Data (The_Callback : Callback := Action) is record
    case The_Callback is
      when Action =>
        The_Action : Action_Routine;
      when Click =>
        The_Click : Click_Routine;
        The_Information : Information;
      when Key =>
        The_Handler : Key_Handler;
        The_Event   : Key_Event;
        The_Code    : Key_Code;
    end case;
  end record;

  package Callback_List is new Definite_Doubly_Linked_Lists (Callback_Data);

  protected Callback_Handling is
    procedure Put (The_Action : Action_Routine);
    procedure Put (The_Click       : Click_Routine;
                   The_Information : Information);
    procedure Put (The_Handler : Key_Handler;
                   The_Event   : Key_Event;
                   The_Code    : Key_Code);

    procedure Finish;
    entry Get (The_Callback : out Callback_Data);
  private
    Is_Enabled : Boolean := True;
    The_Callback_List : Callback_List.Item;
  end Callback_Handling;


  protected body Callback_Handling is

    procedure Put (The_Action : Action_Routine) is
    begin
      if Is_Enabled then
        declare
          The_Callback : constant Callback_Data := (The_Callback => Action,
                                                    The_Action   => The_Action);
        begin
          The_Callback_List.Append (The_Callback);
        end;
      end if;
    end Put;

    procedure Put (The_Click       : Click_Routine;
                   The_Information : Information) is
    begin
      if Is_Enabled  then
        declare
          The_Callback : constant Callback_Data := (The_Callback    => Click,
                                                    The_Click       => The_Click,
                                                    The_Information => The_Information);
        begin
          The_Callback_List.Append (The_Callback);
        end;
      end if;
    end Put;

    procedure Put (The_Handler : Key_Handler;
                   The_Event   : Key_Event;
                   The_Code    : Key_Code) is
    begin
      if Is_Enabled  then
        declare
          The_Callback : constant Callback_Data := (The_Callback => Key,
                                                    The_Handler  => The_Handler,
                                                    The_Event    => The_Event,
                                                    The_Code     => The_Code);
        begin
          The_Callback_List.Append (The_Callback);
        end;
      end if;
    end Put;

    procedure Finish is
    begin
      Is_Enabled := False;
    end Finish;

    entry Get (The_Callback : out Callback_Data) when (not Is_Enabled) or (The_Callback_List.Count > 0) is
      The_Data : Callback_Data;
    begin
      if Is_Enabled then
        The_Data := The_Callback_List.First_Element;
        The_Callback_List.Delete_First;
      else
        The_Data.The_Action := null;
      end if;
      The_Callback := The_Data;
    end Get;

  end Callback_Handling;

  task type Callback_Handler;
  type Callback_Handler_Ptr is access Callback_Handler;

  The_Callback_Handler : Callback_Handler_Ptr with Unreferenced;


  procedure Execute (The_Action : Action_Routine) is
  begin
    Callback_Handling.Put (The_Action);
  end Execute;

  procedure Execute (The_Action      : Click_Routine;
                     The_Information : Information) is
  begin
    Callback_Handling.Put (The_Action, The_Information);
  end Execute;

  procedure Execute (The_Action  : Key_Handler;
                     The_Event   : Key_Event;
                     The_Keycode : Key_Code) is
  begin
    Callback_Handling.Put (The_Action, The_Event, The_Keycode);
  end Execute;

  task type Termination_Handler is
    entry Start (The_Window : Gtk.Window.Gtk_Window);
    entry Finalize;
  end Termination_Handler;
  type Termination_Handler_Ptr is access Termination_Handler;

  The_Termination_Handler : Termination_Handler_Ptr;


  type Data_Type is (Killed, Synchronous, Asynchronous);

  package Message_List is new Indefinite_Doubly_Linked_Lists (Message_Data'class);


  protected Gateway is
    procedure Reset;
    procedure Abort_Service (Error : Ada.Exceptions.Exception_Occurrence);
    procedure Complete_Synchronous_Service;
    entry Synchronous_Request (Data : in out Request_Data'class);
    procedure Asynchronous_Request (Data : Message_Data'class);
    procedure Quit;

    entry Check (The_Data_Type : out Data_Type);
    function Synchronous_Data return Request_Data_Ptr;

    function Next_Message return Message_Data'class;
    procedure Delete_First_Message;

  private
    entry Serviced (Unused_Data : in out Request_Data'class);
    State        : Gateway_State := Failed;
    Data         : Request_Data_Ptr;
    The_Messages : Message_List.Item;
    Fault        : Ada.Exceptions.Exception_Occurrence;
  end Gateway;


  protected body Gateway is

    procedure Reset is
    begin
      State := Idle;
    end Reset;


    procedure Abort_Service (Error : Ada.Exceptions.Exception_Occurrence) is
    begin
      Log.Write ("Abort_Service", Error);
      State := Failed;
      Ada.Exceptions.Save_Occurrence (Fault, Error);
    end Abort_Service;


    procedure Complete_Synchronous_Service is
    begin
      State := Ready;
    end Complete_Synchronous_Service;


    function Next_Message return Message_Data'class is
      The_Message : constant Message_Data'class := The_Messages.First_Element;
    begin
      return The_Message;
    end Next_Message;


    procedure Delete_First_Message is
    begin
      The_Messages.Delete_First;
    end Delete_First_Message;


    procedure Asynchronous_Request (Data : Message_Data'class) is
    begin
      The_Messages.Append (Data);
    end Asynchronous_Request;


    entry Synchronous_Request (Data : in out Request_Data'class)
    when State = Idle or else State = Killed is
    begin
      if State /= Killed then
        Gateway.Data := Data'unchecked_access;
        State := Busy;
        requeue Serviced;
      end if;
    end Synchronous_Request;


    entry Check (The_Data_Type : out Data_Type)
    when not (State in Completed) or else (The_Messages.Count > 0) is
    begin
      if State = Killed then
        The_Data_Type := Killed;
      elsif The_Messages.Count > 0 then
        -- Process all asynchronous calls before synchronous calls.
        The_Data_Type := Asynchronous;
      elsif State = Busy then
        The_Data_Type := Synchronous;
      else
        raise Program_Error;
      end if;
    end Check;


    function Synchronous_Data return Request_Data_Ptr is
    begin
      return Gateway.Data;
    end Synchronous_Data;


    procedure Quit is
    begin
      State := Killed;
    end Quit;

    entry Serviced (Unused_Data : in out Request_Data'class)
    when (State = Ready or else State = Failed or else State = Killed)  is
    begin
      if State = Failed then
        State := Idle;
        Ada.Exceptions.Reraise_Occurrence (Fault);
      elsif State /= Killed then
        State := Idle;
      end if;
    end Serviced;

  end Gateway;


  task type Startup (Startup_Routine : access procedure (Window : Gtk.Window.Gtk_Window)) is
    entry Start (Window : Gtk.Window.Gtk_Window) ;
  end Startup;

  type Startup_Ptr is access Startup;
  Startup_Task : Startup_Ptr;

  The_Gtk_Task : Ada.Task_Identification.Task_Id;

  package Window_Callback is new Gtk.Handlers.Return_Callback (Gtk.Window.Gtk_Window_Record, Boolean);


  The_Termination_Routine : Action_Routine;
  Metrics_At_Close        : Window_Metrics; -- Which window? Ideally there should be one of these per Window!

  type Get_Metrics_Data is new Gui.Router.Request_Data with record
    The_Window : Gtk.Window.Gtk_Window;
    Metrics    : Window_Metrics;
  end record;

  overriding
  procedure Synchronous_Service (Data : in out Get_Metrics_Data) is
  begin
    declare
      Root_X : Glib.Gint := 0;
      Root_Y : Glib.Gint := 0;
    begin
      Gtk.Window.Get_Position (Data.The_Window, Root_X, Root_Y);
      Data.Metrics.X_Position := Integer(Root_X);
      Data.Metrics.Y_Position := Integer(Root_Y);
    end;
    declare
      Width  : Glib.Gint := 0;
      Height : Glib.Gint := 0;
    begin
      Gtk.Window.Get_Size (Data.The_Window, Width, Height);
      Data.Metrics.Width := Integer(Width);
      Data.Metrics.Height := Integer(Height);
    end;
  end Synchronous_Service;

  function Get_Window_Metrics (For_Window : Gtk.Window.Gtk_Window) return Window_Metrics is
  begin
    if The_State = Active then
      declare
        Data : Get_Metrics_Data := (Gui.Router.Request_Data with The_Window => For_Window,
                                                                 Metrics    => (others => 0));
      begin
        Gui.Router.Request (Data);
        return Data.Metrics;
      end;
    else
      return Metrics_At_Close;
    end if;
  end Get_Window_Metrics;


  type Hide_Data is new Message_Data with record
    The_Window : Gtk.Window.Gtk_Window;
  end record;

  overriding
  procedure Asynchronous_Service (Data : Hide_Data) is
  begin
    Gtk.Window.Hide (Data.The_Window);
  end Asynchronous_Service;


  function Close_Window (The_Window : access Gtk.Window.Gtk_Window_Record'class) return Boolean is
  begin
    if The_State = Active then
      The_Termination_Handler.Start (Gtk.Window.Gtk_Window(The_Window)); -- Start termination
    end if;
    return True; -- Don't destroy the main window.
  end Close_Window;


  procedure Close (The_Window : Gtk.Window.Gtk_Window) is
    Unused : Boolean;
  begin
    Unused := Close_Window (The_Window);
  end Close;


  task body Termination_Handler is
    Closed_Window : Gtk.Window.Gtk_Window;
  begin
    accept Start (The_Window : Gtk.Window.Gtk_Window) do
      Closed_Window := The_Window;
    end Start;
    declare -- Position is lost when winow is hidden! So save the metrics in case user asks for them afterwards.
      Data : Get_Metrics_Data := (Gui.Router.Request_Data with The_Window => Closed_Window,
                                                               Metrics    => (others => <>));
    begin
      Request (Data);
      Metrics_At_Close := Data.Metrics;
    end;
    The_State := Closing;
    declare
      Data : constant Hide_Data := (Message_Data with The_Window => Closed_Window);
    begin
      Send (Data);
    end;
    if The_Termination_Routine /= null then -- call user supplied termination routine
      Log.Write ("Calling termination routine");
      The_Termination_Routine.all;
      Log.Write ("Returned after termination routine");
    end if;
    Callback_Handling.Finish;   -- Signal action handler to terminate
    accept Finalize; -- Wait for callback handler to terminate
    Gateway.Quit;
  exception
  when Event: others =>
    Log.Write ("Termination_Handler", Event);
  end Termination_Handler;


  task body Callback_Handler is
    The_Callback : Callback_Data;
  begin
    loop
      Callback_Handling.Get (The_Callback);
      case The_Callback.The_Callback is
      when Action =>
        exit when The_Callback.The_Action = null;
        The_Callback.The_Action.all;
      when Click =>
        The_Callback.The_Click.all (The_Callback.The_Information);
      when Key =>
        The_Callback.The_Handler.all (The_Callback.The_Event, The_Callback.The_Code);
      end case;
    end loop;
    The_Termination_Handler.Finalize; -- Inform termination hanlder that we are done.
  exception
  when Event: others =>
    Log.Write ("Action_Handler", Event);
  end Callback_Handler;


  task body Startup is
    The_Main_Window : Gtk.Window.Gtk_Window;
  begin
    accept Start (Window : Gtk.Window.Gtk_Window) do
      The_Main_Window := Window;
    end Start;
    Startup_Routine.all (The_Main_Window);
    Log.Write ("GUI Startup task terminated");
  exception
  when Event: others =>
    Log.Write ("Startup", Event);
  end Startup;


  procedure Request (Data : in out Request_Data'class) is
    use type Ada.Task_Identification.Task_Id;
  begin
    if The_State = Inactive then
      raise Sequence_Error;
    elsif The_Gtk_Task = Ada.Task_Identification.Current_Task then
      Synchronous_Service (Data);
    else
      Gateway.Synchronous_Request (Data);
    end if;
  exception
  when Event : others =>
    Log.Write ("Gui.Router.Request", Event);
    raise;
  end Request;


  procedure Send (Message : Message_Data'class) is
    use type Ada.Task_Identification.Task_Id;
  begin
    if The_State = Inactive then
      raise Sequence_Error;
    elsif The_Gtk_Task = Ada.Task_Identification.Current_Task then
      Asynchronous_Service (Message);
    else
      Gateway.Asynchronous_Request (Message);
    end if;
  exception
  when Event : others =>
    Log.Write ("Gui.Router.Send", Event);
    raise;
  end Send;


  procedure Execute (Application_Name    : String;
                     Startup_Routine     : access procedure (Window : Gtk.Window.Gtk_Window);
                     Termination_Routine : Action_Routine;
                     Period              : Duration) is
    The_Main_Window : Gtk.Window.Gtk_Window;
    The_Period      : Duration;
  begin
    if The_State /= Inactive then
      raise Sequence_Error;
    end if;
    The_State := Active;
    Gateway.Reset;

    Gtk.Window.Gtk_New (The_Main_Window);
    Gtk.Window.Set_Title (The_Main_Window, Application_Name);

    The_Callback_Handler := new Callback_Handler;

    The_Termination_Routine := Termination_Routine;
    The_Termination_Handler := new Termination_Handler;

    Window_Callback.Connect (The_Main_Window, "delete-event", Close_Window'access);

    The_Period := Period;

    The_Gtk_Task := Ada.Task_Identification.Current_Task;

    Startup_Task := new Startup (Startup_Routine);  -- Task to call the Startup_Routine
    Startup_Task.Start (The_Main_Window);
    begin  -- Main processing loop
      declare
        The_Data_Type  : Data_Type;
        Unused_Boolean : Boolean;
      begin
        loop
          while Gtk.Main.Events_Pending loop
            Unused_Boolean := Gtk.Main.Main_Iteration;
          end loop;
--
-- The followiing loop would be more elegant but for some reason doesn't work using GPL-2014 (Gtk 3.8.2)
--          loop
--            exit when Gtk.Main.Main_Iteration_Do (False);
--          end loop;
          select
            Gateway.Check (The_Data_Type);
            case The_Data_Type is
            when Killed =>
              The_State := Inactive;
              Gtk.Widget.Destroy (Gtk.Widget.Gtk_Widget(The_Main_Window));
              exit;
            when Synchronous =>
              begin
                Synchronous_Service (Gateway.Synchronous_Data.all);
                Gateway.Complete_Synchronous_Service;
              exception
              when Event : others =>
                Gateway.Abort_Service (Event);
              end;
              when Asynchronous =>
                Asynchronous_Service (Gateway.Next_Message);
                Gateway.Delete_First_Message;
            end case;
          or
            delay The_Period;
          end select;
        end loop;
      end;
    end;
  end Execute;


  end Gui.Router;
