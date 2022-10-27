-- *********************************************************************************************************************
-- *                           (c) 2022 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Command_Line;
with Ada.Text_IO;
with Exceptions;
with ENC_2302_Client;

package body Controller is

  package IO renames Ada.Text_IO;

  package ENC renames ENC_2302_Client;


  procedure Control (Ip : String) is
    The_Port    : ENC.Port;
    The_Switch  : ENC.Switch;
    Input_Error : exception;
  begin
    IO.Put_Line ("Control Ports on Host with IP: " & Ip);
    loop
      begin
        IO.Put_Line ("Actual Switches: " & ENC.Switches_Of (Ip)'image);
        IO.Put ("Port input: (1..4)] | ('q' | """") for quit > ");
        declare
          Input : constant String := IO.Get_Line;
        begin
          exit when Input in "q" | "";
          The_Port := ENC.Port'val(Integer'value(Input) - 1);
        exception
        when others =>
          IO.Put_Line ("<<<invalid port>>>");
          raise Input_Error;
        end;
        begin
          IO.Put ("Switch input: (On | Off) > ");
          The_Switch := ENC.Switch'value(IO.Get_Line);
        exception
        when others =>
          IO.Put_Line ("<<<invalid switch>>>");
          raise Input_Error;
        end;
        IO.Put_Line ("Set " & The_Port'image & " to " & The_Switch'image);
        ENC.Set (The_Port, The_Switch, Host => Ip);
      exception
      when Input_Error =>
        null;
      end;
    end loop;
  end Control;


  procedure Execute is
    Nr_Of_Arguments : constant Natural := Ada.Command_Line.Argument_Count;
  begin
    if Nr_Of_Arguments = 0 then
      Control ("192.168.10.160");
    elsif Nr_Of_Arguments = 1 then
      Control (Ada.Command_Line.Argument (1));
    else
      IO.Put_Line ("Incorrect number of parameters");
    end if;
  exception
  when Event: others =>
    IO.Put_Line (Exceptions.Information_Of (Event));
  end Execute;

end Controller;
