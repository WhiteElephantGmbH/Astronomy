-- *********************************************************************************************************************
-- *                       (c) 2021 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Command_Line;
with Ada.Text_IO;
with Os.Horizon;
with Traces;
with Text;

package body Request is

  package Log is new Traces ("Request");

  procedure Handle (Target : String) is

    procedure Evaluate (Item : String) is

      The_Index : Natural := Item'first;
      The_First : Natural;

      function Next_Line return String is
      begin
        while The_Index < Item'last loop
          if Item(The_Index .. The_Index + 1) = "\n" then
            The_Index := The_Index + 2;
            return Item(The_First .. The_Index - 3);
          end if;
          The_Index := The_Index + 1;
        end loop;
        The_Index := Item'last + 1;
        return Item(The_First..Item'last);
      end Next_Line;

    begin -- Evaluate
      if Text.Location_Of ("Traceback", Item) = Item'first then
        Ada.Text_IO.Put_Line (Item);
        Ada.Text_IO.Put_Line ("<<< ERROR: No Connection to Nasa Horizon >>>");
        return;
      end if;
      Ada.Text_IO.Put_Line ("<<< cleaned result >>>");
      while The_Index <= Item'last loop
        The_First := The_Index;
        declare
          Line : constant String := Next_Line;
        begin
          Ada.Text_IO.Put_Line (Line);
        end;
      end loop;
    end Evaluate;

  begin -- Handle
    Ada.Text_IO.Put_Line ("Get Request for: " & Target);
    declare
      Site   : constant String := "8.60986388888,47.70550277777,0.54";
      Start  : constant String := "2022-02-28";
      Stop   : constant String := "2022-03-06";
      Step   : constant String := "60m";
      Result : constant String := Os.Horizon.Result_Of_Get_With ([Target, Site, Start, Stop, Step]);
    begin
      Ada.Text_IO.Put_Line ("<<< result >>>");
      Ada.Text_IO.Put_Line (Result);
      Evaluate (Result);
      Ada.Text_IO.Put_Line ("<<< complete >>>");
    end;
  exception
  when Occurrence: others =>
    Ada.Text_IO.Put_Line ("Failed");
    Log.Termination (Occurrence);
  end Handle;


  procedure Work is
  begin
    if Ada.Command_Line.Argument_Count /= 1 then
      Ada.Text_IO.Put_Line ("Target missing");
      return;
    end if;
    declare
      Target : constant String := Ada.Command_Line.Argument(1);
    begin
      Ada.Text_IO.Put_Line ("Request: " & Target);
      Handle (Target);
    end;
  exception
  when Occurrence: others =>
    Ada.Text_IO.Put_Line ("Failed");
    Log.Termination (Occurrence);
  end Work;

end Request;
