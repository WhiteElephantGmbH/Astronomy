-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with AWS.Client;
with AWS.Response;
with Os.Process;
with PWI4.Protocol;
with Strings;
with Traces;

package body PWI4 is

  package Log is new Traces ("PWI");

  The_Process_Id : Os.Process.Id;

  The_Ip_Address_And_Port : Strings.Element := ["127.0.0.1:8220"];


  function Image_Of (Item : Arc_Second) return String is
  begin
    return Strings.Trimmed (Item'image);
  end Image_Of;


  function Image_Of (Item : Degrees) return String is
  begin
    return Strings.Trimmed (Item'image);
  end Image_Of;


  function Image_Of (Item : Hours) return String is
  begin
    return Strings.Trimmed (Item'image);
  end Image_Of;


  function Startup (Filename   : String;
                    Ip_Address : String) return Boolean is
  begin
    The_Process_Id := Os.Process.Created (Filename);
    The_Ip_Address_And_Port := [Ip_Address];
    return True;
  exception
  when others =>
    return False;
  end Startup;


  procedure Shutdown is
  begin
    Os.Process.Terminate_With (The_Process_Id);
  exception
  when others =>
    Log.Write ("already terminated");
  end Shutdown;


  procedure Execute (Item : String) is
    use type Strings.Element;
    Url : constant String := "http://" & The_Ip_Address_And_Port;
  begin
    Log.Write ("Execute " & Item);
    Protocol.Parse (AWS.Response.Message_Body (AWS.Client.Get (Url & '/' & Item)));
  end Execute;


  procedure Get_System is
  begin
    Execute ("status");
  end Get_System;


  procedure Execute (Device     : String;
                     Command    : String;
                     Parameters : String := "") is
  begin
    Execute (Device & "/" & Command & (if Parameters /= "" then "?" & Parameters else ""));
  end Execute;


  function Image_Of (The_Port : Port) return Character is
  begin
    case The_Port is
    when Port_1 =>
      return '1';
    when Port_2 =>
      return '2';
    end case;
  end Image_Of;

end PWI4;
