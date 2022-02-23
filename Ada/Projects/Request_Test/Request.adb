-- *********************************************************************************************************************
-- *                           (c) 2021 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Command_Line;
with Ada.Text_IO;
with AWS.Client;
with AWS.Response;
with Traces;

package body Request is

  package Log is new Traces ("Request");

  function Http_Url (Target : String) return String is
  begin
    return "http://217.160.64.198:5000/ZzRW8sYHdHrgZGG3?tele=apo&target=" & Target;
  end Http_Url;

  function Https_Url (Target : String) return String is
  begin
    return "https://ssd.jpl.nasa.gov/api/horizons.api"
         & "?MAKE_EPHEM=YES"
         & "&COMMAND='" & Target & "'"
         & "&EPHEM_TYPE=OBSERVER"
         & "&CENTER='coord@399'"
         & "&COORD_TYPE=GEODETIC"
         & "&SITE_COORD='8.60986388888,47.70550277777,0.54'"
         & "&START_TIME='2022-02-10'"
         & "&STOP_TIME='2022-02-11'"
         & "&STEP_SIZE='1 HOURS'"
         & "&QUANTITIES='1'"
         & "&REF_SYSTEM=ICRF"
         & "&CAL_FORMAT=BOTH"
         & "&TIME_DIGITS=MINUTES"
         & "&ANG_FORMAT=DEG"
         & "&APPARENT=AIRLESS"
         & "&RANGE_UNITS=AU"
         & "&SUPPRESS_RANGE_RATE=NO"
         & "&ELEV_CUT='0'"
         & "&SKIP_DAYLT=NO"
         & "&SOLAR_ELONG='0,180'"
         & "&EXTRA_PREC=NO"
         & "&R_T_S_ONLY=NO"
         & "&CSV_FORMAT=YES"
         & "&OBJ_DATA=NO";
  end Https_Url;


  procedure Handle (Url : String) is

    The_Response : AWS.Response.Data;

  begin -- Handle
    Ada.Text_IO.Put_Line ("Get Request for: " & Url);
    The_Response := AWS.Client.Get (Url);
    Ada.Text_IO.Put_Line ("Response: " & AWS.Response.Message_Body (The_Response));
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
      Handle (Http_Url (Target));
      Handle (Https_Url (Target));
    end;
  exception
  when Occurrence: others =>
    Ada.Text_IO.Put_Line ("Failed");
    Log.Termination (Occurrence);
  end Work;

end Request;
