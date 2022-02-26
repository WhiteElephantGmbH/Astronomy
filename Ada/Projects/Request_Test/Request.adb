-- *********************************************************************************************************************
-- *                       (c) 2021 .. 2022 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Command_Line;
with Ada.Text_IO;
--with AWS.Client;
--with AWS.Response;
with Os.Process;
with Strings;
with Traces;

package body Request is

  package Log is new Traces ("Request");

  --function Http_Url (Target : String) return String is
  --begin
  --  return "http://217.160.64.198:5000/ZzRW8sYHdHrgZGG3?tele=apo&target=" & Target;
  --end Http_Url;

  function Https_Url (Target : String) return String is
  begin
    return "https://ssd.jpl.nasa.gov/api/horizons.api"
         & "?MAKE_EPHEM=YES"
         & "&COMMAND='" & Target & "'"
         & "&EPHEM_TYPE=OBSERVER"
         & "&CENTER='coord@399'"
         & "&COORD_TYPE=GEODETIC"
         & "&SITE_COORD='8.60986388888,47.70550277777,0.54'"
         & "&START_TIME='2022-02-27'"
         & "&STOP_TIME='2022-02-28'"
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
    --The_Response : AWS.Response.Data;
  begin
    Ada.Text_IO.Put_Line ("Get Request for: " & Url);
    declare
      Result : constant String := Os.Process.Execution_Of (Executable => "D:\Request_Get.exe",
                                                           Parameters => '"' & Url & '"');
      Response0 : constant String := Strings.Trimmed (Result);
      pragma Style_Checks ("M1800");
      Response1 : constant String := "{""signature"":{""source"":""NASA/JPL Horizons API"",""version"":""1.1""},""result"":""*******************************************************************************\nJPL/DASTCOM            Small-body Index Search Results     2022-Feb-26 05:29:33\n\n Comet and asteroid index search:\n\n   NAME = BORRELLY;\n\n Matching small-bodies: \n\n    record #  Epoch-yr  Primary Desig  >MATCH NAME<\n    --------  --------  -------------  -------------------------\n        1539            1940 UB         Borrelly\n    90000292    1905    19P             Borrelly\n    90000293    1911    19P             Borrelly\n    90000294    1918    19P             Borrelly\n    90000295    1925    19P             Borrelly\n    90000296    1932    19P             Borrelly\n    90000297    1953    19P             Borrelly\n    90000298    1960    19P             Borrelly\n    90000299    1967    19P             Borrelly\n    90000300    1974    19P             Borrelly\n    90000301    1981    19P             Borrelly\n    90000302    1987    19P             Borrelly\n    90000303    1994    19P             Borrelly\n    90000304    2004    19P             Borrelly\n    90000305    2015    19P             Borrelly\n    90001642    1873    C/1873 Q1       Borrelly\n    90001647    1874    C/1874 O1       Borrelly\n    90001649    1874    C/1874 X1       Borrelly\n    90001650    1877    C/1877 C1       Borrelly\n    90001697    1890    C/1889 X1       Borrelly\n    90001737    1903    C/1903 M1       Borrelly\n    90001760    1912    C/1912 V1       Borrelly\n\n (22 matches. To SELECT, enter record # (integer), followed by semi-colon.)\n*******************************************************************************\n""}";
      pragma Style_Checks ("M120");
    begin
     Ada.Text_IO.Put_Line ("Response: " & Response0);
     Ada.Text_IO.Put_Line ("Response: " & Response1);
    end;
    --The_Response := AWS.Client.Get (Url);
    --Ada.Text_IO.Put_Line ("Response: " & AWS.Response.Message_Body (The_Response));
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
      --Handle (Http_Url (Target));
      Handle (Https_Url (Target));
    end;
  exception
  when Occurrence: others =>
    Ada.Text_IO.Put_Line ("Failed");
    Log.Termination (Occurrence);
  end Work;

end Request;
