-- *********************************************************************************************************************
-- *                           (c) 2022 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with AWS.Client;
with AWS.Response;
with Log;

package body Os.Horizon is

  function Result_Of_Get_With (Item : Arguments) return String is

    Url : constant String := "https://ssd.jpl.nasa.gov/api/horizons.api"
                           & "?MAKE_EPHEM=YES"
                           & "&COMMAND='" & Item(1) & "'"
                           & "&EPHEM_TYPE=OBSERVER"
                           & "&CENTER='coord@399'"
                           & "&COORD_TYPE=GEODETIC"
                           & "&SITE_COORD='" & Item(2) & '''
                           & "&START_TIME='" & Item(3) & '''
                           & "&STOP_TIME='" & Item(4) & '''
                           & "&STEP_SIZE='" & Item(5) & '''
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

     Result : constant String := AWS.Response.Message_Body (AWS.Client.Get (Url));

  begin -- Result_Of_Get_With
    Log.Write ("URL<<<" & Url & ">>>");
    Log.Write ("GOT<<<" & Result & ">>>");
    return Result;
  end Result_Of_Get_With;

end Os.Horizon;
