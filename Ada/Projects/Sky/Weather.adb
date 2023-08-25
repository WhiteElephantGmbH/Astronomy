-- *********************************************************************************************************************
-- *                           (c) 2023 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Angle;
with AWS.Client;
with AWS.Response;
with GNATCOLL.JSON;
with Persistent_String;
with Site;
with Strings;
with Traces;

package body Weather is

  package Log is new Traces ("Weather");

  package JS renames GNATCOLL.JSON;

  package Persistent_Key is new Persistent_String ("Weather_Key");

  Weather_Key : Persistent_Key.Data;

  function Actual_Key return String is
  begin
    if Weather_Key.Item = "" then
      Weather_Key.Store ("a709155f006b0d9b2886d37beeb1f0b0");
    end if;
    return Weather_Key.Item;
  end Actual_Key;

  Api_Key : constant String := Actual_Key;

  Is_Defined       : Boolean := False;
  The_Temperature  : Refraction.Celsius;
  The_Air_Pressure : Refraction.Hectopascal;


  function Url return String is

    function Image_Of (Item : Angle.Degrees) return String is
      Small : constant := 10.0**(-7);
      type Value is delta Small range -((2 ** 63 - 1) * Small) .. +((2 ** 63 - 1) * Small);
    begin
      return Strings.Trimmed (Value(Item)'image);
    end Image_Of;

    use type Angle.Value;
    Latitude  : constant Angle.Degrees := +Site.Latitude;
    Longitude : constant Angle.Degrees := +Site.Longitude;

  begin -- Url
    return "https://api.openweathermap.org/data/2.5/weather?units=metric"&
           "&lat=" & Image_Of (Latitude) &
           "&lon=" & Image_Of (Longitude) &
           "&appid=" & Api_Key;
  end Url;


  function Requested return Boolean is
    Ok : constant := 200;
  begin
    Is_Defined := False;
    declare
      Request_Data : constant String := AWS.Response.Message_Body (AWS.Client.Get (Url));
      Json_Data    : constant JS.JSON_Value := JS.Read (Request_Data);
      Cod          : constant Integer := Json_Data.Get ("cod");
    begin
      if Cod = Ok then
        declare
          Name     : constant String := Json_Data.Get ("name");
          Main     : constant JS.JSON_Value := Json_Data.Get ("main");
        begin
          Log.Write ("request from " & (if Name = "" then "unknown" else Name));
          The_Air_Pressure := Refraction.Hectopascal(Integer'(Main.Get ("pressure")));
          Log.Write ("  air pressure:" & The_Air_Pressure'image);
          The_Temperature := Refraction.Celsius(Float'(Main.Get ("temp")));
          Log.Write ("  temperature :" & The_Temperature'image);
          Is_Defined := True;
        exception when others =>
          Log.Error ("failed to get main data");
        end;
      else
        Log.Write ("request error code:" & Cod'image);
      end if;
    end;
    return Is_Defined;
  exception
  when others =>
    Log.Error ("request failed");
    return False;
  end Requested;


  function Temperature return Refraction.Celsius is
  begin
    if not Is_Defined then
      raise Program_Error;
    end if;
    return The_Temperature;
  end Temperature;


  function Air_Pressure return Refraction.Hectopascal is
  begin
    if not Is_Defined then
      raise Program_Error;
    end if;
    return The_Air_Pressure;
  end Air_Pressure;

end Weather;
