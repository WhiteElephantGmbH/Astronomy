-- *********************************************************************************************************************
-- *                           (c) 2019 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Text;
with Traces;
with Strings;

package body PWI.XML is

  package Log is new Traces ("Xml");

  function Zero_And (Item : String) return String is
  begin
    return '0' & Strings.Trimmed (Item);
  end Zero_And;


  function Boolean_Of (Image : String) return Boolean is
  begin
    return Boolean'value(Strings.Uppercase_Of (Image));
  exception
  when others =>
    raise Parsing_Error;
  end Boolean_Of;


  function Image_Of (Item : Boolean) return String is
  begin
    return Strings.Legible_Of (Item'img);
  end Image_Of;


  function Time_Of (Image : String) return Universal_Time is
  begin
    if Image = "" then
      return Universal_Time'(Hours   => 0,
                             Minutes => 0,
                             Seconds => 0);
    else
      return Universal_Time'(Hours   =>   Hour'value(Image(Image'first .. Image'first + 1)),
                             Minutes => Minute'value(Image(Image'first + 3 .. Image'first + 4)),
                             Seconds => Second'value(Image(Image'first + 6 .. Image'first + 7)));
    end if;
  exception
  when others =>
    raise Parsing_Error;
  end Time_Of;


  function Image_Of (Item : Universal_Time) return String is
    Hours   : constant String := Zero_And (Item.Hours'img);
    Minutes : constant String := Zero_And (Item.Minutes'img);
    Seconds : constant String := Zero_And (Item.Seconds'img);
  begin
    return Hours(Hours'last - 1 .. Hours'last) & ':' &
           Minutes(Minutes'last - 1 .. Minutes'last) & ':' &
           Seconds(Seconds'last - 1 .. Seconds'last);
  end Image_Of;


  function Time_Of (Image : String) return Time is
  begin
    if Image = "" then
      return Time'(Hours         => 0,
                   Minutes       => 0,
                   Seconds       => 0,
                   Milli_Seconds => 0);
    else
      return Time'(Hours         =>         Hour'value(Image(Image'first .. Image'last - 10)),
                   Minutes       =>       Minute'value(Image(Image'last - 8 .. Image'last - 7)),
                   Seconds       =>       Second'value(Image(Image'last - 5 .. Image'last - 4)),
                   Milli_Seconds => Milli_Second'value(Image(Image'last - 2 .. Image'last)));
    end if;
    exception
  when others =>
    raise Parsing_Error;
  end Time_Of;


  function Image_Of (Item : Time) return String is
    Minutes       : constant String := Zero_And (Item.Minutes'img);
    Seconds       : constant String := Zero_And (Item.Seconds'img);
    Milli_Seconds : constant String := '0' & Zero_And (Item.Milli_Seconds'img);
  begin
    return Strings.Trimmed (Item.Hours'img) & ' ' &
           Minutes(Minutes'last - 1 .. Minutes'last) & ' ' &
           Seconds(Seconds'last - 1 .. Seconds'last) & '.' &
           Milli_Seconds(Milli_Seconds'last - 2 .. Milli_Seconds'last);
  end Image_Of;


  function Jd_Of (Image : String) return Julian_Day is
  begin
    return Julian_Day'value(Image);
  exception
  when others =>
    begin
      return Julian_Day'value(Image & ".0");
    exception
    when others =>
      raise Parsing_Error;
    end;
  end Jd_Of;


  function Image_Of (Item : Julian_Day) return String is
  begin
    return Strings.Trimmed (Item'img);
  end Image_Of;


  function Focuser_Position_Of (Image : String) return Focuser_Position is
  begin
    return Focuser_Position'value(Image);
  exception
  when others =>
    raise Parsing_Error;
  end Focuser_Position_Of;


  function Image_Of (Item : Focuser_Position) return String is
  begin
    return Strings.Trimmed (Item'img);
  end Image_Of;


  function Rotator_Position_Of (Image : String) return Rotator_Position is
  begin
    return Rotator_Position'value(Image);
  exception
  when others =>
    raise Parsing_Error;
  end Rotator_Position_Of;


  function Image_Of (Item : Rotator_Position) return String is
  begin
    return Strings.Trimmed (Item'img);
  end Image_Of;


  function Declination_Of (Image : String) return Declination is
    Sign_Is_Positive : Boolean;
  begin
    case Image(Image'first) is
    when '+' =>
      Sign_Is_Positive := True;
    when '-' =>
      Sign_Is_Positive := False;
    when others =>
      raise Parsing_Error;
    end case;
    return Declination'(Is_Positive   => Sign_Is_Positive,
                        Degrees       =>       Degree'value(Image(Image'first + 1 .. Image'last - 9)),
                        Minutes       =>       Minute'value(Image(Image'last - 7 .. Image'last - 6)),
                        Seconds       =>       Second'value(Image(Image'last - 4 .. Image'last - 3)),
                        Centi_Seconds => Centi_Second'value(Image(Image'last - 1 .. Image'last)));
  exception
  when others =>
    raise Parsing_Error;
  end Declination_Of;


  function Image_Of (Item : Declination) return String is
    Minutes       : constant String := Zero_And (Item.Minutes'img);
    Seconds       : constant String := Zero_And (Item.Seconds'img);
    Centi_Seconds : constant String := Zero_And (Item.Centi_Seconds'img);
  begin
    return (if Item.Is_Positive then '+' else '-') & Strings.Trimmed (Item.Degrees'img) & ' ' &
           Minutes(Minutes'last - 1 .. Minutes'last) & ' ' &
           Seconds(Seconds'last - 1 .. Seconds'last) & '.' &
           Centi_Seconds(Centi_Seconds'last - 1 .. Centi_Seconds'last);
  end Image_Of;


  function Radian_Of (Image : String) return Radian is
  begin
    return Radian'value(Image);
  exception
  when others =>
    raise Parsing_Error;
  end Radian_Of;


  function Image_Of (Item : Radian) return String is
  begin
    return Strings.Trimmed (Item'img);
  end Image_Of;


  function Arc_Second_Of (Image : String) return Arc_Second is
  begin
    return Arc_Second'value(Image);
  exception
  when others =>
    raise Parsing_Error;
  end Arc_Second_Of;


  function Image_Of (Item : Arc_Second) return String is
  begin
    return Strings.Trimmed (Item'img);
  end Image_Of;


  function Error_Code_Of (Image : String) return Error_Code is
  begin
    return Error_Code'value(Image);
  exception
  when others =>
    raise Parsing_Error;
  end Error_Code_Of;


  function Image_Of (Item : Error_Code) return String is
  begin
    return Strings.Trimmed (Item'img);
  end Image_Of;


  function Port_Number_Of (Image : String) return Port_Number is
  begin
    return Port_Number'value(Image);
  exception
  when others =>
    raise Parsing_Error;
  end Port_Number_Of;


  function Image_Of (Item : Port_Number) return String is
  begin
    return Strings.Trimmed (Item'img);
  end Image_Of;


  function M3_Position_Of (Image : String) return M3_Position is
  begin
    return M3_Position'value(Image);
  exception
  when others =>
    raise Parsing_Error;
  end M3_Position_Of;


  function Image_Of (Item : M3_Position) return String is
  begin
    return Strings.Trimmed (Item'img);
  end Image_Of;


  function Degrees_Of (Image : String) return Celsius is
  begin
    if Strings.Found ('.', Image) then
      return Celsius'value(Image);
    else
      return Celsius'value(Image & ".0");
    end if;
  exception
  when others =>
    raise Parsing_Error;
  end Degrees_Of;


  function Image_Of (Item : Celsius) return String is
  begin
    if Item = Undefined then
      return "";
    end if;
    return Strings.Trimmed (Item'img);
  end Image_Of;


  protected System is

    procedure Set (Data : XML.Response);

    function Fans_Turned_On return Boolean;

    function Mount_Flags return XML.Mount_Flag;

    function Mount_Data return XML.Mount_Info;

    function M3_Data return XML.M3_Info;

    function Rotator_Data return XML.Rotator_Info;

    function Rotator1_Data return XML.Rotator_Info;

  private
    The_Data : XML.Response;
  end System;


  procedure Parse (Data : String) is

    The_Index : Integer := Data'first - 1;


    function Next_Character return Character is
    begin
      The_Index := The_Index + 1;
      if The_Index = Data'last then
        raise Parsing_Error;
      end if;
      return Data(The_Index);
    end Next_Character;


    type Token is (Header, End_Tag, Information, Tag);

    type Tags is (T_Altaz_Derotate,
                  T_Ambient,
                  T_Auto_Focus_Busy,
                  T_Auto_Focus_Last_Result_Success,
                  T_Auto_Focus_Last_Result_Position_Microns,
                  T_Auto_Focus_Last_Result_Tolerance_Microns,
                  T_Alt_Enabled,
                  T_Alt_Motor_Error_Code,
                  T_Alt_Motor_Error_Message,
                  T_Alt_Radian,
                  T_Alt_Rms_Error_Arcsec,
                  T_Azm_Enabled,
                  T_Azm_Motor_Error_Code,
                  T_Azm_Motor_Error_Message,
                  T_Azm_Radian,
                  T_Azm_Rms_Error_Arcsec,
                  T_Backplate,
                  T_Connected,
                  T_Dec,
                  T_Dec_Radian,
                  T_Dec_Target,
                  T_Dec_2000,
                  T_Encoders_Have_Been_Set,
                  T_Fans,
                  T_Finding_Home,
                  T_Focuser,
                  T_Focuser1,
                  T_Focuser2,
                  T_Goto_Complete,
                  T_Is_Finding_Home,
                  T_Jd,
                  T_Lst,
                  T_Mount,
                  T_Moving,
                  T_Moving_Rotate,
                  T_Moving_Tilt,
                  T_M3,
                  T_On,
                  T_On_Target,
                  T_Pointing_Model,
                  T_Port,
                  T_Position,
                  T_Position_Rotate,
                  T_Position_Tilt,
                  T_Primary,
                  T_Ra,
                  T_Ra_Radian,
                  T_Ra_Target,
                  T_Ra_2000,
                  T_Rotator,
                  T_Rotator1,
                  T_Rotator2,
                  T_Secondary,
                  T_Status,
                  T_System,
                  T_Temperature,
                  T_Tracking,
                  T_Unit,
                  T_Utc);

    The_Tag : Tags;

    The_Text_First : Integer;
    The_Text_Last  : Integer;


    function Next_Token return Token is

      function Next_Tag return Tags is
        First : constant Natural := The_Index + 1;
      begin
        while Next_Character /= '>' loop
          null;
        end loop;
        return Tags'value ("T_" & Strings.Uppercase_Of (Data(First .. The_Index - 1)));
      exception
      when others =>
        raise Parsing_Error;
      end Next_Tag;

      The_Character : Character;

    begin -- Next_Token
      case Next_Character is
      when '<' =>
        The_Character := Next_Character;
        case The_Character is
        when '?' =>
          loop
            if Next_Character = '?' then
              exit when Next_Character = '>';
            end if;
          end loop;
          return Header;
        when '/' =>
          The_Tag := Next_Tag;
          return End_Tag;
        when others =>
          The_Index := The_Index - 1;
          The_Tag := Next_Tag;
          return Tag;
        end case;
      when '>' =>
        raise Parsing_Error;
      when others =>
        The_Text_First := The_Index;
        while Next_Character /= '<' loop
          null;
        end loop;
        The_Text_Last := The_Index - 1;
        if Next_Character /= '/' then
          raise Parsing_Error;
        end if;
        if The_Tag /= Next_Tag then
          raise Parsing_Error;
        end if;
        return Information;
      end case;
    end Next_Token;


    function Parsed_Text return String is
    begin
      case Next_Token is
      when Information =>
        return Data(The_Text_First .. The_Text_Last);
      when End_Tag =>
        return "";
      when others =>
        raise Parsing_Error;
      end case;
    end Parsed_Text;


    procedure Check_Tag (Item : Tags) is
    begin
      if Item /= The_Tag then
        raise Parsing_Error;
      end if;
    end Check_Tag;


    function Checked_Tag (Item     : Tags;
                          With_Tag : Tags) return Boolean is
    begin
      if Item = With_Tag then
        return True;
      end if;
      raise Parsing_Error;
    end Checked_Tag;


    The_Response : Response;

    procedure Parse_Status is
    begin
      loop
        case Next_Token is
        when End_Tag =>
          Check_Tag (T_Status);
          exit;
        when Tag =>
          case The_Tag is
          when T_Utc =>
            The_Response.Status.Utc := Time_Of (Parsed_Text);
          when T_Lst =>
            The_Response.Status.Lst := Time_Of (Parsed_Text);
          when T_Jd =>
            The_Response.Status.Jd := Jd_Of (Parsed_Text);
          when others =>
            raise Parsing_Error;
          end case;
        when others =>
          raise Parsing_Error;
        end case;
      end loop;
    end Parse_Status;


    procedure Parse_Focuser (With_Tag    :        Tags;
                             The_Focuser : in out Focuser_Info) is
    begin
      loop
        case Next_Token is
        when End_Tag =>
          Check_Tag (With_Tag);
          exit;
        when Tag =>
          case The_Tag is
          when T_Connected =>
            The_Focuser.Connected := Boolean_Of (Parsed_Text);
          when T_Position =>
            The_Focuser.Position := Focuser_Position_Of (Parsed_Text);
          when T_Unit =>
            if Parsed_Text /= "Micron" then
              raise Parsing_Error;
            end if;
          when T_Moving =>
            The_Focuser.Moving := Boolean_Of (Parsed_Text);
          when T_Goto_Complete =>
            The_Focuser.Goto_Complete := Boolean_Of (Parsed_Text);
          when T_Finding_Home =>
            The_Focuser.Finding_Home := Boolean_Of (Parsed_Text);
          when T_Auto_Focus_Busy =>
            if Checked_Tag (With_Tag, T_Focuser) then
              The_Focuser.Auto_Focus_Busy := Boolean_Of (Parsed_Text);
            end if;
          when T_Auto_Focus_Last_Result_Success =>
            if Checked_Tag (With_Tag, T_Focuser) then
              The_Focuser.Auto_Focus_Last_Result_Success := Boolean_Of (Parsed_Text);
            end if;
          when T_Auto_Focus_Last_Result_Position_Microns =>
            if Checked_Tag (With_Tag, T_Focuser) then
              The_Focuser.Auto_Focus_Last_Result_Position := Focuser_Position_Of (Parsed_Text);
            end if;
          when T_Auto_Focus_Last_Result_Tolerance_Microns =>
            if Checked_Tag (With_Tag, T_Focuser) then
              The_Focuser.Auto_Focus_Last_Result_Tolerance := Focuser_Position_Of (Parsed_Text);
            end if;
          when others =>
            raise Parsing_Error;
          end case;
        when others =>
          raise Parsing_Error;
        end case;
      end loop;
    end Parse_Focuser;


    procedure Parse_Rotator (With_Tag    :        Tags;
                             The_Rotator : in out Rotator_Info) is
    begin
      loop
        case Next_Token is
        when End_Tag =>
          Check_Tag (With_Tag);
          exit;
        when Tag =>
          case The_Tag is
          when T_Connected =>
            The_Rotator.Connected := Boolean_Of (Parsed_Text);
          when T_Position =>
            The_Rotator.Position := Rotator_Position_Of (Parsed_Text);
          when T_Unit =>
            if Parsed_Text /= "Degree" then
              raise Parsing_Error;
            end if;
          when T_Moving =>
            The_Rotator.Moving := Boolean_Of (Parsed_Text);
          when T_Goto_Complete =>
            The_Rotator.Goto_Complete := Boolean_Of (Parsed_Text);
          when T_Finding_Home =>
            The_Rotator.Finding_Home := Boolean_Of (Parsed_Text);
          when T_Altaz_Derotate =>
            if Checked_Tag (T_Rotator, With_Tag) then
              The_Rotator.Alt_Az_Derotate := Boolean_Of (Parsed_Text);
            end if;
          when others =>
            raise Parsing_Error;
          end case;
        when others =>
          raise Parsing_Error;
        end case;
      end loop;
    end Parse_Rotator;


    procedure Parse_Mount is

      procedure Handle_Motor_Error (Axis : String) is
        Error_Message : constant String := Parsed_Text;
      begin
        if Error_Message /= "No error" then
          Log.Error (Axis & "_Motor_Error: " & Error_Message);
        end if;
      end Handle_Motor_Error;

      function Pointing_Model_Has_Been_Set return Boolean is
        Pointing_Model : constant String := Parsed_Text;
      begin
        if Pointing_Model = "" then
          return False;
        end if;
        return Text.Is_Equal (Pointing_Model, Mount.Defined_Pointing_Model);
      end Pointing_Model_Has_Been_Set;

    begin -- Parse_Mount
      loop
        case Next_Token is
        when End_Tag =>
          Check_Tag (T_Mount);
          exit;
        when Tag =>
          case The_Tag is
          when T_Connected =>
            The_Response.Mount.Flags.Connected := Boolean_Of (Parsed_Text);
          when T_On_Target =>
            The_Response.Mount.Flags.On_Target := Boolean_Of (Parsed_Text);
          when T_Moving =>
            declare
              Unused : constant String := Parsed_Text;
            begin
              null;
            end;
          when T_Tracking =>
            The_Response.Mount.Flags.Tracking := Boolean_Of (Parsed_Text);
          when T_Ra =>
            The_Response.Mount.Ra := Time_Of (Parsed_Text);
          when T_Dec =>
            The_Response.Mount.Dec := Declination_Of (Parsed_Text);
          when T_Ra_Target =>
            The_Response.Mount.Ra_Target := Time_Of (Parsed_Text);
          when T_Dec_Target =>
            The_Response.Mount.Dec_Target := Declination_Of (Parsed_Text);
          when T_Ra_2000 =>
            The_Response.Mount.Ra_2000 := Time_Of (Parsed_Text);
          when T_Dec_2000 =>
            The_Response.Mount.Dec_2000 := Declination_Of (Parsed_Text);
          when T_Ra_Radian =>
            The_Response.Mount.Ra_Radian := Radian_Of (Parsed_Text);
          when T_Dec_Radian =>
            The_Response.Mount.Dec_Radian := Radian_Of (Parsed_Text);
          when T_Azm_Radian =>
            The_Response.Mount.Azm_Radian := Radian_Of (Parsed_Text);
          when T_Alt_Radian =>
            The_Response.Mount.Alt_Radian := Radian_Of (Parsed_Text);
          when T_Azm_Rms_Error_Arcsec =>
            The_Response.Mount.Azm_Rms_Error := Arc_Second_Of (Parsed_Text);
          when T_Alt_Rms_Error_Arcsec =>
            The_Response.Mount.Alt_Rms_Error := Arc_Second_Of (Parsed_Text);
          when T_Azm_Motor_Error_Code =>
            The_Response.Mount.Azm_Motor_Error_Code := Error_Code_Of (Parsed_Text);
          when T_Alt_Motor_Error_Code =>
            The_Response.Mount.Alt_Motor_Error_Code := Error_Code_Of (Parsed_Text);
          when T_Azm_Motor_Error_Message =>
            Handle_Motor_Error ("Azm");
          when T_Alt_Motor_Error_Message =>
            Handle_Motor_Error ("Alt");
          when T_Azm_Enabled =>
            The_Response.Mount.Flags.Azm_Enabled := Boolean_Of (Parsed_Text);
          when T_Alt_Enabled =>
            The_Response.Mount.Flags.Alt_Enabled := Boolean_Of (Parsed_Text);
          when T_Pointing_Model =>
            The_Response.Mount.Flags.Pointing_Model_Set := Pointing_Model_Has_Been_Set;
          when T_Is_Finding_Home =>
            The_Response.Mount.Flags.Is_Finding_Home := Boolean_Of (Parsed_Text);
          when T_Encoders_Have_Been_Set =>
            The_Response.Mount.Flags.Encoders_Have_Been_Set := Boolean_Of (Parsed_Text);
          when others =>
            raise Parsing_Error;
          end case;
        when others =>
          raise Parsing_Error;
        end case;
      end loop;
    end Parse_Mount;


    procedure Parse_M3 is
    begin
      loop
        case Next_Token is
        when End_Tag =>
          Check_Tag (T_M3);
          exit;
        when Tag =>
          case The_Tag is
          when T_Connected =>
            The_Response.M3.Connected := Boolean_Of (Parsed_Text);
          when T_Port =>
            The_Response.M3.Port := Port_Number_Of (Parsed_Text);
          when T_Position_Rotate =>
            The_Response.M3.Position_Rotate := M3_Position_Of (Parsed_Text);
          when T_Position_Tilt =>
            The_Response.M3.Position_Tilt := M3_Position_Of (Parsed_Text);
          when T_Unit =>
            if Parsed_Text /= "Degree" then
              raise Parsing_Error;
            end if;
          when T_Moving_Rotate =>
            The_Response.M3.Moving_Rotate := Boolean_Of (Parsed_Text);
          when T_Moving_Tilt =>
            The_Response.M3.Moving_Tilt := Boolean_Of (Parsed_Text);
          when others =>
            raise Parsing_Error;
          end case;
        when others =>
          raise Parsing_Error;
        end case;
      end loop;
    end Parse_M3;


    procedure Parse_Temperature is
    begin
      loop
        case Next_Token is
        when End_Tag =>
          Check_Tag (T_Temperature);
          exit;
        when Tag =>
          case The_Tag is
          when T_Primary =>
            The_Response.Temperature.Primary := Degrees_Of (Parsed_Text);
          when T_Ambient =>
            The_Response.Temperature.Ambient := Degrees_Of (Parsed_Text);
          when T_Secondary =>
            The_Response.Temperature.Secondary := Degrees_Of (Parsed_Text);
          when T_Backplate =>
            The_Response.Temperature.Backplate := Degrees_Of (Parsed_Text);
          when T_M3 =>
            The_Response.Temperature.M3 := Degrees_Of (Parsed_Text);
          when others =>
            raise Parsing_Error;
          end case;
        when others =>
          raise Parsing_Error;
        end case;
      end loop;
    end Parse_Temperature;


    procedure Parse_Fans is
    begin
      loop
        case Next_Token is
        when End_Tag =>
          Check_Tag (T_Fans);
          exit;
        when Tag =>
          case The_Tag is
          when T_On =>
            The_Response.Fans.On := Boolean_Of (Parsed_Text);
          when others =>
            raise Parsing_Error;
          end case;
        when others =>
          raise Parsing_Error;
        end case;
      end loop;
    end Parse_Fans;

  begin -- Parse
    Log.Write (Data);
    if Data'length = 0 or else Data(Data'last) /= Ascii.Lf then
      raise Parsing_Error;
    end if;
    if Next_Token /= Header then
      raise Parsing_Error;
    end if;
    if Next_Token /= Tag then
      raise Parsing_Error;
    end if;
    Check_Tag (T_System);
    loop
      case Next_Token is
      when End_Tag =>
        Check_Tag (T_System);
        exit;
      when Tag =>
        case The_Tag is
        when T_Status =>
          Parse_Status;
        when T_Focuser =>
          Parse_Focuser (T_Focuser, The_Response.Focuser);
        when T_Focuser1 =>
          Parse_Focuser (T_Focuser1, The_Response.Focuser1);
        when T_Focuser2 =>
          Parse_Focuser (T_Focuser2, The_Response.Focuser2);
        when T_Rotator =>
          Parse_Rotator (T_Rotator, The_Response.Rotator);
        when T_Rotator1 =>
          Parse_Rotator (T_Rotator1, The_Response.Rotator1);
        when T_Rotator2 =>
          Parse_Rotator (T_Rotator2, The_Response.Rotator2);
        when T_Mount =>
          Parse_Mount;
        when T_M3 =>
          Parse_M3;
        when T_Temperature =>
          Parse_Temperature;
        when T_Fans =>
          Parse_Fans;
        when others =>
          raise Parsing_Error;
        end case;
      when others =>
        raise Parsing_Error;
      end case;
    end loop;
    System.Set (The_Response);
  end Parse;


  package body Fans is

    function Turned_On return Boolean is
    begin
      return System.Fans_Turned_On;
    end Turned_On;

  end Fans;

  package body Mount is

    The_Pointing_Model : Text.String;

    procedure Define_Pointing_Model (Filename : String) is
    begin
      The_Pointing_Model := Text.String_Of (Filename);
    end Define_Pointing_Model;


    function Defined_Pointing_Model return String is
    begin
      return Text.String_Of (The_Pointing_Model);
    end Defined_Pointing_Model;


    function Flags return Mount_Flag is
    begin
      return System.Mount_Flags;
    end Flags;


    function Info return Mount_Info is
    begin
      return System.Mount_Data;
    end Info;

  end Mount;


  package body M3 is

    function Info return M3_Info is
    begin
      return System.M3_Data;
    end Info;

  end M3;


  package body Rotator is

    function Info return Rotator_Info is
    begin
      return System.Rotator_Data;
    end Info;

    function Info1 return Rotator_Info is
    begin
      return System.Rotator1_Data;
    end Info1;

  end Rotator;


  protected body System is

    procedure Set (Data : Response) is
    begin
      The_Data := Data;
      if Log.Is_Enabled then
        Log.Write ("Status");
        Log.Write ("  Utc        : " & XML.Image_Of (Data.Status.Utc));
        Log.Write ("  Lst        : " & XML.Image_Of (Data.Status.Lst));
        Log.Write ("  Julian Day : " & XML.Image_Of (Data.Status.Jd));
        Log.Write ("Focuser");
        Log.Write ("  Connected                        : " & Image_Of (Data.Focuser.Connected));
        Log.Write ("  Position                         : " & Image_Of (Data.Focuser.Position));
        Log.Write ("  Moving                           : " & Image_Of (Data.Focuser.Moving));
        Log.Write ("  Goto_Complete                    : " & Image_Of (Data.Focuser.Goto_Complete));
        Log.Write ("  Finding_Home                     : " & Image_Of (Data.Focuser.Finding_Home));
        Log.Write ("  Auto_Focus_Busy                  : " & Image_Of (Data.Focuser.Auto_Focus_Busy));
        Log.Write ("  Auto_Focus_Last_Result_Success   : " & Image_Of (Data.Focuser.Auto_Focus_Last_Result_Success));
        Log.Write ("  Auto_Focus_Last_Result_Position  : " & Image_Of (Data.Focuser.Auto_Focus_Last_Result_Position));
        Log.Write ("  Auto_Focus_Last_Result_Tolerance : " & Image_Of (Data.Focuser.Auto_Focus_Last_Result_Tolerance));
        Log.Write ("Focuser1");
        Log.Write ("  Connected     : " & Image_Of (Data.Focuser1.Connected));
        Log.Write ("  Position      : " & Image_Of (Data.Focuser1.Position));
        Log.Write ("  Moving        : " & Image_Of (Data.Focuser1.Moving));
        Log.Write ("  Goto_Complete : " & Image_Of (Data.Focuser1.Goto_Complete));
        Log.Write ("  Finding_Home  : " & Image_Of (Data.Focuser1.Finding_Home));
        Log.Write ("Focuser2");
        Log.Write ("  Connected     : " & Image_Of (Data.Focuser2.Connected));
        Log.Write ("  Position      : " & Image_Of (Data.Focuser2.Position));
        Log.Write ("  Moving        : " & Image_Of (Data.Focuser2.Moving));
        Log.Write ("  Goto_Complete : " & Image_Of (Data.Focuser2.Goto_Complete));
        Log.Write ("  Finding_Home  : " & Image_Of (Data.Focuser2.Finding_Home));
        Log.Write ("Rotator");
        Log.Write ("  Connected       : " & Image_Of (Data.Rotator.Connected));
        Log.Write ("  Position        : " & Image_Of (Data.Rotator.Position));
        Log.Write ("  Moving          : " & Image_Of (Data.Rotator.Moving));
        Log.Write ("  Goto_Complete   : " & Image_Of (Data.Rotator.Goto_Complete));
        Log.Write ("  Finding_Home    : " & Image_Of (Data.Rotator.Finding_Home));
        Log.Write ("  Alt_Az_Derotate : " & Image_Of (Data.Rotator.Alt_Az_Derotate));
        Log.Write ("Rotator1");
        Log.Write ("  Connected     : " & Image_Of (Data.Rotator1.Connected));
        Log.Write ("  Position      : " & Image_Of (Data.Rotator1.Position));
        Log.Write ("  Moving        : " & Image_Of (Data.Rotator1.Moving));
        Log.Write ("  Goto_Complete : " & Image_Of (Data.Rotator1.Goto_Complete));
        Log.Write ("  Finding_Home  : " & Image_Of (Data.Rotator1.Finding_Home));
        Log.Write ("Rotator2");
        Log.Write ("  Connected     : " & Image_Of (Data.Rotator2.Connected));
        Log.Write ("  Position      : " & Image_Of (Data.Rotator2.Position));
        Log.Write ("  Moving        : " & Image_Of (Data.Rotator2.Moving));
        Log.Write ("  Goto_Complete : " & Image_Of (Data.Rotator2.Goto_Complete));
        Log.Write ("  Finding_Home  : " & Image_Of (Data.Rotator2.Finding_Home));
        Log.Write ("Mount");
        Log.Write ("  Connected               : " & Image_Of (Data.Mount.Flags.Connected));
        Log.Write ("  On_Target               : " & Image_Of (Data.Mount.Flags.On_Target));
        Log.Write ("  Tracking                : " & Image_Of (Data.Mount.Flags.Tracking));
        Log.Write ("  Ra                      : " & Image_Of (Data.Mount.Ra));
        Log.Write ("  Dec                     : " & Image_Of (Data.Mount.Dec));
        Log.Write ("  Ra_Target               : " & Image_Of (Data.Mount.Ra_Target));
        Log.Write ("  Dec_Target              : " & Image_Of (Data.Mount.Dec_Target));
        Log.Write ("  Ra_2000                 : " & Image_Of (Data.Mount.Ra_2000));
        Log.Write ("  Dec_2000                : " & Image_Of (Data.Mount.Dec_2000));
        Log.Write ("  Ra_Radian               : " & Image_Of (Data.Mount.Ra_Radian));
        Log.Write ("  Dec_Radian              : " & Image_Of (Data.Mount.Dec_Radian));
        Log.Write ("  Azm_Radian              : " & Image_Of (Data.Mount.Azm_Radian));
        Log.Write ("  Alt_Radian              : " & Image_Of (Data.Mount.Alt_Radian));
        Log.Write ("  Azm_Rms_Error           : " & Image_Of (Data.Mount.Azm_Rms_Error));
        Log.Write ("  Alt_Rms_Error           : " & Image_Of (Data.Mount.Alt_Rms_Error));
        Log.Write ("  Azm_Motor_Error_Code    : " & Image_Of (Data.Mount.Azm_Motor_Error_Code));
        Log.Write ("  Alt_Motor_Error_Code    : " & Image_Of (Data.Mount.Alt_Motor_Error_Code));
        Log.Write ("  Azm_Enabled             : " & Image_Of (Data.Mount.Flags.Azm_Enabled));
        Log.Write ("  Alt_Enabled             : " & Image_Of (Data.Mount.Flags.Alt_Enabled));
        Log.Write ("  Is_Finding_Home         : " & Image_Of (Data.Mount.Flags.Is_Finding_Home));
        Log.Write ("  Encoders_Have_Been_Set  : " & Image_Of (Data.Mount.Flags.Encoders_Have_Been_Set));
        Log.Write ("  Pointing_Model_Set      : " & Image_Of (Data.Mount.Flags.Pointing_Model_Set));
        Log.Write ("M3");
        Log.Write ("  Connected       : " & Image_Of (Data.M3.Connected));
        Log.Write ("  Port            : " & Image_Of (Data.M3.Port));
        Log.Write ("  Position_Rotate : " & Image_Of (Data.M3.Position_Rotate));
        Log.Write ("  Position_Tilt   : " & Image_Of (Data.M3.Position_Tilt));
        Log.Write ("  Moving_Rotate   : " & Image_Of (Data.M3.Moving_Rotate));
        Log.Write ("  Moving_Tilt     : " & Image_Of (Data.M3.Moving_Tilt));
        Log.Write ("Temperature");
        Log.Write ("  Primary   : " & Image_Of (Data.Temperature.Primary));
        Log.Write ("  Ambient   : " & Image_Of (Data.Temperature.Ambient));
        Log.Write ("  Secondary : " & Image_Of (Data.Temperature.Secondary));
        Log.Write ("  Backplate : " & Image_Of (Data.Temperature.Backplate));
        Log.Write ("  M3        : " & Image_Of (Data.Temperature.M3));
        Log.Write ("Fans");
        Log.Write ("  On : " & Image_Of (Data.Fans.On));
        Log.Write ("End");
      end if;
    end Set;


    function Fans_Turned_On return Boolean is
    begin
      return The_Data.Fans.On;
    end Fans_Turned_On;


    function Mount_Flags return Mount_Flag is
    begin
      return The_Data.Mount.Flags;
    end Mount_Flags;


    function Mount_Data return Mount_Info is
    begin
      return The_Data.Mount;
    end Mount_Data;


    function M3_Data return M3_Info is
    begin
      return The_Data.M3;
    end M3_Data;


    function Rotator_Data return Rotator_Info is
    begin
      return The_Data.Rotator;
    end Rotator_Data;


    function Rotator1_Data return Rotator_Info is
    begin
      return The_Data.Rotator1;
    end Rotator1_Data;

  end System;

end PWI.XML;
