-- *********************************************************************************************************************
-- *                       (c) 2023 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Traces;
with Text;

package body PWI4.Protocol is

  package Log is new Traces ("PWI.Protocol");


  The_Actual_Message : Text.String;
  use type Text.String;

  procedure Log_Write (Message : String) is
  begin
    The_Actual_Message := [Message];
    Log.Write (Message);
  end Log_Write;


  procedure Log_Error (Message : String) is
  begin
    Log.Error (The_Actual_Message & Message);
    Log.Force_Enable;
  end Log_Error;


  function Boolean_Of (Image : String) return Boolean is
  begin
    return Boolean'value(Text.Uppercase_Of (Image));
  exception
  when others =>
    Log_Error ("Boolean_Of (Image -> """ & Image & """)");
    raise Parsing_Error;
  end Boolean_Of;


  function Image_Of (Item : Boolean) return String is
  begin
    return Text.Legible_Of (Item'img);
  end Image_Of;


  function Degrees_Of (Image : String) return Degrees is
  begin
    return Degrees'value(Image);
  exception
  when others =>
    Log_Error ("Degrees_Of (Image -> """ & Image & """) out of range");
    return Undefined_Degrees;
  end Degrees_Of;


  function Hours_Of (Image : String) return Hours is
  begin
    return Hours'value(Image);
  exception
  when others =>
    return Undefined_Hours;
  end Hours_Of;


  function Meters_Of (Image : String) return Meters is
  begin
    return Meters'value(Image);
  exception
  when others =>
    Log_Error ("Meters_Of (Image -> """ & Image & """) out of range");
    return Undefined_Meters;
  end Meters_Of;


  function Points_Of (Image : String) return Points is
  begin
    return Points'value(Image);
  exception
  when others =>
    Log_Error ("Points_Of (Image -> """ & Image & """)");
    return 0;
  end Points_Of;


  function Image_Of (Item : Meters) return String is
  begin
    return Text.Trimmed (Item'image);
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
      Log_Error ("Jd_Of (Image -> """ & Image & """)");
      raise Parsing_Error;
    end;
  end Jd_Of;


  function Image_Of (Item : Julian_Day) return String is
  begin
    return Text.Trimmed (Item'img);
  end Image_Of;


  function Focuser_Position_Of (Image : String) return Microns is
  begin
    return Microns'value(Image);
  exception
  when others =>
    Log_Error ("Focuser_Position_Of (Image -> """ & Image & """)");
    raise Parsing_Error;
  end Focuser_Position_Of;


  function Arc_Second_Of (Image : String) return Arc_Second is
  begin
    return Arc_Second'value(Image);
  exception
  when others =>
    Log_Error ("Arc_Second_Of (Image -> """ & Image & """)");
    raise Parsing_Error;
  end Arc_Second_Of;


  function Error_Code_Of (Image : String) return Error_Code is
  begin
    return Error_Code'value(Image);
  exception
  when others =>
    Log_Error ("Error_Code_Of (Image -> """ & Image & """)");
    raise Parsing_Error;
  end Error_Code_Of;


  function Image_Of (Item : Error_Code) return String is
  begin
    return Text.Trimmed (Item'img);
  end Image_Of;


  function Port_Number_Of (Image : String) return Port_Number is
  begin
    return Port_Number'value(Image);
  exception
  when others =>
    Log_Error ("Port_Number_Of (Image -> """ & Image & """)");
    raise Parsing_Error;
  end Port_Number_Of;


  function Image_Of (Item : Port_Number) return String is
  begin
    return Text.Trimmed (Item'img);
  end Image_Of;


  protected System is

    procedure Set (Status : Status_Code);

    procedure Set (Data : Protocol.Response);

    function Mount_Data return Protocol.Mount_Info;

    function Rotator_Data return Protocol.Rotator_Info;

    function Focuser_Data return Protocol.Focuser_Info;

    function M3_Data return Protocol.M3_Info;

    function Site_Data return Site_Info;

  private
    The_Data : Protocol.Response;
  end System;


  procedure Parse (Data : String) is

    The_Index : Integer := Data'first - 1;

    The_Character : Character;

    function Next_Character return Character is
    begin
      The_Index := The_Index + 1;
      if The_Index > Data'last then
        raise Parsing_Error;
      end if;
      The_Character := Data(The_Index);
      return The_Character;
    end Next_Character;

    type Identifier is (I_Acceleration_Degs_Per_Sec_Sqr,
                        I_Altitude_Degs,
                        I_Autofocus,
                        I_Alt_Rms_Error_Arcsec,
                        I_Axis0,
                        I_Axis0_Arcsec,
                        I_Axis0_Wrap_Range_Min_Degs,
                        I_Axis1,
                        I_Axis1_Arcsec,
                        I_Azimuth_Degs,
                        I_Azm_Encoder_Degs,
                        I_Azm_Rms_Error_Arcsec,
                        I_Best_Position,
                        I_Dec_Arcsec,
                        I_Dec_Apparent_Degs,
                        I_Dec_J2000_Degs,
                        I_Dist_To_Target_Arcsec,
                        I_Distance_To_Sun_Degs,
                        I_Exists,
                        I_Field_Angle_Degs,
                        I_Field_Angle_Here_Degs,
                        I_Field_Angle_At_Target_Degs,
                        I_Field_Angle_Rate_At_Target_Degs_Per_Sec,
                        I_Filename,
                        I_Focuser,
                        I_Geometry,
                        I_Gradual_Offset_Progress,
                        I_Height_Meters,
                        I_Is_Enabled,
                        I_Is_Connected,
                        I_Is_Moving,
                        I_Is_Running,
                        I_Is_Slewing,
                        I_Is_Tracking,
                        I_Latitude_Degs,
                        I_Lmst_Hours,
                        I_Julian_Date,
                        I_Longitude_Degs,
                        I_Max_Mech_Position_Degs,
                        I_Max_Velocity_Degs_Per_Sec,
                        I_Measured_Velocity_Degs_Per_Sec,
                        I_Measured_Current_Amps,
                        I_Mech_Position_Degs,
                        I_Min_Mech_Position_Degs,
                        I_Model,
                        I_Mount,
                        I_M3,
                        I_Num_Points_Enabled,
                        I_Num_Points_Total,
                        I_Offsets,
                        I_Path_Angle_At_Target_Degs,
                        I_Path_Angle_Rate_At_Target_Degs_Per_Sec,
                        I_Path_Arcsec,
                        I_Port,
                        I_Position,
                        I_Position_Degs,
                        I_Position_Rotate,
                        I_Position_Timestamp,
                        I_Pwi4,
                        I_Ra_Apparent_Hours,
                        I_Ra_Arcsec,
                        I_Ra_J2000_Hours,
                        I_Rate,
                        I_Response,
                        I_Rms_Error_Arcsec,
                        I_Rotator,
                        I_Servo_Error_Arcsec,
                        I_Setpoint_Velocity_Degs_Per_Sec,
                        I_Site,
                        I_Slew_Time_Constant,
                        I_Spiral_Offset,
                        I_Success,
                        I_Target_Dec_Apparent_Degs,
                        I_Target_Mech_Position_Degs,
                        I_Target_Ra_Apparent_Hours,
                        I_Timestamp_Utc,
                        I_Tolerance,
                        I_Total,
                        I_Transverse_Arcsec,
                        I_Update_Duration_Msec,
                        I_Update_Count,
                        I_Version,
                        I_Version_Field,
                        I_X,
                        I_X_Step_Arcsec,
                        I_Y,
                        I_Y_Step_Arcsec);


    function Next_Identifier return Identifier is
      First : constant Natural := The_Index + 1;
    begin
      while not (Next_Character in '.' | '[' | '=') loop
        null;
      end loop;
      return Identifier'value ("I_" & Text.Uppercase_Of (Data(First .. The_Index - 1)));
    exception
    when others =>
      Log_Error ("Next_Identifier unknown <" & Data(First .. The_Index - 1) & ">");
      raise Parsing_Error;
    end Next_Identifier;


    The_Value : Text.String;

    function Value return String is
    begin
      return The_Value.To_String;
    end Value;


    function Next_Value return String is
      First : constant Natural := The_Index + 1;
    begin
      while Next_Character /= Ascii.Lf loop
        null;
      end loop;
      The_Value := [Data(First .. The_Index - 1)];
      return Value;
    exception
    when others =>
      raise Parsing_Error;
    end Next_Value;


    The_Response : Response;

    procedure Parse_Pwi4 is
    begin
      case Next_Identifier is
      when I_Version =>
        Log_Write ("pwi4.version=" & Next_Value);
      when I_Version_Field =>
        Log_Write ("pwi4.version_field=" & Next_Value);
      when others =>
        raise Parsing_Error;
      end case;
    end Parse_Pwi4;


    procedure Parse_Response is
    begin
      case Next_Identifier is
      when I_Timestamp_Utc =>
        Log_Write ("response.timestamp_utc=" & Next_Value);
      when others =>
        raise Parsing_Error;
      end case;
    end Parse_Response;


    procedure Parse_Site is
    begin
      case Next_Identifier is
      when I_Latitude_Degs =>
        Log_Write ("site.latitude_degs=" & Next_Value);
        The_Response.Site.Latitude := Degrees_Of (Value);
      when I_Longitude_Degs =>
        Log_Write ("site.longitude_degs=" & Next_Value);
        The_Response.Site.Longitude := Degrees_Of (Value);
      when I_Height_Meters =>
        Log_Write ("site.height_meters=" & Next_Value);
        The_Response.Site.Height := Meters_Of (Value);
      when I_Lmst_Hours =>
        Log_Write ("site.lmst_hours=" & Next_Value);
        The_Response.Site.Lmst := Hours_Of (Value);
      when others =>
        raise Parsing_Error;
      end case;
    end Parse_Site;


    procedure Parse_Mount is
    begin
      case Next_Identifier is
      when I_Is_Connected =>
        Log_Write ("mount.is_connected=" & Next_Value);
        The_Response.Mount.Flags.Is_Connected := Boolean_Of (Value);
      when I_Geometry =>
        Log_Write ("mount.geometry=" & Next_Value);
      when I_Timestamp_Utc =>
        Log_Write ("mount.timestamp_utc=" & Next_Value);
      when I_Julian_Date =>
        Log_Write ("mount.julian_date=" & Next_Value);
        The_Response.Mount.Julian_Date := Jd_Of (Value);
      when I_Update_Duration_Msec =>
        Log_Write ("mount.update_duration_msec=" & Next_Value);
      when I_Update_Count =>
        Log_Write ("mount.update_count=" & Next_Value);
      when I_Slew_Time_Constant =>
        Log_Write ("mount.slew_time_constant=" & Next_Value);
      when I_Ra_Apparent_Hours =>
        Log_Write ("mount.ra_apparent_hours=" & Next_Value);
        The_Response.Mount.Ra := Hours_Of (Value);
      when I_Dec_Apparent_Degs =>
        Log_Write ("mount.dec_apparent_degs=" & Next_Value);
        The_Response.Mount.Dec := Degrees_Of (Value);
      when I_Ra_J2000_Hours =>
        Log_Write ("mount.ra_j2000_hours=" & Next_Value);
        The_Response.Mount.Ra_J2000 := Hours_Of (Value);
      when I_Dec_J2000_Degs =>
        Log_Write ("mount.dec_j2000_degs=" & Next_Value);
        The_Response.Mount.Dec_J2000 := Degrees_Of (Value);
      when I_Target_Ra_Apparent_Hours =>
        Log_Write ("mount.target_ra_apparent_hours=" & Next_Value);
        The_Response.Mount.Ra_Target := Hours_Of (Value);
      when I_Target_Dec_Apparent_Degs =>
        Log_Write ("mount.target_dec_apparent_degs=" & Next_Value);
        The_Response.Mount.Dec_Target := Degrees_Of (Value);
      when I_Azimuth_Degs =>
        Log_Write ("mount.azimuth_degs=" & Next_Value);
        The_Response.Mount.Azimuth := Degrees_Of (Value);
      when I_Altitude_Degs =>
        Log_Write ("mount.altitude_degs=" & Next_Value);
        The_Response.Mount.Altitude := Degrees_Of (Value);
      when I_Is_Slewing =>
        Log_Write ("mount.is_slewing=" & Next_Value);
        The_Response.Mount.Flags.Is_Slewing := Boolean_Of (Value);
      when I_Is_Tracking =>
        Log_Write ("mount.is_tracking=" & Next_Value);
        The_Response.Mount.Flags.Is_Tracking := Boolean_Of (Value);
      when I_Field_Angle_Here_Degs =>
        Log_Write ("mount.field_angle_here=" & Next_Value);
      when I_Field_Angle_At_Target_Degs =>
        Log_Write ("mount.field_angle_at_target=" & Next_Value);
        The_Response.Mount.Field_Angle_At_Target := Degrees_Of (Value);
      when I_Field_Angle_Rate_At_Target_Degs_Per_Sec =>
        Log_Write ("mount.field_angle_rate_at_target=" & Next_Value);
        The_Response.Mount.Field_Angle_Rate_At_Target := Degrees_Of (Value);
      when I_Path_Angle_At_Target_Degs =>
        Log_Write ("mount.path_angle_at_target=" & Next_Value);
      when I_Path_Angle_Rate_At_Target_Degs_Per_Sec =>
        Log_Write ("mount.path_angle_rate_at_target=" & Next_Value);
      when I_Distance_To_Sun_Degs =>
        Log_Write ("mount.distance_to_sun=" & Next_Value);
      when I_Axis0_Wrap_Range_Min_Degs =>
        Log_Write ("mount.axis0_wrap_range_min=" & Next_Value);
        The_Response.Mount.Wrap_Range_Min := Degrees_Of (Value);
      when I_Offsets =>
        case Next_Identifier is
        when I_Ra_Arcsec =>
          case Next_Identifier is
          when I_Total =>
            Log_Write ("mount.ra_arcsec.total=" & Next_Value);
          when I_Rate =>
            Log_Write ("mount.ra_arcsec.rate=" & Next_Value);
          when I_Gradual_Offset_Progress =>
            Log_Write ("mount.ra_arcsec.gradual_offset_progress=" & Next_Value);
          when others =>
            raise Parsing_Error;
          end case;
        when I_Dec_Arcsec =>
          case Next_Identifier is
          when I_Total =>
            Log_Write ("mount.dec_arcsec.total=" & Next_Value);
          when I_Rate =>
            Log_Write ("mount.dec_arcsec.rate=" & Next_Value);
          when I_Gradual_Offset_Progress =>
            Log_Write ("mount.dec_arcsec.gradual_offset_progress=" & Next_Value);
          when others =>
            raise Parsing_Error;
          end case;
        when I_Axis0_Arcsec =>
          case Next_Identifier is
          when I_Total =>
            Log_Write ("mount.axis0_arcsec.total=" & Next_Value);
          when I_Rate =>
            Log_Write ("mount.axis0_arcsec.rate=" & Next_Value);
          when I_Gradual_Offset_Progress =>
            Log_Write ("mount.axis0_arcsec.gradual_offset_progress=" & Next_Value);
          when others =>
            raise Parsing_Error;
          end case;
        when I_Axis1_Arcsec =>
          case Next_Identifier is
          when I_Total =>
            Log_Write ("mount.axis1_arcsec.total=" & Next_Value);
          when I_Rate =>
            Log_Write ("mount.axis1_arcsec.rate=" & Next_Value);
          when I_Gradual_Offset_Progress =>
            Log_Write ("mount.axis1_arcsec.gradual_offset_progress=" & Next_Value);
          when others =>
            raise Parsing_Error;
          end case;
        when I_Path_Arcsec =>
          case Next_Identifier is
          when I_Total =>
            Log_Write ("mount.path_arcsec.total=" & Next_Value);
          when I_Rate =>
            Log_Write ("mount.path_arcsec.rate=" & Next_Value);
          when I_Gradual_Offset_Progress =>
            Log_Write ("mount.path_arcsec.gradual_offset_progress=" & Next_Value);
          when others =>
            raise Parsing_Error;
          end case;
        when I_Transverse_Arcsec =>
          case Next_Identifier is
          when I_Total =>
            Log_Write ("mount.traverse_arcsec.total=" & Next_Value);
          when I_Rate =>
            Log_Write ("mount.traverse_arcsec.rate=" & Next_Value);
          when I_Gradual_Offset_Progress =>
            Log_Write ("mount.traverse_arcsec.gradual_offset_progress=" & Next_Value);
          when others =>
            raise Parsing_Error;
          end case;
        when others =>
          raise Parsing_Error;
        end case;
      when I_Spiral_Offset =>
        case Next_Identifier is
        when I_X =>
          Log_Write ("mount.spiral_offsets.x=" & Next_Value);
        when I_Y =>
          Log_Write ("mount.spiral_offsets.y=" & Next_Value);
        when I_X_Step_Arcsec =>
          Log_Write ("mount.spiral_offsets.x_step_arcsec=" & Next_Value);
          The_Response.Mount.Spiral_Offsets.X_Step := Arc_Second_Of (Value);
        when I_Y_Step_Arcsec =>
          Log_Write ("mount.spiral_offsets.y_step_arcsec=" & Next_Value);
          The_Response.Mount.Spiral_Offsets.Y_Step := Arc_Second_Of (Value);
        when others =>
          raise Parsing_Error;
        end case;
      when I_Axis0 =>
        case Next_Identifier is
        when I_Is_Enabled =>
          Log_Write ("mount.axis0.is_enabled=" & Next_Value);
          The_Response.Mount.Flags.Axis0_Is_Enabled := Boolean_Of (Value);
        when I_Rms_Error_Arcsec =>
          Log_Write ("mount.axis0.rms_error_arcsec=" & Next_Value);
        when I_Dist_To_Target_Arcsec =>
          Log_Write ("mount.axis0.dist_to_target_arcsec=" & Next_Value);
        when I_Servo_Error_Arcsec =>
          Log_Write ("mount.axis0.servo_error_arcsec=" & Next_Value);
        when I_Min_Mech_Position_Degs =>
          Log_Write ("mount.axis0.min_mech_position_degs=" & Next_Value);
          The_Response.Mount.Axis0.Min_Position := Degrees_Of (Value);
        when I_Max_Mech_Position_Degs =>
          Log_Write ("mount.axis0.max_mech_position_degs=" & Next_Value);
          The_Response.Mount.Axis0.Max_Position := Degrees_Of (Value);
        when I_Target_Mech_Position_Degs =>
          Log_Write ("mount.axis0.target_mech_position_degs=" & Next_Value);
        when I_Position_Degs =>
          Log_Write ("mount.axis0.position_degs=" & Next_Value);
          The_Response.Mount.Axis0.Position := Degrees_Of (Value);
        when I_Position_Timestamp =>
          Log_Write ("mount.axis0.position_timestamp=" & Next_Value);
        when I_Max_Velocity_Degs_Per_Sec =>
          Log_Write ("mount.axis0.max_velocity_degs_per_sec=" & Next_Value);
        when I_Setpoint_Velocity_Degs_Per_Sec =>
          Log_Write ("mount.axis0.setpoint_velocity_degs_per_sec=" & Next_Value);
        when I_Measured_Velocity_Degs_Per_Sec =>
          Log_Write ("mount.axis0.measured_velocity_degs_per_sec=" & Next_Value);
        when I_Acceleration_Degs_Per_Sec_Sqr =>
          Log_Write ("mount.axis0.acceleration_degs_per_sec_sqr=" & Next_Value);
        when I_Measured_Current_Amps =>
          Log_Write ("mount.axis0.measured_current_amp=" & Next_Value);
        when others =>
          raise Parsing_Error;
        end case;
      when I_Axis1 =>
        case Next_Identifier is
        when I_Is_Enabled =>
          Log_Write ("mount.axis1.is_enabled=" & Next_Value);
          The_Response.Mount.Flags.Axis1_Is_Enabled := Boolean_Of (Value);
        when I_Rms_Error_Arcsec =>
          Log_Write ("mount.axis1.rms_error_arcsec=" & Next_Value);
        when I_Dist_To_Target_Arcsec =>
          Log_Write ("mount.axis1.dist_to_target_arcsec=" & Next_Value);
        when I_Servo_Error_Arcsec =>
          Log_Write ("mount.axis1.servo_error_arcsec=" & Next_Value);
        when I_Min_Mech_Position_Degs =>
          Log_Write ("mount.axis1.min_mech_position_degs=" & Next_Value);
          The_Response.Mount.Axis1.Min_Position := Degrees_Of (Value);
        when I_Max_Mech_Position_Degs =>
          Log_Write ("mount.axis1.max_mech_position_degs=" & Next_Value);
          The_Response.Mount.Axis1.Max_Position := Degrees_Of (Value);
        when I_Target_Mech_Position_Degs =>
          Log_Write ("mount.axis1.target_mech_position_degs=" & Next_Value);
        when I_Position_Degs =>
          Log_Write ("mount.axis1.position_degs=" & Next_Value);
          The_Response.Mount.Axis1.Position := Degrees_Of (Value);
        when I_Position_Timestamp =>
          Log_Write ("mount.axis1.position_timestamp=" & Next_Value);
        when I_Max_Velocity_Degs_Per_Sec =>
          Log_Write ("mount.axis1.max_velocity_degs_per_sec=" & Next_Value);
        when I_Setpoint_Velocity_Degs_Per_Sec =>
          Log_Write ("mount.axis1.setpoint_velocity_degs_per_sec=" & Next_Value);
        when I_Measured_Velocity_Degs_Per_Sec =>
          Log_Write ("mount.axis1.measured_velocity_degs_per_sec=" & Next_Value);
        when I_Acceleration_Degs_Per_Sec_Sqr =>
          Log_Write ("mount.axis1.acceleration_degs_per_sec_sqr=" & Next_Value);
        when I_Measured_Current_Amps =>
          Log_Write ("mount.axis1.measured_current_amp=" & Next_Value);
        when others =>
          raise Parsing_Error;
        end case;
      when I_Model =>
        case Next_Identifier is
        when I_Filename =>
          Log_Write ("mount.model.filename=" & Next_Value);
        when I_Num_Points_Total =>
          Log_Write ("mount.model.num_points_total=" & Next_Value);
          The_Response.Mount.Model.Points_Total := Points_Of (Value);
        when I_Num_Points_Enabled =>
          Log_Write ("mount.model.num_points_enabled=" & Next_Value);
          The_Response.Mount.Model.Points_Enabled := Points_Of (Value);
        when I_Rms_Error_Arcsec =>
          Log_Write ("mount.model.rms_error_arcsec=" & Next_Value);
        when others =>
          raise Parsing_Error;
        end case;
      when others =>
        raise Parsing_Error;
      end case;
    end Parse_Mount;


    procedure Parse_Focuser is
    begin
      case Next_Identifier is
      when I_Exists =>
        Log_Write ("focuser.exists=" & Next_Value);
        The_Response.Focuser.Exists := Boolean_Of (Value);
      when I_Is_Connected =>
        Log_Write ("focuser.is_connected=" & Next_Value);
        The_Response.Focuser.Is_Connected := Boolean_Of (Value);
      when I_Is_Enabled =>
        Log_Write ("focuser.is_enabled=" & Next_Value);
        The_Response.Focuser.Is_Enabled := Boolean_Of (Value);
      when I_Position =>
        Log_Write ("focuser.position=" & Next_Value);
        The_Response.Focuser.Position := Focuser_Position_Of (Value);
      when I_Is_Moving =>
        Log_Write ("focuser.is_moving=" & Next_Value);
        The_Response.Focuser.Is_Moving := Boolean_Of (Value);
      when others =>
        raise Parsing_Error;
      end case;
    end Parse_Focuser;


    procedure Parse_Rotator is
    begin
      case Next_Identifier is
      when I_Exists =>
        Log_Write ("rotator.exists=" & Next_Value);
        The_Response.Rotator.Exists := Boolean_Of (Value);
      when I_Is_Connected =>
        Log_Write ("rotator.is_connected=" & Next_Value);
        The_Response.Rotator.Is_Connected := Boolean_Of (Value);
      when I_Is_Enabled =>
        Log_Write ("rotator.is_enabled=" & Next_Value);
        The_Response.Rotator.Is_Enabled := Boolean_Of (Value);
      when I_Field_Angle_Degs =>
        Log_Write ("rotator.field_angle_degs=" & Next_Value);
        The_Response.Rotator.Field_Angle := Degrees_Of (Value);
      when I_Mech_Position_Degs =>
        Log_Write ("rotator.mech_position_degs=" & Next_Value);
        The_Response.Rotator.Mech_Position := Degrees_Of (Value);
      when I_Is_Moving =>
        Log_Write ("rotator.is_moving=" & Next_Value);
        The_Response.Rotator.Is_Moving := Boolean_Of (Value);
      when I_Is_Slewing =>
        Log_Write ("rotator.is_slewing=" & Next_Value);
        The_Response.Rotator.Is_Slewing := Boolean_Of (Value);
      when others =>
        raise Parsing_Error;
      end case;
    end Parse_Rotator;


    procedure Parse_M3 is
    begin
      case Next_Identifier is
      when I_Exists =>
        Log_Write ("m3.exists=" & Next_Value);
        The_Response.M3.Exists := Value /= "0";
      when I_Port =>
        Log_Write ("m3.port=" & Next_Value);
        The_Response.M3.Port := Port_Number'value(Value);
      when others =>
        raise Parsing_Error;
      end case;
    end Parse_M3;


    procedure Parse_Autofocus is
    begin
      case Next_Identifier is
      when I_Is_Running =>
        Log_Write ("autofocus.is_running=" & Next_Value);
      when I_Success =>
        Log_Write ("autofocus.success=" & Next_Value);
      when I_Best_Position =>
        Log_Write ("autofocus.best_position=" & Next_Value);
      when I_Tolerance =>
        Log_Write ("autofocus.tolerance=" & Next_Value);
      when others =>
        raise Parsing_Error;
      end case;
    end Parse_Autofocus;

  begin -- Parse
    if Data'length = 0 or else Data(Data'last) /= Ascii.Lf then
      raise Parsing_Error;
    end if;
    while The_Index < Data'last loop
      case Next_Identifier is
      when I_Pwi4 =>
        Parse_Pwi4;
      when I_Response =>
        Parse_Response;
      when I_Site =>
        Parse_Site;
      when I_Mount =>
        Parse_Mount;
      when I_Focuser =>
        Parse_Focuser;
      when I_Rotator =>
        Parse_Rotator;
      when I_M3 =>
        Parse_M3;
      when I_Autofocus =>
        Parse_Autofocus;
      when others =>
        raise Parsing_Error;
      end case;
    end loop;
    System.Set (The_Response);
    Log.Normal;
    The_Actual_Message := [];
  exception
  when others =>
    Log.Write (Data);
    raise;
  end Parse;


  procedure Set_Error (Status : Status_Code) is
  begin
    System.Set (Status);
  end Set_Error;


  package body Mount is

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


  package body Focuser is

    function Info return Focuser_Info is
    begin
      return System.Focuser_Data;
    end Info;

  end Focuser;


  package body Rotator is

    function Info return Rotator_Info is
    begin
      return System.Rotator_Data;
    end Info;

  end Rotator;


  package body Site is

    function Info return Site_Info is
    begin
      return System.Site_Data;
    end Info;

  end Site;


  protected body System is

    procedure Set (Status : Status_Code) is
    begin
      Log.Write ("Set Error : " & Status'image);
      The_Data.Mount.Flags.Has_Error := True;
      The_Data.Mount.Flags.Is_Connected := False;
    end Set;


    procedure Set (Data : Response) is
    begin
      The_Data := Data;
      if Log.Is_Enabled then
        Log.Write ("Site");
        Log.Write ("  Latitude  : " & Image_Of (Data.Site.Latitude));
        Log.Write ("  Longitude : " & Image_Of (Data.Site.Longitude));
        Log.Write ("  Height    : " & Image_Of (Data.Site.Height));
        Log.Write ("  Lmst      : " & Image_Of (Data.Site.Lmst));
        Log.Write ("Mount");
        Log.Write ("  Is_Connected     : " & Image_Of (Data.Mount.Flags.Is_Connected));
        Log.Write ("  Is_Slewing       : " & Image_Of (Data.Mount.Flags.Is_Slewing));
        Log.Write ("  Is_Tracking      : " & Image_Of (Data.Mount.Flags.Is_Tracking));
        Log.Write ("  Ra               : " & Image_Of (Data.Mount.Ra));
        Log.Write ("  Dec              : " & Image_Of (Data.Mount.Dec));
        Log.Write ("  Ra_Target        : " & Image_Of (Data.Mount.Ra_Target));
        Log.Write ("  Dec_Target       : " & Image_Of (Data.Mount.Dec_Target));
        Log.Write ("  Ra_J2000         : " & Image_Of (Data.Mount.Ra_J2000));
        Log.Write ("  Dec_J2000        : " & Image_Of (Data.Mount.Dec_J2000));
        Log.Write ("  Azmimuth         : " & Image_Of (Data.Mount.Azimuth));
        Log.Write ("  Altitude         : " & Image_Of (Data.Mount.Altitude));
        Log.Write ("  Axis_0_Enabled   : " & Image_Of (Data.Mount.Flags.Axis0_Is_Enabled));
        Log.Write ("  Axis_0_Position  : " & Image_Of (Data.Mount.Axis0.Position));
        Log.Write ("  Axis_1_Enabled   : " & Image_Of (Data.Mount.Flags.Axis1_Is_Enabled));
        Log.Write ("  Axis_1_Position  : " & Image_Of (Data.Mount.Axis1.Position));
        Log.Write ("  Field_Angle      : " & Image_Of (Data.Mount.Field_Angle_At_Target));
        Log.Write ("  Field_Angle_Rate : " & Image_Of (Data.Mount.Field_Angle_Rate_At_Target));
        Log.Write ("Focuser");
        Log.Write ("  Exists       : " & Image_Of (Data.Focuser.Exists));
        Log.Write ("  Is_Connected : " & Image_Of (Data.Focuser.Is_Connected));
        Log.Write ("  Is_Enabled   : " & Image_Of (Data.Focuser.Is_Enabled));
        Log.Write ("  Is_Moving    : " & Image_Of (Data.Focuser.Is_Moving));
        Log.Write ("  Position     : " & Image_Of (Data.Focuser.Position));
        Log.Write ("Rotator");
        Log.Write ("  Exists        : " & Image_Of (Data.Focuser.Exists));
        Log.Write ("  Is_Connected  : " & Image_Of (Data.Rotator.Is_Connected));
        Log.Write ("  Is_Enabled    : " & Image_Of (Data.Rotator.Is_Enabled));
        Log.Write ("  Is_Moving     : " & Image_Of (Data.Rotator.Is_Moving));
        Log.Write ("  Is_Slewing    : " & Image_Of (Data.Rotator.Is_Slewing));
        Log.Write ("  Field_Angle   : " & Image_Of (Data.Rotator.Field_Angle));
        Log.Write ("  Mech_Position : " & Image_Of (Data.Rotator.Mech_Position));
        Log.Write ("M3");
        Log.Write ("  Exists : " & Image_Of (Data.M3.Exists));
        Log.Write ("  Port   : " & Image_Of (Data.M3.Port));
        Log.Write ("End");
      end if;
      Log.Normal;
    end Set;


    function Mount_Data return Mount_Info is
    begin
      return The_Data.Mount;
    end Mount_Data;


    function M3_Data return M3_Info is
    begin
      return The_Data.M3;
    end M3_Data;


    function Focuser_Data return Focuser_Info is
    begin
      return The_Data.Focuser;
    end Focuser_Data;


    function Rotator_Data return Rotator_Info is
    begin
      return The_Data.Rotator;
    end Rotator_Data;


    function Site_Data return Site_Info is
    begin
      return The_Data.Site;
    end Site_Data;

  end System;

end PWI4.Protocol;
