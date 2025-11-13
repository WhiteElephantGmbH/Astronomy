-- *********************************************************************************************************************
-- *                           (c) 2025 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

with Ada.Text_IO;
with Canon_Interface;

package body Canon is

  package IO renames Ada.Text_IO;
  package CI renames Canon_Interface;


  function String_Of (C_String : String) return String is

    function Last return Natural is
      The_Last : Natural := C_String'first - 1;
    begin
      for Char of C_String loop
        if Char = Ascii.Nul then
          return The_Last;
        end if;
        The_Last := @ + 1;
      end loop;
      return C_String'last;
    end Last;

  begin
    return C_String (C_String'first .. Last);
  end String_Of;


   --  Simple helper to check EDSDK return codes.
  procedure Check (E     : CI.Eds_Error;
                   Where : String) is
    use type CI.Eds_Error;
  begin
    if E /= 0 then
      IO.Put_Line (Where & " failed, error = " & CI.Eds_Error'image (E));
      raise Program_Error;
    end if;
  end Check;


  procedure Test is

    Error       : CI.Eds_Error;
    Cam_List    : aliased CI.Camera_List;
    Camera      : aliased CI.Camera;
    Count       : aliased CI.Eds_Uint32;
    Initialized : Boolean := False;
    Device_Info : aliased CI.Device_Info;

  begin
    IO.Put_Line ("[Canon_Test] Initializing EDSDK...");

    --  1) Initialize SDK
    Error := CI.Initialize_SDK;
    Check (Error, "Initialize_SDK");
    Initialized := True;

    IO.Put_Line ("[Canon_Test] SDK initialized.");

    declare
      use type CI.Eds_Uint32;
    begin
      --  2) Get camera list
      IO.Put_Line ("[Canon_Test] Getting camera list...");
      Error := CI.Get_Camera_List (Cam_List'access);
      Check (Error, "Get_Camera_List");

      --  3) How many cameras?
      Error := CI.Get_Child_Count (Cam_List, Count'access);
      Check (Error, "Get_Child_Count");

      IO.Put_Line ("[Canon_Test] Number of detected cameras: "
                   & CI.Eds_Uint32'image (Count));

      if Count = 0 then
        IO.Put_Line
          ("[Canon_Test] No Canon cameras found. " &
           "Check USB connection and that EOS Utility is not running.");
      else
        --  4) Get first camera in the list
        IO.Put_Line ("[Canon_Test] Getting camera at index 0...");
        Error := CI.Get_Camera_At_Index (Cam_List, 0, Camera'access);
        Check (Error, "Get_Camera_At_Index");

        IO.Put_Line ("[Canon_Test] Reading device information...");
        Error := CI.Get_Device_Info (Camera, Device_Info'access);
        Check (Error, "Get_Device_Info");
        IO.Put_Line ("[Canon_Test] Camera description: " & String_Of (Device_Info.Sz_Device_Description));
        IO.Put_Line ("[Canon_Test] Device Subtype    :" & Device_Info.Device_Sub_Type'image);

        --  5) Open session
        IO.Put_Line ("[Canon_Test] Opening session to camera ...");
        Error := CI.Open_Session (Camera);
        Check (Error, "Open_Session");

        IO.Put_Line ("[Canon_Test] Session successfully opened (connection should now be active).");

        --  Here you could later add a simple Take-Picture test.
        IO.Put_Line ("[Canon] triggering single shot...");
        Error := CI.Send_Command (Camera, CI.Camera_Command_Take_Picture, 0);
        Check (Error, "Send_Command(Take_Picture)");

        -- give the camera time to expose and write to card
        -- (without object-event handling, we just wait a bit)
        delay 5.0;




        IO.Put_Line ("[Canon_Test] Closing session...");
        Error := CI.Close_Session (Camera);
        Check (Error, "Close_Session");

        IO.Put_Line ("[Canon_Test] Releasing camera handle...");
        Error := CI.Release (Camera);
        Check (Error, "Release(Camera)");
      end if;

      --  Always release the camera list.
      IO.Put_Line ("[Canon_Test] Releasing camera list...");
      Error := CI.Release (Cam_List);
      Check (Error, "Release(Camera_List)");

    end;

    --  6) Terminate SDK
    IO.Put_Line ("[Canon_Test] Terminating EDSDK...");
    Error := CI.Terminate_SDK;
    Check (Error, "Terminate_SDK");

    IO.Put_Line ("[Canon_Test] Finished.");

  exception
  when others =>
    if Initialized then
      declare
        Dummy : CI.Eds_Error := CI.Terminate_SDK;
      begin
        null;
      end;
    end if;
    raise;
  end Test;

end Canon;
