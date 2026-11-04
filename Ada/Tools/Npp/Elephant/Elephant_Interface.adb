-- *********************************************************************************************************************
-- *                       (c) 2014 .. 2025 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Client;
with Log;
with Npp.Plugin;
with Scintilla;
with Server;
with System.Address_To_Access_Conversions;
with Win.Dll;

package body Elephant_Interface is

  Interface_Prefix : constant String := "Elephant.";

  Wide_Nul : constant Wide_Character := Wide_Character'first;

  Promote_Toolbar        : aliased Npp.Toolbar_Icons;
  Promote_All_Toolbar    : aliased Npp.Toolbar_Icons;
  Run_Toolbar            : aliased Npp.Toolbar_Icons;
  Show_Reference_Toolbar : aliased Npp.Toolbar_Icons;
  Undo_Reference_Toolbar : aliased Npp.Toolbar_Icons;
  Show_Usage_Toolbar     : aliased Npp.Toolbar_Icons;
  Show_Unused_Toolbar    : aliased Npp.Toolbar_Icons;


  procedure Load_Resources is
    Promote_Bitmap        : aliased constant String := "PromoteBmp" & Ascii.Nul;
    Promote_All_Bitmap    : aliased constant String := "PromoteAllBmp" & Ascii.Nul;
    Run_Bitmap            : aliased constant String := "RunBmp" & Ascii.Nul;
    Show_Reference_Bitmap : aliased constant String := "ShowReferenceBmp" & Ascii.Nul;
    Undo_Reference_Bitmap : aliased constant String := "UndoReferenceBmp" & Ascii.Nul;
    Show_Usage_Bitmap     : aliased constant String := "ShowUsageBmp" & Ascii.Nul;
    Show_Unused_Bitmap    : aliased constant String := "ShowUnusedBmp" & Ascii.Nul;
  begin
    Promote_Toolbar.Bmp := Win.Load_Bitmap (Win.Dll.Instance, Promote_Bitmap);
    Promote_All_Toolbar.Bmp := Win.Load_Bitmap (Win.Dll.Instance, Promote_All_Bitmap);
    Run_Toolbar.Bmp := Win.Load_Bitmap (Win.Dll.Instance, Run_Bitmap);
    Show_Reference_Toolbar.Bmp := Win.Load_Bitmap (Win.Dll.Instance, Show_Reference_Bitmap);
    Undo_Reference_Toolbar.Bmp := Win.Load_Bitmap (Win.Dll.Instance, Undo_Reference_Bitmap);
    Show_Usage_Toolbar.Bmp := Win.Load_Bitmap (Win.Dll.Instance, Show_Usage_Bitmap);
    Show_Unused_Toolbar.Bmp := Win.Load_Bitmap (Win.Dll.Instance, Show_Unused_Bitmap);
  end Load_Resources;


  procedure Promote is
  begin
    Client.Promote;
  exception
  when Item: others =>
    Log.Write (Interface_Prefix & "Promote", Item);
  end Promote;


  procedure Promote_All is
  begin
    Client.Promote (Kind => Server.All_Projects);
  exception
  when Item: others =>
    Log.Write (Interface_Prefix & "Promote_All", Item);
  end Promote_All;


  procedure Run is
  begin
    Client.Promote (Server.Run);
  exception
  when Item: others =>
    Log.Write (Interface_Prefix & "Run", Item);
  end Run;


  procedure Show_Reference is
  begin
    Client.Show_Reference;
  exception
  when Item: others =>
    Log.Write (Interface_Prefix & "Show_Reference", Item);
  end Show_Reference;


  procedure Undo_Reference is
  begin
    Client.Undo_Reference;
  exception
  when Item: others =>
    Log.Write (Interface_Prefix & "Undo_Reference", Item);
  end Undo_Reference;


  procedure Show_Usage is
  begin
    Client.Show_Usage;
  exception
  when Item: others =>
    Log.Write (Interface_Prefix & "Show_Usage", Item);
  end Show_Usage;


  procedure Show_Unused is
  begin
    Client.Show_Unused;
  exception
  when Item: others =>
    Log.Write (Interface_Prefix & "Show_Unused", Item);
  end Show_Unused;


  procedure Initialize is
  begin
    Load_Resources;
    Npp.Plugin.Add_Function ("Show Reference", Show_Reference'access, Show_Reference_Toolbar);
    Npp.Plugin.Add_Function ("Undo Reference", Undo_Reference'access, Undo_Reference_Toolbar);
    Npp.Plugin.Add_Function ("Show Usage", Show_Usage'access, Show_Usage_Toolbar);
    Npp.Plugin.Add_Function ("Show Unused", Show_Unused'access, Show_Unused_Toolbar);
    Npp.Plugin.Add_Function ("Promote All", Promote_All'access, Promote_All_Toolbar);
    Npp.Plugin.Add_Function ("Run", Run'access, Run_Toolbar);
    Npp.Plugin.Add_Function ("Promote", Promote'access, Promote_Toolbar);
    Npp.Plugin.Show_Header;
  exception
  when Item: others =>
    Log.Write (Interface_Prefix & "Initialize", Item);
  end Initialize;


  Is_Ready : Boolean := False;

  procedure Ready is
  begin
    Is_Ready := True;
    Client.Open_Project;
  exception
  when Item: others =>
    Log.Write (Interface_Prefix & "Ready", Item);
  end Ready;


  procedure Update is
  begin
    Client.Update;
  exception
  when Item: others =>
    Log.Write (Interface_Prefix & "Update", Item);
  end Update;


  procedure Activation is
  begin
    if Is_Ready then
      Client.Change_Project;
      Update;
    end if;
  exception
  when Item: others =>
    Log.Write (Interface_Prefix & "Activation", Item);
  end Activation;


  procedure File_Saved is
  begin
    Client.Trim_Trailing_Spaces;
  exception
  when Item: others =>
    Log.Write (Interface_Prefix & "File_Saved", Item);
  end File_Saved;


  ---------------
  -- Interface --
  ---------------

  Name : aliased constant Wide_String := "Elephant Plugin" & Wide_Nul;

  function Get_Name return System.Address is
  begin
    return Name'address;
  end Get_Name;


  function Is_Unicode return BOOL is
  begin
    return Npp.Plugin.Is_Unicode;
  end Is_Unicode;


  procedure Set_Info (Data : Info) is
  begin
    Npp.Plugin.Set_Info ((Data.Npp_Handle, Data.Scintilla_Main_Handle, Data.Scintilla_Second_Handle));
  end Set_Info;


  function Get_Funcs_Array (Count : access INT) return System.Address is
  begin
    return Npp.Plugin.Get_Funcs_Array (Count);
  end Get_Funcs_Array;


  package Convertion is new System.Address_To_Access_Conversions (Scintilla.Notification);

  procedure Be_Notified (Notify : System.Address) is
  begin
    Npp.Plugin.Be_Notified (Convertion.To_Pointer(Notify));
  end Be_Notified;


  function Message_Proc (Unused_Message : UINT;
                         Unused_Wpara   : WPARAM;
                         Unused_Lpara   : LPARAM) return LRESULT is
  begin
    return LRESULT(Win.OK);
  end Message_Proc;


begin
  Npp.Plugin.Define ("Npp_Elephant");
  Npp.Plugin.Install (Set_Info_Call    => Initialize'access,
                      Ready            => Ready'access,
                      Buffer_Activated => Activation'access,
                      Buffer_Updated   => Update'access,
                      File_Saved       => File_Saved'access);
end Elephant_Interface;
