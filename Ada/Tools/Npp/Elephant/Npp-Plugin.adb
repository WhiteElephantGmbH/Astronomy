-- *********************************************************************************************************************
-- *                       (c) 2013 .. 2025 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Characters.Handling;
with Ada.Unchecked_Conversion;
with Definite_Doubly_Linked_Lists;

package body Npp.Plugin is

  Client_Termination : Callback := null;

  procedure Install_Termination (Handler : Callback) is
  begin
    Client_Termination := Handler;
  end Install_Termination;


  Max_Functions : constant := 8;

  subtype Function_Count is Win.INT range 0 .. Max_Functions;

  subtype Function_Range is Function_Count range 1 .. Function_Count'last;

  Item_Name_Length : constant := 64;

  subtype Item_Name is Win.WCHAR_Array (1 .. Item_Name_Length);

  type Callbacks is array (Function_Range) of Callback;

  type Call_List is record
    Calls : Callbacks;
    Count : Function_Count := 0;
  end record;

  Set_Info_Callbacks         : Call_List;
  Tb_Modification_Callbacks  : Call_List;
  Ready_Callbacks            : Call_List;
  Buffer_Activated_Callbacks : Call_List;
  Buffer_Updated_Callbacks   : Call_List;
  File_Saved_Callbacks       : Call_List;

  function Convert is new Ada.Unchecked_Conversion (System.Address, Win.LPARAM);


  The_Name : aliased Wide_String (1 .. 256) := [others => Wide_Nul];

  procedure Define (Wide_Name : Wide_String) is
  begin
    The_Name(1 .. Wide_Name'length) := Wide_Name;
  end Define;


  function Name return System.Address is
  begin
    return The_Name'address;
  end Name;


  Last_Id : Win.INT := 0;

  procedure Install (Set_Info_Call    : Callback := null;
                     Tb_Modification  : Callback := null;
                     Ready            : Callback := null;
                     Buffer_Activated : Callback := null;
                     Buffer_Updated   : Callback := null;
                     File_Saved       : Callback := null) is

    procedure Add (Call : Callback;
                   To   : in out Call_List) is
      use type Win.INT;
    begin
      if Call /= null then
        To.Count := To.Count + 1;
        To.Calls(To.Count) := Call;
      end if;
    end Add;

    use type Win.INT;

  begin -- Install
    Add (Set_Info_Call, Set_Info_Callbacks);
    Add (Tb_Modification, Tb_Modification_Callbacks);
    Add (Ready, Ready_Callbacks);
    Add (Buffer_Activated, Buffer_Activated_Callbacks);
    Add (Buffer_Updated, Buffer_Updated_Callbacks);
    Add (File_Saved, File_Saved_Callbacks);
    if Tb_Modification /= null then
      Last_Id := Last_Id + 1;
    end if;
  end Install;


  function Last_Dialog_Id return Win.INT is
  begin
    return Last_Id;
  end Last_Dialog_Id;


  procedure Call (List : Call_List) is
  begin
    for Index in 1 .. List.Count loop
      List.Calls(Index).all;
    end loop;
  end Call;


  type P_Funct_Plugin_Cmd is access procedure with Convention => C;

  type Func_Item is record
    Name       : Item_Name          := [others => Win.Wide_Nul];
    P_Func     : P_Funct_Plugin_Cmd := null;
    Cmd_Id     : Win.INT            := 0;
    Init2check : Win.BOOL           := Win.FALSE           with Unreferenced;
    P_Sh_Key   : System.Address     := System.Null_Address with Unreferenced;
  end record with Convention => C;

  The_Command  : array (Function_Range) of Callback;
  The_Function : array (Function_Range) of Func_Item;
  The_Toolbar  : array (Function_Range) of Toolbar_Icons;
  The_Count    : aliased Function_Count := 0;

  procedure Command_1 with Convention => C;
  procedure Command_1 is
  begin
    The_Command(1).all;
  end Command_1;

  procedure Command_2 with Convention => C;
  procedure Command_2 is
  begin
    The_Command(2).all;
  end Command_2;

  procedure Command_3 with Convention => C;
  procedure Command_3 is
  begin
    The_Command(3).all;
  end Command_3;

  procedure Command_4 with Convention => C;
  procedure Command_4 is
  begin
    The_Command(4).all;
  end Command_4;

  procedure Command_5 with Convention => C;
  procedure Command_5 is
  begin
    The_Command(5).all;
  end Command_5;

  procedure Command_6 with Convention => C;
  procedure Command_6 is
  begin
    The_Command(6).all;
  end Command_6;

  procedure Command_7 with Convention => C;
  procedure Command_7 is
  begin
    The_Command(7).all;
  end Command_7;

  procedure Command_8 with Convention => C;
  procedure Command_8 is
  begin
    The_Command(8).all;
  end Command_8;


  subtype Id is Win.INT range 1 .. 10; -- fix but extendable for speed reasons

  type Handler_Data is record
    Handler  : Notify_Handler;
    For_Code : Win.INT;
  end record;

  package Handler_List is new Definite_Doubly_Linked_Lists (Handler_Data);

  type Handlers is array (Id) of Handler_List.Item;

  The_Notify_Handlers : Handlers;


  procedure Add_Notify_Handler (Handler  : Notify_Handler;
                                For_Code : Win.INT) is
    use type Handler_List.Item;
  begin
    The_Notify_Handlers(Last_Id) := The_Notify_Handlers(Last_Id) + Handler_Data'(Handler, For_Code);
  end Add_Notify_Handler;


  procedure Add_Function (Wide_Name : Win.WCHAR_Array;
                          Command   : Callback;
                          Toolbar   : Toolbar_Icons := No_Toolbar) is
    use type Win.INT;
  begin
    if The_Count = Function_Range'last then
      raise Too_Many_Commands;
    end if;
    The_Count := The_Count + 1;
    declare
      Item : Func_Item renames The_Function(The_Count);
    begin
      Item.Name (Item.Name'first .. Item.Name'first + Wide_Name'length - 1) := Wide_Name;
      case Function_Range(The_Count) is
      when 1 =>
        Item.P_Func := Command_1'access;
      when 2 =>
        Item.P_Func := Command_2'access;
      when 3 =>
        Item.P_Func := Command_3'access;
      when 4 =>
        Item.P_Func := Command_4'access;
      when 5 =>
        Item.P_Func := Command_5'access;
      when 6 =>
        Item.P_Func := Command_6'access;
      when 7 =>
        Item.P_Func := Command_7'access;
      when 8 =>
        Item.P_Func := Command_8'access;
      end case;
    end;
    The_Command(The_Count) := Command;
    The_Toolbar(The_Count) := Toolbar;
  end Add_Function;


  The_Info : Info;

  function Handle return Win.HWND is
  begin
    return The_Info.Npp_Handle;
  end Handle;


  function Edit_View return Win.HWND is
    The_Current_Edit : aliased Win.INT;
    use type Win.INT;
  begin
    Win.Send_Message (The_Info.Npp_Handle, M_GETCURRENTSCINTILLA, 0, Convert (The_Current_Edit'address));
    if The_Current_Edit = 0 then
      return The_Info.Scintilla_Main_Handle;
    else
      return The_Info.Scintilla_Second_Handle;
    end if;
  end Edit_View;


  function Switched_To (Filename : String) return Boolean is
    use type Win.LRESULT;
    Wide_Filename : Wide_String := Ada.Characters.Handling.To_Wide_String (Filename) & Wide_Nul;
  begin
    return Win.Send_Message (To   => The_Info.Npp_Handle,
                             Msg  => Npp.M_SWITCHTOFILE,
                             Wpar => 0,
                             Lpar => Convert(Wide_Filename(Wide_Filename'first)'address)) = Win.OK;
  end Switched_To;


  function Opened (Filename : String) return Boolean is
    use type Win.LRESULT;
    Wide_Filename : Wide_String := Ada.Characters.Handling.To_Wide_String (Filename) & Wide_Nul;
  begin
    return Win.Send_Message (To   => The_Info.Npp_Handle,
                             Msg  => Npp.M_DOOPEN,
                             Wpar => 0,
                             Lpar => Convert(Wide_Filename(Wide_Filename'first)'address)) = Win.OK;
  end Opened;


  function All_Files_Saved return Boolean is
    use type Win.LRESULT;
  begin
    return Win.Send_Message (To   => The_Info.Npp_Handle,
                             Msg  => Npp.M_SAVEALLFILES,
                             Wpar => 0,
                             Lpar => 0) = Win.OK;
  end All_Files_Saved;


  procedure Hide_Menu is
    Dummy : Win.LRESULT;
  begin
    Dummy := Win.Send_Message (To   => The_Info.Npp_Handle,
                               Msg  => Npp.M_HIDEMENU,
                               Wpar => 0,
                               Lpar => Win.LPARAM(Win.TRUE));
  end Hide_Menu;


  procedure Show_Menu is
    Dummy : Win.LRESULT;
  begin
    Dummy := Win.Send_Message (To   => The_Info.Npp_Handle,
                               Msg  => Npp.M_HIDEMENU,
                               Wpar => 0,
                               Lpar => Win.LPARAM(Win.FALSE));
  end Show_Menu;


  procedure Hide_Tab_Bar is
    Dummy : Win.LRESULT;
  begin
    Dummy := Win.Send_Message (To   => The_Info.Npp_Handle,
                               Msg  => Npp.M_HIDETABBAR,
                               Wpar => 0,
                               Lpar => Win.LPARAM(Win.TRUE));
  end Hide_Tab_Bar;


  procedure Show_Tab_Bar is
    Dummy : Win.LRESULT;
  begin
    Dummy := Win.Send_Message (To   => The_Info.Npp_Handle,
                               Msg  => Npp.M_HIDETABBAR,
                               Wpar => 0,
                               Lpar => Win.LPARAM(Win.FALSE));
  end Show_Tab_Bar;


  procedure Hide_Tool_Bar is
    Dummy : Win.LRESULT;
  begin
    Dummy := Win.Send_Message (To   => The_Info.Npp_Handle,
                               Msg  => Npp.M_HIDETOOLBAR,
                               Wpar => 0,
                               Lpar => Win.LPARAM(Win.TRUE));
  end Hide_Tool_Bar;


  procedure Show_Tool_Bar is
    Dummy : Win.LRESULT;
  begin
    Dummy := Win.Send_Message (To   => The_Info.Npp_Handle,
                               Msg  => Npp.M_HIDETOOLBAR,
                               Wpar => 0,
                               Lpar => Win.LPARAM(Win.FALSE));
  end Show_Tool_Bar;


  procedure Hide_Header is
  begin
    Hide_Menu;
    Hide_Tab_Bar;
    Hide_Tool_Bar;
  end Hide_Header;


  procedure Show_Header is
  begin
    Show_Menu;
    Show_Tab_Bar;
    Show_Tool_Bar;
  end Show_Header;


  ---------------
  -- Interface --
  ---------------

  function Is_Unicode return Win.BOOL is
  begin
    return Win.TRUE;
  end Is_Unicode;


  procedure Set_Info (Data : Info) is
  begin
    The_Info := Data;
    Call (Set_Info_Callbacks);
  end Set_Info;


  function Get_Funcs_Array (Count : access Win.INT) return System.Address is
  begin
    Count.all := The_Count;
    return The_Function(The_Function'first)'address;
  end Get_Funcs_Array;


  procedure Create_Toolbar_Buttons is
  begin
    for Index in 1 .. The_Count loop
      if The_Toolbar(Index) /= No_Toolbar then
        Win.Send_Message (To   => The_Info.Npp_Handle,
                          Msg  => Npp.M_ADDTOOLBARICON,
                          Wpar => Win.WPARAM(The_Function(Index).Cmd_Id),
                          Lpar => Convert(The_Toolbar(Index)'address));
      end if;
    end loop;
  end Create_Toolbar_Buttons;


  procedure Be_Notified (Notif : access Scintilla.Notification) is
    Id_From : constant Win.LONG_PTR := Notif.Nmhdr.Id_From;
    use type Win.INT;
  begin
    if Id_From in Win.LONG_PTR(Id'first) ..  Win.LONG_PTR(Last_Id) then
      for Item of The_Notify_Handlers(Win.INT(Id_From)) loop
        if Item.For_Code = Notif.Nmhdr.Code then
          Item.Handler (Notif.Nmhdr.Hwnd_From);
        end if;
      end loop;
    end if;
    case Notif.Nmhdr.Code is
    when Npp.N_READY =>
      Call (Ready_Callbacks);
    when Npp.N_TBMODIFICATION =>
      Create_Toolbar_Buttons;
      Call (Tb_Modification_Callbacks);
    when Npp.N_FILEBEFOREOPEN =>
      null;
    when Npp.N_FILEOPENED =>
      null;
    when Npp.N_BUFFERACTIVATED =>
      Call (Buffer_Activated_Callbacks);
    when Npp.N_FILEBEFORECLOSE =>
      null;
    when Npp.N_FILECLOSED =>
      null;
    when Npp.N_FILEBEFORESAVE =>
      null;
    when Npp.N_FILESAVED =>
      Call (File_Saved_Callbacks);
    when Npp.N_SHUTDOWN =>
      if Client_Termination /= null then
        Client_Termination.all;
      end if;
    when Scintilla.SCN_UPDATEUI =>
      Call (Buffer_Updated_Callbacks);
    when others =>
      return;
    end case;
  end Be_Notified;

end Npp.Plugin;
