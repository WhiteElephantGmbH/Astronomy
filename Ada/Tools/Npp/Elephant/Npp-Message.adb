-- *********************************************************************************************************************
-- *                       (c) 2013 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Unchecked_Conversion;
with Npp.Plugin;

package body Npp.Message is

  function Convert is new Ada.Unchecked_Conversion (System.Address, Win.LPARAM);

  The_Messages : Win.HWND;

  The_Messages_Data : Dock_Data;

  Initial_Title : aliased constant Wide_String := "Messages" & Wide_Nul;

  Empty_Name : aliased constant Wide_String := "" & Wide_Nul;

  Class_Name : aliased constant Wide_String := "RichEdit20W" & Wide_Nul;

  The_Dialog_Id : Win.INT;


  procedure Load_Rich_Edit_Dll is
    Dll_Name : aliased constant Wide_String := "RichEd20.dll" & Wide_Nul;
    Rich_Edit_Dll : Win.HINSTANCE := Win.Load_Library (Dll_Name) with Unreferenced;
  begin
    null;
  end Load_Rich_Edit_Dll;


  procedure Create_Message_Window is

    use type Win.DWORD;

    Window_Style : constant Win.DWORD := Win.WS_CHILD +
                                         Win.ES_MULTILINE +
                                         Win.ES_READONLY +
                                         Win.ES_SUNKEN +
                                         Win.ES_SAVESEL +
                                         Win.WS_HSCROLL +
                                         Win.WS_VSCROLL;
    use type Win.INT;
    use type Win.LRESULT;

  begin
    The_Messages := Win.Create_Window (Class_Name  => Class_Name,
                                       Window_Name => Empty_Name,
                                       Style       => Window_Style,
                                       X           => 0,
                                       Y           => 0,
                                       Width       => 300,
                                       Height      => 100,
                                       Wnd_Parent  => Plugin.Handle,
                                       Menu        => System.Null_Address,
                                       Instance    => System.Null_Address,
                                       Param       => System.Null_Address);

    The_Messages_Data := (Client      => The_Messages,
                          Name        => Initial_Title'address,
                          Dlg_ID      => The_Dialog_Id,
                          Mask        => DWS_DF_CONT_BOTTOM,
                          Icon_Tab    => System.Null_Address,
                          Add_Info    => System.Null_Address,
                          Rc_Float    => (0, 0, 0, 0),
                          Prev_Cont   => -1,
                          Module_Name => Plugin.Name);

    Win.Send_Message (Plugin.Handle, M_DMMREGASDCKDLG, 0, Convert(The_Messages_Data'address));
  end Create_Message_Window;


  procedure Install is
  begin
    Plugin.Install (Set_Info_Call   => Load_Rich_Edit_Dll'access,
                    Tb_Modification => Create_Message_Window'access);
    The_Dialog_Id := Plugin.Last_Dialog_Id;
  end Install;


  procedure Write (The_Text       : String;
                   The_Color      : Win.COLORREF := Win.Black;
                   Ensure_Visible : Boolean := True) is
    Text : aliased constant String := The_Text & Ascii.Nul;
    Charformat : aliased Win.CHARFORMAT;
    use type Win.LONG;
    use type Win.ULONG;
    Everything : aliased constant Win.CHARRANGE := (Min => -1, Max => -1);
  begin
    Charformat.Mask := Win.CFM_COLOR + Win.CFM_SIZE;
    Charformat.Effects := 0;
    Charformat.Height := 11 * 20;
    Charformat.Textcolor := The_Color;
    Win.Send_Message (The_Messages,
                      Win.EM_SETCHARFORMAT,
                      Win.WPARAM(Win.SCF_SELECTION),
                      Convert(Charformat'address));
    Win.Send_Message (The_Messages,
                      Win.EM_EXSETSEL,
                      Win.WPARAM(0),
                      Convert(Everything'address));
    Win.Send_Message (The_Messages,
                      Win.EM_REPLACESEL,
                      Win.WPARAM(0),
                      Convert(Text(Text'first)'address));
    if Ensure_Visible then
      Win.Send_Message (The_Messages,
                        Win.WM_VSCROLL,
                        Win.WPARAM(Win.SB_BOTTOM),
                        0);
    end if;
  end Write;


  procedure Write_Line (The_Text       : String;
                        The_Color      : Win.COLORREF := Win.Black;
                        Ensure_Visible : Boolean := True) is
  begin
    Write (The_Text & Ascii.Cr & Ascii.Lf, The_Color, Ensure_Visible);
  end Write_Line;


  procedure Clear is
  begin
    Win.Send_Message (Plugin.Handle, M_DMMVIEWOTHERTAB, 0, Convert(Initial_Title'address));
    Win.Send_Message (The_Messages,
                      Win.WM_SETTEXT,
                      Win.WPARAM(0),
                      Win.LPARAM(0));
  end Clear;

end Npp.Message;
