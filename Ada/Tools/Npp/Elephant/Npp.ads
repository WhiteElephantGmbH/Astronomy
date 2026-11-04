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

with Win;
with System;

package Npp with Pure => True is

  Wide_Nul : constant Wide_Character := Wide_Character'first;

  type Notify_Handler is access procedure (Data : System.Address);


  -------------
  -- Messages
  -------------

  M_FIRST : constant := Win.WM_USER + 1000;

  M_GETCURRENTSCINTILLA : constant := M_FIRST + 4;

  M_SETSTATUSBAR           : constant := M_FIRST + 24;
    STATUSBAR_DOC_TYPE     : constant := 0;
    STATUSBAR_DOC_SIZE     : constant := 1;
    STATUSBAR_CUR_POS      : constant := 2;
    STATUSBAR_EOF_FORMAT   : constant := 3;
    STATUSBAR_UNICODE_TYPE : constant := 4;
    STATUSBAR_TYPING_MODE  : constant := 5;

  M_DMMREGASDCKDLG  : constant := M_FIRST + 33;
  M_DMMVIEWOTHERTAB : constant := M_FIRST + 35;
  M_ADDTOOLBARICON  : constant := M_FIRST + 41;

  type Toolbar_Icons is record
    Bmp  : Win.HBITMAP := System.Null_Address;
    Icon : Win.HICON   := System.Null_Address;
  end record with Convention => C;

  No_Toolbar : constant Toolbar_Icons := (others => <>);

  MSG : constant := Win.WM_USER + 1000;

  M_DMMSHOW : constant := MSG + 30;
  -- void NPPM_DMMSHOW(0, tTbData->hClient)

  M_DMMHIDE	: constant := MSG + 31;
  -- void NPPM_DMMHIDE(0, tTbData->hClient)

  M_SWITCHTOFILE : constant := MSG + 37;
  -- BOOL NPPM_SWITCHTOFILE(0, TCHAR *filePathName2switch)
  -- The return value is TRUE (1) if the operation is successful, otherwise FALSE (0).

  M_SAVEALLFILES : constant := MSG + 39;
  -- BOOL NPPM_SAVEALLFILES(0, 0)
  -- The return value is TRUE (1) if the operation is successful, otherwise FALSE (0).

  M_RELOADFILE : constant := MSG + 36;
  -- BOOL NPPM_RELOADFILE(BOOL withAlert, TCHAR *filePathName2Reload)

  M_HIDETABBAR : constant := MSG + 51;
  -- BOOL NPPM_HIDETABBAR(0, BOOL hideOrNot)
  -- if hideOrNot is set as TRUE then tab bar will be hidden otherwise it'll be shown.
  -- return value : the old status value

  M_HIDETOOLBAR : constant := MSG + 70;
  -- BOOL NPPM_HIDETOOLBAR(0, BOOL hideOrNot)
  -- if hideOrNot is set as TRUE then tool bar will be hidden otherwise it'll be shown.
  -- return value : the old status value

  M_HIDEMENU : constant := MSG + 72;
  -- BOOL NPPM_HIDEMENU(0, BOOL hideOrNot)
  -- if hideOrNot is set as TRUE then menu will be hidden otherwise it'll be shown.
  -- return value : the old status value

  M_HIDESTATUSBAR : constant := MSG + 74;
  -- BOOL NPPM_HIDEMENU(0, BOOL hideOrNot)
  -- if hideOrNot is set as TRUE then status bar will be hidden otherwise it'll be shown.
  -- return value : the old status value

  M_DOOPEN : constant := MSG + 77;
  -- BOOL NPPM_DOOPEN(0, const TCHAR *fullPathName2Open)
  -- fullPathName2Open indicates the full file path name to be opened.
  -- The return value is TRUE (1) if the operation is successful, otherwise FALSE (0).

  RUNCOMMAND_USER      : constant := Win.WM_USER + 3000;
    VAR_NOT_RECOGNIZED : constant := 0;
    FULL_CURRENT_PATH  : constant := 1;
    CURRENT_DIRECTORY  : constant := 2;
    FILE_NAME          : constant := 3;
    NAME_PART          : constant := 4;
    EXT_PART           : constant := 5;
    CURRENT_WORD       : constant := 6;
    NPP_DIRECTORY      : constant := 7;

  M_GETFULLCURRENTPATH  : constant := RUNCOMMAND_USER + FULL_CURRENT_PATH;
  M_GETCURRENTDIRECTORY : constant := RUNCOMMAND_USER + CURRENT_DIRECTORY;
  M_GETFILENAME         : constant := RUNCOMMAND_USER + FILE_NAME;
  M_GETNAMEPART         : constant := RUNCOMMAND_USER + NAME_PART;
  M_GETEXTPART          : constant := RUNCOMMAND_USER + EXT_PART;
  M_GETCURRENTWORD      : constant := RUNCOMMAND_USER + CURRENT_WORD;
  M_GETNPPDIRECTORY     : constant := RUNCOMMAND_USER + NPP_DIRECTORY;


  ------------------
  -- Notifications
  ------------------

  N_FIRST : constant := 1000;

  N_READY : constant := N_FIRST + 1;
  -- To notify plugins that all the procedures of launchment of notepad++ are done.
  -- Scintilla.Notification->Nmhdr.Code = N_READY;
  -- Scintilla.Notification->Nmhdr.Hwnd_From = hwndNpp;
  -- Scintilla.Notification->Nmhdr.Id_From = 0;

  N_TBMODIFICATION : constant := N_FIRST + 2;
  -- To notify plugins that toolbar icons can be registered
  -- Scintilla.Notification->Nmhdr.Code = N_TB_MODIFICATION;
  -- Scintilla.Notification->Nmhdr.Hwnd_From = hwndNpp;
  -- Scintilla.Notification->Nmhdr.Id_From = 0;

  N_FILEBEFORECLOSE : constant := N_FIRST + 3;
  -- To notify plugins that the current file is about to be closed
  -- Scintilla.Notification->Nmhdr.Code = N_FILEBEFORECLOSE;
  -- Scintilla.Notification->Nmhdr.Hwnd_From = hwndNpp;
  -- Scintilla.Notification->Nmhdr.Id_From = BufferID;

  N_FILEOPENED : constant := N_FIRST + 4;
  -- To notify plugins that the current file is just opened
  -- Scintilla.Notification->Nmhdr.Code = N_FILEOPENED;
  -- Scintilla.Notification->Nmhdr.Hwnd_From = hwndNpp;
  -- Scintilla.Notification->Nmhdr.Id_From = BufferID;

  N_FILECLOSED : constant := N_FIRST + 5;
  -- To notify plugins that the current file is just closed
  -- Scintilla.Notification->Nmhdr.Code = N_FILECLOSED;
  -- Scintilla.Notification->Nmhdr.Hwnd_From = hwndNpp;
  -- Scintilla.Notification->Nmhdr.Id_From = BufferID;

  N_FILEBEFOREOPEN : constant := N_FIRST + 6;
  -- To notify plugins that the current file is about to be opened
  -- Scintilla.Notification->Nmhdr.Code = N_FILEBEFOREOPEN;
  -- Scintilla.Notification->Nmhdr.Hwnd_From = hwndNpp;
  -- Scintilla.Notification->Nmhdr.Id_From = BufferID;

  N_FILEBEFORESAVE : constant := N_FIRST + 7;
  -- To notify plugins that the current file is about to be saved
  -- Scintilla.Notification->Nmhdr.Code = N_FILEBEFORESAVE;
  -- Scintilla.Notification->Nmhdr.Hwnd_From = hwndNpp;
  -- Scintilla.Notification->Nmhdr.Id_From = BufferID;

  N_FILESAVED : constant := N_FIRST + 8;
  -- To notify plugins that the current file is just saved
  -- Scintilla.Notification->Nmhdr.Code = N_FILESAVED;
  -- Scintilla.Notification->Nmhdr.Hwnd_From = hwndNpp;
  -- Scintilla.Notification->Nmhdr.Id_From = BufferID;

  N_SHUTDOWN : constant := N_FIRST + 9;
  -- To notify plugins that Notepad++ is about to be shutdowned.
  -- Scintilla.Notification->Nmhdr.Code = N_SHUTDOWN;
  -- Scintilla.Notification->Nmhdr.Hwnd_From = hwndNpp;
  -- Scintilla.Notification->Nmhdr.Id_From = 0;

  N_BUFFERACTIVATED : constant := N_FIRST + 10;
  -- To notify plugins that a buffer was activated (put to foreground).
  -- Scintilla.Notification->Nmhdr.Code = NPPN_BUFFERACTIVATED;
  -- Scintilla.Notification->Nmhdr.Hwnd_From = hwndNpp;
  -- Scintilla.Notification->Nmhdr.Id_From = activatedBufferID;

  N_LANGCHANGED : constant := N_FIRST + 11;
  -- To notify plugins that the language in the current doc is just changed.
  -- Scintilla.Notification->Nmhdr.Code = N_LANGCHANGED;
  -- Scintilla.Notification->Nmhdr.Hwnd_From = hwndNpp;
  -- Scintilla.Notification->Nmhdr.Id_From = currentBufferID;

  N_WORDSTYLESUPDATED : constant := N_FIRST + 12;
  -- To notify plugins that user initiated a WordStyleDlg change.
  -- Scintilla.Notification->Nmhdr.Code = N_WORDSTYLESUPDATED;
  -- Scintilla.Notification->Nmhdr.Hwnd_From = hwndNpp;
  -- Scintilla.Notification->Nmhdr.Id_From = currentBufferID;

  N_SHORTCUTREMAPPED : constant := N_FIRST + 13;
  -- To notify plugins that plugin command shortcut is remapped.
  -- Scintilla.Notification->Nmhdr.Code = N_SHORTCUTSREMAPPED;
  -- Scintilla.Notification->Nmhdr.Hwnd_From = ShortcutKeyStructurePointer;
  -- Scintilla.Notification->Nmhdr.Id_From = cmdID;
  -- where ShortcutKeyStructurePointer is pointer of struct ShortcutKey

  N_FILEBEFORELOAD : constant := N_FIRST + 14;
  -- To notify plugins that the current file is about to be loaded
  -- Scintilla.Notification->Nmhdr.Code = NPPN_FILEBEFOREOPEN;
  -- Scintilla.Notification->Nmhdr.Hwnd_From = hwndNpp;
  -- Scintilla.Notification->Nmhdr.Id_From = NULL;

  N_FILELOADFAILED : constant := N_FIRST + 15;
  -- To notify plugins that file open operation failed
  -- Scintilla.Notification->Nmhdr.Code = NPPN_FILEOPENFAILED;
  -- Scintilla.Notification->Nmhdr.Hwnd_From = hwndNpp;
  -- Scintilla.Notification->Nmhdr.Id_From = BufferID;

  N_READONLYCHANGED : constant := N_FIRST + 16;
  -- To notify plugins that current document change the readonly status,
  -- Scintilla.Notification->Nmhdr.Code = N_READONLYCHANGED;
  -- Scintilla.Notification->Nmhdr.Hwnd_From = bufferID;
  -- Scintilla.Notification->Nmhdr.Id_From = docStatus;
  -- where bufferID is BufferID
  -- and docStatus can be combined by DOCSTAUS_READONLY and DOCSTAUS_BUFFERDIRTY

  DOCSTAUS_READONLY    : constant := 1;
  DOCSTAUS_BUFFERDIRTY : constant := 2;

  N_DOCORDERCHANGED : constant := N_FIRST + 16;
  -- To notify plugins that document order is changed
  -- Scintilla.Notification->Nmhdr.Code = N_DOCORDERCHANGED;
  -- Scintilla.Notification->Nmhdr.Hwnd_From = newIndex;
  -- Scintilla.Notification->Nmhdr.Id_From = BufferID;


  -------------
  -- Docking --
  -------------

  CONT_LEFT   : constant := 0;
  CONT_RIGHT  : constant := 1;
  CONT_TOP    : constant := 2;
  CONT_BOTTOM : constant := 3;

  DWS_DF_CONT_LEFT   : constant := CONT_LEFT * 2 ** 28;
  DWS_DF_CONT_RIGHT  : constant := CONT_RIGHT * 2 ** 28;
  DWS_DF_CONT_TOP    : constant := CONT_TOP * 2 ** 28;
  DWS_DF_CONT_BOTTOM : constant := CONT_BOTTOM * 2 ** 28;

  type Dock_Data is record
    Client      : Win.HWND;       -- client Window Handle
    Name        : System.Address; -- name of plugin (shown in window)
    Dlg_ID      : Win.INT;        -- a funcItem provides the function pointer to start a dialog
    Mask        : Win.UINT;       -- mask params: look to above defines
    Icon_Tab    : Win.HICON;      -- icon for tabs
    Add_Info    : System.Address; -- for plugin to display additional informations
    Rc_Float    : Win.RECT;       -- floating position
    Prev_Cont   : Win.INT;        -- stores the privious container (toggling between float and dock)
    Module_Name : System.Address; -- it's the plugin file name. It's used to identify the plugin
  end record with Convention => C;

end Npp;