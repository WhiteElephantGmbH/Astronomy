-- *********************************************************************************************************************
-- *                       (c) 2014 .. 2019 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

package body Npp.Tree_View is

  The_Tree_View : Win.HWND;

  The_Tree_View_Data : Dock_Data;

  Initial_Title : aliased constant Wide_String := "Usage" & Wide_Nul;

  Empty_Name : aliased constant Wide_String := "" & Wide_Nul;

  Class_Name : aliased constant Wide_String := "SysTreeView32" & Wide_Nul;

  The_Dialog_Id : Win.INT;

  Double_Click_Handler : Notify_Handler;
  Focus_Lost_Handler   : Notify_Handler;

  procedure Create_Tree_View_Window is

    use type Win.DWORD;

    Window_Style : constant Win.DWORD := Win.WS_CHILD +
                                         Win.WS_BORDER +
                                         Win.TVS_HASBUTTONS +
                                         Win.TVS_HASLINES +
                                         Win.TVS_LINESATROOT;
    use type Win.INT;

    function Convert is new Ada.Unchecked_Conversion (Win.INT_PTR, System.Address);
    function Convert is new Ada.Unchecked_Conversion (System.Address, Win.LPARAM);

  begin
    The_Tree_View := Win.Create_Window (Class_Name  => Class_Name,
                                        Window_Name => Empty_Name,
                                        Style      => Window_Style,
                                        X          => 0,
                                        Y          => 0,
                                        Width      => 300,
                                        Height     => 100,
                                        Wnd_Parent => Plugin.Handle,
                                        Menu       => Convert(Win.INT_PTR(The_Dialog_Id)),
                                        Instance   => System.Null_Address,
                                        Param      => System.Null_Address);

    The_Tree_View_Data := (Client      => The_Tree_View,
                           Name        => Initial_Title'address,
                           Dlg_ID      => The_Dialog_Id,
                           Mask        => DWS_DF_CONT_RIGHT,
                           Icon_Tab    => System.Null_Address,
                           Add_Info    => System.Null_Address,
                           Rc_Float    => (0, 0, 0, 0),
                           Prev_Cont   => -1,
                           Module_Name => Plugin.Name);

    Win.Send_Message (Plugin.Handle, M_DMMREGASDCKDLG, 0, Convert(The_Tree_View_Data'address));
  end Create_Tree_View_Window;


  function Parent_Of (Tree : Item) return Item is
    function Convert is new Ada.Unchecked_Conversion (Item, Win.LPARAM);
    Parent : Item;
    use type Item;
  begin
    if Tree = Root then
      raise Item_Is_Root;
    end if;
    Parent := Item(Win.Send_Message (The_Tree_View,
                                     Win.TVM_GETNEXTITEM,
                                     Win.TVGN_PARENT,
                                     Convert (Tree)));
    if Parent = 0 then
      return Root;
    else
      return Parent;
    end if;
  end Parent_Of;


  procedure Set_Has_Children (Tree : Item;
                              Mark : Boolean) is

    The_Item : aliased Win.TV_ITEM_ANSI;

    function Convert is new Ada.Unchecked_Conversion (System.Address, Win.LPARAM);

  begin
    The_Item.Mask := Win.TVIF_CHILDREN;
    The_Item.Hitem := Tree;
    The_Item.Children := Boolean'pos(Mark);
    Win.Send_Message (The_Tree_View,
                      Win.TVM_SETITEM_ANSI,
                      0, Convert(The_Item'address));
  end Set_Has_Children;


  function First_Child_Of (Data : Item) return Item is

    Child : Item;

    function Convert is new Ada.Unchecked_Conversion (Item, Win.LPARAM);

    use type Item;

  begin
    Child := Item(Win.Send_Message (The_Tree_View,
                                    Win.TVM_GETNEXTITEM,
                                    Win.TVGN_CHILD,
                                    Convert (Data)));
    if Child = 0 then
      return Root;
    else
      return Child;
    end if;
  end First_Child_Of;


  procedure Delete (Tree : Item) is

    Parent : Item := Root;

    function Convert is new Ada.Unchecked_Conversion (Item, Win.LPARAM);

    use type Item;

  begin
    if Tree /= Root then
      Parent := Parent_Of (Tree);
    end if;
    Win.Send_Message (The_Tree_View,
                      Win.TVM_DELETEITEM,
                      0, Convert (Tree));
    if Parent /= Root and then First_Child_Of (Parent) = Root then
      Set_Has_Children (Parent, False);
    end if;
  end Delete;


  procedure Double_Click_Handling (Handle : Win.HWND) is
    Selected_Item : aliased Win.LRESULT;
    The_Item      : aliased Win.TV_ITEM_ANSI;

    function Convert is new Ada.Unchecked_Conversion (Win.LPARAM, System.Address);
    function Convert is new Ada.Unchecked_Conversion (System.Address, Win.LPARAM);

  begin
    if Double_Click_Handler /= null then
      Selected_Item := Win.Send_Message (Handle,
                                         Win.TVM_GETNEXTITEM,
                                         Win.TVGN_CARET,
                                         Convert(Selected_Item'address));
      The_Item.Hitem := Win.HTREEITEM(Selected_Item);
      The_Item.Mask := Win.TVIF_PARAM;
      Win.Send_Message (Handle,
                        Win.TVM_GETITEM_ANSI,
                        Win.TVGN_CARET,
                        Convert(The_Item'address));
      Double_Click_Handler (Convert (The_Item.Lparam));
    end if;
  end Double_Click_Handling;


  procedure Kill_Focus (Handle : Win.HWND) is
  begin
    if Focus_Lost_Handler /= null then
      Focus_Lost_Handler (Handle);
    end if;
  end Kill_Focus;


  procedure Install (Double_Click : Notify_Handler;
                     Focus_Lost   : Notify_Handler := null) is
  begin
    Plugin.Install (Tb_Modification => Create_Tree_View_Window'access);
    The_Dialog_Id := Plugin.Last_Dialog_Id;
    Plugin.Add_Notify_Handler (Double_Click_Handling'access, Win.NM_DBLCLK);
    Plugin.Add_Notify_Handler (Kill_Focus'access, Win.NM_KILLFOCUS);
    Double_Click_Handler := Double_Click;
    Focus_Lost_Handler := Focus_Lost;
  end Install;


  procedure Clear is
  begin
    Delete (Root);
  end Clear;


  function Add (Title  : String;
                Parent : Item := Root;
                Data   : System.Address := No_Data) return Item is

    Image : aliased constant String := Title & Ascii.Nul;

    The_Item      : aliased Win.TV_ITEM_ANSI;
    The_Insertion : aliased Win.TV_INSERTSTRUCT_ANSI;
    New_Item      : Item;

    use type Item;
    use type Win.UINT;

    function Convert is new Ada.Unchecked_Conversion (System.Address, Win.LPARAM);

  begin
    The_Item.Mask := Win.TVIF_TEXT + Win.TVIF_PARAM + Win.TVIF_CHILDREN;
    The_Item.Text := Image(Image'first)'address;
    The_Item.Lparam := Convert (Data);
    The_Item.Children := 0;
    The_Insertion.Parent := Parent;
    The_Insertion.Insertafter := Item(Win.TVI_LAST);
    The_Insertion.Item := The_Item;
    New_Item := Item(Win.Send_Message (The_Tree_View,
                                       Win.TVM_INSERTITEM_ANSI,
                                       0, Convert(The_Insertion'address)));
    if Parent /= Root then
      Set_Has_Children (Parent, True);
    end if;
    return New_Item;
  end Add;


  procedure Expand (Data : Item) is
    function Convert is new Ada.Unchecked_Conversion (Item, Win.LPARAM);
  begin
    Win.Send_Message (The_Tree_View,
                      Win.TVM_EXPAND,
                      Win.TVE_EXPAND,
                      Convert(Data));
  end Expand;

end Npp.Tree_View;
