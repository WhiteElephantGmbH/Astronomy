-- *********************************************************************************************************************
-- *                       (c) 2002 .. 2019 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Win32.Windef;
with Win32.Wingdi;
with Win32.Winuser;

package Win32.Comctl is

  -- Extended Window Styles

  Ws_Ex_Windowedge       : constant := 16#00000100#;
  Ws_Ex_Clientedge       : constant := 16#00000200#;
  Ws_Ex_Staticedge       : constant := 16#00020000#;
  Ws_Ex_Overlappedwindow : constant := Ws_Ex_Windowedge + Ws_Ex_Clientedge;


  -- Dialog Styles

  Ds_Absalign      : constant := 16#00000001#;
  Ds_Sysmodal      : constant := 16#00000002#;
  Ds_Localedit     : constant := 16#00000020#; -- Edit items get Local storage.
  Ds_Setfont       : constant := 16#00000040#; -- User specified font for Dlg controls
  Ds_Modalframe    : constant := 16#00000080#; -- Can be combined with WS_CAPTION
  Ds_Noidlemsg     : constant := 16#00000100#; -- WM_ENTERIDLE message will not be sent
  Ds_Setforeground : constant := 16#00000200#; -- not in win3.1

  Ds_3dlook        : constant := 16#00000004#;
  Ds_Fixedsys      : constant := 16#00000008#;
  Ds_Nofailcreate  : constant := 16#00000010#;
  Ds_Control       : constant := 16#00000400#;
  Ds_Center        : constant := 16#00000800#;
  Ds_Centermounse  : constant := 16#00001000#;
  Ds_Contexthelp   : constant := 16#00002000#;

  -- New edit control styles

  Es_Savesel         : constant := 16#00008000#;
  Es_Sunken          : constant := 16#00004000#;
  Es_Disablenoscroll : constant := 16#00002000#;
  Es_Selectionbar    : constant := 16#01000000#;
  Es_Nooledragdrop   : constant := 16#00000008#;

  -- New Common Control definitions

  Wm_Notify       : constant := 78;
  Wm_Notifyformat : constant := 85;

  Nfr_Ansi    : constant := 1;
  Nfr_Unicode : constant := 2;

  Sbs_Sizegrip : constant := 16#10#;

  Nm_First      : constant := 0;
  Nm_Click      : constant := Nm_First - 2;
  Nm_Dblclk     : constant := Nm_First - 3;
  Nm_Return     : constant := Nm_First - 4;
  Nm_Rclick     : constant := Nm_First - 5;
  Nm_Rdblclk    : constant := Nm_First - 6;
  Nm_Setfocus   : constant := Nm_First - 7;
  Nm_Killfocus  : constant := Nm_First - 8;
  Nm_Customdraw : constant := Nm_First - 12;
  Nm_Hover      : constant := Nm_First - 13;

  type Nm_Hdr is record
    Hwndfrom : Win32.Windef.HWND;
    Idfrom   : Win32.INT_PTR;
    Code     : Win32.INT;
  end record with Convention => C;

  type Nm_Hdr_Ptr is access Nm_Hdr;

  type Nm_Mouse is record
    Hdr      : Nm_Hdr;
    Itemspec : Win32.DWORD_PTR;
    Itemdata : Win32.DWORD_PTR;
    Point    : Win32.Windef.POINT;
    Hitinfo  : Win32.LPARAM;
  end record with Convention => C;

  type Nm_Mouse_Ptr is access Nm_Mouse;

  type Nm_Listview is record
    Hdr      : Nm_Hdr;
    Item     : Win32.INT;
    Subitem  : Win32.INT;
    Newstate : Win32.UINT;
    Oldstate : Win32.UINT;
    Changed  : Win32.UINT;
    Ptaction : Win32.Windef.POINT;
    Lparam   : Win32.LPARAM;
  end record with Convention => C;

  type Nm_Listview_Ptr is access all Nm_Listview;

  type Nm_Custom_Draw is record
    Hdr       : Nm_Hdr;
    Drawstage : Win32.DWORD;
    Hdc       : Win32.Windef.HDC;
    Rc        : Win32.Windef.RECT;
    Itemspec  : Win32.DWORD;
    Itemstate : Win32.UINT;
    Itemparam : Win32.LPARAM;
  end record with Convention => C;

  type Nmlv_Customdraw is record
    Nmcd       : Nm_Custom_Draw;
    Textcolor  : Win32.Windef.COLORREF;
    Bkgndcolor : Win32.Windef.COLORREF;
    Subitem    : Win32.INT;
  end record with Convention => C;

  type Customdraw_Info_Ptr is access all Nmlv_Customdraw;

  -- Common control shared messages

  Ccm_First            : constant := 16#2000#;
  Ccm_Setcolorscheme   : constant := Ccm_First + 2;
  Ccm_Getcolorscheme   : constant := Ccm_First + 3;
  Ccm_Getdroptarget    : constant := Ccm_First + 4;
  Ccm_Setunicodeformat : constant := Ccm_First + 5;
  Ccm_Getunicodeformat : constant := Ccm_First + 6;

  -- Custom Draw - Return flags
  -- values under 0x00010000 are reserved for global custom draw values.
  -- above that are for specific controls

  Cdrf_Dodefault         : constant := 16#00000000#;
  Cdrf_Newfont           : constant := 16#00000002#;
  Cdrf_Skipdefault       : constant := 16#00000004#;
  Cdrf_Notifypostpaint   : constant := 16#00000010#;
  Cdrf_Notifyitemdraw    : constant := 16#00000020#;
  Cdrf_Notifysubitemdraw : constant := 16#00000020#; -- flags are the same, we can distinguish by context
  Cdrf_Notifyposterase   : constant := 16#00000040#;

  -- Custom Draw - Drawstage flags
  -- values under 0x00010000 are reserved for global custom draw values.
  -- above that are for specific controls

  Cdds_Prepaint  : constant := 16#00000001#;
  Cdds_Postpaint : constant := 16#00000002#;
  Cdds_Preerase  : constant := 16#00000003#;
  Cdds_Posterase : constant := 16#00000004#;
-- the 0x000010000 bit means it's individual item specific
  Cdds_Item          : constant := 16#00010000#;
  Cdds_Itemprepaint  : constant := Cdds_Item + Cdds_Prepaint;
  Cdds_Itempostpaint : constant := Cdds_Item + Cdds_Postpaint;
  Cdds_Itempreerase  : constant := Cdds_Item + Cdds_Preerase;
  Cdds_Itemposterase : constant := Cdds_Item + Cdds_Posterase;
  Cdds_Subitem       : constant := 16#00020000#;

  -- itemState flags

  Cdis_Selected       : constant := 16#0001#;
  Cdis_Grayed         : constant := 16#0002#;
  Cdis_Disabled       : constant := 16#0004#;
  Cdis_Checked        : constant := 16#0008#;
  Cdis_Focus          : constant := 16#0010#;
  Cdis_Default        : constant := 16#0020#;
  Cdis_Hot            : constant := 16#0040#;
  Cdis_Marked         : constant := 16#0080#;
  Cdis_Indeterminate  : constant := 16#0100#;

  -- Listview constants

  Lvm_First                    : constant := 16#1000#;
  Lvm_Getitem_Ansi             : constant := Lvm_First + 5;
  Lvm_Setitem_Ansi             : constant := Lvm_First + 6;
  Lvm_Insertitem_Ansi          : constant := Lvm_First + 7;
  Lvm_Deleteitem               : constant := Lvm_First + 8;
  Lvm_Deleteallitems           : constant := Lvm_First + 9;
  Lvm_Getnextitem              : constant := Lvm_First + 12;
  Lvm_Finditem_Ansi            : constant := Lvm_First + 13;
  Lvm_Getstringwidth_Ansi      : constant := Lvm_First + 17;
  Lvm_Ensurevisible            : constant := Lvm_First + 19;
  Lvm_Editlabel_Ansi           : constant := Lvm_First + 23;
  Lvm_Getcolumn_Ansi           : constant := Lvm_First + 25;
  Lvm_Setcolumn_Ansi           : constant := Lvm_First + 26;
  Lvm_Insertcolumn_Ansi        : constant := Lvm_First + 27;
  Lvm_Deletecolumn             : constant := Lvm_First + 28;
  Lvm_Getcolumnwidth           : constant := Lvm_First + 29;
  Lvm_Setcolumnwidth           : constant := Lvm_First + 30;
  Lvm_Settextcolor             : constant := Lvm_First + 36;
  Lvm_Getcountperpage          : constant := Lvm_First + 40;
  Lvm_Update                   : constant := Lvm_First + 42;
  Lvm_Getitemtext_Ansi         : constant := Lvm_First + 45;
  Lvm_Setitemtext_Ansi         : constant := Lvm_First + 46;
  Lvm_Setitemcount             : constant := Lvm_First + 47;
  Lvm_Sortitems                : constant := Lvm_First + 48;
  LVM_Setitemposition32        : constant := Lvm_First + 49;
  LVM_Getselectedcount         : constant := Lvm_First + 50;
  LVM_Getitemspacing           : constant := Lvm_First + 51;
  Lvm_Getisearchstring_Ansi    : constant := Lvm_First + 52;
  Lvm_Setextendedlistviewstyle : constant := Lvm_First + 54;
  Lvm_Getextendedlistviewstyle : constant := Lvm_First + 55;
  Lvm_Getitem_Wide             : constant := Lvm_First + 75;
  Lvm_Setitem_Wide             : constant := Lvm_First + 76;
  Lvm_Insertitem_Wide          : constant := Lvm_First + 77;
  Lvm_Finditem_Wide            : constant := Lvm_First + 83;
  Lvm_Getstringwidth_Wide      : constant := Lvm_First + 87;
  Lvm_Getcolumn_Wide           : constant := Lvm_First + 95;
  Lvm_Setcolumn_Wide           : constant := Lvm_First + 96;
  Lvm_Insertcolumn_Wide        : constant := Lvm_First + 97;
  Lvm_Getitemtext_Wide         : constant := Lvm_First + 115;
  Lvm_Setitemtext_Wide         : constant := Lvm_First + 116;
  Lvm_Getisearchstring_Wide    : constant := Lvm_First + 117;
  Lvm_Editlabel_Wide           : constant := Lvm_First + 118;

  Lvm_Setunicodeformat         : constant := Ccm_Setunicodeformat;
  Lvm_Getunicodeformat         : constant := Ccm_Getunicodeformat;

  Lvni_All         : constant := 0;
  Lvni_Focused     : constant := 1;
  Lvni_Selected    : constant := 2;
  Lvni_Cut         : constant := 4;
  Lvni_Drophilited : constant := 8;
  Lvni_Above       : constant := 256;
  Lvni_Below       : constant := 512;
  Lvni_Toleft      : constant := 1024;
  Lvni_Toright     : constant := 2048;

  Lvs_Icon            : constant := 16#0000#;
  Lvs_Report          : constant := 16#0001#;
  Lvs_Smallicon       : constant := 16#0002#;
  Lvs_List            : constant := 16#0003#;
  Lvs_Singelselect    : constant := 16#0004#;
  Lvs_Showselalways   : constant := 16#0008#;
  Lvs_Sortascending   : constant := 16#0010#;
  Lvs_Sortdescending  : constant := 16#0020#;
  Lvs_Shareimagelists : constant := 16#0040#;
  Lvs_Nolabelwrap     : constant := 16#0080#;
  Lvs_Autoarrange     : constant := 16#0100#;
  Lvs_Editlabels      : constant := 16#0200#;
  Lvs_Ownerdata       : constant := 16#1000#;
  Lvs_Noscroll        : constant := 16#2000#;
  Lvs_Nocolumnheader  : constant := 16#4000#;
  Lvs_Nosortheader    : constant := 16#8000#;

  Lvs_Ex_Gridlines        : constant := 16#1#;
  Lvs_Ex_Subitemimages    : constant := 16#2#;
  Lvs_Ex_Checkboxes       : constant := 16#4#;
  Lvs_Ex_Trackselect      : constant := 16#8#;
  Lvs_Ex_Headerdragdrop   : constant := 16#10#;
  Lvs_Ex_Fullrowselect    : constant := 16#20#;  -- Applies to report mode only
  Lvs_Ex_Oneclickactivate : constant := 16#40#;
  Lvs_Ex_Twoclickactivate : constant := 16#80#;
  Lvs_Ex_Flatsb           : constant := 16#100#;
  Lvs_Ex_Regional         : constant := 16#200#;
  Lvs_Ex_Infotip          : constant := 16#400#; -- Listview does InfoTips for you
  Lvs_Ex_Underlinehot     : constant := 16#800#;
  Lvs_Ex_Underlinecold    : constant := 16#1000#;
  Lvs_Ex_Multiuworkareas  : constant := 16#2000#;
  Lvs_Ex_Labeltip         : constant := 16#4000#;
  Lvs_Ex_Borderselect     : constant := 16#8000#;

  Lvcf_Fmt     : constant := 16#1#;
  Lvcf_Width   : constant := 16#2#;
  Lvcf_Text    : constant := 16#4#;
  Lvcf_Subitem : constant := 16#8#;

  Lvcfmt_Left            : constant := 16#0#;
  Lvcfmt_Right           : constant := 16#1#;
  Lvcfmt_Center          : constant := 16#2#;
  Lvcfmt_Justifymask     : constant := 16#3#;
  Lvcfmt_Bitmap_On_Right : constant := 16#800#;
  Lvcfmt_Col_Has_Images  : constant := 16#1000#;

  Lvif_Text  : constant := 16#1#;
  Lvif_Image : constant := 16#2#;
  Lvif_Param : constant := 16#4#;
  Lvif_State : constant := 16#8#;

  Lvn_First               : constant := -100;
  Lvn_Beginlabeledit_Ansi : constant := Lvn_First - 5;
  Lvn_Endlabeledit_Ansi   : constant := Lvn_First - 6;
  Lvn_Columnclick         : constant := Lvn_First - 8;
  Lvn_Getdisplayinfo_Ansi : constant := Lvn_First - 50;
  Lvn_Setdisplayinfo_Ansi : constant := Lvn_First - 51;
  Lvn_Beginlabeledit_Wide : constant := Lvn_First - 75;
  Lvn_Endlabeledit_Wide   : constant := Lvn_First - 76;
  Lvn_Getdisplayinfo_Wide : constant := Lvn_First - 77;
  Lvn_Setdisplayinfo_Wide : constant := Lvn_First - 78;

  type Lv_Column_Ansi is record
    Mask    : Win32.UINT;
    Fmt     : Win32.INT;
    Width   : Win32.INT;
    Text    : Win32.LPSTR;
    Maxtext : Win32.INT;
    Subitem : Win32.INT;
  end record with Convention => C;

  type Lv_Column_Wide is record
    Mask    : Win32.UINT;
    Fmt     : Win32.INT;
    Width   : Win32.INT;
    Text    : Win32.LPWSTR;
    Maxtext : Win32.INT;
    Subitem : Win32.INT;
  end record with Convention => C;

  type Lv_Item_Ansi is record
    Mask        : Win32.UINT;
    Item        : Win32.INT;
    Subitem     : Win32.INT;
    State       : Win32.UINT;
    Statemask   : Win32.UINT;
    Text        : Win32.LPSTR;
    Maxtextsize : Win32.INT;
    Image       : Win32.INT;
    Lparam      : Win32.LPARAM;
  end record with Convention => C;

  type Lv_Item_Ansi_Ptr is access Lv_Item_Ansi;

  type Lv_Item_Wide is record
    Mask        : Win32.UINT;
    Item        : Win32.INT;
    Subitem     : Win32.INT;
    State       : Win32.UINT;
    Statemask   : Win32.UINT;
    Text        : Win32.LPWSTR;
    Maxtextsize : Win32.INT;
    Image       : Win32.INT;
    Lparam      : Win32.LPARAM;
  end record with Convention => C;

  type Lv_Item_Wide_Ptr is access Lv_Item_Wide;

  type Lv_Dispinfo_Ansi is record
    Hdr  : Nm_Hdr;
    Item : Lv_Item_Ansi;
  end record with Convention => C;

  type Lv_Dispinfo_Ansi_Ptr is access Lv_Dispinfo_Ansi;

  type Lv_Dispinfo_Wide is record
    Hdr  : Nm_Hdr;
    Item : Lv_Item_Wide;
  end record with Convention => C;

  type Lv_Dispinfo_Wide_Ptr is access all Lv_Dispinfo_Wide;

  -- Progress Bars

  Pbm_Setrange    : constant := Winuser.WM_USER + 1;
  Pbm_Setpos      : constant := Winuser.WM_USER + 2;
  Pbm_Setdeltapos : constant := Winuser.WM_USER + 3;
  Pbm_Setstep     : constant := Winuser.WM_USER + 4;
  Pbm_Stepit      : constant := Winuser.WM_USER + 5;
  Pbm_Setrange32  : constant := Winuser.WM_USER + 6;
  Pbm_Getrange    : constant := Winuser.WM_USER + 7;
  Pbm_Getpos      : constant := Winuser.WM_USER + 8;
  Pbm_Setbarcolor : constant := Winuser.WM_USER + 9;

  -- Track Bars

  Tbs_Autoticks      : constant := 16#1#;
  Tbs_Vert           : constant := 16#2#;
  Tbs_Horz           : constant := 16#0#;
  Tbs_Top            : constant := 16#4#;
  Tbs_Bottom         : constant := 16#0#;
  Tbs_Left           : constant := 16#4#;
  Tbs_Right          : constant := 16#0#;
  Tbs_Both           : constant := 16#8#;
  Tbs_Noticks        : constant := 16#10#;
  Tbs_Enableselrange : constant := 16#20#;
  Tbs_Fixedlength    : constant := 16#40#;
  Tbs_Nothumb        : constant := 16#80#;
  Tbs_Tooltips       : constant := 16#100#;

  Tbm_Getpos         : constant := Winuser.WM_USER;
  Tbm_Getrangemin    : constant := Winuser.WM_USER + 1;
  Tbm_Getrangemax    : constant := Winuser.WM_USER + 2;
  Tbm_Gettic         : constant := Winuser.WM_USER + 3;
  Tbm_Settic         : constant := Winuser.WM_USER + 4;
  Tbm_Setpos         : constant := Winuser.WM_USER + 5;
  Tbm_Setrange       : constant := Winuser.WM_USER + 6;
  Tbm_Setrangemin    : constant := Winuser.WM_USER + 7;
  Tbm_Settangemax    : constant := Winuser.WM_USER + 8;
  Tbm_Cleartics      : constant := Winuser.WM_USER + 9;
  Tbm_Setsel         : constant := Winuser.WM_USER + 10;
  Tbm_Setselstart    : constant := Winuser.WM_USER + 11;
  Tbm_Setselend      : constant := Winuser.WM_USER + 12;
  Tbm_Getptics       : constant := Winuser.WM_USER + 14;
  Tbm_Ticpos         : constant := Winuser.WM_USER + 15;
  Tbm_Numtics        : constant := Winuser.WM_USER + 16;
  Tbm_Selstart       : constant := Winuser.WM_USER + 17;
  Tbm_Selend         : constant := Winuser.WM_USER + 18;
  Tbm_Clearsel       : constant := Winuser.WM_USER + 19;
  Tbm_Setticfreq     : constant := Winuser.WM_USER + 20;
  Tbm_Setpagesize    : constant := Winuser.WM_USER + 21;
  Tbm_Getpagesize    : constant := Winuser.WM_USER + 22;
  Tbm_Setlinesize    : constant := Winuser.WM_USER + 23;
  Tbm_Getlinesize    : constant := Winuser.WM_USER + 24;
  Tbm_Getthumbrect   : constant := Winuser.WM_USER + 25;
  Tbm_Getchannelrect : constant := Winuser.WM_USER + 26;
  Tbm_Setthumblength : constant := Winuser.WM_USER + 27;
  Tbm_Getthumblength : constant := Winuser.WM_USER + 28;
  Tbm_Settooltips    : constant := Winuser.WM_USER + 29;
  Tbm_Gettooltips    : constant := Winuser.WM_USER + 30;
  Tbm_Settipside     : constant := Winuser.WM_USER + 31;
  Tbm_Setbuddy       : constant := Winuser.WM_USER + 32;
  Tbm_Getbuddy       : constant := Winuser.WM_USER + 33;

  Tb_Lineup        : constant := 16#0#;
  Tb_Linedown      : constant := 16#1#;
  Tb_Pageup        : constant := 16#2#;
  Tb_Pagedown      : constant := 16#3#;
  Tb_Thumbposition : constant := 16#4#;
  Tb_Thumbtrack    : constant := 16#5#;
  Tb_Top           : constant := 16#6#;
  Tb_Bottom        : constant := 16#7#;
  Tb_Endtrack      : constant := 16#8#;

-- Tabs

  type Tc_Item is record
    Mask        : Win32.UINT;
    State       : Win32.DWORD;
    Statemask   : Win32.DWORD;
    Text        : Win32.LPSTR;
    Maxtextsize : Win32.INT;
    Image       : Win32.INT;
    Lparam      : Win32.LPARAM;
  end record with Convention => C;

  type Tc_Itemptr is access Tc_Item;

  Tcif_Text       : constant := 16#1#;
  Tcif_Image      : constant := 16#2#;
  Tcif_Rtlreading : constant := 16#4#;
  Tcif_Param      : constant := 16#8#;
  Tcif_State      : constant := 16#10#;

  Tcm_First          : constant := 16#1300#;
  Tcm_Getitem        : constant := Tcm_First + 5;
  Tcm_Insertitem     : constant := Tcm_First + 7;
  Tcm_Deleteitem     : constant := Tcm_First + 8;
  Tcm_Deleteallitems : constant := Tcm_First + 9;
  Tcm_Getcursel      : constant := Tcm_First + 11;
  Tcm_Setcursel      : constant := Tcm_First + 12;
  Tcm_Adjustrect     : constant := Tcm_First + 40;

  Tcn_First       : constant := -550;
  Tcn_Keydown     : constant := Tcn_First - 0;
  Tcn_Selchange   : constant := Tcn_First - 1;
  Tcn_Selchanging : constant := Tcn_First - 2;
  Tcn_Getobject   : constant := Tcn_First - 3;

-- Treeview

  Tvs_Hasbuttons      : constant := 16#01#;
  Tvs_Haslines        : constant := 16#02#;
  Tvs_Linesatroot     : constant := 16#04#;
  Tvs_Editlabels      : constant := 16#08#;
  Tvs_Disabledragdrop : constant := 16#10#;
  Tvs_Showselalways   : constant := 16#20#;

  Tvif_Text           : constant := 16#0001#;
  Tvif_Image          : constant := 16#0002#;
  Tvif_Param          : constant := 16#0004#;
  Tvif_State          : constant := 16#0008#;
  Tvif_Handle         : constant := 16#0010#;
  Tvif_Selectedimage  : constant := 16#0020#;
  Tvif_Children       : constant := 16#0040#;
  Tvif_Di_Setitem     : constant := 16#1000#;

  Tvis_Focused        : constant := 16#0001#;
  Tvis_Selected       : constant := 16#0002#;
  Tvis_Cut            : constant := 16#0004#;
  Tvis_Drophilited    : constant := 16#0008#;
  Tvis_Bold           : constant := 16#0010#;
  Tvis_Expanded       : constant := 16#0020#;
  Tvis_Expandedonce   : constant := 16#0040#;
  Tvis_Overlaymask    : constant := 16#0F00#;
  Tvis_Stateimagemask : constant := 16#F000#;
  Tvis_Usermask       : constant := 16#F000#;

  type Htreeitem is new Win32.INT_PTR;

  Tvi_Root  : constant Htreeitem := -16#10000#;
  Tvi_First : constant Htreeitem := -16#FFFF#;
  Tvi_Last  : constant Htreeitem := -16#FFFE#;
  Tvi_Sort  : constant Htreeitem := -16#FFFD#;

  I_Childrencallback : constant := -1;

  type Tv_Item_Ansi is record
    Mask          : Win32.UINT;
    Hitem         : Htreeitem;
    State         : Win32.UINT;
    Statemask     : Win32.UINT;
    Text          : Win32.LPSTR;
    Maxtextsize   : Win32.INT;
    Image         : Win32.INT;
    Selectedimage : Win32.INT;
    Children      : Win32.INT;
    Lparam        : Win32.LPARAM;
  end record with Convention => C;

  type Tv_Item_Wide is record
    Mask          : Win32.UINT;
    Hitem         : Htreeitem;
    State         : Win32.UINT;
    Statemask     : Win32.UINT;
    Text          : Win32.LPWSTR;
    Maxtextsize   : Win32.INT;
    Image         : Win32.INT;
    Selectedimage : Win32.INT;
    Children      : Win32.INT;
    Lparam        : Win32.LPARAM;
  end record with Convention => C;

  type Tv_Insertstruct_Ansi is record
    Parent      : Htreeitem;
    Insertafter : Htreeitem;
    Item        : Tv_Item_Ansi;
  end record with Convention => C;

  type Tv_Insertstruct_Wide is record
    Parent      : Htreeitem;
    Insertafter : Htreeitem;
    Item        : Tv_Item_Wide;
  end record with Convention => C;

  Tv_First                  : constant := 16#1100#;
  Tvm_Insertitem_Ansi       : constant := Tv_First + 0;
  Tvm_Deleteitem            : constant := Tv_First + 1;
  Tvm_Expand                : constant := Tv_First + 2;
  Tvm_Getitemrect           : constant := Tv_First + 4;
  Tvm_Getcount              : constant := Tv_First + 5;
  Tvm_Getindent             : constant := Tv_First + 6;
  Tvm_Setindent             : constant := Tv_First + 7;
  Tvm_Getimagelist          : constant := Tv_First + 8;
  Tvm_Setimagelist          : constant := Tv_First + 9;
  Tvm_Getnextitem           : constant := Tv_First + 10;
  Tvm_Selectitem            : constant := Tv_First + 11;
  Tvm_Getitem_Ansi          : constant := Tv_First + 12;
  Tvm_Setitem_Ansi          : constant := Tv_First + 13;
  Tvm_Editlabel_Ansi        : constant := Tv_First + 14;
  Tvm_Geteditcontrol        : constant := Tv_First + 15;
  Tvm_Getvisiblecount       : constant := Tv_First + 16;
  Tvm_Hittest               : constant := Tv_First + 17;
  Tvm_Createdragimage       : constant := Tv_First + 18;
  Tvm_Sortchildren          : constant := Tv_First + 19;
  Tvm_Ensurevisible         : constant := Tv_First + 20;
  Tvm_Sortchildrencb        : constant := Tv_First + 21;
  Tvm_Endeditlabelnow       : constant := Tv_First + 22;
  Tvm_Getisearchstring_Ansi : constant := Tv_First + 23;

  Tvm_Insertitem_Wide       : constant := Tv_First + 50;
  Tvm_Getitem_Wide          : constant := Tv_First + 62;
  Tvm_Setitem_Wide          : constant := Tv_First + 63;
  Tvm_Getisearchstring_Wide : constant := Tv_First + 24;
  Tvm_Editlabel_Wide        : constant := Tv_First + 65;

  Tve_Collapse      : constant := 16#0001#;
  Tve_Expand        : constant := 16#0002#;
  Tve_Toggle        : constant := 16#0003#;
  Tve_Collapsereset : constant := 16#8000#;

  Tvsil_Normal : constant := 0;
  Tvsil_State  : constant := 2;

  Tvgn_Root             : constant := 16#0000#;
  Tvgn_Next             : constant := 16#0001#;
  Tvgn_Previous         : constant := 16#0002#;
  Tvgn_Parent           : constant := 16#0003#;
  Tvgn_Child            : constant := 16#0004#;
  Tvgn_Firstvisible     : constant := 16#0005#;
  Tvgn_Nextvisible      : constant := 16#0006#;
  Tvgn_Previousvisible  : constant := 16#0007#;
  Tvgn_Drophilite       : constant := 16#0008#;
  Tvgn_Caret            : constant := 16#0009#;

  type Tv_Hittestinfo is record
    Pt    : Win32.Windef.POINT;
    Flags : Win32.UINT;
    Item  : Htreeitem;
  end record with Convention => C;

  Tvht_Nowhere         : constant := 16#0001#;
  Tvht_Onitemicon      : constant := 16#0002#;
  Tvht_Onitemlabel     : constant := 16#0004#;
  Tvht_Onitemindent    : constant := 16#0008#;
  Tvht_Onitembutton    : constant := 16#0010#;
  Tvht_Onitemright     : constant := 16#0020#;
  Tvht_Onitemstateicon : constant := 16#0040#;
  Tvht_Above           : constant := 16#0100#;
  Tvht_Below           : constant := 16#0200#;
  Tvht_Toright         : constant := 16#0400#;
  Tvht_Toleft          : constant := 16#0800#;

  Tvht_Onitem : constant := Tvht_Onitemicon + Tvht_Onitemlabel + Tvht_Onitemstateicon;

  type Comparison_Function is access function (Parameter_1 : Win32.LPARAM;
                                               Parameter_2 : Win32.LPARAM;
                                               Sort        : Win32.LPARAM)
                                               return Win32.INT;
  type Tv_Sortcb is record
    Parent  : Htreeitem;
    Compare : Comparison_Function;
    Param   : Win32.LPARAM;
  end record with Convention => C;

  type Nm_Treeview_Ansi is record
    Hdr        : Nm_Hdr;
    Action     : Win32.UINT;
    Old_Item   : Tv_Item_Ansi;
    New_Item   : Tv_Item_Ansi;
    Drag_Point : Win32.Windef.POINT;
  end record with Convention => C;

  type Nm_Treeview_Ansi_Ptr is access Nm_Treeview_Ansi;

  type Nm_Treeview_Wide is record
    Hdr        : Nm_Hdr;
    Action     : Win32.UINT;
    Old_Item   : Tv_Item_Wide;
    New_Item   : Tv_Item_Wide;
    Drag_Point : Win32.Windef.POINT;
  end record with Convention => C;

  type Nm_Treeview_Wide_Ptr is access Nm_Treeview_Wide;

  type Tv_Dispinfo_Ansi is record
    Hdr  : Nm_Hdr;
    Item : Tv_Item_Ansi;
  end record with Convention => C;

  type Tv_Dispinfo_Ansi_Ptr is access Tv_Dispinfo_Ansi;

  type Tv_Dispinfo_Wide is record
    Hdr  : Nm_Hdr;
    Item : Tv_Item_Wide;
  end record with Convention => C;

  type Tv_Dispinfo_Wide_Ptr is access Tv_Dispinfo_Wide;

  type Tv_Keydown is record
    Hdr   : Nm_Hdr;
    Vkey  : Win32.WORD;
    Flags : Win32.UINT;
  end record with Convention => C;

  Tvc_Unknown    : constant := 16#0000#;
  Tvc_Bymouse    : constant := 16#0001#;
  Tvc_Bykeyboard : constant := 16#0002#;

  Tvn_First          : constant := -400;
  Tvn_Selchanging    : constant := Tvn_First - 1;
  Tvn_Selchanged     : constant := Tvn_First - 2;
  Tvn_Getdispinfo    : constant := Tvn_First - 3;
  Tvn_Setdispinfo    : constant := Tvn_First - 4;
  Tvn_Itemexpanding  : constant := Tvn_First - 5;
  Tvn_Itemexpanded   : constant := Tvn_First - 6;
  Tvn_Begindrag      : constant := Tvn_First - 7;
  Tvn_Beginrdrag     : constant := Tvn_First - 8;
  Tvn_Deleteitem     : constant := Tvn_First - 9;
  Tvn_Beginlabeledit : constant := Tvn_First - 10;
  Tvn_Endlabeledit   : constant := Tvn_First - 11;
  Tvn_Keydown        : constant := Tvn_First - 12;

-- Richedit messages

  Em_Getlimittext       : constant := Winuser.WM_USER + 37;
  Em_Posfromchar        : constant := Winuser.WM_USER + 38;
  Em_Charfrompos        : constant := Winuser.WM_USER + 39;
  Em_Scrollcaret        : constant := Winuser.WM_USER + 49;
  Em_Canpaste           : constant := Winuser.WM_USER + 50;
  Em_Displayband        : constant := Winuser.WM_USER + 51;
  Em_Exgetsel           : constant := Winuser.WM_USER + 52;
  Em_Exlimittext        : constant := Winuser.WM_USER + 53;
  Em_Exlinefromchar     : constant := Winuser.WM_USER + 54;
  Em_Exsetsel           : constant := Winuser.WM_USER + 55;
  Em_Findtext_Ansi      : constant := Winuser.WM_USER + 56;
  Em_Formatrange        : constant := Winuser.WM_USER + 57;
  Em_Getcharformat      : constant := Winuser.WM_USER + 58;
  Em_Geteventmask       : constant := Winuser.WM_USER + 59;
  Em_Getoleinterface    : constant := Winuser.WM_USER + 60;
  Em_Getparaformat      : constant := Winuser.WM_USER + 61;
  Em_Getseltext         : constant := Winuser.WM_USER + 62;
  Em_Hideselection      : constant := Winuser.WM_USER + 63;
  Em_Pastespecial       : constant := Winuser.WM_USER + 64;
  Em_Requestresize      : constant := Winuser.WM_USER + 65;
  Em_Selectiontype      : constant := Winuser.WM_USER + 66;
  Em_Setbkgndcolor      : constant := Winuser.WM_USER + 67;
  Em_Setcharformat      : constant := Winuser.WM_USER + 68;
  Em_Seteventmask       : constant := Winuser.WM_USER + 69;
  Em_Setolecallback     : constant := Winuser.WM_USER + 70;
  Em_Setparaformat      : constant := Winuser.WM_USER + 71;
  Em_Settargetdevice    : constant := Winuser.WM_USER + 72;
  Em_Streamin           : constant := Winuser.WM_USER + 73;
  Em_Streamout          : constant := Winuser.WM_USER + 74;
  Em_Gettextrange       : constant := Winuser.WM_USER + 75;
  Em_Findwordbreak      : constant := Winuser.WM_USER + 76;
  Em_Setoptions         : constant := Winuser.WM_USER + 77;
  Em_Getoptions         : constant := Winuser.WM_USER + 78;
  Em_Findtextex         : constant := Winuser.WM_USER + 79;
  Em_Getwordbreakprocex : constant := Winuser.WM_USER + 80;
  Em_Setwordbreakprocex : constant := Winuser.WM_USER + 81;

  Em_Findtext_Wide      : constant := Winuser.WM_USER + 123;
  Em_Findtextex_Wide    : constant := Winuser.WM_USER + 124;

-- Richedit v2.0 messages

  Em_Setundolimit       : constant := Winuser.WM_USER + 82;
  Em_Redo               : constant := Winuser.WM_USER + 84;
  Em_Canredo            : constant := Winuser.WM_USER + 85;
  Em_Getundoname        : constant := Winuser.WM_USER + 86;
  Em_Getredoname        : constant := Winuser.WM_USER + 87;
  Em_Stopgrouptyping    : constant := Winuser.WM_USER + 88;
  Em_Settextmode        : constant := Winuser.WM_USER + 89;
  Em_Gettextmode        : constant := Winuser.WM_USER + 90;

  Charformat_Size : constant := 60;
  type Charformat is record
    Size           : Win32.UINT  := Charformat_Size;
    Mask           : Win32.DWORD;
    Effects        : Win32.DWORD;
    Height         : Win32.LONG;
    Offset         : Win32.LONG;
    Textcolor      : Win32.Windef.COLORREF;
    Charset        : Win32.BYTE;
    Pitchandfamily : Win32.BYTE;
    Facename       : Win32.CHAR_Array (0..Win32.Wingdi.LF_FACESIZE -1);
    Padding        : Win32.WORD;
  end record with Convention => C;

  -- EM_SETCHARFORMAT wParam masks */

  Scf_Selection   : constant := 16#0001#;
  Scf_Word        : constant := 16#0002#;
  Scf_Default     : constant := 16#0000#;   -- set the default charformat or paraformat
  Scf_All         : constant := 16#0004#;   -- not valid with SCF_SELECTION or SCF_WORD
  Scf_Useuirules  : constant := 16#0008#;   -- modifier for SCF_SELECTION; says that
                                            -- the format came from a toolbar, etc. and
                                            -- therefore UI formatting rules should be
                                            -- used instead of strictly formatting the
                                            -- selection.
  type Charrange is record
    Min : Win32.LONG;
    Max : Win32.LONG;
  end record with Convention => C;

  -- CHARFORMAT masks

  Cfm_Bold      : constant := 16#00000001#;
  Cfm_Italic    : constant := 16#00000002#;
  Cfm_Underline : constant := 16#00000004#;
  Cfm_Strikeout : constant := 16#00000008#;
  Cfm_Protected : constant := 16#00000010#;
  Cfm_Link      : constant := 16#00000020#;  -- Exchange hyperlink extension
  Cfm_Size      : constant := 16#80000000#;
  Cfm_Color     : constant := 16#40000000#;
  Cfm_Face      : constant := 16#20000000#;
  Cfm_Offset    : constant := 16#10000000#;
  Cfm_Charset   : constant := 16#08000000#;

  -- CHARFORMAT effects
  Cfe_Bold      : constant := 16#0001#;
  Cfe_Italic    : constant := 16#0002#;
  Cfe_Underline : constant := 16#0004#;
  Cfe_Strikeout : constant := 16#0008#;
  Cfe_Protected : constant := 16#0010#;
  Cfe_Link      : constant := 16#0020#;
  Cfe_Autocolor : constant := 16#40000000#;  -- NOTE: this corresponds to
                                             -- CFM_COLOR, which controls it

end Win32.Comctl;
