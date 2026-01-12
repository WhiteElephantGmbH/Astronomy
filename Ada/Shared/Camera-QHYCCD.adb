-- *********************************************************************************************************************
-- *                       (c) 2025 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Unchecked_Deallocation;
with Camera.QHYCCD.C_Interface;
with C.Helper;
with Time;
with Traces;
with Os.Application;

package body Camera.QHYCCD is

  package Log is new Traces ("Qhyccd");

  package AS renames Ada.Streams;
  package IO renames AS.Stream_IO;
  package CI renames C_Interface;

  subtype Stream_Offset is AS.Stream_Element_Offset;

  type Temperatur is delta 0.1 range -99.9 .. 99.9;

  task type Control is

    entry Get (Is_Ready : out Boolean);

    entry Capture_Picture (Filename  : String;
                           The_Time  : Exposure.Item;
                           Parameter : Sensitivity.Item); -- Gain and Offset

    entry Capture_Grid (Size      : Square_Size;
                        The_Time  : Exposure.Item;
                        Parameter : Sensitivity.Item); -- Gain and Offset

    entry Await_Stop;

    entry Shutdown;

  end Control;

  The_Control : access Control;


  protected Exposing is

    procedure Initialize (Item : CI.Handle);

    function Handle return CI.Handle;

    procedure Cancel;

    procedure Close;

  private
    The_Handle : CI.Handle := CI.No_Handle;
  end Exposing;


  procedure Start_Control is
  begin
    The_Control := new Control;
  end Start_Control;


  function Is_Available return Boolean is
    Is_Ready : Boolean;
  begin
    The_Control.Get (Is_Ready);
    return Is_Ready;
  end Is_Available;


  procedure Capture_Picture (Filename  : String;
                             The_Time  : Exposure.Item;
                             Parameter : Sensitivity.Item) is
  begin
    The_Control.Capture_Picture (Filename, The_Time, Parameter);
  end Capture_Picture;


  procedure Capture_Grid (Size      : Square_Size;
                          The_Time  : Exposure.Item;
                          Parameter : Sensitivity.Item) is
  begin
    The_Control.Capture_Grid (Size, The_Time, Parameter);
  end Capture_Grid;


  type Buffer is access AS.Stream_Element_Array;

  The_Buffer    : Buffer with Convention => C;
  The_Width     : aliased CI.Uint32;
  The_Height    : aliased CI.Uint32;
  The_Grid_Size : Square_Size;

  The_Bpp      : aliased CI.Uint32;
  The_Channels : aliased CI.Uint32;


  procedure Disconnect is
    procedure Dispose is new Ada.Unchecked_Deallocation (AS.Stream_Element_Array, Buffer);
  begin
    Log.Write ("Disconnect");
    if The_Buffer /= null then
      Dispose (The_Buffer);
      The_Buffer := null;
    end if;
    begin
      Exposing.Close;
    exception
      when others => null;
    end;
    begin
      declare
        Dummy : CI.Result;
      begin
        Dummy := CI.Release_Resource;
      end;
    exception
      when others => null;
    end;
    Camera_Data.Set (Idle);
  end Disconnect;


  function Grid return Raw_Grid is

    function Pixel_Of (Row    : Rows;
                       Column : Columns) return Pixel is
      use type Stream_Offset;
      use type CI.Uint32;
      -- assumes 16-bit little-endian, 1 channel
      BI : constant Stream_Offset := Stream_Offset((Natural(Row - 1) * Natural(The_Width) + Natural(Column) - 1) * 2);
      Lo : constant AS.Stream_Element := The_Buffer(The_Buffer'first + BI);
      Hi : constant AS.Stream_Element := The_Buffer(The_Buffer'first + BI + 1);
    begin
      return Pixel(Lo) + 256 * Pixel(Hi);
    end Pixel_Of;

    use type CI.Uint32;

    Side         : constant CI.Uint32 := CI.Uint32(The_Grid_Size);
    First_Row    : constant Rows := Rows(The_Height - Side) / 2;
    First_Column : constant Columns := Columns(The_Width - Side) / 2;

    The_Grid : Raw_Grid(1 .. Rows(The_Grid_Size), 1 .. Columns(The_Grid_Size));

  begin -- Grid
    if Camera_Data.Actual.State /= Cropped then
      Raise_Error ("Grid not prepared");
    end if;
    for Row_Grid in 1 .. Rows(Side) loop
      for Column_Grid in 1 .. Columns(Side) loop
        declare
          Row    : constant Rows    := First_Row + Row_Grid - 1;
          Column : constant Columns := First_Column + Column_Grid - 1;
        begin
          The_Grid (Row_Grid, Column_Grid) := Pixel_Of (Row, Column);
        end;
      end loop;
    end loop;
    Disconnect;
    return The_Grid;
  exception
  when others =>
    Disconnect;
    return [];
  end Grid;


  procedure Stop_Capture is
  begin
    Exposing.Cancel;
    The_Control.Await_Stop;
  end Stop_Capture;


  procedure End_Control is
  begin
    The_Control.Shutdown;
  end End_Control;


  protected body Exposing is

    procedure Initialize (Item : CI.Handle) is
    begin
      The_Handle := Item;
    end Initialize;

    function Handle return CI.Handle is (The_Handle);


    procedure Cancel is
      Dummy : CI.Result;
      use type CI.Handle;
    begin
      Camera_Data.Set (Stopping);
      if The_Handle /= CI.No_Handle then
        Dummy := CI.Cancel_Exposing_And_Readout (The_Handle);
      end if;
    end Cancel;


    procedure Close is
      Dummy : CI.Result;
      use type CI.Handle;
    begin
      if The_Handle /= CI.No_Handle then
        Dummy := CI.Close (The_Handle);
        The_Handle := CI.No_Handle;
      end if;
    exception
    when others =>
      The_Handle := CI.No_Handle;
    end Close;
  end Exposing;


  -------------
  -- Control --
  -------------

  task body Control is

    Default_Filename : constant String := "Raw_Picture.FITS";
    Readout_Time     : constant CI.Uint32 := 15_000; -- milliseconds

    procedure Check (Where  : String;
                     Result : CI.Result) is
      use type CI.Result;
    begin
      Log.Write (Where);
      if Result /= CI.QHY_SUCCESS then
        if Camera_Data.Actual.State /= Stopping then
          Raise_Error (Where & " failed, error =" & CI.Result'image (Result));
        end if;
      end if;
    end Check;

    The_Camera : Model;

    The_Filename   : Text.String := [Default_Filename];
    The_Exposure   : Exposure.Item;
    The_Parameter  : Sensitivity.Item;
    Mid_Exposure   : Time.JD;
    CCD_Temperatur : Temperatur;

    The_Length : CI.Uint32;
    The_Gain   : Sensitivity.Gain;
    The_Offset : Sensitivity.Offset;

    use type CI.Handle;


    function One_Device_Is_Ready return Boolean is
      Count : CI.Camera_Count;
      use type CI.Camera_Count;
    begin
      Camera_Data.Check (Idle);
      Check ("Init Resource", CI.Init_Resource);
      Count := CI.Scan;
      Log.Write ("Number detected cameras:" & Count'image);
      Check ("Release Resource", CI.Release_Resource);
      return Count = 1;
    exception
    when others =>
      Disconnect;
      return False;
    end One_Device_Is_Ready;


    procedure Start_Capture is
      Id_Buffer : aliased C.Name(1 .. 64) := [others => Ascii.Nul];
      use type CI.Camera_Count;
      use type Exposure.Kind;
    begin
      Camera_Data.Check (Idle);
      if not The_Parameter.Is_Gain_And_Offset_Or_Default then
        Raise_Error ("Parameter must be Gain and Offset or default value");
      elsif The_Exposure.Mode = Exposure.From_Camera then
        Raise_Error ("Exposure from Camera not supported");
      end if;
      The_Gain := The_Parameter.Value;
      Log.Write ("Gain:" & The_Gain'image);
      The_Offset := The_Parameter.Value;
      Log.Write ("Offset:" & The_Offset'image);
      Camera_Data.Set (Connecting);
      Check ("Init Resource", CI.Init_Resource);
      if CI.Scan /= 1 then
        Raise_Error ("One and only one camera must be connected");
      end if;
      Check ("Get Id", CI.Get_Id (0, Id_Buffer'address));
      Exposing.Initialize (CI.Open (Id_Buffer'address));
      if Exposing.Handle = CI.No_Handle then
        Raise_Error ("Open failed");
      end if;
      declare
        Id_Parts    : constant Text.Strings := Text.Strings_Of (C.Helper.String_Of (Id_Buffer), Separator => '-');
        Camera_Name : constant String := Id_Parts (1);
      begin
        Log.Write ("Camera Name: " & Camera_Name);
        The_Camera := Model'value(Camera_Name);
      exception
      when others =>
        Raise_Error ("Unknown Camera " & Camera_Name);
      end;
      Camera_Data.Set (The_Camera);
      Camera_Data.Set (Connected);
    end Start_Capture;


    procedure Continue_Capture is

      Chip_W : aliased CI.Double;
      Chip_H : aliased CI.Double;
      Img_W  : aliased CI.Uint32;
      Img_H  : aliased CI.Uint32;
      Pix_W  : aliased CI.Double;
      Pix_H  : aliased CI.Double;
      Bpp0   : aliased CI.Uint32;

      use type CI.Uint32;
      use all type CI.Bool;

    begin -- Continue_Capture
      Camera_Data.Set (Capturing);
      Check ("Set Stream Mode", CI.Set_Stream_Mode (Exposing.Handle, CI.Stream_Single_Frame));
      Check ("Initialize", CI.Init_Camera (Exposing.Handle));
      Check ("Set Read Mode", CI.Set_Read_Mode (Exposing.Handle, CI.Photo_Graphic_DSO_16_BIT));
      Check ("Set TRANSFERBIT=16", CI.Set_Param (Exposing.Handle, CI.Control_Transfer_Bit, 16.0));
      Check ("Set USBTRAFFIC=0", CI.Set_Param (Exposing.Handle, CI.Control_USB_Traffic, 0.0));
      Check ("Set Debayer Off", CI.Set_Debayer_On_Off (Exposing.Handle, False));

      Check ("Get Chip Info",
             CI.Get_Chip_Info (Exposing.Handle,
                               Chip_W'access, Chip_H'access,
                               Img_W'access, Img_H'access,
                               Pix_W'access, Pix_H'access,
                               Bpp0'access));
      Log.Write ("ChipInfo - Img_W:" & Img_W'image & " - Img_H:" & Img_H'image & " - Bpp0:" & Bpp0'image);

      CCD_Temperatur := Temperatur (CI.Get_Param (Exposing.Handle, CI.Control_CURRTEMP));
      Check ("Set Bin Mode", CI.Set_Bin_Mode (Exposing.Handle, 1, 1));
      Check ("Set Resolution", CI.Set_Resolution (Exposing.Handle, 0, 0, Img_W, Img_H));
      declare
        Usec : constant CI.Uint32 := CI.Uint32 (The_Exposure.Time * 1_000_000.0);
        Msec : constant CI.Uint32 := Usec / 1000;
        use type Time.Ut;
        Ut : constant Time.Ut := Time.Universal + Duration(The_Exposure.Time / 2.0);
      begin
        Check ("Set Single Frame Timeout", CI.Set_Single_Frame_Timeout (Exposing.Handle, Msec + Readout_Time));
        Check ("Set Param Exposure", CI.Set_Param (Exposing.Handle, CI.Control_Exposure, CI.Double(Usec)));
        Mid_Exposure := Time.Julian_Date_Of (Ut);
      end;
      Check ("Set Param Gain", CI.Set_Param (Exposing.Handle, CI.Control_Gain, CI.Double(The_Gain)));
      Check ("Set Param Offset", CI.Set_Param (Exposing.Handle, CI.Control_Offset, CI.Double(The_Offset)));

      The_Length := CI.Get_Mem_Length (Exposing.Handle);
      The_Buffer := new AS.Stream_Element_Array (1 .. Stream_Offset(The_Length));
      Check ("Exp Single Frame", CI.Exp_Single_Frame (Exposing.Handle));
      Check ("Get Single Frame",
             CI.Get_Single_Frame (Exposing.Handle,
             The_Width'access, The_Height'access,
             The_Bpp'access, The_Channels'access,
             The_Buffer (The_Buffer'first)'address));

      Log.Write ("Frame Width:" & The_Width'image &
                 " - Height:" & The_Height'image &
                 " - Bpp:" & The_Bpp'image &
                 " - Channels:" & The_Channels'image &
                 " - Length:" & The_Length'image);

      if The_Channels /= 1 then
        Raise_Error ("Expected Channels = 1, got" & The_Channels'image);
      end if;
      declare
        Bytes_Per_Sample : constant CI.Uint32 := The_Bpp / Standard'storage_unit;
        Expected         : constant CI.Uint32 := The_Width * The_Height * The_Channels * Bytes_Per_Sample;
      begin
        Log.Write ("Frame Expected bytes:" & Expected'image);
        if The_Length < Expected then
          Raise_Error ("Buffer too small: MemLength=" & The_Length'image & " < Expected=" & Expected'image);
        end if;
      end;
      Camera_Data.Set (Height => Rows(The_Height));
      Camera_Data.Set (Width  => Columns(The_Width));
      Camera_Data.Set (Captured);
    end Continue_Capture;


    procedure Write_Fits is

      Block_Size : constant := 2880;

      use type CI.Uint32;

      Bitpix : constant CI.Uint32 := The_Bpp;
      BPS    : constant CI.Uint32 := Bitpix / Standard'storage_unit; -- bytes per sample
      Height : constant CI.Uint32 := The_Height;
      Width  : constant CI.Uint32 := The_Width;

      subtype Card_String    is String(1..80);
      subtype Keyword_String is String(1..8);
      subtype Value_String   is String(1..21);

      type Pixel_Size is delta 0.01 range 0.0 .. 99.99;

      Pixel_Height : constant Pixel_Size := 3.76; -- um;
      Pixel_Width  : constant Pixel_Size := 3.76; -- um;


      function Keyword_Of (Key : String) return Keyword_String is
        Image : Keyword_String := [others => ' '];
      begin
        Image(Image'first .. Image'first + Key'length - 1) := Key;
        return Image;
      end Keyword_Of;


      function Value_Of (Item : String) return Value_String is
        Image : Value_String := [others => ' '];
      begin
        Image(Image'last - Item'length + 1 .. Image'last) := Item;
        return Image;
      end Value_Of;


      function Card (Key   : String;
                     Value : String := "") return String is
        Image : Card_String := [others => ' '];
        Item  : constant String := Keyword_Of (Key) & (if Value = "" then "" else "= '" & Value & "'");
      begin
        Image(Image'first .. Image'first + Item'length - 1) := Item;
        return Image;
      end Card;


      function Card_Value (Key   : String;
                           Value : String) return Card_String is
        Image : Card_String := [others => ' '];
        Item  : constant String := Keyword_Of (Key) & "= " & Value_Of (Value);
      begin
        Image(Image'first .. Image'first + Item'length - 1) := Item;
        return Image;
      end Card_Value;


      function Card (Key   : String;
                     Value : CI.Uint32) return Card_String is
      begin
        return Card_Value (Key, Value'image);
      end Card;


      function Card (Key   : String;
                     Value : Pixel_Size) return Card_String is
      begin
        return Card_Value (Key, Value'image);
      end Card;


      function Card (Key   : String;
                     Value : Exposure.Duration) return Card_String is
      begin
        return Card_Value (Key, Value'image);
      end Card;


      function Card (Key   : String;
                     Value : Temperatur) return Card_String is
      begin
        return Card_Value (Key, Value'image);
      end Card;


      function Card (Key   : String;
                     Value : Time.JD) return Card_String is
      begin
        return Card_Value (Key, Value'image);
      end Card;


      function Card (Key   : String;
                     Value : Boolean) return Card_String is
      begin
        return Card_Value (Key, (if Value then "T" else "F"));
      end Card;


      The_File  : IO.File_Type;
      The_Count : Natural := 0;

      procedure Put_Block (S0 : String) is
        Buf : AS.Stream_Element_Array(1 .. Stream_Offset(S0'length));
      begin
        for I in S0'range loop
          Buf(Stream_Offset(I - S0'first + 1)) := AS.Stream_Element(Character'pos(S0(I)));
        end loop;
        IO.Write (The_File, Buf);
        The_Count := @ + Buf'length;
      end Put_Block;


      procedure Pad_To_Block_End is
        Reminder : constant Natural := The_Count mod Block_Size;
      begin
        if Reminder /= 0 then
          declare
            Pad_Count : constant Natural := Block_Size - Reminder;
            Zero_Pad  : constant AS.Stream_Element_Array(1 .. Stream_Offset(Pad_Count)) := [others => 0];
          begin
            IO.Write (The_File, Zero_Pad);
            The_Count := @ + Pad_Count;
          end;
        end if;
      end Pad_To_Block_End;


      procedure Write_Data is

        use type Stream_Offset;

        Chunk : AS.Stream_Element_Array(1 .. 8192);
        Pos   : Stream_Offset := Chunk'first;

        procedure Flush is
        begin
          if Pos > Chunk'first then
            IO.Write (The_File, Chunk(Chunk'first .. Pos - 1));
            The_Count := @ + Natural(Pos - Chunk'first);
            Pos := Chunk'first;
          end if;
        end Flush;

        procedure Emit (B : AS.Stream_Element) is
        begin
          Chunk (Pos) := B;
          Pos := Pos + 1;
          if Pos > Chunk'last then
            Flush;
          end if;
        end Emit;

        function Byte_At (I : Stream_Offset) return AS.Stream_Element is
        begin
          return The_Buffer(The_Buffer'first + I);
        end Byte_At;

        -- emit one sample as big-endian
        procedure Emit_Sample (Sample_Byte_Offset : Stream_Offset) is
        begin
          Emit (Byte_At (Sample_Byte_Offset + 1));
          Emit (Byte_At (Sample_Byte_Offset));
        end Emit_Sample;

        Pixels : constant Stream_Offset := Stream_Offset(Width) * Stream_Offset(Height);

      begin -- Write_Data
        for P in 0 .. Pixels - 1 loop
          declare
            Off : constant Stream_Offset := P * Stream_Offset(BPS);
          begin
            Emit_Sample(Off);
          end;
        end loop;
        Flush;
      end Write_Data;

    begin -- Write_Fits
      Camera_Data.Set (Downloading);
      if Bitpix /= 16 then
        Raise_Error ("Unsupported bits per pixel for FITS:" & CI.Uint32'image(Bitpix));
      end if;
      IO.Create (The_File, IO.Out_File, The_Filename.S);
      Put_Block (Card ("SIMPLE", True));
      Put_Block (Card ("BITPIX", Bitpix));
      Put_Block (Card ("NAXIS", 2));
      Put_Block (Card ("NAXIS1", Width));
      Put_Block (Card ("NAXIS2", Height));
      Put_Block (Card ("BLKLEVEL", CI.Uint32(The_Offset)));
      Put_Block (Card ("GAIN", CI.Uint32(The_Gain)));
      Put_Block (Card ("BAYERPAT", "RGGB"));
      Put_Block (Card ("YBINNING", 1));
      Put_Block (Card ("XBINNING", 1));
      Put_Block (Card ("YPIXSZ", Pixel_Height));
      Put_Block (Card ("XPIXSZ", Pixel_Width));
      Put_Block (Card ("ROWORDER", "TOP-DOWN"));
      Put_Block (Card ("BSCALE", 1));
      Put_Block (Card ("BZERO", 32768));
      Put_Block (Card ("EXTEND", True));
      Put_Block (Card ("CCD-TEMP", CCD_Temperatur));
      Put_Block (Card ("JD_UTC", Mid_Exposure));
      Put_Block (Card ("EXPTIME", The_Exposure.Time));
      Put_Block (Card ("INSTRUME", Camera_Data.Actual.Camera'image));
      Put_Block (Card ("SWCREATE", "White Elephant GmbH - " & Os.Application.Name & " v" & Os.Application.Version));
      Put_Block (Card ("END"));
      Pad_To_Block_End;
      Write_Data;
      Pad_To_Block_End;
      IO.Close (The_File);
    end Write_Fits;


    procedure Capture_Picture is
    begin
      Start_Capture;
      Continue_Capture;
      if Camera_Data.Actual.State /= Stopping then
        Write_Fits;
      end if;
      Disconnect;
    exception
    when Camera_Error =>
      Disconnect;
    when Occurrence: others =>
      Disconnect;
      Camera_Data.Set_Fatal (Occurrence);
    end Capture_Picture;


    procedure Capture_Grid is
    begin
      Start_Capture;
      Continue_Capture;
      if Camera_Data.Actual.State /= Stopping then
        Camera_Data.Set (Cropped);
      end if;
    exception
    when Camera_Error =>
      Disconnect;
    when Occurrence: others =>
      Disconnect;
      Camera_Data.Set_Fatal (Occurrence);
    end Capture_Grid;

  begin -- Control
    Camera_Data.Set (Idle);
    loop
      select
        accept Get (Is_Ready : out Boolean) do
          Is_Ready := One_Device_Is_Ready;
        end Get;
      or
        accept Capture_Picture (Filename  : String;
                                The_Time  : Exposure.Item;
                                Parameter : Sensitivity.Item)
        do
          The_Filename := [Filename];
          The_Exposure := The_Time;
          The_Parameter := Parameter;
        end Capture_Picture;
        Capture_Picture;
      or
        accept Capture_Grid (Size      : Square_Size;
                             The_Time  : Exposure.Item;
                             Parameter : Sensitivity.Item)
        do
          The_Grid_Size := Size;
          The_Exposure  := The_Time;
          The_Parameter := Parameter;
        end Capture_Grid;
        Capture_Grid;
      or
        accept Await_Stop do
          Log.Write ("Stopping");
          if not (Camera_Data.Actual.State in Idle | Failed) then
            Disconnect;
          end if;
        end Await_Stop;
      or
        accept Shutdown do
          if Camera_Data.Actual.Camera = QHY600C then
            Disconnect;
          end if;
        end Shutdown;
        exit;
      end select;
    end loop;

  exception
  when Occurrance : others =>
    Log.Termination (Occurrance);
    Disconnect;
  end Control;

end Camera.QHYCCD;
