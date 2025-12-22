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

with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Unchecked_Deallocation;
with Camera.QHYCCD.C_Interface;
with C.Helper;
with Traces;

package body Camera.QHYCCD is

  package Log is new Traces ("Qhyccd");

  package AS renames Ada.Streams;
  package IO renames AS.Stream_IO;
  package CI renames C_Interface;


  task type Control is

    entry Get (Is_Ready : out Boolean);

    entry Capture_Picture (Filename  : String;
                           Time      : Exposure.Item;
                           Parameter : Sensitivity.Item); -- Gain and Offset

    entry Capture_Grid (Size      : Square_Size;
                        Time      : Exposure.Item;
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
                             Time      : Exposure.Item;
                             Parameter : Sensitivity.Item) is
  begin
    The_Control.Capture_Picture (Filename, Time, Parameter);
  end Capture_Picture;


  procedure Capture_Grid (Size      : Square_Size;
                          Time      : Exposure.Item;
                          Parameter : Sensitivity.Item) is
  begin
    The_Control.Capture_Grid (Size, Time, Parameter);
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


  function Grid return Green_Grid is

    function U16_At (Row, Col : CI.Uint32) return Pixel is
      -- assumes 16-bit little-endian, 1 channel
      use type AS.Stream_Element_Offset;
      use type CI.Uint32;
      Index_Bytes : constant AS.Stream_Element_Offset := AS.Stream_Element_Offset((Row * The_Width + Col) * 2);
      Lo : constant AS.Stream_Element := The_Buffer(The_Buffer'first + Index_Bytes);
      Hi : constant AS.Stream_Element := The_Buffer(The_Buffer'first + Index_Bytes + 1);
      V  : constant Natural := Natural(Lo) + 256 * Natural(Hi);
    begin
      return Pixel (V);
    end U16_At;

    use type CI.Uint32;

    W : constant CI.Uint32 := The_Width;
    H : constant CI.Uint32 := The_Height;

    Side : constant CI.Uint32 := (if W < H then W else H);
    X0   : constant CI.Uint32 := (W - Side) / 2;
    Y0   : constant CI.Uint32 := (H - Side) / 2;

    Cell : constant CI.Uint32 := Side / CI.Uint32 (The_Grid_Size);

    The_Grid : Green_Grid (1 .. Rows(The_Grid_Size), 1 .. Columns(The_Grid_Size));

  begin -- Grid
    if Camera_Data.Actual.State /= Cropped then
      Raise_Error ("Grid not prepared");
    end if;

    Log.Write ("### Grid W:" & W'image &
               " - H:" & H'image &
               " - Side:" & Side'image &
               " - Cell:" & Cell'image &
               " - Size:" & The_Grid_Size'image &
               " - X0:" & X0'image &
               " - Y0:" & Y0'image);

    -- For QHY600C Bayer (assume RGGB): green at (even,odd) and (odd,even)
    for R in 1 .. The_Grid_Size loop
      for C in 1 .. The_Grid_Size loop
        declare
          Sum   : Natural := 0;
          Count : Natural := 0;

          Y1 : constant CI.Uint32 := Y0 + CI.Uint32(R - 1) * Cell;
          Y2 : constant CI.Uint32 := (if R = The_Grid_Size then Y0 + Side else Y1 + Cell);

          X1 : constant CI.Uint32 := X0 + CI.Uint32(C - 1) * Cell;
          X2 : constant CI.Uint32 := (if C = The_Grid_Size then X0 + Side else X1 + Cell);
        begin
          for Y in Y1 .. Y2 - 1 loop
            for X in X1 .. X2 - 1 loop
              declare
                Even_Y   : constant Boolean := (Y mod 2 = 0);
                Even_X   : constant Boolean := (X mod 2 = 0);
                Is_Green : constant Boolean := (Even_Y and not Even_X) or (not Even_Y and Even_X);
              begin
                if Is_Green then
                  Sum := Sum + Natural(U16_At (Y, X));
                  Count := Count + 1;
                end if;
              end;
            end loop;
          end loop;

          if Count = 0 then
            The_Grid (Rows(R), Columns(C)) := 0;
          else
            The_Grid (Rows(R), Columns(C)) := Pixel(Sum / Count);
          end if;
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

    The_Filename  : Text.String := [Default_Filename];
    The_Exposure  : Exposure.Item;
    The_Parameter : Sensitivity.Item;

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
      end if;
      if The_Exposure.Mode = Exposure.From_Camera then
        Raise_Error ("Exposure from Camera not supported");
      end if;

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

    The_Length : CI.Uint32;

    type Capture_Kind is (Grid, Picture);


    procedure Continue_Capture (Kind : Capture_Kind) is

      Gain   : constant Sensitivity.Gain   := The_Parameter.Value;
      Offset : constant Sensitivity.Offset := The_Parameter.Value;

      Chip_W : aliased CI.Double;
      Chip_H : aliased CI.Double;
      Img_W  : aliased CI.Uint32;
      Img_H  : aliased CI.Uint32;
      Pix_W  : aliased CI.Double;
      Pix_H  : aliased CI.Double;
      Bpp0   : aliased CI.Uint32;

      use type Sensitivity.Gain;
      use type Sensitivity.Offset;
      use type CI.Uint32;

      function Has (Id : CI.Control_Id) return Boolean is
      begin
        return CI.Is_Control_Available (Exposing.Handle, Id) = CI.QHY_SUCCESS;
      end Has;

      use all type CI.Bool;

      procedure Log_Range (Name : String;
                           Id   : CI.Control_Id) is
        Min  : aliased CI.Double;
        Max  : aliased CI.Double;
        Step : aliased CI.Double;
        R    : CI.Result;
      begin
        if Has (Id) then
          R := CI.Get_Param_Min_Max_Step (Exposing.Handle, Id, Min'access, Max'access, Step'access);
          if R = CI.QHY_SUCCESS then
            Log.Write ("### Range " & Name &
                       " Min:" & Min'image &
                       " Max:" & Max'image &
                       " Step:" & Step'image);
          else
            Log.Warning ("Get range failed for " & Name & " error =" & R'image);
          end if;
        else
          Log.Write ("### Range " & Name & " not available");
        end if;
      exception
        when others =>
          Log.Warning ("Get range failed for " & Name);
      end Log_Range;

      procedure Log_Caps is
        function Yes_No (B : Boolean) return String is (if B then "Yes" else "No");
      begin
        Log.Write ("### Caps" &
                   " - SingleFrame:" & Yes_No (Has (CI.Cam_Single_Frame_Mode)) &
                   " - LiveVideo:" & Yes_No (Has (CI.Cam_Live_Video_Mode)) &
                   " - IsColor:" & Yes_No (Has (CI.Cam_Is_Color)) &
                   " - Debayer:Yes" &
                   " - TransferBit:" & Yes_No (Has (CI.Control_Transfer_Bit)) &
                   " - Speed:" & Yes_No (Has (CI.Control_Speed)) &
                   " - USB:" & Yes_No (Has (CI.Control_USB_Traffic)) &
                   " - DDR:" & Yes_No (Has (CI.Control_DDR)) &
                   " - Channels:" & Yes_No (Has (CI.Control_Channels)) &
                   " - Gain:" & Yes_No (Has (CI.Control_Gain)) &
                   " - Offset:" & Yes_No (Has (CI.Control_Offset)) &
                   " - Exposure:" & Yes_No (Has (CI.Control_Exposure)));
      end Log_Caps;

      procedure Log_Versions is
        Year   : aliased CI.Uint32 := 0;
        Month  : aliased CI.Uint32 := 0;
        Day    : aliased CI.Uint32 := 0;
        Subday : aliased CI.Uint32 := 0;

        FW : aliased C.Name (1 .. 64) := [others => Ascii.Nul];

        R : CI.Result;
      begin
        R := CI.Get_SDK_Version (Year'access, Month'access, Day'access, Subday'access);
        if R = CI.QHY_SUCCESS then
          Log.Write ("### SDK Version " &
                     Year'image & "-" &
                     Month'image & "-" &
                     Day'image & " (" & Subday'image & ")");
        else
          Log.Warning ("Get SDK version failed error =" & R'image);
        end if;

        R := CI.Get_FW_Version (Exposing.Handle, FW'address);
        if R = CI.QHY_SUCCESS then
          Log.Write ("### FW Version " & C.Helper.String_Of (FW));
        else
          Log.Warning ("Get FW version failed error =" & R'image);
        end if;
      exception
        when others =>
          Log.Warning ("Get versions failed");
      end Log_Versions;

      procedure Setup_Read_Mode is
        Mode_Count : aliased CI.Uint32 := 0;
        R          : CI.Result;

        Name_Buf : aliased C.Name (1 .. 64) := [others => Ascii.Nul];
        W0       : aliased CI.Uint32 := 0;
        H0       : aliased CI.Uint32 := 0;

        Mode : constant CI.Uint32 := 0;

      begin
        R := CI.Get_Number_Of_Read_Modes (Exposing.Handle, Mode_Count'access);
        if R = CI.QHY_SUCCESS then
          Log.Write ("### Read modes:" & Mode_Count'image);
        else
          Log.Warning ("Get read mode count failed error =" & R'image);
          return;
        end if;

        if Mode_Count = 0 then
          return;
        end if;

        Check ("Set Read Mode", CI.Set_Read_Mode (Exposing.Handle, Mode));

        R := CI.Get_Read_Mode_Name (Exposing.Handle, Mode, Name_Buf'address);
        if R = CI.QHY_SUCCESS then
          Log.Write ("### Read mode 0 name: " & C.Helper.String_Of (Name_Buf));
        end if;

        R := CI.Get_Read_Mode_Resolution (Exposing.Handle, Mode, W0'access, H0'access);
        if R = CI.QHY_SUCCESS then
          Log.Write ("### Read mode 0 resolution W:" & W0'image & " - H:" & H0'image);
        end if;
      exception
        when others =>
          Log.Warning ("Read mode setup failed");
      end Setup_Read_Mode;

    begin -- Continue_Capture
      Camera_Data.Set (Capturing);

      Check ("Set Stream Mode", CI.Set_Stream_Mode (Exposing.Handle, CI.Stream_Single_Frame));
      Check ("Initialize", CI.Init_Camera (Exposing.Handle));

      Log_Versions;
      Log_Caps;
      Setup_Read_Mode;

      Log_Range ("TRANSFERBIT", CI.Control_Transfer_Bit);
      Log_Range ("SPEED",       CI.Control_Speed);
      Log_Range ("USBTRAFFIC",  CI.Control_USB_Traffic);
      Log_Range ("DDR",         CI.Control_DDR);
      Log_Range ("CHANNELS",    CI.Control_Channels);
      Log_Range ("EXPOSURE",    CI.Control_Exposure);
      Log_Range ("GAIN",        CI.Control_Gain);
      Log_Range ("OFFSET",      CI.Control_Offset);

      -- Prefer 16-bit transfer if supported (common on QHY)
      if Has (CI.Control_Transfer_Bit) then
        Check ("Set TRANSFERBIT=16", CI.Set_Param (Exposing.Handle, CI.Control_Transfer_Bit, 16.0));
      end if;

      -- Try conservative defaults if available (helpful for first light / hangs)
      if Has (CI.Control_Speed) then
        Check ("Set SPEED=0", CI.Set_Param (Exposing.Handle, CI.Control_Speed, 0.0));
      end if;

      if Has (CI.Control_USB_Traffic) then
        Check ("Set USBTRAFFIC=0", CI.Set_Param (Exposing.Handle, CI.Control_USB_Traffic, 0.0));
      end if;

      if Has (CI.Control_DDR) then
        Check ("Set DDR=1", CI.Set_Param (Exposing.Handle, CI.Control_DDR, 1.0));
      end if;

      case Kind is
      when Grid =>
        -- Grid wants raw / mono (1 channel), and NO debayer.
        if Has (CI.Control_Channels) then
          Check ("Set CHANNELS=1", CI.Set_Param (Exposing.Handle, CI.Control_Channels, 1.0));
        end if;
        Check ("Set Debayer Off", CI.Set_Debayer_On_Off (Exposing.Handle, False));

      when Picture =>
        -- Picture for ASTAP: debayer not needed; keep raw mono to avoid SDK buffer surprises.
        Check ("Set Debayer Off", CI.Set_Debayer_On_Off (Exposing.Handle, False));
      end case;

      -- full frame defaults (bin 1, ROI full)
      Check ("Get Chip Info",
        CI.Get_Chip_Info (Exposing.Handle,
                          Chip_W'access, Chip_H'access,
                          Img_W'access,  Img_H'access,
                          Pix_W'access,  Pix_H'access,
                          Bpp0'access));

      Log.Write ("### Chip Img_W:" & Img_W'image &
                 " - Img_H:" & Img_H'image &
                 " - Pix_W:" & Pix_W'image &
                 " - Pix_H:" & Pix_H'image &
                 " - Bpp0:" & Bpp0'image);

      Check ("Set Bin Mode", CI.Set_Bin_Mode (Exposing.Handle, 1, 1));
      Check ("Set Resolution", CI.Set_Resolution (Exposing.Handle, 0, 0, Img_W, Img_H));

      -- exposure in microseconds
      declare
        Usec : constant CI.Uint32 := CI.Uint32 (The_Exposure.Time * 1_000_000.0);
        Msec : constant CI.Uint32 := Usec / 1000;
      begin
        Log.Write ("### Usec:" & Usec'image & " - Msec:" & Msec'image);

        Check ("Set Single Frame Timeout", CI.Set_Single_Frame_Timeout (Exposing.Handle, Msec + Readout_Time));
        Check ("Set Param Exposure", CI.Set_Param (Exposing.Handle, CI.Control_Exposure, CI.Double(Usec)));
      end;

      if Gain /= 0 then
        Check ("Set Param Gain", CI.Set_Param (Exposing.Handle, CI.Control_Gain, CI.Double(Gain)));
      end if;

      if Offset /= 0 then
        Check ("Set Param Offset", CI.Set_Param (Exposing.Handle, CI.Control_Offset, CI.Double(Offset)));
      end if;

      The_Length := CI.Get_Mem_Length (Exposing.Handle);
      if The_Length = 0 then
        Raise_Error ("Get Mem Length returned 0");
      end if;

      The_Buffer := new AS.Stream_Element_Array (1 .. AS.Stream_Element_Offset(The_Length));

      Check ("Exp Single Frame", CI.Exp_Single_Frame (Exposing.Handle));

      begin
        Log.Write ("### Get frame start");
        Check ("Get Single Frame",
          CI.Get_Single_Frame (Exposing.Handle,
                               The_Width'access, The_Height'access,
                               The_Bpp'access, The_Channels'access,
                               The_Buffer (The_Buffer'first)'address));
      exception
      when Item: others =>
        Log.Termination (Item);
        raise;
      end;
      Log.Write ("### Frame W:" & The_Width'image &
                 " - H:" & The_Height'image &
                 " - Bpp:" & The_Bpp'image &
                 " - Ch:" & The_Channels'image &
                 " - MemLen:" & The_Length'image);

      if The_Channels /= 1 then
        Log.Warning ("Grid expected Channels = 1, got" & The_Channels'image);
      end if;

      Camera_Data.Set (Height => Rows(The_Height));
      Camera_Data.Set (Width  => Columns(The_Width));
      Camera_Data.Set (Captured);
      Log.Write ("### Continue_Capture done");
    end Continue_Capture;

    -- FITS helpers -------------------------------------------------------------

    function Card (K, V : String) return String is

      S0 : String(1 .. 80) := [others => ' '];

      procedure Put_Keyword (Key : String) is
        P : Natural := 1;
      begin
        for I in Key'range loop
          exit when P > 8;
          S0(P) := Key(I);
          P := P + 1;
        end loop;
      end Put_Keyword;

      procedure Put_Value_Field (Val : String) is
        -- FITS value field is columns 11..30 (20 chars), right-justified
        Field : String(1 .. 20) := [others => ' '];
        L     : constant Natural := Val'length;
      begin
        if L >= Field'length then
          Field := Val(Val'last - Field'length + 1 .. Val'last);
        else
          Field(Field'last - L + 1 .. Field'last) := Val;
        end if;

        S0(11 .. 30) := Field;
      end Put_Value_Field;

    begin
      Put_Keyword (K);

      if K = "END" then
        -- no "= " and no value for END
        return S0;
      end if;

      S0(9)  := '=';
      S0(10) := ' ';

      -- If caller provides a "raw" value already formatted (e.g. "'TEXT'"),
      -- it should still go into the value field correctly.
      Put_Value_Field (V);
      Log.Write ("### S0: '" & S0 & "'");

      return S0;
    end Card;


    function Card (K : String;
                   V : CI.Uint32) return String is
    begin
      return Card (K, Text.Trimmed (V'image));
    end Card;


    function Card_Logical (K : String;
                           V : Boolean) return String is
    begin
      if V then
        return Card (K, "T");
      else
        return Card (K, "F");
      end if;
    end Card_Logical;


    function End_Card return String is
      S0 : String(1 .. 80) := [others => ' '];
    begin
      S0(1) := 'E';
      S0(2) := 'N';
      S0(3) := 'D';
      return S0;
    end End_Card;


    procedure Write_Fits is

      The_File : IO.File_Type;

      procedure Put_Block (S0 : String) is
        Buf : AS.Stream_Element_Array(1 .. AS.Stream_Element_Offset(S0'length));
      begin
        for I in S0'range loop
          Buf(AS.Stream_Element_Offset(I - S0'first + 1)) := AS.Stream_Element(Character'pos(S0(I)));
        end loop;
        IO.Write (The_File, Buf);
      end Put_Block;

      procedure Pad_To_2880 is
        use type IO.Count;
        Position : constant IO.Count := IO.Index (The_File);
        Reminder : constant IO.Count := Position mod 2880;
        Z        : constant AS.Stream_Element_Array(1 .. 2880) := [others => 0];
      begin
        if Reminder /= 0 then
          IO.Write (The_File, Z(1 .. AS.Stream_Element_Offset(2880 - Reminder)));
        end if;
      end Pad_To_2880;

      procedure Write_Data is
        use type CI.Uint32;
        use type AS.Stream_Element_Offset;

        W    : constant CI.Uint32 := The_Width;
        H    : constant CI.Uint32 := The_Height;
        Ch   : constant CI.Uint32 := The_Channels;
        Bits : constant CI.Uint32 := The_Bpp;
        BPS  : constant CI.Uint32 := (if Bits = 16 then 2 else 1); -- bytes per sample

        -- small streaming buffer to avoid huge allocations
        Chunk : AS.Stream_Element_Array(1 .. 8192);
        Pos   : AS.Stream_Element_Offset := Chunk'first;

        procedure Flush is
        begin
          if Pos > Chunk'first then
            IO.Write (The_File, Chunk(Chunk'first .. Pos - 1));
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

        -- fetch a byte from the raw buffer (1-based indexing)
        function Byte_At (I : AS.Stream_Element_Offset) return AS.Stream_Element is
        begin
          return The_Buffer(The_Buffer'first + I);
        end Byte_At;

        -- emit one sample as big-endian
        procedure Emit_Sample (Sample_Byte_Offset : AS.Stream_Element_Offset) is
          Lo : AS.Stream_Element;
          Hi : AS.Stream_Element;
        begin
          if Bits = 16 then
            Lo := Byte_At (Sample_Byte_Offset);
            Hi := Byte_At (Sample_Byte_Offset + 1);
            -- little -> big
            Emit (Hi);
            Emit (Lo);
          else
            Emit (Byte_At (Sample_Byte_Offset));
          end if;
        end Emit_Sample;

        Pixels : constant AS.Stream_Element_Offset :=
          AS.Stream_Element_Offset(W) * AS.Stream_Element_Offset(H);

      begin -- Write_Data
        if Bits /= 8 and Bits /= 16 then
          Raise_Error ("Unsupported bpp for FITS:" & CI.Uint32'image(Bits));
        end if;

        if Ch <= 1 then
          -- 1-channel: samples contiguous, just endian-swap on output if 16-bit
          for P in 0 .. Pixels - 1 loop
            declare
              Off : constant AS.Stream_Element_Offset :=
                P * AS.Stream_Element_Offset(BPS);
            begin
              Emit_Sample(Off);
            end;
          end loop;

        else
          -- 3-channel: assume interleaved RGBRGB... per pixel, output planar.
          for C in 0 .. AS.Stream_Element_Offset(Ch) - 1 loop
            for P in 0 .. Pixels - 1 loop
              declare
                Sample_Index : constant AS.Stream_Element_Offset := (P * AS.Stream_Element_Offset(Ch) + C);
                Off          : constant AS.Stream_Element_Offset := Sample_Index * AS.Stream_Element_Offset(BPS);
              begin
                Emit_Sample (Off);
              end;
            end loop;
          end loop;
        end if;

        Flush;
      end Write_Data;

      Bitpix : CI.Uint32;
      use type CI.Uint32;

    begin -- Write_Fits
      Camera_Data.Set (Downloading);

      if The_Bpp = 8 then
        Bitpix := 8;
      elsif The_Bpp = 16 then
        Bitpix := 16;
      else
        Raise_Error ("Unsupported bpp for FITS:" & CI.Uint32'image(The_Bpp));
      end if;

      Log.Write ("### Write FITS:" &
                 " - W:" & The_Width'image &
                 " - H:" & The_Height'image &
                 " - Ch:" & The_Channels'image &
                 " - Bpp:" & The_Bpp'image);

      IO.Create (The_File, IO.Out_File, The_Filename.To_String);

      Put_Block (Card_Logical ("SIMPLE", True));
      Put_Block (Card ("BITPIX", Bitpix));

      if The_Channels <= 1 then
        Put_Block (Card ("NAXIS",  2));
        Put_Block (Card ("NAXIS1", The_Width));
        Put_Block (Card ("NAXIS2", The_Height));
      else
        Put_Block (Card ("NAXIS",  3));
        Put_Block (Card ("NAXIS1", The_Width));
        Put_Block (Card ("NAXIS2", The_Height));
        Put_Block (Card ("NAXIS3", The_Channels));
      end if;

      if The_Bpp = 16 then
        Put_Block (Card ("BSCALE", 1));
        Put_Block (Card ("BZERO",  32768));
      end if;

      Put_Block (End_Card);
      Pad_To_2880;

      Log.Write ("### Write_Data");
       Write_Data;
      Log.Write ("### Write_Data complete");

      Pad_To_2880;
      IO.Close (The_File);
      Log.Write ("### Write_Fits complete");
    exception
    when Item: others =>
      Log.Termination (Item);
      raise;
    end Write_Fits;

    procedure Capture_Picture is
    begin
      Start_Capture;
      Continue_Capture (Picture);

      if Camera_Data.Actual.State /= Stopping then
        Write_Fits;
      end if;

      Disconnect;
    exception
      when others =>
        Disconnect;
    end Capture_Picture;

    procedure Capture_Grid is
    begin
      Start_Capture;
      Continue_Capture (Grid);

      if Camera_Data.Actual.State /= Stopping then
        Camera_Data.Set (Cropped);
      end if;
    exception
    when others =>
      Disconnect;
    end Capture_Grid;

  begin
    Camera_Data.Set (Idle);
    loop
      select
        accept Get (Is_Ready : out Boolean) do
          Is_Ready := One_Device_Is_Ready;
        end Get;

      or
        accept Capture_Picture (Filename  : String;
                                Time      : Exposure.Item;
                                Parameter : Sensitivity.Item)
        do
          The_Filename  := [Filename];
          The_Exposure  := Time;
          The_Parameter := Parameter;
        end Capture_Picture;
        Capture_Picture;

      or
        accept Capture_Grid (Size      : Square_Size;
                             Time      : Exposure.Item;
                             Parameter : Sensitivity.Item)
        do
          The_Grid_Size := Size;
          The_Exposure  := Time;
          The_Parameter := Parameter;
        end Capture_Grid;
        Capture_Grid;

      or
        accept Await_Stop do
          Log.Write ("Stopping");
          if not (Camera_Data.Actual.State in Idle | Error) then
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
