-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
pragma Style_Astronomy;

with Log;
with Win32.Winbase;
with Win32.Wincon;
with Win32.Winuser;

package body Os.Console is

  Normal_Attribute : constant Win32.WORD := Win32.WORD(Win32.Wincon.BACKGROUND_BLUE +
                                                       Win32.Wincon.BACKGROUND_RED +
                                                       Win32.Wincon.BACKGROUND_GREEN +
                                                       Win32.Wincon.BACKGROUND_INTENSITY);

  Highlight_Attribute : constant Win32.WORD := Win32.WORD(Win32.Wincon.BACKGROUND_BLUE +
                                                          Win32.Wincon.BACKGROUND_RED +
                                                          Win32.Wincon.BACKGROUND_GREEN);

  Error_Attribute : constant Win32.WORD := Win32.WORD(Win32.Wincon.FOREGROUND_RED +
                                                      Win32.Wincon.BACKGROUND_BLUE +
                                                      Win32.Wincon.BACKGROUND_RED +
                                                      Win32.Wincon.BACKGROUND_GREEN +
                                                      Win32.Wincon.FOREGROUND_INTENSITY +
                                                      Win32.Wincon.BACKGROUND_INTENSITY);

  type Attributes is array (Count range <>) of Win32.WORD;

  Normal_Attributes    : constant Attributes(1..Count'last) := [others => Normal_Attribute];
  Highlight_Attributes : constant Attributes(1..Count'last) := [others => Highlight_Attribute];
  Error_Attributes     : constant Attributes(1..Count'last) := [others => Error_Attribute];


  function Position_Of (The_Location : Location) return Win32.Wincon.COORD is
  begin
    return (X => Win32.SHORT(The_Location.The_Column - Column'first),
            Y => Win32.SHORT(The_Location.The_Row - Row'first));
  end Position_Of;


  procedure Clear_Region (The_Region  : Region;
                          The_Display : Display) is

    use type Win32.SHORT;

    Left   : constant Win32.SHORT := Win32.SHORT(The_Region.At_Location.The_Column - Column'first);
    Top    : constant Win32.SHORT := Win32.SHORT(The_Region.At_Location.The_Row - Row'first);
    Right  : constant Win32.SHORT := Left + Win32.SHORT(The_Region.With_Size.Width) - 1;
    Botton : constant Win32.SHORT := Top + Win32.SHORT(The_Region.With_Size.Height) - 1;

    Outside : constant Win32.Wincon.COORD := (X => Win32.SHORT(The_Region.With_Size.Width),
                                              Y => Win32.SHORT(The_Region.With_Size.Height));

    Fill_Character : constant Win32.Wincon.union_anonymous2_t := (Which     => Win32.Wincon.AsciiChar_kind,
                                                                  AsciiChar => Win32.CHAR'val (32));

    Fill : aliased Win32.Wincon.CHAR_INFO := (Char       => Fill_Character,
                                              Attributes => Normal_Attribute);

    Rectangle : aliased Win32.Wincon.SMALL_RECT := (Left   => Left,
                                                    Top    => Top,
                                                    Right  => Right,
                                                    Bottom => Botton);
    use type Win32.BOOL;

  begin
    if Win32.Wincon.ScrollConsoleScreenBuffer (hConsoleOutput         => The_Display.Handle,
                                               lpScrollRectangle      => Rectangle'unchecked_access,
                                               lpClipRectangle        => null,
                                               dwDestinationOrigin    => Outside,
                                               lpFill                 => Fill'unchecked_access) = Win32.FALSE
    then
      raise Program_Error;
    end if;
  end Clear_Region;


  procedure Show_Cursor_At (The_Location     : Location;
                            The_Cursor_Style : Cursor_Style;
                            The_Display      : Display) is
    The_Info : aliased Win32.Wincon.CONSOLE_CURSOR_INFO;
    use type Win32.BOOL;
  begin
    if Count(Position_Of (The_Location).X) = The_Display.Screen_Size.Width then
      return;
    end if;
    if Win32.Wincon.SetConsoleCursorPosition (The_Display.Handle,
                                              Position_Of (The_Location)) = Win32.FALSE
    then
      raise Program_Error;
    end if;
    case The_Cursor_Style is
    when Insert_Cursor =>
      The_Info.dwSize := 20;
    when Overstrike_Cursor =>
      The_Info.dwSize := 100;
    when Out_Of_Bounds_Cursor =>
      The_Info.dwSize := 50;
    end case;
    The_Info.bVisible := Win32.TRUE;
    if Win32.Wincon.SetConsoleCursorInfo (The_Display.Handle, The_Info'unchecked_access) = Win32.FALSE then
      raise Program_Error;
    end if;
  end Show_Cursor_At;


  procedure Hide_Cursor (The_Display : Display) is
    The_Info : aliased Win32.Wincon.CONSOLE_CURSOR_INFO;
    use type Win32.BOOL;
  begin
    if Win32.Wincon.GetConsoleCursorInfo (The_Display.Handle, The_Info'unchecked_access) = Win32.FALSE then
      raise Program_Error;
    end if;
    The_Info.bVisible := Win32.FALSE;
    if Win32.Wincon.SetConsoleCursorInfo (The_Display.Handle, The_Info'unchecked_access) = Win32.FALSE then
      raise Program_Error;
    end if;
  end Hide_Cursor;


  function Actual_Cursor (The_Display : Display) return Location is
    The_Info : aliased Win32.Wincon.CONSOLE_SCREEN_BUFFER_INFO;
    use type Win32.BOOL;
    use type Win32.SHORT;
  begin
    if Win32.Wincon.GetConsoleScreenBufferInfo (The_Display.Handle, The_Info'unchecked_access) = Win32.FALSE then
      raise Program_Error;
    end if;
    return (The_Column => Column(The_Info.dwCursorPosition.X + 1),
            The_Row    => Row(The_Info.dwCursorPosition.Y + 1));
  end Actual_Cursor;


  procedure Write (The_String    : String;
                   The_Location  : Location;
                   The_Attribute : Attribute;
                   The_Display   : Display) is

    The_Count : aliased Win32.DWORD;

    use type Win32.BOOL;

  begin
    case The_Attribute is
    when Normal =>
      if Win32.Wincon.WriteConsoleOutputAttribute
          (hConsoleOutput         => The_Display.Handle,
           lpAttribute            => Win32.To_PWSTR(Normal_Attributes(Normal_Attributes'first)'address),
           nLength                => The_String'length,
           dwWriteCoord           => Position_Of (The_Location),
           lpNumberOfAttrsWritten => The_Count'unchecked_access) = Win32.FALSE
      then
        raise Program_Error;
      end if;
    when Highlight =>
      if Win32.Wincon.WriteConsoleOutputAttribute
          (hConsoleOutput         => The_Display.Handle,
           lpAttribute            => Win32.To_PWSTR(Highlight_Attributes(Highlight_Attributes'first)'address),
           nLength                => The_String'length,
           dwWriteCoord           => Position_Of (The_Location),
           lpNumberOfAttrsWritten => The_Count'unchecked_access) = Win32.FALSE
      then
        raise Program_Error;
      end if;
    when Error  =>
      if Win32.Wincon.WriteConsoleOutputAttribute
          (hConsoleOutput         => The_Display.Handle,
           lpAttribute            => Win32.To_PWSTR(Error_Attributes(Error_Attributes'first)'address),
           nLength                => The_String'length,
           dwWriteCoord           => Position_Of (The_Location),
           lpNumberOfAttrsWritten => The_Count'unchecked_access) = Win32.FALSE
      then
        raise Program_Error;
      end if;
    end case;
    if Win32.Wincon.WriteConsoleOutputCharacter
        (hConsoleOutput         => The_Display.Handle,
         lpCharacter            => Win32.To_PCSTR(The_String(The_String'first)'address),
         nLength                => The_String'length,
         dwWriteCoord           => Position_Of (The_Location),
         lpNumberOfCharsWritten => The_Count'unchecked_access) = Win32.FALSE
    then
      raise Program_Error;
    end if;
  end Write;


  function Control_Handler (Ctrl_Type : Win32.DWORD) return Win32.BOOL
    with Convention => Stdcall;

  The_Termination_Handling : Termination_Handler;

  function Control_Handler (Ctrl_Type : Win32.DWORD) return Win32.BOOL is
    use type Win32.DWORD;
  begin
    if Ctrl_Type = Win32.Wincon.CTRL_CLOSE_EVENT then
      The_Termination_Handling.all;
      return Win32.FALSE;
    end if;
    return Win32.FALSE;
  end Control_Handler;


  function New_Display (The_Size    : Size;
                        Termination : Termination_Handler) return Display is

    Screen_Size : constant Win32.Wincon.COORD := (X => Win32.SHORT(The_Size.Width),
                                                  Y => Win32.SHORT(The_Size.Height));
    Console_Out : constant Display := (Handle      => Win32.Winbase.GetStdHandle (Win32.Winbase.STD_OUTPUT_HANDLE),
                                       Screen_Size => The_Size);
    Rectangle : aliased Win32.Wincon.SMALL_RECT;

    Unused : Win32.BOOL;
  begin
    The_Termination_Handling := Termination;
    Rectangle.Left := 0;
    Rectangle.Top := 0;
    Rectangle.Right := Win32.SHORT(The_Size.Width - 1);
    Rectangle.Bottom := Win32.SHORT(The_Size.Height - 1);
    Unused := Win32.Wincon.SetConsoleCtrlHandler (Control_Handler'access, Win32.TRUE);
    Unused := Win32.Wincon.SetConsoleOutputCP (437);
    Unused := Win32.Wincon.SetConsoleWindowInfo(Console_Out.Handle, Win32.TRUE, Rectangle'unchecked_access);
    Unused := Win32.Wincon.SetConsoleScreenBufferSize (Console_Out.Handle, Screen_Size);
    Unused := Win32.Wincon.SetConsoleTextAttribute (Console_Out.Handle, Normal_Attribute);
    return Console_Out;
  exception
  when Item: others =>
    Log.Write ("Display.DefineConsoleOutput", Item);
    return (Handle      => System.Null_Address,
            Screen_Size => (Height => 0,
                            Width  => 0));
  end New_Display;


  function Next_Character (The_Keyboard : Keyboard) return Character is
    The_Input     : aliased Win32.Wincon.INPUT_RECORD;
    Amount_Read   : aliased Win32.DWORD;
    The_Character : Character;
    use type Win32.BOOL;
    use type Win32.WORD;
  begin
    loop
      if Win32.Wincon.ReadConsoleInput (The_Keyboard.Handle,
                                        The_Input'unchecked_access, 1,
                                        Amount_Read'unchecked_access) = Win32.FALSE then
        raise Program_Error;
      end if;
      if The_Input.EventType = Win32.Wincon.KEY_EVENT then
        if The_Input.Event.KeyEvent.bKeyDown /= Win32.FALSE then
          case The_Input.Event.KeyEvent.wVirtualKeyCode is
          when Win32.Winuser.VK_INSERT =>
            The_Character := Insert;
          when Win32.Winuser.VK_HOME =>
            The_Character := Home;
          when Win32.Winuser.VK_PRIOR =>
            The_Character := Page_Up;
          when Win32.Winuser.VK_DELETE =>
            The_Character := Delete;
          when Win32.Winuser.VK_END =>
            The_Character := Finish;
          when Win32.Winuser.VK_NEXT =>
            The_Character := Page_Down;
          when Win32.Winuser.VK_LEFT =>
            The_Character := Left_Arrow;
          when Win32.Winuser.VK_RIGHT =>
            The_Character := Right_Arrow;
          when Win32.Winuser.VK_UP =>
            The_Character := Up_Arrow;
          when Win32.Winuser.VK_DOWN =>
            The_Character := Down_Arrow;
          when Win32.Winuser.VK_RETURN =>
            The_Character := Ascii.Cr;
          when others =>
            The_Character := Character(The_Input.Event.KeyEvent.uChar.AsciiChar);
          end case;
          return The_Character;
        end if;
      end if;
    end loop;
  end Next_Character;


  function New_Keyboard return Keyboard is
  begin
    return (Handle => Win32.Winbase.GetStdHandle (Win32.Winbase.STD_INPUT_HANDLE));
  end New_Keyboard;

end Os.Console;
