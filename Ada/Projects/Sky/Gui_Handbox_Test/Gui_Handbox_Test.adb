-- *********************************************************************************************************************
-- *                           (c) 2026 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

pragma Build (Description => "GUI Handbox Test",
              Version  => (0, 0, 0, 0),
              Kind     => Windows,
              Icon     => False,
              Compiler => "GNAT\14.2");

with Gui;

procedure Gui_Handbox_Test is
  Page                                                   : Gui.Page;
  Speed, Handbox, Direction_Buttons, Top, Middle, Bottom : Gui.Group;
  Slower, Faster                                         : Gui.Button with Unreferenced;
  Speed_Value                                            : Gui.Value_Box;
  Speed_Index                                            : Natural := 1;
  Speed_Button_Size                                      : constant := 44;
  Up, Left, Center, Right, Down                          : Gui.Button with Unreferenced;

  procedure Return_To_Target (Event : Gui.Button_Event) is
  begin
    case Event is
    when Gui.Pressed =>
      Gui.Set_Status_Line ("return to target");
    when Gui.Released =>
      null;
    end case;
  end Return_To_Target;


  procedure Move_Up (Event : Gui.Button_Event) is
  begin
    case Event is
    when Gui.Pressed =>
      Gui.Set_Status_Line ("move up");
    when Gui.Released =>
      Gui.Set_Status_Line ("stop moving up");
    end case;
  end Move_Up;


  procedure Move_Down (Event : Gui.Button_Event) is
  begin
    case Event is
    when Gui.Pressed =>
      Gui.Set_Status_Line ("move down");
    when Gui.Released =>
      Gui.Set_Status_Line ("stop moving down");
    end case;
  end Move_Down;


  procedure Move_Left (Event : Gui.Button_Event) is
  begin
    case Event is
    when Gui.Pressed =>
      Gui.Set_Status_Line ("move left");
    when Gui.Released =>
      Gui.Set_Status_Line ("stop moving left");
    end case;
  end Move_Left;


  procedure Move_Right (Event : Gui.Button_Event) is
  begin
    case Event is
    when Gui.Pressed =>
      Gui.Set_Status_Line ("move right");
    when Gui.Released =>
      Gui.Set_Status_Line ("stop moving right");
    end case;
  end Move_Right;


  procedure Show_Speed is
  begin
    case Speed_Index is
    when 0 => Gui.Set_Text (Speed_Value, "7.50""/s");
    when 1 => Gui.Set_Text (Speed_Value, "14.99""/s");
    when others => Gui.Set_Text (Speed_Value, "29.98""/s");
    end case;
  end Show_Speed;


  procedure Decrease_Speed is
  begin
    if Speed_Index > 0 then
      Speed_Index := Speed_Index - 1;
      Show_Speed;
    end if;
  end Decrease_Speed;


  procedure Increase_Speed is
  begin
    if Speed_Index < 2 then
      Speed_Index := Speed_Index + 1;
      Show_Speed;
    end if;
  end Increase_Speed;


  procedure Startup is
  begin
    Page := Gui.Add_Page ("Handbox");
    Handbox := Gui.Create (Page, Gui.Vertical,
                           Expand => Gui.Fixed,
                           Title  => "Handbox");
    Speed := Gui.Create (Handbox, Gui.Horizontal, Expand => Gui.Fixed, Title => "Speed");
    Slower := Gui.Create (Speed, Gui.Arrow_Left, Decrease_Speed'unrestricted_access,
                          The_Button_Size => Speed_Button_Size);
    Speed_Value := Gui.Create_Value (Speed, "14.99""/s", Minimum_Width => 90);
    Faster := Gui.Create (Speed, Gui.Arrow_Right, Increase_Speed'unrestricted_access,
                          The_Button_Size => Speed_Button_Size);
    Direction_Buttons := Gui.Create (Handbox, Gui.Vertical,
                                     Equal_Cells => True,
                                     Ratio       => Gui.Square,
                                     Frame       => True);
    Top := Gui.Create (Direction_Buttons, Gui.Horizontal, Equal_Cells => True);
    Gui.Space (Top);
    Up := Gui.Create (Top, Gui.Arrow_Up,
                      Move_Up'unrestricted_access,
                      Expand => True);
    Gui.Space (Top);
    Middle := Gui.Create (Direction_Buttons, Gui.Horizontal, Equal_Cells => True);
    Left := Gui.Create (Middle, Gui.Arrow_Left,
                        Move_Left'unrestricted_access,
                        Expand => True);
    Center := Gui.Create (Middle, Gui.Cross_Symbol, Return_To_Target'unrestricted_access, Expand => True);
    Right := Gui.Create (Middle, Gui.Arrow_Right,
                         Move_Right'unrestricted_access,
                         Expand => True);
    Bottom := Gui.Create (Direction_Buttons, Gui.Horizontal, Equal_Cells => True);
    Gui.Space (Bottom);
    Down := Gui.Create (Bottom, Gui.Arrow_Down,
                        Move_Down'unrestricted_access,
                        Expand => True);
    Gui.Space (Bottom);
    Gui.Set_Status_Line ("READY");
  end Startup;

begin
  Gui.Execute ("Gui Handbox Test", Startup'access,
               Initial_Metrics => (0, 0, 1, 350));
end Gui_Handbox_Test;
