-- *********************************************************************************************************************
-- *                       (c) 2010 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
-->Style: White_Elephant

package Eps is

  type Value is delta 0.00001 range -10000.0 .. +10000.0;

  type Location is record -- default lower left corner
    X : Value := 0.0;
    Y : Value := 0.0;
  end record;

  type Color_Value is delta 0.01 range 0.00 .. 1.001; -- .001 aonix bug ?

  type Color is record -- default lower left corner
    C : Color_Value;
    M : Color_Value;
    Y : Color_Value;
    K : Color_Value;
  end record;

  Magenta : constant Color := (C => 0.0, M => 1.0, Y => 0.0, K => 0.0);
  Yellow  : constant Color := (C => 0.0, M => 0.0, Y => 1.0, K => 0.0);
  Cyan    : constant Color := (C => 1.0, M => 0.0, Y => 0.0, K => 0.0);
  Black   : constant Color := (C => 0.0, M => 0.0, Y => 0.0, K => 1.0);

  Red   : constant Color := (C => 0.0, M => 1.0, Y => 1.0, K => 0.0);
  Green : constant Color := (C => 1.0, M => 0.0, Y => 1.0, K => 0.0);
  Blue  : constant Color := (C => 1.0, M => 0.0, Y => 0.0, K => 0.0);

  Light_Blue  : constant Color := (C => 0.8, M => 0.4, Y => 0.0, K => 0.0);

  type Line_Style is (Solid, Dashed);

  procedure Create (Filename    : String;
                    Lower_Left  : Location;
                    Upper_Right : Location);

  procedure Set_Gray;

  procedure Set_White;

  procedure Set_Black;

  procedure Set_Color (Item : Color);

  procedure Set_Line (Width : Value);

  procedure Set_Line (Style : Line_Style);

  procedure Start_Line (From  : Location);

  procedure Continue_Line (To : Location);

  procedure Draw_Line;

  procedure Fill;

  procedure Add_Circle (To        : Location;
                        Radius    : Value;
                        Is_Filled : Boolean := False);

  procedure Add_Line (From : Location;
                      To   : Location);

  procedure Save;

  procedure Restore;

  procedure Close;

end Eps;