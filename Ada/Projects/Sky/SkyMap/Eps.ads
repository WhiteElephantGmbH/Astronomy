-- *********************************************************************************************************************
-- *                       (c) 2010 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
-->Style: White_Elephant

package Eps is

  type Angle is delta 0.00001 range 0.0 .. 360.0;

  function "+" (Left, Right : Angle) return Angle;

  function "-" (Left, Right : Angle) return Angle;


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
  Blue  : constant Color := (C => 1.0, M => 1.0, Y => 0.0, K => 0.0);

  Light_Blue : constant Color := (C => 0.8, M => 0.4, Y => 0.0, K => 0.0);
  Dark       : constant Color := (C => 0.0, M => 0.0, Y => 0.0, K => 0.95);
  Silver     : constant Color := (C => 0.0, M => 0.0, Y => 0.1, K => 0.1);

  type Line_Style is (Solid, Dashed);

  type Line_Properties is record
    Line_Color : Color;
    Line_Width : Value;
  end record;


  type Text_Size is range 0..99;


  procedure Create (Filename    : String;
                    Lower_Left  : Location;
                    Upper_Right : Location);

  procedure Set_Gray;

  procedure Set_White;

  procedure Set_Black;

  procedure Set_Color (Item : Color);

  procedure Set_Line (Width : Value);

  procedure Set (Properies : Line_Properties);

  procedure Set_Line (Style : Line_Style);

  procedure Start_Line (From : Location);

  procedure Continue_Line (To : Location);

  procedure Continue_Curve (P1 : Location;
                            P2 : Location;
                            To : Location);

  procedure Start_Elliptical_Arc (Center : Location;
                                  A      : Value;
                                  B      : Value;
                                  From   : Angle;
                                  To     : Angle);

  procedure Continue_Arc (Center : Location;
                          Radius : Value;
                          From   : Angle;
                          To     : Angle);

  procedure Continue_Arc_N (Center : Location;
                            Radius : Value;
                            From   : Angle;
                            To     : Angle);
  procedure Fill;

  procedure Stroke;

  procedure Add_Arc (Center    : Location;
                     Radius    : Value;
                     From      : Angle;
                     To        : Angle;
                     Is_Filled : Boolean := False);

  procedure Add_Circle (To        : Location;
                        Radius    : Value;
                        Is_Filled : Boolean := False);

  procedure Add_Ellipse (To       : Location;
                         A        : Value;
                         B        : Value;
                         Rotation : Angle);

  procedure Add_Hatching (To         : Location;
                          Radius     : Value;
                          Separation : Value;
                          Rotation   : Angle);

  procedure Add_Line (From : Location;
                      To   : Location);

  procedure Add_Line (From     : Location;
                      To       : Location;
                      Center   : Location;
                      Rotation : Angle);

  procedure Define_Font (Item : String;
                         Size : Text_Size);

  procedure Add_Text (Item         : String;
                      Center       : Location;
                      Rotation     : Angle;
                      X_Correction : Value;
                      Y_Correction : Value);

  procedure Save;

  procedure Restore;

  procedure Close;

end Eps;