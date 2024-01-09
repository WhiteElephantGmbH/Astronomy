-- *********************************************************************************************************************
-- *                       (c) 2010 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
-->Style: White_Elephant

package Eps is

  type Angle is delta 0.00001 range 0.0 .. 360.0;

  function "+" (Left, Right : Angle) return Angle;

  function "-" (Left, Right : Angle) return Angle;


  Delta_Value : constant := 0.00001;
  type Value is delta Delta_Value range -10000.0 .. +10000.0 with Small => Delta_Value;

  type Location is record -- default: lower left corner
    X : Value := 0.0;
    Y : Value := 0.0;
  end record;

  type Dimension is record
    Height : Value;
    Width  : Value;
  end record;

  Delta_Color : constant := 0.01;
  type Color_Value is delta Delta_Color range 0.0 .. 1.0 with Small => Delta_Color;

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

  Light_Blue  : constant Color := (C => 0.80, M => 0.40, Y => 0.00, K => 0.00);
  Light_Green : constant Color := (C => 0.42, M => 0.00, Y => 0.82, K => 0.00);
  Dark        : constant Color := (C => 0.00, M => 0.00, Y => 0.00, K => 0.90);
  Silver      : constant Color := (C => 0.00, M => 0.00, Y => 0.10, K => 0.10);

  type Color_Class is range 0 .. 18;

  type Class_Colors is array (Color_Class) of Color;

  type Line_Style is (Solid, Dashed);

  type Line_Properties is record
    Line_Color : Color;
    Line_Width : Value;
  end record;


  type Text_Size is range 0..99;


  procedure Create (Filename : String;
                    Format   : Dimension;
                    Colors   : Class_Colors);

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

  procedure Start_Arc (Center : Location;
                       Radius : Value;
                       From   : Angle;
                       To     : Angle);

  procedure Start_Circle (To     : Location;
                          Radius : Value);

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
  procedure Clip;

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

  procedure Add_Filled_Circle (To     : Location;
                               Radius : Value;
                               Class  : Color_Class);

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
                      Rotation     : Angle := 0.0;
                      X_Correction : Value := 0.0;
                      Y_Correction : Value := 0.0);

  procedure Save;

  procedure Restore;

  procedure Close;

end Eps;
