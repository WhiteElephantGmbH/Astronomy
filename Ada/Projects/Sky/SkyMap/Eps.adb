-- *********************************************************************************************************************
-- *                       (c) 2010 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
-->Style: White_Elephant

with Ada.Text_IO;

package body Eps is

  function "+" (Left, Right : Angle) return Angle is
    Sum : constant Float := Float(Left) + Float(Right);
  begin
    if Sum <= 360.0 then
      return Angle(Sum);
    else
      return Angle(Sum - 360.0);
    end if;
  end "+";


  function "-" (Left, Right : Angle) return Angle is
    Difference : constant Float := Float(Left) - Float(Right);
  begin
    if Difference > 0.0  then
      return Angle(Difference);
    else
      return Angle(Difference + 360.0);
    end if;
  end "-";


  File : Ada.Text_IO.File_Type;

  procedure Write (Item : String) is
  begin
    Ada.Text_IO.Put_Line (File, Item);
  end Write;


  function Image_Of (Item : Angle) return String is
  begin
    return Angle'image(Item);
  end Image_Of;


  function Image_Of (Item : Value) return String is
    Image : constant String := Value'image(Item);
  begin
    if Item < 0.0 then
      return ' ' & Image;
    else
      return Image;
    end if;
  end Image_Of;


  function Image_Of (Item : Color_Value) return String is
  begin
    return Color_Value'image(Item);
  end Image_Of;


  function X_Y_Of (Item : Location) return String is
  begin
    return Image_Of (Item.X) & Image_Of (Item.Y);
  end X_Y_Of;


  procedure Create (Filename : String;
                    Format   : Dimension) is

    function Img (Item : Value) return String is
    begin
      return Natural'image(Natural(Item));
    end Img;

  begin
    Ada.Text_IO.Create (File, Name => Filename);
    Write ("%!PS-Adobe-3.0 EPSF-3.0");
    Write ("%%BoundingBox: 0 0" & Img(Format.Width) & Img(Format.Height));
    Write ("/Arc {");
    Write ("  newpath");
    Write ("  arc");
    Write ("} def");
    Write ("/Circle {");
    Write ("  newpath");
    Write ("  0 360 arc");
    Write ("  stroke");
    Write ("} def");
    Write ("/Filled_Circle {");
    Write ("  newpath");
    Write ("  0 360 arc");
    Write ("  0 setlinewidth");
    Write ("  fill");
    Write ("} def");
    Write ("/Line {");
    Write ("  newpath");
    Write ("  moveto");
    Write ("  lineto");
    Write ("  stroke");
    Write ("} def");
    Write ("/Rotated_Line {");
    Write ("  gsave");
    Write ("  translate");
    Write ("  rotate");
    Write ("  newpath");
    Write ("  moveto");
    Write ("  lineto");
    Write ("  stroke");
    Write ("  grestore");
    Write ("} def");
    Write ("/Rotated_Text {");
    Write ("  /Cy exch def");
    Write ("  /Cx exch def");
    Write ("  gsave");
    Write ("  translate");
    Write ("  rotate");
    Write ("  dup stringwidth pop 2 div Cx sub neg Cy moveto");
    Write ("  show");
    Write ("  grestore");
    Write ("  } def");
    Write ("/Hatching {");
    Write ("  /A exch def");
    Write ("  /C exch def");
    Write ("  /S exch def");
    Write ("  /R exch def");
    Write ("  /D exch def");
    Write ("  gsave");
    Write ("  translate");
    Write ("  A rotate");
    Write ("  0 0 R 0 360 arc");
    Write ("  clip");
    Write ("  newpath");
    Write ("  R neg R neg moveto");
    Write ("  C {0 D rlineto S D neg rmoveto} repeat");
    Write ("  stroke");
    Write ("  grestore");
    Write ("} def");
    Write ("/Ellipse {");
    Write ("  /Kx exch def");
    Write ("  /Ky exch def");
    Write ("  /Dx exch def");
    Write ("  /Dy exch def");
    Write ("  gsave");
    Write ("  translate");
    Write ("  rotate");
    Write ("  newpath");
    Write ("  0 Dy moveto");
    Write ("  Kx Dy Dx Ky Dx 0 curveto");
    Write ("  Dx Ky neg Kx Dy neg 0 Dy neg curveto");
    Write ("  Kx neg Dy neg Dx neg Ky neg Dx neg 0 curveto");
    Write ("  Dx neg Ky Kx neg Dy 0 Dy curveto");
    Write ("  closepath");
    Write ("  stroke");
    Write ("  grestore");
    Write ("} def");
    Write ("/ellipsedict 8 dict def");
    Write ("ellipsedict /mtrx matrix put");
    Write ("/Elliptical_Arc {");
    Write ("  ellipsedict begin");
    Write ("    /endangle exch def");
    Write ("    /startangle exch def");
    Write ("    /yrad exch def");
    Write ("    /xrad exch def");
    Write ("    /y exch def");
    Write ("    /x exch def");
    Write ("    /savematrix mtrx currentmatrix def");
    Write ("    x y translate");
    Write ("    xrad yrad scale");
    Write ("    0 0 1 startangle endangle arc");
    Write ("    savematrix setmatrix");
    Write ("  end");
    Write ("} def");
  end Create;


  procedure Set_Color (Item : Color) is
  begin
    Write (" " & Image_Of(Item.C)  & Image_Of(Item.M) & Image_Of(Item.Y) & Image_Of(Item.K) & " setcmykcolor");
  end Set_Color;


  procedure Set_Gray is
  begin
    Write ("  0.9 setgray");
  end Set_Gray;


  procedure Set_White is
  begin
    Write ("  1 setgray");
  end Set_White;


  procedure Set_Black is
  begin
    Write ("  0 setgray");
  end Set_Black;


  procedure Add_Arc (Center    : Location;
                     Radius    : Value;
                     From      : Angle;
                     To        : Angle;
                     Is_Filled : Boolean := False) is
  begin
    Start_Arc (Center => Center,
               Radius => Radius,
               From   => From,
               To     => To);
    if Is_Filled then
      Fill;
    else
      Stroke;
    end if;
  end Add_Arc;


  procedure Add_Circle (To        : Location;
                        Radius    : Value;
                        Is_Filled : Boolean := False) is
  begin
    if Is_Filled then
      Write (" " & X_Y_Of (To) & Image_Of (Radius) & " Filled_Circle");
    else
      Write (" " & X_Y_Of (To) & Image_Of (Radius) & " Circle");
    end if;
  end Add_Circle;


  procedure Add_Ellipse (To       : Location;
                         A        : Value;
                         B        : Value;
                         Rotation : Angle) is
    K  : constant Value := 0.551784;
    Dx : constant Value := A / 2.0;
    Dy : constant Value := B / 2.0;
  begin
    if (A /= 0.0) and (B /= 0.0) then
      Write (" " & Image_Of (Rotation)
                 & X_Y_Of (To)
                 & Image_Of (Dx)
                 & Image_Of (Dy)
                 & Image_Of (Value'(Dx * K))
                 & Image_Of (Value'(Dy * K))
                 & " Ellipse");
    end if;
  end Add_Ellipse;


  procedure Add_Hatching (To         : Location;
                          Radius     : Value;
                          Separation : Value;
                          Rotation   : Angle) is

    Diameter  : constant Value := Radius * 2.0;
    Count     : constant Natural := Natural(Diameter / Separation);
    Increment : constant Value := Diameter / Count;

  begin
    if Radius /= 0.0 then
      Write (" " & X_Y_Of (To)
                 & Image_Of (Diameter)
                 & Image_Of (Radius)
                 & Image_Of (Increment)
                 & Natural'image (Count + 1)
                 & Image_Of (Rotation)
                 & " Hatching");
    end if;
  end Add_Hatching;


  procedure Add_Line (From : Location;
                      To   : Location) is
  begin
    Write (" " & X_Y_Of (From) & X_Y_Of (To)  & " Line");
  end Add_Line;


  procedure Add_Line (From     : Location;
                      To       : Location;
                      Center   : Location;
                      Rotation : Angle) is
  begin
    Write (" " & Image_Of (From.X)
               & Image_Of (From.Y)
               & Image_Of (To.X)
               & Image_Of (To.Y)
               & Image_Of (Rotation)
               & X_Y_Of (Center)
               & " Rotated_Line");
  end Add_Line;


  procedure Set_Line (Width : Value) is
  begin
    Write (" " & Image_Of (Width) & " setlinewidth");
  end Set_Line;


  procedure Set (Properies : Line_Properties) is
  begin
    Set_Color (Properies.Line_Color);
    Set_Line (Properies.Line_Width);
  end Set;


  procedure Set_Line (Style : Line_Style) is
  begin
    case Style is
    when Solid =>
      Write ("  [] 0 setdash");
    when Dashed =>
      Write ("  [3] 0 setdash");
    end case;
  end Set_Line;


  procedure Start_Line (From  : Location) is
  begin
    Write ("  newpath");
    Write (" " & X_Y_Of (From) & " moveto");
  end Start_Line;


  procedure Continue_Line (To : Location) is
  begin
    Write (" " & X_Y_Of (To) & " lineto");
  end Continue_Line;


  procedure Continue_Curve (P1 : Location;
                            P2 : Location;
                            To : Location) is
  begin
    Write (" " & X_Y_Of (P1) & X_Y_Of (P2) & X_Y_Of (To) & " curveto");
  end Continue_Curve;


  procedure Start_Arc (Center : Location;
                       Radius : Value;
                       From   : Angle;
                       To     : Angle) is
  begin
    Write (" " & X_Y_Of (Center) & Image_Of (Radius) & Image_Of (From) & Image_Of (To) &  " Arc");
  end Start_Arc;


  procedure Start_Circle (To     : Location;
                          Radius : Value) is
  begin
    Start_Arc (Center => To,
               Radius => Radius,
               From   => 0.0,
               To     => 360.0);
  end Start_Circle;


  procedure Start_Elliptical_Arc (Center : Location;
                                  A      : Value;
                                  B      : Value;
                                  From   : Angle;
                                  To     : Angle) is
    Dx : constant Value := A / 2.0;
    Dy : constant Value := B / 2.0;
  begin
    Write ("  newpath");
    if (A /= 0.0) and (B /= 0.0) then
      Write (" " & X_Y_Of (Center)
                 & Image_Of (Dx)
                 & Image_Of (Dy)
                 & Image_Of (From)
                 & Image_Of (To)
                 & " Elliptical_Arc");
    end if;
  end Start_Elliptical_Arc;


  procedure Continue_Arc (Center : Location;
                          Radius : Value;
                          From   : Angle;
                          To     : Angle) is
  begin
    Write (" " & X_Y_Of (Center) & Image_Of (Radius) & Image_Of (From) & Image_Of (To) &  " arc");
  end Continue_Arc;


  procedure Continue_Arc_N (Center : Location;
                            Radius : Value;
                            From   : Angle;
                            To     : Angle) is
  begin
    Write (" " & X_Y_Of (Center) & Image_Of (Radius) & Image_Of (From) & Image_Of (To) &  " arcn");
  end Continue_Arc_N;


  procedure Clip is
  begin
    Write ("  clip");
  end Clip;


  procedure Stroke is
  begin
    Write ("  stroke");
  end Stroke;


  procedure Fill is
  begin
    Write ("  0 setlinewidth");
    Write ("  fill");
  end Fill;


  procedure Define_Font (Item : String;
                         Size : Text_Size) is
  begin
    Write ("  /" & Item & " findfont" & Text_Size'image(Size) & " scalefont setfont");
  end Define_Font;


  procedure Add_Text (Item         : String;
                      Center       : Location;
                      Rotation     : Angle := 0.0;
                      X_Correction : Value := 0.0;
                      Y_Correction : Value := 0.0) is
  begin
    Write ("  (" & Item & ")"
                 & Image_Of (Rotation)
                 & X_Y_Of (Center)
                 & Image_Of (X_Correction)
                 & Image_Of (Y_Correction)
                 & " Rotated_Text");
  end Add_Text;


  procedure Save is
  begin
    Write ("  gsave");
  end Save;


  procedure Restore is
  begin
    Write ("  grestore");
  end Restore;


  procedure Close is
  begin
    Write ("%%EOF");
    Ada.Text_IO.Close (File);
  end Close;

end Eps;