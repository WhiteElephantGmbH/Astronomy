-- *********************************************************************************************************************
-- *                       (c) 2010 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
-->Style: White_Elephant

with Ada.Text_IO;

package body Eps is

  File : Ada.Text_IO.File_Type;

  procedure Write (Item : String) is
  begin
    Ada.Text_IO.Put_Line (File, Item);
  end Write;


  function Image_Of (Item : Value) return String is
  begin
    return Value'image(Item);
  end Image_Of;


  function Image_Of (Item : Color_Value) return String is
  begin
    return Color_Value'image(Item);
  end Image_Of;


  X_Offset : Value;
  Y_Offset : Value;


  function X_Y_Of (Item : Location) return String is
  begin
    return Image_Of (Item.X + X_Offset) & Image_Of (Item.Y + Y_Offset);
  end X_Y_Of;


  procedure Create (Filename    : String;
                    Lower_Left  : Location;
                    Upper_Right : Location) is

    function Img (Item : Value) return String is
    begin
      return Natural'image(Natural(Item));
    end Img;

  begin
    X_Offset := Lower_Left.X;
    Y_Offset := Lower_Left.Y;
    Ada.Text_IO.Create (File, Name => Filename);
    Write ("%!PS-Adobe-3.0 EPSF-3.0");
    Write ("%%BoundingBox: 0 0" & Img(Upper_Right.X + X_Offset) & Img(Upper_Right.Y + Y_Offset));
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
  end Create;


  procedure Set_Color (Item : Color) is
  begin
    Write (" "& Image_Of(Item.C)  & Image_Of(Item.M) & Image_Of(Item.Y) & Image_Of(Item.K) & " setcmykcolor");
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


  procedure Add_Line (From : Location;
                      To   : Location) is
  begin
    Write (" " & X_Y_Of (From) & X_Y_Of (To)  & " Line");
  end Add_Line;


  procedure Set_Line (Width : Value) is
  begin
    Write (" " & Image_Of (Width) & " setlinewidth");
  end Set_Line;


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


  procedure Draw_Line is
  begin
    Write ("  stroke");
  end Draw_Line;


  procedure Fill is
  begin
    Write ("  0 setlinewidth");
    Write ("  fill");
  end Fill;


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