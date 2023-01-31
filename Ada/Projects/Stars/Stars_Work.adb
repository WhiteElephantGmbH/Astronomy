-- *********************************************************************************************************************
-- *                           (c) 2010 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
-->Style: White_Elephant

with Coordinate;
with Constellation;
with Data;
with Map;

with Glib;
with Gdk;
with Gdk.Drawable;
with Gdk.Event;
with Gdk.Font;
with Gdk.GC;
with Gdk.Pixmap;
with Gdk.Rectangle;
with Gdk.Window;
with Gtk;
with Gtk.Box;
with Gtk.Drawing_Area;
with Gtk.Enums;
with Gtk.Main;
with Gtk.Rc;
with Gtk.Handlers;
with Gtk.Style;
with Gdk.Types;
with Gtk.Window;

with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Exceptions;

procedure Stars_Work is

  package GC renames Gdk.GC;
  package GD renames Gdk.Drawable;
  package GE renames Gdk.Event;
  package GF renames Gdk.Font;
  package GP renames Gdk.Pixmap;
  package GR renames Gdk.Rectangle;
  package GT renames Gdk.Types;
  package GW renames Gdk.Window;
  package BO renames Gtk.Box;
  package DA renames Gtk.Drawing_Area;
  package EN renames Gtk.Enums;
  package ST renames Gtk.Style;
  package WI renames Gtk.Window;

  use type Glib.Gint;

  package Math is new Ada.Numerics.Generic_Elementary_Functions (Coordinate.Value);

  Constellation_File : constant String := "Constellations.Csv";

  Radius  : constant := 550;
  Size    : constant := 2 * Radius + 10;
  Offset  : constant Glib.Gint := Size / 2;

  function Sqr  (X : Coordinate.Value) return Coordinate.Value is
    use type Coordinate.Value;
  begin
    return X * X;
  end Sqr;

  Pixmap       : GP.Gdk_Pixmap;
  Font         : GF.Gdk_Font;
  Window       : WI.Gtk_Window;
  Drawing_Area : DA.Gtk_Drawing_Area;
  Line_Style   : GC.Gdk_GC;

  package Configured is new Gtk.Handlers.Return_Callback
    (Widget_Type => DA.Gtk_Drawing_Area_Record,
     Return_Type => Boolean);

  package Destroyed is new Gtk.Handlers.Callback
    (Widget_Type => WI.Gtk_Window_Record);


  function X_Pos_Of (Item : Coordinate.Position) return Glib.Gint is
    use type Coordinate.Position;
  begin
    return Offset - Glib.Gint(Item * Coordinate.Position(Radius));
  end X_Pos_Of;


  function Y_Pos_Of (Item : Coordinate.Position) return Glib.Gint is
    use type Coordinate.Position;
  begin
    return Offset - Glib.Gint(Item * Coordinate.Position(Radius));
  end Y_Pos_Of;


  function X_Pos_Of (Item : Glib.Gint) return Coordinate.Position is
    use type Coordinate.Position;
  begin
    return Coordinate.Position(Offset - Item) / Coordinate.Position(Radius);
  end X_Pos_Of;


  function Y_Pos_Of (Item : Glib.Gint) return Coordinate.Position is
    use type Coordinate.Position;
  begin
    return Coordinate.Position(Offset - Item) / Coordinate.Position(Radius);
  end Y_Pos_Of;


  function Unicode_Of (Item : String) return String is
    Degree : constant String := Character'val(16#C2#) & Character'val(16#BA#);
  begin
    for Index in Item'range loop
      if Item(Index) = '°' then
        return Item(Item'first .. Index - 1) & Degree & Item(Index + 1 .. Item'last);
      end if;
    end loop;
    return Item;
  end Unicode_Of;


  ----------
  -- Draw --
  ----------

  procedure Draw_Line (X1, Y1 : Glib.Gint;
                       X2, Y2 : Glib.Gint) is
  begin
    GD.Draw_Line (Pixmap, Line_Style, X1, Y1, X2, Y2);
  end Draw_Line;


  procedure Draw_Circle (X, Y : Glib.Gint;
                         D    : Float;
                         Fill : Boolean := False) is
  begin
    GD.Draw_Arc (Pixmap, ST.Get_Black (DA.Get_Style (Drawing_Area)), Fill,
                 X - Glib.Gint(D / 2.0), Y - Glib.Gint(D / 2.0), Glib.Gint(D) + 1, Glib.Gint(D) + 1, 0, 360 * 64);
    DA.Draw (Drawing_Area);

  end Draw_Circle;


  procedure Draw_Text (X, Y : Glib.Gint;
                       Text : String) is
  begin
    GD.Draw_Text (Pixmap, Font, ST.Get_Black (DA.Get_Style (Drawing_Area)),
                  X, Y, Text);
  end Draw_Text;


  procedure Draw_Visible_Area (Location : Coordinate.Polar) is

    Location_Ra  : constant Coordinate.Right_Ascension := Coordinate.Ra_Of (Location);
    Location_Dec : constant Coordinate.Declination     := Coordinate.Dec_Of (Location);

    use type Coordinate.Right_Ascension;
    use type Coordinate.Declination;

    function Dec_Of (Angle : Coordinate.Right_Ascension) return Coordinate.Declination is
      use type Coordinate.Value;
    begin
      if Coordinate.Orientation * Location_Dec = 90.0 then
        return 0.0;
      else
        declare
          B : constant Coordinate.Value := Coordinate.Radian_Of (Location_Dec);
          G : constant Coordinate.Value := Coordinate.Radian_Of (Angle);
          D : constant Coordinate.Declination
            := Coordinate.Arc_Of (Math.Arcsin (Math.Sqrt(1.0 / (1.0 + (Sqr(Math.Tan(B)) / Sqr(Math.Cos(G)))))));
        begin
          if abs((Angle) - 12.0) > 6.0 then
            return -D;
          end if;
          return D;
        end;
      end if;
    end Dec_Of;

    The_Ra  : Coordinate.Right_Ascension := 0.0;
    The_Dec : Coordinate.Declination;

    C1 : Coordinate.Cartesian;
    C2 : Coordinate.Cartesian;

    procedure Draw (Is_First : Boolean := False) is
    begin
      The_Dec := Coordinate.Orientation * Dec_Of (The_Ra);
      if Coordinate.Is_Outside (Location_Dec) then
        The_Dec := - The_Dec;
      end if;
      C1 := C2;
      C2 := Coordinate.Point_Of (Dec => The_Dec, Ra => Location_Ra - The_Ra);
      if not Is_First then
        Draw_Line (X1 => X_Pos_Of (Coordinate.X_Of(C1)),
                   Y1 => Y_Pos_Of (Coordinate.Y_Of(C1)),
                   X2 => X_Pos_Of (Coordinate.X_Of(C2)),
                   Y2 => Y_Pos_Of (Coordinate.Y_Of(C2)));
      end if;
    end Draw;

  begin -- Draw_Visible_Area
    Draw (Is_First => True);
    loop
      The_Ra := The_Ra + 0.01;
      Draw;
      exit when The_Ra >= 24.0;
    end loop;
  end Draw_Visible_Area;


  Error : exception;

  procedure Error_Handler (Message : String) is
  begin
    Draw_Text (20, 20, Message);
    raise Error;
  end Error_Handler;

  package Star is new Data (Error_Handler);


  --------------
  -- Draw_Map --
  --------------
  procedure Draw_Map (Drawing_Area : access DA.Gtk_Drawing_Area_Record'class) is

    Win    : GW.Gdk_Window;
    Width  : Glib.Gint;
    Height : Glib.Gint;

    procedure Draw_Line (Part : Constellation.Part) is
      From : constant Coordinate.Cartesian := Star.Point_Of (Part.From);
      To   : constant Coordinate.Cartesian := Star.Point_Of (Part.To);
    begin
      Draw_Line (X1 => X_Pos_Of (Coordinate.X_Of(From)),
                 Y1 => Y_Pos_Of (Coordinate.Y_Of(From)),
                 X2 => X_Pos_Of (Coordinate.X_Of(To)),
                 Y2 => Y_Pos_Of (Coordinate.Y_Of(To)));
    end Draw_Line;

    procedure Draw_Constellation is new Constellation.Itterate (Draw_Line);

  begin -- draw_map
    Win := DA.Get_Window (Drawing_Area);
    GD.Get_Size (Win, Width, Height);
    Gdk.Pixmap.Gdk_New (Pixmap, Win, Width, Height, -1);

    GC.Gdk_New (Line_Style, Win);
    GC.Set_Line_Attributes (Line_Style,
                            Line_Width => 1,
                            Line_Style => GC.Line_Solid,
                            Cap_Style  => GC.Cap_Butt,
                            Join_Style => GC.Join_Round);

    GD.Draw_Rectangle (Pixmap, ST.Get_White (DA.Get_Style (Drawing_Area)),
                       True, 0, 0, Width, Height);

    Draw_Circle (Offset, Offset, 3.0);
    Draw_Circle (Offset, Offset, Float(Radius * 2));
    Draw_Circle (Offset, Offset, Float(Radius));

    Star.Read;
    while Star.Next loop
      declare

        Point : constant Coordinate.Cartesian := Star.Point;

        function Star_Size return Float is
          M    : constant Float := Float(Star.Mag);
          Mmin : constant Float := -1.5;
          Mmax : constant Float := 6.5;
          Bmin : constant Float := 1.0;
          Bmax : constant Float := 8.0;
        begin
          return Bmin + ((Bmax - Bmin) / (Mmin - Mmax)) * (M - Mmax);
        end Star_Size;

      begin
        Draw_Circle (X    => X_Pos_Of (Coordinate.X_Of (Point)),
                     Y    => Y_Pos_Of (Coordinate.Y_Of (Point)),
                     D    => Star_Size,
                     Fill => True);
      end;
    end loop;

    if Constellation.Read (Constellation_File) then
      Draw_Constellation;
    end if;

  end Draw_Map;


  ---------------------
  -- Configure_Event --
  ---------------------
  function Configure_Event (Drawing_Area : access DA.Gtk_Drawing_Area_Record'class)
     return Boolean is
  begin
    Draw_Map (Drawing_Area);
    return True;
  end Configure_Event;


  ------------------
  -- Expose_Event --
  ------------------

  function Expose_Event (Drawing_Area : access DA.Gtk_Drawing_Area_Record'class;
                         Event        :        GE.Gdk_Event) return Boolean is

     Area : constant GR.Gdk_Rectangle := GE.Get_Area (Event);

  begin
     GD.Draw_Pixmap (DA.Get_Window (Drawing_Area),
                     ST.Get_Fg_GC (DA.Get_Style (Drawing_Area), EN.State_Normal),
                     Pixmap, Area.X, Area.Y, Area.X, Area.Y,
                     Glib.Gint (Area.Width), Glib.Gint (Area.Height));
     return True;
  end Expose_Event;


  ------------------------
  -- Button_Press_Event --
  ------------------------

  From_Hr  : Natural;
  Has_From : Boolean := False;

  function Button_Press_Event (Widget : access DA.Gtk_Drawing_Area_Record'class;
                               Event  :        GE.Gdk_Event) return Boolean is

    pragma Warnings (Off, Widget);

    use type Gdk.Pixmap.Gdk_Pixmap;
    use type Glib.Guint;
    use type Coordinate.Declination;

    Xp : constant Glib.Gint := Glib.Gint(GE.Get_X (Event));
    Yp : constant Glib.Gint := Glib.Gint(GE.Get_Y (Event));

    Loc : constant Coordinate.Polar := Coordinate.Location_Of (X => X_Pos_Of (Xp),
                                                               Y => Y_Pos_Of (Yp));
    package Star_Map is new Map (Error_Handler);

    use type Star_Map.Star.Magnitude;

  begin
    if Pixmap /= GP.Null_Pixmap then
      if GE.Get_Button (Event) = 1 then
        declare
          To_Hr : constant Natural := Star.Hr_Of (Loc);
        begin
          if To_Hr /= 0 then
            if Has_From and then To_Hr /= From_Hr then
              Draw_Text (113, 85, "To" & Natural'image(To_Hr));
              declare
                From_Loc : constant Coordinate.Cartesian := Star.Point_Of (From_Hr);
                To_Loc   : constant Coordinate.Cartesian := Star.Point_Of (To_Hr);
                X_From : constant Glib.Gint := X_Pos_Of (Coordinate.X_Of (From_Loc));
                Y_From : constant Glib.Gint := Y_Pos_Of (Coordinate.Y_Of (From_Loc));
                X_To   : constant Glib.Gint := X_Pos_Of (Coordinate.X_Of (To_Loc));
                Y_To   : constant Glib.Gint := Y_Pos_Of (Coordinate.Y_Of (To_Loc));
                Part   : constant Constellation.Part := (From => From_Hr, To => To_Hr);
              begin
                if Constellation.Removed (Part) then
                  Draw_Text (40, 100, "Part Removed");
                else
                  --GC.Set_Function (Line_Style, GC.Set);
                  Draw_Line (X_From, Y_From, X_To, Y_To);
                  Constellation.Add (Item => (From => From_Hr, To => To_Hr),
                                     Name => Star.Name_Of (From_Hr));
                end if;
                DA.Draw (Widget);
              end;
              Has_From := False;
              From_Hr := 0;
            else
              From_Hr := To_Hr;
              Has_From := True;
              GD.Draw_Rectangle (Pixmap, ST.Get_White (DA.Get_Style (Drawing_Area)),
                                 True, 40, 70, 140, 30);
              Draw_Text (40, 85, "From" & Natural'image(From_Hr));
              DA.Draw (Widget);
            end if;
          end if;
        end;
      elsif GE.Get_Button (Event) = 3 then
        Constellation.Save (Constellation_File);
        Star_Map.Draw (Declination              => Coordinate.Dec_Of(Loc),
                       Margin                   => 12.5,
                       Map_Size                 => 570.0,
                       Constellation_Line_Width => 0.4,
                       Ecliptic_Line_Width      => 0.8,
                       Equator_Line_Width       => 0.8,
                       Visibility_Line_Width    => 0.8,
                       Brightness    => (Limit  => -1.5,
                                         Filter => 6.0,
                                         Min    => 0.3,
                                         Max    => 4.0));
        Draw_Visible_Area (Loc);
        Draw_Line (Xp - 3, Yp, Xp + 3, Yp);
        Draw_Line (Xp, Yp - 3, Xp, Yp + 3);
        DA.Draw (Widget);
      elsif GE.Get_Button (Event) = 2 then
        Coordinate.Set (Coordinate.Southern);
        Draw_Map (Widget);
      end if;
    end if;
    return True;
  exception
  when Error =>
    return True;
  when E: others =>
    Draw_Text (200, 200, "Exception" & Ada.Exceptions.Exception_Message(E));
    return True;
  end Button_Press_Event;


  -------------------------
  -- Motion_Notify_Event --
  -------------------------

  function Motion_Notify_Event
    (Widget : access DA.Gtk_Drawing_Area_Record'class;
     Event  : in     GE.Gdk_Event)
     return Boolean
  is
     X, Y   : Glib.Gint;
     State  : GT.Gdk_Modifier_Type;
     Result : GW.Gdk_Window;

     use type GP.Gdk_Pixmap;
     use type GT.Gdk_Modifier_Type;

  begin
    if GE.Get_Is_Hint (Event) then
      GW.Get_Pointer (DA.Get_Window (Widget), X, Y, State, Result);
    else
      X := Glib.Gint (GE.Get_X (Event));
      Y := Glib.Gint (GE.Get_Y (Event));
      State := GE.Get_State (Event);
    end if;
    if Pixmap /= GP.Null_Pixmap then
      declare
        Loc : constant Coordinate.Polar := Coordinate.Location_Of (X => X_Pos_Of (X),
                                                                   Y => Y_Pos_Of (Y));
        Dec : constant Coordinate.Declination := Coordinate.Dec_Of (Loc);

        use type Coordinate.Declination;

      begin
        GD.Draw_Rectangle (Pixmap, ST.Get_White (DA.Get_Style (Drawing_Area)),
                           True, 0, 0, 200, 75);

        if Coordinate.Dec_Of (Loc) >= -90.0 and Coordinate.Dec_Of (Loc) <= 90.0 then
          Draw_Text (40, 40, "Dec:" & Unicode_Of (Coordinate.Image_Of (Coordinate.Dec_Of (Loc))));
          Draw_Text (40, 55, "Ra :" & Coordinate.Image_Of (Coordinate.Ra_Of (Loc)));
          Draw_Text (40, 70, Star.Image_Of (Star.Hr_Of(Loc)) & " " & Star.Name_Of (Loc));
        end if;
        DA.Draw (Widget);
      end;
    end if;
    return True;
  end Motion_Notify_Event;


  ---------
  -- Bye --
  ---------

  procedure Bye (Window : access Gtk.Window.Gtk_Window_Record'class) is
     pragma Warnings (Off, Window);
  begin
     Gtk.Main.Main_Quit;
  end Bye;

  Vbox : BO.Gtk_Box;
  Border_Width : constant := 5;
  Window_Size  : constant := Size + 2 * Border_Width;

  use type GE.Gdk_Event_Mask;

begin
  Gtk.Main.Init;
  Gtk.Rc.Parse ("testgtkrc");
  WI.Gtk_New (Window, EN.Window_Toplevel);
  WI.Set_Title (Window, "Stars");
  WI.Set_Border_Width (Window, Border_Width => Border_Width);
  WI.Resize (Window, Window_Size, Window_Size);

  Destroyed.Connect (Window, "destroy", Destroyed.To_Marshaller (Bye'access));

  BO.Gtk_New_Vbox (Vbox, Homogeneous => False, Spacing => 0);
  WI.Add (Window, Vbox);
  BO.Show (Vbox);

  DA.Gtk_New (Drawing_Area);
  BO.Pack_Start (In_Box => Vbox, Child => Drawing_Area);
  DA.Show (Drawing_Area);
  DA.Unrealize (Drawing_Area);

  DA.Set_Events (Drawing_Area, GE.Exposure_Mask or GE.Leave_Notify_Mask or
                 GE.Button_Press_Mask or GE.Pointer_Motion_Mask or
                 GE.Pointer_Motion_Hint_Mask);

  Configured.Connect (Widget => Drawing_Area,
                      Name   => "expose_event",
                      Marsh  => Configured.To_Marshaller (Expose_Event'access));

  Configured.Connect (Widget => Drawing_Area,
                      Name   => "motion_notify_event",
                      Marsh  => Configured.To_Marshaller (Motion_Notify_Event'access));

  Configured.Connect (Widget => Drawing_Area,
                      Name   => "configure_event",
                      Marsh  => Configured.To_Marshaller (Configure_Event'access));

  Configured.Connect (Widget => Drawing_Area,
                      Name   => "button_press_event",
                      Marsh  => Configured.To_Marshaller (Button_Press_Event'access));

  GF.Load (Font, "-*-8514oem-medium-o-normal--14-140-75-75-p-78-iso8859-1");

  WI.Show_All (Window);
  Gtk.Main.Main;

end Stars;

