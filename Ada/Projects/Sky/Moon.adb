-- *********************************************************************************************************************
-- *                       (c) 2012 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Ada.Calendar;
with Astro;
with Database.Moon;
with Lexicon;
with Site;
with Sky.Data;
with Traces;

package body Moon is

  package Log is new Traces (Id);

  use Astro;
  use MATLIB;

  package Data is

    Undefined : constant REAL := REAL'first;

    procedure Evaluate (UT : Time.Ut);

    function Topocentric_Dec return REAL;

    function Topocentric_RA return REAL;

    function Liberation_Longitude return REAL;

    function Liberation_Latitude return REAL;

    function Topocentric_Moon_Distance return REAL; -- in km

    function Position_Angle return REAL;

    function Subsolar_Longitude return REAL;

    function Colongitude return REAL;

  end Data;


  package body Data is

    use SPHLIB;

    ER : constant REAL := Earth_Equatorial_Radius;

    RA, Dec, R, L, B, P, B0, C0: REAL := Undefined;

    procedure Evaluate (UT : Time.Ut) is

      T, Bet, Lam, Rc_Phi, Rs_Phi, LMST: REAL;

      Geo: VECTOR; -- geozentric moon vector
      Loc: VECTOR; -- Location vector
      Top: VECTOR; -- Topocentric Vector
      Tec: VECTOR; -- Topocentric ecliptical Vector

      use all type Angle.Value;

    begin -- Get_Moon_Parameters
      T := Time.Tet_Of (UT);

      MOOLIB.MOONEQU (T, RA, Dec, R);

      Geo := CART (R, Dec, RA);

      SPHLIB.SITE (PHI   => +Site.Latitude,
                   RCPHI => Rc_Phi,
                   RSPHI => Rs_Phi);

      LMST := +Time.Lmst_Of (UT);

      Loc := [X => Rc_Phi * CS (LMST),
              Y => Rc_Phi * SN (LMST),
              Z => Rs_Phi];

      Top := Geo - Loc;

      Tec := Top;
      EQUECL (T, Tec(X), Tec(Y), Tec(Z));

      POLAR (Top, R, Dec, RA);
      POLAR (Tec, R, Bet, Lam);
      R := R * ER;

      MOOLIB.MOONPAR (T    => T,
                      RA   => RA,
                      LAM  => Lam,
                      BET  => Bet,
                      DEL  => R,
                      L    => L,
                      B    => B,
                      P    => P,
                      B0   => B0,
                      C0   => C0);
    end Evaluate;

    function Topocentric_Dec return REAL is (Dec);

    function Topocentric_RA return REAL is (RA);

    function Topocentric_Moon_Distance return REAL is (R);

    function Liberation_Longitude return REAL is (L);

    function Liberation_Latitude return REAL is (B);

    function Position_Angle return REAL is (P);

    function Subsolar_Longitude return REAL is (B0);

    function Colongitude return REAL is (C0);

  end Data;


  MR : constant REAL := Moon_Radius;

  Minimum_Sun_Altitude : Angle.Degrees;
  Maximum_Sun_Altitude : Angle.Degrees;


  procedure Define (Min_Sun_Altitude : Angle.Degrees;
                    Max_Sun_Altitude : Angle.Degrees) is
    use type Angle.Value;
  begin
    Minimum_Sun_Altitude := Min_Sun_Altitude;
    Maximum_Sun_Altitude := Max_Sun_Altitude;
    Log.Write ("Sun Altitude - Minimum : " & Angle.Signed_Degrees_Image_Of (+Min_Sun_Altitude) &
                           " - Maximum : " & Angle.Signed_Degrees_Image_Of (+Max_Sun_Altitude));
  end Define;


  procedure Define (UT: Time.Ut) renames Data.Evaluate;


  function Direction_Of (Id               : Name.Id;
                         Check_Visibility : Boolean := True) return Space.Direction is

    function RA return REAL renames Data.Topocentric_RA;

    function Dec return REAL renames Data.Topocentric_Dec;

    function R return REAL renames Data.Topocentric_Moon_Distance;

    function L return REAL renames Data.Liberation_Longitude;

    function B return REAL renames Data.Liberation_Latitude;

    function B0 return REAL renames Data.Subsolar_Longitude;

    function C0 return REAL renames Data.Colongitude;

    function P return REAL renames Data.Position_Angle;

    procedure Rotate_Around_Y_Axis (Alpha, Beta : in out REAL;
                                    By          :        REAL) is
      CA : constant REAL := CS (Alpha);
      SA : constant REAL := SN (Alpha);
      CB : constant REAL := CS (Beta);
      SB : constant REAL := SN (Beta);
      CR : constant REAL := CS (By);
      SR : constant REAL := SN (By);
    begin
      Alpha := ATN2 (CB * SA, CA * CB * CR - SB * SR);
      Beta  := ASN (CR * SB + CA * CB * SR);
    end Rotate_Around_Y_Axis;

    Object : constant Sky.Object := Name.Object_Of (Id);
    use type Sky.Object;

    The_RA  : Angle.Degrees := RA;
    The_Dec : Angle.Degrees := Dec;

    Feature_Invisible : exception;


    procedure Evaluate_Feature_Location is

      Item      : constant Feature := Sky.Data.Moon_Feature_Of (Object);
      Info      : Database.Moon.Feature renames Database.Moon.List(Item);
      Longitude : constant REAL := REAL(Info.Longitude);
      Latitude  : constant REAL := REAL(Info.Latitude);
      Kind      : constant Feature_Type := Info.Kind;
      CL        : REAL := Longitude - L; -- corrected longitude
      CB        : REAL := Latitude;
      Delta_Ra  : REAL;
      Delta_Dec : REAL;

      procedure Check_Feature_Visibility is
        DM, MRS, SH : REAL;
        use all type Feature_Type;
      begin
        MOOLIB.MOONSUNH (ETA => Longitude,
                         TET => Latitude,
                         B0  => B0,
                         C0  => C0,
                         H   => SH);
        if SH < Minimum_Sun_Altitude or (if Kind in Mare | Oceanus then False else SH > Maximum_Sun_Altitude) then
          raise Feature_Invisible;
        end if;
        MRS := ASN (MR / R);
        DM := 90.0 - MRS;
        if abs CL > DM then
          raise Feature_Invisible;
        end if;
        if abs CB > DM then
          raise Feature_Invisible;
        end if;
      end Check_Feature_Visibility;

      procedure Evaluate_Deltas is
        X, Xo, Yo, Zo, Wo, M, T, Ome : REAL;
      begin
        CART (R     => MR,
              THETA => CB,
              PHI   => CL,
              X     => Xo,
              Y     => Yo,
              Z     => Zo);
        X := (R - Xo);
        T := Yo * Yo + Zo * Zo;
        M := SQRT (X * X + T);
        Wo := SQRT (T);
        Ome := ATN2 (Zo, Yo) + P;
        Delta_Dec := ASN (Wo * SN (Ome) / M);
        Delta_Ra := -ATN2 (Wo * CS (Ome), X); -- west positive
      end Evaluate_Deltas;

    begin -- Evaluate_Feature_Location
      Rotate_Around_Y_Axis (CL, CB, -B); -- rotate by liberation in latitude
      if Check_Visibility then
        Check_Feature_Visibility;
      end if;
      Evaluate_Deltas;
      The_RA := @ + Delta_Ra;
      The_Dec := @ + Delta_Dec;
    end Evaluate_Feature_Location;

  begin -- Direction_Of
    if RA = Data.Undefined then
      raise Program_Error;
    end if;
    if Object /= Sky.Undefined then
      Evaluate_Feature_Location;
    end if;
    return Space.Direction_Of (Dec => The_Dec,
                               Ra  => The_RA);
  exception
  when Feature_Invisible =>
    return Space.Unknown_Direction;
  end Direction_Of;


  function Direction_Of (UT : Time.Ut) return Space.Direction is
  begin
    return Direction_Of (Name.No_Id, UT);
  end Direction_Of;


  function Direction_Of (Id : Name.Id;
                         UT : Time.Ut) return Space.Direction is
  begin
    Define (UT);
    return Direction_Of (Id, Check_Visibility => False);
  end Direction_Of;


  function Image_Of (Item : Feature_Type) return String is
  begin
     return Lexicon.Image_Of (Lexicon.Word'value(Item'image));
  end Image_Of;


  function Has_Feature (Id   :     Name.Id;
                        Item : out Feature;
                        Kind : out Feature_Type) return Boolean is
    Object : constant Sky.Object := Name.Object_Of (Id);
    use type Sky.Object;
  begin
    if Object = Sky.Undefined then
      return False;
    end if;
    Item := Sky.Data.Moon_Feature_Of (Object);
    Kind := Database.Moon.List(Item).Kind;
    return True;
  end Has_Feature;


  function Feature_Kind_Of (Id : Name.Id) return String is
    Item : Feature;
    Kind : Feature_Type;
  begin
    if Has_Feature (Id, Item, Kind) then
      return Image_Of (Kind);
    end if;
    return "";
  end Feature_Kind_Of;


  procedure Get_New_Phase (Around    :     Time.Ut;
                           Before    : out Time.Ut;
                           After     : out Time.Ut) is
    Unused : Angle.Degrees;
  begin
    Get_New_Phase (Around    => Around,
                   Before    => Before,
                   After     => After,
                   Libration => Unused);
  end Get_New_Phase;


  procedure Get_New_Phase (Around    :     Time.Ut;
                           Before    : out Time.Ut;
                           After     : out Time.Ut;
                           Libration : out Angle.Degrees) is

    use TIMLIB;

    The_Year       : constant Integer := Ada.Calendar.Year (Time.Local_Of (Around));
    First_Lunation : constant Integer := Integer(REAL'truncation(MOOLIB.D1 * REAL(The_Year - 2000) / 100.0));

    The_Lunation : Integer := First_Lunation;
    T_NEW_MOON   : REAL;
    MJD_NEW_MOON : REAL;

    use type Time.Ut;
    use type Angle.Value;

  begin -- Get_New_Phase
    After := Time.Ut'last;
    loop
      T_NEW_MOON := (REAL(The_Lunation) - MOOLIB.D0) / MOOLIB.D1;
      for Unused in 1 .. 6 loop
        MOOLIB.IMPROVE (T_NEW_MOON, Libration);
      end loop;
      MJD_NEW_MOON := 36525.0 * T_NEW_MOON + 51544.5;
      Before := After;
      After := Time.Ut(MJD_NEW_MOON - MJD_OFFSET) * Time.One_Day;
      exit when After > Around;
      The_Lunation := @ + 1;
      if The_Lunation > First_Lunation + 14 then
        raise Program_Error;
      end if;
    end loop;
    Log.Write ("new phase - before " & Time.Image_Of (Before) &
                        " - after " & Time.Image_Of (After) &
                        " - libration " & Angle.Image_Of (+Libration, Show_Signed => True));
  end Get_New_Phase;


end Moon;
