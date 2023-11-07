-- *********************************************************************************************************************
-- *                           (c) 2023 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

with Strings;

package body Alignment.Stars is

    type Objects is array (Id) of Data.Object;

    The_Objects : constant Objects := [
      Albireo         => 7683,
      Aldebaran       => 1733,
      Alderamin       => 8426,
      Algenib         => 319,
      Alkaid          => 5462,
      Alpha_Cam       => 1818,
      Alpha_Fornacis  => 1240,
      Alpha_Lyncis    => 3976,
      Alphard         => 4019,
      Alpheratz       => 295,
      Altair          => 7822,
      Alula_Borealis  => 4648,
      Antares         => 6403,
      Arcturus        => 5611,
      Beta_Aqr        => 8496,
      Betelgeuse      => 2336,
      Capella         => 1984,
      Caph            => 301,
      Castor          => 3164,
      Cor_Caroli      => 5186,
      Deneb           => 8188,
      Denebola        => 4805,
      Diphda          => 465,
      Dubhe           => 4572,
      Eltanin         => 6972,
      Enif            => 8571,
      Gamm_Cas        => 541,
      Gemma           => 6063,
      Gienah_Ghurab   => 4933,
      Hamal           => 894,
      Kochab          => 5834,
      Lambda_Aqr      => 8961,
      Menkar          => 1188,
      Menkent         => 5559,
      Mirach          => 614,
      Mirfak          => 1294,
      Muscida         => 3596,
      Nu_Ophiuchi     => 6965,
      Omega_Cap       => 8244,
      Pi_Herculis     => 6686,
      Polaris         => 701,
      Pollux          => 3263,
      Procyon         => 3216,
      Rasalhague      => 6823,
      Regulus         => 4253,
      Rho_Puppis      => 3458,
      Rigel           => 1989,
      Scheat          => 9038,
      Sirius          => 2765,
      Spica           => 5327,
      Unukalhai       => 6124,
      Vega            => 7268,
      Vindemiatrix    => 5203,
      Zaurak          => 1507,
      Zeta_Herculis   => 6481,
      Zeta_Persei     => 1479,
      Zuben_El_Genubi => 5801];


  function Object_Of (The_Id : Id) return Data.Object is
  begin
     return The_Objects(The_Id);
  end Object_Of;


  function Image_Of (The_Id : Id) return String is
  begin
    return Strings.Legible_Of (The_Id'image);
  end Image_Of;

end Alignment.Stars;
