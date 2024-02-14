-- *********************************************************************************************************************
-- *                       (c) 2023 .. 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Discrete_Set;
with Sky;

private package Alignment.Stars is

  type Id is (
    Albireo,
    Aldebaran,
    Alderamin,
    Algenib,
    Alkaid,
    Alpha_Cam,
    Alpha_Fornacis,
    Alpha_Lyncis,
    Alphard,
    Alpheratz,
    Altair,
    Alula_Borealis,
    Antares,
    Arcturus,
    Beta_Aqr,
    Betelgeuse,
    Capella,
    Caph,
    Castor,
    Cor_Caroli,
    Deneb,
    Denebola,
    Diphda,
    Dubhe,
    Eltanin,
    Enif,
    Gamm_Cas,
    Gemma,
    Gienah_Ghurab,
    Hamal,
    Kochab,
    Lambda_Aqr,
    Menkar,
    Menkent,
    Mirach,
    Mirfak,
    Muscida,
    Nu_Ophiuchi,
    Omega_Cap,
    Pi_Herculis,
    Polaris,
    Pollux,
    Procyon,
    Rasalhague,
    Regulus,
    Rho_Puppis,
    Rigel,
    Scheat,
    Sirius,
    Spica,
    Unukalhai,
    Vega,
    Vindemiatrix,
    Zaurak,
    Zeta_Herculis,
    Zeta_Persei,
    Zuben_El_Genubi);

  subtype Count is Natural range 0 .. Id'pos(Id'last) - Id'pos(Id'first) + 1;

  package Ids is new Discrete_Set (Id);

  subtype Set is Ids.Set;

  function Object_Of (The_Id : Id) return Sky.Object;

  function Image_Of (The_Id : Id) return String;

end Alignment.Stars;
