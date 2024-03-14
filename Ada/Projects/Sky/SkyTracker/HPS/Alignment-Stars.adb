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

with Lexicon;
with Sky.Catalog;
with Text;

package body Alignment.Stars is

  function Object_Of (The_Id : Id) return Sky.Object is
    use all type Sky.Constellation;
    use all type Sky.Greek_Letter;
  begin
    case The_Id is
    when Alpha_Cam =>
      return Sky.Catalog.Object_Of (Alf, C_Cam);
    when Alpha_Lyncis =>
      return Sky.Catalog.Object_Of (Alf, C_Lyn);
    when Gamma_Cas =>
      return Sky.Catalog.Object_Of (Gam, C_Cas);
    when Lamda_Aqr =>
      return Sky.Catalog.Object_Of (Lam, C_Aqr);
    when Nu_Ophiuchi =>
      return Sky.Catalog.Object_Of (Nu, C_Oph);
    when Omega_Cap =>
      return Sky.Catalog.Object_Of (Ome, C_Cap);
    when Pi_Herculis =>
      return Sky.Catalog.Object_Of (Pi, C_Her);
    when Zeta_Herculis =>
      return Sky.Catalog.Object_Of (Zet, C_Her);
    when Zeta_Persei =>
      return Sky.Catalog.Object_Of (Zet, C_Per);
    when others =>
      return Sky.Catalog.Object_Of (Lexicon.Word'value(The_Id'image));
    end case;
  end Object_Of;


  function Image_Of (The_Id : Id) return String is
  begin
    return Text.Legible_Of (The_Id'image);
  end Image_Of;

end Alignment.Stars;
