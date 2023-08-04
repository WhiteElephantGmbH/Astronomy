-- *********************************************************************************************************************
-- *                       (c) 2017 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

package Lexicon is

  type Word is (Albireo,
                Aldebaran,
                All_Objects,
                Altair,
                Andromeda_Galaxie,
                Arkturus,
                Betelgeuse,
                Camera,
                Catalog,
                Clusters,
                Deneb,
                East,
                Eskimo_Nebula,
                Fans,
                Favorites,
                Finish,
                Galaxies,
                Hubbles_Nebula,
                In_Time,
                Jupiter,
                Mars,
                Mercury,
                Mizar,
                Moon,
                Multiple_Stars,
                Neos,
                Nebulas,
                Neptune,
                North,
                Ocular,
                Off,
                On,
                Open_Clusters,
                Optic,
                Orion_Nebula,
                Park_Position,
                Pluto,
                Polaris,
                Pollux,
                Procyon,
                Qr_Code,
                Quasars,
                Regulus,
                Rigel,
                Ring_Nebula,
                Road_Sign,
                Saturn,
                Saturn_Nebula,
                Selection,
                Sirius,
                Solar_System,
                South,
                Stars,
                Start,
                Sun,
                Target,
                Uranus,
                Vega,
                Veil_Nebula,
                Venus,
                Visible,
                West,
                Whirlpool_Galaxy,
                Wild_Duck_Cluster);

  function Word_Of (Name : String) return Word;
    Not_Found : exception;

  function Image_Of (Item : Word) return String;

  function Found (Name     : String;
                  For_Word : Word) return Boolean;

end Lexicon;
