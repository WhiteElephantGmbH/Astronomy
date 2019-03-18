-- *********************************************************************************************************************
-- *                       (c) 2017 .. 2019 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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
                Favorites,
                Galaxies,
                Hubbles_Nebula,
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
                Open_Clusters,
                Orion_Nebula,
                Park_Position,
                Pluto,
                Polaris,
                Pollux,
                Procyon,
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
                Sun,
                Target,
                Uranus,
                Vega,
                Veil_Nebula,
                Venus,
                West,
                Whirlpool_Galaxy,
                Wild_Duck_Cluster);

  function Word_Of (Name : String) return Word;
    Not_Found : exception;

  function Image_Of (Item : Word) return String;

  function Found (Name     : String;
                  For_Word : Word) return Boolean;

end Lexicon;
