-- *********************************************************************************************************************
-- *                           (c) 2024 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

with Gui.Enumeration_Menu_Of;
with Lexicon;
with Sky.Catalog;
with Traces;

package body Name.Catalog is

  package Log is new Traces ("Name.Catalog");

  User_Signal_Define : Define_Signal;


  package Menu is new Gui.Enumeration_Menu_Of (Sky.Catalog_Id, Gui.Radio, Sky.Catalog.Image_Of);

  procedure Handler (The_Catalog : Sky.Catalog_Id) is
  begin
    Log.Write ("Id: " & The_Catalog'img);
    Define (The_Catalog);
    User_Signal_Define.all;
  exception
  when others =>
    Log.Error ("Handler");
  end Handler;


  procedure Create_Menu (Signal_Define : Define_Signal) is
  begin
    User_Signal_Define := Signal_Define;
    Menu.Create (Lexicon.Image_Of (Lexicon.Catalog), Handler'access);
    Handler (Sky.Favorites);
  end Create_Menu;

end Name.Catalog;
