-- *********************************************************************************************************************
-- *                               (c) 2024 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Error;
with Section;
with Stellarium;
with Text;

package body Language.Parameter is

  Localization_Id : constant String := "Localization";
  Language_Key    : constant String := Id;


  procedure Define (Handle : Configuration.File_Handle) is
  begin
    Section.Set (Configuration.Handle_For (Handle, Localization_Id));
    declare
      Image : constant String := Section.String_Value_Of (Language_Key);
    begin
      if Image = "" then
        Error.Raise_With ("No language defined");
      end if;
      begin
        Define (Kind'value(Image));
      exception
      when others =>
        Error.Raise_With ("Incorrect " & Language_Key & ": <" & Image & ">");
      end;
    end;
  end Define;


  procedure Defaults (Put : access procedure (Item : String)) is
  begin
    Put (Text.Bom_8 & "[" & Localization_Id & "]");
    Put (Language_Key & " = " & Text.Legible_Of (Stellarium.Language'img));
  end Defaults;

end Language.Parameter;
