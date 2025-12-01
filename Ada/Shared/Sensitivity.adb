-- *********************************************************************************************************************
-- *                           (c) 2025 by White Elephant GmbH, Schaffhausen, Switzerland                              *
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

with Text;

package body Sensitivity is

  function From_Camera return Item is ((Is_From_Camera => True,
                                        others         => <>));

  function Value (Iso_Value : Iso) return Item is
  begin
    return (Is_From_Camera => False,
            Value          => Iso_Value);
  end Value;


  function Value (Image : String) return Item is
  begin
    return Value (Iso'value(Image));
  end Value;


  function Is_From_Camera (The_Item : Item) return Boolean is (The_Item.Is_From_Camera);

  function Value (The_Item : Item) return Iso is (The_Item.Value);


  procedure Put_Image (S : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'class;
                       V :        Item) is
  begin
    if V.Is_From_Camera then
      S.Put ("[]");
    else
      S.Put (Text.Trimmed (V.Value'image));
    end if;
  end Put_Image;

end Sensitivity;
