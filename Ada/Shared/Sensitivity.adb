-- *********************************************************************************************************************
-- *                       (c) 2025 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

  function Default return Item is ((others => <>));


  function Value (Iso_Value : Iso) return Item is
  begin
    return (Kind   => Is_Iso,
            Value  => Natural(Iso_Value),
            others => <>);
  end Value;


  function Value (Gain_Value   : Gain;
                  Offset_Value : Offset := 0) return Item is
  begin
    return (Kind        => Is_Gain_And_Offset,
            Value       => Natural(Gain_Value),
            With_Offset => Offset_Value);
  end Value;


  function Value (Image : String) return Item is
    Items : constant Text.Strings := Text.Strings_Of (Image, Separator => ',');
  begin
    case Items.Count is
    when 1 =>
      if Items(1) = "[]" then
        return Default;
      else
        return Value (Iso'value(Items(1)));
      end if;
    when 2 =>
      declare
        G : constant String := Items(1);
        O : constant String := Items(2);
      begin
        if G(G'first) = '[' and O(O'last) = ']' then
          return Value (Gain'value(G(G'first + 1 .. G'last)),
                        Offset'value(O(O'first .. O'last - 1)));
        end if;
      end;
    when others =>
      null;
    end case;
    raise Constraint_Error;
  end Value;


  function Is_Default (The_Item : Item) return Boolean is (The_Item.Kind = Is_Default);

  function Is_Iso_Or_Default (The_Item : Item) return Boolean is (The_Item.Kind /= Is_Gain_And_Offset);

  function Is_Gain_And_Offset_Or_Default (The_Item : Item) return Boolean is (The_Item.Kind /= Is_Iso);

  function Value (The_Item : Item) return Iso is (Iso(The_Item.Value));

  function Value (The_Item : Item) return Gain is (Gain(The_Item.Value));

  function Value (The_Item : Item) return Offset is (The_Item.With_Offset);


  procedure Put_Image (S : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'class;
                       V :        Item) is
  begin
    case V.Kind is
    when Is_Default =>
      S.Put ("[]");
    when Is_Iso =>
      S.Put (Text.Trimmed (V.Value'image));
    when Is_Gain_And_Offset =>
      S.Put ("[" & Text.Trimmed (V.Value'image) & ", " & Text.Trimmed (V.With_Offset'image) & "]");
    end case;
  exception
  when others =>
    null;
  end Put_Image;

end Sensitivity;
