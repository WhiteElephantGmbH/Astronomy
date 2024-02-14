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

with Ada.Unchecked_Conversion;
with Strings;

package body Database is

  function Star_Id_Of (Image : String) return Id is

    function Convert is new Ada.Unchecked_Conversion (Star_Info, Id);

    Id_Parts : constant Strings.Item := Strings.Item_Of (Image, ' ');
    The_Info : Star_Info; -- default unknown

  begin -- Star_Id_Of
    if Id_Parts.Count = 3 and then Id_Parts(1) = Star_Image then
      declare
        P1 : constant String := Id_Parts(2);
        P2 : constant String := Id_Parts(3);
      begin
        if P1(P1'first) in '0' .. '9' then
          The_Info.Kind := Numeric;
          The_Info.Count := Star_Number'value(P1);
        else
          declare
            The_Last : Natural := P1'first;
          begin
            for The_Index in Greek_Alphabet'range loop
              declare
                Letter   : constant String  := Greek_Alphabet(The_Index);
                Location : constant Natural := Strings.Location_Of (Letter, P1);
              begin
                if Location = P1'first then
                  The_Last := P1'first + Letter'length - 1; -- always > P1'first
                  The_Info.Count := Star_Number(The_Index);
                  The_Info.Kind := Greek;
                  exit;
                end if;
              end;
            end loop;
            if The_Last = P1'first then
              The_Last := P1'last;
              for The_Index in P1'range loop
                if not (Strings.Lowercase_Of (P1(The_Index)) in 'a' .. 'z') then
                  The_Last := The_Index - 1;
                  exit;
                end if;
              end loop;
              if The_Last = P1'first then
                The_Info.Kind := Alphabetic;
                The_Info.Count := Star_Number(Character'pos(P1(P1'first)) - Character'pos('A'));
              else
                The_Info.Kind := Greek;
                The_Info.Count := Star_Number(Greek_Letter'pos(Greek_Letter'value(P1(P1'first .. The_Last))));
              end if;
            end if;
            if The_Last < P1'last then
              declare
                First_Index : constant Positive := (if P1(The_Last + 1) = '.' then The_Last + 2 else The_Last + 1);
                Index       : constant String := P1(First_Index .. P1'last);
              begin
                if Index /= "" then
                  The_Info.Index := Star_Index'value(Index);
                end if;
              end;
            end if;
          end;
        end if;
        The_Info.C_Id := Constellation'value("C_" & P2);
      exception
      when others =>
        return Unknown;
      end;
    end if;
    return Convert (The_Info);
  end Star_Id_Of;

end Database;
