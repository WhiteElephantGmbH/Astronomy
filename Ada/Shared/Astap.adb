-- *********************************************************************************************************************
-- *                               (c) 2021 by White Elephant GmbH, Schaffhausen, Switzerland                          *
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

with Os.Process;
with Strings;
with Traces;

package body Astap is

  package Log is new Traces ("Astap");


  procedure Solve (Filename :     String;
                   Height   :     Degrees;
                   Ra       : out Degrees;
                   Dec      : out Degrees) is
    Parameters : constant String := "-f " & Filename & " -ra 0.0 -spd 90.0 -fov" & Height'image & " -r 180";
  begin
    Log.Write (Parameters);
    declare
      Output : constant String := Os.Process.Execution_Of (Executable => "./astap",
                                                           Parameters => Parameters);
      --Output : constant String :=
      --  "Creating monochromatic x 2 binning image for solving/star alignment." &
      --  "Will use mean value 1055 as background rather then most common value 469" &
      --  "516 stars, 376 quads selected in the image. 774 database stars, 564 database " &
      --  "quads required for the square search field of 4.5°. Search window at 100% based on " &
      --  "the number of quads. Step size at 100% of image height Using star database H18 " &
      --  " 58 of 58 quads selected matching within 0.01 tolerance.  Solution[""] " &
      --  "x:=4.401762*x+ 0.115247*y+ -5558.009504,  y:=-0.117058*x+ 4.402542*y+ -7875.411421 " &
      --  "Solution found: 12: 34 54.2 +87° 20 51 Solved in 3.2 sec. Δ was 79.5°. No mount info." &
      --  " Used stars up to magnitude: 11.4";

      Solution  : constant String := "Solution found:";
      The_Index : Natural;

      function Next_Number return String is
        The_First  : Natural;
      begin
        while not (Output(The_Index) in '0'..'9') loop
          The_Index := The_Index + 1;
        end loop;
        The_First := The_Index;
        while Output(The_Index) in '0'..'9' loop
          The_Index := The_Index + 1;
        end loop;
        if Output(The_First - 1) = '-' then
          The_First := The_First - 1;
        end if;
        return Output(The_First .. The_Index - 1);
      end Next_Number;

    begin
      The_Index := Strings.Location_Of (Solution, Output);
      if The_Index = Strings.Not_Found then
        raise Not_Solved;
      end if;
      The_Index := The_Index + Solution'length;
      declare
        Hrs : constant String := Next_Number;
        Min : constant String := Next_Number;
        S   : constant String := Next_Number;
        Sec : constant String := S & '.' & Next_Number;
      begin
        Ra := (Degrees'value(Hrs) + Degrees'value(Min) / 60.0 + Degrees'value(Sec) / 3600.0) * 15.0;
        if Hrs(Hrs'first) = '-' then
          raise Not_Solved;
        end if;
      exception
      when others =>
        Log.Error ("Unexpected RA <" & Hrs & ' ' & Min & ' ' & Sec & '>');
        raise Not_Solved;
      end;
      declare
        Deg : constant String := Next_Number;
        Min : constant String := Next_Number;
        Sec : constant String := Next_Number;
      begin
        Dec := abs (Degrees'value(Deg)) + Degrees'value(Min) / 60.0 + Degrees'value(Sec) / 3600.0;
        if Deg(Deg'first) = '-' then
          Dec := - Dec;
        end if;
      exception
      when others =>
        Log.Error ("Unexpected DEC <" & Deg & ' ' & Min & ' ' & Sec & '>');
        raise Not_Solved;
      end;
    end;
  exception
  when others =>
    raise Not_Solved;
  end Solve;

end Astap;
