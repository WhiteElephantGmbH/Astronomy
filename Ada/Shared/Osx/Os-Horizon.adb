-- *********************************************************************************************************************
-- *                           (c) 2022 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Os.Process;
with Log;

package body Os.Horizon is

  function Result_Of_Get_With (Item : Arguments) return String is

    Args : constant String := Strings.Data_Of (Item, Separator => " ");
    
  begin
    Log.Write ("ARG<<<" & Args & ">>>");
    return Os.Process.Execution_Of (Executable =>"/Applications/Horizon_Get",
                                    Parameters => Args);
  end Result_Of_Get_With;

end Os.Horizon;