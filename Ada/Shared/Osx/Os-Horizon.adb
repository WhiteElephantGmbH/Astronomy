-- *********************************************************************************************************************
-- *                           (c) 2022 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Os.Process;

package body Os.Horizon is

  function Result_Of_Get_With (Item : Arguments) return String is
  begin
    return Os.Process.Execution_Of (Executable =>"/Applications/Horizon_Get",
                                    Parameters => Strings.Data_Of (Item, Separator => " "));
  end Result_Of_Get_With;

end Os.Horizon;
