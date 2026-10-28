-- *********************************************************************************************************************
-- *                       (c) 2018 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_Astronomy;

package Os.Shell is

  procedure Execute (File       : String;
                     Operation  : String       := "Open";
                     Directory  : String       := "";
                     Parameters : String       := "";
                     Show_Cmd   : Show_Command := Normal) is
  begin                   
    raise Program_Error; -- not implemented
  end Execute;


end Os.Shell;
