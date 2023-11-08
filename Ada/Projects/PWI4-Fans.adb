-- *********************************************************************************************************************
-- *                       (c) 2019 .. 2023 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

package body PWI4.Fans is

  procedure Execute (Command_Name : String) is
  begin
    Execute (Device  => "fans",
             Command => Command_Name);
  end Execute;


  Is_On : Boolean := False;

  procedure Turn_On is
  begin
    Execute ("on");
    Is_On := True;
  end Turn_On;


  procedure Turn_Off is
  begin
    Execute ("off");
    Is_On := True;
  end Turn_Off;


  function Turned_On return Boolean is
  begin
    return Is_On;
  end Turned_On;


  function Exists return Boolean is
  begin
    if Is_On then
      Turn_On;
    else
      Turn_Off;
    end if;
    return True;
  exception
  when others =>
    return False;
  end Exists;

end PWI4.Fans;
