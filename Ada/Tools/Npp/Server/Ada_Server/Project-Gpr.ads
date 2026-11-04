-- *********************************************************************************************************************
-- *                       (c) 2021 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

package Project.Gpr is

  File_Extension : constant String := ".gpr";

  function File_Is_Generated return Boolean;

  function Filename return String;

  procedure Define_Information_For (Tools_Directory : String;
                                    Libraries       : Text.List);

  function Contains_For (Tools_Directory : String;
                         The_Library     : String) return Boolean;

  function Library_Path return Text.String;

end Project.Gpr;
