-- *********************************************************************************************************************
-- *                       (c) 2021 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

package Project.Resource is

  Extension : constant String := ".rc";

  function Filename return String;

  function Common_Object_File return String;

  procedure Generate;

end Project.Resource;
