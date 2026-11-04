-- *********************************************************************************************************************
-- *                           (c) 2008 .. 2024 by Soudronic AG, Bergdietikon, Switzerland                             *
-- *                      Developed by White Elephant GmbH, Switzerland (www.white-elephant.ch)                        *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

package body Server is

  function Is_In_Project (Name : String) return Boolean is
    pragma Unreferenced (Name);
  begin
    return False;
  end Is_In_Project;


  function Project_Opened (Name : String) return Boolean is
    pragma Unreferenced (Name);
  begin
    return False;
  end Project_Opened;


  procedure Close_Project is
  begin
    null;
  end Close_Project;


  function Edge_Column return Server.Column_Position is
  begin
    return Server.Column_Position'last;
  end Edge_Column;


  function Known_Extensions return String is
  begin
    return "|ads|adb|";
  end Known_Extensions;


  function Case_Updates return Case_Data is
  begin
    return No_Case_Data;
  end Case_Updates;


  function Updates_For (The_Filename : String;
                        First_Line   : Line_Number;
                        Last_Line    : Line_Number;
                        Content      : String) return Tokens is

    pragma Unreferenced (The_Filename);
    pragma Unreferenced (First_Line);
    pragma Unreferenced (Last_Line);
    pragma Unreferenced (Content);
  begin
    return No_Tokens;
  end Updates_For;


  function Referenced (The_Filename : String;
                       At_Column    : Column_Range;
                       At_Line      : Line_Number;
                       Content      : String) return Boolean is
    pragma Unreferenced (The_Filename);
    pragma Unreferenced (At_Column);
    pragma Unreferenced (At_Line);
    pragma Unreferenced (Content);
  begin
    return False;
  end Referenced;


  function Usage (The_Filename : String;
                  At_Column    : Column_Range;
                  At_Line      : Line_Number;
                  Content      : String) return Reference_Data is
    pragma Unreferenced (The_Filename, At_Column, At_Line, Content);
  begin
    return No_Reference_Data;
  end Usage;


  function Usage (The_Filename : String;
                  At_Column    : Column_Range;
                  At_Line      : Line_Number;
                  Content      : String) return References is
  begin
    raise Program_Error;
    return null;
  end Usage;


  function Unused return Reference_Data is
  begin
    return No_Reference_Data;
  end Unused;


  function Unused return References is
  begin
    raise Program_Error;
    return null;
  end Unused;


  procedure Promote (Name : String;
                     Kind : Promotion_Kind := Normal) is
  begin
    null;
  end Promote;


  function Has_Promotion_Message return Boolean is
  begin
    return False;
  end Has_Promotion_Message;


  function Has_Promotion_Error return Boolean is
  begin
    return False;
  end Has_Promotion_Error;


  function Message return String is
  begin
    return "";
  end Message;


  function Filename return String is
  begin
    return "";
  end Filename;


  function Line return Line_Number is
  begin
    return Line_Number'first;
  end Line;


  function Column return Column_Range is
  begin
    return Column_Range'first;
  end Column;


  function Names_Of (Item : Text.List) return String is
  begin
    return Item.To_Data (Names_Separator);
  end Names_Of;

end Server;
