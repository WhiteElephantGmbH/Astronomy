-- *********************************************************************************************************************
-- *                       (c) 2007 .. 2018 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada_95.File;
with Ada_95.Source;
with Ada_95.Token.Data;

package Ada_95.Library is

  subtype Handle is Token.Data.Unit_Handle;

  subtype Element is Token.Data.Library_Element;

  subtype Element_Cursor is Token.Data.Library_Tree.Cursor;

  procedure Start;

  function Added (From : Source.Object'class) return Handle;

  function Unit_Of (Item : File.Unit_Name) return Handle;

  function Spec_Of (Item : File.Unit_Name) return Handle;

  function Body_Of (Unit : Handle) return Handle;

  function Standard_Unit return Handle;

  function Ada_Exceptions_Unit return Handle;

  function Ada_Tags_Unit return Handle;

  function System_Unit return Handle;

  function System_Machine_Code_Unit return Handle;

  function Actual_Unit return Handle with Inline;

  generic
    with procedure Visit (Item : Element_Cursor);
  procedure Iterator;

  generic
    with procedure Handling_For (Item : Handle);
  procedure Iterator_With (Start_Unit : Handle;
                           Visit_Body : Boolean);

  procedure Finalize;

end Ada_95.Library;
