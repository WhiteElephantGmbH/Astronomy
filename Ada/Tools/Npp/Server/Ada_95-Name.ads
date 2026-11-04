-- *********************************************************************************************************************
-- *                       (c) 2007 .. 2020 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada_95.Limits;
with Ada_95.Lexical;
with Ada_95.Word;
with Memory;

package Ada_95.Name is

  ----------------
  -- Name Handling  (Storage: Global_Pool)
  ----------------

  Max_Length : constant := Limits.Maximum_Number_Of_Words;

  type Item;

  type Handle is access Item;
  for Handle'storage_pool use Memory.Global_Pool.all;

  function "=" (Left, Right : Handle) return Boolean with Inline;

  function "<" (Left, Right : Handle) return Boolean with Inline;

  function Comparison (Left, Right : Handle) return Result;

  function Is_Equivalent (Left, Right : Handle) return Boolean;

  function Is_Capitalized_Or_Upper (The_Name : Handle) return Boolean;

  function Is_Lower (The_Name : Handle) return Boolean;

  type Word_List is array (Positive range <>) of Word.Handle;

  type Item (Length : Positive) is record
    Key  : Lexical.Name_Key;
    List : Word_List (1 .. Length);
  end record;

  procedure Add (Part : String);

  procedure Complete_Compound;

  procedure Clear;

  procedure Complete;

  function Handle_Of (Image : String) return Handle;

  function Image_Of (The_Name : Handle) return String;

  function Length_Of (The_Name : Handle) return Natural;

  procedure Finalize;


  ---------------------
  -- Name List Handling  (Storage: Global_Pool)
  ---------------------

  Max_List_Length : constant := 40;

  type List is array (Positive range <>) of Handle;

  subtype List_Elements is List (1 .. Max_List_Length);

  type List_Handle is access all List;
  for List_Handle'storage_pool use Memory.Global_Pool.all;

  function Id_Of (Image     : String;
                  Separator : Character := '.') return List;

  function Image_Of (The_Name  : List;
                     Separator : Character := '.') return String;

  function "=" (Left, Right : List) return Boolean;

  function "<" (Left, Right : List) return Boolean;

  function Comparison (Left, Right : List) return Result;

end Ada_95.Name;
