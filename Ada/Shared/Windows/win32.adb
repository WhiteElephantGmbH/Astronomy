-------------------------------------------------------------------------------
--
--  THIS FILE AND ANY ASSOCIATED DOCUMENTATION IS FURNISHED "AS IS"
--  WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING
--  BUT NOT LIMITED TO THE IMPLIED WARRANTIES OF MERCHANTABILITY
--  AND/OR FITNESS FOR A PARTICULAR PURPOSE.  The user assumes the
--  entire risk as to the accuracy and the use of this file.
--
--  Copyright (C) Intermetrics, Inc. 1995
--  Royalty-free, unlimited, worldwide, non-exclusive use, modification,
--  reproduction and further distribution of this file is permitted.
--
--  This file is now maintained and made available by AdaCore under
--  the same terms.
--
--  Copyright (C) 2000-2010, AdaCore
--
--  Modified 2016 by White Elephant GmbH, Schaffhausen, Switzerland
-------------------------------------------------------------------------------

package body Win32 is

   function Cat (Left, Right : String) return String is
      Terminator : constant Character := Ascii.Nul;
   begin
      if Left (Left'last) = Terminator then
         if Right (Right'last) = Terminator then
            return Left (Left'first .. Left'last - 1) & Right;
         else
            return Left (Left'first .. Left'last - 1) & Right & Terminator;
         end if;
      else
         if Right (Right'last) = Terminator then
            return Left & Right;
         else
            return Left & Right & Terminator;
         end if;
      end if;
   end Cat;

   function Cat (Left, Right : Wide_String) return Wide_String is
      Terminator : constant Wide_Character := Wide_Character'first;
   begin
      if Left (Left'last) = Terminator then
         if Right (Right'last) = Terminator then
            return Left (Left'first .. Left'last - 1) & Right;
         else
            return Left (Left'first .. Left'last - 1) & Right & Terminator;
         end if;
      else
         if Right (Right'last) = Terminator then
            return Left & Right;
         else
            return Left & Right & Terminator;
         end if;
      end if;
   end Cat;

   function Cat (Left, Right : CHAR_Array) return CHAR_Array is
      use type Win32.CHAR;
   begin
      if Left (Left'last) = Nul then
         if Right (Right'last) = Nul then
            return Left (Left'first .. Left'last - 1) & Right;
         else
            return Left (Left'first .. Left'last - 1) & Right & Nul;
         end if;
      else
         if Right (Right'last) = Nul then
            return Left & Right;
         else
            return Left & Right & Nul;
         end if;
      end if;
   end Cat;

   function Cat (Left, Right : WCHAR_Array) return WCHAR_Array is
      use type Win32.WCHAR;
   begin
      if Left (Left'last) = Wide_Nul then
         if Right (Right'last) = Wide_Nul then
            return Left (Left'first .. Left'last - 1) & Right;
         else
            return Left (Left'first .. Left'last - 1) & Right & Wide_Nul;
         end if;
      else
         if Right (Right'last) = Wide_Nul then
            return Left & Right;
         else
            return Left & Right & Wide_Nul;
         end if;
      end if;
   end Cat;

   function Addr (S : String) return PSTR is
      function To_PSTR is new Ada.Unchecked_Conversion (
         System.Address,
         PSTR);
   begin
      return To_PSTR (S (S'first)'address);
   end Addr;

   function Addr (S : String) return PCSTR is
   begin
      return To_PCSTR (S (S'first)'address);
   end Addr;

   function Addr (S : Wide_String) return PWSTR is
   begin
      return To_PWSTR (S (S'first)'address);
   end Addr;

   function Addr (S : Wide_String) return PCWSTR is
   begin
      return To_PCWSTR (S (S'first)'address);
   end Addr;

   function Addr (S : CHAR_Array) return PSTR is
      function To_PSTR is new Ada.Unchecked_Conversion (
         System.Address,
         PSTR);
   begin
      return To_PSTR (S (S'first)'address);
   end Addr;

   function Addr (S : CHAR_Array) return PCSTR is
   begin
      return To_PCSTR (S (S'first)'address);
   end Addr;

   function Addr (S : WCHAR_Array) return PWSTR is
   begin
      return To_PWSTR (S (S'first)'address);
   end Addr;

   function Addr (S : WCHAR_Array) return PCWSTR is
   begin
      return To_PCWSTR (S (S'first)'address);
   end Addr;

   function To_Chars_Ptr (STR : PSTR) return Interfaces.C.Strings.chars_ptr is
      function UC1 is new Ada.Unchecked_Conversion (
         PSTR,
         Interfaces.C.Strings.chars_ptr);
   begin
      return UC1 (STR);
   end To_Chars_Ptr;

   function To_Chars_Ptr
     (STR  : PCSTR)
      return Interfaces.C.Strings.chars_ptr
   is
      function UC2 is new Ada.Unchecked_Conversion (
         PCSTR,
         Interfaces.C.Strings.chars_ptr);
   begin
      return UC2 (STR);
   end To_Chars_Ptr;

   function To_PSTR (CP : Interfaces.C.Strings.chars_ptr) return PSTR is
      function UC3 is new Ada.Unchecked_Conversion (
         Interfaces.C.Strings.chars_ptr,
         PSTR);
   begin
      return UC3 (CP);
   end To_PSTR;

   function To_PCSTR (CP : Interfaces.C.Strings.chars_ptr) return PCSTR is
      function UC4 is new Ada.Unchecked_Conversion (
         Interfaces.C.Strings.chars_ptr,
         PCSTR);
   begin
      return UC4 (CP);
   end To_PCSTR;

   function To_C (S : CHAR_Array) return Interfaces.C.char_array is
      Res : Interfaces.C.char_array (
         Interfaces.C.size_t (S'first) .. Interfaces.C.size_t (S'last));
   begin
      Res := Interfaces.C.char_array (S);
      return Res;
   end To_C;

   function To_Win (S : Interfaces.C.char_array) return CHAR_Array is
      Low  : constant Integer := Integer (S'first);
      High : constant Integer := Integer (S'last);
      Res  : CHAR_Array (Low .. High);
   begin
      Res := CHAR_Array (S);
      return Res;
   end To_Win;

   function To_Win (S : Interfaces.C.wchar_array) return WCHAR_Array is
      Low  : constant Integer := Integer (S'first);
      High : constant Integer := Integer (S'last);
      Res  : WCHAR_Array (Low .. High);
   begin
      Res := WCHAR_Array (S);
      return Res;
   end To_Win;

end Win32;
