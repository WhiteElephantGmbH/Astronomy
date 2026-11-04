-- *********************************************************************************************************************
-- *                       (c) 2021 .. 2026 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Ada.Text_IO;
with Build;
with File;
with Log;
with Project;
with Text;

package body Build_Parser is

  function Evaluated return Boolean is

    End_Detected : exception;

    The_Line   : String(1..255);
    The_Index  : Natural := 0;
    Last_Index : Natural;

    The_File : Ada.Text_IO.File_Type;

    function Next_Token return String is

      function Next_Character return Character is
      begin
        loop
          if The_Index = 0 then
            if Ada.Text_IO.End_Of_File (The_File) then
              raise End_Detected;
            end if;
            Ada.Text_IO.Get_Line (The_File, The_Line, Last_Index);
            if Ada.Text_IO.End_Of_File (The_File) then
              raise End_Detected;
            end if;
            Last_Index := Last_Index + 1;
            The_Line(Last_Index) := ' ';
            The_Index := 1;
          end if;
          if The_Index < Last_Index and then The_Line(The_Index .. The_Index + 1) = "--" then
            The_Index := 0;
          elsif The_Index <= Last_Index then
            The_Index := The_Index + 1;
            return The_Line(The_Index - 1);
          else
            The_Index := 0;
          end if;
        end loop;
      end Next_Character;

      Start_Index : Natural;

    begin -- Next_Token
      loop
        case Next_Character is
        when ' ' | '(' | ',' | ';' =>
          null;
        when ')' =>
          return ")";
        when others =>
          Start_Index := The_Index - 1;
          exit;
        end case;
      end loop;
      loop
        case The_Line(Start_Index) is
        when ')' =>
          return ")";
        when '+' =>
          return "+";
        when '=' =>
          case Next_Character is
          when '>' =>
            return "=>";
          when others =>
            null;
          end case;
        when '"' =>
          loop
            case Next_Character is
            when '"' =>
              return The_Line(Start_Index + 1 .. The_Index - 2);
            when others =>
              null;
            end case;
          end loop;
        when others =>
          case Next_Character is
          when '+' | ')' => -- keep as token
            The_Index := The_Index - 1;
            return The_Line(Start_Index .. The_Index - 1);
          when ' ' | '(' | ',' | ';' =>
            return The_Line(Start_Index .. The_Index - 2);
          when others =>
            null;
          end case;
        end case;
      end loop;
    end Next_Token;

    function Found (Token : String) return Boolean is
    begin
      while Token /= Next_Token loop
        null;
      end loop;
      return True;
    end Found;

    Filename : constant String := Project.Program_Unit;

  begin -- Evaluated
    Build.Clear;
    Log.Write ("||| Evaluate build information from " & Filename);
    if File.Exists (Filename) then
      begin
        Ada.Text_IO.Open (The_File, Ada.Text_IO.In_File, Filename);
      exception
      when Item: others =>
        Log.Write (Item);
        return False;
      end;
      while Found ("pragma") loop
        declare
          Pragma_Name : constant String := Next_Token;
          The_Name    : Text.String;

          function Actual_Name return String is
          begin
            if Text.Is_Null (The_Name) then
              return Next_Token;
            else
              declare
                Name : constant String := The_Name.S;
              begin
                Text.Clear (The_Name);
                return Name;
              end;
            end if;
          end Actual_Name;

        begin
          if Pragma_Name = "Build" then
            Build.Define;
            loop
              declare
                Attribute_Name : constant String := Actual_Name;
                Association    : constant String := Next_Token;
                Attribute      : constant String := Next_Token;

                procedure Define_Compiler is
                begin
                  if not Build.Defined_Compiler (Attribute) then
                    Log.Write ("!!! Build - Compiler <" & Attribute & "> unknown");
                  end if;
                end Define_Compiler;

                procedure Define_Compilers is
                  Token : constant String := Next_Token;
                  Is_Ok : Boolean := False;
                begin
                  if Token = ")" then
                    Is_Ok :=  Build.Defined_Compilers (First  => Attribute,
                                                       Second => "");
                  elsif Next_Token = ")" then
                    Is_Ok :=  Build.Defined_Compilers (First  => Attribute,
                                                       Second => Token);
                  end if;
                  if not Is_Ok then
                    Log.Write ("!!! Build - Compilers unknown");
                  end if;
                end Define_Compilers;

                procedure Define_Kind is
                begin
                  if not Build.Defined_Kind (Attribute) then
                    Log.Write ("!!! Build - Kind <" & Attribute & "> unknown");
                  end if;
                end Define_Kind;

                procedure Define_Libraries is
                  The_Libraries : Text.List;
                begin
                  The_Libraries.Append (Attribute);
                  loop
                    declare
                      Token : constant String := Next_Token;
                    begin
                      exit when Token = ")";
                      The_Libraries.Append (Token);
                    end;
                  end loop;
                  Build.Define_Libraries (The_Libraries);
                end Define_Libraries;

                procedure Define_Interface is
                  The_Interface : Text.List;
                begin
                  The_Interface.Append (Attribute);
                  loop
                    declare
                      Token : constant String := Next_Token;
                    begin
                      if Token = "+" then
                        The_Interface.Append (Next_Token);
                      else
                        The_Name := [Token];
                        exit;
                      end if;
                    end;
                  end loop;
                  Build.Define_Interface (The_Interface);
                end Define_Interface;

                procedure Define_Resource is
                begin
                  if not Build.Defined_Resource (Attribute) then
                    Log.Write ("!!! Build - Resource <" & Attribute & "> unknown");
                  end if;
                end Define_Resource;

                procedure Define_Version is
                  The_Version : Build.Version;
                begin
                  The_Version.Major := Build.Version_Number'value(Attribute);
                  The_Version.Minor := Build.Version_Number'value(Next_Token);
                  The_Version.Variant := Build.Version_Number'value(Next_Token);
                  The_Version.Revision := Build.Version_Number'value(Next_Token);
                  if Next_Token = ")" then
                    Build.Define (The_Version);
                  end if;
                exception
                when others =>
                  Log.Write ("!!! Build - Incorrect version number");
                end Define_Version;

                procedure Define_Icon is
                begin
                  if not Build.Defined_Icon (Attribute) then
                    Log.Write ("!!! Build - Icon => " & Attribute & " unknown");
                  end if;
                end Define_Icon;

              begin
                exit when Attribute_Name = ")" or else Association /= "=>";
                if Attribute_Name = "Compiler" then
                  Define_Compiler;
                elsif Attribute_Name = "Compilers" then
                  Define_Compilers;
                elsif Attribute_Name = "Description" then
                  Build.Define_Description (Attribute);
                elsif Attribute_Name = "Kind" then
                  Define_Kind;
                elsif Attribute_Name = "Libraries" then
                  Define_Libraries;
                elsif Attribute_Name = "Use_Interface" then
                  Define_Interface;
                elsif Attribute_Name = "Resource" then
                  Define_Resource;
                elsif Attribute_Name = "Version" then
                  Define_Version;
                elsif Attribute_Name = "Icon" then
                  Define_Icon;
                end if;
              end;
            end loop;
          elsif Pragma_Name = "Style_White_Elephant" then
            Build.Define_Company ("White Elephant GmbH");
          elsif Pragma_Name = "Style_Soudronic" then
            Build.Define_Company ("Soudronic AG");
          end if;
        end;
      end loop;
    end if;
    return Build.Is_Defined;
  exception
  when End_Detected =>
    Ada.Text_IO.Close (The_File);
    return Build.Is_Defined;
  end Evaluated;

end Build_Parser;
