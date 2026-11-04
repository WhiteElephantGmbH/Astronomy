-- *********************************************************************************************************************
-- *                       (c) 2007 .. 2019 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Log;
with Ada_95.Lexer;
with Ada_95.Name;
with Ada_95.Token.Parser;
with Ada_95.Token.Pre_Parser;
with Memory;

package body Ada_95.Library is

  package Data renames Token.Data;

  package Elements renames Data.Library_Tree;

  The_Library : Elements.Item;

------------------------------------------------------------------------------------------------------------------------

  function Resource (Unit : Handle) return Data.Resource_Handle is
  begin
    return Data.Resource (Unit.all);
  end Resource;


  procedure Clear_Mark (Cursor : Element_Cursor) is
  begin
    Cursor.Is_Marked := False;
  end Clear_Mark;

  procedure Clear_All_Marks is new Elements.Iterator (Visit => Clear_Mark);


  procedure Clear_Depending_On (Unit   : Handle;
                                Cursor : Element_Cursor);

  procedure Clear_All_Depending_On is new Elements.Iterator_With (Parameter  => Handle,
                                                                  Visit_With => Clear_Depending_On);

  procedure Clear_Depending_On (Unit   : Handle;
                                Cursor : Element_Cursor) is

    procedure Clear (The_Unit : Handle) with Inline is
    begin
      Memory.Clear (Resource(The_Unit).Bucket);
    end Clear;

    use type Handle;

  begin
    if not Cursor.Is_Marked then
      --Log.Write ("~~~       CHECK UNIT  : " & Resource(Unit).Attributes.Handle.Id);
      --Log.Write ("~~~       WITH ELEMENT: " & Name.Image_Of(Cursor.Id.all));
      if Cursor.Unit /= null then
        if Cursor.Unit.all in Data.Unit_Body'class then
          --Log.Write ("~~~       CHECK BODY: " & Resource(Cursor.Unit).Attributes.Handle.Id);
          if Data.Is_Used (Unit, By => Cursor.Unit) then
            Cursor.Is_Marked := True;
            --Log.Write ("~~~       - CLEAR BODY ONLY: " & Resource(Cursor.Unit).Attributes.Handle.Id);
            Clear (Cursor.Unit);
            Cursor.Unit := null;
          end if;
        elsif Cursor.Unit.all in Data.Unit_Declaration'class then
          declare
            Depending_Spec : constant Handle := Cursor.Unit;
            Depending_Body : constant Handle := Handle(Data.Unit_Declaration_Handle(Depending_Spec).Implementation);
          begin
            --Log.Write ("~~~       CHECK SPEC: " & Resource(Depending_Spec).Attributes.Handle.Id);
            if Data.Is_Used (Unit, By => Depending_Spec) then
              Cursor.Is_Marked := True;
              --Log.Write ("~~~       -> USED SPEC: " & Resource(Depending_Spec).Attributes.Handle.Id);
              Clear_All_Depending_On (Depending_Spec, The_Library);
              if Depending_Body /= null then
                --Log.Write ("~~~       - CLEAR BODY: " & Resource(Depending_Body).Attributes.Handle.Id);
                Clear (Depending_Body);
              end if;
              --Log.Write ("~~~       - CLEAR SPEC: " & Resource(Depending_Spec).Attributes.Handle.Id);
              Clear (Cursor.Unit);
              Cursor.Unit := null;
            elsif Depending_Body /= null then
              if Data.Is_Used (Unit, By => Depending_Body) then
                --Log.Write ("~~~       - CLEAR BODY: " & Resource(Depending_Body).Attributes.Handle.Id);
                Clear (Depending_Body);
                Data.Unit_Declaration_Handle(Cursor.Unit).Implementation := null;
              end if;
            end if;
          end;
        elsif Cursor.Unit.all in Data.Unit_Type'class then
          declare
            Depending_Spec : constant Handle := Cursor.Unit;
          begin
            --Log.Write ("~~~       CHECK SPEC: " & Resource(Depending_Spec).Attributes.Handle.Id);
            if Data.Is_Used (Unit, By => Depending_Spec) then
              Cursor.Is_Marked := True;
              --Log.Write ("~~~       -> USED SPEC: " & Resource(Depending_Spec).Attributes.Handle.Id);
              Clear_All_Depending_On (Depending_Spec, The_Library);
              --Log.Write ("~~~       - CLEAR SPEC: " & Resource(Depending_Spec).Attributes.Handle.Id);
              Clear (Cursor.Unit);
              Cursor.Unit := null;
            end if;
          end;
        else
          Cursor.Is_Marked := True;
          Log.Write ("???       - UNKNOWN LIBRARY: " & Resource(Cursor.Unit).Attributes.Handle.Id);
        end if;
      end if;
    end if;
  end Clear_Depending_On;

------------------------------------------------------------------------------------------------------------------------

  The_Actual_Unit : Handle;


  function Added (Unit_Name  : File.Unit_Name;
                  Attributes : File.Attributes) return Handle is

    Last_Bucket      : constant Memory.Bucket := Memory.Actual_Bucket;
    Last_Actual_Unit : constant Handle := The_Actual_Unit;

    Filename  : constant String := Attributes.Handle.Id;
    File_Kind : constant File.Kind := Attributes.Extension;

    use type File.Area;
    use type File.Kind;

    The_Unit : Handle;

    procedure Clear_Dependent_Units is
    begin
      --Log.Write ("~~~       CLEAR DEPENDANT UNITS");
      Clear_All_Marks (The_Library);
      Resource(The_Unit).Element.Is_Marked := True; -- stop when cyclic
      Clear_All_Depending_On (The_Unit, The_Library);
    end Clear_Dependent_Units;


    procedure Add_Unit is

      The_Resource : Data.Resources;

      use type Data.Unit_Body_Handle;
      use type Data.Unit_Declaration_Handle;
      use type Handle;
      use type Elements.Cursor;
      use type File.Attributes;

      function Specification_Is_Original return Boolean is
      begin
        if The_Unit.all in Data.Unit_Body'class then
          --Log.Write ("~~~       first specifivation (clear existing implementation)");
          if Data.Unit_Body_Handle(The_Unit).Specification /= null then
            Log.Write ("???       - not first specification");
            raise Program_Error;
          end if;
          Memory.Clear (Resource(The_Unit).Bucket);
          The_Resource.Element.Unit := null;
          return False;
        elsif Resource(The_Unit).Attributes = Attributes then
          --Log.Write ("          specification up to date");
          return True; -- up to date
        elsif The_Unit.all in Data.Unit_Declaration'class then -- clear dependant units
          --Log.Write ("~~~   NEW SPEC " & Attributes.Handle.Id);
          --Log.Write ("~~~   OLD SPEC " & Resource(The_Unit).Attributes.Handle.Id);
          declare
            Specification  : constant Data.Unit_Declaration_Handle := Data.Unit_Declaration_Handle(The_Unit);
            Implementation : constant Handle := Handle(Specification.Implementation);
          begin
            if Implementation /= null then
              --Log.Write ("~~~       CLEAR BODY: " & Resource(Implementation).Attributes.Handle.Id);
              Memory.Clear (Resource(Implementation).Bucket);
              Specification.Implementation := null;
            end if;
          end;
          Clear_Dependent_Units;
          The_Resource.Element.Unit := null;
          return False;
        else -- unknown Library
          --Log.Write ("~~~       REPLACE UNKNOWN LIBRARY: ");
          The_Resource.Element.Unit := null;
          return False;
        end if;
      end Specification_Is_Original;


      function Implementation_Is_Original return Boolean is
        The_Specification : Data.Unit_Declaration_Handle;
      begin
        if The_Unit.all in Data.Unit_Declaration'class then
          The_Specification := Data.Unit_Declaration_Handle(The_Unit);
          The_Unit := Handle(The_Specification.Implementation);
          if The_Unit = null then
            --Log.Write ("~~~       first implementation");
            return False;
          end if;
        elsif The_Unit.all in Data.Unit_Body'class then
          The_Specification := Data.Unit_Body_Handle(The_Unit).Specification;
        end if;
        if Resource(The_Unit).Attributes = Attributes then
          --Log.Write ("~~~       body up to date");
          return True; -- up to date
        else -- clear implementation or unknown library
          --Log.Write ("~~~   NEW " & Attributes.Handle.Id);
          --Log.Write ("~~~   OLD " & Resource(The_Unit).Attributes.Handle.Id);
          if The_Specification = null then
            The_Resource.Element.Unit := null;
          else
            The_Specification.Implementation := null;
          end if;
          if The_Unit.all in Data.Library_Package_Body'class | Data.Library_Subprogram_Body'class then
            --Log.Write ("~~~       clear subunits");
            Clear_Dependent_Units;
          end if;
          return False;
        end if;
      end Implementation_Is_Original;

    begin -- Add_Unit
      Elements.Add (Id          => Unit_Name,
                    To          => The_Library,
                    Data_Cursor => The_Resource.Element);
      if The_Resource.Element = null then
        --Log.Write ("~~~ Library.Add - No element: " & Name.Image_Of (Unit_Name));
        return;
      end if;
      --Log.Write ("~~~ - add " & Name.Image_Of (Unit_Name));
      --Log.Write ("~~~       " & File.Kind'image(File_Kind));
      The_Unit := The_Resource.Element.Unit;
      if The_Unit /= null then
        if File_Kind = File.Specification then
          if Specification_Is_Original then
            return;
          end if;
        else
          if Implementation_Is_Original then
            return;
          end if;
        end if;
      end if;
      if The_Unit = null then -- new unit
        The_Resource.Bucket := Memory.New_Bucket;
      else -- update
        --Log.Write ("~~~ - CLEAR: " & Name.Image_Of (Unit_Name));
        Memory.Clear (Resource(The_Unit).Bucket);
        --Log.Write ("~~~ - NEW: " & Name.Image_Of (Unit_Name));
        The_Resource.Bucket := Memory.New_Bucket;
      end if;
      Memory.Assign (The_Resource.Bucket);
      declare
        New_Resource : constant Data.Resource_Handle := new Data.Resources'(The_Resource);
      begin
        New_Resource.Attributes := Attributes;
        New_Resource.Tokens := Lexer.Tokens_For (Attributes.Handle.all);
        The_Unit := Token.Pre_Parser.Process (New_Resource);
        if The_Unit = null then
          The_Unit := Data.New_Unknown_Library (New_Resource);
        else
          The_Actual_Unit := The_Unit;
          Token.Parser.Process (The_Unit);
          if The_Unit.Resource.Element.Unit = null then
            Log.Write ("!!! - Unit not initialized");
            The_Unit.Resource.Element.Unit := The_Unit;
          end if;
        end if;
      end;
    exception
    when Occurrence: others =>
      Log.Write ("??? Library.Add", Occurrence);
      The_Unit := null;
    end Add_Unit;

  begin
    if Attributes.Location = File.Unknown then
      Log.Write ("??? Library.Add - Unknown Location of File: " & Filename);
    elsif Attributes.Extension = File.Unknown then
      Log.Write ("??? Library.Add - file for " & Name.Image_Of (Unit_Name, Separator => '.') & " not found");
    else
      Memory.Assign_Default_Bucket;
      Add_Unit;
      Memory.Assign (Last_Bucket);
      The_Actual_Unit := Last_Actual_Unit;
    end if;
    return The_Unit;
  end Added;


  procedure Start is
  begin
    Lexer.Start;
    Data.Initialize;
    The_Actual_Unit := null;
  end Start;


  function Added (From : Source.Object'class) return Handle is

    Filename   : constant String := From.Id;
    Attributes : constant File.Attributes := File.Attributes_Of (From);

    use type File.Kind;

  begin
    if Attributes.Extension = File.Unknown then
      return null;
    else
      Log.Write ("+++ add File - " & Filename);
      return Added (Unit_Name   => File.Information_For (Filename).Id,
                    Attributes  => Attributes);
    end if;
  exception
  when Occurrence: others =>
    Log.Write ("??? Library.Added", Occurrence);
    return null;
  end Added;


  function Unit_Of (Item : File.Unit_Name) return Handle is

    The_Element : Elements.Cursor;

    use type Elements.Cursor;
    use type Handle;

  begin
    Elements.Get (Id          => Item,
                  From        => The_Library,
                  Data_Cursor => The_Element);
    if The_Element /= null and then The_Element.Unit /= null then
      return The_Element.Unit;
    else
      declare
        Attributes : constant File.Attributes := File.Attributes_Of (Item);
      begin
        case Attributes.Extension is
        when File.Specification =>
          Log.Write ("+++ add Library Unit - " & Name.Image_Of (Item));
        when File.Implementation =>
          Log.Write ("+++ add Main Body - " & Name.Image_Of (Item));
        when others =>
          Log.Write ("### add Library Unit - " & Name.Image_Of (Item) & " not found");
          return null;
        end case;
        return Added (Unit_Name   => Item,
                      Attributes  => Attributes);
      end;
    end if;
  exception
  when Occurrence: others =>
    Log.Write ("??? Library.Unit_For", Occurrence);
    return null;
  end Unit_Of;


  function Spec_Of (Item : File.Unit_Name) return Handle is

    The_Element : Elements.Cursor;

    use type Elements.Cursor;
    use type Handle;

  begin
    Elements.Get (Id          => Item,
                  From        => The_Library,
                  Data_Cursor => The_Element);
    if The_Element /= null and then The_Element.Unit /= null then
      return The_Element.Unit;
    else
      declare
        Attributes : constant File.Attributes := File.Attributes_Of (Item, File.Specification);
        use type File.Kind;
      begin
        if Attributes.Extension = File.Specification then
          Log.Write ("+++ add Library Spec - " & Name.Image_Of (Item));
          return Added (Unit_Name   => Item,
                        Attributes  => Attributes);
        else
          Log.Write ("--- add Library Spec - " & Name.Image_Of (Item) & " not found");
          return null;
        end if;
      end;
    end if;
  exception
  when Occurrence: others =>
    Log.Write ("??? Library.Unit_For", Occurrence);
    return null;
  end Spec_Of;


  function Body_Of (Unit : Handle) return Handle is
    use type Handle;
  begin
    if Unit.all in Data.Unit_Declaration'class then
      declare
        Implementation : constant Handle := Handle(Data.Unit_Declaration_Handle(Unit).Implementation);
      begin
        if Implementation = null then
          declare
            Id         : constant File.Unit_Name := Resource(Unit).Element.Id.all;
            Attributes : constant File.Attributes := File.Attributes_Of (Id, File.Implementation);
            use type File.Kind;
          begin
            if Attributes.Extension = File.Implementation then
              Log.Write ("+++ add Library Body - " & Name.Image_Of (Id));
              return Added (Unit_Name   => Id,
                            Attributes  => Attributes);
            else
              return null;
            end if;
          end;
        else
          return Implementation;
        end if;
      end;
    else
      return Unit; -- is already body
    end if;
  exception
  when Occurrence: others =>
    Log.Write ("??? Library.Body_Of", Occurrence);
    return null;
  end Body_Of;


  function Ada_Exceptions_Unit return Handle is
  begin
    return Unit_Of (Name.Id_Of (File.Ada_Exceptions_Name));
  end Ada_Exceptions_Unit;


  function Ada_Tags_Unit return Handle is
  begin
    return Unit_Of (Name.Id_Of (File.Ada_Tags_Name));
  end Ada_Tags_Unit;


  function Standard_Unit return Handle is
  begin
    return Unit_Of (Name.Id_Of (File.Standard_Name));
  end Standard_Unit;


  function System_Unit return Handle is
  begin
    return Unit_Of (Name.Id_Of (File.System_Name));
  end System_Unit;


  function System_Machine_Code_Unit return Handle is
  begin
    return Unit_Of (Name.Id_Of (File.System_Machine_Code_Name));
  end System_Machine_Code_Unit;


  function Actual_Unit return Handle is
  begin
    return The_Actual_Unit;
  end Actual_Unit;


  procedure Iterator is
    procedure Iterate is new Elements.Iterator (Visit);
  begin
    Iterate (The_Library);
  end Iterator;

------------------------------------------------------------------------------------------------------------------------

  procedure Clear_Visit (Cursor : Element_Cursor) is
  begin
    Cursor.Is_Visited := False;
  end Clear_Visit;

  procedure Clear_Visits is new Elements.Iterator (Visit => Clear_Visit);


  procedure Iterator_With (Start_Unit : Handle;
                           Visit_Body : Boolean) is

    Max_Nesting : constant := 1024;

    Nesting_Count : Natural := 0;

    use type Handle;

    procedure Visit_Item (Import : Data.Library_Tree.Cursor);

    procedure Visit_All is new Data.Library_Tree.Iterator (Visit_Item);

    procedure Visit_Item (Import : Data.Library_Tree.Cursor) is
      Unit : constant Handle := Import.Unit;
    begin
      if Nesting_Count > Max_Nesting then
        return;
      end if;
      Nesting_Count := Nesting_Count + 1;
      if Unit /= null and then not Token.Is_Null (Unit.Location) and then
        not File.Is_Standard_Or_System (Unit.Location.Id)
      then
        if not Data.Resource(Unit.all).Element.Is_Visited then
          Visit_All (Data.Resource(Unit.all).Imports);
          if not Data.Resource(Unit.all).Element.Is_Visited then
            Data.Resource(Unit.all).Element.Is_Visited := True;
            Handling_For (Unit);
            if Visit_Body then
              declare
                Unit_Body : constant Handle := Body_Of (Unit);
              begin
                if Unit_Body /= null then
                  Visit_All (Data.Resource(Unit_Body.all).Imports);
                  Handling_For (Unit_Body);
                end if;
              end;
            end if;
          end if;
        end if;
      end if;
      Nesting_Count := Nesting_Count - 1;
    end Visit_Item;

  begin -- Iterator_With
    Clear_Visits (The_Library);
    if Start_Unit.all in Data.Unit_Body'class then
      declare
        Specification : constant Handle := Handle(Data.Unit_Body_Handle(Start_Unit).Specification);
      begin
        if Specification /= null then
          Visit_All (Data.Resource(Specification.all).Imports);
          Data.Resource(Specification.all).Element.Is_Visited := True;
          Handling_For (Specification);
        end if;
      end;
    end if;
    Visit_All (Data.Resource(Start_Unit.all).Imports);
    Handling_For (Start_Unit);
  end Iterator_With;

------------------------------------------------------------------------------------------------------------------------

  procedure Finalize is
  begin
    Lexer.Finalize;
    Elements.Clear (The_Library);
  end Finalize;

end Ada_95.Library;
