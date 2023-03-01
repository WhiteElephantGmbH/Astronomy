pragma Style_White_Elephant;

package Database.Stars is

  type Data_Range is range 1 .. 1; --126911;

  type Name_Id is range Unknown_Id .. 2; --414;
  type HD_Id   is range Unknown_Id .. 358906;
  type HIP_Id  is range Unknown_Id .. 120416;
  type HR_Id   is range Unknown_Id .. 9110;

  Names : constant Name_List(First_Id .. Positive(Name_Id'last)) := [
    +"First Name",  -- 1
    +"Second Name"  -- 2
  ];

  type Information is record
    Name_Index : Name_Id;
    HD_Number  : HD_Id;
    HIP_Number : HIP_Id;
    HR_Number  : HR_Id;
    Info       : Star_Information;
  end record;

  type Data is array (Data_Range) of Information;

  pragma Style_Checks ("M200");

  List : constant Data := [
  -- Name HD      HIP     HD     Otype Ra J2000      Dec J2000     Ra PM  Dec PM  PLX   MagU  MagB  MagV  MagR  MagI  Stype
    (  1, 358906, 120416, 9110, (OT01, 242.99889865, -19.46070448, -7.65, -23.71, 6.88, 3.42, 4.05, 4.00, 3.90, 3.87, (B, S2, V)))
  ];

end Database.Stars;
