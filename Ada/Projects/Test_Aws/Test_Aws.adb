pragma Style_Astronomy;

pragma Build (Description => "Test for AWS integration",
              Version     => (1, 0, 0, 0),
              Kind        => Console,
              Icon        => False,
              Libraries   => ("AWSS64"),
              Compiler    => "GNATPRO\23.0");

with Ada.Text_IO;
with Ada.Exceptions;
with AWS.Client;
with AWS.Response;

procedure Test_Aws is

   Result : AWS.Response.Data;

begin
   Ada.Text_IO.Put_Line ("Before Get");

   --Result := AWS.Client.Get ("http://example.com");
   Result := AWS.Client.Get ("https://www.google.com");

   Ada.Text_IO.Put_Line ("After Get:" & AWS.Response.Status_Code (Result)'image);

exception
   when E : others =>
      Ada.Text_IO.Put_Line ("Exception:");
      Ada.Text_IO.Put_Line
        (Ada.Exceptions.Exception_Name (E));

      Ada.Text_IO.Put_Line
        (Ada.Exceptions.Exception_Message (E));

      Ada.Text_IO.Put_Line
        (Ada.Exceptions.Exception_Information (E));
end Test_Aws;