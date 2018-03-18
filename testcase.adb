with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with ClosedLoop;

procedure TestCase is
begin
    ClosedLoop.Init;
    for I in Integer range 0..10000 loop
        Put_Line("This is " & Integer'Image(I) & " time." );
        ClosedLoop.Tick;
    end loop;

end TestCase;