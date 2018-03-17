with ClosedLoop;

procedure TestCase is
    ClosedLoop.Init();
 for I in Integer range 0..100 loop
    ClosedLoop.Tick();
 end loop;

end TestCase；
