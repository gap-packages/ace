gap> START_TEST("examples.tst");
gap> ReadPackage("ace", "tst/regression.g");
true
gap> cases := [["A5",60], ["A5-C5",12], ["C5-fel0",5],
>              ["F27-purec",29], ["F27-fel0",29], ["F27-fel1",29],
>              ["SL219-hard",180], ["perf602p5",480]];;
gap> for case in cases do
>      fixture := ACE_TestExample(case[1]);
>      PushOptions(fixture.options);
>      stats := ACEStats(fixture.fgens, fixture.rels, fixture.sgens);
>      table := ACECosetTable(fixture.fgens, fixture.rels, fixture.sgens);
>      PopOptions();
>      Print(case[1], ": ", stats.index = case[2] and
>          ACE_TestTable(table, fixture.fgens, fixture.rels, fixture.sgens, case[2])
>          and ACE_TestStandard(table, false), "\n");
>    od;
A5: true
A5-C5: true
C5-fel0: true
F27-purec: true
F27-fel0: true
F27-fel1: true
SL219-hard: true
perf602p5: true
gap> # Compare strategies only with the same canonical numbering.
gap> fixture := ACE_TestExample("A5-C5");;
gap> tables := [];;
gap> for strategy in [rec(), rec(purec:=true), rec(felsch:=0),
>                     rec(hlt:=true), rec(hard:=true)] do
>      strategy.workspace := "1M";
>      PushOptions(strategy);
>      Add(tables, ACECosetTable(fixture.fgens, fixture.rels, fixture.sgens : lenlex));
>      PopOptions();
>    od;
gap> ForAll(tables, t -> t = tables[1] and ACE_TestStandard(t, false));
true
gap> table := ACECosetTable(fixture.fgens, fixture.rels, fixture.sgens
>                          : workspace := "1M", semilenlex);;
gap> ACE_TestTable(table, fixture.fgens, fixture.rels, fixture.sgens, 12)
>    and ACE_TestStandard(table, true);
true
gap> oldTCENUM := TCENUM;; TCENUM := ACETCENUM;;
gap> table := CosetTableFromGensAndRels(fixture.fgens, fixture.rels, fixture.sgens
>                                     : workspace := "1M");;
gap> TCENUM := oldTCENUM;;
gap> ACE_TestTable(table, fixture.fgens, fixture.rels, fixture.sgens, 12);
true
gap> # Profiled at <0.1 CPU seconds each on Apple Silicon; stats only, 2M entries.
gap> for name in ["M12-hlt", "M12-fel1"] do
>      fixture := ACE_TestExample(name);
>      PushOptions(fixture.options);
>      stats := ACEStats(fixture.fgens, fixture.rels, fixture.sgens);
>      PopOptions();
>      Print(name, ": ", stats.index = 95040, "\n");
>    od;
M12-hlt: true
M12-fel1: true
gap> STOP_TEST("examples.tst", 1000000);
