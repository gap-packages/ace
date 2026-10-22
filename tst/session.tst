gap> START_TEST("session.tst");
gap> ReadPackage("ace", "tst/regression.g");
true
gap> fixture := ACE_TestExample("A5");;
gap> i := ACEStart(fixture.fgens, fixture.rels, [] : workspace := "1M", max := 5);;
gap> ACEStats(i).index;
0
gap> IsCompleteACECosetTable(i);
false
gap> table := ACECosetTable(i : incomplete);;
#I  ACECosetTable: Coset table is incomplete, reduced & lenlex standardised.
gap> ForAny(table, col -> 0 in col) and ACE_TestStandard(table, false);
true
gap> ACEContinue(i : max := 1000);;
gap> ACEStats(i).index;
60
gap> table := ACECosetTable(i);;
gap> ACE_TestTable(table, fixture.fgens, fixture.rels, [], 60);
true
gap> ACEStandardCosetNumbering(i);;
gap> ACEStats(i).index = Length(ACECosetTable(i)[1]);
true
gap> ACERedo(i);;
gap> ACEStats(i).index;
60
gap> ACEQuit(i);
gap> ACECosetTable(fixture.fgens, fixture.rels, []
>                 : workspace := "1M", max := 5, silent);
fail
gap> table := ACECosetTable(fixture.fgens, fixture.rels, []
>                          : workspace := "1M", max := 5, incomplete);;
#I  ACECosetTable: Coset table is incomplete, reduced & lenlex standardised.
gap> ForAny(table, col -> 0 in col) and ACE_TestStandard(table, false);
true
gap> STOP_TEST("session.tst", 1000000);
