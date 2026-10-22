#############################################################################
##
#A  subgroup-recovery.tst     ACE package                         Greg Gamble
#A                                                             Volkmar Felsch
##
##  A nice example provided by Volkmar Felsch that tests the stability of the
##  ACE interface when bad input is fed to the ACE binary.
##

# Additional recovery and table checks derived from aceds.tst.
# Keep its deletion calls adjacent: no ACE query between them.
# Capture the diagnostic, assert its meaning, then test a valid query.
gap> START_TEST("subgroup-recovery.tst");
gap> ReadPackage("ace", "tst/regression.g");
true
gap> G := PerfectGroup(IsSubgroupFpGroup, 2^5*60, 2);;
gap> fgens := FreeGeneratorsOfFpGroup(G);; rels := RelatorsOfFpGroup(G);;
gap> b := fgens[2];; t := fgens[4];;
gap> i := ACEStart(fgens, rels, [b,t] : workspace := "1M");;
gap> stats := ACEStats(i);;
gap> stats.index = 80 and stats.activecosets = 80 and
>    stats.cputimeUnits = "10^-2 seconds";
true
gap> IsInt(stats.cputime) and stats.cputime >= 0 and
>    IsInt(stats.maxcosets) and stats.maxcosets >= stats.activecosets and
>    IsInt(stats.totcosets) and stats.totcosets >= stats.maxcosets;
true
gap> ACEDeleteSubgroupGenerators(i, [t]) = [b];
true
gap> lev := InfoLevel(InfoACE);; diagnostic := "";;
gap> output := OutputTextString(diagnostic, true);;
gap> oldOutput := InfoOutput(InfoACE);; oldWarningOutput := InfoOutput(InfoWarning);;
gap> SetInfoOutput(InfoACE, output);; SetInfoOutput(InfoWarning, output);;
gap> SetInfoLevel(InfoACE, 3);;
gap> remaining := ACEDeleteSubgroupGenerators(i, [2]);;
gap> SetInfoLevel(InfoACE, lev);; SetInfoOutput(InfoACE, oldOutput);;
gap> SetInfoOutput(InfoWarning, oldWarningOutput);; CloseStream(output);;
gap> PositionSublist(diagnostic, "ERROR") <> fail and
>    PositionSublist(diagnostic, "first argument out of range") <> fail;
true
gap> remaining = [b] and ACESubgroupGenerators(i) = [b];
true
gap> ACEStats(i).index;
640
gap> table := ACECosetTable(i);;
gap> ACE_TestTable(table, fgens, rels, [b], 640);
true
gap> ACEAddSubgroupGenerators(i, [t]) = [b,t];
true
gap> ACEStats(i).index;
80
gap> ACE_TestTable(ACECosetTable(i), fgens, rels, [b,t], 80);
true
gap> ACEQuit(i);
gap> STOP_TEST("subgroup-recovery.tst", 1000000);
