# Exercise the generated standalone input as well as GAP's stream protocol.
gap> START_TEST("standalone.tst");
gap> ReadPackage("ace", "tst/regression.g");
true
gap> tmp := DirectoryTemporary();; infile := Filename(tmp, "example.in");;
gap> lev := InfoLevel(InfoACE);; SetInfoLevel(InfoACE, 0);;
gap> for case in [["A5",60], ["A5-C5",12], ["F27-purec",29], ["SL219-hard",180]] do
>      fixture := ACE_TestExample(case[1]);
>      PushOptions(fixture.options);
>      ACECosetTable(fixture.fgens, fixture.rels, fixture.sgens
>                    : aceinfile := infile, echo := false);
>      PopOptions();
>      input := InputTextFile(infile);
>      transcript := "";
>      output := OutputTextString(transcript, true);
>      code := Process(DirectoryCurrent(), ACEData.binary, input, output, []);
>      CloseStream(input); CloseStream(output);
>      Print(case[1], ": ", code = 0 and
>            PositionSublist(transcript, Concatenation("INDEX = ", String(case[2]), " ")) <> fail
>            and PositionSublist(transcript, "ERROR") = fail, "\n");
>    od;
A5: true
A5-C5: true
F27-purec: true
SL219-hard: true
gap> SetInfoLevel(InfoACE, lev);; RemoveFile(infile);;
gap> STOP_TEST("standalone.tst", 1000000);
