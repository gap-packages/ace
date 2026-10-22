# Helpers shared by the ordinary tests and the opt-in example profile.
# Read the presentations and strategy options from the shipped examples.
ACE_TestFixture := function(fgens, rels, sgens)
    return rec(fgens := fgens, rels := rels, sgens := sgens,
               options := ShallowCopy(ACE_OPTIONS()));
end;

ACE_TestExample := function(name)
    local fixture;
    fixture := ACE_READ_AS_FUNC(
        Filename(DirectoriesPackageLibrary("ace", "examples"), name),
        ACE_TestFixture);
    # Only replace output controls and resource caps, retaining the strategy.
    SANITISE_ACE_OPTIONS(fixture.options,
        rec(echo := false, messages := 0, workspace := "2M"));
    fixture.options.echo := false;
    fixture.options.messages := 0;
    fixture.options.workspace := "2M";
    return fixture;
end;

# Check a complete table independently of the enumerator and its counters.
ACE_TestTable := function(t, fgens, rels, sgens, index)
    local j, perms;
    if Length(t) <> 2 * Length(fgens) or
       not ForAll(t, col -> Length(col) = index and Set(col) = [1..index]) then
        return false;
    fi;
    for j in [1..Length(fgens)] do
        if not ForAll([1..index], c -> t[2*j][t[2*j-1][c]] = c) then
            return false;
        fi;
    od;
    perms := List([1..Length(fgens)], j -> PermList(t[2*j-1]));
    return ForAll(rels, w -> IsOne(MappedWord(w, fgens, perms))) and
           ForAll(sgens, w -> 1^MappedWord(w, fgens, perms) = 1) and
           Length(Orbit(Group(perms), 1)) = index;
end;

# Breadth-first numbering in the documented generator order. Incomplete
# tables can have zeros; these are not new cosets.
ACE_TestStandard := function(t, semilenlex)
    local cols, seen, c, j, next;
    cols := [1..Length(t)];
    if semilenlex then cols := [1,3..Length(t)-1]; fi;
    seen := [1];
    for c in [1..Length(t[1])] do
        for j in cols do
            next := t[j][c];
            if next <> 0 and not next in seen then
                if next <> Length(seen)+1 then return false; fi;
                Add(seen, next);
            fi;
        od;
    od;
    return Length(seen) = Length(t[1]);
end;
