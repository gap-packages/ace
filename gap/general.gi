#############################################################################
####
##
#W  general.gi              ACE Share Package                Alexander Hulpke
#W                                                                Greg Gamble
##
##  This file installs mainly non-interactive ACE  variables  and  functions.
##  Though Alexander will barely recognise it,  some  of his ideas are  still
##  present.
##    
#H  @(#)$Id$
##
#Y  Copyright (C) 2000  Centre for Discrete Mathematics and Computing
#Y                      Department of Computer Science & Electrical Eng.
#Y                      University of Queensland, Australia.
##
Revision.general_gi :=
    "@(#)$Id$";


#############################################################################
####
##
#V  ACETCENUM . . . . . . . .  The ACE version of the coset enumerator TCENUM
##  . . . .  CosetTableFromGensAndRels is set to ACECosetTableFromGensAndRels
##
InstallValue(ACETCENUM, rec(
  name := "ACE (Advanced Coset Enumerator)",
  CosetTableFromGensAndRels := ACECosetTableFromGensAndRels
));

#############################################################################
####
##
#F  InfoACELevel . . . . . . . . . . . . . . .  Get the InfoLevel for InfoACE
##
##
InstallGlobalFunction("InfoACELevel", function()
  return InfoLevel(InfoACE);
end);

#############################################################################
####
##
#F  SetInfoACELevel . . . . . . . . . . . . . . Set the InfoLevel for InfoACE
##
##
InstallGlobalFunction("SetInfoACELevel", function(level)
  SetInfoLevel(InfoACE, level);
end);

#############################################################################
####
##
#F  CALL_ACE . . . . . . . . . Called by ACECosetTable, ACEStats and StartACE
##
##
InstallGlobalFunction(CALL_ACE, function(ACEfname, fgens, rels, sgens)
local optnames, echo, n, infile, instream, outfile, ToACE, 
      IsLowercaseOneCharGen, acegens, ignored;

  optnames := ACE_OPT_NAMES();
  # We have hijacked ACE's echo option ... we don't actually pass it to ACE
  echo := VALUE_ACE_OPTION(optnames, 0, "echo");

  n := Length(fgens);
  if ForAny(fgens, i -> NumberSyllables(i)<>1 or ExponentSyllable(i, 1)<>1) then
    Error("first argument not a valid list of group generators");
  fi;

  if echo > 0 then
    Print(ACEfname, " called with the following arguments:\n");
    Print(" Group generators : ", fgens, "\n");
    Print(" Group relators : ", rels, "\n");
    Print(" Subgroup generators : ", sgens, "\n");
  fi;
  
  if ACEfname = "StartACE" then
    instream := InputOutputLocalProcess(ACEData.tmpdir, ACEData.binary, []);
  else 
    if ACEfname = "ACECosetTableFromGensAndRels" then
      # If option "aceinfile" is set we only want to produce an ACE input file
      infile := VALUE_ACE_OPTION(optnames, ACEData.infile, "aceinfile");
    elif ACEfname = "ACEStats" then
      infile := ACEData.infile; # If option "aceinfile" is set ... we ignore it
    fi;
    instream := OutputTextFile(infile, false);
  fi;
  ToACE := function(list) WRITE_LIST_TO_STREAM(instream, list); end;
  outfile := VALUE_ACE_OPTION(optnames, ACEData.outfile, "aceoutfile");

  # Give a name to the group ACE will be dealing with (this is not
  # actually necessary ... ACE essentially treats it as a comment)
  ToACE([ "Group Name: ", 
          VALUE_ACE_OPTION(optnames, "G", "enumeration"), ";\n" ]);
  
  IsLowercaseOneCharGen := function(g)
    local gstring;
    gstring := String(g);
    return Length(gstring) = 1 and LowercaseString(gstring) = gstring;
  end;

  # Define the generators ACE will use
  ToACE([ "Group Generators: " ]);
  if n <= 26 then
    # if #generators <= 26 tell ACE to use alphabetic generators: a ...
    if ForAll(fgens, g -> IsLowercaseOneCharGen(g)) then
      # Use the user's set of generators for ACE ... if we can
      acegens := List(fgens, g -> String(g));
    else
      acegens := List([1..n], i -> WordAlp(CHARS_LALPHA, i));
    fi;
    ToACE( [ Flat([acegens, ";\n"]) ] );
  else
    # if #generators > 26 tell ACE to use numerical generators: 1 ...
    ToACE([ n, ";\n" ]);
    acegens := List([1..n], i -> String(i));
  fi;

  # Define the group relators ACE will use
  ToACE([ "Group Relators: ", ACE_WORDS(rels, fgens, acegens), ";\n" ]);

  # Give a name to the subgroup ACE will be dealing with (this is not
  # actually necessary ... ACE essentially treats it as a comment)
  ToACE([ "Subgroup Name: ", 
          VALUE_ACE_OPTION(optnames, "H", "subgroup"), ";\n" ]);
  
  # Define the subgroup generators ACE will use
  ToACE([ "Subgroup Generators: ", ACE_WORDS(sgens, fgens, acegens), ";\n" ]);

  if ACEfname  = "ACECosetTableFromGensAndRels" then
    ignored := [ ];
  else 
    ignored := [ "aceinfile" ];
  fi;
  if ACEfname  = "StartACE" then
    Add(ignored, "aceoutfile");
  fi;

  PROCESS_ACE_OPTIONS(
      ACEfname, optnames, echo, ToACE, 
      rec(group      := ACE_ERRORS.argnotopt, # disallowed options
          generators := ACE_ERRORS.argnotopt,
          relators   := ACE_ERRORS.argnotopt), 
      ignored
      );
              
  if ACEfname <> "StartACE" then
    # Direct ACE output to outfile if called via
    # ACECosetTableFromGensAndRels or ACEStats
    ToACE([ "Alter Output:", outfile, ";\n" ]);
    ToACE([ "End;\n" ]); # (one of) the ACE directives that initiate
                         # an enumeration

    if ACEfname = "ACECosetTableFromGensAndRels" then
      ToACE([ "Print Table;\n" ]);
    fi;

    if ACEfname = "ACEStats" or infile = ACEData.infile then
      # Run ACE on the constructed infile
      # ... the ACE output will appear in outfile 
      #     (except for the banner which is directed to ACEData.banner)
      Exec(Concatenation(ACEData.binary, "<", infile, ">", ACEData.banner));
    fi;
  fi;

  if ACEfname = "ACECosetTableFromGensAndRels" then
    return rec(acegens := acegens, 
               infile := infile, 
               outfile := outfile,
               silent := VALUE_ACE_OPTION(optnames, false, "silent"));
  elif ACEfname = "ACEStats" then
    return outfile;
  else # ACEfname = "StartACE"
    Add(ACEData.io, 
        rec(args := rec(fgens := fgens, rels := rels, sgens := sgens),
            options := ACE_OPTIONS(),
            acegens := acegens, 
            stream := instream));
    return Length(ACEData.io);
  fi;
end);

#############################################################################
####
##
#F  ACECosetTableFromGensAndRels . . . . . . .  Non-interactive ACECosetTable
##
##
InstallGlobalFunction(ACECosetTableFromGensAndRels, function(fgens, rels, sgens)
  # Use ACECosetTable non-interactively
  return ACECosetTable(fgens, rels, sgens);
end);

#############################################################################
####
##
#F  ACE_READ_AS_FUNC  . . . . . . . . . . . . . . . Variant of ReadAsFunction 
##  . . . . . . . . . . . .  that allows the passing of the function argument
##  . . . . . . . . . . . . . . . . . . . . ACEfunc to ReadAsFunction(file)()
##
##
InstallGlobalFunction(ACE_READ_AS_FUNC, function(file, ACEfunc)
  local line, instream, rest;

  instream := InputTextFile(file);
  line := FLUSH_ACE_STREAM_UNTIL( instream, 3, 3, ReadLine,
                                  line -> line{[1..5]} = "local" );
  return ReadAsFunction(
             InputTextString(
                 Concatenation([
                     ReplacedString(line, ";", ", ACEfunc;"),
                     "ACEfunc := ", NameFunction(ACEfunc), ";",
                     ReadAll(instream)
                     ]) ) )();
end);

#############################################################################
####
##
#F  ACEExample  . . . . . . . .  Read an example from the examples directory.
##  . . . . . . . . . . . . . . . If given the name of a file in the examples 
##  . . . . . . . . . . . . . . . directory,  that file is displayed and read
##  . . . . . . . . . . . . . . . as a function. Otherwise, if no argument is
##  . . . . . . . . . . . . . . . given or the name given is for a file  that
##  . . . . . . . . . . . . . . . does not exist,  the file  "examples/index"
##  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . is displayed.
##
InstallGlobalFunction(ACEExample, function(arg)
    local name, infoACElevel, file, instream, line, ACEfunc;

    if IsEmpty(arg) then
      name := "index";
    else
      name := arg[1];
      if Length(arg) > 1 then
        ACEfunc := arg[2];
      else
        ACEfunc := ACECosetTableFromGensAndRels;
      fi;
    fi;
    infoACElevel := InfoACELevel();
    SetInfoACELevel(1);
    file := Filename( DirectoriesPackageLibrary( "ace", "examples"), name );
    if file = fail then
      Info(InfoACE + InfoWarning, 1,
           "Sorry! There is no ACE example file with name `", name, "'");
      name := "index";
      file := Filename( DirectoriesPackageLibrary("ace", "examples"), name );
    fi;
    # Display file ... after a few minor modifications
    instream := InputTextFile(file);
    if name <> "index" then
      line := FLUSH_ACE_STREAM_UNTIL( instream, 1, 3, ReadLine,
                                      line -> line{[1..5]} = "local" );
      Info(InfoACE, 1,
           "#", line{[Position(line, ' ')..Position(line, ';') - 1]},
           " are local to ACEExample");
      line := FLUSH_ACE_STREAM_UNTIL( instream, 1, 3, ReadLine, 
                                      line -> line{[1..6]} = "return" );
      Info(InfoACE, 1, 
           CHOMP(ReplacedString(line, "return ACEfunc", NameFunction(ACEfunc)))
           );
    fi;
    FLUSH_ACE_STREAM_UNTIL( instream, 1, 3, ReadLine, line -> line = fail );
    SetInfoACELevel( infoACElevel );
    if name <> "index" then
      return ACE_READ_AS_FUNC(file, ACEfunc);
    fi;
end);

#############################################################################
####
##
#F  CallACE . . . . . . . . . . . . . . . . . . . . . . . . . . .  deprecated
##
InstallGlobalFunction(CallACE, function(arg)

  Error("CallACE is deprecated: Use `ACECosetTableFromGensAndRels' or\n",
        "`ACECosetTable'.\n");
end);

#E  general.gi  . . . . . . . . . . . . . . . . . . . . . . . . . . ends here 
