#############################################################################
####
##
#W  ace.g                   ACE Share Package                Alexander Hulpke
#W                                                                Greg Gamble
##
##  This  file  contains the interface to the ACE (Advanced Coset Enumerator)
##  binary, written by George Havas and Colin Ramsay.  The original interface
##  was written by Alexander Hulpke and modified by Greg Gamble.
##    
#H  @(#)$Id$
##
#Y  Copyright (C) 2000  Centre for Discrete Mathematics and Computing
#Y                      Department of Computer Science & Electrical Eng.
#Y                      University of Queensland, Australia.
##
Revision.ace_g :=
    "@(#)$Id$";


#############################################################################
####
##
#V  ACEData . . . . record used by various functions of the ACE share package
##
##  The fields of ACEData are:
##
##    "binary"  . . the path of the ACE binary
##    "tmpdir"  . . the path of the temporary directory for ACE i/o files
##    "io . . . . . list of data records for StartACE IO Streams
##    "infile"  . . the path of the ACE input file
##    "outfile" . . the path of the ACE output file
##    "banner"  . . the path of the file where ACE's banner is directed
##    "version" . . the version of the current ACE binary
##
ACEData := rec( binary := Filename(DirectoriesPackagePrograms("ace"), "ace"),
                tmpdir := DirectoryTemporary(),
                io := [] # Initially no StartACE IO Streams
              );
ACEData.infile  := Filename(ACEData.tmpdir,"in"); 
ACEData.outfile := Filename(ACEData.tmpdir,"out");
ACEData.banner := Filename(ACEData.tmpdir,"banner");

PrintTo(ACEData.infile, "\n");
# Fire up ACE with a null input (ACEData.infile contains only a "\n")
# ... to generate a banner (which has ACE's current version)
Exec( Concatenation("cd ", Filename(ACEData.tmpdir, ""), "; ",
                    ACEData.binary, " < in > banner") );
# For now use ACEData.scratch to record an input stream
ACEData.scratch := InputTextFile(ACEData.banner);
# Grab the first line of banner, which begins like: "ACE N.nnn   "
ACEData.version := ReadLine(ACEData.scratch);
CloseStream(ACEData.scratch);
# We just want the N.nnn part of the first line of banner
# ... ACEData.scratch now records where N.nnn starts
ACEData.scratch := PositionSublist(ACEData.version, "ACE") + 4;
ACEData.version := ACEData.version{[ACEData.scratch ..
                                    Position(ACEData.version, ' ', 
                                             ACEData.scratch) - 1]};
Unbind(ACEData.scratch); # We don't need ACEData.scratch, anymore.

#############################################################################
####
##  Print a banner . . . . . .  using InfoWarning (so a user can turn it off)
##
Info(InfoWarning,1,"Loading the ACE (Advanced Coset Enumerator) share package");
Info(InfoWarning,1,"         by George Havas <havas@csee.uq.edu.au> and");
Info(InfoWarning,1,"            Colin Ramsay <cram@csee.uq.edu.au>");
Info(InfoWarning,1,"                 ACE binary version: ", ACEData.version);

#############################################################################
####
##
#F  IS_INC_POS_INT_LIST . . . . . . Internal function used in KnownACEoptions
##  . . . . . . . .  returns true if argument is a single positive integer or
##  . . . . . . . . . . .  is a strictly increasing list of positive integers
##
IS_INC_POS_INT_LIST
  := x -> IsPosInt(x) or 
          (IsSSortedList(x) and IsPosInt(x[1]) and x[1] < x[2]);

#############################################################################
####
##
#V  KnownACEoptions . . . . . . record whose fields are the known ACE options
##  . . . . . . . . . . . . . . . . . .  each field is assigned a list of two 
##  . . . . . . . . . . . . . . . . . .  components:  [leastlength, listorfn]
##
##  The known ACE options  are the RecNames of KnownACEoptions.  The value of 
##  of each RecName is a list [ leastlength, listorfn ], where leastlength is
##  an integer specifying the least length of an abbreviation of the  RecName
##  that will match an ACE option,  and listorfn is either a list of  allowed 
##  values or a function that can be used to test that the value of an option 
##  is valid e.g. for the RecName "lookahead", we have knownOptions.lookahead 
##  equal to [ 4, [0..4] ] which indicates that "look", "looka", etc. are all 
##  valid abbreviations of the "lookahead" option,  and the values  that that 
##  option can take are in the (integer) range 0 to 4. 
##
##  If the allowed values listed for an option are 0 and 1,  then  false  and 
##  true are also permitted (we translate false and true to 0 and 1, respect-
##  ively when we call ACE). The empty string signifies that ACE  expects  no
##  value for that option.
##
##  Only single-word versions of options can be used by a user of ACE via the 
##  GAP interface e.g. "cc" is a synonym for "coset coincidence"  as  an  ACE
##  option,  but the latter,  being 2 words,  is  not available via  the  GAP 
##  interface.
##
##  The commented out options are known ACE options that probably  won't work
##  via the GAP interface ... if the user uses these  the  interface  program
##  START_ACE will complain: `unknown (possibly new) or bad'  but  still pass 
##  these options to ACE ... at least the user will then know if ACE does not
##  respond as expected that the options should not be used.  We usually only 
##  warn that certain options might be bad, so that this interface has a good
##  chance of still being functional if new options are added to the ACE bin-
##  ary.
##
##  Some  options  are  `GAP-introduced'  i.e. technically they are  not  ACE 
##  options  ...  there is a comment beside such options;  and  they are also 
##  listed in NonACEbinOptions below.
##

KnownACEoptions := rec(
  aceinfile := [9, IsString], # This is a GAP-introduced option 
                              # (not an ACE binary option)
  nowarn := [6, [0,1]],       # This is a GAP-introduced option 
                              # (not an ACE binary option)
  #sg := [2, <word list>],
  #rl := [2, <relation list>],
  aep  := [3, [1..7]],
  #ai := [2, <filename>],
  ao   := [2, IsString],      # "ACEoutfile" is a GAP-introduced 
  aceoutfile := [8, IsString],# `synonym' for "ao"
  asis := [2, [0,1]],
  #begin := [3, [""]], start := [5, [""]], end := [3, [""]],
  #bye := [3, [""]], exit := [4, [""]], quit := [1, [""]],
  cc   := [2, x -> IsInt(x) and x > 1],
  cfactor := [1, IsInt],      # "cfactor" and "ct" are synonyms
  ct   := [2, IsInt],
  #check := [5, [""]], redo := [4, [""]],
  compaction := [3, [0..100]],
  #continue := [4, [""]],
  cycles := [2, [""]],
  dmode := [4, [0..4]],
  dsize := [4, x -> IsZero(x) or IsPosInt(x)],
  default := [3, [""]],
  ds := [2, IS_INC_POS_INT_LIST],
  dr := [2, IS_INC_POS_INT_LIST],
  #dump := [1, x -> (IsList(x) and x[1] in [0..2] and
  #                  (Length(x) = 1 or (Length(x) = 2 and x[2] in [0,1])) or
  #                 x in [0..2]],
  easy := [4, [""]],
  echo := [4, [0,1]],         # hijacked! ... we don't pass this to ACE
  enumeration := [4, IsString],
  felsch := [3, ["",0,1]],
  ffactor := [1, x -> IsZero(x) or IsPosInt(x)],# "ffactor" and "fill"
  fill := [3, x -> IsZero(x) or IsPosInt(x)],   # are synonyms ... there is
                                                # no "fi" since it's a GAP
                                                # keyword
  ## The next 3 ACE options are done via START_ACE arguments
  #group := [2, [<letter list> / int] ],        # For group generators
  #generators := [3, <word list>],              # For subgroup generators
  #relators := [3, <word list>],                # For group relators
  hard := [2, [""]],
  #help := [4, [""]],
  hlt  := [3, [""]],
  hole := [4, [-1..100]],
  lookahead := [4, [0..4]],
  loop := [4, x -> IsZero(x) or IsPosInt(x)],
  max  := [3, x -> IsZero(x) or (IsInt(x) and x >= 2)],
  mendelsohn := [4, [0,1]],
  messages := [4, IsInt],   # "messages" and "monitor" are synonyms
  monitor := [3, IsInt],
  #mode := [2, [""]],
  nc   := [2, ["",0,1]],    # "nc" and "normal" are synonyms
  normal := [6, [0,1]],
  no   := [2, x -> IsInt(x) and x >= -1],
  #options := [3, [""]],
  oo   := [2, IsInt],       # "oo" and "order" are synonyms
  order := [2, IsInt],
  #parameters := [3, [""]], # decommissioned ACE option
  path := [4, [0,1]],
  pmode := [4, [0..3]],
  psize := [4, x -> IsZero(x) or 
                   (IsEvenInt(x) and IsPrimePowerInt(x))],
  silent := [6, [0,1]],     # This is a GAP-introduced option 
                            # (not an ACE binary option)
  #sr := [2, [0,1]],
  #print := [2, x -> (IsList(x) and Length(x) <= 3 and
  #                  ForAll(x, IsInt)) or IsInt(x)],
  purec := [5, [""]],       # the ACE option is "pure c"
  purer := [5, [""]],       # the ACE option is "pure r"
  rc   := [2, x -> (IsList(x) and Length(x) <= 2 and
                   ForAll(x, IsInt)) or IsInt(x)],
  recover := [4, [""]],     # "recover" and "contiguous"
  contiguous := [6, [""]],  # are synonyms ... "rec" is
                            # not an allowed abbreviation
                            # since it's a GAP  keyword
  rep  := [2, x -> (IsList(x) and Length(x) <= 2 and
                   ForAll(x, IsInt) and x[1] in [1..7]) or
                   x in [1..7]],
  #restart := [7, [""]],    # decommissioned ACE option
  rfactor := [1, IsInt],    # "rfactor" and "rt" are synonyms
  rt   := [2, IsInt],
  row  := [3, [0,1]],
  sc   := [2, IsInt],       # "sc" and "stabilising" are synonyms
  stabilising := [6, IsInt],
  sims := [4, [1,3,5,7,9]],
  standard := [2, [""]],
  statistics := [4, [""]],  # "statistics" and "stats" are synonyms
  stats := [5, [""]],
  #style := [5, [""]],
  subgroup := [4, IsString],
  system := [3, IsString],
  text := [4, IsString],
  time := [2, x -> IsInt(x) and x >= -1],
  #tw := [2, [int,<word>]], trace := [5, [int,<word>]],
  workspace := [2, x -> IsInt(x) or 
                        (IsString(x) and 
                         x[Length(x)] 
                             in "0123456789kmgKMG")]
  );

#############################################################################
####
##
#V  NonACEbinOptions . . . . . . . list of known ACE options that are not ACE
##  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . binary options.
##

NonACEbinOptions := [ "aceinfile", "nowarn", "echo", "aceoutfile", "silent" ];

#############################################################################
####
##
#F  WRITE_LIST_TO_STREAM 
##
##
BindGlobal("WRITE_LIST_TO_STREAM", function(writefn, iostream, list)
local s, Massage;

  if writefn = AppendTo then
    Massage := s -> s;
  else
    Massage := String;
  fi;

  for s in list do
    writefn(iostream, Massage(s));
  od;
end);

#############################################################################
####
##
#F  START_ACE . . . . . . . .  Called by ACECosetTable, ACEStats and StartACE
##
##
BindGlobal("START_ACE", function(ACEfname, fgens, rels, sgens)
local n, nums, options, MatchesKnownOption, FullOptionName, known, echo,
      GetOptionValue, input, ToACE, outfile, IsLowercaseOneCharGen, gens, 
      DoWords, IsValidOptionValue, CheckValidOption, ProcessOption, opt, 
      fullopt, optval, donothing, i; 

  n := Length(fgens);
  if ForAny(fgens, i -> NumberSyllables(i)<>1 or ExponentSyllable(i, 1)<>1) then
    Error("first argument not a valid list of group generators");
  fi;
  nums := List(fgens, i -> GeneratorSyllable(i, 1));
  IsSSortedList(nums); # force sort flag

  if IsEmpty(OptionsStack) then
    options := [];
  else
    options := RecNames(OptionsStack[ Length(OptionsStack) ]);
  fi;
  if "messfile" in options then
    Error("Option `messfile' deprecated: use `ACEoutfile' instead");
  elif "outfile" in options then
    Error("Option `outfile' deprecated: use `ACEinfile' instead");
  fi;

  MatchesKnownOption := function(knownoption, option)
    # Checks if option is a valid abbreviation of knownoption
    # ... knownoption should be a field of KnownACEoptions
    # ... option should be in lowercase already!
    return option = knownoption{[1..Length(option)]} and
           KnownACEoptions.(knownoption)[1] <= Length(option);
  end;

  FullOptionName := function(option)
    # Returns the unabbreviated version of option, provided one
    # exists among the fields of KnownACEoptions. If not, option
    # is returned. As a side-effect the CALL_ACE variable known
    # is set to true if the search for an unabbreviated version
    # of option was successful (and false otherwise).
    # ... option need not be in lowercase already.
    local lcaseopt, list;
    lcaseopt := LowercaseString(option);
    list := Filtered(RecNames(KnownACEoptions), 
                     s -> MatchesKnownOption(s, lcaseopt));
    known := not( IsEmpty(list) ); # Set the CALL_ACE variable `known'
                                   # to indicate option is one of the
                                   # known ACE options
    if known then
        return list[1];  # We assume any match is unique!
    else
        return option;
    fi;
  end;

  # We have hijacked ACE's echo option ... we don't actually pass it to ACE
  echo := ValueOption("echo") = true;

  if echo then
    Print(ACEfname, " called with the following arguments:\n");
    Print(" Group generators : ", fgens, "\n");
    Print(" Group relators : ", rels, "\n");
    Print(" Subgroup generators : ", sgens, "\n");
  fi;
  
  GetOptionValue := function(defaultvalue, list)
    # Check among options (a CALL_ACE variable defined above) for any 
    # settings of the known option synonyms in list. The latest such
    # option in options will prevail and its value will be returned.
    # Otherwise, if there isn't such an option, defaultvalue is returned.
    local option, optval;
    optval := defaultvalue;
    for option in Filtered(options, 
                           option -> 
                              ForAny(list, 
                                     s -> MatchesKnownOption(
                                              s, 
                                              LowercaseString(option)
                                              ))) 
    do
      optval := ValueOption(option);
    od;
    return optval;
  end;
  
  if ACEfname <> "StartACE" then
    if ACEfname = "ACECosetTableFromGensAndRels" then
      # If option "ACEinfile" is set we only want to produce an ACE input file
      input := GetOptionValue(ACEData.infile, [ "aceinfile" ]);
    elif ACEfname = "ACEStats" then
      input := ACEData.infile; # If ACEinfile option used ... we ignore it
    fi;
    PrintTo(input, ""); # Just so we can use AppendTo from here on
    ToACE := function(list) WRITE_LIST_TO_STREAM(AppendTo, input, list); end;
  else  # ACEfname = "StartACE"
    input := InputOutputLocalProcess(ACEData.tmpdir, ACEData.binary, []);
    ToACE := function(list) WRITE_LIST_TO_STREAM(WriteAll, input, list); end;
  fi;
  outfile := GetOptionValue(ACEData.outfile, [ "ao", "aceoutfile" ]);

  # Give a name to the group ACE will be dealing with (this is not
  # actually necessary ... ACE essentially treats it as a comment)
  ToACE([ "Group Name: ", GetOptionValue("G", ["enumeration"]), ";\n" ]);
  
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
      # Try to use the user's set of generators for ACE ... if we can
      gens := List(fgens, g -> String(g));
    else
      gens := List([1..n], i -> WordAlp(CHARS_LALPHA, i));
    fi;
    ToACE( [ Flat([gens, ";\n"]) ] );
  else
    # if #generators > 26 tell ACE to use numerical generators: 1 ...
    ToACE([ n, ";\n" ]);
    gens := [1..n];
  fi;

  DoWords := function(words)
    # Takes a GAP List of words and produces the corresponding ACE list
    local i, j, m, e, p;
    for i in [1..Length(words)] do
      m := NumberSyllables(words[i]);
      for j in [1..m] do
        p := Position(nums, GeneratorSyllable(words[i], j));
        e := ExponentSyllable(words[i], j);
        ToACE([ gens[p] ]);
        if e<>1 then
          ToACE([ "^", e ]);
        fi;
        if j<m then
          ToACE([ "*" ]);
        fi;
      od;
      if i<Length(words) then
        ToACE([ "," ]);
      fi;
    od;
    ToACE([ ";\n" ]);
  end;

  # Define the group relators ACE will use
  ToACE([ "Group Relators: " ]);
  DoWords(rels);

  # Give a name to the subgroup ACE will be dealing with (this is not
  # actually necessary ... ACE essentially treats it as a comment)
  ToACE([ "Subgroup Name: ", GetOptionValue("H", ["subgroup"]), ";\n" ]);
  
  # Define the subgroup generators ACE will use
  ToACE([ "Subgroup Generators:" ]);
  DoWords(sgens);

  # Now we look at options other than "enum...", "subg..." and "echo".
  # When we process the remaining options, we do so in the order
  # supplied by the user ... in general ACE options are NOT orthogonal
  # and so it's important to process them in the user-defined order.

  if echo then
    Print(ACEfname, " called with the following options:\n");
  fi;

  IsValidOptionValue := function(value)
    # Check that value is a valid value of fullopt.
    # This function will only be called when known = true,
    # in which case, fullopt will be a field of KnownACEoptions
    if IsFunction(KnownACEoptions.(fullopt)[2]) then
      return KnownACEoptions.(fullopt)[2](value);
    elif IsBool(value) then
      return KnownACEoptions.(fullopt)[2] in ["", ["",0,1], [0,1]];
    else
      return value in KnownACEoptions.(fullopt)[2];
    fi;
  end;

  CheckValidOption := function(value)
    # Check that opt is a known option and has a valid value.
    # If not warn the user that s/he may have made an error.
    if known then
      if not IsValidOptionValue(value) then
        Print(" #Warning: ", value, 
              ": possibly not an allowed value of ", opt, ".\n");
      fi;
    else
      if LowercaseString(opt{[1..3]}) in ["rel", "gen"] or
         LowercaseString(opt{[1..2]}) = "gr" then
        Print(" #Warning: ", opt, 
              ": should be passed as an argument, NOT an option.\n");
      elif not GetOptionValue(false, ["nowarn"]) then
        Print(" #Warning: ", opt, ": unknown (maybe new) or bad option.\n");
      fi;
    fi;
  end;

  ProcessOption := function(value)
    local aceopt;
    if not donothing then
      if fullopt{[1..4]} = "pure" then
        # Options "pure r" and "pure c" are the only ACE options
        # for which ACE did not have a single-word alternative.
        # So the corresponding GAP options are "purer" and "purec",
        # respectively. Here we reconstruct what ACE expects.
        aceopt := Concatenation(opt{[1..4]}, " ", opt{[5]});
      else
        # The ACE option is the same as the GAP option
        aceopt := opt;
      fi;
      if value = "" then
        ToACE([ aceopt, ";\n" ]);
      else
        ToACE([ aceopt, ":", value, ";\n" ]);
      fi;
    fi;
    if echo then
      if fullopt in NonACEbinOptions then
        # First deal with the non-ACE binary options
        if fullopt = "aceoutfile" and ACEfname<>"StartACE" then
          Print(" ", opt, " := ", optval, " (passed to ACE via option: ao)\n");
        elif (fullopt = "aceoutfile" and ACEfname = "StartACE") or
             (fullopt = "aceinfile" and
              ACEfname<>"ACECosetTableFromGensAndRels") then
          Print(" ", opt, " := ", optval, " (ignored)\n");
        else
          Print(" ", opt, " := ", optval, " (not passed to ACE)\n");
        fi;
      elif value = "" then
        Print(" ", opt, " (no value)\n");
      else
        Print(" ", opt, " := ", value, "\n");
      fi;
    fi;
    # Warn user if opt is an option is unknown or has an unexpected value
    CheckValidOption(value);
  end;

  for opt in options do
    fullopt := FullOptionName(opt); # known is set here as a side-effect
    # We don't pass the options in the RHS list following to the
    # ACE infile, here (i.e. within this `for' loop).
    # The options "echo", "ACEinfile" and "silent" are not passed to the ACE 
    # binary at all, and "ACEoutfile" is only passed to the ACE binary as "ao".
    # We have already dealt with the options  "ACEinfile",  "ao", "ACEoutfile", 
    # "enumeration" and "subgroup".
    donothing := fullopt in ["echo", "aceinfile", "ao", "aceoutfile",
                             "silent", "enumeration", "subgroup" ];
    optval := ValueOption(opt);
    if optval = true then
      # An option detected by GAP as boolean may in fact be a no-value
      # option of ACE ... unknown ACE options detected as being true are
      # assumed to be no-value options (since the user can still over-ride
      # this behaviour by entering values of 0 or 1 explicitly e.g. 
      # ACEStats(... : opt := 1) )
      if not known or IsValidOptionValue("") then
        ProcessOption("");
      else
        ProcessOption(1);
      fi; 
    elif optval = false then
      ProcessOption(0);
    elif IsString(optval) then
      ProcessOption(optval);
    elif IsList(optval) then
      # ProcessOption() is not designed to cope with a list
      # ... we do it `manually'.
      if not donothing then
        ToACE([ opt,":", optval[1] ]);
        for i in [2..Length(optval)] do
          ToACE([ ",",optval[i] ]);
        od;
        ToACE([ ";\n" ]);
      fi;
      if echo then 
        Print(" ",opt," := ", optval, " (brackets are not passed to ACE)\n");
      fi;
      CheckValidOption(optval);
    else
      ProcessOption(optval);
    fi;
  od;
              
  if ACEfname <> "StartACE" then
    # Direct ACE output to outfile if called via
    # ACECosetTableFromGensAndRels or ACEStats
    ToACE([ "Alter Output:", outfile, ";\n" ]);
    ToACE([ "End;\n" ]); # (one of) the ACE directives that initiate
                         # an enumeration

    if ACEfname = "ACECosetTableFromGensAndRels" then
      ToACE([ "Print Table;\n" ]);
    fi;

    if ACEfname = "ACEStats" or input = ACEData.infile then
      # Run ACE on the constructed input
      # ... the ACE output will appear in outfile 
      #     (except for the banner which is directed to ACEData.banner)
      Exec(Concatenation(ACEData.binary, "<", input, ">", ACEData.banner));
    fi;
  fi;

  if ACEfname = "ACECosetTableFromGensAndRels" then
    return rec(gens := gens, 
               infile := input, 
               outfile := outfile,
               silent := GetOptionValue(false, ["silent"]));
  elif ACEfname = "ACEStats" then
    return outfile;
  else # ACEfname = "StartACE"
    Add(ACEData.io, rec(gens := gens, stream := input));
    return Length(ACEData.io);
  fi;
end);

#############################################################################
####
##
#F  ACECosetTableFromGensAndRels . . . . . . .  Non-interactive ACECosetTable
##
BindGlobal("ACECosetTableFromGensAndRels", function(fgens, rels, sgens)
  # Use ACECosetTable non-interactively
  return ACECosetTable(fgens, rels, sgens);
end);

#############################################################################
####
##
#V  ACETCENUM . . . . . . . .  The ACE version of the coset enumerator TCENUM
##  . . . .  CosetTableFromGensAndRels is set to ACECosetTableFromGensAndRels
##
BindGlobal("ACETCENUM", rec(name := "ACE-enumerator",
                            CosetTableFromGensAndRels 
                                := ACECosetTableFromGensAndRels));

#############################################################################
####
##
#F  CallACE . . . . . . . . . . . . . . . . . . . . . . . . . . .  deprecated
##
BindGlobal("CallACE", function(arg)

  Error("CallACE is deprecated: Use `ACECosetTableFromGensAndRels' or\n",
        "`ACECosetTable'.\n");
end);

#############################################################################
####
##
#F  FlushOptionsStack . . . . . . . . . . . Pops all options off OptionsStack
##
##
BindGlobal("FlushOptionsStack", function()
  while not(IsEmpty(OptionsStack)) do
    PopOptions();
  od;
end);

#E  ace.g . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here 
