#############################################################################
####
##
#W  ace.g                   ACE Share Package                     Greg Gamble
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
#V  ACEinfo . . . . record used by various functions of the ACE share package
##
##  The fields of ACEinfo are:
##
##    "binary"  . . the path to the ACE binary
##    "tmpdir"  . . the path to the temporary directory for ACE i/o files
##    "infile"  . . the path of the ACE input file
##    "outfile" . . the path of the ACE output file
##    "file"  . . . the input stream (file-handle) of ACEinfo.outfile
##    "version" . . the version of the current ACE binary
##
ACEinfo := rec( binary := Filename(DirectoriesPackagePrograms("ace"), "ace"),
                tmpdir := DirectoryTemporary()
              ); # to store file and directory names
ACEinfo.infile  := Filename(ACEinfo.tmpdir,"in");
ACEinfo.outfile := Filename(ACEinfo.tmpdir,"out");

PrintTo(ACEinfo.infile, "\n");
# Fire up ACE to extract the version of the current binary
Exec( Concatenation("cd ", Filename(ACEinfo.tmpdir, ""), "; ",
                    ACEinfo.binary, " < in > out") );
ACEinfo.file := InputTextFile(ACEinfo.outfile);
ACEinfo.version := ReadLine(ACEinfo.file);
CloseStream(ACEinfo.file);
ACEinfo.file := PositionSublist(ACEinfo.version, "ACE") + 3;
ACEinfo.version := ACEinfo.version{[ACEinfo.file+1..Position(ACEinfo.version,
                                    ' ', ACEinfo.file)-1]};

#############################################################################
####
##  Print a banner
##
Print("#I  Loading the ACE (Advanced Coset Enumerator) share package,\n");
Print("#I           by George Havas <havas@csee.uq.edu.au> and\n");
Print("#I              Colin Ramsay <cram@csee.uq.edu.au>\n");
Print("#I  Using ACE binary version ", ACEinfo.version, "\n");

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
##  GAP4 interface e.g. "cc" is a synonym for "coset coincidence" as  an  ACE
##  option,  but the latter,  being 2 words,  is not available via  the  GAP4 
##  interface.
##
##  The commented out options are known ACE options that probably  won't work
##  via the GAP4 interface ... if the user uses these the  interface  program
##  CallACE will flag these as `unknown (possibly new) or bad' but still pass 
##  these options to ACE ... at least the user will then know if ACE does not
##  respond as expected that the options should not be used.  We usually only 
##  warn that certain options might be bad, so that this interface has a good
##  chance of still being functional if new options are added to the ACE bin-
##  ary.
##
KnownACEoptions := rec(
  #sg := [2, <word list>],
  #rl := [2, <relation list>],
  aep  := [3, [1..7]],
  #ai := [2, <filename>],
  ao   := [2, IsString], # "messfile" is a GAP4-introduced `synonym' for "ao"
  messfile := [8, IsString],
  asis := [2, [0,1]],
  #begin := [3, [""]], start := [5, [""]], end := [3, [""]],
  #bye := [3, [""]], exit := [4, [""]], quit := [1, [""]],
  cc   := [2, x -> IsInt(x) and x > 1],
  cfactor := [1, IsInt],    # "cfactor" and "ct" are synonyms
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
  echo := [4, [0,1]],       # hijacked! ... we don't pass this to ACE
  enumeration := [4, IsString],
  felsch := [3, ["",0,1]],
  ffactor := [1, x -> IsZero(x) or IsPosInt(x)],# "ffactor" and "fill"
  fill := [3, x -> IsZero(x) or IsPosInt(x)],   # are synonyms ... there is
                                                # no "fi" since it's a GAP4
                                                # keyword
  ## The next 3 ACE options are done via CallACE arguments
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
  outfile := [7, IsString], # This is a GAP4-introduced
                            # option (not an ACE option)
  #parameters := [3, [""]], # decommissioned ACE option
  path := [4, [0,1]],
  pmode := [4, [0..3]],
  psize := [4, x -> IsZero(x) or 
                   (IsEvenInt(x) and IsPrimePowerInt(x))],
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
                            # since it's a GAP4  keyword
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
#F  CALL_ACE  . . . . .  The function that actually does the work when either
##  . . . . . . . . . . . . . . . . CallACE or ACEStats is called by the user
##
BindGlobal("CALL_ACE", function(arg)
local fgens,rels,sgens,a,i,j,k,n,nums,fullopt,opt,optval,options,known,
      donothing,echo,gens,CheckValidOption,DoWords,FullOptionName,
      GetOptionValue,IsValidOptionValue,MatchesKnownOption,ProcessOption,infile,
      line,col,ok,p,rowentries,table,cosettable,redir,stats,statval,words,
      ACEfname;

  fgens := arg[1];
  rels := arg[2];
  sgens := arg[3];
  # only do stats
  stats := Length(arg)>3 and arg[4]=1;

  if stats then 
    ACEfname := "ACEStats";
  else
    ACEfname := "CallACE";
  fi;

  n := Length(fgens);
  if ForAny(fgens, i -> NumberSyllables(i)<>1 or ExponentSyllable(i, 1)<>1) then
    Error("first argument not a valid list of group generators");
  fi;
  nums := List(fgens, i -> GeneratorSyllable(i, 1));
  IsSSortedList(nums); # force sort flag

  options := RecNames(OptionsStack[ Length(OptionsStack) ]);

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
  
  # If option "outfile" is set then we only want to produce an ACE input file
  optval := ValueOption("outfile");
  if optval<>fail and IsString(optval) then
    infile := optval;
  else
    infile := ACEinfo.infile;
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
    
  # Give a name to the group ACE will be dealing with (this is not
  # actually necessary ... ACE essentially treats it as a comment)
  PrintTo(infile, "Group Name: ", 
          GetOptionValue("G", ["enumeration"]), 
          ";\n");
  
  # Define the generators ACE will use
  AppendTo(infile,"Group Generators: ");
  if n <= 26 then
    # if #generators <= 26 tell ACE to use alphabetic generators: a ...
    AppendTo(infile, CHARS_LALPHA{[1..n]}, ";\n");
    gens := List([1..n], i -> WordAlp(CHARS_LALPHA{[1..n]}, i));
  else
    # if #generators > 26 tell ACE to use numerical generators: 1 ...
    AppendTo(infile, n, ";\n");
    gens := [1..n];
  fi;

  DoWords := function(words)
    # Takes a GAP List of words and produces the corresponding ACE list
    local i,j,m,e,p;
    for i in [1..Length(words)] do
      m := NumberSyllables(words[i]);
      for j in [1..m] do
        p := Position(nums, GeneratorSyllable(words[i], j));
        e := ExponentSyllable(words[i], j);
        AppendTo(infile, gens[p]);
        if e<>1 then
          AppendTo(infile, "^", e);
        fi;
        if j<m then
          AppendTo(infile,"*");
        fi;
      od;
      if i<Length(words) then
        AppendTo(infile,",");
      fi;
    od;
    AppendTo(infile,";\n");
  end;

  # Define the group relators ACE will use
  AppendTo(infile,"Group Relators: ");
  DoWords(rels);

  # Give a name to the subgroup ACE will be dealing with (this is not
  # actually necessary ... ACE essentially treats it as a comment)
  AppendTo(infile, "Subgroup Name: ", 
           GetOptionValue("H", ["subgroup"]),
           ";\n");
  
  # define the subgroup generators ACE will use
  AppendTo(infile,"Subgroup Generators:");
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
      else
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
        # So the corresponding GAP4 options are "purer" and "purec",
        # respectively. Here we reconstruct what ACE expects.
        aceopt := Concatenation(opt{[1..4]}, " ", opt{[5]});
      else
        # The ACE option is the same as the GAP4 option
        aceopt := opt;
      fi;
      if value = "" then
        AppendTo(infile, aceopt, ";\n");
      else
        AppendTo(infile, aceopt, ":", value, ";\n");
      fi;
    fi;
    if echo then
      if fullopt in ["echo", "outfile"] then
        # First deal with the non-ACE options
        Print(" ", opt, " := ", optval, " (not passed to ACE)\n");
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
    # We don't pass the options in the RHS list following to the
    # ACE infile, here (i.e. within this `for' loop).
    fullopt := FullOptionName(opt); # known is set here as a side-effect
    donothing := fullopt in ["echo", "outfile", "ao", "messfile", 
                             "enumeration", "subgroup", 
                             "messages", "monitor"];
    optval := ValueOption(opt);
    if optval = true then
      # An option detected by GAP4 as boolean may in fact be a no-value
      # option of ACE ... unknown ACE options detected as being true are
      # assumed to be no-value options (since the user can still over-ride
      # this behaviour by entering values of 0 or 1 explicitly e.g. 
      # CallACE(... : opt := 1) )
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
        AppendTo(infile, opt,":", optval[1]);
        for i in [2..Length(optval)] do
          AppendTo(infile, ",",optval[i]);
        od;
        AppendTo(infile, ";\n");
      fi;
      if echo then 
        Print(" ",opt," := ", optval, " (brackets are not passed to ACE)\n");
      fi;
      CheckValidOption(optval);
    else
      ProcessOption(optval);
    fi;
  od;
              
  # Set the verbosity of the ACE output
  optval := GetOptionValue(fail, ["messages", "monitor"] );
  if optval<>fail and not stats then
    if IsInt(optval) and (optval<-1 or optval>1) then
      AppendTo(infile, "Mess:", optval, ";\n");
    fi;
    redir := "";
  else
    redir := "> temp";
  fi;

  # The user may wish to direct the ACE output to a file
  optval := GetOptionValue(fail, ["ao", "messfile"] );
  if optval<>fail then
    AppendTo(infile, "AO:", optval, ";\n");
    redir := "> temp";
  fi;

  AppendTo(infile, "AO:", ACEinfo.outfile, ";\n");
  AppendTo(infile, "End;\n"); # (one of) the ACE directives that initiate
                              # an enumeration
  if stats = false then
    AppendTo(infile, "Print Table;\n");
  fi;

  if infile<>ACEinfo.infile then
    # We only wanted to write an ACE input file
    return;
  fi;

  # Run ACE on the constructed infile ... the ACE output will
  # appear in ACEinfo.outfile
  Exec(Concatenation("cd ", Filename(ACEinfo.tmpdir, ""), "; ",
                     ACEinfo.binary, " <in ", redir));

  # We now have a look at the ACE output
  infile := InputTextFile(ACEinfo.outfile);
  line := ReadLine(infile);

  # Parse the line for statistics
  a := Filtered(line,i->i in ". " or i in CHARS_DIGITS);
  a := SplitString(a, "", " .");
  if stats then 
    if line{[1..5]}="INDEX" then
      statval:=[Int(a[1]),Int(a[7])+Int(a[8])/10^Length(a[8]),
		Int(a[9]),Int(a[10])];
    else
      statval:=[0,Int(a[6])+Int(a[7])/10^Length(a[7]),Int(a[8]),Int(a[9])];
    fi;

    CloseStream(infile);
    return statval;
  fi;

  # Skip some header until the ` coset ' line
  while line<>fail and line{[1..6]}<>" coset" do
    line := ReadLine(infile);
  od;

  if line = fail then
    PopOptions();
    Error("no coset table output from ACE?");
  fi;

  # Look at the coset table header and determine the column
  # corresponding to each generator:
  #   col[i] = column(gens[i])
  col := Filtered(List(gens,i -> 
                       Position(# List of coset table column headers
                                SplitString(line, "", " |\n"),
                                String(i))),
                  x -> x<>fail );

  # Discard the `---' line
  line := ReadLine(infile);

  # Now read the body of the coset table into table as a GAP4 list
  table := List([1..n],i -> []);
  p := 0;
  ok := true;
  line := ReadLine(infile);
  while ok and line<>fail and Length(line)>1 and line[1]<>'=' do
    p := p+1;
    rowentries := SplitString(line, ""," :|\n");
    # Syntax check
    if Int(rowentries[1])=fail or Int(rowentries[1])<>p then
      PopOptions();
      Error("syntax error in ACE output file");
    fi;
    for i in [1..n] do
      a := Int(rowentries[ col[i] ]);
      ok := a<>0;
      Add(table[i], a);
    od;

    line := ReadLine(infile);
  od;

  CloseStream(infile);
  if not ok then
    return fail;
  fi;

  cosettable := [];
  n := Length(table[1]);
  for i in table do
    Add(cosettable, i);
    if ForAll([1..n], j->i[i[j]] = j) then
      Add(cosettable, i);
    else
      j := [];
      for k in [1..n] do
        j[i[k]] := k;
      od;
      Add(cosettable, j);
    fi;
  od;
  return cosettable;
end);

#############################################################################
####
##
#F  CallACE . . . . . . . . .  The ACE version of the coset enumerator TCENUM
##
BindGlobal("CallACE", function(fgens,rels,sgens)
  local cosettable,silent;
  silent:=ValueOption("silent")=true;
  repeat
    cosettable := CALL_ACE(fgens,rels,sgens);
    if cosettable=fail then
      if silent then return fail;fi;
      Error("the coset enumeration using the `ACE' enumerator did not ",
            "complete.\n If you called the command with restrictive",
            "options relax these. Otherwise increase the memory available",
            "to `ACE'.\nType `quit;' to exit to the main loop.");
    fi;
  until cosettable<>fail;
  StandardizeTable(cosettable);
  return cosettable;
end);

#############################################################################
####
##
#V  ACETCENUM . . . . . . . .  The ACE version of the coset enumerator TCENUM
##  . . . . . . . . . . . . . . . CosetTableFromGensAndRels is set to CallACE
##
BindGlobal("ACETCENUM",rec(name:="ACE-enumerator",
        CosetTableFromGensAndRels:=CallACE));

#############################################################################
####
##
#F  ACEStats  . . . . . . . . . . . . . . . Runs CALL_ACE for statistics only
##  . . . . . . . . . . . . . . . . . . . . . . . no coset table is generated
##
BindGlobal("ACEStats", function(fgens,rels,sgens)
  return CALL_ACE(fgens,rels,sgens,1);
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
