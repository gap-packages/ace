#############################################################################
##
#W  interact.gi             ACE Share Package                     Greg Gamble
##
##  This file  installs  commands for using ACE interactively via IO Streams.
##    
#H  @(#)$Id$
##
#Y  Copyright (C) 2000  Centre for Discrete Mathematics and Computing
#Y                      Department of Computer Science & Electrical Eng.
#Y                      University of Queensland, Australia.
##
Revision.interact_gi :=
    "@(#)$Id$";

#############################################################################
####
##
#F  ACE_STREAM  . . . . . . . . . . . .  Get the index of the ACEData.io list
##  . . . . . . . . . . . . . . . . . . . . . for an interactive ACE session.
##
InstallGlobalFunction(ACE_STREAM, function(arglist)
local ioIndex;

  if IsEmpty(arglist) then
    # Find the first bound ioIndex
    ioIndex := 1;
    while not(IsBound(ACEData.io[ioIndex])) and ioIndex < Length(ACEData.io) do
      ioIndex := ioIndex + 1;
    od;
    if IsBound(ACEData.io[ioIndex]) then
      return ioIndex;
    else
      Info(InfoACE + InfoWarning, 1, 
           "No interactive ACE sessions are currently active");
      return fail;
    fi;
  else
    if IsBound(ACEData.io[ arglist[1] ]) then
      return arglist[1];
    else
      Error("No such interactive ACE session");
    fi;
  fi;
end);

#############################################################################
####
##
#F  ACE_STREAM_ARG_CHK  . . . . . . . . Checks for the right no. of arguments
##  . . . . . . . . . . . . . . . . . . warns user of any  ignored  arguments 
##
InstallGlobalFunction(ACE_STREAM_ARG_CHK, function(arglist)
  if Length(arglist) > 1 then
    Info(InfoACE + InfoWarning, 
         "Expected 0 or 1 arguments, all but first argument ignored");
  fi;
end);

#############################################################################
####
##
#F  ACEStream . . . . . . . . . . . . . . . . . .  User version of ACE_STREAM
##
##  If given (at least) one integer argument returns the first argument if it
##  corresponds  to  an  active  interactive  process  or  raises  an  error,
##  otherwise it returns the default active interactive process. If the  user
##  provides more than one argument then all arguments other than  the  first
##  argument are ignored (and a warning is issued).
##
InstallGlobalFunction(ACEStream, function(arg)
local ioIndex;

  ACE_STREAM_ARG_CHK(arg);
  return ACE_STREAM(arg);
end);

#############################################################################
####
##
#F  ACEStreams  . . . . . . . . . . . . .  Returns the list of indices of all
##  . . . . . . . . . . . . . . . . . . .  active interactive  ACE  processes
##
##
InstallGlobalFunction(ACEStreams, function()
local ioIndex;

  return Filtered( [1..Length(ACEData.io)], i -> IsBound( ACEData.io[i] ) );
end);

#############################################################################
####
##
#F  READ_ACE_ERRORS . . . . . . . . . . . . . . . . . . . . Internal function
##  . . . . . . . . . . . . . . . . . . . . reads interactive ACE output from
##  . . . . . . . . . . . . . . . . . . . . stream when none is expected;  if
##  . . . . . . . . . . . . . . . . . . . . an error is found  returns  true,
##  . . . . . . . . . . . . . . . . . . . . . . . . . . . or false otherwise.
##
##  Writes any output read to Info at InfoACE + InfoWarning level 1.
##
InstallGlobalFunction(READ_ACE_ERRORS, function(stream)
local line, errorfound;

  line := READ_ALL_LINE(stream);
  errorfound := false;
  while line <> fail do
    errorfound := errorfound or line{[1..8]} = "** ERROR";
    Info(InfoACE + InfoWarning, 1, CHOMP(line));
    line := READ_ALL_LINE(stream);
  od;
  return errorfound;
end);

#############################################################################
####
##
#F  INTERACT_TO_ACE_WITH_ERRCHK . . . . . . . . . . . . .  Internal procedure
##  . . . . . . . . . . . . . .  interactive ToACE procedure with error check
##
##  Writes list to the interactive ACE iostream stream and reads from  stream
##  to check for errors. Any output read is written  to  Info  at  InfoACE  +
##  InfoWarning level 1. Used where no output is expected.
##
InstallGlobalFunction(INTERACT_TO_ACE_WITH_ERRCHK, function(stream, list)

  WRITE_LIST_TO_ACE_STREAM(stream, list);
  READ_ACE_ERRORS(stream);
end);

#############################################################################
####
##
#F  FLUSH_ACE_STREAM_UNTIL  . . . . . . . . . . . . . . . . Internal function
##  . . . . . . . . . . . . reads lines in iostream via function readline and
##  . . . . . . . . . . . . prints   them   via    Info    at    InfoACELevel
##  . . . . . . . . . . . . infoLevelFlushed until  IsMyLine(line)  is  true,
##  . . . . . . . . . . . . prints line for which IsMyLine(line) is true  via
##  . . . . . . . . . . . . Info at InfoACELevel infoLevelMyLine and  returns
##  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  that line.
##
##
InstallGlobalFunction(FLUSH_ACE_STREAM_UNTIL, 
function(iostream, infoLevelFlushed, infoLevelMyLine, readline, IsMyLine)
local line;

  line := readline(iostream);
  while not IsMyLine(line) do
    Info(InfoACE, infoLevelFlushed, CHOMP(line));
    line := readline(iostream);
  od;
  if line <> fail then
    Info(InfoACE, infoLevelMyLine, CHOMP(line));
  fi;
  return line;
end);

#############################################################################
####
##
#F  ACE_ENUMERATION_RESULT  . . . . . Flush iostream until line ends in ")\n"
##
##  This is potentially flaky ... it relies on ACE enumeration  result  lines
##  being unique with regard to the property of not starting  with  "** "  or
##  "   " (as ACE error diagnostics do) and ending in a close bracket.
##
InstallGlobalFunction(ACE_ENUMERATION_RESULT, function(iostream, readline)
local line;

  line := FLUSH_ACE_STREAM_UNTIL(iostream, 2, 1, readline,
                                 line -> Length(line) > 1 and
                                         line[Length(line) - 1] = ')');
  if line{[1..8]} = "** ERROR" then
    line := CHOMP( readline(iostream) );
    Info(InfoACE, 1, line);
    Error("ACE Enumeration failed: ", line);
  else
    return CHOMP(line);
  fi;
end);

#############################################################################
####
##
#F  ACEWrite  . . . . . . . . . . . . . . . . . . . .  Primitive write to ACE
##
##  Writes the last argument to the i-th interactive ACE process, where i  is
##  the first argument if there are 2 arguments or  the  default  process  if
##  there is only 1 argument. The action is echoed via Info at InfoACE  level
##  3 (with a `ToACE> ' prompt).
##
InstallGlobalFunction(ACEWrite, function(arg)
local ioIndex, line;

  if Length(arg) in [1, 2] then
    ioIndex := ACE_STREAM(arg{[1..Length(arg) - 1]});
    WRITE_LIST_TO_ACE_STREAM( ACEData.io[ioIndex].stream,
                              arg{[Length(arg)..Length(arg)]} );
  else
    Error("Expected 1 or 2 arguments ... not ", Length(arg), " arguments\n");
  fi;
end);

#############################################################################
####
##
#F  ACERead . . . . . . . . . . . . . . . . . . . . . Primitive read from ACE
##
##  Reads a complete line of  ACE  output,  from  the  i-th  interactive  ACE
##  process, if there is output to be read and returns fail otherwise,  where
##  i is the first argument if there is 1 argument or the default process  if
##  there are no arguments.
##
InstallGlobalFunction(ACERead, function(arg)

  ACE_STREAM_ARG_CHK(arg);
  return READ_ALL_LINE( ACEData.io[ ACE_STREAM(arg) ].stream );
end);

#############################################################################
####
##
#F  ACEReadAll  . . . . . . . . . . . . . . . . . . . Primitive read from ACE
##
##  Reads and returns as many complete lines of ACE  output,  from  the  i-th
##  interactive ACE process, as there are to be read, as a  list  of  strings
##  with the trailing newlines removed and returns the empty list  otherwise,
##  where i is the first argument if there  is  1  argument  or  the  default
##  process if there are no arguments. Also writes via Info at InfoACE  level
##  2 each line read.
##
InstallGlobalFunction(ACEReadAll, function(arg)
local lines, stream, line;

  ACE_STREAM_ARG_CHK(arg);
  lines := [];
  stream := ACEData.io[ ACE_STREAM(arg) ].stream;
  line := READ_ALL_LINE(stream);
  while line <> fail do
    line := CHOMP(line);
    Info(InfoACE, 2, line);
    Add(lines, line);
    line := READ_ALL_LINE(stream);
  od;
  return lines;
end);

#############################################################################
####
##
#F  ACEReadUntil  . . . . . . . . . . . . . . . . . . Primitive read from ACE
##
##  Reads complete lines  of  ACE  output,  from  the  i-th  interactive  ACE
##  process, until a line for which IsMyLine(line) is true, where  i  is  the
##  first argument if there are 2 arguments or the default process  if  there
##  is one argument. The lines read are returned as a list  of  strings  with
##  the  trailing  newlines  removed.  If  IsMyLine(line)   is   never   true
##  ACEReadUntil will wait indefinitely. Also  writes  via  Info  at  InfoACE
##  level 2 each line read.
##
InstallGlobalFunction(ACEReadUntil, function(arg)
local lines, IsMyLine, stream, line;

  lines := [];
  if Length(arg) in [1, 2] then
    IsMyLine := arg[Length(arg)];
    stream := ACEData.io[ ACE_STREAM(arg{[1..Length(arg) - 1]}) ].stream;
    repeat
      line := CHOMP( READ_NEXT_LINE(stream) );
      Info(InfoACE, 2, line);
      Add(lines, line);
    until IsMyLine(line);
  else
    Error("Expected 1 or 2 arguments ... not ", Length(arg), " arguments\n");
  fi;
  return lines;
end);

#############################################################################
####
##
#F  ACE_STATS . . . . . . . . . . . . . . . . Called by ACEStart and ACEStats
##  
##
InstallGlobalFunction(ACE_STATS, function(line)
local stats;

  # Parse line for statistics and return
  stats := Filtered(line, char -> char in ". " or char in CHARS_DIGITS);
  if line{[1..5]}<>"INDEX" then
    # Enumeration failed so the index is missing 
    # ... shove a 0 index on the front of stats
    stats := Concatenation("0 ", stats);
  fi;
  stats := SplitString(stats, "", " .");

  return rec(index     := Int(stats[1]),
             cputime   := Int(stats[7])*10^Length(stats[8])+Int(stats[8]),
             cputimeUnits := Concatenation("10^-", String(Length(stats[8])),
                                           " seconds"),
             maxcosets := Int(stats[9]),
             totcosets := Int(stats[10]));
end);

#############################################################################
####
##
#F  ACE_COSET_TABLE
##
##
InstallGlobalFunction(ACE_COSET_TABLE, 
                      function(enumIndex, acegens, iostream, readline)
local n, line, colIndex, table, i, rowi, j, colj, invcolj;

  n := Length(acegens);

  # Skip some header until the ` coset ' line
  repeat
    line := readline(iostream);
    Info(InfoACE, 2, CHOMP(line));
  until line=fail or line{[1..6]}=" coset";

  # Extract the coset table column headers
  rowi := SplitString(line, "", " |\n");

  # Look at the coset table column headers and determine the column
  # corresponding to each generator:
  #   colIndex[j] = Index of column(acegens[j])
  colIndex := Filtered(List(acegens, j -> Position(rowi, String(j))),
                       x -> x<>fail);

  # Discard the `---' line
  line := readline(iostream);
  Info(InfoACE, 2, CHOMP(line));

  # Now read the body of the coset table into table as a GAP List
  table := List([1 .. 2*n], j -> []);
  i := 0;
  repeat
    line := readline(iostream);
    Info(InfoACE, 2, CHOMP(line));
    i := i + 1;
    rowi := SplitString(line, ""," :|\n");
    for j in [1..n] do
      Add(table[2*j - 1], Int(rowi[ colIndex[j] ]));
    od;
  until i = enumIndex;

  # Now we do the columns corresponding to the inverses of each generator
  for j in [1..n] do
    colj := table[2*j - 1];
    invcolj := table[2*j];
    for i in [1..enumIndex] do
      invcolj[ colj[i] ] := i;
    od;
  od;

  StandardizeTable(table);
  return table;
end);

#############################################################################
####
##
#F  ACE_MODE  . . . . . . . . . . . .  Start, continue or redo an enumeration
##  . . . . . . . . . . . .  also sets enumResult and stats fields of datarec
##
InstallGlobalFunction(ACE_MODE, function(mode, datarec)
  READ_ACE_ERRORS(datarec.stream); # purge any output not yet collected
                                   # e.g. error messages due to unknown options
  WRITE_LIST_TO_ACE_STREAM(datarec.stream, [ mode, ";" ]);
  datarec.enumResult := ACE_ENUMERATION_RESULT(datarec.stream, READ_NEXT_LINE);
  datarec.stats := ACE_STATS(datarec.enumResult);
end);
  
#############################################################################
####
##
#F  ACE_MODE_AFTER_SET_OPTS . . . . . . . Gets ACE stream index, sets options 
##  . . . . . . . . . . . . . . . . . . . and then calls ACE_MODE  to  start,
##  . . . . . . . . . . . . . . . . . . . . . continue or redo an enumeration
##
InstallGlobalFunction(ACE_MODE_AFTER_SET_OPTS, function(mode, arglist)
local ioIndex;
  ioIndex := ACE_STREAM(arglist);
  INTERACT_SET_ACE_OPTIONS(Flat( ["ACE", mode] ), ACEData.io[ioIndex]);
  ACE_MODE(mode, ACEData.io[ioIndex]);
  return ioIndex;
end);
  
#############################################################################
####
##
#F  CHEAPEST_ACE_MODE . . . . . . . . . . . . .  Does ACE_MODE(mode, datarec)
##  . . . . . . . . . . . . . . . . . . . . . for the cheapest mode available
##
InstallGlobalFunction(CHEAPEST_ACE_MODE, function(datarec)
local modes, mode;
  modes := ACE_MODES( datarec.stream );
  mode := First( RecNames(modes), ACEmode -> modes.(ACEmode) );
  if mode = fail then
    Error("None of ACEContinue, ACERedo or ACEStart is possible. Huh???");
  else
    ACE_MODE(mode{[4..Length(mode)]}, datarec);
  fi;
end);
  
#############################################################################
####
##
#F  ACEStart . . . . . . . . . . . . . .  Initiate an interactive ACE session
##
##
InstallGlobalFunction(ACEStart, function(arg)
local ioIndex;

  if Length(arg) = 1 and arg[1] = 0 then
    # Allocate an ioIndex and initiate a stream for ACEStart with 0 args
    # ... the user is on their own, having to use ACEWrite, ACERead to
    # communicate with ACE.
    Add(ACEData.io, 
        rec(#args := rec(),
            #options := ACE_OPTIONS(),
            #acegens := [], 
            stream := InputOutputLocalProcess(
                          ACEData.tmpdir, ACEData.binary, []
                          ) ) );
    return Length(ACEData.io); # ioIndex
  elif Length(arg) <= 1 then 
    return ACE_MODE_AFTER_SET_OPTS("Start", arg);
  elif Length(arg) = 3 then #args are: fgens,   rels,  sgens
    ioIndex := CALL_ACE( "ACEStart", arg[1], arg[2], arg[3] );
    ACE_MODE( "Start", ACEData.io[ioIndex] );
    return ioIndex;
  else
    Error("Expected 0, 1 or 3 arguments ... not ", Length(arg), " arguments\n");
  fi;
end);

#############################################################################
##
#F  ACEQuit . . . . . . . . . . . . . . . .  Close an interactive ACE session
##
InstallGlobalFunction(ACEQuit, function(arg)
local ioIndex;

  ioIndex := ACE_STREAM(arg);
  CloseStream(ACEData.io[ioIndex].stream);
  Unbind(ACEData.io[ioIndex]);
end);

#############################################################################
##
#F  ACEQuitAll . . . . . . . . . . . . . . Close all interactive ACE sessions
##
InstallGlobalFunction(ACEQuitAll, function()
local ioIndex;

  for ioIndex in [1 .. Length(ACEData.io)] do
    if IsBound(ACEData.io[ioIndex]) then
      CloseStream(ACEData.io[ioIndex].stream);
      Unbind(ACEData.io[ioIndex]);
    fi;
  od;
end);

#############################################################################
##
#F  ACE_MODES . . . . . . . . . .  Returns a record of which of the ACE modes
##  . . . . . . . . . . . . . . . . . . Continue, Redo and Start are possible
##
InstallGlobalFunction(ACE_MODES, function(stream)
local modes;

  READ_ACE_ERRORS(stream);
  WRITE_LIST_TO_ACE_STREAM(stream, [ "mode;" ]);
  modes := SplitString(FLUSH_ACE_STREAM_UNTIL(
                           stream, 3, 1, READ_NEXT_LINE,
                           line -> line{[1..8]} = "start = "
                           ),
                       "",
                       " =,\n");
  return rec(ACEContinue := modes[4] = "yes", # Modes in order of `cheapness'
             ACERedo     := modes[6] = "yes",
             ACEStart    := modes[2] = "yes");
end);

#############################################################################
##
#F  ACEModes  . . . . . . . . . . . .  Returns a record of which of the modes
##  . . . . . . . . . . . . .  ACEContinue, ACERedo and ACEStart are possible
##
InstallGlobalFunction(ACEModes, function(arg)
local stream, modes;

  ACE_STREAM_ARG_CHK(arg);
  return ACE_MODES( ACEData.io[ ACE_STREAM(arg) ].stream );
end);

#############################################################################
####
##
#F  ACEContinue  . . . . . . . . . . . .  Continue an interactive ACE session
##
##
InstallGlobalFunction(ACEContinue, function(arg)
  return ACE_MODE_AFTER_SET_OPTS("Continue", arg);
end);

#############################################################################
####
##
#F  ACERedo . . . . . . . . . . . . . . . . . Redo an interactive ACE session
##
##
InstallGlobalFunction(ACERedo, function(arg)
  return ACE_MODE_AFTER_SET_OPTS("Redo", arg);
end);

#############################################################################
####
##
#F  ACEGroupGenerators  . . . . . . . . . . . Return the GAP group generators
##  . . . . . . . . . . . . . . . . . . . . . . of an interactive ACE session
##
##
InstallGlobalFunction(ACEGroupGenerators, function(arg)
local ioIndex;

  ACE_STREAM_ARG_CHK(arg);
  ioIndex := ACE_STREAM(arg);
  if ioIndex = fail or not IsBound( ACEData.io[ioIndex].args.fgens ) then
    Info(InfoACE + InfoWarning, 1, "No group generators saved?");
    return [];
  else
    return ACEData.io[ioIndex].args.fgens;
  fi;
end);

#############################################################################
####
##
#F  ACERelators . . . . . . . . . . . . . . . . . . . Return the GAP relators
##  . . . . . . . . . . . . . . . . . . . . . . of an interactive ACE session
##
##
InstallGlobalFunction(ACERelators, function(arg)
local ioIndex;

  ACE_STREAM_ARG_CHK(arg);
  ioIndex := ACE_STREAM(arg);
  if ioIndex = fail or not IsBound( ACEData.io[ioIndex].args.rels ) then
    Info(InfoACE + InfoWarning, 1, "No relators saved?");
    return [];
  else
    return ACEData.io[ioIndex].args.rels;
  fi;
end);

#############################################################################
####
##
#F  ACESubgroupGenerators . . . . . . . .  Return the GAP subgroup generators
##  . . . . . . . . . . . . . . . . . . . . . . of an interactive ACE session
##
##
InstallGlobalFunction(ACESubgroupGenerators, function(arg)
local ioIndex;

  ACE_STREAM_ARG_CHK(arg);
  ioIndex := ACE_STREAM(arg);
  if ioIndex = fail or not IsBound( ACEData.io[ioIndex].args.sgens ) then
    Info(InfoACE + InfoWarning, 1, "No subgroup generators saved?");
    return [];
  else
    return ACEData.io[ioIndex].args.sgens;
  fi;
end);

#############################################################################
####
##
#F  DisplayACEOptions . . . . . . . . . . .  Displays the current ACE options
##
##
InstallGlobalFunction(DisplayACEOptions, function(arg)

  ACE_STREAM_ARG_CHK(arg);
  DISPLAY_ACE_OPTIONS( ACEData.io[ ACE_STREAM(arg) ] );
end);

#############################################################################
####
##
#F  GetACEOptions . . . . . . . . . . .  Returns the current ACE options
##
##
InstallGlobalFunction(GetACEOptions, function(arg)
local datarec;

  ACE_STREAM_ARG_CHK(arg);
  datarec := ACEData.io[ ACE_STREAM(arg) ];
  if not IsBound(datarec.options) then
    Info(InfoACE + InfoWarning, 1, "No options saved.");
    return rec();
  else
    return datarec.options;
  fi;
end);

#############################################################################
####
##
#F  SET_ACE_OPTIONS . . . . . . . . . . . . . . . . . . .  Internal procedure
##  . . . . . . . . . . . . . . . . . . . . . . . . . Called by SetACEOptions
##
##  SetACEOptions has two forms: the  interactive  version  (below)  and  the
##  non-interactive version defined locally  within  ACECosetTable.  For  the
##  interactive version the data record datarec  is  ACEData.io[ioIndex]  for
##  some integer ioIndex. For the non-interactive version, which will only be
##  invoked from within a break-loop, datarec is ACEData.
##
##  enumResult, stats fields of ACEData.io[ioIndex] if user passed  some  new
##  options
##
InstallGlobalFunction(SET_ACE_OPTIONS, function(datarec)

  datarec.newoptions := OptionsStack[ Length(OptionsStack) ];
  # First we need to scrub any option names in datarec.options that
  # match those in datarec.newoptions ... to ensure that *all* new
  # options are at the end of the stack
  SANITISE_ACE_OPTIONS(datarec.options, datarec.newoptions);
  # datarec.options contains the previous options 
  # We have to pop off newoptions and then push back
  # options and newoptions, to get an updated options
  PopOptions();
  PushOptions(datarec.options);
  PushOptions(datarec.newoptions);
  # The following is needed when SetACEOptions is invoked via ACEExample
  Unbind(OptionsStack[ Length(OptionsStack) ].aceexampleoptions);
  datarec.options := OptionsStack[ Length(OptionsStack) ];
  # We pop options here, to ensure OptionsStack is the same length
  # as before the call to SET_ACE_OPTIONS
  PopOptions();
  Unbind(datarec.newoptions);
end);

#############################################################################
####
##
#F  INTERACT_SET_ACE_OPTIONS  . . . . . . . . . . . . . .  Internal procedure
##  . . . . . . . . . . . . . . . . . . . . . . .  Passes new options to  ACE
##  . . . . . . . . . . . . . . . . . . . . . . .  and updates stored options
##
##  Called by the ACE function with name ACEfname and with datarec  equal  to
##  ACEData.io[ioIndex] for some integer ioIndex,  the  updated  options  are
##  stored in datarec.options.
##
InstallGlobalFunction(INTERACT_SET_ACE_OPTIONS, function(ACEfname, datarec)
local newoptnames, optnames;
  if not IsEmpty(OptionsStack) then
    newoptnames := ShallowCopy(RecNames(OptionsStack[ Length(OptionsStack) ]));
    SET_ACE_OPTIONS(datarec);
    OptionsStack[ Length(OptionsStack) ] := datarec.options;
    optnames := ACE_OPT_NAMES();
    PROCESS_ACE_OPTIONS(ACEfname, optnames, newoptnames,
                        # echo
                        ACE_VALUE_ECHO(optnames),
                        # ToACE
                        function(list) 
                          WRITE_LIST_TO_ACE_STREAM(datarec.stream, list);
                        end,
                        # disallowed (options)
                        rec(group      := ACE_ERRORS.argnotopt,
                            generators := ACE_ERRORS.argnotopt,
                            relators   := ACE_ERRORS.argnotopt), 
                        # ignored
                        [ "aceinfile", "aceoutfile" ]);
  fi;
end);
  
#############################################################################
####
##
#F  SetACEOptions . . . . . . . . . . . .  Interactively, passes  new options 
##  . . . . . . . . . . . . . . . . . . .  to ACE and updates stored  options
##
InstallGlobalFunction(SetACEOptions, function(arg)

  if Length(arg) > 2 then
    Error("Expected 0, 1 or 2 arguments ... not ", Length(arg), " arguments\n");
  elif Length(arg) in [1, 2] and IsRecord( arg[Length(arg)] ) then
    if not IsEmpty(OptionsStack) then
      Info(InfoACE + InfoWarning, 1,
           "Non-empty OptionsStack: SetACEOptions may have been called with");
      Info(InfoACE + InfoWarning, 1,
           "both a record argument and options. The order options are listed");
      Info(InfoACE + InfoWarning, 1,
           "may be incorrect. Please use separate calls to SetACEOptions,");
      Info(InfoACE + InfoWarning, 1,
           "e.g. 'SetACEOptions(<optionsRec>); SetACEOptions(: <options>;' ");
    fi;
    PushOptions( arg[Length(arg)] );
    INTERACT_SET_ACE_OPTIONS(
        "SetACEOptions", ACEData.io[ ACE_STREAM(arg{[1..Length(arg) - 1]}) ]
        );
    PopOptions();
  elif Length(arg) <= 1 then
    INTERACT_SET_ACE_OPTIONS("SetACEOptions", ACEData.io[ ACE_STREAM(arg) ]);
  else
    Error("2nd argument should have been a record\n");
  fi;
end);

#############################################################################
####
##
#F  ACEParameters . . . . . .  Returns the ACE value of ACE parameter options
##
##
InstallGlobalFunction(ACEParameters, function(arg)
local ioIndex, stream, line, fieldsAndValues, parameters, i, opt;

  ACE_STREAM_ARG_CHK(arg);
  ioIndex := ACE_STREAM(arg);
  stream := ACEData.io[ ioIndex ].stream;
  READ_ACE_ERRORS(stream);
  WRITE_LIST_TO_ACE_STREAM(stream, [ "sr:1;" ]);
  line := FLUSH_ACE_STREAM_UNTIL(stream, 2, 2, READ_NEXT_LINE,
                                 line -> line{[1..10]} = "Group Name");
  parameters := rec(enumeration := line{[13..Length(line) - 2]});
  line := FLUSH_ACE_STREAM_UNTIL(stream, 2, 2, READ_NEXT_LINE,
                                 line -> line{[1..13]} = "Subgroup Name");
  parameters.subgroup := line{[16..Length(line) - 2]};
  FLUSH_ACE_STREAM_UNTIL(stream, 2, 2, READ_NEXT_LINE,
                         line -> line{[1..19]} = "Subgroup Generators");
  fieldsAndValues :=
      SplitString( 
          ReplacedString(
              Flat( ACEReadUntil(ioIndex, line -> line{[1..2]} = "C:") ),
              "Fi:", "Fil:"
              ),
          "", " :;" 
          );
  FLUSH_ACE_STREAM_UNTIL(stream, 2, 2, READ_NEXT_LINE,
                         line -> line{[1..6]} = "  #---");
  i := 1;
  while i < Length(fieldsAndValues) do
    parameters.(ACEOptionData( fieldsAndValues[i] ).synonyms[1]) :=
        Int(fieldsAndValues[i + 1]);
    i := i + 2;
  od;
  return parameters;
end);

#############################################################################
####
##
#F  ACEVersion  . . . . . . .  Prints the ACE version and details of packages
##  . . . . . . . . . . . . . . . . . . . . .  included when ACE was compiled
##
InstallGlobalFunction(ACEVersion, function(arg)
local ioIndex, stream, infoACELevel, infoWarningLevel;

  infoACELevel := InfoLevel(InfoACE);
  infoWarningLevel := InfoLevel(InfoWarning);
  SetInfoLevel(InfoACE, 0);
  SetInfoLevel(InfoWarning, 0);
  ACE_STREAM_ARG_CHK(arg);
  ioIndex := ACE_STREAM(arg);
  if ioIndex = fail then 
    # Fire up a new stream ... which we'll close when we're finished
    stream := InputOutputLocalProcess( ACEData.tmpdir, ACEData.binary, [] );
    READ_ACE_ERRORS(stream); # purge ACE banner
  else
    # Use interactive ACE process: ioIndex
    stream := ACEData.io[ ioIndex ].stream;
  fi;
  SetInfoLevel(InfoWarning, infoWarningLevel);
  SetInfoLevel(InfoACE, 1);
  READ_ACE_ERRORS(stream); # purge any output not yet collected
                           # e.g. error messages due to unknown options
  Info(InfoACE, 1, "ACE ", ACEData.version);
  WRITE_LIST_TO_ACE_STREAM(stream, [ "options;" ]);
  FLUSH_ACE_STREAM_UNTIL(stream, 1, 1, READ_NEXT_LINE,
                         line -> line{[1..13]} = "  host info =");
  SetInfoLevel(InfoACE, infoACELevel);
  if ioIndex = fail then 
    CloseStream(stream);
  fi;
end);

#############################################################################
##
#F  ACECosetTable  . . . . . . . . . . . .  Extracts the coset table from ACE
##
InstallGlobalFunction(ACECosetTable, function(arg)
local ioIndex, enumIndex, ACEout, iostream, infoACElevel, datarec,
      cosettable, ACEOnBreak, NormalOnBreak, SetACEOptions, DisplayACEOptions;

  if Length(arg) = 2 or Length(arg) > 3 then
    Error("Expected 0, 1 or 3 arguments ... not ", Length(arg), " arguments\n");
  elif Length(arg) <= 1 then
    # Called as an interactive ACE command
    datarec := ACEData.io[ ACE_STREAM(arg) ];
    INTERACT_SET_ACE_OPTIONS("ACECosetTable", datarec);
    if not IsEmpty(OptionsStack) then
      CHEAPEST_ACE_MODE(datarec); 
    fi;
    if datarec.stats.index = 0 then
      Info(InfoACE + InfoWarning, 1, 
           "The `ACE' coset enumeration failed with the result:");
      Info(InfoACE + InfoWarning, 1, datarec.enumResult);
      Info(InfoACE + InfoWarning, 1, "Try relaxing any restrictive options.");
      Info(InfoACE + InfoWarning, 1, "For interactive ACE process <i>,");
      Info(InfoACE + InfoWarning, 1, 
           "type: 'DisplayACEOptions(<i>);' to see current ACE options.");
      return fail;
    else
      WRITE_LIST_TO_ACE_STREAM(datarec.stream, [ "Print Table;" ]);
      return ACE_COSET_TABLE(datarec.stats.index, 
                             datarec.acegens, 
                             datarec.stream, 
                             READ_NEXT_LINE);
    fi;
  else
    # Called non-interactively

    NormalOnBreak := OnBreak;
    ACEOnBreak := function()
      local infoACElevel;

      infoACElevel := InfoACELevel();
      SetInfoACELevel(1);
      Info(InfoACE, 1, "The `ACE' coset enumeration failed with the result:");
      Info(InfoACE, 1, ACEData.enumResult);
      Info(InfoACE, 1, "Try relaxing any restrictive options:");
      Info(InfoACE, 1, "type: 'DisplayACEOptions();' ",
                       "to see current ACE options;");
      Info(InfoACE, 1, "type: 'SetACEOptions(:<option1> := <value1>, ...);'");
      Info(InfoACE, 1, "to set <option1> to <value1> etc.");
      Info(InfoACE, 1, "(i.e. pass options after the ':' in the usual way)");
      Info(InfoACE, 1, "... and then, type: 'return;' to continue.");
      Info(InfoACE, 1, "Otherwise, type: 'quit;' to quit the enumeration.");
      SetInfoACELevel(infoACElevel);
      OnBreak := NormalOnBreak;
    end;

    SetACEOptions := function()
      if not IsEmpty(OptionsStack) and 
         ACEData.optionsStackDepth = Length(OptionsStack) then
        SET_ACE_OPTIONS(ACEData);
      fi;
    end;

    DisplayACEOptions := function()
      DISPLAY_ACE_OPTIONS( ACEData );
    end;

    repeat
      ACEout := CALL_ACE(        # args are:         fgens,   rels,  sgens
                    "ACECosetTableFromGensAndRels", arg[1], arg[2], arg[3] );
      if ACEout.infile <> ACEData.infile then
        # User only wanted an ACE input file to use directly with standalone
        infoACElevel := InfoACELevel();
        SetInfoACELevel(1);
        Info(InfoACE, 1, "ACE standalone input file: ", ACEout.infile);
        SetInfoACELevel(infoACElevel);
        return;
      fi;
      iostream := InputTextFile(ACEout.outfile);
      ACEData.enumResult := ACE_ENUMERATION_RESULT(iostream, ReadLine);
      enumIndex := ACE_STATS(ACEData.enumResult).index;
      if enumIndex = 0 then
        CloseStream(iostream);
        if ACEout.silent then
          return fail;
        else
          ACEData.options := ACE_OPTIONS();
          ACEData.optionsStackDepth := Length(OptionsStack);
          if ACEData.optionsStackDepth > 0 then
            # We pop options here, in case the user decides to quit
            PopOptions();
          fi;
          OnBreak := ACEOnBreak;
          Error(": No coset table ...");
          if ACEData.options <> rec() then
            PushOptions(ACEData.options);
            Unbind(ACEData.options);
          fi;
        fi;
      else
        cosettable := ACE_COSET_TABLE(
                          enumIndex, ACEout.acegens, iostream, ReadLine);
        CloseStream(iostream);
        return cosettable;
      fi;
    until false;
  fi;
end);

#############################################################################
####
##
#F  ACEStats  . . . Get the subgroup index, time and number of cosets defined
##  . . . . . . . . . .  during an interactive or non-interactive ACE session
##
InstallGlobalFunction(ACEStats, function(arg)
local datarec, iostream, line, stats;

  if Length(arg) <= 1 then 
    # Called as an interactive ACE command
    datarec := ACEData.io[ ACE_STREAM(arg) ];
    INTERACT_SET_ACE_OPTIONS("ACEStats", datarec);
    if not IsEmpty(OptionsStack) then
      CHEAPEST_ACE_MODE(datarec);
    fi;
    return datarec.stats;
  elif Length(arg) = 3 then              # args are: fgens,   rels,  sgens
    # Called non-interactively
    iostream := InputTextFile( CALL_ACE("ACEStats", arg[1], arg[2], arg[3]) );
    stats := ACE_STATS( ACE_ENUMERATION_RESULT( iostream, ReadLine) );
    CloseStream(iostream);
    return stats;
  else
    Error("Expected 0, 1 or 3 arguments ... not ", Length(arg), " arguments\n");
  fi;
end);

#E  interact.gi . . . . . . . . . . . . . . . . . . . . . . . . .  ends here 
