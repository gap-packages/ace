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
#F  ACE_STREAM  . . . . . . . . . . . . . . . Get the index of the ACEData.io
##  . . . . . . . . . . . . . . . . . . . list for an interactive ACE session
##
InstallGlobalFunction("ACE_STREAM", function(arg)
local ioIndex;

  arg := arg[1];
  if Length(arg) = 0 then
    ioIndex := 1;
    while not(IsBound(ACEData.io[ioIndex])) and ioIndex < Length(ACEData.io) do
      ioIndex := ioIndex + 1;
    od;
  else
    ioIndex := arg[1];
  fi;
  if IsBound(ACEData.io[ioIndex]) then
    return ioIndex;
  else
    Error("No such interactive ACE session");
  fi;
end);

#############################################################################
####
##
#F  FLUSH_ACE_STREAM_UNTIL  . . . . . . . Read lines in iostream via function 
##  . . . . . . . . . . .  readline and print them  via Info at  InfoACELevel
##  . . . . . . . . . . .  infoLevelFlushed  until  IsMyLine(line)  is  true,
##  . . . . . . . . . . .  print `MyLine' via Info at InfoACELevel infoLevel-
##  . . . . . . . . . . . . . . . . . . . . . .  MyLine and return that line.
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
##  remaining unique with regard to the property of ending in a close bracket
##
InstallGlobalFunction(ACE_ENUMERATION_RESULT, function(iostream, readline)
local line;

  return CHOMP(FLUSH_ACE_STREAM_UNTIL(iostream, 2, 1,
                                      readline,
                                      line -> Length(line) > 1 and
                                              line[Length(line) - 1] = ')'));
end);

#############################################################################
####
##
#F  ACE_STATS . . . . . . . . . . . . . . . . Called by StartACE and ACEStats
##  
##
InstallGlobalFunction(ACE_STATS, function(line)
local stats;

  # Parse line for statistics and return
  stats := Filtered(line, char -> char in ". " or char in CHARS_DIGITS);
  if line{[1..5]}<>"INDEX" then
    # Enumeration failed so the index is missing 
    # ... shove a 0 index on the front of a
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
#F  START_ACE . . . . . . . . . . . . . . . . . . . . .  Start an enumeration
##  . . . . . . . . . . . . . . . sets enumResult and stats fields of datarec
##
InstallGlobalFunction(START_ACE, function(datarec)
  WriteLine(datarec.stream, "End;"); # Start the enumeration
  datarec.enumResult := ACE_ENUMERATION_RESULT(datarec.stream, READ_NEXT_LINE);
  datarec.stats := ACE_STATS(datarec.enumResult);
end);
  
#############################################################################
####
##
#F  SET_ACE_OPTIONS . . . . . . . Update options, enumResult, stats fields of
##  . . . . . . . . . . . ACEData.io[ioIndex] if user passed some new options
##
InstallGlobalFunction(SET_ACE_OPTIONS, function(ioIndex)
local datarec;

  datarec := ACEData.io[ ioIndex ];
  if Length(OptionsStack) > 0 then
    # User passed some new options
    SetACEOptions(ioIndex);
    if datarec.stats.index = 0 then
      START_ACE(datarec);
    fi;
  fi;
end);
  
#############################################################################
####
##
#F  StartACE . . . . . . . . . . . . . .  Initiate an interactive ACE session
##
##
InstallGlobalFunction(StartACE, function(arg)
local ioIndex, line;

  if Length(arg) <= 1 then 
    ioIndex := ACE_STREAM(arg);
    SetACEOptions(ioIndex); # In case user passed some new options
  elif Length(arg) = 3 then #args are: fgens,   rels,  sgens
    ioIndex := CALL_ACE( "StartACE", arg[1], arg[2], arg[3] );
  else
    Error("Expected 0, 1 or 3 arguments ... not ", Length(arg), " arguments\n");
  fi;

  START_ACE( ACEData.io[ioIndex] );
  return ioIndex;
end);

#############################################################################
##
#F  QuitACE . . . . . . . . . . . . . . . .  Close an interactive ACE session
##
InstallGlobalFunction(QuitACE, function(arg)
local ioIndex;

  ioIndex := ACE_STREAM(arg);
  CloseStream(ACEData.io[ioIndex].stream);
  Unbind(ACEData.io[ioIndex]);
end);

#############################################################################
##
#F  QuitAllACE . . . . . . . . . . . . . . Close all interactive ACE sessions
##
InstallGlobalFunction(QuitAllACE, function()
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
#F  ACECosetTable  . . . . . . . . . . . .  Extracts the coset table from ACE
##
InstallGlobalFunction(ACECosetTable, function(arg)
local ioIndex, enumIndex, ACEout, iostream, infoACElevel, datarec,
      cosettable, ACEOnBreak, NormalOnBreak;

  if Length(arg) = 2 or Length(arg) > 3 then
    Error("Expected 0, 1 or 3 arguments ... not ", Length(arg), " arguments\n");
  else
    NormalOnBreak := OnBreak;
    ACEOnBreak := function()
      local infoACElevel;

      infoACElevel := InfoACELevel();
      SetInfoACELevel(1);
      Info(InfoACE, 1, "The `ACE' coset enumeration failed with the result:");
      Info(InfoACE, 1, datarec.enumResult);
      Info(InfoACE, 1, "Try relaxing any restrictive options:");
      Info(InfoACE, 1, "type: 'CurrentACEOptions();' ",
                       "to see current ACE options;");
      Info(InfoACE, 1, "type: 'SetACEOptions(:<option1> := <value1>, ...);'");
      Info(InfoACE, 1, "to set <option1> to <value1> etc.");
      Info(InfoACE, 1, "(i.e. pass options after the ':' in the usual way)");
      Info(InfoACE, 1, "... and then, type: 'return;' to continue.");
      Info(InfoACE, 1, "Otherwise, type: 'quit;' to quit the enumeration.");
      SetInfoACELevel(infoACElevel);
      OnBreak := NormalOnBreak;
    end;
    if Length(arg) <= 1 then
      # Called as an interactive ACE command
      ioIndex := ACE_STREAM(arg);
      SET_ACE_OPTIONS(ioIndex);
      datarec := ACEData.io[ ioIndex ];
      while datarec.stats.index = 0 do
        Error(": No coset table ...");
        START_ACE(datarec);
      od;
      WriteLine(datarec.stream, "Print Table;");
      return ACE_COSET_TABLE(
                 enumIndex, datarec.acegens, datarec.stream, READ_NEXT_LINE);
    else
      # Called non-interactively ... args are:         fgens,   rels,  sgens
      datarec := ACEData;
      repeat
        ACEout := CALL_ACE( 
                      "ACECosetTableFromGensAndRels", arg[1], arg[2], arg[3] );
        if ACEout.infile <> datarec.infile then
          # User only wanted an ACE input file to use directly with standalone
          infoACElevel := InfoACELevel();
          SetInfoACELevel(1);
          Info(InfoACE, 1, "ACE standalone input file: ", ACEout.infile);
          SetInfoACELevel(infoACElevel);
          return;
        fi;
        iostream := InputTextFile(ACEout.outfile);
        datarec.enumResult := ACE_ENUMERATION_RESULT(iostream, ReadLine);
        enumIndex := ACE_STATS(datarec.enumResult).index;
        if enumIndex = 0 then
          CloseStream(iostream);
          if ACEout.silent then
            return fail;
          else
            datarec.options := ACE_OPTIONS();
            datarec.optionsStackDepth := Length(OptionsStack);
            if datarec.optionsStackDepth > 0 then
              # We pop options here, in case the user decides to quit
              PopOptions();
            fi;
            OnBreak := ACEOnBreak;
            Error(": No coset table ...");
            if datarec.options <> rec() then
              PushOptions(datarec.options);
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
  fi;
end);

#############################################################################
####
##
#F  ACEStats  . . . Get the subgroup index, time and number of cosets defined
##  . . . . . . . . . .  during an interactive or non-interactive ACE session
##
InstallGlobalFunction("ACEStats", function(arg)
local ioIndex, iostream, line, stats;

  if Length(arg) <= 1 then 
    # Called as an interactive ACE command
    ioIndex := ACE_STREAM(arg);
    SET_ACE_OPTIONS(ioIndex);
    return ACEData.io[ioIndex].stats;
  elif Length(arg) = 3 then #args are: fgens,   rels,  sgens
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
