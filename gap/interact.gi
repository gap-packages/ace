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
#F  InfoACELevel . . . . . . . . . . . . . . .  Get the InfoLevel for InfoACE
##
##
InstallGlobalFunction("InfoACELevel", function(level)
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
#F  CHOMP . . . . . . . . . Return string minus a trailing '\n' if it has one
##  . . . . . . . . . . . . . . . . . . . . . (named after the Perl function)
##
InstallGlobalFunction("CHOMP", function(string)

  if string<>fail and string[Length(string)] = '\n' then
    return string{[1 .. Length(string) - 1]};
  else
    return string;
  fi;
end);

#############################################################################
####
##
#F  READ_ALL_LINE . . . . . . . . .  Read a complete line (i.e. until a '\n')
##  . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  or return fail 
##
##  Essentially, like ReadLine but does not return a line fragment ... if the
##  initial ReadLine call doesn't return fail, it waits until it has all  the
##  line before returning.
##
InstallGlobalFunction("READ_ALL_LINE", function(iostream)
local line, moreOfline;

  line := ReadLine(iostream); # If we get fail here, we just return
  if line <> fail then
    while line[Length(line)] <> '\n' do
      Sleep(1);
      moreOfline := ReadLine(iostream);
      if moreOfline <> fail then
        Append(line, moreOfline);
      fi;
    od;
  fi;
  return line;
end);

#############################################################################
####
##
#F  READ_NEXT_LINE . . . . . . . . Read a complete line but never return fail
##
##  Essentially, like  READ_ALL_LINE  but we know there is a complete line to 
##  be got ... so we wait for it, before returning.
##
InstallGlobalFunction("READ_NEXT_LINE", function(iostream)
local line;

  line := READ_ALL_LINE(iostream);
  while line = fail do
    Sleep(1);
    line := READ_ALL_LINE(iostream);
  od;
  return line;
end);

#############################################################################
####
##
#F  FLUSH_ACE_STREAM_UNTIL  . . . . . . Read lines in iostream and print them 
##  . . . . . . . . . . . . until IsMyLine(line) is true and return that line
##
##
InstallGlobalFunction("FLUSH_ACE_STREAM_UNTIL", 
function(iostream, infoLevel, readline, IsMyLine)
local line;

  repeat
    line := readline(iostream);
    Info(InfoACE, infoLevel, CHOMP(line));
  until IsMyLine(line);
  return line;
end);

#############################################################################
####
##
#F  ACE_ENUMERATION_RESULT  . . . Flush iostream until line ends in ")\n"
##
##  This is potentially flaky ... it relies on ACE enumeration  result  lines
##  remaining unique with regard to the property of ending in a close bracket
##
InstallGlobalFunction("ACE_ENUMERATION_RESULT", function(iostream, readline)
local line;

  return FLUSH_ACE_STREAM_UNTIL(iostream, 1,
                                readline,
                                line -> line[Length(line) - 1] = ')');
end);

#############################################################################
####
##
#F  ACE_STATS . . . . . . . . . . . . . . . . Called by StartACE and ACEStats
##  
##
InstallGlobalFunction("ACE_STATS", function(line)
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
#F  ACE_STATS_INDEX . . . . . . . . . . . . . Get the coset enumeration index
##  
##
InstallGlobalFunction("ACE_STATS_INDEX", function(ioIndex)
local iostream;

  if not(IsBound(ACEData.io[ioIndex].stats)) then
    iostream := ACEData.io[ioIndex].stream;
    WriteLine(iostream, "End;");
    ACEData.io[ioIndex].stats 
        := ACE_STATS( ACE_ENUMERATION_RESULT(iostream, READ_NEXT_LINE) );
  fi;
  return ACEData.io[ioIndex].stats.index;
end);

#############################################################################
####
##
#F  ACE_COSET_TABLE
##
##
InstallGlobalFunction("ACE_COSET_TABLE", 
                      function(enumIndex, gens, iostream, readline)
local n, line, colIndex, table, i, rowi, j, colj, invcolj;

  n := Length(gens);

  # Skip some header until the ` coset ' line
  repeat
    line := readline(iostream);
    Info(InfoACE, 2, CHOMP(line));
  until line=fail or line{[1..6]}=" coset";

  # Extract the coset table column headers
  rowi := SplitString(line, "", " |\n");

  # Look at the coset table column headers and determine the column
  # corresponding to each generator:
  #   colIndex[j] = Index of column(gens[j])
  colIndex := Filtered(List(gens, j -> Position(rowi, String(j))),
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
#F  StartACE . . . . . . . . . . . . . .  Initiate an interactive ACE session
##
##
InstallGlobalFunction("StartACE", function(arg)
local ioIndex, line;

  if Length(arg) <= 1 then 
    ioIndex := ACE_STREAM(arg);
  elif Length(arg) = 3 then #args are: fgens,   rels,  sgens
    ioIndex := START_ACE( "StartACE", arg[1], arg[2], arg[3] );
  else
    Error("Expected 0, 1 or 3 arguments ... not ", Length(arg), " arguments\n");
  fi;

  WriteLine(ACEData.io[ioIndex].stream, "End;"); # Start the enumeration
  ACEData.io[ioIndex].stats := ACE_STATS( ACE_ENUMERATION_RESULT(
                                              ACEData.io[ioIndex].stream, 
                                              READ_NEXT_LINE) );
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
local ioIndex, enumIndex, ACEout, iostream, cosettable;

  if Length(arg) <= 1 then 
    # Called as an interactive ACE command
    ioIndex := ACE_STREAM(arg);
    enumIndex := ACE_STATS_INDEX(ioIndex);
    if enumIndex = 0 then
      Info(InfoACE, 0,
           "No coset table: the `ACE' coset enumeration did not complete.",
           "Try relaxing any restrictive options e.g. make more memory",
           "available to `ACE'.");
      return fail;
    else
      WriteLine(ACEData.io[ioIndex].stream, "Print Table;");
      return ACE_COSET_TABLE(enumIndex,
                             ACEData.io[ioIndex].gens, 
                             ACEData.io[ioIndex].stream,
                             READ_NEXT_LINE);
    fi;
  elif Length(arg) = 3 then                 # args are: fgens,   rels,  sgens
    # Called non-interactively
    repeat
      ACEout 
         := START_ACE( "ACECosetTableFromGensAndRels", arg[1], arg[2], arg[3] );
      if ACEout.infile <> ACEData.infile then
        # User only wanted an ACE input file to use directly with standalone
        Print("#I ACE standalone input file : ", ACEout.infile, "\n");
        return;
      fi;
      iostream := InputTextFile(ACEout.outfile);
      enumIndex := ACE_STATS( ACE_ENUMERATION_RESULT(
                                  iostream, ReadLine
                                  ) ).index;
      if enumIndex = 0 then
        CloseStream(iostream);
        if ACEout.silent then
          return fail;
        else
          Error(
              "No coset table: the `ACE' coset enumeration did not complete.\n",
              "Try relaxing any restrictive options e.g. make more memory\n",
              "available to `ACE'.\nType `quit;' to exit to the main loop."
              );
        fi;
      else
        cosettable := ACE_COSET_TABLE(
                          enumIndex, ACEout.gens, iostream, ReadLine);
        CloseStream(iostream);
        return cosettable;
      fi;
    until false;
  else
    Error("Expected 0, 1 or 3 arguments ... not ", Length(arg), " arguments\n");
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
    if not(IsBound(ACEData.io[ioIndex].stats)) then
      iostream := ACEData.io[ioIndex].stream;
      WriteLine(iostream, "End;");
      ACEData.io[ioIndex].stats := ACE_STATS( ACE_ENUMERATION_RESULT(
                                                  iostream, READ_NEXT_LINE) );
    fi;
    return ACEData.io[ioIndex].stats;
  elif Length(arg) = 3 then #args are: fgens,   rels,  sgens
    # Called non-interactively
    iostream := InputTextFile( START_ACE("ACEStats", arg[1], arg[2], arg[3]) );
    stats := ACE_STATS( ACE_ENUMERATION_RESULT( iostream, ReadLine) );
    CloseStream(iostream);
    return stats;
  else
    Error("Expected 0, 1 or 3 arguments ... not ", Length(arg), " arguments\n");
  fi;
end);

#E  interact.gi . . . . . . . . . . . . . . . . . . . . . . . . .  ends here 
