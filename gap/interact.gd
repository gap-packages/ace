#############################################################################
##
#W  interact.gd             ACE Share Package                     Greg Gamble
##
##  This file  declares  commands for using ACE interactively via IO Streams.
##    
#H  @(#)$Id$
##
#Y  Copyright (C) 2000  Centre for Discrete Mathematics and Computing
#Y                      Department of Computer Science & Electrical Eng.
#Y                      University of Queensland, Australia.
##
Revision.interact_gd :=
    "@(#)$Id$";

#############################################################################
##
#D  Declare functions for using ACE interactively via IO Streams.
##
DeclareGlobalFunction("ACE_STREAM");
DeclareGlobalFunction("ACE_STREAM_ARG_CHK");
DeclareGlobalFunction("ACEStream");
DeclareGlobalFunction("ACEStreams");
DeclareGlobalFunction("READ_ACE_ERRORS");
DeclareGlobalFunction("INTERACT_TO_ACE_WITH_ERRCHK");
DeclareGlobalFunction("FLUSH_ACE_STREAM_UNTIL");
DeclareGlobalFunction("ACE_ENUMERATION_RESULT");
DeclareGlobalFunction("ACEWrite");
DeclareGlobalFunction("ACERead");
DeclareGlobalFunction("ACEReadAll");
DeclareGlobalFunction("ACEReadUntil");
DeclareGlobalFunction("ACE_STATS");
DeclareGlobalFunction("ACE_COSET_TABLE");
DeclareGlobalFunction("ACE_MODE");
DeclareGlobalFunction("ACE_MODE_AFTER_SET_OPTS");
DeclareGlobalFunction("CHEAPEST_ACE_MODE");
DeclareGlobalFunction("ACEStart");
DeclareGlobalFunction("ACEQuit");
DeclareGlobalFunction("ACEQuitAll");
DeclareGlobalFunction("ACE_MODES");
DeclareGlobalFunction("ACEModes");
DeclareGlobalFunction("ACEContinue");
DeclareGlobalFunction("ACERedo");
DeclareGlobalFunction("ACEGroupGenerators");
DeclareGlobalFunction("ACERelators");
DeclareGlobalFunction("ACESubgroupGenerators");
DeclareGlobalFunction("DisplayACEOptions");
DeclareGlobalFunction("GetACEOptions");
DeclareGlobalFunction("SET_ACE_OPTIONS");
DeclareGlobalFunction("INTERACT_SET_ACE_OPTIONS");
DeclareGlobalFunction("SetACEOptions");
DeclareGlobalFunction("ACEParameters");
DeclareGlobalFunction("ACEVersion");
DeclareGlobalFunction("ACECosetTable");
DeclareGlobalFunction("ACEStats");

#E  interact.gd . . . . . . . . . . . . . . . . . . . . . . . . .  ends here 
