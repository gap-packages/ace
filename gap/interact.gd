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
DeclareGlobalFunction("EXEC_ACE_DIRECTIVE_OPTION");
DeclareGlobalFunction("DATAREC_AND_NO_VALUE");
DeclareGlobalFunction("DATAREC_AND_ONE_VALUE");
DeclareGlobalFunction("DATAREC_AND_LIST");
DeclareGlobalFunction("ACEDumpVariables");
DeclareGlobalFunction("ACEDumpStatistics");
DeclareGlobalFunction("ACEStyle");
DeclareGlobalFunction("ACEDisplayCosetTable");
DeclareGlobalFunction("ACECycles");
DeclareSynonym("ACEPermutationRepresentation", ACECycles);
DeclareGlobalFunction("ACECosetTable");
DeclareGlobalFunction("ACEStats");
DeclareGlobalFunction("ACERecover");
DeclareGlobalFunction("ACEStandardCosetNumbering");
DeclareGlobalFunction("ACEAddRelators");
DeclareGlobalFunction("ACEAddSubgroupGenerators");
DeclareGlobalFunction("ACEDeleteRelators");
DeclareGlobalFunction("ACEDeleteSubgroupGenerators");
DeclareGlobalFunction("ACECosetCoincidence");
DeclareGlobalFunction("ACERandomCoincidences");
DeclareGlobalFunction("ACENormalClosure");
DeclareGlobalFunction("ACEOrders");
DeclareGlobalFunction("ACEOrder");
DeclareGlobalFunction("ACEStabilisingCosets");
DeclareGlobalFunction("ACETraceWord");

#E  interact.gd . . . . . . . . . . . . . . . . . . . . . . . . .  ends here 
