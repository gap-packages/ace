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
#I  InfoClass
##
DeclareInfoClass("InfoACE");

#############################################################################
##
#D  Declare functions for using ACE interactively via IO Streams.
##
DeclareGlobalFunction("ACE_STREAM");
DeclareGlobalFunction("FLUSH_ACE_STREAM_UNTIL");
DeclareGlobalFunction("ACE_ENUMERATION_RESULT");
DeclareGlobalFunction("ACE_STATS");
DeclareGlobalFunction("ACE_COSET_TABLE");
DeclareGlobalFunction("START_ACE");
DeclareGlobalFunction("SET_ACE_OPTIONS");
DeclareGlobalFunction("StartACE");
DeclareGlobalFunction("QuitACE");
DeclareGlobalFunction("QuitAllACE");
DeclareGlobalFunction("ACECosetTable");
DeclareGlobalFunction("ACEStats");

#E  interact.gd . . . . . . . . . . . . . . . . . . . . . . . . .  ends here 
