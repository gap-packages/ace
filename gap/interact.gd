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
DeclareGlobalFunction("InfoACELevel");
DeclareGlobalFunction("SetInfoACELevel");
DeclareGlobalFunction("ACE_STREAM");
DeclareGlobalFunction("CHOMP");
DeclareGlobalFunction("READ_ALL_LINE");
DeclareGlobalFunction("READ_NEXT_LINE");
DeclareGlobalFunction("FLUSH_ACE_STREAM_UNTIL");
DeclareGlobalFunction("ACE_ENUMERATION_RESULT");
DeclareGlobalFunction("ACE_STATS");
DeclareGlobalFunction("ACE_STATS_INDEX");
DeclareGlobalFunction("ACE_COSET_TABLE");
DeclareGlobalFunction("StartACE");
DeclareGlobalFunction("QuitACE");
DeclareGlobalFunction("QuitAllACE");
DeclareGlobalFunction("ACECosetTable");
DeclareGlobalFunction("ACEStats");

#E  interact.gd . . . . . . . . . . . . . . . . . . . . . . . . .  ends here 
