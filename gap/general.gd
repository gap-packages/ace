#############################################################################
####
##
#W  general.gd              ACE Share Package                     Greg Gamble
##
##  This file declares mainly non-interactive ACE variables and functions.
##    
#H  @(#)$Id$
##
#Y  Copyright (C) 2000  Centre for Discrete Mathematics and Computing
#Y                      Department of Computer Science & Electrical Eng.
#Y                      University of Queensland, Australia.
##
Revision.general_gd :=
    "@(#)$Id$";


#############################################################################
##
#D  Declare variable.
##

DeclareGlobalVariable("ACETCENUM",
  "The ACE (Advanced Coset Enumerator) version of the coset enumerator TCENUM"
  );

#############################################################################
##
#D  Declare functions.
##

DeclareGlobalFunction("InfoACELevel");
DeclareGlobalFunction("SetInfoACELevel");
DeclareGlobalFunction("CALL_ACE");
DeclareGlobalFunction("ACECosetTableFromGensAndRels");
DeclareGlobalFunction("ACE_READ_AS_FUNC");
DeclareGlobalFunction("ACEExample");

#############################################################################
##
#D  Declare deprecated function.
##

DeclareGlobalFunction("CallACE");

#E  general.gd  . . . . . . . . . . . . . . . . . . . . . . . . . . ends here 