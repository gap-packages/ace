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
#I  InfoClass
##
DeclareInfoClass("InfoACE");

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
DeclareGlobalFunction("ACEPackageVersion");

#############################################################################
##
#O  IS_ACE_MATCH( <list>, <sub> )
#O  IS_ACE_MATCH( <list>, <sub>, <at> )
##
##  returns `true' if <sub> matches a sublist of <list> from position  1  (or
##  position <at>, in the case of the second version), or `false', otherwise.
##  If <sub> is empty `true' is returned. If <list> is  empty  but  <sub>  is
##  non-empty  `false'  is  returned.  (This  operation   is   identical   to
##  IsMatchingSublist introduced in GAP 4.3; when backward compatibility with
##  GAP  4.2  is  no  longer  necessary  or  desirable  all  occurrences   of
##  IS_ACE_MATCH may be replaced with  IsMatchingSublist  and  the  operation
##  IS_ACE_MATCH may be deleted.)
##
DeclareOperation("IS_ACE_MATCH", [ IsList, IsList, IS_INT ] );

DeclareGlobalFunction("CALL_ACE");
DeclareGlobalFunction("ACECosetTableFromGensAndRels");
DeclareGlobalFunction("IsACEStandardCosetTable");
DeclareGlobalFunction("IsACEGeneratorsInPreferredOrder");
DeclareGlobalFunction("ACE_READ_AS_FUNC");
DeclareGlobalFunction("ACEExample");
DeclareGlobalFunction("ACEReadResearchExample");
DeclareGlobalFunction("ACEPrintResearchExample");
DeclareGlobalFunction("ACEDirectoryTemporary");
DeclareGlobalFunction("ACE_ERROR");

#############################################################################
##
#D  Declare deprecated function.
##

DeclareGlobalFunction("CallACE");

#E  general.gd  . . . . . . . . . . . . . . . . . . . . . . . . . . ends here 
