#############################################################################
####
##
#W  ace.gd                     ACE Package                   Alexander Hulpke
#W                                                                Greg Gamble
##
##  `Head' file for the GAP interface to the ACE (Advanced Coset Enumerator),
##  by George Havas and Colin Ramsay.  The original interface was written  by 
##  Alexander Hulpke and extensively modified by Greg Gamble.
##    
#Y  Copyright (C) 2006  Centre for Discrete Mathematics and Computing
#Y                      Department of Information Technology & Electrical Eng.
#Y                      University of Queensland, Australia.
##


#############################################################################
####
##
#V  ACEData . . . . . . . record used by various functions of the ACE package
##
DeclareGlobalVariable( "ACEData",
  "A record containing various data associated with the ACE package."
  );

#############################################################################
##
#I  InfoClass
##
DeclareInfoClass("InfoACE");

#E  ace.gd . . . . . . . . . . . . . . . . . . . . . . . . . . . .  ends here 
