#############################################################################
##
#W  example.gd              ACE Share Package                     Greg Gamble
##
##  This  file  declares  a  number of  ACE  example functions,  for examples
##  originally  written  as  ACE  input  files  by  Colin Ramsay,  and  later
##  translated to GAP by Greg Gamble.
##    
#H  @(#)$Id$
##
#Y  Copyright (C) 2000  Centre for Discrete Mathematics and Computing
#Y                      Department of Computer Science & Electrical Eng.
#Y                      University of Queensland, Australia.
##
Revision.example_gd :=
    "@(#)$Id$";

#D  Declare functions: ACEexample0() .. ACEexample..()
##
##  The names may seem odd,  since there are apparent  omissions.  The  names
##  are of the form ACEexample<n><a>,  where <n> is an integer that indicates
##  the example is drawn from Colin Ramsay's `test0..<n>.in' (where 0..<n> is
##  the integer <n>,  0-filled on the left to 3 digits)  and <a> is either an
##  empty string  or a letter that distinguishes examples drawn from the same
##  file.
##

DeclareGlobalFunction("ACEexample0");
DeclareGlobalFunction("ACEexample0a");
DeclareGlobalFunction("ACEexample1");
DeclareGlobalFunction("ACEexample1a");
DeclareGlobalFunction("ACEexample1b");
DeclareGlobalFunction("ACEexample1c");
DeclareGlobalFunction("ACEexample2");
DeclareGlobalFunction("ACEexample2a");
DeclareGlobalFunction("ACEexample2b");
DeclareGlobalFunction("ACEexample5");
DeclareGlobalFunction("ACEexample6");
DeclareGlobalFunction("ACEexample7");
DeclareGlobalFunction("ACEexample7a");
DeclareGlobalFunction("ACEexample7b");
DeclareGlobalFunction("ACEexample7c");

#E  example.gd  . . . . . . . . . . . . . . . . . . . . . . . . .  ends here 
