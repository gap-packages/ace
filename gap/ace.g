#############################################################################
####
##
#W  ace.g                   ACE Share Package                Alexander Hulpke
#W                                                                Greg Gamble
##
##  `Head' file for the GAP interface to the ACE (Advanced Coset Enumerator),
##  by George Havas and Colin Ramsay.  The original interface was written  by 
##  Alexander Hulpke and extensively modified by Greg Gamble.
##    
#H  @(#)$Id$
##
#Y  Copyright (C) 2000  Centre for Discrete Mathematics and Computing
#Y                      Department of Computer Science & Electrical Eng.
#Y                      University of Queensland, Australia.
##
Revision.ace_g :=
    "@(#)$Id$";


#############################################################################
####
##
#V  ACEData . . . . record used by various functions of the ACE share package
##
##  The fields of ACEData are:
##
##    "binary"  . . the path of the ACE binary
##    "tmpdir"  . . the path of the temporary directory for ACE i/o files
##    "io . . . . . list of data records for ACEStart IO Streams
##    "infile"  . . the path of the ACE input file
##    "outfile" . . the path of the ACE output file
##    "version" . . the version of the current ACE binary
##
ACEData := rec( binary := Filename(DirectoriesPackagePrograms("ace"), "ace"),
                tmpdir := DirectoryTemporary(),
                io := [] # Initially no ACEStart IO Streams
              );
ACEData.infile  := Filename(ACEData.tmpdir, "in"); 
ACEData.outfile := Filename(ACEData.tmpdir, "out");

PrintTo(ACEData.infile, "\n");
# Fire up ACE with a null input (ACEData.infile contains only a "\n")
# ... to generate a banner (which has ACE's current version)
Exec(Concatenation(ACEData.binary, "<", ACEData.infile, ">", ACEData.outfile));
# For now use ACEData.scratch for an input stream
ACEData.scratch := InputTextFile(ACEData.outfile);
# Grab the first line of outfile, which begins like: "ACE N.nnn   "
ACEData.version := ReadLine(ACEData.scratch);
CloseStream(ACEData.scratch);
# We just want the N.nnn part of the first line of outfile
# ... ACEData.scratch now records where N.nnn starts
ACEData.scratch := PositionSublist(ACEData.version, "ACE") + 4;
ACEData.version := ACEData.version{[ACEData.scratch ..
                                    Position(ACEData.version, ' ', 
                                             ACEData.scratch) - 1]};
Unbind(ACEData.scratch); # We don't need ACEData.scratch, anymore.

# Set the default level of InfoACE
SetInfoLevel(InfoACE, 1);

#############################################################################
####
##  Print a banner . . . . . .  using InfoWarning (so a user can turn it off)
##
Info(InfoWarning,1,"Loading the ACE (Advanced Coset Enumerator) share package");
Info(InfoWarning,1,"         by George Havas <havas@csee.uq.edu.au> and");
Info(InfoWarning,1,"            Colin Ramsay <cram@csee.uq.edu.au>");
Info(InfoWarning,1,"                 ACE binary version: ", ACEData.version);

#############################################################################
####
##  For backward compatibility with  GAP  4.2  we  ensure  OnBreakMessage  is
##  defined, but with a non-function definition.
##
if not( "OnBreakMessage" in NamesGVars() ) then
  OnBreakMessage := "";
fi;

#############################################################################
####
##  Ensure no zombie ACE processes from interactive (ACEStart)  sessions  are 
##  . . . . .  . . . . . . . . . . . .  left lying around when user quits GAP
##
InstallAtExit( ACEQuitAll );

#E  ace.g . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here 
