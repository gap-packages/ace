#############################################################################
####
##
#W  ace.g                      ACE Package                   Alexander Hulpke
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
#V  ACEData . . . . . . . record used by various functions of the ACE package
##
##  The fields of ACEData are:
##
##    "binary"  . . the path of the ACE binary
##    "tmpdir"  . . the path of the temporary directory for ACE i/o files
##    "ni"  . . . . record for a non-interactive process
##    "io"  . . . . list of data records for ACEStart IO Streams
##    "infile"  . . the path of the ACE input file
##    "outfile" . . the path of the ACE output file
##    "version" . . the version of the current ACE binary
##
DeclareGlobalVariable( "ACEData",
  "A record containing various data associated with the ACE package."
  );
InstallValue( ACEData,
  rec( binary := Filename(DirectoriesPackagePrograms("ace"), "ace"),
       tmpdir := DirectoryTemporary(),
       ni     := rec(),
       io     := [] # Initially no ACEStart IO Streams
       )
);
ACEData.infile  := Filename(ACEData.tmpdir, "in"); 
ACEData.outfile := Filename(ACEData.tmpdir, "out");

PrintTo(ACEData.infile, "\n");
# Fire up ACE with a null input (ACEData.infile contains only a "\n")
# ... to generate a banner (which has ACE's current version)
Exec(Concatenation(ACEData.binary, "<", ACEData.infile, ">", ACEData.outfile));
ACEData.version := StringFile( ACEData.outfile );
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
if not QUIET and BANNER then

Info(InfoWarning,1,"    The ACE (Advanced Coset Enumerator) Package");
Info(InfoWarning,1,"  C code by George Havas <havas@itee.uq.edu.au> and");
Info(InfoWarning,1,"            Colin Ramsay <cram@itee.uq.edu.au>");
Info(InfoWarning,1,"                 ACE binary version: ", ACEData.version);
Info(InfoWarning,1,"  GAP code by Greg Gamble <gregg@itee.uq.edu.au> and");
Info(InfoWarning,1,"         Alexander Hulpke <hulpke@math.colostate.edu>");
Info(InfoWarning,1,"                 ACE package version: ", 
                                     PACKAGES_VERSIONS.ace);
Info(InfoWarning,1,"");
Info(InfoWarning,1,"                 For help, type: ?ACE");

fi;

#############################################################################
####
##
#V  ACEIgnoreUnknownDefault . . . . . . . . . . . .  the default value of the 
##  . . . . . . . . . . . . . . . . . . . . . . . . `aceignoreunknown' option
##
ACEIgnoreUnknownDefault := true;

#############################################################################
####
##  Ensure no zombie ACE processes from interactive (ACEStart)  sessions  are 
##  . . . . .  . . . . . . . . . . . .  left lying around when user quits GAP
##
InstallAtExit( ACEQuitAll );

#E  ace.g . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here 
