#############################################################################
####
##
#W  doL28.g             ACE Research Example                      Greg Gamble
##
##  Provides some easy applications of the pgrelfind.g functions,  using  the
##  perfect simple group L_2(8).
##
#H  @(#)$Id$
##
#Y  Copyright (C) 2000  Centre for Discrete Mathematics and Computing
#Y                      Department of Computer Science & Electrical Eng.
#Y                      University of Queensland, Australia.
##
Revision.doL28_g :=
    "@(#)$Id$";

if not IsBound(PGRelFind) then
  RequirePackage("ace", "3.0");
  ACEReadResearchExample("pgrelfind.g");
fi;
## Begin
L2_8;
L28 := TranslatePresentation([a,b], L2_8.rels, L2_8.suwo, [a^3*b, a^2*b]);
PushOptions( rec(ACEworkspace := 10^3) ); # Override the default 10^8
                                          # for the entire session
                                          # ... just to speed things along
# No options ... except for ACEworkspace
PGRelFind(L28.gens, L28.rels, L28.suwo);
# Using option "head"
x := L28.gens[1]; y := L28.gens[2]; # So we can use x, y in head option
PGRelFind(L28.gens, L28.rels, L28.suwo : head := x*y*x*y*x*y^-1*x*y*x*y);
# Using option "ACEworkspace"
PGRelFind(L28.gens, L28.rels, L28.suwo : ACEworkspace := 10^3);
# Using option "Ntails" ... Ntails should be <= 2048
PGRelFind(L28.gens, L28.rels, L28.suwo : Ntails := 256);
# Using option "maxTailLength" (overrides Ntails if used)
PGRelFind(L28.gens, L28.rels, L28.suwo : maxTailLength := 6);
# Using option "minMiddleLength"
PGRelFind(L28.gens, L28.rels, L28.suwo : minMiddleLength := 6);
# Using option "maxMiddleLength"
PGRelFind(L28.gens, L28.rels, L28.suwo : maxMiddleLength := 20);
# Using option "Nrandom"
PGRelFind(L28.gens, L28.rels, L28.suwo : Nrandom := 1000);
# Using option "Nrandom" again ... but this time Nrandom is a function
#                                  of middle length
PGRelFind(L28.gens, L28.rels, L28.suwo
          : Nrandom := len -> 1000 * (LogInt(len + 1, 2) + 1));
FlushOptionsStack(); # Just so we don't get ACE warnings
## End

#E  doL28.g . . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here 
