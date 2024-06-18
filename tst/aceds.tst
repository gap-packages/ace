#############################################################################
##
#A  aceds.tst                 ACE package                         Greg Gamble
#A                                                             Volkmar Felsch
##
##  A nice example provided by Volkmar Felsch that tests the stability of the
##  ACE interface when bad input is fed to the ACE binary.
##

gap> START_TEST( "Testing ACEDeleteSubgroupGenerators with bad input" );
gap> G := PerfectGroup( IsSubgroupFpGroup, 2^5*60, 2 );
A5 2^4 E N 2^1
gap> F := FreeGroupOfFpGroup( G );
<free group on the generators [ a, b, s, t, u, v, d ]>
gap> a:=F.1;; b:=F.2;; s:=F.3;; t:=F.4;; u:=F.5;; v:=F.6;; d:=F.7;;
gap> fgens := FreeGeneratorsOfFpGroup( G );;                       
gap> rels := RelatorsOfFpGroup( G );        
[ a^2*d^-1, b^3, (a*b)^5, s^2, t^2, u^2, v^2, d^2, s^-1*t^-1*s*t, 
  u^-1*v^-1*u*v, s^-1*u^-1*s*u, s^-1*v^-1*s*v, t^-1*u^-1*t*u, t^-1*v^-1*t*v, 
  a^-1*s*a*u^-1, a^-1*t*a*v^-1, a^-1*u*a*s^-1, a^-1*v*a*t^-1, 
  b^-1*s*b*d^-1*v^-1*t^-1, b^-1*t*b*v^-1*u^-1*t^-1*s^-1, b^-1*u*b*v^-1*u^-1, 
  b^-1*v*b*u^-1, d^-1*a^-1*d*a, d^-1*b^-1*d*b, d^-1*s^-1*d*s, d^-1*t^-1*d*t, 
  d^-1*u^-1*d*u, d^-1*v^-1*d*v ]
gap> i := ACEStart( fgens, rels, [ b, t ] );
1
gap> stats:=ACEStats( i );; Unbind(stats.cputime); stats;
rec( activecosets := 80, cputimeUnits := "10^-2 seconds", index := 80, 
  maxcosets := 123, totcosets := 187 )
gap> ACEDeleteSubgroupGenerators( i, [ t ] );             
[ b ]
gap> lev := InfoLevel(InfoACE);
1
gap> SetInfoLevel(InfoACE, 3);
gap> ACEDeleteSubgroupGenerators( i, [ 2 ] );
#I  ** ERROR (continuing with next line)
#I     first argument out of range
#I  start = yes, continue = yes, redo = yes
#I  ***
#I  INDEX = 640 (a=640 r=1673 h=1 n=1673; l=2 c=0.00; m=811 t=1672)
#I    #--- ACE 3.001: Run Parameters ---
#I  Group Name: G;
#I  Group Relators: (s)^2, (t)^2, (u)^2, (v)^2, (d)^2, aad, (b)^3, (st)^2, 
#I    (uv)^2, (su)^2, (sv)^2, (tu)^2, (tv)^2, Asau, Atav, Auas, Avat, Bvbu, 
#I    dAda, dBdb, (ds)^2, (dt)^2, (du)^2, (dv)^2, Bubvu, Bsbdvt, Btbvuts, 
#I    (ab)^5;
#I  Subgroup Name: H;
#I  Subgroup Generators: b;
#I    #---------------------------------
[ b ]
gap> SetInfoLevel(InfoACE, lev);
gap> ACEQuit(i);
gap> STOP_TEST( "aceds.tst", 1000000 );
