#############################################################################
##
#W  example.gi              ACE Share Package                     Greg Gamble
##
##  This  file  installs  a  number of  ACE  example functions,  for examples
##  originally  written  as  ACE  input  files  by  Colin Ramsay,  and  later
##  translated to GAP by Greg Gamble.
##    
#H  @(#)$Id$
##
#Y  Copyright (C) 2000  Centre for Discrete Mathematics and Computing
#Y                      Department of Computer Science & Electrical Eng.
#Y                      University of Queensland, Australia.
##
Revision.example_gi :=
    "@(#)$Id$";

##
##  The functions defined here are:
##     ACEexample0,  ACEexample0a, ACEexample1,  ACEexample1a, ACEexample1b,
##     ACEexample1c, ACEexample2,  ACEexample2a, ACEexample2b, ACEexample5,
##     ACEexample6,  ACEexample7,  ACEexample7a, ACEexample7b, ACEexample7c
##
##  The names may seem odd,  since there are apparent  omissions.  The  names
##  are of the form ACEexample<n><a>,  where <n> is an integer that indicates
##  the example is drawn from Colin Ramsay's `test0..<n>.in' (where 0..<n> is
##  the integer <n>,  0-filled on the left to 3 digits)  and <a> is either an
##  empty string  or a letter that distinguishes examples drawn from the same
##  file.
##

#############################################################################
##
#F  ACEexample0() . . . enumerate the cosets of the Identity subgroup of M_12
##  . . . . . . . . . . . . . . . . . . . . . . . . . . . by the HLT strategy
##
InstallGlobalFunction(ACEexample0, function()
  local f, M12, a, b, c;
  # We define the Matthieu group M_12 on 3 generators. |M_12| = 95040
  f := FreeGroup("a","b","c"); a := f.1; b := f.2; c := f.3;
  M12 := f / [a^11, b^2, c^2, (a*b)^3, (a*c)^3, (b*c)^10,
              c*b*c*b*a*b*c*b*c*a^-5];
  # 3rd argument to ACECosetTable() are the generators of the Identity subgroup
  return ACECosetTable(FreeGeneratorsOfFpGroup(M12), RelatorsOfFpGroup(M12), [] :
                 # Options that ACECosetTable processes first anyway
                 echo, enum := "M_12", subg := "Id", 
                 # Other options
                 mess := 25000, hlt, wo := "1250k");
end);

#############################################################################
##
#F  ACEexample0a()  . . enumerate the cosets of the Identity subgroup of M_12
##  . . . . . . . . . . . . . . . . . . . . . . . .  by the Felsch 1 strategy
##
InstallGlobalFunction(ACEexample0a, function()
  local f, M12, a, b, c;
  # We define the Matthieu group M_12 on 3 generators. |M_12| = 95040
  f := FreeGroup("a","b","c"); a := f.1; b := f.2; c := f.3;
  M12 := f / [a^11, b^2, c^2, (a*b)^3, (a*c)^3, (b*c)^10,
              c*b*c*b*a*b*c*b*c*a^-5];
  # 3rd argument to ACECosetTable() is the generators of the Identity subgroup
  return ACECosetTable(FreeGeneratorsOfFpGroup(M12), RelatorsOfFpGroup(M12), [] :
                 # Options that ACECosetTable processes first anyway
                 echo, enum := "M_12", subg := "Id", 
                 # Other options
                 mess := 25000, felsch := 1, wo := "1250k",
                 pmod := 2,     fill := 4,   dsiz := 2000);
end);

#############################################################################
##
#F  ACEexample1()  . . . . enumerate [G : H] where |G : H| = 1 and |G| = 2^17
##  . . . . . . . . . . . . . . . . . . . . . . . .  by the Felsch 1 strategy
##
InstallGlobalFunction(ACEexample1, function()
  local f, g, a, b, c, A, B, C;
  # We define a group G of order 2^17 on 3 generators
  f := FreeGroup("a","b","c"); a := f.1;  b := f.2;  c := f.3;
                               A := a^-1; B := b^-1; C := c^-1;
  g := f / [a*B*C*b*a*c, b*A*C*b*a*a*c*A, a*c*c*A*A*B*a*b];
  # 3rd argument to ACECosetTable() is the generators of the subgroup
  return ACECosetTable(FreeGeneratorsOfFpGroup(g), RelatorsOfFpGroup(g), 
                 [a*B*C*b*a*c, b*A*C*b*a*a*c*A, a*c*c*A*A*B*a*b] :
                 # Options that ACECosetTable processes first anyway
                 echo, enum := "G (order 2^17)", subg := "H (index 1)", 
                 # Other options
                 mess := 10000, felsch := 1, wo := "2500k");
end);

#############################################################################
##
#F  ACEexample1a() . . . . enumerate [G : H] where |G : H| = 2 and |G| = 2^18
##  . . . . . . . . . . . . . . . . . . . . . . . .  by the Felsch 1 strategy
##
InstallGlobalFunction(ACEexample1a, function()
  local f, g, a, b, c, x, A, B, C;
  # We define a group G of order 2^18 on 4 generators
  f := FreeGroup("a","b","c","x"); a := f.1;  b := f.2;  c := f.3;  x := f.4;
                                   A := a^-1; B := b^-1; C := c^-1;
  g := f / [a*B*C*b*a*c, b*A*C*b*a*a*c*A, a*c*c*A*A*B*a*b,
            x^2, Comm(a,x), Comm(b,x), Comm(c,x)];
  # 3rd argument to ACECosetTable() is the generators of the subgroup
  return ACECosetTable(FreeGeneratorsOfFpGroup(g), RelatorsOfFpGroup(g), 
                 [a*B*C*b*a*c, b*A*C*b*a*a*c*A, a*c*c*A*A*B*a*b] :
                 # Options that ACECosetTable processes first anyway
                 echo, enum := "G (order 2^18)", subg := "H (index 2)", 
                 # Other options
                 mess := 50000, felsch := 1, wo := "2500k");
end);

#############################################################################
##
#F  ACEexample1b() . . . enumerate [G : H] where |G : H| = 6 and |G| = 2^18.3
##  . . . . . . . . . . . . . . . . . . . . . . . .  by the Felsch 1 strategy
##
InstallGlobalFunction(ACEexample1b, function()
  local f, g, a, b, c, x, y, A, B, C;
  # We define a group G of order 2^18.3 on 5 generators
  f := FreeGroup("a","b","c","x","y"); 
       a := f.1;  b := f.2;  c := f.3;  x := f.4; y := f.5;
       A := a^-1; B := b^-1; C := c^-1;
  g := f / [a*B*C*b*a*c, b*A*C*b*a*a*c*A, a*c*c*A*A*B*a*b,
            x^2, Comm(a,x), Comm(b,x), Comm(c,x),
            y^3, Comm(a,y), Comm(b,y), Comm(c,y), Comm(x,y)];
  # 3rd argument to ACECosetTable() is the generators of the subgroup
  return ACECosetTable(FreeGeneratorsOfFpGroup(g), RelatorsOfFpGroup(g), 
                 [a*B*C*b*a*c, b*A*C*b*a*a*c*A, a*c*c*A*A*B*a*b] :
                 # Options that ACECosetTable processes first anyway
                 echo, enum := "G (order 2^18.3)", subg := "H (index 6)", 
                 # Other options
                 mess := 100000, felsch := 1, wo := "2500k");
end);

#############################################################################
##
#F  ACEexample1c() . . . enumerate [G : H] where |G : H| = 6 and |G| = 2^18.3
##  . . . . . . . . . . . . . . . . . . . . . . . . . .  by the hard strategy
##
InstallGlobalFunction(ACEexample1c, function()
  local f, g, a, b, c, x, y, A, B, C;
  # We define a group G of order 2^18.3 on 5 generators
  f := FreeGroup("a","b","c","x","y"); 
       a := f.1;  b := f.2;  c := f.3;  x := f.4; y := f.5;
       A := a^-1; B := b^-1; C := c^-1;
  g := f / [a*B*C*b*a*c, b*A*C*b*a*a*c*A, a*c*c*A*A*B*a*b,
            x^2, Comm(a,x), Comm(b,x), Comm(c,x),
            y^3, Comm(a,y), Comm(b,y), Comm(c,y), Comm(x,y)];
  # 3rd argument to ACECosetTable() is the generators of the subgroup
  return ACECosetTable(FreeGeneratorsOfFpGroup(g), RelatorsOfFpGroup(g), 
                 [a*B*C*b*a*c, b*A*C*b*a*a*c*A, a*c*c*A*A*B*a*b] :
                 # Options that ACECosetTable processes first anyway
                 echo, enum := "G (order 2^18.3)", subg := "H (index 6)", 
                 # Other options
                 hard, mend, ct := 2000, no := 11, pmod := 2, fill := 2, 
                 dsiz := 15000, mess := 100000, wo := "2500k");
end);

#############################################################################
##
#F  ACEexample2() . . enumerate the cosets of the Identity subgroup of F(2,7)
##  . . . . . . . . . . . . . . . . . . . . . . . . . . by the purec strategy
##
InstallGlobalFunction(ACEexample2, function()
  local f, g, a, b, c, d, e, x, y;
  # We define F(2,7) on 7 generators
  f := FreeGroup("a","b","c","d","e", "x", "y"); 
       a := f.1;  b := f.2;  c := f.3;  d := f.4; 
       e := f.5;  x := f.6;  y := f.7;
  g := f / [a*b*c^-1, b*c*d^-1, c*d*e^-1, d*e*x^-1, 
            e*x*y^-1, x*y*a^-1, y*a*b^-1];
  # 3rd argument to ACECosetTable() are the generators of the Identity subgroup
  return ACECosetTable(FreeGeneratorsOfFpGroup(g), RelatorsOfFpGroup(g), [] :
                 # Options that ACECosetTable processes first anyway
                 echo, enum := "F(2,7), aka C_29", subg := "Id", 
                 # Other options
                 wo := "2m", mess := 25000, purec);
end);

#############################################################################
##
#F  ACEexample2a() .  enumerate the cosets of the Identity subgroup of F(2,7)
##  . . . . . . . . . . . . . . . . . . . . . . . .  by the Felsch 0 strategy
##
InstallGlobalFunction(ACEexample2a, function()
  local f, g, a, b, c, d, e, x, y;
  # We define F(2,7) on 7 generators
  f := FreeGroup("a","b","c","d","e", "x", "y"); 
       a := f.1;  b := f.2;  c := f.3;  d := f.4; 
       e := f.5;  x := f.6;  y := f.7;
  g := f / [a*b*c^-1, b*c*d^-1, c*d*e^-1, d*e*x^-1, 
            e*x*y^-1, x*y*a^-1, y*a*b^-1];
  # 3rd argument to ACECosetTable() are the generators of the Identity subgroup
  return ACECosetTable(FreeGeneratorsOfFpGroup(g), RelatorsOfFpGroup(g), [] :
                 # Options that ACECosetTable processes first anyway
                 echo, enum := "F(2,7), aka C_29", subg := "Id", 
                 # Other options
                 wo := "2m", mess := 25000, felsch);
end);

#############################################################################
##
#F  ACEexample2b() .  enumerate the cosets of the Identity subgroup of F(2,7)
##  . . . . . . . . . . . . . . . . . . . . . . . .  by the Felsch 1 strategy
##
InstallGlobalFunction(ACEexample2b, function()
  local f, g, a, b, c, d, e, x, y;
  # We define F(2,7) on 7 generators
  f := FreeGroup("a","b","c","d","e", "x", "y"); 
       a := f.1;  b := f.2;  c := f.3;  d := f.4; 
       e := f.5;  x := f.6;  y := f.7;
  g := f / [a*b*c^-1, b*c*d^-1, c*d*e^-1, d*e*x^-1, 
            e*x*y^-1, x*y*a^-1, y*a*b^-1];
  # 3rd argument to ACECosetTable() are the generators of the Identity subgroup
  return ACECosetTable(FreeGeneratorsOfFpGroup(g), RelatorsOfFpGroup(g), [] :
                 # Options that ACECosetTable processes first anyway
                 echo, enum := "F(2,7), aka C_29", subg := "Id", 
                 # Other options
                 wo := "2m", mess := 25000, felsch := 1);
end);

#############################################################################
##
#F  ACEexample5() . . .  enumerate the cosets of the Identity subgroup of C_5
##  . . . . . . . . . . . . . . . . . . . . . . . .  by the Felsch 0 strategy
##
InstallGlobalFunction(ACEexample5, function()
  local f, g, a, b;
  # We define C_5 on 2 generators
  f := FreeGroup("a","b"); a := f.1;  b := f.2;
  g := f / [a^5, b];
  # 3rd argument to ACECosetTable() are the generators of the Identity subgroup
  return ACECosetTable(FreeGeneratorsOfFpGroup(g), RelatorsOfFpGroup(g), [] :
                 # Options that ACECosetTable processes first anyway
                 echo, enum := "C_5", subg := "Id", 
                 # Other options
                 mess := -1, felsch);
end);

#############################################################################
##
#F  ACEexample6() . . . . enumerate [SL(2,19) : H] where |SL(2,19) : H| = 180
##  . . . . . . . . . . . . . . . . . . . . . . . . . .  by the hard strategy
##
InstallGlobalFunction(ACEexample6, function()
  local f, g, x, y, X, Y;
  # We define SL(2,19) on 2 generators
  f := FreeGroup("x","y"); x := f.1;  y := f.2; 
                           X := x^-1; Y := y^-1;
  g := f / [x*Y*X*Y*X*Y, Y*X*X*y*x*x, x*y^4*x*y^10*x*y^4*x*y^29*x^12];
  # 3rd argument to ACECosetTable() are the generators of the subgroup H 
  return ACECosetTable(FreeGeneratorsOfFpGroup(g), RelatorsOfFpGroup(g), [y] :
                 # Options that ACECosetTable processes first anyway
                 echo, enum := "SL(2,19)", subg := "H (index 180)", 
                 # Other options
                 mess := 50000, hard, mend, pmod := 2);
end);

#############################################################################
##
#F  ACEexample7() . . enumerate the cosets of the Identity subgroup of cfd0R7 
##  . . . . . . . . . . . . . . . .  (of order 2^17) by the Felsch 1 strategy
##
InstallGlobalFunction(ACEexample7, function()
  local f, g, a, b, c, A, B, C;
  # We define a 3-generator deficiency 0 group cfd0R7 on 3 generators
  f := FreeGroup("a","b","c"); a := f.1;  b := f.2;  c := f.3;
                               A := a^-1; B := b^-1; C := c^-1;
  g := f / [a*B*C*b*a*c, b*A*C*b*a*a*c*A, a*c*c*A*A*B*a*b];
  # 3rd argument to ACECosetTable() are the generators of the Identity subgroup
  return ACECosetTable(FreeGeneratorsOfFpGroup(g), RelatorsOfFpGroup(g), [] :
                 # Options that ACECosetTable processes first anyway
                 echo, enum := "cfd0R7 (order 2^17)", subg := "Id", 
                 # Other options
                 wo := "6m", felsch := 1, mend,   dmod := 3, dsiz := 1000,
                 psiz := 2,  fill := 5,   mess := 100000);
end);

#############################################################################
##
#F  ACEexample7a() . . . . . enumerate [cfd0R7 : H] where |cfd0R7 : H| = 2^14
##  . . . . . . . . . . . . . . .  (|cfd0R7| = 2^17) by the Felsch 1 strategy
##
InstallGlobalFunction(ACEexample7a, function()
  local f, g, a, b, c, A, B, C;
  # We define a 3-generator deficiency 0 group cfd0R7 on 3 generators
  f := FreeGroup("a","b","c"); a := f.1;  b := f.2;  c := f.3;
                               A := a^-1; B := b^-1; C := c^-1;
  g := f / [a*B*C*b*a*c, b*A*C*b*a*a*c*A, a*c*c*A*A*B*a*b];
  # 3rd argument to ACECosetTable() are the generators of the subgroup H
  return ACECosetTable(FreeGeneratorsOfFpGroup(g), RelatorsOfFpGroup(g), [b*c] :
                 # Options that ACECosetTable processes first anyway
                 echo, enum := "cfd0R7 (order 2^17)", 
                 subg := "H (index 2^14)", 
                 # Other options
                 wo := "6m", felsch := 1, mend,   dmod := 3, dsiz := 1000,
                 psiz := 2,  fill := 5,   mess := 50000);
end);

#############################################################################
##
#F  ACEexample7b() . . . . .  enumerate [cfd0R7 : H] where |cfd0R7 : H| = 2^3
##  . . . . . . . . . . . . . . .  (|cfd0R7| = 2^17) by the Felsch 1 strategy
##
InstallGlobalFunction(ACEexample7b, function()
  local f, g, a, b, c, A, B, C;
  # We define a 3-generator deficiency 0 group cfd0R7 on 3 generators
  f := FreeGroup("a","b","c"); a := f.1;  b := f.2;  c := f.3;
                               A := a^-1; B := b^-1; C := c^-1;
  g := f / [a*B*C*b*a*c, b*A*C*b*a*a*c*A, a*c*c*A*A*B*a*b];
  # 3rd argument to ACECosetTable() are the generators of the subgroup H
  return ACECosetTable(FreeGeneratorsOfFpGroup(g), RelatorsOfFpGroup(g), 
                 [b*c, A*B*A*A*b*c*a*b*C] :
                 # Options that ACECosetTable processes first anyway
                 echo, enum := "cfd0R7 (order 2^17)", 
                 subg := "H (index 2^3)", 
                 # Other options
                 wo := "6m", felsch := 1, mend,   dmod := 3, dsiz := 1000,
                 psiz := 2,  fill := 5,   mess := 50000);
end);

#############################################################################
##
#F  ACEexample7c() . . . . . .  enumerate [cfd0R7 : H] where |cfd0R7 : H| = 1
##  . . . . . . . . . . . . . . .  (|cfd0R7| = 2^17) by the Felsch 1 strategy
##
InstallGlobalFunction(ACEexample7c, function()
  local f, g, a, b, c, A, B, C;
  # We define a 3-generator deficiency 0 group cfd0R7 on 3 generators
  f := FreeGroup("a","b","c"); a := f.1;  b := f.2;  c := f.3;
                               A := a^-1; B := b^-1; C := c^-1;
  g := f / [a*B*C*b*a*c, b*A*C*b*a*a*c*A, a*c*c*A*A*B*a*b];
  # 3rd argument to ACECosetTable() are the generators of the subgroup H
  return ACECosetTable(FreeGeneratorsOfFpGroup(g), RelatorsOfFpGroup(g), 
                 [b*c, A*B*A*A*b*c*a*b*C, A*c*c*c*a*c*B*c*A] :
                 # Options that ACECosetTable processes first anyway
                 echo, enum := "cfd0R7 (order 2^17)", 
                 subg := "H (index 1)", 
                 # Other options
                 wo := "6m", felsch := 1, mend,   dmod := 3, dsiz := 1000,
                 psiz := 2,  fill := 5,   mess := 20000);
end);

#E  example.gi  . . . . . . . . . . . . . . . . . . . . . . . . .  ends here 
