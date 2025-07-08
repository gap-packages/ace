
% styles.mp - Colin Ramsay - 30 Jan 01
%
% The diagrams for styles 0-2, 5-7 & 8.

% u is our `unit of measure'.  The default value is the normal PostScript
% "big point" of 1/72 inch.  vs is the vertical spacing, and hs is the
% the horizontal spacing between the three (major) columns & their items.
outputtemplate := "%j.%c.eps"; % to produce *.eps suffixed files
numeric u,vs,hs;  
u = 1bp;  vs = 72u;  hs = 108u;

% The `parameters' of the items.
numeric w,ww,www;  			% width, x1/2, x1/3 & x1/4
	w=72u; ww=w/2; www=w/3; wwww=w/4;
numeric h,hh;  h = 36u;  hh = h/2;	% height & half height
numeric p[];  p1 = 16u;  p2 = 12u;	% width.height of UL box
              p3 = 12u;			% height of lower box(es)
              p4 = 16u;  p5 = 12u;	% width.height of upper box

% The za[], zb[] & zc[] are the centers of the items in the columns.  
% Cols run from left to right (0-2, and items from bottom to top (0-6).
pair za[], zb[], zc[];

za0 = (0,0);
za1 = (0,vs);
za2 = (0,2vs);
za3 = (0,3vs);
za4 = (0,4vs);
za5 = (0,5vs);
za6 = (0,6vs);

zb0 = (hs,0);
zb1 = (hs,vs);
zb2 = (hs,2vs);
zb3 = (hs,3vs);
zb4 = (hs,4vs);
zb5 = (hs,5vs);
zb6 = (hs,6vs);

zc0 = (2hs,0);
zc1 = (2hs,vs);
zc2 = (2hs,2vs);
zc3 = (2hs,3vs);
zc4 = (2hs,4vs);
zc5 = (2hs,5vs);
zc6 = (2hs,6vs);

% This macro draws the basic item.
def maa(expr aa) =
  pickup pencircle scaled 1bp;
  draw aa+(-ww,-hh)--aa+(ww,-hh)--aa+(ww,hh)--aa+(-ww,hh)--cycle;
  pickup defaultpen;
  draw aa+(-ww,-hh)+(0,p3)--aa+(ww,-hh)+(0,p3);
  draw aa+(-ww,hh)+(0,-p2)--aa+(-ww,hh)+(p1,-p2)--aa+(-ww,hh)+(p1,0);
  draw 
    aa+(-0.5p4,hh)--aa+(-0.5p4,hh-p5)--aa+(0.5p4,hh-p5)--aa+(0.5p4,hh);
enddef;

% This macro adds the state number.
def mab(expr aa, ab) =
  label.lrt(aa,ab+(-ww,hh));
enddef;

% This macro adds the action number, or "-".
def mac(expr aa, ab) =
  label.bot(aa,ab+(0,hh));
enddef;

% This macro adds the label.
def mad(expr aa, ab) = 
  label(aa,ab);
enddef;

% This macro adds a single result line.
def mae(expr aa, ab) =
  label.top(aa,ab+(0,-hh)); 
enddef;

% This macro adds a double result line.
def maf(expr aa,ab,ac) =
  label.top(aa,ac+(-wwww,-hh));
  label.top(ab,ac+(wwww,-hh));
  draw ac+(0,-hh)--ac+(0,-hh+p3);
enddef;

% This macro adds a triple result line.
def mag(expr aa,ab,ac,ad) =
  label.top(aa,ad+(-www,-hh));
  label.top(ab,ad+(0,-hh));
  label.top(ac,ad+(www,-hh));
  draw ad+(-ww+www,-hh)--ad+(-ww+www,-hh+p3);
  draw ad+(ww-www,-hh)--ad+(ww-www,-hh+p3);
enddef;

%% Style 0, R/C; ie, R<0 & C<0 or R=C=0 (defaulted R & C values).

beginfig(0);

maa(za0);
mab("31",za0);
mac("6",za0);
mad("Chk",za0);
maf(btex $>0$ etex, btex $-256$ etex, za0);

maa(za3);
mab("44",za3);
mac("7",za3);
mad("SG",za3);
mag(btex $-260$ etex, btex $-2$ etex, btex $-1$ etex, za3);
drawarrow(za3+(0,-hh)..za0+(0,hh));
drawarrow(za3+(www,-hh){down}..{down}zb2+(0,hh));

maa(za4);
mab("19",za4);
mac("-",za4);
mad("Redo",za4);
mae(btex $-1$ etex,  za4);
drawarrow(za4+(0,-hh)..za3+(0,hh));
label.top(btex isave $\leftarrow$ F etex, za4+(0,hh+12pt));
label.top(btex cdapp $\leftarrow$ F etex, za4+(0,hh));

maa(zb1);
mab("46",zb1);
mac("5",zb1);
mad("CL",zb1);
maf(btex $-2$ etex, btex $-1$ etex, zb1);
drawarrow(zb1+(-wwww,-hh){down}..{down}za0+(0,hh));
drawarrow(zb1+(wwww,-hh)..zb1+(wwww,-hh-h));
label.bot(btex To state 41 etex, zb1+(wwww,-hh-h));
label.bot(btex of CR style etex, zb1+(wwww,-hh-h-12pt));
label.lrt(btex cdapp $\leftarrow$ T etex, zb1+(wwww,-hh-2pt));

maa(zb2);
mab("45",zb2);
mac("1",zb2);
mad("RD",zb2);
mag(btex $-2$ etex, "0",btex $-1$ etex, zb2);
drawarrow(zb2+(-www,-hh){down}..zb1+(-w-wwww,0)..{down}za0+(0,hh));
drawarrow(zb2+(0,-hh)..zb1+(0,hh));
drawarrow(zb2+(www,-hh){down}..zb2+(w,0)..{down}zb2+(0,hh));
label.lrt(btex isave $\leftarrow$ T etex, zb2+(0,-h-3pt));

maa(zb4);
mab("1",zb4);
mac("-",zb4);
mad("Start",zb4);
mae(btex $-1$ etex,  zb4);
drawarrow(zb4+(0,-hh){down}..{down}za3+(0,hh));
label.top(btex isave $\leftarrow$ F etex, zb4+(0,hh+12pt));
label.top(btex cdapp $\leftarrow$ F etex, zb4+(0,hh));

maa(zc4);
mab("10",zc4);
mac("-",zc4);
mad("Continue",zc4);
mae(btex $-1$ etex,  zc4);
drawarrow(zc4+(0,-hh){down}..{down}zb2+(0,hh));
label.top(btex isave $\leftarrow$ F etex, zc4+(0,hh+12pt));
label.top(btex cdapp $\leftarrow$ F etex, zc4+(0,hh));

endfig;

%% Style 1, R*; ie, R<0 & C=0.

beginfig(1);

maa(za0);
mab("31",za0);
mac("6",za0);
mad("Chk",za0);
maf(btex $>0$ etex, btex $-256$ etex, za0);

maa(za4);
mab("59",za4);
mac("5",za4);
mad("CL", za4);
maf(btex $-2$ etex, btex $-1$ etex, za4);
label.lrt(btex cdapp $\leftarrow$ T etex, za4+(wwww+4pt,-hh-2pt));
drawarrow(za4+(-wwww,-hh){down}..{down}za0+(0,hh));
drawarrow(za4+(wwww,-hh){down}..{down}zb3+(0,hh));

maa(za5);
mab("20",za5);
mac("-",za5);
mad("Redo",za5);
mae(btex $-1$ etex,  za5);
label.top(btex isave $\leftarrow$ T etex, za5+(0,hh));
drawarrow(za5+(0,-hh)..za4+(0,hh));

maa(zb3);
mab("60",zb3);
mac("7",zb3);
mad("SG",zb3);
mag(btex $-2$ etex, btex $-1$ etex, btex $-260$ etex, zb3);
drawarrow(zb3+(-www,-hh){down}..{down}za0+(0,hh));
drawarrow(zb3+(0,-hh){down}..{down}zc2+(0,hh));

maa(zb5);
mab("2",zb5);
mac("-",zb5);
mad("Start",zb5);
mae(btex $-1$ etex,  zb5);
label.top(btex isave $\leftarrow$ T etex, zb5+(0,hh+12pt));
label.top(btex cdapp $\leftarrow$ T etex ,zb5+(0,hh));
drawarrow(zb5+(0,-hh)..zb3+(0,hh));

maa(zc1);
mab("62",zc1);
mac("3",zc1);
mad("CO",zc1);
maf("0", btex $-1$ etex, zc1);
drawarrow(zc1+(wwww,-hh){down}..zc1+(w,-hh)..zc2+(w,hh)
	..{down}zc2+(0,hh));

maa(zc2);
mab("61",zc2);
mac("9",zc2);
mad("RP",zc2);
mag(btex $-2$ etex, "0", btex $-1$ etex, zc2);
drawarrow(zc2+(-www,-hh){down}..zc1+(-w,hh)..{down}za0+(0,hh));
drawarrow(zc2+(0,-hh)..zc1+(0,hh));
drawarrow(zc2+(www,-hh){down}..zc2+(www+www,-hh)..zc2+(www+www,hh)
	..{down}zc2+(0,hh));

maa(zc5);
mab("11",zc5);
mac("-",zc5);
mad("Continue",zc5);
mae(btex $-1$ etex,  zc5);
label.top(btex isave $\leftarrow$ T etex, zc5+(0,hh));
drawarrow(zc5+(0,-hh)..zc2+(0,hh));

endfig;

%% Style 2, Cr; ie, R<0 & C>0.

beginfig(2);

maa(za0);
mab("31",za0);
mac("6",za0);
mad("Chk",za0);
maf(btex $>0$ etex, btex $-256$ etex, za0);

maa(za5);
mab("47",za5);
mac("5",za5);
mad("CL",za5);
maf(btex $-2$ etex, btex $-1$ etex, za5);
label.lrt(btex cdapp $\leftarrow$ T etex, za5+(wwww+4pt,-hh-2pt));
drawarrow(za5+(-wwww,-hh){down}..{down}za2..za0+(0,hh));
drawarrow(za5+(wwww,-hh){down}..{down}zb4+(0,hh));

maa(za6);
mab("21",za6);
mac("-",za6);
mad("Redo",za6);
mae(btex $-1$ etex, za6);
label.top(btex isave $\leftarrow$ T etex, za6+(0,hh));
drawarrow(za6+(0,-hh)..za5+(0,hh));

maa(zb3);
mab("49",zb3);
mac("8",zb3);
mad("RS",zb3);
mag(btex $-2$ etex, "0", btex $-1$ etex, zb3);
drawarrow(zb3+(-www,-hh){down}..{down}za0+(0,hh));
drawarrow(zb3+(0,-hh){down}..zb3+(ww,-h-hh)..{down}zc2+(0,hh));
drawarrow(zb3+(www,-hh){down}..{down}zc2+(0,hh));

maa(zb4);
mab("48",zb4);
mac("7",zb4);
mad("SG",zb4);
mag(btex $-2$ etex, btex $-1$ etex, btex $-260$ etex, zb4);
drawarrow(zb4+(-www,-hh){down}..zb3+(wwww-w,hh)..{down}za2..za0+(0,hh));
drawarrow(zb4+(0,-hh)..zb3+(0,hh));

maa(zb6);
mab("3",zb6);
mac("-",zb6);
mad("Start",zb6);
mae(btex $-1$ etex, zb6);
label.top(btex isave $\leftarrow$ T etex, zb6+(0,hh+12pt));
label.top(btex cdapp $\leftarrow$ T etex ,zb6+(0,hh));
drawarrow(zb6+(0,-hh)..zb4+(0,hh));

maa(zc0);
mab("52",zc0);
mac("1",zc0);
mad("RD",zc0);
mag(btex $-2$ etex, "0", btex $-1$ etex, zc0);
label.bot(btex To state 35 etex, zc0+(wwww,-hh-h));
label.bot(btex of C style etex, zc0+(wwww,-hh-h-12pt));
drawarrow(zc0+(-www,-hh){down}..zb0..{down}za0+(0,hh));
drawarrow(zc0+(0,-hh){down}..{down}zc0+(0.5www,-hh-h));
drawarrow(zc0+(www,-hh){down}..{down}zc0+(0.5www,-hh-h));

maa(zc1);
mab("51",zc1);
mac("3",zc1);
mad("CO",zc1);
maf("0", btex $-1$ etex, zc1);
drawarrow(zc1+(wwww,-hh){down}..{down}zc0+(0,hh));

maa(zc2);
mab("50",zc2);
mac("4",zc2);
mad("CD",zc2);
mag(btex $-2$ etex, "0", btex $-1$ etex, zc2);
drawarrow(zc2+(-www,-hh){down}..zc1+(-w,hh)..{down}za0+(0,hh));
drawarrow(zc2+(0,-hh)..zc1+(0,hh));
drawarrow(zc2+(www,-hh){down}..zc1+(w,0)..zc0+(ww,h)..{down}zc0+(0,hh));

maa(zc6);
mab("12",zc6);
mac("-",zc6);
mad("Continue",zc6);
mae(btex $-1$ etex, zc6);
label.top(btex isave $\leftarrow$ T etex, zc6+(0,hh));
drawarrow(zc6+(0,-hh)..zc2+(0,hh));

endfig;

%% Style 5, C; ie, R=0 & C>0.

beginfig(5);

maa(za0);
mab("31",za0);
mac("6",za0);
mad("Chk",za0);
maf(btex $>0$ etex, btex $-256$ etex, za0);

maa(za5);
mab("32",za5);
mac("5",za5);
mad("CL",za5);
maf(btex $-2$ etex, btex $-1$ etex, za5);
label.lrt(btex cdapp $\leftarrow$ T etex, za5+(wwww+4pt,-hh-2pt));
drawarrow(za5+(-wwww,-hh){down}..{down}za2..za0+(0,hh));
drawarrow(za5+(wwww,-hh){down}..{down}zb4+(0,hh));

maa(za6);
mab("24",za6);
mac("-",za6);
mad("Redo",za6);
mae(btex $-1$ etex, za6);
label.top(btex isave $\leftarrow$ T etex, za6+(0,hh));
drawarrow(za6+(0,-hh)..za5+(0,hh));

maa(zb3);
mab("34",zb3);
mac("8",zb3);
mad("RS",zb3);
mag(btex $-2$ etex, "0", btex $-1$ etex, zb3);
drawarrow(zb3+(-www,-hh){down}..{down}za0+(0,hh));
drawarrow(zb3+(0,-hh){down}..zb3+(ww,-h-hh)..{down}zc2+(0,hh));
drawarrow(zb3+(www,-hh){down}..{down}zc2+(0,hh));

maa(zb4);
mab("33",zb4);
mac("7",zb4);
mad("SG",zb4);
mag(btex $-2$ etex, btex $-1$ etex, btex $-260$ etex, zb4);
drawarrow(zb4+(-www,-hh){down}..zb3+(wwww-w,hh)..{down}za2..za0+(0,hh));
drawarrow(zb4+(0,-hh)..zb3+(0,hh));

maa(zb6);
mab("6",zb6);
mac("-",zb6);
mad("Start",zb6);
mae(btex $-1$ etex, zb6);
label.top(btex isave $\leftarrow$ T etex, zb6+(0,hh+12pt));
label.top(btex cdapp $\leftarrow$ T etex ,zb6+(0,hh));
drawarrow(zb6+(0,-hh)..zb4+(0,hh));

maa(zc1);
mab("36",zc1);
mac("3",zc1);
mad("CO",zc1);
maf("0", btex $-1$ etex, zc1);
drawarrow(zc1+(wwww,-hh){down}..zc1+(w,-hh)..zc2+(w,hh)
	..{down}zc2+(0,hh));

maa(zc2);
mab("35",zc2);
mac("4",zc2);
mad("CD",zc2);
mag(btex $-2$ etex, "0", btex $-1$ etex, zc2);
drawarrow(zc2+(-www,-hh){down}..zc1+(-w,hh)..{down}za0+(0,hh));
drawarrow(zc2+(0,-hh)..zc1+(0,hh));
drawarrow(zc2+(www,-hh){down}..zc2+(www+www,-hh)..zc2+(www+www,hh)
	..{down}zc2+(0,hh));

maa(zc6);
mab("15",zc6);
mac("-",zc6);
mad("Continue",zc6);
mae(btex $-1$ etex, zc6);
label.top(btex isave $\leftarrow$ T etex, zc6+(0,hh));
drawarrow(zc6+(0,-hh)..zc2+(0,hh));

endfig;

%% Style 6, Rc; ie, R>0 & C<0.

beginfig(6);

maa(za0);
mab("31",za0);
mac("6",za0);
mad("Chk",za0);
maf(btex $>0$ etex, btex $-256$ etex, za0);

maa(za5);
mab("53",za5);
mac("7",za5);
mad("SG",za5);
mag(btex $-260$ etex, btex $-2$ etex, btex $-1$ etex, za5);
drawarrow(za5+(0,-hh)..za0+(0,hh));
drawarrow(za5+(www,-hh){down}..{down}zb4+(0,hh));

maa(za6);
mab("25",za6);
mac("-",za6);
mad("Redo",za6);
mae(btex $-1$ etex,  za6);
drawarrow(za6+(0,-hh)..za5+(0,hh));
label.top(btex isave $\leftarrow$ F etex, za6+(0,hh+12pt));
label.top(btex cdapp $\leftarrow$ F etex, za6+(0,hh));

maa(zb1);
mab("58",zb1);
mac("4",zb1);
mad("CD",zb1);
mag(btex $-2$ etex, "0", btex $-1$ etex, zb1);
drawarrow(zb1+(-www,-hh){down}..{down}za0+(0,hh));
drawarrow(zb1+(0,-hh)..zb1+(0,-2h));
drawarrow(zb1+(www,-hh)..zb1+(www,-2h));
label.lft(btex cdapp $\leftarrow$ F etex, zb1+(0,6pt-h-hh));
label.lft(btex To state 30 etex, zb1+(-2pt,-h-hh-12pt));
label.lft(btex of R style etex, zb1+(-2pt,-h-hh-24pt));
label.rt(btex cdapp $\leftarrow$ F etex, zb1+(www,6pt-h-hh));
label.rt(btex To state 28 etex, zb1+(www+2pt,-h-hh-12pt));
label.rt(btex of R style etex, zb1+(www+2pt,-h-hh-24pt));

maa(zb2);
mab("57",zb2);
mac("3",zb2);
mad("CO",zb2);
maf("0", btex $-1$ etex, zb2);
drawarrow(zb2+(wwww,-hh){down}..{down}zb1+(0,hh));

maa(zb3);
mab("55",zb3);
mac("5",zb3);
mad("CL",zb3);
maf(btex $-2$ etex, btex $-1$ etex, zb3);
draw zb3+(-wwww,-hh){down}..{left}zb3+(-wwww-1.25h,-1.25h)
	..{down}za1+(0,hh);
drawarrow(zb3+(wwww,-hh){down}..{down}zb2+(0,hh));
label.lrt(btex cdapp $\leftarrow$ T etex, zb3+(www-6pt,-hh-2pt));

maa(zb4);
mab("54",zb4);
mac("1",zb4);
mad("RD",zb4);
mag(btex $-2$ etex, "0", btex $-1$ etex, zb4);
draw zb4+(-www,-hh){down}..zb3+(-ww-www,hh)..{down}za2+(0,hh);
drawarrow(zb4+(0,-hh)..zb3+(0,hh));
drawarrow(zb4+(www,-hh){down}..{down}zc3+(0,hh));

maa(zb6);
mab("7",zb6);
mac("-",zb6);
mad("Start",zb6);
mae(btex $-1$ etex,  zb6);
drawarrow(zb6+(0,-hh){down}..{down}za5+(0,hh));
label.top(btex isave $\leftarrow$ F etex, zb6+(0,hh+12pt));
label.top(btex cdapp $\leftarrow$ F etex, zb6+(0,hh));

maa(zc3);
mab("56",zc3);
mac("5",zc3);
mad("CL",zc3);
maf(btex $-2$ etex, btex $-1$ etex, zc3);
draw zc3+(-wwww,-hh){down}..{left}zc3+(-wwww-1.25h,-1.25h)
	--zb3+(0.2h,-1.25h);
draw zb3+(-0.075h,-1.25h)--zb3+(-wwww-1.25h,-1.25h);
drawarrow(zc3+(wwww,-hh){down}..zb1+(ww,h)..{down}zb1+(0,hh));
label.lrt(btex cdapp $\leftarrow$ T etex, zc3+(www-4pt,-hh-2pt));

maa(zc6);
mab("16",zc6);
mac("-",zc6);
mad("Continue",zc6);
mae(btex $-1$ etex,  zc6);
drawarrow(zc6+(0,-hh){down}..{down}zb4+(0,hh));
label.top(btex isave $\leftarrow$ F etex, zc6+(0,hh+12pt));
label.top(btex cdapp $\leftarrow$ F etex, zc6+(0,hh));

endfig;

%% Style 7, R; ie, R>0 & C=0;

beginfig(7);

maa(za0);
mab("31",za0);
mac("6",za0);
mad("Chk",za0);
maf(btex $>0$ etex, btex $-256$ etex, za0);

maa(za4);
mab("37",za4);
mac("7",za4);
mad("SG",za4);
mag(btex $-260$ etex, btex $-2$ etex, btex $-1$ etex, za4);
drawarrow(za4+(0,-hh)..za0+(0,hh));
drawarrow(za4+(www,-hh){down}..{down}zb3+(0,hh));

maa(za5);
mab("26",za5);
mac("-",za5);
mad("Redo",za5);
mae(btex $-1$ etex,  za5);
drawarrow(za5+(0,-hh)..za4+(0,hh));
label.top(btex isave $\leftarrow$ F etex, za5+(0,hh+12pt));
label.top(btex cdapp $\leftarrow$ F etex, za5+(0,hh));

maa(zb1);
mab("30",zb1);
mac("3",zb1);
mad("CO",zb1);
maf("0", btex $-1$ etex, zb1);
drawarrow(zb1+(www,-hh){down}..zb1+(w+ww,hh)..zb3+(w+ww,-hh)
	..zb3+(ww,h+hh)..{down}zb3+(0,hh));

maa(zb2);
mab("29",zb2);
mac("2",zb2);
mad("Lx",zb2);
maf(btex $-2$ etex, btex $-1$ etex, zb2);
drawarrow(zb2+(-wwww,-hh){down}..zb1+(-ww-wwww,hh)..{down}za0+(0,hh));
drawarrow(zb2+(wwww,-hh){down}..{down}zb1+(0,hh));

maa(zb3);
mab("28",zb3);
mac("1",zb3);
mad("RD",zb3);
mag(btex $-2$ etex, "0", btex $-1$ etex, zb3);
drawarrow(zb3+(-www,-hh){down}..zb2+(-ww-wwww,hh)..{down}za0+(0,hh));
drawarrow(zb3+(0,-hh)..zb2+(0,hh));
drawarrow(zb3+(www,-hh){down}..zb3+(w,0)..{down}zb3+(0,hh));

maa(zb5);
mab("8",zb5);
mac("-",zb5);
mad("Start",zb5);
mae(btex $-1$ etex,  zb5);
drawarrow(zb5+(0,-hh){down}..{down}za4+(0,hh));
label.top(btex isave $\leftarrow$ F etex, zb5+(0,hh+12pt));
label.top(btex cdapp $\leftarrow$ F etex, zb5+(0,hh));

maa(zc5);
mab("17",zc5);
mac("-",zc5);
mad("Continue",zc5);
mae(btex $-1$ etex,  zc5);
drawarrow(zc5+(0,-hh){down}..{down}zb3+(0,hh));
label.top(btex isave $\leftarrow$ F etex, zc5+(0,hh+12pt));
label.top(btex cdapp $\leftarrow$ F etex, zc5+(0,hh));

endfig;

%% Style 8, CR; ie, R>0 & C>0.

beginfig(8);

maa(za0);
mab("31",za0);
mac("6",za0);
mad("Chk",za0);
maf(btex $>0$ etex, btex $-256$ etex, za0);

maa(za5);
mab("38",za5);
mac("5",za5);
mad("CL",za5);
maf(btex $-2$ etex, btex $-1$ etex, za5);
label.lrt(btex cdapp $\leftarrow$ T etex, za5+(wwww+4pt,-hh-2pt));
drawarrow(za5+(-wwww,-hh){down}..{down}za2..za0+(0,hh));
drawarrow(za5+(wwww,-hh){down}..{down}zb4+(0,hh));

maa(za6);
mab("27",za6);
mac("-",za6);
mad("Redo",za6);
mae(btex $-1$ etex, za6);
label.top(btex isave $\leftarrow$ T etex, za6+(0,hh));
drawarrow(za6+(0,-hh)..za5+(0,hh));

maa(zb3);
mab("40",zb3);
mac("8",zb3);
mad("RS",zb3);
mag(btex $-2$ etex, "0", btex $-1$ etex, zb3);
drawarrow(zb3+(-www,-hh){down}..{down}za0+(0,hh));
drawarrow(zb3+(0,-hh){down}..zb3+(ww,-h-hh)..{down}zc2+(0,hh));
drawarrow(zb3+(www,-hh){down}..{down}zc2+(0,hh));

maa(zb4);
mab("39",zb4);
mac("7",zb4);
mad("SG",zb4);
mag(btex $-2$ etex, btex $-1$ etex, btex $-260$ etex, zb4);
drawarrow(zb4+(-www,-hh){down}..zb3+(wwww-w,hh)..{down}za2..za0+(0,hh));
drawarrow(zb4+(0,-hh)..zb3+(0,hh));

maa(zb6);
mab("9",zb6);
mac("-",zb6);
mad("Start",zb6);
mae(btex $-1$ etex, zb6);
label.top(btex isave $\leftarrow$ T etex, zb6+(0,hh+12pt));
label.top(btex cdapp $\leftarrow$ T etex ,zb6+(0,hh));
drawarrow(zb6+(0,-hh)..zb4+(0,hh));

maa(zc0);
mab("43",zc0);
mac("1",zc0);
mad("RD",zc0);
mag(btex $-2$ etex, "0", btex $-1$ etex, zc0);
drawarrow(zc0+(-www,-hh){down}..zb0..{down}za0+(0,hh));
draw zc0+(0,-hh){down}..tension 1.3..{up}zc0+(w+wwww,-hh);
drawarrow(zc0+(www,-hh){down}..tension 1.3..zc0+(w+wwww,-hh){up}
	..{up}zc2+(w+wwww,hh)..tension 1.3..{down}zc2+(0,hh));

maa(zc1);
mab("42",zc1);
mac("3",zc1);
mad("CO",zc1);
maf("0", btex $-1$ etex, zc1);
drawarrow(zc1+(wwww,-hh){down}..{down}zc0+(0,hh));

maa(zc2);
mab("41",zc2);
mac("4",zc2);
mad("CD",zc2);
mag(btex $-2$ etex, "0", btex $-1$ etex, zc2);
drawarrow(zc2+(-www,-hh){down}..zc1+(-w,hh)..{down}za0+(0,hh));
drawarrow(zc2+(0,-hh)..zc1+(0,hh));
drawarrow(zc2+(www,-hh){down}..zc1+(w,0)..zc0+(ww,h)..{down}zc0+(0,hh));

maa(zc6);
mab("18",zc6);
mac("-",zc6);
mad("Continue",zc6);
mae(btex $-1$ etex, zc6);
label.top(btex isave $\leftarrow$ T etex, zc6+(0,hh));
drawarrow(zc6+(0,-hh)..zc2+(0,hh));

endfig;

end;

