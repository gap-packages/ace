
/**************************************************************************

	control.c
	Colin Ramsay
	18 Jun 98

	ADAPTIVE COSET ENUMERATOR, Version 2.000

	Copyright 1998 
	Centre for Discrete Mathematics and Computing,
	Department of Mathematics and 
	Department of Computer Science & Electrical Engineering,
	The University of Queensland, QLD 4072.

This is the overall controlling code for the coset enumerator.  Rewritten 
by Colin Ramsay, based on code by George Havas et al. and parser by Martin
Schoenert.

The design is by George Havas, who acknowledges valuable discussions with 
many people; including John Cannon, Volkmar Felsch, John Leech, Joachim 
Neubueser, Mike Newman, Martin Schoenert, Charles Sims and Leonard Soicher.

This programme provides a package for the enumeration of cosets of a 
subgroup of a finitely presented group and for the manipulation of partial 
and complete coset tables.

For further reference see: 
  Cannon, Dimino, Havas, Watson, "Implementation and Analysis of the 
    Todd-Coxeter Algorithm", Mathematics of Computation, 27(123), 1973, 
    pp. 463-490.
  J. Neubueser, "An elementary introduction to coset-table methods in 
    computational group theory", Groups - St Andrews 1981, London Math. 
    Soc. Lecture Note Ser. 71, Cambridge University Press, Cambridge, 1984,
    pp. 1-45.
  Havas, "Coset enumeration strategies", ISSAC'91 (Proc. 1991 International
    Symposium on Symbolic and Algebraic Computation, S.M. Watt, editor), 
    ACM Press, New York (1991) 191-199.
  C.C. Sims, Computation with finitely presented groups, Cambridge 
    University Press, Cambridge, 1994.

This programme is based on a long legacy of previous versions: 
  Fortran TC (GH, 1969-75); 
  TC2 (GH and Bill Alford, 1976-88); 
  C language TC3 and TC4 (GH and Kate Lian, 1989-91); 
  TC5 (GH, JXL, Wei Ming Yan and Ian Peake, 1992-1995).

For a full description of this code, see the (yet-to-be-written) document:
  Colin Ramsay, "ACE: an Adaptive Coset Enumerator", Technical Report #2, 
    Centre for Discrete Mathematics and Computing, Department of Computer 
    Science & Electrical Engineering / Department of Mathematics, The 
    University of Queensland, 1998.

**************************************************************************/

#include <ctype.h>

#include "tc.h"

extern void tc_text(int);
extern void tc_apply(int, int*, int*, Logic);
extern void tc_enum(void);
extern void read_commands(void);
extern void setDefaults(void);
extern char *utl_date(void);
extern double utl_clock(void);
extern char *StrDup(char*);

	/******************************************************************
	Mostly, these are the global variables visible across all files.  
	These are declared (& described) in tc.h and are all defined here 
	for convenience.
	******************************************************************/

int workspace, workmult, *cosettable;

FILE *fop, *fip;

char *grpname, *subgrpname, status[3];

double begintime, endtime;
double totaltime;

Logic asis, adapt, restart, rtenum, mendel;
Logic index1, overfl, indexc, compct, mstflg, ctflg;

int maxpar[3], cosetlim, maxrow;	/* cosetlim could be static, since 
					all use is in this file. */
int rinsgpar[3], nrinsgp;
int fipar[3], fillfactor;
int ctpar[3], ctfactor;
int rtpar[3], rtfactor;

int msgctrl, msgincr, lastout, nextout;
int prpar[3];

int *edp, *edpbeg, *edpend;

int nalive, maxcos, totcos;

int ngennum, ngenal, ndgen;
char algen[28];
int *geninv, ncol, **colptr;
int *gencol, *colgen, *invcol;
static int *gencolp;			/* Kludge, to store &gencol[0] */

int ndrel;
Wordlhdr relhdr;
int *relind, *relexp, *rellen, *relators, trellen;
int newrel, newrelpar;

int nsgpg;
Wordlhdr sghdr;
int *subggen, *subgindex, *subglength, tsgenlen;
int newsg, newsgpar;

int knc, knr, nextdf, comppc;

int mstlevel, *mst;

int *crepx;

int topded, disded;

	/******************************************************************
	If we're working on a `normal' Unix box, "uname -n" returns the 
	name of the host, which we print out neatly at the start of a run. 
	(Of course, we could also access this using the "sys:...;" ACE 
	command.)  If required, define __UNIX in the make file.
	******************************************************************/

#ifdef __UNIX
# define HNAME                   \
    fprintf(fop, "host name: "); \
    fflush(fop);                 \
    system("uname -n");
#else
# define HNAME
#endif

        /******************************************************************
	static void initenum(void)

	The one-off initialisation for the enumerator proper.
        ******************************************************************/

static void initenum(void)
  {
  edp = edpbeg = edpend = NULL;
  geninv = NULL;
  colptr = NULL;
  gencol = colgen = invcol = NULL;
  relind = relexp = rellen = relators = NULL;
  subggen = subgindex = subglength = NULL;
  }

        /******************************************************************
	int main(int argc, char *argv[])

	A lot of the stuff in here should actually be folded into
	setDefaults()/initenum().
        ******************************************************************/

int main(int argc, char *argv[])	/* arguments are unused */		
  {
  setvbuf(stdout, NULL, _IOLBF, 0);

  /* Do some interactive i/face initialisation */
  
  fop = stdout;				/* All i/o to stdin/stdout */
  fip = stdin;

  grpname = StrDup("G");		/* Default group name */
  subgrpname = StrDup("H");		/* Default subgroup name */

  setDefaults();			/* Defaults / initial values */

  relhdr = sghdr = NULL;		/* Relator / generator lists */

  /* Do some enumerator initialisation */

  initenum();

  workspace = DEFWORK;
  workmult = 1;
  cosettable = 
    (int*)malloc((long)workspace*(long)workmult*(long)sizeof(int));
  if (cosettable == NULL) 
    { 
    fprintf (fop, "\nUnable to allocate default workspace\n"); 
    exit(-1);
    }

  /* And away we go ... */

  fprintf(fop, "Start ACE 2.000 on %s", utl_date());
  HNAME;

  read_commands(); 			/* execute until EOF */

  fprintf(fop, "\nEnd ACE 2.000 on %s", utl_date());

  return 0;				/* ? change to index, if found */
  }

        /******************************************************************
	static void print_wordl(Wordlhdr wh, int n)

	n is the current output column.  Not quite sure how this would cope
	with a really nasty presentation!  Note that this prints out words
	in exp form.  If no enumeration has yet been run, exp is at its
	default of 1, so a printout will not be `exponentiated'.
        ******************************************************************/

static void print_wordl(Wordlhdr wh, int n)
  {
  Wordlel *wp;
  int i, len;
  char c;

  if (wh != NULL) 
    {
    for (wp = wh->first; wp != NULL; wp = wp->next) 
      {
      len = wp->len / wp->exp;

      if (ngenal == 0) 
        { 					/* numeric generators */
        if (wp->exp == 1) 
          { 
          n += 2 + len*2; 			/* +2 for \ , *2 for \ n */
          if (n > LLL) 
            { 
            fprintf(fop, "\n  "); 
            n = 2+2 + len*2; 
            } 
          }
        else 
          {
          n += 2+4 + len*2; 			/* 4 for ()^e */ 
          if (n > LLL) 
            { 
            fprintf(fop, "\n  "); 
            n = 4+4 + len*2; 
            } 
          fprintf(fop, "(");  
          }

        for (i = 1; i <= len; i++) 
          { fprintf(fop, "%d ", wp->wordgen[i]); }

        if (wp->exp != 1) 
          { fprintf(fop, ")^%d", wp->exp); }
        if (wp->next != NULL && len != 0)	/* len = 0 not poss? */
          { fprintf(fop, ", "); }
        }
      else 
        { 					/* alphabetic generators */
        if (wp->exp == 1) 
          { 
          n += 2 + len;
          if (n > LLL) 
            { 
            fprintf(fop, "\n  "); 
            n = 2+1 + len; 
            } 
          }
        else 
          {
          n += 2+4 + len; 			/* 4 for ()^x */ 
          if (n > LLL) 
            { 
            fprintf(fop, "\n  "); 
            n = 3+4 + len; 
            } 
          fprintf(fop, "(");  
          }

        for (i = 1; i <= len; i++) 
          { 
          c = (wp->wordgen[i] > 0) ? algen[wp->wordgen[i]] 
                                   : toupper(algen[-wp->wordgen[i]]);
          fprintf(fop, "%c", c);
          }

        if (wp->exp != 1) 
          { fprintf(fop, ")^%d", wp->exp); }
        if (wp->next != NULL && len !=0) 
          { fprintf(fop, ", "); }
        }
      }
    }

  fprintf(fop, ";\n"); 
  }

        /******************************************************************
	void printdetails(void)
        ******************************************************************/

void printdetails(void)
  {
  fprintf(fop, "\nEnumeration: %s;\n", grpname);

  if (msgctrl != 0)			/* basic details */ 
    {
    fprintf(fop, "Group Generators: ");
    if (ngennum != 0) 
      { fprintf(fop, "%d;\n", ndgen); }
    else if (ngenal != 0) 
      { fprintf(fop, "%s;\n", &algen[1]); }
    fprintf(fop, "Group Relators: ");
    print_wordl(relhdr, 16);
    fprintf(fop, "Subgroup name: %s;\n", subgrpname);
    fprintf(fop, "Subgroup Generators: ");
    print_wordl(sghdr, 21);
    }

  /* Include adapt, mendelsohn, ... */

  if (msgctrl < 0)			/* further details */
    {
    fprintf(fop, "CT factor:%d,%d,%d; ", ctpar[0], ctpar[1], ctpar[2]);
    fprintf(fop, "RT factor:%d,%d,%d; ", rtpar[0], rtpar[1], rtpar[2]);
    fprintf(fop, "Fill factor:%d,%d,%d; ", fipar[0], fipar[1], fipar[2]);
    fprintf(fop, "NO:%d,%d,%d;\n", rinsgpar[0], rinsgpar[1] , rinsgpar[2]);
    switch (workmult)
      {
      case 1:
        fprintf(fop, "Workspace:%d; ", workspace);
        break;
      case KILO:
        fprintf(fop, "Workspace:%dK; ", workspace);
        break;
      case MEGA:
        fprintf(fop, "Workspace:%dM; ", workspace);
        break;
      case GIGA:
        fprintf(fop, "Workspace:%dG; ", workspace);
        break;
      }
    fprintf(fop, "Max:%d,%d,%d; ", maxpar[0], maxpar[1], maxpar[2]);
    fprintf(fop, "Compact:%d; ", comppc);
    fprintf(fop, "Message:%d;\n", msgctrl);
    }
  }

        /******************************************************************
	static void free_reduce(Wordlhdr h)

	Free reduction of all words in a word list.  Can reduce words to	
	zero length.  This does work, but full understanding may cause
	brain damage!  I think it may rely on a guard value of 0 in
	wordgen[0]?
        ******************************************************************/

static void free_reduce(Wordlhdr h)
  {
  Wordlptr p;
  int l, n, len; 				/* last, next, length */

  if (h == NULL) 
    { return; }

  for (p = h->first; p != NULL; p = p->next) 
    {
    len = p->len; 
    l = 1; 
    n = 2; 

    while (n <= p->len) 
      {
      while (p->wordgen[l] == -p->wordgen[n]) 	/* adjacent gen/inv pair */
        { 
        len -= 2; 
        n++; 
        l--; 
        if (n > p->len) 
          { break; }
        }

      if (n > p->len) 
        { break; }

      p->wordgen[++l] = p->wordgen[n++];	/* MAY be redundant ! */
      }

    p->len = len;
    }
  }

        /******************************************************************
	static void cyc_reduce(Wordlhdr h)

	Cyclic reduction of all words in a word list.  Can reduce words to	
	zero length.
        ******************************************************************/

static void cyc_reduce(Wordlhdr h)
  {
  Wordlptr p;
  int f, b; 				/* front & back indices */

  if (h == NULL) 
    { return; }

  for (p = h->first; p != NULL; p = p->next) 
    {
    for (f = 1, b = p->len; 
         f < b && p->wordgen[f] == -p->wordgen[b]; f++, b--)
      { ; }

    if (f != 1)
      {
      for (b = 1, p->len -= 2*(f-1); b <= p->len; b++)
        { p->wordgen[b] = p->wordgen[b + f-1]; }
      }
    } 
  }

        /******************************************************************
	static void sort_wordl(Wordlhdr h)

	Sort word list into nondecreasing length, using a stable selection 
	sort, moving words from 'orig' to 'sorted'.  More brain damage!
        ******************************************************************/

static void sort_wordl(Wordlhdr h)
  {
  Wordlel *sorted, *orig, *lastlong, *prior, *st;

  if (h == NULL) 
    { return; }

  for (orig = h->first, sorted = NULL; orig != NULL; ) 
    {
    for (st = prior = lastlong = orig; st->next != NULL; st = st->next)
      {
      if (lastlong->len <= st->next->len) 
        { 
        prior = st; 
        lastlong = st->next; 
        }
      }

    if (prior == lastlong) 
      { orig = lastlong->next; }
    else 
      { prior->next = lastlong->next; }

    lastlong->next = sorted; 	/* prefix (latest) longest to list */
    if (sorted == NULL) 
      { h->last = lastlong; }
    sorted = lastlong;
    }

  h->first = sorted;
  }

        /******************************************************************
	static void check_involutory(void)

	Find all involutory generators i.  Record involutions by setting 
	geninv[i] = -1, otherwise 0.  Should change to Logic type, but
	note the use of -ve in tc_assemble_gens()!
        ******************************************************************/

static void check_involutory(void)
  { 
  Wordlel *p;
  int k;

  for (k = 1; k <= ndgen; k++) 
    { geninv[k] = 0; }

  if (relhdr == NULL) 
    { return; }

  for (p = relhdr->first; p != NULL; p = p->next)
    {
    if (p->len == 2 && p->wordgen[1] == p->wordgen[2]) 
      { geninv[abs(p->wordgen[1])] = -1; }
    }
  }

        /******************************************************************
	static void base_exp(Wordlel *p)

	Compute exponent of word *p.  Btry is current attempt at base
	length.  This counts up, so get exp correct (i.e., as large as
	possible).  Originally used internally to save storage space (but
	not time), now only used for print-out?
        ******************************************************************/

static void base_exp(Wordlel *p)
  {
  int i, j, btry;

  for (btry = 1; btry <= p->len/2; btry++) 
    {
    if (p->len % btry == 0) 
      { 				/* possible base length */
      p->exp = p->len / btry;
      for (i = 1; i <= btry; i++) 
        { 				/* for each gen in possible base */
	for (j = i + btry; j <= p->len; j += btry) 
          { 				/* for each poss copy */
	  if (p->wordgen[i] != p->wordgen[j]		/* mismatch */
              && (geninv[abs(p->wordgen[i])] == 0	/* noninvolution */
                  || p->wordgen[i] != -p->wordgen[j]))	/* inv mismatch */
            { goto eLoop; } 		/* this p->exp failed */
          }
        }
      return; 				/* this p->exp is the exponent */
      }

    eLoop:
      { ; }				/* try next potential exponent */
    }

  p->exp = 1; 				/* nontrivial exponent not found */
  }

        /******************************************************************
	static void get_exp(void)

	Compute exponents of all words in both lists.  Note: it might make
	sense to combine this with get_length?
        ******************************************************************/

static void get_exp(void)
  {
  Wordlel *ptr;

  if (relhdr != NULL)
    {
    for (ptr = relhdr->first; ptr != NULL; ptr = ptr->next)
      { base_exp(ptr); }
    }

  if (sghdr != NULL)
    {
    for (ptr = sghdr->first; ptr != NULL; ptr = ptr->next)
      { base_exp(ptr); }
    }
  }

        /******************************************************************
	static void get_length(void)

	Compute the total length of relators and subgrp generators and put 
	them into trellen and tsgenlen respectively.
        ******************************************************************/

static void get_length(void)
  {
  Wordlel *ptr;

  trellen = 0;
  if (relhdr != NULL)
    {
    for (ptr = relhdr->first; ptr != NULL; ptr = ptr->next)
      { trellen += ptr->len; }
    }

  tsgenlen = 0;
  if (sghdr != NULL)
    {
    for (ptr = sghdr->first; ptr != NULL; ptr = ptr->next)
      { tsgenlen += ptr->len; }
    }
  }

        /******************************************************************
	static void tc_assemble_edp(void)

	Build the edp data structure by scanning through the relators[]
	array.
        ******************************************************************/

static void tc_assemble_edp(void)
  {
  int i, k, l, m, ii, start, wm, rel, gen;

  start = 0;
  for (gen = 1; gen <= ndgen; gen++) 
    {
    i = gencol[gen]; 
    ii = gencol[-gen];
    edpbeg[i] = start; 			/* edps for gen start here */

    for (rel = 1; rel <= ndrel; rel++)
      {
      /* k points to the beginning and l to the end of the base of (the 
      first copy of) relator rel.  Find all occurrences of generator gen 
      or its inverse and note their positions in edp.  Relexp[rel] < 0 
      means this relator specifies an involution so is ignored. */

      if (relexp[rel] > 0) 
        {
	k = relind[rel];
	l = k - 1 + rellen[rel]/relexp[rel];

	for (m = k; m <= l; m++) 
          {
	  wm = relators[m];
	  if (wm == i || wm == ii) 
	    { 
            edp[start++] = m; 
            edp[start++] = rellen[rel];
            }
          }
        }
      }

    edpend[i] = start - 2;		/* last edp for gen */
    }
  }

        /******************************************************************
	static void tc_assemble_gens(void)

	Reord which group generators are involutory, and allocate 
	generators to columns.  Note the temporary setting of gencol[g]=0 
	if non-involutory, and = -1 if involutory to track involutions.
        ******************************************************************/

static void tc_assemble_gens(void)
  {
  int i, j, gens;

  for (i = 1; i <= ndgen; i++) 
    { gencol[i] = geninv[i]; }

  /* Allocate columns to the generators and inverses and note ncol. 
  Also set up the tables colgen and invcol. */

  ncol = 0;

  if (ndgen == 1)		/* single generator, no choice */
    {
    ncol = 2;
    invcol[1] = 2; 
    invcol[2] = 1; 
    colgen[1] = 1;
    colgen[2] = -1; 
    gencol[1] = 1;
    gencol[-1] = 2;
    } 
  else 
    {
    for (i = 1; i <= ndgen; i++) 
      {
      if ((gens = gencol[i]) > 0) 
        { continue; }		/* already allocated due to col swap */
      else if (gens == 0) 
        { 
        /* noninvolutory generator, allocate 2 columns in coset table */

	ncol++; 
        invcol[ncol] = ncol + 1;
	colgen[ncol] = i; 
        gencol[i] = ncol;
	ncol++; 
        invcol[ncol] = ncol - 1;
	colgen[ncol] = -i; 
        gencol[-i] = ncol;
        }
      else if (i != 1) 
        {
	/* Involutory generator & not column 1.  Allocate only 1 column in
	the coset table. (Prefer not to have an involution in col 1.) */

	ncol++;
	invcol[ncol] = ncol; 
        colgen[ncol] = i;
	gencol[i] = ncol; 
        gencol[-i] = ncol;
        }
      else 
        {
        /* Involutory generator & column 1.  Try to find a noninvolutory 
	generator.  For coincidence processing, first 2 columns of the 
	coset table need to assigned to noninvolutory generator and its 
	inverse or to 2 involutory generators. */

	for (j = 1; j <= ndgen; j++)
          {
	  if (gencol[j] == 0) 
            { 
            /* A noninvolutory generator.  Assign noninvolutory generator 
            j to columns 1/2 & involutory generator 1 to column 3. */

	    ncol = 3;

	    invcol[1] = 2; 
            colgen[1] = j;
	    gencol[j] = 1; 
            invcol[2] = 1;
	    colgen[2] = -j; 
            gencol[-j] = 2;

	    invcol[3] = 3; 
            colgen[3] = 1;
	    gencol[1] = 3; 
            gencol[-1] = 3;

	    break;
	    }
          }

	if (j > ndgen) 
          {
          /* all generators are involutary, so allocate 1 column each */

	  ncol++;
	  invcol[1] = 1; 
          colgen[1] = 1;
	  gencol[1] = 1; 
          gencol[-1] = 1;
          }
        }
      }
    }
  }

        /******************************************************************
	static void tc_newrel_newsg(void)

	Apply current cosets to new relators and subgroup gens.  Note that
	asis = 1 here, so that any newbies are tagged on at the end.
        ******************************************************************/

static void tc_newrel_newsg(void)
  {
  int i, j, subg, cyclic_perms, cyclic_count;
  int *beg, *end;

  overfl = indexc = index1 = FALSE; 
  disded = 0; 
  topded = maxrow + 1;

  if (newsg != 0) 
    {
    SET2(status, 'N', 'S');
    for (subg = nsgpg - newsg + 1; subg <= nsgpg; subg++) 
      {
      beg = &subggen[subgindex[subg]];
      end = beg - 1 + subglength[subg];

      tc_apply(1, beg, end, TRUE);

      if (overfl) 
        {
        tc_text(13); 
        return; 
        }
      if (index1) 
        { 
        tc_text(12); 
        indexc = TRUE; 
        return; 
        }
      }
    }

  if (newrel != 0) 
    {
    SET2(status, 'N', 'R');
    for (i = 1; i < nextdf; i++)	/* for all existing cosets */
      {
      if (CT(i, 1) < 0) 
        { continue; }

      for (j = ndrel - newrel + 1; j <= ndrel; j++) 
        {
        cyclic_count = mendel ? rellen[j]/relexp[j] : 1;
        for (cyclic_perms = 0; cyclic_perms < cyclic_count; cyclic_perms++)
          {
          beg = &relators[relind[j] + cyclic_perms];
          end = beg + rellen[j] - 1;

          tc_apply(i, beg, end, FALSE);

          if (index1) 
            { 
            tc_text(12); 
            indexc = TRUE; 
            return;
            }

          if (CT(i, 1) < 0)		/* No collapse, but coincidence */
            { break; }
          }

        if (CT(i, 1) < 0) 
          { break; }
        }
      }
    }
  }

        /******************************************************************
	static void tc_set_relators(void)

	Process the group relators and subgroup generators.  Note how we
	double up the relators, so we can do `cyclic' scans efficiently.
	We don't seem to store inverses, these could be helpful if we
	want EDP of both g & G?
        ******************************************************************/

static void tc_set_relators(void)
  {
  int i, j, m, first, second;
  Wordlel *p;

  /* Duplicate each relator for tracing efficiency. */

  if (relhdr != NULL) 
    {
    second = 0; 
    i = 1;
    for (p = relhdr->first; p != NULL; p = p->next) 
      {
      rellen[i] = p->len; 
      relexp[i] = p->exp;
      first = second; 
      second = first + p->len; 
      relind[i] = first;
      for (j = 1; j <= p->len; j++) 
        { relators[first++] = relators[second++] = p->wordgen[j]; }
      i++;
      }
    }

  /* Process the subgroup generators. */

  if (sghdr != NULL) 
    {
    i = 1; 
    m = 0;
    for (p = sghdr->first; p != NULL; p = p->next) 
      {
      subglength[i] = p->len; 
      subgindex[i] = m;
      for (j = 1; j <= p->len; j++) 
        { subggen[m++] = p->wordgen[j]; }
      i++;
      }
    }
  }

        /******************************************************************
	static void tc_translate(void)

	Translate the subgroup generators and group defining relators from 
	arrays in terms of generators and inverses to arrays in terms of 
	their associated column numbers in the coset table.
        ******************************************************************/

static void tc_translate(void)
  { 
  int i, k;
  int *p;

  for (p = subggen; p < subggen + tsgenlen; p++)
    { *p = gencol[*p]; }

  for (i = 1; i <= ndrel; i++) 
    {
    k = relators[relind[i]];
    if (rellen[i] == 2 && relexp[i] == 2 
        && gencol[k] == gencol[-k]) 
      { relexp[i] = -2; }
    }

  for (p = relators; p < relators + 2*trellen; p++)
    { *p = gencol[*p]; }
  }

        /******************************************************************
	static void tc_step2(void)

	The final wrapper around the enumerator proper!  For (aesthetic)
	efficiency, much of the stuff here could be moved to the outside of
	the loops in tc_start()!
        ******************************************************************/

static void tc_step2(void)
  {
  int i, nmax;

  mstflg = FALSE; 
  ctflg = TRUE;
  newrel = newrelpar; 
  newrelpar = 0; 
  newsg = newsgpar; 
  newsgpar = 0;

  totaltime = 0.0;			/* `proper' place for these ? */
  begintime = utl_clock();

  /* Setup the required workspace, apart from the coset table.  This long
  chain of memory allocations is _horrible_!  Some time real soon we'll
  replace this with a single allocation of all the required space, and
  just set the pointers as required. */

  if (edpbeg != NULL)
    { free(edpbeg); }
  edpbeg = (int *)malloc((2*ndgen + 1)*sizeof(int));
  if (edpbeg == NULL)
    {
    fprintf(fop, "** Out of memory: %s[%d]\n", "edpbeg", 
            (2*ndgen + 1)*sizeof(int));
    return; 
    }

  if (edpend != NULL)
    { free(edpend); }
  edpend = (int *)malloc((2*ndgen + 2)*sizeof(int));
  if (edpend == NULL)
    {
    fprintf(fop, "** Out of memory: %s[%d]\n", "edpend", 
            (2*ndgen + 2)*sizeof(int));
    return; 
    }

  if (edp != NULL)
    { free(edp); }
  edp = (int *)malloc((2*trellen + 2)*sizeof(int));
  if (edp == NULL)
    {
    fprintf(fop, "** Out of memory: %s[%d]\n", "edp", 
            (2*trellen + 2)*sizeof(int));
    return; 
    }

  if (relators != NULL)
    { free(relators); }
  relators = (int *)malloc((2*trellen + 1)*sizeof(int));
  if (relators == NULL)
    {
    fprintf(fop, "** Out of memory: %s[%d]\n", "relators", 
            (2*trellen + 1)*sizeof(int));
    return; 
    }

  if (relind != NULL)
    { free(relind); }
  relind = (int *)malloc((ndrel + 1)*sizeof(int));
  if (relind == NULL)
    {
    fprintf(fop, "** Out of memory: %s[%d]\n", "relind", 
            (ndrel + 1)*sizeof(int));
    return; 
    }

  if (rellen != NULL)
    { free(rellen); }
  rellen = (int *)malloc((ndrel + 1)*sizeof(int));
  if (rellen == NULL)
    {
    fprintf(fop, "** Out of memory: %s[%d]\n", "rellen", 
            (ndrel + 1)*sizeof(int));
    return; 
    }

  if (relexp != NULL)
    { free(relexp); }
  relexp = (int *)malloc((ndrel + 1)*sizeof(int));
  if (relexp == NULL)
    {
    fprintf(fop, "** Out of memory: %s[%d]\n", "relexp", 
            (ndrel + 1)*sizeof(int));
    return; 
    }

  if (subggen != NULL)
    { free(subggen); }
  subggen = (int *)malloc((tsgenlen + 1)*sizeof(int));
  if (subggen == NULL)
    {
    fprintf(fop, "** Out of memory: %s[%d]\n", "subggen", 
            (tsgenlen + 1)*sizeof(int));
    return; 
    }

  if (subgindex != NULL)
    { free(subgindex); }
  subgindex = (int *)malloc((nsgpg + 1)*sizeof(int));
  if (subgindex == NULL)
    {
    fprintf(fop, "** Out of memory: %s[%d]\n", "subgindex", 
            (nsgpg + 1)*sizeof(int));
    return; 
    }

  if (subglength != NULL)
    { free(subglength); }
  subglength = (int *)malloc((nsgpg + 3)*sizeof(int));
  if (subglength == NULL)
    {
    fprintf(fop, "** Out of memory: %s[%d]\n", "subglength", 
            (nsgpg + 3)*sizeof(int));
    return; 
    }

  /* Should really use ncol, not ndgen.  Perhaps later, but it's a bit of
  a chicken and egg situation! */

  if (colgen != NULL)
    { free(colgen); }
  colgen = (int *)malloc((2*ndgen + 1)*sizeof(int));
  if (colgen == NULL)
    {
    fprintf(fop, "** Out of memory: %s[%d]\n", "colgen", 
            (2*ndgen + 1)*sizeof(int));
    return; 
    }

  if (invcol != NULL)
    { free(invcol); }
  invcol = (int *)malloc((2*ndgen + 1)*sizeof(int));
  if (invcol == NULL)
    {
    fprintf(fop, "** Out of memory: %s[%d]\n", "invcol", 
            (2*ndgen + 1)*sizeof(int));
    return; 
    }

  if (gencol != NULL)
    { free(gencolp); }
  gencolp = (int *)malloc((2*ndgen+1)*sizeof(int));
  if (gencolp == NULL)
    {
    fprintf(fop, "** Out of memory: %s[%d]\n", "gencol(p)", 
            (2*ndgen+1)*sizeof(int));
    return; 
    }
  gencol = &gencolp[ndgen];	/* Adjust for +/- indices */

  tc_set_relators(); 		/* put relators and subgroup gens into y */
  tc_assemble_gens(); 		/* set gen translation tables */

  /* Convert relators and subgroup gens represented by group gens into an 
  internal format in which group gens are represented by associated colunm 
  numbers in the coset table. */

  tc_translate();

  tc_assemble_edp();		/* build essentially different positions */

  if (fillfactor == 0)			/* Use default value */
    { fillfactor = (5*(ncol + 2))/4; }

  /* Build the empty coset table.  Although coset numbers are limited to
  2G, the workspace can exceed the 32-bit limit; hence the messing
  around with floating-point to find the max number of cosets given the
  number of columns.  Note the extra rounding down by 1 (for safety), and
  that coset #0 dne.  This code is interim, and will be done properly
  real soon now. */

  /*nmax = workspace/ncol - 1;*/
  nmax = (int)(((double)workspace*(double)workmult)/(double)ncol) -1 -1;

  if (cosetlim > 0) 
    { maxrow = MIN0(nmax, cosetlim); } 
  else 
    { maxrow = nmax; }

  if (colptr != NULL)
    { free(colptr); }
  colptr = (int **)malloc((ncol + 1)*sizeof(int **));
  if (colptr == NULL)
    {
    fprintf(fop, "** Out of memory: %s[%d]\n", "colptr", 
            (ncol + 1)*sizeof(int **));
    return; 
    }

  /* We use maxrow instead of nmax so that the used memory is kept
  contiguous - for cache efficiency?  Note that coset #0 is unused.  Note
  the IP27/R10000 64-bit addressing kludge! */

  colptr[0] = NULL;
  for (i = 1; i <= ncol; i++)
    { colptr[i] = cosettable + (long)(i-1)*(long)(maxrow+1); }

  /* Apply current cosets to new relators and subgroup gens if necessary */

  if (restart && (newrel > 0 || newsg > 0)) 
    {
    tc_newrel_newsg(); 
    newrel = 0; 
    newsg = 0;
    if (indexc) 
      { return; }
    }

  /* Go for it ... */

  tc_enum();
  }

        /******************************************************************
	void tc_start(void)

	Top-level wrapper function.  Preprocesses the relators/generators,
	print some info, and then loop over all the combinations requested.
        ******************************************************************/

void tc_start(void)
  {
  if (!asis) 
    {
    free_reduce(relhdr); 
    free_reduce(sghdr); 
    cyc_reduce(relhdr);
    sort_wordl(relhdr); 
    sort_wordl(sghdr);
    }

  /* Geninv[] is needed by check_involutory */

  if (geninv != NULL)
    { free(geninv); }
  geninv = (int *)malloc((ndgen + 1)*sizeof(int));
  if (geninv == NULL)
    {
    fprintf(fop, "** Out of memory: %s[%d]\n", "geninv", 
            (ndgen + 1)*sizeof(int));
    return; 
    }

  check_involutory();		/* combine all three of these into one? */
  get_exp(); 
  get_length(); 

  printdetails();

  /* Note that, after these loops finish, the values of the loop control
  variables are greater than their final values.  This is only a problem
  for cosetlim, which is why it is `aliased' by maxrow within the 
  enumerator. */

  for (cosetlim = maxpar[0]; cosetlim <= maxpar[1]; cosetlim += maxpar[2])
    {
    for (nrinsgp = rinsgpar[0]; nrinsgp <= rinsgpar[1]; 
         nrinsgp += rinsgpar[2])
      {
      for (fillfactor = fipar[0]; fillfactor <= fipar[1]; 
           fillfactor += fipar[2])
        {
        for (ctfactor = ctpar[0]; ctfactor <= ctpar[1]; 
             ctfactor += ctpar[2])
          {
          for (rtfactor = rtpar[0]; rtfactor <= rtpar[1]; 
               rtfactor += rtpar[2]) 
            { tc_step2(); }
          }
        }
      }
    }
  }

