
/**************************************************************************

        ex0a.c
        Colin Ramsay (cram@csee.uq.edu.au)
        22 Dec 98

        ADAPTIVE COSET ENUMERATOR, Version 3.000

        Copyright 1998 
        Centre for Discrete Mathematics and Computing,
        Department of Mathematics and 
        Department of Computer Science & Electrical Engineering,
        The University of Queensland, QLD 4072.
	(http://www.csee.uq.edu.au/~havas/cdmc.html)

This is a simple example of how to drive ACE Level 0 (i.e., the core
enumerator) directly.  No attempt is made to be terribly clever, or general
purpose, or to do much error checking.  Note that we do not need to setup
the deduction stack or the preferred definition list, since these are never
used in R-style.

**************************************************************************/

#include "al0.h"

	/******************************************************************
	Macro to dump the table.
	******************************************************************/

#define DUMP2 \
  printf("     |   b   B a/A\n"); \
  printf(" ----+------------\n"); \
  for (i = 1; i <= nextdf; i++)   \
    {                             \
    printf("%4d |", i);           \
    for (j = 1; j <= ncol; j++)   \
      { printf("%4d", CT(i,j)); } \
    if (i == knr)                 \
      { printf("  knr"); }        \
    if (i == knh)                 \
      { printf("  knh"); }        \
    if (i == nextdf)              \
      { printf("  nextdf"); }     \
    printf("\n");                 \
    }

	/******************************************************************
	Group name: A_5 ;
	Group generators: a, b ;
	Group relators: a^2, b^3, (ab)^5 ;
	Subgroup name: C_3 ;
	Subgroup generators: ab ;

	This gives index (5!/2)/5 = 12.  We _choose_ to treat a as an 
	involution.  The columns of the table (generators) are numbered 
	1..ncol & the rows (cosets) 1..maxrow.  Note the extra space for 
	the unused 0th positions here; in the other arrays we may, or may
	not, use position 0 - take care!
	******************************************************************/

void bld_pres(void)
  {
  int *p;
  int trellen, tsgenlen;

  /*
  pdqcol = (int *)malloc(pdsiz*sizeof(int));
  pdqrow = (int *)malloc(pdsiz*sizeof(int));
  */

  /*
  dedrow = (int *)malloc(dedsiz*sizeof(int));
  dedcol = (int *)malloc(dedsiz*sizeof(int));
  */

  ncol = 3;

  colptr = (int **)malloc((ncol + 1)*sizeof(int **));
  p = (int *)malloc(ncol*(maxrow+1)*sizeof(int));
  colptr[0] = NULL;                     /* Not used */
  colptr[1] = p;                        /* Column for b */
  colptr[2] = p + (maxrow+1);           /* Column for B */
  colptr[3] = p + 2*(maxrow+1);         /* Column for a/A */
  col1ptr = colptr[1];
  col2ptr = colptr[2];

  invcol = (int *)malloc((ncol+1)*sizeof(int));
  invcol[0] = 0;                        /* Not used */
  invcol[1] = 2;
  invcol[2] = 1;
  invcol[3] = 3;			/* Involution */

  ndrel = 3;
  trellen = 1*2 + 1*3 + 2*5;

  relexp = (int *)malloc((ndrel+1)*sizeof(int));
  relexp[0] = 0;                        /* Not used */
  relexp[1] = 2;  			/* a^2 */
  relexp[2] = 3;                        /* b^3 */
  relexp[3] = 5;                        /* (ab)^5 */

  rellen = (int *)malloc((ndrel+1)*sizeof(int));
  rellen[0] = 0;                        /* Not used */
  rellen[1] = 2;
  rellen[2] = 3;
  rellen[3] = 10;

  /* Column 3 is a & column 1 is b */

  relators = (int *)malloc(2*trellen*sizeof(int));
  relators[0] = relators[1] = 
                relators[2] = relators[3] = 3;
  relators[4] = relators[5] = relators[6] = 
                relators[7] = relators[8] = relators[9] = 1;
  relators[10]  = 3;  relators[11]  = 1;
  relators[12]  = 3;  relators[13]  = 1;
  relators[14]  = 3;  relators[15]  = 1;
  relators[16]  = 3;  relators[17]  = 1;
  relators[18]  = 3;  relators[19]  = 1;
  relators[20]  = 3;  relators[21]  = 1;
  relators[22]  = 3;  relators[23]  = 1;
  relators[24]  = 3;  relators[25]  = 1;
  relators[26]  = 3;  relators[27]  = 1;
  relators[28]  = 3;  relators[29]  = 1;

  relind = (int *)malloc((ndrel+1)*sizeof(int));
  relind[0] = -1;                       /* Not used */
  relind[1] = 0;
  relind[2] = 4;
  relind[3] = 10;

  /* Note that edpbeg & edpend step up in 2s!  a is an invol & b is not. */

  edp = (int *)malloc(2*trellen*sizeof(int));
  edp[0] = 4;   edp[1] = 3;		/* 1st sig. b @ pos 4 & of len 3 */
  edp[2] = 11;  edp[3] = 10;		/* 2nd sig. b */
  edp[4] = 10;  edp[5] = 10;		/* Only sig. a (invol. ignored) */

  edpbeg = (int *)malloc((ncol + 1)*sizeof(int));
  edpbeg[0] = -1;                       /* Not used */
  edpbeg[1] = 0;                        /* 1st index for b/B (cols 1/2) */
  edpbeg[2] = -1;                       /* There are no B's */
  edpbeg[3] = 4;                        /* 1st index for a/A (col 3) */

  edpend = (int *)malloc((ncol + 1)*sizeof(int));
  edpend[0] = -1;                       /* Not used */
  edpend[1] = 2;                        /* Last index for b/B (cols 1/2) */
  edpend[2] = -1;                       /* There are no B's */
  edpend[3] = 4;                        /* Last index for a/A (col 3) */

  nsgpg = 1;
  tsgenlen = 2*1;

  subggen = (int *)malloc(tsgenlen*sizeof(int));
  subggen[0] = 3;                       /* Column 3 is a */
  subggen[1] = 1;                       /* Column 1 is b */

  subgindex = (int *)malloc((nsgpg + 1)*sizeof(int));
  subgindex[0] = -1;                    /* Not used */
  subgindex[1] = 0;

  subglength = (int *)malloc((nsgpg + 1)*sizeof(int));
  subglength[0] = 0;                    /* Not used */
  subglength[1] = 2;
  }

	/******************************************************************
	Subgroup name: identity ;
	Subgroup generators: ;

	Blow away the subgroup generator(s); this gives index (5!/2) = 60.
	We leave the data-structure alone, and simply set nsgpg to zero!
	******************************************************************/

void edit_pres(void)
  { nsgpg = 0; }

int main(void)
  {
  int i,j;

  printf("\n%s (Level 0): example programme \"a\"\n", ACE_VER);
  printf("Start time: %s", al0_date());
  printf("\n======== #0 ========\n");

  al0_init();
  al0_dump(TRUE);

  maxrow = 100;				/* 100 physical/allowed cosets */
  /*dedsiz = 25;*/			/* 25 physical/allowed dedns */
  /*pdsiz = 256;*/			/* `standard' size is 2^8 */

  bld_pres();

  msgctrl = TRUE; 			/* `full' messaging ... */
  msghol  = TRUE;
  msgincr = 1;

  /* Set up to do a `pure' R-style enumerations. */

  rfactor = 5000;
  cfactor = 0;
  rfill   = FALSE;
  lahead  = 0;
  comppc  = 100;

  /* We may default these, or set them to non-defaults, or do a `non-pure'
  enumeration.  They have no effect on such a small table. */

  /* pcomp  = TRUE; */
  /* rfill  = TRUE; */
  /* mendel = TRUE; */

  /* Start mode (#0) & R-style (#7): the index 12 subgroup. */

  printf("======== #1 ========\n");

  al0_rslt(al0_enum(0,7));
  STATDUMP;

  DUMP2;
  al0_dump(TRUE);

  /* Now do the full group. */

  printf("======== #2 ========\n");

  edit_pres();

  al0_rslt(al0_enum(0,7));
  STATDUMP;

  printf("====================\n");
  printf("\nStop time: %s\n", al0_date());

  return(0);
  }

