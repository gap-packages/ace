
/**************************************************************************

        ex0c.c
        Colin Ramsay (cram@csee.uq.edu.au)
        13 Feb 98

        ADAPTIVE COSET ENUMERATOR, Version 3.000

        Copyright 1998 
        Centre for Discrete Mathematics and Computing,
        Department of Mathematics and 
        Department of Computer Science & Electrical Engineering,
        The University of Queensland, QLD 4072.
	(http://www.csee.uq.edu.au/~havas/cdmc.html)

This example of how to drive ACE Level 0, is intended as a template for
testing C-style (rfactor=0, cfactor>0) in its various modes (start,
continue & redo).  The presentation we play with is F(2,7), or some variant
thereof.  This is a good example to test the _code_ for the various pdefn
values, although it is _not_ a good example of the general effect of the
various strategies!

**************************************************************************/

#include "al0.h"

	/******************************************************************
	Routine to dump the first & last x rows of the table.
	******************************************************************/

void dmp_tab(int cnt)
  {
  int i,j;

  printf("      |    a    A    b    B    c    C    d    D"
                "    e    E    f    F    g    G\n");
  printf(" -----+----------------------------------------"
                "------------------------------\n");

  if (2*cnt >= nextdf)
    {
    for (i = 1; i <= nextdf; i++)
      {
      printf(" %4d |", i);
      for (j = 1; j <= ncol; j++)
        { printf("%5d", CT(i,j)); }
      if (i == knr)
        { printf("  knr"); }
      if (i == knh)
        { printf("  knh"); }
      if (i == nextdf)
        { printf("  nextdf"); }
      printf("\n");
      }
    }
  else
    {
    for (i = 1; i <= cnt; i++)
      {
      printf(" %4d |", i);
      for (j = 1; j <= ncol; j++)
        { printf("%5d", CT(i,j)); }
      if (i == knr)
        { printf("  knr"); }
      if (i == knh)
        { printf("  knh"); }
      if (i == nextdf)
        { printf("  nextdf"); }
      printf("\n");
      }
    printf("   -- |\n");
    for (i = nextdf-cnt+1; i <= nextdf; i++)
      {
      printf(" %4d |", i);
      for (j = 1; j <= ncol; j++)
        { printf("%5d", CT(i,j)); }
      if (i == knr)
        { printf("  knr"); }
      if (i == knh)
        { printf("  knh"); }
      if (i == nextdf)
        { printf("  nextdf"); }
      printf("\n");
      }
    }
  }

	/******************************************************************
	Group name: F(2,7) ~ C_29 ;
	Group generators: a,b,c,d,e,f,g ;
	Group relators: ab=c, bc=d, cd=e, de=f, ef=g, fg=a, ga=b ;
	Subgroup name: Id ;
	Subgroup generators:  ;
	******************************************************************/

void bld_pres(void)
  {
  int *p, trellen;

  pdqcol = (int *)malloc(pdsiz*sizeof(int));
  pdqrow = (int *)malloc(pdsiz*sizeof(int));

  dedrow = (int *)malloc(dedsiz*sizeof(int));
  dedcol = (int *)malloc(dedsiz*sizeof(int));

  ncol = 14;

  colptr = (int **)malloc((ncol + 1)*sizeof(int **));
  p = (int *)malloc(ncol*(maxrow+1)*sizeof(int));
  colptr[0]  = NULL;
  colptr[1]  = p;                  	/* a */
  colptr[2]  = p + (maxrow+1);   	/* A */
  colptr[3]  = p + 2*(maxrow+1);   	/* b */
  colptr[4]  = p + 3*(maxrow+1);  	/* B */
  colptr[5]  = p + 4*(maxrow+1);   	/* c */
  colptr[6]  = p + 5*(maxrow+1);  	/* C */
  colptr[7]  = p + 6*(maxrow+1);  	/* d */
  colptr[8]  = p + 7*(maxrow+1);   	/* D */
  colptr[9]  = p + 8*(maxrow+1);  	/* e */
  colptr[10] = p + 9*(maxrow+1); 	/* E */
  colptr[11] = p + 10*(maxrow+1);  	/* f */
  colptr[12] = p + 11*(maxrow+1);  	/* F */
  colptr[13] = p + 12*(maxrow+1); 	/* g */
  colptr[14] = p + 13*(maxrow+1);	/* G */
  col1ptr = colptr[1];
  col2ptr = colptr[2];

  invcol = (int *)malloc((ncol+1)*sizeof(int));
  invcol[0]  = 0;
  invcol[1]  = 2;   invcol[2]  = 1;
  invcol[3]  = 4;   invcol[4]  = 3;
  invcol[5]  = 6;   invcol[6]  = 5;
  invcol[7]  = 8;   invcol[8]  = 7;
  invcol[9]  = 10;  invcol[10] = 9;
  invcol[11] = 12;  invcol[12] = 11;
  invcol[13] = 14;  invcol[14] = 13;

  ndrel = 7;
  trellen = 7*(3*1);

  relexp = (int *)malloc((ndrel+1)*sizeof(int));
  relexp[0] = 0;
  relexp[1] = relexp[2] = relexp[3] = relexp[4] = 1;
  relexp[5] = relexp[6] = relexp[7] = 1;

  rellen = (int *)malloc((ndrel+1)*sizeof(int));
  rellen[0] = 0;
  rellen[1] = rellen[2] = rellen[3] = rellen[4] = 3;
  rellen[5] = rellen[6] = rellen[7] = 3;

  /* The string stored in relators[] is:
	0  &  3		abC abC
	6  &  9		bcD bcD
	12 & 15		cdE cdE
	18 & 21		deF deF
	24 & 27		efG efG
	30 & 33		fgA fgA
	36 & 39		gaB gaB
  */

  relators = (int *)malloc(2*trellen*sizeof(int));
  relators[0]  = 1;   relators[1]  = 3;   relators[2]  = 6;   
  relators[3]  = 1;   relators[4]  = 3;   relators[5]  = 6;
  relators[6]  = 3;   relators[7]  = 5;   relators[8]  = 8;   
  relators[9]  = 3;   relators[10] = 5;   relators[11] = 8;
  relators[12] = 5;   relators[13] = 7;   relators[14] = 10;
  relators[15] = 5;   relators[16] = 7;   relators[17] = 10;
  relators[18] = 7;   relators[19] = 9;   relators[20] = 12;
  relators[21] = 7;   relators[22] = 9;   relators[23] = 12;
  relators[24] = 9;   relators[25] = 11;  relators[26] = 14;
  relators[27] = 9;   relators[28] = 11;  relators[29] = 14;
  relators[30] = 11;  relators[31] = 13;  relators[32] = 2;
  relators[33] = 11;  relators[34] = 13;  relators[35] = 2;
  relators[36] = 13;  relators[37] = 1;   relators[38] = 4;
  relators[39] = 13;  relators[40] = 1;   relators[41] = 4;

  relind = (int *)malloc((ndrel+1)*sizeof(int));
  relind[0] = -1;
  relind[1] = 0;
  relind[2] = 6;
  relind[3] = 12;
  relind[4] = 18;
  relind[5] = 24;
  relind[6] = 30;
  relind[7] = 36;

  edp = (int *)malloc(2*trellen*sizeof(int));
  edp[0]  = 0;   edp[1]  = 3;		/* 1st a */
  edp[2]  = 37;  edp[3]  = 3;		/* 2nd a */
  edp[4]  = 32;  edp[5]  = 3;		/* 1st A */
  edp[6]  = 1;   edp[7]  = 3;		/* 1st b */
  edp[8]  = 6;   edp[9]  = 3;		/* 2nd b */
  edp[10] = 38;  edp[11] = 3;		/* 1st B */
  edp[12] = 7;   edp[13] = 3;		/* 1st c */
  edp[14] = 12;  edp[15] = 3;		/* 2nd c */
  edp[16] = 2;   edp[17] = 3;		/* 1st C */
  edp[18] = 13;  edp[19] = 3;		/* 1st d */
  edp[20] = 18;  edp[21] = 3;		/* 2nd d */
  edp[22] = 8;   edp[23] = 3;		/* 1st D */
  edp[24] = 19;  edp[25] = 3;		/* 1st e */
  edp[26] = 24;  edp[27] = 3;		/* 2nd e */
  edp[28] = 14;  edp[29] = 3;		/* 1st E */
  edp[30] = 25;  edp[31] = 3;		/* 1st f */
  edp[32] = 30;  edp[33] = 3;		/* 2nd f */
  edp[34] = 20;  edp[35] = 3;		/* 1st F */
  edp[36] = 31;  edp[37] = 3;		/* 1st g */
  edp[38] = 36;  edp[39] = 3;		/* 2nd g */
  edp[40] = 26;  edp[41] = 3;		/* 1st G */

  edpbeg = (int *)malloc((ncol + 1)*sizeof(int));
  edpend = (int *)malloc((ncol + 1)*sizeof(int));
  edpbeg[0] = -1;   edpend[0]  = -1;
  edpbeg[1]  = 0;   edpend[1]  = 2;	/* Posns for a */
  edpbeg[2]  = 4;   edpend[2]  = 4;	/* A */
  edpbeg[3]  = 6;   edpend[3]  = 8;	/* b */
  edpbeg[4]  = 10;  edpend[4]  = 10;	/* B */
  edpbeg[5]  = 12;  edpend[5]  = 14;	/* c */
  edpbeg[6]  = 16;  edpend[6]  = 16;	/* C */
  edpbeg[7]  = 18;  edpend[7]  = 20;	/* d */
  edpbeg[8]  = 22;  edpend[8]  = 22;	/* D */
  edpbeg[9]  = 24;  edpend[9]  = 26;	/* e */
  edpbeg[10] = 28;  edpend[10] = 28;	/* E */
  edpbeg[11] = 30;  edpend[11] = 32;	/* f */
  edpbeg[12] = 34;  edpend[12] = 34;	/* F */
  edpbeg[13] = 36;  edpend[13] = 38;	/* g */
  edpbeg[14] = 40;  edpend[14] = 40;	/* G */

  nsgpg = 0;
  }

	/******************************************************************
	Subgroup name: Whole group ;
	Subgroup generators: g ;
	******************************************************************/

void nuke0(void)
  {
  int tsgenlen;

  nsgpg = 1;
  tsgenlen = 1;

  subggen = (int *)malloc(tsgenlen*sizeof(int));
  subggen[0] = 13;

  subgindex = (int *)malloc((nsgpg + 1)*sizeof(int));
  subgindex[0] = -1;                    /* Not used */
  subgindex[1] = 0;

  subglength = (int *)malloc((nsgpg + 1)*sizeof(int));
  subglength[0] = 0;                    /* Not used */
  subglength[1] = 1;
  }

int main(void)
  {
  printf("\n%s (Level 0): example programme \"c\"\n", ACE_VER);
  printf("Start time: %s", al0_date());

  al0_init();
  maxrow = 142855;			/* ~ wo:2M; */

  dedsiz = 10000;			/* 10000 physical/allowed dedns */
  pdsiz  = 256;				/* What effect does this have ? */

  bld_pres();

  msgctrl = TRUE;
  /*msghol  = TRUE;*/
  msgincr = 10000;

  /* pcomp  = TRUE; */

  /* Pure C-style & start mode (machine #6): the full group.  Note that the
  first run is slower, since the memory for the table has not been paged in
  yet.  Note the test of relators in subgroup. */

  printf("\n======== #1 ========\n");

  rfactor = 0;
  cfactor = 5000;

  al0_rslt(al0_enum(0,5));
  dmp_tab(8);
  al0_dump(TRUE);
  STATDUMP;

  printf("\n======== #2 ========\n");

  pdefn = 0;
  printf("** pdefn = %d\n", pdefn);

  for (nrinsgp = 0; nrinsgp <= ndrel; nrinsgp++)
    {
    printf("** nrinsgp = %d\n", nrinsgp);
    al0_rslt(al0_enum(0,5));
    STATDUMP;
    }

  nrinsgp = 0;			/* Pure C-style for remainder of run */

  /* Do the pdefn=1 mode. */

  printf("\n======== #3 ========\n");

  pdefn = 1;
  printf("** pdefn = %d\n", pdefn);

  for (ffactor = 4.0; ffactor <= 64.0; ffactor += 0.25)
    {
    printf("** ffactor = %3.1f\n", ffactor);
    al0_rslt(al0_enum(0,5));
    STATDUMP;
    }

  /* Do the pdefn=2 mode. */

  printf("\n======== #4 ========\n");

  pdefn = 2;
  printf("** pdefn = %d\n", pdefn);

  for (ffactor = 4.0; ffactor <= 64.0; ffactor += 0.25)
    {
    printf("** ffactor = %3.1f\n", ffactor);
    al0_rslt(al0_enum(0,5));
    STATDUMP;
    }

  /* Do the pdefn=3 mode. */

  printf("\n======== #5 ========\n");

  pdefn = 3;
  printf("** pdefn = %d\n", pdefn);

  for (ffactor = 4.0; ffactor <= 64.0; ffactor += 0.25)
    {
    printf("** ffactor = %3.1f\n", ffactor);
    al0_rslt(al0_enum(0,5));
    STATDUMP;
    }

  printf("\n====================\n");

  printf("\nStop time: %s\n", al0_date());

  return(0);
  }

