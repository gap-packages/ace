
/**************************************************************************

        ex0b.c
        Colin Ramsay (cram@csee.uq.edu.au)
        18 Jan 99

        ADAPTIVE COSET ENUMERATOR, Version 3.000

        Copyright 1999
        Centre for Discrete Mathematics and Computing,
        Department of Mathematics and 
        Department of Computer Science & Electrical Engineering,
        The University of Queensland, QLD 4072.
	(http://www.csee.uq.edu.au/~havas/cdmc.html)

This example of how to drive ACE Level 0, is intended as a template for
testing R-style (rfactor>0, cfactor=0) in its various modes (start,
continue & redo).  The presentation we play with is C_4 x C_9 x C_25 (of
order 900), or some variant thereof.  Note that the pdl & dedn stack are
not required here.  We also invoke some of the Level 0 routines directly,
and we do our I/O through ACE's fop/fip streams.

**************************************************************************/

#include "al0.h"

	/******************************************************************
	Routine to dump the first & last x rows of the table.
	******************************************************************/

void dmp_tab(int cnt)
  {
  int i,j;

  fprintf(fop, "      |    a    A    b    B    c    C\n");
  fprintf(fop, " -----+------------------------------\n");

  if (2*cnt >= nextdf)
    {
    for (i = 1; i <= nextdf; i++)
      {
      fprintf(fop, " %4d |", i);
      for (j = 1; j <= ncol; j++)
        { fprintf(fop, "%5d", CT(i,j)); }
      if (i == knr)
        { fprintf(fop, "  knr"); }
      if (i == knh)
        { fprintf(fop, "  knh"); }
      if (i == nextdf)
        { fprintf(fop, "  nextdf"); }
      fprintf(fop, "\n");
      }
    }
  else
    {
    for (i = 1; i <= cnt; i++)
      {
      fprintf(fop, " %4d |", i);
      for (j = 1; j <= ncol; j++)
        { fprintf(fop, "%5d", CT(i,j)); }
      if (i == knr)
        { fprintf(fop, "  knr"); }
      if (i == knh)
        { fprintf(fop, "  knh"); }
      if (i == nextdf)
        { fprintf(fop, "  nextdf"); }
      fprintf(fop, "\n");
      }
    fprintf(fop, "   -- |\n");
    for (i = nextdf-cnt+1; i <= nextdf; i++)
      {
      fprintf(fop, " %4d |", i);
      for (j = 1; j <= ncol; j++)
        { fprintf(fop, "%5d", CT(i,j)); }
      if (i == knr)
        { fprintf(fop, "  knr"); }
      if (i == knh)
        { fprintf(fop, "  knh"); }
      if (i == nextdf)
        { fprintf(fop, "  nextdf"); }
      fprintf(fop, "\n");
      }
    }
  }

	/******************************************************************
	Group name: C_4 x C_9 x C_25 ;
	Group generators: a, b, c ;
	Group relators: a^4, b^9, c^25, [a,b], [a,c], [b,c] ;
	Subgroup name: Id ;
	Subgroup generators:  ;
	******************************************************************/

void bld_pres(void)
  {
  int *p, i, trellen;

  ncol = 6;

  colptr = (int **)malloc((ncol + 1)*sizeof(int **));
  p = (int *)malloc(ncol*(maxrow+1)*sizeof(int));
  colptr[0] = NULL;                     /* Not used */
  colptr[1] = p;                        /* a */
  colptr[2] = p + (maxrow+1);           /* A */
  colptr[3] = p + 2*(maxrow+1);         /* b */
  colptr[4] = p + 3*(maxrow+1);         /* B */
  colptr[5] = p + 4*(maxrow+1);         /* c */
  colptr[6] = p + 5*(maxrow+1);         /* C */
  col1ptr = colptr[1];
  col2ptr = colptr[2];

  invcol = (int *)malloc((ncol+1)*sizeof(int));
  invcol[0] = 0;                        /* Not used */
  invcol[1] = 2;
  invcol[2] = 1;
  invcol[3] = 4;
  invcol[4] = 3;
  invcol[5] = 6;
  invcol[6] = 5;

  ndrel = 6;
  trellen = 1*4 + 1*9 + 1*25 + 4 + 4 + 4;

  relexp = (int *)malloc((ndrel+1)*sizeof(int));
  relexp[0] = 0;                        /* Not used */
  relexp[1] = 4;  			/* a^4 */
  relexp[2] = 9;                        /* b^9 */
  relexp[3] = 25;           		/* c^25 */
  relexp[4] = 1;                        /* ABab */
  relexp[5] = 1;                        /* ACac */
  relexp[6] = 1;                        /* BCbc */

  rellen = (int *)malloc((ndrel+1)*sizeof(int));
  rellen[0] = 0;
  rellen[1] = 4;
  rellen[2] = 9;
  rellen[3] = 25;
  rellen[4] = 4;
  rellen[5] = 4;
  rellen[6] = 4;

  /* The string stored in relators[] is:
       0-3, 4-7		aaaa  aaaa  
       8-16, 17-25	bbbbbbbbb  bbbbbbbbb
       26-50, 51-75	ccccccccccccccccccccccccc  ccc...ccc
       76-79, 80-83	ABab  ABab
       84-87, 88-91	ACac  ACac
       92-95, 96-99	BCbc  BCbc 
  */

  relators = (int *)malloc(2*trellen*sizeof(int));

  for (i = 0; i <= 7; i++)
    { relators[i] = 1; }
  for (i = 8; i <= 25; i++)
    { relators[i] = 3; }
  for (i = 26; i <= 75; i++)
    { relators[i] = 5; }

  relators[76] = relators[80] = 2;
  relators[77] = relators[81] = 4;
  relators[78] = relators[82] = 1;
  relators[79] = relators[83] = 3;

  relators[84] = relators[88] = 2;
  relators[85] = relators[89] = 6;
  relators[86] = relators[90] = 1;
  relators[87] = relators[91] = 5;

  relators[92] = relators[96] = 4;
  relators[93] = relators[97] = 6;
  relators[94] = relators[98] = 3;
  relators[95] = relators[99] = 5;
 
  relind = (int *)malloc((ndrel+1)*sizeof(int));
  relind[0] = -1;                       /* Not used */
  relind[1] = 0;
  relind[2] = 8;
  relind[3] = 26;
  relind[4] = 76;
  relind[5] = 84;
  relind[6] = 92;

  edp = (int *)malloc(2*trellen*sizeof(int));
  edp[0]  = 0;   edp[1]  = 4;		/* 1st a */
  edp[2]  = 78;  edp[3]  = 4;		/* 2nd a */
  edp[4]  = 86;  edp[5]  = 4;		/* 3rd a */
  edp[6]  = 76;  edp[7]  = 4;		/* 1st A */
  edp[8]  = 84;  edp[9]  = 4;		/* 2nd A */
  edp[10] = 8;   edp[11] = 9;		/* 1st b */
  edp[12] = 79;  edp[13] = 4;		/* 2nd b */
  edp[14] = 94;  edp[15] = 4;		/* 3rd b */
  edp[16] = 77;  edp[17] = 4;		/* 1st B */
  edp[18] = 92;  edp[19] = 4;		/* 2nd B */
  edp[20] = 26;  edp[21] = 25;		/* 1st c */
  edp[22] = 87;  edp[23] = 4;		/* 2nd c */
  edp[24] = 95;  edp[25] = 4;		/* 3rd c */
  edp[26] = 85;  edp[27] = 4;		/* 1st C */
  edp[28] = 93;  edp[29] = 4;		/* 2nd C */

  edpbeg = (int *)malloc((ncol + 1)*sizeof(int));
  edpend = (int *)malloc((ncol + 1)*sizeof(int));

  edpbeg[0] = -1;  edpend[0] = -1;	/* Not used */
  edpbeg[1] = 0;   edpend[1] = 4;	/* Posns for a */
  edpbeg[2] = 6;   edpend[2] = 8;	/* A */
  edpbeg[3] = 10;  edpend[3] = 14;	/* b */
  edpbeg[4] = 16;  edpend[4] = 18;	/* B */
  edpbeg[5] = 20;  edpend[5] = 24;	/* c */
  edpbeg[6] = 26;  edpend[6] = 28;	/* C */

  nsgpg = 0;
  }

	/******************************************************************
	Subgroup name: C_2 x C_3 ;
	Subgroup generators: a^2, b^3 ;
	******************************************************************/

void nuke0(void)
  {
  int tsgenlen;

  nsgpg = 2;
  tsgenlen = 1*2 + 1*3;

  subggen = (int *)malloc(tsgenlen*sizeof(int));
  subggen[0] = subggen[1] = 1;
  subggen[2] = subggen[3] = subggen[4] = 3;

  subgindex = (int *)malloc((nsgpg + 1)*sizeof(int));
  subgindex[0] = -1;                    /* Not used */
  subgindex[1] = 0;
  subgindex[2] = 2;

  subglength = (int *)malloc((nsgpg + 1)*sizeof(int));
  subglength[0] = 0;                    /* Not used */
  subglength[1] = 2;
  subglength[2] = 3;
  }

int main(void)
  {
  al0_init();

  fprintf(fop, "\n%s (Level 0): example programme \"b\"\n", ACE_VER);
  fprintf(fop, "Start time: %s", al0_date());

  maxrow = 5000;			/* 5000 physical & allowed rows */
  bld_pres();

  msgctrl = TRUE;
  msghol  = TRUE;			/* incl holes msgs */
  msgincr = 50;

  /* Pure R-style (#7) & start mode (#0): the full group. */

  fprintf(fop, "\n======== #1 ========\n");

  rfill   = FALSE;
  rfactor = 250;
  cfactor = 0;
  comppc  = 100;
  lahead  = 0;

  al0_rslt(al0_enum(0,7));

  al0_dump(TRUE);
  STATDUMP;

  /* Tinker with the parameters & rerun in start mode. */

  fprintf(fop, "======== #2 ========\n");

  /* rfill  = TRUE; */
  /* pcomp  = TRUE; */
  /* mendel = TRUE; */

  maxrow  = 901;
  lahead  = 1;
  comppc  = 0;

  al0_rslt(al0_enum(0,7));
  STATDUMP;

  /* Do a non-trivial subgroup, in a run containing some continue / redo
  phases. */

  nuke0();

  /* Do one pass through the loop */

  fprintf(fop, "======== #3 ========\n");

  maxrow  = 1000;
  rfactor = 25;
  comppc  = 10;
  tlimit  = 0;

  al0_rslt(al0_enum(0,7));

  fprintf(fop, "Percent holes: %4.2f%%\n", al0_nholes());
  fprintf(fop, "*NH: knr=%d knh=%d nalive=%d nextdf=%d (cpu=+%4.2f)\n", 
                     knr,   knh,   nalive,   nextdf,    deltatime);
  dmp_tab(6);
  al0_dump(FALSE);
  STATDUMP;

  /* Do another pass through the loop */

  fprintf(fop, "======== #4 ========\n");

  al0_rslt(al0_enum(1,7));

  fprintf(fop, "Percent holes: %4.2f%%\n", al0_nholes());
  fprintf(fop, "*NH: knr=%d knh=%d nalive=%d nextdf=%d (cpu=+%4.2f)\n", 
                     knr,   knh,   nalive,   nextdf,    deltatime);
  dmp_tab(6);
  al0_dump(FALSE);
  STATDUMP;

  /* Continue till done */

  fprintf(fop, "======== #5 ========\n");

  tlimit = -1;

  al0_rslt(al0_enum(1,7));

  fprintf(fop, "Percent holes: %4.2f%%\n", al0_nholes());
  fprintf(fop, "*NH: knr=%d knh=%d nalive=%d nextdf=%d (cpu=+%4.2f)\n", 
                     knr,   knh,   nalive,   nextdf,    deltatime);
  dmp_tab(6);
  al0_dump(FALSE);
  STATDUMP;

  /* Compact, then continue (returns immediately) */

  fprintf(fop, "======== #6 ========\n");

  al0_compact();
  fprintf(fop, "*CO: knr=%d knh=%d nalive=%d nextdf=%d (cpu=+%4.2f)\n", 
                     knr,   knh,   nalive,   nextdf,    deltatime);

  al0_rslt(al0_enum(1,7));

  dmp_tab(6);
  al0_dump(FALSE);
  STATDUMP;

  /* Redo */

  fprintf(fop, "======== #7 ========\n");

  al0_rslt(al0_enum(2,7));

  dmp_tab(6);
  al0_dump(FALSE);
  STATDUMP;

  fprintf(fop, "====================\n");

  fprintf(fop, "\nStop time: %s\n", al0_date());

  return(0);
  }

