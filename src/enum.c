
/**************************************************************************

	enum.c
	Colin Ramsay
	17 Jun 98

	ADAPTIVE COSET ENUMERATOR, Version 2.000

	Copyright 1998 
	Centre for Discrete Mathematics and Computing,
	Department of Mathematics and 
	Department of Computer Science & Electrical Engineering,
	The University of Queensland, QLD 4072.

This is the main enumeration stuff for the coset enumerator.  Rewritten by 
Colin Ramsay, based on code by George Havas et al.

**************************************************************************/

#include "tc.h"

extern void ditrace(char*, int, int, int, int, int);
extern void tc_coinc(int, int);
extern void tc_compact(void);
extern void tc_text(int);
/*extern float utl_second(void);*/

#define INDEXRET      \
  tc_text(12);        \
  indexc = TRUE;       \
  if (msgctrl != 0)   \
    { printStats(); } \
  return

#define OVERFLRET     \
  tc_text(13);        \
  overfl = TRUE;      \
  if (msgctrl != 0)   \
    { printStats(); } \
  return

#define NEXTC(k)                         \
  k = nextdf;                            \
  for (col = 1; col <= ncol; col++)      \
    { CT(k, col) = 0; }                  \
  nextdf++;                              \
  totcos++;                              \
  nalive++;                              \
  maxcos = MAX0(maxcos, nalive);         \
  if (msgctrl != 0 && nalive == nextout) \
    {                                    \
    nextout += msgincr;                  \
    if (nalive != lastout)               \
      {                                  \
      lastout = nalive;                  \
      tc_text(11);                       \
      }                                  \
    }

	/******************************************************************
	If the next coset to be checked RT-style matches the next to be
	defined, then all existing cosets close against all relators, and
	we're done.  There may be deductions queued, but since no two rows
	in the same column can have identical entries, each column of each
	relator table is a permutation - i.e., the EDP criterion is
	fulfilled.  So we can ignore any queued deduction.  Whether or not
	these should be included in the count of `discarded' deductions is
	a mute point!
	******************************************************************/

#define NEXTKNR                          \
  do                                     \
    { knr++; }                           \
  while (knr < nextdf && CT(knr,1) < 0); \
  if (knr == nextdf)                     \
    {                                    \
    disded += maxrow - topded + 1;       \
    topded = maxrow + 1;                 \
    INDEXRET;                            \
    }

#define LOOKAHEAD              \
  procRT = 0;                  \
  SET2(status, 'T', 'L');      \
  tc_proc_ded();               \
  if (index1)                  \
    { INDEXRET; }              \
  if (overfl)                  \
    { OVERFLRET; }             \
  rtenum = FALSE;              \
  if (lmtRT == 0)              \
    {                          \
    lmtCT = 1000;              \
    lmtRT = lmtCT/(2*trellen); \
    }

	/******************************************************************
	The preferred definition list (pdl) is implemented as a ring stack.
	It's size should be a power of 2, so that the NEXTPD macro can be
	fast (masking the ring index with Oxff to cycle from 255 back to
	0).  However, cycling is not expected in general.
	******************************************************************/

#define NEXTPD(i)  (++i) & 0xff

static int pdlcol[256], pdlrow[256];

static int sgdef;   	/* records no of definitions in subgroup phase */
static int knrdef;  	/* counts knr definitions in RTstyle */
static int kncdef; 	/* counts knc definitions in CTstyle */
static int gapdef;  	/* counts preferred definitions made in CTstyle */
static int gapfull; 	/* counts no of full pref defs in CTstyle */
static int totgap;  	/* counts total no of pref defs found */
static int pdldead; 	/* counts no of dead pdls in CTstyle */
static int ffdef;   	/* counts fill factor definitions in CTstyle */
static int knrfill; 	/* counts row fill definitions made in RTstyle */
static int ctded;   	/* counts CTstyle deductions */
static int rtded;   	/* counts RTstyle deductions */
static int laded;   	/* counts lookahead deductions */
static int ctcoinc; 	/* counts CTstyle primary coincidences */
static int rtcoinc; 	/* counts RTstyle primary coincidences */
static int lacoinc; 	/* counts lookahead primary coincidences */
static int apcoinc; 	/* counts apply primary coincidences */

	/******************************************************************
	static void printStats(void)

	Dumps some statistics at the end of a run.
	******************************************************************/

static void printStats(void)
  {
  fprintf(fop, 
    "#DEF: gap=%d knc=%d ff=%d dead pdl=%d; knr=%d fill=%d (sg=%d)\n", 
    gapdef, kncdef, ffdef, pdldead, knrdef, knrfill, sgdef);

  fprintf(fop, 
    "#GAP: tot=%d full=%d  #DED: CT=%d RT=%d LA=%d discarded=%d\n", 
    totgap, gapfull, ctded, rtded, laded, disded); 

  /* At this stage we have an index.  However, deductions were discarded
  and not all cosets have been `tested' against the relators.  So the index
  is not formally proved.  It always seems to be correct, but it could be a
  multiple of the actual value. */

  if (indexc && disded && knr < nextdf - 1) 
    { fprintf(fop, "WARNING: deductions discarded / cosets not"
                   " applied (index may be multiple)\n"); }

  fprintf(fop, "#COINC: CT=%d RT=%d LA=%d ap=%d\n", 
    ctcoinc, rtcoinc, lacoinc, apcoinc); 
  }

	/******************************************************************
	void tc_apply(int hcoset, int *beg, int *end, Logic defflg)

	Apply a coset (hcoset) to a word stored in beg .. end; used only
	for subgroup generators / group relations to coset 1.  If defflg, 
	define cosets to complete the trace, otherwise do not. 
	******************************************************************/

void tc_apply(int hcoset, int *beg, int *end, Logic defflg)
  {
  int *forscan, *backscan;
  int col, i, j, ifront, iback, ji, k;

  DITRACE(("apply 0", hcoset, defflg, 0, *beg, *end));
  ifront = iback = hcoset;

  /* Forward scan, leaving ifront set to coset at left of leftmost hole in
  relator or to the last coset in the relator if no hole. */

  for (forscan = beg; forscan <= end; forscan++) 
    { 
    if ((i = CT(ifront, *forscan)) != 0) 
      { ifront = i; }
    else 
      { break; }
    }

  if (i == 0)				/* Hole - backward scan required */
    {
    for (backscan = end; backscan >= forscan; backscan--) 
      {
      j = *backscan; 
      ji = invcol[j]; 

      if ((i = CT(iback, ji)) != 0) 
        { iback = i; }
      else 
        {
        if (backscan == forscan) 
          {
          /* The backward scan has only one gap, so note the deduction to
          complete the cycle, and we'll fall through & return. */

          CT(iback, ji) = ifront; 
          rtded++;
          DITRACE(("apply 1", iback, ji, ifront, *beg, *end));
          if (!rtenum) 
            { SAVEDED(iback, ji); }

          /* Surely, since backscan == forscan and there was a hole, then
          CT(ifront, j) == 0 (still) if j is not an involution or we have
          just set CT(ifront, j) = iback on the fly.  So all we need do is
          CT(ifront, j) = iback; (if required) and then return?  Given 
          code does this, but rather sloppily! */
 
          if (CT(ifront, j) != 0) 	/* Can happen, if involution ! */
            { ifront = CT(ifront, j); }
          else 				/* Not involution ? */
	    { 
            CT(ifront, j) = iback; 
            ifront = iback;
            }
          }
        else if (defflg) 		/* Define a new coset */
          {
          /* If j is an involution, and occurs next to itself, then after
          the first the others are not needed - ACE seems to recover from
          this, but index doesn't.  Note that removing j^2 is a Tietze
          transformation, not a reduction! */

          if (nextdf >= topded || nalive >= maxrow) 
            { OVERFLRET; }
          NEXTC(k); 
          knrdef++; 
          CT(iback,ji) = k;  
          CT(k,j) = iback; 
          if (!rtenum) 
            { SAVEDED(iback,ji); }
          DITRACE(("apply 2", iback, ji, k, *beg, *end));
          iback = k;
          } 
        else 
          { return; }			/* New coset definition disabled */
        }
      }
    }

  if (ifront != iback)			/* no hole, proper closure ? */
    { 
    tc_coinc(ifront, iback); 
    apcoinc++; 
    }
  }

	/******************************************************************
	static void tc_norelator(void)

	Called if the coset table is incomplete (after tracing subgroup 
	generators), and there are no relators.  It will normally just fill
	up the table and overflow, giving an incomplete coset table.
	******************************************************************/

static void tc_norelator(void)
  {
  int col, icol, k;

  do 
    {
    for (icol = 1; icol <= ncol; icol++) 
      {
      if (CT(knr, icol) == 0)
        {
        if (nalive >= maxrow || nextdf >= topded) 
          { OVERFLRET; }
        else 
          {
          NEXTC(k); 
          knrdef++;
          CT(k, invcol[icol]) = knr; 
          CT(knr, icol) = k;
          DITRACE(("enum 1", knr, icol, k, 0, 0)) ;
          }
        }
      }

    /* This bit of code is almost, but not quite, NEXTKNR+INDEXRET */

    do 
      { knr++; }
    while (knr < nextdf && CT(knr,1) < 0); 

    if (knr == nextdf) 
      { 
      tc_text(12); 
      indexc = TRUE; 
      return;
      }
    } 
  while (CT(knr, 1) >= 0);
  }

	/******************************************************************
	static void tc_subg_proc(void)

	Process subgroup generators, including relators in subgroup
	******************************************************************/

static void tc_subg_proc(void)
  {
  int i, subg, rel, cyclic_perms, cyclic_count; 
  int *beg, *end;

  for (subg = 1; subg <= nsgpg; subg++) 
    {
    beg = &subggen[subgindex[subg]];
    end = beg - 1 + subglength[subg];

    tc_apply(1, beg, end, TRUE);

    if (overfl) 
      { return; }		/* OVERFLRET was called in tc_apply() */
    if (index1) 
      { 			/* use INDEXRET ? want stats printout */
      tc_text(12); 
      indexc = TRUE; 
      return;
      }
    }

  if (nrinsgp > 0) 
    { rel = MIN0(nrinsgp, ndrel); }
  else if (nrinsgp == 0) 
    { rel = ndrel; }
  else 
    { rel = 0; }

  /* If Mendelsohn flag scan all positions in relator, else just EDPs */

  for (i = 1; i <= rel; i++) 
    {
    cyclic_count = mendel ? rellen[i]/relexp[i] : 1;
    for (cyclic_perms = 0; cyclic_perms < cyclic_count; cyclic_perms++) 
      {
      beg = &relators[relind[i] + cyclic_perms];
      end = beg + rellen[i] - 1;

      tc_apply(1, beg, end, TRUE);

      if (overfl) 
        { return; }		/* OVERFLRET was called in tc_apply() */
      if (index1) 
        {			/* use INDEXRET? */
        tc_text(12); 
        indexc = TRUE; 
        return;
        }
      }
    }
  }

	/******************************************************************
	static void tc_proc_ded(void)

	This is called only via the LOOKAHEAD macro, and treats the coset 
	table as a deduction list.  Coincidences are processed and 
	deductions are saved.  Could we start off at row=knc, instead 
	of row=1?  This last is not compatible with knc >= knr (and there
	could also have been discarded deductions)!  (What about row=knr?)

	Depending on circumstances, we may or may not compact the table, 
	detect a major collapse, discard deductions, or return to RTstyle, 
	on return from this routine.  Investigate if the `right' thing is
	always done, and if calling this from other places is beneficial.
	******************************************************************/

static void tc_proc_ded(void)
  { 
  int  i, iback, ifront, j, ji, row, irow, ires, col, icol, initnal;
  int *beg, *f_end, *forscan, *backscan, *edppt, *end;

  initnal = nalive;
  for (row = 1; row < nextdf; row++) 
    {
    if (CT(row,1) < 0) 
      { continue; }

    for (col = 1; col <= ncol; col++) 
      {		/* test if it could be looked as a right deduction */
      if (CT(row,col) == 0 || row > CT(row,col)) 
        { continue; }

      /* From here to the end is the main processing code.  At this point
      row/col are the current coset/generator pair, their table entry is
      defined, is not a coincidence, and is the `canonic' one of it and
      its inverse. */
 
      else 
        {	/* some continues, to prevent multiple processing? */
        icol = col; 
        irow = row; 
        ires = CT(row,col);
        j = invcol[icol]; 
        if (j < icol)				/* start at smallest col */
          { 
          icol = j; 
          SWAP(irow,ires);
          } 
        else if (icol == j && irow > ires) 	/* can't happen ?! */
          { SWAP(irow,ires); }

        procDed: 

        edppt = &edp[edpbeg[icol]]; 
        end = &edp[edpend[icol]];

	/* go through edps relevant for icol */

        for ( ; edppt <= end; edppt += 2)
          {
          beg = &relators[*edppt];
          if (icol == *beg) 
            { 
            iback = irow; 
            ifront = ires;
            } 
          else 
            { 
            iback = ires; 
            ifront = irow;
            }
          f_end = beg + *(edppt + 1) - 1;

          for (forscan = beg + 1; forscan <= f_end; forscan++) 
            {
            if ((i = CT(ifront, *forscan)) != 0) 
              { ifront = i; }
            else 
              { break; }		/* incomplete forward scan */
            }

          if (i == 0)	/* forward scan is incomplete - do backward scan */
            {
            for (backscan = f_end; backscan >= forscan; backscan--) 
              {
              ji = invcol[*backscan];
              if ((i = CT(iback,ji)) != 0) 
                { iback = i; }
              else if (backscan == forscan) 
                {
                /* Closure with new deduction.  Fill in the deduction &
                inverse and queue the new deduction. */

                CT(ifront,*forscan) = iback; 
                CT(iback,ji) = ifront; 
                laded++;

                DITRACE(("enum 2", iback, ji, ifront, *beg, *f_end));
                if (iback < irow || (iback == irow && ji < icol))
                  { SAVEDED(iback,ji); }
                } 
              else 
                { break; }
              }
            }

          if (i != 0 && ifront != iback) 
            {
            SET2(status, 'T', 'C'); 
            tc_coinc(iback, ifront); 
            lacoinc++;

            /* if (adapt && (nalive < initnal/4)) */
            if (adapt && (initnal-nalive > 1000) && (nalive < initnal/3)) 
              { 				/* big collapse */
              if (msgctrl != 0) 
                { tc_text(11); }
              disded += maxrow - topded + 1;
              topded = maxrow + 1; 
              return;
              }

            SET2(status, 'T', 'L'); 
            if (index1) 
              { return; }

            /* has current deduction (i.e., coset) become redundant? */

            if (CT(irow,1) < 0 || CT(ires,1) < 0) 
              { goto endCosetLoop; }
            } 
          }				/* end for(...edppt...) */

        /* icol involutory and deduction needs inverting? */

        if (icol == invcol[icol] && irow < ires)
          { 
          SWAP(irow, ires); 
          goto procDed; 
          }
        }				/* end else */
      }					/* end for(...col...) */

    endCosetLoop: 
      continue;				/* add message code? */
    }					/* end for(...row...) */
  }

	/******************************************************************
	void tc_enum(void)

	Construct a (partial or complete) coset table using a composite
	enumeration method.  The components of the method comprise 
	relator-table based enumeration (RTenum) and coset-table based 
	enumeration (CTenum).  RTenum is a variant of the HLT method while
	CTenum is a variant of the Felsch method.  The explicit nature of 
	the composite and of the variants is adaptive, governed by 
	parameters: CTfactor, RTfactor, FILLfactor, and NO of relators in 
	subgroup.  After collapses generate a long deduction queue in 
	CTstyle (currently arbitrarily set to > MAX0(1000, nalive/8)) 
	switch to RTstyle, discarding deductions (if adaptive).

	Note: pull some of this out as functions.  Perhaps separate `scan'
	functions for RT & CT mode (?overhead).  Anything else we can do
	to make this more readable/maintainable/...?

	Some (partly obsolete) descriptive comments ...

	CTenum is basically a CT-style (i.e., Felsch-style) enumeration,
	except for the addition of the `fill factor'.

	RTenum varies from the HLT method in that lookahead is done by a 
	CT scan.  Note that when 'maxrow' is near 'maxcos', because the 
	infinite looping test involves 'knr', the statistics for 'maxcos' 
	and 'totcos' may vary and, in some cases, vary significantly. 

	ded_pt: unworked deductions are placed at the back of y.  ded_pt 
	is the pointer of the deductions stack.  *ded_pt is the row, 
	*(ded_pt+1) is the column, CT(row,colunm) is the position in CT at 
	which the next deduction can be filled in.  When no further space 
	is available for the deductions stack subsequent deductions are 
	discarded and a message is printed at the end of the enumeration 
	to tell how many deductions are discarded.
	CT(nextdf,1): last position available for the deductions stack. 
	CTMode: TRUE, when in Felsch defining phase, otherwise FALSE.
	******************************************************************/

void tc_enum(void)
  { 
  int i, j, ji, k, iback, ifront, rel, row, irow, col, icol, ires;
  int *forscan, *backscan, *edppt, *edpptend, *f_end, *beg;
  Logic CTMode;
  int lmtRT;  		/* limit, cosets processed in RT defining phase */
  int procRT; 		/* counts cosets processed in RT defining phase */
  int lmtCT;  		/* limit for cosets defined in CT defining phase */
  int defCT;  		/* counts cosets defined in CT defining phase */
  int toppd;  		/* top of PDL stack */
  int botpd;  		/* bottom of PDL stack */
  int cyclic_perms, cyclic_count; 
  int precnal;		/* pre-coincidence nalive */

  indexc = index1 = FALSE; 
  disded = 0; 
  CTMode = TRUE; 
  knr = knc = 1; 
  overfl = FALSE;

  knrdef = gapfull = kncdef = ffdef = pdldead = 0; 
  gapdef = totgap = 0; 
  topded = maxrow + 1; 
  toppd = botpd = 0; 
  knrfill = ctded = rtded = laded = 0;
  ctcoinc = rtcoinc = lacoinc = apcoinc = 0;

  lmtCT = ctfactor; 
  lmtRT = abs(rtfactor); 
  rtenum = (lmtCT == 0); 
  procRT = defCT = 0;

  if (msgctrl != 0)
    { tc_text(15); } 

  if (!restart) 
    {
    for (j = 1; j <= ncol; j++) 
      { CT(1, j) = 0; }
    maxcos = totcos = nalive = 1; 
    nextdf = 2;
    nextout = msgincr = abs(msgctrl); 
    lastout = 1; 
    compct = TRUE;

    if (nrinsgp >= 0 || nsgpg != 0) 
      {
      SET2(status, 'S', 'G'); 
      tc_subg_proc();
      DITRACE(("enum 3", nalive, nextdf, maxcos, 0, 0));
      if (indexc || overfl) 
        { return; }
      } 
    } 

  sgdef = nextdf - 1;
  if (ndrel == 0) 
    { 
    tc_norelator(); 
    return;
    }

  if (lmtCT == 0) 
    { SET2(status, 'R', 'D'); }
  else 
    { SET2(status, 'C', 'D'); }

  for ( ; ; ) 
    { 					/* main enumeration loop */

  for ( ; ; ) 
    { 					/* CTenum phase loop */

  while (topded <= maxrow) 
    { 					/* deductions exist */
    irow = CT(topded, 1); 
    icol = CT(topded++, 2);
    ires = CT(irow,icol); 
 
    if ((j = invcol[icol]) < icol) 
      { 
      icol = j; 
      SWAP(irow,ires);
      } 
    else if (icol == j && irow > ires) 
      { SWAP(irow,ires); }

    procDed: 

    edppt = &edp[edpbeg[icol]]; 
    edpptend = &edp[edpend[icol]];

    /* go through edps relevant for icol */

    for ( ; edppt <= edpptend; edppt += 2) 
      {
      /* scan forward through relator starting at this deduction */
      beg = &relators[*edppt];
      if (icol == *beg) 
        { 
        iback = irow; 
        ifront = ires;
        } 
      else 
        { 
        iback = ires; 
        ifront = irow;
        }
      f_end = beg + *(edppt + 1) - 1;

      for (forscan = beg + 1; forscan <= f_end; forscan++) 
        {
        if ((i = CT(ifront, *forscan)) != 0) 
          { ifront = i; }
        else 
          { break; }			/* forward scan is incomplete */
        }

      if (i == 0) 			/* forward scan is incomplete */
        {
        for (backscan = f_end; backscan >= forscan; backscan--) 
          {
          ji = invcol[*backscan]; 
          if ((i = CT(iback, ji)) != 0) 
            { iback = i; }
          else if (backscan == forscan) 
            {				/* closes, with deduction */
            CT(ifront, *forscan) = iback; 
            CT(iback, ji) = ifront; 
            ctded++;
            SAVEDED(iback, ji);
            DITRACE(("enum 5", iback, ji, ifront, *beg, *f_end));
            } 
          else if (backscan - 1 > forscan) 
            { break; }			/* gap > 1 */
          else 
            { 				/* save gap of length 1, on pdl */
            pdlrow[botpd] = iback; 
            pdlcol[botpd] = ji; 
            botpd = NEXTPD(botpd); 
            if (botpd == toppd) 
              { toppd = NEXTPD(toppd); }
            totgap++; 
            break; 
            }
          }
        }

      if (i != 0 && ifront != iback) 
        {
        precnal = nalive; 		/* `save' pre-coincidence nalive */
        if (*status == 'C')			/* CD or CC */
          { SET2(status, 'C', 'C'); }
        else 
          { SET2(status, 'L', 'C'); }
        tc_coinc(iback, ifront);
        ctcoinc++;

        if (adapt && (((precnal-nalive > 1000) && (nalive < precnal/3)) 
            || ((maxrow-topded > 1000) && (maxrow-topded > nalive/10)))) 
          { 			/* big collapse or long ded stack */
          SET2(status, 'B', 'S');
          if (msgctrl != 0) 
            { tc_text(11); }

          defCT = 0; 
          CTMode = FALSE; 
          disded += maxrow - topded + 1;
          topded = maxrow + 1; 
          lmtRT = nalive; 
          procRT = 0;

          goto RTstart;
          }

        if (*status == 'L')			/* LA or LC */
          { SET2(status, 'L', 'A'); }
        else
          { SET2(status, 'C', 'D'); } 
        toppd = botpd = 0; 			/* Reset PDL empty */

        if (index1) 
          { INDEXRET; }

        /* test if the current deduction has become redundant */

        if (compct) 					/* ??? */
          { 
          SET2(status, 'R', 'D'); 
          lmtCT = lmtRT = 0;
 
          goto RTstart;
          } 
        else if (CT(irow, 1) < 0 || CT(ires, 1) < 0) 
          { goto endDedProcLoop; }
        }
      }

    /* is icol involutory and does deduction need inverting? */

    if (icol == invcol[icol] && irow < ires)
      { 
      SWAP(irow, ires); 
      goto procDed;
      } 

    endDedProcLoop: 
      { continue; }					/* Needed ? */
    } 

  /* There are no more deductions, so fill a gap */

  DITRACE(("enum 6", lmtRT, 0, 0, 0, 0));
  if (*status == 'A' && defCT < lmtCT && CTMode)	/* AL */
    { SET2(status, 'C', 'D'); }

  if (defCT < lmtCT && CTMode) 
    {
    for ( ; knc < nextdf; knc++) 
      {
      if (CT(knc, 1) < 0) 
        { continue; }
      for (icol = 1; icol <= ncol; icol++)
        {
        if (CT(knc, icol) == 0) 
          { goto CTdefine; }
        }
      }
    INDEXRET; 				/* coset table is complete */

    CTdefine: 

    if (nalive >= maxrow) 
      { OVERFLRET; }
    if (nextdf >= topded) 
      {
      /* If the % dead is < comppc, then don't bother.  We could do this 
      in integers, but maybe overflow problems? */

      if ((100.0*nalive)/nextdf > (100.0 - comppc)) 
        { OVERFLRET; }
      else 
        { 
        tc_compact(); 
        toppd = botpd = 0; 		/* Reset PDL empty */
        }
      } 

    NEXTC(k); 
    defCT++;
    if (toppd == botpd) 		/* no pref defns */
      { 
      CT(k,invcol[icol]) = knc; 
      CT(knc,icol) = k; 
      SAVEDED(knc,icol); 
      kncdef++;
      }
    else if (knc*fillfactor - nextdf < 0)
      { 			/* fill factor is APPROXIMATELY violated */
      CT(k,invcol[icol]) = knc; 
      CT(knc,icol) = k; 
      SAVEDED(knc,icol); 
      ffdef++;
      }
    else 
      {					/* any pdl's still `active'? */
      while (CT(pdlrow[toppd],pdlcol[toppd]) != 0) 
        { 
        gapfull++; 
        toppd = NEXTPD(toppd); 
        if (toppd == botpd) 
          { break; }
        }

      if (toppd != botpd) 
        { 				/* ... yes - use them */
        row = pdlrow[toppd]; 
        col = pdlcol[toppd]; 
        toppd = NEXTPD(toppd); 
        CT(k,invcol[col]) = row; 
        CT(row,col) = k; 
        SAVEDED(row,col); 
        gapdef++;
        DITRACE(("enum 7", row, col, k, 0, 0));
        } 
      else 
        { 				/* ... no - */
        CT(k,invcol[icol]) = knc; 
        CT(knc,icol) = k; 
        SAVEDED(knc,icol); 
        pdldead++;
        }
      }

    DITRACE(("enum 8", row, col, k, knc, 0));
    } 
  else
    { break; }		/* lmtCT is exceeded or we're not in CTmode */
  
    } 						/* end of CT phase loop */

  DITRACE(("enum 9", 0, 0, 0, 0, 0));
  defCT = 0; 
  SET2(status, 'R', 'D');
  CTMode = FALSE;

  RTstart: 

  if (CT(knr,1) < 0) 
    { NEXTKNR; }

  while (procRT < lmtRT || rtenum) 		/* RTenum phase loop */
    { 

  /* scan forward through relator as long as table entries present */
  for (rel = 1; rel <= ndrel; rel++) 
    {
    if (relexp[rel] < 0) 
      { continue; }

    cyclic_count = mendel ? rellen[rel]/relexp[rel] : 1;
    for (cyclic_perms = 0; cyclic_perms < cyclic_count; cyclic_perms++) 
      {
      beg = &relators[relind[rel] + cyclic_perms];
      f_end = beg + rellen[rel];
      ifront = iback = knr;

      for (forscan = beg; forscan < f_end; forscan++) 
        {
        if ((i = CT(ifront, *forscan)) != 0) 
          { ifront = i; }
        else 
          { break; }
        }

      if (i == 0)
        {
        for (backscan = f_end - 1; backscan >= forscan; backscan--) 
          {
          j = *backscan; 
          ji = invcol[j]; 

          if ((i = CT(iback, ji)) != 0) 
            { iback = i; }
          else if (backscan == forscan) 
            {
            /* the backward scan has only one gap so note the deduction to 
            complete the cycle. */

            CT(iback, ji) = ifront; 
            rtded++;
            DITRACE(("enum 10", iback, ji, ifront, *beg, *f_end));
            if (!rtenum) 
              { SAVEDED(iback,ji); }
            if (CT(ifront, j) != 0) 
              { ifront = CT(ifront, j); }
            else 
              { 
              CT(ifront, j) = iback; 
              ifront = iback;
              }
            } 
          else 
            {
            if (nalive >= maxrow || nextdf >= topded) 
              {
              if (rtenum)	/* in pure RTstyle we do CT lookahead. */
                {
                LOOKAHEAD; 
                DITRACE(("enum 11", 0, 0, 0, 0, 0));
                SET2(status, 'A', 'L');
 
                goto endMainLoop;
                } 
              else if (topded <= maxrow) 
                { 
                SET2(status, 'A', 'L');
 
                goto endMainLoop;
                } 
              else if (nalive >= maxrow || 
                       ((100.0*nalive)/nextdf > (100.0 - comppc)))
                { OVERFLRET; }
              else 
                { 
                tc_compact();
 
                goto redo;
                }
              }
 
            NEXTC(k); 
            knrdef++;
            CT(iback,ji) = k;  
            CT(k,j) = iback; 
            if (!rtenum) 
              { SAVEDED(iback,ji); }
            DITRACE(("enum 12", iback, ji, k, *beg, *f_end));
            iback = k;
            }
          }
        }

      /* End of the relator scan, so ensure that ifront = iback */

      if (ifront != iback) 
        {
        SET2(status, 'R', 'C');
        tc_coinc(iback, ifront); 
        rtcoinc++;
        SET2(status, 'R', 'D');
        if (index1)
          { INDEXRET; } 
        else if (CT(knr,1) < 0) 
          { break; }
        }

      continue;
      }

    if (CT(knr,1) < 0) 
      { break; }
    }

  if (CT(knr,1) < 0) 
    { 
    NEXTKNR; 
    continue;
    }

  /* scan across this row to see that all the entries are defined. */

  for (icol = 1; icol <= ncol; icol++)
    {
    if (CT(knr, icol) == 0) 
      { 					/* we have a vacancy */
      if (nalive >= maxrow || nextdf >= topded) 
        {
        if (rtenum) 
          { 			/* if in pure RT we do table lookahead */
          LOOKAHEAD;
          DITRACE(("enum 13", 0, 0, 0, 0, 0));
          SET2(status, 'A', 'L');

          goto endMainLoop;
          } 
        else if (topded <= maxrow) 
          { 					/* deductions exist */
          SET2(status, 'A', 'L');

          goto endMainLoop;
          } 
        else if (nalive >= maxrow || 
                 ((100.0*nalive)/nextdf > (100.0 - comppc)))
          { OVERFLRET; }
        else 
          { tc_compact(); }
        } 

      NEXTC(k); 
      knrfill++;
      CT(k,invcol[icol]) = knr; 
      CT(knr,icol) = k;  
      if (!rtenum) 
        { SAVEDED(knr,icol); };
      DITRACE(("enum 14", knr, icol, k, 0, 0));
      } 
    }

  NEXTKNR; 
  procRT++;

  redo: 
    { continue; }		/* redundant ? */

    } 				/* end of RTenum phase loop */

  DITRACE(("enum 15", lmtRT, 0, 0, 0, 0));
  SET2(status, 'A', 'L');
 
  procRT = 0; 
  if (rtfactor < 0) 
    { procRT = lmtRT; }
  if (knc < knr) 
    { knc = knr; }
  if (lmtCT != 0) 
    { CTMode = TRUE; }
  else 
    { CTMode = FALSE; }

  endMainLoop: 

  toppd = botpd = 0; 		/* Reset PDL empty */

    } 				/* end of main enumeration loop */
  }

