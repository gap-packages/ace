
/**************************************************************************

	aceutils.c
	Colin Ramsay
	19 Jun 98

	ADAPTIVE COSET ENUMERATOR, Version 2.000

	Copyright 1998 
	Centre for Discrete Mathematics and Computing,
	Department of Mathematics and 
	Department of Computer Science & Electrical Engineering,
	The University of Queensland, QLD 4072.

This contains some utilities for the coset enumerator.  Rewritten by Colin 
Ramsay, based on code by George Havas et al.

**************************************************************************/

	/******************************************************************
	We seem to need sys/types.h & time.h.  On other systems, we might 
	need sys/time.h.
	******************************************************************/

#include <sys/types.h>
#include <time.h>

#include <string.h>

#include "tc.h"

extern void tc_error(char*);

	/******************************************************************
	char *utl_date(void)

	Gets the system date/time, and converts it to ASCII string.
	******************************************************************/

char *utl_date(void)
  {
  time_t t = time(NULL);
  return ctime(&t);
  }

	/******************************************************************
	double utl_clock(void)

	Clock() returns the _actual_ cpu time _used_, in seconds.  It's 
	equivalent to the `user' time in the `time' system command.  Type 
	clock_t is usually defined as a (signed) long, but seems to 
	actually be a 32-bit unsigned - we try our best to preserve all
	information over a variety of machines!  Note that 64-bit machines 
	may sign extend, hence the truncation.  CLOCKS_PER_SEC (usually 
	1000000, but may be 100 on a PC) converts clock() to seconds.
	Note that, even if CLOCKS_PER_SECOND > 100, resolution may only be 
	10mS.

	Todo: at some time in the future, we should probably do this right!
	******************************************************************/

double utl_clock(void)
  {
  unsigned long ulc = 0xffffffffUL & (unsigned long)clock();

  return (double)ulc/(double)CLOCKS_PER_SEC;
  }

	/******************************************************************
	double utl_diff(double c1, double c2)

	We assume that c1/c2 are values from utl_clock().  This routine 
	finds the difference between two times, by assuming that either 0 
	or 1 `overflow' has taken place.  Doubles are used for all timing
	to allow (long) times to be properly processed.  Provided that the 
	run is `short' (w.r.t. to the normal rollover interval of 71m35s) 
	or that progress messages are output `frequently', then the
	difference will be correct.  On long runs with few messages, then 
	the difference may be incorrect.
	******************************************************************/


double utl_diff(double c1, double c2)
  {
  double clkroll = ((double)65536*(double)65536)/(double)CLOCKS_PER_SEC;

  if (c2 >= c1)
    { return (c2-c1); }
  else
    { return (clkroll - c1 + c2); }
  }

	/******************************************************************
	char *StrDup(char *s)

	strdup() is not ANSII C, so this is our version.
	Note that the error is fatal, so should be a tc_abort() call!
	******************************************************************/

char *StrDup(char *s)
  {
  char *t;

  if ((t = malloc(strlen(s)+1)) == NULL)
    { tc_error("out of memory in StrDup()"); }

  strcpy(t,s);
  return (t);
  }

	/******************************************************************
	static int outlen(int i)

	Returns the print-length of an integer i (i.e., ~ $\log_{10}i$).
	The int i is assumed to satisfy i >= 0.
	******************************************************************/

static int outlen(int i)
  {
  int l = 1;

  while ((i /= 10) != 0)
    { l++; }

  return l;
  }

	/******************************************************************
	void ditrace(char *s, int i1, int i2, int i3, int i4, int i5)

	If debug/trace is enabled, DITRACE() is defined as an alias for 
	this function, else it's void.  The info printed includes _all_
	`significant' actions (definitions, deductions, coincidences), and
	can be used as the basis for a formal proof of things.

	Note: take care, can generate _lots_ of output!

	Todo: make the printout more informative;  also add an ability to
	select only some of the data to print.
	******************************************************************/

#ifdef __DI
void ditrace(char *s, int i1, int i2, int i3, int i4, int i5)
  {
  fprintf(fop, "%s: i1=%d,i2=%d,i3=%d,i4=%d,i5=%d\n", s,i1,i2,i3,i4,i5);
  }
#endif

	/******************************************************************
	void tc_text(int arg)

	The numeric argument serves purely to select which printout to do,
	and has no significance.  We start at 11 since it's the first odd
	prime number whose digits sum to 2 (the next is 101).
	******************************************************************/

void tc_text(int arg)
  {
  double difftime;

  endtime = utl_clock();
  difftime = utl_diff(begintime, endtime);
  totaltime += difftime;

  /* TEST/DEBUG */ /*
  fprintf(fop, "* beg=%4.2f, end=%4.2f, dif=%4.2f, tot=%4.2f\n",
          begintime, endtime, difftime, totaltime);
  */

  switch(arg) 
    {
    case 11:
      fprintf(fop, "%s a=%d m=%d t=%d knr=%d knc=%d cpu=+%4.2f\n", 
              status, nalive, maxcos, totcos, knr, knc, difftime);
      break;
    case 12:
      fprintf(fop, 
              "INDEX = %d (MAX=%d TOT=%d KNR=%d KNC=%d CPU=%4.2f)\n", 
              nalive, maxcos, totcos, knr, knc, totaltime);
      break;
    case 13:
      fprintf(fop, "OVERFLOW (MAX=%d TOT=%d KNR=%d KNC=%d CPU=%4.2f)\n", 
              maxcos, totcos, knr, knc, totaltime);
      break;
    case 14:
      fprintf(fop, "CO a=%d m=%d t=%d knr=%d knc=%d cpu=+%4.2f\n", 
              nalive, maxcos, totcos, knr, knc, difftime);
      break;
    case 15:
      fprintf(fop, 
        "Max=%d CTF=%d RTF=%d FillF=%d NRinS=%d Mendel=%d Adapt=%d\n",
        maxrow, ctfactor, rtfactor, fillfactor, nrinsgp, mendel, adapt);
      break;
    default:
      fprintf(fop, "WARNING: tc_text() called with unknown argument\n");
      break;
    }
  
  begintime = utl_clock();
  }

	/******************************************************************
	void setDefaults(void)

	Does this set everything that needs to be set - check with
	'main()' & 'read_commands()'.  Probably should reorganise this.
	******************************************************************/

void setDefaults(void)
  {
  nsgpg = ndrel = 0; 
  comppc = 20; 

  SET3(prpar, 0, 0, 1);
  SET3(maxpar, 0, 0, 1);
  SET3(ctpar, 0, 0, 1);
  SET3(rtpar, 0, 0, 1);
  SET3(fipar, 0, 0, 1);
  SET3(rinsgpar, 0, 0, 1);

  msgctrl = newrelpar = newsgpar = 0;
  restart = mendel = asis = adapt = FALSE;
  index1 = overfl = FALSE; 
  compct = TRUE;
  newrel = newsg = mstlevel = 0;
  mst = crepx = NULL;
  mstflg = ctflg = FALSE; 
  strcpy(status, "XX");			/* initial status - `unknown' */
  }

	/******************************************************************
	void tc_compact(void)

	Remove unused rows from the coset table, by closing up all used
	rows to the front.
	******************************************************************/

void tc_compact(void)
  {
  int i, j, irow, col, hlt, fel;

  DITRACE(("compact 1", nalive, nextdf, knc, knr, 0));

  if (compct) 
    { return; }
  if (topded <= maxrow) 
    { tc_error("Compact called while deductions exist"); }

  fel = hlt = 0; 		/* counters for knc and knr adjustment */

  /* find the lowest redundant coset irow */

  for (irow = 1; irow < nextdf; irow++) 
    { 
    if (CT(irow, 1) < 0) 
      { break; }
    }

  /* compact the coset table */

  for (i = irow; i < nextdf; i++)
    {
    if (CT(i, 1) < 0) 
      { 
      if (i <= knr) 
        { hlt++; }
      if (i <= knc) 
        { fel++; } 
      }
    else 
      { 			/* convert row i to row irow.  */
      for (col = 1; col <= ncol; col++) 
        {
        if ((j = CT(i, col)) != 0) 
          {
          if (j == i)  
            { j = irow; }
          else 
            { CT(j, invcol[col]) = irow; }
          }
        CT(irow, col) = j;
        }
      irow++;
      }
    }

  knr -= hlt;			/* adjust counters */
  knc -= fel; 
  nextdf = irow;

  compct = TRUE; 
  mstflg = FALSE; 
  if (msgctrl != 0) 
    { tc_text(14); }
 
  DITRACE(("compact 2", nalive, nextdf, knc, knr, 0));
  }

	/******************************************************************
	void tc_cycles(void)

	Print out the coset table in cycles (permutation representation).
	This should only be called when a completed coset table is present;
	i.e., when a finite index has been computed.  Should the check(s)
	be at the point of call, or inside the function?  Note the use of
	the sign bit to track processed cosets for each generator.
	******************************************************************/

void tc_cycles(void)
  {
  int i, j, k, kn, t, length;
  Logic id;

  tc_compact(); 

  for (j = 1; j <= ndgen; j++) 
    {
    k = gencol[j]; 		/* find the column k for generator j */
    id = TRUE;			/* assume action is the identity */

    if (ngennum != 0) 		/* print lhs & record its length */
      { 
      fprintf(fop, "%d = ", j);
      length = outlen(j) + 3;
      } 
    else 
      { 
      fprintf(fop, "%c = ", algen[j]);
      length = 4;
      }

    for (i = 1; i <= nalive; i++) 
      {
      if (CT(i, k) == i)  	/* skip if i is a one-cycle */
        { 
        CT(i, k) = -i; 
        continue; 
        }

      /* have we used coset i in previous cycle?  */

      if (CT((kn = i), k) <= 0) 
        { continue; } 

      id = FALSE;		/* action of generator not identity */

      /* no, trace out this cycle  */

      length += outlen(kn) + 1;
      if (length < LLL) 
        { fprintf(fop, "(%d", kn); }
      else
        {
        fprintf(fop, "\n  (%d", kn); 
        length = outlen(kn) + 3;
        }

      t = CT(kn, k);
      CT(kn, k) = -t; 		/* mark this coset as used */
      kn = t;

      while (CT(kn,k) > 0) 
        {
        length += outlen(kn) + 1;
        if (length < LLL) 
          { fprintf(fop, ",%d", kn); }
        else 
          { 
          fprintf(fop, ",\n  %d", kn); 
          length = outlen(kn) + 2;
          } 

        t = CT(kn, k);
        CT(kn, k) = -t;
        kn = t;
        }

      /* we have reached the end of the cycle */

      fprintf(fop, ")"); 
      length++;
      }

    if (id) 
      { fprintf(fop, "identity\n"); } 
    else 
      { fprintf(fop, "\n"); }

    /* change all the (negative) values in this column back to positive */

    for (i = 1; i <= nalive; i++) 
      { CT(i, k) = -CT(i, k); }
    }
  }  

