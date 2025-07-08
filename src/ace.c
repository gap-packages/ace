
/*******************************************************************************

  ace.c
  1 Nov 03, 19 Apr 14
  Colin Ramsay, uqcramsa@uq.edu.au

  ACE 4.100: Advanced Coset Enumerator, Version 4.1, Release 00

Copyright (c)  2014  Centre for Discrete Mathematics and Computing, 
                     The University of Queensland, Australia

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.


This is the top level stuff for Level 2 of ACE; that is, the stand-alone,
interactive `demonstration application'.

Historical fact: the first run of ACE's Level 2 interactive interface which
included an actual enumeration (as opposed to just sitting in the main loop
twiddling its thumbs) lasted from 10:56:57 am to 10:59:03 am on Tues 29th Dec 
1998, and took place in the Dept of CS & EE at The Univ of Qld.  The group was 
A_5, over the trivial subgroup, and the correct answer (ie, 60) was obtained.

*******************************************************************************/

#include "al2.h"

#include <setjmp.h>		/* for setjmp() & longjmp() */
#include <time.h>		/* for time() & ctime() */

/*****************************************************************************
Stuff declared in al2.h; ie, globals for Level 2
*****************************************************************************/

Logic okstart, okcont, okredo;
Logic tabinfo, tabindex;
Coset lresult;
Logic echo, skipnl;
int   currip;
char  currkey[64], currname[128];
SInt *currword, currsiz, currexp;
SInt  intcnt;
BInt  intarr[32];

/*****************************************************************************
Private stuff ... currently, just the jump buffer for error handling
*****************************************************************************/

jmp_buf env;

/*******************************************************************************
All Level 2 errors are filtered by one of the following three handlers. These 
take the appropriate action, and then jump back to the top-level main() routine;
ie, the outermost level of Level 2.  If we're not aborting, we swallow the 
remainder of any input line; so we may lose some commands, or mangle a 
multi-line command.  Although the code for continue() & restart() is the same, 
they return to different points (& do different things) in main().

Warning: The error-handling is fairly basic, since it's not our intent to
develop a fully-fledged interactive interface.  We simply tidy-up the best we 
can and carry on.  For complicated presentations or long series of commands, use
an input (ie, batch) file.

TBA ... continue with next *cmd*, taking skipnl into account
*******************************************************************************/

/*****************************************************************************
  void al2_continue(char *msg)

An error has occurred, but it doesn't affected the ok... flags, or the table's
validity.
*****************************************************************************/

void al2_continue(char *msg)
  {
  fflush(stdout);
  printf("** ERROR (continuing with next line)\n");
  printf("   %s\n", msg);

  while (!(currip == '\n' || currip == '\r' || currip == EOF))
    { al2_nextip(); }

  longjmp(env,1);
  }

/*****************************************************************************
  void al2_restart(char *msg)

Something nasty has happened & we'll be disallowing continue/redo.
*****************************************************************************/

void al2_restart(char *msg)
  {
  fflush(stdout);
  printf("** ERROR (restarting with next line)\n");
  printf("   %s\n", msg);

  while (!(currip == '\n' || currip == '\r' || currip == EOF))
    { al2_nextip(); }

  longjmp(env,2);
  }

/*******************************************************************************
  void al2_abort(char *msg)

No point in being clever here, we're going to stop.  This routine is not used at
present.

TBA ... delete (as unused), or *should* we use it someplace(s)?
*******************************************************************************/

void al2_abort(char *msg)
  {
  fflush(stdout);
  printf("** ERROR (aborting)\n");
  printf("   %s\n", msg);

  longjmp(env,3);
  }

/*****************************************************************************
  static char *datetime(void)

Gets the system date/time, and converts it to ASCII string.  Note that this 
includes a trailing '\n'.
*****************************************************************************/

static char *datetime(void)
  {
  time_t t = time(NULL);
  return ctime(&t);
  }

/*****************************************************************************
  int main(void)

ACE takes no arguments, and normally returns 0; something -ve will be returned
on an `error'.  All input is from stdin & all output is to stdout; stderr is
not used.
*****************************************************************************/

int main(void)
  {
  al2_init();			/* Initialise Levels 2, 1 & 0 */

  printf("%s                       %s", ACEVER, datetime());
  printf("========================================================\n");

  switch(setjmp(env))
    {
    case 0:			/* First time through */

      /* Set to "Default" mode.  Note that the init's done via al2_init() are
      *not* the same, for some variables, as setting default mode. */

      pdefn    = TRUE;		/* Default is to use the pdl ... */
      ffactor1 = 0;		/* ... with fill factor of ~5(ncol+2)/4 */
      pdsiz1   = 256;		/* ... and a 256 byte list       */

      lahead = 0;		/* We do a CL, not a lookahead */

      dedmode = 4;		/* Process all deductions ... */
      dedsiz1 = 1000;

      break;

    case 1:			/* Non-fatal error (continuable) */

      break;

    case 2:			/* Non-fatal error (restartable) */

      okstart = ((costable != NULL) && (ndgen > 0));
      okcont  = okredo = FALSE;

      tabinfo = tabindex = FALSE;

      break;

    case 3:			/* Fatal error (aborts) */

      printf("========================================================\n");
      printf("%s                       %s", ACEVER, datetime());

      exit(-1);
      break;

    default:			/* Reality failure */

      printf("** INTERNAL ERROR\n");
      printf("   unknown jump to error handler\n");
      printf("========================================================\n");
      printf("%s                       %s", ACEVER, datetime());

      exit(-2);
      break;
    }

  /* If costable is NULL at this point, then either this is the first time
  through, or an attempt to allocate the requested workspace has failed.  In 
  either case, we attempt to allocate the default amount of workspace.  If this
  fails, then we terminate extremely prejudicially.  The type size_t is used by 
  malloc() etc, and is "an unsigned integer type that can represent the size of 
  the largest data object you can declare" (P.J. Plauger, The Standard C 
  Library, Prentice Hall, 1992, p219). */

  if (costable == NULL)
    {
    costable = (Entry *)malloc( (size_t)DEFWORK * (size_t)sizeof(Entry) );
    if (costable == NULL)
      {
      printf("** MEMORY PROBLEM\n");
      printf("   unable to allocate default workspace\n");
      printf("========================================================\n");
      printf("%s                       %s", ACEVER, datetime());

      exit(-3);
      }

    workspace = DEFWORK;
    workmult  = 1;

    /* We have a newly allocated table, so start is (maybe) possible.
    Continuing & redoing are not.  The table has no information. */

    okstart = (ndgen > 0);
    okcont  = okredo = FALSE;

    tabinfo = tabindex = FALSE;
    }

  al2_cmdloop();		/* Where it all happens. */

  printf("========================================================\n");
  printf("%s                       %s", ACEVER, datetime());

  return(0);
  }

