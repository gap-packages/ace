
/*******************************************************************************

  parser.c
  1 Nov 03, 21 Apr 14
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


Parser code for stand-alone ACE.  More properly, this is a mishmash of bits'n'
pieces which, together with the cmdloop.c stuff & some helper functions, does
lexical analysis, parsing, input & command processing, and error checking.  It's
not pretty, and should be rewritten in a more `maintainable' form.

*******************************************************************************/

#include "al2.h"

#include <ctype.h>

/*****************************************************************************
The parser is recursive so, under normal circumstances, we'd need a forward
declaration for the pwrd() routine.  However, this routine is used externally,
so it's declared in al2.h and a declaration is not needed.
*****************************************************************************/

/* SInt al2_pwrd(SInt); */

/*****************************************************************************
  void al2_readkey(void)

Read a keyword (ie, a command) into currkey[], converting it to LC.  This 
removes all leading WS, compresses middle WS to single ' ', and removes 
trailing WS.  It checks for bad characters and too short/long key, and 
advances position to argument (if necessary).  Note that currkey has 64 posns 
(0..63), and we have to reserve one for the string terminating '\0' character.
*****************************************************************************/

void al2_readkey(void)
  {
  SInt i = 0;

  /* build the keyword up in currkey[] ... */
  while ( currip != ':' && currip != ';' && currip != '\n' && currip != '\r' 
          && currip != EOF ) 
    {
    if (islower(currip)) 
     {
     if (i > 62)  { al2_continue("keyword too long"); }
     currkey[i++] = currip; 
     }
    else if (isupper(currip)) 
      { 
      if (i > 62)  { al2_continue("keyword too long"); }
      currkey[i++] = tolower(currip); 
      }
    else if (currip == ' ' || currip == '\t') 
      { 
      /* If 1st posn (ie, i is 0), ignore it (leaving i at 0).  Else squash
      down to single space.  Can leave trailing space (hence 63, not 62). */
      if (i > 0 && currkey[i-1] != ' ')
        { 		
        if (i > 63)  { al2_continue("keyword too long"); }
        currkey[i++] = ' '; 
        }
      }
    else  { al2_continue("keywords must only contain letters"); }
    al2_nextip();
    }

  /* Remove any trailing space & terminate string.  Complain if empty.  Skip 
  any following ':' & WS, ready for argument or cmd termination. */

  if (i > 0 && currkey[i-1] == ' ')  { i--; }	
  currkey[i] = '\0';
  if (i == 0)  { al2_continue("empty keyword"); }
  if (currip == ':')  { al2_nextnw(); }
  }

/*****************************************************************************
  void al2_readname(void)

Read a `name' (ie, command argument) into currname[].  Used for group & 
subgroup names & descriptions, and the "sys" & "text" cmds.  There is only one
of these (a fixed length <128 global; up to 127 data & a terminating \0), so 
we may need to take a copy if it'll be required later.  Note that currip has 
been setup to point to a non-blank char (ie, either the first char of the 
string or an end-of-command char).  We strip any trailing spaces & tabs from 
the name, for `neatness'.  We *assume* ASCII coding for the i/p.  An empty 
name is ok.
*****************************************************************************/

void al2_readname(void)
  {
  SInt i = 0;

  while (currip != ';' && currip != '\n' && currip != '\r' && currip != EOF) 
    { 
    if ( !( (currip >= ' ' && currip <= '~') || (currip == '\t') ) )
      { al2_continue("string contains invalid character"); } 
    if (i > 126)  { al2_continue("string too long"); }
    currname[i++] = currip; 
    al2_nextip();
    }

  while ( i > 0 && (currname[i-1] == ' ' || currname[i-1] == '\t') )  { i--; }
  currname[i] = '\0'; 
  }

/*****************************************************************************
  SInt al2_readmult(void)

Reads (& consumes) the multiplier for the workspace size, if we recognise it.
Else use the default of x1.
*****************************************************************************/

SInt al2_readmult(void)
  {
  SInt u = 1;

  if      (currip == 'k' || currip == 'K')  { u = KILO;  al2_nextnw(); }
  else if (currip == 'm' || currip == 'M')  { u = MEGA;  al2_nextnw(); }
  else if (currip == 'g' || currip == 'G')  { u = GIGA;  al2_nextnw(); }

  return u;
  }

/*****************************************************************************
  SInt al2_readgen(void)

Reads in a (possibly comma separated) list of LC generator letters.  These are
stored in the order they're read in the currname array, in posns 1...  
Duplicates are verboten and the number of generators read is returned.  Currip
is guaranteed to be a letter at entry, so j > 0 on return is certain (in the 
absence of errors).
TBA ... a final, trailing ',' is accepted (ie, "Gr: a,b,;")!
*****************************************************************************/

SInt al2_readgen(void)
  {
  SInt i, j = 0;

  while (currip != ';' && currip != '\n' && currip != '\r' && currip != EOF) 
    {
    if (islower(currip))
      {
      for (i = 1; i <= j; i++)
        { if (currname[i] == currip) { al2_continue("duplicate generator"); }}
      currname[++j] = currip;
      }
    else  { al2_continue("generators are letters between 'a' & 'z'"); }

    al2_nextnw();
    if (currip == ',')  { al2_nextnw(); }
    }

  return(j);
  }

/*****************************************************************************
  Logic al2_match(char *pattern)

Test whether currkey can be matched to pattern (with optional part [..]).
*****************************************************************************/

Logic al2_match(char *pattern)
  {
  SInt i;

  /* first try to match the required part ... */
  for (i = 0; pattern[i] != '\0' && pattern[i] != '['; i++) 
    { if (pattern[i] != currkey[i])  { return FALSE; } }

  /* if the rest is optional, try to match it ... */
  if (pattern[i] == '[') 
    {
    for ( ; pattern[i+1] != '\0' && pattern[i+1] != ']'; i++) 
      { if (pattern[i+1] != currkey[i])  { return (currkey[i] == '\0'); } }
    }

  /* everything matched, but the keyword should not be longer ... */
  return (currkey[i] == '\0');
  }

/*****************************************************************************
  void al2_endcmd(void)

To terminate a command, we must see a ';' or a newline.  Note that currip is
*not* consumed.
*****************************************************************************/

void al2_endcmd(void)
  {
  if (currip != ';' && currip != '\n' && currip != '\r' && currip != EOF) 
    { al2_continue("command must be terminated by ';' or <newline>"); }
  }

/*****************************************************************************
  static BInt al2_readuint(void)

Read in an unsigned (big) integer; a contiguous stream of digits. 
Leaves currip at next non-WS char.
*****************************************************************************/

static BInt al2_readuint(void)
  {
  BInt u = 0;

  if (isdigit(currip)) 
    {
    while (isdigit(currip))  { u = 10*u + (currip - '0');  al2_nextip(); }
    al2_skipws();
    }
  else  { al2_continue("number must begin with digit"); }

  return(u);
  }

/*****************************************************************************
  BInt al2_readint(void)

Read in a, possibly signed, (big) integer.
*****************************************************************************/

BInt al2_readint(void)
  {
  if      (isdigit(currip))                 { return( al2_readuint()); }
  else if (currip == '+')    { al2_nextnw();  return( al2_readuint()); }
  else if (currip == '-')    { al2_nextnw();  return(-al2_readuint()); }
  else    { al2_continue("number must begin with digit or '+' or '-'"); }

  return(-1);			/* never get here; stop compiler whinging */
  }

/*****************************************************************************
  void al2_readia(void)

Read comma-separated list of <= 32 integers into the `integer' array.  List
may be empty.
*****************************************************************************/

void al2_readia(void)
  {
  intcnt = 0;

  if ( !( isdigit(currip) || currip == '+' || currip == '-' ) )  { return; }

  intarr[intcnt++] = al2_readint();
  while (currip == ',') 
    {
    if (intcnt == 32)  { al2_continue("too many integers in sequence"); }
    al2_nextnw(); 
    intarr[intcnt++] = al2_readint();
    }
  }

/*****************************************************************************
The functions from hereon are responsible for implementing the recursive-
descent parser; this reads in a word, a list of words, or a list of relations,
depending on the entry point.  The current word is built-up in currword, and 
when this has been done successfully it is added to a temp list of words (or 
simply returned, if required).  If an error occurs, then this list will be 
`valid'; it will contain all words up to, but not including, the one in error.
Currently this list is accessed via a pointer in the `top-level' function 
_rdwl() or _rdrl().  This pointer should really be made a global, so that we 
could attempt error-recovery or free up the space it uses (currently, errors 
may cause memory leakage).  A successful call to either of the top-level 
functions returns a new list, which should be used to replace the current list
of either group relators or subgroup generators.  It is the caller's (of the 
parser) responsibility to deallocate any replaced list.
*****************************************************************************/

/*****************************************************************************
  static void al2_addgen(SInt pos, SInt gen)

Add a generator to the current word, at the nominated posn, growing the word 
as necessary.  Gen'rs are simply +/- ints.  Currsiz is the currently allocated
space, so existing entries in currword are in the range [0] .. [currsiz-1].
Note that no checking is done on the value of pos; it's up to the calling
routines to keep track of what's in currword[], and where, and to ensure that 
there are no `gaps' before [pos].
*****************************************************************************/

static void al2_addgen(SInt pos, SInt gen)
  {
  if (currword == NULL)
    {
    currsiz = 16;
    if ( ( currword = (SInt *)malloc(currsiz*sizeof(SInt)) ) == NULL )
      { al2_continue("out of memory (initial word)"); }
    }
  else if (pos >= currsiz)
    {
    currsiz *= 2;
    if ((currword = (SInt *)realloc(currword, currsiz*sizeof(SInt))) == NULL)
      { al2_continue("out of memory (adding generator)"); }
    }

  currword[pos] = gen;
  }

/*****************************************************************************
  static void al2_addwrd(SInt dst, SInt src, SInt len)

Add a word to the current word.  Note that this is used to copy from currword 
to itself, so either dst <= src or dst >= src+len.
*****************************************************************************/

static void al2_addwrd(SInt dst, SInt src, SInt len)
  {
  SInt i;
  for (i = 0; i < len; i++)  { al2_addgen(dst+i, currword[src+i]); }
  }

/*****************************************************************************
  static void al2_invwrd(SInt pos, SInt len)

Sneakily invert a subword of the current word.  Note that we have to reverse 
the order *and* invert *all* entries.  So we have to touch all posns; hence 
some of the apparently unnecessary work.
*****************************************************************************/

static void al2_invwrd(SInt pos, SInt len)
  {
  SInt i, gen1, gen2;

  for (i = 1; i <= (len+1)/2; i++) 
    {
    gen1 = currword[pos + i-1];   gen2 = currword[pos + len-i];
    currword[pos + i-1] = -gen2;  currword[pos + len-i] = -gen1;
    }
  }

/*****************************************************************************
  static Wlelt *al2_newwrd(SInt len)

Make a new word-list element, and copy the first len values from currword into
it.  Note that currword is indexed from 0, while data in the list is indexed 
from 1 (sigh)  At this stage all words are fully expanded, and have exponent 
1.  However, we need to flag those words which were *entered* as involutions 
(ie, as x^2, not xx).
*****************************************************************************/

static Wlelt *al2_newwrd(SInt len)
  {
  Wlelt *p;  SInt i;

  if ( ( p = al1_newelt() ) == NULL )
    { al2_restart("no memory for new word-list element"); }
  if ( ( p->word = (SInt *)malloc((len+1)*sizeof(SInt)) ) == NULL )
    { al2_restart("no memory for word-list element data"); }

  for (i = 1; i <= len; i++)  { p->word[i] = currword[i-1]; }
  p->len = len;
  p->exp = 1;

  if (len == 2 && currword[0] == currword[1] && currexp == 2)  
        { p->invol = TRUE; }
  else  { p->invol = FALSE; }

  return(p);
  }

/*****************************************************************************
  static SInt al2_pelt(SInt beg)

Parses an element into currword, beginning at position beg, and returns the 
length of the parsed element.  The BNF for an element:
  <element> = <generator> ["'"]
            | "(" <word> { "," <word> } ")" ["'"]
            | "[" <word> { "," <word> } "]" ["'"]
Note that (a,b) & [a,b] are parsed as commutators, but (ab) & [ab] as words.  
Also, [a,b,c] & (a,b,c) are parsed as [[a,b],c] & ((a,b),c).
*****************************************************************************/

static SInt al2_pelt(SInt beg)
  {
  SInt len = 0, len2, gen, sign;  char ch;

  if (isalpha(currip))			/* we have 'a'..'z' or 'A'..'Z' */
    {
    if (!galpha)  { al2_restart("you specified numeric generators"); }

    if (islower(currip))  { ch = currip;           sign =  1; }
    else                  { ch = tolower(currip);  sign = -1; }
    al2_nextnw();

    gen = genal[ch-'a'+1];
    if (gen == 0) 
      { al2_restart("<letter> must be one of the generator letters"); }
    al2_addgen(beg, sign*gen);
    len = 1;
    }
  else if (isdigit(currip) || currip == '+' || currip == '-') 
    {					/* parse a numeric generator */
    if (galpha)  { al2_restart("you specified alphabetic generators"); }

    sign = 1;
    if (currip == '+') 
      {
      al2_nextnw();
      if (!isdigit(currip)) 
        { al2_restart("'+' must be followed by generator number"); }
      }
    else if (currip == '-')
      {
      al2_nextnw();
      if (!isdigit(currip)) 
        { al2_restart("'-' must be followed by generator number"); }
      sign = -1;
      }

    gen = al2_readuint();
    if (gen == 0 || gen > ndgen) 
      { al2_restart("<number> must be one of the generator numbers"); }
    al2_addgen(beg, sign*gen);
    len = 1;
    }
  else if (currip == '(' || currip == '[') 
    { 					/* parenthesised word / commutator */
    ch = currip;
    al2_nextnw();
    len = al2_pwrd(beg);

    while (currip == ',') 
      {
      al2_nextnw();
      len2 = al2_pwrd(beg+len);
      al2_addwrd(beg+len+len2, beg, len+len2);
      al2_invwrd(beg, len);
      al2_invwrd(beg+len, len2);
      len = 2*(len + len2);
      }

    if (ch == '(' && currip != ')')  { al2_restart("matching ')' missing"); }
    if (ch == '[' && currip != ']')  { al2_restart("matching ']' missing"); }
    al2_nextnw();
    }
  else					/* otherwise this is an error */
    { al2_restart("<word> must begin with a <generator>, a '(' or a '['"); }

  /* A "'" inverts the current element.  "''" is not allowed. */

  if (currip == '\'')  { al2_invwrd(beg, len);  al2_nextnw(); }

  return(len);
  }

/*****************************************************************************
  static SInt al2_pfact(SInt beg)

Parses a factor into currword, beginning at position beg, and returns the 
length of the parsed factor.  The BNF for a factor:
  <factor> = <element> [ ["^"] <integer> | "^" <element> ]
Note that if alphabetic generators are used then the exponentiation "^" can be
dropped (but not the conjugation "^"), and the exponent "-1" can be 
abbreviated to "-".  So "a^-1 b" can be written as "a^-1b", "a-1b", "a^-b", or
"a-b".
*****************************************************************************/

static SInt al2_pfact(SInt beg)
  {
  SInt len, len2, i;

  len = al2_pelt(beg);				/* parse (first) element */

  if ( currip == '^' ||
       (galpha && (isdigit(currip) || currip == '+' || currip == '-')) ) 
    {
    if (currip == '^')  { al2_nextnw(); }	/* strip away the '^' */

    if (isdigit(currip) || currip == '-' || currip == '+') 
      {
      if (currip == '+') 
        {
        al2_nextnw();
        if (!galpha && !isdigit(currip)) 
          { al2_restart("'+' must be followed by exponent number"); }
        }                
      else if (currip == '-') 
        {
        al2_invwrd(beg, len);
        al2_nextnw();
        if (!galpha && !isdigit(currip)) 
          { al2_restart("'-' must be followed by exponent number"); }
        }

      /* If we're using alphabetic generators & dropping the "^", then "a^-1" 
      can be coded as "a-", so we might not have a digit here.  We'll fall 
      through, using the element as already parsed. */

      if (isdigit(currip)) 
        {
        currexp = al2_readuint();
        for (i = 2; i <= currexp; i++)
          { al2_addwrd(beg + (i-1)*len, beg, len); }
        len = len*currexp;
        }
      }
    else if (isalpha(currip) || currip == '(' || currip == '[') 
      {						/* this is sneaky ... */
      len2 = al2_pelt(beg+len);
      al2_addwrd(beg+len+len2, beg+len, len2);
      al2_invwrd(beg, len);
      al2_invwrd(beg, len+len2);
      len = len2 + len + len2;
      }
    else 
      { al2_restart("'^' must be followed by exponent or element"); }
    }

  return(len);
  }

/*****************************************************************************
  SInt al2_pwrd(SInt beg)

Parses a word into currword starting at position beg.  Words are defined by 
the following BNF:  <word> = <factor> { "*" | "/" <factor> }
The "*" can be dropped everywhere; but of course two numeric generators, or a 
numeric exponent and a numeric generator, must be separated by a whitespace.  
We use currexp to help detect when a relator/generator of the form x^2/X^2 (or
one of its variants) has been entered.  At the *start* of every word (ie, when
beg is 0) we prime it to 1.  The returned length is that of the (new) bit
parsed, which may only be part of the current word.
*****************************************************************************/

SInt al2_pwrd(SInt beg)
  {
  SInt len, len2;  char ch;

  if (beg == 0)  { currexp = 1; }

  len = al2_pfact(beg);

  while ( currip == '*'   || currip == '/' || isalpha(currip) || 
          isdigit(currip) || currip == '+' || currip == '-'   || 
          currip == '('   || currip == '[' ) 
    {
    if      (currip == '*')  { ch = '*';  al2_nextnw(); }
    else if (currip == '/')  { ch = '/';  al2_nextnw(); }
    else                     { ch = '*'; }

    len2 = al2_pfact(beg+len);
    if (ch == '/')  { al2_invwrd(beg+len, len2); }
    len += len2;
    }

  return(len);
  }

/*****************************************************************************
  static Wlelt *al2_rdwrd(void)

This parses a word into currword, copies it into a properly setup new word-
list element, and returns a pointer to it.
*****************************************************************************/

static Wlelt *al2_rdwrd(void)
  { return( al2_newwrd(al2_pwrd(0)) ); }

/*****************************************************************************
  static void al2_pawrd(Wlist *p)

Parse a word and add it to a list of words.
*****************************************************************************/

static void al2_pawrd(Wlist *p)
  { al1_addwl(p, al2_rdwrd()); }

/*****************************************************************************
  Wlist *al2_rdwl(void)

Creates a new list of words, parsers a sequence of words into it, and return 
the list of words.  The list can be spread across multiple input lines.
*****************************************************************************/

Wlist *al2_rdwl(void)
  {
  Wlist *p;

  if ( ( p = al1_newwl() ) == NULL )
    { al2_continue("unable to create new word-list"); }

  if (currip != ';')
    {
    al2_pawrd(p);
    while (currip == ',')  { al2_nextnw();  al2_pawrd(p); }
    }

  return(p);
  }

/*****************************************************************************
  static void al2_parel(Wlist *l)

Parse a relation (a relator is shorthand for the relation W = 1), and add it
to a list of words (ie, relators).  Note that W1 = W2 = W3 becomes W1W2' & 
W1W3', etc.
*****************************************************************************/

static void al2_parel(Wlist *l)
  {
  SInt len1, len2;

  len1 = al2_pwrd(0);			/* parse left hand (LH) side word */

  len2 = 0;
  while (currip == '=')			/* parse sequence of RH sides */ 
    {
    al2_nextnw();
    len2 = al2_pwrd(len1);
    al2_invwrd(len1, len2);
    al1_addwl(l, al2_newwrd(len1+len2));
    }

  if (len2 == 0)			/* no RH(s), take LH as relator */
    { al1_addwl(l, al2_newwrd(len1)); }
  }

/*****************************************************************************
  Wlist *al2_rdrl(void)

Allocates, reads and returns a list of relators.  Note that this is *not* the 
same as a list of words (ie, subgroup generators) since we're allowed things 
like W1 = W2.  So we have to invoke the parser via the parse relator function 
_parel().
*****************************************************************************/

Wlist *al2_rdrl(void)
  {
  Wlist *p;

  if ( ( p = al1_newwl() ) == NULL )
    { al2_continue("unable to create new word-list"); }
 
  if (currip != ';')
    {
    al2_parel(p);
    while (currip == ',')  { al2_nextnw();  al2_parel(p); }
    }

  return(p);
  }

