
/**************************************************************************

        parser.c
        Colin Ramsay
        17 Jun 98

	ADAPTIVE COSET ENUMERATOR, Version 2.000

	Copyright 1998 
	Centre for Discrete Mathematics and Computing,
	Department of Mathematics and 
	Department of Computer Science & Electrical Engineering,
	The University of Queensland, QLD 4072.

Parser and dispatcher code.  Parser initially written by Martin Schoenert, 
modified by George Havas, and turned into readable code by Colin Ramsay.
Note that, although this works fine, it does not have much in the way of
error checking; particularly array overflows & memory allocation failures.
Nor does it allow input line editing!  There also needs to be more checking
before we fire up an option.  Check inter-consistency of the global data!
**************************************************************************/

#include <setjmp.h>
#include <ctype.h>
#include <string.h>

#include "tc.h"

extern void printdetails(void);
extern void tc_add_cc(int);
extern void tc_cycles(void);
extern void tc_start(void);
extern void tc_normcl(Logic);
extern void tc_o(int);
extern void tc_print_ct(int, int, int);
extern void tc_sc(int);
extern void tc_text(int);
extern int tc_tracew(int, int*, int*);
extern char *StrDup(char*);

static int parse_word(int);		/* Function forward declaration */

static int Ch;				/* Parser current i/p character */

	/******************************************************************
	This defaults to FALSE, and should be left that way for interactive
	use.  If output is redirected to a file, we might want to set this 
	so that the commands are also logged.
	******************************************************************/

static Logic echo;

static Logic skip_newlines_flag_input;	/* treat '\n' as whitespace */
static jmp_buf again_input;		/* error, jump back to main loop */

	/******************************************************************
	Contains the current keyword.  Make 64 a #define?  Best size?
	******************************************************************/

static char key[64];

static int alpha_gens[256];		/* what is this ? alpha/numeric */
static int *curr_word;			/* current word */
static int size_curr_word;

	/******************************************************************
	Wordlhdr new_empty_word_list(void)

	Make a new empty word list.  The  ACE 'list' module implements a 
	list type which provides singly-linked lists with a header node.  A
	list header contains the list length and pointers to the first and 
	last nodes.
	******************************************************************/

Wordlhdr new_empty_word_list(void)
  {
  Wordlhdr l = (Wordlhdr)malloc(sizeof(*l));	/* name this type ! */

  l->length = 0;
  l->first = l->last = NULL;

  return l;
  }

	/******************************************************************
	void add_word_to_word_list(Wordlhdr l, Wordlptr word)

	Append a word to a word list - assumes l != NULL.
	******************************************************************/

void add_word_to_word_list(Wordlhdr l, Wordlptr word)
  {
  if (word->len == 0) 
    { return; }				/* ignore empty words */

  if (l->length == 0) 			/* add word to end of list ... */
    { l->first = word; }
  else                  
    { l->last->next = word; }

  l->last = word; 
  l->length++;
  }

	/******************************************************************
	static void concat_word_lists(Wordlhdr *l1, Wordlhdr l2)

	Append l2 (which is not NULL, but could be empty) to l1, and 
	delete l2's header node.
	******************************************************************/

static void concat_word_lists(Wordlhdr *l1, Wordlhdr l2)
  {
  if (*l1 == NULL) 
    { 
    *l1 = l2; 
    return;
    } 

  if ((*l1)->length == 0) 
    { **l1 = *l2; }				/* copy all 3 fields */
  else 
    { 
    (*l1)->last->next = l2->first; 
    (*l1)->last = l2->last; 
    (*l1)->length += l2->length;
    }

  free(l2);
  }

	/******************************************************************
	static void next_char_input(void)

	Get the next input character.  Note: we accept any character at
	this stage, 'nasties' are detected later.  We can't call tc_error()
	from here, since that calls this!
	******************************************************************/

static void next_char_input(void)
  {
  if (Ch != EOF) 
    { 
    Ch = fgetc(fip); 

    if (echo && Ch != EOF) 
      { fputc(Ch, fop); }
    }
  }

	/******************************************************************
	static void alter_input(char *name)

	Switch to a new input file.  Print warning if not possible.  We 
	can't call tc_error() from here, since that calls this!  Should
	we push back Ch, and prime it from the new input?
	******************************************************************/

static void alter_input(char *name)
  {
  /* close the current input file (unless it is 'stdin') */
  if (fip != stdin && fip != NULL)  
    { fclose(fip); }
  fip = NULL;

  /* try to open the new input file (unless it is 'stdin') */
  if (strcmp(name, "stdin") != 0)  
    { 
    if ((fip = fopen(name, "r")) == NULL)
      { fprintf(fop, "Can't open new input, using 'stdin'\n"); }
    }

  /* use stdin if we didn't or couldn't open another file */
  if (fip == NULL)  
    { fip = stdin; }

  Ch = '\0'; 				/* initialise Ch (? needed) */
  }

	/******************************************************************
	void tc_error(char *mesg)

	Signal an error and skip to the next line on stdin.
	******************************************************************/

void tc_error(char *mesg)
  {
  if (fip == stdin) 
    {
    fprintf(fop, "\nError: %s (skipping to next line)\n", mesg);
    while (Ch != '\n' && Ch != '\r' && Ch != EOF) 
      { next_char_input(); }
    }
  else 
    {
    fprintf(fop, "\nError: %s (switching back to 'stdin')\n", mesg); 
    alter_input("stdin");
    }

  longjmp(again_input, 1);
  }

	/******************************************************************
	static void skip_whitespaces_input(void)

	Skip all whitespace characters.
	******************************************************************/

static void skip_whitespaces_input(void)
  {
  Logic is_comment_input = (Ch == '#');

  while (Ch == ' ' || Ch == '\t' || is_comment_input || 
         (skip_newlines_flag_input && (Ch == '\n' || Ch == '\r'))) 
    {
    next_char_input();
    is_comment_input = (Ch == '#' || 
              (is_comment_input && Ch != '\n' && Ch != '\r' && Ch != EOF));
    }
  }

	/******************************************************************
	static void next_nonwhite_input(void)

	Get the next nonwhite input character.  Note the `lookahead' to the
	next non-white.  This can cause problems in interective use.
	******************************************************************/

static void next_nonwhite_input(void)
  {
  next_char_input(); 
  skip_whitespaces_input();
  }

	/******************************************************************
	static void alter_output(char *name)

	Close the current output file (unless it is 'stdout')
	******************************************************************/

static void alter_output(char *name)
  {
  /* close the current output file (unless it is 'stdout') */
  if (fop != stdout && fop != NULL)  
    { fclose(fop); }
  fop = NULL;

  /* try to open the new output file (unless it is 'stdout') */
  if (strcmp(name, "stdout") != 0)  
    { 
    if ((fop = fopen(name, "w")) == NULL)
      { fprintf(fop, "Can't open new output, using 'stdout'\n"); }
    else
      { setvbuf(fop, NULL, _IOLBF, 0); }		/* line buffered */
    }

  /* use stdout if we didn't or couldn't open another file */
  if (fop == NULL)  
    { fop = stdout; }
  }

	/******************************************************************
	static void readkey(void)

	Read a keyword into 'key', converting it to LC.  This removes all 
	leading WS, compresses middle WS to single ' ', and removes
	trailing WS.  It checks for bad characters and too short/long key,
	and advances position to argument (if necessary).
	******************************************************************/

static void readkey(void)
  {
  int i = 0;

  /* Copy the keyword into 'key' */
  while (Ch != ':' && Ch != ';' && Ch != '\n' && Ch != '\r' && Ch != EOF) 
    {
    if (islower(Ch)) 
     {
     if (i > 62)
       { tc_error("keyword to long"); }
     key[i++] = Ch; 
     }
    else if (isupper(Ch)) 
      { 
      if (i > 62)
        { tc_error("keyword to long"); }
      key[i++] = tolower(Ch); 
      }
    else if (Ch == ' ' || Ch == '\t') 
      { 
      if (0 < i && key[i-1] != ' ')	/* leading/multiple spaces? */
        { 
        if (i > 63)			/* may be removed trailing space */
          { tc_error("keyword to long"); }
        key[i++] = ' '; 
        }
      }
    else 
      { tc_error("keywords must only contain letters"); }
    next_char_input();
    }

  if (0 < i && key[i-1] == ' ')		/* remove trailing space */
    { i--; }
  key[i] = '\0';			/* string terminator */

  if (i == 0)
    { tc_error("Empty keyword"); }

  if (Ch == ':')			/* skip any following ':' & WS */
    { 
    next_char_input(); 
    skip_whitespaces_input();
    }
  }

	/******************************************************************
	static Logic matches(char *name)

	Test whether input keyword 'key' matches 'name'.
	******************************************************************/

static Logic matches(char *name)
  {
  int i;

  /* first try to match the required part */
  for (i = 0; name[i] != '\0' && name[i] != '['; i++) 
    { 
    if (name[i] != key[i]) 
      { return FALSE; } 
    }

  /* if the rest is optional, try to match it */
  if (name[i] == '[') 
    {
    for ( ; name[i+1] != '\0' && name[i+1] != ']'; i++) 
      {
      if (name[i+1] != key[i]) 
        { return (key[i] == '\0'); } 
      }
    }

  /* everything matched (but 'keyword' should not be longer) */
  return (key[i] == '\0');
  }

	/******************************************************************
	static char *readname(void)

	Read a name.  Used for group/subgroup names/descriptions, I/O 
	filenames, and system calls.  There only ever seems to be one of 
	these, so make it a (fixed length = 128?) global, as is key[].  
	(Take a copy if `permanent' copy required.)
	******************************************************************/

static char *readname(void)
  {
  char *name = (char *)malloc(sizeof(char)*128);
  int i = 0;

  /* parse the characters into name */
  while (Ch != ';' && Ch != '\n' && Ch != '\r' && Ch != EOF) 
    { 
    if (!((Ch >= ' ' && Ch <= '~') || (Ch == '\t')))
      { tc_error("name contains invalid character"); } 
    if (i > 126)			/* 0..126 is data, 127 is '\0' */
      { tc_error("name too long"); }

    name[i++] = Ch; 
    next_char_input();
    }

  name[i++] = '\0'; 
  return name;
  }

	/******************************************************************
	static int read_mult(void)

	Read multiplier for workspace size.
	******************************************************************/

static int read_mult(void)
  {
  int u = 1;				/* Default is x1 */

  if (Ch == 'k' || Ch == 'K')
    { 
    u = KILO;
    next_char_input();
    skip_whitespaces_input();
    }
  else if (Ch == 'm' || Ch == 'M')
    { 
    u = MEGA; 
    next_char_input();
    skip_whitespaces_input();
    }
  else if (Ch == 'g' || Ch == 'G')
    { 
    u = GIGA; 
    next_char_input();
    skip_whitespaces_input();
    }

  return u;
  }

	/******************************************************************
	static int read_unsigned(void)

	Read an unsigned integer.  No overflow check!
	******************************************************************/

static int read_unsigned(void)
  {
  int u;

  if (isdigit(Ch)) 
    {
    u = 0;
    while (isdigit(Ch)) 
      { 
      u = 10*u + Ch - '0'; 
      next_char_input();
      }
    skip_whitespaces_input();
    }
  else 
    { tc_error("number must begin with digit"); }

  return u;
  }

	/******************************************************************
	static int read_integer(void)

	Read an integer.
	******************************************************************/

static int read_integer(void)
  {
  if (isdigit(Ch)) 
    { return read_unsigned(); }
  else if (Ch == '+') 
    { 
    next_nonwhite_input(); 
    return read_unsigned(); 
    }
  else if (Ch == '-') 
    { 
    next_nonwhite_input(); 
    return -read_unsigned();
    }
  else 
    { tc_error("number must begin with digit or '+' or '-'"); }

  return 0;		/* Stop compiler warning; never get here! */
  }

	/******************************************************************
	static Intarray *read_Intarray(void)

	Read a comma separated sequence of integers.  Don't like this
	static thingy - make it a single global?  Sensible size & check it.
	******************************************************************/

static Intarray *read_Intarray(void)
  {
  static Intarray ia;		/* need to return pointer to this ! */

  ia.icount = 0;
  ia.integers[ia.icount++] = read_integer();
  while (Ch == ',') 
    {
    if (ia.icount == 64)
      { tc_error("too many integers in sequence"); }

    next_nonwhite_input(); 
    ia.integers[ia.icount++] = read_integer();
    }
  return &ia;
  }

	/******************************************************************
	static void read_looppar(int *a)

	Read and assign loop control parameters - first element is (min) 
	value, optional second element is max value (default = min),
	optional third element is (positive) step value (default = 1). Any 
	value checks needed?
	******************************************************************/

static void read_looppar(int *a)
  {
  Intarray *iap = read_Intarray();

  switch (iap->icount)
    {
    case 1:
      a[1] = a[0] = iap->integers[0]; 
      a[2] = 1;
      break;
    case 2:
      a[0] = iap->integers[0]; 
      a[1] = iap->integers[1]; 
      a[2] = 1;
      break;
    case 3:
      a[0] = iap->integers[0]; 
      a[1] = iap->integers[1]; 
      a[2] = iap->integers[2];
      break;
    default:
      tc_error("list must contain 1, 2, or 3 integers");
      break;
    }
  }

	/******************************************************************
	static void read_numer_gens(void) ?? !

	Reads an integer and sets 'ngennum' to this.
	******************************************************************/

static void read_numer_gens(void)
  {
  int i;

  /* clear the generators information */
  ngennum = 0;
  for (i = 1; i <= ngenal; i++) 
    { 
    alpha_gens[(int)(algen[i])] = 0; 
    algen[i] = '\0';
    }
  ngenal = 0;

  /* read the generators */
  ngennum = read_integer();
  }

	/******************************************************************
	static void read_alpha_gens(void) ?? !

	Reads a list of alphabetic generators, sets 'ngenal' to the 
	number of alphabetic generators, 'algen[<i>]' to the letter of 
	the <i>-th alphabetic generator, and 'alpha_gens[<ch>]' to the 
	alphabetic generator for the letter <ch>.  In fact, alpha_gens need
	only be 26 long, and can be local to this & previous function!
	Where is ngenal (initially) set?
	******************************************************************/

static void read_alpha_gens(void)
  {
  int i;

  /* clear the generators information (common to read_numer_gens) */
  ngennum = 0;
  for (i = 1; i <= ngenal; i++) 
    { 
    alpha_gens[(int)(algen[i])] = 0; 
    algen[i] = '\0';
    }
  ngenal = 0;

  /* read the generators */
  while (Ch != ';' && Ch != '\n' && Ch != '\r' && Ch != EOF) 
    {
    if (islower(Ch)) 
      {
      if (alpha_gens[Ch] != 0) 
        { tc_error("generator must not be declared twice"); }
      ngenal++;
      alpha_gens[Ch] = ngenal;
      algen[ngenal] = Ch;
      }
    else 
      { tc_error("generator must be a letter between 'a' and 'z'"); }
    next_nonwhite_input();
    if (Ch == ',') 
      { next_nonwhite_input(); }
    }
  }

	/******************************************************************
	static void add_gen_to_curr_word(int pos, int gen)

	Add a generator to current word.  Make the default size 1024 a
	#define?
	******************************************************************/

static void add_gen_to_curr_word(int pos, int gen)
  {
  /* allocate or resize the current word as necessary */
  if (curr_word == NULL) 
    {
    size_curr_word = 1024;
    curr_word = (int*)malloc(sizeof(int)*size_curr_word);
    }
  else if (size_curr_word <= pos) 
    {
    size_curr_word = 2*size_curr_word;
    curr_word = (int*)realloc(curr_word, sizeof(int)*size_curr_word);
    }

  curr_word[pos] = gen;				/* add the generator */
  }

	/******************************************************************
	static void add_word_to_curr_word(int pos_dst,int pos_src,int len)

	Add a word to current word.  Fold with previous, to avoid lots of 
	malloc's etc.  Copies to itself?
	******************************************************************/

static void add_word_to_curr_word(int pos_dst, int pos_src, int len)
  {
  int i;
  for (i = 1; i <= len; i++) 
    { add_gen_to_curr_word(pos_dst + i-1, curr_word[pos_src + i-1]); }
  }

	/******************************************************************
	static void invert_subword_of_curr_word(int pos, int len)

	Invert a subword of current word.
	******************************************************************/

static void invert_subword_of_curr_word(int pos, int len)
  {
  int i, gen1, gen2;

  for (i = 1; i <= (len+1)/2; i++) 
    {
    gen1 = curr_word[pos + i-1]; 
    gen2 = curr_word[pos + len-i];
    curr_word[pos + i-1] = -gen2; 
    curr_word[pos + len-i] = -gen1;
    }
  }

	/******************************************************************
	static Wordlptr new_word(int len)

	Make a new ACE word, and copy the first len values from curr_word 
	into it.  Memory checks on the malloc's?
	******************************************************************/

static Wordlptr new_word(int len)
  {
  Wordlptr w;
  int i;

  /* allocate and initialise the word */
  w = (Wordlptr)malloc(sizeof(*w));
  w->exp = 1; 
  w->next = NULL;

  /* copy the first len values across */
  w->len = len;
  w->wordgen = (int*)malloc((len+1)*sizeof(int));
  for (i = 1; i <= len; i++) 
    { w->wordgen[i] = curr_word[i-1]; }

  return w; 					/* return the word */
  }

	/******************************************************************
	static int parse_element(int beg)

	Parses [a,b,c] as [[a,b],c]?
	******************************************************************/

static int parse_element(int beg)
  {
  int len, len2, gen, sign;
  char ch;

  if (isalpha(Ch))
    {				/* parse an alphabetic generator */
    if (islower(Ch)) 
      { 
      ch = Ch; 
      sign = 1;
      }
    else
      { 
      ch = tolower(Ch); 
      sign = -1;
      }
    gen = alpha_gens[(int)ch];
    next_nonwhite_input();
    if (gen == 0 || ngenal < gen) 
      { tc_error("<letter> must be one of the generator letters"); }
    add_gen_to_curr_word(beg, sign*gen);
    len = 1;
    }
  else if (isdigit(Ch) || Ch == '+' || Ch == '-') 
    {				/* parse a numeric generator */
    sign = 1;
    if (Ch == '+') 
      {
      next_nonwhite_input();
      if (!isdigit(Ch)) 
        { tc_error("'+' must be followed by generator number"); }
      }
    else if (Ch == '-')
      {
      next_nonwhite_input();
      if (!isdigit(Ch)) 
        { tc_error("'-' must be followed by generator number"); }
      sign = -1;
      }
    gen = read_unsigned();
    if (gen == 0 || ngennum < gen) 
      { tc_error("<number> must be one of the generator numbers"); }
    add_gen_to_curr_word(beg, sign*gen);
    len = 1;
    }
  else if (Ch == '(' || Ch == '[') 
    {				/* parse parenthesised word / commutator */
    ch = Ch;
    next_nonwhite_input();
    len = parse_word(beg);
    while (Ch == ',') 
      {
      next_nonwhite_input();
      len2 = parse_word(beg+len);
      add_word_to_curr_word(beg+len+len2, beg, len+len2);
      invert_subword_of_curr_word(beg, len);
      invert_subword_of_curr_word(beg+len, len2);
      len = 2*(len + len2);
      }
    if (ch == '(' && Ch != ')') 
      { tc_error("'(' must have a matching ')'"); }
    if (ch == '[' && Ch != ']') 
      { tc_error("'[' must have a matching ']'"); }
    next_nonwhite_input();
    }
  else 				/* otherwise this is an error */
    { tc_error("<word> must begin with a <generator>, a '(' or a '['"); }

  /* allow |'| for inversion.  Multiple inversion allowed? */
  if (Ch == '\'') 
    { 
    invert_subword_of_curr_word(beg, len); 
    next_nonwhite_input();
    }

  return len; 			/* return the length */
  }

	/******************************************************************
	static int parse_factor(int beg)
	******************************************************************/

static int parse_factor(int beg)
  {
  int len, len2, pow, i;

  len = parse_element(beg); 		/* parse (first) element */

  /* parse an exponent */
  if (Ch == '^' ||
      (ngennum == 0 && (isdigit(Ch) || Ch == '+' || Ch == '-'))) 
    {
    if (Ch == '^')			/* strip away the '^' */
      { next_nonwhite_input(); }

    /* parse '-' [<integer>] or <integer> */
    if (isdigit(Ch) || Ch == '-' || Ch == '+') 
      {
      if (Ch == '+') 
        {
        next_nonwhite_input();
        if (ngennum != 0 && !isdigit(Ch)) 
          { tc_error("'+' must be followed by exponent number"); }
        }                
      else if (Ch == '-') 
        {
        invert_subword_of_curr_word(beg, len);
        next_nonwhite_input();
        if (ngennum != 0 && !isdigit(Ch)) 
          { tc_error("'-' must be followed by exponent number"); }
        }

      if (isdigit(Ch))			/* ? guarranteed a digit ! */ 
        {
        pow = read_unsigned();
        for (i = 2; i <= pow; i++) 
          { add_word_to_curr_word(beg + (i-1)*len, beg, len); }
        len = len*pow;
        }
      }
    /* parse <element> (i.e., a conjugation) */
    else if (isalpha(Ch) || Ch == '(' || Ch == '[') 
      {
      len2 = parse_element(beg+len);
      add_word_to_curr_word(beg+len+len2, beg+len, len2);
      invert_subword_of_curr_word(beg, len);
      invert_subword_of_curr_word(beg, len+len2);
      len = len2 + len + len2;
      }
    /* otherwise raise an error */
    else 
      { tc_error("'^' must be followed by exponent or element"); }
    }

  return len; 				/* return the length */
  }

	/******************************************************************
	static int parse_word(int beg)

	Parses a word into 'curr_word' starting at position <beg>.  Words 
	are defined by the following BNF:

	<word>    = <factor> { '*' | '/' <factor> }
	<factor>  = <element> [ '^' <integer> | '^' <element> ]
	<element> = <generator> [']
	          | '(' <word> { ',' <word> } ')' [']
	          | '[' <word> { ',' <word> } ']' [']

	The '*' can be dropped everywhere (but of course two numeric 
	generators or a numeric exponent and a numeric generator must be 
	separated by a whitespace).  If alphabetic generators are used the 
	exponentiation '^' can be dropped (but not the conjugation '^'), 
	and the exponent '-1' can be abbreviated to '-'.  So 'a^-1*b' can 
	be written as 'a^-1*b', 'a^-1b', 'a-1*b', 'a-1b', 'a^-*b', 'a^-b', 
	'a-*b', or 'a-b'.
	******************************************************************/

static int parse_word(int beg)
  {
  int len, len2;
  char ch;

  len = parse_factor(beg); 		/* parse the first factor */

  /* parse more factors */
  while (Ch == '*' || Ch == '/' || isalpha(Ch) || isdigit(Ch) 
         || Ch == '+' || Ch == '-' || Ch == '(' || Ch == '[') 
    {
    /* read the operator (if any) */
    if (Ch == '*') 
      { 
      ch = '*'; 
      next_nonwhite_input();
      }
    else if (Ch == '/') 
      { 
      ch = '/'; 
      next_nonwhite_input();
      }
    else                  
      { ch = '*'; }

    /* parse the next factor, if necessary invert it */
    len2 = parse_factor(beg+len);
    if (ch == '/') 
      { invert_subword_of_curr_word(beg+len, len2); }
    len += len2;
    }

  return len; 				/* return the length */
  }

	/******************************************************************
	static Wordlptr read_word(void)

	This parses a word into curr_word[], copies it into a new word, and
	return a pointer to it.  Use return new_word(parse_word(0)) ?
	******************************************************************/

static Wordlptr read_word(void)
  {
  int len;

  len = parse_word(0);
  return new_word(len);
  }

	/******************************************************************
	static void parse_add_word(Wordlhdr l)

	Parse a word and add it to the list of words.
	******************************************************************/

static void parse_add_word(Wordlhdr l)
  {
  int len;

  len = parse_word(0);
  add_word_to_word_list(l, new_word(len));
  }

	/******************************************************************
	static Wordlhdr read_word_list(void)

	Reads and returns a list of words.
	******************************************************************/

static Wordlhdr read_word_list(void)
  {
  Wordlhdr l;

  l = new_empty_word_list(); 	/* allocate a list of words */

  /* parse a sequence of words */
  if (Ch != ';') 
    {
    parse_add_word(l);
    while (Ch == ',') 
      { 
      next_nonwhite_input(); 
      parse_add_word(l); 
      }
    }

  return l; 			/* return the list of words */
  }

	/******************************************************************
	static void parse_add_relator(Wordlhdr l)

	W1 = W2 = W3 becomes W1W2' & W1W3'?
	******************************************************************/

static void parse_add_relator(Wordlhdr l)
  {
  int len1, len2;

  len1 = parse_word(0); 	/* parse left hand side word */

  /* parse a sequence of right-hand side words */
  len2 = 0;
  while (Ch == '=') 
    {
    next_nonwhite_input();
    len2 = parse_word(len1);
    invert_subword_of_curr_word(len1, len2);
    add_word_to_word_list(l, new_word(len1+len2));
    }

  /* if no right-hand side, take left-hand side as a relator */
  if (len2 == 0) 
    { add_word_to_word_list(l, new_word(len1)); }
  }

	/******************************************************************
	static Wordlhdr read_relator_list(void)

	Reads and returns a list of relators.
	******************************************************************/

static Wordlhdr read_relator_list(void)
  {
  Wordlhdr l = new_empty_word_list(); 	/* allocate new list of words */

  if (Ch != ';')			/* parse a sequence of relators */
    {
    parse_add_relator(l);
    while (Ch == ',') 
      {
      next_nonwhite_input();
      parse_add_relator(l);
      }
    }

  return l; 				/* return the list of words */
  }

	/******************************************************************
	static void end_of_command(void)

	To terminate a command, we must see a ';' or a newline.
	******************************************************************/

static void end_of_command(void)
  {
  if (Ch != ';' && Ch != '\n' && Ch != '\r' && Ch != EOF) 
    { tc_error("command must be terminated by ';' or <newline>"); }
  }

	/******************************************************************
	static void tc_delete_words(Intarray *d, Wordlhdr w)

	d specifies words to be deleted by position in the wordlist w, and
	is an array of indexes which must be strictly increasing and in 
	the range 1 ... w->length.
	******************************************************************/

static void tc_delete_words(Intarray *d, Wordlhdr w)
  { 
  int l, m, last, diff; 
  Wordlptr *prior, *nextp, p, pnode;

  for (last = 0, l = 0; l < d->icount; last = d->integers[l++]) 
    {
    if (last >= d->integers[l] || d->integers[l] > w->length)
      { tc_error("Invalid integer in delete sequence"); }
    }

  prior = &w->first; 
  pnode = NULL;

  for (last = 0, l = 0; l < d->icount; last = d->integers[l++]) 
    {
    diff = d->integers[l] - last; 
    nextp = &((*prior)->next); 
    for (m = 1; m < diff; m++) 
      { 
      pnode = *prior; 
      prior = nextp; 
      nextp = &((*prior)->next);
      }
    p = *prior; 
    *prior = (*prior)->next; 			/* remove the node */
    free(p->wordgen); 
    free(p);
    }

  if (last == w->length) 
    { w->last = pnode; }
  w->length -= d->icount;
  }

	/******************************************************************
	void prt_hlp(void)
	******************************************************************/

void prt_hlp(void)
  {
  printf("\n");
  printf("adapt[ive]: 0/1;\n");
  printf("ai / alter input: <filename>;\n");
  printf("ao / alter output: <filename>;\n");
  printf("asis: 0/1;\n");
  printf("bye / q[uit] / exit;\n");
  printf("cc / coset coincidence: <integer>;\n");
  printf("com[paction]: <percentage>;\n");
  printf("cont[inue] / restart;\n");
  printf("ct[ factor]: <integer>[,<integer>[,<integer>]];\n");
  printf("cy / print cycles;\n");
  printf("def[ault];\n");
  printf("dr / delete rel[ators] / delete group rel[ators]:"
         " <integer>[,<integer>[,<integer>]];\n");
  printf("ds / delete gen[erators] / delete subgroup gen[erators]:"
         " <integer>[,<integer>[,<integer>]];\n");
  printf("echo: 0/1;\n");
  printf("end / start[ enumeration];\n");
  printf("enum[eration] / group name: <group name>;\n");
  printf("fel[sch];\n");
  printf("fi[ll factor]: <integer>[,<integer>[,<integer>]];\n");
  printf("gen[erators] / subgroup gen[erators]: <word list>;\n");
  printf("gr[oup generators]: <generator list>;\n");
  printf("ha[rd];\n");
  printf("h[elp];\n");
  printf("hlt / look[ahead] / easy;\n");
  printf("max[ cosets]: <integer>[,<integer>[,<integer>]];\n");
  printf("mend[elsohn]: 0/1;\n");
  printf("mon[itor] / mess[ages]: [-]<integer>;\n");
  printf("nc / normal closure: <integer>;\n");
  printf("no[ of relators in subgroup] / relators in subgroup:"
         " -1/<integer>[,<integer>[,<integer>]];\n");
  printf("oo / order option: [-]<integer>;\n");
  printf("pr[int coset table]: [-]<integer>[,<integer>[,<integer>]];\n");
  printf("rel[ators] / group relators: <relator list>;\n");
  printf("rl / add rel[ators]: <relator list>;\n");
  printf("rt[ factor]: [-]<integer>[,<integer>[,<integer>]];\n");
  printf("sc / stabilizing cosets: [-]<integer>;\n");
  printf("sg / add gen[erators]: <word list>;\n");
  printf("sr / print details;\n");
  printf("subg[roup] / subgroup name: <subgroup name>;\n");
  printf("sys[tem]: <command>;\n");
  printf("tw / trace word: <integer>,<word>;\n");
  printf("wo[rkspace]: <integer>[k/K/m/M/g/G];\n");
  printf("# ... <newline>: a comment\n");
  printf("\n");
  }

	/******************************************************************
	void read_commands(void)

	Top-level loop of the interpreter.
	******************************************************************/

void read_commands(void)
  {
  int i,j;
  Intarray *iap;
  Wordlptr w;
  Wordlhdr wl;
  char *n;

  setjmp(again_input); 		/* test return value to see if first time
				or from tc_error. Print message? */
  alter_input("stdin"); 	/* start with the standard input */
  echo = FALSE; 		/* start in quiet (interactive) mode */

  while (TRUE) 			/* while there is input */
    {
    /* start reading the next command */
    next_nonwhite_input();
    skip_newlines_flag_input = TRUE;
    skip_whitespaces_input();
    skip_newlines_flag_input = FALSE;

    if (Ch == EOF)  
      { break; }		/* maybe we have reached end of file? */

    readkey(); 			/* read the keyword */

    /* basic enumeration arguments ... */

    if (matches("enum[eration]") || matches("group name")) 
      { 
      n = readname();
      free(grpname);		/* Prevent memory leakage */ 
      grpname = StrDup(n);
      }

    else if (matches("gr[oup generators]")) 
      {
      if (isdigit(Ch) || Ch == '+' || Ch == '-') 
        {
        read_numer_gens(); 
        end_of_command();
        if (ngennum <= 0) 
          { tc_error("Illegal number of generators"); }
        else 
          { ndgen = ngennum; }
        }
      else 
        { 
        read_alpha_gens(); 
        end_of_command(); 
        ndgen = ngenal;
        }
      }

    /* There could be a memory leakage here, since we throw away the old
    relator list! */

    else if (matches("rel[ators]") || matches("group relators")) 
      {
      skip_newlines_flag_input = TRUE; 
      skip_whitespaces_input();
      wl = read_relator_list(); 
      end_of_command();
      ndrel = wl->length; 
      relhdr = wl;
      }

    else if (matches("rl") || matches("add rel[ators]")) 
      {
      skip_newlines_flag_input = TRUE; 
      skip_whitespaces_input();
      wl = read_relator_list(); 
      end_of_command();
      newrelpar += wl->length; 
      asis = TRUE; 				/* Prevents reordering ! */
      concat_word_lists(&relhdr , wl);
      }

    else if (matches("dr") || matches("delete rel[ators]")
             || matches("delete group rel[ators]")) 
      {
      iap = read_Intarray(); 
      end_of_command();
      tc_delete_words(iap, relhdr);
      }

    else if (matches("subg[roup]") || matches("subgroup name")) 
      { 
      n = readname(); 
      free(subgrpname);			/* Prevent memory leakage */ 
      subgrpname = StrDup(n);
      }

    /* There could be a memory leakage here, since we throw away the old
    generator list! */

    else if (matches("gen[erators]") || matches("subgroup gen[erators]")) 
      {
      skip_newlines_flag_input = TRUE; 
      skip_whitespaces_input();
      wl = read_word_list(); 
      end_of_command();
      nsgpg = wl->length; 
      sghdr = wl;
      }

    else if (matches("sg") || matches("add gen[erators]")) 
      {
      skip_newlines_flag_input = TRUE; 
      skip_whitespaces_input();
      wl = read_word_list(); 
      end_of_command();
      newsgpar += wl->length; 
      asis = TRUE; 				/* Prevents reordering ! */
      concat_word_lists(&sghdr , wl);
      }

    else if (matches("ds") || matches("delete gen[erators]")
             || matches("delete subgroup gen[erators]")) 
      {
      iap = read_Intarray(); 
      end_of_command();
      tc_delete_words(iap, sghdr);
      }

    /* i) If (the first) rtfactor < 0, then (the first) ctfactor must be 
    positive, else we could loop doing nothing.  ii) At a minimum, the
    number of generators must be specified (free group). */

    else if (matches("end") || matches("start[ enumeration]")) 
      { 
      end_of_command();

      if (rtpar[0] < 0 && ctpar[0] < 1)
        { tc_error("rtfactor < 0 requires ctfactor > 0"); }
      if (ndgen < 1)
        { tc_error("no group generators specified"); }

      tc_start();
      }

    /* parameters for the enumeration ... */

    else if (matches("com[paction]")) 
      { 
      i = read_integer(); 
      end_of_command();
      if (!(i >= 0 && i <= 100)) 
        { tc_error("value must be in 0..100"); }
      comppc = i;
      }

    /* A value of (0,0,1) means a single loop with as many rows (cosets) 
    as possible, and (m0,m1,m2), with 1 <= m0 <= m1 & m2 >= 1, means loop
    from m0 to (at most) m1 incrementing by m2.  (The amount of memory 
    made available may mean that m1 can't be achieved.) */

    else if (matches("max[ cosets]")) 
      { 
      read_looppar(maxpar);
      if (!(maxpar[0] > 0 && maxpar[0] <= maxpar[1] && maxpar[2] > 0) &&
          !(maxpar[0] == 0 && maxpar[1] == 0 && maxpar[2] == 1))
        { tc_error("bad parameters"); }
      end_of_command();
      }

    /* A value of (m0,m1,m2), with 0 <= m0 <= m1 & m2 >= 1, means loop 
    from m0 to (at most) m1 incrementing by m2, using the value as the 
    ctfactor. */

    else if (matches("ct[ factor]")) 
      { 
      read_looppar(ctpar);
      if (!(ctpar[0] >= 0 && ctpar[0] <= ctpar[1] && ctpar[2] > 0))
        { tc_error("bad parameters"); }
      end_of_command();
      }

    /* A value of (m0,m1,m2), with m0 <= m1 & m2 >= 1, means loop from m0 
    to (at most) m1 incrementing by m2, using the value as the rtfactor. 
    (Note that -ve rtfactors mean do RT-style deinitions at most once.) */

    else if (matches("rt[ factor]")) 
      { 
      read_looppar(rtpar);  
      if (!(rtpar[0] <= rtpar[1] && rtpar[2] > 0))
        { tc_error("bad parameters"); }
      end_of_command();
      }

    /* A value of (0,0,1) means a single loop with the default fill factor,
    and (m0,m1,m2), with 1 <= m0 <= m1 & m2 >= 1, means loop from m0 to (at
    most) m1 incrementing by m2.  (A fill factor of 1 means that the pdl
    is not used.) */

    else if (matches("fi[ll factor]")) 
      { 
      read_looppar(fipar);  
      if (!(fipar[0] > 0 && fipar[0] <= fipar[1] && fipar[2] > 0) &&
          !(fipar[0] == 0 && fipar[1] == 0 && fipar[2] == 1))
        { tc_error("bad parameters"); }
      end_of_command();
      }

    /* A value of (-1,-1,1) means a single loop without using any of the
    relators as subgroup generators, and (m0,m1,m2), with 0 <= m0 <= m1 & 
    m2 >= 1, means loop from m0 to (at most) m1 incrementing by m2 using 
    that number of relators (in order). */

    else if (matches("no[ of relators in subgroup]") 
              || matches("relators in subgroup")) 
      { 
      read_looppar(rinsgpar);
      if (!(rinsgpar[0]>=0 && rinsgpar[0]<=rinsgpar[1] && rinsgpar[2]>0) &&
          !(rinsgpar[0] == -1 && rinsgpar[1] == -1 && rinsgpar[2] == 1))
        { tc_error("bad parameters"); }
      end_of_command();
      }

    else if (matches("mend[elsohn]")) 
      { 
      i = read_integer();
      end_of_command(); 
      if (!(i == TRUE || i == FALSE))
        { tc_error("flag variable, must be 0 or 1"); } 
      mendel = i; 
      }

    else if (matches("asis")) 
      { 
      i = read_integer();
      end_of_command(); 
      if (!(i == TRUE || i == FALSE))
        { tc_error("flag variable, must be 0 or 1"); } 
      asis = i;
      }

    else if (matches("adapt[ive]")) 
      { 
      i = read_integer();
      end_of_command();
      if (!(i == TRUE || i == FALSE))
        { tc_error("flag variable, must be 0 or 1"); } 
      adapt = i;
      }

    else if (matches("def[ault]")) 
      {
      end_of_command(); 
      SET3(ctpar, 0, 0, 1);
      SET3(rtpar, 0, 0, 1);
      SET3(fipar, 0, 0, 1);
      SET3(rinsgpar, 0, 0, 1);
      mendel = FALSE;
      asis = FALSE; 
      adapt = FALSE; 
      }

    else if (matches("ha[rd]")) 
      {
      end_of_command(); 
      SET3(ctpar, 1000, 1000, 1);
      SET3(rtpar, 1, 1, 1);
      SET3(fipar, 0, 0, 1);
      SET3(rinsgpar, 0, 0, 1);
      mendel = FALSE;
      asis = FALSE; 
      adapt = FALSE; 
      }

    else if (matches("fel[sch]")) 
      { 
      end_of_command(); 
      SET3(ctpar, 1000, 1000, 1);
      SET3(rtpar, 0, 0, 1);
      SET3(fipar, 1, 1, 1);
      SET3(rinsgpar, -1, -1, 1);
      mendel = FALSE; 
      asis = TRUE; 
      adapt = FALSE; 
      }

    else if (matches("hlt") || matches("look[ahead]") || matches("easy")) 
      {
      end_of_command(); 
      SET3(ctpar, 0, 0, 1);
      SET3(rtpar, 1000, 1000, 1);
      SET3(fipar, 1, 1, 1);
      SET3(rinsgpar, -1, -1, 1);
      mendel = FALSE; 
      asis = TRUE; 
      adapt = FALSE; 
      }

    else if (matches("wo[rkspace]"))
      {
      i = read_integer(); 
      j = read_mult();			/* 1 or K or M or G */
      end_of_command();

      if (i <= 0)			/* Change `bad' value to default */
        { 
        i = DEFWORK;
        j = 1;
        }
      else if (j == 1 && i < KILO)	/* Minimum allowed is 1xKILO */
        { i = KILO; }

      workspace = i;
      workmult = j;

      /* Note the casts to long for use on theIP27/R10000 system.  Compile
      with -64 allows the 8G physical barrier to be broken. */

      if (cosettable != NULL) 
        { free(cosettable); }
      cosettable = 
        (int *)malloc((long)workspace*(long)workmult*(long)sizeof(int));
      if (cosettable == NULL) 
        { 
        fprintf (fop, "\nUnable to resize workspace\n"); 
        exit(-1);
        }
      }

    else if (matches("mon[itor]") || matches("mess[ages]")) 
      { 
      i = read_integer(); 
      end_of_command(); 
      msgctrl = i;
      }

    /* Continue is misnamed - it's a restart! */

    else if (matches("cont[inue]") || matches("restart")) 
      { 
      end_of_command(); 
      restart = TRUE;
      }

    /* post enumeration processing ... */

    /* A value of (0,0,1) means print all of the coset table, and 
    (m0,m1,m2), with 1 <= abs(m0) <= m1 & m2 >= 1, means print rows
    abs(m0) to m1, incrementing by m2.  m0 < 0 means include coset reps. */

    else if (matches("pr[int coset table]")) 
      { 
      read_looppar(prpar);
      end_of_command(); 
      if (!(abs(prpar[0])>0 && abs(prpar[0])<=prpar[1] && prpar[2]>0) &&
          !(prpar[0] == 0 && prpar[1] == 0 && prpar[2] == 1))
        { tc_error("bad print parameters"); }
      tc_print_ct(prpar[0], prpar[1], prpar[2]); 
      }

    else if (matches("sr") || matches("print details")) 
      {
      end_of_command();
      i = msgctrl; 
      msgctrl = 1;		/* force basic details only */
      printdetails(); 
      if (ctflg)		/* `further' details if CT is present */ 
        { tc_text(11); }
      msgctrl = i;    
      }

    else if (matches("cy") || matches("print cycles")) 
      {
      end_of_command();
      if (!ctflg) 
        { tc_error("No coset table for CY"); }
      else 
        { tc_cycles(); }		/* !! check for index also ?? */
      }

    else if (matches("cc") || matches("coset coincidence")) 
      {
      i = read_integer(); 
      end_of_command(); 
	
      /* print the coset representative of coset i & add it to the 
      subgroup and do the resulting enumeration */

      if (!ctflg) 
        { tc_error("No coset table for CC"); }
      if (i > nalive || i < 2) 
        { tc_error("Invalid coset for CC"); }
      tc_print_ct(-i, i, 1); 
      tc_add_cc(i);

      if (!indexc) 
        { 
        asis = TRUE; 
        restart = TRUE; 
        tc_start(); 
        }
      }

    else if (matches("sc") || matches("stabilizing cosets")) 
      {
      i = read_integer(); 
      end_of_command();
      if (!ctflg) 
        { tc_error("No coset table for SC"); }
      else 
        { tc_sc(i); }
      }

    else if (matches("oo") || matches("order option")) 
      { 
      i = read_integer(); 
      end_of_command(); 
      tc_o(i);
      }

    else if (matches("nc") || matches("normal closure")) 
      {
      if (isdigit(Ch) || Ch == '+' || Ch == '-') 
        { i = read_integer(); }
      else 
        { i = 0; }
      end_of_command(); 
      if (!(i == TRUE || i == FALSE))
        { tc_error("flag variable, must be 0 or 1"); } 
      tc_normcl(i);
      }

    else if (matches("tw") || matches("trace word")) 
      {
      skip_newlines_flag_input = TRUE; 
      skip_whitespaces_input();
      i = read_integer();
      if (Ch != ',') 
        { tc_error("number must be followed by ','"); }
      next_nonwhite_input(); 
      w = read_word(); 
      end_of_command();
 
      if (!ctflg) 
        { tc_error("No coset table for TW"); }
      if (i > nalive || i < 1) 
        { tc_error("Invalid coset for TW"); } 

      /* Trace word w through the coset table, starting at coset i. 
      First, translate gens into columns of the coset table. */

      for (j = 1; j <= w->len; j++) 
        { w->wordgen[j] = gencol[w->wordgen[j]]; }
      fprintf (fop, "%d * word = %d\n", i,
               tc_tracew(i, &w->wordgen[1], &w->wordgen[w->len]));
      free(w->wordgen); 
      free(w);
      }

    /* miscellaneous ... */

    else if (matches("h[elp]"))
      { prt_hlp(); }

    else if (matches("ai") || matches("alter input")) 
      { 
      n = readname(); 
      alter_input(n);
      }

    else if (matches("sys[tem]")) 
      { 
      n = readname(); 
      system(n);
      }

    else if (matches("ao") || matches("alter output")) 
      { 
      n = readname(); 
      alter_output(n);
      }

    else if (matches("echo")) 
      { 
      i = read_integer();
      end_of_command(); 
      if (!(i == TRUE || i == FALSE))
        { tc_error("flag variable, must be 0 or 1"); } 
      echo = i;
      }

    else if (matches("bye") || matches ("q[uit]") || matches ("exit")) 
      { 
      end_of_command();
      break;
      }

    /* otherwise signal an error ... */

    else 
      { tc_error("There is no such keyword"); }
    }
  }

