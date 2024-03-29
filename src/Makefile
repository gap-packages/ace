# Makefile.in for ACE version 3.001 (GAP interface version)
# 
# With most UNIX operating systems set CC to gcc e.g. for Linux, SunOS/Solaris.
# For SGIs however, usually one needs the `native' cc.
########################################################################
# Generic with gcc.
# Tested on:
#   "boronia"   Linux 2.2.6, gcc 2.7.2.3 & egcs-2.91.66 19990314, make 3.77
#   "hamal"     Linux 2.2.14, gcc 2.95.2, make 3.78.1
#   "darter"	PC with Pentium 75+, Linux 2.0.21, gcc 2.7.2.1, 
#		  make 3.76.1, bash 1.14.5(1)
#   "flute"	sun4m/SPARC, SunOS 5.5, gcc 2.95.2, SunOS make & ksh
#   "mango"     SunOS 5.8, gcc 2.95.2, SunOS make
#
# Compiler switches
#
# -Wall     : print all warnings
# -pedantic : be bloody picky
# -O2/3     : optimise quite a bit / a lot
# -g        : for xxgdb debugger
# -pg       : profiling info for gprof
########################################################################
CC ?= gcc
EXTRA_CFLAGS ?= -Wall -pedantic -O2

########################################################################
# SunOS 5.5 boxes with `native' cc.
# Tested on "flute" :-
# 
# hardware name         sun4m 
# processor type        SPARC 
# operating system      SunOS 5.5 (ie, Solaris 2.x)  
# shell                 ksh (ie, /bin/ksh)
# compiler              cc (ie, /opt/SUNWspro/SC3.0.1/bin/cc)
# make                  SunOS (ie, /usr/ccs/bin/make)
#
# Compiler switches/flags (potentially)
#
# -xO1/2/3/4 : various optimisation levels
# -g         : for dbx(1)/debugger(1) debugger
# -p         : profiling info for prof
# -v         : more/stricter semantic checks
# -Xa        : ANSI C plus Sun C compatability
########################################################################
#CC = cc -xO2 -v -Xa

########################################################################
# SGI/IRIX boxes with cc; if 64bit is supported use cc -64.  
# Tested on:
#   "ozone"	Origin 2000 with R10000 (mips), IRIX64 6.5, 
#		  cc (MIPS / MIPSpro C compiler)
# Compiler switches
#
# -O2/3     : optimise quite a bit / a lot
# -fullwarn : extra checks & additional warnings
# -pedantic : warns of (non-standard) #ident
# -ansi     : pure ANSI/ISO C
# -64       : new style 64-bit mode
# -n32      : new style 32-bit mode
# -o32      : old style 32-bit mode
########################################################################
#CC = cc -O2 -ansi
#CC = cc -64 -O2 -ansi

# ACE Level 0, 1 & 2 switches 
#
# AL0_STAT   : If defined, include the statistics package.  Usually 
#            : defined.
# AL0_CC     : If defined, include "CC:" / "DD:" messages in the message
# AL0_DD     :   feature.  Usually both on.
# AL1_BINARY : If defined, use 1024 etc instead of 1000 etc for the
#                workspace multipliers.  Usually not defined.
# AL2_HINFO  : Prints host name etc at start of run.  Usually defined.

FLAGS  = -DAL2_HINFO -DAL0_STAT -DAL0_CC -DAL0_DD

# Sources

SRC2  = ace.c parser.c postproc.c util2.c
SRC1  = control.c util1.c
SRC0  = coinc.c enum.c util0.c
SRC0A = enum00.c enum01.c enum02.c

# Level 2 (interactive interface)

ace: al2.h $(SRC2) al1.h $(SRC1) al0.h $(SRC0) $(SRC0A)
	$(CC) $(FLAGS) $(EXTRA_CFLAGS) $(CFLAGS) -o ace \
		$(SRC2) $(SRC1) $(SRC0) $(LDFLAGS)
