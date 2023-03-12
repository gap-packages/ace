
:: ...\src\make.cmd, 17 Aug 14
::
:: This is a 'make' file to be run under Window's cmd.exe command processor (see
:: the make.linux/make.cygwin files for more details).  Set up GCC to point to
:: your favourite version of GNU's gcc, and choose which version of FLAGS you
:: want.  Note that this builds (or tries to build) all the executables at once,
:: and dumps some gcc info for you.

:: set GCC=C:\Users\cram\Downloads\x86_64-w64-mingw32-gcc-4.7.2-release-win64_rubenvb\mingw64\bin\gcc
:: set GCC=C:\Users\cram\Downloads\mingw-w64-bin-x86_64-20130104\mingw64\bin\gcc
:: set GCC=C:\Users\cram\Downloads\mingw-w64-bin-x86_64-20130324\mingw64\bin\gcc
   set GCC=C:\mingw64\x86_64-w64-mingw32-gcc-4.7.4-release-win64_rubenvb\mingw64\bin\x86_64-w64-mingw32-gcc-4.7.3.exe

set OPTS=-std=c99 -pedantic -Wall

:: set FLAGS=-O2 -DB8S8C8 -DCLK64
:: set FLAGS=-O2 -DB8S4C8 -DCLK64
:: set FLAGS=-O2 -DB8S4C5 -DCLK64
   set FLAGS=-O2 -DB8S4C4 -DCLK64
:: set FLAGS=-O2 -DB8S4C5 -DCLK32
:: set FLAGS=-O2 -DB4S4C4 -DCLK32
:: set FLAGS=-O2 -DB4S4C3 -DCLK32

set SRC2=ace.c cmdloop.c parser.c postproc.c util2.c
set SRC1=control.c util1.c
set SRC0=coinc.c enum.c util0.c

%GCC% --version
%GCC% -dumpmachine

%GCC% %OPTS% %FLAGS% -o ace %SRC2% %SRC1% %SRC0%

%GCC% %OPTS% %FLAGS% -o test0a test0a.c %SRC0%
%GCC% %OPTS% %FLAGS% -o ex0a ex0a.c %SRC0%
%GCC% %OPTS% %FLAGS% -o ex0d ex0d.c %SRC0%

%GCC% %OPTS% %FLAGS% -o ex1a ex1a.c %SRC1% %SRC0%
%GCC% %OPTS% %FLAGS% -o ex1d ex1d.c %SRC1% %SRC0%

%GCC% %OPTS% -o test3 test3.c
%GCC% %OPTS% -o test4 test4.c
%GCC% %OPTS% -o test5 test5.c
%GCC% %OPTS% -o test7 test7.c

