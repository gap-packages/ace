# This is a copy of `make.flute'.
# ACE version 2, on flute (sun4m/SPARC) & using gcc.
# Also works for darter (PC/i586).

CC = gcc -Wall -pedantic -ansi

FLAGS = -D__UNIX

SRC = aceutils.c coinc.c control.c enum.c parser.c postproc.c

ace: $(SRC) tc.h
	$(CC) $(FLAGS) -o ace $(SRC)

aceG: $(SRC) tc.h
	gcc -g -o aceG $(SRC)

aceT: $(SRC) tc.h
	$(CC) $(FLAGS) -D__DI -o aceT $(SRC)

aceO: $(SRC) tc.h
	$(CC) $(FLAGS) -O2 -o aceO $(SRC)
