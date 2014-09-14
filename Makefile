############################################################################
# Makefile for arithmetic encode/decode library and sample program
# arguements:
#	No argument			Build everythin
#	DEBUG=1				Build with debugging output and symbols
#	clean				Delete all compiled/linked output
#
############################################################################
CC = gcc
LD = gcc
CFLAGS = -O2 -Wall -Wextra -ansi -pedantic -c
LDFLAGS = -O2 -o

# Libraries
LIBS = -L. -larcode -loptlist

# Treat NT and non-NT windows the same
ifeq ($(OS),Windows_NT)
	OS = Windows
endif

ifeq ($(OS),Windows)
	EXE = .exe
	DEL = del
else	#assume Linux/Unix
	EXE =
	DEL = rm -f
endif

# Handle debug/no debug
ifneq ($(DEBUG), 1)
	CFLAGS += -DNDEBUG
else
	CFLAGS += -g
endif

all:		sample$(EXE)

sample$(EXE):	sample.o libarcode.a liboptlist.a
		$(LD) $< $(LIBS) $(LDFLAGS) $@

sample.o:	sample.c arcode.h optlist.h
		$(CC) $(CFLAGS) $<

libarcode.a:	arcode.o bitfile.o
		ar crv libarcode.a arcode.o bitfile.o
		ranlib libarcode.a

arcode.o:	arcode.c arcode.h bitfile.h
		$(CC) $(CFLAGS) $<

bitfile.o:	bitfile.c bitfile.h
		$(CC) $(CFLAGS) $<

liboptlist.a:	optlist.o
		ar crv liboptlist.a optlist.o
		ranlib liboptlist.a

optlist.o:	optlist.c optlist.h
		$(CC) $(CFLAGS) $<

clean:
		$(DEL) *.o
		$(DEL) *.a
		$(DEL) sample$(EXE)
