############################################################################
# Makefile for arithmetic encode/decode library and sample program
#
#   $Id: Makefile,v 1.2 2007/09/08 15:48:14 michael Exp $
#   $Log: Makefile,v $
#   Revision 1.2  2007/09/08 15:48:14  michael
#   Replace getopt with optlist.
#   Changes required for LGPL v3.
#
#   Revision 1.1.1.1  2004/04/04 14:54:13  michael
#   Initial version
#
#
############################################################################
CC = gcc
LD = gcc
CFLAGS = -O2 -Wall -ansi -c
LDFLAGS = -O2 -o

# libraries
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
	DEL = rm
endif

all:		sample$(EXE)

sample$(EXE):	sample.o libarcode.a liboptlist.a
		$(LD) $< $(LIBS) $(LDFLAGS) $@

sample.o:	sample.c bitfile.h arcode.h optlist.h
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
