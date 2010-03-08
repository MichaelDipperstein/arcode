############################################################################
# Makefile for arithmetic encode/decode library and sample program
#
#   $Id: Makefile,v 1.1.1.1 2004/04/04 14:54:13 michael Exp $
#   $Log: Makefile,v $
#   Revision 1.1.1.1  2004/04/04 14:54:13  michael
#   Initial version
#
#
############################################################################
CC = gcc
LD = gcc
CFLAGS = -O2 -Wall -ansi -c
LDFLAGS = -O2 -o

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

sample$(EXE):	sample.o arcode.o bitfile.o getopt.o
		$(LD) $^ $(LDFLAGS) $@

sample.o:	sample.c bitfile.h arcode.h getopt.h
		$(CC) $(CFLAGS) $<

arcode.o:	arcode.c arcode.h bitfile.h
		$(CC) $(CFLAGS) $<

getopt.o:	getopt.c getopt.h
		$(CC) $(CFLAGS) $<

bitfile.o:      bitfile.c bitfile.h
		$(CC) $(CFLAGS) $<

clean:
		$(DEL) *.o
		$(DEL) sample$(EXE)
