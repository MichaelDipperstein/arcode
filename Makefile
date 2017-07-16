############################################################################
# Makefile for arithmetic encode/decode library and sample program
# arguements:
#	No argument		Build everything
#	DEBUG=1			Build with debugging output and symbols
#	clean			Delete all compiled/linked output
#
############################################################################
CC = gcc
LD = gcc
CFLAGS = -Wall -Wextra -ansi -pedantic -c
LDFLAGS = -O2 -o

# Libraries
LIBS = -L. -Lbitfile -Loptlist -larcode -lbitfile -loptlist

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
	CFLAGS += -O2 -DNDEBUG
else
	CFLAGS += -g
endif

all:		sample$(EXE)

sample$(EXE):	sample.o libarcode.a optlist/liboptlist.a bitfile/libbitfile.a
		$(LD) $< $(LIBS) $(LDFLAGS) $@

sample.o:	sample.c arcode.h optlist/optlist.h
		$(CC) $(CFLAGS) $<

libarcode.a:	arcode.o
		ar crv libarcode.a arcode.o
		ranlib libarcode.a

arcode.o:	arcode.c arcode.h bitfile/bitfile.h
		$(CC) $(CFLAGS) $<

bitfile/libbitfile.a:
		cd bitfile && $(MAKE) libbitfile.a

optlist/liboptlist.a:
		cd optlist && $(MAKE) liboptlist.a

clean:
		$(DEL) *.o
		$(DEL) *.a
		$(DEL) sample$(EXE)
		cd optlist && $(MAKE) clean
		cd bitfile && $(MAKE) clean
