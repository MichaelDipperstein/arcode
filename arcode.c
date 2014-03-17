/***************************************************************************
*                 Arithmetic Encoding and Decoding Library
*
*   File    : arcode.c
*   Purpose : Use arithmetic coding to compress/decompress file streams
*   Author  : Michael Dipperstein
*   Date    : April 2, 2004
*
****************************************************************************
*   UPDATES
*
*   $Id: arcode.c,v 1.5 2007/09/08 15:47:02 michael Exp $
*   $Log: arcode.c,v $
*   Revision 1.5  2007/09/08 15:47:02  michael
*   Changes required for LGPL v3.
*
*   Revision 1.4  2006/03/02 06:43:37  michael
*   Expanded tabs
*
*   Revision 1.3  2006/01/12 07:39:24  michael
*   Use BitFileGetBitsIntBit and FilePutBitsInt for reading and writing the
*   header.  This makes the code a little cleaner, but the new header is not
*   compatible with the old header.
*
*   Revision 1.2  2004/08/13 13:10:27  michael
*   Add support for adaptive encoding
*
*   Use binary search when trying to find decoded symbol
*
*   Revision 1.1.1.1  2004/04/04 14:54:13  michael
*   Initial version
*
****************************************************************************
*
* Arcode: An ANSI C Arithmetic Encoding/Decoding Routines
* Copyright (C) 2004, 2006-2007 by Michael Dipperstein (mdipper@cs.ucsb.edu)
*
* This file is part of the arcode library.
*
* The arcode library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public License as
* published by the Free Software Foundation; either version 3 of the
* License, or (at your option) any later version.
*
* The arcode library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
* General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public License
* along with this program.  If not, see <http://www.gnu.org/licenses/>.
*
***************************************************************************/

/***************************************************************************
*                             INCLUDED FILES
***************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <assert.h>
#include "arcode.h"
#include "bitfile.h"

#ifdef NDEBUG
#define PrintDebug(ARGS) do {} while (0)
#else
#define PrintDebug(ARGS) printf ARGS
#endif

#if !(USHRT_MAX < ULONG_MAX)
#error "Implementation requires USHRT_MAX < ULONG_MAX"
#endif

/***************************************************************************
*                            TYPE DEFINITIONS
***************************************************************************/
#define EOF_CHAR    (UCHAR_MAX + 1)

typedef unsigned short probability_t;       /* probability count type */

typedef struct
{
    /* probability ranges for each symbol: [ranges[LOWER(c)], ranges[UPPER(c)]) */
    probability_t ranges[EOF_CHAR + 2];
    probability_t cumulativeProb;           /* sum for all ranges */
} ranges_t;


/***************************************************************************
*                                CONSTANTS
***************************************************************************/
/* number of bits used to compute running code values */
#define PRECISION           (8 * sizeof(probability_t))

/* 2 bits less than precision. keeps lower and upper bounds from crossing. */
#define MAX_PROBABILITY     (1 << (PRECISION - 2))

/***************************************************************************
*                                  MACROS
***************************************************************************/
/* set bit x to 1 in probability_t.  Bit 0 is MSB */
#define MASK_BIT(x) (probability_t)(1 << (PRECISION - (1 + (x))))

/* indices for a symbol's lower and upper cumulative probability ranges */
#define LOWER(c)    (c)
#define UPPER(c)    ((c) + 1)

/***************************************************************************
*                            GLOBAL VARIABLES
***************************************************************************/
probability_t lower;          /* lower bound of current code range */
probability_t upper;          /* upper bound of current code range */

probability_t code;           /* current MSBs of encode input stream */

unsigned char underflowBits;      /* current underflow bit count */

/***************************************************************************
*                               PROTOTYPES
***************************************************************************/
/* read write file headers */
void WriteHeader(bit_file_t *bfpOut, ranges_t *ranges);
int ReadHeader(bit_file_t *bfpIn, ranges_t *ranges);

/* applies symbol's ranges to current upper and lower range bounds */
void ApplySymbolRange(int symbol, ranges_t *ranges, char staticModel);

/* routines for encoding*/
void WriteEncodedBits(bit_file_t *bfpOut);
void WriteRemaining(bit_file_t *bfpOut);
int BuildProbabilityRangeList(FILE *fpIn, ranges_t *ranges);
void InitializeAdaptiveProbabilityRangeList(ranges_t *ranges);

/* routines for decoding */
void InitializeDecoder(bit_file_t *bfpOut);
probability_t GetUnscaledCode(ranges_t *ranges);
int GetSymbolFromProbability(probability_t probability, ranges_t *Sranges);
void ReadEncodedBits(bit_file_t *bfpIn);

/***************************************************************************
*                                FUNCTIONS
***************************************************************************/

/***************************************************************************
*   Function   : ArEncodeFile
*   Description: This routine generates a list of arithmetic code ranges for
*                a file and then uses them to write out an encoded version
*                of that file.
*   Parameters : inFile - FILE stream to encode
*                outFile - FILE stream to write encoded output to
*                staticModel - TRUE if encoding with a static model
*   Effects    : File is arithmetically encoded
*   Returned   : TRUE for success, otherwise FALSE.
***************************************************************************/
int ArEncodeFile(FILE *inFile, FILE *outFile, char staticModel)
{
    int c;
    bit_file_t *bOutFile;               /* encoded output */
    ranges_t ranges;                    /* probability ranges for all symbols */

    /* open input and output files */
    if (NULL == inFile)
    {
        inFile = stdin;
    }

    if (outFile == NULL)
    {
        bOutFile = MakeBitFile(stdout, BF_WRITE);
    }
    else
    {
        bOutFile = MakeBitFile(outFile, BF_WRITE);
    }

    if (NULL == bOutFile)
    {
        fprintf(stderr, "Error: Creating binary output file\n");
        return FALSE;
    }

    if (staticModel)
    {
        /* count symbols in file and come up with a list of probability ranges */
        if (!BuildProbabilityRangeList(inFile, &ranges))
        {
            fclose(inFile);
            BitFileClose(bOutFile);
            fprintf(stderr, "Error determining frequency ranges.\n");
            return FALSE;
        }

        rewind(inFile);

        /* write information required to decode file to encoded file */
        WriteHeader(bOutFile, &ranges);
    }
    else
    {
        /* initialize probability ranges assuming uniform distribution */
        InitializeAdaptiveProbabilityRangeList(&ranges);
    }

    /* initialize coder start with full probability range [0%, 100%) */
    lower = 0;
    upper = ~0;                     /* all ones */
    underflowBits = 0;

    /* encode symbols one at a time */
    while ((c = fgetc(inFile)) != EOF)
    {
        ApplySymbolRange(c, &ranges, staticModel);
        WriteEncodedBits(bOutFile);
    }

    ApplySymbolRange(EOF_CHAR, &ranges, staticModel);   /* encode an EOF */
    WriteEncodedBits(bOutFile);

    WriteRemaining(bOutFile);         /* write out least significant bits */
    outFile = BitFileToFILE(bOutFile);          /* make file normal again */

    return TRUE;
}

/***************************************************************************
*   Function   : SymbolCountToProbabilityRanges
*   Description: This routine converts the ranges array containing only
*                symbol counts to an array containing the upper and lower
*                probability ranges for each symbol.
*   Parameters : ranges - pointer to structure containing ranges
*   Effects    : ranges struct containing symbol counts in the upper field
*                for each symbol is converted to a list of upper and lower
*                probability bounds for each symbol.
*   Returned   : None
***************************************************************************/
void SymbolCountToProbabilityRanges(ranges_t *ranges)
{
    int c;

    ranges->ranges[0] = 0;                  /* absolute lower bound is 0 */
    ranges->ranges[UPPER(EOF_CHAR)] = 1;    /* add 1 EOF character */
    ranges->cumulativeProb++;

    /* assign upper and lower probability ranges */
    for (c = 1; c <= UPPER(EOF_CHAR); c++)
    {
        ranges->ranges[c] += ranges->ranges[c - 1];
    }

    /* dump list of ranges */
    PrintDebug(("Ranges:\n"));
    for (c = 0; c < UPPER(EOF_CHAR); c++)
    {
        PrintDebug(("%02X\t%d\t%d\n", c, ranges->ranges[LOWER(c)],
            ranges->ranges[UPPER(c)]));
    }

    return;
}

/***************************************************************************
*   Function   : BuildProbabilityRangeList
*   Description: This routine reads the input file and builds the global
*                list of upper and lower probability ranges for each
*                symbol.
*   Parameters : fpIn - file to build range list for
*                ranges - struct with upper and lower probability ranges
*   Effects    : ranges struct is made to contain probability ranges for
*                each symbol.
*   Returned   : TRUE for success, otherwise FALSE.
***************************************************************************/
int BuildProbabilityRangeList(FILE *fpIn, ranges_t *ranges)
{
    int c;

    /***********************************************************************
    * unsigned long is used to hold the largest counts we can have without
    * any rescaling.  Rescaling may take place before probability ranges
    * are computed.
    ***********************************************************************/
    unsigned long countArray[EOF_CHAR];
    unsigned long totalCount = 0;
    unsigned long rescaleValue;

    if (fpIn == NULL)
    {
        return FALSE;
    }

    /* start with no symbols counted */
    for (c = 0; c < EOF_CHAR; c++)
    {
        countArray[c] = 0;
    }

    while ((c = fgetc(fpIn)) != EOF)
    {
        if (totalCount == ULONG_MAX)
        {
            fprintf(stderr, "Error: file too large\n");
            return FALSE;
        }

        countArray[c]++;
        totalCount++;
    }

    /* rescale counts to be < MAX_PROBABILITY */
    if (totalCount >= MAX_PROBABILITY)
    {
        rescaleValue = (totalCount / MAX_PROBABILITY) + 1;

        for (c = 0; c < EOF_CHAR; c++)
        {
            if (countArray[c] > rescaleValue)
            {
                countArray[c] /= rescaleValue;
            }
            else if (countArray[c] != 0)
            {
                countArray[c] = 1;
            }
        }
    }

    /* copy scaled symbol counts to range list */
    ranges->ranges[0] = 0;
    ranges->cumulativeProb = 0;
    for (c = 0; c < EOF_CHAR; c++)
    {
        ranges->ranges[UPPER(c)] = countArray[c];
        ranges->cumulativeProb += countArray[c];
    }

    /* convert counts to a range of probabilities */
    SymbolCountToProbabilityRanges(ranges);
    return TRUE;
}

/***************************************************************************
*   Function   : WriteHeader
*   Description: This function writes each symbol contained in the encoded
*                file as well as its rescaled number of occurrences.  A
*                decoding algorithm may use these numbers to reconstruct
*                the probability range list used to encode the file.
*   Parameters : bfpOut - pointer to open binary file to write to.
*                ranges - structure containing symbol range counts
*   Effects    : Symbol values and symbol counts are written to a file.
*   Returned   : None
***************************************************************************/
void WriteHeader(bit_file_t *bfpOut, ranges_t *ranges)
{
    int c;
    probability_t previous = 0;         /* symbol count so far */

    PrintDebug(("HEADER:\n"));

    for(c = 0; c <= (EOF_CHAR - 1); c++)
    {
        if (ranges->ranges[UPPER(c)] > previous)
        {
            /* some of these symbols will be encoded */
            BitFilePutChar((char)c, bfpOut);
            previous = (ranges->ranges[UPPER(c)] - previous); /* symbol count */
            PrintDebug(("%02X\t%d\n", c, previous));

            /* write out PRECISION - 2 bit count */
            BitFilePutBitsInt(bfpOut, &previous, (PRECISION - 2),
                sizeof(probability_t));

            /* current upper range is previous for the next character */
            previous = ranges->ranges[UPPER(c)];
        }
    }

    /* now write end of table (zero count) */
    BitFilePutChar(0x00, bfpOut);
    previous = 0;
    BitFilePutBits(bfpOut, (void *)&previous, PRECISION - 2);
}

/***************************************************************************
*   Function   : InitializeAdaptiveProbabilityRangeList
*   Description: This routine builds the initial global list of upper and
*                lower probability ranges for each symbol.  This routine
*                is used by both adaptive encoding and decoding.
*                Currently it provides a uniform symbol distribution.
*                Other distributions might be better suited for known data
*                types (such as English text).
*   Parameters : ranges - structure containing the ranges array
*   Effects    : ranges array is made to contain initial probability ranges
*                for each symbol.
*   Returned   : NONE
***************************************************************************/
void InitializeAdaptiveProbabilityRangeList(ranges_t *ranges)
{
    int c;

    ranges->ranges[0] = 0;      /* absolute lower range */

    /* assign upper and lower probability ranges assuming uniformity */
    for (c = 1; c <= UPPER(EOF_CHAR); c++)
    {
        ranges->ranges[c] = ranges->ranges[c - 1] + 1;
    }

    ranges->cumulativeProb = UPPER(EOF_CHAR);

    /* dump list of ranges */
    PrintDebug(("Ranges:\n"));
    for (c = 0; c < UPPER(EOF_CHAR); c++)
    {
        PrintDebug(("%02X\t%d\t%d\n", c, ranges->ranges[LOWER(c)],
            ranges->ranges[UPPER(c)]));
    }

    return;
}

/***************************************************************************
*   Function   : ApplySymbolRange
*   Description: This function is used for both encoding and decoding.  It
*                applies the range restrictions of a new symbol to the
*                current upper and lower range bounds of an encoded stream.
*                If an adaptive model is being used, the probability range
*                list will be updated after the effect of the symbol is
*                applied.
*   Parameters : symbol - The symbol to be added to the current code range
*                ranges - structure containing upper and lower symbol ranges
*                staticModel - TRUE if encoding/decoding with a static
*                              model.
*   Effects    : The current upper and lower range bounds are adjusted to
*                include the range effects of adding another symbol to the
*                encoded stream.  If an adaptive model is being used, the
*                probability range list will be updated.
*   Returned   : None
***************************************************************************/
void ApplySymbolRange(int symbol, ranges_t *ranges, char staticModel)
{
    unsigned long range;        /* must be able to hold max upper + 1 */
    unsigned long rescaled;     /* range rescaled for range of new symbol */
                                /* must hold range * max upper */

    /* for updating dynamic models */
    int i;
    probability_t original;     /* range value prior to rescale */
    probability_t delta;        /* range for individual symbol */

    /***********************************************************************
    * Calculate new upper and lower ranges.  Since the new upper range is
    * dependant of the old lower range, compute the upper range first.
    ***********************************************************************/
    range = (unsigned long)(upper - lower) + 1;         /* current range */

    /* scale upper range of the symbol being coded */
    rescaled = (unsigned long)(ranges->ranges[UPPER(symbol)]) * range;
    rescaled /= (unsigned long)(ranges->cumulativeProb);

    /* new upper = old lower + rescaled new upper - 1*/
    upper = lower + (probability_t)rescaled - 1;

    /* scale lower range of the symbol being coded */
    rescaled = (unsigned long)(ranges->ranges[LOWER(symbol)]) * range;
    rescaled /= (unsigned long)(ranges->cumulativeProb);

    /* new lower = old lower + rescaled new upper */
    lower = lower + (probability_t)rescaled;

    if (!staticModel)
    {
        /* add new symbol to model */
        ranges->cumulativeProb++;

        for (i = UPPER(symbol); i <= UPPER(EOF_CHAR); i++)
        {
            ranges->ranges[i] += 1;
        }

        /* halve current values if cumulativeProb is too large */
        if (ranges->cumulativeProb >= MAX_PROBABILITY)
        {
            ranges->cumulativeProb = 0;
            original = 0;

            for (i = 1; i <= UPPER(EOF_CHAR); i++)
            {
                delta = ranges->ranges[i] - original;
                original = ranges->ranges[i];

                if (delta <= 2)
                {
                    /* prevent probability from being 0 */
                    ranges->ranges[i] = ranges->ranges[i - 1] + 1;
                }
                else
                {
                    ranges->ranges[i] = ranges->ranges[i - 1] + (delta / 2);
                }

                ranges->cumulativeProb +=
                    (ranges->ranges[i] - ranges->ranges[i - 1]);
            }
        }
    }

    assert(lower <= upper);
}

/***************************************************************************
*   Function   : WriteEncodedBits
*   Description: This function attempts to shift out as many code bits as
*                possible, writing the shifted bits to the encoded output
*                file.  Only bits that will be unchanged when additional
*                symbols are encoded may be written out.
*
*                If the n most significant bits of the lower and upper range
*                bounds match, they will not be changed when additional
*                symbols are encoded, so they may be shifted out.
*
*                Adjustments are also made to prevent possible underflows
*                that occur when the upper and lower ranges are so close
*                that encoding another symbol won't change their values.
*   Parameters : bfpOut - pointer to open binary file to write to.
*   Effects    : The upper and lower code bounds are adjusted so that they
*                only contain only bits that may be affected by the
*                addition of a new symbol to the encoded stream.
*   Returned   : None
***************************************************************************/
void WriteEncodedBits(bit_file_t *bfpOut)
{
    for (;;)
    {
        if ((upper & MASK_BIT(0)) == (lower & MASK_BIT(0)))
        {
            /* MSBs match, write them to output file */
            BitFilePutBit((upper & MASK_BIT(0)) != 0, bfpOut);

            /* we can write out underflow bits too */
            while (underflowBits > 0)
            {
                BitFilePutBit((upper & MASK_BIT(0)) == 0, bfpOut);
                underflowBits--;
            }
        }
        else if ((lower & MASK_BIT(1)) && !(upper & MASK_BIT(1)))
        {
            /***************************************************************
            * Possible underflow condition: neither MSBs nor second MSBs
            * match.  It must be the case that lower and upper have MSBs of
            * 01 and 10.  Remove 2nd MSB from lower and upper.
            ***************************************************************/
            underflowBits += 1;
            lower &= ~(MASK_BIT(0) | MASK_BIT(1));
            upper |= MASK_BIT(1);

            /***************************************************************
            * The shifts below make the rest of the bit removal work.  If
            * you don't believe me try it yourself.
            ***************************************************************/
        }
        else
        {
            /* nothing left to do */
            return ;
        }

        /*******************************************************************
        * Shift out old MSB and shift in new LSB.  Remember that lower has
        * all 0s beyond it's end and upper has all 1s beyond it's end.
        *******************************************************************/
        lower <<= 1;
        upper <<= 1;
        upper |= 1;
    }
}

/***************************************************************************
*   Function   : WriteRemaining
*   Description: This function writes out all remaining significant bits
*                in the upper and lower ranges and the underflow bits once
*                the last symbol has been encoded.
*   Parameters : bfpOut - pointer to open binary file to write to.
*   Effects    : Remaining significant range bits are written to the output
*                file.
*   Returned   : None
***************************************************************************/
void WriteRemaining(bit_file_t *bfpOut)
{
    BitFilePutBit((lower & MASK_BIT(1)) != 0, bfpOut);

    /* write out any unwritten underflow bits */
    for (underflowBits++; underflowBits > 0; underflowBits--)
    {
        BitFilePutBit((lower & MASK_BIT(1)) == 0, bfpOut);
    }
}

/***************************************************************************
*   Function   : ArDecodeFile
*   Description: This routine opens an arithmetically encoded file, reads
*                it's header, and builds a list of probability ranges which
*                it then uses to decode the rest of the file.
*   Parameters : inFile - FILE stream to decode
*                outFile - FILE stream to write decoded output to
*                staticModel - TRUE if decoding with a static model
*   Effects    : Encoded file is decoded
*   Returned   : TRUE for success, otherwise FALSE.
***************************************************************************/
int ArDecodeFile(FILE *inFile, FILE *outFile, char staticModel)
{
    int c;
    probability_t unscaled;
    bit_file_t *bInFile;
    ranges_t ranges;

    /* handle file pointers */
    if (NULL == outFile)
    {
        outFile = stdout;
    }

    if (NULL == inFile)
    {
        fprintf(stderr, "Error: Invalid input file\n");
        return FALSE;
    }

    bInFile = MakeBitFile(inFile, BF_READ);

    if (NULL == bInFile)
    {
        fprintf(stderr, "Error: Unable to create binary input file\n");
        return FALSE;
    }

    if (staticModel)
    {
        /* build probability ranges from header in file */
        if (ReadHeader(bInFile, &ranges) == FALSE)
        {
            BitFileClose(bInFile);
            fclose(outFile);
            return FALSE;
        }
    }
    else
    {
        /* initialize ranges for adaptive model */
        InitializeAdaptiveProbabilityRangeList(&ranges);
    }


    /* read start of code and initialize bounds, and adaptive ranges */
    InitializeDecoder(bInFile);

    /* decode one symbol at a time */
    for (;;)
    {
        /* get the unscaled probability of the current symbol */
        unscaled = GetUnscaledCode(&ranges);

        /* figure out which symbol has the above probability */
        if((c = GetSymbolFromProbability(unscaled, &ranges)) == -1)
        {
            /* error: unknown symbol */
            break;
        }

        if (c == EOF_CHAR)
        {
            /* no more symbols */
            break;
        }

        fputc((char)c, outFile);

        /* factor out symbol */
        ApplySymbolRange(c, &ranges, staticModel);
        ReadEncodedBits(bInFile);
    }

    inFile = BitFileToFILE(bInFile);        /* make file normal again */

    return TRUE;
}

/****************************************************************************
*   Function   : ReadHeader
*   Description: This function reads the header information stored by
*                WriteHeader.  The header can then be used to build a
*                probability range list matching the list that was used to
*                encode the file.
*   Parameters : bfpIn - file to read from
*                ranges - structure containing probability range list
*   Effects    : Probability range list is built.
*   Returned   : TRUE for success, otherwise FALSE
****************************************************************************/
int ReadHeader(bit_file_t *bfpIn, ranges_t *ranges)
{
    int c;
    probability_t count;

    PrintDebug(("HEADER:\n"));
    ranges->cumulativeProb = 0;

    for (c = 0; c <= UPPER(EOF_CHAR); c++)
    {
        ranges->ranges[UPPER(c)] = 0;
    }

    /* read [character, probability] sets */
    for (;;)
    {
        c = BitFileGetChar(bfpIn);
        count = 0;

        /* read (PRECISION - 2) bit count */
        if (BitFileGetBitsInt(bfpIn, &count, (PRECISION - 2),
            sizeof(probability_t)) == EOF)
        {
            /* premature EOF */
            fprintf(stderr, "Error: unexpected EOF\n");
            return FALSE;
        }

        PrintDebug(("%02X\t%d\n", c, count));

        if (count == 0)
        {
            /* 0 count means end of header */
            break;
        }

        ranges->ranges[UPPER(c)] = count;
        ranges->cumulativeProb += count;
    }

    /* convert counts to range list */
    SymbolCountToProbabilityRanges(ranges);
    return TRUE;
}

/****************************************************************************
*   Function   : InitializeDecoder
*   Description: This function starts the upper and lower ranges at their
*                max/min values and reads in the most significant encoded
*                bits.
*   Parameters : bfpIn - file to read from
*   Effects    : upper, lower, and code are initialized.  The probability
*                range list will also be initialized if an adaptive model
*                will be used.
*   Returned   : TRUE for success, otherwise FALSE
****************************************************************************/
void InitializeDecoder(bit_file_t *bfpIn)
{
    int i;

    code = 0;

    /* read PERCISION MSBs of code one bit at a time */
    for (i = 0; i < (int)PRECISION; i++)
    {
        code <<= 1;

        /* treat EOF like 0 */
        if(BitFileGetBit(bfpIn) == 1)
        {
            code |= 1;
        }
    }

    /* start with full probability range [0%, 100%) */
    lower = 0;
    upper = ~0;         /* all ones */
}

/****************************************************************************
*   Function   : GetUnscaledCode
*   Description: This function undoes the scaling that ApplySymbolRange
*                performed before bits were shifted out.  The value returned
*                is the probability of the encoded symbol.
*   Parameters : ranges - structure containing the cumulative probability
*   Effects    : None
*   Returned   : The probability of the current symbol
****************************************************************************/
probability_t GetUnscaledCode(ranges_t *ranges)
{
    unsigned long range;        /* must be able to hold max upper + 1 */
    unsigned long unscaled;

    range = (unsigned long)(upper - lower) + 1;

    /* reverse the scaling operations from ApplySymbolRange */
    unscaled = (unsigned long)(code - lower) + 1;
    unscaled = unscaled * (unsigned long)(ranges->cumulativeProb) - 1;
    unscaled /= range;

    return ((probability_t)unscaled);
}

/****************************************************************************
*   Function   : GetSymbolFromProbability
*   Description: Given a probability, this function will return the symbol
*                whose range includes that probability.  Symbol is found
*                binary search on probability ranges.
*   Parameters : probability - probability of symbol.
*                ranges - structure containing probability ranages
*   Effects    : None
*   Returned   : -1 for failure, otherwise encoded symbol
****************************************************************************/
int GetSymbolFromProbability(probability_t probability, ranges_t *ranges)
{
    int first, last, middle;    /* indicies for binary search */

    first = 0;
    last = UPPER(EOF_CHAR);
    middle = last / 2;

    /* binary search */
    while (last >= first)
    {
        if (probability < ranges->ranges[LOWER(middle)])
        {
            /* lower bound is higher than probability */
            last = middle - 1;
            middle = first + ((last - first) / 2);
            continue;
        }

        if (probability >= ranges->ranges[UPPER(middle)])
        {
            /* upper bound is lower than probability */
            first = middle + 1;
            middle = first + ((last - first) / 2);
            continue;
        }

        /* we must have found the right value */
        return middle;
    }

    /* error: none of the ranges include the probability */
    fprintf(stderr, "Unknown Symbol: %d (max: %d)\n", probability,
        ranges->ranges[UPPER(EOF_CHAR)]);
    return -1;
}

/***************************************************************************
*   Function   : ReadEncodedBits
*   Description: This function attempts to shift out as many code bits as
*                possible, as bits are shifted out the coded input is
*                populated with bits from the encoded file.  Only bits
*                that will be unchanged when additional symbols are decoded
*                may be shifted out.
*
*                If the n most significant bits of the lower and upper range
*                bounds match, they will not be changed when additional
*                symbols are decoded, so they may be shifted out.
*
*                Adjustments are also made to prevent possible underflows
*                that occur when the upper and lower ranges are so close
*                that decoding another symbol won't change their values.
*   Parameters : bfpOut - pointer to open binary file to read from.
*   Effects    : The upper and lower code bounds are adjusted so that they
*                only contain only bits that will be affected by the
*                addition of a new symbol.  Replacements are read from the
*                encoded stream.
*   Returned   : None
***************************************************************************/
void ReadEncodedBits(bit_file_t *bfpIn)
{
    int nextBit;        /* next bit from encoded input */

    for (;;)
    {
        if (( upper & MASK_BIT(0)) == (lower & MASK_BIT(0)))
        {
            /* MSBs match, allow them to be shifted out*/
        }
        else if ((lower & MASK_BIT(1)) && !(upper & MASK_BIT(1)))
        {
            /***************************************************************
            * Possible underflow condition: neither MSBs nor second MSBs
            * match.  It must be the case that lower and upper have MSBs of
            * 01 and 10.  Remove 2nd MSB from lower and upper.
            ***************************************************************/
            lower   &= ~(MASK_BIT(0) | MASK_BIT(1));
            upper  |= MASK_BIT(1);
            code ^= MASK_BIT(1);

            /* the shifts below make the rest of the bit removal work */
        }
        else
        {
            /* nothing to shift out */
            return;
        }

        /*******************************************************************
        * Shift out old MSB and shift in new LSB.  Remember that lower has
        * all 0s beyond it's end and upper has all 1s beyond it's end.
        *******************************************************************/
        lower <<= 1;
        upper <<= 1;
        upper |= 1;
        code <<= 1;

        if ((nextBit = BitFileGetBit(bfpIn)) == EOF)
        {
            /* either all bits are shifted out or error occurred */
        }
        else
        {
            code |= nextBit;                /* add next encoded bit to code */
        }
    }

    return;
}
