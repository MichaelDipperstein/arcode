/***************************************************************************
*              Sample Program Using Arithmetic Encoding Library
*
*   File    : sample.c
*   Purpose : Demonstrate usage of arithmetic encoding library
*   Author  : Michael Dipperstein
*   Date    : March 10, 2004
*
****************************************************************************
*
* SAMPLE: Sample usage of the arcode Arithmetic Encoding Library
* Copyright (C) 2004, 2007, 2014, 2017 by
* Michael Dipperstein (mdipperstein@gmail.com)
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
#include "optlist/optlist.h"
#include "arcode.h"

/***************************************************************************
*                               PROTOTYPES
***************************************************************************/

/***************************************************************************
*                                CONSTANTS
***************************************************************************/
#ifndef FALSE
#define FALSE   0
#endif

#ifndef TRUE
#define TRUE    1
#endif

/***************************************************************************
*                                FUNCTIONS
***************************************************************************/

/****************************************************************************
*   Function   : main
*   Description: This is the main function for this program, it validates
*                the command line input and, if valid, it will call
*                functions to encode or decode a file using the arithmetic
*                coding algorithm.
*   Parameters : argc - number of parameters
*                argv - parameter list
*   Effects    : Encodes/Decodes input file
*   Returned   : EXIT_SUCCESS for success, otherwise EXIT_FAILURE.
****************************************************************************/
int main(int argc, char *argv[])
{
    option_t *optList, *thisOpt;
    FILE *inFile, *outFile; /* input & output files */
    char encode;            /* encode/decode */
    model_t model;          /* static/adaptive model*/

    /* initialize data */
    inFile = NULL;
    outFile = NULL;
    encode = TRUE;
    model = MODEL_STATIC;

    /* parse command line */
    optList = GetOptList(argc, argv, "acdi:o:h?");
    thisOpt = optList;

    while (thisOpt != NULL)
    {
        switch(thisOpt->option)
        {
            case 'a':       /* adaptive model vs. static */
                model = MODEL_ADAPTIVE;
                break;

            case 'c':       /* compression mode */
                encode = TRUE;
                break;

            case 'd':       /* decompression mode */
                encode = FALSE;
                break;

            case 'i':       /* input file name */
                if (inFile != NULL)
                {
                    fprintf(stderr, "Multiple input files not allowed.\n");
                    fclose(inFile);

                    if (outFile != NULL)
                    {
                        fclose(outFile);
                    }

                    FreeOptList(optList);
                    exit(EXIT_FAILURE);
                }
                else if ((inFile = fopen(thisOpt->argument, "rb")) == NULL)
                {
                    perror("Opening Input File");

                    if (outFile != NULL)
                    {
                        fclose(outFile);
                    }

                    FreeOptList(optList);
                    exit(EXIT_FAILURE);
                }

                break;

            case 'o':       /* output file name */
                if (outFile != NULL)
                {
                    fprintf(stderr, "Multiple output files not allowed.\n");
                    fclose(outFile);

                    if (inFile != NULL)
                    {
                        fclose(inFile);
                    }

                    FreeOptList(optList);
                    exit(EXIT_FAILURE);
                }
                else if ((outFile = fopen(thisOpt->argument, "wb")) == NULL)
                {
                    perror("Opening Output File");

                    if (inFile != NULL)
                    {
                        fclose(inFile);
                    }

                    FreeOptList(optList);
                    exit(EXIT_FAILURE);
                }

                break;

            case 'h':
            case '?':
                printf("Usage: %s <options>\n\n", FindFileName(argv[0]));
                printf("options:\n");
                printf("  -c : Encode input file to output file.\n");
                printf("  -d : Decode input file to output file.\n");
                printf("  -i <filename> : Name of input file.\n");
                printf("  -o <filename> : Name of output file.\n");
                printf("  -a : Use adaptive model instead of static.\n");
                printf("  -h | ?  : Print out command line options.\n\n");
                printf("Default: %s -c\n", FindFileName(argv[0]));

                FreeOptList(optList);
                return(EXIT_SUCCESS);
        }

        optList = thisOpt->next;
        free(thisOpt);
        thisOpt = optList;
    }

    /* validate command line */
    if (NULL == inFile)
    {
        fprintf(stderr, "Input file must be provided\n");
        fprintf(stderr, "Enter \"%s -?\" for help.\n", FindFileName(argv[0]));

        if (outFile != NULL)
        {
            fclose(outFile);
        }

        exit (EXIT_FAILURE);
    }
    else if (NULL == outFile)
    {
        fprintf(stderr, "Output file must be provided\n");
        fprintf(stderr, "Enter \"%s -?\" for help.\n", FindFileName(argv[0]));

        if (inFile != NULL)
        {
            fclose(inFile);
        }

        exit (EXIT_FAILURE);
    }

    /* we have valid parameters encode or decode */
    if (encode)
    {
        ArEncodeFile(inFile, outFile, model);
    }
    else
    {
        ArDecodeFile(inFile, outFile, model);
    }

    fclose(inFile);
    fclose(outFile);
    return EXIT_SUCCESS;
}
