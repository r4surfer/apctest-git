/*****************************************************************************
                   C A E L U S   I N C L U D E   M O D U L E
                    	(c)Copyright 1991 CAELUS, Inc.
                     A L L   R I G H T S   R E S E R V E D
==============================================================================
                      M O D U L E   D E S C R I P T I O N
------------------------------------------------------------------------------
	Output structure relationships header.
	needs to be included in main.c, startpgm.c, mkcall.c

==============================================================================
               M O D I F I C A T I O N   H I S T O R Y
------------------------------------------------------------------------------
 	DATE		WHO	DESCRIPTION
 	--------	----	----------------------------------------------
	11/12/97	KAB	Initial definition

==============================================================================
               M O D U L E   B E G I N S   H E R E
*****************************************************************************/
static char dostrct_SccsId[] = "@(#)include dostrct.h  Version 5.1  11/12/97";

#ifndef EXTERN               /* if not defined make things external */
#  define EXTERN extern
#endif
#include <stdio.h>           /* need standard file descriptor defination */

EXTERN FILE *Strfd;	/* file descriptor for redirect output */
char   Sbuf[80];	/* path for structure file */
char   pgmname[80];	/* program name for output */

