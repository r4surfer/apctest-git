/*****************************************************************************
                   C A E L U S   I N C L U D E   M O D U L E
                    	(c)Copyright 1991 CAELUS, Inc.
                     A L L   R I G H T S   R E S E R V E D
==============================================================================
                      M O D U L E   D E S C R I P T I O N
------------------------------------------------------------------------------
	Time limit definitions - Used by B_Cgen in main.c
				 wb_init.c in scrn
				 tlim.c in util
             Turn it on or off here, then recompile tlim in util, B_Cgen, 
              and wb_init in scrn in that order!

==============================================================================
               M O D I F I C A T I O N   H I S T O R Y
------------------------------------------------------------------------------
 	DATE		WHO	DESCRIPTION
 	--------	----	----------------------------------------------
	02/17/93	MDH	Initial definition

==============================================================================
               M O D U L E   B E G I N S   H E R E
*****************************************************************************/
static char tbomb_SccsId[] = "@(#)include tbomb.h  Version 5.1  09/16/94";

/* #define DEMO */  /* Turn the whole mess on: comment this out to turn off */

#define LMON 0		/* These three will allow use of this utility */
#define LDAY 0		/* until this date. If all 0 no time limit */
#define LYR 0

