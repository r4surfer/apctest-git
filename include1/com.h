/*****************************************************************************
                 C A E L U S   I N C L U D E   M O D U L E
          	       (c)Copyright 1991 CAELUS, Inc.
                  A L L   R I G H T S   R E S E R V E D
==============================================================================
                   M O D U L E   D E S C R I P T I O N
------------------------------------------------------------------------------
	com.h		include file for COM blocks

==============================================================================
               M O D I F I C A T I O N   H I S T O R Y
------------------------------------------------------------------------------
 	DATE		WHO	DESCRIPTION
 	--------	----	----------------------------------------------
	10/14/91	DWL	Wrote & Debugged Header

==============================================================================
               M O D U L E   B E G I N S   H E R E
*****************************************************************************/
static char com_SccsId[] = "@(#)include com.h  Version 5.1  09/16/94";

#ifndef EXTERN
#  define EXTERN extern
#  define ISZERO
#endif

EXTERN char *g_com;
EXTERN long g_cidx;
