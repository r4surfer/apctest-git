/*****************************************************************************
                   C A E L U S   I N C L U D E   M O D U L E
                    	(c)Copyright 1991 CAELUS, Inc.
                     A L L   R I G H T S   R E S E R V E D
==============================================================================
                      M O D U L E   D E S C R I P T I O N
------------------------------------------------------------------------------
	Keyword definition table: 
         NOTE:  This table is defined and initialized in keyword0.h

==============================================================================
               M O D I F I C A T I O N   H I S T O R Y
------------------------------------------------------------------------------
 	DATE		WHO	DESCRIPTION
 	--------	----	----------------------------------------------

==============================================================================
               M O D U L E   B E G I N S   H E R E
*****************************************************************************/
static char keyword_SccsId[] = "@(#)include keyword.h  Version 5.1  09/16/94";

extern struct keyword 
  {    /* *** STRUCTURE is also defined in keyword0.h *** */
        char *key;
        int keyval;
	int keycnt;
  } Keytable[];
