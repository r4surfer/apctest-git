/*****************************************************************************
                   C A E L U S   I N C L U D E   M O D U L E
                    	(c)Copyright 1991 CAELUS, Inc.
                     A L L   R I G H T S   R E S E R V E D
==============================================================================
                      M O D U L E   D E S C R I P T I O N
------------------------------------------------------------------------------

  	CONSTANT used in  WVSB

==============================================================================
               M O D I F I C A T I O N   H I S T O R Y
------------------------------------------------------------------------------
 	DATE		WHO	DESCRIPTION
 	--------	----	----------------------------------------------
	1991		unknown Did Original

==============================================================================
               M O D U L E   B E G I N S   H E R E
*****************************************************************************/
static char const_SccsId[] = "@(#)include const.h  Version 5.1  09/16/94";

#define CHARR		1		/* character type */
#define INTT		4		/* integer type */
#define FUNCTION	32
#define CONSTANT	64
#define PICTURE		160		/* picture data type */
#ifndef NULL
#define NULL		0
#endif

#define MAX_STR_SIZE	257
#define MAX_VARNAM_SIZE 68		/* Max varname size = 64,
					 * plus 3 for name conventions,
					 * plus 1 for null terminator */

#define YES		1
#define NO		0
#define On		1
#define Off		0
#define TRUE		1
#define FALSE		0

#define VAR_RECORD	1024

#define COMMA		0x2c
#define SEMICOLON	0x3b
#define Space		' '
#define QUOTE		0x22
#define SQUOTE		0x27
