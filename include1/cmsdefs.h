/*****************************************************************************
                   C A E L U S   I N C L U D E   M O D U L E
                    	(c)Copyright 1991 CAELUS, Inc.
                     A L L   R I G H T S   R E S E R V E D
==============================================================================
                      M O D U L E   D E S C R I P T I O N
------------------------------------------------------------------------------
	Created for portability

==============================================================================
               M O D I F I C A T I O N   H I S T O R Y
------------------------------------------------------------------------------
 	DATE		WHO	DESCRIPTION
 	--------	----	----------------------------------------------
	1991		???	Wrote original
	09/02/93	MDH	Revise types for use on Alpha add typedefs 

==============================================================================
               M O D U L E   B E G I N S   H E R E
*****************************************************************************/
static char cmsdefs_SccsId[] = "@(#)include cmsdefs.h  Version 5.1  10/20/94";

#ifndef ISDECL
typedef signed short	S16;		/* 16 bit signed int */
typedef unsigned short	U16;		/* 16 bit unsigned int */
typedef signed int	S32;		/* 32 bit signed int */
typedef unsigned int	U32;		/* 32 bit unsigned int */
#endif

typedef S32		BCD;
typedef U32		UBCD;
#define BCDis(x)	(*(UBCD *) x)

#define	max(a,b)	(((a) > (b)) ? (a) : (b))
#define	min(a,b)	(((a) < (b)) ? (a) : (b))

#define DS	0
#define DV	1
#define DM	2
#define IS	3
#define IV	4
#define IM	5
#define TS	6
#define TV	7
#define TM	8
/*  FAC definitions */
#define FACBASE		128

#define NOLINE		0	
#define BRIGHT		0
#define BLANK		24
#define BLINK		16

#define MODIFY		0
#define PROTECT		4


#define ALLCHARS	0
#define UPCASE		1
#define NUMERIC		2

