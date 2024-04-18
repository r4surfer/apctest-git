/*****************************************************************************
                 C A E L U S   I N C L U D E   M O D U L E
          	       (c)Copyright 1991 CAELUS, Inc.
                  A L L   R I G H T S   R E S E R V E D
==============================================================================
                   M O D U L E   D E S C R I P T I O N
------------------------------------------------------------------------------

  symbol.h:	header for variable descriptors
  Symbol definition
 	1. constant was changed to represent type
 	2. order of member is rearanged
 	3. In general type is positve or 0, but if it is negative
 	   it means the struct is { int type; char *p; } only
 	   May be used when it is needed (negative reserved )
 	    When it is negative, it represent scalar only,
 	   It represent a char string null terminated when it is -1
 	   It represent a int when it is -4, or BCD whne it is -8
 	4. type == 0 -> p point to a UFB

==============================================================================
               M O D I F I C A T I O N   H I S T O R Y
------------------------------------------------------------------------------
 	DATE		WHO	DESCRIPTION
 	--------	----	----------------------------------------------
	1990		PSY	Wrote & Debugged Module
	10/22/91	DWL	added orig_tbytes field

==============================================================================
               M O D U L E   B E G I N S   H E R E
*****************************************************************************/
static char symbol_SccsId[] = "@(#)include symbol.h  Version 5.1  09/16/94";

struct symbol 
  {
    long  type;		/*  1: CHAR,  4: INT,  8: DOUBLE,  8: BCD */
			/* -1: CHAR, -4: INT, -8: DOUBLE, -8: BCD */
    char  *p;		/* data area */
    int	  len;		/* internal string defined length */
    int	  rank;		/* 0: scalar, 1: vector, 2: matrix */
    int	  dim1;		/* dimension one */
    int	  dim2;		/* dimension two */
    int	  tbyte;	/* data size */
    int	  orig_tbyte;	/* original total bytes */
  } symbol;
			/* read header see negative value */
#define CHARTYP 1
#define STRTYP  1
#define INTTYP	4
#define BCDTYP	8
#define FUNTYP	0
#define CONSTYP 16

#define	SCALAR	0
#define	VECTOR	1
#define	MATRIX	2

/* added the DATA structure 03-21-91 PBP */
struct data_s
  {
    long val;	/* the value of an integer, or */
		/* the value of the 1st or 2nd BCD, or */
		/* a pointer to a string */
    long typ;	/* contains the variable type and the line number */
		/* in the form (0x10000000*type + line) */
  };
