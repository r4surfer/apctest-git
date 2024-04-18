/*****************************************************************************
                   C A E L U S   I N C L U D E   M O D U L E
                    	(c)Copyright 1991 CAELUS, Inc.
                     A L L   R I G H T S   R E S E R V E D
==============================================================================
                      M O D U L E   D E S C R I P T I O N
------------------------------------------------------------------------------
		Grammar variable definitions - used only in B_Cgen code

==============================================================================
               M O D I F I C A T I O N   H I S T O R Y
------------------------------------------------------------------------------
 	DATE		WHO	DESCRIPTION
 	--------	----	----------------------------------------------
	1991		unknown
	03/19/92	MDH	Made it use EXTERN to eliminate duplicate 0 file
	10/19/93	MDH	Add file desc's for Comment file in -c Option
	03/22/94        MDH     Add subscript error severity flag
	06/01/97	KAB	Eunice - added PCONTINUE flag

==============================================================================
               M O D U L E   B E G I N S   H E R E
*****************************************************************************/
static char grammar_SccsId[] = "@(#)include grammar.h  Version 5.2  06/05/97";

#ifndef EXTERN               /* if not defined make things external */
#  define EXTERN extern
#endif
#include <stdio.h>           /* need standard file descriptor defination */
#ifndef EOF
#define EOF -1
#endif
#ifndef abs
#define abs(a) (((a) >= 0) ? (a) : (-(a)))
#endif

/* Flags used with Gflag */
#define CONTINUE  0x01		/* CONTINUE to NEXT line */
#define ERRRUN	  0x02		/* error in run */
#define MATF      0x04		/* MATRIX FLAG */ 
#define ERRMSG    0x08		/* error message has been print */
#define LETTER    0x10		/* keep char */
#define ERRSEM    0x20		/* semantic error */
#define ENDFIL    0x40		/* end of BASIC program */
#define CHKDF     0x80		/* check flag           */
#define ISFUN     0x0100	/* in function */
#define SUBF      0x0200	/* begin of SUB*/
#define ISBREAK   0x0400	/* force break on new line */
#define COMMENT   0x0800	/* comment here: */
#define XCONTINUE 0x1000	/* turn off line # */		
#define TERM	  0x2000	/* terminate on \" or \' */
#define ISCALL    0x4000        /* Is the CALL statement */
#define COMF      0x8000	/* COM statement */
#define PCONTINUE 0x010000	/* Continue Pending */
EXTERN int Gflag;		/* global flag */

EXTERN int Curkey;		/* Current BASIC line NO */
EXTERN int Curch;		/* Current char */
EXTERN int Gotyp;		/* goto or gosub type */
EXTERN int Termchar;		/* char to be terminated */

#define TKBUFNO 150
#define TKBUFMAX (TKBUFNO * ONE_KB)
EXTERN unsigned char *Tknbuf;		/* token buffer */
EXTERN unsigned char *Tknptr;		/* ptr of token */
EXTERN unsigned char *Tknmax;		/* ptr to top of token buffer */
EXTERN int Tknptr_i;

#define KEYQNO (100 * 4)
#define KEYQMAX (KEYQNO * ONE_KB)
EXTERN long *Keyq;	/* sequence of key value area */
EXTERN int Keyq_i;	/* index of key value */

EXTERN FILE *Enterfd;	/* file descriptor for redirect input by ENTER */
EXTERN FILE *Outfd;	/* file descriptor for redirect output */
EXTERN FILE *Tmpfd;	/* file descriptor for redirect output */
EXTERN FILE *Datafd;	/* file descriptor for DATA statement  */
EXTERN int Btmpfd;	/* file descriptor for tmp file */
EXTERN FILE *Comfd;	/* file descriptor for Comment file during C option */

EXTERN int Col ;
EXTERN long Dim1;
EXTERN long Dim2;
EXTERN int Gtyp;

EXTERN int chk_subscrpt;      /* subscript check severity */
