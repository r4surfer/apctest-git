/*****************************************************************************
                 C A E L U S   I N C L U D E   M O D U L E
          	       (c)Copyright 1991 CAELUS, Inc.
                  A L L   R I G H T S   R E S E R V E D
==============================================================================
                   M O D U L E   D E S C R I P T I O N
------------------------------------------------------------------------------
	xmain.h		header

==============================================================================
               M O D I F I C A T I O N   H I S T O R Y
------------------------------------------------------------------------------
 	DATE		WHO	DESCRIPTION
 	--------	----	----------------------------------------------
	1991		Unknown	Wrote & Debugged header	
	9/20/91		DWL	Added STRTMP2
	05/24/94        MDH     Up MAX_RET gosub return table size

==============================================================================
               M O D U L E   B E G I N S   H E R E
*****************************************************************************/
static char xmain_SccsId[] = "@(#)include xmain.h  Version 5.1  09/16/94";

#ifndef abs
#define abs(a) (((a) >= 0) ? (a) : (-(a)))
#endif

#define STRCONST 	0x10		/* String Literal */
#define STRADR   	0x11		/* String Variable */
#define STRTMP   	0x12		/* Temporary string with length */
#define STRTMP1  	0x13		/* Temp. String with temp. length */
#define STRTMP2		0x14		/* Temp. String literal for CAT */
#define	STRTMPVEC	0x15		/* Temp String Vector */
#define STRTMPMAT	0x18		/* Temp String Matrix */
#define STRSTRT  	0x1f		/* String Symbol Structure */

#define INTCONST 	0x40
#define INTADR   	0x41
#define INTTMP	 	0x42
#define INTTMPVEC	0x45
#define INTTMPMAT	0x48
#define INTSTRT  	0x4f

#define BCDCONST 	0x80
#define BCDADR   	0x81
#define BCDTMP	 	0x82
#define BCDTMPVEC	0x85
#define BCDTMPMAT	0x88
#define BCDSTRT  	0x8f


#define IOERRX	 0xfb
#define EODX	 0xfc
#define GETX	 0xfb
#define PUTX	 0xfd
#define DATAX    0xfe
#define TIMEOX   0xff

#define GFMAX 6000
extern long g_f[];

#define GCMAX 3000
extern long g_c[];

#define GMMAX 300
extern long g_m[];

#define JMPMAX 10
#define JMPSIZE 10
extern int g_jmpbuf[JMPMAX][JMPSIZE];
extern long g_find;
extern long g_ctmp;
extern long g_cind;
extern long g_jmpind;
extern long mat_int;
extern long mat_exp[2];


/********** new Globals for jmpbuf **************/
#define MAX_RET 128
extern long G_stack[MAX_RET];		/* return stack */
extern int G_rcnt;			/* # of elements in return table */
extern int G_scnt;			/* index to return stack */

extern long G_chan;			/* global channel number */
