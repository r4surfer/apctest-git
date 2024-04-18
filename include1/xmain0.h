/*****************************************************************************
                 C A E L U S   I N C L U D E   M O D U L E
          	       (c)Copyright 1991 CAELUS, Inc.
                  A L L   R I G H T S   R E S E R V E D
==============================================================================
                   M O D U L E   D E S C R I P T I O N
------------------------------------------------------------------------------
	xmain0.h:	header

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
static char xmain0_SccsId[] = "@(#)include xmain0.h  Version 5.1  09/16/94";

#ifndef abs
#define abs(a) (((a) >= 0) ? (a) : (-(a)))
#endif

#define STRCONST	0x10
#define STRADR  	0x11
#define STRTMP  	0x12
#define STRTMP1 	0x13
#define STRTMP2		0x14
#define STRTMPVEC	0x15
#define STRTMPMAT	0x18
#define STRSTRT		0x1f

#define INTCONST	0x40
#define INTADR		0x41
#define INTTMP		0x42
#define INTTMPVEC	0x45
#define INTTMPMAT	0x48
#define INTSTRT		0x4f

#define BCDCONST	0x80
#define BCDADR  	0x81
#define BCDTMP		0x82
#define BCDTMPVEC	0x85
#define BCDTMPMAT	0x88
#define BCDSTRT		0x8f

#define IOERRX	 0xfb
#define EODX	 0xfc
#define GETX	 0xfb
#define PUTX	 0xfd
#define DATAX    0xfe
#define TIMEOX   0xff

#define GFMAX 6000
long g_f[GFMAX];

#define GCMAX 3000
long g_c[GCMAX];

#define GMMAX 300
long g_m[GCMAX];

#define JMPMAX 10
#define JMPSIZE 10
int g_jmpbuf[JMPMAX][JMPSIZE];

long g_find=0;
long g_cind=0;
long g_ctmp=0;
long g_jmpind=0;
long mat_int;
long mat_exp[2];

/********** new Globals for jmpbuf **************/
#define MAX_RET 128
long G_stack[MAX_RET];		/* return stack */
int G_rcnt = 0;			/* # of elements in return table */
int G_scnt = 0;			/* index to return stack */

long G_chan;			/* global channel number */
int Call_args;			/* count of caller's arg list */
