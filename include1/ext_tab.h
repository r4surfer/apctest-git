/*****************************************************************************
                   C A E L U S   I N C L U D E   M O D U L E
                        (c)Copyright 1991 CAELUS, Inc.
                     A L L   R I G H T S   R E S E R V E D
==============================================================================
                      M O D U L E   D E S C R I P T I O N
------------------------------------------------------------------------------
	EXT_tab holds a table of EXTRACT parameters and associated info.
	        This table contains all of the parameters handled by EXTRACT on
		the WANG, and what must be done on UNIX.  Not all EXTRACT
		paramters are supported by the IDSI implementation of EXTRACT.
		THIS TABLE IS SEARCHED WITH A BINARY SEARCH SO IT MUST BE KEPT
		IN ALPHABETIC ORDER!!! And the constant that defines the size
		must be kept up to date.  The table has the following structure:
		   keyword:     The 2 character keyword defined by WANG
		   action:      The processing action need on UNIX, ie
			  	  PASS - pass parameter through, it is supported
			  	  REPL - replace this with another parameter
				  DFLT - return a default response
				  IGNR - ignore argument pair
				  COND - conditional default returned
		   replacement: The alternate keyword or the Default response
				This currently has a max length of 80 bytes.
==============================================================================
               M O D I F I C A T I O N   H I S T O R Y
------------------------------------------------------------------------------
 	DATE		WHO	DESCRIPTION
 	--------	----	----------------------------------------------
	02/28/92	MDH	Set up original attempt
	03/03/92	KEN	Added Lengths, types, and some defaults
	10/17/94	KEN	Change P# from DFLT to PASS
				Alter D- so that 0x04 is returned
	12/24/97	DXL	Change DS & DF from DFLT to PASS

==============================================================================
               M O D U L E   B E G I N S   H E R E
*****************************************************************************/
static char EXT_tab_SccsId[] = "@(#)include ext_tab.h  Version 5.1  10/20/94";

#define CMS_ADM "cmsadmin"      /* CMS Administration group name */
#define TABMAX 116            /* Number of wang supported EXTRACT keywords */

/* define the actions to take in UNIX */
#define PASS  0  	/* Pass pair to EXTRACT				    */
#define REPL  1		/* Replace Keyword, pass pair to EXTRACT	    */
#define DFLT  2		/* Move Default to Receiver, do not pass to EXTRACT */
#define IGNR  3		/* Ignore pair, receiver unchanged		    */
#define COND  4		/* Do not pass to EXT., Dflt to Recvr is conditional*/

#define CHAR  0		/* Type of receiver (default)
			    */
#ifdef BIN
#undef BIN
#endif

#define BIN   1		/* Type of receiver (default)			    */

struct param_tab
  {  /* EXTRACT parameter table */
    char keyword[3]; 			/* parameter keyword        */
    int  action;			/* action necessary on UNIX */
    int  type;				/* type of receiver         */
    int  length;			/* receiver length          */
    unsigned char replacement[80];	/* replacement keyword or   * 
					 * default response         */
  } ;

static struct param_tab EXT_tab[TABMAX] = 
 {/* kwrd, actn, type, lng, replacement  */
   { "A ", IGNR, CHAR,   1, "?"         },	/* Autologoff enabled     */
   { "A$", IGNR, CHAR,   2, "? "        },	/* A + device num         */
   { "A?", IGNR, CHAR, 256, "?"         },	/* ASCII->EBCDIC trans tbl*/
   { "BP", IGNR, BIN ,   4, "????"      },	/* # of mod'able bufs     */
   { "C ", IGNR, CHAR,  16, "?"         },	/* Cluster Info           */
   { "C#", DFLT, CHAR,   4, "CCMM"      },	/* CPU ID Num             */
   { "C$", IGNR, CHAR,   8, "?"         },	/* Wangnet config file    */
   { "CF", PASS, CHAR,   8, " "         },	/* Program file name      */
   { "CL", PASS, CHAR,   8, " "         },	/* Program Library name   */
   { "CS", IGNR, CHAR,   3, "?"         },	/* Currency Symbol        */
   { "CV", PASS, CHAR,   6, " "         },	/* Program Volume name    */
   { "D ", PASS, CHAR,  24, " "         },	/* Device Info            */
   { "D#", IGNR, BIN ,   4, "????"      },	/* System disk device num */
   { "D$", IGNR, CHAR,  34, "?"         },	/* Disk Volume Info       */
   { "D(", IGNR, CHAR,   4, "?"         },	/* Sharer Task Port name  */
   { "D+", IGNR, CHAR,  -2, "varlist"   },	/* Device num's of class  */
/* { "D-", DFLT, CHAR,  24, "?"         },	/* Device Info            */
/* Note: D- usually returns 24 bytes. We only care about hex(04) in byte 1*/
   { "D-", DFLT, BIN,    4, "0x04040404"},	/* Device Info            */
   { "D2", IGNR, BIN ,   4, "????"      },	/* Deflt mod'able area size*/
   { "D@", DFLT, BIN ,   4, "0x00000000"},	/* Disk I/O count         */
   { "DC", IGNR, BIN ,   4, "????"      },	/* Max devices on System  */
   { "DF", PASS, CHAR,   1, "A"         },	/* Date Format            */
   { "DL", PASS, CHAR,  -1, "varlist"   },	/* Device num's of class  */
   { "DP", DFLT, CHAR,   1, "."         },	/* National decimal point */
   { "DS", PASS, CHAR,   1, "/"         },	/* National date separator*/
   { "DV", IGNR, CHAR,  24, "?"         },	/* Disk Volume Info       */
   { "DY", IGNR, BIN ,   4, "????"      },	/* Num of clock units/day */
   { "E:", PASS, BIN ,   4, " "         },	/* Elapsed time for prog  */
   { "E?", IGNR, CHAR, 256, "?"         },	/* EBCDIC->ASCII trans tbl*/
   { "F(", IGNR, CHAR,   4, "?"         },	/* File trans mgr port    */
   { "FC", DFLT, CHAR,   1, " "         },	/* Deflt file protect class */
   { "FN", PASS, BIN ,   4, " "         },	/* Deflt printer form     */
   { "GF", IGNR, CHAR,   4, "NNNN"      },	/* Supported subsystems   */
   { "GV", IGNR, CHAR,   6, "?"         },	/* Cluster Volume name    */
   { "HZ", IGNR, BIN ,   4, "0x0000003c"},	/* AC Line Frequency      */
   { "I ", IGNR, CHAR,  12, "?"         },	/* IOP type info for device */
   { "I$", IGNR, CHAR,  12, "?"         },	/* I with 2byte number    */
   { "ID", PASS, CHAR,   3, " "         },	/* User's ID              */
   { "IL", PASS, CHAR,   8, " "         },	/* Default input library  */
   { "IV", PASS, CHAR,   6, " "         },	/* Default input volume   */
   { "JC", PASS, CHAR,   1, " "         },	/* Deflt background class */
   { "JL", PASS, BIN ,   4, " "         },	/* Deflt bkgd time limit  */
   { "JN", PASS, CHAR,   8, " "         },	/* Current bkgd job name  */
   { "JS", PASS, CHAR,   1, " "         },	/* Deflt bkgd submit stat */
   { "L ", IGNR, CHAR,   8, "?"         },	/* Data Link Proc status  */
   { "LI", PASS, BIN ,   4, " "         },	/* Deflt printer lines/page */
   { "LN", IGNR, CHAR,  38, "?"         },	/* Data Link Proc Info    */
   { "M#", IGNR, BIN ,   4, "????"      },	/* Max device number      */
   { "MC", IGNR, BIN ,   4, "????"      },	/* Max num of workstations */
   { "MD", IGNR, BIN ,   4, "????"      },	/* Max device type number */
   { "ME", COND, CHAR,   4, "0x00000000"},	/* Execute-access mask    */
   { "MF", IGNR, BIN ,   4, "0x00000040"},	/* Max num of open files  */
   { "MR", COND, CHAR,   4, "0x00000000"},	/* Read-access mask       */
   { "MT", IGNR, BIN ,   4, "????"      },	/* Max number of tasks    */
   { "MW", COND, CHAR,   4, "0x00000000"},	/* Write-access mask      */
   { "NA", PASS, CHAR,  24, " "         },	/* User's name            */
   { "NC", IGNR, CHAR,   3, "?"         },	/* National Code          */
   { "NR", IGNR, BIN ,   4, "????"      },	/* Size of Nonres memory  */
   { "NS", DFLT, CHAR,   1, ","         },	/* National thou separator */
   { "O@", DFLT, BIN ,   4, "0x00000000"},	/* Cnt I/O transactions   */
   { "OL", PASS, CHAR,   8, " "         },	/* Default output library */
   { "OV", PASS, CHAR,   6, " "         },	/* Default output volume  */
   { "P#", PASS, BIN ,   4, "0x00000000"},	/* Default printer number */
   { "P(", IGNR, CHAR,   4, "?"         },	/* Printer task name      */
   { "P+", DFLT, BIN ,   4, "0x00000000"},	/* Program Page-in count  */
   { "P-", DFLT, BIN ,   4, "0x00000000"},	/* Program Page-out count */
   { "P:", PASS, BIN ,   4, " "         },	/* Processor time         */
   { "P@", DFLT, BIN ,   4, "0x00000000"},	/* Printer I/O count      */
   { "PC", PASS, CHAR,   1, " "         },	/* Default printer class  */
   { "PF", DFLT, CHAR,   1, " "         },	/* deflt file class for ptr */
   { "PL", PASS, CHAR,   8, " "         },	/* Default program library */
   { "PM", PASS, CHAR,   1, " "         },	/* Default printer mode   */
   { "PR", PASS, BIN ,   4, " "         },	/* User's deflt printer # */
   { "PS", IGNR, BIN ,   4, "????"      },	/* Physical size ??       */
   { "PT", IGNR, BIN ,   4, "????"      },	/* Parents task number    */
   { "PV", PASS, CHAR,   6, " "         },	/* Default program volume */
   { "RD", DFLT, CHAR,   1, "N"         },	/* SPOOLSCR               */
   { "RL", PASS, CHAR,   8, " "         },	/* Run Library            */
   { "RS", DFLT, CHAR,   8, "?"         },	/* Sys for print routing  */
   { "RV", PASS, CHAR,   6, " "         },	/* Run volume             */
   { "S#", DFLT, CHAR,   6, "CMS UX"    },	/* System version number  */
   { "S$", IGNR, CHAR,  16, "?"         },	/* System name            */
   { "S(", IGNR, CHAR,   4, "?"         },	/* Session mgr Port name  */
   { "S+", DFLT, BIN ,   4, "0x00000000"},	/* System page-in count   */
   { "S-", DFLT, BIN ,   4, "0x00000000"},	/* System page-out count  */
   { "S2", IGNR, BIN ,   4, "????"      },	/* Task's data area size  */
   { "SA", IGNR, BIN ,   4, "????"      },	/* System Autologoff time */
   { "SL", PASS, CHAR,   8, " "         },	/* Default spool library  */
   { "SO", IGNR, CHAR,   1, "?"         },	/* System Options         */
   { "SS", DFLT, BIN ,   4, "0x007fffff"},	/* Remaining Stack Space  */
   { "SV", PASS, CHAR,   6, " "         },	/* Default prnt volume    */
   { "T ", IGNR, CHAR,  48, "?"         },	/* Task Information       */
   { "T#", PASS, BIN ,   4, " "         },	/* Current user's task #  */
   { "T$", IGNR, CHAR,  20, "?"         },	/* Tape volume info       */
   { "T(", IGNR, CHAR,   4, "?"         },	/* Task Mgr's port name   */
   { "T+", IGNR, CHAR, 124, "?"         },	/* Task Information       */
   { "T@", DFLT, BIN ,   4, "0x00000000"},	/* Tape I/O Count         */
   { "TC", IGNR, CHAR,   1, ":"         },	/* National time Separator */
   { "TP", IGNR, BIN ,   4, "????"      },	/* Task's Priority number */
   { "TS", IGNR, BIN ,   4, "????"      },	/* Sharer's DMS/TX timeout */
   { "TT", PASS, CHAR,   1, " "         },	/* Task Type F/B          */
   { "TV", IGNR, CHAR,   6, "?"         },	/* Tape Volume info       */
   { "UE", COND, CHAR,   4, "0xffffffff"},	/* User's execute access  */
   { "UR", COND, CHAR,   4, "0xffffffff"},	/* User's read access mask */
   { "UW", COND, CHAR,   4, "0xffffffff"},	/* User's write access    */
   { "V ", IGNR, CHAR,   8, "?"         },	/* Volume VCB address     */
   { "VM", IGNR, BIN ,   4, "????"      },	/* Tot Virtual Mem size   */
   { "W#", PASS, BIN ,   4, " "         },	/* This WS device number  */
   { "W$", DFLT, CHAR,   8, "CAELUS  "  },	/* System Wangnet ID      */
   { "W@", DFLT, BIN ,   4, "0x00000000"},	/* This WS I/O count      */
   { "WL", PASS, CHAR,   8, " "         },	/* Default work library   */
   { "WV", PASS, CHAR,   6, " "         },	/* Default work volume    */
   { "X(", IGNR, CHAR,   4, "?"         },	/* System Task port naem  */
   { "XL", REPL, CHAR,   8, "OL"        },	/* System IPL library     */
   { "XP", IGNR, CHAR,   8, "?"         },	/* System Paging library  */
   { "XV", PASS, CHAR,   6, " "         },	/* System IPL Volume      */
   { "XW", IGNR, CHAR,   8, "?"         } };	/* System work library    */
