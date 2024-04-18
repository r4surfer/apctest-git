/*****************************************************************************
                   C A E L U S   I N C L U D E   M O D U L E
                        (c)Copyright 1991 CAELUS, Inc.
                     A L L   R I G H T S   R E S E R V E D
==============================================================================
                      M O D U L E   D E S C R I P T I O N
------------------------------------------------------------------------------
	SET_tab holds a table of SET parameters and associated info.
	        This table contains all of the parameters handled by SET on
		the WANG, and what must be done on UNIX.  Not all SET
		paramters are supported by the IDSI implementation of SET.
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
	03/04/92	KEN	Copied EXTRACT for SET

==============================================================================
               M O D U L E   B E G I N S   H E R E
*****************************************************************************/
static char SET_tab_SccsId[] = "@(#)include set_tab.h  Version 5.1  09/16/94";

#define TABMAX 26
/* define the actions to take in UNIX */
#define PASS  0  	/* Pass pair to SET				    */
#define REPL  1  	/* Replace Keyword and pass to SET		    */
#define DFLT  2		/* Move Default to Receiver, do not pass to SET     */
#define IGNR  3		/* Ignore pair, receiver unchanged		    */
#define COND  4		/* Do not pass to SET., Dflt to Recvr is conditional*/

#define CHAR  0		/* Type of receiver (default)			    */
#define BIN   1		/* Type of receiver (default)			    */

struct param_tab
  {  /* SET parameter table */
    char keyword[3]; 			/* parameter keyword */
    int  action;			/* action necessary on UNIX */
    int  type;				/* type of receiver         */
    int  length;			/* receiver length          */
    unsigned char replacement[80];	/* replacement keyword or 
					   default response */
    char *Section, *Key;                /* ini address              */
  } ;

static struct param_tab SET_tab[TABMAX] = 
 {/* kwrd, actn, type, lng, replacement  */
   { "FC", IGNR, CHAR, 1, " ", " ", " "},	   /* File protect class     */
   { "FH", IGNR, CHAR,-1, "?", " ", " "},	   /* File access list	     */
   { "FN", PASS, BIN , 4, " ", "Printer", "Form"}, /* Printer form number    */
   { "IL", PASS, CHAR, 8, " ", "DataBase","InLib"},/* Default input library  */
   { "IV", PASS, CHAR, 6, " ", "DataBase","InVol"},/* Default input volume   */
   { "JC", IGNR, CHAR, 1, " ", "Job", "Class"},	   /* Deflt background class */
   { "JL", IGNR, BIN , 4, "?", "Job", "Limit"},	   /* Deflt bkgd time limit  */
   { "JS", IGNR, CHAR, 1, " ", "Job", "Queue"},	   /* Deflt bkgd submit stat */
   { "LI", PASS, BIN , 4, " ", "Printer", "LinesPerPage"}, /* Deflt print lines/page */
   { "OL", PASS, CHAR, 8, " ", "DataBase", "OutLib"},	/* Default output library */
   { "OV", PASS, CHAR, 6, " ", "DataBase", "OutVol"},	/* Default output volume  */
   { "P#", PASS, BIN , 4, " ", "Printer", "Printer"},	/* Default printer number */
   { "PC", PASS, CHAR, 1, " ", "Printer", "Class"}, /* Default printer class  */
   { "PF", IGNR, CHAR, 1, " ", "Printer", "Form"},  /* deflt print file class */
   { "PL", IGNR, CHAR, 8, "?", "Program", "RunLib"},/* Default program library*/
   { "PM", PASS, CHAR, 1, " ", "Printer", "Mode"},  /* Default printer mode   */
   { "PR", PASS, BIN , 4, " ", "Printer", "Printer"},/*Default printer number */
   { "PV", IGNR, CHAR, 6, "?", "Program", "RunVol"},/* Default program volume */
   { "RD", IGNR, CHAR, 1, "N", " ", " "},	    /* SPOOLSCR Remote disp.  */
   { "RL", PASS, CHAR, 8, " ", "Program", "RunLib"},/* Run Library            */
   { "RR", IGNR, BIN , 4, "0x00000000", " ", " "},  /* Spoolsys (RS) return cd*/
   { "RS", IGNR, CHAR, 8, "?", " ", " "},	    /* Sys for print routing  */
   { "RV", PASS, CHAR, 6, " ", "Program", "RunVol"},/* Run volume             */
   { "SL", PASS, CHAR, 8, " ", "Printer", "SpoolLib"}, /* Default spool library  */
   { "SV", PASS, CHAR, 6, " ", "Printer", "SpoolVol"}, /* Default prnt volume    */
   { "WV", PASS, CHAR, 6, " ", "DataBase","WorkVol"}}; /* Default work volume    */