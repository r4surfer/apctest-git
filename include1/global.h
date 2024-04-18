/*****************************************************************************
                 C A E L U S   I N C L U D E   M O D U L E
          		(c)Copyright 1991 CAELUS, Inc.
                  A L L   R I G H T S   R E S E R V E D
==============================================================================
                   M O D U L E   D E S C R I P T I O N
------------------------------------------------------------------------------
				global.h
==============================================================================
               M O D I F I C A T I O N   H I S T O R Y
------------------------------------------------------------------------------
 	DATE		WHO		DESCRIPTION
 	--------	----	----------------------------------------------
	1991		unknown
	9/20/91		DP	added ZONE_flag
	10/17/91	DP	added COLTAB_flag
	11/06/91	MDH	added wb_prtprefix
	11/13/91	MDH	added chk_subscript flag
	12/03/91	MDH	added locked record variables 
				  wb_file_lock & wb_rec_lock
	12/20/91	MDH	added INPUT flag variable to unlock keyboard
	03/22/94        MDH     Remove CHK_SUBSCRP : passed if necessary

==============================================================================
               M O D U L E   B E G I N S   H E R E
*****************************************************************************/
static char global_SccsId[] = "@(#)include global.h  Version 5.1  09/16/94";

#ifndef EXTERN
# define EXTERN extern
#endif

EXTERN int  wb_angle;	
EXTERN int  wb_width;
EXTERN int  wb_printer;
EXTERN int  wb_crt;
EXTERN int  wb_pause;
EXTERN int  wb_wisp;
EXTERN int  keep_position;	/* do we need to keep the cursor in
				 * in the middle of the line? */
EXTERN int  global_row;		/* what row to keep the cursor at */
EXTERN int  global_col;		/* what column to keep the cursor at */
EXTERN int  INPUT_flag;		/* currently getting input: unlock keyboard */
EXTERN int  FILE_flag;		/* opened print file already? */
EXTERN int  ZONE_flag;		/* print in next print zone */
EXTERN int  COLTAB_flag; 	/* TAB was encountered & not handled yet */
EXTERN int  wb_pfd;		/* file descriptor	*/
EXTERN char wb_prtpath[257];	/* path name of spooler file */
EXTERN int  matin_flag;		/* used in wb_mread to determine
				 * if it is a MAT READ or MAT INPUT */
EXTERN char wb_prtprefix[9];    /* prefix for print file names */
EXTERN int wb_file_lock;	/* disam file descriptor of file with lock */
EXTERN int wb_rec_lock;		/* number of locked record in proc_file_lock */
