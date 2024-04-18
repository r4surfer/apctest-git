/*****************************************************************************
                 C A E L U S   I N C L U D E   M O D U L E
          	       (c)Copyright 1991 CAELUS, Inc.
                  A L L   R I G H T S   R E S E R V E D
==============================================================================
                   M O D U L E   D E S C R I P T I O N
------------------------------------------------------------------------------
			wsb_print.h

==============================================================================
               M O D I F I C A T I O N   H I S T O R Y
------------------------------------------------------------------------------
 	DATE		WHO	DESCRIPTION
 	--------	----	----------------------------------------------
	8/10/91		BB 	wrote module
	10/15/91	DP 	split wsb.h into wsb_print.h and wsb_scrn.h
	10/28/91	DP	modifed wsb_read_lines and wsb_merge_lines to
				not pass len
	12/30/91	MDH	Removed wsb_merge_lines 
	06/05/92	MDH	Change lines param in read_lines to char
	06/11/92	MDH	Change wsb_prt_erase to get addr of erase_cnt

==============================================================================
               M O D U L E   B E G I N S   H E R E
*****************************************************************************/
static char wsb_print_SccsId[] = "@(#)include wsb_print.h  Version 5.1  09/16/94";

#ifndef WSB_H
#define WSB_H

extern int   last_req;
extern int   curr_pos;	/* next position to add to wsb */

extern void wsb_prt_init ();
	/* Initiate the wsb area to a blank screen.  This routine would be 
	 * called prior to adding any fields for a new ACCEPT statement.  */

extern void wsb_prt_literal (char *literal, int len, int col);
	/* Add a literal to the wsb area at the beginning of wsb.scrn_buf. */

extern void wsb_prt_bcd (long *val, int col);
	/* Add a bcd to the wsb area at the beginning of wsb.scrn_buf. */

extern void wsb_prt_int (long *val, int col);
	/* Add a int to the wsb area at the beginning of wsb.scrn_buf. */

extern void wsb_prt_tab ();
	/* Add spaces to wsb.scrn_buf when a comma is encountered so output 
	 * will start in the next print zone, or add space out to the 
	 * specified tab location */

extern void wsb_prt_erase (int row, int col, int *erase_cnt);
	/* write spaces to wsb.scrn_buf to erase the designated number of 
	 * characters.  */

extern void wsb_prt_fac (int fac);
	/* Set the field attribute character to the value specified.  */

extern void wsb_prt_vwang (unsigned char function, char lines, 
                            unsigned char row, unsigned char col); 
	/* Call the vwang routine using the current state of the wsb area.  
	 * For use with the PRINT AT statement to keep the print position. */

extern void wsb_read_input (char *buffer, int count);
	/* Read the data entered by the user from wsb.scrn_buf into buffer.*/

extern void wsb_read_lines (char lines, unsigned char row, unsigned char col); 
	/* Call the vwang routine using the current state of the wsb area.  
	 * For use with the PRINT AT statement to read the existing characters 
	 * on the line to be printed */

#endif /* WSB_H */
