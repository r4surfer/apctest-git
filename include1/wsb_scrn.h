/*****************************************************************************
                 C A E L U S  I N C L U D E   M O D U L E
          	      (c)Copyright 1991 CAELUS, Inc.
                  A L L   R I G H T S   R E S E R V E D
==============================================================================
                   M O D U L E   D E S C R I P T I O N
------------------------------------------------------------------------------
			wsb_scrn.h

==============================================================================
               M O D I F I C A T I O N   H I S T O R Y
------------------------------------------------------------------------------
 	DATE		WHO	DESCRIPTION
 	--------	----	----------------------------------------------
	8/10/91		BB 	wrote module
	10/15/91	DP	split wsb.h into wsb_print.h and wsb_scrn.h
	01/24/92	MDH	Changed status to unsigned char in call_vwang

==============================================================================
               M O D U L E   B E G I N S   H E R E
*****************************************************************************/
static char wsb_scrn_SccsId[] = "@(#)include wsb_scrn.h  Version 5.1  09/16/94";

#ifndef WSB_H
#define WSB_H

extern int  last_req;
extern int  curr_pos;	/* next position to add to wsb */
extern void wsb_init ();
	/* Initiate the wsb area to a blank screen. This routine would be 
	 * called prior to adding any fields for a new ACCEPT statement.  */

extern void wsb_add_literal (char *literal, int len);
	/* Add a literal to the wsb area at the current screen location.  If 
	 * the literal is too long it is wrapped onto the follow lines.  */

extern void wsb_at (int row, int col, int erase_cnt);
	/* Position the row and column at the specified position to emulate 
	 * the AT call.  */

extern int  wsb_add_str (char *ptr, int len, unsigned char **wsb);
	/* Add a string to the wsb at the current position and FAC value.  */

extern int wsb_add_int(long *intaddr, char *pic, unsigned char **wsb, int *len);
	/* Add a integer to the wsb at the current position and FAC value. */

extern int wsb_add_bcd(UBCD *intaddr, char *pic, unsigned char **wsb, int *len);
	/* Add a BCD to the wsb at the current position and FAC value.  */

extern void wsb_set_fac (int fac);
	/* Set the field attribute character to the value specified.  */

extern void wsb_call_vwang (char function, unsigned char *term_list, 
			    char *term_pfcode, unsigned char *status);
	/* Call the vwang routine using the current state of the wsb area.  */

extern void wsb_adv_pos (int cnt);

extern void wsb_set_len (int len);

extern int wsb_add_field (unsigned char *ptr, int len, unsigned char **wsb_ptr);

extern void wsb_dump ();

#endif /* WSB_H */
