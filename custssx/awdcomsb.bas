       REM  *************************************************************~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *10/08/2009! mods to logic path (AWD001)              ! CMG *~
            *10/23/2009! mods to add sequence number (AWD002)     ! CMG *~
            *10/23/2009! mod to add more status' (AWD003)         ! CMG *~
            *************************************************************

        sub "AWDCOMSB" (dt_rec$, #1, scr_dept$, usr$)                                      
	 
        dim com_rec$128, key$29, rec$128, st$2

REM	if str(dt_rec$,64,02) <> "12" and                          ~
	   str(dt_rec$,64,02) <> "14" and                          ~
	   str(dt_rec$,64,02) <> "16" then goto L03000  

/* (AWD003) begin */
        st$ = str(dt_rec$,64,02)

        if st$ = "04" or st$ = "06" or st$ = "07" then goto updte_st
        if st$ = "08" or st$ = "12" or st$ = "14" then goto updte_st
        if st$ = "16" then goto updte_st

               goto L03000
 
updte_st:
/* (AWD003) end   */
/* (AWD001) */            
REM	if str(dt_rec$,64,02) <> "14" then gosub check_st14        
	if str(dt_rec$,64,02) = "14" then gosub check_st14        

        init(" ") com_rec$
	str(com_rec$,1,18)  = str(dt_rec$,24,18) /* barcode */
        str(com_rec$,19,3)  = str(dt_rec$,42,03) /* dept */
/* (AWD001) */
REM        if scr_dept$ <> "   " then str(com_rec$,19,3)  = scr_dept$
        str(com_rec$,19,3)  = str(dt_rec$,42,03) /* dept */
        if str(dt_rec$,64,02) = "14" or str(dt_rec$,64,02) = "16" ~
            then str(com_rec$,19,3)  = scr_dept$

	str(com_rec$,22,2)  = str(dt_rec$,45,02) /* proc */
	str(com_rec$,24,6)  = str(dt_rec$,47,06) /* date */
	str(com_rec$,30,8)  = str(dt_rec$,116,8) /* time */
	str(com_rec$,38,5)  = str(dt_rec$,01,05) /* load */
	str(com_rec$,43,2)  = str(dt_rec$,104,2) /* shift */
	str(com_rec$,45,2)  = str(dt_rec$,64,02) /* status */
	str(com_rec$,47,3)  = usr$               /* userid */
	str(com_rec$,50,1)  = "S"                
	str(com_rec$,51,25) = str(dt_rec$,189,25) /* part */
	str(com_rec$,76,08) = str(dt_rec$,96,08) /* warranty # */
/*(AWD002)*/
        str(com_rec$,84,05) = str(dt_rec$,111,05) /* Sequence */

	key$ = str(com_rec$,1,29)
	rec$ = com_rec$
	read #1, key = key$, hold, using AWDBAYBW, rec$, eod goto L02000
AWDBAYBW: FMT CH(128)

L01000:
        rewrite #1, using AWDBAYBW, com_rec$
        goto L03000

L02000:
        write #1, using AWDBAYBW, com_rec$
        goto L03000

check_st14:  /* if adding st 14 make sure there is a st 12 */
        init(" ") com_rec$
	str(com_rec$,1,18)  = str(dt_rec$,24,18) /* barcode */
        str(com_rec$,19,3)  = str(dt_rec$,42,03) /* dept */
/* (AWD001) */
REM        if scr_dept$ <> "   " then str(com_rec$,19,3)  = scr_dept$
        str(com_rec$,19,3)  = str(dt_rec$,42,03) /* dept */
	str(com_rec$,22,2)  = str(dt_rec$,45,02) /* proc */
	str(com_rec$,24,6)  = str(dt_rec$,47,06) /* date */
	str(com_rec$,30,8)  = str(dt_rec$,116,8) /* time */
	str(com_rec$,38,5)  = str(dt_rec$,01,05) /* load */
	str(com_rec$,43,2)  = str(dt_rec$,104,2) /* shift */
	str(com_rec$,45,2)  = "12"               /* status */
	str(com_rec$,47,3)  = usr$               /* userid */
	str(com_rec$,50,1)  = "S"                
	str(com_rec$,51,25) = str(dt_rec$,189,25) /* part */
	str(com_rec$,76,08) = str(dt_rec$,96,08) /* warranty # */
/*(AWD002)*/
        str(com_rec$,84,05) = str(dt_rec$,111,05) /* Sequence */

	key$ = str(com_rec$,1,29)
	rec$ = com_rec$
	read #1, key = key$, hold, using AWDBAYBW, rec$, eod goto L02500
        return

L02500:
        write #1, using AWDBAYBW, com_rec$
        return
        
L03000:
        end

