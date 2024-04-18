/*
REM         +---------------------------------------------------------------+
REM         | EWDAPPCUS -> AWDAPPCUS                                        |
REM         |   EWD key : Cust_code + nn (day of week) (mon = 01)           |
REM         |       data: from_time x(4) + to_time x(4) + "-00000000"       |
REM         |                                                               |
REM         |   AWD key : '0' + cust_code + n (seq #, by time range)        |
REM         |       data: from_time x(4) + to_time x(4) + '-' + days        |
REM         |                   were days = "MTWRFSU" for time range        |
REM         |                                                               |
REM         +---------------------------------------------------------------+
*/
        dim                              /* FILE = APCPLNDT            */~
            rec$128, rec2$128, key$24, key2$24  /* Detail Record       */

        dim f2%(10%),                    /* = 0 if the file is open    */~
            f1%(10%),                    /* = 1 if READ was successful */~
            fs%(10%),                    /* = 1 if file open, -1 if it */~
            cust$9,                      /* customer code              */~
            rslt$(10%)20                 /* Text from file opening     */ 
            
       dim message$256
       dim time_tbl$(7)8, day_tbl$(7)7

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #2  ! GENCODES ! GENCODES for testing                     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #2, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            call "OPENCHCK" (#2, fs%(2%), f2%(2%), 500%, rslt$(2%))

            mat f1% = zer
            upd% = 0%
            tst% = 0%
	    cnt% = 0%           
	    key$ = "EWDAPPCUS              "
	    dow$ = "MTWRFSU"
	    init(" ") rec$, rec2$, cust$, time_tbl$()
	    init(" ") day_tbl$()

L01000:      /* main loop */
            read #2, key > key$, using L50760, rec$,          ~
                                                   eod goto L56890
            if str(rec$,1,9) <> "EWDAPPCUS" then L56890
            if (cust$ = "         ") then cust$ = str(rec$,10,9)
            if (str(rec$,10,9) <> cust$) then gosub cust_break 

	    cnt% = cnt% + 1

            time$ = str(rec$,25,8)
	    day$ = str(rec$,16,2)
	    key$ = str(rec$,1,24)
	    day% = 0%
	    convert day$ to day%, data goto data_error
data_error:
            for l% = 1% to 7%
                if time$ = time_tbl$(l%) then time_found
                if time_tbl$(l%) <> "        " then end_loop
add_time:
                time_tbl$(l%) = time$
                str(day_tbl$(l%),day%,1) = "Y"
                goto L01000   

time_found:
                str(day_tbl$(l%),day%,1) = "Y"
                goto L01000   

end_loop:
            next l%

	    goto L01000

cust_break:                                    
            for l% = 1% to 7%
            if time_tbl$(l%) <= "00000000" then exit_sub
	    init(" ") rec2$ 
            str(rec2$,1,18) = "AWDAPPCUS00000001"                            
            str(rec2$,11,9) = cust$ & "000000"
            str(rec2$,25,30) = "                              "
            str(rec2$,25,9) = time_tbl$(l%) 
	    dayt$ = "       " 
	    d% = 1%
            for x% = 1% to 7%
                if str(day_tbl$(l%),x%,1%) <> "Y" then skip_day                 
                str(dayt$,d%,1%) = str(dow$,x%,1%) 
                d% = d% + 1%
skip_day:
            next x%
   	    convert l% to str(rec2$,20,1), pic (0)
	    key2$ = str(rec2$,1,24)
            read #2,key = key2$,hold,eod goto no_rec
REM         delete #2
            goto exit_sub
no_rec:
            str(rec2$,25,9) = time_tbl$(l%) 
            str(rec2$,33,1) = "-"             
            str(rec2$,34,7) = dayt$             
	    message$ = rec2$
	    call "LOGFILE" (message$)
            write #2, using L50760, rec2$ 
            next l%
exit_sub:
            cust$ = str(rec$,10,9)
	    init(" ") time_tbl$(), day_tbl$()
            return

L50760:     FMT CH(128)
L56890:     gosub cust_break                                    
            print "         records read    = ", cnt%
            print "         records test    = ", tst%
            print "         records updated = ", upd%
             end
