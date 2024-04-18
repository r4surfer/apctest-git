REM         +---------------------------------------------------------------+
REM         | copy model 849 with ref 27/28 to 819                          |
REM         +---------------------------------------------------------------+


        dim                              /* FILE = APCPLNDT            */~
            pc_rec$102,key$40,prc_rec$58 /* Detail Record              */

        dim f2%(40%),                    /* = 0 if the file is open    */~
            f1%(40%),                    /* = 1 if READ was successful */~
            fs%(40%),                    /* = 1 if file open, -1 if it */~
            pc_ref$8,                    /*   doesn't exist, or 0 if   */~
            r_rec$30, readkey$50,        /*   not yet checked (OPENCHCK*/~
            rslt$(40%)20,des$40          /* Text from file opening     */ 
            
       dim message$256

       dim new$3 
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
            * #1  ! APCPCMST ! Pricing Master Definition File (New)     *~
            * #2  ! 366AR    ! Pricing Changes                          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1, "APCPCMST",                                      ~
                        varc,     indexed,  recsize =   102,            ~
                        keypos =    9, keylen = 40,                     ~
                        alt key  1, keypos =   1, keylen =  8            

            select #2, "366AR",                                         ~
                        varc,     indexed,  recsize =   58,             ~
                        keypos =    1, keylen = 40

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),0%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),0%, rslt$(2%))

            mat f1% = zer
            upd% = 0%
	    cnt = 0%           
	    del% = 0%           
L00500:      /* main loop */
            read #2, using L50770, prc_rec$,          ~
                                                   eod goto ENDJOB
	    key$ = str(prc_rec$,1,40) 

L01000:      /* main loop */
            read #1, key 0% = key$, hold, eod goto L00500 
            goto L01750

L01500:      /* main loop */
            read #1, hold, eod goto L00500 

L01750:      /* main loop */
            get #1, using L50760, pc_rec$
	    cnt = cnt + 1
	    des$ = str(pc_rec$,9,40)
            if (str(pc_rec$,9,40) <> key$) then L00500   	    
            val1$ = str(prc_rec$,41,9)
            val2$ = str(prc_rec$,50,9)
	    convert cnt to cnt$, pic (####0)
            message$ = key$ & ":" & val1$ & ":" & val2$ & "/" & cnt$
	    call "LOGFILE" (message$)
            convert str(prc_rec$,41,9) to value1, data goto CNVERROR 
            convert str(prc_rec$,50,9) to value2, data goto CNVERROR 
            get #1,using L50010, tmpval
            if tmpval <> value1 then L01500 
            put #1,using L50010, value2
            rewrite #1
	    message$ = "updated"
	    call "LOGFILE" (message$)
            goto L01500

L50010: FMT POS(49), PD(14,4)
L50760: FMT CH(102)
L50770: FMT CH(58)
L50780: FMT CH(43)
CNVERROR:   print "data conversion error"
ENDJOB:     print "         records read    = ", cnt
            print "         records updated = ", upd%
	    stop
