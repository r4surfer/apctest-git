REM         +---------------------------------------------------------------+
REM         | load changes from Oracle into CUSTOMER                        |
REM         +---------------------------------------------------------------+


        dim                              /* FILE = CUSTOMER            */~
            hold_key$9,                  /* Detail Record              */~
            hold_type$4                    

        dim f2%(60%),                    /* = 0 if the file is open    */~
            f1%(60%),                    /* = 1 if READ was successful */~
            fs%(60%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(60%)20                 /* Text from file opening     */ 
            
            dim comp_so$8, comp_line$2, csv_rec$256, conf_flag$1     
            dim comma(50), func$10, vendor$9, bf_key$15 
            dim v1$8, v2$8, v3$8, message$78, sort_name$30 
	    dim fields$(50)256, init_rec$256, wrk_rec$256
            dim key0$9, sold_name$30, status$1, sold_ad1$30, sold_ad2$30
            dim sold_ad3$30, sold_ad4$30, sold_city$18, sold_state$2
	    dim sold_zip$9, bill_name$30, bill_ad1$30, bill_ad2$30
	    dim bill_ad3$30, bill_ad4$30, bill_city$18, bill_state$2
	    dim bill_zip$9,logmsg$256
	    dim rec$(6)200

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$41, pname$32, apc1$41                   /* (EWD055) */
                                                            /* (EWD060) */
                                                            /* (EWD066) */
                                                            /* (EWD068) */
                                                            /* (EWD072) */
                                                            /* (AWD077) */
                                                            /* (AWD082) */ 
        REM *************************************************************

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
            * #1  ! CUSTOMER !                                          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************
            select #1,   "CUSTOMER",                                     ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),0%, rslt$(1%))

            mat f1% = zer

	    /* Open file from Oracle */
	    csv_rec$ = "test.csv"
	    func$ = "open"
	    err% = 0%
REM  logmsg$ = "open"
REM  call "LOGFILE" (logmsg$)
             call "SEQREAD" (func$,csv_rec$,err%)
	     if err% <> 0% then goto END_JOB

	     /* initialise variables */
            v1% = 0%
	    v2% = 0%
	    v3% = 0%
             sold_sw$ = "0"
	     mult_bill$ = "0"
             mult_rec$ = "0"
	    init(" ") init_rec$, hold_key$, hold_type$

L01000:      /* main loop */
            hold_key$  = str(fields$(1),1,9)
            hold_type$ = str(fields$(2),1,4) 
	    func$ = "read"
	    csv_rec$ = init_rec$
	    err% = 0%
	    /* Read record */
REM  logmsg$ = "read"
REM  call "LOGFILE" (logmsg$)
             call "SEQREAD" (func$,csv_rec$,err%)
	     if err% <> 0% then goto END_JOB
REM   logmsg$ = csv_rec$
REM   call "LOGFILE" (logmsg$)
             l = 0
	     v1% = v1% + 1%
             wrk_rec$ = csv_rec$            

            /* append '|' to record */
	     for y = 1 to 255        
		x = 256 - y
		if str(wrk_rec$,x,1) <= " " then goto skip_char 
                   str(wrk_rec$,x+1,1) = "|"
		   y = 255
skip_char:
	     next y

            /* find '|' field delimiters and table */
	     for x = 1 to 255  
		 if str(wrk_rec$,x,1) <> "|" then goto skip_it
                     l = l + 1
                     comma(l) = x
		     if l > 30 then x = 255 
skip_it:
	     next x

            /* break out fields and store in 'fields$' table */
	     fields$(1) = str(wrk_rec$,1,(comma(1)-1)) & "         "
             for x = 2 to 28
		 strlen = comma(x) - comma(x - 1) -1
	         fields$(x) = str(wrk_rec$,(comma(x-1)+1),strlen) & "                            "
    	     next x

            /* skip obsolete records */
             if str(fields$(3),1,1) = "O" then goto L01000    
            /* check to see if input file sorted */
            if hold_key$  > str(fields$(1),1,9) then goto not_sorted
            if hold_key$  = str(fields$(1),1,9) and                        ~
               hold_type$ > str(fields$(2),1,4) then goto not_sorted

   	     vendor$ = str(fields$(1),1,9) & "        "           
REM    logmsg$ = "customer " & vendor$
REM    call "LOGFILE" (logmsg$)
   	     read #1,hold,key = vendor$, USING CUSTOMERFMT,                  ~  
	   key0$, sort_name$, sold_name$, sold_ad1$, sold_ad2$, sold_ad3$,   ~
		   sold_ad4$, sold_city$, sold_state$, sold_zip$,            ~
                   bill_name$, bill_ad1$, bill_ad2$, bill_ad3$,              ~
		   bill_ad4$, bill_city$, bill_state$, bill_zip$, status$,   ~
	                               eod goto L02000
CUSTOMERFMT: FMT CH(9), CH(30), 5*CH(30), CH(18), CH(2), POS(211), CH(9),   ~ 
             POS(253), 5*CH(30), CH(18), CH(2), POS(424), CH(9), POS(793),   ~
	     CH(1)

REMlogmsg$ = "found " & vendor$ & ":" & str(fields$(2),1,4) & ":" & hold_type$
REM    call "LOGFILE" (logmsg$)
            /* set switches */
            if str(fields$(2),1,4) = "0000" then sold_sw$ = "1"
            if hold_key$ <> str(fields$(1),1,9) then goto new_rec
	    mult_rec$ = "1"
	    if hold_type$ = "0000" then goto continue
	    goto continue
REM         mult_bill$ = "1"
REM         goto L01000  

new_rec:     /* account break, reset switches */
             sold_sw$ = "0"
            if str(fields$(2),1,4) = "0000" then sold_sw$ = "1"
	     mult_bill$ = "0"
             mult_rec$ = "0"

continue:
            if str(fields$(2),1,4) <> "0000" then goto bill_record

sold_record:  /* update sold address */
	     v2% = v2% + 1%
             sold_name$  = fields$(28)
             sold_ad1$   = fields$(04)
             sold_ad2$   = fields$(05)
             sold_ad3$   = fields$(06)
             sold_ad4$   = fields$(07)
             sold_city$  = fields$(08)
             sold_state$ = fields$(09)
             sold_zip$   = fields$(10)
   	     rewrite #1, USING CUSTOMERFMT,                                  ~  
	   key0$, sort_name, sold_name$, sold_ad1$, sold_ad2$, sold_ad3$,    ~
		   sold_ad4$, sold_city$, sold_state$, sold_zip$,            ~
                   bill_name$, bill_ad1$, bill_ad2$, bill_ad3$,              ~
		   bill_ad4$, bill_city$, bill_state$, bill_zip$, status$
             goto L01000

bill_record:  /* update bill to address */

	     /* skip inactive billing addresses */
REM	   if str(fields$(3),1,1) <> "A" then goto L01000    
   	   if str(fields$(3),1,1) <> "A" and                               ~
   	      str(fields$(3),1,1) <> "I" then goto L01000    

	     /* skip active addr after first one */
             if mult_bill$ = "1" then goto L01000
             mult_bill$ = "1"

	     v3% = v3% + 1%
             if sold_sw$ = "0" then sold_name$ = "BILL-TO"
             sort_name$  = fields$(28) /* changed for Christie */
             bill_name$  = fields$(28)
             bill_ad1$   = fields$(04)
             bill_ad2$   = fields$(05)
             bill_ad3$   = fields$(06)
             bill_ad4$   = fields$(07)
             bill_city$  = fields$(08)
             bill_state$ = fields$(09)
             bill_zip$   = fields$(10)
   	     rewrite #1, USING CUSTOMERFMT,                                  ~  
	   key0$, sort_name$, sold_name$, sold_ad1$, sold_ad2$, sold_ad3$,   ~
		   sold_ad4$, sold_city$, sold_state$, sold_zip$,            ~
                   bill_name$, bill_ad1$, bill_ad2$, bill_ad3$,              ~
		   bill_ad4$, bill_city$, bill_state$, bill_zip$, status$
             goto L01000

L02000:  /* NOT FOUND */
             init(" ") rec$()
	     str(rec$(),1,9) = vendor$ 
REM   logmsg$ = "write"
REM  call "LOGFILE" (logmsg$)
             write #1,using FULLREC, rec$()
FULLREC:     FMT 6*CH(200)
             read #1,hold, key = vendor$, using FULLREC, rec$() 
             if str(fields$(2),1,4) <> "0000" then goto L03000     
	     v2% = v2% + 1%
             sold_name$  = fields$(28)
             sold_ad1$   = fields$(04)
             sold_ad2$   = fields$(05)
             sold_ad3$   = fields$(06)
             sold_ad4$   = fields$(07)
             sold_city$  = fields$(08)
             sold_state$ = fields$(09)
             sold_zip$   = fields$(10)
             key0$ = vendor$
   	     rewrite #1, USING CUSTOMERFMT,                                  ~  
	 key0$, sort_name$, sold_name$, sold_ad1$, sold_ad2$, sold_ad3$,       ~
		   sold_ad4$, sold_city$, sold_state$, sold_zip$,            ~
                   bill_name$, bill_ad1$, bill_ad2$, bill_ad3$,              ~
		   bill_ad4$, bill_city$, bill_state$, bill_zip$, status$
             goto L01000

L03000:
	     v3% = v3% + 1%
             sold_name$ = "BILL-TO"
             sort_name$  = fields$(28) /* changed for Christie */
             bill_name$  = fields$(28)
             bill_ad1$   = fields$(04)
             bill_ad2$   = fields$(05)
             bill_ad3$   = fields$(06)
             bill_ad4$   = fields$(07)
             bill_city$  = fields$(08)
             bill_state$ = fields$(09)
             bill_zip$   = fields$(10)
             key0$ = vendor$
   	     rewrite #1, USING CUSTOMERFMT,                                  ~  
        key0$, sort_name, sold_name$, sold_ad1$, sold_ad2$, sold_ad3$,       ~
		   sold_ad4$, sold_city$, sold_state$, sold_zip$,            ~
                   bill_name$, bill_ad1$, bill_ad2$, bill_ad3$,              ~
		   bill_ad4$, bill_city$, bill_state$, bill_zip$, status$
             goto L01000
	      
not_sorted: /* NOT SORTED */
            message$ = "NOT SORTED!!!!!!!!!!!" 
            call "SHOSTAT" (message$)
            goto END_JOB

END_JOB:    /* END OF JOB */
            convert v1% to v1$, pic (########)
            convert v2% to v2$, pic (########)
            convert v3% to v3$, pic (########)
            message$ = "READ=" & v1$ & " SOLD=" & v2$ & " BILL=" & v3$ 
            call "SHOSTAT" (message$)
            end
