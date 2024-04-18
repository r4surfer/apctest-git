REM         +---------------------------------------------------------------+
REM         | load accountmaster changes into CUSTOMER                      |
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
            dim v1$8, v2$8, v3$8, message$256
	    dim fields$(50)256, init_rec$256, wrk_rec$256
            dim key0$9, status$1, xref$9, sort_name$30         

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
	    csv_rec$ = "test2.csv"           
	    func$ = "open"
	    err% = 0%
             call "SEQREAD" (func$,csv_rec$,err%)
	     if err% <> 0% then goto END_JOB

	     /* initialise variables */
            v1% = 0%
	    v2% = 0%
	    v3% = 0%
             sold_sw$ = "0"
	     mult_bill$ = "0"
             mult_rec$ = "0"
	    init(" ") init_rec$, hold_key$

L01000:      /* main loop */
            hold_key$  = str(fields$(1),1,9)
	    func$ = "read"
	    csv_rec$ = init_rec$
	    err% = 0%
	    /* Read record */
             call "SEQREAD" (func$,csv_rec$,err%)
	     if err% <> 0% then goto END_JOB
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
		     if l > 33 then x = 255 
skip_it:
	     next x

            /* break out fields and store in 'fields$' table */
	     fields$(1) = str(wrk_rec$,1,(comma(1)-1)) & "         "
REM          for x = 2 to 30
             for x = 2 to 32
		 strlen = comma(x) - comma(x - 1) -1
	         fields$(x) = str(wrk_rec$,(comma(x-1)+1),strlen) & "                            "
    	     next x

   	     vendor$ = str(fields$(1),1,9) & "        "           

   	     read #1,hold,key = vendor$, USING CUSTOMERFMT,                  ~  
		    sort_name$, xref$, status$, eod goto L02000 
CUSTOMERFMT: FMT POS(10), CH(30), POS(780), CH(09), POS(793), CH(1)                      

	     v3% = v3% + 1%

REM   sort_name$ = fields$(03) /* changed to use ship to name */
	     xref$ = fields$(32)
	     status$ = fields$(14)
REM  message$ = "# " & vendor$ & "/" & sort_name$ & "/" & status$ & "/" & xref$
REM  call "LOGFILE" (message$)
    	     rewrite #1, USING CUSTOMERFMT,                                  ~  
		   sort_name$, xref$, status$ 

             goto L01000

L02000:  /* NOT FOUND */
REM         message$ = "NOTFND = " & vendor$   
REM         call "LOGFILE" (message$)
             goto L01000
	      
END_JOB:    /* END OF JOB */
            convert v1% to v1$, pic (########)
            convert v2% to v2$, pic (########)
            convert v3% to v3$, pic (########)
REM         message$ = "READ=" & v1$ & " SOLD=" & v2$ & " BILL=" & v3$ 
REM         call "LOGFILE" (message$)
            end
