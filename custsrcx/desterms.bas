REM         +---------------------------------------------------------------+
REM         | change terms of sale for Lowes                                |
REM         +---------------------------------------------------------------+


        dim                              /* FILE = APCPLNDT            */~
            hold_key$9,                  /* Detail Record              */~
            hold_type$4                    

        dim f2%(60%),                    /* = 0 if the file is open    */~
            f1%(60%),                    /* = 1 if READ was successful */~
            fs%(60%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(60%)20                 /* Text from file opening     */ 
            
            dim vendor$9, terms$20   
            dim v1$8, v2$8, v3$8, message$256

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

	     /* initialise variables */
            v1% = 0%
	    v2% = 0%
	    v3% = 0%
             sold_sw$ = "0"
	     mult_bill$ = "0"
             mult_rec$ = "0"
	    init(" ") init_rec$, hold_key$, hold_type$

   	     vendor$ = "LO       "                                
L01000:      /* main loop */

   	     read #1,hold,key > vendor$, USING CUSTOMERFMT,                  ~  
		   vendor$, terms$, eod goto END_JOB                         
CUSTOMERFMT: FMT CH(9), POS(543), CH(20)                                       

            /* check to see if input file sorted & set switches */
            if vendor$ < "LO0000   " then goto END_JOB               
            if vendor$ > "LO9999   " then goto END_JOB               

            terms$ = "1.5/30              "
   	     rewrite #1, USING CUSTOMERFMT,                                  ~  
		   vendor$, terms$                                            
             goto L01000

END_JOB:    /* END OF JOB */
            convert v1% to v1$, pic (########)
            convert v2% to v2$, pic (########)
            convert v3% to v3$, pic (########)
            message$ = "READ=" & v1$ & " SOLD=" & v2$ & " BILL=" & v3$ 
            end
