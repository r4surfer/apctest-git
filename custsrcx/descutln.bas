REM         +---------------------------------------------------------------+
REM         | set cut lenghth field in hnymaster                            |
REM         +---------------------------------------------------------------+


        dim                              /* FILE = APCPLNDT            */~
            comp_key$11,                 /* Detail Record              */~
            bck_key$11,                 /* Detail Record              */~
            inv_num$8,                  /* Detail Record              */~
            part_info$20                   

        dim f2%(40%),                    /* = 0 if the file is open    */~
            f1%(40%),                    /* = 1 if READ was successful */~
            fs%(40%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(40%)20                 /* Text from file opening     */ 
            
            dim comp_so$16, comp_line$3                  
            dim rec$(6)150,key$25            
            dim aes_descr$32


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
            * #1  ! HNYMASTR !                                          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  900,                                  ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),0%, rslt$(1%))

            mat f1% = zer
            cnt% = 0%
            cnt1% = 0%
            cnt2% = 0%
	    init(hex(00)) key$
	     init(" ") rec$()        

L01000:      /* main loop */
             read #1, hold, using L50760, rec$(),         ~
                                        eod goto END_JOB
             cnt% = cnt% + 1%

            key$       = str(rec$(),01,25)
            aes_descr$ = str(rec$(),26,32)

            init(" ") aes_cut$,x$, y$
            x$ = bin(34%,1)                /* Suff a '"' Quote into X$  */
            y$ = "'"                       /* Set to ''' = Single Quote */
                                           /* 1st Check Inches          */
            p% = pos(aes_descr$ = x$ )
            if p% = 0% then goto PO_9                            
            aes_cut$ = str(aes_descr$,p%-4%, 4%)
	    for l = 1 to 4
		if str(aes_cut$,l,1) < "0" or                   ~
		   str(aes_cut$,l,1) > "9" then str(aes_cut$,l,1) = "0"
            next l
            str(rec$(),183,4) = aes_cut$

PO_9:
            if p% <> 0% then goto PO_10
                                           /* 2nd Check Feet            */
            p% = pos(aes_descr$ = y$ )

            if p% = 0% then goto PO_10                           
            aes_cut$ = str(aes_descr$,p%-4%, 5%)
	    for l = 1 to 4
		if str(aes_cut$,l,1) < "0" or                   ~
		   str(aes_cut$,l,1) > "9" then str(aes_cut$,l,1) = "0"
            next l
            aes_cut  = 0                         
            convert str(aes_cut$,1,4) to aes_cut, data goto PO_10
            aes_cut = aes_cut * 12 +.5
	    convert aes_cut to aes_cut$, pic (0000)
            str(rec$(),183,4) = aes_cut$
PO_10:

             rewrite #1, using L50760, rec$()
             goto L01000

L50760:     FMT 6*CH(150)                                                    
END_JOB:    end
