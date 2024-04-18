REM         +---------------------------------------------------------------+
REM         | fix part info in apcplndt                                     |
REM         +---------------------------------------------------------------+


        dim                              /* FILE = APCPLNDT            */~
            key$23,                      /* Detail Record              */~
            rec$256, rec2$64,            /* Detail Record              */~
            desc$30, status$2            /* Detail Record              */

        dim f2%(40%),                    /* = 0 if the file is open    */~
            f1%(40%),                    /* = 1 if READ was successful */~
            fs%(40%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(40%)20                 /* Text from file opening     */ 
            
            


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
            * #1  ! APCPLNDT !                                          *~
            * #2  ! GENCODES !                                          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,   "APCPLNDT",                                     ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =   24, keylen =   23,                    ~
                        alt key  1, keypos =   47, keylen =  57,         ~
                            key  2, keypos  =  53, keylen =  51,         ~
                            key  3, keypos  =   1, keylen =  23, dup,    ~
                            key  4, keypos  =  96, keylen =   8, dup

            select #5,  "APCPLNAD",                                      ~
                        varc,     indexed,  recsize =  64,               ~
                        keypos = 19,   keylen = 33,                      ~
                        alt key 1, keypos =  1, keylen = 33

            select #2,   "GENCODES",                                     ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =    1, keylen =   24                      

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),0%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),0%, rslt$(2%))
            call "OPENCHCK" (#5, fs%(5%), f2%(5%),0%, rslt$(5%))

            mat f1% = zer

	     init(" ") rec$, rec2$

L01000:      /* main loop */
             read #1, hold, using L50760, rec$,                            ~
                                        eod goto L02000 
             status$ = str(rec$,64,2)

/* don't update ones with data */
             if status$ > "  " then goto L01000

             str(rec$,64,2) = "14"
	     delete #1                      
	     write #1, using L50760, rec$
             goto L01000

L02000:      /* main loop */
             read #5, hold, using L50765, rec2$,                            ~
                                        eod goto END_JOB
             status$ = str(rec2$,32,2)

/* don't update ones with data */
             if status$ > "  " then goto L02000

             str(rec2$,32,2) = "14"
             str(rec2$,30,2) = "02"
             str(rec2$,60,3) = "FIX"
	     delete #5                      
	     write #5, using L50765, rec2$
             goto L02000

L50760:     FMT CH(256)
L50765:     FMT CH(64) 
END_JOB:    end
