REM         +---------------------------------------------------------------+
REM         | fix date in apcemply                                          |
REM         +---------------------------------------------------------------+


        dim                              /* FILE = APCPLNDT            */~
            ro_key$25,prc_key$15,        /* Detail Record              */~
            rec$(4)256                  

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
            * #1  ! APCEMPLY !                                          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************
            select #1,   "APCEMPLY",                                       ~
/*PAR000*/              varc,     indexed,  recsize = 1024,             ~
                        keypoS =    7, keylen =   5,                    ~
                        alt key 1, keypos =  1, keylen = 11, dup,       ~
			    key 2, keypos = 12, keylen = 26, dup

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),0%, rslt$(1%))

            mat f1% = zer
            rec_in = 0
            rec_mod = 0
	     init(" ") prc_key$

L01000:      /* main loop */
             read #1, hold, using L50760, rec$(),                      ~
                                        eod goto END_JOB
             rec_in = rec_in + 1

             if str(rec$(1),1018,1) = "N" then goto L01000                
             str(rec$(1),1018,1) = "N"                
             rewrite #1, using L50760, rec$(),                         ~
                                        eod goto END_JOB
             goto L01000

L50760:     FMT 4*CH(256)                                          
END_JOB:    end
