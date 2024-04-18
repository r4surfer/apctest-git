REM         +---------------------------------------------------------------+
REM         | fix part info in apccompt                                     |
REM         +---------------------------------------------------------------+


        dim                              /* FILE = APCPLNDT            */~
            edi_key$13,                 /* Detail Record              */~
            edi_key2$22,                /* Detail Record              */~
            tmp_key2$22,                /* Detail Record              */~
            tmp_key$13                  /* temp file key              */

        dim f2%(40%),                    /* = 0 if the file is open    */~
            f1%(40%),                    /* = 1 if READ was successful */~
            fs%(40%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(40%)20                 /* Text from file opening     */ 
            
            dim edi_rec$256                             
            


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
            * #1  ! APCEDIMT !                                          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1, "APCEDIST",          ~                             
                        varc,     indexed,  recsize = 256,               ~
                        keypos =   1 , keylen =  22,                     ~
                        alt key    1 , keypos =  10, keylen =  13, dup,  ~
                            key    2 , keypos = 233, keylen =  06, dup

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),0%, rslt$(1%))

            mat f1% = zer
            cnt% = 0%  
            cnt2% = 0%  
	     init(" ") edi_key$

PHASE2: 
             edi_key2$ = "TI112304003063531"
             tmp_key2$ = "TI112304003063562"

L03000:      read #1,hold, key > edi_key2$, using FMT001, edi_rec$,   ~
                           eod goto END_JOB 
             edi_key2$ = str(edi_rec$,1,22)
	     if str(edi_key2$,1,17) > str(tmp_key2$,1,17) then goto END_JOB
             if str(edi_key2$,7,3) <> "040" then goto L03000
REM          if str(edi_key2$,10,8) < "02632550" then goto L03000
REM	     if str(edi_key2$,10,8) > "02632999" then goto L03000
             delete #1
             str(edi_rec$,1,1) = "S"
            cnt2% = cnt2% + 1%
            write #1,using FMT001,edi_rec$
             print at(10,10), edi_key22$
             goto L03000
TEMPEDI:     FMT CH(13)
FMT001:      FMT CH(256)                    

END_JOB:        
            end
