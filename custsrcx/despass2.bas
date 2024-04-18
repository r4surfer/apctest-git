REM         +---------------------------------------------------------------+
REM         | fix part info in apccompt                                     |
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
            dim rec$(8)253            


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
            * #1  ! AWDPLNSR !                                          *~
            * #2  ! AWDPLNSX !                                          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1, "AWDPLNSR",                                       ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =   42, keylen =  12,                     ~
                        alt key  1, keypos =   7, keylen =  47,          ~
                            key  2, keypos = 163, keylen =  13,          ~
                            key  3, keypos =   1, keylen =  53

            select #2, "AWDPLNSX",                                       ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =   42, keylen =  12,                     ~
                        alt key  1, keypos =   7, keylen =  47,          ~
                            key  2, keypos = 163, keylen =  13,          ~
                            key  3, keypos =   1, keylen =  53,          ~
			    key  4, keypos = 205, keylen =  12, dup 

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),0%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),500%, rslt$(2%))

            mat f1% = zer
            cnt% = 0%
            cnt1% = 0%
            cnt2% = 0%
	     init(" ") rec$()        

L01000:      /* main loop */
             read #1, using L50760, rec$(),         ~
                                        eod goto END_JOB
             cnt% = cnt% + 1%
             write #2, using L50760, rec$()
             goto L01000

L50760:     FMT 8*CH(253)                                                    
END_JOB:    end
