REM         +---------------------------------------------------+
REM         | set color in hnymaster                            |
REM         +---------------------------------------------------+


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

            select  #2, "AESPRDLS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 256,                                   ~
                        keypos = 1, keylen = 30,                         ~
                        alt key 1, keypos = 37, keylen = 25, dup,        ~
                            key 2, keypos = 31, keylen = 31, dup,        ~
                            key 3, keypos = 239, keylen = 13, dup  

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),0%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),0%, rslt$(2%))

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

            str(rec$(),187,1) = "0"         

            read #2, key 1% = key$, eod goto L01150

            a = len(key$)
            if a < 10 then goto L01150
            color_cd$ = str(key$,5%, 1%)
REM         if len(key$) <> 7 then goto L01150
REM         lb_color$ = "        "
REM         lb_descr$ = str(aes_rec$,62%,32%)
REM         if str(lb_descr$,1,6) = "BEIGE " then color_cd$ = "BEIGE"
REM         if str(lb_descr$,1,6) = "WHITE " then color_cd$ = "2"
REM         if str(lb_descr$,1,5) = "CLAY "  then color_cd$ = "7"
L01150:
            str(rec$(),187,1) = color_cd$
            rewrite #1, using L50760, rec$()
            goto L01000

L50760:     FMT 6*CH(150)                                                    
END_JOB:    end
