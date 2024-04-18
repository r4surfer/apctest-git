REM         +---------------------------------------------------------------+
REM         | delete old records from awdbaybw                              |
REM         +---------------------------------------------------------------+

/*          *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *06/22/2015! SR66111  mod to consolidate NC & TX      ! PWW *~
            *          !          into a single program run.      !     *~
            ************************************************************* */


        dim                              /* FILE = AWDBAYBW            */~
            pc_rec$128, pc_key$29        /* Detail Record              */

        dim date$6, pg_dte$6, prd_dte$6

        dim f2%(40%),                    /* = 0 if the file is open    */~
            f1%(40%),                    /* = 1 if READ was successful */~
            fs%(40%),                    /* = 1 if file open, -1 if it */~
            rslt$(40%)20                 /* Text from file opening     */ 
                                                  
        dim library$8,                   /* (SR66111)                  */~
            volume$6,                    /* (SR66111)                  */~
            vtoc$22,                     /* (SR66111)                  */~
            so$8,                        /* (SR66111)                  */~
            st$2                         /* (SR66111)                  */

            mat f2% = con
            mat f1% = zer

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! AWDBAYBW ! Scanning Log File to send to oracle      *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "AWDBAYBW",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =    1, keylen =   29
/*SR66111 */
            select #11,  "AWDBAYBW",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =    1, keylen =   29

            mat f2% = zer
            mat fs% = zer
/*SR66111 + */
               x% = 1%
               init(" ") filename$, library$, volume$, vtoc$
               filename$ = "AWDBAYBW" 
               library$  = "APCDATA " 
               volume$   = "?" 

               call "FIND" addr(filename$, library$, volume$,1%,x%,vtoc$)
               if vtoc$ = " " then goto file_error
                  volume$ = vtoc$
                open nogetparm #1,                                       ~
                     shared,                                             ~
                     file    = filename$,                                ~
                     library = library$,                                 ~
                     volume  = volume$

               x% = 1%
               init(" ") filename$, library$, volume$, vtoc$
               filename$ = "AWDBAYBW" 
               library$  = "NEDATA" 
               volume$   = "?" 
               call "FIND" addr(filename$, library$, volume$,1%,x%,vtoc$)
               if vtoc$ = " " then goto file_error
                  volume$ = vtoc$
                open nogetparm #11,                                      ~
                     shared,                                             ~
                     file    = filename$,                                ~
                     library = library$,                                 ~
                     volume  = volume$
/*SR66111 - */

/*SR66111   call "OPENCHCK" (#1, fs%(1%), f2%(1%),0%, rslt$(1%))  */

            init(" ") date$, pg_dte$,prd_dte$
            date$ = date
            err% = 0%                        /* More than 7 Days Old  */ 
            call "DATE" addr("G+",date$, -7%,pg_dte$,err%)
            ff% = 1% : RCNT = 0                      /*SR66111 */
            ncntx$ = "NC "

L00900:     pc_key$, pc_rec$ = all(hex(00))

            read #ff%, hold, key > pc_key$, using L50760, pc_rec$,       ~
                                                      eod goto L56890

                goto first
L01000:      /* main loop */
            read #ff%, hold, using L50760, pc_rec$, eod goto L56890

first:
             RCNT = RCNT + 1
             goto L61000             /* disable for testing              */
             if MOD(RCNT,100) <> 0 then goto L61000
             convert RCNT to RCNT$, pic (00000000)
             call "SHOSTAT" ("Processing... " & ncntx$ & RCNT$)
        /*   if RCNT >= 10000 then goto read_hist_done */
L61000:   

            pc_key$ = str(pc_rec$,1,29)

REM               print at(02,02);hex(84); str(pc_key$,1,18)
REM               print at(03,02);hex(84); str(pc_key$,19,3)
REM               print at(04,02);hex(84); str(pc_key$,22,2)

	    cnt% = cnt% + 1

            if str(pc_rec$,50,1) = "S" then goto L01000


            get pc_rec$, using breakout, prd_dte$
breakout: FMT POS(24), CH(06)

            if str(prd_dte$,1,6) =  " " then goto del_blank

            if (str(prd_dte$,1,6) > str(pg_dte$,1,6) ) then goto L01000

del_blank:

            delete #ff%
	    upd% = upd% + 1
            goto L01000


L50760:     FMT CH(128)
L56890:     if ntx_done% > 0% then goto all_done
            ff% = 11%        /*SR66111  */
            ntx_done% = 1%
            ncntx$ = "NTX "
            RCNT = 0
            goto L00900


     all_done
 
REM         print "         records read    = ", cnt%
REM         print "         records test    = ", tst%
REM         print "         records updated = ", upd%

             end

            file_error
               err% = 2%
 
            end


	      

