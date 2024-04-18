REM         +---------------------------------------------------------------+
REM         | Extract of awdbayex production file                           |
REM         +---------------------------------------------------------------+
        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDBAYEX                             *~
            *  Creation Date     -                                      *~
            *  Last Modified Date- 04/29/2013                           *~
            *  Written By        - Christie M. Gregory                  *~
            *  Last Modified By  -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *04/29/2013! (AWD001) mod for schema                  ! CMG *~
            *06/18/2015! SR66111  mod to consolidate NC & TX      ! PWW *~
            *          !          into a single program run.      !     *~
            *************************************************************

        dim                              /* FILE = APCPLNDT            */~
            pc_rec$128,                  /* Detail Record              */~
            prd_dte$6                    /* Production date            */
            
        dim schema$8                     /* Schema           (AWD001)  */                   

        dim f2%(40%),                    /* = 0 if the file is open    */~
            f1%(40%),                    /* = 1 if READ was successful */~
            fs%(40%),                    /* = 1 if file open, -1 if it */~
            r_rec$256,                   /*   not yet checked (OPENCHCK*/~
            rslt$(40%)20                 /* Text from file opening     */ 
            
        dim library$8,                   /* (SR66111)                  */~
            volume$6,                    /* (SR66111)                  */~
            vtoc$22,                     /* (SR66111)                  */~
            so$8,                        /* (SR66111)                  */~
            st$2                         /* (SR66111)                  */

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
            * #1  ! AWDBAYBW ! Pricing Master Definition File (New)     *~
            * #2  ! AWDBAYEX !                                          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "AWDBAYBW",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =    1, keylen =   29
/*SR66111 */            
            select #11, "AWDBAYBW",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =    1, keylen =   29

            select #2,  "AWDBAYEX",                                      ~
                        varc,     consec,  recsize = 256
                        
            select #4, "GENCODES",                                       ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24                              

            init(" ") axd$
            mat f1% = zer
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

/*SR66111   call "OPENCHCK" (#1, fs%(1%), f2%(1%),0%, rslt$(1%)) */
            call "OPENCHCK" (#4, fs%(4%), f2%(4%),0%, rslt$(4%)) /* (AWD001) */  
/* (AWD001) */            
            schema_err%, schema% = 0%
            init(" ") schema$
            call "SCHEMA" (schema$, schema%, #4, schema_err%)            
            
                                  
REM         call "OPENCHCK" (#2, fs%(2%), f2%(2%),42%, rslt$(2%))
            library$        = "ORAFILES"
            volume$         = "CARLO2"
/* (AWD001) */            
/*SR66111   if schema% = 2% then volume$ = "NE2" */
            file$           = "AWDBAYEX"
            call "OPENFILE" (#2, "IO   ", f2%(2%), rslt$(2%), axd$ )
            if f2%(2%) <> 0% then goto L01100
            call "FILEBGON" (#2)

L01100:    open nodisplay #2, output, space = 100%,                    ~
                dpack   = 100%, ipack = 100%, file = file$,              ~
                library = library$, volume = volume$, blocks = 5%


           ff% = 1% : RCNT = 0                      /*SR66111 */
           ncntx$ = "NC "
L01000:      /* main loop */
	    init(" ") pc_rec$
            read #ff%, hold, using L50760, pc_rec$,          ~
                                                      eod goto L56890
             goto L61000     /* disable for testing                     */
             RCNT = RCNT + 1
             if MOD(RCNT,100) <> 0 then goto L61000
             convert RCNT to RCNT$, pic (00000000)
             call "SHOSTAT" ("Processing... " & ncntx$ & RCNT$)
        /*   if RCNT >= 10000 then goto read_hist_done */
L61000:   

            if str(pc_rec$,50,1) <> "S" then goto L01000

            get pc_rec$, using breakoutA, prd_dte$
breakoutA: FMT POS(24), CH(06)

            if str(prd_dte$,1,6) =  " " then goto L01000


	    cnt% = cnt% + 1
	    init(" ") r_rec$
            get pc_rec$, using breakout, dateNbr
breakout: FMT POS(24), PD(11,1)

            convert int(dateNbr) to dateStr$, pic (########)

            str(r_rec$,1,19) = str(pc_rec$,1,18) & "|" 
            str(r_rec$,20,4) = str(pc_rec$,19,3) & "|"
            str(r_rec$,24,3) = str(pc_rec$,22,2) & "|" 
            str(r_rec$,27,9) = dateStr$ & "|"
            str(r_rec$,36,9) = str(pc_rec$,30,8) & "|" 
            str(r_rec$,45,6) = str(pc_rec$,38,5) & "|" 
            str(r_rec$,51,3) = str(pc_rec$,43,2) & "|" 
            str(r_rec$,54,3) = str(pc_rec$,45,2) & "|" 
            str(r_rec$,57,4) = str(pc_rec$,47,3) & "|" 
            str(r_rec$,61,26) = str(pc_rec$,51,25) & "|"
            str(r_rec$,87,9) = str(pc_rec$,76,8) & "|" 
            str(r_rec$,96,6) = str(pc_rec$,84,5) & "|" 

      	    write #2, using EXTRACT, r_rec$


EXTRACT:  FMT CH(256)

            str(pc_rec$,50,1) = "T"

            rewrite #ff%, using L50760, pc_rec$, ~
		     eod goto L01000

	    upd% = upd% + 1
            goto L01000

L50760:     FMT CH(128)
L56890:     if ntx_done% > 0% then goto all_done
            ff% = 11%        /*SR66111  */
            ntx_done% = 1%
            ncntx$ = "NTX "
            RCNT = 0
            goto L01000


     all_done
         end
	      
            file_error
               err% = 2%
 
            end


