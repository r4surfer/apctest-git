        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLN41                             *~
            *  Creation Date     - 06/27/08                             *~
            *  Last Modified Date- 04/03/2013                           *~
            *  Written By        - Christie M. Gregory                  *~
            *  Last Modified By  -                                      *~
            *                                                           *~
            *  Description       - New program to create daily history  *~
            *                      data to feed oracle warehouse.       *~
            *                      Data collected from ORABOL           *~
            *                                          ORAINV           *~
            *                      Populates files     DAYBOL           *~
            *                                          DAYINV           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 09/20/08 ! New Program for (AWD) - Last Mod Date    ! CMG *~
            *04/03/2013! (AWD001) mod for schema                  ! CMG *~
            *06/18/2015! SR66111  mod to consolidate NC & TX      ! PWW *~
            *          !          into a single program run.      !     *~
            *04/30/2021! CR2829 Add GS1-128                       ! RDB *~
            *************************************************************


        dim                              /* (Program) - Variables      */~
            filename$8,                  /* Used By EWDOPEN            */~
            cnt$28,                      /* Screen Display             */~
            hdr$45, msg$(3%)79,          /* Askuser - Var's            */~
            date$8,                      /* REPORT TITLE               */~
            userid$3                     /* Current User Id            */

        dim                                                              ~  
            readkey$100,                 /* Readkey                    */~
            readkey1$100,                /* Generic Readkey 1          */~
            daybol_key$100,              /* Day bol readkey            */~
            dayinv_key$100,              /* Day Inv readkey            */~ 
            orabol_rec$(2)256,           /* Orabol Rec                 */~
            fields$(500%)256,            /* Generic Fields             */~
            num_fields(500%)             /* Generic Fields             */ 

        dim                                                              ~
            so_number$8,                 /* SO number                  */~
            so_seqnr$3,                  /* SO Seqnr                   */~
            hdisc$14,                    /* Header Disc                */~
            ldisc$14,                    /* Line Disc                  */~
            sqty$14,                     /* Ship Qty                   */~
            uprice$14,                   /* Inv Unit Price             */~
            ieprice$14,                  /* Inv Extended Price         */~
            part_num$25,                 /* Part Number                */~
            inv_type$1,                  /* INvoice Type               */~
            part_desc$30                 /* Part Desc                  */

        dim schema$8                     /* Schema           (AWD001)  */  

/* CR2829 */
        dim gs128$20                     /* GS1-128 barcode number    */        

        dim f2%(15%),                    /* = 0 if the file is open    */~
            f1%(15%),                    /* = 1 if READ was successful */~
            fs%(15%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(15%)20                 /* Text from file opening     */

        dim comma$1,                     /* Comma for EWDAPPSN File    */~
            file$8,                      /* File Name                  */~
            library$8,                   /* Library Name = APCDATA     */~
            volume$6                     /* DISK VOLUME = CARLOS       */

        dim vtoc$22,                     /* (SR66111)                  */~
            so$8,                        /* (SR66111)                  */~
            st$2                         /* (SR66111)                  */



        REM *************************************************************~
            *                     S E L E C T                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************
            
            select #4, "GENCODES",                                       ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24                

            select #6,  "ARIMASTR",                                      ~
                        varc,     indexed,  recsize = 2000,              ~
                        keypos =    1, keylen =  16,                     ~
                        alt key  1, keypos =   18, keylen =  12, dup,    ~
                            key  2, keypos =   31, keylen =  25, dup,    ~
                            key  3, keypos =   17, keylen =  39, dup


/*SR66111*/ select #16,  "ARIMASTR",                                     ~
                        varc,     indexed,  recsize = 2000,              ~
                        keypos =    1, keylen =  16,                     ~
                        alt key  1, keypos =   18, keylen =  12, dup,    ~
                            key  2, keypos =   31, keylen =  25, dup,    ~
                            key  3, keypos =   17, keylen =  39, dup

            select #7,  "ARILINES",                                      ~
                        varc,     indexed,  recsize =  750,              ~
                        keypos =    1, keylen =  19,                     ~
                        alt key  1, keypos =   46, keylen =  19, dup   

/*SR66111*/ select #17,  "ARILINES",                                     ~
                        varc,     indexed,  recsize =  750,              ~
                        keypos =    1, keylen =  19,                     ~
                        alt key  1, keypos =   46, keylen =  19, dup   

            select #13, "ORABOL",                                        ~
                        varc,     indexed,  recsize = 512,               ~
                        keypos =    1, keylen =  32,                     ~
                        alt key  1, keypos =   13, keylen =  20, dup,    ~
                            key  2, keypos =  186, keylen =   8, dup,    ~
                            key  3, keypos =   22, keylen =  11   

/*SR66111*/ select #23, "ORABOL",                                        ~
                        varc,     indexed,  recsize = 512,               ~
                        keypos =    1, keylen =  32,                     ~
                        alt key  1, keypos =   13, keylen =  20, dup,    ~
                            key  2, keypos =  186, keylen =   8, dup,    ~
                            key  3, keypos =   22, keylen =  11   


            select #14, "ORAINV",                                        ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =    7, keylen =  18,                     ~
                       alt key   1, keypos =    1, keylen =  24     

/*SR66111*/ select #24, "ORAINV",                                        ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =    7, keylen =  18,                     ~
                       alt key   1, keypos =    1, keylen =  24     



            select #25, "DAYBOL",                                       ~
                        varc,     consec, recsize = 512


            select #26, "DAYINV",                                       ~
                        varc,     consec, recsize = 256

/* (AWD001) */
            filename$ = "GENCODES" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error            
/*SR66111   filename$ = "ARIMASTR" : call "EWDOPEN" (#6, filename$, err%)~
            if err% <> 0% then gosub open_error                          ~
            filename$ = "ARILINES" : call "EWDOPEN" (#7, filename$, err%)~
            if err% <> 0% then gosub open_error                          ~
                                                                         ~
            filename$ = "ORABOL" : call "EWDOPEN" (#13, filename$, err%) ~
            if err% <> 0% then gosub open_error                          ~
            filename$ = "ORAINV" : call "EWDOPEN" (#14, filename$, err%) ~
            if err% <> 0% then gosub open_error          SR66111       */

/*SR66111 + */
               x% = 1%
               init(" ") filename$, library$, volume$, vtoc$
               filename$ = "ARIMASTR" 
               library$  = "APCDATA " 
               volume$   = "?" 
               call "FIND" addr(filename$, library$, volume$,1%,x%,vtoc$)
               if vtoc$ = " " then goto file_error
                  volume$ = vtoc$
                open nogetparm #6,                                       ~
                     shared,                                             ~
                     file    = filename$,                                ~
                     library = library$,                                 ~
                     volume  = volume$

               x% = 1%
               init(" ") filename$, library$, volume$, vtoc$
               filename$ = "ARIMASTR" 
               library$  = "NEDATA" 
               volume$   = "?" 
               call "FIND" addr(filename$, library$, volume$,1%,x%,vtoc$)
               if vtoc$ = " " then goto file_error
                  volume$ = vtoc$
                open nogetparm #16,                                      ~
                     shared,                                             ~
                     file    = filename$,                                ~
                     library = library$,                                 ~
                     volume  = volume$
                     
               x% = 1%
               init(" ") filename$, library$, volume$, vtoc$
               filename$ = "ARILINES" 
               library$  = "APCDATA " 
               volume$   = "?" 
               call "FIND" addr(filename$, library$, volume$,1%,x%,vtoc$)
               if vtoc$ = " " then goto file_error
                  volume$ = vtoc$
                open nogetparm #7,                                       ~
                     shared,                                             ~
                     file    = filename$,                                ~
                     library = library$,                                 ~
                     volume  = volume$

               x% = 1%
               init(" ") filename$, library$, volume$, vtoc$
               filename$ = "ARILINES" 
               library$  = "NEDATA" 
               volume$   = "?" 
               call "FIND" addr(filename$, library$, volume$,1%,x%,vtoc$)
               if vtoc$ = " " then goto file_error
                  volume$ = vtoc$
                open nogetparm #17,                                      ~
                     shared,                                             ~
                     file    = filename$,                                ~
                     library = library$,                                 ~
                     volume  = volume$
                     
               x% = 1%
               init(" ") filename$, library$, volume$, vtoc$
               filename$ = "ORABOL" 
               library$  = "APCDATA " 
               volume$   = "?" 
               call "FIND" addr(filename$, library$, volume$,1%,x%,vtoc$)
               if vtoc$ = " " then goto file_error
                  volume$ = vtoc$
                open nogetparm #13,                                      ~
                     shared,                                             ~
                     file    = filename$,                                ~
                     library = library$,                                 ~
                     volume  = volume$

               x% = 1%
               init(" ") filename$, library$, volume$, vtoc$
               filename$ = "ORABOL" 
               library$  = "NEDATA" 
               volume$   = "?" 
               call "FIND" addr(filename$, library$, volume$,1%,x%,vtoc$)
               if vtoc$ = " " then goto file_error
                  volume$ = vtoc$
                open nogetparm #23,                                      ~
                     shared,                                             ~
                     file    = filename$,                                ~
                     library = library$,                                 ~
                     volume  = volume$

               x% = 1%
               init(" ") filename$, library$, volume$, vtoc$
               filename$ = "ORAINV" 
               library$  = "APCDATA " 
               volume$   = "?" 
               call "FIND" addr(filename$, library$, volume$,1%,x%,vtoc$)
               if vtoc$ = " " then goto file_error
                  volume$ = vtoc$
                open nogetparm #14,                                      ~
                     shared,                                             ~
                     file    = filename$,                                ~
                     library = library$,                                 ~
                     volume  = volume$

               x% = 1%
               init(" ") filename$, library$, volume$, vtoc$
               filename$ = "ORAINV" 
               library$  = "NEDATA" 
               volume$   = "?" 
               call "FIND" addr(filename$, library$, volume$,1%,x%,vtoc$)
               if vtoc$ = " " then goto file_error
                  volume$ = vtoc$
                open nogetparm #24,                                      ~
                     shared,                                             ~
                     file    = filename$,                                ~
                     library = library$,                                 ~
                     volume  = volume$

/*SR66111 - */

            
            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************


            call "EXTRACT" addr("ID", userid$)
            date$ = date
/* (AWD001) */
            schema_err%, schema% = 0%
            init(" ") schema$
            call "SCHEMA" (schema$, schema%, #4, schema_err%)                

            call "DATEFMT" (date$)
            mat num_fields     = zer

REM 0% = extract entire file and 1% = only new daily data
            extract% = 1%


            file$   = "DAYBOL" 
            ff% = 25%  
            gosub open_file

            file$   = "DAYINV" 
            ff% = 26%  
            gosub open_file

/*SR66111 + */
            ff1% = 6%
            ff2% = 7%
            ff3% = 13%
            ff4% = 14%
            ncntx$ = "NC "
        begin_files_analysis
            gosub initialize_variables
            gosub files_analysis
            if ff1% > 6% then goto exit_pgm
            ff1% = 16%
            ff2% = 17%
            ff3% = 23%
            ff4% = 24%
            ncntx$ = "NTX "
            goto begin_files_analysis
            
        exit_pgm
              goto exit_program



        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables
            init(" ") readkey$, fields$(), filename$, cnt$, hdr$, msg$(),~
                      date$, library$, volume$, readkey1$



        return

        REM *************************************************************~
            *************************************************************




        files_analysis
            cnt% = 0%
            comma$ = "|"

            init(" ") readkey$, fields$()
            readkey$ = all(hex(00))
REM            str(readkey$,1,9)  = "LO0537"
REM            str(readkey$,10,8) = "02298688"

            gosub read_orabol
            gosub read_orainv

        return



        read_orabol
REM            call "SHOSTAT" ("READ ORABOL ") 
            done% = 0% : RCNT = 0
            mat num_fields = zer
        read_orabol_nxt
            read #ff3%, key > readkey$, eod goto read_orabol_done 

               cnt% = cnt% + 1%
             goto L61000            /*  disable for testing                */
             RCNT = RCNT + 1
             if MOD(RCNT,100) <> 0 then goto L61000
             convert RCNT to RCNT$, pic (00000000)
             call "SHOSTAT" ("Processing orabol... " & ncntx$ & RCNT$)
        /*   if RCNT >= 10000 then goto read_hist_done */
L61000:   



               get #ff3%, using orabol_fmt, readkey$
orabol_fmt:          FMT CH(32)

            if str(readkey$,1,1) <> "S" then goto read_orabol_done

            goto L34990
            if mod(cnt%,50%) <> 0% then goto L34990
               convert cnt% to str(cnt$,19%,8%), pic(########)
               print at(02,02);hex(84);cnt$;

L34990:
                get #ff3%, using L35000, fields$(1), fields$(2), fields$(3), ~
                       fields$(4), fields$(5), fields$(6), fields$(7),    ~
                       fields$(8), fields$(9), fields$(10), fields$(11),  ~
                       fields$(12), num_fields(13), num_fields(14),       ~
                       num_fields(15), num_fields(16), num_fields(17),    ~
                       num_fields(18), fields$(19), fields$(20),          ~
                       fields$(21),  fields$(22), fields$(23), fields$(24),~
                       fields$(25), fields$(26), fields$(27), fields$(28) 
/* CR2829 */

L35000:           FMT CH(01), CH(06), CH(05), CH(09), CH(08), CH(03), CH(06),~
                      CH(06), CH(10), CH(25), CH(20), CH(20), PD(14,4),      ~
                      PD(14,4), PD(14,4), PD(14,4), PD(14,4), PD(14,4),      ~
                      CH(04), CH(04), CH(04), CH(06), CH(08), CH(16), CH(16),~
                      CH(10), CH(10), CH(20)


            call "DATFMTC" (fields$(7), date%, fields$(7))    /* order date */
            call "DATFMTC" (fields$(8), date%, fields$(8))    /* ship date  */
            call "DATFMTC" (fields$(22), date%, fields$(22))  /* Due date  */

            num_fields(9) = 0
            convert str(fields$(9),2,9) to num_fields(9), data goto bad_quote

bad_quote:

            convert num_fields(9) to fields$(9), pic(#########0)



            convert num_fields(13) to fields$(13), pic(-00000000.0000)
            convert num_fields(14) to fields$(14), pic(-00000000.0000)
            convert num_fields(15) to fields$(15), pic(-00000000.0000)
            convert num_fields(16) to fields$(16), pic(-00000000.0000)
            convert num_fields(17) to fields$(17), pic(-00000000.0000)
            convert num_fields(18) to fields$(18), pic(-00000000.0000)

            if fields$(24) < " " then fields$(24) = " "
            if fields$(25) < " " then fields$(25) = " "
/* CR2829 */
            if str(fields$(28),1%,4%) <>  "0000" then init(" ") fields$(28)

               gosub write_daybol
               if extract% = 1% then gosub update_orabol
              goto read_orabol_nxt
        read_orabol_done
             done% = 1%
        return

        update_orabol

            read #ff3%, hold, key = readkey$, using orabol_fmt1,        ~
                           orabol_rec$(), eod goto update_bol_done

orabol_fmt1:         FMT 2*CH(256)

                delete #ff3%

                str(orabol_rec$(),1,1) = "T"
                str(orabol_rec$(),2,6) = date

                put #ff3%, using orabol_fmt2, orabol_rec$()

                write #ff3%

orabol_fmt2:          FMT 2*CH(256)
        update_bol_done
        return


        write_daybol

            put #25, using L35800, fields$(3),    /* Load                 */~
                                   comma$,        /* "|" Delimiter        */~
                                   fields$(4),    /* Cuscode              */~
                                   comma$,        /* "|" Delimiter        */~
                                   fields$(5),    /* SO                   */~
                                   comma$,        /* "|" Delimiter        */~
                                   fields$(6),    /* Seq                  */~
                                   comma$,        /* "|" Delimiter        */~  
                                   fields$(7),    /* Order Date           */~
                                   comma$,        /* "|" Delimiter        */~
                                   fields$(8),    /* Ship Date            */~
                                   comma$,        /* "|" Delimiter        */~
                                   fields$(9),    /* Quote Number         */~
                                   comma$,        /* "|" Delimiter        */~
                                   fields$(10),   /* Part                 */~
                                   comma$,        /* "|" Delimiter        */~
                                   fields$(11),   /* Sub part             */~
                                   comma$,        /* "|" Delimiter        */~
                                   fields$(12),   /* Info part            */~
                                   comma$,        /* "|" Delimiter        */~
                                   fields$(13),   /* Order Qty            */~
                                   comma$,        /* "|" Delimiter        */~
                                   fields$(14),   /* Ship Qty             */~
                                   comma$,        /* "|" Delimiter        */~ 
                                   fields$(15),   /* Order Disc Percent   */~
                                   comma$,        /* "|" Delimiter        */~ 
                                   fields$(16),   /* Line Disc Percent    */~
                                   comma$,        /* "|" Delimiter        */~
                                   fields$(17),   /* Price                */~
                                   comma$,        /* "|" Delimiter        */~ 
                                   fields$(18),   /* Ext Price            */~
                                   comma$,        /* "|" Delimiter        */~
                                   fields$(19),   /* Category             */~
                                   comma$,        /* "|" Delimiter        */~
                                   fields$(20),   /* Region               */~
                                   comma$,        /* "|" Delimiter        */~
                                   fields$(21),   /* Salesman             */~
                                   comma$,        /* "|" Delimiter        */~
                                   fields$(22),   /* Due Date             */~
                                   comma$,        /* "|" Delimiter        */~
                                   fields$(23),   /* Invoice              */~
                                   comma$,        /* "|" Delimiter        */~
                                   fields$(24),   /* Job                  */~
                                   comma$,        /* "|" Delimiter        */~
                                   fields$(25),   /* PO                   */~
                                   comma$,        /* "|" Delimiter        */~
                                   fields$(28),   /* GS1-128   CR2829     */~
                                   comma$,        /* "|" Delimiter        */~
                                   " "            /* Filler               */

                write #25

        return
L35800:      FMT CH(05), CH(01), CH(09), CH(01), CH(08), CH(01), CH(03), CH(01), ~
                 CH(10), CH(01), CH(10), CH(01), CH(10), CH(01), CH(25), CH(01), ~
                 CH(20), CH(01), CH(20), CH(01), CH(14), CH(01), CH(14), CH(01), ~
                 CH(14), CH(01), CH(14), CH(01), CH(14), CH(01), CH(14), CH(01), ~ 
                 CH(04), CH(01), CH(04), CH(01), CH(04), CH(01), CH(10), CH(01), ~
                 CH(08), CH(01), CH(16), CH(01), CH(16), CH(01), CH(20), CH(01), ~
                 CH(224)


        read_orainv
REM         call "SHOSTAT" ("READ ORAINV ") 
            done% = 0% : RCNT = 0
            mat num_fields = zer
            init(" ") readkey$
        read_orainv_nxt
            read #ff4%, key > readkey$, eod goto read_orainv_done 

               cnt% = cnt% + 1%

             goto L61001                /* disable for testing              */
             RCNT = RCNT + 1
             if MOD(RCNT,100) <> 0 then goto L61001
             convert RCNT to RCNT$, pic (00000000)
             call "SHOSTAT" ("Processing orainv ... " & ncntx$ & RCNT$)
        /*   if RCNT >= 10000 then goto read_hist_done */
L61001:   


               get #ff4%, using orainv_fmt, readkey$
orainv_fmt:          FMT POS(07), CH(18)

            if str(readkey$,1,1) <> "S" then goto read_orainv_done
            goto L34995
            if mod(cnt%,50%) <> 0% then goto L34995
               convert cnt% to str(cnt$,19%,8%), pic(########)
               print at(02,02);hex(84);cnt$;

L34995:
               gosub read_arimastr
               gosub read_arilines


               goto read_orainv_nxt

        read_orainv_done
             done% = 1%
        return

        read_arimastr
           init(" ") readkey1$
           str(readkey1$,1,17) = str(readkey$,2,17)
           read #ff1%, key = readkey1$, using arimastr_fmt, so_number$, ~
                          post_date$, hdisc, inv_type$, eod goto arimastr_done

arimastr_fmt:        FMT POS(34), CH(8), POS(533), CH(06), POS(801), ~
                           PD(15,4), POS(891), CH(1)

                  call "DATFMTC" (post_date$, date%, post_date$)
                  convert hdisc to hdisc$, pic(-00000000.0000)

        arimastr_done
        return

        read_arilines
            init(" ") readkey1$
            str(readkey1$,1,17) = str(readkey$,2,17)
        read_arilines_next
            read #ff2%, key > readkey1$, using arilines_fmt, readkey1$, ~
                                            eod goto arilines_done
arilines_fmt:     FMT CH(20)

                  if str(readkey1$,1,17) <> str(readkey$,2,17) then ~ 
                                            goto arilines_done
                   get #ff2%, using arilines_fmt1, part_num$, part_desc$, ~
                          sqty, uprice, ldisc, so_seqnr$, gs128$
/* CR2829 */
arilines_fmt1:     FMT POS(24), CH(25), CH(30), POS(93), PD(14,4),     ~
                       POS(133), PD(14,4), PD(15,4), POS(194), CH(03), ~
                       POS(688), CH(20)

                  convert sqty to sqty$, pic(-00000000.0000)
                  convert ldisc to ldisc$, pic(-00000000.0000)

                  ieprice = 0.00
                  if sqty <= 0 then goto no_ship
                  ieprice = round(sqty * uprice, 2)

                  dsc_amt = 0 - round(uprice * ldisc * .01, 2)
                  ieprice = round(ieprice + dsc_amt, 2)

                  dsc_amt   = 0 - round(uprice * hdisc * .01, 2)
                  ieprice = round(ieprice + dsc_amt, 2)
no_ship:

                  convert uprice  to uprice$,  pic(-00000000.0000)

                  convert ieprice to ieprice$, pic(-00000000.0000)

               gosub write_dayinv
               goto read_arilines_next
        arilines_done
        return

        write_dayinv
                put #26, using dayinv_fmt,                                  ~
                                   str(readkey1$,10,8),                     ~
                                   comma$,        /* "|" Delimiter        */~
                                   str(readkey1$,18,3),                     ~
                                   comma$,        /* "|" Delimiter        */~
                                   so_seqnr$,                               ~
                                   comma$,        /* "|" Delimiter        */~
                                   post_date$,                              ~
                                   comma$,        /* "|" Delimiter        */~
                                   hdisc$,                                  ~
                                   comma$,        /* "|" Delimiter        */~
                                   ldisc$,                                  ~
                                   comma$,        /* "|" Delimiter        */~
                                   sqty$,                                   ~
                                   comma$,        /* "|" Delimiter        */~
                                   uprice$,                                 ~
                                   comma$,        /* "|" Delimiter        */~
                                   ieprice$,                                ~
                                   comma$,        /* "|" Delimiter        */~
                                   so_number$,                              ~
                                   comma$,        /* "|" Delimiter        */~
                                   inv_type$,                               ~
                                   comma$,        /* "|" Delimiter        */~
                                   part_num$,                               ~
                                   comma$,        /* "|" Delimiter        */~
                                   part_desc$,                              ~
                                   comma$,        /* "|" Delimiter        */~
                                   gs128$,        /* CR2829               */~
                                   comma$,        /* "|" Delimiter        */~
                                   " "            /* Filler               */


                write #26

        return

dayinv_fmt:      FMT CH(08), CH(01), CH(03), CH(01), CH(03), CH(01),  ~
           CH(8), CH(01), CH(14), CH(01), CH(14), CH(01), CH(14), CH(01), ~
                      CH(14), CH(01), CH(14), CH(01), CH(8), CH(01),  ~
                      CH(1), CH(01), CH(25), CH(1), CH(30), CH(1), ~
                      CH(20), CH(1), CH(66)             /* CR2829 */
                      
        open_error
           comp% = 2%
           hdr$  = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = "(Open Error) - File = " & filename$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return


        open_file
            init(" ") library$, volume$
            library$        = "ORAFILES"
            volume$         = "CARLO2"
/*SR66111   if schema% = 2% then volume$ = "NE2"    (AWD001) */

             open nodisplay #ff%, output, space = 100%,                  ~
                dpack   = 100%, ipack = 100%, file = file$,              ~
                library = library$, volume = volume$, blocks = 5%
        return



        exit_program
            end

            file_error
               err% = 2%
 
            end



