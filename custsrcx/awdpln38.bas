        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLN38                             *~
            *  Creation Date     - 06/27/08                             *~
            *  Last Modified Date-                                      *~
            *  Written By        - Christie M. Gregory                  *~
            *  Last Modified By  -                                      *~
            *                                                           *~
            *  Description       - New program to create daily history  *~
            *                      data to feed oracle warehouse.       *~
            *                      Data collected from BCKMASTR         *~
            *                                          BCKLINES         *~
            *                                          ARIMASTR         *~
            *                                          ARILINES         *~
            *                                          APCPLNAD         *~
            *                                          ORAINV           *~
            *                      Populates files     DAYHIST          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/30/08 ! New Program for (AWD) - Last Mod Date    ! CMG *~
            *          !                                          !     *~
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
            savekey$100,                 /* Save Readkey               */~
            fields$(500%)256,            /* Generic Fields             */~
            num_fields(500%)             /* Generic Fields             */ 
            

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


        dim                                                              ~
            cuscode$9,                   /* Customer Code              */~
            invoice$8,                   /* Invoice Number             */~
            po$16,                       /* Purchase Order             */~
            so_number$8,                 /* Sales Order Number         */~
            salesman$4,                  /* Saleman Number             */~
            region$4,                    /* Region Code                */~
            ship_date$10,                /* Ship Date                  */~
            post_date$10,                /* Post Date                  */~
            order_date$10,               /* Order Date                 */~
            due_date$10,                 /* Due Date                   */~
            quote$10,                    /* Quote Number               */~ 
            job$16,                      /* Job Number                 */~
            hdisc$14,                    /* Header Disc Percent        */~
            ldisc$14,                    /* Line Disc Percent          */~
            load$5,                      /* Load Number                */~
            check$8,                     /* Check Number               */~
            line_num$3,                  /* Line Number                */~
            line_num1$2,                 /* Line Number1               */~
            so_line_num$3,               /* SO Line Number             */~
            save_line$2,                 /* Save Line Number           */~
            part_number$25,              /* Part Number                */~
            catcode$4,                   /* Category Code              */~
            sub_part$20,                 /* Sub Part                   */~
            info_part$20,                /* info part                  */~
            oqty$14,                     /* order qty                  */~
            sqty$14,                     /* ship/invoice qty           */~
            uprice$14,                   /* order unit price           */~
            iuprice$14,                  /* invoice unit price         */~
            eprice$14,                   /* Extended Price             */~
            ieprice$14,                  /* Invoice Extended Price     */~
            so_line$10,                  /* SO Number & Line           */~
            cus_inv$17,                  /* Customer & Invoice         */~
            support$(30%)3,              /* Support Departments        */~
            dept$3,                      /* Department                 */~
            proc$2,                      /* Process Code               */~
            shift$2,                     /* Shift                      */~
            scan_date$10,                /* Scanning Date              */~
            status$2,                    /* Status Code                */~
            uid$3,                       /* Scanning ID                */~
            time$8,                      /* Scanning Time              */~
            barcode$18,                  /* Scanning Barcode           */~
            warranty$8,                  /* Warranty ID                */~
            dayhist_key$37               /* Dayhist readkey            */





        REM *************************************************************~
            *                     S E L E C T                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************


            select #1,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 1,    keylen = 24

            select #2,  "BCKMASTR",                                      ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #3,  "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19

            select #5,  "APCPLNAD",                                      ~
                        varc,     indexed,  recsize =  64,               ~
                        keypos = 19,   keylen = 33,                      ~
                        alt key 1, keypos =  1, keylen = 33


            select #6,  "ARIMASTR",                                      ~
                        varc,     indexed,  recsize = 2000,              ~
                        keypos =    1, keylen =  16,                     ~
                        alt key  1, keypos =   18, keylen =  12, dup,    ~
                            key  2, keypos =   31, keylen =  25, dup,    ~
                            key  3, keypos =   17, keylen =  39, dup

            select #7,  "ARILINES",                                      ~
                        varc,     indexed,  recsize =  750,              ~
                        keypos =    1, keylen =  19,                     ~
                        alt key  1, keypos =   46, keylen =  19, dup   


    
            select #12, "APCPLNOR",                                      ~
                        varc,     indexed,  recsize =  170,              ~
                        keypos = 1,    keylen = 51,                      ~
                        alt key 1, keypos = 27, keylen = 25,             ~
                            key 2, keypos = 70, keylen =  8, dup,        ~
                            key 3, keypos = 78, keylen =  8, dup,        ~
                            key 4, keypos = 52, keylen =  8,             ~
                            key 5, keypos = 36, keylen = 16, dup


            select #13, "ORAINV",                                        ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =    7, keylen =  18,                     ~
                       alt key   1, keypos =    1, keylen =  24     

            select #16, "APCPLNWT",                                      ~
                         varc,    indexed,  recsize =  128,              ~
                         keypos =    1, keylen =  8,                     ~
                         alt key 1, keypos =  9, keylen = 10, dup,       ~
                             key 2, keypos =  9, keylen = 18     

            select #17, "BCKSUBPT",                                      ~
                       varc,      indexed,  recsize = 256,               ~
                       keypos  =    1, keylen =  11,                     ~ 
                       alt key   1, keypos =   12, keylen =  11, dup,    ~
                           key   2, keypos =   23, keylen =  45, dup  


            select #25, "DAYHIST",                                       ~
                        varc,     indexed, recsize = 512,               ~
                        keypos = 331,    keylen = 37


            filename$ = "GENCODES" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKMASTR" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKLINES" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNAD" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "ARIMASTR" : call "EWDOPEN" (#6, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "ARILINES" : call "EWDOPEN" (#7, filename$, err%)
            if err% <> 0% then gosub open_error      
            filename$ = "APCPLNOR" : call "EWDOPEN" (#12, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "ORAINV" : call "EWDOPEN" (#13, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNWT" : call "EWDOPEN" (#16, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKSUBPT" : call "EWDOPEN" (#17, filename$, err%)
            if err% <> 0% then gosub open_error


            
            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************


            call "EXTRACT" addr("ID", userid$)
            date$ = date

            call "DATEFMT" (date$)
            mat num_fields     = zer

REM 0% = extract entire file and 1% = only new daily data
            extract% = 0%


            file$   = "DAYHIST" 
            ff% = 25%  
            gosub open_file

            gosub get_support

            gosub initialize_variables
            if extract% = 0% then gosub files_analysis
            if extract% = 1% then gosub files_analysis_daily
              goto exit_program



        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables
            init(" ") readkey$, fields$(), filename$, cnt$, hdr$, msg$(),~
                      date$, library$, volume$, readkey1$

            init(" ") cuscode$, invoice$, po$, so_number$, salesman$,    ~
              region$, ship_date$, post_date$, order_date$, due_date$,   ~
              quote$, job$, hdisc$, load$, check$, line_num$, line_num1$,~
              part_number$, catcode$, sub_part$, info_part$, oqty$,      ~
              sqty$, uprice$, iuprice$, eprice$, ieprice$, so_line$,     ~
              dept$, proc$, shift$, scan_date$, status$, uid$, time$,    ~
              barcode$, warranty$, ldisc$, cus_inv$, dayhist_key$,       ~
              savekey$, so_line_num$


        return

        REM *************************************************************~
            *************************************************************

        get_support
          init(" ") readkey$, support$()
          support% = 0%
          str(readkey$, 1, 9) = "PLAN SUPP"
        support_next
          read #1, key > readkey$, using L34950, readkey$, ~
                                       eod goto support_done
L34950:          FMT CH(24)

                 if str(readkey$, 1, 9) <> "PLAN SUPP"    ~           
                           then goto support_done
                     support% = support% + 1%
                     support$(support%) = str(readkey$, 10, 3)
                 goto support_next
        support_done
        return

        
        files_analysis_daily

        return


        files_analysis
            comma$ = "|"

            init(" ") readkey$, fields$()
            readkey$ = all(hex(00))
REM            str(readkey$,1,9)  = "LO0537"
REM            str(readkey$,10,8) = "02298688"

            gosub create_mast

        return

        create_mast
             cnt% = 0%


             gosub read_mast
                   goto L34970
        create_mast_nxt
             gosub read_mast_nxt
             if done% = 1% then goto mast_done
L34970:
              goto create_mast_nxt
        return
        mast_done
        return


        read_mast
            done% = 0%
            mat num_fields = zer
        read_mast_nxt
            if extract% = 0% then                       ~
            read #6, key > readkey$, eod goto read_mast_done ~
            else                                        ~
            read #6, key = readkey$, eod goto read_mast_done
        
                goto L34980


L34980:         cnt% = cnt% + 1%
REM            goto L67155

REM            if extract% = 0% then goto L34990

               get #6, using mastr_fmt, readkey$
mastr_fmt:          FMT CH(17)

                 if extract% = 1% and str(readkey$,10,8) <> invoice$ then ~
                                      goto read_mast_done

            goto L34990
            if mod(cnt%,50%) <> 0% then goto L34990
               convert cnt% to str(cnt$,19%,8%), pic(########)
               print at(02,02);hex(84);cnt$;

L34990:
                get #6, using L35000, cuscode$, invoice$, po$, so_number$, ~
                                      ship_date$, salesman$, region$,      ~ 
                                      post_date$


L35000:           FMT CH(09), CH(08), CH(16), CH(08), POS(413), CH(06),     ~
                      POS(501), CH(04), POS(516), CH(04), POS(533), CH(06)

            if so_number$ = " " then goto read_mast_nxt

            call "DATFMTC" (ship_date$, date%, ship_date$)            
            call "DATFMTC" (post_date$, date%, post_date$)

            gosub get_bckmastr
            gosub get_apcplnor
            gosub get_arilines

              goto read_mast_nxt
        read_mast_done
             done% = 1%
        return


        get_bckmastr
           hdisc = 0.00
           quote% = 0%
           init(" ") readkey1$ 
           str(readkey1$, 1, 9) = cuscode$
           str(readkey1$,10,16) = so_number$

           read #2, key = readkey1$, eod goto bckmastr_done


             get #2, using L35100, quote$, job$, order_date$, due_date$,  ~
                                    hdisc

L35100:      FMT POS(599), CH(10), POS(619), CH(16), POS(806), CH(06),  ~
                     POS(818), CH(06), POS(859), PD(14,4)

             convert str(quote$,2,9) to quote%, data goto bad_quote

bad_quote:
            convert quote% to quote$,pic(#########0)


            convert hdisc to hdisc$, pic(-00000000.0000)

            call "DATFMTC" (order_date$, date%, order_date$)            
            call "DATFMTC" (due_date$, date%, due_date$)

        bckmastr_done
        return

        get_apcplnor
           init(" ") readkey1$
           readkey1$ = so_number$

           read #12, key 4% = readkey1$, eod goto apcplnor_done

                get #12, using L35150, check$, load$

L35150:          FMT POS(78), CH(08), POS(94), CH(05)

        apcplnor_done
        return

        get_arilines
           init(" ") readkey1$, line_num1$
           str(readkey1$, 1, 9) = cuscode$
           str(readkey1$,10, 8) = invoice$
           cus_inv$ = str(readkey1$,1,17)

       get_next_ariline
           read #7, key > readkey1$, using L35200, readkey1$,              ~
                                            eod goto arilines_done

L35200:       FMT CH(20)
         
              if str(readkey1$, 1, 17) <> cus_inv$ then goto arilines_done

              get #7, using L35300, line_num$, so_line_num$, part_number$,~
                         catcode$, sqty, iuprice, ldisc

L35300:           FMT POS(18), CH(03), CH(03), POS(24), CH(25), POS(81),  ~
                      CH(04), POS(93), PD(14,4), POS(109), PD(14,4),      ~
                      POS(141), PD(14,4)

REM Need to use the line number from the order not the invoice line number !!

                  convert so_line_num$ to line_num%, data goto bad_line
bad_line:
                  convert line_num% to line_num1$, pic(00)

                  convert sqty to sqty$, pic(-000000000000)

                  convert iuprice to iuprice$, pic(-00000000.0000)

                  ieprice = 0.00
                  if sqty <= 0 then goto no_ship
                  ieprice = round(sqty * iuprice, 2)

                  dsc_amt = 0 - round(iuprice * ldisc * .01, 2)
                  ieprice = round(ieprice + dsc_amt, 2)

                  dsc_amt   = 0 - round(iuprice * hdisc * .01, 2)
                  ieprice = round(ieprice + dsc_amt, 2)
no_ship:
                  convert ieprice to ieprice$, pic(-00000000.0000)

             gosub get_bcklines

               goto get_next_ariline
        arilines_done
        return

        get_bcklines
           init(" ") readkey1$ 
           str(readkey1$, 1, 16) = so_number$
           str(readkey1$, 17, 3) = so_line_num$

           read #3, key = readkey1$, eod goto bcklines_done

           get #3, using L35400, oqty, uprice, ldisc

L35400:       FMT POS (93), PD(14,4), POS(165), PD(14,4), PD(14,4)

              convert oqty to oqty$, pic(-000000000000) 

              convert uprice to uprice$, pic(-00000000.0000)

              convert ldisc to ldisc$, pic(-00000000.0000)

              eprice = round(oqty * uprice, 2)

              dsc_amt   = 0 - round(eprice * ldisc * .01, 2)
              eprice = round(eprice + dsc_amt, 2)

              dsc_amt   = 0 - round(ieprice * hdisc * .01, 2)
              eprice = round(eprice + dsc_amt, 2)

              convert eprice to eprice$, pic(-00000000.0000)

              gosub get_barcodes
              if warr% = 0% then gosub get_warr_so
        bcklines_done
             init(" ") readkey1$
             str(readkey1$, 1, 9) = cuscode$
             str(readkey1$,10, 8) = invoice$
             str(readkey1$,18, 3) = line_num$
        return

        get_barcodes
           warr% = 0%
           init(" ") readkey1$, so_line$, barcode$, scan_date$, dept$, ~
                     shift$, status$, time$, uid$, save_line$
           str(readkey1$, 1, 8) = so_number$
           str(readkey1$, 9, 2) = line_num1$
           so_line$ = str(readkey1$, 1, 10)
        get_bar_next
           read #5, key 1% > readkey1$, using L35500, readkey1$,       ~
                                   eod goto barcodes_done
L35500:         FMT CH(33)

                if str(readkey1$, 1, 10) <> so_line$ then goto barcodes_done

                if str(readkey1$, 32, 2) < "12" then goto get_bar_next

                if save_line$ <> " " and save_line$ = str(readkey1$,9,2) ~ 
                                              then goto get_bar_next

                save_line$ = str(readkey1$,9,2)


                dept$ = str(readkey1$,25, 3)
                if dept$ >= "106" then goto get_bar_next
                for i% = 1% to support%
                  if dept$ = support$(i%) then goto get_bar_next
                next i%
 
                barcode$   = str(readkey1$, 1,18)
                scan_date$ = str(readkey1$,19,6)
                call "DATFMTC" (scan_date$, date%, scan_date$)

                shift$     = str(readkey1$,30,2)
                status$    = str(readkey1$,32,2)
 
                get #5, using L35600, time$, uid$
L35600:          FMT POS(52), CH(08), CH(03)


                init(" ") savekey$
                savekey$ = str(readkey1$,1,33)
                 
                gosub get_warranty


                init(" ") readkey1$
                str(readkey1$, 1,33) = savekey$

                goto get_bar_next

        barcodes_done
        return

        get_warranty
           init(" ") readkey1$
           readkey1$ = barcode$

           read #16, key 2% = readkey1$, eod goto warranty_done

             get #16, using L35700, warranty$, sub_part$, info_part$
L35700:         FMT CH(08), POS(52), CH(20), CH(20)

             warr% = 1%
             gosub write_dayhist

        warranty_done
        return

        get_warr_so
           warr1% = 0%
           init(" ") readkey1$
           readkey1$ = so_line$
        get_war_so_next
           read #16, key 2% > readkey1$, eod goto warr_so_done

             get #16, using L35750, warranty$, readkey1$, sub_part$, info_part$
L35750:         FMT CH(08), CH(18), POS(52), CH(20), CH(20)

             if str(readkey1$,1,10) <> so_line$ then goto warr_so_done
             barcode$ = str(readkey1$,1,18)
             warr1% = 1%
             gosub write_dayhist

             goto get_war_so_next
        warr_so_done
          if warr1% = 1% then return
          init(" ") warranty$, barcode$
          barcode$ = so_line$
          gosub write_dayhist
        return

        write_dayhist
          init(" ") dayhist_key$
          str(dayhist_key$, 1, 9) = invoice$ & "|"
          str(dayhist_key$,10,19) = barcode$ & "|"
          str(dayhist_key$,29, 9)  = warranty$ & "|"

          read #25, hold, key = dayhist_key$, eod goto put_dayhist
                delete #25
put_dayhist:

            put #25, using L35800, job$,          /* Job                  */~
                                   comma$,        /* "|" Delimiter        */~
                                   cuscode$,      /* Customer             */~
                                   comma$,        /* "|" Delimiter        */~
                                   po$,           /* PO Number            */~
                                   comma$,        /* "|" Delimiter        */~
                                   load$,         /* Load Number          */~
                                   comma$,        /* "|" Delimiter        */~  
                                   so_number$,    /* Sales Order          */~
                                   comma$,        /* "|" Delimiter        */~
                                   so_line_num$,  /* Line Number          */~
                                   comma$,        /* "|" Delimiter        */~
                                   order_date$,   /* Order Date           */~
                                   comma$,        /* "|" Delimiter        */~
                                   ship_date$,    /* Ship Date            */~
                                   comma$,        /* "|" Delimiter        */~
                                   post_date$,    /* Post Date            */~
                                   comma$,        /* "|" Delimiter        */~
                                   quote$,        /* Quote Number         */~
                                   comma$,        /* "|" Delimiter        */~
                                   part_number$,  /* Part Number          */~
                                   comma$,        /* "|" Delimiter        */~
                                   sub_part$,     /* Sub Part             */~
                                   comma$,        /* "|" Delimiter        */~ 
                                   info_part$,    /* Info Part            */~
                                   comma$,        /* "|" Delimiter        */~ 
                                   oqty$,         /* Order Qty            */~
                                   comma$,        /* "|" Delimiter        */~
                                   sqty$,         /* Ship Qty             */~
                                   comma$,        /* "|" Delimiter        */~ 
                                   hdisc$,        /* Header Disc          */~
                                   comma$,        /* "|" Delimiter        */~
                                   ldisc$,        /* Line Disc            */~
                                   comma$,        /* "|" Delimiter        */~
                                   uprice$,       /* Unit Price           */~
                                   comma$,        /* "|" Delimiter        */~
                                   eprice$,       /* Extended Price       */~
                                   comma$,        /* "|" Delimiter        */~
                                   iuprice$,      /* Invoice Unit Price   */~
                                   comma$,        /* "|" Delimiter        */~
                                   ieprice$,      /* Invoice Extended Pric*/~
                                   comma$,        /* "|" Delimiter        */~
                                   catcode$,      /* Category Code        */~
                                   comma$,        /* "|" Delimiter        */~
                                   region$,       /* Region               */~
                                   comma$,        /* "|" Delimiter        */~
                                   salesman$,     /* Salesman             */~
                                   comma$,        /* "|" Delimiter        */~ 
                                   check$,        /* Check Number         */~
                                   comma$,        /* "|" Delimiter        */~
                                   due_date$,     /* Due Date             */~
                                   comma$,        /* "|" Delimiter        */~
                                   invoice$,      /* Invoice              */~
                                   comma$,        /* "|" Delimiter        */~
                                   barcode$,      /* Barcode              */~
                                   comma$,        /* "|" Delimiter        */~ 
                                   warranty$,     /* Warranty             */~
                                   comma$,        /* "|" Delimiter        */~
                                   scan_date$,    /* Scan Date            */~
                                   comma$,        /* "|" Delimiter        */~ 
                                   dept$,         /* Department           */~
                                   comma$,        /* "|" Delimiter        */~
                                   proc$,         /* Process Code         */~
                                   comma$,        /* "|" Delimiter        */~
                                   shift$,        /* Shift Code           */~
                                   comma$,        /* "|" Delimiter        */~
                                   status$,       /* Status               */~
                                   comma$,        /* "|" Delimiter        */~
                                   time$,         /* Time                 */~
                                   comma$,        /* "|" Delimiter        */~
                                   uid$,          /* User Id              */~
                                   comma$,        /* "|" Delimiter        */~
                                   " "            /* Filler               */


                write #25

        return
L35800:      FMT CH(16), CH(01), CH(09), CH(01), CH(16), CH(01), CH(05), CH(01), ~
                 CH(08), CH(01), CH(03), CH(01), CH(10), CH(01), CH(10), CH(01), ~
                 CH(10), CH(01), CH(10), CH(01), CH(25), CH(01), CH(20), CH(01), ~
                 CH(20), CH(01), CH(14), CH(01), CH(14), CH(01), CH(14), CH(01), ~ 
                 CH(14), CH(01), CH(14), CH(01), CH(14), CH(01), CH(14), CH(01), ~
                 CH(14), CH(01), CH(04), CH(01), CH(04), CH(01), CH(04), CH(01), ~
                 CH(08), CH(01), CH(10), CH(01), CH(08), CH(01), CH(18), CH(01), ~
                 CH(08), CH(01), CH(10), CH(01), CH(03), CH(01), CH(02), CH(01), ~
                 CH(02), CH(01), CH(02), CH(01), CH(08), CH(01), CH(03), CH(01), ~
                 CH(108)


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


             open nodisplay #ff%, output, space = 100%,                  ~
                dpack   = 100%, ipack = 100%, file = file$,              ~
                library = library$, volume = volume$, blocks = 5%
        return



        exit_program
            end




