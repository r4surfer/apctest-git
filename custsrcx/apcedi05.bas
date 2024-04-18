        REM *-----------------------------------------------------------*~
            *                                                           *~
            * AAAAAA  PPPP   CCCC  EEEE  DDDD  IIIIII   0000   5555     *~
            * AA  AA  PP  P CC     EE    DD  D   II    0    0  55       *~
            * AAAAAA  PPPP  CC     EEEE  DD  D   II    0    0  5555     *~
            * AA  AA  PP    CC     EE    DD  D   II    0    0     55    *~
            * AA  AA  PP     CCCC  EEEE  DDDD  IIIIII   0000   5555     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * APCEDI05 - Utility To Retransmit, Purge, Or Print Edi     *~
            *            Data.                                          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/15/93 ! Original                                 ! VRW *~
            *          !                                          !     *~
            * 01/15/98 ! Y2K Modifications                        ! DJD *~
            * 01/15/98 ! No dim's for rpt_bdte$ and rpt_edte$     ! DJD *~
            *          ! were found in the program so they were   !     *~
            *          ! added.  Also added dim for rpt_key$      !     *~
            *          !                                          !     *~
            *          ! (1) Fixed Y2K bug with report.           ! ERN *~
            *          ! (2) Disabled PF-14 Delete.  The routine  !     *~
            *          !     assumes only one year will exist in  !     *~
            *          !     APCEDIMC -- there are currently 3.   !     *~
            *          ! (3) Fixed Y2K bug with PF-12 Purge.      !     *~
            * 10/06/00 ! Mod to add report of Invoices with no    ! CMG *~
            *          !     Acknowledgments. (EWD001)            !     *~            
            *-----------------------------------------------------------*


        dim apcmc_key$28,                /* ARIEDIMC File Key          */~
            axd$(64)4,                   /*                            */~
            cde_key$12,                  /* GENCODES File Key          */~
            cde_str$56,                  /* GENCODES Rewrite String    */~
            detail_send_item$226,        /* Flat File Detail String    */~
            desc1$12,                    /* Screen Description         */~
            desc2$11,                    /* Screen Description         */~
            desc3$10,                    /* Screen Description         */~
            desc4$17,                    /* Screen Description         */~
            desc5$5,                     /* Screen Description         */~
            dummy$43,                    /* Dummy Key Variable         */~
            edi_cust$9,                  /* Customer Number            */~
            edi_desc$15,                 /* Customer Description       */~
            edi_duns$15,                 /* Customer DUNS Number       */~
            errormsg$79,                 /* Error Message              */~
            inv_key$13,                  /* APCEDIMC File Key 1        */~
            f2%(64),                     /* = 0 if the file is open    */~
            fs%(64),                     /* = 1 Open, -1 doesn't exist */~
            hd_str$39,                   /* Header String              */~
                                         /*   0 if not checked         */~
            line1$79,                    /* Screen Line One            */~
            line2$79,                    /* Screen Line Two            */~
            line4$79,                    /* Screen Line Four           */~
            line5$79,                    /* Screen Line Five           */~
            line6$79,                    /* Screen Line Six            */~
            new_cntl$9,                  /* Next Control Number        */~
            old_cntl$9,                  /* Old Control Number         */~
            part1$4,                     /* Record Part Of APCEDIMC    */~
            part2$12,                    /* Record Part Of APCEDIMC    */~
            rec_tot$5,                   /* Record Counter             */~
            rec_type$1,                  /* Record Type                */~
            rpt_duns$15,                 /* DUNS Number                */~
            rpt_cust$9,                  /* Customer Number            */~
            rpt_doc$16,                  /* Invoice Number             */~
            rpt_seq$3,                   /* Invoice Sequence           */~
            rpt_in$25,                   /* APC Part Number            */~
            rpt_qty$5,                   /* Part Quantity              */~
            rpt_typ$8,                   /* Document Type              */~
            rpt_flag$1,                  /* Accept or Reject Flag      */~
            rpt_stat$5,                  /* Document Status (S/T/R)    */~
            rpt_sdte$8,                  /* Status Date                */~
            rpt_cntl$9,                  /* Control Number             */~
            rpt_key$6,                   /* Used in date index key     */~
            rslt$(64)20,                 /* Text From File Opening     */~
            save_id$3,                   /* Save EDI Identifier        */~
            sav_inv$8,                   /* Save Invoice Number        */~
            sav_sdte$6,                  /* Save Date                  */~
            sav_duns$15,                 /* Save DUNS Number           */~
            sav_key$13,                  /* APCEDIMC File Key 1 (Temp) */~
            sort$10,                     /* Report Sort Description    */~
            tmp_dte$8,                   /* Temporary Holding Date     */~
            tr_bdate$8,                  /* Transaction Begin Date     */~
            tr_edate$10, pg_date$10,     /* Transaction End Date       */~
            rpt_bdte$8, cnt$37,          /* (Formatted) Begin Date     */~
            rpt_edte$8,                  /* (Formatted) End Date       */~
            tr_all$1,                    /* All Transactions           */~
            tr_inv$1,                    /* Transaction Item Invoice   */~
            tr_ina$1,                    /* Transaction Item Inv. Ack. */~
            tr_poa$1,                    /* Transaction Item PO. Ack.  */~
/*EWD001*/  tr_woa$1,                    /* Trans Invoices w/out Ack.  */~
/*EWD001*/  readkey$22,                  /* Find Ack READKEY           */~            
            tr_srt$1,                    /* Transaction Sort Code (C/I)*/~
            tr_str$20,                   /* Trailer String             */~
            tr_type$1,                   /* Transaction Type           */~
            type$2,                      /* Record Type                */~
            yr_edate$6,                  /* comparrison date           */~
            rpt_kdte$			 /* date for key lookup        */

/* <<<<<<<<<< Y2K >>>>>>>>>> */
        dim blankdate$6                  /* Used in date compares      */
/* <<<<<<<<<< Y2K >>>>>>>>>> */          

            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! APCEDIMC ! APC EDI Master Control History File      *~
            * #2  ! APCEDISN ! APC EDI Send Flat File                   *~
            * #3  ! GENCODES ! GENCODES Table File                      *~
            * #4  ! APCEDIWK ! Sort File for Report                     *~
            *************************************************************~
            *              File Selection and Open Calls                *~
            *************************************************************

            select #1, "APCEDIMC",                                       ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =   1 , keylen =  22,                     ~
                        alt key    1 , keypos =  10, keylen =  13, dup,  ~
                            key    2 , keypos = 233, keylen =  06, dup

            select #2, "APCEDISN",                                       ~
                        varc,     consec,   recsize = 230

            select #3, "GENCODES",                                       ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =   1 , keylen =  24

            select #4, "APCEDIWK",                                       ~
                        varc,     indexed,  recsize = 135,               ~
                        keypos =   1 , keylen =  43

            select #5, "APCEDIMC",                                       ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =   1 , keylen =  22,                     ~
                        alt key    1 , keypos =  10, keylen =  13, dup,  ~
                            key    2 , keypos = 233, keylen =  06, dup

            call "SHOSTAT" ("Opening EDI Files...")
            call "OPENCHCK" (#1, fs%(1 ), f2%(1 ),  100%, rslt$(1 ))
            call "OPENCHCK" (#3, fs%(3 ), f2%(3 ),    0%, rslt$(3 ))

        REM *************************************************************~
            *       M A I N  P R O C E S S  S E C T I O N                ~
            *************************************************************~

        gosub initialize_data              /* Initialize Data Variables */

        show_screen
        accept                                                           ~
             at (02,02), fac(hex(84)), line1$,                  ch(79),  ~
             at (03,02), fac(hex(a4)), line2$,                  ch(79),  ~
             at (04,02), fac(hex(94)), errormsg$,               ch(79),  ~
             at (06,03), fac(hex(a4)), desc1$,                  ch(12),  ~
             at (06,23), "Type",                                         ~
             at (06,39), fac(hex(81)), tr_type$,                ch(01),  ~
             at (06,48), "(T-Transmit, P-Purge, R-Report)",              ~
             at (08,03), fac(hex(a4)), desc2$,                  ch(11),  ~
             at (08,23), "Begin Date",                                   ~
             at (08,39), fac(hex(82)), tr_bdate$,               ch(08),  ~
             at (08,48), "(MMDDYYYY Format)",                            ~
             at (09,23), "End Date",                                     ~
             at (09,39), fac(hex(82)), tr_edate$,               ch(08),  ~
             at (11,03), fac(hex(a4)), desc3$,                  ch(10),  ~
             at (11,23), "All(Send Rec's)",                              ~
             at (11,39), fac(hex(81)), tr_all$,                 ch(01),  ~
             at (11,48), "(Use 'X' Only, One Choice)",                   ~
             at (12,23), "Invoices (Sent)",                              ~
             at (12,39), fac(hex(81)), tr_inv$,                 ch(01),  ~
             at (14,03), fac(hex(a4)), desc4$,                  ch(17),  ~
             at (14,23), "Invoices Ack   ",                              ~
             at (14,39), fac(hex(81)), tr_ina$,                 ch(01),  ~
             at (15,23), "P. O. Ack      ",                              ~
             at (15,39), fac(hex(81)), tr_poa$,                 ch(01),  ~
/*EWD001*/   at (16,23), "Invoice w/o Ack",                              ~
/*EWD001*/   at (16,39), fac(hex(81)), tr_woa$,                 ch(01),  ~
                                                                         ~             
             at (18,03), fac(hex(a4)), desc5$,                  ch(05),  ~
             at (18,23), "By",                                           ~
             at (18,39), fac(hex(81)), tr_srt$,                 ch(01),  ~
             at (18,48), "(C-Customer, I-Invoice)",                      ~
             at (21,02), fac(hex(a4)), line4$,                  ch(79),  ~
             at (22,02), fac(hex(84)), line5$,                  ch(79),  ~
             at (23,02), fac(hex(84)), line6$,                  ch(79),  ~
             keys(pfkeys$), key(keyhit%)

             errormsg$=" "

             if keyhit% <> 0 then L01810
                gosub verify_screen_data  /* Verify Screen Data Entered */
                if errormsg$ <> " " then goto show_screen
                pfkeys$ = hex(00010c0e0f10)
                line4$ = "Press PF14 to Continue Transaction.."
                if tr_type$<>"P" then L01730
L01730:            str(line5$,30,22)="PF14 - Purge EDI Data"
                if tr_type$<>"R" then L01770
                   select printer (134)
                   str(line5$,30,22)="PF14 - Print EDI Data"
L01770:         if tr_type$<>"T" then goto show_screen
                   str(line5$,30,22)="PF14 - Create Transfer"
                goto show_screen

L01810:      if keyhit% <> 14 then L02310
                if tr_type$<>"T" then goto L02020
                   call "SHOSTAT" ("Creating EDI Transmit File...")

                   call "OPENCHCK" (#2, fs%(2 ), f2%(2 ),    0%, rslt$(2))
                   if fs%(2) = -1% then L01900
                      close #2
                      call "FILEBGON" (#2) /* Scratch Old Send File */

L01900:            call "OPENFILE" (#2, "OUTPT", f2%(2), rslt$(2),       ~
                                    axd$(2))

                   gosub build_invoice_recs    /* Rebuild Transmit File */
                   gosub build_poack_recs

                   lcnt%=lcnt%+2%
                   if lcnt%>55% then gosub print_header1
                   print
                   print using L07790 , grnd_tot%

                   close #2                      /* Close APCEDISN File */
L02020:         if tr_type$<>"P" then L02100
            rem *****************************
                   errormsg$ = "This function has been purged from the program..."
                   goto show_screen
            rem *****************************
                   call "SHOSTAT" ("Purging APCEDIMC File..")
                   apcmc_key$=hex(00)
                   if tr_inv$="X" then str(apcmc_key$,1,2) = "TI"
                   if tr_ina$="X" then str(apcmc_key$,1,2) = "RI"
                   if tr_poa$="X" then str(apcmc_key$,1,2) = "TS"
/* <<<<<<<<<< Y2K >>>>>>>>>> */
/* Since tr_bdate$ is entered in mmddyyyy format in the accept section */
/* there should be no need to worry about how the date is formmated    */
/* here                                                                */
/* <<<<<<<<<< Y2K >>>>>>>>>> */        
                   str(apcmc_key$,3,4)=str(tr_bdate$,1,4)
                   gosub purge_apcedimc_file     /* Purge APCEDIMC File */

L02100:         if tr_type$<>"R" then L02290
                   call "SHOSTAT" ("Printing EDI Report..")
                   call "OPENCHCK" (#4, fs%(4 ), f2%(4 ),    0%, rslt$(4))
                   if fs%(4) = -1% then L02170
                      close #4
                      call "FILEBGON" (#4)

L02170:            call "OPENCHCK" (#4, fs%(4), f2%(4 ),  200%, rslt$(4))
                   call "OPENCHCK" (#5, fs%(5), f2%(5 ),    0%, rslt$(5 ))

                   gosub sort_edi_report             /* Sort EDI Report */
                   close #5
                   gosub print_edi_report           /* Print EDI Report */
                   print using L07930
                   lcnt%=lcnt%+3%
                   if lcnt%>55% then gosub print_header2
                   print                         /* Print Report Totals */
                   print using L07950 , inv_tot%, head_tot%
                   print using L07960 , ina_tot%
L02290:         gosub initialize_data      /* Initialize Data Variables */

L02310:      if keyhit% = 1 then gosub initialize_data

             if keyhit% = 12 then gosub purge_date

             if keyhit% = 15 then call "PRNTSCRN"

             if keyhit% = 16 then exit_program

          goto show_screen

        verify_screen_data
            if tr_ina$=" " and tr_poa$=" " and tr_inv$=" " and           ~
               tr_all$=" " and tr_woa$=" " then                          ~
               errormsg$ = "At least One Item Must Be Selected"
            if tr_all$ = " " or tr_all$ = "X" then L02470
               errormsg$ = "Use The Character 'X' Only In The Item Fields"
L02470:     if tr_srt$ = "C" or tr_srt$ = "I" then L02620
               errormsg$ = "Only Enter 'C' Or 'I' In The Sort Field"
            if tr_ina$ = " " or tr_ina$ = "X" then L02510
               errormsg$ = "Use The Character 'X' Only In The Item Fields"
L02510:     if tr_poa$ = " " or tr_poa$ = "X" then L02530
               errormsg$ = "Use The Character 'X' Only In The Item Fields"
L02530:     if tr_inv$ = " " or tr_inv$ = "X" then L02580
               errormsg$ = "Use The Character 'X' Only In The Item Fields"

/* <<<<<<<<<< Y2K >>>>>>>>>> */
/* Again - since the date is entered MMDDYYYY by the operator we should */
/* not have to worry about this code                                    */
/* <<<<<<<<<< Y2K >>>>>>>>>> */
            if tr_bdate$>tr_edate$ then                                  ~
               errormsg$ = "Beginning Date Cannot Be Greater Than Ending ~
        ~Date"
L02580:     if str(tr_edate$,1,2)>"12" or str(tr_edate$,3,2) > "31" then ~
               errormsg$ = "Invalid Ending Date Entered"
            if str(tr_bdate$,1,2)>"12" or str(tr_bdate$,3,2) > "31" then ~
               errormsg$ = "Invalid Beginning Date Entered"
L02620:     if tr_bdate$ = " " and tr_edate$ = " "  then                 ~
               errormsg$ = "Date Range Cannot Be Blank"
            if tr_bdate$ > " " and tr_edate$ = " "  then                 ~
               tr_edate$ = tr_bdate$
            if tr_type$ <> "T" and tr_type$ <> "P" and tr_type$ <> "R"   ~
               then errormsg$ = "Please Enter 'T', 'P', Or 'R' For Type"
            if errormsg$ <> " " then return
            if tr_type$ <> "R" then return

/* <<<<<<<<<< Y2K >>>>>>>>>> */

/* This simply moves the format of the date around to make it into a    */
/* YYYYMMDD date                                                        */
/* Note Original code was modified to handle four digit year!           */
               rpt_bdte$=str(tr_bdate$,5,4) : rpt_edte$=str(tr_edate$,5,4)
               str(rpt_bdte$,5,4)=str(tr_bdate$,1,4)
               str(rpt_edte$,5,4)=str(tr_edate$,1,4)
               call "DATEFMT" (rpt_bdte$) : call "DATEFMT" (rpt_edte$)

/* <<<<<<<<<< Y2K >>>>>>>>>> */
               sort$ = "SORT: CUST"
               if tr_srt$ = "I" then sort$ = "SORT:  INV"
        return

        initialize_data
            init (" ") apcmc_key$, cde_key$, cde_str$, detail_send_item$,~
                       edi_cust$, edi_duns$, new_cntl$, old_cntl$,       ~
                       rec_tot$, save_id$, type$, edi_desc$, tr_str$,    ~
                       hd_str$, tr_type$, tr_bdate$, tr_edate$, tr_inv$, ~
                       tr_ina$, tr_poa$, tr_all$, tmp_dte$, sav_inv$, tr_woa$

            edi_cntl, new_cntl%, old_cntl% = 0%
            grnd_tot%, page_no% = 0%
            lcnt% = 98%

            date$ = date
            trans_date$ = date
            time$ = time
/* <<<<<<<<<< Y2K >>>>>>>>>> */
            call "DATUFMTC" (blankdate$)
/* <<<<<<<<<< Y2K >>>>>>>>>> */
            call "DATEFMT" (date$)
            call "TIME"(rpt_time$)
            call "EXTRACT" addr("ID", user$)

            tr_srt$ = "C"

            line1$ = "                                                   ~
        ~              TODAY " & date$
            line2$ = "EDI Retransmit, Purge, and Report Utility          ~
        ~   (Y2K)      APCEDI05*"
            line4$ = "Enter Required Data and Press <Return>."
            line5$ = "PF1 - Start Over"
            desc1$ = transaction:"
            desc2$ = "Scan Range:"
            desc3$ = "Documents:"
            desc4$ = "Acknowledgements:"
            desc5$ = "Sort:"

            line6$ = "                             PF15 - Print Screen   ~
        ~                PF16 - Exit"
            pfkeys$ = hex(00010c0f10)

            gosub init_counters

        return

        init_counters
            head_tot%, name_tot%, line_tot%, po_tot%, rec_tot% = 0%
            inv_tot%, hdtr_tot%, ina_tot% = 0%
            sav_inv$ = " "
        return

        purge_apcedimc_file
           end      /* Make sure that routine was disabled! */
           apcmc_key$ = " "    /* Start File At First/Next Purge Record */
        purge_next
           read #1,hold,key > apcmc_key$,using L03290 ,apcmc_key$,         ~
                                                            eod goto L03540
L03290:         FMT CH(22)
             if str(apcmc_key$,3%,4%) > str(tr_edate$,1%,4%) then return
                                              /* Purge All Sent Records */
                if tr_all$ <> "X" then goto L03370
                   if str(apcmc_key$,1%,1%) = "S" then                   ~
                                                          goto purge_next
                      goto L03520
                                            /* PURGE ALL SENT INVOICES */
L03370:         if tr_inv$ <> "X" then goto L03420
                   if str(apcmc_key$,1%,1%) <> "T" then goto L03470
                      if str(apcmc_key$,2%,1%) <> "I" then goto L03420
                         goto L03520
                                            /* PURGE ALL SENT PO ACK'S */
L03420:            if tr_poa$ <> "X" then goto L03470
                      if str(apcmc_key$,1%,1%) <> "T" then goto L03470
                      if str(apcmc_key$,2%,1%) <> "S" then goto L03470
                         goto L03520
                                            /* PURGE ALL INVOICE ACK'S */
L03470:         if tr_ina$ <> "X" then goto L03530
                   if str(apcmc_key$,1%,1%) <> "R" then                  ~
                                                        goto purge_next
                      if str(apcmc_key$,2%,1%) <> "I" then               ~
                                                        goto purge_next
L03520:         delete #1
L03530:         goto purge_next
L03540: return

        purge_date
           close printer
           call "SHOSTAT" ( "Purging All Data Less than Date" )

           date%, cnt%, cnt1% = 0%
           cnt$ = "Scanned [xxxxxxxx] Deleted [xxxxxxxx]"
           pg_date$ = tr_edate$
           call "DATEOKC" (pg_date$, date%, errormsg$)
           call "DATUFMTC" (pg_date$) 

           apcmc_key$ = all(hex(00))    /* Start File At First/Next Purge Record */
        purge_date_nxt
           read #1,hold,key 2% > apcmc_key$,using L03660 ,apcmc_key$,      ~
                                                 eod goto purge_date_done
L03660:         FMT POS(233), CH(6)
           cnt% = cnt% + 1%
           if mod(cnt%,100) = 0 then goto L03670
               convert cnt% to str(cnt$,10%,8%), pic(########)
               convert cnt1% to str(cnt$,29%,8%), pic(########)

               print at(04,22);hex(84);cnt$;

L03670:       if str(apcmc_key$,1%,6%) > str(pg_date$,1%,6%) then            ~
                                                     goto purge_date_done
              cnt1% = cnt1% + 1%
              delete #1
              apcmc_key$ = all(hex(00))
              goto purge_date_nxt
        purge_date_done
        return

        sort_edi_report
            if tr_all$<>"X" then L03830
               tr_inv$="X" : tr_ina$="X"            /* Select ALL Case */

/* <<<<<<<<<< Y2K >>>>>>>>>> */
L03830:     if tr_woa$ = "X" then tr_inv$ = "X"     /* (EWD001) */
            str(rpt_kdte$,1,4)   = str(tr_edate$,5,4)
            str(rpt_kdte$,5,2)   = str(tr_edate$,1,2)
            str(rpt_kdte$,7,2)   = str(tr_edate$,3,2)
            call "DATECONV" (rpt_kdte$)
            yr_edate$ = str(rpt_kdte$,1%,6%)
             
            str(rpt_kdte$,1,4)   = str(tr_bdate$,5,4)
            str(rpt_kdte$,5,2)   = str(tr_bdate$,1,2)
            str(rpt_kdte$,7,2)   = str(tr_bdate$,3,2)
            call "DATECONV" (rpt_kdte$)
            rpt_key$ = str(rpt_kdte$,1%,6%)
	    REM rpt_key$ = blankdate$
/* <<<<<<<<<< Y2K >>>>>>>>>> */
            
            read #1, key 2 >=rpt_key$, using L03920 , apcmc_key$, rpt_key$,~
               eod goto L04370
L03920:        FMT CH(22), POS(233), CH(6)
            goto L03990

        read_nxt_rec

           read #1, using L03920 , apcmc_key$, rpt_key$,  eod goto L04370

L03990:       if rpt_key$ > yr_edate$ then return
/* <<<<<<<<<< Y2K >>>>>>>>>> */

              if str(apcmc_key$,22,1) = "H" then read_nxt_rec
              if str(apcmc_key$,22,1) = "N" then read_nxt_rec
              if str(apcmc_key$,2,1)  = "S" then read_nxt_rec

              init (" ") rpt_duns$, rpt_cust$, rpt_doc$, rpt_seq$,       ~
                         rpt_in$, rpt_qty$, rpt_typ$, rpt_flag$,         ~
                         rpt_stat$, rpt_sdte$, rpt_cntl$, sav_sdte$

              if tr_inv$ <> "X" then L04230
                 if str(apcmc_key$,22,1) <> "I" then L04230

                      rpt_typ$="INV"
/* <<<<<<<<<< Y2K >>>>>>>>>> */
                      get #1, using L04160 , rpt_id$, rpt_doc$,            ~
                          rpt_seq$, rpt_in$, rpt_qty$, rpt_sdte$,        ~
                          rpt_cntl$, rpt_cust$
L04160:               FMT POS(7), CH(3), CH(8), CH(3), POS(54), CH(25),  ~
                          POS(84), CH(5), POS(233), CH(6), CH(9), CH(9)
                      sav_sdte$ = rpt_sdte$
                rem   call "DATEFMT" (rpt_sdte$)
                      

/* <<<<<<<<<< Y2K >>>>>>>>>> */
                      gosub sort_detail
                      goto read_nxt_rec

L04230:       if tr_ina$ <> "X" then read_nxt_rec
                 if str(apcmc_key$,22,1) <> "A" then read_nxt_rec

                      rpt_typ$="ACK"
/* <<<<<<<<<< Y2K >>>>>>>>>> */
                      get #1, using L04300, rpt_doc$, rpt_seq$, rpt_duns$, ~
                          rpt_cntl$, rpt_flag$, rpt_sdte$, rpt_cust$
L04300:               FMT POS(10), CH(08), CH(3), POS(38), CH(15), CH(9),~
                          POS(82), CH(1), POS(233), CH(6), POS(248), CH(9)
                      sav_sdte$ = rpt_sdte$
               rem    call "DATEFMT" (rpt_sdte$)
/* <<<<<<<<<< Y2K >>>>>>>>>> */
                      gosub sort_detail
                      goto read_nxt_rec

L04370:          close #4
        return

        sort_detail
           if str(apcmc_key$,22,1)<> "I" then L04470
              gosub find_ack      /* (EWD001) */
              if found% = 1% then return
        
           if str(apcmc_key$,22,1)="A" then L04470
              cde_key$="PARTNERS1"               /* Get Customer DUNS # */
              str(cde_key$,10,3)=rpt_id$
              read #3, key=cde_key$, using L04450 , rpt_duns$, eod goto L04470
L04450:          FMT POS(25), CH(15)
                                                 /* Set Document Status */
L04470:    if str(apcmc_key$,1,1)<>"T" then L04490
              rpt_stat$=tranS"
L04490:    if str(apcmc_key$,1,1)<>"R" then L04510
              rpt_stat$="RECVD"
L04510:    if str(apcmc_key$,1,1)<>"S" then L04540
              rpt_stat$="PEND"

L04540:    if rpt_typ$ <> "ACK" then L04620             /* Set Customer # */
              str(inv_key$,1,8)  = rpt_doc$
              str(inv_key$,9,3)  = "  0"
              str(inv_key$,12,2) = " H"
              read #5, key 1 = inv_key$, using L04600, rpt_cust$,          ~
                   eod goto L04620
L04600:            FMT POS(248), CH(09)

/* <<<<<<<<<< Y2K >>>>>>>>>> */
L04620:    if tr_srt$ <> "C" then L04710
        rem   call "DATUFMTC" (rpt_sdte$)
        rem   call "DATUFMTC" (sav_sdte$)
              call "DATEFMT"  (rpt_sdte$)
              write #4, using L04670 , sav_sdte$, rpt_duns$, rpt_cust$,    ~
              rpt_doc$, rpt_seq$, rpt_cust$, rpt_doc$, rpt_seq$, rpt_in$,~
              rpt_qty$, rpt_typ$, rpt_flag$, rpt_stat$, rpt_sdte$,       ~
              rpt_cntl$, eod goto L04800
L04670:       FMT CH(08), CH(15), CH(09), CH(08), CH(03), CH(09), CH(08),~
                  CH(03), CH(25), CH(10), CH(08), CH(01), CH(05), CH(08),~
                  CH(09)
/* <<<<<<<<<< Y2K >>>>>>>>>> */


/* <<<<<<<<<< Y2K >>>>>>>>>> */
L04710:    if tr_srt$ <> "I" then L04790
        rem   call "DATUFMTC" (rpt_sdte$)
        rem   call "DATUFMTC" (sav_sdte$)
              call "DATEFMT"  (rpt_sdte$)
              write #4, using L04760 , sav_sdte$, rpt_duns$, rpt_doc$,     ~
              rpt_cust$,rpt_seq$, rpt_cust$, rpt_doc$, rpt_seq$, rpt_in$,~
              rpt_qty$, rpt_typ$, rpt_flag$, rpt_stat$, rpt_sdte$,       ~
              rpt_cntl$, eod goto L04800
L04760:       FMT CH(08), CH(15), CH(08), CH(09), CH(03), CH(09), CH(08),~
                  CH(03), CH(25), CH(10), CH(08), CH(01), CH(05), CH(08),~
                  CH(09)
L04790:  return
/* <<<<<<<<<< Y2K >>>>>>>>>> */

L04800:    call "SHOSTAT" ("Duplicate Sort Record" &                     ~
                           rpt_duns$ & rpt_doc$ & rpt_cust$ & rpt_seq$ )
           stop
         return

/* <<<<<<<<<< Y2K >>>>>>>>>> */
        print_edi_report
	   rem stop
           dummy$ = blankdate$
           call "OPENFILE" (#4, "INPUT", f2%(4 ), rslt$(4 ), axd$(4 ))
           read #4, key > dummy$, using L04990, rpt_duns$, rpt_cust$,      ~
              rpt_doc$, rpt_seq$,rpt_in$, rpt_qty$, rpt_typ$, rpt_flag$, ~
              rpt_stat$, rpt_sdte$, rpt_cntl$, eod goto L05290

                sav_duns$ = rpt_duns$
                tmp_dte$  = rpt_sdte$
                goto L05020
/* <<<<<<<<<< Y2K >>>>>>>>>> */

        read_detail
/* <<<<<<<<<< Y2K >>>>>>>>>> */
           read #4, using L04990, rpt_duns$, rpt_cust$, rpt_doc$, rpt_seq$,~
                rpt_in$, rpt_qty$, rpt_typ$, rpt_flag$, rpt_stat$,       ~
                rpt_sdte$, rpt_cntl$, eod goto L05290
L04990:    FMT POS(9), CH(15), POS(44), CH(9), CH(8), CH(3), CH(25),     ~
               CH(10), CH(08), CH(01), CH(05), CH(08), CH(09)
/* <<<<<<<<<< Y2K >>>>>>>>>> */

L05020:    
           rem call "SHOSTAT" ("SAV_INV$=" & sav_inv$)
           rem stop
           rem call "SHOSTAT" ("RPT_DOC$=" & rpt_doc$)
	   rem stop
           if sav_inv$ = rpt_doc$ then L05190
              sav_inv$ = rpt_doc$
              if rpt_typ$ = "INV" then inv_tot% = inv_tot% + 1%

           if rpt_typ$ <> "INV" then L05190  /* Find Matching Acknowledge */
              inv_key$ = hex(00)
              str(inv_key$,1,8)  = rpt_doc$
              str(inv_key$,13,1) = "A"
L05100:       read #1, key 1 > inv_key$, using L05120, sav_key$,           ~
                   eod goto L05190
L05120:            FMT POS(10), CH(13)
                   inv_key$=sav_key$
              if str(sav_key$,1,8)  > str(inv_key$,1,8) then L05190
              if str(sav_key$,13,1) <> "A" then L05100
                 get #1, using L05170, rpt_flag$
L05170:              FMT POS(82), CH(1)

L05190:    gosub print_detail_break
           lcnt%=lcnt%+1%
           if lcnt%>55% then gosub print_header2

/* <<<<<<<<<< Y2K >>>>>>>>>> */
           call "DATEFMT" (rpt_sdte$)
	   
           print using L07910 , rpt_cust$, rpt_doc$, rpt_seq$,             ~
                         rpt_in$, rpt_qty$, rpt_typ$, rpt_flag$,         ~
                         rpt_stat$, rpt_sdte$, rpt_cntl$
/* <<<<<<<<<< Y2K >>>>>>>>>> */

           if rpt_typ$ = "INV"  then head_tot% = head_tot% + 1%          ~
                                else ina_tot%=ina_tot%+1%
           goto read_detail
L05290:    close #4
           call "FILEBGON" (#4)

        return

        print_detail_break
/* <<<<<<<<<< Y2K >>>>>>>>>> */
             call "DATUFMTC" (tmp_dte$)
             call "DATUFMTC" (rpt_sdte$)
             if tmp_dte$=rpt_sdte$ and                                   ~
                sav_duns$=rpt_duns$ then return
                sav_duns$=rpt_duns$
                tmp_dte$=rpt_sdte$
                print using L07930
                print                         /* Print Report Totals */
                print using L07950 , inv_tot%, head_tot%
                print using L07960 , ina_tot%
                inv_tot%, head_tot%, ina_tot%=0%
                lcnt% = 99%
                gosub print_header2
/* <<<<<<<<<< Y2K >>>>>>>>>> */
        return

        build_invoice_recs
           if tr_all$="X"  then L05520          /* Retransmit All Records */
           if tr_inv$<>"X" then return      /* Retransmit Invoices Only */

/* <<<<<<<<<< Y2K >>>>>>>>>> */
                             /* Start File At First Invoice Send Record */
L05520:    apcmc_key$=hex(00)
           str(apcmc_key$,1,2)="TI"
           str(apcmc_key$,3,4)=str(tr_bdate$,1,4)
/* <<<<<<<<<< Y2K >>>>>>>>>> */

        read_next_inv
           read #1, hold, key>apcmc_key$, using L05600 , type$, part1$,    ~
                edi_id$, part2$, detail_send_item$, edi_cust$,           ~
                eod goto L05880
L05600:         FMT CH(2), CH(4), CH(3), CH(12), CH(226), CH(9)
                apcmc_key$=type$
                str(apcmc_key$,3,4)=part1$
                str(apcmc_key$,7,3)=edi_id$
                str(apcmc_key$,10,12)=part2$
                str(apcmc_key$,22,1)=str(detail_send_item$,1,1)
           if str(apcmc_key$,3,4)>str(tr_edate$,1,4) then return

           if type$<>"TI" then L05880
              rec_type$=str(detail_send_item$,1,1)   /* Set Record Type */
              gosub reset_transmit_flag
              if edi_id$=save_id$ then L05740
                 gosub create_header_trailer
                 save_id$=edi_id$
L05740:          put #2, using L05750 , detail_send_item$
L05750:          FMT CH(226)
                 write #2              /* Write APCEDISN Detail Records */

*       *******************************************
*        Add Up All Counters For The Audit Report *
*       *******************************************
              rec_tot%=rec_tot%+1%              /* Account Record Total */
              grnd_tot%=grnd_tot%+1%             /* Grand Total Records */
              if rec_type$ = "H" then head_tot%=head_tot%+1%    /*Header*/
              if rec_type$ = "I" then line_tot%=line_tot%+1% /*Line Item*/
              if rec_type$ = "N" then name_tot%=name_tot%+1% /* Address */

              goto read_next_inv            /* Get Next APCEDIMC Record */
L05880:    if rec_tot%=0% then return          /* Create Trailer Record */
              save_id$="EOD"
              gosub create_header_trailer
        return

        build_poack_recs

           if tr_all$="X"  then L05990          /* Retransmit All Records */
           if tr_poa$<>"X" then return       /* Check PO ACK. Selection */

/* <<<<<<<<<< Y2K >>>>>>>>>> */
                             /* Start File At First PO ACK. Send Record */
L05990:    apcmc_key$=hex(00)
           str(apcmc_key$,1,2)="TS"
           str(apcmc_key$,3,20)=hex(00)
           str(apcmc_key$,3,4)=str(tr_bdate$,1,4)
/* <<<<<<<<<< Y2K >>>>>>>>>> */

           read #1, hold, key>apcmc_key$, using L06140 , type$, part1$,    ~
                edi_id$, part2$, detail_send_item$, edi_cust$,           ~
                eod goto L06450
                sav_id$=edi_id$
                gosub get_edi_duns
                goto L06210

        read_next_po
           read #1, hold, key>apcmc_key$, using L06140 , type$, part1$,    ~
                edi_id$, part2$, detail_send_item$, edi_cust$,           ~
                eod goto L06450
L06140:         FMT CH(2), CH(4), CH(3), CH(12), CH(226), CH(9)
                apcmc_key$=type$
                str(apcmc_key$,3,4)=part1$
                str(apcmc_key$,7,3)=edi_id$
                str(apcmc_key$,10,12)=part2$
                str(apcmc_key$,22,1)=str(detail_send_item$,1,1)

L06210:    if str(apcmc_key$,3,4)>str(tr_edate$,1,4) then return
           if type$<>"TS" then return

              if edi_id$=sav_id$ then L06320
                 sav_id$=edi_id$
                 lcnt%=lcnt%+2%
                 if lcnt%>55% then gosub print_header1
                 print
                 print using L07760 , edi_duns$, edi_desc$, po_tot%
                 gosub get_edi_duns
                 po_tot%=0%
L06320:       rec_type$=str(detail_send_item$,1,1)   /* Set Record Type */
              gosub reset_transmit_flag
              put #2, using L06350 , str(detail_send_item$,2,225)
L06350:       FMT CH(225), CH(1)
              write #2                 /* Write APCEDISN Detail Records */

*       *******************************************
*        Add Up All Counters For The Audit Report *
*       *******************************************
              grnd_tot%=grnd_tot%+1%             /* Grand Total Records */
              if rec_type$ = "A" then po_tot%=po_tot%+1%  /* Acknowledge*/

              goto read_next_po             /* Get Next APCEDIMC Record */
L06450:
              if po_tot% = 0% then return
                 lcnt%=lcnt%+2%
                 if lcnt%>55% then gosub print_header1
                 print
                 print using L07760 , edi_duns$, edi_desc$, po_tot%
        return

        get_edi_duns

           cde_key$="PARTNERS1"            /* Get Customers Duns Number */
           str(cde_key$,10,3)=edi_id$
           read #3, key=cde_key$, using L06590 , edi_duns$, edi_desc$,     ~
                eod goto L06660
L06590:         FMT POS(25), CH(15), POS(41), CH(15)
        return

        create_header_trailer

           gosub get_edi_duns

L06660:    cde_key$="EDI TRADE"          /* Get Next EDI Control Number */
           str(cde_key$,10,3)=edi_id$
           read #3, hold, key=cde_key$, using L06700 , cde_str$, edi_cntl, ~
                eod goto L06790
L06700:         FMT CH(56), PD(09)
                if edi_cntl > 999999999 then edi_cntl = 0
                old_cntl%=edi_cntl
                if save_id$<>"EOD" then edi_cntl=edi_cntl+1
                new_cntl%=edi_cntl
                put #3 using L06700 , cde_str$, edi_cntl
                rewrite #3  /*Rewrite Updated Control Number To GENCODES*/

                                       /* Convert Numeric Data To Ascii */
L06790:         convert old_cntl% to old_cntl$, pic(#########)
                convert new_cntl% to new_cntl$, pic(#########)

                if save_id$<=" " then L06980     /* Create Trailer Record */
                   rec_tot%  = rec_tot%+1%
                   convert rec_tot%  to rec_tot$,  pic(#####)
                   tr_str$ = "#EOT "
                   str(tr_str$,6,9)=old_cntl$
                   str(tr_str$,16,5)=rec_tot$
                   call "SPCESMSH" (tr_str$,1%)
                   put #2, using L06910, tr_str$, /* Trailer String      */~
                                       " "      /* End Of Record       */
L06910:            FMT CH(20), CH(210)
                   write #2         /* Write Trailer Record To APCEDISN */
                   hdtr_tot% = hdtr_tot%+1%       /* Add Total Trailers */
                   grnd_tot% = grnd_tot%+1%
                   gosub print_audit_report

                if save_id$="EOD" then return   /* Create Header Record */
L06980:            hd_str$=edi_duns$                  /* Destination Id */
                   str(hd_str$,17,8)=trans_date$
                   str(hd_str$,26,4)=time$
                   str(hd_str$,31,9)=new_cntl$  /* Next Control Number */
                   call "SPCESMSH" (hd_str$,1%)
                   put #2, using L07100, "#INVDATA",  /* Format Id       */~
                                       " FROM ",    /* Fixed Language  */~
                                       "APCBUI",    /* Origination Id  */~
                                       " TO ",      /* Fixed Language  */~
                                       hd_str$,     /* Header String   */~
                                       " "          /* Spaces          */

L07100:            FMT CH(8), CH(6), CH(6), CH(4), CH(39), CH(167)
                   write #2          /* Write Header Record To APCEDISN */
                   hdtr_tot% = hdtr_tot%+1%        /* Add Total Headers */
                   rec_tot%  = rec_tot%+1%
                   grnd_tot% = grnd_tot%+1%
        return

        reset_transmit_flag
              delete #1
              tr_dte$=date
              put #1, using L07230 , type$, part1$, edi_id$, part2$,       ~
                                  detail_send_item$, tr_dte$, old_cntl$, ~
                                  edi_cust$
L07230:       FMT CH(2), CH(4), CH(3), CH(12), CH(211), CH(6), CH(9),    ~
                  CH(9)
              write #1
        return

        find_ack                             /* (EWD001) */
           found% = 0%
           init(" ") readkey$
           str(readkey$,1%,8%)  = rpt_doc$
           str(readkey$,9%,3%)  = "   "
           str(readkey$,12%,2%) = "  "
        find_ack_next
           read #5, key 1 > readkey$, using L05120, readkey$, eod goto L07240

           if str(readkey$,1%,8%) <> str(rpt_doc$,1%,8%) then goto L07240
           if str(readkey$,12%,2%) <> " A" then goto find_ack_next

                found% = 1%
L07240: return                              /* (EWD001) */

        print_header1
          page_no% = page_no% + 1%
          print page
          print using L07670 , date$, rpt_time$, user$, page_no%
          print
          print using L07690
          lcnt% = 5%
        return

        print_header2
          if lcnt% <> 99% then print using L07930
          page_no% = page_no% + 1%
          print page
          print using L07850 , date$, rpt_time$, sort$
          print using L07870 , user$, rpt_duns$, rpt_bdte$, rpt_edte$,     ~
                             page_no%
          print
          print using L07890
          print using L07930
          lcnt% = 6%
        return

        print_audit_report
          lcnt% = lcnt% + 7%
          if lcnt% > 55% then gosub print_header1
          print using L07710
          print using L07730 , edi_duns$, edi_desc$, head_tot%
          print using L07740 , name_tot%
          print using L07750 , line_tot%
          print using L07770 , hdtr_tot%
          print
          print using L07780 , rec_tot%
          gosub init_counters
        return

*       *****************************************************************
*        FORMAT STATEMENTS FOR AUDIT REPORT
*       ****************************************************************

L07670: %######## ######## ID: ###  APC EDI RE-TRANSMISSION AUDIT REPORT ~
        ~      PAGE: ####
L07690: %DUNS NUMBER     CUSTOMER        ITEM TYPE             RE-TRANSMI~
        ~T COUNT
L07710: %----------------------------------------------------------------~
        ~-----
L07730: %############### ############### INVOICE HEADER              #####
L07740: %                                INVOICE NAME & ADDRESS      #####
L07750: %                                INVOICE LINES ITEMS         #####
L07760: %############### ############### PO ACKNOWLEDGEMENTS         #####
L07770: %                                ACCOUNT HEADERS/TRAILERS    #####
L07780: %TOTAL RECORDS                                              ######
L07790: %GRAND TOTAL RECORDS                                        ######

*       *****************************************************************
*        FORMAT STATEMENTS FOR EDI TRANSACTION HISTORY REPORT
*       ****************************************************************

L07850: %######## ########                              APC EDI TRANSACTI~
        ~ON HISTORY REPORT           ##########
L07870: %USER ID: ###  DUNS: ###############                FROM ########~
        ~ TO ########                PAGE: ####
L07890: %!APC  CUST!INVOICE NBR !LI NBR!APC PART NUMBER           !  QTY ~
        ~ ! DOC !A/R!STATUS   DATE  !CNTRL NBR!
L07910: %!#########!  ########  !  ### !##########################! #####~
        ~ ! ### ! # !#####  ########!#########!
L07930: %!---------!------------!------!--------------------------!------~
        ~-!-----!---!---------------!---------!
L07950: %Total Invoices: ######    Total Line Items: ######
L07960: %Total Invoice Acknowledgements: ######

        exit_program

         close #1                                /* Close APCEDIMC File */
         close #3                                /* Close GENCODES File */
         end