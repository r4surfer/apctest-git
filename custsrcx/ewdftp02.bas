        REM *-----------------------------------------------------------*~
            *                                                           *~
            * EEEEEE W    W DDDD  FFFFFF TTTTT PPPPP    0000   2222     *~
            * E      W    W D   D F        T   P    P  0    0  2  2     *~
            * EEEE   W WW W D   D FFFFF    T   PPPPP   0    0    2      *~
            * E      WW  WW D   D F        T   P       0    0   2       *~
            * EEEEEE W    W DDDD  F        T   P        0000   2222     *~
            *                                                           *~            
            *-----------------------------------------------------------*~
            * EWDFTP02 - Create Flat File To Send To Norandex/Reynolds. *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/29/00 ! Original                                 ! CMG *~
            * 12/04/01 ! (EWD001) - Mod to use first part of PO   ! CMG *~
            *          ! instead of Customer for Branch           !     *~
            * 02/10/03 ! (EWD002) - Mods for WW Line Number       ! CMG *~
            * 05/28/03 ! (EWD003) - Mods to have same logic as WW ! CMG *~
            *          !   reciver file that goes to Norandex.    !     *~
            * 04/17/06 ! (AWD004) - mods for NE file              ! CMG *~
            *01/09/2010! (AWD005) - mod to combine mulls and SSS  ! CMG *~
            *          !  into one line like WW                   !     *~
            *-----------------------------------------------------------*


        dim ewdrn_key$22,                /* ARIEDIMR File Key          */~
            ewdrnRec$224,                /* EWDFTPMR Rec (AWD005)      */~
            cde_key$12,                  /* GENCODES File Key          */~
            cde_str$56,                  /* GENCODES Rewrite String    */~
            edi_id$3,                    /* Cutomer Trading ID         */~
            f2%(64%),                    /* = 0 if the file is open    */~
            fs%(64%),                    /* = 1 Open, -1 doesn't exist */~
            rec_tot$5,                   /* Record Counter             */~
            lne_tot$7,                   /* Total Lines                */~
            qty_tot$10,                  /* Total Qty                  */~
            amt_tot$12,                  /* Total Amt                  */~
            rslt$(64%)20,                /* Text From File Opening     */~
            readkey$24, desc$30,         /* Gencodes Primary Key       */~
            save_id$3,                   /* Save EDI Identifier        */~
            sav_type$2,                  /* Transaction Type Code      */~
            edi_type$2,                  /* Record Type                */~
            work_date1$10, work_date2$10 /* Temp dates                 */    

        dim edi_date$6,                  /* EDI Invoice Date           */~
            edi_cust$9,                  /* EDI Customer Code          */~
            proc_cust$9,                 /* Customer to check code     */~
            edi_inv$8,                   /* EDI Invoice Number         */~
            sav_sss_inv$8,               /* EDI Invoice Number         */~
            sav_inv$8,                   /* Save Invoice Number        */~
            savInv$8,                    /* Save Invoice Number        */~
            edi_seq$3,                   /* EDI Invoice Sequence Num   */~
            edi_ww_seq$3,                /* WW Line Item         EWD002*/~
            edi_nw_seq$2,                /* New Line Item        EWD003*/~
            edi_part$10,                 /* EDI Nor/Rey Part Number    */~
            edi_qty$7,                   /* EDI Invoice Quantity       */~
            sss_edi_qty$7,               /* Screen EDI Invoice Quantity*/~
            sav_ww_seq$2,                /* Save WW Seq Number         */~
            edi_uom$2,                   /* EDI Unit of Measure        */~
            edi_cpu$9,                   /* EDI Cost Per Unit          */~
            edi_desc$39,                 /* EDI Part Description       */~
            edi_vendor$6,                /* EDI Ellison Vendor Code    */~
            edi_po$16,                   /* EDI Purchase Order         */~
            edi_so$8,                    /* EDI Sales Order            */~
            edi_width$20,                /* EDI Part Number Width      */~
            edi_height$20,               /* EDI Part Number Height     */~
            edi_ell_part$25,             /* EDI Ellison Part Number    */~
            edi_filler$118,              /* EDI Filler Area            */~
            edi_code$1,                  /* Send Code for Nor/Rey      */~
            edi_cd_desc$30               /* Send Code Description      */

                                         /*  (EWD003)                  */~
        dim sss_date$6,                  /* EDI Invoice Date           */~
            sss_cust$9,                  /* EDI Customer Code          */~
            sss_inv$8,                   /* EDI Invoice Number         */~
            sss_seq$3,                   /* EDI Invoice Sequence Num   */~
            sss_nw_seq$3,                /* New Line Item        EWD003*/~
            sss_part$10,                 /* EDI Nor/Rey Part Number    */~
            sss_uom$2,                   /* EDI Unit of Measure        */~
            sss_cpu$9,                   /* EDI Cost Per Unit          */~
            sss_desc$39,                 /* EDI Part Description       */~
            sss_vendor$6,                /* EDI Ellison Vendor Code    */~
            sss_po$16,                   /* EDI Purchase Order         */~
            sss_so$8,                    /* EDI Sales Order            */~
            sss_width$20,                /* EDI Part Number Width      */~
            sss_height$20,               /* EDI Part Number Height     */~
            sss_ell_part$25,             /* EDI Ellison Part Number    */~
            sss_filler$118,              /* EDI Filler Area            */~
            sss_code$1,                  /* Send Code for Nor/Rey      */~
            sss_cd_desc$30               /* Send Code Description      */

        dim volume$8,                    /* Volume          (AWD004)   */~
            schema$8                     /* Schema          (AWD004)   */

/* (AWD005) */
        dim wwLne$3,                     /* ww line number  (AWD005)   */~
            swwLne$3,                    /* save ww line number (AWD005)*/~
            totQty$7                     /* Total Quantity (AWD005)    */   

            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! EWDFTPMR ! EWD EDI Master Control History File      *~
            * #2  ! EWDEDISR ! EWD EDI Send Flat File                   *~
            * #3  ! GENCODES ! GENCODES Table File                      *~
            * #4  ! CUSTOMER ! Customer Master File                     *~   
            *************************************************************~
            *              File Selection and Open Calls                *~
            *************************************************************

            select #1, "EWDFTPMR",                                       ~
                        varc,     indexed,  recsize = 224,               ~
                        keypos =   7 , keylen =  22,                     ~
                        alt key    1 , keypos =   1, keylen =  28
                        
            select #2, "EWDFTPSN",                                       ~
                        varc,     consec,   recsize = 400

            select #3, "GENCODES",                                       ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =   1 , keylen =  24

            select #4, "CUSTOMER",                                       ~
                        varc, indexed, recsize = 1200,                   ~
                        keypos = 1, keylen =   9,                        ~
                        alt key  1, keypos =  10, keylen =  30, dup,     ~
                            key  2, keypos = 424, keylen =   9, dup,     ~
                            key  3, keypos = 771, keylen =   9, dup,     ~
                            key  4, keypos = 780, keylen =   9, dup      


            call "SETPRNT" ("FTPR", "FTPR", 0%, 0%)
            select printer (134)

            call "SHOSTAT" ("Opening EDI Files...")
            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 1000%, rslt$(1%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),    0%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%),    0%, rslt$(4%))  

/* (AWD004) - begin */
            schema_err%, schema% = 0%
            init(" ") schema$, volume$
            call "SCHEMA" (schema$, schema%, #3, schema_err%)


            if schema% = 1% then volume$ = "CARLO2"
            if schema% = 2% then volume$ = "NE2"


            open nodisplay #2, output, space = 100%,                     ~
                dpack   = 100%, ipack = 100%, file = "EWDFTPSN",         ~
                library = "FTPNORDX", volume = volume$, blocks = 5%

/* (AWD004) - END */

REM            call "OPENFILE" (#2, "OUTPT", f2%(2%), rslt$(2%), axd$(2%))


        REM *************************************************************~
            *       M A I N  P R O C E S S  S E C T I O N                ~
            *************************************************************~

        call "SHOSTAT" ("Creating EDI Transmit File...")
        prt% = 0%
        gosub initialize_data               /* Initialize Data Variable */
        gosub build_invoice_recs /* Write Invoice Recs To EWDFTPSN File */
        goto  exit_program                              /* Exit Program */

        initialize_data
            init (" ") ewdrn_key$, cde_key$, cde_str$, edi_cust$,          ~
                       edi_part$, rec_tot$, save_id$, edi_type$,           ~
                       edi_desc$, edi_date$, edi_qty$, edi_inv$, edi_seq$, ~
                       edi_uom$, edi_cpu$, edi_vendor$, edi_po$, edi_so$,  ~
                       edi_width$, edi_height$, edi_ell_part$, edi_filler$,~
                       sav_inv$, edi_code$, edi_cd_desc$, edi_ww_seq$,     ~
                       edi_nw_seq$, sss_edi_qty$, totQty$
                                               /*  (EWD003)                  */
            init (" ") sss_date$, sss_cust$, sss_inv$, sss_seq$, sss_nw_seq$,~
                       sss_part$, sav_ww_seq$, sss_uom$, sss_cpu$, sss_desc$, ~
                       sss_vendor$, sss_po$, sss_so$, sss_width$, sss_height$,~
                       sss_ell_part$, sss_filler$, sss_code$, sss_cd_desc$,   ~
                       sav_sss_inv$, proc_cust$

            init("9") wwLne$, swwLne$, savInv$          /* (AWD005) */

            if prt% <> 0% then return

            grnd_tot%, total%, total_qty%, page_no%, edi_nw_seq%, ~
                 ediQty%, totQty% = 0%


            edi_cpu, total_amt, totCpu = 0
            lcnt% = 99%

            
       REM  trans_date$ = date                                 /* Y2K */
            work_date1$ = date                                 /* Y2K */
            call "DATFMTC" (work_date1$, wd%, trans_date$)     /* Y2K */
            trans_date$ = str(trans_date$, 3, 6)               /* Y2K */

            date$ = date
            call "DATEFMT" (date$)
            rpt_time$ = time
            call "TIME"(rpt_time$)

            call "EXTRACT" addr("ID", user$)

           dim testDate$10, checkDate$6
           testDate$, checkDate$ = all(hex(00))
           testDate$ = "20091201"
REM           call "DATE" addr("G+",testDate$,-60%,testDate$,err%)
           call "DATEFMT" (testDate$)
           call "DATUFMTC" (testDate$)


        return


        check_process                          /* Check Process Flag   */
                                       /* Get EDI Id and Customer Data */
                                       /* from the Customer Master File*/
           read #4,key = proc_cust$,using  L01390, edi_id$, ~
                                                   eod goto L01270
           
L01390:         FMT POS(1000), CH(03)

           edi% = 1%                           /* 'K' = Kleinsmidth    */
           init(" ") readkey$, desc$           /* 'P' = PC Anywhere    */
           str(readkey$,1%,9%)   = "EDI TRADE"
           str(readkey$,10%,15%) = edi_id$
           read #3,key = readkey$, using L01240 , desc$, eod goto L01270
L01240:       FMT POS(25), CH(30)

        return
L01270:    edi% = 0%
        return

        build_invoice_recs
                            /* Start File At First Invoice Send Record */
           write_sss% = 0%
           ewdrn_key$="SI"
           str(ewdrn_key$,3,20)=hex(00)
           read #1,hold,key > ewdrn_key$,using ewdrnRecFmt, ewdrnRec$, ~
                                            eod goto L01780

ewdrnRecFmt:      FMT CH(224) 

                                                               /*  (EWD002)  */
                                                              /*  (EWD003)   */
                edi_ww_seq$ = str(ewdrnRec$,221%,3%)
                convert edi_ww_seq$ to edi_ww_seq%, data goto L01375

L01375:         convert edi_ww_seq% to sav_ww_seq$, pic(00)
              
                edi_nw_seq% = edi_nw_seq% + 1%
                convert edi_nw_seq% to edi_nw_seq$, pic(00)


                sav_sss_inv$ = edi_inv$
                                                              /*  (EWD003)   */
                wwLne$, swwLne$ = edi_ww_seq$                    /* (AWD005) */
                savInv$ = str(ewdrnRec$,18%,8%)

                goto L01450

        read_next_inv
           read #1,hold,key > ewdrn_key$,using ewdrnRecFmt, ewdrnRec$, ~
                                            eod goto L01780

                                                              /*  (EWD002)  */

           wwLne$ = str(ewdrnRec$,221%,3%)        /* WW Seq Number */
L01450:    

           str(ewdrn_key$,1%,22%)  = str(ewdrnRec$,7%,22%)
           proc_cust$ = str(ewdrnRec$,9%,9%)
           gosub check_process                 /* Skip Invoice    */
             if edi% = 0% then goto read_next_inv  

                                              /* End of Invoices */
           if str(ewdrn_key$,1,2) <> "SI" then gosub createNorRec
           if str(ewdrn_key$,1,2) <> "SI" then return
           checkDate$ = str(ewdrnRec$,1,6)

REM           if checkDate$ >= str(testDate$,1,6) then goto processRec
REM                  goto read_next_inv
REM processRec
           if str(ewdrnRec$,18%,8%) = "04128656" then goto noTransmit
           
           if savInv$ <> str(ewdrnRec$,18%,8%) then gosub createNorRec
           if wwLne$ <> swwLne$ then gosub createNorRec
              gosub unpackData
noTransmit:
              sav_type$="TI"
              gosub reset_transmit_flag

              goto read_next_inv

CreateNorRec:
/* (AWD004) */
           swwLne$ = wwLne$
           savInv$ = str(ewdrnRec$,18%,8%)

           init(" " ) edi_vendor$
           if schema% = 1% then edi_vendor$ = "064300"
           if schema% = 2% then edi_vendor$ = "021654"

              edi_seq%, edi_ww_seq% = 0%             /* (EWD002) */
              convert edi_seq$ to edi_seq%, data goto L01500

L01500:       convert edi_seq% to edi_seq$, pic(00)


              convert edi_ww_seq$ to edi_ww_seq%, data goto L01505

L01505:       convert edi_ww_seq% to edi_ww_seq$, pic(00)

               goto writeLne

                                                     /* (EWD002) */
              gosub check_ww_seq                     /* (EWD003)  - BEG  */

              if ww_line_num% = 0% and write_sss% = 1% then gosub write_sss
              if sss% = 1% then goto read_next_inv

              if str(sav_ww_seq$,1%,2%) = str(edi_ww_seq$,1%,2%) ~
                                                     then goto same_lne
                 sav_ww_seq$ = edi_ww_seq$
                 edi_nw_seq% = edi_nw_seq% + 1%
                 convert edi_nw_seq% to edi_nw_seq$, pic(00)

          same_lne
              if str(sav_sss_inv$,1%,8%) = str(edi_inv$,1%,8%)   ~
                                                   then goto same_inv
                 sav_sss_inv$ = edi_inv$
                 edi_nw_seq$ = " "
                 edi_nw_seq% = 0%
                 edi_nw_seq% = edi_nw_seq% + 1%
                 convert edi_nw_seq% to edi_nw_seq$, pic(00)

          same_inv
                                                         /*  (EWD003) - END  */
              convert edi_qty$ to edi_qty%, data goto L01510

L01510:       convert edi_qty% to edi_qty$, pic(0000000)

writeLne:

              convert edi_Qty% to edi_qty$, pic(0000000)

              line_ext = 0.00
REM              line_ext = edi_cpu * edi_qty%
              line_ext = totCpu * ediQty%

REM              edi_cpu = edi_cpu * 100000
              totCpu = totCpu * 100000

REM              convert edi_cpu to edi_cpu$, pic(000000000)
              convert totCpu to edi_cpu$, pic(000000000)

              p% = 0%
              p% = pos(edi_po$ = "-")
REM              if p% <> 0% then p% = p% + 1%
              p% = p% + 1%
              
              gosub unpack_invoice_dates
REM    str(edi_cust$,4%,3%)                                    /*  (EWD001) */
REM remove edi_nw_seq and put in wwLne$
              cmg% = 0%
              convert edi_ww_seq$ to cmg%, data goto badNum
badNum:
              convert cmg% to edi_nw_seq$, pic(00)

                 put #2, using L01620, "F", edi_vendor$, edi_inv$, work_date2$,~
                                   str(edi_po$,1%,3%), str(edi_po$,p%,6%),     ~
                                   edi_nw_seq$, edi_part$, edi_qty$, edi_cpu$, ~
                                   edi_desc$,  edi_uom$, edi_filler$,          ~
                                   edi_ell_part$, edi_width$, edi_height$,     ~
                                   edi_so$,edi_code$,edi_cd_desc$, edi_ww_seq$,~
                                   edi_filler$
                                
                                                       /*      (EWD002)      */
L01620:          FMT CH(01), CH(06), CH(22), CH(06), CH(03), CH(06), CH(02),  ~
                     CH(10), CH(07), CH(09), CH(39), CH(02), CH(50), CH(40),  ~
                     CH(20), CH(20), CH(08), CH(01), CH(30), CH(03), CH(115)
                     
                 write #2              /* Write EWDFTPSN Detail Records */

*       *******************************************
*        Add Up All Counters For The Audit Report *
*       *******************************************

              grnd_tot%  = grnd_tot%  + 1%            /* Grand Total Records */
              total_qty% = total_qty% + edi_qty%
              total_amt  = total_amt  + line_ext

              if str(sav_inv$,1%,8%) = str(edi_inv$,1%,8%) then goto L01630
                 sav_inv$ = str(edi_inv$,1%,8%)
                 total%   = total% + 1%
                 
L01630:       gosub print_audit_report
              
              init(" ") edi_desc$
              totQty% = 0%
              edi_qty% = 0%
              totCpu  = 0.00


              return
              goto read_next_inv            /* Get Next EWDFTPMR Record */

L01780: return

        write_sss                          /*      (EWD003)  - BEG       */
              convert sss_edi_qty$ to sss_edi_qty%, data goto L01810

L01810:       convert sss_edi_qty% to sss_edi_qty$, pic(0000000)

              line_ext = 0.00
              line_ext = sss_cpu * sss_edi_qty%

              sss_cpu = sss_cpu * 100000

              convert sss_cpu to sss_cpu$, pic(000000000)

              p% = 0%
              p% = pos(sss_po$ = "-")
REM              if p% <> 0% then p% = p% + 1%
              p% = p% + 1%
              
              gosub unpack_invoice_dates_sss
              edi_nw_seq% = edi_nw_seq% + 1%
              convert edi_nw_seq% to edi_nw_seq$, pic(00)

                 put #2, using L01620, "F", sss_vendor$, sss_inv$, work_date2$,~
                                   str(sss_po$,1%,3%), str(sss_po$,p%,6%),     ~
                                   edi_nw_seq$,sss_part$,sss_edi_qty$,sss_cpu$,~
                                   sss_desc$,  sss_uom$, sss_filler$,          ~
                                   sss_ell_part$, sss_width$, sss_height$,     ~
                                   sss_so$,sss_code$,sss_cd_desc$,sss_ww_seq$, ~
                                   sss_filler$
                                
                 write #2              /* Write EWDFTPSN Detail Records */



              grnd_tot%  = grnd_tot%  + 1%             /* Grand Total Records */
              total_qty% = total_qty% + edi_qty%
              total_amt  = total_amt  + line_ext

              write_sss%, sss%, sss_edi_qty%, edi_qty% = 0%
              sss_edi_cpu = 0.00
              init(" ") sss_edi_qty$
        return
                                                       /*  (EWD003)  - END  */

        unpack_invoice_dates_sss    /* Unpack dates before they're sent.Y2K */
        
            
                work_date1$ = str(sss_date$)            /* Invoice Date     */
                gosub unpack_date

        return

        unpack_invoice_dates    /* Unpack dates before they're sent. Y2K */
        
            
                work_date1$ = str(edi_date$)           /* Invoice Date     */
                gosub unpack_date

        return


        unpack_date         /* in - work_date1$ - PD(11,1)          Y2K */
                            /* out- work_date1$ - formatted             */
                            /*      work_date2$ - YYDDMM                */
                            /* convert work_date2$ - MMDDYY             */

            call "DATFMTC" (work_date1$, wd%, work_date2$)
            work_date2$ = str(work_date2$, 3, 6)                  
            work_date2$ = str(work_date2$,3%,2%) & str(work_date2$,5%,2%) & ~
                          str(work_date2$,1%,2%)
                          
        return
       

        reset_transmit_flag
              delete #1

                                          /* Reset Transmit Flag To 'T' */
              str(ewdrnRec$,7%,2%) = "TI"

              put #1, using ewdrnRecFmt, ewdrnRec$


/* L01370, edi_date$, sav_type$, edi_cust$, edi_inv$,~
                              edi_seq$, edi_part$, edi_qty$, edi_uom$,        ~
                              edi_cpu, edi_desc$, edi_vendor$, edi_po$,       ~
                              edi_ell_part$, edi_width$, edi_height$, edi_so$,~
                              edi_code$, edi_cd_desc$, edi_ww_seq$
*/

              write #1
        return

L01370:         FMT CH(06), CH(02), CH(09), CH(08), CH(03), CH(10), CH(07),   ~
                    CH(02), PD(14,5), CH(39), CH(06), CH(16), CH(25), CH(20), ~
                    CH(20), CH(08), CH(01), CH(30), CH(03)

        print_header
          page_no% = page_no% + 1%
          print page
          print using L03330 , date$, rpt_time$, user$, page_no%
          print
          print using L03350
          lcnt% = 5%
        return

        print_audit_report
          lcnt% = lcnt% + 3%
          if lcnt% > 55% then gosub print_header
          print using L03370
          print using L03390 , edi_cust$, edi_po$, edi_inv$, edi_seq$
REM          print using L03370
        return

        print_total
          prt% = 1%
          init(" ") rec_tot$
          convert grnd_tot% to rec_tot$, pic(#####)

          print using L03440, rec_tot$

          init(" ") rec_tot$
          convert total% to rec_tot$, pic(#####)

          print using L03445, rec_tot$

          call "SETPRNT" ("FTPR", "FTPR", 0%, 1%)
        return

        write_trailer
          gosub initialize_data               /* Initialize Data Variable */
          init(" ") rec_tot$, lne_tot$, qty_tot$, amt_tot$
          convert total% to rec_tot$, pic(00000)

          convert grnd_tot% to lne_tot$, pic(0000000)

          convert total_qty% to qty_tot$, pic(0000000000)

          total_amt = total_amt * 100

          convert total_amt to amt_tot$, pic(000000000000)

          put #2, using L02000, "T", rec_tot$, lne_tot$, qty_tot$, amt_tot$," "

L02000:     FMT CH(01), CH(05), CH(07), CH(10), CH(12), CH(365)

          write #2              /* Write EWDFTPSN Detail Records */

          prt% = 0%
        return
        
                                                        /*  (EWD003)  BEG */
        check_ww_seq
             ww_line_num% = 0%
REM             if sav_ww_seq$ <> edi_ww_seq$ then return
             if sav_sss_inv$ <> edi_inv$ then return
             if sav_ww_seq$ <> edi_ww_seq$ then return

             ww_line_num% = 1%
REM                sav_ww_seq$ = edi_ww_seq$
                sav_sss_inv$ = edi_inv$
                gosub check_sss
                if sss% = 0% then return
              sss_qty% = 0%
              convert edi_qty$ to sss_qty%, data goto L02500

L02500:       sss_edi_qty% = sss_qty%
              convert sss_edi_qty% to sss_edi_qty$, pic(0000000)

              sss_edi_cpu = sss_edi_cpu + edi_cpu
              write_sss% = 1%

        return

        check_sss
           sss% = 0%
           init(" ") readkey$, desc$ 
           str(readkey$,1%,9%)   = "SCREENONL"
           str(readkey$,10%,15%) = str(edi_ell_part$,1%,3%)
           read #3,key = readkey$, eod goto not_sss
               sss% = 1%
               gosub set_saves
        not_sss
        return

        set_saves
           sss_date$ = edi_date$
REM           sss_type$ = edi_type$
           sss_cust$ = edi_cust$
           sss_inv$  = edi_inv$
           sss_seq$  = edi_seq$
           sss_part$ = edi_part$
           sss_uom$  = edi_uom$
           sss_cpu   = edi_cpu
           sss_desc$ = edi_desc$
           sss_vendor$ = edi_vendor$
           sss_po$   =  edi_po$
           sss_ell_part$ = edi_ell_part$
           sss_width$ = edi_width$
           sss_height$ = edi_height$
           sss_so$   = edi_so$
           sss_code$ = edi_code$
           sss_cd_desc$ = edi_cd_desc$
           sss_ww_seq$ = edi_ww_seq$
        return
                                                        /*  (EWD003)   END */


        unpackData

           edi_date$ = str(ewdrnRec$,1%,6%)
           edi_type$ = str(ewdrnRec$,7%,2%)
           edi_cust$ = str(ewdrnRec$,9%,9%)
           edi_inv$ = str(ewdrnRec$,18%,8%)
           edi_seq$ = str(ewdrnRec$,26%,3%)
           edi_part$ = str(ewdrnRec$,29%,10%)
           edi_qty$ = str(ewdrnRec$,39%,7%)
           edi_uom$ = str(ewdrnRec$,46%,2%)


           get str(ewdrnRec$) using fmtCpu, edi_cpu
fmtCpu:        FMT POS(48), PD(14,5)


           edi_vendor$ = str(ewdrnRec$,95%,6%)
           edi_po$ = str(ewdrnRec$,101%,16%)
           edi_ell_part$ = str(ewdrnRec$,117%,25%)

           if edi_desc$ = " " then gosub checkMull
           if edi_desc$ = " " and mull% = 0% then  ~
                  edi_desc$ = str(ewdrnRec$,56%,39%)
           if edi_desc$ = " " and mull% = 1% then  ~
                  edi_desc$ = "MULL-" & str(ewdrnRec$,56%,34%)


           edi_width$ = str(ewdrnRec$,142%,20%)
           edi_height$ = str(ewdrnRec$,162%,20%)
           edi_so$ = str(ewdrnRec$,182%,8%)
           edi_code$ = str(ewdrnRec$,190%,1%)
           edi_cd_desc$ = str(ewdrnRec$,191%,30%)
           edi_ww_seq$ = str(ewdrnRec$,221%,3%)

           convert edi_qty$ to ediQty%, data goto badQty

badQty:
           if edi_qty% = 0% then edi_qty% = ediQty%


REM           totQty% = totQty% + ediQty%
           totCpu  = totCpu  + edi_cpu
        return

        checkMull
           mull% = 0%
REM not mull if only 19 or less 
           if len(edi_ell_part$) <= 19 then return
REM mull info in 23, 25
           if len(edi_ell_part$) > 22 and         ~
               str(edi_ell_part$,23,25) >= "A01" then mull% = 1%
REM mull info in 20, 22
           if len(edi_ell_part$) > 19 and         ~
               str(edi_ell_part$,20,22) >= "A01" then mull% = 1%
        return

*       *****************************************************************
*        FORMAT STATEMENTS FOR AUDIT REPORT
*       ****************************************************************

L03330: %######## ######## ID: ###     EWD EDI TRANSMISSION AUDIT REPORT ~
        ~      PAGE: ####
L03350: %CUSTOMER        PURCHASE ORDER         INVOICE NUMBER       SEQ

L03370: %----------------------------------------------------------------~
        ~-----
L03390: %############### ####################   ###############      ###

L03440: %TOTAL RECORDS                                              ######

L03445: %TOTAL INVOICES                                             ######


        exit_program
         if write_sss% = 1% then gosub write_sss          /*  (EWD003)  */
         gosub print_total
         gosub write_trailer
         close #1                                /* Close EWDFTPMR File */
         close #2                                /* Close EWDEDISR File */
         close #3                                /* Close GENCODES File */
         end


