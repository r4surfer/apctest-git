        rem**************************************************************~
*                            ( as of 11/21/00 - CMG )                  *~
*        apcedipo - create customer sales orders from edi po's         *~
*                   ( remove re-ference to 'APCPRICE' no longer used ) *~
*                                                                      *~
*                   special sub routines                               *~
*                                                                      *~
*                   (1) apcpln3b - calc default due date - old apcdate *~
*                   (2) bcknext  - assign next s.o. number             *~
*                   (3) apcpcsub - calculate price of part             *~
*                   (4) apcpln5b - update (apcplnor,apcplnsc,apcplnsd) *~
*                   (5) apcprzsb - find new series name                *~
*                                                                      *~
*          note- all subroutines have debug 'STOP' statements in place *~
*                but are currently commented out.                      *~
*                                                                      *~
*              - special code at lines (4000 - 4060) for re-formatting *~
*                s.o. po number. lowes/home quarters (build_bckmastr)  *~
*                                                                      *~
*              - special code at lines (2390 - 2440) for re-formatting *~
*                s.o. po number. lowes/home quarters (check_po)        *~
*                                                                      *~
*              - fixed price for stock parts for all sku number parts. *~
*                part price is found in apcskuno file. entered by      *~
*                customer service. sales literature and parts are valid*~
*                sku's numbers with no mfg 19 digit part no.           *~
*                                                                      *~
*              - daily sales report file where booking entries are made*~
*                new routine 'UPDATE_BOOKINGS' creates a new sales     *~
*                order entry for file. s.o. dollars for edi orders will*~
*                show on the daily report. (bckbkgrf - #16 )           *~
*                                                                      *~
*        --->  - (build_sales_acct) - special routine to lookup the    *~
*                sales account and discount account for the specified  *~
*                model.                                                *~
*                                                                      *~
*        --->  - for sales quote data (edi_partner) = "099" all the    *~
*                s.o. data is pulled from the file (apcpcrq) the viqs  *~
*                bridge interface file.                                *~
*                                                                      *~
*        --->  - rq_quote$ and rq_job$ are saved in the 1st two        *~
*                variable fields in bckmastr. also header and line     *~
*                item text can be passed from the viqs quote system.   *~
*                                                                      *~
*        --->  - rq_fax$ flag will be "Y" if the sales order acknow.   *~
*                is to be sent. ( not turned on in 'APCEDIPO' )        *~
*                                                                      *~
*        --->  - rq_due$ when passed via control file, will be used by *~
*                (apcpln3b). with an fob$ code of '00' date passed     *~
*                will be used.                                         *~
*        --->  - (update_daily_sales) new update the daily sales       *~
*                register file. used by program (apcrpt04)             *~
*                (EWD002) Now Done in (APCPLN6B)                       *~
*       11/18/97 - Checked For 60403 Revision.  No Mods Needed  DJD    *~
*                                                                      *~
*       11/13/98 - y2k compliance                               DJD    *~
*       05/28/98 = (EWD001) Wood Surround Group Code            RHH    *~
*       11/05/98 - (EWD003) Mod for Private Labels. Note that   RHH    *~
*                    3rd channel is a dummy. Does not support          *~
*                    multiple Private lables on same S.O.              *~
*       11/09/98 - (EWD004) Mod to remove all references to the RHH    *~
*                    File (APCPCRQ) Channel (#12)                      *~
*       08/24/00 - (EWD005) Mod to use four digits of customer  CMG    *~
*                    code in Purchase Order                            *~
*       11/21/00 - (EWD006) Mod to have enough room in          CMG    *~
*                     APCEDI for all 8 digits of SO.                   *~
*     03/31/2020 - CR1837  Add PlyGem category PGPA             RDB    *~
*     04/07/2020 - CR2501  Add brand 18 with 19                 RDB    *~
************************************************************************

        sub "APCEDIPO" ( #1,             /* (CUSTOMER) File            */~
                         #2,             /* (GENCODES) File            */~
                         #3,             /* (APCEDIRF) EDI Customer Ref*/~
                         #4,             /* (APCEDI  ) Processing File */~
                         #5,             /* (APCSKUNO) Part Sku No. Fil*/~
                         #6,             /* (BCKMASTR) S.O. Header File*/~
                         #7,             /* (BCKLINES) S.O. Detail File*/~
                         #8,             /* (STORNAME) Store Master    */~
                         #9,             /* (BCKBUF2 ) S.O. Detail Buf */~
                         #10,            /* (BCKHLNES) S.O. Detail Hist*/~
                         #11,            /* (DEMMASTR) Demmand Master  */~
                                         /* (APCPCRQ ) (EWD004)        */~
                         #13,            /* (HNYMASTR) Inventory Master*/~
                         #14,            /* (CPRPRICE) CAELUS PRICE FIL*/~
                         #15,            /* (APCPLNSA) DAILY SALES REG.*/~
                         #16,            /* (BCKBKGRF) S.O. BOOKING FIL*/~
                         #17,            /* (CATEGORY) OBTAIN G/L ACCTS*/~
                         #19,            /* (BCKPRIDX) S.O. PRINT INDEX*/~
                         #20,            /* (HNYQUAN ) INV QUANTITIES  */~
                         #21,            /* (APCPLNOR) S.O. HEADER DATA*/~
                         #22,            /* (APCPLNSC) S.O. LINE DATA  */~
                         #23,            /* (APCPULLS) STOCK PULL'S    */~
                         #24,            /* (APCPLNDP) DEPT UNITS      */~
                         #25,            /* (APCPLNSD) S.O. SCHEDULING */~
                         #29,            /* (EWDSCHED) New             */~
                         edi_buf$(),     /* - (APCEDI02) TRANS BUFFER  */~
                         err% )          /* ERROR FLAG - 0% = OK       */

        dim cat_key$4,                   /* PRIM KEY VIQS-PRICE QUOTE  */~
            or_cat$4,                    /* Customer Delivery Code     */~
            due_date$8,                  /* Calculate Default Due Date */~
            or_no$(5%)8,                 /* PLANNING VALUES            */~
            stk$(99%)25, qty$(99%)4,     /* Part (25), Qty (4)         */~
            due_dte$8,                   /* DUE DATE OVERRIDE          */~
            datetime$7,                  /* DATE AND TIME STAMP        */~
            fob$20,                      /* FOB - Delivery Info        */~
            fob1$20,                     /* FOB - Delivery Info        */~
            rpt_time$8,                  /* REPORT TIME                */~
            edi_buf$(6%)20,              /* EDI BUFFER FOR (APCEDI02)  */~
            edi_partner$3,               /* TRADING PARTNER CODE       */~
            edi_p$15,                    /* TRADING PARTNER ID         */~
            check_po$16,                 /* USE WHEN S.O. EXISTS       */~
            msg$41,                      /* USED BY CHECK_PO           */~
            mm$40,                       /* Display Text               */~
            cnt$3,                       /* Completed S.O.             */~
            dte$8,                       /*                            */~
            qty$4,                       /* LINE ITEM QTY              */~
            ln_price$10,                 /* LINE ITEM PRICE            */~
            s_23m$3,                     /* Model Code for Series      */~
            s_23$8,                      /* New Series Code            */~
            s_so$8, s_ln$3, s_prv$30,    /* Not Used       (EWD003)    */~
            s_1$2,                       /* Private Label Code(EWD003) */~
            sku_key$28,                  /* Primary Key                */~
            sku_code$3,                  /* Customer Sku Code          */~
            sku_price$10,                /* Price Assigned to Sku      */~
            so$16,                       /* SALES ORDER ASSIGNED       */~
            txt$(15%)25,                 /* ERROR MESSAGES             */~
            title$40,                    /* REPORT TITLE               */~
            readkey$50,                  /* Generic Key                */~
            desc$32,                     /* DESCRIPTION - SALES ACCT   */~
            edi_key$41,                  /*                            */~
            edi_processed$1,             /* Data Processed ( Y or N )  */~
            edi_dun$15,                  /* Parent Partner Control     */~
            sav_dun$15,                  /* Parent Partner Control     */~
            edi_store$6,                 /* Parent Store Code          */~
            sav_store$6,                 /* Parent Store Code          */~
            edi_po$16,                   /* Parent PO Number           */~
            sav_po$16,                   /*    ""  ""   ""             */~
            edi_ln$3,                    /* Parent PO Line Number      */~
            edi_sku$25,                  /* Parent SKU Number          */~
            edi_qty$10,                  /* PO Line Item Quantity      */~
            edi_so$16,                   /* S.O. Number Assigned       */~
            edi_cust$9,                  /* APC Customer Code          */~
/*EWD005*/  edi_cust2$4,                 /* APC Customer Code2         */~
            edi_part$25,                 /* APC Part Number            */~
            edi_rec$102,                 /* (APCEDI) Record            */~
            edi_part_desc$32             /* APC Part Description       */

        dim rh1$8,          /* SORE DUE DATE                           */~
            rh2$2,          /* STORE FOB CODE                          */~
            rh3$2,          /* STORE HOW SHIP CODE                     */~
            rh4$8           /* Store Today's Date                      */

        dim a02$9,          /* Customer Code                           */~
            a03$16,         /* Sales Order Mumber                      */~
            a04$16,         /* Purchase Order Number                   */~
            a05$(6%)30,     /* Ship-To Name and Address                */~
            a06$(6%)30,     /* Sold-To Name and Address                */~
            a07$20,         /* Payment Terms                           */~
            a08$20,         /* How Ship Information                    */~
            a09$20,         /* F.O.B. Information                      */~
            a10$(2%)50,     /* Shipping Instructions                   */~
            a11$9,          /* Sales account number                    */~
            a12$9,          /* Discounts Account                       */~
            a13$(3%)4,      /* Salesman Codes                          */~
            a14%(3%),       /* Percentage of Sale credited to salesman.*/~
            a16$4,          /* Region code                             */~
            a17$200,        /* Variable Fields                         */~
            a18$4,          /* Internal ID to text in TXTFILE.         */~
            a19$3,          /* Store Code                              */~
            a20$6,          /* Order Date                              */~
            a21$6,          /* Cancellation Date                       */~
            a22$6,          /* Due Date default                        */~
            a23$6,          /* Date Released                           */~
            a24$6,          /* Originally Input On (date)              */~
            a25$3,          /* Originally Input By (user)              */~
            a26$6,          /* Last Modified On (date)                 */~
            a27$3,          /* Last Modified By (user)                 */~
            a28$9,          /* Adjustment Reason Code                  */~
            a29$1,          /* Export Flag                             */~
            a30$1,          /* Pricing Code                            */~
            a33$1,          /* Credit Hold Flag                        */~
            a36$2,          /* Customer Type Code                      */~
            a37$9,          /* Account X-Ref                           */~
            a38$4,          /* Currency code                           */~
            a39$104         /* Filler                                  */


        dim b01$9,          /* Customer Code                           */~
            b02$16,         /* Sales Order number                      */~
            b03$3,          /* Sequence Number                         */~
            b04$3,          /* Item Number                             */~
            b05$25,         /* Part Number                             */~
            b06$32,         /* Part Number description                 */~
            b07$4,          /* Category code                           */~
            b15$4,          /* Stocking UOM                            */~
            b16$4,          /* Pricing Unit of Measure                 */~
            b20$1,          /* Taxable (y/n) indicator                 */~
            b21$9,          /* Sales Account number                    */~
            b22$9,          /* Discounts Account                       */~
            b23$6,          /* Due Date - Original                     */~
            b24$6,          /* Due Date - Current                      */~
            b25$6,          /* Required Ship Date                      */~
            b26$6,          /* Lot Number                              */~
            b27$8,          /* Code/# of a Project                     */~
            b28$8,          /* Filler                                  */~
            b29$1,          /* Demand Type                             */~
            b30$1,          /* Priority Code                           */~
            b31$4,          /* Internal ID to text in TXTFILE.         */~
            b32$1,          /* Allocation Flag                         */~
            b33$32,         /* Filler                     (EWD001)     */~
            b34$1,          /* Configuration Group Code   (EWD001)     */~
            b35$2,          /* Configuration type         (EWD003)     */~
            b36$2,          /* Private Label Code         (EWD003)     */~
            b37$17,         /* Filler Area                (EWD003)     */~
            rq_grp$1        /* Wood surround Group Code   (EWD001)     */

/*  <<<<<<<<<<< Y2K >>>>>>>>>> */
        dim blankdate$8     /* Create an empty date for compares       */
            call "DATUFMTC" (blankdate$)
/*  <<<<<<<<<<< Y2K >>>>>>>>>> */

            edi_partner$ = str(edi_buf$(1%),1%,3%)
            edi_p$       = str(edi_buf$(2%),1%,15%)
            due_dte$     = str(edi_buf$(3%),1%,8%)
            fob$         = str(edi_buf$(4%),1%,20%)
            
            fob1$ = "06/02-Two    Wk-(11)"          /* F.O.B. Info.     */
            date$ = date
            call "DATEFMT" (date$)
            init(" ") due_date$
            
/*  <<<<<<<<<<< Y2K >>>>>>>>>> */                        
            due_date$ = blankdate$
            if due_dte$ <> " " and due_dte$ <> blankdate$ then due_date$ = due_dte$
/* <<<<<<<<<<< Y2K >>>>>>>>>> */                        

        REM CALL "APCDATE" (DUE_DATE$)

            txt$( 1%) = "(1)-Error Update BCKMASTR"
            txt$( 2%) = "(2)-Error Customer CR-REF"
            txt$( 3%) = "(3)-Error Lookup Customer"
            txt$( 4%) = "(4)-Error Getting Next SO"
            txt$( 5%) = "(5)-Error Updating Group "
            txt$( 6%) = "(6)-Error Lookup SKU No. "
            txt$( 7%) = "(7)-Error Reading HNYMAST"
            txt$( 8%) = "(8)-Error Calc Price     "
            txt$( 9%) = "(9)-Error Update BCKLINES"
            txt$(10%) = "(10)-S.O. Already Exists "

            init(" ") sku_code$, sku_key$, sku_price$
            err% = 0%

        REM *************************************************************~
            *              M A I N L I N E   P R O C E S S I N G        *~
            *-----------------------------------------------------------*~
            * Special Routines                                          *~
            *************************************************************

            gosub select_printer
            t_run_cnt% = 0% : t_run_amt = 0.0 : cnt% = 0%
            init(" ") sav_dun$, sav_po$, sav_store$
            edi_processed$ = "N" : bckmastr% = 0% : bcklines% = 0%
            call "SHOSTAT" ("Converting PO'S to Sales Orders")
        REM ERROR% = 99%                           /* Not Applicable   */
            edi_key$ = all(hex(00))
            str(edi_key$,1%,1%)  = edi_processed$
            str(edi_key$,2%,15%) = edi_p$
        process_next
            read #4,key > edi_key$, using L02520 , edi_rec$,               ~
                                                    eod goto process_done
L02520:       FMT CH(102)
            if edi_processed$ <> str(edi_rec$,10%,1%) then               ~
                                                        goto process_done
            error% = 0%
            edi_key$   = str(edi_rec$,10%,41%)
            edi_dun$   = str(edi_rec$,11%,15%)
            if edi_p$ <> edi_dun$ then goto process_next

                                         /* (EWD004) Remove APCPCRQ    */
            rq_grp$ = "Z"                /* (EWD001) - Set Default     */      
        REM                              /* Standard EDI Processing    */
            edi_store% = 0%              /* (EWD004) Remove            */
            convert str(edi_rec$,26%,6%) to edi_store%,data goto L02680
L02680:
            convert edi_store% to edi_store$, pic(000000)

            edi_po$    = str(edi_rec$,32%,16%)
            edi_ln$    = str(edi_rec$,48%,3%)
            edi_sku$   = str(edi_rec$,51%,25%)
            edi_qty$   = str(edi_rec$,76%,10%)
            gosub get_customer

        REM - PRICE QUOTE ENTRY POINT
            if len(due_dte$) >= 6 then goto L02830
            init(" ") due_date$, fob$
            due_date$ = blankdate$                      /* Y2K */
                                                  /* (EWD004) Remove    */

L02830:     call "APCPLN3B" (due_date$, edi_cust$, fob$, #1, #2)
            call "DATUNFMT" (due_date$)
            gosub check_fob
            if fob$ <> " " then fob1$ = fob$
            dte$ = due_date$
            call "DATEFMT" (dte$)

            if error% = 0% then goto L02920
               goto process_next
L02920:     gosub l_part
            if error% = 0% then goto L02950
               goto process_next
L02950:     if edi_dun$ = sav_dun$ and edi_store$ = sav_store$ and       ~
                                        edi_po$ = sav_po$ then goto L03080
               sav_dun$   = edi_dun$
               sav_store$ = edi_store$
               sav_po$    = edi_po$
               if bckmastr% = 1% then gosub write_bckmastr
                  bckmastr% = 1%
                  bcklines% = 0%
                  gosub lookup_customer
                  gosub assign_so
                  gosub build_bckmastr

L03080:     gosub build_price
            gosub build_bcklines
            gosub build_sales_acct
            gosub write_bcklines
            read #4,hold,key = edi_key$, using L02520 , edi_rec$,          ~
                                                    eod goto process_done
            if error% <> 0% then goto L03240
               delete #4
               str(edi_rec$,10%,1%) = "Y"               /* (EWD006) */
               put str(edi_rec$,98%,4%), using L03180 , edi_so%
L03180:           FMT BI(4)
               str(edi_rec$,32%,16%) = a04$
               write #4, using L02520  , edi_rec$

               /* (EWD004) update_viqs  */

L03240:        if po% = 1% then error% = 10%  /*MOD S.O. ALREADY EXISTS*/
               gosub print_detail
            goto process_next
        process_done
            if bckmastr% = 1% then gosub write_bckmastr
            print using L06530
            convert t_run_cnt% to t_run_cnt$, pic(####)
            convert t_run_amt to t_run_amt$, pic(######.##-)
            print using L06620 , " ", " ", t_run_cnt$, t_run_amt$, " "
            print using L06470

            gosub close_printer
        goto exit_program


        check_po                   /* CHECK TO SE IF SO ENTERED FOR PO */
          po% = 0%
          check_po$ = edi_po$
          if edi_partner$ = "001" then             /* LOWES STORES     */~
                         check_po$ = edi_cust2$ & "-" & edi_po$
                                                   /* (EWD005)         */
        REM  IF EDI_PARTNER$ = "002" THEN          /* HOME QUARTERS    */~
        REM              CHECK_PO$ = EDI_PO$ & "-" & STR(EDI_CUST$,3%,4%)

          read #6,key 1% = check_po$, using L03500 , msg$, eod goto L03550
            goto L03510
L03490:   read #6, using L03500 , msg$, eod goto L03550
L03500:     FMT CH(41)
L03510:   if check_po$ <> str(msg$,26%,16%) then goto L03550
          if edi_cust$ <> str(msg$,1%,9%) then goto L03490
             po% = 1%
             so$ = str(msg$,10%,16%)
L03550:   msg$ = " "
        return

        REM *************************************************************~
            *  B U I L D   E D I   S A L E S   O R D E R                *~
            *-----------------------------------------------------------*~
            * Special Routines                                          *~
            *************************************************************

        get_customer                                     /* (APCEDIRF) */
            if error% <> 0% then return
            edi_cust2% = 0%
            readkey$ = all(hex(00))
            str(readkey$,1%,15%) = edi_dun$
            str(readkey$,16%,6%) = edi_store$
            read #3,key = readkey$, using L03700 , edi_cust$,eod goto L03720
L03700:        FMT POS(22), CH(9)

            convert str(edi_cust$,3%,4%) to edi_cust2%, data goto L03720

            convert edi_cust2% to edi_cust2$, pic(#000)

            call "SPCSMASH" (edi_cust2$)
        return
L03720:    error% = 2%
           edi_cust$ = edi_store$
        return

        l_part                                           /* (APCSKUNO) */
            if error% <> 0% then return                  /* (EWD004)   */
            sku_code$ = " "
            sku_price = 0.0
            read #1,key = edi_cust$, using L03830, sku_code$,eod goto L04030
L03830:        FMT POS(1000), CH(3)

            sku_key$ = all(hex(00))
            str(sku_key$,1%,3%)  = sku_code$
            str(sku_key$,4%,25%) = edi_sku$
            read #5,key = sku_key$, using L03900 , edi_part$, sku_price,   ~
                                                            eod goto L04030
L03900:       FMT POS(32), CH(25), PD(14,4)
                                                         /* (HNYMASTR) */
            read #13,key = edi_part$, using L03940 ,                       ~
                                           edi_part_desc$, eod goto L04050
L03940:       FMT POS(26), CH(32)
            s_23% = 0%                             /* (EWD003) - Begin */
            s_23m$ = str(edi_part$,1%,3%)
            init(" ") s_so$, s_ln$, s_prv$, s_1$
            prv% = 1%                                 /* Use BCKLINES    */
            call "APCPRZSB" (prv%, s_1$, edi_cust$, s_23m$, s_so$,         ~
                                   s_ln$, s_prv$, s_23$, s_23%,            ~
                                   #1, #2, #7, #7, x_er% )
            if x_er% <> 0% then return
               str(edi_part_desc$,1%,8%) = s_23$

        return                                     /* (EWD003) - End   */
L04030:     error% = 6%
        return
L04050:     error% = 7%
        return

        lookup_customer
            ord_amt, t_ord_amt = 0.0
            t_ord_cnt% = 0%
            if error% <> 0% then return
            read #1,key = edi_cust$, using L04270 ,                        ~
                                   a05$(),      /* Ship To Name/Address*/~
                                   a31,         /* Order Disc Pcnt     */~
                                   a30$,        /* Pricing Code        */~
                                   a07$,        /* Payment Terms       */~
                                   a08$,        /* How Ship Info       */~
                                   a13$(),      /* Salesman Codes      */~
                                   a14%(),      /* Pcnt of Sale        */~
                                   a16$,        /* Sales Region Code   */~
                                   a37$,        /* Account Xref        */~
                                   sku_code$,   /* Customer Sku Code   */~
                                   a36$,        /* Customer Type Code  */~
                                   a38$,        /* Currency Code       */~
                                   eod goto L04400

L04270:    FMT POS(253), 6*CH(30), POS(517), PD(14,4), CH(1), POS(543),  ~
               CH(20), CH(20), POS(714), 3*CH(4), 3*BI(1), CH(4),        ~
               POS(771), CH(9), POS(1000), CH(3), POS(1023), CH(2),      ~
               POS(1045), CH(4)

            a08$ = "00/OUR TRUCK (EDI)"
            init(" ") or_cat$                 /* (EWD004) Remove       */  
            gosub check_po                    /*MOD SEE IF S.O. EXISTS */
        return
L04400:    error% = 3%                              /* LOOKUP_CUSTOMER */
        return

        build_bcklines
            if error% <> 0% then return
            ln_amt = 0.0 : seq% = 0%
            b01$ = edi_cust$                       /* Customer Code    */
            b02$ = edi_so$                         /* Sales Order Ass. */
            convert edi_ln$ to seq%, data goto L04490
L04490:
            convert seq% to b03$, pic(###)         /* Next Seq No.     */
            b04$ = " "                             /* Item Number      */
            b05$ = edi_part$                       /* APC Part No.     */
            b06$ = edi_part_desc$                  /* Part Description */
            b07$ = str(edi_part$,1%,3%) & "2"      /* Category Code    */
            x = 0.0
            convert edi_qty$ to x, data goto L04570
L04570:
            b08 = x                                /* Order Quantity   */
            b09 = 0.0                              /* Qty Shipped Tot  */
            b10 = x                                /* Qty Open         */
            b11 = 0.0                              /* Qty Sch Ship     */
            b12 = x                                /* Qty Allocated    */
            b13 = 0.0                              /* Qty Pre-Invoiced */
                                                   /* B14 - Calc Price */
            b15$ = "EA  "                          /* Stocking Unit    */
            b16$ = "EA  "                          /* Pricing Unit     */
            b17 = 1.00                             /* Conversion Price */
            b18 = b14                              /* Unit Price       */
            b19 = 0.0                              /* Price Discount   */
            b20$ = "N"                             /* Taxable (Y/N)    */
            b21$ = "3607-313"                      /* Sales Account No.*/
            b22$ = "3650-313"                      /* Discounts Account*/
/* <<<<<<<<<< Y2K >>>>>>>>>> */
            b23$ = due_date$                       /* Due Date - Orig  */
            b24$ = b23$                            /* Due Date - Curr  */
            b25$ = b23$                            /* Req Ship Date    */
/* <<<<<<<<<< Y2K >>>>>>>>>> */
            b26$ = " "                             /* Lot Number       */
            b27$ = " "                             /* Code/Project     */
            b28$ = " "                             /* Filler           */
            b29$ = "1"                             /* Demand Type      */
            b30$ = "A"                             /* Priority Code    */
            b31$ = " "                             /* Text Id          */
                                          /* (EWD004) QUOTE LINE TEXT  */
            b32$ = "C"                             /* Allocation Flag  */
            b33$ = " "                             /* (EWD001) Not Used*/
            b34$ = rq_grp$                         /* (EWD001) Config  */
            b35$ = "  "                            /* (EWD003) Fig Type*/
            b36$ = s_1$                            /* (EWD003) Prv Lab */
            b37$ = " "                             /* (EWD003) Filler  */
   
            ln_amt = round(x * b14, 2)
            ord_amt = ord_amt + ln_amt
            ord% = seq%
            convert x to qty$, pic(####)

            convert b14 to ln_price$, pic(######.##-)

            t_ord_cnt% = t_ord_cnt% + x
            t_ord_amt  = t_ord_amt + ln_amt
            t_run_cnt% = t_run_cnt% + x
            t_run_amt  = t_run_amt + ln_amt

        return

        build_bckmastr
            if error% <> 0% then return
                                         /* INITIALIZE - ORD_AMT = 0.0 */
            ord_amt = 0.0 : ord% = 1%    /*              ORD% = 1%     */
            a02$ = edi_cust$                       /* Customer Code    */
            a03$ = edi_so$                         /* Sales Order Code */
                                                   /* Customer PO No.  */
            a04$ = edi_po$
            if edi_partner$ = "001" then           /* LOWES STORES     */~
                            a04$ = edi_cust2$ & "-" & edi_po$
                                                   /* (EWD005) - Change to get all */
                                                   /* four digits of customer code */
                                                   
        REM    IF EDI_PARTNER$ = "002" THEN        /* HOME QUARTERS    */~
        REM                 A04$ = EDI_PO$ & "-" & STR(EDI_CUST$,3%,4%)

                                                   /* A05$ - Customer  */
            a06$(1%) = "BILL-TO"                   /* SOLD-TO NAME/ADD */
                                                   /* A07$ - Customer  */
                                                   /* A08$ - Customer  */
            a09$ = fob1$                           /* F.O.B. Info.     */
            a10$(1%) = " "                         /* Shipping Instr.  */
            a10$(2%) = " "
                                                   /* (EWD004) Remove  */
            a11$ = " "                             /* Sales Acct No.   */
            a12$ = "3650-313 "                     /* Discounts Account*/
                                                   /* A13$()- Customer */
                                                   /* A14%()- Customer */
                                                   /* A16$  - Customer */
            a17$ = " "                             /* Variable Fields  */
            a18$ = " "                             /* Text Id          */
                                                   /* (EWD004) Remove  */
            a19$ = "300"                           /* Store Code       */
            a20$ = date                            /* Order Date       */
            a21$ = " "                             /* Cancell Date     */

/* <<<<<<<<<< Y2K >>>>>>>>>> */
            a22$ = due_date$                       /* Due Date         */
            a23$ = a22$                            /* Date Released    */
            a24$ = date                            /* Creation Date    */
/* <<<<<<<<<< Y2K >>>>>>>>>> */

            a25$ = "EDI"                           /* Created By       */
                                             /* (EWD004) Created By    */
            a26$ = " "                             /* Last Mod Date    */
            a27$ = a25$                            /* Last Mod By User */
            a28$ = " "                             /* Adjustment Reason*/
            a29$ = "N"                             /* Export Flag      */
                                                   /* A30$ - Customer  */
                                                   /* A31  - Customer  */
            a32  = ord_amt                         /* A32 - Tot Dollars*/
            a33$ = " "                             /* Credit Hold Flag */
            a34% = ord%                            /* A34% - Last Seq  */
            a35% = 1%                              /* Next BOL Number  */
                                                   /* A36$ - Customer  */
                                                   /* A37$ - Customer  */
                                                   /* A38$ - Customer  */
            a39$ = " "                             /* Filler Area      */

        return

        build_price                      /* GET CORRECT SIZE AND PRICE */
            if error% <> 0% then return
            if sku_price < .01 then goto L05660
               b14 = sku_price
               goto L05670
L05660:     b14 = 0.0
L05670: return
            error% = 8%
        return

        assign_so
            if error% <> 0% then return
            if po% = 1% then goto L05790       /*MOD S.O. ALREADY EXISTS */
               so$ = " " : edi_so% = 0%
               call "BCKNEXT" ( #8, #7, #9, #10, #11, "300", so$ )
               if so$ <> " " then goto L05790
                  goto L05850

L05790:        edi_so$ = str(so$,1%,8%)
               convert edi_so$ to edi_so%, data goto L05850

               convert edi_so% to edi_so$, pic(00000000)

        return
L05850:     error% = 4%
        return

        update_planning
           e% = 0%
           or_no$(1%) = a02$
           or_no$(2%) = a03$
           or_no$(3%) = "99999999"
           or_no$(4%) = "99999999"
           or_no$(5%) = " "
           rh1$ = a22$ & "  "            /* Due Date                   */
           rh2$ = str(a09$,1%,2%)        /* FOB                        */
           rh3$ = str(a08$,1%,2%)        /* How Ship                   */
           rh4$ = date & "  "            /* Todays Date                */


        call "APCPLN5B"  (1%,            /* Option Code (NEW ORDER)    */~
                          rh1$,          /* S.O. Due Date/Delivery Date*/~
                          or_no$(),      /* Customer Number            */~
                                         /* Customer S.O. Number       */~
                                         /* APC Invoice Assoc. with SO */~
                                         /* APC Check Assoc. with Invoi*/~
                          a04$,          /* Customer P.O. Number       */~
                          "00",          /* Current S.O. Stat PLAN STAT*/~
                          rh4$,          /* Date Assoc. with Stat Chg. */~
                          a13$(1%),      /* APC Salesman Code          */~
                          rh2$,          /* Customer Delivery(PLAN DEL?*/~
                          rh3$,          /* Secial Instr (PLAN HOWS)   */~
                          "99999",       /* Load Number Sched/Assigned */~
                          rh4$,          /* Date S.O. Created          */~
                          rh4$,          /* Date S.O. Last Changed     */~
                          a25$,          /* S.O. Last Modified By      */~
                          "        ",    /* Date B.O.L. Created        */~
                          a18$,          /* S.O. Header Text Id        */~
                          stk$(),        /*                            */~
                          qty$(),        /*                            */~
                          #1,            /* CUSTOMER - Master File     */~
                          #21,           /* APCPLNOR - APC Header File */~
                          #22,           /* APCPLNSC - APC Schedule    */~
                          #6,            /* BCKMASTR - S.O. HEADER     */~
                          #7,            /* BCKLINES - S.O. DETAIL     */~
                          #2,            /* GENCODES - TABLES          */~
                          #23,           /* APCPULLS - APC Pull Stock  */~
                          #20,           /* HNYQUAN  - Inventory Qty   */~
                          #24,           /* APCPLNDP - Plan Dept Master*/~
                          #25,           /* APCPLNSD - Plan S.O. Sched */~
                          #13,           /* HNYMASTR - PART MASTER     */~
                          #15,           /* APCPLNSA - Daily Sales     */~
                          #29,           /* EWDSCHED - New Spec. Sched */~
                          e% )           /* 0 = OK, 1 = ERROR          */
            if e% <> 0% then goto L06350
        return
L06350:     error% = 5%
        return

        REM *************************************************************~
            *       R E P O R T   F O R M A T S / P R I N T I N G       *~
            *-----------------------------------------------------------*~
            * Special Routines                                          *~
            *************************************************************

L06440: %!---------------------------------------------------------------~
        ~----------------------------------------------------------------!

L06470: %+---------------------------------------------------------------~
        ~----------------------------------------------------------------+

L06500: %! ######## @ ########                       ####################~
        ~####################                                  PAGE: ### !

L06530: %!------!---------!----------------!---!-------------------------~
        ~-!-------------------------!----!----------!--------!-----------!

L06560: %! STORE!APC CUST ! STORE PO NUMBER!LN !<---- SKU PART NO. ------~
        ~>!<--- APC PART NUMBER --->! QTY!LN ITEM PC!DUE DATE!SALES ORDER!

L06590: %!######!#########!################!###! ########################~
        ~#!#########################!####!##########!########!  ######## !

L06620: %!      !#########!################!   !                         ~
        ~ ! T O T A L S ----------->!####!##########!        !  ######## !

        header
              init(" ") readkey$, title$
              str(readkey$,1%,9%) = "PARTNERS "
L06680:       read #2,key > readkey$, using L06700 , readkey$,             ~
                                       str(title$,9%,32%), eod goto L06740
L06700:          FMT CH(24), CH(32)
              if edi_dun$ <> str(title$,9%,15%) then goto L06680
                 str(title$,1%,8%) = "EDI RPT "

L06740:    if lcnt% <> 99% then print using L06470
           pageno% = pageno% + 1%
           print page
           print using L06470
           print using L06500 , date$, rpt_time$, title$, pageno%
           print using L06440
           print using L06560
           lcnt% = 4%
        return

        print_detail
          if lcnt% > 57% then gosub header
             if error% = 0% then goto L06890
                edi_part$ = txt$(error%)

L06890:      print using L06530
             print using L06590 , edi_store$, edi_cust$, a04$,    edi_ln$, ~
                                edi_sku$, edi_part$, qty$, ln_price$,    ~
                                dte$, edi_so$
             lcnt% = lcnt% + 2%
        return

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        select_printer
            pageno% = 0% : lcnt% = 99%
            rpt_time$ = " "
            call "TIME" (rpt_time$)
            date$ = date
            call "DATEFMT" (date$)
            call "SETPRNT" ("APCSO", " ", 0%, 0%)
            select printer (134)
        return

        close_printer
            call "SETPRNT" ("APCSO", " ", 0%, 1%)
        return

        write_bckmastr                        /* CREATE S.O. HEADER    */
            if error% <> 0% and bcklines% <> 1% then return
            init(" ") readkey$ : bckmastr% = 0% : bcklines% = 0%
            a32 = ord_amt : a34% = ord%
            str(readkey$,1%,9%)   = a02$
            str(readkey$,10%,16%) = a03$
            read #6,key = readkey$, eod goto L07220
        REM    RETURN                        /* S.O. ALREADY EXISTS    */
L07220:        put #6, using L07920 , a02$,                                ~
                                    a03$,                                ~
                                    a04$,                                ~
                                    a05$(),             /* (6) AT (30) */~
                                    a06$(),             /* (6) AT (30) */~
                                    a07$,                                ~
                                    a08$,                                ~
                                    a09$,                                ~
                                    a10$(),             /* (2) AT (50) */~
                                    a11$,                                ~
                                    a12$,                                ~
                                    a13$(),             /* (3) AT (4)  */~
                                    a14%(),                              ~
                                    a16$,                                ~
                                    a17$,                                ~
                                    a18$,                                ~
                                    a19$,                                ~
                                    a20$,                                ~
                                    a21$,                                ~
                                    a22$,                                ~
                                    a23$,                                ~
                                    a24$,                                ~
                                    a25$,                                ~
                                    a26$,                                ~
                                    a27$,                                ~
                                    a28$,                                ~
                                    a29$,                                ~
                                    a30$,                                ~
                                    a31,                                 ~
                                    a32,                                 ~
                                    a33$,                                ~
                                    a34%,                                ~
                                    a35%,                                ~
                                    a36$,                                ~
                                    a37$,                                ~
                                    a38$,                                ~
                                    a39$

           if po% = 1% then goto L07660        /*MOD S.O. ALREADY EXISTS */
              write #6, eod goto L07840
              gosub update_planning
              gosub update_bookings

L07660:    po% = 0%
           if lcnt% > 57% then gosub header
           print using L06530
           convert t_ord_cnt% to t_ord_cnt$, pic(####)
           convert t_ord_amt to t_ord_amt$, pic(######.##-)
           print using L06620 , a02$     , a04$   , t_ord_cnt$, t_ord_amt$,~
                                                                 edi_so$
           lcnt% = lcnt% + 2%
                                                     /* SCREEN DISPLAY */
              cnt% = cnt% + 1%
              convert cnt% to cnt$, pic(###)
              mm$ = "S.O. (        ) Completed (   )"
              str(mm$,7%,8%)  = edi_so$
              str(mm$,28%,3%) = cnt$
              call "SHOSTAT" (mm$)
        return
L07840:    error% = 1%
        return


L07920: FMT                 /* FILE #6  -- BCKMASTR                    */~
            CH(9),          /* Customer Code                           */~
            CH(16),         /* Sales Order Mumber                      */~
            CH(16),         /* Purchase Order Number                   */~
            6*CH(30),       /* Ship-To Name and Address                */~
            6*CH(30),       /* Sold-To Name and Address                */~
            CH(20),         /* Payment Terms                           */~
            CH(20),         /* How Ship Information                    */~
            CH(20),         /* F.O.B. Information                      */~
            2*CH(50),       /* Shipping Instructions                   */~
            CH(9),          /* Sales account number                    */~
            CH(9),          /* Discounts Account                       */~
            3*CH(4),        /* Salesman Codes                          */~
            3*BI(1),        /* Percentage of Sale credited to salesman.*/~
            CH(4),          /* Region code                             */~
            CH(200),        /* Variable Fields                         */~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            CH(3),          /* Store Code                              */~
            CH(6),          /* Order Date                              */~
            CH(6),          /* Cancellation Date                       */~
            CH(6),          /* Due Date default                        */~
            CH(6),          /* Date Released                           */~
            CH(6),          /* Originally Input On (date)              */~
            CH(3),          /* Originally Input By (user)              */~
            CH(6),          /* Last Modified On (date)                 */~
            CH(3),          /* Last Modified By (user)                 */~
            CH(9),          /* Adjustment Reason Code                  */~
            CH(1),          /* Export Flag                             */~
            CH(1),          /* Pricing Code                            */~
            PD(14,4),       /* Order Discount Percent                  */~
            PD(14,4),       /* Open Order Amount                       */~
            CH(1),          /* Credit Hold Flag                        */~
            BI(2),          /* Last Sequence Number Used               */~
            BI(4),          /* Next BOL Number                         */~
            CH(2),          /* Customer Type Code                      */~
            CH(9),          /* Account X-Ref                           */~
            CH(4),          /* Currency code                           */~
            CH(104)         /* Filler                                  */

        write_bcklines                        /* CREATE S.O. LINE ITEM */
            if error% <> 0% then return
            init(" ") readkey$
            str(readkey$,1%,16%) = edi_so$
            str(readkey$,17%,3%) = edi_ln$
            read #7,key = readkey$, eod goto L08380
        REM   RETURN                         /*MOD S.O. ALREADY EXISTS */
L08380:        put #7, using L08780 , b01$,                                ~
                                    b02$,                                ~
                                    b03$,                                ~
                                    b04$,                                ~
                                    b05$,                                ~
                                    b06$,                                ~
                                    b07$,                                ~
                                    b08,                                 ~
                                    b09,                                 ~
                                    b10,                                 ~
                                    b11,                                 ~
                                    b12,                                 ~
                                    b13,                                 ~
                                    b14,                                 ~
                                    b15$,                                ~
                                    b16$,                                ~
                                    b17,                                 ~
                                    b18,                                 ~
                                    b19,                                 ~
                                    b20$,                                ~
                                    b21$,                                ~
                                    b22$,                                ~
                                    b23$,                                ~
                                    b24$,                                ~
                                    b25$,                                ~
                                    b26$,                                ~
                                    b27$,                                ~
                                    b28$,                                ~
                                    b29$,                                ~
                                    b30$,                                ~
                                    b31$,                                ~
                                    b32$,                                ~
                                    b33$,       /*(EWD001) */            ~
                                    b34$,       /*(EWD001) */            ~
                                    b35$,       /*(EWD003) */            ~
                                    b36$,       /*(EWD003) */            ~
                                    b37$        /*(EWD003) */               

           if po% = 1% then return           /* S.O. ALREADY EXISTS    */
              write #7, eod goto L08750
              bcklines% = 1%                 /* AT LEAST ONE LINE ITEM */
        return
L08750:    error% = 9%
        return

L08780: FMT                 /* FILES #7 -- BCKBUF2 AND BCKLINES        */~
            CH(9),          /* Customer Code                           */~
            CH(16),         /* Sales Order number                      */~
            CH(3),          /* Sequence Number                         */~
            CH(3),          /* Item Number                             */~
            CH(25),         /* Part Number                             */~
            CH(32),         /* Part Number description                 */~
            CH(4),          /* Category code                           */~
            PD(14,4),       /* Order Quantity                          */~
            PD(14,4),       /* Quantity Shipped (total)                */~
            PD(14,4),       /* Quantity Open                           */~
            PD(14,4),       /* Quantity scheduled for Shipment         */~
            PD(14,4),       /* Quantity Allocated                      */~
            PD(14,4),       /* Quantity Pre-Invoiced                   */~
            PD(14,4),       /* Unit Price @ Stocking UOM               */~
            CH(4),          /* Stocking UOM                            */~
            CH(4),          /* Pricing Unit of Measure                 */~
            PD(14,7),       /* Conversion Factor (Pricing to Stkng)    */~
            PD(14,4),       /* Unit Price                              */~
            PD(14,4),       /* Pricing Discount Percent                */~
            CH(1),          /* Taxable (y/n) indicator                 */~
            CH(9),          /* Sales Account number                    */~
            CH(9),          /* Discounts Account                       */~
            CH(6),          /* Due Date - Original                     */~
            CH(6),          /* Due Date - Current                      */~
            CH(6),          /* Required Ship Date                      */~
            CH(6),          /* Lot Number                              */~
            CH(8),          /* Code/# of a Project                     */~
            CH(8),          /* Filler                                  */~
            CH(1),          /* Demand Type                             */~
            CH(1),          /* Priority Code                           */~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            CH(1),          /* Allocation Flag                         */~
            CH(32),         /* Not used (EWD001)                       */~
            CH(01),         /* Configuration Group Code (EWD001)       */~
            CH(02),         /* Configuration Type Code  (EWD003)       */~
            CH(02),         /* Private Label Code       (EWD003)       */~ 
            CH(17)          /* Filler Area              (EWD003)       */

        update_bookings                 /* ONLY CREATED FOR S.O. HEADER*/
                                        /* (BCKMASTR)                  */
            call "GETDTTM"  addr(datetime$)
            discamt = round(a32 * a31 *.01,2)
            put #16, using  L09260, "1",     /* Type (1) New S.O.      */~
                                  a02$,      /* Customer Code          */~
                                  a03$,      /* Sales Order Number     */~
                                  datetime$, /* Date and Time Stamp    */~
                                  a05$(1),   /* Ship to Customer Name  */~
                                  a28$,      /* Adjustment Reason Code */~
                                  a32,       /* Gross Pay Amount of S.O*/~
                                  discamt    /* Discount Amount Dollars*/

L09260:     FMT CH(1), CH(9), CH(16), CH(7), CH(30), CH(9), 2*PD(14,4)
            write #16, eod goto L09380

*        Update BCK Print Index Files as requested.
*         Order Acknowledgement Record First /* (EWD004) Remove        */
        return
L09380:     call "SHOSTAT" ("(Error)-Updating Booking File FOR - "& a03$)
            stop
        return

        check_fob
            readkey$ = all(hex(00))
            str(readkey$,1%,9%)   = "PLAN DELV"
            str(readkey$,10%,15%) = str(fob$,1%,2%)
            read #2,key = readkey$, using L09500 , desc$,eod goto L09520
L09500:        FMT POS(25), CH(30)
            str(fob$,3%,18%) = "/" & desc$
L09520: return

                                       /* (EWD004) get_quote_info */
                                       /* (EWD004) update_viqs    */
        build_sales_acct                    /* FOR ALL S.O. LINE ITEMS */
            init(" ") cat_key$
            if len(edi_part$) < 19 then cat_key$ = "PART"
/* CR1837 CR2501 */
            if len(edi_part$) < 19 and (b36$ = "19" or b36$ = "18") ~
                      then cat_key$ = "PGPA"
            if len(cat_key$) > 3 then goto L10130       /* Line is Part */
            str(cat_key$,1%,3%) = str(edi_part$,1%,3%) /*Based on Model*/
            str(cat_key$,4%,1%) = "2"                  /*Color = White */
            if len(or_cat$) > 2 then cat_key$ = or_cat$
L10130:     read #17,key = cat_key$, using L10160, b21$, b22$,            ~
                                                            eod goto     ~
        L10180
L10160:        FMT POS(35), 2*CH(9)
            b07$ = cat_key$
L10180: return


        exit_program

        end