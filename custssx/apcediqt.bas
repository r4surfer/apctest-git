*       ****************************************************************~
*                           ( As of 11/21/00 - RHH )                   *~
*        APCEDIQT - Create Customer Sales Orders from External and     *~
*                   Internal Sales Quotes. (New Windows 95 System)     *~
*                   This Subroutine is designed to nun as a Background *~
*                   Utility called from the program (JBPOST2)          *~
*                                                                      *~
*                   Special Sub Routines ( Similar to APCEDIPO )       *~
*                                                                      *~
*                   (1) APCPLN3B - Calc Default Due Date - Old APCDATE *~
*                   (2) BCKNEXT  - Assign Next S.O. Number             *~
*                   (3) APCPCSUB - Calculate Price of Part             *~
*                   (4) APCPLN5B - Update (APCPLNOR,APCPLNSC,APCPLNSD) *~
*                   (5) APCPRZSB - Find New Series Name                *~
*                                                                      *~
*          Note- All Subroutines have Debug 'STOP' Statements in Place *~
*                but are currently Commented out.                      *~
*                                                                      *~
*                Process control is maintained by accessing the Quote  *~
*                in the new (APCPCRQB) Buffer File. Now only one (1)   *~
*                Sales Order at a time is Processed.                   *~
*                                                                      *~
*              - Daily Sales Report file where Booking Entries are made*~
*                New Routine 'UPDATE_BOOKINGS' Creates a New Sales     *~
*                Order Entry for file. S.O. Dollars for EDI Orders will*~
*                show on the Daily Report. (BCKBKGRF - #16 )           *~
*                                                                      *~
*        --->  - (BUILD_SALES_ACCT) - Special routine to lookup the    *~
*                Sales Account and Discount Account for the specified  *~
*                Model.                                                *~
*                                                                      *~
*        --->  - For Sales Quote Data (EDI_PARTNER) = "099" all the    *~
*                S.O. Data is pulled from the file (APCPCRQ) the VIQS  *~
*                Bridge Interface File.                                *~
*                                                                      *~
*        --->  - RQ_QUOTE$ and RQ_JOB$ are saved in the 1st two        *~
*                Variable Fields in BCKMASTR. Also Header and Line     *~
*                Item text can be passed from the VIQS Quote System.   *~
*                                                                      *~
*        --->  - RQ_FAX$  Flag will be "Y" if the Sales Order Acknow.  *~
*                is to be sent.                                        *~
*                                                                      *~
*        --->  - RQ_DUE$  When passed will be used by (APCPLN3B). With *~
*                FOB$ Code of '00' Date passed will be used.           *~
*                                                                      *~
*        --->  - (APCPLNSA) Update file with Sales Order Data for the  *~
*                daily sales Report. UPDATE_DAILY_SALES                *~
*                                                                      *~
*                                                                      *~
*       11/18/97 CHECKED FOR REVISION 60403  - NONE NEEDED      DJD    *~
*                                                                      *~
*       04/02/98 y2k compliance - DJD                                  *~
*       05/28/98 (EWD001) Special Mod for Wood Surround Group Code     *~
*                                                                      *~
*----------------------------------------------------------------------*~
*                             M O D I F I C A T I O N S                *~
*---WHEN---+---------------------------WHAT----------------------+-WHO-*~
* 11/21/00 ! (EWD002) Mod to have enough room in                 ! CMG *~
*          !          APCEDI for all 8 digits of SO.             !     *~   
* 12/20/18 ! (CR1829) cust length change 6 -> 9 for Dallas       ! DES *~
************************************************************************

        sub "APCEDIQT" ( #1,             /* (CUSTOMER) File            */~
                         #2,             /* (GENCODES) File            */~
                         #3,             /* (APCEDIRF) EDI Customer Ref*/~
                         #4,             /* (APCEDI  ) Processing File */~
                         #5,             /* (APCPCRQB) VIQS Buffer File*/~
                         #6,             /* (BCKMASTR) S.O. Header File*/~
                         #7,             /* (BCKLINES) S.O. Detail File*/~
                         #8,             /* (STORNAME) Store Master    */~
                         #9,             /* (BCKBUF2 ) S.O. Detail Buf */~
                         #10,            /* (BCKHLNES) S.O. Detail Hist*/~
                         #11,            /* (DEMMASTR) Demmand Master  */~
                         #12,            /* (APCPCRQ ) VIQS COMM FILE  */~
                         #13,            /* (HNYMASTR) Inventory Master*/~
                         #14,            /* (CPRPRICE) CAELUS PRICE FIL*/~
                         #15,            /* (APCPLNSA) Daily Sales Reg.*/~
                         #16,            /* (BCKBKGRF) S.O. BOOKING FIL*/~
                         #17,            /* (CATEGORY) OBTAIN G/L ACCTS*/~
                         #19,            /* (BCKPRIDX) S.O. PRINT INDEX*/~
                         #20,            /* (HNYQUAN ) INV QUANTITIES  */~
                         #21,            /* (APCPLNOR) S.O. HEADER DATA*/~
                         #22,            /* (APCPLNSC) S.O. LINE DATA  */~
                         #23,            /* (APCPULLS) STOCK PULL'S    */~
                         #24,            /* (APCPLNDP) DEPT UNITS      */~
                         #25,            /* (APCPLNSD) S.O. SCHEDULING */~
                         #26,            /* (TXTFILE)                  */~
                         #27,            /* (ARMTERMS)                 */~
                         #28,            /* (BOMSPEC)                  */~
                         #30,            /* (APCVIQER) - ERROR LOG FILE*/~
                         edi_partner$,   /* PARTNER ID (099)           */~
                         edi_p$ )        /* Trading Id/Dun's Number    */


        dim rq_key$41, cat_key$4,        /* PRIM KEY VIQS-PRICE QUOTE  */~
            rq_d1$4, rq_d2$4,            /* HEADER AND LINE TEXT ID    */~
            rq_job$16 ,                  /* CUSTOMER JOB NAME          */~
            rq_po$16,                    /* Customer P.O. Number       */~
            rq_quote$16,                 /* Customer Quote Id          */~
            rq_desc$32,                  /* MFG Part Description       */~
            rq_so$16, viqs_so$16,        /* Customer S.O. Number       */~
            rq_hows$2, or_hows$2,        /* Customer Howship Code      */~
            rq_fob$2,  or_cat$4,         /* Customer Delivery Code     */~
            rq_ship1$50,                 /* Shipping Instr's Line 1    */~
            rq_ship2$50,                 /* Shipping Instr's Line 2    */~
            rq_due$6,                    /* Sales Order Due Date       */~
            rq_fax$1,                    /* (Y) OR (N) - SEND FAX      */~
            rq_credit$1, sav_credit$1,   /* Credit Hold Flag           */~
            due_date$8,                  /* CALCULATE DEFAULT DUE DATE */~
            or_no$(5%)9,                 /* PLANNING VALUES            */~
            or_status$2,                 /* Planning Status Code       */~
            sa_key$17,                   /* PRIMARY KEY (APCPLNSA)     */~
            sa_rec$32,                   /*                            */~
            sa_st$2,                     /*                            */~
            stk$(99%)25, qty$(99%)4,     /* Part (25), Qty (4)         */~
            datetime$7,                  /* DATE AND TIME STAMP        */~
            fob$20,                      /* FOB - Delivery Info        */~
            fob1$20,                     /* FOB - Delivery Info        */~
            edi_partner$3,               /* TRADING PARTNER CODE       */~
            edi_p$15,                    /* TRADING PARTNER ID         */~
            err_key$45,                  /* Error Log Key              */~
            err_store$6,                 /* Error STORE                */~
            err_quote$16,                /* Error QUOTE                */~
            err_ln$3,                    /* Error LINE NO              */~
            check_po$16,                 /* USE WHEN S.O. EXISTS       */~
            msg$41,                      /* USED BY CHECK_PO           */~
            dte$8,                       /*                            */~
            qty$4,                       /* LINE ITEM QTY              */~
            ln_price$10,                 /* LINE ITEM PRICE            */~
            s_23m$3,                     /* Model Code for Series      */~
            s_23$8,                      /* New Series Code            */~
            sku_key$28,                  /* Primary Key                */~
            sku_code$3,                  /* Customer Sku Code          */~
            sku_price$10,                /* Price Assigned to Sku      */~
            so$16,                       /* SALES ORDER ASSIGNED       */~
            txt$(20%)35,                 /* ERROR MESSAGES             */~
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
            sav_fax$1,                   /* SAVE FROM RQ_FAX$          */~
            edi_ln$3,                    /* Parent PO Line Number      */~
            edi_sku$25,                  /* Parent SKU Number          */~
            edi_qty$10,                  /* PO Line Item Quantity      */~
            edi_so$16,                   /* S.O. Number Assigned       */~
            edi_cust$9,                  /* APC Customer Code          */~
            edi_part$25,                 /* APC Part Number            */~
            edi_rec$102,                 /* (APCEDI) Record            */~
            edi_part_desc$32             /* APC Part Description       */

        dim hdr_rec$64,     /* New Header Buffer Record Only '00' s    */~
            hdr_key$24,     /* Primary Key for buffer                  */~
            hdr_cust$6,     /* Customer Code                           */~
            hdr_quote$16,   /* VIQS Qutoe Id.                          */~
            sav_key$38      /* Primary Key for a Single Sales Order    */

        dim rh1$8,          /* Store Due Date                          */~
            rh2$2,          /* Store FOB Code                          */~
            rh3$2,          /* Store Howship Code                      */~
            rh4$8           /* Store Today's Date                      */

        dim a02$9, billto$9,/* Customer Code                           */~
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
            b20$1, tax$1,   /* Taxable (y/n) indicator                 */~
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
            b34$1, rq_grp$1,/* Wood Surround Group Code   (EWD001)     */~
            b35$21          /* Filler Area                (EWD001)     */


/*  <<<<<<<<<<< Y2K >>>>>>>>>> */
        dim blankdate$8     /* Create an empty date for compares       */
            call "DATUFMTC" (blankdate$)
/*  <<<<<<<<<<< Y2K >>>>>>>>>> */



            fob1$ = "06/02-Two    Wk-(11)"          /* F.O.B. Info.     */
            date$ = date
            call "DATEFMT" (date$)
            init(" ") due_date$

/*  <<<<<<<<<<< Y2K >>>>>>>>>> */                        
            due_date$ = blankdate$
            if due_dte$ <> " " and due_dte$ <> blankdate$ then due_date$ = due_dte$
/* <<<<<<<<<<< Y2K >>>>>>>>>> */                        

        REM CALL "APCDATE" (DUE_DATE$)

            txt$( 1%) = "(1)@Unable To Read -APCPCRQ Record?"
            txt$( 2%) = "(2)@Unable to Load Customer Data?  "
            txt$( 3%) = "(3)@Customer/P.O. already on File? "
            txt$( 4%) = "(4)@Unable to Assign S.O. Number?  "
            txt$( 5%) = "(5)@Unable to Convert S.O. Number? "
            txt$( 6%) = "(6)@Unable to Load Sales G/L Accts?"
            txt$( 7%) = "(7)@Unable to Update (BCKLINES)?   "
            txt$( 8%) = "(8)-Unable to Read (APCEDI)?       "
            txt$( 9%) = "(9)-Unable to Write to (APCEDI)?   "
            txt$(10%) = "(10)-Unable to Update VIQS?        "
            txt$(11%) = "(11)-Unable to Update (BCKMASTR)?  "
            txt$(12%) = "(12)-S.O. Missing Line Items?      "
            txt$(13%) = "(13)-Unable to Update Planning?    "
            txt$(14%) = "(14)-Unable to Update Bookings Reg?"
            txt$(15%) = "(15)-Unable to Update Acknowledge ?"
            txt$(16%) = "(16)-Unable to Update Daily Sales? "
            txt$(17%) = "(17)-Unable to Update Customer $$ ?"
            txt$(18%) = "(18)-Unable to Update Bill-To  $$ ?"
            txt$(19%) = "(19)-Note No Fax Sent?             "
            txt$(20%) = "(20)-Error in Main Line Process    "

            quote% = 0%

        REM *************************************************************~
            *              M A I N L I N E   P R O C E S S I N G        *~
            *-----------------------------------------------------------*~
            * Special Routines                                          *~
            *************************************************************

        main_line
            init(" ") hdr_key$
            error% = 99%
            str(hdr_key$,1%,2%) = "00"
        main_line_next
            init(" ") hdr_cust$, hdr_quote$, hdr_rec$
            read #5,hold,key > hdr_key$, using L02540 , hdr_key$,          ~
                                                 eod goto main_line_done
L02540:        FMT CH(24)
            if str(hdr_key$,1%,2%) <> "00" then goto main_line_done
               hdr_cust$  = str(hdr_key$,3%,6%)
               hdr_quote$ = str(hdr_key$,9%,16%)

               gosub process                 /* Create (1) Sales Order */
            read #5,hold,key = hdr_key$, using L02650 , hdr_rec$,          ~
                                                      eod goto L02630
               delete #5
L02630:     str(hdr_rec$,1%,2%) = "01"
            put #5, using L02650 , hdr_rec$
L02650:       FMT CH(64)
            write #5, eod goto L02710
            goto main_line_next
        main_line_done
        goto exit_program

L02710:     error% = 20%
            gosub update_error_log
            goto main_line_next

        process
            init(" ") sav_dun$, sav_po$, sav_store$, sku_code$, sku_key$,~
                      sku_price$, sav_fax$, sav_key$
            quote% = 1%                    /* Set for Quote Processing */
            edi_processed$ = "N"
            bckmastr% = 0% : bcklines%  = 0%
            error%    = 0% : no_update% = 0%
            edi_key$ = all(hex(00))
            str(edi_key$,1%,1%)  = edi_processed$   /* Only 'N' s      */
            str(edi_key$,2%,15%) = edi_p$           /* Dun's Number    */
            str(edi_key$,17%,6%) = hdr_cust$        /* Customer Code   */
            str(edi_key$,23%,16%)= hdr_quote$       /* VIQS Quote No.  */
            sav_key$ = str(edi_key$,1%,38%)
        process_next
            read #4,hold,key > edi_key$, using L02910 , edi_rec$,          ~
                                                    eod goto process_done
L02910:       FMT CH(102)
            if sav_key$ <> str(edi_rec$,10%,38%) then goto process_done
               edi_key$ = str(edi_rec$,10%,41%)
               edi_dun$ = str(edi_rec$,11%,15%)
               gosub get_quote_info

        REM - PRICE QUOTE ENTRY POINT
            init(" ") due_date$, fob$
	    /* Y2K */
            if rq_due$ <> blankdate$ and len(rq_due$) > 4 then due_date$ = rq_due$ 
            str(fob$,1%,2%)  = rq_fob$             /* From VIQS System */
            call "APCPLN3B" (due_date$, edi_cust$, fob$, #1, #2)
            call "DATUNFMT" (due_date$)
            gosub check_fob
            if fob$ <> " " then fob1$ = fob$
            dte$ = due_date$
            call "DATEFMT" (dte$)

            if edi_dun$ = sav_dun$ and edi_store$ = sav_store$ and       ~
                                        edi_po$ = sav_po$ then goto L03260
               sav_dun$   = edi_dun$
               sav_store$ = edi_store$
               sav_po$    = edi_po$
               if bckmastr% = 1% then gosub write_bckmastr
                  bckmastr%   = 1%
                  bcklines%   = 0%
                  sav_fax$    = rq_fax$
                  sav_cnt%    = rq_cnt%
                  sav_credit$ = rq_credit$
                  err_store$  = str(edi_rec$,26%,6%)
                  err_quote$  = str(edi_rec$,32%,16%)
                  err_ln$     = str(edi_rec$,48%,3%)
                  gosub lookup_customer
                  gosub assign_so
                  gosub build_bckmastr

L03260:     gosub build_price
            err_store$  = str(edi_rec$,26%,6%)
            err_quote$  = str(edi_rec$,32%,16%)
            err_ln$     = str(edi_rec$,48%,3%)
            gosub build_bcklines
            gosub build_sales_acct
            gosub write_bcklines
            read #4,hold,key = edi_key$, using L02910 , edi_rec$,          ~
                                                    eod goto L03500
               delete #4
            str(edi_rec$,10%,1%) = "Y"            /* (EWD002) */
            put str(edi_rec$,98%,4%), using L03380 , edi_so%
L03380:        FMT BI(4)
            str(edi_rec$,32%,16%) = a04$
            write #4, using L02910  , edi_rec$, eod goto L03540

        process_error
            gosub update_viqs

            goto process_next
        process_done
            if bckmastr% = 1% then gosub write_bckmastr
        return                                  /* Normal Exit Point   */
                                                /* for Process Routine */
L03500:     error% = 8%                         /* Unable to Read      */
            gosub update_error_log
            goto process_error

L03540:     error% = 9%                           /* Unable to Write   */
            gosub update_error_log
            goto process_error

        check_po           /* Check to see if S.O. Entered for P.O. No */
          check_po$ = edi_po$
          read #6,key 1% = check_po$, using L03630 , msg$, eod goto L03690
            goto L03640
L03620:   read #6, using L03630 , msg$, eod goto L03690
L03630:     FMT CH(41)
L03640:   if check_po$ <> str(msg$,26%,16%) then goto L03690
          if edi_cust$ <> str(msg$,1%,9%) then goto L03620
             so$ = str(msg$,10%,16%)
             error% = 3% : no_update% = 1%
             gosub update_error_log
L03690:   msg$ = " "
        return

        REM *************************************************************~
            *  B U I L D   E D I   S A L E S   O R D E R                *~
            *-----------------------------------------------------------*~
            * Special Routines                                          *~
            *************************************************************

        lookup_customer
            ord_amt = 0.0
            read #1,key = edi_cust$, using L03960 ,                        ~
                                   a05$(),      /* Ship To Name/Address*/~
                                   a31,         /* Order Disc Pcnt     */~
                                   a30$,        /* Pricing Code        */~
                                   a07$,        /* Payment Terms       */~
                                   a08$,        /* How Ship Info       */~
                                   a13$(),      /* Salesman Codes      */~
                                   a14%(),      /* Pcnt of Sale        */~
                                   a16$,        /* Sales Region Code   */~
                                   a37$,        /* Account Xref        */~
                                   tax$,        /* Taxable Y or N      */~
                                   sku_code$,   /* Customer Sku Code   */~
                                   a36$,        /* Customer Type Code  */~
                                   a38$,        /* Currency Code       */~
                                   eod goto L04080

L03960:    FMT POS(253), 6*CH(30), POS(517), PD(14,4), CH(1), POS(543),  ~
               CH(20), CH(20), POS(714), 3*CH(4), 3*BI(1), CH(4),        ~
               POS(771), CH(9), POS(794), CH(1), POS(1000), CH(3),       ~
               POS(1023), CH(2), POS(1045), CH(4)

            a08$ = "00/OUR TRUCK (EDI)"
            init(" ") or_cat$
            str(a08$,1%,2%) = rq_hows$
            gosub check_howship

            gosub check_po                    /*MOD SEE IF S.O. EXISTS */
        return
L04080:    error% = 2% : no_update% = 1%            /* LOOKUP_CUSTOMER */
           gosub update_error_log
        return

        build_bcklines
            ln_amt = 0.0 : seq% = 0%
            b01$ = edi_cust$                       /* Customer Code    */
            b02$ = edi_so$                         /* Sales Order Ass. */
            seq% = edi_ln%                         /* Sales Order Line */
            convert seq% to b03$, pic(###)         /* Next Seq No.     */

            b04$ = " "                             /* Item Number      */
            b05$ = edi_part$                       /* APC Part No.     */
            b06$ = edi_part_desc$                  /* Part Description */
            b07$ = str(edi_part$,1%,3%) & "2"      /* Category Code    */
            x = 0.0
            convert edi_qty$ to x, data goto L04250
L04250:
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
            b20$ = tax$                            /* Taxable (Y/N)    */
            b21$ = "3607-313"                      /* Sales Account No.*/
            b22$ = "3650-313"                      /* Discounts Account*/
            b23$ = str(due_date$,1%,6%)            /* Due Date - Orig  */
            b24$ = b23$                            /* Due Date - Curr  */
            b25$ = b23$                            /* Req Ship Date    */
            b26$ = " "                             /* Lot Number       */
            b27$ = " "                             /* Code/Project     */
            b28$ = " "                             /* Filler           */
            b29$ = "1"                             /* Demand Type      */
            b30$ = "A"                             /* Priority Code    */
            b31$ = " "                             /* Text Id          */
            b31$ = rq_d2$                          /* QUOTE LINE TEXT  */
            b32$ = "C"                             /* Allocation Flag  */
            b33$ = " "                             /* (EWD001) Not Used*/
            b34$ = rq_grp$                         /* (EWD001) W/S Grp */
            b35$ = " "                             /* (EWD001) Filler  */
   
            ln_amt = round(x * b14, 2)
            ord_amt = ord_amt + ln_amt
            ord% = seq%
            convert x to qty$, pic(####)

            convert b14 to ln_price$, pic(######.##-)

        return

        build_bckmastr
                                         /* INITIALIZE - ORD_AMT = 0.0 */
            ord_amt = 0.0 : ord% = 1%    /*              ORD% = 1%     */
            a02$ = edi_cust$                       /* Customer Code    */
            a03$ = edi_so$                         /* Sales Order Code */
                                                   /* Customer PO No.  */
            a04$ = edi_po$
                                                   /* A05$ - Customer  */
            a06$(1%) = "BILL-TO"                   /* SOLD-TO NAME/ADD */
                                                   /* A07$ - Customer  */
                                                   /* A08$ - Customer  */
            a09$ = fob1$                           /* F.O.B. Info.     */
            a10$(1%) = " "                         /* Shipping Instr.  */
            a10$(2%) = " "
            a10$(1%) = rq_ship1$                   /* Update Ship Instr*/
            a10$(2%) = rq_ship2$                   /* Update Ship Ln 2 */

            a11$ = " "                             /* Sales Acct No.   */
            a12$ = "3650-313 "                     /* Discounts Account*/
                                                   /* A13$()- Customer */
                                                   /* A14%()- Customer */
                                                   /* A16$  - Customer */
            a17$ = " "                             /* Variable Fields  */
            a18$ = " "                             /* Text Id          */
            str(a17$,1%,20%)  = rq_quote$          /* Cust Quote Id    */
            str(a17$,21%,20%) = rq_job$            /* Cust Job Name    */
            a18$ = rq_d1$                          /* Quote Header Txt */

            a19$ = "300"                           /* Store Code       */
            a20$ = date                            /* Order Date       */
            a21$ = " "                             /* Cancell Date     */
            a22$ = str(due_date$,1%,6%)            /* Due Date         */
            a23$ = a22$                            /* Date Released    */
            a24$ = date                            /* Creation Date    */
            a25$ = "VIQ"                           /* Created By       */
            a26$ = " "                             /* Last Mod Date    */
            a27$ = a25$                            /* Last Mod By User */
            a28$ = " "                             /* Adjustment Reason*/
            a29$ = "N"                             /* Export Flag      */
                                                   /* A30$ - Customer  */
                                                   /* A31  - Customer  */
            a32  = ord_amt                         /* A32 - Tot Dollars*/
            a33$ = " "                             /* Credit Hold Flag */
            if sav_credit$ = "Y" then a33$ = "H"   /* Set Credit Hold  */
            a34% = ord%                            /* A34% - Last Seq  */
            a35% = 1%                              /* Next BOL Number  */
                                                   /* A36$ - Customer  */
                                                   /* A37$ - Customer  */
                                                   /* A38$ - Customer  */
            a39$ = " "                             /* Filler Area      */

        return

        build_price                      /* GET CORRECT SIZE AND PRICE */
            if sku_price < .01 then goto L05200
               b14 = sku_price
               goto L05210
L05200:     b14 = 0.0
L05210: return

        assign_so
               if viqs_so% = 0% then goto L05280
                  so$ = viqs_so$             /* VIQS Assigned S.O.     */
                  goto L05330

L05280:        so$ = " " : edi_so% = 0%
               call "BCKNEXT" ( #8, #7, #9, #10, #11, "300", so$ )
               if so$ <> " " then goto L05330
                  goto L05390

L05330:        str(edi_so$,1%,8%) = str(so$,1%,8%)
               convert edi_so$ to edi_so%, data goto L05420

               convert edi_so% to str(edi_so$,1%,8%), pic(00000000)

        return
L05390:     error% = 4% : no_update% = 1%
            gosub update_error_log
        return
L05420:     error% = 5% : no_update% = 1%
        return

        update_planning
           e% = 0%
           or_no$(1%) = a02$
           or_no$(2%) = a03$
           or_no$(3%) = "99999999"
           or_no$(4%) = "99999999"
           or_no$(5%) = " "
           or_status$ = "00"
           if sav_credit$ = "Y" then or_status$ = "99"

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
                          or_status$,    /* Current S.O. Stat PLAN STAT*/~
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
                          e% )           /* 0 = OK, 1 = ERROR          */
            if e% <> 0% then goto L05950
        return
L05950:     error% = 13%
            gosub update_error_log
        return

        REM *************************************************************~
            *       R E P O R T   F O R M A T S / P R I N T I N G       *~
            *-----------------------------------------------------------*~
            * Special Routines                                          *~
            *************************************************************

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        write_bckmastr                        /* CREATE S.O. HEADER    */
            if bcklines% <> 1% then return
            init(" ") readkey$ : bckmastr% = 0% : bcklines% = 0%
            if no_update% = 1% then goto L06670

            a32 = ord_amt : a34% = ord%
            str(readkey$,1%,9%)   = a02$
            str(readkey$,10%,16%) = a03$
            read #6,hold,key = readkey$, eod goto L06190
                  delete #6                  /* S.O. Already Exists    */
L06190:        put #6, using L07260 , a02$,                                ~
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

              write #6, eod goto L06670
                  gosub update_planning
                  gosub update_bookings
                  gosub update_daily_sales
                  gosub update_customer

              if sav_fax$ = "Y" then gosub send_fax
              if ord% <> sav_cnt% then goto L06700
           no_update% = 0%
        return
L06670:    error% = 11% : no_update% = 0%
           gosub update_error_log
        return
L06700:    error% = 12%
           gosub update_error_log
        return

        update_customer                  /* Update Customer A/R Totals */
            disc_amt = 0.0 : curtemp4 = 0.0
            disc_amt = round(ord_amt * a31 * .01, 2)
            curtemp4 = round( ord_amt - disc_amt, 2)
            if curtemp4 < .01 then return                /* No Update  */

            read #1,hold,key = a02$, eod goto L06980
            get #1 using L06820 , topen1, topen2, billto$
L06820:         FMT POS(733), 2*PD(14,4), POS(780), CH(9)
            if billto$ = a02$ then                                       ~
                topen1 = topen1 + curtemp4
                topen2 = topen2 + curtemp4
            put #1 using L06870 , date, topen1, topen2
L06870:         FMT POS(226), CH(6), POS(733), 2*PD(14,4)
            rewrite #1

            if billto$ = a02$ then L06970
                read #1,hold,key = billto$, eod goto L07010
                get #1 using L06930 , topen1, topen2
L06930:              FMT POS(733), 2*PD(14,4)
                topen1 = topen1 + curtemp4
                put #1 using L06870 , date, topen1, topen2
                rewrite #1
L06970: return
L06980:     error% = 17%                   /* Unable To Update Customer*/
            gosub update_error_log
        return
L07010:     error% = 18%                   /* Unable To Update Bill-To */
            gosub update_error_log
        return


        send_fax
            err% = 0%
            call "APCFAXSD" (a02$,           /* Customer CODE          */~
                             a03$,           /* Customer Sales Order   */~
                             err%,           /* 1%=Not Open, 2%=No Fax */~
                             #2,             /* (GENCODES) 3%=NOT SAVED*/~
                             #6,             /* (BCKMASTR)             */~
                             #7,             /* (BCKLINES)             */~
                             #26,            /* (TXTFILE )             */~
                             #1,             /* (CUSTOMER)             */~
                             #27,            /* (ARMTERMS)             */~
                             #13,            /* (HNYMASTR)             */~
                             #28)            /* (BOMSPEC )             */
                                             /* CLEAR BUFFER AND START */
            if err% <> 0% then goto L07220
        return
L07220:     error% = 19%
            gosub update_error_log
        return

L07260: FMT                 /* FILE #6  -- BCKMASTR                    */~
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

        write_bcklines                        /* Create S.O. Line Item */
            if no_update% = 1% then goto L08090 /* Critical Error Occurr */

            init(" ") readkey$                /* Note - Sales Order    */
            str(readkey$,1%,16%) = edi_so$    /* Cannot Exist          */
            str(readkey$,17%,3%) = edi_ln$
            read #7,hold,key = readkey$, eod goto L07730
                  delete #7                  /*MOD S.O. ALREADY EXISTS */
L07730:        put #7, using L08130 , b01$,                                ~
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
                                    b35$        /*(EWD001) */                        

              write #7, eod goto L08090
              bcklines% = 1%                 /* At Least One Line Item */
        return
L08090:    error% = 7% : no_update% = 1%
           gosub update_error_log
        return

L08130: FMT                 /* FILES #7 -- BCKBUF2 AND BCKLINES        */~
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
            CH(01),         /* wood surround Group Code (EWD001)       */~
            CH(21)          /* Filler Area               (EWD001)      */

        update_bookings                 /* ONLY CREATED FOR S.O. HEADER*/
                                        /* (BCKMASTR)                  */
            call "GETDTTM"  addr(datetime$)
            discamt = round(a32 * a31 *.01,2)
            put #16, using  L08610, "1",       /* Type (1) New S.O.      */~
                                  a02$,      /* Customer Code          */~
                                  a03$,      /* Sales Order Number     */~
                                  datetime$, /* Date and Time Stamp    */~
                                  a05$(1),   /* Ship to Customer Name  */~
                                  a28$,      /* Adjustment Reason Code */~
                                  a32,       /* Gross Pay Amount of S.O*/~
                                  discamt    /* Discount Amount Dollars*/

L08610:     FMT CH(1), CH(9), CH(16), CH(7), CH(30), CH(9), 2*PD(14,4)
            write #16, eod goto L08720

*        Update BCK Print Index Files as requested.
*         Order Acknowledgement Record First
               write #19 using L08690 , "A", a19$, " ", "A", a02$, a03$,   ~
                                           " ", date, " ", eod goto L08750

L08690:         FMT CH(1), CH(3), CH(6), CH(1), CH(9), CH(16), CH(3),    ~
                    CH(6), CH(19)
        return
L08720:     error% = 14%
            gosub update_error_log
        return
L08750:     error% = 15%
            gosub update_error_log
        return

        check_fob
            readkey$ = all(hex(00))
            str(readkey$,1%,9%)   = "PLAN DELV"
            str(readkey$,10%,15%) = str(fob$,1%,2%)
            read #2,key = readkey$, using L08840 , desc$,eod goto L08860
L08840:        FMT POS(25), CH(30)
            str(fob$,3%,18%) = "/" & desc$
L08860: return

        get_quote_info
            init(" ") rq_key$, rq_d1$, rq_d2$, rq_po$, rq_job$, rq_desc$,~
                      rq_quote$, rq_so$, rq_ship1$, rq_ship2$, rq_fax$,  ~
                      rq_due$, viqs_so$, rq_credit$, rq_grp$

            rq_p1 = 0.0 : rq_p2 = 0.0
            str(rq_key$,1%,1%)  = "1"               /* Processed Data  */
            str(rq_key$,2%,16%) = str(edi_rec$,32%,16%)/* Quote Id     */
            str(rq_key$,18%,3%) = str(edi_rec$,48%,3%) /* Line Item    */
                                                   /* (EWD001) - Begin */  
            read #12,hold,key = rq_key$, using L09020 , rq_p1, rq_p2,    ~
                               rq_grp$, rq_d1%, rq_d2%,                  ~
                               viqs_so$, rq_po$, rq_job$, rq_desc$,      ~
                               rq_hows$, rq_fob$, rq_ship1$, rq_ship2$,  ~
                               rq_fax$, rq_due$, rq_credit$,             ~
                               eod goto L09390
L09020:        FMT POS(58), 2*PD(14,4), POS(84), CH(1),                  ~
                                        POS(386), 2*BI(4), CH(16),       ~
                   2*CH(16), POS(454), CH(32), 2*CH(2), 2*CH(50), CH(1), ~
                   CH(6), CH(1)
                                                   /* (EWD001) - End   */
            rq_quote$  = str(edi_rec$,32%,16%)     /* Quote Id Number  */
            edi_store$ = str(edi_rec$,26%,6%)      /* Customer Code    */
            edi_po$    = rq_po$                    /* Customer PO No.  */
            edi_ln$    = str(edi_rec$,48%,3%)      /* Cust PO Line Item*/
            convert edi_ln$ to edi_ln%, data goto L09110
L09110:
            if edi_ln% = 1% then                                         ~
               get str(edi_rec$,100%,1%), using L09140 , rq_cnt%
L09140:            FMT BI(1)

            edi_sku$   = str(edi_rec$,51%,25%)     /* MFG Part Number  */
            edi_qty$   = str(edi_rec$,76%,10%)     /* Line Item Qty    */
            edi_cust$  = edi_store$                /* Customer Code    */
            edi_part$  = edi_sku$                  /* MFG Part Number  */
            edi_part_desc$ = rq_desc$              /* Product Descript */
            sku_price  = rq_p1                     /* Line Item Price  */
            if rq_p2 > .01 then sku_price = rq_p2
            viqs_so% = 0%
            convert viqs_so$ to viqs_so%, data goto L09250
L09250:
            s_23% = 0%                             /* Series Descript  */
            s_23m$ = str(edi_part$,1%,3%)
            call "APCPRZSB" (edi_cust$, s_23m$, s_23$, s_23%, #1, #2,    ~
                                                               x_er% )
            if x_er% <> 0% then goto L09330
               str(edi_part_desc$,1%,8%) = s_23$

L09330:     if rq_d1% = 0% then goto L09360
               put rq_d1$, using L09350 , rq_d1%     /* Header Text Id   */
L09350:            FMT BI(4)
L09360:     if rq_d2% = 0% then return
               put rq_d2$, using L09350 , rq_d2%     /* Line Item Text   */
        return
L09390:     error% = 1% : no_update% = 1%
            gosub update_error_log
        return

        update_viqs
            read #12,hold,key = rq_key$, using L09460 , rq_so$,            ~
                                                      eod goto L09520
L09460:        FMT POS(394), CH(16)
            rq_so$ = edi_so$
            put #12, using L09490 , rq_so$, date
L09490:        FMT POS(394), CH(16), POS(448), CH(6)
            rewrite #12
        return
L09520:     error% = 10%
            gosub update_error_log
        return

        build_sales_acct                    /* FOR ALL S.O. LINE ITEMS */
            init(" ") cat_key$
            if len(edi_part$) < 19 then cat_key$ = "PART"
            if len(cat_key$) > 3 then goto L09630        /* Line is Part */
            str(cat_key$,1%,3%) = str(edi_part$,1%,3%) /*Based on Model*/
            str(cat_key$,4%,1%) = "2"                  /*Color = White */
            if len(or_cat$) > 2 then cat_key$ = or_cat$
L09630:     read #17,key = cat_key$, using L09650 , b21$, b22$,            ~
                                                            eod goto L09680
L09650:        FMT POS(35), 2*CH(9)
            b07$ = cat_key$
        return
L09680:     error% = 6% : no_update% = 1%
            gosub update_error_log
        return

        check_howship
            init(" ") readkey$, or_hows$, or_cat$
            str(readkey$,1%,9%)   = "PLAN HOWS"
            str(readkey$,10%,15%) = str(a08$,1%,2%)
            read #2,key = readkey$, using L09770 , desc$,eod goto L09860
L09770:        FMT POS(25), CH(30)
            str(a08$,3%,18%) = "/" & desc$
            or_hows$ = str(a08$,1%,2%)
            if or_hows$ = "02" or or_hows$ = "04" or or_hows$ = "06"     ~
                                                  then or_cat$ = "SAMP"
            if or_hows$ = "03" or or_hows$ = "05" then or_cat$ = "DISP"
            if or_hows$ = "30" then or_cat$ = "SALV"
            if or_hows$ = "31" then or_cat$ = "SCRA"
        return
L09860:     a08$ = "00/Our Truck (VIQS)"
        return

        update_daily_sales
            init(" ") sa_key$, sa_rec$
            sa_st$ = "00"
            str(sa_key$,1%,9%)  = a02$
            str(sa_key$,10%,8%) = str(a03$,1%,8%)
            read #15,hold,key = sa_key$, using L09960 , sa_rec$,           ~
                                                      eod goto L10000
L09960:        FMT CH(32)
               delete #15
               sa_st$ = "01"

L10000:     str(sa_rec$,1%,6%)  = date          /* Date of Last Change */
            str(sa_rec$,7%,4%)  = a13$(1%)      /* Salesman Code       */
            str(sa_rec$,11%,9%) = a02$          /* Customer Code       */
            str(sa_rec$,20%,8%) = str(a03$,1%,8%) /* Sales Order       */
            str(sa_rec$,28%,2%) = sa_st$        /* 00=New, 01=Change   */
            str(sa_rec$,30%,3%) = "   "
            put #15, using L09960 , sa_rec$
            write #15, eod goto L10090

L10090: return
            error% = 16%
            gosub update_error_log
        return

        update_error_log
            init(" ") err_key$
            cnt% = 0%
            str(err_key$,1%,8%)   = date
            str(err_key$,9%,8%)   = time
            str(err_key$,17%,6%)  = err_store$
            str(err_key$,23%,16%) = err_quote$
            str(err_key$,39%,3%)  = err_ln$

L10230:     convert cnt% to str(err_key$,42%,3%),pic(000)
            read #30,hold,key = err_key$, eod goto L10280
               cnt% = cnt% + 1%
               goto L10230

L10280:     put #30, using L10290, err_key$, str(txt$(error%),1%,35%)
L10290:       FMT CH(45), CH(35)
            write #30, eod goto L10310
L10310: error% = 0%
        return

        exit_program
            if error% <> 99% then edi_partner$ = "000"    /* Processed */
        end
