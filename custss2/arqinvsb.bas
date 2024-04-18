        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   AAA   RRRR    QQQ   IIIII  N   N  V   V   SSS   BBBB    *~
            *  A   A  R   R  Q   Q    I    NN  N  V   V  S      B   B   *~
            *  AAAAA  RRRR   Q   Q    I    N N N  V   V   SSS   BBBB    *~
            *  A   A  R   R  Q Q Q    I    N  NN   V V       S  B   B   *~
            *  A   A  R   R   QQQ   IIIII  N   N    V     SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARQINVSB - Displays and prints data regarding invoices.   *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/30/86 ! Original                                 ! ERN *~
            * 05/28/87 ! Standard Costing Enhancements            ! MJB *~
            * 03/30/88 ! Modified to recognize Inv Type 'X'       ! MJB *~
            * 10/05/88 ! Fixed possible Division by Zero error.   ! JDH *~
            * 05/18/89 ! Enlarged NET INVOICE field on report.    ! MLJ *~
            * 05/31/89 ! Fixed print of single invoice or range   ! MLJ *~
            *          !   of invoices, including exports.        !     *~
            * 06/12/89 ! Fixed page overflow problem on printing  ! MLJ *~
            *          !   of multiple line items per invoice.    !     *~
            * 08/08/89 ! Fixed MC rounding problem for Hong Kong  ! MLJ *~
            * 08/24/89 ! Added Multi-Currency visability.         ! MLJ *~
            * 06/03/91 ! PRR 11841.  Now Line Item Disc% prints.  ! JDH *~
            * 03/03/92 ! Minor mods for DEC Compatibility.        ! JDH *~
            * 03/05/92 ! PRR 12336-Fix PostDate after Invoice Prnt! JDH *~
            *          ! PRR 11360-Curncy tgl to LI & Pay Sch scrn!     *~
            *          ! & Changed all Currency toggles to PF(24).!     *~
            *          ! PRR 12290,12291-Changed logic to read    !     *~
            *          !   ARIMASTR based on new key Billto/Shipto!     *~
            *          ! PRR 12312- 0 fill inv # on Find if neces.!     *~
            *          !   Code not active due to consistency     !     *~
            *          !   issues & next ASCII sequence problems. !     *~
            *          ! Correct honoring invoice range on rpts.  !     *~
            * 10/14/92 ! PRR 12646 Fix Display of passed in ShipTo! JDH *~
            * 03/15/93 ! PRR 12604 Grammar.                       ! JIM *~
            * 03/15/93 ! PRR 12818 Add PO # to ARM010 report.     ! JIM *~
            * 03/15/93 ! Added RUNTIME$ to End of Reports.        ! JIM *~
            * 03/26/93 ! Added Whether Paid in Full.              ! JDH *~
            * 07/07/93 ! Added PF(22) on Line Summary to allow    ! MLJ *~
            *          !  toggle between CMS/Customer Part Number.!     *~
            *          ! Added HNYMASTR for call to PTXREFSB.     !     *~
            * 08/02/93 ! PRR 12999. Fixed Line Item monetary print! JDH *~
            * 03/31/95 ! PRR 13187- Use Shadow Cust Xref File for ! RJH *~
            *          !  display & printing of Xref part info.   !     *~
            * 07/11/96 ! Changes for the year 2000.               ! DXL *~
            * 09/10/91 ! Added PO NUMBER TO INVOICE PRINT - 15381 ! RHH *~
            * 03/10/98 ! Mods to the current Version of Caelus    ! RHH *~
            *          !   program for R6.04.03 - (EWD) Mods      !     *~ 
            * 05/05/98 ! (EWD001) - Add Code for Blankdate$       ! RHH *~
            *          ! to fix problem with Payment Schedule     !     *~
            *          ! printing. - termsnet$()                  !     *~
            * 12/01/98 ! (EWD002) - Mod to print Job Name on Inv  ! RHH *~
            * 04/09/99 ! (EWD003) - Add Invoice Rsn/Desc to Dsply ! BWS *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "ARQINVSB" (billin$,         /* Bill-to Customer for disp  */~
                        asofu$,          /* Display As Of date- unfmtd */~
                        #3,              /* CUSTOMER channel           */~
                        #5, #6,          /* ARIMASTR & ARILINES        */~
                        ret%,            /* In/Out Status Flag         */~
                        invnr$,          /* Invoice for Display        */~
                        shipto$,         /* Ship-to for INVNR$         */~
                        curr$,           /* Multi-Currency Usage Flag  */~
                        stat_curr_code$) /* STATUTORY Currency Code    */

*        RET% is passed as 0% for Bill-to Level inquiry, 1% for display
*          of a specific invoice (invoice specified by INVNR$ and
*          SHIPTO$).  It is passed back as sent if no errors occurred,
*          else it is returned as 2% (bill-to or invoice not on file).
*        MULTI-CURRENCY:  CURR$ is the multi-currency flag.  "Y" if on,
*           "N" if off.   STAT_CURR_CODE$ is the statutory currency code,
*           all blanks if CURR$ = "N".


        dim                                                              ~
            alltypes$9,                  /* Invoice Types              */~
            aracct$12, aracctdescr$30,   /* A/R Account                */~
            artype$3,  artypedescr$20,   /* A/R Type                   */~
            balance(3),                  /* A/R Balance                */~
            bck_key$25, bck_job$20,      /* Customer Job Name (EWD002) */~
            billto$9, billtoname$30,     /* Bill-to Customer and Name  */~
            billin$9,                    /* Bill-to Passed In          */~
            blankdate$8,                 /* Blank Date for Comp(EWD001)*/~
            bol$3,                       /* Bill-of-Lading Number      */~
            carrier$9, carrierdescr$30,  /* Carrier                    */~
            cat$4, catdescr$30,          /* Part Category              */~
            comm%(3), comm$(3)3,         /* Split Commissions          */~
            company$60,                  /* Company Name               */~
            conv$10,                     /* UOM Conversion Factor      */~
            countfor$9,                  /* Last Billto counted for    */~
            curr$1,                      /* Multi-Currency Usage Flag  */~
            currkey$17,                  /* ARIMSCUR Read Key          */~
            currarl$20,                  /* ARILNCUR Read Key          */~
            currmst$4,                   /* CURMASTR Read Key          */~
            cursor%(2),                  /* Cursor location for edit   */~
            cus_range$22, cus_range$(4)9,/* Customer Range for print   */~
            date$8,                      /* Date for screen display    */~
            descr$32,                    /* Part/Line Description      */~
            dfac$(30)1,                  /* Display FACs               */~
            discacct$12,                 /* Header level Discs Acct    */~
            discacctdescr$30,            /*                            */~
            discs$12, discsdescr$30,     /* Sales Discs Acct- Lines    */~
            disp$(14)79,                 /* Screen Display Array       */~
            disp_code$10,                /* Screen Display - Currency  */~
            disp_gross$15,               /* Screen Display - Gross     */~
            disp_invcost$10,             /* Screen Display - Inv. Cost */~
            disp_invdiscamt$15,          /* Screen Display - Disc. Amt */~
            disp_frtamt$15,              /* Screen Display - Freight   */~
            disp_linediscamt$10,         /* Screen Display - LI Disc   */~
            disp_lineext$10,             /* Screen Display - LI Ext.   */~
            disp_price$10,               /* Screen Display - LI Price  */~
            disp_stccost$10,             /* Screen Display - Std Cost  */~
            disp_taxamt$15,              /* Screen Display - Tax Amt   */~
            disp_net$15,                 /* Screen Display - Net Inv   */~
            doctype$20,                  /* Source Doc Type Descr      */~
            entered$8,                   /* Invoice Entry Date         */~
            errormsg$79,                 /* Error message              */~
            exchg_rate$10,               /* Currency Exchange Rate     */~
            expdate$8,                   /* Recurring Expiration Date  */~
            findshipto$9, findinvnr$8,   /* Find Arguments             */~
            fob$20,                      /* FOB Description            */~
            from_date$8, from_dateu$6,   /* Print From Date            */~
            frtacct$12, frtacctdescr$30, /* Freight Account            */~
            frtbill$20,                  /* Air/Frt Bill Number        */~
            hdr1$(8)16, hdr2$(6)10,      /* Screen Column Headings     */~
            hdr3$(6)27, hdr4$(2)10,      /*                            */~
            howship$20,                  /* How Ship Instructions      */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            invdate$8,                   /* Invoice Date               */~
            invnr$8,                     /* Invoice Number             */~
            invrsn$9, invrsndescr$30,    /* Reason Code                */~
            inv_range$22, inv_range$(4)8,/* Invoice Range for print    */~
            item$3,                      /* PO Line ID                 */~
            lastinv$8, lastshipto$9,     /* Last Invoice / Ship-to     */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            linediscpct$6,               /* Line Discount Percent      */~
            linekey$50,                  /* Line Plow Key              */~
            lines$(100)79,               /* STATUTORY - Line Sum Array */~
            c_lines$(100)79,             /* STAT - Cust Line Sum Array */~
            lotqty(30), lot$(30)18,      /* Lot Distribution           */~
            lotmsg$16,                   /* Multiple Lots Message      */~
            lst_lines$(14)79,            /* Inv Listing Display Lines  */~
            ltr_lines$(14)79,            /* TRANSACTION - List Array   */~
            mc_display$1,                /* TRANSACTION Display Flag   */~
            nonstockmsg$16,              /* Part Non-Stock Message     */~
            onlyshipto$9, onlytype$1,    /* Display Limitors           */~
            onlyshipto_d$9,              /* Ship-To for display        */~
            openqty$10,                  /* Sales Order Left Open      */~
            order$10,                    /* Sales Order Qty            */~
            paid_in_full$18,             /* Paid message               */~
            part$25,                     /* Part Number                */~
            pay$(30,2)39,                /* Payment Schedule           */~
            pc$, pcdescr$30,             /* Price Code Info            */~
            pf$(3)79, pfkeys$32,         /* PF Literals and Keys       */~
            plo$132,                     /* Print Line Out             */~
            plo_tran_desc$30,            /* Detail List - Tran Descr   */~
            po$16,                       /* PO Number                  */~
            pono$(14)16,                 /* PO Number for toggle       */~
            postdate$8,                  /* Post Date                  */~
            plowkey$50,                  /* Miscellaneous Read/Plow Key*/~
            postgl$12,                   /* G/L Post Flag              */~
            posthny$1,                   /* Inventory Post Flag        */~
            priceuom$4,                  /* Price UOM                  */~
            pricestk$10,                 /* Price at Stkng UOM         */~
            print_text$1,                /* Print Text on Invoice      */~
            print_types$16,              /* Inv Types to Print         */~
            readkey$50,                  /* Misc. Read Key             */~
            refpart$25,                  /* Xref Part Number           */~
            reftype$1,                   /* Xref Part Type(Cust or Mnf)*/~
            refdescr$30,                 /* Xref Part Description      */~
            region$4, regiondescr$30,    /* Sales Region               */~
            report$30, rptid$6,          /* Report to Print            */~
            rpthdr$80,                   /* Report Heading Line        */~
            rptplow$50,                  /* Plow Key for Report        */~
            runtime$8,                   /* Report Run Time            */~
            sales$12, salesdescr$30,     /* Sales Distr Acct- Lines    */~
            salesacct$12,                /* Sales Distribution Account */~
            salesacctdescr$30,           /*                            */~
            salesman$(3)4,               /* Salesmen                   */~
            salesmandescr$(3)30,         /*                            */~
            save_invnr$8, save_shipto$9, /* Save 'em fer later         */~
            save_type$4,  save_post$8,   /* Save 'em fer later         */~
            seq$3,                       /* Line Item Sequence Number  */~
            ship$10,                     /* Quantity Shipped           */~
            shipdate$8,                  /* Ship Date                  */~
            shipto$9,                    /* Ship-to                    */~
            shipto$(6)31,                /* Ship-to Address            */~
            so$16, soseq$3,              /* Sales Order Number, Seq    */~
            sono$(14)16,                 /* S.O. for toggle            */~
            soldto$(6)31,                /* Sold To Address            */~
            stat_curr_code$4,            /* STATUTORY Currency Code    */~
            stat_gross$15,               /* STATUTORY - Inv Gross      */~
            stat_invcost$10,             /* STATUTORY - Inventory Cost */~
            stat_invdiscamt$15,          /* STATUTORY - Discount Amt   */~
            stat_frtamt$15,              /* STATUTORY - Freight Amt    */~
            stat_linediscamt$10,         /* STATUTORY - LI Discount    */~
            stat_lineext$10,             /* STATUTORY - LI Extension   */~
            stat_price$10,               /* STATUTORY - LI Price       */~
            stat_stccost$10,             /* STATUTORY - Standard Cost  */~
            stat_taxamt$15,              /* STATUTORY - Tax Amt        */~
            stat_net$15,                 /* STATUTORY - Net Invoive    */~
            stat_curr_desc$30,           /* STATUTORY - Description    */~
            stlmnt$14,                   /* Settlement Number          */~
            stkuom$4,                    /* Stocking UOM               */~
            str$3, strdescr$30,          /* Store                      */~
            sum_lines$(100)79,           /* Line Summary Display       */~
            taxable$7,                   /* Line Taxable Flag          */~
            taxacct$12, taxacctdescr$30, /* Sales Tax Account          */~
            taxcode$10,                  /* Sales Tax Code             */~
            terms$20, termsdescr$28,     /* Payment Terms Code         */~
            termscur(30),                /*   Amts Due in Trans Curr   */~
            termsdue(30), termsdiscs(30),/*   Amts Due and Cash Disc%s */~
            termsdisc$(30)8,             /*   Discount Due Dates       */~
            termsnet$(30)8,              /*   Net Due Date             */~
            text$(113,1)70,              /* Text Sizing Array          */~
            tr_lines$(100)79,            /* TRANSACTION-Line Sum Array */~
            tr_clines$(100)79,           /* TRAN - Cust Line Sum Array */~
            textid$4, textidl$4,         /* Text Hdr/Line IDs          */~
            tran_curr_code$4,            /* TRANSACTION Currency Code  */~
            tran_gross$15,               /* TRANSACTION - Gross        */~
            tran_invcost$10,             /* TRANSACT  - Inventory Cost */~
            tran_invdiscamt$15,          /* TRANSACTION - Discount Amt */~
            tran_frtamt$15,              /* TRANSACTION - Freight Amt  */~
            tran_linediscamt$10,         /* TRANSACT  - LI Discount    */~
            tran_lineext$10,             /* TRANSACT  - LI Extension   */~
            tran_price$10,               /* TRANSACT  - LI Price       */~
            tran_stccost$10,             /* TRANSACT  - Standard Cost  */~
            tran_taxamt$15,              /* TRANSACTION - Tax Amt      */~
            tran_net$15,                 /* TRANSACTION - Net Invoice  */~
            tran_curr_desc$30,           /* TRANSACTION - Description  */~
            type$4, types$9,             /* Invoice Type(s)            */~
            userid$3,                    /* User ID                    */~
            vf$200                       /* Variable Fields Data       */

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
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
            * # 2 ! SYSFILE2 ! Caelus Management System Information     *~
            * # 3 ! CUSTOMER ! Customer Master File (passed in)         *~
            * # 4 ! GLMAIN   ! General Ledger Chart Of Accounts File.   *~
            * # 5 ! ARIMASTR ! Invoice Master File     (passed in)      *~
            * # 6 ! ARILINES ! Invoice Line Items File (passed in)      *~
            * # 7 ! CATEGORY ! Inventory Category Codes File            *~
            * # 8 ! GENCODES ! System General Codes file.               *~
            * #10 ! HNYMASTR ! Inventory Master File                    *~
            * #11 ! SLMMASTR ! Salesman master file                     *~
            * #12 ! STORNAME ! Store Master File                        *~
            * #13 ! JOBMASTR ! Project Master File.                     *~
            * #14 ! TXTFILE  ! System Text File                         *~
            * #15 ! ARMTRIAL ! A/R Trial Balance File                   *~
            * #16 ! BCKMASTR ! Sales Order Header             (EWD002)  *~ 
            * #40 ! ARIMSCUR ! Shadow - ARIMASTR                        *~
            * #41 ! ARILNCUR ! Shadow - ARILINES                        *~
            * #42 ! CURMASTR ! Multi-Currency Master File               *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 2, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select # 4, "GLMAIN",                                        ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   9                      ~

            select # 7, "CATEGORY",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    1, keylen =   4                      ~

            select # 8, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24                      ~

            select #10, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key 1, keypos = 109,  keylen =  9, dup,      ~
                            key 2, keypos =  90,  keylen =  4, dup,      ~
                            key 3, keypos =  26,  keylen = 32, dup       ~

            select #11, "SLMMASTR",                                      ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   4                      ~

            select #12, "STORNAME",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   3                      ~

            select #13, "JOBMASTR",                                      ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos =    1, keylen =   8                      ~

            select #14, "TXTFILE",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  11                      ~

            select #15, "ARMTRIAL",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =    1, keylen =  21                      ~

                                                   /* (EWD002) Begin  */
            select #16,  "BCKMASTR",                                     ~
                        varc,     indexed,  recsize =  1000,             ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup
                                                   /* (EWD002) End    */
            select #40, "ARIMSCUR",                                      ~
                        varc,     indexed,  recsize =  400,              ~
                        keypos =    5, keylen =  17,                     ~
                        alt key 1, keypos = 1,  keylen = 21

            select #41, "ARILNCUR",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    5, keylen =  20,                     ~
                        alt key 1, keypos = 1,  keylen = 24

            select #42, "CURMASTR",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =   4

            call "OPENCHCK" (# 2, fs%( 2%), f2%( 2%), 0%, rslt$( 2%))
            call "OPENCHCK" (# 3, fs%( 3%), f2%( 3%), 0%, rslt$( 3%))
            call "OPENCHCK" (# 4, fs%( 4%), f2%( 4%), 0%, rslt$( 4%))
            call "OPENCHCK" (# 5, fs%( 5%), f2%( 5%), 0%, rslt$( 5%))
            call "OPENCHCK" (# 6, fs%( 6%), f2%( 6%), 0%, rslt$( 6%))
            call "OPENCHCK" (# 7, fs%( 7%), f2%( 7%), 0%, rslt$( 7%))
            call "OPENCHCK" (# 8, fs%( 8%), f2%( 8%), 0%, rslt$( 8%))
            call "OPENCHCK" (#10, fs%(10%), f2%(10%), 0%, rslt$(10%))
            call "OPENCHCK" (#11, fs%(11%), f2%(11%), 0%, rslt$(11%))
            call "OPENCHCK" (#12, fs%(12%), f2%(12%), 0%, rslt$(12%))
            call "OPENCHCK" (#13, fs%(13%), f2%(13%), 0%, rslt$(13%))
            call "OPENCHCK" (#14, fs%(14%), f2%(14%), 0%, rslt$(14%))
            call "OPENCHCK" (#15, fs%(15%), f2%(15%), 0%, rslt$(15%))
            call "OPENCHCK" (#16, fs%(16%), f2%(16%), 0%, rslt$(16%))

            if curr$ = "N" then L09000
            call "OPENCHCK" (#40, fs%(40%), f2%(40%), 0%, rslt$(40%))
            call "OPENCHCK" (#41, fs%(41%), f2%(41%), 0%, rslt$(41%))
            call "OPENCHCK" (#42, fs%(42%), f2%(42%), 0%, rslt$(42%))

            stat_curr_desc$, tran_curr_desc$ = " "
            currmst$ = str(stat_curr_code$)
            call "READ100" (#42, currmst$, f1%(42))
            if f1%(42) = 0% then L02850
                get #42 using L02836, stat_curr_desc$
L02836:             FMT POS(05), CH(30)

L02850:     cms%, refflag% = 0%

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

*        Perform start-up tasks required regardless of tasks
            blankdate$ = " "                     /* (EWD001)      */
            call "DATUFMTC" (blankdate$)
            billto$ = billin$
            date$ = date : call "DATEFMT" (date$)
            asof$ = asofu$  :  call "DATEFMT" (asof$)
            call "DESCRIBE" (#3, billto$, billtoname$, 0%, f1%(3))
            if f1%(3) = 1% then L09120
                ret% = 2%  :  goto exit_program
L09120:     str(line2$,62%) = "ARQINVSB: " & str(cms2v$,,8%)
            call "REDALT0" (#3, billto$, 4%, billto%)  /* Really a */
                                                       /* Bill-To? */

*        If a specific invoice has been requested, handle it here
            gosub L09270
            if ret% <> 1% then first_screen
                gosub load_invoice
                if invonfile% = 1% then L09230
                     ret% = 2%
                     call "ASKUSER" (2%, "INVOICE DISPLAY",              ~
                               "The Invoice requested is not on file.",  ~
                               " ", "Press any PF Key to Continue...")
                     goto exit_program
L09230:         gosub display_invoice
                goto  exit_program


L09270
*        Count the number of Shiptos with invoices under the Billto
            if billto% = 0% then L09390
              if countfor$ = billto$ then L09450
                shiptos% = 0%
                countfor$ = billto$
                plowkey$ = all(hex(00))
                str(plowkey$,,9) = str(billto$)
L09320:         call "REDALT2" (#5, plowkey$, 4%, f1%(5))
                if f1%(5) = 0% then L09450
                     get #5 using L09334, temp1$, temp2$
L09334:                   FMT POS(1783), 2*CH(9)
                     if billto$ <> temp1$ then L09450
                          shiptos% = shiptos% + 1%
                          plowkey$ = all(hex(ff))
                          str(plowkey$,,18) = str(temp1$,,9) &           ~
                                              str(temp2$,,9)
                          goto L09320

L09390:     shiptos% = 1%   /* Passed in Bill-To is really a Ship-To */
            onlytype$ = " "
            onlyshipto$ = billto$
            call "READ100" (#3, billto$, f1%(3%))
            if f1%(3%) = 1% then get #3 using L09420, billto$
L09420:         FMT POS(780), CH(9)
            call "DESCRIBE" (#3, billto$, billtoname$, 0%, f1%(3))
            if f1%(3%) = 0% then billtoname$ = "(NOT On File!)"
            goto L09455

L09450:     onlyshipto$, onlytype$ = " "
L09455:     if sosw% = 0% then hdr1$(5) = "Customer's PO"                ~
                          else hdr1$(5) = "Sales Order #"
            return

        REM *************************************************************~
            *                  M A I N   S C R E E N                    *~
            *-----------------------------------------------------------*~
            * Handles Invoice Summary Listing Screen.                   *~
            *************************************************************

        main_screen
            ret%, cms%, refflag% = 0%  :  inpmessage$ = " "
            gosub'101(0%)
            errormsg$ = " "
                if keyhit%  =  0% then show_invoice
                if keyhit%  =  2% then first_screen
                if keyhit%  =  5% then next_screen
                if keyhit%  =  8% then find_invoice
                if keyhit%  =  9% then switch_po_so
                if keyhit%  = 10% then list_only_shipto
                if keyhit%  = 11% then list_only_type
                if keyhit%  = 12% then print_listing
                if keyhit%  = 14% then print_invoices
                if keyhit%  = 16% then exit_program
                if keyhit%  = 32% then exit_program
                goto main_screen

        first_screen
            lastinv$ = hex(00)
            first%   = 1%
            if onlyshipto$ = " " then shipto% = 1%
            goto L10290
        next_screen
            first% = 0%
L10290:     gosub load_summary
            goto  main_screen

        find_invoice
            findinvnr$ = " " : findshipto$ = onlyshipto$
L10340:     gosub'101(1%)
                errormsg$ = " "
                if keyhit% <> 1% then L10370
                            findshipto$, findinvnr$ = " "
                            goto main_screen
L10370:              plowkey$ = str(billto$) & str(findshipto$) & hex(00)
                     call "PLOWALTS" (#5, plowkey$, 4%, 18%, f1%(5))
                     if f1%(5) <> 0% then L10415
                          errormsg$ = "Not a Ship-to for this Bill-to."
                          goto L10340
L10415:              gosub zero_pad
                     lastinv$ = findinvnr$ addc all(hex(ff))
                     plowkey$ = str(billto$) & str(findshipto$) & lastinv$
                     findmode% = 0% : break% = 18%
                     findshipto$, findinvnr$ = " "
                     goto next_screen

        zero_pad
            return /* Take this out to pad w/zeros if desired */
            readkey$ = str(billto$) & str(findshipto$) & findinvnr$
            call "REDALT0" (#5, readkey$, 4%, f1%(5))
            if f1%(5) = 1% then return   /* Found it 'as is' */
                convert findinvnr$ to temp%, data goto L10450 /* Not a # */
                convert temp% to temp$, pic(00000000)   /* Zero-fill it */
                readkey$ = str(billto$) & str(findshipto$) & temp$
                call "REDALT0" (#5, readkey$, 4%, f1%(5))
                if f1%(5) = 1% then findinvnr$ = temp$  /* Found filled */
L10450:              return

        list_only_shipto
L10480:     gosub'101(2%)
                errormsg$ = " "
                onlyshipto$ = onlyshipto_d$
                if keyhit% <> 1% then L10510
                     onlyshipto$ = " "
                     if billto% = 0% then onlyshipto$ = billin$
                     goto main_screen
L10510:         if onlyshipto$ = " " then first_screen
                     plowkey$ = str(billto$) & str(onlyshipto$) & hex(00)
                     call "PLOWALTS" (#5, plowkey$, 4%, 18%, f1%(5))
                     if f1%(5) <> 0% then L10580
                          errormsg$ = "Not a Ship-to for this Bill-to."
                          goto L10480
L10580:              lastinv$    = hex(00)
                     goto first_screen

        list_only_type
L10620:     gosub'101(3%)
                errormsg$ = " "
                if keyhit% <> 1% then L10650
                     onlytype$ = " "
                     goto main_screen
L10650:         if onlytype$ = " " then first_screen
                     if pos("ACDFGMORX" = onlytype$) > 0% then L10690
                        errormsg$ = "Enter A, C, D, F, G, M, O, R or X"
                        goto L10620
L10690:              lastinv$ = hex(00)
                     if onlyshipto$ = " " then shipto% = 1%
                     if shipto% = 1% then first_screen else next_screen


        switch_po_so
            if sosw% = 0% then hdr1$(5) = "Sales Order #"                ~
                          else hdr1$(5) = "Customer's PO"
            for j% = 1% to disp%
                if sosw% = 0% then str(disp$(j%),34,16) = sono$(j%)      ~
                              else str(disp$(j%),34,16) = pono$(j%)
            next j%
            if sosw% = 0% then sosw% = 1% else sosw% = 0%
            goto main_screen

        REM *************************************************************~
            *             R E P O R T    P R I N T I N G                *~
            *-----------------------------------------------------------*~
            * Get Report Parameters for both Listing and Invoice Detail.*~
            *************************************************************

        print_invoices
            report$ = "INVOICE DETAIL"
            rptid$  = "ARM010"
            fields% = 5%
            goto L11170

        print_listing
            report$ = "INVOICE LISTING"
            rptid$  = "ARM009"
            fields% = 4%

L11170:     cus_range$(1) = "ALL"  :  cus_range$(2) = " "
                if onlyshipto$ <> " " then cus_range$(1) = onlyshipto$
                gosub'152(1%)
            inv_range$(1) = "ALL"  :  inv_range$(2) = " "
                gosub'152(2%)

            from_date$ = "19010101"
            call "DATECONV" (from_date$)
            from_dateu$ = from_date$
            call "DATEFMT" (from_date$)

            init(" ") types$
            if onlytype$ = " " then init("Z") types$ else                ~
                str(types$,pos("ACDFGMORX" = onlytype$),1) = "Z"
            print_text$ = "N"

L11280:     inpmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
            lastfieldnr% = 0%
            gosub'102(0%)
                  if keyhit%  =  1% then main_screen
                  if keyhit%  = 16% then print_report
                  if keyhit%  = 32% then exit_program
                  if keyhit% <>  0% then L11280
L11360:     fieldnr% = cursor%(1) - 5
                if fieldnr% < 1% or fieldnr% > fields% then L11280
                if fieldnr%  = lastfieldnr% then L11280
            gosub'052(fieldnr%)
L11400:     gosub'102(fieldnr%)
                  if keyhit%  =  1 then main_screen
                  if keyhit% <>  0 then L11400
            gosub'152(fieldnr%)
                  if errormsg$ <> " " then L11400
                     lastfieldnr% = fieldnr%
                     goto L11360

        print_report
            gosub report_control
            goto  main_screen


        REM *************************************************************~
            *             I N V O I C E   D I S P L A Y                 *~
            *-----------------------------------------------------------*~
            * Display Invoice Detail.                                   *~
            *************************************************************

        show_invoice   /* Invoice Detail display from Main Screen      */
            i% = cursor%(1) - 5%
            if i% < 1% or i% > disp% then main_screen
            for c% = i% to 1% step -1%
                if str(disp$(c%),,9) = " " then L12130
                     shipto$ = str(disp$(c%),,9)
                     goto L12140
L12130:     next c%
L12140:     invnr$  = str(disp$(i%),11,8)
            gosub load_invoice
            if invonfile% = 0% then main_screen
                gosub display_invoice
                goto main_screen


        display_invoice
            inpmessage$, errormsg$ = " "
            str(line2$,,61%) = "Bill-to: " & billto$ & "  " &            ~
                               "Ship-to: " & shipto$ & "  " &            ~
                               "Invoice: " & invnr$
            t% = 0%

        detail_hdr1
            gosub'111
                if keyhit%  = 16% then       return
                if keyhit%  =  2% then gosub line_summary
                if keyhit%  =  5% then       detail_hdr2
                if keyhit%  =  8% then gosub payment_schedule
                if keyhit%  =  9% then gosub variable_fields
                if keyhit%  = 14% then gosub print_invoice
                if keyhit%  = 26% then gosub display_header_text
                if keyhit%  = 32% then       return
                goto detail_hdr1

        detail_hdr2
            gosub'112
                if keyhit%  = 16% then       return
                if keyhit%  =  2% then gosub line_summary
                if keyhit%  =  4% then       detail_hdr1
                if keyhit%  =  8% then gosub payment_schedule
                if keyhit%  =  9% then gosub variable_fields
                if keyhit%  = 14% then gosub print_invoice
                if keyhit%  = 26% then gosub display_header_text
                if keyhit%  = 32% then       return
                goto detail_hdr2

        payment_schedule
            gosub'113
                if keyhit%  = 16% then return
                if keyhit%  = 32% then return
                     goto payment_schedule

        variable_fields
            call "VFINPSUB" ("ARIMASTR", "D",                            ~
                  "A/R Trial Balance Inquiry: Invoice Payment Schedule", ~
                  line2$, "NN", vf$, keyhit%)
            if keyhit% = 1% then keyhit% = 32% else keyhit% = 99%
            return

        display_header_text
            call "TXTDSPLY" (#14, f2%(14), "015", line2$, textid$,       ~
                                                                text$())
            return


        line_summary
            inpmessage$ = "Position Cursor and Press RETURN to see" &    ~
                          " Line Item Detail."
            gosub'120
              inpmessage$ = " "
              if keyhit% = 16 then       return
              if keyhit% =  2 then t% = 0%
              if keyhit% =  3 then t% = min(85%,maxlines%-12%)
              if keyhit% =  4 then t% = t%-12%
              if keyhit% =  5 then t% = min(85%, t%+12%, maxlines%-12%)
              if keyhit% =  6 then t% = t%-1%
              if keyhit% =  7 then t% = min(85%, t%+1%, maxlines%-1%)
                                   t% = max(0, t%)
              if keyhit% = 14 then gosub print_invoice
              if keyhit% = 32 then       return
              if keyhit% >  0 then line_summary
            c% = cursor%(1) - 4%
            if c% < 1% or c% > 15% then line_summary
                c%   = c% + t%
                seq$ = str(lines$(c%),,3)
                if c% < 1% or c% > maxlines% then line_summary
                     gosub load_line_item
                     if lineonfile% = 0% then line_summary

        line_detail
            gosub'121
                if keyhit%  = 16% then       line_summary
                if keyhit%  =  6% then gosub prev_line
                if keyhit%  =  7% then gosub next_line
                if keyhit%  =  8% then       display_lots
                if keyhit%  = 14% then gosub print_invoice
                if keyhit%  = 26% then gosub display_line_text
                if keyhit%  = 32% then return
                     goto line_detail

        prev_line
            if c% = 1% then return else c% = c% - 1%
            goto  L13110
        next_line
            if c% = maxlines% then return else c% = c% + 1%
L13110:     seq$  = str(lines$(c%),,3)
            gosub load_line_item
            return


        display_line_text
            call "TXTDSPLY" (#14, f2%(14), "016", line2$, textidl$,      ~
                                                                text$())
            return


        display_lots
            gosub'122
                if keyhit%  = 16% then line_detail
                if keyhit%  = 32% then return
                     goto display_lots

        print_invoice:
            report$ = "INVOICE DETAIL"
            rptid$  = "ARM010"
            save_shipto$ = shipto$
            save_invnr$  = invnr$
            save_type$   = type$
            save_post$   = postdate$
            cus_range$(1) = shipto$ : cus_range$(2) = " " : gosub'152(1%)
            inv_range$(1) = invnr$  : inv_range$(2) = " " : gosub'152(2%)
            init ("Z") types$

            from_date$  = "19010101"
            call "DATECONV" (from_date$)
            from_dateu$ = from_date$
            call "DATEFMT" (from_date$)

            print_text$ = "Y"
            gosub report_control
            shipto$   = save_shipto$
            invnr$    = save_invnr$
            type$     = save_type$
            postdate$ = save_post$
            return


        REM *************************************************************~
            *             P R I N T   L I S T I N G                     *~
            *-----------------------------------------------------------*~
            * Print Listing of Invoices per Selection Criteria Entered. *~
            *************************************************************
        listing_printing
            total_frt, total_tax, total_net = 0
            invcount% = 0%

        listing_loop
            gosub next_invoice
            if f1%(5) = 0% then end_listing
            get #5 using L14160, shipto$, invnr$, po$, shipto$(1),        ~
                                invdate$, postdate$, frt, tax, net, str$,~
                                artype$
L14160:         FMT CH(9), CH(8), CH(16), POS(53), CH(30), POS(521),     ~
                    CH(6), XX(6), CH(6), POS(817), 3*PD(14,4), POS(870), ~
                    CH(3), POS(907), CH(1)
            call "DATEFMT" (invdate$ )
            call "DATEFMT" (postdate$)
            if artype$ = "A" then artype$ = "A/R"
            if artype$ = "C" then artype$ = "CSH"
            if artype$ = "E" then artype$ = "EXP"
            total_frt = total_frt + frt
            total_tax = total_tax + tax
            total_net = total_net + net
            if line% > 55% then gosub listing_heading
            print using L14620, shipto$, shipto$(1), invnr$, invdate$,    ~
                               type$, artype$, postdate$, str$, po$,     ~
                               frt, tax, net
            line% = line% + 1%
            invcount% = invcount% + 1%
            goto listing_loop


        end_listing
            if line% = 857% then return
                print using L14650
                print using L14680, invcount%, total_frt, total_tax,      ~
                                   total_net
                runtime$ = " " : call "TIME" (runtime$)
                print using L14711, runtime$
                return


        listing_heading
            gosub page_heading
            print using L14530
            print using L14560
            print using L14590
            line% = line% + 3%
            return

L14530: % SHIP-TO                                  INVOICE  INVOICE INV. ~
        ~A/R   DATE

L14560: % CUSTOMER SHIP-TO NAME (PER INVOICE)      NUMBER    DATE   TYPE ~
        ~TYP  POSTED  STORE CUSTOMER PO         FREIGHT SALES TAX   NET IN~
        ~VCE
L14590: %--------- ------------------------------ -------- -------- ---- ~
        ~--- -------- ----- ---------------- ---------- --------- --------~
        ~---
L14620: %######### ############################## ######## ######## #### ~
        ~ ### ########  ###  ################ -######.## -#####.## -######~
        ~#.##
L14650: %                                                                ~
        ~                                    ---------- ---------- -------~
        ~---
L14680: %                                                                ~
        ~    REPORT TOTALS (##### INVOICES)  -######.## -#####.## -#######~
        ~.##

L14711: % *** END OF REPORT @ ######## ***

        REM *************************************************************~
            *             P R I N T   I N V O I C E S                   *~
            *-----------------------------------------------------------*~
            * Print Detail for Invoices per Selection Criteria Entered. *~
            *************************************************************
        invoice_printing
            invcount% = 0%

        invoice_loop
            gosub next_invoice
            if f1%(5) = 1% then L15081
                if line% = 857% then return
                     print
                     print using L15990, invcount%
                     print
                     runtime$ = " " : call "TIME" (runtime$)
                     print using L14711, runtime$
                     return
L15081:     gosub load_invoice
            gosub lookup_job_name            /* (EWD002) Find Job */   
                                             /*    bck_job$       */
            if curr$ = "N" then L15109
                currkey$ = str(shipto$,,9) & str(invnr$)
                call "READ100" (#40, currkey$, f1%(40))
                if f1%(40) = 0% then L15091
                    get #40 using L15088, exchg_rate
L15088:                 FMT POS(84), PD(14,7)
                    call "CONVERT" (exchg_rate, -2.7, exchg_rate$)
                    goto L15092
L15091:         exchg_rate$ = "1.0000000 "
L15092:         currmst$ = str(tran_curr_code$)
                call "READ100" (#42, currmst$, f1%(42))
                if f1%(42) = 0% then L15109
                   get #42 using L15098, plo_tran_desc$
L15098:               FMT POS(5), CH(30)

L15109:     line% = 1000%  :  invcount% = invcount% + 1%
            plo$ = "Ship-to Customer  "          & str(shipto$,,9) &     ~
                   "              Ship-to  "     & str(shipto$(1)) &     ~
                   "   Sold-to  "                & str(soldto$(1))
                gosub print_line
            plo$ = "Invoice Number    " & invnr$ & ", PO " & po$
                str(plo$,51) = shipto$(2)  :  str(plo$,93) = soldto$(2)
                gosub print_line
            plo$ = "        Date      " & invdate$
                str(plo$,51) = shipto$(3)  :  str(plo$,93) = soldto$(3)
                gosub print_line
            plo$ = "        Type      " & doctype$
                str(plo$,51) = shipto$(4)  :  str(plo$,93) = soldto$(4)
                gosub print_line
            plo$ = "        Posted    " & postdate$
                str(plo$,51) = shipto$(5)  :  str(plo$,93) = soldto$(5)
                gosub print_line
            plo$ = "        A/R Type  " & artypedescr$
                                                   /* (EWD002)        */
                str(plo$,42%) = "Job Name " & bck_job$
    
        REM     str(plo$,51) = shipto$(6) 
        REM     str(plo$,93) = soldto$(6)
                                                   /* (EWD002)        */
                gosub print_line

            if curr$ = "N" then L15195
                plo$ = "Transaction Curr  " & str(plo_tran_desc$)
                gosub print_line
                plo$ = "Exchange Rate     " & exchg_rate$ & " " &        ~
                   tran_curr_code$ & "/" & stat_curr_code$
                gosub print_line
                str(plo$,47) = "-- ALL AMOUNTS DISPLAYED IN STATUTORY CUR~
        ~RENCY --"
                gosub print_line
L15195:     gosub print_line

            plo$ = "Gross Invoice"
                call "CONVERT" (gross, 2.2, str(plo$,19,10))
                str(plo$, 33) = "Entered " & entered$ & " by " & userid$
                str(plo$, 75) = "Store       " & str$
                str(plo$,100) = strdescr$
                gosub print_line
            plo$ = "Invoice Discs"
                call "CONVERT" (invdiscamt, 2.2, str(plo$,19,10))
                str(plo$, 33) = "-###.##% Discount"
                convert invdiscpct to str(plo$,33,7), pic(-###.##)
                str(plo$, 75) = "How Shipped " & howship$
                gosub print_line
            plo$ = "Sales Tax Amt"
                call "CONVERT" (taxamt, 2.2, str(plo$,19,10))
                str(plo$, 33) = "-###.##% Tax.  Tax Code " & taxcode$
                convert taxpct to str(plo$,33,7), pic(-###.##)
                str(plo$, 75) = "FOB Terms   " & fob$
                gosub print_line
            plo$ = "Freight Charges"
                call "CONVERT" (frtamt, 2.2, str(plo$,19,10))
                str(plo$, 75) = "Carrier     " & carrier$
                str(plo$,100) = carrierdescr$
                gosub print_line
            str(plo$, 19) = "----------"
                str(plo$, 75) = "Cartons"
                call "CONVERT" (cartons, 2.2, str(plo$,87,10))
                 gosub print_line
            plo$ = "Net Invoice Amt"
                call "CONVERT" (net, 2.2, str(plo$,19,10))
                str(plo$, 33) = "Terms: " & termsdescr$
                str(plo$, 75) = "Weight"
                call "CONVERT" (weight, 2.2, str(plo$,87,10))
                gosub print_line
            str(plo$,33) = "Settlement No: " & stlmnt$
                if so$ <> " " then str(plo$, 75) = "SO - BOL    " & so$
                if bol$ <> " " then plo$ = plo$ & "-" & bol$
                str(plo$,100%) = " PO NUMBER: " & po$   /* (EWD) Mod */
                gosub print_line
            gosub print_line

            plo$ = "Sales Region      " & str(region$) & "          " &  ~
                                              regiondescr$
                str(plo$, 75) = "Net Invoice " & str(aracct$) & " " &    ~
                                                     aracctdescr$
                gosub print_line
            plo$ = "Salesmen/Split %  " & str(salesman$(1)) &  "   " &   ~
                     str(comm$(1)) & "    " & salesmandescr$(1)
                str(plo$, 75) = "Sales Tax   " & str(taxacct$) & " " &   ~
                                               taxacctdescr$
                gosub print_line
            plo$ = "                  " & str(salesman$(2)) &  "   " &   ~
                     str(comm$(2)) & "    " & salesmandescr$(2)
                str(plo$, 75) = "Freight     " & str(frtacct$) & " " &   ~
                                                     frtacctdescr$
                gosub print_line
            plo$ = "                  " & str(salesman$(3)) &  "   " &   ~
                     str(comm$(3)) & "    " & salesmandescr$(3)
                str(plo$, 75) = "Sales Disc  " & str(discacct$) & " " &  ~
                                                     discacctdescr$
                gosub print_line

            str(plo$, 75) = "Sales Dist  " & str(salesacct$) & " " &     ~
                                                 salesacctdescr$
                gosub print_line
            gosub print_line
                                                /* (EWD001)  */        
            if termsdescr$ <> "Dated" or termsnet$(2) = " " or         ~
                                 termsnet$(2) = blankdate$ then L15560
                plo$ = "Payment Schedule"
                for p% = 1% to 29% step 2%
                                                        /* (EWD001)   */
                     if termsnet$(p%) = " " or termsnet$(p%) = blankdate$ ~
                                              then L15550   /* Y2K */
                     if termsnet$(p%) = " " then L15550
                          str(plo$, 18) = pay$(p%, 1)
                          str(plo$, 67) = pay$(p%+1%, 1)
                          gosub print_line
                next p%
L15550:         gosub print_line

L15560:     if textid$ = " " then L15600
                status% = 0%
L15570:         call "TXTPRINT" (#14, f2%(14), 134%, textid$, " ", 18%,  ~
                                 line%, 55%, "Y", " ", status%)
                if status% = 0% then L15600
                     gosub page_heading
                     goto  L15570

L15600:     if line% < 55% then print else line% = 99%
            gosub line_heading
            linekey$ = str(shipto$) & str(invnr$) & hex(00)

          inv_line_loop
            call "PLOWNEXT" (#6, linekey$, 17%, f1%(6))
            if f1%(6) = 0% then invoice_loop

            seq$ = str(linekey$,18)
            gosub load_line_item

            if line% > 55% then gosub line_heading
            print using L15945, seq$, part$, cat$, item$, project$,       ~
                               ship$, stkuom$, lot$(1), stat_price$,     ~
                               priceuom$, linediscpct$,                  ~
                               stat_linediscamt$, sales$
            print using L15955, descr$, order$, stat_invcost$,            ~
                               stat_lineext$, discs$
            line% = line% + 2%
            if refpart$ = " " then L15695
                print using L15962, refpart$, refdescr$
                line% = line% + 1%

L15695:     if str(lot$(),,19) = " " then L15765
                if line% > 55% then gosub line_heading
                print :  print using L15970
                         print using L15980 : line% = line% + 3%
                for l% = 1% to 29% step 4%
                  if lot$(l%) = " " then L15765
                     plo$ = " "
                     for l1% = 0% to 3%
                       if lot$(l%+l1%) = " " or l% + l1% > 30% then L15750
                          str(plo$,l1%*20%+27%) = lot$(l%+l1%)
                     next l1%
L15750:              print using L15900, plo$ :  line% = line% + 1%
                next l%

L15765:     if textidl$ = " " or print_text$ = "N" then L15805
                status% = 0%
L15775:         call "TXTPRINT" (#14, f2%(14), 134%, textidl$, " ", 9%,  ~
                                 line%, 55%, "Y", " ", status%)
                if status% = 0% then L15805
                     gosub line_heading
                     goto  L15775

L15805:     print  :  line% = line% + 1%
            goto inv_line_loop

        line_heading
            if line% > 55% then gosub page_heading
            print using L15915
            print using L15925
            print using L15935
            line% = line% + 3%
            return


        print_line   /* Prints Detail Invoice Line           */
            if line% > 55% then gosub page_heading
            print using L15900, plo$
            line% = line% + 1%  :  plo$ = " "
            return


L15900: %################################################################~
        ~#################################################################~
        ~###
L15915: %                                             PO.             SHI~
        ~PPED  QTY        UNIT PRICE PRCE  LINE    DISC AMT   SALES ACCT
L15925: %   SEQ PART NUMBER / DESCRIPTION        CTGY ITM  PROJECT  / SA ~
        ~QTYS  UOM LOT NO /UNIT COST  UOM DISC%   / NET EXT  /DISCS ACCT
L15935: %   --- -------------------------------- ---- --- -------- ------~
        ~---- ---- ------ ---------- ---- ------ ---------- ------------
L15945: %   ### ##########################       #### ### ######## ######~
        ~#### #### ###### ########## #### ###### ########## ############
L15955: %       #################################                  ######~
        ~####             ##########             ########## ############
L15962: %        Cust Xref Part: ######################### ##############~
        ~################

L15970: %        Lot Distribution: Lot No    Quantity  Lot No    Quantity~
        ~  Lot No    Quantity  Lot No    Quantity
L15980: %                          ------  ----------  ------  ----------~
        ~  ------  ----------  ------  ----------
L15990: %#### Invoices Listed

        REM *************************************************************~
            *          C O M M O N   P R I N T   R O U T I N E S        *~
            *-----------------------------------------------------------*~
            * Stuff in common to both reports.                          *~
            *************************************************************

        next_invoice
            call "PLOWALTS" (#5, rptplow$, 4%, 9%, f1%(5))
            if f1%(5) = 0% then return
                if str(rptplow$,19,8) >  inv_range$(3) then L16120
                     str(rptplow$,19,8) = inv_range$(3)
                     goto next_invoice
L16120:         if str(rptplow$,19,8) <= inv_range$(4) then L16135
                     str(rptplow$,19,8) = all(hex(ff))
                     goto next_invoice
L16135:         if str(rptplow$,10,9) <= cus_range$(4) then L16180
                     f1%(5) = 0%
                     return

L16180:     get #5 using L16190, shipto$, invnr$, postdate$, type$
L16190:         FMT CH(9), CH(8), POS(533), CH(6), POS(891), CH(1)
            if postdate$ < from_dateu$ or postdate$ > asofu$             ~
                                             then next_invoice
            if pos(str(types$) = type$) = 0% then next_invoice
            on pos ("ACDFGMORX" = type$) gosub L16270, L16280, L16290,      ~
                                 L16300, L16310, L16320, L16330, L16340, L16345
            return
L16270:         type$ = "Adj"  : doctype$ = "Adjustment"        : return
L16280:         type$ = "CM"   : doctype$ = "Credit Memo"       : return
L16290:         type$ = "Drct" : doctype$ = "Direct"            : return
L16300:         type$ = "FC"   : doctype$ = "Finance Charge"    : return
L16310:         type$ = "Gen"  : doctype$ = "Generated"         : return
L16320:         type$ = "Manl" : doctype$ = "Manual"            : return
L16330:         type$ = "Ordr" : doctype$ = "On-Order"          : return
L16340:         type$ = "Rcrg" : doctype$ = "Recurring Master"  : return
L16345:         type$ = "Xprt" : doctype$ = "Export Invoice"    : return


        page_heading
            page% = page% + 1%  :  line% = 7%
            print page
            print using L16490, date$, runtime$, company$, rptid$
            print using L16520, asof$, rpthdr$, page%
            print
            print using L16550, cus_range$, from_date$, asof$
            print using L16580, inv_range$, print_types$
            print
            return


L16490: %RUN DATE: ######## ########               ######################~
        ~######################################               ARQINVSB:###~
        ~###
L16520: %   AS OF: ########             #################################~
        ~###############################################          PAGE: ##~
        ~###
L16550: %                SELECTION CRITERIA:  SHIP-TO CUSTOMERS: ########~
        ~##############     POST DATES: ######## TO ########

L16580: %                                     INVOICE NUMBERS  : ########~
        ~##############     INV. TYPES: ####################



        report_control:
            page% = 0%
            line% = 857%
            runtime$ = " " : call "TIME" (runtime$)
            call "COMPNAME" (12%, company$, u3%)
            call "SHOSTAT"  ("Printing " & report$)
            if cus_range$(1) = "ALL" then cus_range$ = "ALL" else        ~
                cus_range$ = str(cus_range$(1)) & " to " & cus_range$(2)
            if inv_range$(1) = "ALL" then inv_range$ = "ALL" else        ~
                inv_range$ = str(inv_range$(1)) & "  to " & inv_range$(2)
            if types$ = "ZZZZZZZZZ" then print_types$ = "ALL" else       ~
                                         print_types$ = " "
            alltypes$ = "ACDFGMORX"
            for x% = 1% to 9%
                if str(types$,x%,1) = " " then L16840
                     str(types$,x%,1) = str(alltypes$,x%,1)
                     if print_types$ = "ALL" then L16840
                          if print_types$ = " "                          ~
                                then print_types$ = str(types$,x%,1)     ~
                                else print_types$ = print_types$ & " " & ~
                                                    str(types$,x%,1)
L16840:     next x%
            rptplow$ = all(hex(00))
            str(rptplow$, 1, 9) = billto$
            str(rptplow$,10, 9) = cus_range$(3)
            rpthdr$   = report$ & " for Bill-to " & billto$ & "  " &     ~
                                                    billtoname$
            call "STRING" addr("CT", rpthdr$, 80%)
            call "SETPRNT" (rptid$, " ", 0%, 0%)
            select printer (134)
            report%   = 1%
            if rptid$ = "ARM009" then gosub listing_printing             ~
                                 else gosub invoice_printing
            report%   = 0%
            close printer  :  select ws
            call "SETPRNT" (rptid$, " ", 0%, 1%)
            if line% <> 857% then return
                call "ASKUSER" (2%, report$,                             ~
                          "No invoices were found within the criteria",  ~
                          "specified.  Press any PF Key to Continue...", ~
                          " ")
                return


        REM *************************************************************~
            *                 D E F A U L T S                           *~
            *-----------------------------------------------------------*~
            * Set Input Messages for Report Screen.                     *~
            *************************************************************

        deffn'052(fieldnr%)
            on fieldnr% gosub L21100, L21130, L21160, L21190, L21220
            return

L21100:     inpmessage$ = "Enter Ship-to Customer Range to Print."
            return

L21130:     inpmessage$ = "Enter Invoice Number Range to Print."
            return

L21160:     inpmessage$ = "Enter Earliest Post Date to Print."
            return

L21190:     inpmessage$ = "Enter which Invoice Types to Print."
            return

L21220:     inpmessage$ = "Print Invoice Text? (Y/N)."
            return


        REM *************************************************************~
            *                L O A D   S U M M A R Y                    *~
            *-----------------------------------------------------------*~
            * Loads data for summary screen display.                    *~
            *************************************************************
        load_summary
            init(" ") disp$(), ltr_lines$(), lst_lines$()
            disp% = 0%
            eof%  = 1%
            if findmode% = 1% then load_loop
                if first% = 1% then                                      ~
                    plowkey$ = str(billto$) & str(onlyshipto$) & lastinv$~
                    else                                                 ~
                    plowkey$ = str(plowkey$,,18) & lastinv$
                if onlyshipto$ = " " then break% = 9% else break% = 18%

        load_loop
            call "PLOWALTS" (#5, plowkey$, 4%, break%, f1%(5))
            if f1%(5) = 1% then L30230
L30170:         eof% = 0%
                if disp% < 14% then disp$(disp%+1%) = "*EOF*"
                if disp% < 14% then ltr_lines$(disp%+1%)= "*EOF*"
                  return

L30230:     get #5 using L30250, shipto$, invnr$, po$, so$, invdate$,     ~
                                postdate$, netinv, str$, type$
L30250:         FMT CH(9), CH(8), CH(16), CH(16), POS(521), CH(6), XX(6),~
                    CH(6), POS(833), PD(14,4), POS(870), CH(3), POS(891),~
                    CH(1)
            if postdate$ > asofu$ then load_loop
            if onlytype$   <> " " and type$   <> onlytype$ then load_loop
            if onlyshipto$ <> " " and shipto$ <> onlyshipto$ then L30170

            disp% = disp% + 1%  :  lastinv$ = invnr$
            call "DATEFMT" (invdate$ )
            call "DATEFMT" (postdate$)
            if type$ = "A" then type$ = "Adj"
            if type$ = "C" then type$ = "CM"
            if type$ = "D" then type$ = "Drct"
            if type$ = "F" then type$ = "FC"
            if type$ = "G" then type$ = "Gen"
            if type$ = "M" then type$ = "Manl"
            if type$ = "O" then type$ = "Ordr"
            if type$ = "R" then type$ = "Rcrg"
            if type$ = "X" then type$ = "Xprt"

            pono$(disp%) = po$
            sono$(disp%) = so$
            disp$(disp%) = str(shipto$,,9) & " " & str(invnr$,,8) & " " &~
                           str(invdate$,,8) & " " & str(type$,,4) &      ~
                           "                  " &                        ~
                           str(postdate$,,8) & "  " & str(str$)
            if sosw% = 0% then str(disp$(disp%),34,16) = pono$(disp%)    ~
                          else str(disp$(disp%),34,16) = sono$(disp%)
            netinv = round(netinv,2)
            convert netinv to str(disp$(disp%),66,14), pic(-##,###,###.##)
            if disp% = 1% then L30500
                if shipto$ = lastshipto$ then str(disp$(disp%),,9) = " "
L30500:     lastshipto$ = shipto$

            if curr$ = "N" then L30540

            ltr_lines$(disp%) = disp$(disp%)
            currkey$ = str(shipto$) & str(invnr$)
            call "READ100" (#40, currkey$, f1%(40))
            if f1%(40) = 0% then L30512
                get #40 using L30509, tran_curr_code$, trnet
L30509:              FMT CH(04), POS(54), PD(14,4)
                convert trnet to str(ltr_lines$(disp%),66,14),           ~
                                                     pic (-##,###,###.##)
L30512:     tran_curr_desc$ = " "
            if f1%(40) = 0% then currmst$ = str(stat_curr_code$) else    ~
                                 currmst$ = str(tran_curr_code$)
            call "READ100" (#42, currmst$, f1%(42))
            if f1%(42) = 0% then L30540
                get #42 using L30525, tran_curr_desc$
L30525:              FMT POS(05), CH(30)
                str(ltr_lines$(disp%),34,16) = tran_curr_desc$

L30540:     if disp% < 14% then load_loop else return

        REM *************************************************************~
            *                L O A D   I N V O I C E                    *~
            *-----------------------------------------------------------*~
            * Loads a specific invoice for display or report.           *~
            *************************************************************
        load_invoice
            paid_in_full$ = " "
            readkey$ = str(shipto$) & invnr$
            call "READ100" (#5, readkey$, invonfile%)
            if invonfile% = 0% then return

            if report% = 0% then print at(03,02), "Loading Invoice..."
            get #5 using L35010, po$, so$, bol$,                          ~
                     shipto$(), soldto$(), shipdate$, howship$, fob$,    ~
                     carrier$, cartons, weight, frtbill$, salesman$(),   ~
                     comm%(), region$, pc$, invdate$, expdate$,          ~
                     postdate$, entered$, userid$, vf$,                  ~
                     aracct$, frtacct$, taxacct$, salesacct$, discacct$, ~
                     gross, invdiscpct, invdiscamt, frtamt, taxamt, net, ~
                     stlmnt$, str$, taxcode$, taxpct, type$, invrsn$,    ~
                     textid$, posthny$, postgl$, artype$, terms$,        ~
                     termsdue(), termsdiscs(), termsdisc$(), termsnet$()

            call "CONVERT" (gross, 2.2, stat_gross$)
            call "CONVERT" (invdiscamt, 2.2, stat_invdiscamt$)
            call "CONVERT" (frtamt, 2.2, stat_frtamt$)
            call "CONVERT" (taxamt, 2.2, stat_taxamt$)
            call "CONVERT" (net, 2.2, stat_net$)
            mat termscur = termsdue  /* Set Tran Amt to Stat Amt */

            call "ARMBALNC" (billto$, stlmnt$, asofu$, 8%, "N", #15,     ~
                             #2, balance())
            if abs(balance(1%)) < .005                                   ~
                                then paid_in_full$ = "** Paid in Full **"~
                                else paid_in_full$ = "** Outstanding **"

            if curr$ = "N" then L31410
                currkey$ = str(shipto$) & str(invnr$)
                call "READ100" (#40, currkey$, f1%(40))
                if f1%(40) = 0% then L31390
                    get #40 using L31340, tran_curr_code$, tgross,        ~
                            tinvdiscamt, tfrtamt, ttaxamt, tnet,         ~
                            termscur()
L31340:                 FMT CH(4), POS(22), 5*PD(14,4), POS(92),         ~
                            30*PD(14,4)
                    call "CONVERT" (tgross, 2.2, tran_gross$)
                    call "CONVERT" (tinvdiscamt, 2.2, tran_invdiscamt$)
                    call "CONVERT" (tfrtamt, 2.2, tran_frtamt$)
                    call "CONVERT" (ttaxamt, 2.2, tran_taxamt$)
                    call "CONVERT" (tnet, 2.2, tran_net$)
                    goto L31410
L31390:         str(tran_gross$)      = str(stat_gross$)
                str(tran_invdiscamt$) = str(stat_invdiscamt$)
                str(tran_frtamt$)     = str(stat_frtamt$)
                str(tran_taxamt$)     = str(stat_taxamt$)
                str(tran_net$)        = str(stat_net$)
                str(tran_curr_code$)  = str(stat_curr_code$)

L31410:     if textid$ = hex(ffffffff) or textid$ = hex(00000000) then   ~
                                                            textid$ = " "
*        Format Ship-To / Sold-To Address
            if str(shipto$(6),17,1) <> " " or str(shipto$(6),16,1) <> " "~
               or pos(str(shipto$(6),27,4) = " ") > 0% then L31424
            temp$ = str(shipto$(6),27,4)
               str(shipto$(6),28,4) = temp$
               str(shipto$(6),27,1) = "-"
L31424:            call "LINSMASH" (shipto$())
            if str(soldto$(6),17,1) <> " " or str(soldto$(6),16,1) <> " "~
               or pos(str(soldto$(6),27,4) = " ") > 0% then L31436
            temp$ = str(soldto$(6),27,4)
               str(soldto$(6),28,4) = temp$
               str(soldto$(6),27,1) = "-"
L31436:            call "LINSMASH" (soldto$())

            call "DATEFMT" (invdate$ )
            call "DATEFMT" (shipdate$)
            call "DATEFMT" (postdate$)
            call "DATEFMT" (expdate$ )
            call "DATEFMT" (entered$ )
            if postgl$ = "N" then postgl$ = "(Not Posted)" else          ~
                                  postgl$ = " "
            if artype$ = "A" then artypedescr$ = "Accounts Receivable"
            if artype$ = "C" then artypedescr$ = "Cash"
            if artype$ = "E" then artypedescr$ = "Expense"
            init (" ") pay$()
            if str(stlmnt$,11,2) = "00" then L31580
                termsdescr$ = " " : goto L31740
L31580:     for i% = 1% to 30%
                                                        /* (EWD001)    */
              if termsnet$(i%) = " " or termsnet$(i%) = blankdate$ then L31680
                convert i% to str(pay$(i%,1),,2), pic(#0)
                convert i% to str(pay$(i%,2),,2), pic(#0)
                call "CONVERT" (termsdue  (i%),2.2, str(pay$(i%,1), 5,10))
                call "CONVERT" (termscur  (i%),2.2, str(pay$(i%,2), 5,10))
                call "CONVERT" (termsdiscs(i%),2.2, str(pay$(i%,1),16, 5))
                call "CONVERT" (termsdiscs(i%),2.2, str(pay$(i%,2),16, 5))
                str(pay$(i%,1),22,8) = termsdisc$(i%)
                str(pay$(i%,2),22,8) = termsdisc$(i%)
                     call "DATEFMT" (str(pay$(i%,1),22,8))
                     call "DATEFMT" (str(pay$(i%,2),22,8))
                str(pay$(i%,1),31,8) = termsnet$(i%)
                str(pay$(i%,2),31,8) = termsnet$(i%)
                     call "DATEFMT" (str(pay$(i%,1),31,8))
                     call "DATEFMT" (str(pay$(i%,2),31,8))
            next i%
                                                    /* (EWD001)     */
L31680:     if termsnet$(2) = " " or termsnet$(2) = blankdate$ then L31700

                termsdescr$ = "Dated"  :  goto L31740
L31700:     if termsdiscs(1) <> 0 then termsdescr$ = str(pay$(1,1),16,14)
            if termsdiscs(1)  = 0 then termsdescr$ = "NET"               ~
                                  else termsdescr$ = termsdescr$ & " NET"
            termsdescr$ = termsdescr$ & " " & str(pay$(1,1),31,8)
L31740:     stlmnt$ = str(stlmnt$,,8) & "-" & str(stlmnt$,9,2) & "-" &   ~
                                              str(stlmnt$,11)
            call "DESCRIBE" (#4, aracct$ , aracctdescr$ , 0%, f1%(4))
            call "GLFMT" (aracct$   )
            call "DESCRIBE" (#4, taxacct$, taxacctdescr$, 0%, f1%(4))
            call "GLFMT" (taxacct$  )
            call "DESCRIBE" (#4, frtacct$, frtacctdescr$, 0%, f1%(4))
            call "GLFMT" (frtacct$  )
            call "DESCRIBE" (#4, salesacct$, salesacctdescr$, 0%, f1%(4))
            call "GLFMT" (salesacct$)
            call "DESCRIBE" (#4, discacct$, discacctdescr$, 0%, f1%(4))
            call "GLFMT" (discacct$)
            init(" ") salesmandescr$(), comm$()
            for i% = 1% to 3%
                if salesman$(i%) = " " then L31920
                     call "DESCRIBE" (#11, salesman$(i%),                ~
                                         salesmandescr$(i%), 0%, f1%(11))
                     convert comm%(i%) to comm$(i%), pic(##0)
L31920:     next i%
            readkey$ = "REGIONS  " & region$
            call "DESCRIBE" (#8, readkey$, regiondescr$, 0%, f1%(8))
            readkey$ = "PRICECODE" & pc$
            call "DESCRIBE" (#8 , readkey$, pcdescr$   , 0%, f1%(8))
            readkey$ = "INVREASON" & invrsn$
            call "DESCRIBE" (#8, readkey$, invrsndescr$, 0%, f1%(8))
            readkey$ = "CARRIERS " & carrier$
            call "DESCRIBE" (#8, readkey$, carrierdescr$, 0%, f1%(8))
            call "DESCRIBE" (#12, str$ , strdescr$ , 0%, f1%(12))

*        Now load up line items
          if report% = 1% then return
            maxlines%, c% = 0%
            init(" ") lines$(), c_lines$(), tr_lines$(), tr_clines$(),   ~
                      sum_lines$()
            readkey$ = str(shipto$) & str(invnr$) & hex(00)
L32080:     call "PLOWNEXT" (#6, readkey$, 17%, f1%(6))
            if f1%(6) = 0% then return
                maxlines%, c% = maxlines% + 1%
                get #6 using L32150, str(lines$(c%), 1, 3),   /* Seq #  */~
                                    str(lines$(c%), 5,25),   /* Part   */~
                                    str(lines$(c%),31,27),   /* Descr  */~
                                    ship, ext
L32150:              FMT POS(18), CH(3), XX(3), CH(25), CH(32), POS(93), ~
                         PD(14,4), POS(157), PD(14,4)
                call "CONVERT" (ship, 2.2, str(lines$(c%),59,10))
                call "CONVERT" (ext , 2.2, str(lines$(c%),70,10))

*        Get Corresponding Customer Part Number...
                c_lines$(c%) = lines$(c%)  :  xret% = 0%
                refpart$, refdescr$ = " "
                /* Get Cross reference part from shadow file */
                call "PTUSEDSB" ("R", "ARI ", str(readkey$,,17%),        ~
                                 str(lines$(c%),,3%), refpart$,          ~
                                 refdescr$, reftype$, xret%)
                str(c_lines$(c%),5%,25%)  = refpart$
                str(c_lines$(c%),31%,27%) = refdescr$
                if xret% <> 0% then L32187
                   str(c_lines$(c%),31%,27%) = " "
                   str(c_lines$(c%),5%,25%)  = "** No Cross Reference **"
L32187:         if xret% = 1% then refflag% = 1%

*        Transaction Currency...

                if curr$ = "N" then L32300

                tr_lines$(c%) = lines$(c%)
                tr_clines$(c%) = c_lines$(c%)
                currarl$ = str(shipto$) & str(invnr$) &                  ~
                                                     str(lines$(c%),1,12)
                call "READ100" (#41, currarl$, f1%(41))
                if f1%(41) = 0% then L32300
                    get #41 using L32280, tran_curr_code$, trext
L32280:                 FMT CH(04), POS(49), PD(14,4)
                    call "CONVERT" (trext, 2.2, str(tr_lines$(c%),70,10))
                    tr_clines$(c%) = tr_lines$(c%)
                    str(tr_clines$(c%),5%,52%) = str(c_lines$(c%),5%,52%)
L32300:         goto L32080

        load_line_item
            readkey$ = str(shipto$) & str(invnr$) & seq$
            call "READ100" (#6, readkey$, lineonfile%)
            if lineonfile% = 0% then return

            get #6 using L35550, seq$, item$, part$, descr$, cat$,        ~
                     order, ship, openqty, pricestk, stkuom$, priceuom$, ~
                     conv, price, linediscpct, linediscamt, lineext,     ~
                     taxable$, sales$, discs$, textidl$, soseq$, lot$(), ~
                     lotqty(), nonstockmsg$, project$, invcost, stccost
            if textidl$ = hex(ffffffff) or textidl$ = hex(00000000)      ~
                                                      then textidl$ = " "

            /* Get Cross reference part from shadow file */
            refpart$,refdescr$,reftype$ = " "
            call "PTUSEDSB" ("R", "ARI ", str(readkey$,,17%), seq$,      ~
                              refpart$, refdescr$, reftype$, xret%)

            if nonstockmsg$ <> " " then nonstockmsg$ = "(Non-Stock Part)"
            if taxable$ = "Y" then taxable$ = "TAXABLE"                  ~
                              else taxable$ = " "
            call "DESCRIBE" (#4, sales$, salesdescr$, 0%, u3%)
            call "GLFMT" (sales$)
            call "DESCRIBE" (#4, discs$, discsdescr$, 0%, u3%)
            call "GLFMT" (discs$)
            lotmsg$ = " "
            if ship = 0 then L32560
            invcost = round(invcost/ship,2)
            stccost = round(stccost/ship,2)
            goto L32570
L32560:     invcost, stccost = 0
L32570:     call "CONVERT" (order  , 2.2, order$   )
            call "CONVERT" (openqty, 2.2, openqty$ )
            call "CONVERT" (ship   , 2.2, ship$    )
            call "CONVERT" (conv   , 2.7, conv$    )
            call "CONVERT" (invcost, 2.4, stat_invcost$ )
            call "CONVERT" (stccost, 2.4, stat_stccost$ )
            if soseq$ <> " " then order$   = " "
            if soseq$  = " " then openqty$ = " "
            call "DESCRIBE" (#7, cat$, catdescr$, 0%, f1%(7))
            for i% = 1% to 30%
                if lotqty(i%) = 0 then L32700
                call "CONVERT" (lotqty(i%), 2.2, str(lot$(i%),9,10))
            next i%
L32700:     if lot$(2) <> " " then lotmsg$ = "(Multiple Lots)"
            call "DESCRIBE" (#13, project$, projectdescr$, 0%, u3%)

            call "CONVERT" (price      , 2.4, stat_price$ )
            call "CONVERT" (linediscpct, 2.2, linediscpct$)
            call "CONVERT" (lineext    , 2.2, stat_lineext$)
            call "CONVERT" (linediscamt, 2.2, stat_linediscamt$)
            call "CONVERT" (pricestk   , 2.4, pricestk$   )

            if curr$ <> "Y" then return

            currkey$ = str(shipto$) & str(invnr$) & seq$
            call "READ100" (#41, currkey$, f1%(41))
            if f1%(41) = 1% then L32900
                tran_invcost$     = stat_invcost$
                tran_linediscamt$ = stat_linediscamt$
                tran_lineext$     = stat_lineext$
                tran_price$       = stat_price$
                tran_stccost$     = stat_stccost$
                return
L32900:     get #41 using L32920, tprice, tlinediscamt, tlineext, convunt
L32920:         FMT POS(33), 3*PD(14,4), POS(71), PD(14,7)
            call "CONVERT" (tprice      , 2.2, tran_price$      )
            call "CONVERT" (tlinediscamt, 2.2, tran_linediscamt$)
            call "CONVERT" (tlineext    , 2.2, tran_lineext$    )
            tinvcost = invcost * convunt
            call "CONVERT" (tinvcost    , 2.2, tran_invcost$    )
            tstccost = stccost * convunt
            call "CONVERT" (tstccost    , 2.2, tran_stccost$    )
            return

*       **** LARGE FORMAT STATEMENTS **********************************
L35010: FMT                 /* FILE: ARIMASTR                          */~
            XX(9),          /* Customer Code                           */~
            XX(8),          /* Invoice Number                          */~
            CH(16),         /* Purchase Order Number                   */~
            CH(16),         /* Sales Order Number                      */~
            CH(3),          /* Bill of Lading Number                   */~
            6*CH(30),       /* Ship To Name and Address                */~
            6*CH(30),       /* Sold-To Name and Address                */~
            CH(6),          /* Ship Date                               */~
            CH(20),         /* How Ship Information                    */~
            CH(20),         /* F.O.B. Information                      */~
            CH(6),          /* Shipping Carrier Code                   */~
            PD(14,4),       /* Number of Cartons                       */~
            PD(14,4),       /* Shipment Weight                         */~
            CH(20),         /* Freight/ Air Bill Number                */~
            3*CH(4),        /* Salesman Id                             */~
            3*BI(01),       /* Percentage of Sale credited to salesman.*/~
            CH(4),          /* Region Code                             */~
            CH(1),          /* Price Code                              */~
            CH(6),          /* Invoice Date                            */~
            CH(6),          /* Recurring Expiration Date               */~
            CH(6),          /* Post Date                               */~
            CH(6),          /* Entry Date                              */~
            CH(3),          /* User ID who entered transaction         */~
            CH(200),        /* Variable Fields Data Area               */~
            CH(9),          /* Net Invoice Distr. Account (A/R)        */~
            CH(9),          /* Freight Account Code                    */~
            CH(9),          /* Sales Tax Account Code                  */~
            CH(9),          /* Sales Distribution Account              */~
            CH(9),          /* Sales Discounts Distribution Account    */~
            PD(14,4),       /* Gross Invoice Amount                    */~
            PD(14,4),       /* Discount Percentage                     */~
            PD(14,4),       /* Discount amount                         */~
            PD(14,4),       /* Freight Amount                          */~
            PD(14,4),       /* Sales Tax Amount                        */~
            PD(14,4),       /* Net Invoice Amount                      */~
            XX(8),          /* Filler                                  */~
            XX(9),          /* Bill-to Cross Reference                 */~
            CH(12),         /* Settlement Code                         */~
            CH(3),          /* Store Code                              */~
            CH(10),         /* Sales Tax Code                          */~
            PD(14,4),       /* Sales Tax Percent                       */~
            CH(1),          /* Invoice Type                            */~
            CH(9),          /* Invoice Reason Code                     */~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            CH(1),          /* Post Inventory with this Transaction?   */~
            CH(1),          /* Post G/L with this Transaction?         */~
            CH(1),          /* A/R Type (A/C/E)                        */~
            CH(20),         /* Payment Terms                           */~
            30*PD(14,4),    /* Payment Amount Due                      */~
            30*PD(14,4),    /* Cash Discount Percent                   */~
            30*CH(6),       /* Discount Terms                          */~
            30*CH(6)        /* Net Payment Terms                       */

L35550: FMT                 /* FILE: ARILINES                          */~
            XX(9),          /* Customer Code                           */~
            XX(8),          /* Invoice Number                          */~
            CH(3),          /* General purpose sequence number         */~
            CH(3),          /* Purchase Order Line Number              */~
            CH(25),         /* Part Number                             */~
            CH(32),         /* Part description                        */~
            CH(4),          /* Category Code                           */~
            PD(14,4),       /* Order Quantity                          */~
            PD(14,4),       /* Quantity Shipped                        */~
            PD(14,4),       /* Open Quantity                           */~
            PD(14,4),       /* Unit Price- at Stocking UOM             */~
            CH(4),          /* Stocking UOM                            */~
            CH(4),          /* Pricing Unit of Measure                 */~
            PD(14,7),       /* Conversion Factor (buy to sell)         */~
            PD(14,4),       /* Unit Price at Pricing UOM               */~
            PD(14,4),       /* Line Item Discount                      */~
            PD(14,4),       /* Discount Amount                         */~
            PD(14,4),       /* Extension                               */~
            CH(1),          /* Taxable Purchase (Y/N)                  */~
            CH(9),          /* Sales Account Number                    */~
            CH(9),          /* Discounts Account                       */~
            XX(6),          /* Filler                                  */~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            CH(3),          /* General purpose sequence number         */~
            30*CH(6),       /* Lot Number                              */~
            30*PD(14,4),    /* Quantity in corresponding lot           */~
            CH(1),          /* Non-Stock Flag (Y=Non-Stock else blank) */~
            CH(8),          /* Project                                 */~
            XX(4),          /* Seq Number                              */~
            PD(14,4),       /* Total Inventory Cost                    */~
            PD(14,4)        /* Total Stqndard  Cost                    */

        REM *************************************************************~
            *               M A I N   S C R E E N                       *~
            *-----------------------------------------------------------*~
            * Invoice Listing screen.                                   *~
            *************************************************************

        deffn'101(opt%)
            hdr1$(1) = "Ship-to #"  /* HDR1$(5) = "Customer's PO" */
            hdr1$(2) = "Invoice#"   :  hdr1$(6) = " Posted"
            hdr1$(3) = "Inv Date"   :  hdr1$(7) = "Store"
            hdr1$(4) = "Type"       :  hdr1$(8) = "   Net Amount"
            str(line2$,,61%) = "Bill-to: " & billto$ & "  " & billtoname$

            init(hex(86)) dfac$()  :  init(hex(84)) lfac$()
            if billto% = 1% then onlyshipto_d$ = onlyshipto$             ~
                            else onlyshipto_d$ = " "
            if opt% = 0% then L40097
                init(hex(8c)) dfac$(), lfac$()
                lfac$(opt%) = hex(81)
                if opt% = 1% and onlyshipto$ <> " " then                 ~
                                                       lfac$(4) = hex(8c)
                if opt% = 1% and onlyshipto$  = " " then                 ~
                                                       lfac$(4) = hex(81)
L40097:     gosub setpf1

L40099:     mc_display$ = "N"
*        STATUTORY - Invoice Listing Setup
            if sosw% = 0% then hdr1$(5) = "Customer's PO"                ~
                          else hdr1$(5) = "Sales Order #"
            mat lst_lines$ = disp$
            goto L40115
L40105
*        TRANSACTION - Invoice Listing Setup
            hdr1$(5) = "Currency     "
            mat lst_lines$ = ltr_lines$

L40115:     accept                                                       ~
               at (01,02),                                               ~
                  "A/R Trial Balance Inquiry: INVOICE LISTING",          ~
               at (01,66), "As Of:",                                     ~
               at (01,73), fac(hex(8c)), asof$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), fac(hex(ac)), hdr1$(1)               , ch(09),~
               at (05,12), fac(hex(ac)), hdr1$(2)               , ch(08),~
               at (05,21), fac(hex(ac)), hdr1$(3)               , ch(08),~
               at (05,30), fac(hex(ac)), hdr1$(4)               , ch(04),~
               at (05,35), fac(hex(ac)), hdr1$(5)               , ch(16),~
               at (05,52), fac(hex(ac)), hdr1$(6)               , ch(08),~
               at (05,61), fac(hex(ac)), hdr1$(7)               , ch(05),~
               at (05,68), fac(hex(ac)), hdr1$(8)               , ch(13),~
                                                                         ~
               at (06,02), fac(hex(80))  , lst_lines$(1)        , ch(79),~
               at (06,02), fac(dfac$( 1)), lst_lines$(1)        , ch(79),~
               at (07,02), fac(dfac$( 2)), lst_lines$(2)        , ch(79),~
               at (08,02), fac(dfac$( 3)), lst_lines$(3)        , ch(79),~
               at (09,02), fac(dfac$( 4)), lst_lines$(4)        , ch(79),~
               at (10,02), fac(dfac$( 5)), lst_lines$(5)        , ch(79),~
               at (11,02), fac(dfac$( 6)), lst_lines$(6)        , ch(79),~
               at (12,02), fac(dfac$( 7)), lst_lines$(7)        , ch(79),~
               at (13,02), fac(dfac$( 8)), lst_lines$(8)        , ch(79),~
               at (14,02), fac(dfac$( 9)), lst_lines$(9)        , ch(79),~
               at (15,02), fac(dfac$(10)), lst_lines$(10)       , ch(79),~
               at (16,02), fac(dfac$(11)), lst_lines$(11)       , ch(79),~
               at (17,02), fac(dfac$(12)), lst_lines$(12)       , ch(79),~
               at (18,02), fac(dfac$(13)), lst_lines$(13)       , ch(79),~
               at (19,02), fac(dfac$(14)), lst_lines$(14)       , ch(79),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
               at (22,39), fac(lfac$( 4)), findshipto$          , ch(09),~
               at (22,49), fac(lfac$( 1)), findinvnr$           , ch(08),~
               at (23,34), fac(lfac$( 2)), onlyshipto_d$        , ch(09),~
               at (24,39), fac(lfac$( 3)), onlytype$            , ch(01),~
                     keys(str(pfkeys$)), key(keyhit%)

               if curr$ = "N" then L40330

               if keyhit% <> 24 then L40330
                  if mc_display$ = "Y" then L40325
                       mc_display$ = "Y"
                       str(pf$(3),1,8) = "(24)Stat"
                       goto L40105
L40325:           str(pf$(3),1,8) = "(24)Tran"
                  goto L40099

L40330:        if keyhit% <> 13 then L40350
                  call "MANUAL" ("ARQINVSB")
                  goto L40115

L40350:        if keyhit% <> 15 then L40370
                  call "PRNTSCRN"
                  goto L40115

L40370:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        setpf1: on opt% + 1% goto L40395, L40480, L40490, L40500
L40395:    inpmessage$ = "Position Cursor and Press Return to see"  &    ~
                         " Invoice Details,  PF9 to toggle SO/PO"
           pf$(1) = "(2)First  ( 8)Find Ship-to & Invoice xxxxxxxxx xxx"&~
                    "xxxxx        (13)Instructions"
           pf$(2) = "(5)Next   (10)List Only Ship-to xxxxxxxxx (12)Prin"&~
                    "t Listing    (15)Print Screen"
           if curr$ = "N" then                                           ~
           pf$(3) = "          (11)List Only Invoice Type x    (14)Prin"&~
                    "t Invoices   (16)Exit Display"                      ~
           else                                                          ~
           pf$(3) = "(24)Tran  (11)List Only Invoice Type x    (14)Prin"&~
                    "t Invoices   (16)Exit Display"

           pfkeys$ = hex(ff02ffff05ffff08090a0b0c0d0e0f10ff182000)
           if curr$  = "Y"  then L40440  : str(pfkeys$,18,1) = hex(ff)
L40440:    if first% = 0%   then L40450  : str(pf$(1), 1,8)  = " "
                                          str(pfkeys$, 2,1) = hex(ff)
L40450:    if eof%   = 1%   then L40460  : str(pf$(2), 1,8)  = " "
                                          str(pfkeys$, 5,1) = hex(ff)
L40460:    if shiptos% > 1% then L40470  : str(pf$(2),11,26) = " "
                                          str(pfkeys$,10,1) = hex(ff)
L40470:    return

L40480:    inpmessage$ = "Enter Ship-to and Invoice Number or Press" &   ~
                         " PF-1 to Return to Listing." : goto L40510
L40490:    inpmessage$ = "Enter Ship-to to limit listing to or Press" &  ~
                         " PF-1 to Return to Listing." : goto L40510
L40500:    inpmessage$ = "Enter Invoice Type to limit listing to or"  &  ~
                         " Press PF-1 to Return to Listing."
L40510:    pf$(1) = "(1)Return     Find Ship-to & Invoice              "&~
                    "             (13)Instructions"
           pf$(2) = "              List Only Ship-to                   "&~
                    "             (15)Print Screen"
           pf$(3) = "              List Only Invoice Type              "&~
                    "                             "
           if opt%    <> 1% then str(pf$(1),15,25) = " "
           if shiptos% = 1% then str(pf$(2),11,26) = " "
           pfkeys$ = hex(01ffffffffffffffffffffff0dff0fffffffff00)
           return


        REM *************************************************************~
            *        R E P O R T   P A R A M E T E R S                  *~
            *-----------------------------------------------------------*~
            * Get Report Parameters.                                    *~
            *************************************************************

        deffn'102(fieldnr%)
            str(line2$,,61%) = "Bill-to: " & billto$ & "  " & billtoname$
            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            if fieldnr% > 0% then lfac$(fieldnr%) = hex(81)
            gosub setpf2

L41130:     accept                                                       ~
               at (01,02), "A/R Trial Balance Inquiry: Print",           ~
               at (01,35), fac(hex(8c)), report$                , ch(30),~
               at (01,66), "As Of:",                                     ~
               at (01,73), fac(hex(8c)), asof$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Ship-to Customers to Print",                 ~
               at (06,30), fac(lfac$( 1)), cus_range$(1)        , ch(09),~
               at (06,40), "to",                                         ~
               at (06,43), fac(lfac$( 1)), cus_range$(2)        , ch(09),~
                                                                         ~
               at (07,02), "Invoice Numbers to Print",                   ~
               at (07,30), fac(lfac$( 2)), inv_range$(1)        , ch(08),~
               at (07,40), "to",                                         ~
               at (07,43), fac(lfac$( 2)), inv_range$(2)        , ch(08),~
                                                                         ~
               at (08,02), "Starting Post Date",                         ~
               at (08,30), fac(lfac$( 3)), from_date$           , ch(08),~
               at (08,40), "to",                                         ~
               at (08,43), fac(hex(8c))  , asof$                , ch(08),~
                                                                         ~
               at (09,02), "Selected Invoice Types",                     ~
               at (09,30), "A -  C -  D -  F -  G -  M -  O -  R -  X -",~
               at (09,32), fac(lfac$( 4)), str(types$,1,1)      , ch(01),~
               at (09,37), fac(lfac$( 4)), str(types$,2,1)      , ch(01),~
               at (09,42), fac(lfac$( 4)), str(types$,3,1)      , ch(01),~
               at (09,47), fac(lfac$( 4)), str(types$,4,1)      , ch(01),~
               at (09,52), fac(lfac$( 4)), str(types$,5,1)      , ch(01),~
               at (09,57), fac(lfac$( 4)), str(types$,6,1)      , ch(01),~
               at (09,62), fac(lfac$( 4)), str(types$,7,1)      , ch(01),~
               at (09,67), fac(lfac$( 4)), str(types$,8,1)      , ch(01),~
               at (09,72), fac(lfac$( 4)), str(types$,9,1)      , ch(01),~
                                                                         ~
               at (10,02), "Include Text? (Y/N)",                        ~
               at (10,30), fac(lfac$( 5)), print_text$          , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(str(pfkeys$)), key(keyhit%)

               if keyhit% <> 13 then L41600
                  call "MANUAL" ("ARQINVSB")
                  goto L41130

L41600:        if keyhit% <> 15 then L41640
                  call "PRNTSCRN"
                  goto L41130

L41640:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                return

        setpf2
           pf$(1) = "(1)Exit Report Function                           "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "             (16)Print Report"
           pfkeys$ = hex(01ffffffffffffffffffffff0dff0f10ffff2000)
           if fieldnr% = 0% then return
                pf$(3) = " "
                str(pfkeys$,16,1) = hex(ff)
                return


        REM *************************************************************~
            *               D E T A I L   H D R   1                     *~
            *-----------------------------------------------------------*~
            * Invoice Detail- Hdr Page 1.                               *~
            *************************************************************

        deffn'111

            gosub setpf11
L42029:     mc_display$ = "N" : disp_code$ = " "
*        STATUTORY - HDR1 Setup
            if curr$ = "N" then L42033
                str(disp_code$) = "---" & str(stat_curr_code$) & "---"
L42033:     disp_gross$      = stat_gross$
            disp_invdiscpct  = invdiscpct
            disp_invdiscamt$ = stat_invdiscamt$
            disp_frtamt$     = stat_frtamt$
            disp_taxamt$     = stat_taxamt$
            disp_net$        = stat_net$
            goto L42051
L42040
*        TRANSACTION - HDR1 Setup
            str(disp_code$) = "---" & str(tran_curr_code$) & "---"
            disp_gross$      = tran_gross$
            disp_invdiscamt$ = tran_invdiscamt$
            disp_frtamt$     = tran_frtamt$
            disp_taxamt$     = tran_taxamt$
            disp_net$        = tran_net$

L42051:     accept                                                       ~
               at (01,02),                                               ~
                  "A/R Trial Balance Inquiry: INVOICE DETAIL   Hdr1",    ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Ship", at (05,04), "To",                     ~
               at (04,08), fac(hex(84)),   shipto$(1)           , ch(31),~
               at (05,08), fac(hex(84)),   shipto$(2)           , ch(31),~
               at (06,08), fac(hex(84)),   shipto$(3)           , ch(31),~
               at (07,08), fac(hex(84)),   shipto$(4)           , ch(31),~
               at (08,08), fac(hex(84)),   shipto$(5)           , ch(31),~
               at (09,08), fac(hex(84)),   shipto$(6)           , ch(31),~
                                                                         ~
               at (04,42), "Sold",  at (05,44), "To",                    ~
               at (04,48), fac(hex(84)),   soldto$(1)           , ch(31),~
               at (05,48), fac(hex(84)),   soldto$(2)           , ch(31),~
               at (06,48), fac(hex(84)),   soldto$(3)           , ch(31),~
               at (07,48), fac(hex(84)),   soldto$(4)           , ch(31),~
               at (08,48), fac(hex(84)),   soldto$(5)           , ch(31),~
               at (09,48), fac(hex(84)),   soldto$(6)           , ch(31),~
               at (10,29), fac(hex(84)),   disp_code$           , ch(10),~
                                                                         ~
               at (11,02), "Gross Invoice Amount",                       ~
               at (11,24), fac(hex(84)), disp_gross$            , ch(15),~
               at (12,02), "Inv Disc ( -###.00 %)",                      ~
               at (12,13), fac(hex(84)), disp_invdiscpct,   pic(-###.00),~
               at (12,24), fac(hex(84)), disp_invdiscamt$       , ch(15),~
               at (13,02), "Freight Charges",                            ~
               at (13,24), fac(hex(84)), disp_frtamt$           , ch(15),~
               at (14,02), "Sales Tax Amount",                           ~
               at (14,24), fac(hex(a4)), disp_taxamt$           , ch(15),~
               at (15,02), "Net Invoice Amount",                         ~
               at (15,24), fac(hex(84)), disp_net$              , ch(15),~
                                                                         ~
               at (11,42), "Invoiced:",                                  ~
               at (11,52), fac(hex(84)),   invdate$             , ch(08),~
               at (12,42), "Posted  :",                                  ~
               at (12,52), fac(hex(84)),   postdate$            , ch(08),~
               at (13,42), "Shipped :",                                  ~
               at (13,52), fac(hex(84)),   shipdate$            , ch(08),~
               at (14,42), "Tax Code:",                                  ~
               at (14,52), fac(hex(84)),   taxcode$             , ch(10),~
               at (14,63), "Tax%:",                                      ~
               at (14,68), fac(hex(84)),   taxpct         , pic(-###.00),~
               at (15,42), "A/R Type:",                                  ~
               at (15,52), fac(hex(84)),   artypedescr$         , ch(20),~
               at (16,42), "   Terms:",                                  ~
               at (16,52), fac(hex(84)),   termsdescr$          , ch(28),~
                                                                         ~
               at (17,02), "Settlement",                                 ~
               at (17,19), fac(hex(84)),   stlmnt$              , ch(14),~
               at (17,42), "Invoice Entered MM/DD/YY by",                ~
               at (17,58), fac(hex(84)),   entered$             , ch(08),~
               at (17,70), fac(hex(84)),   userid$              , ch(03),~
               at (18,02), "Sales Order-BOL",                            ~
               at (18,19), fac(hex(84)),   so$                  , ch(16),~
               at (18,36), fac(hex(84)),   bol$                 , ch(03),~
               at (19,02), "Customer PO",                                ~
               at (19,19), fac(hex(84)),   po$                  , ch(16),~
               at (19,42), fac(hex(84)),   paid_in_full$        , ch(18),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(str(pfkeys$)), key(keyhit%)

               if curr$ = "N" then L42385

               if keyhit% <> 24 then L42385
                  if mc_display$ = "Y" then L42380
                     mc_display$ = "Y"
                     str(pf$(2),47,4) = "Stat"
                     goto L42040
L42380:           str(pf$(2),47,4) = "Tran"
                  goto L42029

L42385:        if keyhit% <> 13 then L42405
                  call "MANUAL" ("ARQINVSB")
                  goto L42051

L42405:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L42051

        setpf11
           inpmessage$ = " "
           pf$(1) = "(2)Line Items     (8)See Payment Schedule (14)Prin"&~
                    "t Invoice    (13)Instructions"
           if curr$ = "N" then                                           ~
           pf$(2) = "(5)Header Page 2  (9)See Variable Fields          "&~
                    "             (15)Print Screen"                      ~
           else                                                          ~
           pf$(2) = "(5)Header Page 2  (9)See Variable Fields  (24)Tran"&~
                    " Currency    (15)Print Screen"
           pf$(3) = "                                          (26)Disp"&~
                    "lay Text     (16)Exit Display"
           pfkeys$ = hex(ff02ffff05ffff0809ffffff0d0e0f10ff181aff20ff)
           if curr$ = "Y" then L42490
                str(pfkeys$,18,1) = hex(ff)
L42490:    if termsdescr$ = "Dated" then L42500
                str(pf$(1),19,24) = " " : str(pfkeys$, 8,1) = hex(ff)
L42500:    if vf$ <> " " then L42510
                str(pf$(2),19,24) = " " : str(pfkeys$, 9,1) = hex(ff)
L42510:    if textid$ <> " " then L42520
                str(pf$(3),43,18) = " " : str(pfkeys$,19,1) = hex(ff)
L42520:    return


        REM *************************************************************~
            *               D E T A I L   H D R   2                     *~
            *-----------------------------------------------------------*~
            * Invoice Detail- Hdr Page 2.                               *~
            *************************************************************

        deffn'112
            gosub setpf12

L43090:     accept                                                       ~
               at (01,02),                                               ~
                  "A/R Trial Balance Inquiry: INVOICE DETAIL   Hdr2",    ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Shipping Store",                             ~
               at (04,30), fac(hex(84)),   str$                 , ch(30),~
               at (04,49), fac(hex(84)),   strdescr$            , ch(30),~
               at (05,02), "How Ship",                                   ~
               at (05,30), fac(hex(84)),   howship$             , ch(20),~
               at (06,02), "FOB",                                        ~
               at (06,30), fac(hex(84)),   fob$                 , ch(20),~
               at (07,02), "Carrier",                                    ~
               at (07,30), fac(hex(84)),   carrier$             , ch(09),~
               at (07,49), fac(hex(84)),   carrierdescr$        , ch(30),~
               at (08,02), "Air/Freight Bill",                           ~
               at (08,30), fac(hex(84)),   frtbill$             , ch(20),~
               at (09,02), "Cartons and Weight",                         ~
               at (09,30), fac(hex(84)),   cartons     , pic(-######.##),~
               at (09,41), fac(hex(84)),   weight      , pic(-######.##),~
/*EWD003*/     at (10,02), "Invoice Reason",                             ~
/*EWD003*/     at (10,30), fac(hex(84)),   invrsn$              , ch(09),~
/*EWD003*/     at (10,49), fac(hex(84)),   invrsndescr$         , ch(30),~
                                                                         ~
               at (11,02), "Sales Region",                               ~
               at (11,30), fac(hex(84)),   region$              , ch(04),~
               at (11,49), fac(hex(84)),   regiondescr$         , ch(30),~
               at (12,02), "Salesmen / Split %",                         ~
               at (12,30), fac(hex(84)),   salesman$(1)         , ch(04),~
               at (12,36), fac(hex(84)),   comm$(1)             , ch(03),~
               at (12,49), fac(hex(84)),   salesmandescr$(1)    , ch(30),~
               at (13,30), fac(hex(84)),   salesman$(2)         , ch(04),~
               at (13,36), fac(hex(84)),   comm$(2)             , ch(03),~
               at (13,49), fac(hex(84)),   salesmandescr$(2)    , ch(30),~
               at (14,30), fac(hex(84)),   salesman$(3)         , ch(04),~
               at (14,36), fac(hex(84)),   comm$(3)             , ch(03),~
               at (14,49), fac(hex(84)),   salesmandescr$(3)    , ch(30),~
                                                                         ~
               at (15,02), "Net Invoice Distr.",                         ~
               at (15,30), fac(hex(84)),   aracct$              , ch(12),~
               at (15,49), fac(hex(84)),   aracctdescr$         , ch(30),~
               at (16,02), "Sales Tax Account ",                         ~
               at (16,30), fac(hex(84)),   taxacct$             , ch(12),~
               at (16,49), fac(hex(84)),   taxacctdescr$        , ch(30),~
               at (17,02), "Freight Account",                            ~
               at (17,30), fac(hex(84)),   frtacct$             , ch(12),~
               at (17,49), fac(hex(84)),   frtacctdescr$        , ch(30),~
               at (18,02), "Sales Distr Dflt",                           ~
               at (18,30), fac(hex(84)),   salesacct$           , ch(12),~
               at (18,49), fac(hex(84)),   salesacctdescr$      , ch(30),~
               at (19,02), "Sales Discs Account",                        ~
               at (19,30), fac(hex(84)),   discacct$            , ch(12),~
               at (19,49), fac(hex(84)),   discacctdescr$       , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(str(pfkeys$)), key(keyhit%)

               if keyhit% <> 13 then L43730
                  call "MANUAL" ("ARQINVSB")
                  goto L43090

L43730:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L43090

        setpf12
           inpmessage$ = " "
           pf$(1) = "(2)Line Items     (8)See Payment Schedule (14)Prin"&~
                    "t Invoice    (13)Instructions"
           pf$(2) = "(4)Header Page 1  (9)See Variable Fields          "&~
                    "             (15)Print Screen"
           pf$(3) = "                                          (26)Disp"&~
                    "lay Text     (16)Exit Display"
           pfkeys$ = hex(ff02ff04ffffff0809ffffff0d0e0f101aff20ff)
           if termsdescr$ = "Dated" then L43920
                str(pf$(1),19,24) = " " : str(pfkeys$, 8,1) = hex(ff)
L43920:    if vf$ <> " " then L43940
                str(pf$(2),19,24) = " " : str(pfkeys$, 9,1) = hex(ff)
L43940:    if textid$ <> " " then L43960
                str(pf$(3),43,18) = " " : str(pfkeys$,17,1) = hex(ff)
L43960:    return


        REM *************************************************************~
            *             P A Y M E N T   S C H E D U L E               *~
            *-----------------------------------------------------------*~
            * Display Payment Schedule.                                 *~
            *************************************************************

        deffn'113
            gosub setpf13
            hdr2$(1) = "##"
            hdr2$(2) = "    Amount"
            hdr2$(3) = "Disc%"
            hdr2$(4) = "Disc Due"
            hdr2$(5) = " Net Due"

L44100:     mc_display$ = "N"    /* Start with statutory values */
            hdr2$(6) = "-" & stat_curr_code$ & "-"
            cur% = 1%
            if curr$ = "Y" then str(pf$(2),47,4) = "Tran"
L44120:     call "STRING" addr("RJ", hdr2$(6), 10%, hdr2$(6))
            if curr$ <> "Y" then hdr2$(6) = " "

L44140:     accept                                                       ~
               at (01,02),                                               ~
                  "A/R Trial Balance Inquiry: INVOICE PAYMENT SCHEDULE", ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,06), fac(hex(84)), hdr2$(6)               , ch(10),~
               at (04,46), fac(hex(84)), hdr2$(6)               , ch(10),~
                                                                         ~
               at (05,02), fac(hex(ac)), hdr2$(1)               , ch(02),~
               at (05,06), fac(hex(ac)), hdr2$(2)               , ch(10),~
               at (05,17), fac(hex(ac)), hdr2$(3)               , ch(05),~
               at (05,23), fac(hex(ac)), hdr2$(4)               , ch(08),~
               at (05,32), fac(hex(ac)), hdr2$(5)               , ch(08),~
               at (05,42), fac(hex(ac)), hdr2$(1)               , ch(02),~
               at (05,46), fac(hex(ac)), hdr2$(2)               , ch(10),~
               at (05,57), fac(hex(ac)), hdr2$(3)               , ch(05),~
               at (05,63), fac(hex(ac)), hdr2$(4)               , ch(08),~
               at (05,72), fac(hex(ac)), hdr2$(5)               , ch(08),~
                                                                         ~
               at (06,02), fac(hex(84))  ,  pay$( 1, cur%)      , ch(39),~
               at (07,02), fac(hex(84))  ,  pay$( 2, cur%)      , ch(39),~
               at (08,02), fac(hex(84))  ,  pay$( 3, cur%)      , ch(39),~
               at (09,02), fac(hex(84))  ,  pay$( 4, cur%)      , ch(39),~
               at (10,02), fac(hex(84))  ,  pay$( 5, cur%)      , ch(39),~
               at (11,02), fac(hex(84))  ,  pay$( 6, cur%)      , ch(39),~
               at (12,02), fac(hex(84))  ,  pay$( 7, cur%)      , ch(39),~
               at (13,02), fac(hex(84))  ,  pay$( 8, cur%)      , ch(39),~
               at (14,02), fac(hex(84))  ,  pay$( 9, cur%)      , ch(39),~
               at (15,02), fac(hex(84))  ,  pay$(10, cur%)      , ch(39),~
               at (16,02), fac(hex(84))  ,  pay$(11, cur%)      , ch(39),~
               at (17,02), fac(hex(84))  ,  pay$(12, cur%)      , ch(39),~
               at (18,02), fac(hex(84))  ,  pay$(13, cur%)      , ch(39),~
               at (19,02), fac(hex(84))  ,  pay$(14, cur%)      , ch(39),~
               at (20,02), fac(hex(84))  ,  pay$(15, cur%)      , ch(39),~
                                                                         ~
               at (06,42), fac(hex(84))  ,  pay$(16, cur%)      , ch(39),~
               at (07,42), fac(hex(84))  ,  pay$(17, cur%)      , ch(39),~
               at (08,42), fac(hex(84))  ,  pay$(18, cur%)      , ch(39),~
               at (09,42), fac(hex(84))  ,  pay$(19, cur%)      , ch(39),~
               at (10,42), fac(hex(84))  ,  pay$(20, cur%)      , ch(39),~
               at (11,42), fac(hex(84))  ,  pay$(21, cur%)      , ch(39),~
               at (12,42), fac(hex(84))  ,  pay$(22, cur%)      , ch(39),~
               at (13,42), fac(hex(84))  ,  pay$(23, cur%)      , ch(39),~
               at (14,42), fac(hex(84))  ,  pay$(24, cur%)      , ch(39),~
               at (15,42), fac(hex(84))  ,  pay$(25, cur%)      , ch(39),~
               at (16,42), fac(hex(84))  ,  pay$(26, cur%)      , ch(39),~
               at (17,42), fac(hex(84))  ,  pay$(27, cur%)      , ch(39),~
               at (18,42), fac(hex(84))  ,  pay$(28, cur%)      , ch(39),~
               at (19,42), fac(hex(84))  ,  pay$(29, cur%)      , ch(39),~
               at (20,42), fac(hex(84))  ,  pay$(30, cur%)      , ch(39),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(str(pfkeys$)), key(keyhit%)

               if keyhit% <> 13 then L44750
                  call "MANUAL" ("ARQINVSB")
                  goto L44140

L44750:        if keyhit% <> 15 then L44760
                  call "PRNTSCRN"
                  goto L44140

L44760:        if keyhit% <> 24 then return
                  if mc_display$ = "Y" then L44100
                     str(pf$(2),47,4) = "Stat"
                     mc_display$ = "Y"
                     cur% = 2%
                     hdr2$(6) = "-" & tran_curr_code$ & "-"
                     goto L44120

        setpf13
           inpmessage$ = " "
           pf$(1) = "                                                  "&~
                    "             (13)Instructions"
           if curr$ = "Y" then                                           ~
           pf$(2) = "                                          (24)Tran"&~
                    " Currency    (15)Print Screen"                      ~
           else                                                          ~
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "             (16)Return      "
           pfkeys$ = hex(ffffffffffffffffffffffff0dff0f10ff1820ff)
           if curr$ <> "Y" then str(pfkeys$,18,1) = hex(ff)
           return


        REM *************************************************************~
            *           L I N E   I T E M   S U M M A R Y               *~
            *-----------------------------------------------------------*~
            * Invoice Listing - Line Item Summary Screen.               *~
            *************************************************************

        deffn'120
            hdr3$(1) = "Seq"               :  hdr3$(4) = "   Shipped"
            hdr3$(2) = "Part Number"       :  hdr3$(5) = " Extension"
            hdr3$(3) = "Part Description"

            gosub setpf20

L45094:     mc_display$ = "N"
*        STATUTORY - Line Summary Setup
            if curr$ = "N" then hdr3$(6) = " " else                      ~
                str(hdr3$(6)) = "CURR: " & str(stat_curr_code$)
            if cms% = 1% then mat sum_lines$ = c_lines$                  ~
                         else mat sum_lines$ = lines$
            goto L45130
L45101
*        TRANSACTION - Line Summary Setup
            str(hdr3$(6%)) = "CURR: " & str(tran_curr_code$)
            if cms% = 1% then mat sum_lines$ = tr_clines$                ~
                         else mat sum_lines$ = tr_lines$


L45130:     accept                                                       ~
               at (01,02),                                               ~
                  "A/R Trial Balance Inquiry: INVOICE LINE ITEM SUMMARY",~
               at (01,55), fac(hex(84)), hdr3$(6)               , ch(10),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), fac(hex(ac)), hdr3$(1)               , ch(03),~
               at (04,06), fac(hex(ac)), hdr3$(2)               , ch(25),~
               at (04,32), fac(hex(ac)), hdr3$(3)               , ch(27),~
               at (04,60), fac(hex(ac)), hdr3$(4)               , ch(10),~
               at (04,71), fac(hex(ac)), hdr3$(5)               , ch(10),~
                                                                         ~
               at (05,02), fac(hex(80)), sum_lines$(t% +  1%)   , ch(79),~
               at (05,02), fac(hex(86)), sum_lines$(t% +  1%)   , ch(79),~
               at (06,02), fac(hex(86)), sum_lines$(t% +  2%)   , ch(79),~
               at (07,02), fac(hex(86)), sum_lines$(t% +  3%)   , ch(79),~
               at (08,02), fac(hex(86)), sum_lines$(t% +  4%)   , ch(79),~
               at (09,02), fac(hex(86)), sum_lines$(t% +  5%)   , ch(79),~
               at (10,02), fac(hex(86)), sum_lines$(t% +  6%)   , ch(79),~
               at (11,02), fac(hex(86)), sum_lines$(t% +  7%)   , ch(79),~
               at (12,02), fac(hex(86)), sum_lines$(t% +  8%)   , ch(79),~
               at (13,02), fac(hex(86)), sum_lines$(t% +  9%)   , ch(79),~
               at (14,02), fac(hex(86)), sum_lines$(t% + 10%)   , ch(79),~
               at (15,02), fac(hex(86)), sum_lines$(t% + 11%)   , ch(79),~
               at (16,02), fac(hex(86)), sum_lines$(t% + 12%)   , ch(79),~
               at (17,02), fac(hex(86)), sum_lines$(t% + 13%)   , ch(79),~
               at (18,02), fac(hex(86)), sum_lines$(t% + 14%)   , ch(79),~
               at (19,02), fac(hex(86)), sum_lines$(t% + 15%)   , ch(79),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(str(pfkeys$)), key(keyhit%)

               if curr$ = "N" then L45500

               if keyhit% <> 24 then L45500
                   if mc_display$ = "Y" then L45489
                        mc_display$ = "Y"
                        str(pf$(3%),47%,4%) = "Stat"
                        goto L45101
L45489:            str(pf$(3%),47%,4%) = "Tran"
                   goto L45094

L45500:        if keyhit% <> 13 then L45540
                  call "MANUAL" ("ARQINVSB")
                  goto L45130

L45540:        if keyhit% <> 15% then L45560
                  call "PRNTSCRN"
                  goto L45130

L45560:        if keyhit% <> 22% then L45580
                   if cms% <> 0% then L45566
                       cms% = 1%
                       hdr3$(2%) = "Customer Part Number"
                       hdr3$(3%) = "Cust Part Description"
                       if mc_display$ = "Y" then L45101 else L45094
L45566:            cms% = 0%
                   hdr3$(2%) = "Part Number"
                   hdr3$(3%) = "Part Description"
                   if mc_display$ = "Y" then L45101 else L45094

L45580:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        setpf20
           inpmessage$ = "Position Cursor and Press Return to see"  &    ~
                         " Line Item Details."
           pf$(1) = "(2)First   (5)Next                        (14)Prin"&~
                    "t Invoice    (13)Instructions"
           pf$(2) = "(3)Last    (6)Down                        (22)Part"&~
                    " Toggle      (15)Print Screen"
           if curr$ = "N" then                                           ~
           pf$(3) = "(4)Prev    (7)Up                                  "&~
                    "             (16)Exit Display"                      ~
           else                                                          ~
           pf$(3) = "(4)Prev    (7)Up                          (24)Tran"&~
                    " Currency    (16)Exit Display"

           pfkeys$ = hex(ff020304050607ffffffffff0d0e0f10ff18201600)
           if refflag% = 1% then L45712
                str(pf$(2%),43%,21%) = " " : str(pfkeys$,20%,1%) = hex(ff)

L45712:    if curr$ = "Y" then L45720
                str(pfkeys$,18,1) = hex(ff)

L45720:    if t% <> 0% then L45760
                str(pf$(1),,8), str(pf$(3),,8), str(pf$(2),12,7) = " "
                str(pfkeys$,2,1), str(pfkeys$,4,1), str(pfkeys$,6,1)     ~
                                                              = hex(ff)
L45760:    if t% + 15% < maxlines% then L45800
                str(pf$(2),,8), str(pf$(1),12,7), str(pf$(3),12,7) = " "
                str(pfkeys$,3,1), str(pfkeys$,5,1), str(pfkeys$,7,1)     ~
                                                              = hex(ff)
L45800:    return


        REM *************************************************************~
            *             L I N E   I T E M   D E T A I L               *~
            *-----------------------------------------------------------*~
            * Display Line Item Detail.                                 *~
            *************************************************************

        deffn'121
            gosub setpf21
L46054
*        Statutory Set Up
            mc_display$ = "N"
            if curr$ <> "Y" then hdr3$(6) = " " else                     ~
                                 hdr3$(6) = "CURR: " & stat_curr_code$
            disp_linediscamt$ = stat_linediscamt$
            disp_lineext$     = stat_lineext$
            disp_price$       = stat_price$
            disp_invcost$     = stat_invcost$
            disp_stccost$     = stat_stccost$
            goto L46090
L46074
*        Transaction Set Up
            hdr3$(6) = "CURR: " & tran_curr_code$
            disp_linediscamt$ = tran_linediscamt$
            disp_lineext$     = tran_lineext$
            disp_price$       = tran_price$
            disp_invcost$     = tran_invcost$
            disp_stccost$     = tran_stccost$

L46090:     accept                                                       ~
               at (01,02),                                               ~
                  "A/R Trial Balance Inquiry: INVOICE LINE DETAIL",      ~
               at (01,53), fac(hex(84)), hdr3$(6)               , ch(10),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Part Code",                                  ~
               at (05,26), fac(hex(84)),   part$                , ch(25),~
               at (05,60), fac(hex(84)),   nonstockmsg$         , ch(16),~
               at (06,02), "Part/Line Description",                      ~
               at (06,26), fac(hex(84)),   descr$               , ch(32),~
               at (06,60), "Line Sequence",                              ~
               at (06,74), fac(hex(84)),   seq$                 , ch(03),~
                                                                         ~
               at (07,02), "Xref Part/Description",                      ~
               at (07,26), fac(hex(84)),   refpart$             , ch(25),~
               at (07,53), fac(hex(84)),   refdescr$            , ch(27),~
                                                                         ~
               at (08,02), "Part Category",                              ~
               at (08,26), fac(hex(84)),   cat$                 , ch(04),~
               at (08,45), fac(hex(84)),   catdescr$            , ch(30),~
                                                                         ~
               at (09,02), "Quantity Shipped",                           ~
               at (09,26), fac(hex(84)),   ship$                , ch(10),~
               at (09,38), fac(hex(84)),   stkuom$              , ch(04),~
               at (09,45), "Lot",                                        ~
               at (09,49), fac(hex(84)),   lot$(1)              , ch(06),~
               at (09,66), fac(hex(84)),   lotmsg$              , ch(16),~
                                                                         ~
               at (10,02), "Sales Analysis Book Qty",                    ~
               at (10,26), fac(hex(84)),   order$               , ch(10),~
               at (11,02), "Open Quantity Remaining",                    ~
               at (11,26), fac(hex(84)),   openqty$             , ch(10),~
                                                                         ~
               at (12,02), "Unit Price Per xxxx",                        ~
               at (12,17), fac(hex(84)),   priceuom$            , ch(04),~
               at (12,26), fac(hex(84)),   disp_price$          , ch(10),~
               at (12,45), "UOM Conversion",                             ~
               at (12,60), fac(hex(84)),   conv$                , ch(10),~
               at (13,02), "Unit Inventory Cost",                        ~
               at (13,26), fac(hex(84)),   disp_invcost$        , ch(10),~
               at (13,45), "Unit Std Cost",                              ~
               at (13,60), fac(hex(84)),   disp_stccost$        , ch(10),~
               at (14,02), "Line Item Discount",                         ~
               at (14,26), fac(hex(84)),   disp_linediscamt$    , ch(10),~
               at (14,45), "Discount %    ",                             ~
               at (14,60), fac(hex(84)),   linediscpct$         , ch(06),~
               at (15,02), "Line Item Extension",                        ~
               at (15,26), fac(hex(84)),   disp_lineext$        , ch(10),~
               at (15,45), fac(hex(84)),   taxable$             , ch(07),~
                                                                         ~
               at (17,02), "PO Item Reference",                          ~
               at (17,26), fac(hex(84)),   item$                , ch(03),~
               at (18,02), "Project Number",                             ~
               at (18,26), fac(hex(84)),   project$             , ch(08),~
               at (18,45), fac(hex(84)),   projectdescr$        , ch(30),~
               at (19,02), "Sales Distribution Acct",                    ~
               at (19,26), fac(hex(84)),   sales$               , ch(12),~
               at (19,45), fac(hex(84)),   salesdescr$          , ch(30),~
               at (20,02), "Sales Discounts Account",                    ~
               at (20,26), fac(hex(84)),   discs$               , ch(12),~
               at (20,45), fac(hex(84)),   discsdescr$          , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(str(pfkeys$)), key(keyhit%)

               if keyhit% <> 13 then L46750
                  call "MANUAL" ("ARQINVSB")
                  goto L46090

L46750:        if keyhit% <> 15 then L46760
                  call "PRNTSCRN"
                  goto L46090

L46760:        if keyhit% <> 24 then return
                  if mc_display$ = "Y" then L46770
                     mc_display$ = "Y"
                     str(pf$(2),47,4) = "Stat"
                     goto L46074
L46770:           str(pf$(2),47,4) = "Tran"
                  goto L46054

        setpf21
           inpmessage$ = " "
           pf$(1) = "(6)Prev Line      (8)See Lot Distribution (14)Prin"&~
                    "t Invoice    (13)Instructions"
        if curr$ <> "Y" then                                             ~
           pf$(2) = "(7)Next Line                                      "&~
                    "             (15)Print Screen"                      ~
           else                                                          ~
           pf$(2) = "(7)Next Line                              (24)Tran"&~
                    " Currency    (15)Print Screen"
           pf$(3) = "                                          (26)Disp"&~
                    "lay Text     (16)Exit Display"
           pfkeys$ = hex(ffffffffff060708ffffffff0d0e0f101a1820ff)
           if c% > 1% then L46900
                str(pf$(1), 1,12) = " " : str(pfkeys$, 6,1) = hex(ff)
L46900:    if c% < maxlines% then L46920
                str(pf$(2), 1,12) = " " : str(pfkeys$, 7,1) = hex(ff)
L46920:    if lot$(2) <> " " then L46940
                str(pf$(1),19,24) = " " : str(pfkeys$, 8,1) = hex(ff)
L46940:    if textidl$ <> " " then L46960
                str(pf$(3),43,18) = " " : str(pfkeys$,17,1) = hex(ff)
L46960:    if curr$ <> "Y" then str(pfkeys$,18,1) = hex(ff)
           return

        REM *************************************************************~
            *             L O T   D I S T R I B U T I O N               *~
            *-----------------------------------------------------------*~
            * Display Line Item's Lot Distribution.                     *~
            *************************************************************

        deffn'122
            gosub setpf22
            hdr4$(1) = "Lot No."
            hdr4$(2) = "  Quantity"

L47110:     accept                                                       ~
               at (01,02),                                               ~
                  "A/R Trial Balance Inquiry: INVOICE LINE LOT DISTR.",  ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), "Part",                                       ~
               at (03,07), fac(hex(84)), part$                  , ch(25),~
               at (03,34), "Descr",                                      ~
               at (03,40), fac(hex(84)), descr$                 , ch(32),~
                                                                         ~
               at (05,02), fac(hex(ac)), hdr4$(1)               , ch(06),~
               at (05,10), fac(hex(ac)), hdr4$(2)               , ch(10),~
               at (05,42), fac(hex(ac)), hdr4$(1)               , ch(06),~
               at (05,50), fac(hex(ac)), hdr4$(2)               , ch(10),~
                                                                         ~
               at (06,02), fac(hex(84))  ,  lot$( 1)            , ch(18),~
               at (07,02), fac(hex(84))  ,  lot$( 2)            , ch(18),~
               at (08,02), fac(hex(84))  ,  lot$( 3)            , ch(18),~
               at (09,02), fac(hex(84))  ,  lot$( 4)            , ch(18),~
               at (10,02), fac(hex(84))  ,  lot$( 5)            , ch(18),~
               at (11,02), fac(hex(84))  ,  lot$( 6)            , ch(18),~
               at (12,02), fac(hex(84))  ,  lot$( 7)            , ch(18),~
               at (13,02), fac(hex(84))  ,  lot$( 8)            , ch(18),~
               at (14,02), fac(hex(84))  ,  lot$( 9)            , ch(18),~
               at (15,02), fac(hex(84))  ,  lot$(10)            , ch(18),~
               at (16,02), fac(hex(84))  ,  lot$(11)            , ch(18),~
               at (17,02), fac(hex(84))  ,  lot$(12)            , ch(18),~
               at (18,02), fac(hex(84))  ,  lot$(13)            , ch(18),~
               at (19,02), fac(hex(84))  ,  lot$(14)            , ch(18),~
               at (20,02), fac(hex(84))  ,  lot$(15)            , ch(18),~
                                                                         ~
               at (06,42), fac(hex(84))  ,  lot$(16)            , ch(18),~
               at (07,42), fac(hex(84))  ,  lot$(17)            , ch(18),~
               at (08,42), fac(hex(84))  ,  lot$(18)            , ch(18),~
               at (09,42), fac(hex(84))  ,  lot$(19)            , ch(18),~
               at (10,42), fac(hex(84))  ,  lot$(20)            , ch(18),~
               at (11,42), fac(hex(84))  ,  lot$(21)            , ch(18),~
               at (12,42), fac(hex(84))  ,  lot$(22)            , ch(18),~
               at (13,42), fac(hex(84))  ,  lot$(23)            , ch(18),~
               at (14,42), fac(hex(84))  ,  lot$(24)            , ch(18),~
               at (15,42), fac(hex(84))  ,  lot$(25)            , ch(18),~
               at (16,42), fac(hex(84))  ,  lot$(26)            , ch(18),~
               at (17,42), fac(hex(84))  ,  lot$(27)            , ch(18),~
               at (18,42), fac(hex(84))  ,  lot$(28)            , ch(18),~
               at (19,42), fac(hex(84))  ,  lot$(29)            , ch(18),~
               at (20,42), fac(hex(84))  ,  lot$(30)            , ch(18),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(str(pfkeys$)), key(keyhit%)

               if keyhit% <> 13 then L47690
                  call "MANUAL" ("ARQINVSB")
                  goto L47110

L47690:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L47110

        setpf22
           inpmessage$ = " "
           pf$(1) = "                                                  "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "             (16)Return      "
           pfkeys$ = hex(ffffffffffffffffffffffff0dff0f10ffff20ff)
           return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'152(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L51150,         /* Ship-to Range    */~
                                    L51210,         /* Invoice Range    */~
                                    L51270,         /* From Post Date   */~
                                    L51360,         /* Selected Types   */~
                                    L51410          /* Print Text?      */
                  return

L51150
*        Ship-to Range                         CUS_RANGE$
            call "TESTRNGE" (cus_range$(1), cus_range$(2),               ~
                             cus_range$(3), cus_range$(4),               ~
                             errormsg$)
                return

L51210
*        Invoice Range                         INV_RANGE$()
            call "TESTRNGE" (inv_range$(1), inv_range$(2),               ~
                             inv_range$(3), inv_range$(4),               ~
                             errormsg$)
            return

L51270
*        From Post Date                        FROM_DATE$
            call "DATEOK" (from_date$, u3%, errormsg$)
            if errormsg$ <> " " then return
                call "DATUNFMT" (from_date$) : from_dateu$ = from_date$
                if from_date$ > asofu$ then                              ~
                     errormsg$ = "From Date must be on or before As Of"
                call "DATEFMT" (from_date$)
                return

L51360
*        Selected Types                        TYPES$
            if types$ <> " " then return
                errormsg$ = "At least one Type must be selected"
                return

L51410
*        Print Text?                           PRINT_TEXT$
            if print_text$= "Y" or print_text$ = "N" then return
                errormsg$ = "Enter 'Y' or 'N'."
                return

        lookup_job_name                           /* (EWD002)-BCKMASTR */
          init(" ") bck_key$, bck_job$
          str(bck_key$,1%,9%)   = shipto$
          str(bck_key$,10%,16%) = so$
          read #16,key = bck_key$, using L60000, bck_job$, eod goto L60010
L60000:      FMT POS(619), CH(20)
L60010:   return                                  /* (EWD002) - End    */


        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            if billto% = 0% then billto$ = onlyshipto$
            end
