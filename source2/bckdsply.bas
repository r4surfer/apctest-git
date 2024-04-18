        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  BBBB    CCC   K   K  DDDD    SSS   PPPP   L      Y   Y   *~
            *  B   B  C      K  K   D   D  S      P   P  L       Y Y    *~
            *  BBBB   C      K K    D   D   SSS   PPPP   L        Y     *~
            *  B   B  C      K  K   D   D      S  P      L        Y     *~
            *  BBBB    CCC   K   K  DDDD    SSS   P      LLLLL    Y     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BCKDSPLY - Sales Order Inquiry.                           *~
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
            * 07/30/87 ! Added Selective Summary Listing From Main!     *~
            *          ! Input Screen.                            ! DAW *~
            * 12/08/87 ! Link to SOSTATUS                         ! JDH *~
            * 01/18/88 ! Added History files                      ! JDH *~
            * 02/02/88 ! Added Order Status(open,closed,cancelled)! JDH *~
            * 08/22/88 ! Literal 'Changed' to 'Cancelled' if appro! JDH *~
            *          !  & 'RETURN' rather than PF10 for List    !     *~
            * 10/19/88 ! Added ability to find via SO# or PO#     ! JDH *~
            * 05/01/89 ! Added ability to get to DEMSTAT from here! JDH *~
            *          !  & changed screen literals qty shipped   !     *~
            *          !  & added sort by PO#, See ATC , See      !     *~
            *          !  Options, chngd 1st scrn to View via PO#s!     *~
            * 07/05/89 ! Added ability to display summary & line  ! MLJ *~
            *          !  items in statutory or transaction amts. !     *~
            * 02/05/90 ! Added BCKHCLNS to get history currency   ! JDH *~
            *          !  info, added call to SOSTATUS @ line item!     *~
            *          !  screens, added 'all amts in stat' to    !     *~
            *          !  order listing report, misc MC mods.     !     *~
            * 03/01/90 ! modified input screen to allow for 16    ! LAB *~
            *          ! character SO #'s.  modified screen header!     *~
            *          ! to reduce ship to name if line is to long!     *~
            * 05/16/90 ! Added 'Export' and 'Yet to Ship'. Trans- ! JDH *~
            *          !  action line ext now uses open qty.      !     *~
            * 05/18/90 ! Added ability to find via Order Date.    ! JDH *~
            * 06/22/90 ! Added toggle on Line Item Summary Screen ! JDH *~
            *          !  Original Order Qty to Current Order Qty.!     *~
            * 06/27/90 ! Transaction currency price now stocking. ! JDH *~
            * 05/22/91 ! PRR 10047, 11919 & 11961 Eliminate print !     *~
            *          !   of customers w/ no SOs in the range.   ! JIM *~
            * 07/08/91 ! PRRs 12025 & 11979. Corrected LI hdr prnt! JDH *~
            * 07/11/91 ! Fixed Rpt optn 'Include History' edit.   ! JDH *~
            * 09/25/91 ! PRRs 10577, 10657, 11859.  Added option  ! JDH *~
            *          !   to print Open and/or Closed orders.    !     *~
            * 01/23/92 ! New PLOWCODE allows to FIND by PO#.      ! KAB *~
            * 09/25/92 ! PRR 12116. Fixed status on Listing Rpt.  ! JDH *~
            *          ! PRR 12317. Fixed 'No Orders Found' test. !     *~
            * 11/24/92 ! PRR 12660. Added Part text to text prntng! JDH *~
            *          ! Added shipping status (Open or Closed).  !     *~
            *          ! PRR 12665. Rpt Qtys option Invc or Ship. !     *~
            * 12/11/92 ! PRR 12738. Cust Sort Name on line 2 now  ! JDH *~
            *          !   correct length in all cases.           !     *~
            * 06/28/93 ! Added PF(22)Pt Toggle on Line Summary to ! MLJ *~
            *          !   toggle between CMS part & Cust Part.   !     *~
            * 08/20/93 ! Changed 5th arg of call to TXTPRINT to   ! MLJ *~
            *          !   'BCK008'.  Detail report now honors    !     *~
            *          !   text printing.                         !     *~
            * 10/29/93 ! Added shipping priority to summary lines ! MLJ *~
            *          !   display.  PF(8) now toggles on Seq/Pty !     *~
            *          !   and orig/curr.                         !     *~
            * 03/07/94 ! Changed BOMSPEC record length in SELECT  ! WPH *~
            * 09/23/94 ! Added ARQCUSCR to hdr 1.                 ! JDH *~
            * 10/04/94 ! PRR 13285 - Calculate the Stocking Price ! RJH *~
            *          !   instead of using the less accurate file!     *~
            *          !   value.                                 !     *~
            * 03/13/95 ! PRR 13187- Use Xref Shadow file for XPart! RJH *~
            * 07/30/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            alloc$10, allocflag$1,       /* Allocation Qty & Type      */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            cancel$8,                    /* Cancel Date                */~
            cat$4, catdescr$30,          /* Part Category              */~
            changemsg$22,                /* Change or Cancelled Date   */~
            comm%(3), comm$(3)3,         /* Split Commissions          */~
            company$60,                  /* Company Name               */~
            curr$1,                      /* Multi-currency flag        */~
            curr_code$4,                 /* Sales Order Currency Code  */~
            curcode$4,                   /* Currency Code  - Display   */~
            curdescr$30,                 /* Currency Descr - Display   */~
            curlabel$11,                 /* Currency Label - Display   */~
            cfac1$1,                     /* Currency Display Label FAC */~
            cfac2$1, cfac3$1,            /* Currency Display Code  FAC */~
            sodatef$8,                   /* Compare SO_DATE$ Unformt   */~
            conv$10,                     /* UOM Conversion Factor      */~
            co_range$(4)9,               /* Customer Number Range      */~
            crhold$4,                    /* Credit Hold Flag           */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            demtype$1, demprty$1,        /* Demand Type & Priority     */~
            descr$32,                    /* Part/Line Description      */~
            dfac$(30)1,                  /* Display FACs               */~
            discacct$12,                 /* Header level Discs Acct    */~
            discacctdescr$30,            /*                            */~
            discs$12, discsdescr$30,     /* Sales Discs Acct- Lines    */~
            disp$(14)79,                 /* Screen Display Array       */~
            duedate$8,                   /* Current Due Date           */~
            entered$8,                   /* Order Entry Date           */~
            errormsg$79,                 /* Error message              */~
            exchg_msg$35,                /* Exchg Rate - info message  */~
            exchg_rate$10,               /* Exchg Rate - TRAN/STAT     */~
            export_msg$6,                /* Is it an export order?     */~
            finddate$10,                 /* Find Arg for Date Formatted*/~
            findso$16,                   /* Find Argument              */~
            findpo$16,                   /* Find Argument for PO#      */~
            fob$20,                      /* FOB Description            */~
            hdr1$(8)16,                  /* Screen Column Headings     */~
            hdr3$(5)27,                  /*                            */~
            hist$1,                      /* Including/Excluding Flag   */~
            historymsg$18,               /* Including/Excluding Msg    */~
            histrptmsg$13,               /* Hist or Current Files Msg  */~
            howrptmsg$20,                /* Open or Closed Orders Msg  */~
            howship$20,                  /* How Ship Instructions      */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            item$3,                      /* PO Line ID                 */~
            lastdate$8,                  /* Last Date for Date sort    */~
            lastinv$8,                   /* Last Invoice - Line Level  */~
            lastmod$8,                   /* Last Modified Date         */~
            lastso$16,                   /* Last Order   / Ship-to     */~
            lastpo$16,                   /* Last PO                    */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            linediscamt$10,              /* Line Discount Amount       */~
            linediscpct$10,              /* Line Discount Percent      */~
            lineext$10,                  /* Line Extension             */~
            linekey$50,                  /* Line Plow Key              */~
            lines$(100,4)79,             /* Line Summary Display       */~
            lot$6,                       /* Lot Distribution           */~
            od_range$(4)8,               /* Order Date Range           */~
            openqty$10,                  /* Sales Order Left Open      */~
            order$10,                    /* Sales Order Qty            */~
            origdue$8,                   /* Original Due Date          */~
            part$25,                     /* Part Number                */~
            pc$, pcdescr$30,             /* Price Code Info            */~
            pf$(3)79, pfkeys$32,         /* PF Literals and Keys       */~
            plo$132,                     /* Print Line Out             */~
            po$16,                       /* PO Number                  */~
            plowcur$25,                  /* Miscellaneous Read/Plow Key*/~
            plowhis$25,                  /* Miscellaneous Read/Plow Key*/~
            plowhdr$(2)39,               /* Plowscreen headers         */~
            plowkey$50,                  /* Miscellaneous Read/Plow Key*/~
            preinv$10,                   /* Pre-invoiced Quantity      */~
            price$10,                    /* Unit Price                 */~
            priceuom$4,                  /* Price UOM                  */~
            pricestk$10,                 /* Price at Stkng UOM         */~
            print_how$1,                 /* Print Open, Closed, or Both*/~
            print_text$1,                /* Print Text on Order?       */~
            prstat$6,                    /* Order Status for printing  */~
            qty_based_on$1,              /* Open Qtys - Invc or ship   */~
            qty_based_msg$(2)9,          /* Based on msg for Listing   */~
            readcm$4,                    /* CURMASTR Read Key          */~
            readcurr$19,                 /* BCKLNCUR Read Key          */~
            readkey$50,                  /* Misc. Read Key             */~
            readkey1$50,                 /* New Misc. Read Key         */~
            refsource$4,                 /* Xref Part Source           */~
            reftype$(100)1,              /* Xref Part Source type      */~
            region$4, regiondescr$30,    /* Sales Region               */~
            report$15, rptid$6,          /* Report to Print            */~
            rpthdr$80,                   /* Report Heading Line        */~
            rptplow$50,                  /* Plow Key for Report        */~
            runtime$8,                   /* Report Run Time            */~
            sales$12, salesdescr$30,     /* Sales Distr Acct- Lines    */~
            salesacct$12,                /* Sales Distribution Account */~
            salesacctdescr$30,           /*                            */~
            salesman$(3)4,               /* Salesmen                   */~
            salesmandescr$(3)30,         /*                            */~
            save_co$16,                  /* Save this for Later        */~
            save_so$16,                  /* Save him fer later         */~
            schdl$10,                    /* Scheduled Qty              */~
            seq$3,                       /* Line Item Sequence Number  */~
            sfac$(3)1,                   /* Sort FAC                   */~
            ship$10,                     /* Quantity Shipped           */~
            shipcode$1,                  /* Shipping Priority Code     */~
            shipdate$8,                  /* Ship Date                  */~
            shipto$9, shiptoname$30,     /* Ship-to Number             */~
            shipto$(6)30,                /* Ship-to Address            */~
            ship_rnge$42,                /* Ship to Range to Print     */~
            shpinstr$(2)50,              /* Shipping Instructions      */~
            so$16,                       /* Sales Order Number         */~
            sodate$8,                    /* Order Date                 */~
            sorsn$9, sorsndescr$30,      /* Reason Code                */~
            so_range$38, so_range$(4)16, /* Order Range to print       */~
            soldto$(6)30,                /* Sold To Address            */~
            statmsg$13,                  /* Amounts in statutory msg   */~
            status$(2)9,                 /* Order Status - Inv & Ship  */~
            statutory$4,                 /* Statutory currency code    */~
            stat_curdescr$30,            /* Statutory Descr - Display  */~
            stat_linediscamt$10,         /* Line Disc -  STATUTORY     */~
            stat_lineext$10,             /* Line Ext  -  STATUTORY     */~
            stat_price$10,               /* Unit Price - STATUTORY     */~
            statrptmsg$30,               /* Amounts in statutory msg   */~
            stkuom$4,                    /* Stocking UOM               */~
            str$3, strdescr$30,          /* Store                      */~
            taxable$7,                   /* Line Taxable Flag          */~
            temp$16,                     /* Misc. Temp variable        */~
            terms$20,                    /* Payment Terms Code         */~
            text$(113,1)70,              /* Text Sizing Array          */~
            textid$4, textidl$4,         /* Text Hdr/Line IDs          */~
                      textidp$4,         /* Text Part ID               */~
            to_ship$10,                  /* Remaining to Ship          */~
            to_ship_ext$10,              /* Remaining to Ship Extention*/~
            tran_curdescr$30,            /* Trans Descr  - Display     */~
            tran_descr$32,               /* Trans Descr with parens    */~
            tran_display$1,              /* Display in TRANSACTION ?   */~
            tran_linediscamt$10,         /* Line Disc -  TRANSACTION   */~
            tran_lineext$10,             /* Line Ext  -  TRANSACTION   */~
            tran_price$10,               /* Unit Price - TRANSACTION   */~
            userin$3, usermod$3,         /* User ID                    */~
            vf$200,                      /* Variable Fields Data       */~
            workdate$8                   /* Date for workfile          */

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20,                 /* Text from file opening     */~
            hist%(14)                    /* FAC History Dim/Current Bri*/~

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
            * # 3 ! CUSTOMER ! Customer Master File                     *~
            * # 4 ! GLMAIN   ! General Ledger Chart Of Accounts File.   *~
            * # 5 ! BCKMASTR ! Sales Order Master- Headers              *~
            * # 6 ! BCKLINES ! Sales Order Master- Lines                *~
            * # 7 ! CATEGORY ! Inventory Category Codes File            *~
            * # 8 ! GENCODES ! System General Codes file.               *~
            * # 9 ! BCKHMSTR ! SO History Master file - Headers         *~
            * #10 ! BCKHLNES ! SO History Lines Detail file             *~
            * #11 ! SLMMASTR ! Salesman master file                     *~
            * #12 ! STORNAME ! Store Master File                        *~
            * #13 ! JOBMASTR ! Project Master File.                     *~
            * #14 ! TXTFILE  ! System Text File                         *~
            * #16 ! SHPLINES ! Shipment Schedule- Lines                 *~
            * #17 ! SHPHDRS  ! Shipment Scheduling / Pre-Invoicing- Hea *~
            * #18 ! ARIMASTR ! Invoice Master- Headers                  *~
            * #19 ! ARILINES ! Invoice Master- Line Items               *~
            * #21 ! PIPCROSS ! hard peg cross reference                 *~
            * #22 ! DEMMASTR ! Demand Master File                       *~
            * #23 ! JBMASTR2 ! Production job master file               *~
            * #24 ! JBSTATUS ! Production job actual structure (RTE) ac *~
            * #25 ! PIPOUT   ! Planned inventory use detail rec         *~
            * #26 ! JBCROSS2 ! Cross reference of RTE & BOM planned for *~
            * #27 ! VBKMASTR ! PURCHASE ORDER HEADER FILE               *~
            * #28 ! VBKLINES ! Purchase Order Line Items File           *~
            * #29 ! JBCREDIT ! Production job credits received detail f *~
            * #30 ! RCVLINES ! Receiver Line Items File  (Purchasing)   *~
            * #31 ! PAYMASTR ! PAYABLES MAIN FILE  (INVOICE DATES)      *~
            * #32 ! PAYLINES ! PAYABLES LINE ITEM FILE                  *~
            * #33 ! HNYMASTR ! Inventory Master File                    *~
            * #34 ! WCMASTR  ! Workcenter Master File                   *~
            * #35 ! RTEMASTR ! Production routing master file           *~
            * #36 ! BOMSPEC  ! OPTIONS Selected File  for Option disp.  *~
            * #38 ! SFCUM2   ! CUMULATIVE FORECAST - Shop Floor         *~
            * #39 ! PIPMASTR ! PLANNED INV MASTER FILE                  *~
            * #40 ! CALMASTR ! CALENDER PLANNING CALENDER               *~
            * #41 ! PIPIN    ! EXPECTED ADDITIONS TO INVENTORY          *~
            * #42 ! HNYDETAL ! INVENTORY DETAIL RECORDS                 *~
            * #45 ! BCKLNCUR ! Currency Specific BCKLINES               *~
            * #46 ! CURMASTR ! Multi-Currency Master                    *~
            * #47 ! BCKHCLNS ! BCK History Currency Line File           *~
            * #50 ! WORKFILE ! Workfile to sort by PO #                 *~
            * #51 ! WORKFIL2 ! Workfile to sort by Date                 *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 2, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select #3,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select # 4, "GLMAIN",                                        ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   9                      ~

            select #5,  "BCKMASTR",                                      ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #6,  "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19

            select # 7, "CATEGORY",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    1, keylen =   4                      ~

            select # 8, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24                      ~

            select #9,  "BCKHMSTR",                                      ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #10, "BCKHLNES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19

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

            select #16, "SHPLINES",                                      ~
                        varc,     indexed,  recsize = 600,               ~
                        keypos =   10, keylen =  22

            select #17, "SHPHDRS",                                       ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =  1,   keylen = 28

            select #18, "ARIMASTR",                                      ~
                        varc,     indexed,  recsize = 2000,              ~
                        keypos =    1, keylen =  17,                     ~
                        alt key  1, keypos =   10, keylen =   8,         ~
                            key  2, keypos =   18, keylen =  16, dup,    ~
                            key  3, keypos =   34, keylen =  16, dup

            select #19, "ARILINES",                                      ~
                        varc,     indexed,  recsize =  750,              ~
                        keypos =    1, keylen =  20

            select #21, "PIPCROSS",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  71,                     ~
                        alt key  1, keypos =   20, keylen =  52,         ~
                            key  2, keypos =   39, keylen =  33          ~

            select #22, "DEMMASTR",                                      ~
                        varc,     indexed,  recsize =  123,              ~
                        keypos =    2, keylen =  27,                     ~
                        alt key  1, keypos =   10, keylen =  19,         ~
                            key  2, keypos =    1, keylen =  28          ~

            select #23, "JBMASTR2",                                      ~
                        varc,     indexed,  recsize = 1300,              ~
                        keypos =    1, keylen =   8                      ~

            select #24, "JBSTATUS",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    1, keylen =  12,                     ~
                        alt key  1, keypos =   21, keylen =  44,         ~
                            key  2, keypos =   29, keylen =  36          ~

            select #25, "PIPOUT",                                        ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =   20, keylen =  37          ~

            select #26, "JBCROSS2",                                      ~
                        varc,     indexed,  recsize =   94,              ~
                        keypos =   29, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  47,         ~
                            key  2, keypos =   48, keylen =  47          ~

            select #27, "VBKMASTR",                                      ~
                        varc,     indexed,  recsize = 1030,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   10, keylen =  16          ~

            select #28, "VBKLINES",                                      ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos =    1, keylen =  28                      ~

            select #29, "JBCREDIT",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  22,                     ~
                        alt key  1, keypos =   23, keylen =  48          ~

            select #30, "RCVLINES",                                      ~
                        varc,     indexed,  recsize =  800,              ~
                        keypos =   26, keylen =  52,                     ~
                        alt key  1, keypos =    1, keylen =  69,         ~
                            key  2, keypos =   42, keylen =  36,         ~
                            key  3, keypos =  128, keylen =  24          ~

            select #31, "PAYMASTR",                                      ~
                        varc,     indexed,  recsize =  350,              ~
                        keypos =    1, keylen =  25                      ~

            select #32, "PAYLINES",                                      ~
                        varc,     indexed,  recsize =  541,              ~
                        keypos =   36, keylen =  28,                     ~
                        alt key  1, keypos =    1, keylen =  63,         ~
                            key  2, keypos =   17, keylen =  47          ~

            select #33, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  3, keypos =   26, keylen =  32, dup     ~

            select #34, "WCMASTR",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    2, keylen =   5,                     ~
                        alt key  1, keypos =    1, keylen =   6          ~

            select #35, "RTEMASTR",                                      ~
                        varc,     indexed,  recsize =  400,              ~
                        keypos =    5, keylen =  31,                     ~
                        alt key  1, keypos =    1, keylen =  35          ~

            select #36, "BOMSPEC",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos = 26, keylen = 54,                        ~
                        alt key 1, keypos = 57, keylen = 23

            select #38, "SFCUM2",                                        ~
                        varc, indexed, recsize = 1985,                   ~
                        keypos = 1,  keylen = 25                         ~

            select #39, "PIPMASTR",                                      ~
                        varc, indexed, recsize = 2024,                   ~
                        keypos = 2,  keylen = 25,                        ~
                alt key 1, keypos = 1, keylen = 26                       ~

            select #40, "CALMASTR",                                      ~
                        varc, indexed, recsize = 1962,                   ~
                        keypos = 1,  keylen = 2                          ~

            select #41, "PIPIN",                                         ~
                        varc, indexed, recsize =   60,                   ~
                        keypos = 30, keylen = 19,                        ~
                alt key 1, keypos = 1, keylen = 48                       ~

            select #42, "HNYDETAL",                                      ~
                        varc, indexed, recsize =  150,                   ~
                        keypos = 1,  keylen = 42,                        ~
                alt key 1, keypos = 43, keylen = 6, dup,                 ~
                    key 2, keypos =  49, keylen = 2, dup                 ~

            select #45, "BCKLNCUR",                                      ~
                        varc, indexed, recsize = 100,                    ~
                        keypos = 5,  keylen = 19,                        ~
                alt key 1, keypos = 1, keylen = 23                       ~

            select #46, "CURMASTR"                                       ~
                        varc, indexed, recsize = 256,                    ~
                        keypos = 1,  keylen = 4

            select #47, "BCKHCLNS",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    5, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  23          ~

            select #50, "WORKFILE",                                      ~
                        varc, indexed, recsize =   32,                   ~
                        keypos = 1,  keylen = 32

            select #51, "WORKFIL2",                                      ~
                        varc, indexed, recsize =   22,                   ~
                        keypos = 1,  keylen = 22

            call "SHOSTAT" ("Opening files, one moment please")
            call "OPENCHCK" (# 2, fs%( 2), f2%( 2), 0%, rslt$( 2))
            call "OPENCHCK" (# 3, fs%( 3), f2%( 3), 0%, rslt$( 3))
            call "OPENCHCK" (# 4, fs%( 4), f2%( 4), 0%, rslt$( 4))
            call "OPENCHCK" (# 5, fs%( 5), f2%( 5), 0%, rslt$( 5))
            call "OPENCHCK" (# 6, fs%( 6), f2%( 6), 0%, rslt$( 6))
            call "OPENCHCK" (# 7, fs%( 7), f2%( 7), 0%, rslt$( 7))
            call "OPENCHCK" (# 8, fs%( 8), f2%( 8), 0%, rslt$( 8))
            call "OPENCHCK" (# 9, fs%( 9), f2%( 9), 0%, rslt$( 9))
            call "OPENCHCK" (#10, fs%(10), f2%(10), 0%, rslt$(10))
            call "OPENCHCK" (#11, fs%(11), f2%(11), 0%, rslt$(11))
            call "OPENCHCK" (#12, fs%(12), f2%(12), 0%, rslt$(12))
            call "OPENCHCK" (#13, fs%(13), f2%(13), 0%, rslt$(13))
            call "OPENCHCK" (#14, fs%(14), f2%(14), 0%, rslt$(14))
            call "OPENCHCK" (#16, fs%(16), f2%(16), 0%, rslt$(16))
            call "OPENCHCK" (#17, fs%(17), f2%(17), 0%, rslt$(17))
            call "OPENCHCK" (#18, fs%(18), f2%(18), 0%, rslt$(18))
            call "OPENCHCK" (#19, fs%(19), f2%(19), 0%, rslt$(19))
            call "OPENCHCK" (#21, fs%(21), f2%(21), 0%, rslt$(21))
            call "OPENCHCK" (#22, fs%(22), f2%(22), 0%, rslt$(22))
            call "OPENCHCK" (#23, fs%(23), f2%(23), 0%, rslt$(23))
            call "OPENCHCK" (#24, fs%(24), f2%(24), 0%, rslt$(24))
            call "OPENCHCK" (#25, fs%(25), f2%(25), 0%, rslt$(25))
            call "OPENCHCK" (#26, fs%(26), f2%(26), 0%, rslt$(26))
            call "OPENCHCK" (#27, fs%(27), f2%(27), 0%, rslt$(27))
            call "OPENCHCK" (#28, fs%(28), f2%(28), 0%, rslt$(28))
            call "OPENCHCK" (#29, fs%(29), f2%(29), 0%, rslt$(29))
            call "OPENCHCK" (#30, fs%(30), f2%(30), 0%, rslt$(30))
            call "OPENCHCK" (#31, fs%(31), f2%(31), 0%, rslt$(31))
            call "OPENCHCK" (#32, fs%(32), f2%(32), 0%, rslt$(32))
            call "OPENCHCK" (#33, fs%(33), f2%(33), 0%, rslt$(33))
            call "OPENCHCK" (#34, fs%(34), f2%(34), 0%, rslt$(34))
            call "OPENCHCK" (#35, fs%(35), f2%(35), 0%, rslt$(35))
            call "OPENCHCK" (#36, fs%(36), f2%(36), 0%, rslt$(36))
            call "OPENCHCK" (#38, fs%(38), f2%(38), 0%, rslt$(38))
            call "OPENCHCK" (#39, fs%(39), f2%(39), 0%, rslt$(39))
            call "OPENCHCK" (#40, fs%(40), f2%(40), 0%, rslt$(40))
            call "OPENCHCK" (#41, fs%(41), f2%(41), 0%, rslt$(41))
            call "OPENCHCK" (#42, fs%(42), f2%(42), 0%, rslt$(42))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ =  " "
            call "DATUFMTC" (blankdate$)

*        Perform start-up tasks required regardless of tasks
            date$ = date : call "DATEFMT" (date$)
            str(line2$,62%) = "BCKDSPLY: " & str(cms2v$,,8%)
            sopt% = 0%  /* Flag to Control INPUT/EDIT on Params   */
            no%   = 0%  /* Flag to Control Valid Data on Report   */
            xinflag% , hflag% = 0% /* Exclude/include & History file */
            manyprt%   = 0%        /* 1% = Coming from Range Screen   */
            workflag1% = 0%        /* 1% = PO Workfile has been built */
            workflag2% = 0%        /* 1% = Date Workfile is built     */
            po_flag%   = 0%        /* Screen order: 1% = PO Order     */
            date_flag% = 0%        /* Screen order: 1% = Date Order   */
            tgl%       = 1%        /* Start showing Current Order Qty */
            refflag% = 0%          /* Cust Part Numbers Present       */

*        Check for Multi-currency usage, get Statutory description
            curr$ = "N"
            tran_display$ = "N" /* Start out displaying statutory */
            statutory$, stat_curdescr$, statmsg$, statrptmsg$ = " "
            call "READ100" (#2, "SWITCHS.CUR", f1%(2))
            if f1%(2) = 0% then L10000
                get #2 using L09220, curr$, statutory$
L09220:             FMT POS(21), CH(1), CH(4)
            if curr$ = "N" then statutory$ = " "
            if curr$ = "N" then L10000
            call "OPENCHCK" (#45, fs%(45), f2%(45), 0%, rslt$(45))
            call "OPENCHCK" (#46, fs%(46), f2%(46), 0%, rslt$(46))
            call "OPENCHCK" (#47, fs%(47), f2%(47), 0%, rslt$(47))
            statrptmsg$ = "-- ALL AMOUNTS IN STATUTORY --"
            statmsg$ = "- Statutory -"
            readcm$ = statutory$
            call "READ100" (#46, readcm$, f1%(46))
            if f1%(46) = 0% then L10000
                get #46 using L09300, stat_curdescr$
L09300:              FMT POS(5), CH(30)

L10000: REM *************************************************************~
            * S E L E C T   S H I P  -  T O  /  M A I N    S C R E E N  *~
            *-----------------------------------------------------------*~
            * Get Ship-to ad Display List of Orders on file.            *~
            *************************************************************

        select_shipto
            sopt%, tgl% = 1%
            po_flag%, date_flag%, refflag% = 0%
            if workflag1% = 0% then L10066
                call "FILEBGON" (#50) : workflag1% = 0%
L10066:     if workflag2% = 0% then L10070
                call "FILEBGON" (#51) : workflag2% = 0%
L10070:     gosub'100(1%)                /* Get Ship to for Display    */
            errormsg$ = " "
                if keyhit%  =  5% then next_shipto
                if keyhit%  =  8% then find_cus_via_so
                if keyhit%  =  9% then find_cus_via_po
                if keyhit%  = 12% then print_listing
                if keyhit%  = 14% then print_orders
                if keyhit%  = 16% then exit_program
                if keyhit%  = 32% then exit_program
            gosub'150(1%)      /* Test Data for the Items on Screen 1  */
                if errormsg$ <> " " then select_shipto
                save_co$     = shipto$
                goto first_screen

        find_cus_via_so
            xinflag% = 0% : findso$ = hex(010203)
            plowhdr$(2%) = "  SO Number          Customer Code"
            readkey$ = hex(0684)& "Select Sales Order, or PF16 To Return"
            call "PLOWCODE" (#6, findso$, readkey$,-3016%,-0.09, f1%(6), ~
                                 plowhdr$(), 0, 1)
            findso$ = " " : if f1%(6) = 0% then select_shipto
            get #6, shipto$, lastso$ : lastso$ = lastso$ addc all(hex(ff))
               save_co$ = shipto$ : goto next_screen

        find_cus_via_po
            xinflag% = 0% : findso$ = hex(2021)
            plowhdr$(1%) = "  PO Number          Customer SO Number"
            readkey$ = hex(0684) & "Select Purchase Order, or" &         ~
                                   "  PF16 To Return"
            call "PLOWCODE" (#5, findso$, readkey$, 3000%, 1.25, f1%(5), ~
                                 plowhdr$(), 0, 1)
            findso$ = " " : if f1%(5) = 0% then select_shipto
*          FINDSO$ = " " : GOTO SELECT_SHIPTO
            get #5, shipto$, lastso$ : lastso$ = lastso$ addc all(hex(ff))
               save_co$ = shipto$ : goto next_screen

        next_shipto       /* Find and Load Next Bill-to on File        */
            errormsg$   = " "
            saveshipto$ = shipto$
L10190:     call "PLOWNEXT" (#3, shipto$, 0%, f1%(3))
            if f1%(3) = 1% then L10250
                errormsg$ = "End of Customer File Reached."
                shipto$   = saveshipto$
L10230:         call "DESCRIBE" (#3, shipto$, shiptoname$, 0%, f1%(3))
                goto select_shipto
L10250:     plowkey$ = str(shipto$) & hex(00)
            call "PLOWNEXT" (#5, plowkey$, 9%, f1%(5))
            if f1%(5) = 1% then L10230

            if xinflag% = 0% then L10190
            plowkey$ = str(shipto$) & hex(00)
            call "PLOWNEXT" (#9, plowkey$, 9%, f1%(9))
            if f1%(9) = 1% then L10230 else L10190

        main_screen
            tgl% = 1%  :  refflag% = 0%
            shipto$ = save_co$
            call "DESCRIBE" (#3, shipto$, shiptoname$, 0%, f1%(3))
            co_range$(1%) = shipto$ : co_range$(2%) = " "
            sopt% = 0%
            gosub'101(0%)      /* Sales Order Listing Screen           */
            errormsg$ = " "
                if keyhit%  =  0% then show_order
                if keyhit%  =  2% then first_screen
                if keyhit%  =  5% then next_screen
                if keyhit%  =  8% then find_order
                if keyhit%  =  9% then find_po
                if keyhit%  = 10% then find_date
                if keyhit%  = 12% then print_listing
                if keyhit%  = 14% then print_orders
                if keyhit%  = 16% then select_shipto
                if keyhit%  = 32% then exit_program
                if keyhit%  = 26% then gosub show_cust_credit
                goto main_screen

        first_screen
            lastso$, lastpo$, lastdate$ = hex(00)
            first%   = 1%
            goto L10490
        next_screen
            first% = 0%
L10490:     gosub load_summary
            goto  main_screen

        find_order
            findso$ = " "
            gosub'101(1%)           /* Sales Order Listing Screen   */
                errormsg$ = " "
                if keyhit%  <> 1% then L10570
                findso$ = " "
                goto main_screen
L10570:              lastso$ = findso$ addc all(hex(ff))
                     findso$ = " "
                     po_flag%, date_flag% = 0%
                     goto next_screen

        find_po
            findpo$ = " "
            gosub'101(2%)           /* Sales Order Listing Screen   */
                errormsg$ = " "
                if keyhit%  <> 1% then L10660
                findpo$ = " "
                goto main_screen
L10660:              lastpo$ = findpo$ addc all(hex(ff))
                     findpo$, lastso$ = " "
                     po_flag% = 1% : date_flag% = 0%
                     goto next_screen

        find_date
            finddate$ = "19010101"
            call "DATFMTC" ( finddate$ )
L10730:     gosub'101(3%)           /* Sales Order Listing Screen   */
                errormsg$ = " "
                if keyhit%  <> 1% then L10760
                finddate$ = " "
                goto main_screen
L10760:              call "DATEOKC" (finddate$, u3%, errormsg$)
                     if errormsg$ <> " " then L10730
                     call "DATUFMTC" (finddate$)
                     lastdate$ = finddate$ addc all(hex(ff))
                     finddate$, lastso$ = " "
                     po_flag% = 0% : date_flag% = 1%
                     goto next_screen

        REM *************************************************************~
            *             R E P O R T    P R I N T I N G                *~
            *-----------------------------------------------------------*~
            * Get Report Parameters for both Listing and Order Detail.  *~
            *************************************************************

        print_orders
            report$ = "ORDER DETAIL"
            rptid$  = "BCK008"
            goto L11161

        print_listing
            report$ = "ORDER LISTING"
            rptid$  = "BCK007"

L11161:     od_range$(1) = "ALL"  :  od_range$(2) = " "
            so_range$(1) = "ALL"  :  so_range$(2) = " "                  ~
                : gosub'152(2%)   /* Validate Range w/SO$              */
            print_text$ = "N" : hist$ = "N"
            qty_based_on$ = "I" : gosub'152(7%) /* Pick up Print Msgs */
            print_how$ = "B"  : howrptmsg$ = "Open & Closed Orders"
            if xinflag% = 1% then hist$ = "Y"
            call "DESCRIBE" (#3, shipto$, shiptoname$, 0%, f1%(3))
            if shipto$=" " then co_range$(1)="ALL" else                  ~
                co_range$(1) = shipto$
                co_range$(2) = " ": gosub'152(1%)
            if sopt% = 0% then L11200
*        Input Mode Logic for Report Parameters
            for fieldnr% = 1% to 7%
                gosub'052(fieldnr%)  /* Set Input Messages         */
L11192:         gosub'102(fieldnr%)  /* Get Report Parameters      */
                    if keyhit%  =  1% then select_shipto
                    if keyhit% <>  0% then L11192
                gosub'152(fieldnr%)  /* Test Range Input Data      */
                    if errormsg$ <> " " then L11192
            next fieldnr%

*        Edit Mode Logic for Report Parameters
L11200:     inpmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
            lastfieldnr% = 0%
            gosub'102(0%)             /* Report Parameters Screen      */
                  if keyhit%  =  1% then L11640
                  if keyhit%  = 16% then print_report
                  if keyhit%  = 32% then exit_program
                  if keyhit% <>  0% then L11200
L11280:     fieldnr% = cursor%(1) - 5
                if fieldnr% < 1% or fieldnr% > 7% then L11200
                if fieldnr%  = lastfieldnr% then L11200
            gosub'052(fieldnr%)          /* Set Input Messages         */
L11320:     gosub'102(fieldnr%)          /* Get Report Parameters      */
                  if keyhit%  =  1% then select_shipto
                  if keyhit% <>  0% then L11320
            gosub'152(fieldnr%)          /* Test Range Input Data      */
                  if errormsg$ <> " " then L11320
                     lastfieldnr% = fieldnr%
                     goto L11280

        print_report
          if hist$ = "Y" then L11627
            mc% = 5% : lc% = 6% : cc% = 45% : manyprt% = 1%
            goto L11630
L11627:     mc% = 9% : lc% = 10% : cc% = 47% : manyprt% = 1%
L11630:     gosub report_control
L11640:     if sopt% = 0% then main_screen
            goto select_shipto

        REM *************************************************************~
            *             I N V O I C E   D I S P L A Y                 *~
            *-----------------------------------------------------------*~
            * Display Order Detail.                                     *~
            *************************************************************

        show_order   /* Order Detail display from Main Screen        */
            i% = cursor%(1) - 5%
            if i% < 1% or i% > disp% then main_screen
            so$ = str(disp$(i%),,16)
              if hist%(i%) = 1% then L12065
              mc% = 5% : lc% = 6% : cc% = 45% : hflag% = 0%
              goto L12070
L12065:       mc% = 9% : lc% = 10% : cc% = 47% : hflag% = 1%
L12070:     gosub load_order
            if soonfile% = 0% then main_screen
                gosub display_order
                if keyhit% = 32% then select_shipto else main_screen


        display_order
            inpmessage$, errormsg$ = " "

        REM THE FOLLOWING CODE REDUCES THE LENGTH OF SHIPTONAME$ TO
        REM PREVENT THE LINE FROM BECOMING TO LONG
            x%   = len(shipto$) + len(so$)
            hld% = min(43% - x%, 30%)
            hld% = min(len(shiptoname$), hld%)

            str(line2$,,61%) = "Ship-to: " & shipto$ & "(" &             ~
                               str(shiptoname$,,hld%) & ") SO: " & so$
            t% = 0%

            if openflag% = 0% then status$(1%) = "CLOSED"                ~
                              else status$(1%) = " OPEN"
            if crhold$ = "C"  then status$(1%) = "CANCELLED"
            if crhold$ = "H"  then status$(1%) = "CR HOLD"
            if shipflag% = 0% then status$(2%) = "CLOSED"                ~
                              else status$(2%) = " OPEN"
            if crhold$ = "C"  then status$(2%) = "CANCLD"
            if crhold$ = "H"  then status$(2%) = "CR HOLD"

        detail_hdr1
            gosub'111
                if keyhit%  = 16% then       return
                if keyhit%  =  2% then gosub line_summary
                if keyhit%  =  5% then       detail_hdr2
                if keyhit%  =  9% then gosub variable_fields
                if keyhit%  = 14% then gosub print_order
                if keyhit%  = 25% then gosub display_header_text
                if keyhit%  = 26% then gosub show_cust_credit
                if keyhit%  = 32% then       return
                goto detail_hdr1

        detail_hdr2
            gosub'112
                if keyhit%  = 16% then       return
                if keyhit%  =  2% then gosub line_summary
                if keyhit%  =  4% then       detail_hdr1
                if keyhit%  =  9% then gosub variable_fields
                if keyhit%  = 14% then gosub print_order
                if keyhit%  = 25% then gosub display_header_text
                if keyhit%  = 26% then gosub show_cust_credit
                if keyhit%  = 32% then       return
                goto detail_hdr2

        variable_fields
            call "VFINPSUB" ("BCKMASTR", "D",                            ~
                             "Sales Order Inquiry: VARIABLE FIELDS",     ~
                             line2$, "NN", vf$, keyhit%)
            if keyhit% = 1% then keyhit% = 32% else keyhit% = 99%
            return

        display_header_text
            call "TXTDSPLY" (#14, f2%(14), "013", line2$, textid$,       ~
                                                                text$())
            return

        show_cust_credit
            call "ARQCUSCR" (shipto$)
            return


        line_summary
            sum_or_det% = 0%  /* Coming from Summary or Detail */
            inpmessage$ = "Position Cursor and Press RETURN to see" &    ~
                          " Line Item Detail."
            gosub'120     /* Order Listing - Line Item Summary Screen  */
              inpmessage$ = " "
              if keyhit% = 16% then       return
              if keyhit% =  2% then t% = 0%
              if keyhit% =  3% then t% = min(85%,maxlines%-12%)
              if keyhit% =  4% then t% = t%-12%
              if keyhit% =  5% then t% = min(85%, t%+12%, maxlines%-12%)
              if keyhit% =  6% then t% = t%-1%
              if keyhit% =  7% then t% = min(85%, t%+1%, maxlines%-1%)
                                   t% = max(0, t%)
              if keyhit% =  9% then gosub display_atc_or_options_or_status
              if keyhit% = 10% then gosub display_atc_or_options_or_status
              if keyhit% = 11% then gosub display_atc_or_options_or_status
              if keyhit% = 14% then gosub print_order
              if keyhit% = 32% then       return
              if keyhit% >  0% then line_summary
            c% = cursor%(1) - 4%
            if c% < 1% or c% > 15% then line_summary
                c%   = c% + t%
                seq$ = str(lines$(c%, 1),,3)
                if c% < 1% or c% > maxlines% then line_summary
                     gosub load_line_item
                     if lineonfile% = 0% then line_summary

        line_detail
            sum_or_det% = 1%  /* Coming from Summary or Detail */
            gosub'121     /* Display Line Item Detail                  */
             if keyhit%  = 16% then       line_summary
             if keyhit%  =  6% then gosub prev_line
             if keyhit%  =  7% then gosub next_line
             if keyhit%  =  9% then gosub display_atc_or_options_or_status
             if keyhit%  = 10% then gosub display_atc_or_options_or_status
             if keyhit%  = 11% then gosub display_atc_or_options_or_status
             if keyhit%  = 14% then gosub print_order
             if keyhit%  = 26% then gosub display_line_text
             if keyhit%  = 32% then return
                  goto line_detail

        prev_line
            if c% = 1% then return else c% = c% - 1%
            goto  L13110

        next_line
            if c% = maxlines% then return else c% = c% + 1%
L13110:     seq$  = str(lines$(c%, 1),,3)
            gosub load_line_item
            return


        display_line_text
            call "TXTDSPLY" (#14, f2%(14), "014", line2$, textidl$,      ~
                                                                text$())
            return


        print_order:
            report$ = "ORDER DETAIL"
            rptid$  = "BCK008"
            save_co$     = shipto$
            co_range$(1) = shipto$ : co_range$(2) = " " : gosub'152(1%)
            save_so$     = so$
            so_range$(1) =  so$  : so_range$(2) = " " : gosub'152(2%)
            od_range$(1) = "ALL" : od_range$(2) = " " : gosub'152(3%)
            print_text$ = "Y"
L13362:     u3% = 2%
            call "ASKUSER" (u3%, "*** PRINT OPTION ***",                 ~
                  "Press PF-1 to Abort Print,",                          ~
                  "PF-16 to Base Open Quantities on Shipping, or",       ~
                  "RETURN to Base Open Quantities on Invoicing")
            if u3% =  1% then return
            if u3% <> 0% and u3% <> 16% then L13362
                if u3% = 0% then qty_based_on$ = "I"                     ~
                            else qty_based_on$ = "S"
            gosub'152(7%)
            gosub report_control
            so$     = save_so$
            shipto$ = save_co$
            call "DESCRIBE" (#3, shipto$, shiptoname$, 0%, f1%(3))
            gosub load_order
            return

        display_atc_or_options_or_status
            if sum_or_det% = 1% then L13540
                c% = cursor%(1) - 4% + t%
                if c% < 1% and c% > maxlines% then return
L13540:     seq$  = str(lines$(c%, 1),, 3)
            part$ = str(lines$(c%, 1), 5, 25)
            if keyhit% = 11% then L13590
            if keyhit% = 10% then L13600

            call "PIPATCSB" (part$, #39, #33, #38, #40, #41, #25, #42,   ~
                                    #22, #21)
                return

L13590:     call "BOMOPDSP" (so$, seq$, part$, #33, #36)
                return

L13600:     call "SOSTATUS" (shipto$, so$, seq$, 0%, #6, #16, #17, #5,   ~
                                  #18, #19, #3, #9, #10, #14, 1%, hflag%,~
                                  #21, #22, #23, #24, #25, #26, #27, #28,~
                                  #29, #30, #31, #32, #33, #34, #35)
                return

        REM *************************************************************~
            *             P R I N T   L I S T I N G                     *~
            *-----------------------------------------------------------*~
            * Print Listing of Orders per Selection Criteria Entered.   *~
            *************************************************************

        listing_printing
            gtotal_order, gtotal_open, gsocount% = 0%
              if mc% = 9% then histrptmsg$ = "HISTORY FILES"             ~
                          else histrptmsg$ = "CURRENT FILES"
            readkey1$ = co_range$(3)
L14080:     str(readkey1$,10) = all (hex(ff))
            print_activity% = 0% /* 'No print activity this Customer' */
            call "PLOWNEXT" (#mc%, readkey1$, 0%, f1%(mc%))
            if f1%(mc%) = 0% then L14430  /* If not found               */
            if str(readkey1$,,9) > co_range$(4) then L14430 /* No More */
            rptplow$ = str(readkey1$,,9) & so_range$(3)
            total_order, total_open = 0%
            socount% = 0%
            shipto$ = str(readkey1$,,9)
            gosub listing_loop
            goto L14080

        listing_loop
L14190:     gosub next_order
            if f1%(mc%) = 0% then end_listing
            gosub valid_data : if no% <> 0% then L14190
            open% = 1%  /* Start as 1%; Conditional in next two lines */
            if qty_based_on$ = "I" and openflag% = 0% then open% = 0%
            if qty_based_on$ = "S" and shipflag% = 0% then open% = 0%
            if print_how$ = "O" and open% = 0% then L14190
            if print_how$ = "C" and open% > 0% then L14190

            print_activity% = 1% /* 'Print activity this Customer' */
            prstat$ = " OPEN"
            net_order   = gross_order + disc_order
            total_order = total_order + net_order
            if qty_based_on$ = "S" then L14250
                net_open = gross_open + disc_open /* Invoice Open Amt */
                if openflag% = 0% then prstat$ = "CLOSED"
                goto L14260
L14250:     net_open = gross_left + disc_left     /* Ship Open Amt */
            if shipflag% = 0% then prstat$ = "CLOSED"

L14260:     total_open  = total_open  + net_open
            if crhold$ = "C" then prstat$ = "CANCEL"
            if crhold$ = "H" then prstat$ = "CRHOLD"

            if export_msg$ = " " then x$ = " " else x$ = "*"

            socount%     = socount%     + 1%
            gsocount%    = gsocount%    + 1%
            if line% > 55% then gosub listing_heading
            print using L14620, so$, po$, sodate$, cancel$, str$, pc$,    ~
                               region$, salesman$(1), salesman$(2),      ~
                               salesman$(3), prstat$, x$, sodiscpct,     ~
                               net_order, net_open
            line%        = line%        + 1%
            goto listing_loop


        end_listing
            if line% = 857% then return
            if print_activity% = 0% then return
                print using L14650
                print using L14680, socount%, total_order, total_open
                call "DESCRIBE" (#3, shipto$, shiptoname$, 0%, f1%(3))
                print "**END OF LISTING FOR : " & shipto$ & " (" &       ~
                                                  shiptoname$ & ")"
                print : print : line% = line% + 5%
                gtotal_order = gtotal_order + total_order
                gtotal_open  = gtotal_open  + total_open
                return

L14430
*        Print Grand Totals for All Orders Listed
            if gsocount% = 0% and mc% = 9% then L14443
            if gsocount% = 0% and mc% = 5% then L14439
            gosub page_heading
            print using L14712, gsocount%, gtotal_order, gtotal_open
            bsocount% = bsocount% + gsocount%
            runtime$ = " " : call "TIME" (runtime$)
              if mc% = 9% then L14442
            print using L14722, runtime$
L14439:     if sopt% = 1% then shipto$, shiptoname$, co_range$(2) = " "
            return

L14442:     print using L14724, runtime$
L14443:     mc% = 5% : lc% = 6% : cc% = 45% : line% = 56%
            goto listing_printing

        listing_heading
            gosub page_heading
            print using L14530, qty_based_msg$(2%), qty_based_msg$(1%)
            print using L14560
            print using L14590
            line% = line% + 3%
            return

L14530: % Ship-To                            Order   Cancel        Prc  S~
        ~ales               ####### E   Order                       ######~
        ~###
L14560: %Sales Order No.  Customer Po No.    Date     Date   Store Cde Re~
        ~gion Sales Persons  Status X  Disc %   Net Order Amt    Net Open ~
        ~Amt
L14590: %---------------- ---------------- -------- -------- ----- --- --~
        ~---- -------------- ------ -  ------  --------------  -----------~
        ~---
L14620: %################ ################ ######## ########  ###   #   #~
        ~###  #### #### #### ###### # -###.## -###,###,###.## -###,###,###~
        ~.##
L14650: %                                                                ~
        ~                                      --------------- -----------~
        ~---
L14680: %                                                                ~
        ~  CUSTOMER TOTALS (##### ORDERS)     -###,###,###.## -###,###,###~
        ~.##

L14712: %                                                                ~
        ~    REPORT TOTALS (##### ORDERS)     -###,###,###.## -###,###,###~
        ~.##

L14722: % *** END OF CURRENT REPORT  @  ######## ***
L14724: % *** END OF HISTORY REPORT  @  ######## ***

        REM *************************************************************~
            *             P R I N T   O R D E R S                       *~
            *-----------------------------------------------------------*~
            * Print Detail for Orders per Selection Criteria Entered.   *~
            *************************************************************
        order_printing
            gsocount% = 0%
              if mc% = 9% then histrptmsg$ = "HISTORY FILES"             ~
                          else histrptmsg$ = "CURRENT FILES"
            readkey1$ = co_range$(3)
L14800:     str(readkey1$,10) = all (hex(ff))
            print_activity% = 0% /* 'No print activity this Customer' */
            call "PLOWNEXT" (#mc%, readkey1$, 0%, f1%(mc%))
            if f1%(mc%) = 0% then L16240  /* If not found               */
            if str(readkey1$,,9) > co_range$(4) then L16240 /* No More */
            rptplow$ = str(readkey1$,,9) & so_range$(3)
            socount% = 0%
            shipto$  = str(readkey1$,,9)
            gosub order_loop
            goto L14800

        order_loop
L14900:     gosub next_order
            if f1%(mc%) = 0% then L14930
            if openflag% = 0% then status$(1%) = "CLOSED"                ~
                              else status$(1%) = " OPEN"
            if crhold$ = "C"  then status$(1%) = "CANCELLED"
            if crhold$ = "H"  then status$(1%) = "CR HOLD"
            if shipflag% = 0% then status$(2%) = "CLOSED"                ~
                              else status$(2%) = " OPEN"
            if crhold$ = "C"  then status$(2%) = "CANCELD"
            if crhold$ = "H"  then status$(2%) = "CR HOLD"
            open% = 1%  /* Start as 1%; Conditional in next two lines */
            if qty_based_on$ = "I" and openflag% = 0% then open% = 0%
            if qty_based_on$ = "S" and shipflag% = 0% then open% = 0%
            if print_how$ = "O" and open% = 0% then L14900
            if print_how$ = "C" and open% > 0% then L14900
            gosub valid_data : if no% <> 0% then L14900 else L15000
L14930:         if gsocount% = 0% then return
*                   GSOCOUNT% = GSOCOUNT% + SOCOUNT%
                     if print_activity% = 0% then return
                     print
                     print using L16410, socount%
                     print
                     call "DESCRIBE" (#3, shipto$, shiptoname$,          ~
                                                              0%, f1%(3))
                     print "** END OF LISTING FOR : " & shipto$ & " (" & ~
                                                  shiptoname$ & ")"
                     return

L15000:     line% = 1000% : socount% = socount% + 1%
            gsocount% = gsocount% + 1%
            print_activity% = 1% /* 'Print activity this Customer' */
            plo$ = "Ship-to Customer  "          & str(shipto$,,9) &     ~
                   "              Ship-to  "     & str(shipto$(1)) &     ~
                   "   Sold-to  "                & str(soldto$(1))
                gosub print_line
            plo$ = "Order Number      " & so$
                str(plo$,51) = shipto$(2)  :  str(plo$,93) = soldto$(2)
                gosub print_line
            plo$ = "Customer PO       " & po$
                str(plo$,51) = shipto$(3)  :  str(plo$,93) = soldto$(3)
                gosub print_line
            plo$ = "Order / Cancel    " & sodate$ & " / " & cancel$
                str(plo$,51) = shipto$(4)  :  str(plo$,93) = soldto$(4)
                gosub print_line
            plo$ = "Entered           " & entered$ & " by " & userin$
                str(plo$,51) = shipto$(5)  :  str(plo$,93) = soldto$(5)
                gosub print_line
            plo$ = "Last Changed      " & lastmod$ & " by " & usermod$
                str(plo$,51) = shipto$(6)  :  str(plo$,93) = soldto$(6)
                gosub print_line
            if curr$ = "N" then L15203
            if curr_code$ = statutory$ then L15201
            plo$ = "Currency          " & curr_code$ & "        " &      ~
                tran_descr$
                str(plo$,75) = "Exchange Rate " & exchg_rate$ & " " &    ~
                curr_code$ & "/" & statutory$
                gosub print_line
L15201:     str(plo$,43) = statrptmsg$
L15203:     gosub print_line

            plo$ = "                  -- ORDER--  -- OPEN --"
                gosub print_line
            plo$ = "Gross Order  "
                call "CONVERT" (gross_order, 2.2, str(plo$,19,10))
                if qty_based_on$ = "S" then                              ~
                    call "CONVERT" (gross_left , 2.2, str(plo$,31,10))   ~
                    else                                                 ~
                    call "CONVERT" (gross_open , 2.2, str(plo$,31,10))
                str(plo$, 47) = "------STATUS--------"
                str(plo$, 75) = "Store       " & str$
                str(plo$,100) = strdescr$
                gosub print_line
            plo$ = "-###.##% Disc"
                call "CONVERT" (sodiscpct , 2.2, str(plo$, 1, 7))
                call "CONVERT" (disc_order, 2.2, str(plo$,19,10))
                if qty_based_on$ = "S" then                              ~
                    call "CONVERT" (disc_left , 2.2, str(plo$,31,10))    ~
                    else                                                 ~
                    call "CONVERT" (disc_open , 2.2, str(plo$,31,10))
                str(plo$, 47) = "Invoicing: " & status$(1%)
                str(plo$, 75) = "How Ship    " & howship$
                gosub print_line
            plo$ = "                  ----------  ----------"
                str(plo$, 47) = "Shipping:  " & status$(2%)
                str(plo$, 75) = "FOB Terms   " & fob$
                gosub print_line
            plo$ = "Net Order Amt"
                net_order = gross_order + disc_order
                net_open  = gross_open  + disc_open
                net_left  = gross_left  + disc_left
                call "CONVERT" (net_order, 2.2, str(plo$,19,10))
                if qty_based_on$ = "S" then                              ~
                    call "CONVERT" (net_left , 2.2, str(plo$,31,10))     ~
                    else                                                 ~
                    call "CONVERT" (net_open , 2.2, str(plo$,31,10))
                str(plo$, 55) = export_msg$
                str(plo$, 75) = "Terms Code  " & terms$
                gosub print_line
            gosub print_line

            plo$ = "Sales Region      " & str(region$) & "          " &  ~
                                              regiondescr$
                str(plo$, 75) = "Sales Disc  " & str(discacct$) & " " &  ~
                                                     discacctdescr$
                gosub print_line
            plo$ = "Salesmen/Split %  " & str(salesman$(1)) &  "   " &   ~
                     str(comm$(1)) & "    " & salesmandescr$(1)
                gosub print_line
            plo$ = "                  " & str(salesman$(2)) &  "   " &   ~
                     str(comm$(2)) & "    " & salesmandescr$(2)
                str(plo$, 75) = "Price Code  " & pc$
                str(plo$,100) = pcdescr$
                gosub print_line
            plo$ = "                  " & str(salesman$(3)) &  "   " &   ~
                     str(comm$(3)) & "    " & salesmandescr$(3)
                gosub print_line
            gosub print_line

            plo$ = "Shipping Instructions: " & shpinstr$(1)
                gosub print_line
            plo$ = "                       " & shpinstr$(2)
                gosub print_line
            gosub print_line

            if textid$ = " " or print_text$ <> "Y" then L15800
                status% = 0%
L15740:         call "TXTPRINT" (#14, f2%(14), 134%, textid$, "BCK008",  ~
                                 18%, line%, 55%, "Y", " ", status%)
                if status% = 0% then L15800
                     gosub page_heading
                     goto  L15740

L15800:     if line% < 51% then L15806
                line% = 99%
                goto L15810
L15806:     print
            line% = line% + 1%
L15810:     gosub line_heading
            linekey$ = str(so$) & hex(00)

          so_line_loop
            call "PLOWNEXT" (#lc%, linekey$, 16%, f1%(lc%))
            if f1%(lc%) = 0% then order_loop

            seq$ = str(linekey$,17)
            gosub load_line_item

            if line% > 53% then gosub line_heading
            print using L16360, seq$, part$, cat$, item$, duedate$,       ~
                               order$, stkuom$, lot$, price$,            ~
                               priceuom$, str(linediscpct$,5),           ~
                               linediscamt$, sales$
            if qty_based_on$ = "S" then                                  ~
                print using L16380, descr$, shipdate$, to_ship$,          ~
                                   to_ship_ext$, discs$                  ~
                else                                                     ~
                print using L16380, descr$, shipdate$, openqty$, lineext$,~
                                   discs$
            line% = line% + 2%

            if textidl$ = " " or print_text$ = "N" then L16064
                status% = 0%
L16020:         call "TXTPRINT" (#14, f2%(14%), 134%, textidl$, "BCK008",~
                                 9%, line%, 55%, "Y", " ", status%)
                if status% = 0% then L16064
                     gosub line_heading
                     goto  L16020

L16064
*        Part Text Printing
            if print_text$ = "N" then L16080
                call "READ100" (#33, part$, f1%(33%))
                if f1%(33%) = 0% then L16080
                    get #33 using L16070, textidp$
L16070:                 FMT POS(98), CH(4)
                    if textidp$ = " " or textidp$ = hex(ffffffff) then   ~
                                                                    L16080
                        status% = 0%
L16074:                 call "TXTPRINT" (#14, f2%(14%), 134%, textidp$,  ~
                             "BCK008", 9%, line%, 55%, "Y", " ", status%)
                        if status% = 0% then L16080
                            gosub line_heading
                            goto  L16074

L16080:     print  :  line% = line% + 1%
            goto so_line_loop

        line_heading
            if line% > 53% then gosub page_heading
            print using L16300
            print using L16320
            print using L16340
            line% = line% + 3%
            return


        print_line   /* Prints Detail Order Line             */
            if line% > 55% then gosub page_heading
            print using L16270, plo$
            line% = line% + 1%  :  plo$ = " "
            return

L16240
*        Print Grand Total Count of Orders Printed
            if gsocount% = 0% and mc% = 9% then L16254
            if gsocount% = 0% and mc% = 5% then L16250
            gosub page_heading
            print using L16410, gsocount%
            print
            bsocount% = bsocount% + gsocount%
            runtime$ = " " : call "TIME" (runtime$)
              if mc% = 9% then L16253
            print using L14722, runtime$
L16250:     if sopt% = 1% then shipto$, shiptoname$, co_range$(2) = " "
            return

L16253:     print using L14724, runtime$
L16254:     mc% = 5% : lc% = 6% : cc% = 45% : line% = 56%
            goto order_printing

L16270: %################################################################~
        ~#################################################################~
        ~###
L16300: %                                             Po. Due Date    Ord~
        ~ered  Qty                   Prce  Line    Disc Amt   Sales Acct
L16320: %   Seq Part Number / Description        Ctgy Itm Ship Dte  /Open~
        ~ Qty  Uom Lot No Unit Price  Uom Disc %  /Open Ext  /Discs Acct
L16340: %   --- -------------------------------- ---- --- -------- ------~
        ~---- ---- ------ ---------- ---- ------ ---------- ------------
L16360: %   ### ##########################       #### ### ######## ######~
        ~#### #### ###### ########## #### ###### ########## ############
L16380: %       #################################         ######## ######~
        ~####                                    ########## ############

L16410: %#### Orders Listed

        REM *************************************************************~
            *          C O M M O N   P R I N T   R O U T I N E S        *~
            *-----------------------------------------------------------*~
            * Stuff in common to both reports.                          *~
            *************************************************************

        next_order
            call "PLOWNEXT" (#mc%, rptplow$, 9%, f1%(mc%))
            if f1%(mc%) = 0% then return
            if str(rptplow$,10) <= so_range$(4) then L16550
                f1%(mc%) = 0%
                return
L16550:     so$ = str(rptplow$,10)  :  gosub load_order
            return


        page_heading
            page% = page% + 1%  :  line% = 4%
            print page
            print using L16690, date$, runtime$, company$, rptid$
            print using L16720, qty_based_msg$(1%), rpthdr$, page%
            if rptid$ = "BCK007" then                                    ~
              print using L16750, so_range$, histrptmsg$, howrptmsg$,     ~
                                 statrptmsg$  else                       ~
              print using L16750, so_range$, histrptmsg$, howrptmsg$, " "
            print
            return


L16690: %RUN DATE: ######## ########               ######################~
        ~######################################               BCKDSPLY:###~
        ~###
L16720: %OPEN AMTS BASED ON: #########  #################################~
        ~###############################################          PAGE: ##~
        ~###
L16750: %FOR ORDERS: ######################################   ###########~
        ~##      ####################     ##############################




        report_control:
            page% = 0%
            line% = 857%
            bsocount% = 0%   /* This is a total of history & current */
            runtime$ = " ":call "TIME" (runtime$)
            call "COMPNAME" (12%, company$, u3%)
            call "SHOSTAT"  ("Printing " & report$)
            if so_range$(1) <> "ALL" then L16871
                so_range$ = "ALL" : goto L16874
L16871:     if so_range$(1) <> so_range$(2) then L16873
                so_range$ = so_range$(1) : goto L16874
L16873:     so_range$ = str(so_range$(1)) & "  to " & so_range$(2)
L16874:     if co_range$(1) <> "ALL" then L16877
                ship_rnge$ = "ALL" : goto L16892
L16877:     if co_range$(1) = co_range$(2) then L16881
                ship_rnge$ = str(co_range$(1)) & "  to " & co_range$(2)
                goto L16892
L16881:     ship_rnge$ = str(co_range$(1))
            call "DESCRIBE" (#3, co_range$(1), str(ship_rnge$,11),       ~
                                                              1%, f1%(3))
L16892:     rpthdr$   = report$ & " for Ship-to " & ship_rnge$
            call "STRING" addr("CT", rpthdr$, 80%)
            call "SETPRNT" (rptid$, " ", 0%, 0%)
            select printer (134)
            report%   = 1%
            if rptid$ = "BCK007" then gosub listing_printing             ~
                                 else gosub order_printing
            report%   = 0% : manyprt% = 0%
            close printer  :  select ws
            call "SETPRNT" (rptid$, " ", 0%, 1%)
            if bsocount% <> 0% then return
                call "ASKUSER" (2%, report$,                             ~
                          "No orders were found within the criteria",    ~
                          "specified.  Press any PF Key to Continue...", ~
                          " ")
                return


        REM *************************************************************~
            *                 D E F A U L T S                           *~
            *-----------------------------------------------------------*~
            * Set Input Messages for Report Screen.                     *~
            *************************************************************

        deffn'052(fieldnr%)
            on fieldnr% gosub L21082, L21130, L21142, L21220, L21160, L21250,  ~
                              L21290
            return

L21082:     inpmessage$ = "Enter Customer Number Range to Print."
            return

L21130:     inpmessage$ = "Enter Order Number Range to Print."
            return

L21142:     inpmessage$ = "Enter Order Date Range to Print."
            return

L21160:     inpmessage$ = "Print Including HISTORY Files? (Y/N)."
            return

L21220:     inpmessage$ = "Print Order Text? (Y/N)."
            return

L21250:     inpmessage$ = "'O' = Open Orders Only; 'C' = Closed Orders "&~
                          "Only; 'B' = Both"
            return

L21290:     inpmessage$ = "Open Quantities Based On: 'I' = Invoicing or"&~
                          " 'S' = Shipping"
            return

        REM *************************************************************~
            *                L O A D   S U M M A R Y                    *~
            *-----------------------------------------------------------*~
            * Loads data for summary screen display.                    *~
            *************************************************************
        load_summary
            if po_flag%   = 1% then load_posummary
            if date_flag% = 1% then load_datesummary
            init(" ") disp$()
            disp% = 0%
            eof%  = 1%
            plowcur$ = str(shipto$) & lastso$
              if xinflag% = 1% then L30100   /* SIMO_PLOW */

            call "PLOWNEXT" (#5, plowcur$, 9%, f1%(5))
            if f1%(5) = 1% then curr_only   /* NO SIM)_PLOW */
                eof% = 0%
                if disp% < 14% then disp$(disp%+1%) = "*EOF*"
                return

L30100: REM SIMO_PLOW LOGIC(?) TO READ BOTH CURRENT & HISTORY FILES
            plowhis$ = str(shipto$) & lastso$
            call "PLOWNEXT" (#5, plowcur$, 9%, f1%(5))
              if f1%(5) = 0% then hist_only
L30120:     call "PLOWNEXT" (#9, plowhis$, 9%, f1%(9))
              if f1%(9) = 0% then curr_only
L30130:     if plowcur$ > plowhis$ then L30165
              gosub get_cur_data
              if disp% = 14% then return
            call "PLOWNEXT" (#5, plowcur$, 9%, f1%(5))
              if f1%(5) = 0% then L30215
            goto L30130

L30165:     gosub get_his_data
              if disp% = 14% then return else L30120

        hist_only
L30185:     call "PLOWNEXT" (#9, plowhis$, 9%, f1%(9))
              if f1%(9) <> 0% then L30215
L30195:     eof% = 0%
                if disp% < 14% then disp$(disp%+1%) = "*EOF*"
              return

L30215:     gosub get_his_data
              if disp% = 14% then return
            goto L30185

        curr_only
L30240:       gosub get_cur_data
              if disp% = 14% then return
            call "PLOWNEXT" (#5, plowcur$, 9%, f1%(5))
              if f1%(5) = 0% then L30195 else L30240

        get_cur_data
            get #5 using L30280, so$, po$, str$, sodate$, cancel$,        ~
                                sodiscpct, netso, crhold$
L30280:         FMT XX(9), 2*CH(16), POS(803), CH(3), 2*CH(6), POS(859), ~
                    2*PD(14,4), CH(1)

            disp% = disp% + 1%  :  lastso$ = so$  :  lastpo$ = po$
              hist%(disp%) = 0% :  lastdate$ = sodate$
              goto L30360

        get_his_data
            get #9 using L30330, so$, po$, str$, sodate$, cancel$,        ~
                                sodiscpct, netso, crhold$
L30330:         FMT XX(9), 2*CH(16), POS(803), CH(3), 2*CH(6), POS(859), ~
                    2*PD(14,4), CH(1)

            disp% = disp% + 1%  :  lastso$ = so$
              hist%(disp%) = 1%

L30360:     call "DATEFMT" (sodate$)
            call "DATEFMT" (cancel$)
            disp$(disp%) = str(so$) & " " & str(po$) & " " &             ~
                           str(sodate$) & " " & str(cancel$) & " " &     ~
                           str(str$)
            netso = round(netso - (netso *sodiscpct * .01), 2)
            convert netso to str(disp$(disp%),64,13), pic(-#####,###.##)
            convert sodiscpct to str(disp$(disp%),57,6), pic(###.##)
            if sodiscpct < 0 then                                        ~
                convert sodiscpct to str(disp$(disp%),57,6), pic(-##.##)
            if crhold$ = "H" then str(disp$(disp%),78,2) = "**"
              return

        REM *************************************************************~
            *             L O A D   P O   S U M M A R Y                 *~
            *-----------------------------------------------------------*~
            * Loads data for summary screen display                     *~
            *    and creates/reads work file sorted by PO #             *~
            *************************************************************
        load_posummary
            init(" ") disp$()
            disp% = 0%
            eof%  = 1%
            if workflag1% = 0% then gosub create_workfile1
            plowkey$ = str(lastpo$) & lastso$
        po_loop
            if disp% > 0% then L30551
            call "READ102" (#50, plowkey$, f1%(50))
            goto L30554

L30551:     call "READNEXT" (#50, f1%(50))
L30554:     if f1%(50) = 1% then L30569
                eof% = 0%
                if disp% < 14% then disp$(disp%+1%) = "*EOF*"
                return

L30569:     get #50, using L30572, po$, so$
L30572:         FMT 2*CH(16)
            readkey$ = str(shipto$) & so$
            call "READ100" (#5, readkey$, f1%(5))
                if f1%(5) = 0% then po_loop
            gosub get_cur_data
            if disp% < 14% then po_loop else return

        REM *************************************************************~
            *           L O A D   D A T E   S U M M A R Y               *~
            *-----------------------------------------------------------*~
            * Loads data for summary screen display                     *~
            *    and creates/reads work file sorted by Date             *~
            *************************************************************
        load_datesummary
            init(" ") disp$()
            disp% = 0%
            eof%  = 1%
            if workflag2% = 0% then gosub create_workfile2
            plowkey$ = str(lastdate$,,6) & lastso$
        date_loop
            if disp% > 0% then L30651
            call "READ102" (#51, plowkey$, f1%(51))
            goto L30654

L30651:     call "READNEXT" (#51, f1%(51))
L30654:     if f1%(51) = 1% then L30669
                eof% = 0%
                if disp% < 14% then disp$(disp%+1%) = "*EOF*"
                return

L30669:     get #51, using L30672, workdate$, so$
L30672:         FMT CH(6), CH(16)
            readkey$ = str(shipto$) & so$
            call "READ100" (#5, readkey$, f1%(5))
                if f1%(5) = 0% then date_loop

            gosub get_cur_data

            if disp% < 14% then date_loop else return



        REM *************************************************************~
            *              C R E A T E   W O R K F I L E                *~
            *-----------------------------------------------------------*~
            * Creates a workfile to display Sales Orders by PO #        *~
            *************************************************************
        create_workfile1

            plowkey$ = str(shipto$) & hex(00)
            call "READ102" (#5, plowkey$, f1%(5))
                if f1%(5) <> 0% then L30831
L30821:              return clear
                     return clear
                     po_flag% = 0% : workflag1% = 0%
                     goto first_screen        /* This shouldn't happen */

L30831:     call "WORKOPEN" (#50, "IO", 500%, f2%(50))
                if f2%(50) = 1% then L30821   /* Get out if open fails */
            call "SHOSTAT" ("Searching by PO #. . .")
            goto L30849

        make1_loop

            call "READNEXT" (#5, f1%(5))

L30849:     if f1%(5) = 0% then return

            get #5, using L30855, temp$, so$, po$
L30855:         FMT CH(9), 2*CH(16)
            if temp$ <> shipto$ then return
            workflag1% = 1%
            write #50, using L30863, po$, so$
L30863:         FMT 2*CH(16)
            goto make1_loop

        REM *************************************************************~
            *              C R E A T E   W O R K F I L E                *~
            *-----------------------------------------------------------*~
            * Creates a workfile to display Sales Orders by Date        *~
            *************************************************************
        create_workfile2

            plowkey$ = str(shipto$) & hex(00)
            call "READ102" (#5, plowkey$, f1%(5))
                if f1%(5) <> 0% then L30930
L30920:              return clear
                     return clear
                     date_flag% = 0% : workflag2% = 0%
                     goto first_screen        /* This shouldn't happen */

L30930:     call "WORKOPEN" (#51, "IO", 500%, f2%(51))
                if f2%(51) = 1% then L30920   /* Get out if open fails */
            call "SHOSTAT" ("Searching by Date. . .")
            goto L30948

        make2_loop

            call "READNEXT" (#5, f1%(5))

L30948:     if f1%(5) = 0% then return

            get #5, using L30954, temp$, so$, workdate$
L30954:         FMT CH(9), CH(16), POS(806), CH(6)
            if temp$ <> shipto$ then return
            workflag2% = 1%
            write #51, using L30962, workdate$, so$
L30962:         FMT CH(6), CH(16)
            goto make2_loop

        REM *************************************************************~
            *                L O A D   O R D E R                        *~
            *-----------------------------------------------------------*~
            * Loads a specific invoice for display or report.           *~
            *************************************************************
        load_order
            readkey$ = str(shipto$) & so$
            call "READ100" (#mc%, readkey$, soonfile%)
            if soonfile% = 0% then return

            if report% = 0% then print at(03,02), "Loading Order..."
            get #mc% using L35010, po$, shipto$(), soldto$(), terms$,     ~
                     howship$, fob$, shpinstr$(), salesacct$, discacct$, ~
                     salesman$(), comm%(), region$, vf$, textid$, str$,  ~
                     sodate$, cancel$, entered$, userin$, lastmod$,      ~
                     usermod$, sorsn$, export_msg$, pc$, sodiscpct,      ~
                     gross_open, crhold$, curr_code$
            if export_msg$ = "Y" then export_msg$ = "EXPORT" else        ~
                                      export_msg$ = " "
            if curr_code$ = " " then curr_code$ = statutory$
            tran_descr$, tran_curdescr$ = " "
            if curr$ = "N" or curr_code$ = statutory$ then L31170
                readcm$ = curr_code$
            call "READ100" (#46, readcm$, f1%(46))
            if f1%(46) = 0% then L31170
                get #46 using L31166, tran_curdescr$
L31166:             FMT POS(5), CH(30)
            tran_descr$ = tran_curdescr$
            call "PUTPAREN" (tran_descr$)
L31170:     tran_curdescr$ = tran_curdescr$
            if textid$ = hex(ffffffff) or textid$ = hex(00000000) then   ~
                                                            textid$ = " "
            disc_open = 0 - round(gross_open * sodiscpct * .01, 2)
            stat_disc_open = disc_open
            call "LINSMASH" (shipto$())
            call "LINSMASH" (soldto$())
            call "DATEFMT" (sodate$ )
            call "DATEFMT" (cancel$ )
            call "DATEFMT" (entered$)
            call "DATEFMT" (lastmod$)
            call "DESCRIBE" (#4, salesacct$, salesacctdescr$, 0%, f1%(4))
            call "GLFMT" (salesacct$)
            call "DESCRIBE" (#4, discacct$, discacctdescr$, 0%, f1%(4))
            call "GLFMT" (discacct$)
            init(" ") salesmandescr$(), comm$()
            for i% = 1% to 3%
                if salesman$(i%) = " " then L31360
                     call "DESCRIBE" (#11, salesman$(i%),                ~
                                         salesmandescr$(i%), 0%, f1%(11))
                     convert comm%(i%) to comm$(i%), pic(##0)
L31360:     next i%
            readkey$ = "REGIONS  " & region$
            call "DESCRIBE" (#8, readkey$, regiondescr$, 0%, f1%(8))
            readkey$ = "PRICECODE" & pc$
            call "DESCRIBE" (#8 , readkey$, pcdescr$   , 0%, f1%(8))
            readkey$ = "ADJREASON" & sorsn$
            call "DESCRIBE" (#8, readkey$, sorsndescr$, 0%, f1%(8))
            call "DESCRIBE" (#12, str$ , strdescr$ , 0%, f1%(12))

*        Now load up line items
            maxlines%, c%, openflag%, shipflag% = 0%
            gross_order, disc_order = 0
            tran_gross_order, tran_disc_order = 0
            stat_gross_order, stat_disc_order = 0
            tran_gross_open = 0
            gross_left = 0
            init(" ") lines$()
            readkey$ = str(so$) & hex(00)
            readcurr$ = str(so$) & hex(00)

        line_loop
            call "PLOWNEXT" (#lc%, readkey$, 16%, f1%(lc%))
            if f1%(lc%) = 1% then L31620
                disc_order = 0 - round(gross_order * sodiscpct * .01, 2)
                disc_left  = 0 - round(gross_left  * sodiscpct * .01, 2)
                stat_disc_order = disc_order
                tran_disc_order = 0 - round(tran_gross_order * sodiscpct ~
                     * .01, 2)
                tran_disc_open  = 0 - round(tran_gross_open * sodiscpct  ~
                     * .01, 2)
                return
L31620:     maxlines%, c% = maxlines% + 1%
            get #lc% using L31670, str(lines$(c%, 1), 1, 3),  /* Seq #  */~
                                str(lines$(c%, 1), 5,25),    /* Part   */~
                                str(lines$(c%, 1),31,27),    /* Descr  */~
                                order, invcd, openqty, preinv, stkprice, ~
                                conv, price,                             ~
                                linedisc, shipcode$

            stkprice = price / conv  /* More accurate value */

L31670:         FMT POS(26), CH(3), XX(3), CH(25), CH(32), POS(93),      ~
                    PD(14,4), PD(14,4), PD(14,4), POS(133), PD(14,4),    ~
                    POS(141), PD(14,4), POS(157), PD(14,7), PD(14,4),    ~
                                        POS(173), PD(14,4),POS(277), CH(1)

*       *  If SHIPCODE$ blank, use CUSTOMER default- if customer default
*       *  is blank the set shipcode to '3'.
            if shipcode$ <> " " then L31695
                call "READ100" (#3, shipto$, f1%(3%))
                    if f1%(3%) = 0% then L31690
                get #3 using L31688, shipcode$
L31688:             FMT POS(733), CH(1)
L31690:         if shipcode$ = " " then shipcode$ = "3"
L31695:     current_order = openqty + invcd
            leftqty = openqty - preinv /* Remaining to be shipped */
            call "CONVERT" (current_order  , 2.2, str(lines$(c%,1),59,10))
              if openqty <> 0 then openflag% = 1%
              if leftqty  > 0 then shipflag% = 1%
            call "CONVERT" (openqty, 2.2, str(lines$(c%, 1),70,10))
            lines$(c%, 2) = lines$(c%, 1)
                str(lines$(c%,2),1%,3%) = shipcode$
            call "CONVERT" (order  , 2.2, str(lines$(c%, 2),59,10))
            ext = round(order * stkprice, 2)
            leftext = round(leftqty * stkprice, 2)
            gross_order = gross_order + ext - round(ext*linedisc*.01,2)
            gross_left = gross_left + leftext -                          ~
                                       round(leftext * linedisc * .01, 2)
            stat_gross_order = gross_order
            stat_gross_open  = gross_open
            exchg_rate$, exchg_msg$ = " "
        REM Get Corresponding Customer Part Numbers...
            lines$(c%,3%) = lines$(c%, 1%)
            str(lines$(c%,3%),5%,52%) = " "
            if lc% = 6% then refsource$ = "BCK " else refsource$ = "BCKH"
            call "PTUSEDSB" ("R", refsource$,                            ~
                 str(so$),  str(lines$(c%,1%),1%,3%),                    ~
                 str(lines$(c%,3%),5%,25%), str(lines$(c%,3%),31%,27%),  ~
                 reftype$(c%), ret%)
            if ret% <> 0% then L31755
                 str(lines$(c%,3%),5%,25%) = "** No Cross Reference **"
                 str(lines$(c%,3%),31%,27%)= " "
L31755:     if ret% = 1% then refflag% = 1%
            lines$(c%,4%) = lines$(c%, 3%)
            call "CONVERT" (order  , 2.2, str(lines$(c%,4%),59%,10%))
                str(lines$(c%,4),1%,3%) = shipcode$
        REM Continue With CMS Part Number Logic...
            if curr$ = "N" or curr_code$ = statutory$ then line_loop
                call "PLOWNEXT" (#cc%, readcurr$, 16%, f1%(cc%))
                if f1%(cc%) = 0% then line_loop
            get #cc% using L31800, unitpric, exchg_rate /*Price - Stk UOM*/
L31800:         FMT POS(24), PD(14,4), POS(54), PD(14,7)
            call "CONVERT"(exchg_rate, 2.7, exchg_rate$)
            exchg_msg$ = "Exchange Rate: " & exchg_rate$ & " " &         ~
                         curr_code$ & "/" & statutory$
            grossext = round(order * unitpric, 2)
            tran_gross_order = tran_gross_order + grossext -             ~
                round(grossext * linedisc * .01,2)
            openext = round(openqty * unitpric, 2)
            tran_gross_open = tran_gross_open  + openext -               ~
                round(openext * linedisc * .01,2)
            goto line_loop

        load_line_item
            init(" ") stat_price$, stat_lineext$, stat_linediscamt$,     ~
                      tran_price$, tran_lineext$, tran_linediscamt$
            readkey$ = str(so$) & seq$
            readcurr$ = readkey$
            call "READ100" (#lc%, readkey$, lineonfile%)
            if lineonfile% = 0% then return

            get #lc% using L35330, seq$, item$, part$, descr$, cat$,      ~
                     order, ship, openqty, schdl, alloc, preinv,         ~
                     pricestk, stkuom$, priceuom$, conv, price,          ~
                     linediscpct, taxable$, sales$, discs$,              ~
                     origdue$, duedate$, shipdate$, lot$, project$,      ~
                     demtype$, demprty$, textidl$, allocflag$, lastinv$, ~
                     shipcode$
            if textidl$ = hex(ffffffff) or textidl$ = hex(00000000)      ~
                                                      then textidl$ = " "
            if taxable$ = "Y" then taxable$ = "TAXABLE"                  ~
                              else taxable$ = " "

            pricestk = price / conv    /* Trying for better accuracy */

            call "DATEFMT" (origdue$ )
            call "DATEFMT" (duedate$ )
            call "DATEFMT" (shipdate$)
            call "DESCRIBE" (#4, sales$, salesdescr$, 0%, u3%)
            call "GLFMT" (sales$)
            call "DESCRIBE" (#4, discs$, discsdescr$, 0%, u3%)
            call "GLFMT" (discs$)
            call "CONVERT" (order  , 2.2, order$   )
            call "CONVERT" (openqty, 2.2, openqty$ )
            call "CONVERT" (ship   , 2.2, ship$    )
            call "CONVERT" (conv   , 2.7, conv$    )
            call "CONVERT" (schdl  , 2.2, schdl$   )
            call "CONVERT" (alloc  , 2.2, alloc$   )
            call "CONVERT" (preinv , 2.2, preinv$  )
            to_ship = openqty - preinv
            call "CONVERT" (to_ship, 2.2, to_ship$ )
            call "DESCRIBE" (#7, cat$, catdescr$, 0%, f1%(7))
            call "DESCRIBE" (#13, project$, projectdescr$, 0%, u3%)

            lineext     = round(openqty * pricestk, 2)
            linediscamt = 0 - round(lineext * linediscpct * .01, 2)
            lineext     = lineext + linediscamt
            to_ship_ext = round(to_ship * pricestk, 2)
            to_ship_ext = to_ship_ext - round(to_ship_ext * linediscpct  ~
                                                                * .01, 2)
            call "CONVERT" (price      , 2.4, price$      )
            call "CONVERT" (linediscpct, 2.2, linediscpct$)
            call "CONVERT" (lineext    , 2.2, lineext$    )
            call "CONVERT" (to_ship_ext, 2.2, to_ship_ext$)
            call "CONVERT" (linediscamt, 2.2, linediscamt$)
            call "CONVERT" (pricestk   , 2.4, pricestk$   )
            stat_price$       = price$
            stat_lineext$     = lineext$
            stat_linediscamt$ = linediscamt$
            if curr$ = "N" or curr_code$ = statutory$ then return
                call "READ100" (#cc%, readcurr$, f1%(cc%))
                if f1%(cc%) = 0% then return
            get #cc% using L32360, unitpric, pricpric
L32360:         FMT POS(24), PD(14,4), PD(14,4)
            tranext = round(openqty * unitpric, 2)
            trandisc = 0 - round(tranext * linediscpct * .01, 2)
            tranext = tranext + trandisc
            call "CONVERT" (pricpric, 2.4, tran_price$)
            call "CONVERT" (trandisc, 2.2, tran_linediscamt$)
            call "CONVERT" (tranext,  2.2, tran_lineext$)
            return


*       **** LARGE FORMAT STATEMENTS **********************************
L35010: FMT                 /* FILE: BCKMASTR                          */~
            XX(9),          /* Customer Code                           */~
            XX(16),         /* Sales Order Number                      */~
            CH(16),         /* Purchase Order Number                   */~
            6*CH(30),       /* Ship To Name and Address                */~
            6*CH(30),       /* Sold-To Name and Address                */~
            CH(20),         /* Payment Terms Code                      */~
            CH(20),         /* How Ship Information                    */~
            CH(20),         /* F.O.B. Information                      */~
            2*CH(50),       /* Shipping Instructions                   */~
            2*CH(9),        /* Sales Acct and Sales Discs Acct         */~
            3*CH(4),        /* Salesman Id                             */~
            3*BI(01),       /* Percentage of Sale credited to salesman.*/~
            CH(4),          /* Region Code                             */~
            CH(200),        /* Variable Fields                         */~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            CH(3),          /* Store Code                              */~
            CH(6),          /* Order Date                              */~
            CH(6),          /* Cancel Date                             */~
            XX(6),          /* Due Date Default                        */~
            XX(6),          /* Ship Date Default                       */~
            CH(6),          /* Date Entered                            */~
            CH(3),          /* User ID who entered transaction         */~
            CH(6),          /* Date Last Changed                       */~
            CH(3),          /* User ID who last changed                */~
            CH(9),          /* Change Reason Code                      */~
            CH(1),          /* Export Flag                             */~
            CH(1),          /* Price Code                              */~
            PD(14,4),       /* SO Discount %                           */~
            PD(14,4),       /* Gross Open Order Amount                 */~
            CH(1),          /* Credit Hold Flag                        */~
            POS(893),       /*  go directly to currency code           */~
            CH(04)          /* Order Currency Code                     */

L35330: FMT                 /* FILE: BCKLINES                          */~
            XX(9),          /* Customer Code                           */~
            XX(16),         /* Sales Order Number                      */~
            CH(3),          /* General purpose sequence number         */~
            CH(3),          /* Purchase Order Line Number              */~
            CH(25),         /* Part Number                             */~
            CH(32),         /* Part description                        */~
            CH(4),          /* Category Code                           */~
            PD(14,4),       /* Order Quantity                          */~
            PD(14,4),       /* Quantity Shipped                        */~
            PD(14,4),       /* Open Quantity                           */~
            PD(14,4),       /* Scheduled Quantity                      */~
            PD(14,4),       /* Allocated Quantity                      */~
            PD(14,4),       /* Preinvoiced Qty                         */~
            PD(14,4),       /* Unit Price- at Stocking UOM             */~
            CH(4),          /* Stocking UOM                            */~
            CH(4),          /* Pricing Unit of Measure                 */~
            PD(14,7),       /* Conversion Factor (buy to sell)         */~
            PD(14,4),       /* Unit Price at Pricing UOM               */~
            PD(14,4),       /* Line Item Discount                      */~
            CH(1),          /* Taxable Purchase (Y/N)                  */~
            CH(9),          /* Sales Account Number                    */~
            CH(9),          /* Discounts Account                       */~
            3*CH(6),        /* Orig, Current Due / Ship Dates          */~
            CH(6),          /* Lot Number                              */~
            CH(8),          /* Project                                 */~
            XX(8),          /* Filler                                  */~
            2*CH(1),        /* Demand Type / Priority                  */~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            CH(1),          /* Allocation Flag                         */~
            CH(8),          /* Last Invoice Number                     */~
            POS(277), CH(1) /* Shipping Priority Code                  */

        REM *************************************************************~
            *           S E L E C T    S H I P - T O                    *~
            *-----------------------------------------------------------*~
            * Get Ship-to for display.                                  *~
            *************************************************************

        deffn'100(fieldnr%)
            inpmessage$ = "Enter Customer Code & Press RETURN or "    &  ~
                          "PF11 to Include Archived Information."
            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            on fieldnr%  gosub      L40065          /* Ship-to Customer */
            goto L40080
                  lfac$(fieldnr%) = hex(80)  :  return
L40065:           lfac$(fieldnr%) = hex(81)  :  return
                  lfac$(fieldnr%) = hex(82)  :  return

L40080:     accept                                                       ~
               at (01,02), "Sales Order Inquiry",                        ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Ship-to Customer",                           ~
               at (06,30), fac(lfac$( 1)), shipto$              , ch(09),~
               at (06,49), fac(hex(8c))  , shiptoname$          , ch(30),~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(5)Next Ship-to",                            ~
               at (23,02), "(8)Find Via SO# (Current Files Only)",       ~
               at (24,02), "(9)Find Via PO# (Current Files Only)",       ~
               at (22,65), "(13)Instructions",                           ~
               at (23,47), "(12)Print Listing",                          ~
               at (24,47), "(14)Print Details",                          ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Exit Program",                           ~
               at (22,20), "(11)Search Including HISTORY",               ~
                     keys(hex(00050809ff0b0c0d0e0f10)), key(keyhit%)

               if keyhit% <> 13 then L40182 : call "MANUAL" ("BCKDSPLY")
                                             goto L40080

L40182:        if keyhit% =  0% then xinflag% = 0%
               if keyhit% = 11% then xinflag% = 1%

               if keyhit% <> 15 then return  : call "PRNTSCRN"
                                               goto L40080

        REM *************************************************************~
            *               M A I N   S C R E E N                       *~
            *-----------------------------------------------------------*~
            * Sales Order Listing Screen.                               *~
            *************************************************************

        deffn'101(opt%)
            hdr1$(1) = "Sales Order #"  :  hdr1$(5) = "Str"
            hdr1$(2) = "PO Number"      :  hdr1$(6) = "Disc %"
            hdr1$(3) = "Ord Date"       :  hdr1$(7) = " Net Open Amt"
            hdr1$(4) = " Cancel"        :  hdr1$(8) = "CR"
            init(hex(ac)) sfac$()
            if po_flag% = 0% and date_flag% = 0% then sfac$(1) = hex(a4)
            if po_flag%   = 1% then sfac$(2) = hex(a4)
            if date_flag% = 1% then sfac$(3) = hex(a4)

            str(line2$,,61%) = "Ship-to: " & shipto$ & " (" &            ~
                               shiptoname$ & ")"
            init(hex(86)) dfac$()  :  init(hex(84)) lfac$()
                for i = 1 to 14
                if hist%(i) = 0% then dfac$(i) = hex(86)                 ~
                                 else dfac$(i) = hex(8e)
                next i
            if opt% = 0% then L40285
                init(hex(8c)) dfac$(), lfac$()
                init(hex(ac)) sfac$()
                lfac$(opt%) = hex(81)
L40285:     gosub setpf1
              if xinflag% = 0% then historymsg$ = "CURRENT Files Only"   ~
                               else historymsg$ = "Including HISTORY"    ~

L40295:     accept                                                       ~
               at (01,02),                                               ~
                  "Sales Order Inquiry: ORDER LISTING",                  ~
               at (01,40), fac(hex(8c)), historymsg$            , ch(18),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (04,65), fac(hex(8c)), statmsg$               , ch(13),~
               at (05,02), fac(sfac$( 1)), hdr1$( 1)            , ch(16),~
               at (05,19), fac(sfac$( 2)), hdr1$( 2)            , ch(16),~
               at (05,36), fac(sfac$( 3)), hdr1$( 3)            , ch(08),~
               at (05,45), fac(hex(ac)), hdr1$(4)               , ch(08),~
               at (05,54), fac(hex(ac)), hdr1$(5)               , ch(03),~
               at (05,58), fac(hex(ac)), hdr1$(6)               , ch(06),~
               at (05,65), fac(hex(ac)), hdr1$(7)               , ch(13),~
               at (05,79), fac(hex(ac)), hdr1$(8)               , ch(02),~
                                                                         ~
               at (06,02), fac(hex(80))  , disp$( 1)            , ch(79),~
               at (06,02), fac(dfac$( 1)), disp$( 1)            , ch(79),~
               at (07,02), fac(dfac$( 2)), disp$( 2)            , ch(79),~
               at (08,02), fac(dfac$( 3)), disp$( 3)            , ch(79),~
               at (09,02), fac(dfac$( 4)), disp$( 4)            , ch(79),~
               at (10,02), fac(dfac$( 5)), disp$( 5)            , ch(79),~
               at (11,02), fac(dfac$( 6)), disp$( 6)            , ch(79),~
               at (12,02), fac(dfac$( 7)), disp$( 7)            , ch(79),~
               at (13,02), fac(dfac$( 8)), disp$( 8)            , ch(79),~
               at (14,02), fac(dfac$( 9)), disp$( 9)            , ch(79),~
               at (15,02), fac(dfac$(10)), disp$(10)            , ch(79),~
               at (16,02), fac(dfac$(11)), disp$(11)            , ch(79),~
               at (17,02), fac(dfac$(12)), disp$(12)            , ch(79),~
               at (18,02), fac(dfac$(13)), disp$(13)            , ch(79),~
               at (19,02), fac(dfac$(14)), disp$(14)            , ch(79),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
               at (22,28), fac(lfac$( 1)), findso$              , ch(16),~
               at (23,28), fac(lfac$( 2)), findpo$              , ch(16),~
               at (24,28), fac(lfac$( 3)), finddate$            , ch(10),~
                     keys(str(pfkeys$)), key(keyhit%)

               if keyhit% <> 13 then L40515
                  call "MANUAL" ("BCKDSPLY")
                  goto L40295

L40515:        if keyhit% <> 15 then L40535
                  call "PRNTSCRN"
                  goto L40295

L40535:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        setpf1: on opt% + 1% goto L40560, L40635, L40700, L40800
L40560:    inpmessage$ = "Position Cursor and Press RETURN to see"  &    ~
                         " Order Details."
           pf$(1) = "(2)First  (8)Find Order  xxxxxxxxxxxxxxxx   (12)Pr"&~
                    "int Listing  (13)Instructions"
           pf$(2) = "(5)Next   (9)Find PO     xxxxxxxxxxxxxxxx   (14)Pr"&~
                    "int Details  (15)Print Screen"
           pf$(3) = "         (10)Find Date   xxxxxxxx           (26)Cu"&~
                    "st Credit    (16)Exit Display"
           pfkeys$ = hex(ff02ffff05ffff08090aff0c0d0e0f101aff2000)
           if first% = 0%   then L40615  : str(pf$(1), 1,8)  = " "
                                          str(pfkeys$, 2,1) = hex(ff)
L40615:    if eof%   = 1%   then L40622  : str(pf$(2), 1,8)  = " "
                                          str(pfkeys$, 5,1) = hex(ff)
L40622:    if xinflag% = 0% then L40628  : str(pf$(2),11,31) = " "
                                          str(pfkeys$, 9,1) = hex(ff)
                                          str(pf$(3),10,31) = " "
                                          str(pfkeys$,10,1) = hex(ff)
L40628:    return

L40635:    inpmessage$ = "Enter Sales Order Number or Press PF-1 to" &   ~
                         " Return to Listing."
           pf$(1) = "(1)Return    Find Order                           "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "                             "
           pfkeys$ = hex(01ffffffffffffffffffffff0dff0fffffffff00)
           return

L40700:    inpmessage$ = "Enter Purchase Order Number or Press PF-1 to" &~
                         " Return to Listing."
           pf$(1) = "(1)Return                                         "&~
                    "             (13)Instructions"
           pf$(2) = "             Find PO                              "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "                             "
           pfkeys$ = hex(01ffffffffffffffffffffff0dff0fffffffff00)
           return

L40800:    inpmessage$ = "Enter Date or Press PF-1 to Return to Listing."
           pf$(1) = "(1)Return                                         "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "             Find Date                            "&~
                    "                             "
           pfkeys$ = hex(01ffffffffffffffffffffff0dff0fffffffff00)
           return

        REM *************************************************************~
            *        R E P O R T   P A R A M E T E R S                  *~
            *-----------------------------------------------------------*~
            * Get Report Parameters.                                    *~
            *************************************************************

        deffn'102(fieldnr%)
            str(line2$,,61%) = "Ship-to: " & shipto$ & " (" &            ~
                               shiptoname$ & ")"
            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            if fieldnr% > 0% then lfac$(fieldnr%) = hex(81)
            gosub setpf2

L41130:     accept                                                       ~
               at (01,02), "Sales Order Inquiry: Print",                 ~
               at (01,29), fac(hex(8c)), report$                , ch(15),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Customer Numbers to Print",                  ~
               at (06,30), fac(lfac$( 1)), co_range$(1)         , ch(09),~
               at (06,48), "to",                                         ~
               at (06,51), fac(lfac$( 1)), co_range$(2)         , ch(09),~
               at (07,02), "Order Numbers to Print",                     ~
               at (07,30), fac(lfac$( 2)), so_range$(1)         , ch(16),~
               at (07,48), "to",                                         ~
               at (07,51), fac(lfac$( 2)), so_range$(2)         , ch(16),~
               at (08,02), "Order Date(s) to Print",                     ~
               at (08,30), fac(lfac$( 3)), od_range$(1)         , ch(08),~
               at (08,48), "to",                                         ~
               at (08,51), fac(lfac$( 3)), od_range$(2)         , ch(08),~
               at (09,02), "Include Text? (Y/N)",                        ~
               at (09,30), fac(lfac$( 4)), print_text$          , ch(01),~
               at (10,02), "Include History Files? ",                    ~
               at (10,30), fac(lfac$( 5)), hist$                , ch(01),~
               at (11,02), "Open or Closed Orders?",                     ~
               at (11,30), fac(lfac$( 6)), print_how$           , ch(01),~
               at (12,02), "Open Qtys Based On?",                        ~
               at (12,30), fac(lfac$( 7)), qty_based_on$        , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(str(pfkeys$)), key(keyhit%)

               if keyhit% <> 13 then L41600
                  call "MANUAL" ("BCKDSPLY")
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
            * Order Detail- Hdr Page 1.                                 *~
            *************************************************************

        deffn'111
            cfac2$, cfac3$ = hex(9c)
            gosub setpf11
            net_order = gross_order + disc_order
            stat_net_order = stat_gross_order + stat_disc_order
            net_open  = gross_open  + disc_open
            stat_net_open = stat_gross_open + stat_disc_open
            if curr$ = "N" or curr_code$ = statutory$ then L42104
                tran_net_order = tran_gross_order + tran_disc_order
                tran_net_open  = tran_gross_open  + tran_disc_open
                cfac2$ = hex(84)
                curdescr$ = stat_curdescr$
                if tran_display$ = "N" then tran_display$ = "Y" else     ~
                   tran_display$ = "N" /* Set up to reverse below */
                gosub hdr1_redisplay    /* Reverses Stat & Tran */
L42104:     changemsg$ = "  Changed: MM/DD/YY by"
            if crhold$ = "C" then changemsg$ = "Cancelled: MM/DD/YY by"

L42110: accept                                                           ~
          at (01,02), "Sales Order Inquiry: ORDER DETAIL   Hdr1",        ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
          at (04,02), "Ship", at (05,04), "To",                          ~
               at (04,08), fac(hex(84)),   shipto$(1)           , ch(30),~
               at (05,08), fac(hex(84)),   shipto$(2)           , ch(30),~
               at (06,08), fac(hex(84)),   shipto$(3)           , ch(30),~
               at (07,08), fac(hex(84)),   shipto$(4)           , ch(30),~
               at (08,08), fac(hex(84)),   shipto$(5)           , ch(30),~
               at (09,08), fac(hex(84)),   shipto$(6)           , ch(30),~
                                                                         ~
          at (04,42), "Sold",  at (05,44), "To",                         ~
               at (04,48), fac(hex(84)),   soldto$(1)           , ch(30),~
               at (05,48), fac(hex(84)),   soldto$(2)           , ch(30),~
               at (06,48), fac(hex(84)),   soldto$(3)           , ch(30),~
               at (07,48), fac(hex(84)),   soldto$(4)           , ch(30),~
               at (08,48), fac(hex(84)),   soldto$(5)           , ch(30),~
               at (09,48), fac(hex(84)),   soldto$(6)           , ch(30),~
                                                                         ~
          at (11,02), fac(cfac2$), curdescr$                    , ch(30),~
          at (11,37), fac(cfac3$), exchg_msg$                   , ch(35),~
          at (12,37), "-- O R D E R --",                                 ~
          at (12,57), "--- O P E N ---",                                 ~
                                                                         ~
          at (13,02), "Gross Order Amount",                              ~
          at (13,37), fac(hex(84)), gross_order    ,pic(-###,###,###.##),~
          at (13,57), fac(hex(84)), gross_open     ,pic(-###,###,###.##),~
                                                                         ~
          at (14,02), "Order Disc ( -###.## %)",                         ~
          at (14,15), fac(hex(84)), sodiscpct,              pic(-###.00),~
          at (14,37), fac(hex(a4)), disc_order     ,pic(-###,###,###.##),~
          at (14,57), fac(hex(a4)), disc_open      ,pic(-###,###,###.##),~
                                                                         ~
          at (15,02), "Net Order Amount",                                ~
          at (15,37), fac(hex(84)), net_order      ,pic(-###,###,###.##),~
          at (15,57), fac(hex(84)), net_open       ,pic(-###,###,###.##),~
                                                                         ~
          at (17,02), "Customer PO",                                     ~
               at (17,14), fac(hex(84)),   po$                  , ch(16),~
          at (17,31), "-------STATUS-------",                            ~
          at (18,02), "Order Date",                                      ~
          at (18,31), "Invoicing:",                                      ~
               at (18,14), fac(hex(84)),   sodate$              , ch(08),~
               at (18,42), fac(hex(84)),   status$(1%)          , ch(09),~
               at (20,37), fac(hex(84)),   export_msg$          , ch(06),~
          at (19,02), "Cancel Date",                                     ~
               at (19,14), fac(hex(84)),   cancel$              , ch(08),~
          at (19,31), "Shipping: ",                                      ~
               at (19,42), fac(hex(84)),   status$(2%)          , ch(07),~
          at (18,52), "Entered: MM/DD/YY by",                            ~
               at (18,61), fac(hex(84)),   entered$             , ch(08),~
               at (18,73), fac(hex(84)),   userin$              , ch(03),~
               at (19,50), fac(hex(8c)),   changemsg$           , ch(22),~
               at (19,61), fac(hex(84)),   lastmod$             , ch(08),~
               at (19,73), fac(hex(84)),   usermod$             , ch(03),~
                                                                         ~
                                                                         ~
          at (21,02), fac(hex(a4)),   inpmessage$               , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(str(pfkeys$)), key(keyhit%)

               if keyhit% <> 8 then L42670
                  gosub hdr1_redisplay
                  goto L42110

L42670:        if keyhit% <> 13 then L42692
                  call "MANUAL" ("BCKDSPLY")
                  goto L42110

L42692:        if keyhit% <> 10 then L42710
                  call "SOSTATUS" (shipto$, so$, " ", 0%, #6, #16, #17,  ~
                              #5, #18, #19, #3, #9, #10, #14, 1%, hflag%,~
                              #21, #22, #23, #24, #25, #26, #27, #28,    ~
                              #29, #30, #31, #32, #33, #34, #35)
                  goto L42110

L42710:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L42110

        setpf11
           inpmessage$ = " "
           if curr$ = "N" or curr_code$ = statutory$ then                ~
           pf$(1) = "(2)Line Items                             (14)Prin"&~
                    "t Order      (13)Instructions"                      ~
               else                                                      ~
           pf$(1) = "(2)Line Items     (8)Transaction Currency (14)Prin"&~
                    "t Order      (13)Instructions"
           pf$(2) = "                  (9)See Variable Fields  (25)Disp"&~
                    "lay Text     (15)Print Screen"
           pf$(3) = "(5)Header Page 2  (10)Order Status        (26)Cust"&~
                    " Credit      (16)Exit Display"
           pfkeys$ = hex(ff02ffff05ffff08090affff0d0e0f10191a20ff)
           if curr$ = "N" or curr_code$ = statutory$ then                ~
                str(pfkeys$, 8,1) = hex(ff)
           if vf$ <> " " then L42860
                str(pf$(2),19,24) = " " : str(pfkeys$, 9,1) = hex(ff)
L42860:    if textid$ <> " " then L42880
                str(pf$(2),43,18) = " " : str(pfkeys$,17,1) = hex(ff)
L42880:    return


        REM *************************************************************~
            *               D E T A I L   H D R   2                     *~
            *-----------------------------------------------------------*~
            * Order Detail- Hdr Page 2.                                 *~
            *************************************************************

        deffn'112
            gosub setpf12

L43090:     accept                                                       ~
               at (01,02), "Sales Order Inquiry: ORDER DETAIL   Hdr2",   ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Store",                                      ~
               at (04,30), fac(hex(84)),   str$                 , ch(30),~
               at (04,49), fac(hex(84)),   strdescr$            , ch(30),~
               at (05,02), "How Ship",                                   ~
               at (05,30), fac(hex(84)),   howship$             , ch(20),~
               at (06,02), "FOB",                                        ~
               at (06,30), fac(hex(84)),   fob$                 , ch(20),~
               at (07,02), "Shipping Instructions",                      ~
               at (07,30), fac(hex(84)),   shpinstr$(1)         , ch(50),~
               at (08,30), fac(hex(84)),   shpinstr$(2)         , ch(50),~
               at (09,02), "Price Code",                                 ~
               at (09,30), fac(hex(84)),   pc$                  , ch(01),~
               at (09,49), fac(hex(84)),   pcdescr$             , ch(30),~
               at (10,02), "Payment Terms Code",                         ~
               at (10,30), fac(hex(84)),   terms$               , ch(20),~
                                                                         ~
               at (12,02), "Sales Region",                               ~
               at (12,30), fac(hex(84)),   region$              , ch(04),~
               at (12,49), fac(hex(84)),   regiondescr$         , ch(30),~
               at (13,02), "Salesmen / Split %",                         ~
               at (13,30), fac(hex(84)),   salesman$(1)         , ch(04),~
               at (13,36), fac(hex(84)),   comm$(1)             , ch(03),~
               at (13,49), fac(hex(84)),   salesmandescr$(1)    , ch(30),~
               at (14,30), fac(hex(84)),   salesman$(2)         , ch(04),~
               at (14,36), fac(hex(84)),   comm$(2)             , ch(03),~
               at (14,49), fac(hex(84)),   salesmandescr$(2)    , ch(30),~
               at (15,30), fac(hex(84)),   salesman$(3)         , ch(04),~
               at (15,36), fac(hex(84)),   comm$(3)             , ch(03),~
               at (15,49), fac(hex(84)),   salesmandescr$(3)    , ch(30),~
                                                                         ~
               at (17,02), "Sales Distr Default",                        ~
               at (17,30), fac(hex(84)),   salesacct$           , ch(12),~
               at (17,49), fac(hex(84)),   salesacctdescr$      , ch(30),~
               at (18,02), "Sales Discs Account",                        ~
               at (18,30), fac(hex(84)),   discacct$            , ch(12),~
               at (18,49), fac(hex(84)),   discacctdescr$       , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(str(pfkeys$)), key(keyhit%)

               if keyhit% <> 13 then L43612
                  call "MANUAL" ("BCKDSPLY")
                  goto L43090

L43612:        if keyhit% <> 10 then L43630
                  call "SOSTATUS" (shipto$, so$, " ", 0%, #6, #16, #17,  ~
                              #5, #18, #19, #3, #9, #10, #14, 1%, hflag%,~
                              #21, #22, #23, #24, #25, #26, #27, #28,    ~
                              #29, #30, #31, #32, #33, #34, #35)
                  goto L43090

L43630:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L43090

        setpf12
           inpmessage$ = " "
           pf$(1) = "(2)Line Items                             (14)Prin"&~
                    "t Order      (13)Instructions"
           pf$(2) = "(4)Header Page 1  (9)See Variable Fields  (25)Disp"&~
                    "lay Text     (15)Print Screen"
           pf$(3) = "                  (10)Order Status        (26)Cust"&~
                    " Credit      (16)Exit Display"
           pfkeys$ = hex(ff02ff04ffffffff090affff0d0e0f10191a20ff)
           if vf$ <> " " then L43780
                str(pf$(2),19,24) = " " : str(pfkeys$, 9,1) = hex(ff)
L43780:    if textid$ <> " " then L43800
                str(pf$(2),43,18) = " " : str(pfkeys$,17,1) = hex(ff)
L43800:    return


        REM *************************************************************~
            *           L I N E   I T E M   S U M M A R Y               *~
            *-----------------------------------------------------------*~
            * Order Listing - Line Item Summary Screen.                 *~
            *************************************************************

        deffn'120
            if tgl% = 2% or tgl% = 4% then L45068
                hdr3$(1%) = "Seq" :  hdr3$(4%) = "Crnt Order"
                goto L45080
L45068:     hdr3$(1%) = "Pty" :  hdr3$(4%) = "Orig Order"

L45080:     hdr3$(2%) = "Part Number" :  hdr3$(5%) = "  Open Qty"
            hdr3$(3%) = "Part Description"

            gosub setpf20

L45130:     accept                                                       ~
               at (01,02), "Sales Order Inquiry: LINE ITEM SUMMARY",     ~
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
               at (05,02), fac(hex(80)),  lines$(t% +  1%, tgl%), ch(79),~
               at (05,02), fac(hex(86)),  lines$(t% +  1%, tgl%), ch(79),~
               at (06,02), fac(hex(86)),  lines$(t% +  2%, tgl%), ch(79),~
               at (07,02), fac(hex(86)),  lines$(t% +  3%, tgl%), ch(79),~
               at (08,02), fac(hex(86)),  lines$(t% +  4%, tgl%), ch(79),~
               at (09,02), fac(hex(86)),  lines$(t% +  5%, tgl%), ch(79),~
               at (10,02), fac(hex(86)),  lines$(t% +  6%, tgl%), ch(79),~
               at (11,02), fac(hex(86)),  lines$(t% +  7%, tgl%), ch(79),~
               at (12,02), fac(hex(86)),  lines$(t% +  8%, tgl%), ch(79),~
               at (13,02), fac(hex(86)),  lines$(t% +  9%, tgl%), ch(79),~
               at (14,02), fac(hex(86)),  lines$(t% + 10%, tgl%), ch(79),~
               at (15,02), fac(hex(86)),  lines$(t% + 11%, tgl%), ch(79),~
               at (16,02), fac(hex(86)),  lines$(t% + 12%, tgl%), ch(79),~
               at (17,02), fac(hex(86)),  lines$(t% + 13%, tgl%), ch(79),~
               at (18,02), fac(hex(86)),  lines$(t% + 14%, tgl%), ch(79),~
               at (19,02), fac(hex(86)),  lines$(t% + 15%, tgl%), ch(79),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(str(pfkeys$)), key(keyhit%)

               if keyhit% <> 8% then L45500
                  if tgl% <> 1% then L45483
                      tgl% = 2%  :  goto L45488
L45483:           if tgl% <> 2% then L45485
                      tgl% = 1%  :  goto L45488
L45485:           if tgl% <> 3% then L45487
                      tgl% = 4%  :  goto L45488
L45487:           tgl% = 3%
L45488:           if tgl% = 1% or tgl% = 3% then L45492
                      hdr3$(1%) = "Pty"
                      hdr3$(4%) = "Orig Order"
                      goto L45130
L45492:            hdr3$(1%) = "Seq"
                   hdr3$(4%) = "Crnt Order"
                   goto L45130

L45500:        if keyhit% <> 13 then L45540
                  call "MANUAL" ("BCKDSPLY")
                  goto L45130

L45540:        if keyhit% <> 15% then L45560
                  call "PRNTSCRN"
                  goto L45130

L45560:        if keyhit% <> 22% then L45585
                  if tgl% <> 1% then L45564
                      tgl% = 3%  :  goto L45569
L45564:           if tgl% <> 2% then L45566
                      tgl% = 4%  :  goto L45569
L45566:           if tgl% <> 3% then L45568
                      tgl% = 1%  :  goto L45569
L45568:           tgl% = 2%
L45569:           if tgl% > 2% then L45572
                      hdr3$(2%) = "Part Number"
                      hdr3$(3%) = "Part Description" : goto L45130
L45572:           hdr3$(2%) = "Customer Part Number"
                  hdr3$(3%) = "Cust Part Description"
                  goto L45130

L45585:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        setpf20
           inpmessage$ = "Position Cursor and Press RETURN to see"  &    ~
                         " Line Item Details."
           pf$(1) = "(2)First   (5)Next    (8)Seq/Pty Cur/Orig (11)Opti"&~
                    "on List      (13)Instructions"
           pf$(2) = "(3)Last    (6)Down    (9)See ATC          (14)Prin"&~
                    "t Order      (15)Print Screen"
           pf$(3) = "(4)Prev    (7)Up     (10)Order Status     (22)Part"&~
                    " Toggle   (16/32)Exit Display"
           pfkeys$ = hex(ff02030405060708090a0bff0d0e0f10ffff201600)
           if refflag% = 1% then L45720
                str(pf$(3%),43%,18%) = " "
                str(pfkeys$,20%,1%)  = hex(ff)
L45720:    if t% <> 0% then L45760
                str(pf$(1),,8), str(pf$(3),,16), str(pf$(2),12,7) = " "
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
            cfac1$, cfac2$ = hex(9c)
            gosub setpf21
            if curr$ = "N" or curr_code$ = statutory$ then L46150
                cfac1$    = hex(8c)
                cfac2$    = hex(84)
                curlabel$ = "Currency"
                curcode$  = statutory$
                curdescr$ = stat_curdescr$
                if tran_display$ = "N" then tran_display$ = "Y" else     ~
                   tran_display$ = "N" /* Set up to reverse below */
                gosub line_redisplay    /* Reverses Stat & Tran */
L46140:
L46150:     accept                                                       ~
               at (01,02), "Sales Order Inquiry: LINE DETAIL",           ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Part Code",                                  ~
               at (05,30), fac(hex(84)),   part$                , ch(25),~
               at (05,64), "Line Sequence",                              ~
               at (05,78), fac(hex(84)),   seq$                 , ch(03),~
               at (06,02), "Part/Line Description",                      ~
               at (06,30), fac(hex(84)),   descr$               , ch(32),~
               at (06,64), "PO Line Ref  ",                              ~
               at (06,78), fac(hex(84)),   item$                , ch(03),~
               at (07,02), "Part Category",                              ~
               at (07,30), fac(hex(84)),   cat$                 , ch(04),~
               at (07,49), fac(hex(84)),   catdescr$            , ch(30),~
                                                                         ~
               at (08,02), "Due and Ship Dates",                         ~
               at (08,30), fac(hex(84)),   duedate$             , ch(08),~
               at (08,40), fac(hex(84)),   shipdate$            , ch(08),~
               at (08,49), "Original Due Date:",                         ~
               at (08,68), fac(hex(84)),   origdue$             , ch(08),~
                                                                         ~
               at (09,02), "Quantity Ordered",                           ~
               at (09,30), fac(hex(84)),   order$               , ch(10),~
               at (09,42), fac(hex(84)),   stkuom$              , ch(04),~
               at (09,49), "Lot",                                        ~
               at (09,53), fac(hex(84)),   lot$                 , ch(06),~
               at (09,60), "Alloc",                                      ~
               at (09,67), fac(hex(84)),   alloc$               , ch(10),~
               at (09,79), fac(hex(84)),   allocflag$           , ch(01),~
                                                                         ~
               at (10,02), "Open Quantity Remaining",                    ~
               at (10,30), fac(hex(84)),   openqty$             , ch(10),~
               at (10,49), "Quantity Scheduled",                         ~
               at (10,71), fac(hex(84)),   schdl$               , ch(10),~
               at (11,02), "Remaining to Ship",                          ~
               at (11,30), fac(hex(84)),   to_ship$             , ch(10),~
               at (11,49), "Shipped NOT Invoiced",                       ~
               at (11,71), fac(hex(84)),   preinv$              , ch(10),~
                                                                         ~
               at (12,49), "Shipped AND Invoiced",                       ~
               at (12,71), fac(hex(84)),   ship$                , ch(10),~
                                                                         ~
               at (13,02), fac(cfac1$),    curlabel$            , ch(10),~
               at (13,36), fac(cfac2$),    curcode$             , ch(04),~
               at (13,49), fac(cfac1$),    curdescr$            , ch(30),~
                                                                         ~
               at (14,02), "Unit Price Per xxxx",                        ~
               at (14,17), fac(hex(84)),   priceuom$            , ch(04),~
               at (14,30), fac(hex(84)),   price$               , ch(10),~
               at (14,49), "UOM Conversion",                             ~
               at (14,64), fac(hex(84)),   conv$                , ch(10),~
               at (15,02), "Line Item Discount",                         ~
               at (15,30), fac(hex(84)),   linediscamt$         , ch(10),~
               at (15,49), "Discount %    ",                             ~
               at (15,64), fac(hex(84)),   linediscpct$         , ch(10),~
               at (16,02), "Line Item Extension",                        ~
               at (16,30), fac(hex(84)),   lineext$             , ch(10),~
               at (16,49), fac(hex(84)),   taxable$             , ch(07),~
                                                                         ~
               at (17,02), "Last Invoice Number",                        ~
               at (17,30), fac(hex(84)),   lastinv$             , ch(08),~
               at (17,49), "Demand Type ",                               ~
               at (17,61), fac(hex(84)),   demtype$             , ch(01),~
               at (17,64), "Priority ",                                  ~
               at (17,73), fac(hex(84)),   demprty$             , ch(01),~
               at (18,02), "Project Number",                             ~
               at (18,30), fac(hex(84)),   project$             , ch(08),~
               at (18,49), fac(hex(84)),   projectdescr$        , ch(30),~
               at (19,02), "Sales Distribution Acct",                    ~
               at (19,30), fac(hex(84)),   sales$               , ch(12),~
               at (19,49), fac(hex(84)),   salesdescr$          , ch(30),~
               at (20,02), "Sales Discounts Account",                    ~
               at (20,30), fac(hex(84)),   discs$               , ch(12),~
               at (20,49), fac(hex(84)),   discsdescr$          , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(str(pfkeys$)), key(keyhit%)

               if keyhit% <> 8 then L47010
                  gosub line_redisplay
                  goto L46140

L47010:        if keyhit% <> 13 then L47050
                  call "MANUAL" ("BCKDSPLY")
                  goto L46150

L47050:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L46150

        setpf21
           inpmessage$ = " "
           if curr$ = "N" or curr_code$ = statutory$ then                ~
           pf$(1) = "(6)Prev Line                              (11)Opti"&~
                    "on List      (13)Instructions"                      ~
               else                                                      ~
           pf$(1) = "(6)Prev Line     (8)Transaction Currency  (11)Opti"&~
                   "on List       (13)Instructions"
           pf$(2) = "(7)Next Line     (9)See ATC               (14)Prin"&~
                    "t Order      (15)Print Screen"
           pf$(3) = "                (10)Order Status          (26)Disp"&~
                    "lay Text  (32/16)Exit Display"
           pfkeys$ = hex(ffffffffff060708090a0bff0d0e0f101aff20ff)
           if curr$ = "N" or curr_code$ = statutory$ then                ~
                str(pfkeys$, 8,1) = hex(ff)
           if c% > 1% then L47260
                str(pf$(1), 1,12) = " " : str(pfkeys$, 6,1) = hex(ff)
L47260:    if c% < maxlines% then L47280
                str(pf$(2), 1,12) = " " : str(pfkeys$, 7,1) = hex(ff)
L47280
*         IF TEXTIDL$ <> " " THEN 46960
*              STR(PF$(3),43,18) = " " : STR(PFKEYS$,17,1) = HEX(FF)
           return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

            deffn'150(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50130          /* Ship-to Number   */
                  return

L50130
*        Ship-to Number
            shiptoname$ = hex(06) & "Select Ship-to Customer"
            call "GETCODE" (#3, shipto$, shiptoname$, 0%, 1.30, f1%(3))
            if f1%(3) = 1% then L50200
                errormsg$ = "Ship-to Number not on file." : return
L50200:     plowkey$ = str(shipto$) & hex(00)
            call "PLOWNEXT" (#5, plowkey$, 9%, f1%(5))
            if f1%(5) = 1% then return

            if xinflag% = 0% then L50230
            plowkey$ = str(shipto$) & hex(00)
            call "PLOWNEXT" (#9, plowkey$, 9%, f1%(9))
            if f1%(9) = 1% then return

L50230:         errormsg$ = "No orders on file for this Ship-to."
                return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'152(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L51150,         /* Customer Number  */~
                                    L51210,         /* Sales Order Nmbr */~
                                    L51270,         /* Order Date       */~
                                    L51390,         /* Print Text?      */~
                                    L51430,         /* History?         */~
                                    L51440,         /* Print Opn or Clsd*/~
                                    L51460          /* Open Qty Based on*/
                  return

L51150
*        Customer Range                        CO_RANGE$
            call "TESTRNGE" (co_range$(1), co_range$(2),                 ~
                             co_range$(3), co_range$(4), errormsg$)
            return

L51210
*        Order Range                           SO_RANGE$()
            call "TESTRNGE" (so_range$(1), so_range$(2),                 ~
                             so_range$(3), so_range$(4), errormsg$)
            return

L51270
*        Order Date                            OD_RANGE$
        REM Test Data for Received Date Range
                if od_range$(1) <> "ALL" then L51290
                   od_range$(2) = " " : return
L51290:         call "DATEOK" (od_range$(1), bdate%, errormsg$)
                if errormsg$ <> " " then return
                if od_range$(2%) = " " or od_range$(2%) = blankdate$ then ~
                                          od_range$(2%) = od_range$(1%)
                call "DATEOK" (od_range$(2), edate%, errormsg$)
                if errormsg$ > " " then return
                if edate% >= bdate% then L51381
                errormsg$ = "Ending Date Cannot be less than "     &     ~
                            "Beginning Date"
                return

L51381:     REM Unformat OD_RANGE$(1) & OD_RANGE$(2)
            od_range$(3) = od_range$(1) : call "DATUNFMT" (od_range$(3))
            od_range$(4) = od_range$(2) : call "DATUNFMT" (od_range$(4))
            return

L51390
*        Print Text?                           PRINT_TEXT$
            if print_text$= "Y" or print_text$ = "N" then return
                errormsg$ = "Enter 'Y' or 'N'."
                return

L51430
*        Include History Files?                 HIST$
            if hist$= "Y" or hist$ = "N" then return
                errormsg$ = "Enter 'Y' or 'N'."
                return

L51440
*        Print Open, Closed, or Both?           PRINT_HOW$
            if pos("OCB" = print_how$) <> 0% then L51448
                errormsg$ = "Please enter 'O', 'C', or 'B'."
                return
L51448:     if print_how$ = "O" then howrptmsg$ = "Open Orders Only"
            if print_how$ = "C" then howrptmsg$ = "Closed Orders Only"
            if print_how$ = "B" then howrptmsg$ = "Open & Closed Orders"
            return

L51460
*        Base Open Qtys on Invoicing or Shipping QTY_BASED_ON$
            if pos("IS" = qty_based_on$) <> 0% then L51480
                errormsg$ = "Invalid Code; Enter 'I' or 'S'."
                return
L51480:     qty_based_msg$(1%) = "Invoicing"
            qty_based_msg$(2%) = "Invoice"
            if qty_based_on$ = "I" then return
                qty_based_msg$(1%) = " Shipping"
                qty_based_msg$(2%) = "  Ship "
            return

        REM *************************************************************~
            *           V A L I D A T E     D A T A                     *~
            *-----------------------------------------------------------*~
            * Validate data before printing report line.                *~
            *************************************************************

        valid_data
            errormsg$ = " "
            no% = 0%

*        Order Range  -  Validate Sales Order Number Off File
            if so_range$(1) = "ALL" then L51700
                if so$ < so_range$(1) or so$ > so_range$(2) then L51760

*        Order Date  -  Validate Sales Order Date Off File
L51700:     if od_range$(1) = "ALL" then L51740
                sodatef$ = sodate$ : call "DATUNFMT" (sodatef$)
                if sodatef$ < od_range$(3) or sodatef$ > od_range$(4)    ~
                   then L51760

L51740:     return

L51760:     no% = 1%
            return

*       ****************************************************************~
*         Preparation for Re-Display in transaction or statutory       *~
*       ****************************************************************~

        hdr1_redisplay
            if tran_display$ = "Y" then L51940
*       Re-display HDR1 screen using transaction values
            tran_display$ = "Y"
          pf$(1) = "(2)Line Items     (8)Statutory Currency   (14)Print"&~
                   " Order      (13)Instructions"
            gross_order = tran_gross_order
            disc_order  = tran_disc_order
            net_order   = tran_net_order
            gross_open  = tran_gross_open
            disc_open   = tran_disc_open
            net_open    = tran_net_open
            curdescr$   = tran_curdescr$
            cfac3$      = hex(84)
            return
*       Re-display HDR1 screen using Statutory values.
L51940:     tran_display$ = "N"
          pf$(1) = "(2)Line Items     (8)Transaction Currency (14)Print"&~
                   " Order      (13)Instructions"
            gross_order = stat_gross_order
            disc_order  = stat_disc_order
            net_order   = stat_net_order
            gross_open  = stat_gross_open
            disc_open   = stat_disc_open
            net_open    = stat_net_open
            curdescr$   = stat_curdescr$
            cfac3$      = hex(9c)
            return

        line_redisplay
            if tran_display$ = "Y" then L52200
*       Re-display LINE screen using transaction values.
            tran_display$ = "Y"
          pf$(1) = "(6)Prev Line     (8)Statutory Currency    (11)Optio"&~
                   "n List      (13)Instructions"
            if c% > 1% then L52110
                str(pf$(1),1,12) = " " : str(pfkeys$,6,1) = hex(ff)
L52110:     price$       = tran_price$
            linediscamt$ = tran_linediscamt$
            lineext$     = tran_lineext$
            cfac1$       = hex(8c)
            cfac2$       = hex(84)
            curcode$     = curr_code$
            curdescr$    = tran_curdescr$
            return
*       Re-display LINE screen using statutory values.
L52200:     tran_display$ = "N"
          pf$(1) = "(6)Prev Line     (8)Transaction Currency  (11)Optio"&~
                   "n List      (13)Instructions"
            if c% > 1% then L52230
                str(pf$(1),1,12) = " " : str(pfkeys$,6,1) = hex(ff)
L52230:     price$       = stat_price$
            linediscamt$ = stat_linediscamt$
            lineext$     = stat_lineext$
            cfac1$       = hex(8c)
            cfac2$       = hex(84)
            curcode$     = statutory$
            curdescr$    = stat_curdescr$
            return

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
            call "SHOSTAT" ("One Moment Please")
            if workflag1% = 1% then call "FILEBGON" (#50)
            if workflag2% = 1% then call "FILEBGON" (#51)
            end
