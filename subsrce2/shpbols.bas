        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS   H   H  PPPP   BBBB    OOO   L       SSS           *~
            *  S      H   H  P   P  B   B  O   O  L      S              *~
            *   SSS   HHHHH  PPPP   BBBB   O   O  L       SSS           *~
            *      S  H   H  P      B   B  O   O  L          S          *~
            *   SSS   H   H  P      BBBB    OOO   LLLLL   SSS           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SHPBOLS  - Prints Bills Of Lading Using The Criteria      *~
            *            Passed From The Caller (SHPDOCPR).             *~
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
            * 09/24/86 ! Original                                 ! JIM *~
            * 01/28/87 ! Skip over if on credit hold              ! ERN *~
            * 05/06/87 ! Added Serial Number printing by Lot      ! JIM *~
            * 08/11/87 ! HES's SO Polymer customization           ! JIM *~
            * 10/18/88 ! Increased DIM for F2%, etc. to 64        ! JDH *~
            * 02/21/89 ! Formatted ZIP Code as 12345-xxxx         ! MJB *~
            * 08/16/89 ! Added package qtys/types to packing slip.! JDH *~
            * 09/22/89 ! Minor tweek of ZIP format.               ! JDH *~
            * 09/25/89 ! Fixed performance after scheduling.      ! JDH *~
            * 02/01/90 ! Already invoiced lines no longer printed.! JDH *~
            * 05/23/90 ! Qty Remaining calculated differently.    ! JDH *~
            * 09/26/90 ! Added shipping instructions & 'How Ship',! JDH *~
            *          !   Chngd store name from descriptive to   !     *~
            *          !   address name. PRR 11088, 11673, 11671. !     *~
            * 06/14/91 ! Added Shipping Label function (SHPLABSB).! JIM *~
            * 06/20/91 ! PRR 12056.  No more SHOSTAT at end.      ! JDH *~
            * 12/18/91 ! Added TEXT Printing.  Valid Sources are  ! LDJ *~
            *          ! PART & BOM Header.  (Uses effective BOM  !     *~
            *          ! based on current due date if there is an !     *~
            *          ! effective bill, otherwise uses 1st bill  !     *~
            *          ! on file for part.                        !     *~
            * 01/06/92 ! Corrected TXTPRINT width argument.       ! JDH *~
            * 02/21/92 ! PRR 12315 Added RptID to call to TXTPRINT! JDH *~
            *          !   & adjust COM statement.                !     *~
            *          ! PRR 12023  Added SO Text to BOL.         !     *~
            * 10/12/92 ! PRR 12589 serial # printing fix.         ! JDH *~
            * 10/21/92 ! Removed FACs from page zero.             ! JIM *~
            * 10/22/92 ! Changed COMs to DIMs & added Arguments.  ! JDH *~
            * 06/28/94 ! PRR 13212. No text if line isn't printed.! JDH *~
            * 03/13/95 ! Customer X-Ref Number Printing.          ! RJH *~
            * 07/07/95 ! Added ability to print customer text.    ! JDH *~
            * 07/22/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

            sub "SHPBOLS" (date$, from_date%, from_stor$, i$(),          ~
                           low_date$, print_labels$, so$(), to_date%,    ~
                           to_stor$, #3, #4, #5, #6, #7)

        REM *************************************************************~
            *THIS SUBROUTINE IS CALLED BY 'SHPDOCPR'. IT PRINTS BILLS OF*~
            *LADING BASED ON THE CRITERIA PASSED.                       *~
            *************************************************************

        dim        /* OLD COM VARIABLES                                */~
            bcklines_key$19,             /* Key to BCKLINES            */~
            bckmastr_key$25,             /* Key to BCKMASTR            */~
            bol$3,                       /* Bill of lading # (BCKPRIDX)*/~
            carton$7,                    /* Edited # of cartons        */~
            cust_code$9,                 /* Customer code from BCKPRIDX*/~
            date$8,                      /* Date for screen display    */~
            from_stor$3,                 /* Low store for compare      */~
            hdr$40,                      /* ASKUSER constant           */~
            hdr_desc$30,                 /* Header description         */~
            i$(24)80,                    /* Screen image               */~
            invoice$8,                   /* Invoice # from SHPHDRS     */~
            low_date$8,                  /* Low date for compare       */~
            msg$(3)79,                   /* ASKUSER constant           */~
            part_nmbr$25,                /* Part # from BCKLINES       */~
            part_desc$32,                /* Part desc from BCKLINES    */~
            po$16,                       /* PO # from BCKMASTR         */~
            po_line$3,                   /* PO line #                  */~
            print_labels$1,              /* Print Shipping Labels?     */~
            qtyship$10,                  /* Edited quantity shipped    */~
            schd_date$8,                 /* Sched ship date fr SHPHDRS */~
            ship_date$8,                 /* Ship date from BCKPRIDX    */~
            ship_to$(6)31,               /* Ship to name/addr- BCKMASTR*/~
            shphdrs_key$28,              /* Key to SHPHDRS             */~
            shplines_key$22,             /* Key to SHPLINES            */~
            so$16,                       /* Sales order # from BCKPRIDX*/~
            so$(2,2)16,                  /* Sales Order # Range For Pri*/~
            stock_uom$4,                 /* Stock UOM from BCKMASTR    */~
            stor_code$3,                 /* Store code from BCKPRIDX   */~
            tdate$10,                    /* Temporary Date             */~
            to_stor$3,                   /* High store for compare     */~
            type$1,                      /* 'A'=Ackn, 'B'=BOL, 'P'-Pick*/~
            weight$6,                    /* Edited shipment weight     */~
                   /* COMMON DATA FILE ARRAYS                          */~
            f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20                  /* Text from file opening     */

        dim        /* LOCAL VARIABLES                                  */~
            act_date$6,                  /* Actual ship date - SHPHDRS */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            bom$(490)3,                  /* Effective BOM's            */~
            bomid$3,                     /* BOM Rev ID                 */~
            brk_stor$3,                  /* Controls store breaks      */~
            carr_code$6,                 /* Carrier code from SHPHDRS  */~
            carr_name$30,                /* Carrier name               */~
            crhold$1,                    /* Credit Hold Flag           */~
            due_curr$6,                  /* Current Due Date from BCKLI*/~
            fob$20,                      /* FOB from BCKMASTR          */~
            howship$20,                  /* How Ship from BCKMASTR/SHPH*/~
            instr$(2)50,                 /* Shipping instructions      */~
            lot$(30)6,                   /* Lot #s from xxxLINES       */~
            package_qty$4,               /* Number of package units    */~
            package_type$8,              /* Type of package unit       */~
            part_type$3,                 /* Part Type (Purch or Mfg)   */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            prog$8,                      /* Program name               */~
            qty_ship(30),                /* Qty shipped from xxxLINES  */~
            readkey$64,                  /* Miscell Read variable      */~
            readkey2$64,                 /* Miscell Read variable      */~
            rptid$6,                     /* Report ID for headers      */~
            rs$2,                        /* Check for 'RS' in SERMASTR */~
            ser$6,                       /* Serial # literal           */~
            serbol$3,                    /* Check for BOL in SERMASTR  */~
            serprint$95,                 /* Serial # print area        */~
            serseq$3,                    /* Check for Seq in SERMASTR  */~
            serso$16,                    /* Check for SO in SERMASTR   */~
            serial$20,                   /* Serial # from SERMASTR     */~
            serkey$99,                   /* SERMASTR key               */~
            seq$3,                       /* SO sequence # from SHPLINES*/~
            so_seqnr$3,                  /* SO sequence # from BCKLINES*/~
            store$(4)30,                 /* Store name and address     */~
            txtid$4,                     /* Text ID Pointer BOM & Part */~
            txtid_bckl$4,                /* Text ID Pointer SO Lines   */~
            txtid_bckm$4,                /* Text ID Pointer SO Header  */~
            txtid_cust$4,                /* Text ID Pointer Customer   */~
            xref_cus$1,                  /* Print Customer Xref Part   */~
            xref_mnf$1,                  /* Print Manufactur Xref Part */~
            xref_descr$32,               /* Xref Part # Description    */~
            xref_part$25,                /* Xref Part Number           */~
            xref_type$1                  /* Xref Part Type(Cust or Mnf)*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

            mat f2% = con : mat f1% = zer

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! SYSFILE2 ! Caelus Management System Information     *~
            * #2  ! ENGMASTR ! Engineering Master (BOM Effectivity File)*~
            * #3  ! BCKPRIDX ! SO Document Print Index File             *~
            * #4  ! BCKMASTR ! BACKLOG MASTER FILE (GET STORE NUMBER)   *~
            * #5  ! BCKLINES ! BACK LOG LINE ITEM FILE                  *~
            * #6  ! SHPHDRS  ! Shipment Scheduling / Pre-Invoicing- Hea *~
            * #7  ! SHPLINES ! Shipment Scheduling / Pre-Invoicing- Lin *~
            * #8  ! STORNAME ! Store Information File                   *~
            * #9  ! SERMASTR ! Serial Number Tracking Master File       *~
            * #10 ! GENCODES ! General Codes File                       *~
            * #13 ! HNYMASTR ! Inventory Master File                    *~
            * #14 ! TXTFILE  ! System Text File                         *~
            * #15 ! BOMMASTR ! Bill of Material Master File             *~
            * #16 ! CUSTOMER ! Customer Master File                     *~
            *************************************************************~

            select #1,  "SYSFILE2",                                      ~
                        varc,     indexed,  recsize = 500,               ~
                        keypos =    1, keylen =  20

            select #2,  "ENGMASTR",                                      ~
                        varc,     indexed,  recsize =2015,               ~
                        keypos =    1, keylen =  29

            select #8, "STORNAME", varc, indexed, recsize = 300,         ~
                keypos = 1, keylen = 3

            select #9, "SERMASTR", varc, indexed, recsize = 300,         ~
                keypos = 52, keylen = 45,                                ~
                alt key  1, keypos = 32, keylen = 45,                    ~
                    key  2, keypos =  1, keylen = 76

            select #10, "GENCODES", varc, indexed, recsize = 128,        ~
                keypos = 1, keylen = 24

            select #13, "HNYMASTR", varc, indexed, recsize = 900,        ~
                        keypos =   1, keylen =  25,                      ~
                    alt key 1, keypos = 102, keylen =  9, dup,           ~
                        key 2, keypos =  90, keylen =  4, dup

            select #14, "TXTFILE",                                       ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =   1, keylen =  11

            select #15,  "BOMMASTR",                                     ~
                        varc,     indexed,  recsize = 150,               ~
                        keypos =  26,  keylen = 31,                      ~
                        alt key  1, keypos =   1, keylen =  56

            select #16, "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key 1,  keypos = 10,   keylen =  30,         ~
                            key 2,  keypos = 424,  keylen =   9,         ~
                            key 3,  keypos = 771,  keylen =   9,         ~
                            key 4,  keypos = 780,  keylen =   9

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)
            call "DATEFMT" (date$)

            call "OPENCHCK" (#1, fs%(1), f2%(1), 0%, rslt$(1))
            call "OPENCHCK" (#2, fs%(2), f2%(2), 0%, rslt$(2))
            call "OPENCHCK" (#8, fs%(8), f2%(8), 0%, rslt$(8))
            call "OPENCHCK" (#9, fs%(9), f2%(9), 0%, rslt$(9))
            call "OPENCHCK" (#10, fs%(10), f2%(10), 0%, rslt$(10))
            call "OPENCHCK" (#13, fs%(13), f2%(13), 0%, rslt$(13))
            call "OPENCHCK" (#14, fs%(14), f2%(14), 0%, rslt$(14))
            call "OPENCHCK" (#15, fs%(15), f2%(15), 0%, rslt$(15))
            call "OPENCHCK" (#16, fs%(16), f2%(16), 0%, rslt$(16))

            call "SHOSTAT" ("Bills of Lading being selected for print")
            rptid$ = "SHP004"
            call "EXTRACT" addr("CF", prog$)
            plowkey$ = "B" : str(plowkey$,2) = all(hex(00))
            nbr_recs% = 0% : max_lines% = 54%

*        Check if Cross Reference Parts are to be printed
            xref_cus$, xref_mnf$  = "N"
            call "READ100" (#01, "SWITCHS.BCK", f1%(01%))
            if f1%(01%) = 1% then get #01 using L09220, xref_cus$,xref_mnf$

L09220:         FMT POS(63), CH(1), CH(1)

        REM *************************************************************~
            *          M A I N   P R O G R A M   L O O P                *~
            *************************************************************

        plow_thru_bckpridx
            call "PLOWNXT1" (#3, plowkey$, 1%, f1%(3))
            if f1%(3) = 0% then goto end_of_report
            get #3 using L10110, stor_code$, ship_date$, type$,           ~
                cust_code$, so$, bol$

            gosub check_xref_print_settings  /* Also gets text id */

        REM RECORD LAYOUT FOR FILE 'BCKPRIDX' ***************************
L10110:         FMT  XX(1),              /* 'A'ck, 'B'ol, or 'P'ick    */~
                     CH(3),              /* Store code                 */~
                     CH(6),              /* Ship date                  */~
                     CH(1),              /* Type code (A,B,P) again    */~
                     CH(9),              /* Customer code              */~
                     CH(16),             /* Sales order number         */~
                     CH(3),              /* Bill of lading number      */~
                     XX(6)               /* Setup date                 */

            if stor_code$ < from_stor$ then goto plow_thru_bckpridx
            if stor_code$ > to_stor$ then goto plow_thru_bckpridx
            if so$ <= so$(2,1) or so$ > so$(2,2) then plow_thru_bckpridx
            if low_date$ = "ALL" then goto L10310
            ship_date% = 0%
            tdate$ = ship_date$
            call "DATEFMT" (tdate$, ship_date%)

            if low_date$ <> " " and low_date$ <> blankdate$ then goto L10280
            if ship_date$ = " " or ship_date$ = blankdate$ then goto L10310
            goto L10290
L10280:     if ship_date% < from_date% then goto plow_thru_bckpridx
L10290:     if ship_date% > to_date% then goto plow_thru_bckpridx

L10310:     bckmastr_key$ = str(cust_code$,,9) & so$
            call "READ100" (#4, bckmastr_key$, f1%(4))
            if f1%(4) = 0% then goto bckpridx_deleter
            get #4 using L10370, po$, ship_to$(), howship$,fob$, instr$(),~
                                txtid_bckm$, crhold$
            if crhold$ = "H" then plow_thru_bckpridx

        REM RECORD LAYOUT FOR FILE 'BCKMASTR' ***************************
L10370:         FMT  XX(9),              /* Customer code              */~
                     XX(16),             /* Sales Order number         */~
                     CH(16),             /* Purchase Order number      */~
                     6*CH(30),           /* Ship-to name & address     */~
                     XX(180),            /* Sold-to name & address     */~
                     XX(20),             /* Terms code                 */~
                     CH(20),             /* How to ship                */~
                     CH(20),             /* FOB                        */~
                     2*CH(50),           /* Shipping instructions      */~
                     XX(9),              /* Sales account number       */~
                     XX(9),              /* Discount account number    */~
                     XX(12),             /* Salesman codes             */~
                     XX(3),              /* Commission split           */~
                     XX(4),              /* Region code                */~
                     XX(200),            /* Variable fields            */~
                     CH(4),              /* Text ID                    */~
                     XX(3),              /* Store code                 */~
                     XX(6),              /* Order date (original)      */~
                     XX(6),              /* Cancel date                */~
                     XX(6),              /* Default due date           */~
                     XX(6),              /* Ship date                  */~
                     XX(6),              /* Date originally entered    */~
                     XX(3),              /* User ID                    */~
                     XX(6),              /* Date last changed          */~
                     XX(3),              /* User ID                    */~
                     XX(9),              /* Reason code for adjustment */~
                     XX(1),              /* Sales analysis period      */~
                     XX(1),              /* Price code                 */~
                     XX(8),              /* Discount percent           */~
                     XX(8),              /* Gross open amount          */~
                     CH(1),              /* Credit hold flag           */~
                     XX(2),              /* General sequence number    */~
                     XX(4),              /* Next BOL number            */~
                     XX(2),              /* Customer type              */~
                     XX(9),              /* Account cross-reference    */~
                     XX(108)             /* Filler                     */

            if nbr_recs% = 0% then gosub one_time_routine
            gosub store_routine /* GET STORE NAME & ADDR */
            carton, weight = 0 : mat qty_ship = zer
            init (" ") carr_code$, carr_name$, carton$, weight$, lot$(), ~
                qtyship$, schd_date$, hdr_desc$, package_qty$,           ~
                package_type$
            if str(ship_to$(6),17,1) <> " "                              ~
                or str(ship_to$(6),16,1) <> " "                          ~
                or pos(str(ship_to$(6),27,4) = " ") > 0% then L10790
                  temp$ = str(ship_to$(6),27,4)
                  str(ship_to$(6),28,4) = temp$
                  str(ship_to$(6),27,1) = "-"
L10790:     call "SPCESMSH" (ship_to$(6), 2%)
            call "LINSMASH" (ship_to$())
            if bol$ = " " then goto print_bol_headers

            shphdrs_key$ = str(cust_code$,,9) & str(so$,,16) & bol$
            call "READ100" (#6, shphdrs_key$, f1%(6))
            if f1%(6) = 0% then goto bckpridx_deleter
            get #6 using L10890, schd_date$, carr_code$, howship$, fob$,  ~
                            instr$(), act_date$, carton, weight, invoice$

        REM RECORD LAYOUT FOR FILE 'SHPHDRS' ****************************
L10890:         FMT  XX(9),              /* Customer code              */~
                     XX(16),             /* Sales Order number         */~
                     XX(3),              /* Bill of Lading #           */~
                     XX(3),              /* Store from Sales Order     */~
                     CH(6),              /* Scheduled Ship date        */~
                     CH(6),              /* Carrier code               */~
                     CH(20),             /* How to ship                */~
                     CH(20),             /* FOB                        */~
                     2*CH(50),           /* Shipping instructions      */~
                     CH(6),              /* Actual Ship date           */~
                     XX(20),             /* Freight Bill number        */~
                     PD(14, 4),          /* Number of Cartons          */~
                     PD(14, 4),          /* Shipment Weight            */~
                     XX(8),              /* Freight Amount             */~
                     CH(8),              /* Invoice number             */~
                     XX(59)              /* Filler                     */

            call "DATEFMT" (schd_date$)
            if carr_code$ = " " then goto skip_carrier
                call "DESCRIBE" (#10, str("CARRIERS ",,9) & carr_code$,  ~
                     carr_name$, 0%, f1%(10))
                if f1%(10) = 0% then carr_name$ = "UNKNOWN CARRIER"
        skip_carrier
            carton = round(carton, 0)
            weight = round(weight, 0)
            if carton <> 0 then convert carton to carton$, pic(###,###)
            if weight <> 0 then convert weight to weight$, pic(######)
        print_bol_headers
            if bol$ = " " then hdr_desc$ = "ORDER ENTRY"
            if bol$ <> " " and (act_date$ = " " or act_date$ = blankdate$) then~
                hdr_desc$ = "SHIPMENT SCHEDULING"
            if bol$ <> " " and act_date$ <> " " and act_date$ <> blankdate$ ~
                then hdr_desc$ = "SHIPPING"
            if hdr_desc$ = " " then hdr_desc$ = "*** UNKNOWN ***"
            page_nbr% = 0%
            gosub page_heading

            bcklines_key$ = so$
            str(bcklines_key$,17) = all(hex(00))
        bcklines_loop
            call "PLOWNEXT" (#5, bcklines_key$, 16%, f1%(5))
            if f1%(5) = 0% then goto end_of_lines
            get #5 using L11350, so_seqnr$, po_line$, part_nmbr$,         ~
                part_desc$, qty_ship(1), qty_open, qty_schd, qty_preinv, ~
                stock_uom$, due_curr$, lot$(1), txtid_bckl$

        REM RECORD LAYOUT FOR FILE 'BCKLINES' ***************************
L11350:         FMT  XX(9),              /* Customer code              */~
                     XX(16),             /* Sales Order number         */~
                     CH(3),              /* Sales Order sequence number*/~
                     CH(3),              /* Purchase Order line number */~
                     CH(25),             /* Part number                */~
                     CH(32),             /* Part description           */~
                     XX(4),              /* Category code              */~
                     XX(8),              /* Order quantity             */~
                     PD(14, 4),          /* Quantity shipped           */~
                     PD(14, 4),          /* Open order quantity        */~
                     PD(14, 4),          /* Quantity scheduled         */~
                     XX(8),              /* Filler                     */~
                     PD(14, 4),          /* Quantity Pre-Invoiced      */~
                     XX(8),              /* Unit price @ stocking UOM  */~
                     CH(4),              /* Stocking UOM               */~
                     XX(4),              /* Pricing UOM                */~
                     XX(8),              /* Conversion factor          */~
                     XX(8),              /* Unit price @ pricing UOM   */~
                     XX(8),              /* Discount percent           */~
                     XX(1),              /* Taxable code               */~
                     XX(9),              /* Sales account number       */~
                     XX(9),              /* Discount account number    */~
                     XX(6),              /* Original Due date          */~
                     CH(6),              /* Current Due date           */~
                     XX(6),              /* Ship date                  */~
                     CH(6),              /* Lot number                 */~
                     XX(10),             /* Filler                     */~
                     XX(6),              /* Project code               */~
                     XX(1),              /* Demand type                */~
                     XX(1),              /* Priority                   */~
                     CH(4),              /* Text ID                    */~
                     XX(1),              /* Allocation flag            */~
                     XX(8),              /* Invoice number             */~
                     XX(46)              /* Filler                     */

                FMT POS(24), PD(14,4), PD(14,4)

            /* Get Xref Part Number, if available */
            call "PTUSEDSB" ( "R", "BCK ",                               ~
                             str(bcklines_key$,,16%),  so_seqnr$,        ~
                             xref_part$, xref_descr$, xref_type$, ret%)
            if ret% = 0% then xref_part$,xref_descr$,xref_type$ = " "

            if bol$ = " " then goto print_detail_lines /* Prnt from BCK */
            shplines_key$ = str(key(#5),,16) & str(bol$,,3) &            ~
                str(key(#5),17,3)
            call "READ100" (#7, shplines_key$, f1%(7))
            if f1%(7) = 0% then goto bcklines_loop
            get #7 using L11770, seq$, qty_ship, lot$(), qty_ship(),      ~
                                package_qty$, package_type$

        REM RECORD LAYOUT FOR FILE 'SHPLINES' ***************************
L11770:         FMT  XX(9),              /* Customer code              */~
                     XX(16),             /* Sales Order number         */~
                     XX(3),              /* Bill of Lading #           */~
                     CH(3),              /* Sales Order line number    */~
                     PD(14, 4),          /* Quantity Scheduled         */~
                     30*CH(6),           /* Lot numbers                */~
                     30*PD(14, 4),       /* Quantity Shipped           */~
                     XX(4),              /* Text ID for Invoice Line   */~
                     CH(4),              /* Number of package units    */~
                     CH(8),              /* Type of package units      */~
                     XX(125)             /* Filler                     */

            if act_date$ <> " " and act_date$ <> blankdate$ then ~
                                     goto print_detail_lines /*Shipping*/
                mat qty_ship = zer : qty_ship(1) = qty_ship  /*Schdling*/
                qty_preinv = qty_preinv + qty_schd
        print_detail_lines
            qty_open =  max(0, round(qty_open - qty_preinv, 2))
            gosub detail_print_routine
            goto bcklines_loop

        end_of_lines
            print
            print using L60400  /* End of Document */
        bckpridx_deleter
            if print_labels$ <> "Y" then delete #3 /* If No, Delete Now */
            goto plow_thru_bckpridx

        end_of_report
            if nbr_recs% <> 0% then goto end_of_job
L12020:         comp% = 2%
                hdr$ = "*** NULL SET SELECTED ***"
                msg$(1) = "There are no Bills of Lading that satisfy " & ~
                     "your criteria"
                msg$(3) = "Press RETURN to continue"
                call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
                if comp% <> 0% then L12020
                goto exit_program
        end_of_job
            close printer
            call "SETPRNT" ("SHP004", prog$, 0%, 1%)
            goto exit_program

        page_heading
            page_nbr% = page_nbr% + 1% : nbr_lines% = 0%
            if page_nbr% > 1% then print using L60360, "* CONTINUED *"
        page_0_heading
            print page
            print using L60040, rptid$, date$, hdr_desc$
            if pagesw% <> 0% then return
            print using L60060 /* BOX 1 LINE 1 */
            print using L60080, store$(1), "#", "#"
            print using L60100, store$(2)
            print using L60120, store$(3), stor_code$, schd_date$,        ~
                cust_code$, str(so$,,16) & "-" & bol$, page_nbr%
            print using L60140, store$(4)
            print using L60150, instr$(1)
            print using L60155, instr$(2)
            print using L60220, ship_to$(1)
            print using L60240, ship_to$(2)
            print using L60260, ship_to$(3)
            print using L60280, ship_to$(4), po$, carr_name$, carton$,    ~
                weight$
            print using L60300, ship_to$(5)
            print using L60320, ship_to$(6), fob$
            print using L60330, howship$
            print
            nbr_lines% = nbr_lines% + 16%
            if page_nbr% > 1% then L12368
                gosub'200 (txtid_bckm$)
                gosub'200 (txtid_cust$)
L12368:     print
            print using L60160 /* COLUMN HEADERS */
            if act_date$ <> " " and act_date$ <> blankdate$ then ~
                                     print using L60180, "#"              ~
                                else print using L60190, "#"
            print using L60200 /* UNDERSCORES */
            nbr_lines% = nbr_lines% + 4%
            ser$ = "S/N'S:"
            return

        detail_print_routine

            if xref_part$ = " " then L12430
            if xref_type$ = "M" and xref_mnf$ <> "Y" then L12430
            if xref_type$ = "C" and xref_cus$ <> "Y" then L12430
                part_nmbr$ = xref_part$ : part_desc$ = xref_descr$

L12430:     for l% = 1% to 30%
                if nbr_lines% > max_lines% then gosub page_heading
                qtyship$ = " "
                if bol$ = " " then print_it
                if qty_ship(l%) = 0 then L13094
                    qty_ship(l%) = round(qty_ship(l%), 2)
                    convert qty_ship(l%) to qtyship$, pic(###,###.##)
        print_it
            if l% <> 1% then L13060
                print using L60340, po_line$, so_seqnr$, part_nmbr$,      ~
                    part_desc$, stock_uom$, qty_open, qtyship$,          ~
                    lot$(l%), package_qty$, package_type$                ~

           /* Xref Part Additional Printing */
            if xref_type$ = "M" and xref_mnf$ = "B" then L13036 else L13042
L13036:         print using L60422, "Manufactor Part No.:", xref_part$,   ~
                                    "(" & xref_descr$ & ")"
                goto L13048             /* Iterate */
L13042:     if xref_type$ = "C" and xref_cus$ = "B" then L13044 else L13080
L13044:         print using L60422, "Customer Part No.:", xref_part$,     ~
                                    "(" & xref_descr$ & ")"
L13048:         nbr_lines% = nbr_lines% + 1%
                goto L13080

L13060:         print using L60340, " ", " ", " ", " ", " ", " ",         ~
                        qtyship$, lot$(l%), " ", " "

L13080:         nbr_lines% = nbr_lines% + 1%
                gosub serial_number_print
                if bol$ = " " then L13094
                goto L13100
L13094:         l% = 30%
L13100:     next l%
            if bol$ = " " then L13108
                if qty_ship(1%) = 0 then L13110
L13108:             gosub print_text
L13110:     return

        one_time_routine
            nbr_recs% = 1%
            select printer(134)
            call "SETPRNT" ("SHP004", prog$, 0%, 0%)

        REM PRINT 'PAGE ZERO' -- THE SELECTION SCREEN *******************
            pagesw% = 1%
            gosub page_0_heading
            print skip (4)
L13212:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then goto L13220
                str(i$(), i%, 1%) = " "
                goto L13212
L13220:     print using L60380, "   ---- SELECTION CRITERIA ----"
            print
            for n% = 6% to 19%
                print using L60380, i$(n%)
            next n%
            pagesw% = 0%
            return

        store_routine
            if stor_code$ = brk_stor$ then return
            init (" ") store$() : store$(1) = "UNKNOWN STORE CODE"
            brk_stor$ = stor_code$
            call "READ100" (#8, stor_code$, f1%(8))
            if f1%(8) = 0% then return
            get #8 using L13370, store$(1), store$(2), store$(3),         ~
                store$(4)
L13370:         FMT  POS(34), CH(30), POS(64), 3*CH(30)
            call "LINSMASH" (store$())
            return

        serial_number_print
            ser$ = "S/N'S:" : serkey$, serprint$ = " " : ser% = 0%
            serkey$ = "t" & str(stor_code$) & str(lot$(l%))
            str(serkey$, 32, 1) = hex(00)
        plow_serial_master
            call "PLOWALTS" (#9, serkey$, 2%, 31%, f1%(9))
            if f1%(9) = 0% then goto plow_serial_done
                get #9 using L13500, serial$, rs$, serso$, serbol$, serseq$
L13500:         FMT /* SERMASTR #9 */ POS(32), CH(20), POS(216), CH(2),  ~
                     CH(16), CH(3), CH(3)
                if rs$ <> "RS" then goto plow_serial_master
                if serso$ <> so$ then goto plow_serial_master
                if serbol$ <> bol$ then goto plow_serial_master
                if serseq$ <> seq$ then goto plow_serial_master
                if len(serprint$) + len(serial$) + ser% >                ~
                     len(str(serprint$)) then gosub serial_print
                str(serprint$, len(serprint$) + ser%) = serial$
                ser% = 2%
                goto plow_serial_master

        plow_serial_done
            if ser% <> 0% then gosub serial_print
            return

        serial_print
            if nbr_lines% > max_lines% then gosub page_heading
            print using L60351, ser$, serprint$
            ser$, serprint$ = " " : ser% = 0%
            nbr_lines% = nbr_lines% + 1%
            return

        check_xref_print_settings
            txtid_cust$ = all(hex(ff))
            call "READ100" (#16, cust_code$, f1%(16%))
            if f1%(16%) = 0% then return   /* Shouldn't happen */
            get #16 using L13730, txtid_cust$, temp_cus$, temp_mnf$
L13730:         FMT POS(789), CH(4), POS(1092), CH(1), CH(1)

            if temp_cus$ = " " then L13780
                if pos("YNB" = temp_cus$) <> 0% then xref_cus$ = temp_cus$

L13780:     if temp_mnf$ = " " then L13810
                if pos("YNB" = temp_mnf$) <> 0% then xref_mnf$ = temp_mnf$

L13810:     return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            *            I M A G E   S T A T E M E N T S                *~
            *************************************************************

L60040: %BILL OF LADING                                 ######  PRINTED #~
        ~####### PER ##############################
L60060: %                                               +-----+----------~
        ~-+-----------+----------------------+------+
L60080: %   FROM: ##############################        !STORE! SHIP DATE~
        ~ ! CUSTOMER# ! BILL OF LADING #     ! PAGE !
L60100: %         ##############################        !-----!----------~
        ~-!-----------!----------------------!------!
L60120: %         ##############################        ! ### !  ########~
        ~ ! ######### ! #################### ! #### !
L60140: %         ##############################        +-----+----------~
        ~-+-----------+----------------------+------+
L60150: %                                               Ship Instr: #####~
        ~#############################################
L60155: %                                                           #####~
        ~#############################################
L60160: %P.O. S.O.                                                       ~
        ~            QUANTITY   QUANTITY LOT     PKG PACKAGE
L60180: %LINE SEQ# PART NUMBER               PART DESCRIPTION            ~
        ~     UOM   REMAINING    SHIPPED NUMBER  QTY TYPE
L60190: %LINE SEQ# PART NUMBER               PART DESCRIPTION            ~
        ~     UOM  UNCOMMITED  SCHEDULED NUMBER  QTY TYPE
L60200: %---- ---- ------------------------- ----------------------------~
        ~---- ---- ---------- ---------- ------ ---- --------
L60220: %SHIP-TO: ###############################       +----------------~
        ~--+----------------------+---------+--------+
L60240: %         ###############################       ! PURCHASE ORDER ~
        ~  ! CARRIER              ! CARTONS ! WEIGHT !
L60260: %         ###############################       !----------------~
        ~--!----------------------!---------!--------!
L60280: %         ###############################       ! ###############~
        ~# ! #################### ! ####### ! ###### !
L60300: %         ###############################       +----------------~
        ~--+----------------------+---------+--------+
L60320: %         ###############################            FOB: #######~
        ~#############
L60330: %                                               How Ship: #######~
        ~#############
L60340: % ### #### ######################### ############################~
        ~#### #### ###,###.## ########## ###### #### ########
L60351: %###### #########################################################~
        ~######################################
L60360: %                                                                ~
        ~                  #############
L60380: %               #################################################~
        ~###############################
L60400: %                                             ** END OF DOCUMENT ~
        ~**

L60422: %           ################### #########################  ######~
        ~############################

        print_text
*          Print Sales Order Text (If Any) From The 'BCKLINES' File ****
            gosub'200(txtid_bckl$)       /* Print Text (if any)        */

*          Print The Part Text (If Any) From The 'HNYMASTR' File *******
            call "READ100" (#13, part_nmbr$, f1%(13))
            if f1%(13) = 0% then return
            get #13 using L60480, txtid$, part_type$
L60480:         FMT  POS(98), CH(4), POS(180), CH(3)
            gosub'200(txtid$)            /* Print Text (if any)        */

*          Print the Bill of Material Text (if any) from 'BOMMASTR' file
*          First we have to find the BOM ID to use!

            convert part_type$ to part_type%, data goto L60730
            if part_type% > 0% and part_type% < 500% then return
            if part_type% > 789% and part_type% < 800% then return
            call "PIPINDEX" (#1, due_curr$, dateindex%, ret%)
            if ret% = 1% then return /* Planning Calendar Messed Up!   */
            if dateindex% = 0% then return /* Off Planning Calendar!   */
            readkey2$ = str(part_nmbr$,,25) & "1"
            call "READ102" (#2, readkey2$, f1%(2))
            if f1%(2%) <> 1% then return
            get #2, using L60630, readkey2$, bom$()
L60630:        FMT CH(29), 490 * CH(3)
            if str(readkey2$,,25) <> str(part_nmbr$,,25) then return
            bomid$ = bom$(dateindex%)
            if bomid$ = " " then  break% = 25% else break% = 28%
            readkey$ = str(part_nmbr$,,25%) & bomid$
            call "PLOWNEXT" (#15, readkey$, break%, f1%(15%))
            if f1%(15%) = 0% then return
            get #15, using L60710, txtid$
L60710:         FMT POS(90),CH(4)
            gosub'200(txtid$)
L60730:     return

        REM *************************************************************~
            *  Print Text For Either The Part or BOM files              *~
            *************************************************************

        deffn'200(txtid$)
            if txtid$ = hex(ffffffff) then return
            if txtid$ = " " then return
                txt% = (page_nbr% * 100%) + nbr_lines%
                comp% = 0%
L60840:         call "TXTPRINT" (#14, f2%(14), 134%, txtid$, "SHP004",   ~
                     9%, nbr_lines%, max_lines%, "T", " ", comp%)
                if comp% = 0% then goto L60890
                     gosub page_heading
                     goto L60840
L60890:         if txt% = (page_nbr% * 100%) + nbr_lines% then return
                     print
                     nbr_lines% = nbr_lines% + 1%
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
            if print_labels$ <> "Y" then goto L65170
            if nbr_recs% = 0% then goto L65170
*        OK, call the Shipping Label printing subroutine.
            call "SHPLABSB" ("B", from_date%, from_stor$, low_date$,     ~
                             so$(), to_date%, to_stor$, #3, #4, #6)
L65170
*        Bill of Lading subroutine ends here.
            end

