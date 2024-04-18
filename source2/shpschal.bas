        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *   SSS   H   H  PPPP    SSS    CCC   H   H   AAA   L       *~
            *  S      H   H  P   P  S      C   C  H   H  A   A  L       *~
            *   SSS   HHHHH  PPPP    SSS   C      HHHHH  AAAAA  L       *~
            *      S  H   H  P          S  C   C  H   H  A   A  L       *~
            *   SSS   H   H  P       SSS    CCC   H   H  A   A  LLLLL   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SHPSCHAL - Allow shipment scheduling by alloting          *~
            *            quantities over multiple sales orders at one   *~
            *            time.  Primarily by part.                      *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1993  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/18/93 ! Original                                 ! MLJ *~
            * 01/14/94 ! Added SO/Line contiguous in SHPLINES     ! WPH *~
            * 04/21/94 ! Corrected LINE2$ overflow, moved BCKSWTCH! MLJ *~
            *          !   to 9000s, Qty available on redisplay   !     *~
            *          !   now considers fully scheduled lines.   !     *~
            * 07/07/97 ! Millie Date conversion                   ! DER *~
            *          !  Blank date, Date Range, Pass unfmt date !     *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            autoapp$1,                   /* Export Auto Approval       */~
            blankdate$8,                 /* blank unfmt date           */~
            bol$3,                       /* Bill Of Lading             */~
            bomid$3,                     /* Bill of Materials ID       */~
            carrier$(500)9,              /* Carrier Array              */~
            carrier_code$9,              /* Carrier Code               */~
            committed(500),              /* Already Shp'd, Schl'd      */~
            comp$1,                      /* Completion Flag            */~
            crhold$1,                    /* Credit Hold Flag           */~
            cursor%(2),                  /* Cursor location for edit   */~
            cuscode$9,                   /* Customer Code              */~
            cust_rng$(4)9,               /* Customer Range             */~
            customer$(500)9,             /* Customer Code              */~
            date$8,                      /* Date for screen display    */~
            def_percent(500),            /*                            */~
            def_unit(500),               /*                            */~
            dfac$(12)1,                  /* Display Fac                */~
            descr$34,                    /* Part Number Description    */~
            duedate$(500)8,              /* Order Due Date             */~
            due_date$6,                  /* Order Due Date             */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            expappr$(500)1,              /* Export Approval Flag       */~
            export$1,                    /* Export Flag                */~
            field1$(500)16,              /* So/Po Toggle               */~
            field2$(500)3,               /* So/Po Line Toggle          */~
            field3$(500)8,               /* Ship/Due Date Toggle       */~
            field4$(500)10,              /* Qty/Carrier Toggle         */~
            fob$20,                      /* FOB                        */~
            hdr$(2)25,                   /* Screen 1 Column Headings   */~
            hdr1$9,                      /* Screen 2 Column Headings   */~
            hdr2$16,                     /* Screen 2 Column Headings   */~
            hdr3$3,                      /* Screen 2 Column Headings   */~
            hdr4$2,                      /* Screen 2 Column Headings   */~
            hdr5$8,                      /* Screen 2 Column Headings   */~
            hdr6$8,                      /* Screen 2 Column Headings   */~
            hdr7$3,                      /* Screen 2 Column Headings   */~
            hdr8$10,                     /* Screen 2 Column Headings   */~
            hdr9$10,                     /* Screen 2 Column Headings   */~
            howship$20,                  /* How To Ship                */~
            i$(24)80,                    /* Screen Image               */~
            incr$4,                      /* BCKBUFFR Increment         */~
            inpmessage$79,               /* Informational Message      */~
            instr$(2)50,                 /* Shipping Instructions      */~
            lastscqty(500),              /* Last Scheduled Qty         */~
            last_key$40,                 /* Last Alternate Key         */~
            last_part$25,                /* Last Part Loaded           */~
            last_so$16,                  /* Last SO for BOL assignment */~
            lfac$(12)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            lot_no$(30)6,                /* Lot Number Array (Blank)   */~
            lot_qty(30),                 /* Lot Quantity Array (Zero)  */~
            neg_avail%(500),             /* Negative Qty Avail flag    */~
            orderdate$(500)8,            /* Order Date                 */~
            order(500),                  /*                            */~
            order_date$6,                /* Order Date                 */~
            oshipdate$(500)8,            /* Orig Ship Date             */~
            over(500),                   /*                            */~
            overship$1,                  /* Overshipment Allowed Flag  */~
            ouqty(500), ouqty$(500)10,   /* Original Uncommitted Qty   */~
            p_atc1$(500)10,              /* Part ATC1           - HDR  */~
            p_atc2$(500)10,              /* Part ATC2           - HDR  */~
            p_descr$(500)34,             /* Part Description    - HDR  */~
            pfac$1,                      /* Part Qty FAC        - HDR  */~
            p_part$(500)25,              /* Part Number         - HDR  */~
            p_qty(500), p_qty$(500)10,   /* Part Qty Available  - HDR  */~
            p_ohqty(1),                  /* HNYTOTSB On Hand           */~
            pipatcdt$8,                  /* date passed to PIPATCDZ    */~
            part$25,                     /* Part Number                */~
            part_rng$(4)25,              /* Part Range                 */~
            pcode$1,                     /* Priority Code              */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Mis Read/Plow Key          */~
            po$(500)16,                  /* Purchase Order Number      */~
            poline$(500)3,               /* Purchase Order Line        */~
            po_line$3,                   /* Purchase Order Line        */~
            po_nbr$16,                   /* Purchase Order Number      */~
            pos%(1),                     /* SEARCH Receiver            */~
            priority$(500)1,             /* Priority Code              */~
            prtbol$1,                    /* Print BOL?                 */~
            prtpickl$1,                  /* Print Pick List?           */~
            qtyflag$(500)3,              /* Qty Status Flag            */~
            qty_flag$3,                  /* Qty Status Flag            */~
            readkey$99,                  /* Miscellaneous Read key     */~
            scqty(500), scqty$(500)10,   /* Schedule Quantity          */~
            shipcode$1,                  /* Shipping Priority Code     */~
            shipdate$(500)8,             /* Req Ship Date              */~
            ship_date$6,                 /* Req Ship Date              */~
            ship_rng$(4)10,              /* Required Ship Date Range   */~
            so$(500)16,                  /* Sales Order Number         */~
            soline$(500)3,               /* Sales Order Line           */~
            so_line$3,                   /* Sales Order Line           */~
            so_nbr$16,                   /* Sales Order Number         */~
            so_rng$(4)16,                /* Sales Order Range          */~
            store$3,                     /* Store Number               */~
            sstore$3,                    /* Store Number               */~
            sys_def_percent(500),        /*                            */~
            textid$4,                    /* Text ID                    */~
            this_key$40,                 /* Current Alternate Key      */~
            undate$6,                    /* Unformatted System Date    */~
            unqty(500), unqty$(500)10,   /* Remaining Uncommitted Qty  */~
            userid$3                     /* Current User Id            */~

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
            * #01 ! ARILINES ! Invoice Master- Line Items               *~
            * #02 ! ARIMASTR ! Invoice Master- Headers                  *~
            * #03 ! BCKBUFFR ! S.O. Header Buffer                       *~
            * #04 ! BCKHMSTR ! SO History Master file - Headers         *~
            * #05 ! BCKHLNES ! SO History Lines Detail file             *~
            * #06 ! BCKLINES ! BACK LOG LINE ITEM FILE                  *~
            * #07 ! BCKMASTR ! S.O. Header Master File                  *~
            * #08 ! BCKPRIDX ! SO Document Print Index File             *~
            * #10 ! CALMASTR ! PLANNING CALENDAR FILE                   *~
            * #11 ! CUSTOMER ! Customer Master File                     *~
            * #12 ! DEMMASTR ! Demand Master File                       *~
            * #14 ! GENCODES ! System General Codes file.               *~
            * #15 ! HNYALTRS ! Inventory Alternate Part File            *~
            * #16 ! HNYDETAL ! INVENTORY DETAILS                        *~
            * #17 ! HNYMASTR ! Inventory Master File                    *~
            * #18 ! HNYPOOL  ! Inventory LIFO/FIFO pool records         *~
            * #19 ! HNYQUAN  ! Inventory Costs & Quantity Master (Summa *~
            * #20 ! JBCREDIT ! Production job credits received detail f *~
            * #21 ! JBCROSS2 ! Cross reference of RTE & BOM planned for *~
            * #22 ! JBMASTR2 ! Production job master file               *~
            * #23 ! JBSTATUS ! Production job actual structure (RTE) ac *~
            * #24 ! PAYLINES ! PAYABLES LINE ITEM FILE                  *~
            * #25 ! PAYMASTR ! PAYABLES MAIN FILE  (INVOICE DATES)      *~
            * #26 ! PIPCROSS ! hard peg cross reference                 *~
            * #27 ! PIPIN    ! Planned inventory additions detail       *~
            * #28 ! PIPMASTR ! Planned Inventory Position Master File   *~
            * #29 ! PIPOUT   ! Planned inventory use detail rec         *~
            * #30 ! RCVLINES ! Receiver Line Items File  (Purchasing)   *~
            * #32 ! RTEMASTR ! Production routing master file           *~
            * #33 ! SFCUM2   ! Cumulative sales forecast file           *~
            * #35 ! SHIPTEMP ! Temporary Indexed Work File              *~
            * #36 ! SHPHDRS  ! Shipment Scheduling / Pre-Invoicing- Hea *~
            * #37 ! SHPLINES ! Shipment Scheduling / Pre-Invoicing- Lin *~
            * #38 ! STORNAME ! Store Master File                        *~
            * #39 ! SYSFILE2 ! Caelus Management System Information     *~
            * #40 ! TXTFILE  ! System Text File                         *~
            * #41 ! VBKLINES ! Purchase Order Line Items File           *~
            * #42 ! VBKMASTR ! PURCHASE ORDER HEADER FILE               *~
            * #43 ! WCMASTR  ! Workcenter Master File                   *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "ARILINES",                                      ~
                        varc,     indexed,  recsize =  750,              ~
                        keypos =    1, keylen =  20

            select #02, "ARIMASTR",                                      ~
                        varc,     indexed,  recsize = 2000,              ~
                        keypos =    1, keylen =  17,                     ~
                        alt key  1, keypos =   10, keylen =   8,         ~
                            key  2, keypos =   18, keylen =  16, dup,    ~
                            key  3, keypos =   34, keylen =  16, dup

            select #03, "BCKBUFFR",                                      ~
                        varc,     indexed,  recsize = 1020,              ~
                        keypos =    1, keylen =  10,                     ~
                        alt key  2, keypos =   30, keylen =  16,         ~
                            key  1, keypos =    4, keylen =   7, dup

            select #04, "BCKHMSTR",                                      ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #05, "BCKHLNES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19

            select #06, "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19

            select #07, "BCKMASTR",                                      ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #08, "BCKPRIDX",                                      ~
                        varc,     indexed,  recsize =   45,              ~
                        keypos =   11, keylen =  29,                     ~
                        alt key  1, keypos =    1, keylen =  39

            select #10, "CALMASTR",                                      ~
                        varc,     indexed,  recsize = 1962,              ~
                        keypos =    1, keylen =   2

            select #11, "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #12, "DEMMASTR",                                      ~
                        varc,     indexed,  recsize =  123,              ~
                        keypos =    2, keylen =  27,                     ~
                        alt key  1, keypos =   10, keylen =  19,         ~
                            key  2, keypos =    1, keylen =  28

            select #14, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #15, "HNYALTRS",                                      ~
                        varc,     indexed,  recsize =   60,              ~
                        keypos =    1, keylen =  33

            select #16, "HNYDETAL",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  42,                     ~
                        alt key  2, keypos =   49, keylen =   2, dup,    ~
                            key  1, keypos =   43, keylen =   6, dup

            select #17, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup

            select #18, "HNYPOOL",                                       ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =  38

            select #19, "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =  17,  keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44

            select #20, "JBCREDIT",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  22,                     ~
                        alt key  1, keypos =   23, keylen =  48          ~

            select #21, "JBCROSS2",                                      ~
                        varc,     indexed,  recsize =   94,              ~
                        keypos =   29, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  47,         ~
                            key  2, keypos =   48, keylen =  47          ~

            select #22, "JBMASTR2",                                      ~
                        varc,     indexed,  recsize = 1300,              ~
                        keypos =    1, keylen =   8                      ~

            select #23, "JBSTATUS",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    1, keylen =  12,                     ~
                        alt key  1, keypos =   21, keylen =  44,         ~
                            key  2, keypos =   29, keylen =  36          ~

            select #24, "PAYLINES",                                      ~
                        varc,     indexed,  recsize =  541,              ~
                        keypos =   36, keylen =  28,                     ~
                        alt key  1, keypos =    1, keylen =  63,         ~
                            key  2, keypos =   17, keylen =  47          ~

            select #25, "PAYMASTR",                                      ~
                        varc,     indexed,  recsize =  350,              ~
                        keypos =    1, keylen =  25                      ~

            select #26, "PIPCROSS",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  71,                     ~
                        alt key  1, keypos =   20, keylen =  52,         ~
                            key  2, keypos =   39, keylen =  33

            select #27, "PIPIN",                                         ~
                        varc,     indexed,  recsize =   60,              ~
                        keypos =   30, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  48

            select #28, "PIPMASTR",                                      ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    2, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  26

            select #29, "PIPOUT",                                        ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =   20, keylen =  37

            select #30, "RCVLINES",                                      ~
                        varc,     indexed,  recsize =  800,              ~
                        keypos =   26, keylen =  52,                     ~
                        alt key  1, keypos =    1, keylen =  69,         ~
                            key  2, keypos =   42, keylen =  36,         ~
                            key  3, keypos =  128, keylen =  24          ~

            select #32, "RTEMASTR",                                      ~
                        varc,     indexed,  recsize =  400,              ~
                        keypos =    5, keylen =  31,                     ~
                        alt key  1, keypos =    1, keylen =  35

            select #33, "SFCUM2",                                        ~
                        varc,     indexed,  recsize = 1985,              ~
                        keypos =    1, keylen =  25

            select #35, "SHIPTEMP",                                      ~
                        varc,     indexed,  recsize = 200,               ~
                        keypos =    1, keylen =  54,                     ~
                        alt key  1, keypos =  145, keylen =  40, dup

            select #36, "SHPHDRS",                                       ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =  28

            select #37, "SHPLINES",                                      ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =   10, keylen =  22

            select #38  "STORNAME",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   3


            select #39, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #40, "TXTFILE",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  11

            select #41, "VBKLINES",                                      ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos =    1, keylen =  28                      ~

            select #42, "VBKMASTR",                                      ~
                        varc,     indexed,  recsize = 1030,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   10, keylen =  16          ~

            select #43, "WCMASTR",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    2, keylen =   5,                     ~
                        alt key  1, keypos =    1, keylen =   6

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01%), f2%(01%),   0%, rslt$(01%))
            call "OPENCHCK" (#02, fs%(02%), f2%(02%),   0%, rslt$(02%))
            call "OPENCHCK" (#03, fs%(03%), f2%(03%), 100%, rslt$(03%))
            call "OPENCHCK" (#04, fs%(04%), f2%(04%),   0%, rslt$(04%))
            call "OPENCHCK" (#05, fs%(05%), f2%(05%),   0%, rslt$(05%))
            call "OPENCHCK" (#06, fs%(06%), f2%(06%),   0%, rslt$(06%))
            call "OPENCHCK" (#07, fs%(07%), f2%(07%),   0%, rslt$(07%))
            call "OPENCHCK" (#08, fs%(08%), f2%(08%), 100%, rslt$(08%))
            call "OPENCHCK" (#10, fs%(10%), f2%(10%),   0%, rslt$(10%))
            call "OPENCHCK" (#11, fs%(11%), f2%(11%),   0%, rslt$(11%))
            call "OPENCHCK" (#12, fs%(12%), f2%(12%),   0%, rslt$(12%))
            call "OPENCHCK" (#14, fs%(14%), f2%(14%),   0%, rslt$(14%))
            call "OPENCHCK" (#15, fs%(15%), f2%(15%),   0%, rslt$(15%))
            call "OPENCHCK" (#16, fs%(16%), f2%(16%),   0%, rslt$(16%))
            call "OPENCHCK" (#17, fs%(17%), f2%(17%),   0%, rslt$(17%))
            call "OPENCHCK" (#18, fs%(18%), f2%(18%),   0%, rslt$(18%))
            call "OPENCHCK" (#19, fs%(19%), f2%(19%),   0%, rslt$(19%))
            call "OPENCHCK" (#20, fs%(20%), f2%(20%),   0%, rslt$(20%))
            call "OPENCHCK" (#21, fs%(21%), f2%(21%),   0%, rslt$(21%))
            call "OPENCHCK" (#22, fs%(22%), f2%(22%),   0%, rslt$(22%))
            call "OPENCHCK" (#23, fs%(23%), f2%(23%),   0%, rslt$(23%))
            call "OPENCHCK" (#24, fs%(24%), f2%(24%),   0%, rslt$(24%))
            call "OPENCHCK" (#25, fs%(25%), f2%(25%),   0%, rslt$(25%))
            call "OPENCHCK" (#26, fs%(26%), f2%(26%),   0%, rslt$(26%))
            call "OPENCHCK" (#27, fs%(27%), f2%(27%),   0%, rslt$(27%))
            call "OPENCHCK" (#28, fs%(28%), f2%(28%),   0%, rslt$(28%))
            call "OPENCHCK" (#29, fs%(29%), f2%(29%),   0%, rslt$(29%))
            call "OPENCHCK" (#30, fs%(30%), f2%(30%),   0%, rslt$(30%))
            call "OPENCHCK" (#32, fs%(32%), f2%(32%),   0%, rslt$(32%))
            call "OPENCHCK" (#33, fs%(33%), f2%(33%),   0%, rslt$(33%))
            call "OPENCHCK" (#36, fs%(36%), f2%(36%), 100%, rslt$(36%))
            call "OPENCHCK" (#37, fs%(37%), f2%(37%), 200%, rslt$(37%))
            call "OPENCHCK" (#38, fs%(38%), f2%(38%),   0%, rslt$(38%))
            call "OPENCHCK" (#39, fs%(39%), f2%(39%),   0%, rslt$(39%))
            call "OPENCHCK" (#40, fs%(40%), f2%(40%),   0%, rslt$(40%))
            call "OPENCHCK" (#41, fs%(41%), f2%(41%),   0%, rslt$(41%))
            call "OPENCHCK" (#42, fs%(42%), f2%(42%),   0%, rslt$(42%))
            call "OPENCHCK" (#43, fs%(43%), f2%(43%),   0%, rslt$(43%))

            call "WORKOPEN" (#35, "IO", 1000%, f2%(35%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$)
            date$, undate$ = date
            call "DATEFMT" (date$)
            blankdate$ = " "
            call "DATUNFMT" (blankdate$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            hdr$(1%) = "From                    "
            hdr$(2%) = "To                      "
            hdr1$    = "Customer "
            hdr3$    = " Ln"
            hdr4$    = "Pr"
            hdr5$    = "Ord Date"
            hdr7$    = "Flg"
            hdr8$    = "Uncommited"

            str(line2$,62%) = "SHPSCHAL: " & str(cms2v$,,8%)

*        See If User Is An Administator...
            call "CMSMACHK" ("ARM", lfac$(1%), lfac$(2%))
            if lfac$(1%) = "Y" or lfac$(2%) = "Y" then admin% = 1%
            if admin% = 1% then L09340
                u3% = 2%
                call "ASKUSER" (u3%, "*** UNAUTHORIZED USE ***",         ~
                     " You must be a Module or Data Base Administrator" &~
                     " to run this program.", " ", "Press RETURN to a"  &~
                     "cknowledge and exit program.")
                goto exit_program

L09340
*        Clear Any Sales Order For This User That Was In Process...
            plowkey$ = all(hex(00))  :  str(plowkey$,,3%) = userid$
            call "READ101" (#3, plowkey$, f1%(3%))
                if f1%(3%) = 0% then L09480
            so_nbr$ = key(#3, 2%)
            get #3 using L09400, plowkey$
L09400:          FMT XX(10), CH(8)
            u3% = 2%
                call "ASKUSER" (u3%, "IN-PROCESS MESSAGE",               ~
                     "You did not complete processing Sales Order " &    ~
                     so_nbr$,"in the program " & str(plowkey$,,8%) & ".",~
                     "Press (RETURN) to exit this program.")
                goto exit_program

L09480
*        Restart SHPSCHAL...
            plowkey$ = all(hex(00))  :  str(plowkey$,,3%) = userid$
            str(plowkey$,3%,1%) = hex(01)
            str(plowkey$,,3%) = userid$
            call "READ101" (#3, plowkey$, f1%(3%))
                if f1%(3%) = 0% then L09600

                call "ASKUSER" (u3%, "*** RESTART NOTE*** ",             ~
                     "Sales Order Processing was not completed.", " ",   ~
                     "Press (RETURN) To Continue")
                gosub clear_in_process

L09600:     call "BCKSWTCH" ("BCK", "OVRSHIP ", overship$, temp, u3%)
              temp  = 0    /* Just so I don't get a compile error */
            call "BCKSWTCH" ("BCK","OVRSHIP%", temp$,                    ~
                              sys_def_percent(p%), u3%)
            temp$ = " "  /* Just so I don't get a compile error */
            call "BCKSWTCH" ("BCK", "XAUTOAPP", autoapp$, temp, u3%)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  7%
L10100:         gosub'051(fieldnr%)
                      if enabled% = 0% then L10190
L10120:         gosub'101(fieldnr%, 1%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10170
                         fieldnr% = max(1%, fieldnr% - 1%)
                         goto L10100
L10170:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10120
L10190:         gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10120
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for selection screen.      *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then L11280
                  if keyhit% <>  0% then editpg1
L11120:     if cursor%(1%) < 10% then fieldnr% = cursor%(1%) - 5%
            if cursor%(1%) = 11% then fieldnr% = cursor%(1%) - 6%
            if cursor%(1%) > 12% and cursor%(1%) < 15% then              ~
                 fieldnr% = cursor%(1%) - 7%
            if fieldnr% < 1% or fieldnr% >  7% then editpg1
            if fieldnr% = lastfieldnr% then editpg1
            gosub'051(fieldnr%)
                  if enabled% =  0% then editpg1
L11200:     gosub'101(fieldnr%, 2%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11200
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11200
                  lastfieldnr% = fieldnr%
            goto L11120

L11280:     found% = 0%
            gosub build_shiptemp
            if found% <> 0% then L12000
L11310:         u3% = 2%
                call "ASKUSER" (u3%, "*** No Selections Found ***",      ~
                     "No Sales Orders were found to meet the selection "&~
                     "criteria entered."," ","Press RETURN to acknowled"&~
                     "ge and re-enter.")
                if u3% = 0% then inputmode
                goto L11310

L12000: REM *************************************************************~
            *     M A I N   P R O C E S S I N G   S E C T I O N         *~
            *-----------------------------------------------------------*~
            * Collect & Display Orders then process those scheduled.    *~
            *************************************************************~

            l%, main% = 0%
            p% = 1%
            gosub dataload
            l% = 0%

L12110:     gosub'102(0%,1%)                            /* Display Only */
                 if keyhit% = 16% then datasave
                 if keyhit% =  1% then gosub startover
                 if keyhit% =  2% then l% = 0%
                 if keyhit% =  3% then l%= max(0%,min(85%,maxlines%-12%))
                 if keyhit% =  4% then l%= max(0%,l%-12%)
                 if keyhit% =  5% then                                   ~
                                 l%=max(0%,min(85%,l%+12%,maxlines%-12%))
                 if keyhit% =  6% then l%=max(0%,l%-1%)
                 if keyhit% =  7% then                                   ~
                                 l%=max(0%,min(85%,l%+1%,maxlines%-1%))
                 if keyhit% <> 8% then L12240
                     if main% = 0% then main% = 1% else main% = 0%
L12240:          if keyhit% = 10% then gosub see_qty
                 if keyhit% = 11% then gosub see_atc
                 if keyhit% = 17% then gosub see_status
                 if keyhit% = 19% then gosub unschedule
                 if keyhit% = 20% then gosub prev_part
                 if keyhit% = 21% then gosub next_part
                 if keyhit% <> 0% then L12110
            fieldnr% = cursor%(1%) -7%
            if main% = 0% then inpmessage$ = "Enter The Shipping Date A"&~
                "nd/Or Quantity To Schedule." else                       ~
                inpmessage$ = "Enter The Carrier Code For This Line."
            if fieldnr% > 12% then L12110
L12360:     gosub'102(fieldnr%,2%)                         /* Edit Mode */
                 if keyhit% =  1% then gosub startover
                 if keyhit% <> 3% then L12410
                     gosub unschedule
                     goto L12110
L12410:          if keyhit% <> 0% then L12360
            gosub'152(fieldnr%)
                 if errormsg$ <> " " then L12360
            goto L12110                        /* Return To Display Mode */

        REM *************************************************************~
            *             B U I L D    S H I P T E M P                  *~
            *-----------------------------------------------------------*~
            * Build SHIPTEMP comtaining orders selected based on the    *~
            * selection criteria entered.                               *~
            *************************************************************


        build_shiptemp
            call "FILEBGON" (#35)
            call "WORKOPEN" (#35, "IO", 1000%, f2%(35%))
            call "SHOSTAT" ("Collecting Sales Orders")
            last_part$, last_so$ = " "
            maxparts% = 0%
            incr% = 1%
            init(hex(00)) plowkey$
            if so_rng$(1%) = "ALL" or so_rng$(1%) = "FIRST" or           ~
               so_rng$(1%) = "LAST" then L14190                           ~
                   else str(plowkey$,1%,16%) = so_rng$(1%)
L14190:     call "READ104" (#6, plowkey$, f1%(6%))
                if f1%(6%) <> 0% then L14280
            return

*        Read BCKLINES And Get Those Order Lines Which Meet The Criteria
*        Entered Creating SHIPTEMP File...

L14260:     call "READNEXT" (#6, f1%(6%))
                if f1%(6%) = 0% then L15100           /* Load Part Array */
L14280:     get #6  using L14310, cuscode$, so_nbr$, so_line$, po_line$,  ~
                    part$, descr$, shpd_qty, open_qty, schld_qty,        ~
                    preinv_qty, due_date$, ship_date$, shipcode$
L14310:     FMT CH(9), CH(16), 2*CH(3), CH(25), CH(32), POS(101),        ~
                3*PD(14,4), POS(133), PD(14,4), POS(206), 2*CH(6),       ~
                POS(277), CH(1)

            u_qty = open_qty - preinv_qty - schld_qty
                if u_qty <= 0 then L14260              /* No Uncommitted */

            if so_rng$(1%) = "ALL" or so_rng$(1%) = "FIRST" or           ~
                                    so_rng$(1%) = "LAST" then L14420
                if so_nbr$ < so_rng$(1%) or                              ~
                   so_nbr$ > so_rng$(2%) then L14260
L14420:     if cust_rng$(1%) = "ALL" or cust_rng$(1%) = "FIRST" or       ~
                                    cust_rng$(1%) = "LAST" then L14460
                if cuscode$ < cust_rng$(1%) or                           ~
                   cuscode$ > cust_rng$(2%) then L14260
L14460:     if part_rng$(1%) = "ALL" or part_rng$(1%) = "FIRST" or       ~
                                     part_rng$(1%) = "LAST" then L14500
                if part$ < part_rng$(1%) or                              ~
                   part$ > part_rng$(2%) then L14260
L14500:     if ship_rng$(1%) = "ALL"   or   ~
               ship_rng$(1%) = "FIRST" or   ~
               ship_rng$(2%) = "LAST" then L14550
                if ship_date$ < ship_rng$(3%) or              ~
                   ship_date$ > ship_rng$(4%) then L14260

L14550:     str(plowkey$) = str(cuscode$) & str(so_nbr$) & hex(00)
            call "READ100" (#7, plowkey$, f1%(7%))
                if f1%(7%) = 0% then L14260
            get #7 using L14590, po_nbr$, store$, order_date$, crhold$
L14590:         FMT POS(26), CH(16), POS(803), CH(3),CH(6),POS(875), CH(1)
            if store$ <> sstore$ then L14260
                if crhold$ <> " " then L14260

            if shipcode$ <> " " then L14700
                call "READ100" (#11, cuscode$, f1%(11%))
                    if f1%(11%) = 0% then L14680
                get #11 using L14670, shipcode$
L14670:             FMT POS(733), CH(1)
L14680:         if shipcode$ = " " then shipcode$ = "3"

L14700:     qty_flag$ = " "
            if shpd_qty = 0 then L14730
                str(qty_flag$,1%,1%) = "I"
L14730:     if schld_qty = 0 then L14750
                str(qty_flag$,2%,1%) = "S"
L14750:     if preinv_qty = 0 then L14780
                str(qty_flag$,3%,1%) = "P"

L14780:     call "DESCRIBE" (#17, part$, descr$, 1%, f1%(17%))

*        See If Order Already In Process, Can't Load If It Is...
            if so_nbr$ = last_so$ then L14980    /* Ok, Flagged This Run */
            call "REDALT0" (#3, so_nbr$, 2%, f1%(3%))
                if f1%(3%) <> 0% then L14260     /* Can't Use This Order */
*        Flag Order As Now In Process...
            convert incr% to incr$, pic(000#)
            readkey$ = " "
            str(readkey$,1%,3%)   = userid$
            str(readkey$,4%,3%)   = all(hex(00))
            str(readkey$,7%,4%)   = incr$
            str(readkey$,11%,8%)  = "SHPSCHAL"
            str(readkey$,21%,25%) = str(cuscode$) & str(so_nbr$)
            write #3 using L14940, readkey$, " ", " ", " ", " ",          ~
                                  eod goto L14260
L14940:     FMT CH(45), 3*CH(250), CH(225)
            last_so$ = so_nbr$  :  incr% = incr% + 1%

*        Write To SHIPTEMP...
L14980:     put #35 using L15020, part$, shipcode$, cuscode$, so_nbr$,    ~
                    so_line$, po_nbr$, po_line$, order_date$, ship_date$,~
                    due_date$, u_qty, 0, qty_flag$, descr$, cuscode$,    ~
                    so_nbr$, ship_date$, " "
L15020:     FMT CH(25), CH(1), CH(9), CH(16), CH(3), CH(16), CH(3),      ~
                3*CH(6), 2*PD(14,4), CH(3), CH(34), CH(9), CH(16), CH(6),~
                CH(25)

            write #35
            found% = 1%
            goto L14260

L15100
*        Load Part Header Array To Be Used In Upper Half Of Display...
            init(hex(00)) plowkey$ : p%, maxparts% = 0% : last_part$= " "
            call "READ104" (#35, plowkey$, f1%(35%))
                 if f1%(35%) <> 1% then return          /* Just In Case */
            goto L15170
L15150:     call "READNEXT" (#35, f1%(35%))
                 if f1%(35%) <> 1% then L15490
L15170:     get #35 using L15180, part$, descr$
L15180:         FMT CH(25), POS(111), CH(34)

            if part$ = last_part$ then L15150
            p% = p% + 1%
            maxparts% = maxparts% + 1%
            p_part$(p%), last_part$  = part$
            p_descr$(p%) = descr$

            pip%, shelf%, atc1%, atc2%, err% = 0%
            pipatcdt$ = date
            call "PIPATCDZ" (p_part$(p%), pipatcdt$, #28, #39, #33, pip%,  ~
                            shelf%, atc1%, atc2%, err%)
            if err% <> 1% then L15380
                p_atc1$(p%) = "Date Err" :  goto L15450
L15380:     if err% <> 2% then L15400
                p_atc1$(p%) = "No Plan"  :  goto L15450
L15400:     convert atc1% to p_atc1$(p%), pic(-#########)
                call "STRING" addr("LJ", p_atc1$(p%),10%)
            convert atc2% to p_atc2$(p%), pic(-#########)
                call "STRING" addr("LJ", p_atc2$(p%),10%)

L15450:     p_ohqty(1%) = 1                             /* Get On Hand */
            call "HNYTOTSB" (p_part$(p%), sstore$, " ", p_ohqty(), 0%)
            p_qty(p%) = p_ohqty(1%)
            goto L15150

L15490
*        Reduce Qty Available By Qty Scheduled In BCKLINES...
            init(hex(00)) readkey$
            call "READ104" (#6, readkey$, f1%(6%))
                 if f1%(6%) <> 1% then return           /* Just in case */
            goto L15570

L15550:     call "READNEXT" (#6, f1%(6%))
                if f1%(6%) <> 1% then L15670
L15570:     get #6 using L15580, part$, schld_qty
L15580:         FMT POS(32), CH(25), POS(117), PD(14,4)
            if schld_qty = 0 then L15550
            search p_part$() = str(part$) to pos%() step 25%
                if pos%(1%) = 0% then L15550
            p% = (pos%(1%) + 24%) / 25%
            p_qty(p%) = p_qty(p%) - schld_qty
            if p_qty(p%) < 0 then neg_avail%(p%) = 1%                    ~
                             else neg_avail%(p%) = 0
            goto L15550

L15670:     for i% = 1% to maxparts%
                call "CONVERT" (p_qty(i%), -2.3, p_qty$(i%))
            next i%
            return

        REM *************************************************************~
            *     M I S C E L A N E O U S   S U B R O U T I N E S       *~
            *************************************************************~

        clear_in_process
            readkey$ = all(hex(00))
            str(readkey$,,3%) = userid$
            call "DELETE" (#3, readkey$, 6%)
            return

        see_qty
            call "HNYQDISP" (p_part$(p%), #17, #19, #18, #39)
            return

        see_atc
            call "PIPATCSB" (p_part$(p%), #28, #17, #33, #10, #27, #29,  ~
                            #16, #12, #26)
            return

        see_status
            d% = cursor%(1%) - 7%
            call "SOSTATUS" (customer$(l%+d%), so$(l%+d%), " ", 0%,      ~
                            #6, #37, #36, #7, #2, #1, #11, #4, #5, #40,  ~
                            1%, 0%, #26, #12, #22, #23, #29, #21, #42,   ~
                            #41, #20, #30, #25, #24, #17, #43, #32)
            return

        unschedule                      /* Also Used To Start Line Over */
            d% = cursor%(1%) - 7%
            if scqty(l%+d%) = 0 then L16370
                p_qty(p%) = p_qty(p%) + scqty(l%+d%)     /* Reset Avail */
                call "CONVERT" (p_qty(p%), -2.2, p_qty$(p%))
                if p_qty(p%) < 0 then neg_avail%(p%) = 1%                ~
                                 else neg_avail%(p%) = 0%
            unqty(l%+d%) = ouqty(l%+d%)              /* Reset Uncommt'd */
                call "CONVERT" (unqty(l%+d%), 2.2, unqty$(l%+d%))
L16370:     scqty(l%+d%), lastscqty(l%+d%) = 0       /* Reset Scheduled */
                call "CONVERT" (scqty(l%+d%), 2.2, scqty$(l%+d%))
                field4$(l%+d%) = scqty$(l%+d%)
            shipdate$(l%+d%) = oshipdate$(l%+d%)     /* Reset Ship Date */
            carrier$(l%+d%)  = " "                   /* Blank Carrier   */
            errormsg$ = " "
            return

        prev_part
            gosub update_shiptemp
            p% = p% - 1%
            gosub dataload
            l% = 0%
            return

        next_part
            gosub update_shiptemp
            p% = p% + 1%
            gosub dataload
            l% = 0%
            return

        update_shiptemp                 /* Rewrite Last Array Processed */
            for i% = 1% to maxlines%
                plowkey$ = str(p_part$(p%)) & str(priority$(i%)) &       ~
                           str(customer$(i%)) & str(so$(i%)) &           ~
                           str(soline$(i%))
                call "READ101" (#35, plowkey$, f1%(35%))
                    if f1%(35%) <> 1% then L16710        /* Just In Case */
                call "DATUNFMT" (shipdate$(i%))
                put #35 using L16690, str(shipdate$(i%),1%,6%), ouqty(i%),~
                        scqty(i%), str(carrier$(i%),1%,9%)
L16690:         FMT POS(80), CH(6), POS(92), 2*PD(14,4), POS(176), CH(9)
                rewrite #35
L16710:     next i%
            return

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub update_shiptemp
            gosub dataput
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20170,         /* Part Range             */~
                              L20220,         /* Customer Range         */~
                              L20270,         /* Req Ship Date Range    */~
                              L20320,         /* Sales Order Range      */~
                              L20370,         /* Store Number           */~
                              L20410,         /* Print Pick List?       */~
                              L20470          /* Print BOL?             */
            return

L20170: REM Def/Enable Part Range                  PART_RNG$()
            if part_rng$(1%) = " " then part_rng$(1%) = "ALL"
            inpmessage$ = "Enter Part Number Range."
            return

L20220: REM Def/Enable Customer Range              CUST_RNG$()
            if cust_rng$(1%) = " " then cust_rng$(1%) = "ALL"
            inpmessage$ = "Enter Customer Number Range."
            return

L20270: REM Def/Enable Required Ship Date Range    SHIP_RNG$()
            if ship_rng$(1%) = " " then ship_rng$(1%) = "ALL"
            inpmessage$ = "Enter Required Ship Date Range."
            return

L20320: REM Def/Enable Sales Order Range           SO_RNG$()
            if so_rng$(1%) = " " then so_rng$(1%) = "ALL"
            inpmessage$ = "Enter Sales Order Number Range."
            return

L20370: REM Def/Enable Store Number                STORE$
            inpmessage$ = "Enter Store Number."
            return

L20410: REM Def/Enable Print Pick List?            PRTPICKL$
            if prtpickl$ = " " then prtpickl$ = "Y"
            inpmessage$ = "Enter 'N' If You Do NOT Want To Print Pick L"&~
                          "ists; 'I' for Immediate Printing."
            return

L20470: REM Def/Enable Print BOL?                  PRTBOL$
            if prtbol$ = " " then prtbol$ = "N"
            inpmessage$ = "Enter 'Y' If You Want To Print BOLs; 'I' for"&~
                          " Immediate Printing."
            return

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables
            init(" ") cust_rng$(), part_rng$(), prtbol$, prtpickl$,      ~
                      ship_rng$(), so_rng$(), sstore$, p_atc1$(),        ~
                      p_atc2$(), p_descr$(), p_part$(), p_qty$(),        ~
                      last_part$, bomid$, carrier_code$, cuscode$,       ~
                      descr$, due_date$, errormsg$, inpmessage$,         ~
                      order_date$, part$, pcode$, po_line$, po_nbr$,     ~
                      qty_flag$, ship_date$, so_line$, so_nbr$, plowkey$,~
                      readkey$, bol$, overship$, autoapp$,               ~
                      expappr$(), lot_no$()

            mat over            = zer
            mat order           = zer
            mat p_qty           = zer
            mat lot_qty         = zer
            mat def_unit        = zer
            mat def_percent     = zer
            mat sys_def_percent = zer
            mat lastscqty       = zer

            maxparts%, maxlines% = 0%

            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1993  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
                return clear all
                gosub clear_in_process
                goto inputmode

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************

        dataload

*        Init Arrays Used By Lines Load...
            init(" ") carrier$(), customer$(), duedate$(), field1$(),    ~
                      field2$(), field3$(), field4$(), orderdate$(),     ~
                      ouqty$(), po$(), poline$(), priority$(),           ~
                      scqty$(), shipdate$(), so$(), soline$(), unqty$(), ~
                      oshipdate$(), qtyflag$()
            mat scqty = zer  :  mat unqty = zer
            mat ouqty = zer  :  mat committed = zer
            mat lastscqty = zer

*        Set SHIPTEMP Readkey For Lines Load...
            init(hex(00)) plowkey$
            str(plowkey$,1%,25%) = str(p_part$(p%))
            l% = 1%
            maxlines% = 0%
            call "READ104" (#35, plowkey$, f1%(35%))
            goto L30280

*        Load Detail Lines Arrays, Lower Half Of Screen Display...
L30270:     call "READNEXT" (#35, f1%(35%))
L30280:         if f1%(35%) <> 1% then return
            get #35 using L30300, part$
L30300:         FMT CH(25)
            if part$ > p_part$(p%) then return
            get #35 using L30360, priority$(l%), customer$(l%), so$(l%),  ~
                    soline$(l%), po$(l%), poline$(l%), orderdate$(l%),   ~
                    shipdate$(l%), duedate$(l%), ouqty(l%), scqty(l%),   ~
                    qtyflag$(l%), carrier$(l%)
L30360:      FMT POS(26), CH(1), CH(9), CH(16), CH(3), CH(16), CH(3),    ~
                 3*CH(6), 2*PD(14,4), CH(3), POS(176), CH(9)

             call "DATEFMT" (orderdate$(l%))
             call "DATEFMT" (shipdate$(l%))
                 oshipdate$(l%) = shipdate$(l%)
             call "DATEFMT" (duedate$(l%))
             call "CONVERT" (ouqty(l%), 2.2, ouqty$(l%))  /* orig uncom*/
                 unqty(l%)  = ouqty(l%)
                 unqty$(l%) = ouqty$(l%)
             call "CONVERT" (scqty(l%), 2.2, scqty$(l%))
             l% = l% + 1%  :  maxlines% = maxlines% + 1%
             goto L30270

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************

        dataput

*        Set Up To Process SHIPTEMP On Alternate Key...
            last_key$ = " "
            init(hex(00)) plowkey$
            call "REDALT4" (#35, plowkey$, 1%, f1%(35%))
                if f1%(35%) <> 1% then return           /* Just in Case */
            goto L32170

        read_shiptemp
            call "READNEXT" (#35, f1%(35%))
L32170:         if f1%(35%) <> 0% then L32200
                    gosub clear_in_process
                    return
L32200:     get #35 using L32230, part$, shipcode$, cuscode$, so_nbr$,    ~
                    so_line$, po_nbr$, po_line$, order_date$, ship_date$,~
                    due_date$, u_qty, s_qty, qty_flag$, carrier_code$
L32230:     FMT CH(25), CH(1), CH(9), CH(16), CH(3), CH(16), CH(3),      ~
                3*CH(6), 2*PD(14,4), CH(3), POS(176), CH(9)
            if s_qty > 0 then L32270
                goto read_shiptemp                /* No qty to schedule */
L32270:     str(this_key$) = str(cuscode$) & str(so_nbr$) &              ~
                             str(ship_date$) & str(carrier_code$)
            if this_key$ = last_key$ then L32820

*        Assign New BOL (One Per Sales Order)...
            readkey$ = str(cuscode$) & str(so_nbr$)
            call "READ101" (#7, readkey$, f1%(7%))
                get #7 using L32350, bol%
L32350:             FMT POS(878), BI(4)
            convert bol% to bol$, pic(##0)
            bol% = bol% + 1%
            if bol% = 1000% then bol% = 1%
            put #7 using L32350, bol%
            rewrite #7
            call "STRING" addr("LJ", bol$, 3%)

*        Get Misc Header Info From BCKMASTR...
            init(" ") howship$, fob$, instr$(), textid$, export$
            readkey$ = str(cuscode$) & str(so_nbr$)
            call "READ100" (#7, readkey$, f1%(7%))
                if f1%(7%) <> 1% then L32530             /* Just In Case */
            get #7 using L32500, howship$, fob$, instr$(), textid$,       ~
                                export$
L32500:     FMT POS(422), 2*CH(20), 2*CH(50), POS(799), CH(4), POS(857), ~
                CH(1)

L32530
*        Write BOL Header...
            readkey$ = str(cuscode$) & str(so_nbr$) & str(bol$)
            call "DELETE" (#36, readkey$, 28%)
            if autoapp$ = "Y" then expappr$(p%) = "Y"
            write #36 using L32610, cuscode$, so_nbr$, bol$, sstore$,     ~
                      ship_date$, carrier_code$, howship$, fob$,         ~
                      instr$(), " ", " ", 0, 0, 0, " ", textid$,         ~
                      export$, expappr$(p%), " "
L32610:     FMT CH(9), CH(16), 2*CH(3), 2*CH(6), 2*CH(20), 2*CH(50),     ~
                CH(6), CH(20), 3*PD(14,4), CH(8), CH(4), 2*CH(1), CH(53)
            call "TXTFUTIL" (#40, f2%(40%), "TOS2", textid$)
            str(last_key$) = str(cuscode$) & str(so_nbr$) &              ~
                             str(ship_date$) & str(carrier_code$)

*        Write Shipping Documents Print File...
            if prtpickl$ = "N" then L32740
                printdate$ = ship_date$
            if prtpickl$ = "I" then printdate$ = blankdate$
            write #8 using L32790, "P", store$, printdate$, "P",          ~
                     cuscode$, so_nbr$, bol$, " "

L32740:     if prtbol$ = "N" then L32810
                printdate$ = ship_date$
            if prtbol$ = "I" then printdate$ = blankdate$
            write #8 using L32790, "B", store$, printdate$, "B",          ~
                     cuscode$, so_nbr$, bol$, " "
L32790:     FMT CH(1), CH(3), CH(6), CH(1), CH(9), CH(16), CH(3), CH(6)

L32810
*        Write SHPLINES...
L32820:     if u_qty > 0 then comp$ = " " else comp$ = "C"
            write #37 using L32880, cuscode$, so_nbr$, bol$, so_line$,    ~
                      s_qty, lot_no$(), lot_qty(), textid$, " ", " ",    ~
                      comp$, so_nbr$, so_line$, " "
L32880:     FMT CH(9), CH(16), CH(3), CH(3), PD(14,4), 30*CH(6),         ~
                30*PD(14,4), 2*CH(4), CH(8), CH(1), CH(16), CH(3),CH(105)
            call "TXTFUTIL" (#40, f2%(40%), "TOS2", textid$)

*        Write BCKLINES And Adjust Quantity Scheduled...
            readkey$ = str(so_nbr$) & str(so_line$)
            call "READ101" (#6, readkey$, f1%(6%))
            get #6 using L32960, bckscqty
L32960:         FMT POS(117), PD(14,4)
            bckscqty = bckscqty + s_qty
            put #6 using L32960, bckscqty
            rewrite #6
            goto read_shiptemp

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub set_pf1
              if edit% <> 2% then L40110
                  if fieldnr% = 0% then inpmessage$ = "To Modify, Posit"&~
                      "ion Cursor To Desired Value And Press Return."
L40110:       if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              lfac$(fieldnr%) = hex(81)
              str(line2$,1%,40%) = "Specify Shipping Criteria"

L40160:     accept                                                       ~
               at (01,02), "Shipping Schedule Allotment by Part",        ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,23), fac(hex(ac)), hdr$(1%)               , ch(25),~
               at (05,50), fac(hex(ac)), hdr$(2%)               , ch(25),~
                                                                         ~
               at (06,02), "Part",                                       ~
               at (06,23), fac(lfac$(1%)), part_rng$(1%)        , ch(25),~
               at (06,50), fac(lfac$(1%)), part_rng$(2%)        , ch(25),~
                                                                         ~
               at (07,02), "Customer",                                   ~
               at (07,23), fac(lfac$(2%)), cust_rng$(1%)        , ch(09),~
               at (07,50), fac(lfac$(2%)), cust_rng$(2%)        , ch(09),~
                                                                         ~
               at (08,02), "Required Ship Date",                         ~
               at (08,23), fac(lfac$(3%)), ship_rng$(1%)        , ch(10),~
               at (08,50), fac(lfac$(3%)), ship_rng$(2%)        , ch(10),~
                                                                         ~
               at (09,02), "Sales Order",                                ~
               at (09,23), fac(lfac$(4%)), so_rng$(1%)          , ch(16),~
               at (09,50), fac(lfac$(4%)), so_rng$(2%)          , ch(16),~
                                                                         ~
               at (11,02), "Store Number",                               ~
               at (11,23), fac(lfac$(5%)), sstore$              , ch(03),~
                                                                         ~
               at (13,02), "Print Pick List?",                           ~
               at (13,23), fac(lfac$(6%)), prtpickl$            , ch(01),~
                                                                         ~
               at (14,02), "Print BOL?",                                 ~
               at (14,23), fac(lfac$(7%)), prtbol$              , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L40610
                  call "MANUAL" ("SHPSCHAL")
                  goto L40160

L40610:        if keyhit% <> 15% then L40650
                  call "PRNTSCRN"
                  goto L40160

L40650:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40840                          /* Input Mode */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2%)= "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40800
                str(pf$(3%),64%)  = " "  :  str(pfkeys$,16%,1%) = hex(ff)
L40800:     if fieldnr% > 2% then L40820
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40820:     return

L40840: if fieldnr% > 0% then L40930           /* Edit Mode - Select Fld */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Continue    "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40930:                                          /* Edit Mode - Enabled */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
              if neg_avail%(p%) = 1% then pfac$ = hex(94)                ~
                                     else pfac$ = hex(84)
              if main% <> 0% then L42210
                   mat field1$ = so$        : mat field2$ = soline$
                   mat field3$ = shipdate$  : mat field4$ = scqty$
                   hdr2$ = "Sales Order #    "
                   hdr6$ = "Shp Date"
                   hdr9$ = "  Schedule"
                   if fieldnr% > 0% then L42180
                       init(hex(86)) lfac$(), dfac$()
                       goto L42190
L42180:            init(hex(8c)) lfac$(), dfac$()
L42190:            lfac$(fieldnr%), dfac$(fieldnr%) = hex(81)
                   goto L42320
L42210:       mat field1$ = po$       : mat field2$ = poline$
              mat field3$ = duedate$  : mat field4$ = carrier$
              hdr2$ = "Purchase Oder #  "
              hdr6$ = "Due Date"
              hdr9$ = "Carrier   "
              if fieldnr% > 0% then L42290
                  init(hex(86)) dfac$() :  init(hex(8c)) lfac$()
                  goto L42300
L42290:       init(hex(8c)) dfac$(), lfac$()
L42300:       dfac$(fieldnr%) = hex(81)

L42320:       gosub set_pf2
              str(line2$,1%,40%) = "Schedule Ship Date, Quantity and Carr~
        ~ier"
L42350:     accept                                                       ~
               at (01,02), "Shipping Schedule Allotment by Part",        ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Part:",                                      ~
               at (04,08), fac(hex(84)), p_part$(p%)            , ch(25),~
               at (04,36), fac(hex(8c)), p_descr$(p%)           , ch(34),~
               at (05,02), "ATC1:",                                      ~
               at (05,08), fac(hex(84)), p_atc1$(p%)            , ch(10),~
               at (05,19), "ATC2:",                                      ~
               at (05,25), fac(hex(84)), p_atc2$(p%)            , ch(10),~
               at (05,36), "Qty Available:",                             ~
               at (05,51), fac(pfac$),   p_qty$(p%)             , ch(10),~
                                                                         ~
               at (07,02), fac(hex(ac)), hdr1$                  , ch(09),~
               at (07,12), fac(hex(ac)), hdr2$                  , ch(16),~
               at (07,29), fac(hex(ac)), hdr3$                  , ch(03),~
               at (07,33), fac(hex(ac)), hdr4$                  , ch(02),~
               at (07,36), fac(hex(ac)), hdr5$                  , ch(08),~
               at (07,45), fac(hex(ac)), hdr6$                  , ch(08),~
               at (07,54), fac(hex(ac)), hdr7$                  , ch(03),~
               at (07,58), fac(hex(ac)), hdr8$                  , ch(10),~
               at (07,69), fac(hex(ac)), hdr9$                  , ch(10),~
                                                                         ~
               at (08,02), fac(hex(8c)),   customer$ (l%+  1%)  , ch(09),~
               at (09,02), fac(hex(8c)),   customer$ (l%+  2%)  , ch(09),~
               at (10,02), fac(hex(8c)),   customer$ (l%+  3%)  , ch(09),~
               at (11,02), fac(hex(8c)),   customer$ (l%+  4%)  , ch(09),~
               at (12,02), fac(hex(8c)),   customer$ (l%+  5%)  , ch(09),~
               at (13,02), fac(hex(8c)),   customer$ (l%+  6%)  , ch(09),~
               at (14,02), fac(hex(8c)),   customer$ (l%+  7%)  , ch(09),~
               at (15,02), fac(hex(8c)),   customer$ (l%+  8%)  , ch(09),~
               at (16,02), fac(hex(8c)),   customer$ (l%+  9%)  , ch(09),~
               at (17,02), fac(hex(8c)),   customer$ (l%+ 10%)  , ch(09),~
               at (18,02), fac(hex(8c)),   customer$ (l%+ 11%)  , ch(09),~
               at (19,02), fac(hex(8c)),   customer$ (l%+ 12%)  , ch(09),~
                                                                         ~
               at (08,12), fac(hex(8c)),   field1$   (l%+  1%)  , ch(16),~
               at (09,12), fac(hex(8c)),   field1$   (l%+  2%)  , ch(16),~
               at (10,12), fac(hex(8c)),   field1$   (l%+  3%)  , ch(16),~
               at (11,12), fac(hex(8c)),   field1$   (l%+  4%)  , ch(16),~
               at (12,12), fac(hex(8c)),   field1$   (l%+  5%)  , ch(16),~
               at (13,12), fac(hex(8c)),   field1$   (l%+  6%)  , ch(16),~
               at (14,12), fac(hex(8c)),   field1$   (l%+  7%)  , ch(16),~
               at (15,12), fac(hex(8c)),   field1$   (l%+  8%)  , ch(16),~
               at (16,12), fac(hex(8c)),   field1$   (l%+  9%)  , ch(16),~
               at (17,12), fac(hex(8c)),   field1$   (l%+ 10%)  , ch(16),~
               at (18,12), fac(hex(8c)),   field1$   (l%+ 11%)  , ch(16),~
               at (19,12), fac(hex(8c)),   field1$   (l%+ 12%)  , ch(16),~
                                                                         ~
               at (08,29), fac(hex(8c)),   field2$   (l%+  1%)  , ch(03),~
               at (09,29), fac(hex(8c)),   field2$   (l%+  2%)  , ch(03),~
               at (10,29), fac(hex(8c)),   field2$   (l%+  3%)  , ch(03),~
               at (11,29), fac(hex(8c)),   field2$   (l%+  4%)  , ch(03),~
               at (12,29), fac(hex(8c)),   field2$   (l%+  5%)  , ch(03),~
               at (13,29), fac(hex(8c)),   field2$   (l%+  6%)  , ch(03),~
               at (14,29), fac(hex(8c)),   field2$   (l%+  7%)  , ch(03),~
               at (15,29), fac(hex(8c)),   field2$   (l%+  8%)  , ch(03),~
               at (16,29), fac(hex(8c)),   field2$   (l%+  9%)  , ch(03),~
               at (17,29), fac(hex(8c)),   field2$   (l%+ 10%)  , ch(03),~
               at (18,29), fac(hex(8c)),   field2$   (l%+ 11%)  , ch(03),~
               at (19,29), fac(hex(8c)),   field2$   (l%+ 12%)  , ch(03),~
                                                                         ~
               at (08,34), fac(hex(8c)),   priority$ (l%+  1%)  , ch(01),~
               at (09,34), fac(hex(8c)),   priority$ (l%+  2%)  , ch(01),~
               at (10,34), fac(hex(8c)),   priority$ (l%+  3%)  , ch(01),~
               at (11,34), fac(hex(8c)),   priority$ (l%+  4%)  , ch(01),~
               at (12,34), fac(hex(8c)),   priority$ (l%+  5%)  , ch(01),~
               at (13,34), fac(hex(8c)),   priority$ (l%+  6%)  , ch(01),~
               at (14,34), fac(hex(8c)),   priority$ (l%+  7%)  , ch(01),~
               at (15,34), fac(hex(8c)),   priority$ (l%+  8%)  , ch(01),~
               at (16,34), fac(hex(8c)),   priority$ (l%+  9%)  , ch(01),~
               at (17,34), fac(hex(8c)),   priority$ (l%+ 10%)  , ch(01),~
               at (18,34), fac(hex(8c)),   priority$ (l%+ 11%)  , ch(01),~
               at (19,34), fac(hex(8c)),   priority$ (l%+ 12%)  , ch(01),~
                                                                         ~
               at (08,36), fac(hex(8c)),  orderdate$ (l%+  1%)  , ch(08),~
               at (09,36), fac(hex(8c)),  orderdate$ (l%+  2%)  , ch(08),~
               at (10,36), fac(hex(8c)),  orderdate$ (l%+  3%)  , ch(08),~
               at (11,36), fac(hex(8c)),  orderdate$ (l%+  4%)  , ch(08),~
               at (12,36), fac(hex(8c)),  orderdate$ (l%+  5%)  , ch(08),~
               at (13,36), fac(hex(8c)),  orderdate$ (l%+  6%)  , ch(08),~
               at (14,36), fac(hex(8c)),  orderdate$ (l%+  7%)  , ch(08),~
               at (15,36), fac(hex(8c)),  orderdate$ (l%+  8%)  , ch(08),~
               at (16,36), fac(hex(8c)),  orderdate$ (l%+  9%)  , ch(08),~
               at (17,36), fac(hex(8c)),  orderdate$ (l%+ 10%)  , ch(08),~
               at (18,36), fac(hex(8c)),  orderdate$ (l%+ 11%)  , ch(08),~
               at (19,36), fac(hex(8c)),  orderdate$ (l%+ 12%)  , ch(08),~
                                                                         ~
               at (08,45), fac(lfac$( 1%)),  field3$ (l%+  1%)  , ch(08),~
               at (09,45), fac(lfac$( 2%)),  field3$ (l%+  2%)  , ch(08),~
               at (10,45), fac(lfac$( 3%)),  field3$ (l%+  3%)  , ch(08),~
               at (11,45), fac(lfac$( 4%)),  field3$ (l%+  4%)  , ch(08),~
               at (12,45), fac(lfac$( 5%)),  field3$ (l%+  5%)  , ch(08),~
               at (13,45), fac(lfac$( 6%)),  field3$ (l%+  6%)  , ch(08),~
               at (14,45), fac(lfac$( 7%)),  field3$ (l%+  7%)  , ch(08),~
               at (15,45), fac(lfac$( 8%)),  field3$ (l%+  8%)  , ch(08),~
               at (16,45), fac(lfac$( 9%)),  field3$ (l%+  9%)  , ch(08),~
               at (17,45), fac(lfac$(10%)),  field3$ (l%+ 10%)  , ch(08),~
               at (18,45), fac(lfac$(11%)),  field3$ (l%+ 11%)  , ch(08),~
               at (19,45), fac(lfac$(12%)),  field3$ (l%+ 12%)  , ch(08),~
                                                                         ~
               at (08,54), fac(hex(8c)),    qtyflag$ (l%+  1%)  , ch(03),~
               at (09,54), fac(hex(8c)),    qtyflag$ (l%+  2%)  , ch(03),~
               at (10,54), fac(hex(8c)),    qtyflag$ (l%+  3%)  , ch(03),~
               at (11,54), fac(hex(8c)),    qtyflag$ (l%+  4%)  , ch(03),~
               at (12,54), fac(hex(8c)),    qtyflag$ (l%+  5%)  , ch(03),~
               at (13,54), fac(hex(8c)),    qtyflag$ (l%+  6%)  , ch(03),~
               at (14,54), fac(hex(8c)),    qtyflag$ (l%+  7%)  , ch(03),~
               at (15,54), fac(hex(8c)),    qtyflag$ (l%+  8%)  , ch(03),~
               at (16,54), fac(hex(8c)),    qtyflag$ (l%+  9%)  , ch(03),~
               at (17,54), fac(hex(8c)),    qtyflag$ (l%+ 10%)  , ch(03),~
               at (18,54), fac(hex(8c)),    qtyflag$ (l%+ 11%)  , ch(03),~
               at (19,54), fac(hex(8c)),    qtyflag$ (l%+ 12%)  , ch(03),~
                                                                         ~
               at (08,58), fac(hex(8c)),      unqty$ (l%+  1%)  , ch(10),~
               at (09,58), fac(hex(8c)),      unqty$ (l%+  2%)  , ch(10),~
               at (10,58), fac(hex(8c)),      unqty$ (l%+  3%)  , ch(10),~
               at (11,58), fac(hex(8c)),      unqty$ (l%+  4%)  , ch(10),~
               at (12,58), fac(hex(8c)),      unqty$ (l%+  5%)  , ch(10),~
               at (13,58), fac(hex(8c)),      unqty$ (l%+  6%)  , ch(10),~
               at (14,58), fac(hex(8c)),      unqty$ (l%+  7%)  , ch(10),~
               at (15,58), fac(hex(8c)),      unqty$ (l%+  8%)  , ch(10),~
               at (16,58), fac(hex(8c)),      unqty$ (l%+  9%)  , ch(10),~
               at (17,58), fac(hex(8c)),      unqty$ (l%+ 10%)  , ch(10),~
               at (18,58), fac(hex(8c)),      unqty$ (l%+ 11%)  , ch(10),~
               at (19,58), fac(hex(8c)),      unqty$ (l%+ 12%)  , ch(10),~
                                                                         ~
               at (08,69), fac(dfac$( 1%)),  field4$ (l%+  1%)  , ch(10),~
               at (09,69), fac(dfac$( 2%)),  field4$ (l%+  2%)  , ch(10),~
               at (10,69), fac(dfac$( 3%)),  field4$ (l%+  3%)  , ch(10),~
               at (11,69), fac(dfac$( 4%)),  field4$ (l%+  4%)  , ch(10),~
               at (12,69), fac(dfac$( 5%)),  field4$ (l%+  5%)  , ch(10),~
               at (13,69), fac(dfac$( 6%)),  field4$ (l%+  6%)  , ch(10),~
               at (14,69), fac(dfac$( 7%)),  field4$ (l%+  7%)  , ch(10),~
               at (15,69), fac(dfac$( 8%)),  field4$ (l%+  8%)  , ch(10),~
               at (16,69), fac(dfac$( 9%)),  field4$ (l%+  9%)  , ch(10),~
               at (17,69), fac(dfac$(10%)),  field4$ (l%+ 10%)  , ch(10),~
               at (18,69), fac(dfac$(11%)),  field4$ (l%+ 11%)  , ch(10),~
               at (19,69), fac(dfac$(12%)),  field4$ (l%+ 12%)  , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L43900
                   call "MANUAL" ("SHPSCHAL")
                   goto L42350

L43900:        if keyhit% <> 15% then L43940
                   call "PRNTSCRN"
                   goto L42350

L43940:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
        if edit% <> 1% then L44300
            pf$(1%)= "(1)St Ovr  (4)Prev  (7)Up     (10)See Qty   (19)U"&~
                     "nschedule     (13)Instructions"
            pf$(2%)= "(2)First   (5)Next  (8)Toggle (11)See ATC   (20)P"&~
                     "rev Part      (15)Print Screen"
            pf$(3%)= "(3)Last    (6)Down            (17)Ord Stat  (21)N"&~
                     "ext Part      (16)Save Data   "
            pfkeys$ = hex(0102030405060708ff0a0bff0dff0f1011ff13141500)

*        Deactivate PF 2 Thru 7 If Less Than 13 Lines...
            if maxlines% > 12% then L44140
                str(pf$(1%),12%,18%) = " "
                str(pf$(2%), 1%,20%) = " "
                str(pf$(3%), 1%,20%) = " "
                str(pfkeys$,2%,6%) = hex(ff)
L44140
*        Deactivate PF 20 and 21 If Only 1 Part...
            if maxparts% > 1% then L44200
                str(pf$(2%),45%,19%) = " "
                str(pf$(3%),45%,19%) = " "
                str(pfkeys$,20%,2%) = hex(ff)
                return
L44200
*        Deactivate PF 20 On 1st Part...
            if p_part$(p%) <> p_part$(1%) then L44240
                str(pf$(2%),45%,19%) = " "
                str(pfkeys$,20%,1%) = hex(ff)
L44240
*        Deactivate PF 21 If Last Part...
            if p_part$(p%) <> p_part$(maxparts%) then L44280
                str(pf$(3%),45%,19%) = " "
                str(pfkeys$,21%,1%) = hex(ff)
L44280:     return

L44300:     pf$(1%)= "(1)Start Over                                    "&~
                     "              (13)Instructions"
            pf$(2%)= "                                                 "&~
                     "              (15)Print Screen"
            pf$(3%)= "(3)Start Line Over                               "&~
                     "                              "
            pfkeys$ = hex(01ff03ffffffffffffffffff0dff0fffffffffffff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test Data For Selection Screen Items.                     *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50170,         /* Part Range             */~
                              L50240,         /* Customer Range         */~
                              L50310,         /* Req Ship Date Range    */~
                              L50530,         /* Sales Order Range      */~
                              L50600,         /* Store Number           */~
                              L50680,         /* Print Pick List?       */~
                              L50740          /* Print BOL?             */
            return

L50170: REM Test for Part Number Range            PART_RNG$()
            errormsg$ = " "
            call "TESTRNGE" (part_rng$(1%), part_rng$(2%),               ~
                             part_rng$(3%), part_rng$(4%),               ~
                             errormsg$, #15)
            return

L50240: REM Test for Customer                     CUST_RNG$()
            errormsg$ = " "
            call "TESTRNGE" (cust_rng$(1%), cust_rng$(2%),               ~
                             cust_rng$(3%), cust_rng$(4%),               ~
                             errormsg$, #11)
            return

L50310: REM Test for Required Ship Date           SHIP_RNG$()
            errormsg$ = " "
            if ship_rng$(1%) = " " then ship_rng$(1%) = "ALL"
            if ship_rng$(1%) <> "ALL" then L50380
               ship_rng$(2%) = " "
               ship_rng$(3%) = all(hex(00))
               ship_rng$(4%) = all(hex(ff))
            return
L50380:     call "DATEOKC" (ship_rng$(1%), r%, errormsg$)
            if errormsg$ <> " " then return
            ship_rng$(3%) = ship_rng$(1)
            call "DATUFMTC" (ship_rng$(3%))
            if ship_rng$(2%) <> " " then L50450
               ship_rng$(2%) = ship_rng$(1%)
               ship_rng$(4%) = ship_rng$(3%)
            return
L50450:     call "DATEOKC" (ship_rng$(2%), r%, errormsg$)
            if errormsg$ <> " " then return
            ship_rng$(4) = ship_rng$(2%)
            call "DATUFMTC" (ship_rng$(4%))
            if ship_rng$(3%) <= ship_rng$(4%) then L50510
              errormsg$ = "Starting Date Can Not Be Greater Than Ending Date"
L50510:     return

L50530: REM Test for Sales Order                  SO_RNG$()
            errormsg$ = " "
            call "TESTRNGE" (so_rng$(1%), so_rng$(2%),                   ~
                             so_rng$(3%), so_rng$(4%),                   ~
                             errormsg$, #6)
            return

L50600: REM Test for Store Number                 STORE$
            errormsg$ = " "
            if sstore$ = "?" then sstore$ = " "
            call "GETCODE" (#38, sstore$, descr$, 0%, 0, f1%(38%))
                if f1%(38%) <> 0% then return
            errormsg$ = "You MUST Enter Or Select A Store Number"
            return

L50680: REM Test for Print Pick List?             PRTPICKL$
            errormsg$ = " "
            if prtpickl$ = "Y" or prtpickl$ = "N" or prtpickl$ = "I"     ~
                                                              then return
                errormsg$ = "You MUST Enter 'Y', 'N', or 'I'"
            return

L50740: REM Test for Print BOL?                   PRTBOL$
            errormsg$ = " "
            if prtbol$ = "Y" or prtbol$ = "N" or prtbol$ = "I" then return
                errormsg$ = "You MUST Enter 'Y', 'N', or 'I'"
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test Data For Line Item Modifications.                    *~
            *************************************************************

        deffn'152(fieldnr%)
            errormsg$ = " "
            d% = fieldnr%
            if main% = 0% then L52110 else L52540

L52110: REM Screen 1 - Test Ship Date...
            call "DATEOK" (field3$(l%+d%), r%, errormsg$)
                shipdate$(l%+d%) = field3$(l%+d%)
                convert field4$(l%+d%) to scqty(l%+d%), data goto L52160
                call "CONVERT" (scqty(l%+d%), 2.2, scqty$(l%+d%))
L52160:         if errormsg$ <> " " then return
            call "DATUNFMT" (field3$(l%+d%))
            if field3$(l%+d%) >= undate$ then L52220
                errormsg$ = "Cannot Schedule Prior To Today"
                call "DATEFMT" (field3$(l%+d%))
                return
L52220:     call "DATEFMT" (field3$(l%+d%))

        REM Test Schedule Quantity...
            if field4$(l%+d%) = " " then field4$(l%+d%) = "0"
            call "NUMTEST" (field4$(l%+d%), 0, 999999999, errormsg$, 2.2,~
                            scqty(l%+d%))
                if errormsg$ <> " " then return
            if scqty(l%+d%) <= unqty(l%+d%) then L52410
                if overship$ = "Y" then goto L52340
                    errormsg$ = "Schedule Quantity Cannot Exceed Uncomm"&~
                                "itted Quantity"
                    return
L52340:     call "SHPOVRSB" (p_part$(p%), soline$(l%+d%), ouqty(l%+d%),  ~
                  scqty(l%+d%), def_percent(p%), def_unit(p%),           ~
                  sys_def_percent(p%), u3%)
            if u3% = 16% then L52410
                if u3% <> 1% then L52340
                    errormsg$ = "Please Re-Enter Schedule Quantity"
                    return
L52410:     if scqty(l%+d%) <= p_qty(p%) then L52430
                neg_avail%(p%) = 1%
L52430:     call "CONVERT" (scqty(l%+d%), 2.2, field4$(l%+d%))
            scqty$(l%+d%) = field4$(l%+d%)
            p_qty(p%) = p_qty(p%) - scqty(l%+d%) + lastscqty(l%+d%)
                call "CONVERT" (p_qty(p%), -2.2, p_qty$(p%))
            unqty(l%+d%) = ouqty(l%+d%) - scqty(l%+d%)
            if unqty(l%+d%) < 0 then unqty(l%+d%) = 0
            call "CONVERT" (unqty(l%+d%), 2.2, unqty$(l%+d%))
            lastscqty(l%+d%) = scqty(l%+d%)
            return

L52540: REM Screen 1 Toggled - Test Carrier Code...
            if field4$(l%+d%) = "?" then field4$(l%+d%) = " "
            readkey$ = "CARRIERS " & str(field4$(l%+d%),1%,9%)
            descr$ = hex(06) & "Select Carrier, PF(16) If None"
            call "PLOWCODE" (#14, readkey$, descr$, 9%, 0.30, f1%(14%))
                if f1%(14%) <> 0% then L52630
            if str(field4$(l%+d%),1%,9%) = " " then L52640
                errormsg$ = "You MUST Enter Or Select A Valid Carrier"
L52630:     field4$(l%+d%)  = str(readkey$,10%,9%)
L52640:     carrier$(l%+d%) = str(field4$(l%+d%),1%,9%)
            return

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1993  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "SHOSTAT" ("One Moment Please")
            end
