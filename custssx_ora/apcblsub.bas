        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCBLSUB                             *~
            *  Creation Date     - 01/16/97                             *~
            *  Last Modified Date- 11/10/98                             *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - APC Version of Print B.O.L. used by  *~
            *                      (SHPDOCPR)                           *~
            *                                                           *~
            *                                                           *~
            *  Code Tables Used  - (PLAN SHFT) - Shift Codes            *~
            *                      (PLAN DEPT) - Department Codes       *~
            *                      (PLAN PROC) - Planning Process Codes *~
            *                                                           *~
            *  Special Comments  - New Sub (APCPRZSB)                   *~
            *                                                           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/16/97 ! Mods to Switch (SHPBOLS) to New Planning ! RHH *~
            *          !   System.                                !     *~
            * 02/12/97 ! Mod to Change Max No. of Detail Lines    ! RHH *~
            *          !    MAX_LINES = 24 Changed from 28 at     !     *~
            *          !    (Line = 9150)                         !     *~
            * 04/03/97 ! Mod to Print the new Series Name in the  ! RHH *~
            *          !    Part Number description.              !     *~
            * 11/13/97 ! Mod for Upgrade to new Release R6.04.03  ! RHH *~
            * 03/07/98 ! Y2K Modifications                        ! LDJ *~
            * 07/01/98 ! (EWD001) Mod to print BOL's by Load      ! RHH *~
            *          !    and maintain Drop Seq. Uses a New Temp!     *~
            *          !    work file (EWDBOLS).                  !     *~
            * 10/07/98 ! (EWD002) New Sort for BOL to be in same  ! RHH *~
            *          !    Order as the New Delivery Tickets.    !     *~
            *          !    (EWDBOL01) and (EWDBOL02)             !     *~
            * 10/20/98 ! (EWD003) Mod for new Glass Warranty      ! RHH *~
            *          !    Do not display on the BOL             !     *~
            * 11/10/98 ! (EWD004) Mod for Private Label and Series! RHH *~
            * 05/07/02 ! (EWD005) Mods to get description from    ! CMG *~
            *          !          Oracle.                         !     *~
            *06/06/2011! (AWD006) - add orcl usr & pswd lookup    ! CMG *~
            *          !                                          !     *~    
            *************************************************************

            sub "APCBLSUB" (load$, #3, #4, #5, #6, #7) 

        REM *************************************************************~
            *THIS SUBROUTINE IS CALLED BY 'SHPDOCPR'. IT PRINTS BILLS OF*~
            *LADING BASED ON THE CRITERIA PASSED.                       *~
            *************************************************************

        com        /* FIELDS COMMON TO ALL MODULES                     */~
            bcklines_key$19,             /* Key to BCKLINES            */~
            bckmastr_key$25,             /* Key to BCKMASTR            */~
            bol$3,                       /* Bill of lading # (BCKPRIDX)*/~
            carton$7,                    /* Edited # of cartons        */~
            cust_code$9,                 /* Customer code from BCKPRIDX*/~
            date$8,                      /* Date for screen display    */~
            from_date%,                  /* Low date for compare       */~
            from_stor$3,                 /* Low store for compare      */~
            hdr$40,                      /* ASKUSER constant           */~
            hdr_desc$30,                 /* Header description         */~
            high_date$10,                /* High date for compare               (Y2K, LDJ)*/~
            i$(24%)80,                   /* Screen image               */~
            invoice$8,                   /* Invoice # from SHPHDRS     */~
            low_date$10,                 /* Low date for compare       */~
            msg$(3%)79,                  /* ASKUSER constant           */~
            part_nmbr$25,                /* Part # from BCKLINES       */~
            part_desc$32,                /* Part desc from BCKLINES    */~
            po$16,                       /* PO # from BCKMASTR         */~
            po_line$3,                   /* PO line #                  */~
            qtyship$10,                  /* Edited quantity shipped    */~
            schd_date$8,                 /* Sched ship date fr SHPHDRS */~
            ship_date$8,                 /* Ship date from BCKPRIDX    */~
            ship_to$(6%)31,              /* Ship to name/addr- BCKMASTR*/~
            shphdrs_key$28,              /* Key to SHPHDRS             */~
            shplines_key$22,             /* Key to SHPLINES            */~
            so$16,                       /* Sales order # from BCKPRIDX*/~
            so$(2%,2%)16,                /* Sales Order # Range For Pri*/~
            stock_uom$4,                 /* Stock UOM from BCKMASTR    */~
            stor_code$3,                 /* Store code from BCKPRIDX   */~
            to_date%,                    /* High date for compare      */~
            to_stor$3,                   /* High store for compare     */~
            type$1,                      /* 'A'=Ackn, 'B'=BOL, 'P'-Pick*/~
            userid$3,                    /* Current User Id            */~
            weight$6,                    /* Edited shipment weight     */~
                   /* COMMON DATA FILE ARRAYS                          */~
            f2%(30%),                    /* = 0 if the file is open    */~
            f1%(30%),                    /* = 1 if READ was successful */~
            fs%(30%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(30%)20                 /* Text from file opening     */

        dim        /* LOCAL VARIABLES                                  */~
            load$5,                      /* Load No/Check Load (EWD001)*/~
            apc$(3%)30,                  /*                            */~
            apc_scr$120,                 /*                            */~
            apc_prt$60,                  /*                            */~
            apc_sze$20,                  /*                            */~
            act_date$6,                  /* Actual ship date - SHPHDRS */~
            apc_terms$20,                /* Terms Code Description     */~
            apc_ship$100,                /* Shipping Instructions      */~
            apc_textid$4,                /*                            */~
            blankdate$6,                 /* Null Date value test          (Y2K, LDJ) */~
            total_qty$10,                /* TOTAL QUANTITY SHIPPED     */~
            oracle_tot$10,               /* TOTAL QUANTITY SHIPPED  ORA*/~
            brk_stor$3,                  /* Controls store breaks      */~
            carr_code$6,                 /* Carrier code from SHPHDRS  */~
            carr_name$30,                /* Carrier name               */~
            crhold$1,                    /* Credit Hold Flag           */~
            or_key1$8,                   /* Customer S.O.              */~
            or_load$5,                   /* Load Number                */~
            or_drop$2,                   /* Customer Drop Number       */~
            or_cust$9,                   /* Customer Code              */~
            or_po$16,                    /* Customer P.O. Number       */~
            or_so$8,                     /* Customer S.O.              */~
            wrk_key$50, wrk_key1$50,     /* Delivery Sort              */~
            wrk_rec$60, seq$5,           /* Store (BCKPRIDX) Data      */~
            sav_po$16, sav_key2$16,      /* Delivery Sort              */~
            cust_phone$10,               /* CUSTOMER PHONE NUMBER      */~
            prt_phone$14,                /* FORMATTED PHONE NUMBER     */~
            fob$20,                      /* FOB from BCKMASTR          */~
            lot$(30)6,                   /* Lot #s from xxxLINES       */~
            mode$5,                      /* Mode for Opening Work File */~
            package_qty$4,               /* Number of package units    */~
            package_type$8,              /* Type of package unit       */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            prog$8,                      /* Program name               */~
            qty_ship(30%),               /* Qty shipped from xxxLINES  */~
            rptid$6,                     /* Report ID for headers      */~
            rs$2,                        /* Check for 'RS' in SERMASTR */~
            s_23m$3,                     /* Model Code for Series      */~
            s_23$8,                      /* New Series Code            */~
            s_so$8,                      /* Sales Order       (EWD004) */~
            s_ln$3,                      /* Sales Order Line  (EWD004) */~
            s_prv$30,                    /* Private Label Name(EWD004) */~
            s_1$2,                       /* Private Label Code(EWD004) */~ 
            ser$6,                       /* Serial # literal           */~
            serbol$3,                    /* Check for BOL in SERMASTR  */~
            serprint$95,                 /* Serial # print area        */~
            serso$16,                    /* Check for SO in SERMASTR   */~
            serial$20,                   /* Serial # from SERMASTR     */~
            serkey$99,                   /* SERMASTR key               */~
            so_seqnr$3,                  /* SO sequence # from BCKLINES*/~
            store$(4%)30                 /* Store name and address     */

        dim                              /* FILE - (TEXTFILE)          */~
            text_key$11,                 /* TEXT PRIMARY KEY           */~
            sav_key1$11,                 /* TEXT LOOKUP KEY            */~
            textid$4,                    /* S.O. TEXTID$               */~
            text$(2%)70,                 /* TEXT BUFFER ARRAY          */~
            text_desc$60,                /* TEXT DESCRIPTION           */~
            sku_code$3,                  /* SKU CODE FROM CUSTOMER     */~
            sku_no$25,                   /* SKU NUMBER                 */~
            sku_key$28                   /* PRIMARY KEY                */

        dim                              /*  (EWD005)                  */~
            error$256,                   /* Error String               */~
            ora_seqnr$3,                 /* Oracle SO Seq Number       */~
            sav_seqnr$3,                 /* Save Sequence Number       */~
            ora_usr$3,                   /* Userid for Stored Procedure*/~
            ora_desc$250,                /* Oracle Part Description    */~
            server$25,                   /* Connection String          */~
            user$25,                     /* User Name to Connect       */~
            pass$25,                     /* Password to Connect        */~
            field$256,                   /* Query Return Field         */~
            stmt1$250,                   /* First Query String         */~
            stmt2$250,                   /* Second Query String        */~
            fields$(4%)100               /* String of Oracle Info      */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(EWD) Version for Printing B.O.L.       "
            pname$ = "APCBLSUB - Rev: R7.00"

        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! APCPLNOR ! New Planning Sales Order History         *~
            * #2  ! APCPLNDT ! Master Planning File            (EWD004) *~ 
            * #3  ! BCKPRIDX ! SO Document Print Index File             *~
            * #4  ! BCKMASTR ! BACKLOG MASTER FILE (GET STORE NUMBER)   *~
            * #5  ! BCKLINES ! BACK LOG LINE ITEM FILE                  *~
            * #6  ! SHPHDRS  ! Shipment Scheduling / Pre-Invoicing- Hea *~
            * #7  ! SHPLINES ! Shipment Scheduling / Pre-Invoicing- Lin *~
            * #8  ! STORNAME ! Store Information File                   *~
            * #9  ! SERMASTR ! Serial Number Tracking Master File       *~
            * #10 ! GENCODES ! General Codes File                       *~
            * #11 ! HNYMASTR ! PART MASTER FILE                         *~
            * #15 ! AMTBOMIF ! BOM GENERATOR MASTER VALIDITY FILE       *~
            * #16 ! APCSKUNO ! APC SKEW NUMBER FILE                     *~
            * #17 ! TEXTFILE ! SYSTEM TEXT FILE                         *~
            * #20 ! CUSTOMER ! CUSTOMER MASTER FILE                     *~
            * #22 ! EWDBOL01 ! 1st Sort P.O. within Drop (EWD002)       *~
            * #23 ! EWDBOL02 ! 2nd Sort Reverse P.O. Seq for Load(EWD002)*~
            * #24 ! SYSFILE2 ! Caelus Management System General Informa *~
            *************************************************************~

            select #1,   "APCPLNOR",                                     ~
                        varc,     indexed,  recsize = 170,               ~
                        keypos =    1, keylen =   51,                    ~
                        alt key  1, keypos =   27, keylen =  25,         ~
                            key  2, keypos =   70, keylen =   8, dup,    ~
                            key  3, keypos =   78, keylen =   8, dup,    ~
                            key  4, keypos =   52, keylen =   8,         ~
                            key  5, keypos =   36, keylen =  16, dup
                                                     /* (EWD004) Begin */
            select #2,  "APCPLNDT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =  24,  keylen =  23,                     ~
                        alt key 1, keypos = 47, keylen = 57,             ~
                            key 2, keypos = 53, keylen = 51,             ~
                            key 3, keypos =  1, keylen = 23, dup,        ~
                            key 4, keypos = 96, keylen =  8, dup
                                                     /* (EWD004) End   */  
            select #8, "STORNAME", varc, indexed, recsize = 300,         ~
                keypos = 1, keylen = 3

            select #9, "SERMASTR", varc, indexed, recsize = 300,         ~
                keypos = 52, keylen = 45,                                ~
                alt key  1, keypos = 32, keylen = 45,                    ~
                    key  2, keypos =  1, keylen = 76

            select #10, "GENCODES", varc, indexed, recsize = 128,        ~
                keypos = 1, keylen = 24


            select #11,"HNYMASTR", varc, indexed, recsize = 900,         ~
                        keypos =   1, keylen =  25,                      ~
                    alt key 1, keypos = 102, keylen =  9, dup,           ~
                        key 2, keypos =  90, keylen =  4, dup


            select #15, "AMTBOMIF",                                      ~
                        varc,     indexed,  recsize =  120,              ~
                        keypos =  1,   keylen =  32

            select #16, "APCSKUNO",                                      ~
                        varc,     indexed,  recsize =  73,               ~
                        keypos =    1, keylen =  28,                     ~
                        alt key  1, keypos =   29, keylen =  28, dup


            select #17, "TXTFILE",                                       ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =   1, keylen =  11

            select #20, "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #22, "EWDBOL01",                                      ~
                        varc,     indexed,  recsize = 110,               ~
                        keypos =   1, keylen =  50

            select #23, "EWDBOL02",                                      ~                                      
                        varc,     indexed,  recsize = 110,               ~
                        keypos =   1, keylen =  50  

           select #24,  "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 0%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%), 0%, rslt$(2%))
            call "OPENCHCK" (#8, fs%(8%), f2%(8%), 0%, rslt$(8%))
            call "OPENCHCK" (#9, fs%(9%), f2%(9%), 0%, rslt$(9%))
            call "OPENCHCK" (#10, fs%(10%), f2%(10%), 0%, rslt$(10%))
            call "OPENCHCK" (#11, fs%(11%), f2%(11%), 0%, rslt$(11%))
            call "OPENCHCK" (#15, fs%(15%), f2%(15%), 0%, rslt$(15%))
            call "OPENCHCK" (#16, fs%(16%), f2%(16%), 0%, rslt$(16%))
            call "OPENCHCK" (#17, fs%(17%), f2%(17%), 0%, rslt$(17%))
            call "OPENCHCK" (#20, fs%(20%), f2%(20%), 0%, rslt$(20%))
/*(AWD006) */
            call "OPENCHCK" (#24, fs%(24%), f2%(24%), 0%, rslt$(24%))

            call "EXTRACT" addr("ID", ora_usr$)
            call "DATUFMTC" (blankdate$)                      /* (Y2K, LDJ) */
            gosub oracle_connect 
            call "SHOSTAT" ("Sorting Bills of Lading")
            gosub delivery_sort_bol                        

            call "SHOSTAT" ("Bills of Lading being selected for print")
            rptid$ = "SHP004"
            call "EXTRACT" addr("CF", prog$)
            init(" ") plowkey$, wrk_key$             /* (EWD001) - Mod  */
            nbr_recs% = 0% : max_lines% = 24%
            total_qty, tot_qty, oracle_tot = 0.0 
            total_qty$, oracle_tot$ = " "

        REM *************************************************************~
            *          M A I N   P R O G R A M   L O O P                *~
            *************************************************************

        plow_thru_bckpridx
           REM CALL "PLOWNXT1" (#3, PLOWKEY$, 1%, F1%(3))
           REM IF F1%(3) = 0% THEN GOTO END_OF_REPORT
           REM GET #3 USING 10110, STOR_CODE$, SHIP_DATE$, TYPE$,         ~
           REM     CUST_CODE$, SO$, BOL$

           REM call "PLOWNXT1" (#21, plowkey$, 1%, f1%(21))
           REM if f1%(21) = 0% then goto end_of_report
                                               /* (EWD002) - Mod        */
           read #23,hold,key > wrk_key$, using L10180, wrk_key$, plowkey$,~
                                               eod goto end_of_report
L10180:       FMT CH(50), CH(48)
            get #23 using L10183, stor_code$, ship_date$, type$,          ~
                    cust_code$, so$, bol$
                                               /* (EWD001) - End       */
        REM RECORD LAYOUT FOR FILE 'BCKPRIDX' ***************************
                FMT  XX(1),              /* 'A'ck, 'B'ol, or 'P'ick    */~
                     CH(3),              /* Store code                 */~
                     CH(6),              /* Ship date                  */~
                     CH(1),              /* Type code (A,B,P) again    */~
                     CH(9),              /* Customer code              */~
                     CH(16),             /* Sales order number         */~
                     CH(3),              /* Bill of lading number      */~
                     XX(6)               /* Setup date                 */

                                         /* (EWD001 - EWD002)-Begin Mod*/
        REM RECORD LAYOUT FOR FILE 'EWDBOL02' **************************
L10183:         FMT  XX(50),             /* 'wrk_key$' Used for Sort   */~  
                     XX(1),              /* 'A'ck, 'B'ol, or 'P'ick    */~
                     CH(3),              /* Store code                 */~
                     CH(6),              /* Ship date                  */~
                     CH(1),              /* Type code (A,B,P) again    */~
                     CH(9),              /* Customer code              */~
                     CH(16),             /* Sales order number         */~
                     CH(3),              /* Bill of lading number      */~
                     XX(1)               /* Filler                     */
                                         /* (EWD001-EWD002)-End Mods   */

            if stor_code$ < from_stor$ then goto plow_thru_bckpridx
            if stor_code$ > to_stor$ then goto plow_thru_bckpridx
            if so$ <= so$(2,1) or so$ > so$(2,2) then plow_thru_bckpridx
            if low_date$ = "ALL" then goto L10310
            call "DATECONV" (ship_date$, ship_date%)    /* (Y2K, LDJ) */
            if low_date$ <> " " then goto L10280
            if ship_date$ = blankdate$ then goto L10310         /* (Y2K, LDJ) */
            goto L10290
L10280:     if ship_date% < from_date% then goto plow_thru_bckpridx
L10290:     if ship_date% > to_date% then goto plow_thru_bckpridx

L10310:     bckmastr_key$ = str(cust_code$,,9) & so$
            call "READ100" (#4, bckmastr_key$, f1%(4))
            if f1%(4) = 0% then goto bckpridx_deleter
            get #4 using L10370, po$, ship_to$(), apc_terms$, fob$,       ~
                                     apc_ship$, apc_textid$, crhold$
            if crhold$ = "H" then gosub ask_credit_hold
            if comp% <> 0% then goto plow_thru_bckpridx


        REM RECORD LAYOUT FOR FILE 'BCKMASTR' ***************************
L10370:         FMT  XX(9),              /* Customer code              */~
                     XX(16),             /* Sales Order number         */~
                     CH(16),             /* Purchase Order number      */~
                     6*CH(30),           /* Ship-to name & address     */~
                     XX(180),            /* Sold-to name & address     */~
                     CH(20),             /* Terms code                 */~
                     XX(20),             /* How to ship                */~
                     CH(20),             /* FOB                        */~
                     CH(100),            /* Shipping instructions      */~
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
            get #6 using L10890, schd_date$, carr_code$, fob$, act_date$, ~
                carton, weight, invoice$

        REM RECORD LAYOUT FOR FILE 'SHPHDRS' ****************************
L10890:         FMT  XX(9),              /* Customer code              */~
                     XX(16),             /* Sales Order number         */~
                     XX(3),              /* Bill of Lading #           */~
                     XX(3),              /* Store from Sales Order     */~
                     CH(6),              /* Scheduled Ship date        */~
                     CH(6),              /* Carrier code               */~
                     XX(20),             /* How to ship                */~
                     CH(20),             /* FOB                        */~
                     XX(100),            /* Shipping instructions      */~
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
            if bol$ <> " " and (act_date$ = " "  or act_date$ = blankdate$) then   /* (Y2K, LDJ) */ ~
                hdr_desc$ = "SHIPMENT SCHEDULING"
            if bol$ <> " " and act_date$ <> " " then                     ~
                hdr_desc$ = "SHIPPING"
            if hdr_desc$ = " " then hdr_desc$ = "*** UNKNOWN ***"
            page_nbr% = 0%
REM            gosub page_heading
            nbr_lines% = 99%
            bcklines_key$ = so$
            str(bcklines_key$,17) = all(hex(00))
            gosub oracle_procedure
            gosub oracle_line
        bcklines_loop
            gosub oracle_fields
            call "PLOWNEXT" (#5, bcklines_key$, 16%, f1%(5))
            if f1%(5) = 0% then goto end_of_lines
            if oci_err% > 0% and oracle% = 1% then goto end_of_lines
            get #5 using L11350, so_seqnr$, po_line$, part_nmbr$,        ~
                part_desc$, rhh_order, qty_ship(1), qty_open, qty_schd,  ~
                stock_uom$, lot$(1), textid$, s_1$      /* (EWD004)    */
            gosub glass_warranty          /* (EWD003) - Glass Warranty */
            if glass_warranty% = 1% then goto bcklines_loop
                                          /* (EWD003) - end            */        

        REM RECORD LAYOUT FOR FILE 'BCKLINES' ***************************
L11350:         FMT  XX(9),              /* Customer code              */~
                     XX(16),             /* Sales Order number         */~
                     CH(3),              /* Sales Order sequence number*/~
                     CH(3),              /* Purchase Order line number */~
                     CH(25),             /* Part number                */~
                     CH(32),             /* Part description           */~
                     XX(4),              /* Category code              */~
                     PD(14,4),           /* Order quantity             */~
                     PD(14, 4),          /* Quantity shipped           */~
                     PD(14, 4),          /* Open order quantity        */~
                     PD(14, 4),          /* Quantity scheduled         */~
                     XX(16),             /* Filler                     */~
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
                     XX(6),              /* Current Due date           */~
                     XX(6),              /* Ship date                  */~
                     CH(6),              /* Lot number                 */~
                     XX(10),             /* Filler                     */~
                     XX(6),              /* Project code               */~
                     XX(1),              /* Demand type                */~
                     XX(1),              /* Priority                   */~
                     CH(4),              /* Text ID                    */~
                     XX(1),              /* Allocation flag            */~
                     XX(8),              /* Invoice number             */~
                     XX(27),             /* Skip Not/Applic   (EWD004) */~
                     CH(2),              /* Private Label Code(EWD004) */~
                     XX(17)              /* Filler            (EWD004) */

            if bol$ = " " then goto print_detail_lines
            shplines_key$ = str(key(#5),,16) & str(bol$,,3) &            ~
                str(key(#5),17,3)
            call "READ100" (#7, shplines_key$, f1%(7))
            if f1%(7) = 0% then goto bcklines_loop
            get #7 using L11770, qty_schd, lot$(), qty_ship(),            ~
                                package_qty$, package_type$

        REM RECORD LAYOUT FOR FILE 'SHPLINES' ***************************
L11770:         FMT  XX(9),              /* Customer code              */~
                     XX(16),             /* Sales Order number         */~
                     XX(3),              /* Bill of Lading #           */~
                     XX(3),              /* Sales Order line nuber     */~
                     PD(14, 4),          /* Quantity Scheduled         */~
                     30*CH(6),           /* Lot numbers                */~
                     30*PD(14, 4),       /* Quantity Shipped           */~
                     XX(4),              /* Text ID for Invoice Line   */~
                     CH(4),              /* Number of package units    */~
                     CH(8),              /* Type of package units      */~
                     XX(125)             /* Filler                     */

            if (act_date$ <> " " and act_date$ <> blankdate$) then goto print_detail_lines      /* (Y2K, LDJ) */
                mat qty_ship = zer : qty_ship(1) = qty_schd
        print_detail_lines
            qty_open =  round(qty_open - qty_schd, 2)
            gosub detail_print_routine
            goto bcklines_loop

        end_of_lines
            convert total_qty to total_qty$, pic(###,###.##)

            convert oracle_tot to oracle_tot$, pic(###,###)

            total_qty, oracle_tot = 0.0
            if tot_oracle% = 1% then print using L60640,       ~
                                       oracle_tot$, total_qty$ ~
               else print using L60560, total_qty$
            print using L60480  /* End of Document */
            textid$ = apc_textid$
            gosub lookup_text
            print using L60500, text_desc$

        bckpridx_deleter
        REM DELETE #3
            delete #23
            gosub oracle_delete
            goto plow_thru_bckpridx

        end_of_report
            gosub delete_work              /* (EWD001) - Always Delete */
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
            call "SETPRNT" (rptid$, prog$, 0%, 1%)
            goto exit_program

        glass_warranty                               /* (EWD003) - Begin*/
            glass_warranty% = 0%
            if len(part_nmbr$) > 18% then return
            if str(part_nmbr$,5%,4%) <> "WARR" then return
               glass_warranty% = 1%
        return                                       /* (EWD003) - End  */ 


        page_heading
            page_nbr% = page_nbr% + 1% : nbr_lines% = 0%
            if page_nbr% > 1% then print using L60440, "* CONTINUED *"
        page_0_heading
            print page
            print using L60040, rptid$, date$, hdr_desc$
            if pagesw% <> 0% then return
            print using L60060 /* BOX 1 LINE 1 */
            print using L60080,            "#", "#"
            print using L60100
            print using L60120,            stor_code$, schd_date$,        ~
                cust_code$, str(so$,,16) & "-" & bol$, page_nbr%
            print using L60140
            gosub lookup_load
            gosub lookup_customer
            print using L60160                              /* New Line */
            print using L60172, or_load$, or_drop$          /* New Line */
        REM PRINT SKIP(1)
            print using L60240, ship_to$(1)
            print using L60260, ship_to$(2)
            print using L60280, ship_to$(3)
            print using L60300, ship_to$(4), po$, carr_name$, carton$,    ~
                weight$
            print using L60320, ship_to$(5)
            print using L60340, ship_to$(6), fob$
            print using L60360, prt_phone$, apc_terms$
            print using L60380, str(apc_ship$,1%,60%)
            print skip(1)
            if oracle% <> 0% then goto oracle_column
            print using L60180 /* COLUMN HEADERS */
            print using L60200, "#"
            print using L60220 /* UNDERSCORES */
            ser$ = "S/N'S:"
            return
        oracle_column
            print using L60600 /* COLUMN HEADERS */
            print using L60610, "#"
            print using L60620 /* UNDERSCORES */
            ser$ = "S/N'S:"
            return            

        detail_print_routine
            if str(fields$(),4%,1%) <> "0" and oracle% = 1%            ~
                              then goto print_config
            for l% = 1% to 30%
                                                 /* APC MOD - 11/09/90 */
                if (nbr_lines% + 3%) > max_lines% then gosub page_heading
                qtyship$ = " "
                if bol$ <> " " then goto L12460
                    if qty_ship(1) <> 0 then goto L12460
                    rhh_order = round(rhh_order, 2)
                    convert rhh_order to qtyship$, pic(###,###.##)

L12460:         if bol$ = " " then print_it
                              /* (EWD002) - 10/09/98                  */
                              /* - PRINT (0) SHIP LINES               */
                    if qty_ship(l%) = 0 and l% > 1% then return

                              /* (EWD002) - end of Mod                */

                    qty_ship(l%) = round(qty_ship(l%), 2)
                    convert qty_ship(l%) to qtyship$, pic(###,###.##)
        print_it
                gosub lookup_text
                gosub lookup_skew_no
REM             if oracle% = 1% then gosub get_oracle_print  ~
REM             else gosub build_print
                if oracle% <> 1% then gosub build_print
                   if oracle% = 1% then goto L01365
                   if l% <> 1% then goto L13060   /* APC MOD - 11/09/90 */
                      if err% = 0% then goto L13047

                      print using L60400, po_line$, so_seqnr$, part_nmbr$,~
                          part_desc$, stock_uom$, qty_open, qtyship$,    ~
                          lot$(l%), package_qty$, package_type$
                   if sku_no$ = " " and text_desc$ = " " then goto L13080
                      print using L60530, sku_no$, text_desc$
                      nbr_lines% = nbr_lines% + 1%
                      goto L13080

L13047:               print using L60400, po_line$, so_seqnr$, part_nmbr$,~
                          apc$(1)   , stock_uom$, qty_open, qtyship$,    ~
                          lot$(l%), package_qty$, package_type$
                      print using L60400, " ", " ", " ", apc$(2), " ",    ~
                                  " ", " ", " ", " ", " "
                      print using L60400, " ", " ", " ", apc$(3), " ",    ~
                                  " ", " ", " ", " ", " "
               if sku_no$ = " " and text_desc$ = " " then goto L13058
                      print using L60530, sku_no$, text_desc$
                      nbr_lines% = nbr_lines% + 1%

L13058:               nbr_lines% = nbr_lines% + 2%
                      goto L13080
L13060:            print using L60400, " ", " ", " ", " ", " ", " ",      ~
                               qtyship$, lot$(l%), " ", " "
                   goto L13080
L01365:         tot_qty = 0.0
                convert qtyship$ to tot_qty, data goto L01370
L01370:
                total_qty = round(total_qty + tot_qty, 2)


                gosub get_oracle_total
                print using L60580, ora_seqnr$, str(fields$(),26%,3%),    ~
                     str(fields$(),29%,3%), str(ora_desc$,1%,70%),       ~
                      stock_uom$, qty_open, qtyship$
                pos% = 71%
                for k% = 1% to 4%
                    if str(ora_desc$,pos%,70%) <> " " then                ~
                    print using L60580, " ", " ", " ", str(ora_desc$,pos%,70%)
                    if str(ora_desc$,pos%,70%) <> " " then                ~
                       nbr_lines% = nbr_lines% + 1%
                    pos% = pos% + 70%
                next k%
                print using L60580, " ", " ", " ", str(fields$(),282%,50%)
                nbr_lines% = nbr_lines% + 1%
                if sku_no$ = " " and text_desc$ = " " then goto L13080
                      print using L60530, sku_no$, text_desc$
                      nbr_lines% = nbr_lines% + 1%
                goto L13080

L13080:         nbr_lines% = nbr_lines% + 1%

                if ora_seqnr$ = " " then goto L13085
                if oracle% = 1% then goto L13085
                print using L60220
                nbr_lines% = nbr_lines% + 1%
L13085:         gosub serial_number_print
                if bol$ = " " then return
            next l%
            return
        print_config
            if (nbr_lines% + 3%) > max_lines% then gosub page_heading
            print using L60580, ora_seqnr$, str(fields$(),26%,3%),     ~
                     " ", str(ora_desc$,1%,70%), " ", " ", " "
            gosub get_oracle_total
            gosub oracle_fields
REM            if oci_err% = 100% then return
            if oci_err% = 100% then goto end_of_lines
            goto detail_print_routine


         get_oracle_total
                convert str(fields$(),26%,3%) to lne_tot, data goto L13090

L13090:         oracle_tot = round(oracle_tot + lne_tot, 2)

         return

        one_time_routine
            nbr_recs% = 1%
            select printer(134)
            call "SETPRNT" (rptid$, prog$, 0%, 0%)

        REM PRINT 'PAGE ZERO' -- THE SELECTION SCREEN *******************
            pagesw% = 1%
            gosub page_0_heading
            print skip (4)
            print using L60460, "   ---- SELECTION CRITERIA ----"
            print
            for n% = 6% to 19%
                print using L60460, i$(n%)
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
L13370:         FMT  POS(4), CH(30), POS(64), 3*CH(30)
            call "LINSMASH" (store$())
            return

        serial_number_print
            ser$ = "S/N'S:" : serkey$, serprint$ = " " : ser% = 0%
            serkey$ = "t" & str(stor_code$) & str(lot$(l%))
            str(serkey$, 32, 1) = hex(00)
        plow_serial_master
            call "PLOWALTS" (#9, serkey$, 2%, 31%, f1%(9))
            if f1%(9) = 0% then goto plow_serial_done
                get #9 using L13500, serial$, rs$, serso$, serbol$
L13500:         FMT /* SERMASTR #9 */ POS(32), CH(20), POS(216), CH(2),  ~
                     CH(16), CH(3)
                if rs$ <> "RS" then goto plow_serial_master
                if serso$ <> so$ then goto plow_serial_master
                if serbol$ <> bol$ then goto plow_serial_master
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
            print using L60420, ser$, serprint$
            ser$, serprint$ = " " : ser% = 0%
            nbr_lines% = nbr_lines% + 1%
            return


        REM *************************************************************~
            *            I M A G E   S T A T E M E N T S                *~
            *************************************************************

L60040: %                                               ######  PRINTED #~
        ~####### PER ##############################
L60060: %                                               +-----+----------~
        ~-+-----------+----------------------+------+
L60080: %                                               !STORE! SHIP DATE~
        ~ ! CUSTOMER# ! BILL OF LADING #     ! PAGE !
L60100: %                                               !-----!----------~
        ~-!-----------!----------------------!------!
L60120: %                                               ! ### !  ########~
        ~ ! ######### ! #################### ! #### !
L60140: %                                               !     !          ~
        ~ !           !                      !      !
L60160: %                                               +-----+----------~
        ~-+-----------+----------------------+------+
L60172: %LOAD NUMBER: #####  DROP NUMBER: ##                             ~

L60180: %P.O. S.O.                                                       ~
        ~            QUANTITY   QUANTITY LOT     PKG PACKAGE
L60200: %LINE SEQ# PART NUMBER               PART DESCRIPTION            ~
        ~     UOM   REMAINING    SHIPPED NUMBER  QTY TYPE
L60220: %---- ---- ------------------------- ----------------------------~
        ~---- ---- ---------- ---------- ------ ---- --------
L60240: %SHIP-TO: ###############################       +----------------~
        ~--+----------------------+---------+--------+
L60260: %         ###############################       ! PURCHASE ORDER ~
        ~  ! CARRIER              ! CARTONS ! WEIGHT !
L60280: %         ###############################       !----------------~
        ~--!----------------------!---------!--------!
L60300: %         ###############################       ! ###############~
        ~# ! #################### ! ####### ! ###### !
L60320: %         ###############################       +----------------~
        ~--+----------------------+---------+--------+
L60340: %         ###############################       FOB  : ##########~
        ~##########
L60360: %         CUSTOMER PHONE NO: ##############     TERMS: ##########~
        ~##########
L60380: %                                               SHIP : ##########~
        ~##################################################
L60400: % ### #### ######################### ############################~
        ~#### #### ###,###.## ########## ###### #### ########
L60420: %###### #########################################################~
        ~######################################
L60440: %                                                                ~
        ~                  #############
L60460: %               #################################################~
        ~###############################
L60480: %                                             ** End of Document ~
        ~**
L60500: %Special Text: ##################################################~
        ~##########

L60530: %          Sku No.: #########################     Text:  ########~
        ~####################################################

L60560: %          Total Number of Units  =                              ~
        ~                     ##########

L60580: % ### #### #### #################################################~
        ~#######################   #### ###,###.## ##########

L60600: %S.O.                                                            ~
        ~                                QUANTITY   QUANTITY 
L60610: %SEQ#    QTY           PART DESCRIPTION                          ~
        ~                          UOM   REMAINING   SHIPPED 
L60620: %---- --------- -------------------------------------------------~
        ~------------------------- ---- ---------- ----------
L60640: %  ##########                                                    ~
        ~                                          ##########


        build_print                      /* PRINT DESCRIPTIONS      */
                                         /* APC$(1) = 30 CHARACTERS */
                                         /* APC$(2) = 30 CHARACTERS */
            if oracle% <> 0% then return
            err% = 0%                    /* APC$(3) = 30 CHARACTERS */
            if len(part_nmbr$) < 19% then err% = 1%
            if err% = 1% then goto L60780
            init(" ") apc$(), apc_scr$, apc_prt$, apc_sze$
            read #11,key = part_nmbr$,using L60670, apc$(), eod goto L60710
L60670:       FMT POS(606), 2*CH(30), CH(20)
            apc_sze$ = apc$(3)
            goto L60760

L60710:       call "APCDESCR" (part_nmbr$,apc_scr$,apc_prt$,apc_sze$,#15,~
                                                          err% )
              apc$(1) = str(apc_prt$, 1%,30%)
              apc$(2) = str(apc_prt$,31%,30%)
                                                      /* BUILD SIZE */
L60760:       apc$(3) = "WIDTH: " & str(apc_sze$,1%,7%) & " HEIGHT: " &  ~
                                    str(apc_sze$,11%,6%)
L60780:     tot_qty = 0.0
            convert qtyship$ to tot_qty, data goto L60800
L60800:
            total_qty = round(total_qty + tot_qty, 2)

            s_23% = 0%                                /* (EWD004) Begin */
            s_23m$ = str(part_nmbr$,1%,3%)
            s_so$  = str(so$,1%,8%)                   /* Sales Order    */
            s_ln$  = so_seqnr$                        /* line Item      */
            init(" ") s_prv$                          /* s_1$ Passed In */
            prv% = 0%                                 /* Use APCPLNDT   */
            call "APCPRZSB" (prv%, s_1$, cust_code$, s_23m$, s_so$,      ~
                                   s_ln$, s_prv$, s_23$, s_23%,          ~
                                   #20, #10, #2, #5, x_er% )
            if x_er% <> 0% then return
               str(apc$(1%),1%,8%) = s_23$

        return                                        /* (EWD004) End   */

        get_oracle_print                              /*  (EWD005)      */
            oracle%, config% = 0%
            lne_tot = 0.00
            init(" ") ora_desc$

            field_num% = 5%
            gosub oracle_getfield
            ora_seqnr$ = field$

            if ora_seqnr$ <> sav_seqnr$ and sav_seqnr$ <> " "       ~
               then gosub print_extra_line 
            if ora_seqnr$ = sav_seqnr$ and sav_seqnr$ <> " "        ~
               then ora_seqnr$ = " "   ~
               else sav_seqnr$ = ora_seqnr$

            pos% = 1%
            for field_num% = 6% to 11%
                gosub oracle_getfield
                str(fields$(),pos%, field_len%) = field$
                pos% = pos% + field_len%
            next field_num%
            
            ora_desc$ = str(fields$(),32%,250%)
REM         if str(ora_desc$,34%,8%) <> "Warranty" then goto no_warr
REM            init(" ") sav_seqnr$
REM            gosub oracle_fetch
REM            goto get_oracle_print
REM     no_warr
            oracle%, tot_oracle% = 1%

            if str(fields$(),4%,1%) <> "0" then config% = 1%
               if config% = 1% then return

            if ora_seqnr$ = " " then str(fields$(),26%,3%) = " "     ~
               else str(fields$(),29%,3%) = " "
        return
 
        print_extra_line
            nbr_lines% = nbr_lines% + 1%
            print using L60620
        return

        oracle_connect
REM            user$   = "MSSQL"
REM            pass$   = "MSSQL"
            gosub get_user_pswd       /* (AWD006) */
            server$, stmt1$, stmt2$ = " "

            oci_err% = 0%
            no_fields% = 0%

            call "CONNECT" (user$, pass$, server$, oci_err%)
        return

/* (AWD006) beg */
        get_user_pswd
            call "READ100" (#24, "ORACLE PASSWORD", f1%(24%))   /* SYSFILE2 */
            if f1%(24%) <> 0% then get #24 using ORCL_PSWD, user$, pass$
ORCL_PSWD:         FMT POS(21), CH(50), POS(50)

        return

/* (AWD006) END */

        oracle_procedure
            init(" ") stmt1$, sav_seqnr$
            oracle%, tot_oracle% = 0%
            str(stmt1$,1%,24%) = "CALL MSSQL.RPT_DISPLAY('"
            str(stmt1$,25%,20%) = str(so$,1%,8%) & "', '" & ora_usr$ & "')"
            gosub oracle_exec
        return

        oracle_line
            init(" ") stmt1$
            str(stmt1$,1%,40%)   = "SELECT * FROM MSSQL.DISPLAY WHERE SALESO" 
            str(stmt1$,41%,26%)  = "RDER = '" & str(so$,1%,8%) & "' AND USER"
            str(stmt1$,67%,27%)  = "ID = '" & ora_usr$ & "' ORDER BY DISPLAY"
            str(stmt1$,94%,40%)  = "ORDER ASC, LINENUMBER ASC, CONFIG DESC, "
            str(stmt1$,134%,15%) = "UNITID ASC   "

REM            call "SHOSTAT" ("stmt1 " & str(stmt1$,1%,40%))  stop
REM            call "SHOSTAT" (" " & str(stmt1$,41%,26%))  stop
REM            call "SHOSTAT" (" " & str(stmt1$,67%,27%))  stop
REM            call "SHOSTAT" (" " & str(stmt1$,94%,37%))  stop
REM            call "SHOSTAT" (" " & str(stmt1$,131%,15%))  stop

            gosub oracle_flush
            gosub oracle_query
        return

        oracle_delete
            init(" ") stmt1$, error$
            str(stmt1$,1%,40%)  = "DELETE FROM MSSQL.DISPLAY WHERE SALESORD" 
            str(stmt1$,41%,24%) = "ER = '" & str(so$,1%,8%) & "' AND USER"
            str(stmt1$,65%,27%) = "ID = '" & ora_usr$ & "'"
            gosub oracle_flush
            gosub oracle_exec
REM            call "ERROR" (error$)
REM            call "SHOSTAT" ("ERROR ERROR RETURN VALUE --> " & error$)
REM            stop

            gosub oracle_flush
        return

        oracle_fields
            gosub oracle_fetch
            if oci_err% > 0 or oci_err% < -1 then return
            oracle% = 1%
            gosub get_oracle_print
        return



        oracle_discnnct
            call "DISCNNCT" (oci_err%)
        return

        oracle_query
            oci_err% = 0%
            call "QUERY" (stmt1$, stmt2$, oci_err%)
        return

        oracle_exec
            oci_err% = 0%
            call "EXEC" (stmt1$, stmt2$, oci_err%)
        return

        oracle_flush
            oci_err% = 0%
            call "FLUSH" (oci_err%)
        return

        oracle_fetch
            oci_err% = 0%
            no_fields% = 0%
            call "FETCH" (no_fields%, oci_err%)
        return

        oracle_getfield
            oci_err% = 0%
            field_len% = 0%
            init(" ") field$, name$
REM            call "GETFIELD" (field_num%, field$, oci_err%)
            call "FIELDINF" (field_num%, field$, name$, field_len%, oci_err%)
        return

                                                      /*  (EWD005)      */

        lookup_load
            init(" ") or_drop$, or_cust$, or_po$, or_so$, or_load$ 
            or_key1$ = all(hex(00))
            str(or_key1$,1%,8%)  = so$
          read #1,key 4% = or_key1$, using L60920, or_drop$, or_cust$,    ~
                                 or_po$, or_so$, or_load$, eod goto L60940
L60920:        FMT POS(25), CH(2), CH(9), CH(16), CH(8), POS(94), CH(5)
        return
L60940:     or_load$  = "NONE " : or_cust$ = cust_code$
            or_drop$  = "XX"    : or_po$   = "9999999999999999"
            or_so$    = so$
        return

        lookup_text                                    /* Look Up Text */
            text_desc$ = " "
            if textid$ <> " " then goto L61020
               goto L61150
L61020:     text_key$ = all(hex(00))
            str(text_key$,1%,1%) = "M"
            str(text_key$,2%,3%) = "   "
            str(text_key$,5%,4%) = textid$
            str(text_key$,9%,1%) = "1"
            sav_key1$ = text_key$
            read #17,key > text_key$, using L61100, text_key$, text$(),   ~
                                                           eod goto L61150
L61100:          FMT CH(11), POS(64), 2*CH(70)
            if str(sav_key1$,1%,9%) <> str(text_key$,1%,9%) then         ~
                                                            goto L61150
            if text$(1) <> " " then text_desc$ = str(text$(1),1%,60%)    ~
                               else text_desc$ = str(text$(2),1%,60%)
L61150: return

        lookup_skew_no
            sku_no$ = " "
            sku_key$ = all(hex(00))
            str(sku_key$,1%,3%) = sku_code$
            str(sku_key$,4%,25) = part_nmbr$
            read #16,key 1% = sku_key$, using L61240, sku_no$,            ~
                                                           eod goto L61250
L61240:        FMT XX(3), CH(25)
L61250: return

        ask_credit_hold
                comp% = 2%
                hdr$ = "****** CREDIT HOLD ******"
                msg$(1) = "Sales Order (" & so$ & ") for Customer (" &   ~
                          cust_code$ & ") on HOLD"
                msg$(3) = "Press RETURN to Print, or PF(16) to Skip"
                call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
        return

        lookup_customer
            prt_phone$ = "(XXX) XXX-XXXX"
            read #20,key = cust_code$, using L61400, cust_phone$,         ~
                                                sku_code$, eod goto L61440
L61400:        FMT POS(453), CH(10), POS(1000), CH(3)
            str(prt_phone$,2%,3%)  = str(cust_phone$,1%,3%)
            str(prt_phone$,7%,3%)  = str(cust_phone$,4%,3%)
            str(prt_phone$,11%,4%) = str(cust_phone$,7%,4%)
L61440: return
                                              /* (EWD001) - Begin       */
                                              /*   Replaced by (EWD002) */
                 
                                              /* (EWD002) - Begin       */
        delivery_sort_bol                        
            mode% = 1% : gosub open_work      /* Open a Temporary Work  */
            mode% = 3% : gosub open_work      /* file for EWDBOLS Sort  */

            plowkey$ = all(hex(00))           /* Put in Drop Seq within */
            str(plowkey$,1%,1%) = "B"         /* the Load               */
        delivery_sort_bol_nxt
            read #3,hold,key 1% > plowkey$, using L61800, plowkey$,       ~
                                          eod goto delivery_sort_bol_done
L61800:        FMT CH(39)

            if str(plowkey$,1%,1%) <> "B" then goto delivery_sort_bol_nxt
            cust_code$ = str(plowkey$,12%,9%)
            so$        = str(plowkey$,21%,16%)
            gosub lookup_load

            delete #3                 
            init(" ") wrk_key$, wrk_rec$
            if or_drop$ = "XX" then or_drop$ = "99"
            str(wrk_key$,1%,5%)  = or_load$    /* Load Number          */
            str(wrk_key$,6%,2%)  = or_drop$    /* Drop Number          */
            str(wrk_key$,8%,9%)  = or_cust$  /* Customer Code        */
            str(wrk_key$,17%,5%) = "99999"     /* P.O. Number          */
            str(wrk_key$,22%,16%)= or_po$      /* P.O. Number          */
            str(wrk_key$,38%,8%) = or_so$      /* Sales order No.      */

            str(wrk_rec$,1%,39%) = plowkey$
            put #22, using L61810, wrk_key$, wrk_rec$
L61810:       FMT CH(50), CH(60)
            write #22, eod goto L61820
            goto delivery_sort_bol_nxt
L61820:         errormsg$ = "(Error) Sorting (EWDBOL01)???"
                gosub error_prompt
                goto delivery_sort_bol_nxt
        delivery_sort_bol_done                      /* Reverse Sort (2) */
                                                    /* (EWDBOL01)       */
               init(" ") wrk_key$, wrk_rec$, sav_key2$, sav_po$
        delivery_sort_1
               read #22,key > wrk_key$, using L61810, wrk_key$, wrk_rec$,~
                                            eod goto delivery_sort_1_done
               init(" ") wrk_key1$
               if sav_key2$ = str(wrk_key$,1%,16%) then goto L61830
                  seq% = 99999%                     /* Load, Drop, Cust */
                  sav_key2$ = str(wrk_key$,1%,16%)

L61830:        if sav_po$ = str(wrk_key$,22%,16%) then goto L61840
                  sav_po$ = str(wrk_key$,22%,16%)   /* Subtract when    */ 
                  seq% = seq% - 2%                  /* P.O. Changes     */
                  convert seq% to seq$, pic(00000)

L61840:        wrk_key1$             = wrk_key$
               str(wrk_key1$,17%,5%) = seq$
                                                    /* (EWDBOL02)       */ 
               put #23, using L61810, wrk_key1$, wrk_rec$
               write #23, eod goto L61850
               goto delivery_sort_1
L61850:             errormsg$ = "Error Sorting Reverse (EWDBOL02)"
                    gosub error_prompt
                    goto delivery_sort_1
        delivery_sort_1_done
              
        return
                                                  /* (EWD002) - Begin */
        open_work
            if mode% = 1% then mode$ = "OUTPT"
            if mode% = 2% then mode$ = "INPUT"
            if mode% = 3% then mode$ = "SHARE"
            f2% = 0%
            call "WORKOPN2" (#22, mode$, 500%, f2%)
            if f2% <> 0% then goto L64800

            f2% = 0%
            call "WORKOPN2" (#23,mode$, 500%, f2%)
            if f2% <> 0% then goto L64810
        return
L64800:     errormsg$ = "Error - Cannot Open (EWDBOL01)"
            gosub error_prompt
        return

L64810:     errormsg$ = "Error - Cannot Open (EWDBOL02)"
            gosub error_prompt
        return
        delete_work
            call "FILEBGON" (#22)
            call "FILEBGON" (#23)
        return

        error_prompt
           comp% = 2%
           hdr$ = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return
                                                 /* (EWD002) - End Mods*/ 
        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************
        gosub oracle_discnnct

        exit_program
            call "SHOSTAT" ("One Moment Please")
            end
