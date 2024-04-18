        REM *************************************************************~
            *                                                           *~
            *  Current Adjustment Factor = .05 ( or 5 Percent)          *~
            *                                                           *~
            *  Program Name      - APCSALE1   (By Department)           *~
            *  Creation Date     - 04/05/93                             *~
            *  Last Modified Date- 10/31/05                             *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - This Program Creates a Daily/Weekly  *~
            *                      Sales Report. Has been modified to   *~
            *                      Calculate the the Product Unit 'Cost'*~
            *                      and 'Price' for Calculating the      *~
            *                      Gross Profit and Gross Profit Percent*~
            *                                                           *~
            *  Code Tables Used  - (MODEL    ) - Model Codes            *~
            *                      (PLAN DEPT) - Department Codes       *~
            *                      (PLAN REGN) - EWD Region Codes       *~
            *                                                           *~
            *  Subroutines Used  - (APCCST0B) Primary Sub for Calc. of  *~
            *                                 all Costing Information.  *~
            *                                                           *~
            *  Special Comments  - Line Item Unit Price (BCK_TOTAL) is  *~
            *                      the ( Net ) Product Price. All       *~
            *                      Discounts (Header) and (Line Item)   *~
            *                      have been applied.                   *~
            *                                                           *~
            *                DD% = The Subscript for Department Found.  *~
            *        DEPTS$(DD%) - Active Departments Description Texts.*~
            *   DEPTS(DD%,1%-5%) - 'DAILY' Active Department Values.    *~
            *                      1% = S.O. Quantities    (Sales Order)*~
            *                      2% = S.O. Amounts       (Sales Order)*~
            *                      3% = S.O. Total Cost    ( APCCST0B  )*~
            *                      4% = S.O. Gross Profit  (Calculated )*~
            *                      5% = S.O. Gross Profit %(Calculated))*~
            *                           Note - Same Calc. Process for   *~
            *                                  Weekly Totals.           *~
            *  DEPTS(DD%,6%-10%) - 'WEEKLY' Active Department Values.   *~
            *                      6% = S.O. Quantities                 *~
            *                      7% = S.O. Amounts                    *~
            *                      8% = S.O. Total Cost                 *~
            *                      9% = S.O. Gross Profit               *~
            *                     10% = S.O. Gross Profit %             *~
            *                                                           *~
            *             FACTOR - Set to ( 5% Percent ) to Adjust      *~
            *                      All Sales Order Dollar Totals.       *~
            *                                                           *~
            *     Transportation - No transportation chg's or fee's are *~
            *                      included in the 'Total Cost' Bucket. *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/05/93 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 01/10/95 ! Mod's for New Products (706,712,722,732) ! RHH *~
            * 05/04/95 ! Mod - Add New Table 'APCSALES' to Store  ! RHH *~
            *          !       the Bucket Location for Product.   !     *~
            * 07/08/97 ! Mods to put in sync with new planning and! RHH *~
            *          !   and align products with new dept. codes!     *~
            *          !   Also Calc the Cost, Gross Profit, and  !     *~
            *          !   Gross Profit Percent.                  !     *~
            * 07/23/97 ! Mod - Add Swithch for 'Long' or 'Short'  ! RHH *~
            *          !       version of Report With or without  !     *~
            *          !       Gross Profit Information.          !     *~
            * 11/11/97 ! Mod - Add new file for labor costing     ! RHH *~
            *          !       APCPLNDP                           !     *~
            * 03/30/98 ! Y2K modifications                        ! ERN *~
            * 04/17/02 ! Mod to change company name  (EWD001)     ! TLM *~
            * 10/31/05 ! (AWD002) CR347 Mod for Sub Part          ! CMG *~
            *02/22/2019! CR-1894  Increase size of EMP DEPT to 3  ! DES *~
            *************************************************************

        dim                                                              ~
            readkey$30, desc$30,         /* Check Sales Data           */~
            model$3, sash$1,             /* MODEL AND TSO, FGO, BSO    */~
            company$30,                  /* For Report Company Name    */~
            rpt_date$10, rpt_time$8,      /* For Report Time            */~
            date$10,                     /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            pf$(3%)79,                   /*                            */~
            pfkeys$32,                   /*                            */~
            lfac$(10%)1,                 /*                            */~
            inpmessage$79,               /*                            */~
            errormsg$79,                 /*                            */~
            userid$3                     /* Current User Id            */

        dim                              /* File = (BCKLINES)          */~
            bck_key$19,                  /* Primary Key                */~
            bck_cust$9,                  /* Customer Code for S.O.     */~
            ord_key$25,                  /* S.O. BCKMASTR Primary Key  */~
            save_order$8,                /* Save Last S.O. Processed   */~
            bck_ord$16,                  /* Sales Order                */~
            bck_seq$3,                   /* Line Item Seq              */~
            sub_part$20,                 /* Line Item Seq              */~
            bck_part$25                  /* Part Number                */

        dim beg_so$8,                    /*                            */~
            beg_so$(8%)8,                /* 1st Sales Order for Date   */~
            beg_dte$(8%)10,              /* Date Assoc with 1st Order  */~
            beg_dt1$(8%)10,              /* Date Assoc with 1st Order  */~
            beg_dte$10,                  /* Formatted Date             */~
            beg_dt1$10, date_entered$10, /* Unformatted Date           */~
            days$(8%)9,                  /* Day of the Week            */~
            day$9, count$30              /* Day of the Week            */

        dim f2%(64%),                    /* = 0 if the file is open    */~
            f1%(64%),                    /* = 1 if READ was successful */~
            fs%(64%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64%)20                 /* Text from file opening     */

        dim                              /* Costing Variables          */~
            cst(9%), cuscode$9,          /* MFG Calculated Costs       */~
            tmp$(7%,50%)25,              /* Store all Raw Mat'l Part No*/~
            tmc(7%,50%),                 /* Assoc. Cut Inches In Dec.  */~
            tmct(7%,50%),                /* Assoc. Total Cost Raw Mat'l*/~
            tmu%(7%,50%),                /* Assoc. Unit of Measure     */~
            tmd$(7%,50%)32,              /* Assoc. Raw Mat'l Desc      */~
            tmuc(7%,50%),                /* Assoc. Raw Mat'l Unit Cost */~
            tmsi(7%,50%),                /* Assoc. Scrap Inches Decimal*/~
            tmsc(7%,50%),                /* Assoc. Scrap Mat'l Cost    */~
            tmeq$(7%,50%)3,              /* Assoc. Calc Typ and Eq No. */~
            tmph$(7%,50%)5,              /* Assoc. Phantom Designature */~
            tcnt%(7%),                   /* Assoc. Count for Each Type */~
            lab(10%),                    /* Breakdown of Labor Cost    */~
            avg_pay(15%),                /* Avg Pay By Dept            */~
            uph(15%),                    /* Avg Unit Per Manhour Dept  */~
            tc(25%),                     /* Total Cost's               */~
            tt(25%),                     /* Cost Total Buckets         */~
            rm_mat(20%),                 /* Total Vinyl,Misc, Mat      */~
            rm_mats(20%),                /* Total Vinyl,Misc, Mat Scrap*/~
            apc_err%(20%),               /* Store Error Code each Modul*/~
            sale(1000%,3%),              /* STORE 'COST SALE' VALUES   */~
            sale$(1000%)2,               /* Store No Cost Flags        */~
            pc(36%),                     /* 36 PRICE SHEETS            */~
            pl_key3$15,                  /* Primary Key (APCPLNDP)     */~
            pl_dept$3, dp_key$24,        /* Store Product Department Cd*/~
            depts$(50%)30, rpt$1,        /* Active Department Buckets  */~
            depts(50%,10%)               /* Active Values              */

        dim                              /* (PAR000)                   */~
            flag$1,                      /* Calling Program Flag       */~
            pgm$1,                       /* Calling Program BCKUPDTE?? */~
            so_inv$8,                    /* Sales Order or Invoice     */~
            item_no$3,                   /* Item Number                */~
            bcksubpt_rec$256,            /* BCKSUBPT Record            */~
            flds$(35%)4,                 /* Part Number Fields         */~
            info_flds$(35%)4,            /* Additional Info Fields     */~
            field1$1,                    /* New Part Field 1 GRDTYPE   */~
            field2$1,                    /* New Part Field 2 GRDSIZE   */~
            field3$1,                    /* New Part Field 3 GRDCOLOR  */~
            field4$1,                    /* New Part Field 4 HARDWARE  */~
            field5$1,                    /* New Part Field 5 FOAM      */~
            partno1$20                   /* Subpart number             */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$41, pname$21
            apc$   = "(New)Daily/Weekly Sales Reporting Utility"
            pname$ = "APCSALE1 - Rev: R6.04"

        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! BCKLINES ! S.O. Line Item File                      *~
            * #2  ! BCKMASTR ! S.O. HEADER FILE                         *~
            * #3  ! GENCODES ! Master Table File                        *~
            * #4  ! APCPLNDP ! New Planning Master Department File      *~
            *                  C O S T I N G   F I L E S                *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #5  ! APCCUTEQ ! Saw Optimization Cross-Reference File    *~
            * #6  ! HNYMASTR ! Part Master File                         *~
            * #7  ! HNYQUAN  ! Inventory Quantities Master File         *~
            * #3  ! GENCODES ! Master Code Table File                   *~
            * #9  ! AMTBOMCD ! Master Equation File                     *~
            * #10 ! AMTBOMIF ! Master Part Validity File                *~
            * #11 ! APCEMPLY ! Employee Master File                     *~
            * #12 ! APCEQUAT ! Equation an Parts Cross Reference File   *~
            * #13 ! APCCSTHP ! Hardware and Packaging Costing Components*~
            * #14 ! APCCSTLR ! Departments Average Hourly Rates         *~
            * #15 ! CPRPRICE ! Master System Price File                 *~
            * #16 ! CUSTOMER ! Master Customer File                     *~
            * #17 ! APCPCMST ! Pricing Definition file                  *~
            * #18 ! APCSKUNO ! Home Center's Skuno File                 *~
            * #19 ! APCPCMSK ! Pricing Key Definition File              *~
            * #20 ! APCPCMSD ! Pricing Master Calc Definition File      *~
            * #21 ! APCCSTEX ! APC COSTING EXCEPTION FILE               *~
            * #22 ! APCSTOCK ! APC STOCK MASTER FILE                    *~
            * #42 ! AWDPCMST ! Pricing Definition file                  *~
            * #63 ! BCKSUBPT ! Sub Part File                 (PAR000)   *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19

            select #2,  "BCKMASTR",                                      ~
                        varc,     indexed,  recsize =  1000,             ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #3,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #4,   "APCPLNDP",                                     ~
                        varc,     indexed,  recsize = 32,                ~
                        keypos =   11, keylen =  12,                     ~
                        alt key  1, keypos =    9, keylen =  14,         ~
                            key  2, keypos =    4, keylen =  12,         ~
                            key  3, keypos =    1, keylen =  15

        REM - COSTING FILES

            select #5,  "APCCUTEQ",                                      ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    2, keylen =   7,                     ~
                        alt key  1, keypos  =     1, keylen =  8

            select #6,  "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =  1,   keylen =  25,                     ~
                        alt key  1, keypos  =   102, keylen =  9, dup,   ~
                            key  2, keypos  =    90, keylen =  4, dup,   ~
                            key  3, keypos  =    26, keylen = 32, dup

            select #7,  "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =   650,             ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44

        REM SELECT #8,  "GENCODES",                                      ~
        REM             VARC,     INDEXED,  RECSIZE = 128,               ~
        REM             KEYPOS = 1,    KEYLEN = 24

            select #9,  "AMTBOMCD",                                      ~
                        varc,     indexed,  recsize = 250,               ~
                        keypos = 1,    keylen = 42

            select #10, "AMTBOMIF",                                      ~
                        varc,     indexed,  recsize =  120,              ~
                        keypos =    1, keylen =  32

            select #11, "APCEMPLY",                                      ~
                        varc,     indexed,  recsize =  1024,             ~
                        keypos =    7, keylen =   5,                     ~
                        alt key  1, keypos =    1, keylen =  11, dup,    ~
                            key  2, keypos  =  12, keylen =  26, dup

            select #12, "APCEQUAT",                                      ~
                        varc,     indexed,  recsize =   16,              ~
                        keypos =    1, keylen =   8

            select #13, "APCCSTHP",                                      ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    1, keylen =  20

            select #14, "APCCSTLR",                                      ~
                        varc,     indexed,  recsize =  102,              ~
                        keypos =    1, keylen =  3

            select #15, "CPRPRICE"                                       ~
                        varc,     indexed,  recsize = 700,               ~
                        keypos = 1,    keylen =  47

            select #16,  "CUSTOMER",                                     ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #17, "APCPCMST"                                       ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos = 9,    keylen =  53,                     ~
                        alt key  1, keypos  =     1, keylen = 8

            select #18, "APCSKUNO"                                       ~
                        varc,     indexed,  recsize =  73,               ~
                        keypos = 1,    keylen =  28,                     ~
                        alt key  1, keypos  =    29, keylen = 28, dup

            select #19, "APCPCMSK"                                       ~
                        varc,     indexed,  recsize =  64,               ~
                        keypos = 1,    keylen =   5

            select #20, "APCPCMSD"                                       ~
/*(AWD002)*/            varc,     indexed,  recsize =  768,              ~
                        keypos = 1,    keylen =   9

            select #21,  "APCCSTEX",                                     ~
                        varc,     indexed,  recsize = 1100,              ~
                        keypos =    1, keylen =    9

            select #22, "APCSTOCK",                                      ~
                        varc,     indexed,  recsize =  70,               ~
                        keypos =    1, keylen =  32,                     ~
                        alt key  1, keypos =   8, keylen = 32

            select #42, "AWDPCMST",                                      ~
	                varc,     indexed,  recsize =   128,             ~
		        keypos =    9, keylen =  53,                     ~
		        alt key  1, keypos  =  1, keylen =  8
/*PAR000*/
            select #63, "BCKSUBPT",                                      ~
                       varc,      indexed,  recsize = 256,               ~
                       keypos  =    1, keylen =  11,                     ~ 
                       alt key   1, keypos =   12, keylen =  11, dup,    ~
                           key   2, keypos =   23, keylen =  45, dup     


            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 0%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%), 0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%), 0%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%), 0%, rslt$(4%))
        REM - COSTING FILES
            call "OPENCHCK" (#5, fs%(5%), f2%(5%), 0%, rslt$(5%))
            call "OPENCHCK" (#6, fs%(6%), f2%(6%), 0%, rslt$(6%))
            call "OPENCHCK" (#7, fs%(7%), f2%(7%), 0%, rslt$(7%))
        REM CALL "OPENCHCK" (#8 , FS%(8%), F2%(8%),0%, RSLT$(8%))
            call "OPENCHCK" (#9 , fs%(9%), f2%(9%), 0%, rslt$(9%))
            call "OPENCHCK" (#10, fs%(10%), f2%(10%), 0%, rslt$(10%))
            call "OPENCHCK" (#11, fs%(11%), f2%(11%), 50%, rslt$(11%))
            call "OPENCHCK" (#12, fs%(12%), f2%(12%), 50%, rslt$(12%))
            call "OPENCHCK" (#13, fs%(13%), f2%(13%), 50%, rslt$(13%))
            call "OPENCHCK" (#14, fs%(14%), f2%(14%), 50%, rslt$(14%))
            call "OPENCHCK" (#15, fs%(15%), f2%(15%), 50%, rslt$(15%))
            call "OPENCHCK" (#16, fs%(16%), f2%(16%),  0%, rslt$(16%))
            call "OPENCHCK" (#17, fs%(17%), f2%(17%),  0%, rslt$(17%))
            call "OPENCHCK" (#18, fs%(18%), f2%(18%),  0%, rslt$(18%))
            call "OPENCHCK" (#19, fs%(19%), f2%(19%),  0%, rslt$(19%))
            call "OPENCHCK" (#20, fs%(20%), f2%(20%),  0%, rslt$(20%))
            call "OPENCHCK" (#21, fs%(21%), f2%(21%), 50%, rslt$(21%))
            call "OPENCHCK" (#22, fs%(22%), f2%(22%), 50%, rslt$(22%))
            call "OPENCHCK" (#42, fs%(42%), f2%(42%), 50%, rslt$(42%))
/*PAR000*/  call "OPENCHCK" (#63, fs%(63%), f2%(63%),  0%, rslt$(63%))

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATFMTC" (date$)
            call "COMPNAME" (12%, company$, ret%)             /* (EWD001) */
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
            rpt$ = "0"
            gosub load_sale
            gosub load_depts

        inputmode_restart
            init(" ") beg_so$(), beg_dte$(), beg_dt1$(), days$()
            factor = .05000                   /* Sales Adjustment Set  */
            for i% = 1% to 8%                 /* to (3) Percent        */
                beg_so$(i%) = "99999999"
                beg_dte$(i%) = date$
            next i%

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub'101(1%,1%)
                  if keyhit% = 1%  then gosub startover
                  if keyhit% = 16% then goto exit_program
                  if keyhit% <> 0% then goto inputmode

        REM CHECK DATES AND CONVERT SALES ORDERS
            for i% = 1% to 7%                      /* CHECK CUT-OFF */
               if str(beg_dte$(i%),1%,3%) <> "END" and                   ~
                  str(beg_dte$(i%),1%,3%) <> "end" then goto L10180
                  beg_dte$(i%), beg_dt1$(i%) = "END"
                  goto L10240
L10180:        call "DATEOKC" (beg_dte$(i%) ,date%, errormsg$)
               if date% <> 0% then goto L10220
                  beg_dte$(i%) = "MMDDYYYY"
                  goto inputmode
L10220:        beg_dt1$(i%)= beg_dte$(i%)
               call "DATUFMTC" (beg_dt1$(i%))
L10240:        convert beg_so$(i%) to beg_so%, data goto L10290

               convert beg_so% to beg_so$(i%), pic(00000000)
            next i%
            goto L10320
L10290:        errormsg$ = "Invalid Sales Order Number."
               beg_so$(i%) = "********"
               goto inputmode
L10320: REM CHECK DATES AND SALES ORDERS
            for i% = 1% to 6%
              if beg_so$(i%) > beg_so$(i%+1%) then goto L10400
              if beg_dte$(i%) = "END" then goto L10370
              if beg_dt1$(i%) > beg_dt1$(i%+1%) then goto L10440
L10370:     next i%
            goto L10480

L10400:        errormsg$ = "Invalid Sales Order Number."
               beg_so$(i%) = "********"
               goto inputmode

L10440:        errormsg$ = "Invalid Sales Order Date."
               beg_dt1$(i%) = "********"
               goto inputmode
L10470: REM BEGIN REPORT
L10480:     gosub'101(0%,2%)
               if keyhit% = 1%  then gosub startover
               if keyhit% = 16% then goto exit_program
               if keyhit% = 14% then rpt$ = "1"
               if keyhit% = 14% then goto L10550
               if keyhit% <> 0% then goto L10470

L10550:     for i% = 1% to 7%
              call "DATE" addr("GD", beg_dt1$(i%), days$(i%), date%)
            next i%

        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

            gosub generate_report
            goto exit_program

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode_restart

        REM *************************************************************~
            *             R E P O R T   E N T R Y   S C R E E N         *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40130          /* All Fields        */
              goto L40170

L40130:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
                  lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40170:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,64), "Today:",                                     ~
               at (01,71), fac(hex(8c)), date$                  , ch(10),~
               at (02,20), fac(hex(a4)), apc$                   , ch(41),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Beginning Sales Order,Day (1):",             ~
               at (06,35), fac(lfac$(1%)), beg_so$(1%)          , ch(08),~
               at (06,50), "For Date :",                                 ~
               at (06,60), fac(lfac$(1%)), beg_dte$(1%)         , ch(10),~
                                                                         ~
               at (07,02), "Beginning Sales Order,Day (2):",             ~
               at (07,35), fac(lfac$(1%)), beg_so$(2%)          , ch(08),~
               at (07,50), "For Date :",                                 ~
               at (07,60), fac(lfac$(1%)), beg_dte$(2%)         , ch(10),~
                                                                         ~
               at (08,02), "Beginning Sales Order,Day (3):",             ~
               at (08,35), fac(lfac$(1%)), beg_so$(3%)          , ch(08),~
               at (08,50), "For Date :",                                 ~
               at (08,60), fac(lfac$(1%)), beg_dte$(3%)         , ch(10),~
                                                                         ~
               at (09,02), "Beginning Sales Order,Day (4):",             ~
               at (09,35), fac(lfac$(1%)), beg_so$(4%)          , ch(08),~
               at (09,50), "For Date :",                                 ~
               at (09,60), fac(lfac$(1%)), beg_dte$(4%)         , ch(10),~
                                                                         ~
               at (10,02), "Beginning Sales Order,Day (5):",             ~
               at (10,35), fac(lfac$(1%)), beg_so$(5%)          , ch(08),~
               at (10,50), "For Date :",                                 ~
               at (10,60), fac(lfac$(1%)), beg_dte$(5%)         , ch(10),~
                                                                         ~
               at (11,02), "Beginning Sales Order,Day (6):",             ~
               at (11,35), fac(lfac$(1%)), beg_so$(6%)          , ch(08),~
               at (11,50), "For Date :",                                 ~
               at (11,60), fac(lfac$(1%)), beg_dte$(6%)         , ch(10),~
                                                                         ~
               at (12,02), "Beginning Sales Order,Day (7):",             ~
               at (12,35), fac(lfac$(1%)), beg_so$(7%)          , ch(08),~
               at (12,50), "For Date :",                                 ~
               at (12,60), fac(lfac$(1%)), beg_dte$(7%)         , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)
               close ws
               return

        set_pf1
           inpmessage$ = "Enter Applicable Sales Order Numbers and Dates"
        if edit% = 2% then L40800     /*  Input Mode             */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                                        " &        ~
                     "                                       "
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)
            return

L40800: if fieldnr% > 0% then L40930  /*  Edit Mode - Select Fld */

           inpmessage$ = "Press <RETURN> To Create Long Report, or PF(14)~
        ~ for Short Report?"

            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (14)Short Report"
            pf$(2%)= "                                        " &        ~
                     "                                       "
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffffff0e1000)
            return
L40930:                              /*  Edit Mode - Enabled    */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                                        " &        ~
                     "                                       "
            pf$(3%)= "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

                                         /* Daily Sales report Header  */
L55050: %                            ##############################      ~
        ~      Page: ###

L55080: %########## @ ########                                           ~
        ~     (APCSALE1)

L55110: %Weekly Totals For         Tot Qty's  Tot Amt's  Tot Costs  Gross~
        ~s Prof GRP  %

L55140: %########## To ##########  ---------- ---------- ---------- -----~
        ~------ --------
L55160: %Daily Totals: ##########  Tot Qty's  Tot Amt's  Tot Costs  Gross~
        ~s Prof GRP  %

L55190: %###########               ---------- ---------- ---------- -----~
        ~------ --------
L55210: %######################### #######.## #######.## #######.## #####~
        ~##.##- ###.##-%
L55230: %                          ---------- ---------- ---------- -----~
        ~------ --------
L55250: %                          #######.## #######.## #######.## #####~
        ~##.##- ###.##-%

L55280: % Total S.O.'s Deleted  :   ######
L55290: % TotaL S.O.'s Written  :   ######
L55300: % Sales Adjustemnt Pcnt :   ###.##-%
L55310: % Last S.O. For the Day : ########

L55330: %               Sales Orders and Dates for Weekly Report

L55350: %Beg. S.O. Number, Day (1) ######## for Date ########## (#########)
L55360: %Beg. S.O. Number, Day (2) ######## for Date ########## (#########)
L55370: %Beg. S.O. Number, Day (3) ######## for Date ########## (#########)
L55380: %Beg. S.O. Number, Day (4) ######## for Date ########## (#########)
L55390: %Beg. S.O. Number, Day (5) ######## for Date ########## (#########)
L55400: %Beg. S.O. Number, Day (6) ######## for Date ########## (#########)
L55410: %Beg. S.O. Number, Day (7) ######## for Date ########## (#########)


        print_header
          init(" ") rpt_date$, rpt_time$
          rpt_date$ = date
          call "TIME" (rpt_time$)
          call "DATFMTC" (rpt_date$)
          page_no% = page_no% + 1%
          print page
          print using L55050, company$, page_no%
          print using L55080, rpt_date$, rpt_time$
          print
          print
          lcntr% = 4%
        return

        print_header_a
          gosub print_header
          print using L55330
          print
          print using L55350, beg_so$(1%), beg_dte$(1%), days$(1%)
          print
          print using L55360, beg_so$(2%), beg_dte$(2%), days$(2%)
          print
          print using L55370, beg_so$(3%), beg_dte$(3%), days$(3%)
          print
          print using L55380, beg_so$(4%), beg_dte$(4%), days$(4%)
          print
          print using L55390, beg_so$(5%), beg_dte$(5%), days$(5%)
          print
          print using L55400, beg_so$(6%), beg_dte$(6%), days$(6%)
          print
          print using L55410, beg_so$(7%), beg_dte$(7%), days$(7%)
        return

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        select_printer
            page_no% = 0% : lcntr% = 0%
REM            company$ = "Ellison Window & Door"             /* (EWD001) */
            call "SETPRNT" ("APCSLS", " ", 0%, 0%)
            select printer (134)
            gosub print_header_a
        return

        close_printer
            call "SETPRNT" ("APCSLS", " ", 0%, 1%)
        return

        generate_report
            call "SHOSTAT" ("Creating Weekly Sales Report")
            gosub select_printer
            init(" ") save_order$
            inc% = 0% : inc_max% = 7%
            count% = 0%
            count$ = "Line Items Counted [ XXXXXXX ]"

            total_order% = 0%   : total_delete% = 0%
            total_order_1% = 0% : total_delete_1% = 0%
        generate_next
            inc% = inc% + 1%
            if inc% > inc_max% then generate_done
            if beg_dt1$(inc%) = "END" then goto generate_done
            if beg_so$(inc%) = "99999999" then goto generate_done
               bck_key$ = all(hex(00))
               str(bck_key$,1%,8%) = beg_so$(inc%)
               beg_dte$ = beg_dte$(inc%)       /* Set Formatted Date   */
               beg_dt1$ = beg_dt1$(inc%)       /* Set Unformatted Date */
                                               /* Get the Day of Week  */
               call "DATE" addr("GD", beg_dt1$, day$, date%)
               beg_so$ = beg_so$(inc%)
               convert beg_so$ to check_so%, data goto L60390
L60390:                                        /* Save Integer for next*/
                                               /* S.O. to be Processed */
               total_order% = 1% : total_delete% = 0%
        generate_daily
            gosub so_line_value                /* CHECK S.O ENTRY DATE */
            if order% <> 0% then goto L60490
               goto generate_done

L60490:     count% = count% + 1%
            if mod(count%,50%) <> 0% then goto L60540
               convert count% to str(count$,22%,7%), pic(#######)
               print at(04,21);hex(84);count$;

L60540:     tot_cst = 0.0
            if rpt$ = "0" then gosub calc_cost
            gosub find_dept
            if bck_ord$ < beg_so$(inc% + 1%) then goto L60610
               gosub print_daily_totals
               goto generate_next

L60610:     pp% = pos("456" = sash$)
            if pp% <> 0%   then pl_dept$ = "100"   /* TSO, BSO, FGO    */

            if x_err%  = 2%  then pl_dept$ = "101" /* Components/Parts */
                                                   /* No Cost for Prod */
            if tot_cst = 0.0 and rpt$ = "0" then pl_dept$ = "102"
            for dd% = 1% to d_max%
                if str(depts$(dd%),1%,3%) = pl_dept$ then goto L60720
            next dd%
            dd% = d_max%                           /* Misc. Product    */
                                                   /* Misc. Product    */
L60720:     tot_cst = round(tot_cst * bck_qty, 2)  /* Daily Totals     */
            depts(dd%,1%) = round(depts(dd%,1%) + bck_qty, 2)
            depts(dd%,2%) = round(depts(dd%,2%) + bck_total, 2)
            depts(dd%,3%) = round(depts(dd%,3%) + tot_cst, 2)
                                                   /* Weekly Totals    */
            depts(dd%,6%) = round(depts(dd%,6%) + bck_qty, 2)
            depts(dd%,7%) = round(depts(dd%,7%) + bck_total, 2)
            depts(dd%,8%) = round(depts(dd%,8%) + tot_cst, 2)
            if beg_so$ = bck_ord$ then goto generate_daily
        cont_after_delete
               total_order% = total_order% + 1% /* Processed S.O.      */
               check_so% = check_so% + 1%       /* Set-Up for next S.O.*/
               convert check_so% to beg_so$, pic(00000000)
               if beg_so$ = bck_ord$ then goto generate_daily
                  total_order% = total_order% - 1%   /* Deleted S.O.   */
                  total_delete% = total_delete% + 1% /* has been Found */
                  goto cont_after_delete
         goto generate_daily

        generate_done
            if beg_dt1$(inc%) <> "END" then gosub print_daily_totals
            gosub print_weekly_totals
            gosub close_printer
        return

        print_daily_totals
           total1 = 0.0 : total2 = 0.0 : total3 = 0.0
           total4 = 0.0 : total5 = 0.0 : total6 = 0.0
           for i% = 1% to d_max%         /* Adjust the Cost by a Factor*/
                                         /* Percentage.                */
               adjust    = round(depts(i%,2%) * factor, 2)
               adj_sales = round(depts(i%,2%) - adjust, 2)

               depts(i%,4%) = round(adj_sales - depts(i%,3%), 2)
               if rpt$ = "1" then depts(i%,4%) = 0.0 /* NOT APPLICABLE */

               if adj_sales <> 0.0 then                                  ~
                  depts(i%,5%) = round(depts(i%,4%)/adj_sales, 2%)
               depts(i%,5%) = round(depts(i%,5%) * 100.0, 2%)

               total1 = round(total1 + depts(i%,1%), 2) /* Quantity    */
               total2 = round(total2 + depts(i%,2%), 2) /* Sales Price */
               total3 = round(total3 + depts(i%,3%), 2) /* Total Cost  */
           next i%
           adjust    = round(total2 * factor, 2)
           adj_sales = round(total2 - adjust, 2)
           total4    = round(adj_sales - total3, 2)
           if rpt$ = "1" then total4 = 0.0           /* NOT APPLICABLE */

           if adj_sales <> 0.0 then                                      ~
              total5 = round(total4 / adj_sales, 2)     /* Gross Prof %*/
           total5 = round(total5 * 100.0, 2)            /* G.P. Percent*/

           total6 = round(factor * 100.0, 2)     /* Convert to Percent */
           gosub print_header

           print using L55160, beg_dte$
           print using L55190, day$
           print
                                             /*Print Department Detail */
           lcntr% = lcntr% + 3%
           for i% = 1% to d_max%
               print using L55210, str(depts$(i%),5%,25%), depts(i%,1%),  ~
                               depts(i%,2%), depts(i%,3%), depts(i%,4%), ~
                               depts(i%,5%)
               lcntr% = lcntr% + 1%
               if lcntr% > 52% then gosub print_header
           next i%

           print using L55230
           print using L55250, total1, total2, total3, total4, total5
           print
           print using L55280, total_delete%
           print using L55290, total_order%
           print using L55300, total6
           print
           print using L55310, beg_so$

           total_order_1%  = total_order_1%  + total_order%
           total_delete_1% = total_delete_1% + total_delete%
           total1 = 0.0 : total2 = 0.0 : total3 = 0.0
           total4 = 0.0 : total5 = 0.0 : total6 = 0.0

           total_order% = 0% : total_delete% = 0%
           for i% = 1% to d_max%             /* Zero Only Daily Totals */
               for j% = 1% to 5%             /* for Applic. Departments*/
                 depts(i%,j%) = 0.0
               next j%
           next i%
        return

        print_weekly_totals
           total1 = 0.0 : total2 = 0.0 : total3 = 0.0
           total4 = 0.0 : total5 = 0.0 : total6 = 0.0
           for i% = 1% to d_max%         /* Adjust the Cost by a Factor*/
                                         /* Percentage.                */
               adjust = round(depts(i%,7%) * factor, 2)
               adj_sales = round(depts(i%,7%) - adjust, 2)

               depts(i%,9%) = round(adj_sales - depts(i%,8%), 2)
               if rpt$ = "1" then depts(i%,9%) = 0.0 /* NOT APPLICABLE */

               if adj_sales <> 0.0 then                                  ~
                  depts(i%,10%) = round(depts(i%,9%)/adj_sales, 2%)
               depts(i%,10%) = round(depts(i%,10%) * 100.0, 2%)

               total1 = round(total1 + depts(i%,6%), 2) /* Quantity    */
               total2 = round(total2 + depts(i%,7%), 2) /* Sales Price */
               total3 = round(total3 + depts(i%,8%), 2) /* Total Cost  */
           next i%
           adjust    = round(total2 * factor,    2)
           adj_sales = round(total2 - adjust,    2)
           total4    = round(adj_sales - total3, 2)
           if rpt$ = "1" then total4 = 0.0           /* NOT APPLICABLE */

           if adj_sales <> 0.0 then                                      ~
              total5 = round(total4 / adj_sales, 2)     /* Gross Prof %*/
           total5 = round(total5 * 100.0, 2)            /* G.P. Percent*/
           total6 = round(factor * 100.0, 2)

           gosub print_header

           print using L55110
           print using L55140, beg_dte$(1%), beg_dte$(7%)
           print
           lcntr% = lcntr% + 3%
           for i% = 1% to d_max%
               print using L55210, str(depts$(i%),5%,25%), depts(i%,6%),  ~
                               depts(i%,7%), depts(i%,8%), depts(i%,9%), ~
                               depts(i%,10%)
               lcntr% = lcntr% + 1%
               if lcntr% > 52% then gosub print_header
           next i%
           print using L55230
           print using L55250, total1, total2, total3, total4, total5
           print
           print using L55280, total_delete_1%
           print using L55290, total_order_1%
           print using L55300, total6
        return

        sales_order             /* For S.O. get the Header Discount    */
             init(" ") date_entered$
             save_order$ = str(bck_ord$,1%,8%) /*1st Line Item of Order*/
             ord_disc = 0.0
             init(" ") ord_key$
             str(ord_key$,1%,9%)  = bck_cust$
             str(ord_key$,10%,8%) = str(bck_ord$,1%,8%)
             read #2,key = ord_key$, using L62220, date_entered$,ord_disc,~
                                                  eod goto L62230
L62220:         FMT POS(830), CH(6), POS(859), PD(14,4)
L62230: return

        so_line_value           /* Output - BCK_TOTAL = Unit Net Price */
             order% = 0%        /* Note   - (All Discounts Taken )     */
             read #1,key > bck_key$, using L62300, bck_cust$, bck_ord$,   ~
                                bck_seq$, bck_part$, bck_qty, bck_price, ~
                                             bck_ln_disc, eod goto L62470
L62300:        FMT CH(09), CH(16), CH(03), XX(03), CH(25), XX(32),       ~
                              XX(04), PD(14,4), POS(165), 2*PD(14,4)
             str(bck_key$,1%,16%) = bck_ord$        /* Set Primary Key */
             str(bck_key$,17%,3%) = bck_seq$
                                  /* For 1st Line Item of the S.O. get */
                                  /* the S.O. (ORD_DISC) for Calc Net  */
             if save_order$ <> str(bck_ord$,1%,8%) then gosub sales_order
             bck_total = round( bck_price * bck_qty, 2)
             discamt = round(bck_total * bck_ln_disc * .01, 2)
             bck_total = round(bck_total - discamt, 2) /*Line Item Disc*/

             discamt = round(bck_total * ord_disc * .01, 2)
             bck_total = round(bck_total - discamt, 2) /* S.O. Discount*/

             model$ = str(bck_part$,1%,3%)
             sash$  = str(bck_part$,11%,1%)       /* TS0, FGO, BSO     */
          order% = 1%
L62470: return                                    /* Prod% = 22, Part  */

        find_dept
            init(" ") pl_key3$, pl_dept$
            str(pl_key3$,1%,3%) = model$
        find_dept_nxt
            read #4,key 3% > pl_key3$, using L62550, pl_key3$,            ~
                                                    eod goto L62610
L62550:        FMT CH(15)
            if model$ <> str(pl_key3$,1%,3%) then goto L62610
               pl_dept$ = str(pl_key3$,11%,3%)
               gosub check_supp
               if supp% = 1% then goto find_dept_nxt
        return
L62610:     pl_dept$ = "103"                            /* Misc. Parts */
        return

        check_supp
            supp% = 0%
            init(" ") readkey$
            str(readkey$,1%,9%)  = "PLAN SUPP"
            str(readkey$,10%,3%) = pl_dept$
            read #3,key = readkey$, eod goto L62710
            supp% = 1%
L62710: return

        calc_cost

/*PAR000*/ init(" ") so_inv$, item_no$, bcksubpt_rec$, field1$, field2$, ~
                     field3$, field4$, field5$
/*PAR000*/ so_inv$  = bck_ord$
/*PAR000*/ item_no$ = bck_seq$
/*PAR000*/ gosub lookup_sub_part
/*PAR000*/ partno1$ = str(bcksubpt_rec$,48%,20%)

           calc% = 0%
           mat lab  = zer     : mat tc   = zer
           mat apc_err% = zer : mat pc = zer
           width = 0
           convert str(bck_part$,13%,4%) to width, data goto L62790
L62790:                                        /* The value of KK% is  */
                                               /* for the SALE() and   */
           kk% = 1%                            /* the SALE$() Buckets  */
           convert str(bck_part$,1%,3%) to kk%, data goto L62830
L62830:
           if bck_qty <> 0 then goto L62880     /* Check Quantity Ship  */
              mat cst  = zer                   /* No Product Shipped   */
              tot_cst = 0.0                    /* No Cost for Product  */
        return
L62880:    p_err% = 0%                         /* Calc Product Cost    */
           x_err% = 0%
           if len(bck_part$) < 19 then x_err% = 2%  /* Product is Part */
           if width = 0 then x_err% = 2%                     /* Part   */
           if x_err% <> 0% then goto L63530
                                         /* When set means that 'Cost' */
                                         /* Cannot be Calculated, only */
                                         /* Price can be Calculated    */
                                         /* CALC% = 8%, Price Only     */
           if str(sale$(kk%),2%,1%) = "*" then calc% = 8% /* For Wind's*/
           call "APCCST0B" ( calc%,      /* Calculation Method         */~
                             bck_part$,  /* MFG Part Number            */~
			     sub_part$,         ~
                             0.0,        /* Cost Adjustment Dollars    */~
                             tmp$(),     /* Raw Mat'l Part Numbers     */~
                             tmc(),      /* Raw Mat'l Cut Inches in Dec*/~
                             tmct(),     /* Raw Mat'l Costs            */~
                             tmu%(),     /* Raw Mat'l Calc Unit of Meas*/~
                             tmd$(),     /* Raw Mat'l Descriptions     */~
                             tmuc(),     /* Raw Mat'l Unit Cost        */~
                             tmsi(),     /* Raw Mat'l Scrap Inches Dec */~
                             tmsc(),     /* Raw Mat'l Scrap Cost       */~
                             tmeq$(),    /* Calc Type and Equation No. */~
                             tmph$(),    /* Phantom Number             */~
                             tcnt%(),    /* Raw Mat'l Type Counts      */~
                             "A",        /* Labor Type (A) or (S)tand  */~
                             lab(),      /* Labor Costs (1 thru 10)    */~
                             avg_pay(),  /* Avg Hourly Pay by Dept     */~
                             uph(),      /* Avg Units Per Manhour Dept */~
                             tc(),       /* Material Costs (1 thru 25) */~
                             tt(),       /* Total Cost Buckets         */~
                             rm_mat(),   /* Material Costs (1 thru 10) */~
                             rm_mats(),  /* Mat'l Scrap Costs(1 thru 9)*/~
                             cuscode$,   /* Customer Code for Pricing  */~
                             pc(),       /* 35 Prices                  */~
                             price,      /* Calc. Price for Customer   */~
                             #5,         /*   (APCCUTEQ)               */~
                             #6,         /*   (HNYMASTR)               */~
                             #7,         /*   (HNYQUAN )               */~
                             #3,         /*   (GENCDSIN)               */~
                             #9,         /*   (AMTBOMCD)               */~
                             #10,        /*   (AMTBOMIF)               */~
                             #12,        /*   (APCEQUAT)               */~
                             #13,        /*   (APCCSTHP)               */~
                             #14,        /*   (APCCSTLR)               */~
                             #15,        /*   (CPRPRICE)               */~
                             #16,        /*   (CUSTOMER)               */~
                             #17,        /*   (APCPCMST)               */~
                             #42,        /*   (AWDPCMST)               */~
                             #18,        /*   (APCSKUNO)               */~
                             #19,        /*   (APCPCMSK)               */~
                             #20,        /*   (APCPCMSD)               */~
                             #21,        /*   (APCCSTEX)               */~
                             #22,        /*   (APCSTOCK)               */~
                             #4,         /*   (APCPLNDP)               */~
                             apc_err%()) /* 0% = Ok, Non Zero Error    */
            price = 0.0
            if str(sale$(kk%),2%,1%) <> "*" then goto L63460
               x_err% = 3%               /* No Cost Window             */
               p_err% = 0%
               goto L63530
L63460:     for i% = 1% to 20%
              if apc_err%(i%) = 0% then goto L63510
                 p_err% = i%
                 x_err% = 1%
                 cst_err% = cst_err% + 1%
L63510:     next i%

L63530:   gosub store_cost                    /* Gather All MFG Costs  */
          if tot_cst = 0.0 then gosub update_error_log

        return

        update_error_log

        return

        store_cost
           mat cst = zer                     /* Total Vinyl and Misc.  */
           tot_mat = 0.0 : tot_cst = 0.0     /* (Mat'l + Scrap) Cost   */
           tot_mat = round(tc(3%) + tc(6%) + tc(9%) + tc(12%) + tc(15%) +~
                           tc(18%) + tc(21%) + tc(19%) - tc(20%), 4%)
                                             /* Include W/F Amount     */
                                             /* Include Freight        */
                                             /* Deduct Vinyl Discount  */
           if x_err% > 1% then gosub compute_cost
                                     /* Use Cost from Costing Errors   */
                                     /* LAB(8%) and LAB(9%) are Zero   */
                                     /* Since Parts are not Calculated.*/
           tot_cst = round( tot_mat + lab(8%) + lab(9%), 4)
        return

        compute_cost
           tot_mat = 0.0
           mat lab = zer : mat tc = zer
           unit_price = 0.0
           if bck_qty <> 0 then unit_price = round(bck_total/bck_qty, 2)

           if x_err% <> 2% then goto L63900
                                                  /* Product is a Part */
              if unit_price = 0 then tot_mat = sale(kk%,3%)              ~
                                else tot_mat = unit_price * sale(kk%,2%)
           if tot_mat = 0 then goto L63900
        return
                                                  /* No Cost Window    */
L63900:    tot_mat = (pc(1%) * .50) * sale(kk%,1%)/* Calc based on the */
                                                  /* Catalog Price     */
           if tot_mat = 0 then tot_mat = unit_price * sale(kk%,1%)
           if tot_mat = 0 then tot_mat = sale(kk%,3%)
        return

        load_sale
           call "SHOSTAT" ("Loading Costing Tables")
           mat sale = zer
           init(" ") readkey$, sale$()
           str(readkey$,1%,9%)   = "COST SALE"
        load_sale_nxt
           read #3,key > readkey$, using L64040, readkey$, desc$,         ~
                                                eod goto load_sale_done
L64040:       FMT CH(24), CH(30)
           if str(readkey$,1%,9%) <> "COST SALE" then goto load_sale_done
              kk% = 1%
              convert str(readkey$,10%,3%) to kk%, data goto L64080
L64080:
              convert str(desc$,1%,8%)  to sale(kk%,1%), data goto L64100
L64100:
              convert str(desc$,11%,8%) to sale(kk%,2%), data goto L64120
L64120:
              convert str(desc$,22%,8%) to sale(kk%,3%), data goto L64140
L64140:
                                                  /*Set Product Percent*/
              sale(kk%,1%) = round(sale(kk%,1%) / 100.0, 2)
                                                  /*Set Part Percent   */
              sale(kk%,2%) = round(sale(kk%,2%) / 100.0, 2)
                                                  /*KK%,3% = Part $$   */
              sale(kk%,3%) = round(sale(kk%,3%), 2)
              str(sale$(kk%),1%,1%) = "*"         /*Cost of Sale Exists*/
              goto load_sale_nxt
        load_sale_done
           init(" ") readkey$
           str(readkey$,1%,9%) = "COST NONE"
        load_nocost_nxt
           read #3,key > readkey$, using L64040, readkey$, desc$,         ~
                                                eod goto load_nocost_done
           if str(readkey$,1%,9%) <> "COST NONE" then                    ~
                                                  goto load_nocost_done
              kk% = 0%
              convert str(readkey$,10%,3%) to kk%, data goto L64330
L64330:
              if kk% = 0% then goto load_nocost_nxt
              str(sale$(kk%),2%,1%) = "*" /* Flag, which means that    */
              goto load_nocost_nxt        /* Product or Part cannot be */
        load_nocost_done                  /* costed.                   */
        return

        load_depts
            init(" ") depts$(), dp_key$, desc$
            mat depts = zer
            d_max% = 0%
            str(dp_key$,1%,9%) = "PLAN DEPT"
        load_depts_nxt
            read #3,key > dp_key$, using L64040, dp_key$, desc$,          ~
                                                 eod goto load_depts_done
            if str(dp_key$,1%,9%) <> "PLAN DEPT" then                    ~
                                                  goto load_depts_done
               pl_dept$ = str(dp_key$,10%,3%)
               gosub check_supp
               if supp% = 1% then goto load_depts_nxt
                  dd% = 0%
                  convert pl_dept$ to dd%, data goto L64550
L64550:
                  if dd% > 94% then goto load_depts_done
                     d_max% = d_max% + 1%
                     str(depts$(d_max%),1%,4%)  = pl_dept$ & " "
                     str(depts$(d_max%),5%,25%) = str(desc$,1%,25%)
                     goto load_depts_nxt
        load_depts_done
            d_max% = d_max% + 1%
            depts$(d_max%) = "100 " & "Vinyl TSO,BSO,FGO Sash's "
            d_max% = d_max% + 1%
            depts$(d_max%) = "101 " & "Component/Misc. Parts    "
            d_max% = d_max% + 1%
            depts$(d_max%) = "102 " & "No Cost Product          "
            d_max% = d_max% + 1%
            depts$(d_max%) = "103 " & "Errors - Miscellaneous   "
        return


        lookup_sub_part                              /* (PAR000) - BEG */
            init(" ") flag$, pgm$, bcksubpt_rec$, flds$(), info_flds$()
            flag$ = "0"                  /* Sales Order Info         */
            pgm$  = "1" 


            convert so_inv$ to so_inv%, data goto convert_alpha

            convert so_inv% to so_inv$, pic(00000000)
         
            goto order_converted

convert_alpha:          
            convert str(so_inv$,2%,7%) to so_inv%, data goto sub_part1
sub_part1:
            convert so_inv% to str(so_inv$,2%,7%), pic(0000000)


order_converted:
            convert item_no$ to item_no%, data goto sub_part2 
sub_part2:

            convert item_no% to item_no$, pic(###)                                            

        call "AWDBKSUB"   (flag$,        /* Flag 0=SalesOrder 1=Invoice*/~
                          pgm$,          /* Calling Program 0=BCKUPDTE */~
                                         /* 1=Any Other 2=Delete       */~
                                         /* 3=Invoice                  */~
                          so_inv$,       /* SO or Invoice Num to lookup*/~
                          item_no$,      /* Item Number                */~
                          bcksubpt_rec$, /* Record If BCKUPDTE then    */~
                                         /* pass in else pass out      */~
                          flds$(),       /* Part Number Fields         */~
                          info_flds$(),  /* Information Fields         */~
                          #63,           /* BCKSUBPT File              */~
                          suberr1%)      /* Error Code                 */


            if suberr1% = 0% then return



            str(bcksubpt_rec$,48%,20%) = "00000                    "
            errormsg$ = "AWDBKSUB ERROR = "&so_inv$ & " Line= " & item_no$

            suberr1% = 0%

        return


        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*

        exit_program

        end
