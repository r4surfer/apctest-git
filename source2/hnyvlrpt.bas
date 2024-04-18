        REM *************************************************************~
            *                                                           *~
            *  H   H  N   N  Y   Y V   V   L      RRRR   PPPP   TTTTT   *~
            *  H   H  NN  N   Y Y  V   V   L      R   R  P    P   T     *~
            *  HHHHH  N N N    Y   V   V   L      RRRR   PPPP     T     *~
            *  H   H  N  NN    Y    V  V   L      R   R  P        T     *~
            *  H   H  N   N    Y     V     LLLLL  R   R  P        T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYVLRPT - PRINTS INVENTORY VALUATION REPORT, SORTED BY   *~
            *            PART NUMBER FOR A PARTICULAR INVENTORY TYPE.   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+-----------------WHAT---------------------+-WHO-*~
            * 11/22/85 ! ORIGINAL.                                ! SGA *~
            * 08/29/86 ! PRR# 4404 - FIXED TO INCLUDE LAST STORE/ ! LKM *~
            *          ! ACCOUNT IN TOTALING                            *~
            * 02/04/87 ! Changed HNYQUAN Format                   ! KAB *~
            * 05/13/87 ! Std Cost Changes (HNYMASTR/QUAN files).  ! ERN *~
            * 07/15/87 ! Brought subroutines CALLs up to date, etc! JIM *~
            * 05/25/88 ! Now totals last store printed            ! HES *~
            * 06/09/89 ! Corrected page break logic               ! MJB *~
            * 08/16/90 ! - Added Part Category Selection & Sort   ! MJB *~
            *          ! - Reformatted Screen to standard         !     *~
            *          ! - Removed 1 work file and changed format !     *~
            *          !    of remaining work file.               !     *~
            *          ! - Modified to perform extract only once  !     *~
            *          !    and generate reports based on alt key !     *~
            *          !    access for multiple runs.             !     *~
            *          ! - Added edit mode                        !     *~
            *          ! - Added Page 0 for Print Selections      !     *~
            *          ! - Removed little boxes                   !     *~
            * 04/18/91 ! (PRR 11803, 11864) Corrected Breakpoint  ! RJB *~
            *          !      logic in PRINT REPORT SECTION to    !     *~
            *          !      correct sub-totaling problems. Also !     *~
            *          !      added call to "ALLFREE" per the new !     *~
            *          !      standards.                          !     *~
            *          ! (PRR 11780) Changed display verbage from !     *~
            *          !      G/L to Inventory Asset Account      !     *~
            *          !(PRR 11900, 11903) Corrected by 11803 and !     *~
            *          !     11864 added call to "SETPRNT".       !     *~
            *          !QC-FIXES Removed 'ALLFREE' unneccessay    !     *~
            * 03/27/92 ! PRR 12194.  Fixed printed Cost Method &  ! JDH *~
            *          !   added Store to report.                 !     *~
            * 04/10/92 ! PRR 12376.  Added calls to GLFMT.        ! JDH *~
            *          ! PRR 12379. Fix Hdr & Total Descr on break!     *~
            * 01/12/93 ! Page 0 Facs fix                          ! RJH *~
            * 03/16/93 ! PRR 12696 Add user option to print in BG.! JIM *~
            * 03/16/93 ! DIM'd LOs, HIs, etc. Added EOR w/ Time.  ! JIM *~
            * 03/16/93 ! Column headers removed from Page zero.   ! JIM *~
            * 03/16/93 ! PRR 12696 Add 'S'ummary print option.    ! JIM *~
            * 03/16/93 ! Standardized report headers a little &   ! JIM *~
            *          !   added real, actual RPTIDs. (SES, too). ! JIM *~
            * 04/07/93 ! Added Core Value Coding.  Track Core     ! JBK *~
            *          !   values by account. Special subtotals   !     *~
            *          !   and totals for Core Value.             !     *~
            * 04/20/93 ! QC Rework- extended TESTRNGEs.           ! JIM *~
            * 12/29/93 ! PRR 13081. Corrected low range testing.  ! JDH *~
            * 02/24/94 ! Last minute move of core testing.        ! JDH *~
            *************************************************************

        dim                                                              ~
            account$12,                  /* INVENTORY ASSET ACCOUNT    */~
            accountdesc$30,              /* ACCOUNT DESCRIPTION        */~
            bkground$1,                  /* Print in Background? Y/N   */~
            blankline$79,                /* USED IN SCREEN FORMAT      */~
            columnttl$51,                /* Column titles line         */~
            catcode$4,                   /* Part Category Code         */~
            catdescr$30,                 /* Category Code Description  */~
            company$60,                  /* Company or Division Name   */~
            core_inv_flag$1,             /* Core Value Inv Trans Flag  */~
            core_fg_acct$12,             /* Core Finished Goods Acct   */~
            corepart$25,                 /* Core Part Number           */~
            corestccost$10,              /* Core Cost for Print        */~
            core_sum_qty$10,             /* Core Quantiy for Print     */~
            costmthd$12,                 /* Literal Cost Method        */~
            cursor%(2),                  /* Cursor location for edit   */~
            corevalue$10,                /* Core Value for Part        */~
            coreacctdescr$30,            /* G/L Descr for Core FG Acct */~
            coreaccttotal$12,            /* Total of Core FG Accts     */~
            coresubvalue$13,             /* Total of Core FG Accts     */~
            core_sum_title$76,           /* Title for Core Summaries   */~
            date$8,                      /* MM/DD/YY FORMATED DATE     */~
            descr$32,                    /* DESCRIPTION                */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* ERROR MESSAGE FOR INPUT    */~
            fmcatcode$4,                 /* Starting Category Code     */~
            fmpart$25,                   /* Starting Part Number       */~
            fmstore$3,                   /* Starting Store Number      */~
            hnycstcd$1,                  /* INVENTORY COST METHOD      */~
            index%(1),                   /* Search Index               */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lastpart$25, lastpartdescr$32, /* Last Part Number Printed */~
            last_account_printed$12,     /* LAST G/L ACCOUNT PRINTED   */~
            last_acctdescr$30,           /* LAST G/L ACCOUNT PRINTED   */~
            line2$79,                    /* Screen Line #2             */~
            sub_core_acct$(100)9,        /* Array for Core Value Acct  */~
            sub_core_acct(100),          /* Array for Core Value       */~
            tot_core_acct$(200)9,        /* Array for Core Value Acct  */~
            tot_core_acct(100),          /* Array for Core Value       */~
            tocatcode$4,                 /* Ending Category Code       */~
            topart$25,                   /* Ending Part Number         */~
            last_ctgy_printed$25,        /* Last Category Printed      */~
            last_ctgydescr$30,           /* Last Category Printed      */~
            message$78,                  /* INFORMATIONAL MESSAGE      */~
            tostore$3,                   /* Ending Store Number        */~
            last_store_printed$3,        /* LAST STORE PRINTED         */~
            last_storedescr$30,          /* LAST STORE PRINTED         */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            lot$6,                       /* LOT NUMBER                 */~
            newreadkey$43,               /* READ KEY FOR WORK FILE     */~
            partnr$25,                   /* THIS PART NUMBER           */~
            partsprinted$12,             /* Edited # of parts printed  */~
            plowkey$99,                  /* Plow/Read Key              */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pgm$8,                       /* Current File (ProgName)    */~
            prqty$10,                    /* Quantity for print         */~
            pravgcost$10,                /* Avg Cost for print         */~
            prtotcost$10,                /* Tot Cost for print         */~
            qtylit$(3)21,                /* Quantity type literal      */~
            s_or_d$1,                    /* Summary or Detail report?  */~
            sortflag$1,                  /* ORDER TO SORT REPORT       */~
            storedesc$30,                /* STORE DESCRIPTION          */~
            storenr$3,                   /* STORE NUMBER               */~
            subheader$60,                /* Guess                      */~
            sum_qty$10,                  /* Print area                 */~
            sum_cost$10,                 /* Print area                 */~
            qty(5),                      /* QUANTITIES FROM DISK.      */~
            qtytype$1,                   /* TYPE OF INVENTORY QUANTITY */~
            time$08,                     /* TIME IN HOUR AND MINUTES   */~
            tt$1,                        /* Task Type- Fg or Bg        */~
            username$24, userid$3        /* USER NAME & ID             */

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20,                 /* RETURN CODE FROM "FILEOPEN"*/~
            axd$(64)4                    /* AXD POINTER FROM "FILEOPEN"*/

        dim lopart$25, hipart$25, lostore$3, histore$3, locatcode$4,     ~
            hicatcode$4, bgkey$20, prompt$(7)28, rptid$6

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.  THEY ARE AN INTRINSIC PART OF THE   */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !          D E S C R I P T I O N           *~
            *-----+----------+------------------------------------------*~
            * # 2 ! HNYMASTR ! INVENTORY MASTER FILE                    *~
            * # 3 ! HNYQUAN  ! INVENTORY STORE QUANTITY DETAIL FILE     *~
            * # 4 ! STORNAME ! STORE NAMES AND ADDRESS                  *~
            * # 5 ! CATEGORY ! Part Category File                       *~
            * # 6 ! WORK2    ! WORK FILE                                *~
            * # 7 ! GLMAIN   ! GENERAL LEDGER MASTER FILE               *~
            * #08 ! SYSFILE2 ! System Info (ranges for background)      *~
            * #50 ! DUMMY    ! Dummy for CORCSTSB                       *~
            *************************************************************

            select  #2, "HNYMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 900,                                  ~
                         keypos = 1, keylen = 25,                        ~
                         alternate key 1, keypos = 102, keylen = 9, dup, ~
                                   key 2, keypos = 90,  keylen = 4, dup

            select #3,  "HNYQUAN",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 650,                                   ~
                        keypos = 17, keylen = 44,                        ~
                        alternate key 1, keypos =  1, keylen = 44

            select  #4, "STORNAME",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 300,                                  ~
                         keypos = 1, keylen = 3

            select # 5, "CATEGORY",                                      ~
                        varc, indexed,                                   ~
                        recsize = 200,                                   ~
                        keypos  =   1, keylen = 4

            select #6, "WORK6",          /* WORK FILE */                 ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 272,                                  ~
                         keypos = 1, keylen = 4,                         ~
                         alt key 1, keypos = 39, keylen = 34, dup,       ~
                             key 2, keypos =  5, keylen = 34, dup,       ~
                             key 3, keypos = 73, keylen = 29, dup

            select #7, "GLMAIN",         /* GENERAL LEDGER MASTER FILE */~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 300,                                  ~
                         keypos = 1, keylen = 9

            select #08,  "SYSFILE2",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 500,                                  ~
                         keypos = 1, keylen = 20

            select #50, "DUMMY", varc, indexed,                          ~
                                 recsize = 5, keypos = 1, keylen = 1

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENFILE" (#2, "SHARE", f2%(2), rslt$(2), axd$(2))
            call "OPENFILE" (#3, "SHARE", f2%(3), rslt$(3), axd$(3))
            call "OPENFILE" (#4, "SHARE", f2%(4), rslt$(4), axd$(4))
            call "OPENFILE" (#5, "SHARE", f2%(5), rslt$(5), axd$(5))
            call "OPENFILE" (#7, "SHARE", f2%(7), rslt$(7), axd$(7))
            call "OPENFILE" (#08, "SHARE", f2%(8%), rslt$(8%), axd$(8%))
            if f2%(2) > 0 then L65000

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * SETS DATES FOR SCREEN DISPLAY.                            *~
            *************************************************************

            call "EXTRACT" addr ("NA", username$, "ID", userid$, "TT",   ~
                tt$, "CF", pgm$)
            bgkey$ = "z" & pgm$ & "." & userid$
            date$ = date : call "DATEFMT" (date$)
            time$ = " "  : call "TIME" (time$)
            call "COMPNAME" (12%, company$, ret%)
            wseq% = 0  :  ret% = 0%
            partsprinted%, pagenumber% = 0
            pageline% = 99%
            recnbr% = val(str(rslt$(3),17,4),4)

            qtylit$(1) = "Quantity On Hand"
            qtylit$(2) = "Quantity Back Ordered"
            qtylit$(3) = "Quantity On Order"
            str(columnttl$, 1) = "Beginning Code"
            str(columnttl$,27) = "Ending Code"
            prompt$(1%) = "Part Number"
            prompt$(2%) = "Store Number"
            prompt$(3%) = "Part Category"
            prompt$(4%) = "Quantity Type"
            prompt$(5%) = "Report Sort Order"
            prompt$(6%) = "Summary or Detail? (S/D)"
            prompt$(7%) = "Print in Background? (Y/N)"

*        See if Core is on
            plowkey$ = "SWITCHS.COR"
            call "READ100" (#8, plowkey$, core_on%)
                if core_on% <> 1% then L09440
            get #8 using L09410, core_inv_flag$
L09410:         FMT POS(134), CH(1)
            if core_inv_flag$ <> "Y" then core_on% = 0%

L09440
*        Test Background printing
            if tt$ = "B" then goto print_in_background
            call "DELETE" (#08, bgkey$, 20%)  /* SYSFILE2 FG Housekeep */

            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
            str(line2$,62%) = str(pgm$) & ": " & str(cms2v$,,8%)

        REM *************************************************************~
            *         I N P U T   S E L E C T I O N S                   *~
            *-----------------------------------------------------------*~
            * Input Selection & Sort Criteria.                          *~
            *************************************************************

        inputmode1
            gosub initialize_variables

            for fieldnr% = 1% to 7%
L10100:         gosub'051(fieldnr%)
                    if enabled% = 0 then L10240
L10120:         gosub'101(fieldnr%, 1%)
                    if keyhit%  =  1% then gosub startover
                    if keyhit% <>  4% then L10200
L10150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10120
                         if fieldnr% = 1% then L10100
                         goto L10150
L10200:             if keyhit% = 16% and fieldnr% = 1% then exit_program
                    if keyhit% <>  0 then L10120
                gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                    if errormsg$ <> " " then L10120
L10240:     next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles EDIT MODE for range selection screen.             *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then goto test_background
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 6%
            if fieldnr% < 1% or fieldnr% >  7% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
            goto L11120

        REM *************************************************************~
            *            E X T R A C T   S E C T I O N                  *~
            *-----------------------------------------------------------*~
            * Extract data for report based on input criteria.          *~
            *************************************************************

        test_background     /* Find partition user wants to print from */
            if bkground$ <> "Y" then goto extract_data     /* Wants FG */
                call "READ101" (#08, bgkey$, f1%(8%))      /* SYSFILE2 */
*        User wants print in BG. Pass params to 'ME' in BG via SYSFILE2.
                put #08, using L15130, bgkey$, fmpart$, topart$, lopart$, ~
                     hipart$, fmstore$, tostore$, lostore$, histore$,    ~
                     fmcatcode$, tocatcode$, locatcode$, hicatcode$,     ~
                     qtytype$, sortflag$, s_or_d$, bkground$, " ", " "
L15130:              FMT CH(20), 4*CH(25), 4*CH(3), 4*CH(4), 4*CH(1),    ~
                          CH(255), CH(93)
                if f1%(8%) = 0% then write #08 else rewrite #08
                call "TASKUP" ("ME", 0%)     /* Spawn BG HNYVLRPT task */
                goto exit_program

        print_in_background   /* We're in BG. Get params from SYSFILE2 */
            call "READ101" (#08, bgkey$, f1%(8%))          /* SYSFILE2 */
            if f1%(8%) <> 0% then goto L15270
                message$ = "rptReport " & pgm$ & " in Background: Cancell~
        ~ed."                                     /* Ouch. Really ouch! */
                call "MESSAGE" addr ("XM", str(userid$) & hex(20),       ~
                     message$, 78%, 0%)
                goto exit_program
L15270:     get #08, using L15130, bgkey$, fmpart$, topart$, lopart$,     ~
                hipart$, fmstore$, tostore$, lostore$, histore$,         ~
                fmcatcode$, tocatcode$, locatcode$, hicatcode$,          ~
                qtytype$, sortflag$, s_or_d$, bkground$/* From FG task */
            delete #08                 /* Bye-bye, SYSFILE2 parameters */

        extract_data
            if wseq% > 0% then print_report
            call "WORKOPEN" (#6, "OUTPUT", recnbr%, f2%(6))
            call "SHOSTAT" ("Extracting Records...")

        readloop
            call "READNEXT" (#3, f1%(3))
                if f1%(3) = 0 then print_report
            get #3, using L15430, partnr$, storenr$, lot$, qty(),         ~
                                 cost, account$, hnycstcd$
L15430:         FMT POS(17), CH(25), CH(3), CH(6), POS(69), 5*PD(14,4),  ~
                             POS(117), PD(14,4), POS(259), CH(9),        ~
                             POS(403), CH(1)
            call "DESCRIBE" (#2, partnr$, descr$, 0%, f1%(2))
            if f1%(2) <> 0% then L15510
                descr$ = "Part Code Not on File"
                catcode$ = hex(00)
                goto L15530
L15510:     get #2 using L15520, catcode$
L15520:         FMT POS(90), CH(4)
L15530:     call "DESCRIBE" (#4, storenr$, storedesc$, 0%, f1%(4))
            if storedesc$ = " " then storedesc$ = "Store Code not on file"
            call "DESCRIBE" (#7, account$, accountdesc$, 0%, f1%(7))
            if accountdesc$ = " " then                                   ~
                                  accountdesc$ = "Asset Acct not on file"
            call "DESCRIBE" (#5, catcode$, catdescr$, 0%, f1%(5))
            if catdescr$ = " "                                           ~
                then catdescr$ = "Category Code not on file"
            wseq% = wseq% + 1%
            write #6 using L16080, wseq%, account$, partnr$, storenr$,    ~
                     partnr$, lot$, catcode$, partnr$, descr$,           ~
                     storedesc$, accountdesc$, catdescr$, qty(),         ~
                     cost, hnycstcd$
            goto readloop

        REM *************************************************************~
            *                  PRINT REPORT SECTION                     *~
            *                                                           *~
            *************************************************************

        print_report
            if s_or_d$ = "D"                                             ~
                then call "SHOSTAT" ("Printing Inventory Valuation Detail~
        ~ report.")                                                       ~
                else call "SHOSTAT" ("Printing Inventory Valuation Summar~
        ~y report.")
            close #6
            call "WORKOPN2" (#6, "INPUT", 1000%, f2%(6))
            select printer(134)
            if s_or_d$ = "D" then rptid$ = "HNY055" else rptid$ = "HNY056"
            call "SETPRNT" (rptid$, " ", 0%, 0%)
            if s_or_d$ = "D"                                             ~
                then subheader$ = "INVENTORY VALUATION DETAIL REPORT"    ~
                else subheader$ = "INVENTORY VALUATION SUMMARY REPORT"
            call "FMTTITLE" (subheader$, " ", 2%)         /* Center it */
            partsprinted%, pagenumber%, gpartsprinted%, newpart% =0%
            sum_cost, sum_qty, total_cat_cost, grand_total = 0
            storenr$, account$, lastpart$, catcode$ = " "
            init (hex(00)) newreadkey$
            convert sortflag$ to sort%
            convert qtytype$  to qtyp%
            okprint% = 0%

*       ** Reads begin here
            call "REDALT4" (#6, newreadkey$, sort%, f1%(6%))
            goto L15930
        read_loop1
            call "READNEXT" (#6, f1%(6%))
L15930:        if f1%(6%) = 0 then wrap_up
            get #6 using L16080, wseq%, account$, partnr$, storenr$,      ~
                   partnr$, lot$, catcode$, partnr$, descr$,             ~
                   storedesc$, accountdesc$, catdescr$, qty(),           ~
                   cost, hnycstcd$
            call "GLFMT" (account$)
            if partnr$ <= lopart$ or partnr$ > hipart$ then read_loop1
            if catcode$ <= locatcode$ or                                 ~
                          catcode$ > hicatcode$ then read_loop1
            if storenr$ <= lostore$ or storenr$ > histore$ then read_loop1
            if qty(qtyp%) = 0 then read_loop1
            okprint% = okprint% + 1%
            if okprint% = 1% then gosub print_params

*       *** Format Statement for Work File
L16080:     FMT BI( 4),                  /* Record Sequence Number     */~
                CH( 9),                  /* Inv. Asset Account Number  */~
                CH(25),                  /* Part Number                */~
                CH( 3),                  /* Store Number               */~
                CH(25),                  /* Part Number                */~
                CH( 6),                  /* Lot Number                 */~
                CH( 4),                  /* Category Code              */~
                CH(25),                  /* Part Number                */~
                CH(32),                  /* Part Description           */~
                CH(30),                  /* Store Description          */~
                CH(30),                  /* Acct Description           */~
                CH(30),                  /* Category Description       */~
                5 * PD(14,4),            /* 5 Quantity Fields          */~
                PD(14,4),                /* Part Cost                  */~
                CH( 1)                   /* Inventory Costing Method   */

*       **** Report selection branching is done here
            if sort% > 1% then L16340
                if last_store_printed$ = " " then L16320
                if last_store_printed$ = storenr$ then L16320
                     gosub print_store_totals
                     pageline% = 99%
                     last_store_printed$ = storenr$
                     last_storedescr$    = storedesc$
L16320:         gosub print_detail
                goto read_loop1
L16340:     if sort% > 2% then L16430
                if last_account_printed$ = " " then L16410
                if last_account_printed$ = account$ then L16410
                     gosub print_account_totals
                     pageline% = 99%
                     last_account_printed$ = account$
                     last_acctdescr$       = accountdesc$
L16410:         gosub print_detail
                goto read_loop1
L16430:     if sort% > 3% then read_loop1
                if last_ctgy_printed$ <> catcode$ then L16470
                if last_ctgy_printed$ = " " then L16510
                if last_ctgy_printed$ = catcode$ then L16510
L16470:              gosub print_ctgy_totals
                     pageline% = 99%
                     last_ctgy_printed$ = catcode$
                     last_ctgydescr$    = catdescr$
L16510:         gosub print_detail
                goto read_loop1

        REM Print Part And Lot Information
        print_detail
            if pageline% > 54% then gosub print_page_heading
            if lastpart$ <> " " then L16670
                lastpart$ = partnr$
                lastpartdescr$ = descr$
                last_store_printed$ = storenr$
                last_storedescr$    = storedesc$
                last_account_printed$ = account$
                last_acctdescr$       = accountdesc$
                last_ctgy_printed$ = catcode$
                last_ctgydescr$    = catdescr$
                newpart% = 1%
L16670:     if lastpart$ = partnr$ then L16690
                gosub print_part_totals
L16690:     totalcost = round(cost*qty(qtyp%),2)
            sum_qty = sum_qty + qty(qtyp%)
            sum_cost = sum_cost + totalcost
            if s_or_d$ <> "D" then goto L16770
            gosub format_detail
            call "CONVERT" (qty(qtyp%), 2.2, prqty$)
            call "CONVERT" (cost, 2.4, pravgcost$)
            call "CONVERT" (totalcost, 2.4, prtotcost$)
L16770:     if newpart% = 0% then L16831
                if s_or_d$ <> "D" then goto L16810
                print using L50420, partnr$, descr$, storenr$, lot$,      ~
                                   costmthd$, prqty$, pravgcost$,        ~
                                   prtotcost$
L16810:         newpart% = 0%
                partsprinted% = partsprinted% + 1%
                if s_or_d$ <> "D" then goto L16870
                goto L16860
L16831:     if s_or_d$ <> "D" then return
            print using L50420, " ", " ", storenr$, lot$, costmthd$,      ~
                               prqty$, pravgcost$, prtotcost$
L16860:     pageline% = pageline% + 1%
L16870:     return

        print_part_totals
            if pageline% > 56% then gosub print_page_heading
            total_cat_cost = total_cat_cost + sum_cost
            if s_or_d$ = "D" and core_on% = 1% then gosub check_for_core
            if s_or_d$ = "D" then goto L16982
                if sum_qty = 0                                           ~
                     then average_cost = 0                               ~
                     else average_cost = round(sum_cost / sum_qty, 4)
                if core_on% <> 1% then print using L50700, lastpart$,     ~
                                       lastpartdescr$, sum_qty,          ~
                                       average_cost, sum_cost            ~
                else                   print using L51370, lastpart$,     ~
                                       lastpartdescr$, sum_qty,          ~
                                       average_cost, sum_cost
                pageline% = pageline% + 1%
                if core_on% = 1% then gosub check_for_core
                goto L17030                    /* Reset total variables */
L16982:     call "CONVERT" (sum_qty, 2.2, sum_qty$)
            call "CONVERT" (sum_cost, 2.4, sum_cost$)
            print skip(1)
            print using L50660, sum_qty$, sum_cost$
            if core_on% = 1% and reman% = 1% then                        ~
                                              gosub print_part_totals_core
            print skip(1)
            pageline% = pageline% + 3%
L17030:     lastpart$ = partnr$
            lastpartdescr$ = descr$
            newpart% = 1%
            sum_qty, sum_cost = 0
            return


        format_detail  /* Format Print line variables */
            if hnycstcd$="R" then costmthd$ = "Act LIFO"
            if hnycstcd$="X" then costmthd$ = "Act LIFO/Adj"
            if hnycstcd$="A" then costmthd$ = "Avg Cost"
            if hnycstcd$="B" then costmthd$ = "Mod Avg Cost"
            if hnycstcd$="S" then costmthd$ = "Std LIFO"
            if hnycstcd$="F" then costmthd$ = "Fixed Std"
            if hnycstcd$="L" then costmthd$ = "Last Cost"
            if hnycstcd$="M" then costmthd$ = "Manual Cost"
            if hnycstcd$="T" then costmthd$ = "Std FIFO"
            if hnycstcd$="P" then costmthd$ = "Act FIFO"
            if hnycstcd$="Y" then costmthd$ = "Act FIFO/Adj"
            if hnycstcd$=" " then costmthd$ = "No Selection"

            return

        REM *************************************************************~
            *    Prints Number Of Parts Printed, Total Cost For All     *~
            *    Parts Printed And Exits Program If End Of Print.       *~
            *************************************************************
        wrap_up
            if wseq% > 0% and okprint% > 0% then L19220
                if tt$ <> "B" then goto L19100
                     message$ = "rptReport " & pgm$ & " in Background: Nu~
        ~ll set selected."
                     call "MESSAGE" addr ("XM", str(userid$) & hex(20),  ~
                         message$, 78%, 0%)
                     goto exit_program

L19100:         call "ASKUSER" (okprint%, "*** NULL SET SELECTED ***",   ~
                     "There are no records in the criteria you selected",~
                     " ", "Press (RETURN) to acknowledge and continue")
                goto L19420
L19220:     on sort% gosub print_store_totals,                           ~
                           print_account_totals,                         ~
                           print_ctgy_totals

            if pageline% > 52% then gosub print_page_heading
            print skip(2)
            gosub'200(gpartsprinted%)
            if s_or_d$ <> "D"                                            ~
                then print using L50712, "Grand Totals", " ",             ~
                     partsprinted$, grand_total                          ~
                else print using L50580, gpartsprinted%, grand_total
            pageline% = pageline% + 3%
            if core_on% = 1% then core_sum_title$ = "GRAND TOTAL CORE V"&~
                             "ALUE SUMMARY BY ACCOUNT NUMBER"
            if core_on% = 1% then gosub print_core_totals
            time$ = " " : call "TIME" (time$)
            print using L50681, time$
L19420:     close printer
            call "SETPRNT" (rptid$, " ", 0%, 1%)
            if tt$ <> "B" then goto L19494
                message$ = "rptReport " & pgm$ & " in Background: Complet~
        ~ed."
                call "MESSAGE" addr ("XM", str(userid$) & hex(20),       ~
                     message$, 78%, 0%)
                goto exit_program
L19494:     pageline% = 99%
            goto inputmode1

        print_params
            pagenumber% = -1%
            gosub print_page_heading
L19525:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then L19545
                str(i$(), i%, 1%) = hex(20)
                goto L19525
L19545:     print skip(3)
            print using L50770
            print tab (56) : print columnttl$
            print using L50567, prompt$(1%), fmpart$, topart$
            print using L50567, prompt$(2%), fmstore$, tostore$
            print using L50567, prompt$(3%), fmcatcode$, tocatcode$
            print using L50567, prompt$(4%), qtytype$
            print using L50567, prompt$(5%), sortflag$
            print using L50567, prompt$(6%), s_or_d$
            print using L50567, prompt$(7%), bkground$
            print using L50790
            pageline% = 99%
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20150,      /* Part Range          */      ~
                              L20190,      /* Store Range         */      ~
                              L20230,      /* Category Range      */      ~
                              L20300,      /* Quantity Type       */      ~
                              L20330,      /* Print Order         */      ~
                              L20352,      /* Summary or Detail?  */      ~
                              L20360       /* Print in Background?*/
            return

L20150: REM Default/Enable For Part Range
            if fmpart$ = " " then fmpart$ = "ALL"
            return

L20190: REM Default/Enable For Store Range
            if fmstore$ = " " then fmstore$ = "ALL"
            return

L20230: REM Default/Enable For Category Range
            if fmcatcode$ = " " then fmcatcode$ = "ALL"
            return

        REM Default/Enable For Last Part Number
            return

L20300: REM Default/Enable For Quantity Type
            return

L20330: REM Default/Enable For Print Order
            return

L20352: REM Default/Enable For Summary or Detail? (S/D)        S_OR_D$
            if s_or_d$ = " " then s_or_d$ = "D"
            return

L20360: REM Default/Enable For Print in Background? (Y/N)      BKGROUND$
            if bkground$ = " " then bkground$ = "N"
            return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G E S    *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(fieldnr%)
            if fieldnr% <> 0% then L28110
                inpmessage$ = edtmessage$
                return

L28110
*        Define the Input Message for the Screen/Field Indicated
            restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Part Number range, partial, 'FIRST', 'LAST', 'ALL' or '?'~
        ~ Wildcard.",                                                     ~
         "Enter Store Number range, partial, 'ALL' or '?' Wildcard.",    ~
         "Enter Part Category range, partial, 'ALL' or '?' Wildcard.",   ~
         "Enter '1' - ON HAND, '2' - BACK ORDERED or '3' - ON ORDER.   ",~
         "Enter '1' sort by STORE, '2' by INVENTORY ASSET ACCOUNT or '3' ~
        ~by PART CATEGORY.",                                              ~
         "Enter 'S' for Summary report; otherwise 'D'.                 ",~
         "Enter 'Y' to print in the Background; otherwise, 'N'.        "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, qtytype$, sortflag$,       ~
                      fmcatcode$, fmpart$, fmstore$, hicatcode$,         ~
                      hipart$, histore$, locatcode$, lopart$, lostore$,  ~
                      tocatcode$, topart$, tostore$, blankline$,         ~
                      last_ctgy_printed$, last_store_printed$,           ~
                      last_account_printed$, last_acctdescr$,            ~
                      last_storedescr$, last_ctgydescr$, bkground$,      ~
                      s_or_d$
            return


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
            goto inputmode1

        REM *************************************************************~
            * Print subtotals on Store, Account or Category break.      *~
            *                                                           *~
            **************************************************************
        print_store_totals
            if partsprinted% = 0 then return
            if pageline% > 54% then gosub print_page_heading
            gosub print_part_totals
            if s_or_d$ = "D" then goto L30150
                gosub'200(partsprinted%)
                if core_on% = 1% then L30125
                print using L50732
                print using L50712, "Store " & last_store_printed$ &      ~
                     " Total", last_storedescr$, partsprinted$,          ~
                     total_cat_cost
                goto L30180
L30125:         print using L51290
                if coresubvalue = 0 then init (" ")  coresubvalue$       ~
                   else call "CONVERT" (coresubvalue, 2.4, coresubvalue$)
                print using L51330, "Store " & last_store_printed$ &      ~
                     " Total", last_storedescr$, partsprinted$,          ~
                     total_cat_cost, coresubvalue$
                goto L30180
L30150:     print using L50503
            print using L50490, last_store_printed$, partsprinted%,       ~
                               total_cat_cost
L30180:     pageline% = pageline% + 2%
            if core_on% = 1% then core_sum_title$ = "CORE VALUE SUMMARY"&~
                             " BY ACCOUNT NUMBER FOR STORE " &           ~
                              last_store_printed$
            if core_on% = 1% then gosub print_core_subtotals
            gpartsprinted% = gpartsprinted% + partsprinted%
            grand_total = grand_total + total_cat_cost
            total_cat_cost, sum_qty, sum_cost = 0
            coresubvalue  = 0
            partsprinted% = 0
            return

        print_account_totals
            if partsprinted% = 0 then return
            if pageline% > 54% then gosub print_page_heading
            gosub print_part_totals
            if s_or_d$ = "D" then goto L30360
                gosub'200(partsprinted%)
                if core_on% = 1% then L30325
                print using L50732
                print using L50712, "Account " & last_account_printed$ &  ~
                     " Total", last_acctdescr$, partsprinted$,           ~
                     total_cat_cost
                goto L30390
L30325:         print using L51290
                if coresubvalue = 0 then init (" ")  coresubvalue$       ~
                   else call "CONVERT" (coresubvalue, 2.4, coresubvalue$)
                print using L51330, "Account " & last_account_printed$ &  ~
                     " Total", last_acctdescr$, partsprinted$,           ~
                     total_cat_cost, coresubvalue$
                goto L30390
L30360:     print using L50534
            print using L50520, last_account_printed$, partsprinted%,     ~
                               total_cat_cost
L30390:     gpartsprinted% = gpartsprinted% + partsprinted%
            grand_total = grand_total + total_cat_cost
            pageline% = pageline% + 2%
            if core_on% = 1% then core_sum_title$ = "CORE VALUE SUMMARY"&~
                             " BY ACCOUNT NUMBER FOR ACCOUNT " &         ~
                              last_account_printed$
            if core_on% = 1% then gosub print_core_subtotals
            total_cat_cost, sum_qty, sum_cost = 0
            coresubvalue  = 0
            partsprinted% = 0
            return

        print_ctgy_totals
            if partsprinted% = 0 then return
            if pageline% > 54% then gosub print_page_heading
            gosub print_part_totals
            if s_or_d$ = "D" then goto L30570
                gosub'200(partsprinted%)
                if core_on% = 1% then L30555
                print using L50732
                print using L50712, "Category " & last_ctgy_printed$ &    ~
                     " Total", last_ctgydescr$, partsprinted$,           ~
                     total_cat_cost
                goto L30600
L30555:         print using L51290
                if coresubvalue = 0 then init (" ")  coresubvalue$       ~
                   else call "CONVERT" (coresubvalue, 2.4, coresubvalue$)
                print using L51330, "Category " & last_ctgy_printed$ &    ~
                     " Total", last_ctgydescr$, partsprinted$,           ~
                     total_cat_cost, coresubvalue$
                goto L30600
L30570:     print using L50563
            print using L50550, last_ctgy_printed$, partsprinted%,        ~
                               total_cat_cost
L30600:     gpartsprinted% = gpartsprinted% + partsprinted%
            grand_total = grand_total + total_cat_cost
            pageline% = pageline% + 2%
            if core_on% = 1% then core_sum_title$ = "CORE VALUE SUMMARY"&~
                             " BY ACCOUNT NUMBER FOR CATEGORY " &        ~
                              last_ctgy_printed$
            if core_on% = 1% then gosub print_core_subtotals
            total_cat_cost, sum_qty, sum_cost = 0
            coresubvalue  = 0
            partsprinted% = 0%
            return

        deffn'200(xyz%)
            convert xyz% to partsprinted$, pic (####,###,##0)
            call "STRING" addr ("LJ", partsprinted$, 12%)
            return

*        Check for Reman Part and print Core value if needed
        check_for_core
            reman% = 0%
            corepart$ = lastpart$
            call "CORVALSB" ("CK", corepart$, " ", " ", corestccost,     ~
                             " ", core_fg_acct$, " ", " ", " ", 0%, " ", ~
                             " ", #3, #8, #2, #50, ret%)
            if ret% <> 0% then return
            if corestccost = 0 then return
            reman% = 1%

            search str(sub_core_acct$()) = str(core_fg_acct$,1%,9%) to   ~
                                                         index%() step 9%
            if index%(1%) <> 0% then L30960

                sub_core_cnt% = sub_core_cnt% + 1%
                if sub_core_cnt% > 100% then sub_core_cnt% = 100%
                index%(1%) = sub_core_cnt%
                sub_core_acct$(index%(1%)) = core_fg_acct$

L30960:     sub_core_acct(index%(1%)) = sub_core_acct(index%(1%)) +      ~
                                        round(corestccost * sum_qty, 2)
            coresubvalue = coresubvalue + round(corestccost * sum_qty, 2)

            call "GLFMT" (core_fg_acct$)
            call "CONVERT" (sum_qty, 2.2, core_sum_qty$)
            call "CONVERT" (corestccost, 2.4, corestccost$)
            call "CONVERT" (corestccost * sum_qty, 2.4, corevalue$)

            if pageline% > 56% then gosub print_page_heading
            if s_or_d$ = "D" then print using L50920, corepart$,          ~
                               core_fg_acct$, core_sum_qty$,             ~
                               corestccost$, corevalue$
            if s_or_d$ = "S" then print using L51170, corepart$,          ~
                               core_sum_qty$, corestccost$, corevalue$
            pageline% = pageline% + 1%
            return

        print_part_totals_core
            if pageline% > 56% then gosub print_page_heading
            print using L50960, core_sum_qty$, corevalue$
            pageline% = pageline% + 1%
            return

        print_core_subtotals
            if sub_core_cnt% < 1% then return
            if pageline% > 53% then gosub print_page_heading

            call "STRING" addr("CT", core_sum_title$, 76%)
            gosub print_core_acct_heading
            coresubvalue = 0
            for i% = 1% to sub_core_cnt%
                call "DESCRIBE" (#7, sub_core_acct$(i%), coreacctdescr$, ~
                                                             0%, f1%(7%))
                if coreacctdescr$ = " " then                             ~
                           coreacctdescr$ = "Core Asset Acct not on file"
                call "CONVERT" (sub_core_acct(i%), 2.2, coreaccttotal$)
                core_fg_acct$ = sub_core_acct$(i%)
                call "GLFMT" (core_fg_acct$)
                if pageline% < 57% then L31340
                     gosub print_page_heading
                     gosub print_core_acct_heading
L31340:         print using L51080, core_fg_acct$, coreacctdescr$,        ~
                                   coreaccttotal$
                coresubvalue = coresubvalue + sub_core_acct(i%)
                pageline% = pageline% + 1%
            next i%

            if pageline% < 57% then L31430
                gosub print_page_heading
                gosub print_core_acct_heading
L31430:     call "CONVERT" (coresubvalue, 2.2, coresubvalue$)
            print using L51110
            print using L51140, coresubvalue$
            print
            pageline% = pageline% + 3%

            for i% = 1% to sub_core_cnt%
                search str(tot_core_acct$()) = str(sub_core_acct$(i%))   ~
                                                      to index%() step 9%
                if index%(1%) <> 0% then L31590

                     tot_core_cnt% = tot_core_cnt% + 1%
                     if tot_core_cnt% > 200% then tot_core_cnt% = 200%
                     index%(1%) = tot_core_cnt%
                     tot_core_acct$(index%(1%)) = sub_core_acct$(i%)

L31590:         tot_core_acct(index%(1%)) = tot_core_acct(index%(1%)) +  ~
                                                        sub_core_acct(i%)
            next i%

            init (" ")  sub_core_acct$()
            mat sub_core_acct = zer
            sub_core_cnt% = 0%
            coresubvalue  = 0
            return

        print_core_acct_heading
            if pageline% > 53% then gosub print_page_heading
            print
            print using L50990, core_sum_title$
            print
            print using L51020
            print using L51050
            pageline% = pageline% + 5%
            return

        print_core_totals
            if tot_core_cnt% < 1% then return
            if pageline% < 54% then L32035
                print_detail_hd% = 1%
                gosub print_page_heading
                print_detail_hd% = 0%

L32035:     call "STRING" addr("CT", core_sum_title$, 76%)
            gosub print_core_acct_heading
            coresubvalue = 0
            for i% = 1% to tot_core_cnt%
                call "DESCRIBE" (#7, tot_core_acct$(i%), coreacctdescr$, ~
                                                             0%, f1%(7%))
                if coreacctdescr$ = " " then                             ~
                           coreacctdescr$ = "Core Asset Acct not on file"
                call "CONVERT" (tot_core_acct(i%), 2.2, coreaccttotal$)
                core_fg_acct$ = tot_core_acct$(i%)
                call "GLFMT" (core_fg_acct$)
                if pageline% < 57% then L32170
                     print_detail_hd% = 1%
                     gosub print_page_heading
                     print_detail_hd% = 0%
                     gosub print_core_acct_heading
L32170:         print using L51080, core_fg_acct$, coreacctdescr$,        ~
                                   coreaccttotal$
                coresubvalue = coresubvalue + tot_core_acct(i%)
                pageline% = pageline% + 1%
            next i%

            if pageline% < 57% then L32260
                print_detail_hd% = 1%
                gosub print_page_heading
                print_detail_hd% = 0%
                gosub print_core_acct_heading
L32260:     call "CONVERT" (coresubvalue, 2.2, coresubvalue$)
            print using L51110
            print using L51140, coresubvalue$
            print
            pageline% = pageline% + 3%

            init (" ")  tot_core_acct$()
            mat tot_core_acct = zer
            tot_core_cnt% = 0%
            coresubvalue  = 0
            return

        REM *************************************************************~
            *      S C R E E N    # 1   I N P U T / E D I T             *~
            *-----------------------------------------------------------*~
            * Input & Edit for Report Selection Criteria                *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40850,         /* Part Number       */   ~
                                L40850,         /* Store Number      */   ~
                                L40850,         /* Part Category     */   ~
                                L40850,         /* Quantity Type     */   ~
                                L40850,         /* Sort Order        */   ~
                                L40850,         /* Summary or Detail?*/   ~
                                L40850          /* Print in Bckgrnd? */
              goto L41000

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40850:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L41000:     accept                                                       ~
               at (01,02),                                               ~
                  "Input Report Selection Criteria",                     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,30), fac(hex(ac)),   columnttl$           , ch(51),~
                                                                         ~
               at (07,02), fac(hex(8c))  , prompt$(1%)          , ch(28),~
               at (07,30), fac(lfac$(1%)), fmpart$              , ch(25),~
               at (07,56), fac(lfac$(1%)), topart$              , ch(25),~
                                                                         ~
               at (08,02), fac(hex(8c))  , prompt$(2%)          , ch(28),~
               at (08,30), fac(lfac$(2%)), fmstore$             , ch(03),~
               at (08,56), fac(lfac$(2%)), tostore$             , ch(03),~
                                                                         ~
               at (09,02), fac(hex(8c))  , prompt$(3%)          , ch(28),~
               at (09,30), fac(lfac$(3%)), fmcatcode$           , ch(04),~
               at (09,56), fac(lfac$(3%)), tocatcode$           , ch(04),~
                                                                         ~
               at (10,02), fac(hex(8c))  , prompt$(4%)          , ch(28),~
               at (10,30), fac(lfac$(4%)), qtytype$             , ch(01),~
                                                                         ~
               at (11,02), fac(hex(8c))  , prompt$(5%)          , ch(28),~
               at (11,30), fac(lfac$(5%)), sortflag$            , ch(01),~
                                                                         ~
               at (12,02), fac(hex(8c))  , prompt$(6%)          , ch(28),~
               at (12,30), fac(lfac$(6%)), s_or_d$              , ch(01),~
                                                                         ~
               at (13,02), fac(hex(8c))  , prompt$(7%)          , ch(28),~
               at (13,30), fac(lfac$(7%)), bkground$            , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L42600
                  call "MANUAL" (pgm$) : goto L41000

L42600:        if keyhit% <> 15 then L42750
                  call "PRNTSCRN" : goto L41000

L42750:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L43700     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L43500
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L43500:     if fieldnr% > 1% then L43600
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L43600:     return

L43700: if fieldnr% > 0% then L44150  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Print Report"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L44150:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L45100,         /* Part Number            */~
                              L45200,         /* Store Number           */~
                              L45300,         /* Part Category          */~
                              L45500,         /* Quantity Type          */~
                              L45600,         /* Print Order            */~
                              L45661,         /* Summary or Detail?     */~
                              L45670          /* Print in Background?   */
            return

L45100: REM Test Data For Part Number Range
            call "TESTRNGE" (fmpart$, topart$, lopart$, hipart$,         ~
                             errormsg$, #2)
            return

L45200: REM Test Data For Store Number Range
            call "TESTRNGE" (fmstore$, tostore$, lostore$, histore$,     ~
                             errormsg$, #4)
            return

L45300: REM Test Data For Category Code Range
            call "TESTRNGE" (fmcatcode$, tocatcode$,                     ~
                             locatcode$, hicatcode$, errormsg$, #5)
            return

L45500: REM Test Data For Quantity Type Selected
            if qtytype$ = "1" or qtytype$ = "2" or                       ~
                                 qtytype$ = "3" then return
            errormsg$ = "Quantity Type Must Be '1', '2', '3'."
            return

L45600: REM Test Data For Print Order Selection
            if sortflag$ = "1" or sortflag$ = "2"                        ~
                               or sortflag$ = "3" then return
            errormsg$ = " Print Order Must Be '1', '2', or '3'."
            return

L45661: REM Test Data For Summary or Detail? (S/D)       S_OR_D$
            if s_or_d$ = "S" or s_or_d$ = "D" then return
            errormsg$ = "Enter 'S' or 'D'"
            return

L45670: REM Test Data For Print in Background?           BKGROUND$
            if bkground$ = "Y" or bkground$ = "N" then return
            errormsg$ = "Enter 'Y' or 'N'"
            return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Handles All The Print Formatting.                         *~
            *************************************************************

L50060: %RUN DATE: ######## @ ########       ############################~
        ~################################                     ############~
        ~###
L50090: %                                    ############################~
        ~################################                          PAGE: #~
        ~###
L50120: %                                                  In Order of Pa~
        ~rt Number By Store               Requestor: #####################~
        ~###

L50180: %                                     In Order of Part Number By ~
        ~Inventory Asset Account Number   Requestor: #####################~
        ~###

L50230: %                                              In Order of Part N~
        ~umber By Category Code           Requestor: #####################~
        ~####

L50290: %Store ### - ###############################  Using #############~
        ~########

L50320: %Account Number ############ - ###############################  U~
        ~sing #####################

L50350: %Category Code #### - ##############################  Using #####~
        ~################

L50364: %Part Number                Part Description                Store~
        ~  Lot     Cost Method     Quantity    Avg Cost    Total Cost


L50380: %-------------------------  ------------------------------  -----~
        ~  ------  ------------  ----------  ----------  ------------

L50420:    %#########################  ##############################   #~
        ~##   ######  ############  ##########  ##########    ############

L50490: %                    ********** Total Number of Parts Printed for~
        ~ Store Number ### is ##########  Valued at  -####,###,###.##

L50503: %                               ---------------------------------~
        ~------------------------------------------------------------


L50520: %           **********  Total Number of Parts Printed for Asset A~
        ~ccount  ######### is ##########  Valued at  -####,###,###.##

L50534: %                       -----------------------------------------~
        ~------------------------------------------------------------

L50550: %                 **********  Total Number of Parts Printed for C~
        ~ategory Code #### is ##########  Valued at  -####,###,###.##

L50563: %                             -----------------------------------~
        ~------------------------------------------------------------

L50567: %                          ############################ #########~
        ~################ #########################

L50580: %         ******************** GRAND TOTAL Number of Parts Printe~
        ~d for this Report is ##########  Valued at -#,###,###,###.##

        %  Grand Total Of Cost Of Parts Printed is  -##,###,###,###.##

L50660: %                                                       ---------~
        ~>Total PART Quantity ########## Value is -#,###,###,###.##

L50681: %                                                  *** END OF REP~
        ~ORT @ ######## ***
L50700: %      #########################   ##############################~
        ~##   -###,###,###.##  -###,###,###.####  -####,###,###.##
L50712: % ***  #########################   ##############################~
        ~##    Parts Printed:  ############       -####,###,###.##
L50720: %      -------------------------   ------------------------------~
        ~--   ---------------  -----------------   ---------------
L50732: %      -------------------------   ------------------------------~
        ~--   ---------------                      ---------------
L50740: %      Part Code                   Description                   ~
        ~            Quantity       Average Cost        Total Cost

L50770: %                          ------------------------- Report Selec~
        ~tion Parameters --------------------------

L50790: %                          --------------------------------------~
        ~------------------------------------------

*        Image statements for core value printing

L50920:    %     Core Value            #########################  GL: ###~
        ~#########    at Standard   ##########  ##########    ##########

L50960: %                                                       ---->Tota~
        ~l Part Core Quantity ########## Value is -#,###,###,###.##

L50990: %                              ##################################~
        ~######################################

L51020: %                                      ACCOUNT      DESCRIPTION  ~
        ~                      CORE VALUE

L51050: %                                      ------------ -------------~
        ~------------------- ------------

L51080: %                                      ############ #############~
        ~################### ############

L51110: %                                                                ~
        ~                    ------------

L51140: %                                                                ~
        ~                   #############

L51170: %      Core Value              ################################  ~
        ~      ##########         ########## STC                   #######~
        ~###

L51210: %  Part Code                   Description                       ~
        ~        Quantity       Average Cost        Total Cost     Core Va~
        ~lue

L51250: %  -------------------------   --------------------------------  ~
        ~ ---------------  -----------------   ---------------  ----------~
        ~---

L51290: %  -------------------------   --------------------------------  ~
        ~ ---------------                      ---------------  ----------~
        ~---

L51330: % ***  #####################   ################################  ~
        ~  Parts Printed:  ############       -####,###,###.##  ##########~
        ~###

L51370: %  #########################   ################################  ~
        ~ -###,###,###.##  -###,###,###.####  -####,###,###.##
        REM *************************************************************~
            *  P A G E   H E A D I N G / C O N T R O L   R O U T I N E  *~
            *                                                           *~
            * TRACKS WHICH LINE OF THE PAGE WE ARE ON, SKIPS TO NEW PAGE*~
            * AND PRINTS HEADINGS IF WE'RE NOT ABLE TO FIT IT ALL ON ONE*~
            *************************************************************
        print_page_heading
            print page
            pagenumber% = pagenumber% + 1
            print using L50060, date$, time$, company$, pgm$ & "-" &      ~
                rptid$
            print using L50090, subheader$, pagenumber%
            if sort% > 1% then L60200
                if last_store_printed$ <> " " then L60130
                     last_store_printed$ = storenr$    /* 1st time only */
                     last_storedescr$    = storedesc$
L60130:         print using L50120, username$
                print skip(1)
                if pagenumber% = 0% then L60370
                if print_detail_hd% <> 1% then L60150
                     pageline% = 6%
                     goto L60380
L60150:         print using L50290, last_store_printed$,                  ~
                                   last_storedescr$, qtylit$(qtyp%)
                print skip(1)
                goto L60335

L60200:     if sort% > 2% then L60280
                if last_account_printed$ <> " " then L60210
                     last_account_printed$ = account$  /* 1st time only */
                     last_acctdescr$       = accountdesc$
L60210:         print using L50180, username$
                print skip(1)
                if pagenumber% = 0% then L60370
                if print_detail_hd% <> 1% then L60230
                     pageline% = 6%
                     goto L60380
L60230:         print using L50320, last_account_printed$,                ~
                                   last_acctdescr$, qtylit$(qtyp%)
                print skip(1)
                goto L60335

L60280:     if last_ctgy_printed$ <> " " then L60290
                last_ctgy_printed$ = catcode$          /* 1st time only */
                last_ctgydescr$    = catdescr$
L60290:     print using L50230, username$
            print skip(1)
            if pagenumber% = 0% then L60370
            if print_detail_hd% <> 1% then L60310
                pageline% = 6%
                goto L60380
L60310:     print using L50350, catcode$, catdescr$, qtylit$(qtyp%)
            print skip(1)
            if pagenumber% = 0% then L60370
L60335:     if s_or_d$ = "D" then goto L60350
                if core_on% = 1% then L60345
                print using L50740
                print using L50720
                goto L60370
L60345:         print using L51210
                print using L51250
                goto L60370
L60350:     print using L50364
            print using L50380
L60370:     pageline% = 8%
L60380:     return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND DISPLAYS MESSAGE *~
            * (ONLY IN FOREGROUND) WHILE WE LINK BACK TO THE MENU.      *~
            *************************************************************
        exit_program
             call "FILEBGON" (#6)
             call "SHOSTAT" ("One Moment Please")
             end
