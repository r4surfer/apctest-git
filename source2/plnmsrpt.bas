        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  PPPP   L      N   N  M   M   SSS   RRRR   PPPP   TTTTT   *~
            *  P   P  L      NN  N  MM MM  S      R   R  P   P    T     *~
            *  PPPP   L      N N N  M M M   SSS   RRRR   PPPP     T     *~
            *  P      L      N  NN  M   M      S  R   R  P        T     *~
            *  P      LLLLL  N   N  M   M   SSS   R   R  P        T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PLNMSRPT - Generates a 13 bucket master schedule report by*~
            *            range of parts-generic ref.-category-type.     *~
            *            Optionally prints standard cost - details and  *~
            *            summary reports.  Uses 13 weeks or months with *~
            *            selection of calendar or fiscal months.        *~
            *      Note- The Net w/Carry row's accuracy is dependent on *~
            *            the accuracy of the Prior Adds field.  This is *~
            *            calculated based on HNYDETAL, which may have   *~
            *            been purged at some time.  Therefore, the Net  *~
            *            w/Carry row's accuracy is dubious, at best! It *~
            *            should be used as an indicator of increases &  *~
            *            decreases to inventory rather than the reality *~
            *            of expected inventory levels for that period.  *~
            *----------------------------------------------------------Q*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 09/04/86 ! ORIGINAL                                 ! HDC *~
            * 07/01/87 ! Standard Costing Changes                 ! MJB *~
            * 01/10/89 ! Changed to report 13 periods regardless  ! MJB *~
            *          !  of end of planning calendar             !     *~
            * 10/29/90 ! Corrected Range testing on Category Code ! MJB *~
            * 12/05/90 ! Changed HNYDETAL accumulations to weekly ! MJB *~
            *          !  buckets instead of all in PRIOR.        !     *~
            *          ! Modified bucketing logic to speed it up. !     *~
            * 01/12/93 ! Page 0 Facs, Header, & End Report w/Time.! RJH *~
            * 08/20/93 ! PRR 11693.  Multiple corrections.        ! JDH *~
            *          !   Added Type 7 demands to All Req section!     *~
            *          !   Added 'QC' tags to open puch section   !     *~
            *          !   Added 'RO' tags to buy advices section !     *~
            *          !   Added 'BW' & 'RW' to buy advices sec   !     *~
            *          !   Added 'QC', 'RO', 'BW', & 'RW' tags to !     *~
            *          !     sources section                      !     *~
            *          !   Corrected open jobs bucket values      !     *~
            *          !   Corrected buy advices bucket values    !     *~
            *          !   Should be better after all that!       !     *~
            *          ! Fixed rpt name & page #s.                !     *~
            * 08/23/96 ! Changes for the year 2000.               ! DXL *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        dim                                                              ~
            abc$1,                       /* ABC CATEGORY CODE          */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            bucketdescr$(12)20,          /* Cost Bucket descriptions   */~
            bucketid$(12)10,             /* Cost Bucket ID's           */~
            buyer1$3,                    /* BUYER/PLANNER              */~
            buyer2$3,                    /* ANOTHER BUYER/PLANNER      */~
            catcode$4,                   /* CATEGORY CODE              */~
            catcoder$4,                  /* CAT CODE REQUESTED         */~
            catto$4,                     /* To CATCODE                 */~
            cattosave$4,                 /* To CATCODE for RELOAD      */~
            catsave$4,                   /* LAST CAT CODE FOR RELOAD   */~
            catdescr$30,                 /* CAT CODE REQ. DESCRIPTION  */~
            costs(12),                   /* Part Standard Costs        */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            dateline$28,                 /* DATES TO BE REPORTED       */~
            datelsave$28,                /* LAST DATES FOR RELOAD      */~
            date_pcd$9,                  /* PLNNED DATES FOR DEMANDS   */~
            date_req$8,                  /* REQTD DATES FOR DEMANDS    */~
            diskkey$50,                  /* ALTERNATE READKEY$         */~
            duedate$8,                   /* DUE DATE                   */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            fismonst$(17)8,              /* FISCAL MONTH START         */~
            freq$25,                     /* REPORT BY (M-F-W)          */~
            freqsave$25,                 /* LAST REPORT BY FOR RELOAD  */~
            genref$16,                   /* GENERIC X-REF              */~
            gensave$16,                  /* LAST GEN REF REQ FOR RELOAD*/~
            genrefr$16,                  /* REQ. GENERIC REF.          */~
            gento$16,                    /* To GENREF                  */~
            gentosave$16,                /* GENTO For RELOAD           */~
            gendescr$30,                 /* GENERIC DESCRIPTION        */~
            genread$41,                  /* KEY FOR PLOWING HNYGENER   */~
            highstart$8,                 /* HIGHEST POSSIBLE START     */~
            hnyplow$25,                  /* PLOW KEY FOR HNYMASTR      */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            leadtime$10,                 /* LEAD TIME                  */~
            limit$25,                    /* LIMIT TO HNYMASTR PLOW     */~
            line2$79,                    /* SCREEN LINE                */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            moq$10,                      /* MINIMUM ORDER QUANTITY     */~
            mv$9,                        /* MAIN VENDOR                */~
            part$25,                     /* PART NUMBER TO BE REPORTED */~
            partsave1$25,                /* LAST FROM PART FOR RELOAD  */~
            firstpart$25,                /* FROM PART                  */~
            lastpart$25,                 /* LAST PART                  */~
            partsave2$25,                /* LAST TO PART FOR RELOAD    */~
            parttest$25,                 /* PART NUMBER FROM DEMMASTR  */~
            partdesc$32,                 /* DESCRIPTION OF PART        */~
            pf16$20,                     /* SCREEN DISPLAY VALUES      */~
            pf4$20,                      /* SCREEN DISPLAY VALUE 4     */~
            pf5$20,                      /* SCREEN DISPLAY VALUE 5     */~
            plbase$6,                    /* PLANNING BASE DATE         */~
            plowkey$50,                  /* PLOW KEY                   */~
            priority$1,                  /* PRIORITY                   */~
            readkey$100,                 /* KEY FOR READING FILES      */~
            rpthead$60,                  /* COMPANY NAME-TOP OF RPT    */~
            rlsdate$8,                   /* RELEASE DATE               */~
            rptweek$(14)8,                /* REPORT DATES              */~
            rundate$8,                   /* RUN DATE OF REPORT         */~
            runtime$8,                   /* RUN TIME OF REPORT         */~
            s$1,                         /* U = UNPLN DEM/P = PNLD DEM */~
            scrdate$8,                   /* START DATE FOR HORIZON RPT */~
            datesave1$8,                 /* LAST START DATE FOR RELOAD */~
            scrdate1$8,                  /* START DATE FOR COMPARISON  */~
            seldate$6,                   /* SELECTION DATE FOR REPORT  */~
            set$8,                       /* Cost Set                   */~
            setid$4,                     /* Cost Set ID                */~
            setdescr$30,                 /* Cost Set Description       */~
            srckey$100,                  /* KEY FOR SOURCES (PIPIN)    */~
            srcrpt$1,                    /* SRC & USES ANSWER          */~
            srcsave$1,                   /* LAST SRCRPT FOR RELOAD     */~
            srctag$19,                   /* TAG NUMBER FROM PIPIN      */~
            start$6,                     /* START DATE FOR REPORT      */~
            stdcost$1,                   /* Incl. STD COST ANSWER      */~
            stdsave$1,                   /* LAST STDCOST FOR RELOAD    */~
            tag$19,                      /* TAG NUMBER                 */~
            tdate$10,                    /* Temporary Date             */~
            type$10,                     /* PART TYPE                  */~
            typer$10,                    /* PART TYPE REQUESTED        */~
            typeto$10,                   /* To PART TYPE               */~
            typetosave$10,               /* To PART TYPE For RELOAD    */~
            typesave$10,                 /* LAST PART TYPE FOR RELOAD  */~
            typedescr$30,                /* TYPE REQUESTED DESCRIPTION */~
            vend$9,                      /* VENDOR NUMBER              */~
            demreqt(13),                 /* TOTAL DEMANDS              */~
            demsum(13),                  /* SUM OF DEMANDS             */~
            plnreqt(13),                 /* TOTAL PLANS                */~
            plnsum(13),                  /* SUM OF PLANS               */~
            week%(14),                   /* MONDAYS OF 9 WEEKS FOR RPT */~
            week$(14)8,                  /* WEEKS CONVERSION           */~
            weekoh(13),                  /* WEEKS AVAILABE TO SELL     */~
            weeknra(13),                 /* WEEKS ADDITIONS            */~
            weeknru(13),                 /* WEEKS USES                 */~
            weeknrn(13),                 /* WEEKS NET POSITION         */~
            weeknrj(13),                 /* WEEKS JOBS                 */~
            weeknrp(13),                 /* WEEKS PURCHASES            */~
            weeknrb(13),                 /* WEEKS BUY ADVICES          */~
            weeknrw(13),                 /* WEEKS WORK ADVICES         */~
            sum$1,                       /* SUM (Y/N) on INPUT         */~
            sumsave$1,                   /* LAST SUM (Y/N) FOR RELOAD  */~
            sumrnrj(13),                 /* SUM - JOBS                 */~
            sumrnrp(13),                 /* SUM - PURCHASES            */~
            sumrnrb(13),                 /* SUM - BUY ADVICES          */~
            sumrnrw(13),                 /* SUM - WORK ADVICES         */~
            sumrnra(13),                 /* SUM - NET REQ ADDS         */~
            weekreq(13),                 /* REQT'S FOR TOTAL FORCSTS   */~
            sumrnrn(13),                 /* SUM - NET ADD - USE        */~
            sumrnru(13),                 /* SUM - NET REQ USE          */~
            weekreq1(13),                /* REQT'S FOR PLANNED FRCTS   */~
            weekreq2(13),                /* REQT'S FOR TOTAL SALES     */~
            weekreq3(13),                /* REQT'S FOR PLANNED SALES   */~
            weekreq4(13),                /* REQT'S FOR TOT NNET SALES  */~
            weekreq5(13),                /* REQT'S FOR PLN NNET SALES  */~
            weekreq6(13),                /* REQT'S FOR TOT MISC ADDS   */~
            weekreq7(13),                /* REQT'S FOR PLN MISC ADDS   */~
            weekstds(13,13),             /* 12 Std Buckets & total     */~
            yymmdd$(490)6                /* PRODUCTION CALENDAR        */

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64),                     /* = 1 IF READ WAS SUCCESSFUL */~
            rslt$(64)20,                 /* TEXT FROM FILE OPENING     */~
            axd$(64)4                    /* ALT KEY POINTER FROM OPEN'G*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* THE FILE OPENING ROUTINES.                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! HNYQUAN  ! Inventory Store Quantity File            *~
            * #2  ! HNYMASTR ! Inventory Master File                    *~
            * #6  ! CALMASTR ! Planning Production Calendar File        *~
            * #7  ! PIPIN    ! Planned inventory additions detail; feed *~
            * #9  ! JBMASTR2 ! JOB MASTER FILE                          *~
            * #10 ! VBKMASTR ! PURCHASE ORDER MASTER FILE               *~
            * #11 ! VBKLINES ! PURCHASE ORDER DETAIL FILE               *~
            * #12 ! SYSFILE2 ! Caelus Management System Information     *~
            * #13 ! DEMMASTR ! DEMAND MASTER FILE                       *~
            * #14 ! WORKFILE ! WORK FILE FOR SORTING DEMANDS            *~
            * #15 ! CATEGORY ! CATEGORY MASTER FILE                     *~
            * #16 ! PIPOUT   ! Planned inventory deductions             *~
            * #17 ! HNYDETAL ! Deatail of Inv. Transactions             *~
            * #18 ! HNYGENER ! Generic Cross-Reference File             *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "HNYQUAN",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  650,                                  ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =   44

            select #2,  "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  900,                                  ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            select #6,  "CALMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1962,                                  ~
                        keypos =    1, keylen =   2                      ~

            select #7,  "PIPIN",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   60,                                  ~
                        keypos =   30, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  48          ~

            select #9, "JBMASTR2",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 1300,                                   ~
                       keypos = 1, keylen = 8

            select #10, "VBKMASTR",                                      ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 1030,                                   ~
                       keypos = 1, keylen = 25,                          ~
                       alt key 1, keypos =10, keylen = 16

            select #11, "VBKLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 700,                                   ~
                        keypos = 1, keylen = 28

            select #12, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos =    1, keylen =  20                      ~

            select #13, "DEMMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 123,                                   ~
                        keypos =  2, keylen = 27,                        ~
                        alt key 1,  keypos = 10, keylen = 19,            ~
                            key 2,  keypos =  1, keylen = 28

            select #14, "WORKFILE",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 47,                                   ~
                         keypos = 1, keylen = 17

            select  #15,"CATEGORY",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 500,                                  ~
                         keypos = 1, keylen = 4

            select  #16,"PIPOUT",                                        ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize =  64,                                  ~
                         keypos = 1, keylen = 57 ,                       ~
                         alt key 1,  keypos = 20, keylen = 37

            select  #17,"HNYDETAL",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize =  150,                                 ~
                         keypos = 1, keylen = 42

            select  #18,"HNYGENER",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize =  100,                                 ~
                         keypos =17, keylen = 25 ,                       ~
                         alt key 1,  keypos = 1, keylen = 41

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENFILE" (#1,  "SHARE", f2%(1 ), rslt$(1 ), axd$(1 ))
            call "OPENFILE" (#2,  "SHARE", f2%(2 ), rslt$(2 ), axd$(2 ))
            call "OPENFILE" (#6,  "SHARE", f2%(6 ), rslt$(6 ), axd$(6 ))
            call "OPENFILE" (#7,  "SHARE", f2%(7 ), rslt$(7 ), axd$(7 ))
            call "OPENFILE" (#9,  "SHARE", f2%(9 ), rslt$(9 ), axd$(9 ))
            call "OPENFILE" (#10, "SHARE", f2%(10), rslt$(10), axd$(10))
            call "OPENFILE" (#11, "SHARE", f2%(11), rslt$(11), axd$(11))
            call "OPENFILE" (#12, "SHARE", f2%(12), rslt$(12), axd$(12))
            call "OPENFILE" (#13, "SHARE", f2%(13), rslt$(13), axd$(13))
            call "OPENFILE" (#15, "SHARE", f2%(15), rslt$(15), axd$(15))
            call "OPENFILE" (#16, "SHARE", f2%(16), rslt$(16), axd$(16))
            call "OPENFILE" (#17, "SHARE", f2%(17), rslt$(17), axd$(17))
            call "OPENFILE" (#18, "SHARE", f2%(18), rslt$(18), axd$(18))

            if f2%(12) <> 0 then L65000

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            openflag = 0
            printerflag = 0
            date$ = date
            call "DATEFMT" (date$)
            run% = 0%
            str(line2$,62%) = "PLNMSRPT: " & str(cms2v$,,8%)
            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press RETURN."
            call "COMPNAME" (12%, rpthead$, u3%)

*        Get Cost Bucket Descriptors
            buckets% = 3%
            set$ =  "        "
            call "STCSETID" (buckets%, #12, set$, setid$,                ~
                             bucketid$(), bucketdescr$(), setdescr$)
            if buckets% > 0% then L09230
                setid$ = " " : def% = 1%
                goto L09300

L09230:     get #12, using L09240, def%
L09240:         FMT POS(442), BI(1)
            if def% < 1% or def% > 12% then def% = 1%

            if buckets% = 12% then L09300
            temp% = 10% * buckets% + 1%:str(bucketid$(),temp%)    = " "
            temp% = 20% * buckets% + 1%:str(bucketdescr$(),temp%) = " "
L09300:     if buckets% = 0% then buckets% = 12%
*          STR(BUCKETNR$()) = " "

*          FOR I% = 1% TO BUCKETS%
*              CONVERT I% TO BUCKETNR$(I%), PIC(##)
*          NEXT I%

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
            gosub clear_variables
            lastpart$, typedescr$,gendescr$,catdescr$,dateline$,monweek$,~
            freq$, scrdate$, firstpart$,srcrpt$,sum$,stdcost$,genrefr$,  ~
            typer$, catcoder$ ,stdcost$,typeto$,catto$,gento$ = " "
            editmode% = 0%
            screend% = 0%
            pageno% = 0%
            hit% = 0%
            summary% = 0%
            if run% = 1% then pf5$ = "(5)Load Last Range" else pf5$ = " "
            pf16$ = "(16)Exit Program"
            init (" ") fismonst$()
                futsumnrj, prisumnrj  , futsumnrb, prisumnrb, futsumnrw, ~
                prisumnrw, futsumnrp, prisumnrp, prisumnra,prisumnru,    ~
                prisumnrn, futsumnra, futsumnra, futsumnrn,futsumnru = 0
                mat sumrnrj  = zer  : mat sumrnrb = zer
                mat sumrnrw  = zer  : mat sumrnrp = zer
                mat costs    = zer  : mat sumrnra  = zer
                mat sumrnrn  = zer  : mat sumrnru  = zer

            for fieldnr% = 1 to  9
            if fieldnr% > 1% then pf5$ = " "
            if fieldnr% > 3 then pf6$ = "(6)Proceed To Edit"
L10350:     gosub'051(fieldnr%)
                if enabled% = 0 then L10510
L10370:     if fieldnr% >1 then pf4$ ="(4)Previous Field" else pf4$ = " "
L10380:     gosub'101(fieldnr%)
                if keyhit%  =  1 then gosub startover
                if keyhit% <>  4 then       L10470
L10420:              fieldnr% = max(1%, fieldnr% - 1%)
                     gosub'051(fieldnr%)
                     if enabled% = 1% then L10370
                     if fieldnr% = 1% then L10350
                     goto L10420
L10470:         if run% = 0% then L10490
                if keyhit% =  5 and fieldnr% = 1% then reload
L10490:         if keyhit% = 16 and fieldnr% = 1% then L65000
                if keyhit% <>  0 then       L10380
L10510:    gosub'151(fieldnr%)
*       ****   IF ERRORMSG$ <> " " THEN EDITMODE% = 0%
                if errormsg$ <> " " then L10380
           next fieldnr%

L11000: REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

            pf16$ = "(16)Run Report"
            pf6$ = " "
            pf4$ = " "
            pf5$ = " "
            inpmessage$ = edtmessage$
            errormsg$ = " "
            editmode% = 1%
L11110:     gosub'101(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L11110
            fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or  fieldnr% > 9% then L11110
            gosub'051(fieldnr%)
                  if enabled% = 0 then L11110

L11240:     gosub'101(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11240
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11240
            inpmessage$ = edtmessage$
            goto L11110

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            partsave1$ = firstpart$
            partsave2$ = lastpart$
            datesave1$ = scrdate$
            freqsave$ = freq$
            datelsave$ = dateline$
            cattosave$ = catto$
            catsave$ = catcoder$
            gentosave$ = gento$
            gensave$ = genrefr$
            typetosave$ = typeto$
            monwsave$ =  monweek$
            sumsave$ = sum$
            stdsave$ = stdcost$
            srcsave$ = srcrpt$
            typesave$ = typer$
            gosub L30000
            goto inputmode

        reload
            firstpart$ = partsave1$
            lastpart$ = partsave2$
            scrdate$ = datesave1$
            freq$ = freqsave$
            dateline$ = datelsave$
            catcoder$ = catsave$
            catto$ = cattosave$
            genrefr$ = gensave$
            gento$ = gentosave$
            monweek$ = monwsave$
            sum$ = sumsave$
            stdcost$ = stdsave$
            srcrpt$ = srcsave$
            typer$ = typesave$
            typeto$ = typetosave$
            for fieldnr% = 3 to 9
            gosub'151 (fieldnr%)
            next fieldnr%
            goto L11000
        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 0
                  on fieldnr% gosub                                      ~
                                   L20190 ,         /* MONTH / WEEK     */~
                                   L20240 ,         /* START DATE       */~
                                   L20290 ,         /* PART NUMBER      */~
                                   L20360 ,         /* GENERIC REF      */~
                                   L20430 ,         /* CATEGORY         */~
                                   L20500 ,         /* PART TYPE        */~
                                   L20570 ,         /* INCL. SRC & USE  */~
                                   L20630 ,         /* INCL. STD. COST  */~
                                   L20690           /* SUMMARY          */
                  return

L20190: REM Default/Enable For Start Date For Master Schedule Report
            inpmessage$ = "Enter 'W' for WEEKLY , 'C' for CALENDAR MONTHS~
        ~, or 'F' for FISCAL MONTHS"
            enabled% = 1%
            return

L20240: REM Default/Enable For Start Date For Master Schedule Report
            inpmessage$ = "Enter The Starting Date For The Report OR Leav~
        ~e Blank for Default."
            if scrdate$ = " " or scrdate$ = blankdate$ then scrdate$ = date$
            enabled% = 1%
            return

L20290: REM Default/Enable For Part Numbers
            inpmessage$ = "Enter the Part Number for the Report Or ALL"
            enabled% = 1%
            if firstpart$ = " " then firstpart$ = "ALL"
            return

L20360: REM Default/Enable For Generic Reference
            inpmessage$ = "Accept 'ALL' or Enter A Range of References"
            enabled% = 1%
            if firstpart$ = lastpart$ then enabled% = 0%
            if genrefr$ = " " then genrefr$ = "ALL"
            return

L20430: REM Default/Enable For Part Category
            inpmessage$ = "Accept 'ALL' or Enter A Range of Categories"
            enabled% = 1%
            if firstpart$ = lastpart$ then enabled% = 0%
            if catcoder$ = " " then catcoder$  = "ALL"
            return

L20500: REM Default/Enable For Part Type
            inpmessage$ = "Accept values or Enter A Range of Types"
            enabled% = 1
            if firstpart$ = lastpart$ then enabled% = 0%
            if typer$ <> " " then return
                typer$ = "000"
                typeto$ = "999"
                return

L20570: REM Default/Enable Report Srce & Uses
            inpmessage$ = "Enter 'Y' To Report SOURCES & USE  or 'N'" &  ~
                          " For No Report."
            enabled% = 1%
            return

L20630: REM Default/Enable Standard Cost
                inpmessage$ = "Enter 'Y' To Include STANDARD COST or"  & ~
                              " 'N' For No Report."
            enabled% = 1%
            return

L20690: REM Default/Enable Summary Report Options
            inpmessage$ = "Enter 'Y', 'N' or 'O' For Only"
            enabled% = 1%
            return

        clear_variables
            init(" ")errormsg$, inpmessage$, genref$,typedesc$,          ~
                     type$, catcode$, buyer1$, buyer2$, sellu$, buyu$,   ~
                     priority$, mv$, abc$, rptweek$()

                priornrb , futurenrb , priornrw , futurenrw ,            ~
                priorreq1, priorreq2 , priorreq3, priorreq4 ,            ~
                priorreq5, futurereq1, futurereq2,futurereq3,            ~
                futurereq4,futurereq5, futurereq6,futurereq7,            ~
                priorreq6, priorreq7 , priornrp , futurenrp ,            ~
                priornrj , futurenrj , pdemsum  , fdemsum   ,            ~
                priornra , futurenra , priornru , futurenru ,            ~
                fplntotl , pplntotl  , pdemtotl , fdemtotl  ,            ~
                futurenrn, priornrn  , pplnsum  , fplnsum   =  0

                mat weekreq  = zer  : mat weekreq1 = zer
                mat weekreq2 = zer  : mat weekreq3 = zer
                mat weekreq4 = zer  : mat weekreq5 = zer
                mat weekreq6 = zer  : mat weekreq7 = zer
                mat weekoh   = zer  : mat weeknra  = zer
                mat weeknrj  = zer  : mat weeknrb  = zer
                mat weeknrw  = zer  : mat weeknrp  = zer
                mat demreqt  = zer  : mat plnreqt  = zer
                mat weekstds = zer  : mat weeknru  = zer
                mat plnsum   = zer  : mat demsum   = zer
                mat weeknrn  = zer
            return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * OR WILL RETURN USER BACK TO WHERE THEY WERE.  MUST PUSH   *~
            * TWO BUTTONS TO START OVER FOR SAFETY.                     *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

            keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return

            return clear all
            goto L09000


L30000
*       *****************************************************************
*                     REPORT PROCESSING SECTION                         *
*       *****************************************************************
            if printerflag = 1 then L30070
               select printer (134)
               printerflag = 1
               call "SETPRNT" ("PLN003", " ", 0%, 0%)
L30070:     if openflag = 1 then L30100
               call "WORKOPEN" (#14, "IO   ", 1000%, f2%(14))
               openflag = 1
L30100:     init(" ")start$
            init (hex (00)) diskkey$, plowkey$
            test% = 1 : cnt% = 1
            gosub read_hnyquan
            if screend% > 0% then L30170
                gosub set_header_stuff
                gosub print_params
L30170:     if sum$ <> "O" then L30200
            call "SHOSTAT"("Generating Summary Only - Adding Part "&part$)
            goto L30220
L30200:     call"SHOSTAT"("Generating MASTER SCHEDULE for part "& part$)
            gosub print_header   : gosub print_std_part_data
L30220:     gosub tot_forcst     : gosub plan_forcst
            gosub tot_netsales   : gosub pln_netsales
            gosub tot_nnetsales  : gosub pln_nnetsales
            gosub tot_miscadds   : gosub pln_miscadds
            gosub rel_orders     : gosub pip_out
            gosub net_uses
            if sum$ = "O" then end_report
            gosub print_summary_data
            gosub print_first_six
            gosub print_summary_data_2
            gosub print_next_seven
            if stdcost$ = "Y" then gosub print_standard_costs
            if srcrpt$ <> "Y" then end_report
                srcend%, useend% = 0% :  linecnt% = 99%
                init(hex(00))srckey$
                str(srckey$,1%,25%) = part$
                gosub get_uses
L30395:         gosub sources
                if srcend% = 1% and useend% = 1% then end_report
                gosub print_detail
                goto L30395

        REM *************************************************************~
            *   Read files for data and put into appropriate buckets    *~
            *************************************************************
        read_hnyquan
            if hit% = 1% then L30520
            if part$ <> firstpart$ then L30520
            call "READ100" (#2, part$,f1%(2))
            if f1%(2) = 1 then L30560
L30520:     call "PLOWNEXT" (#2,hnyplow$,0%,f1%(2))
            if hnyplow$ > limit$ then L38155
            if f1%(2) = 0 then L38155

L30560:     get #2 using L30660, part$, partdesc$, genref$, sellu$,       ~
                   buyu$, catcode$, mv$, abc$, leadtime$, type$, moq$,   ~
                   buyer1$, buyer2$, sstk, pansiz, priority$

            call "STCCOSTS" (part$, " ", #12, 2%, totcost, costs())

            if genrefr$ = "ALL" then L30700
            if genrefr$ > genref$ then L30520
            if gento$ < genref$ then L30520

L30660:         FMT CH(25), CH(32), CH(16), 2*CH(4), XX(8), CH(4),       ~
                    XX(8),  CH(9),  CH(1),  XX(58), 3*CH(10), CH(3),     ~
                    XX(106), CH(3), XX(6), 2*PD(14,4), CH(1)

L30700:     typedesc$ = " "
            if type$ = "000" then typedesc$ = "Generic"
            if type$ > "001" and type$ < "200" then typedesc$ = "NonPlan"
            if type$ > "199" and type$ < "490" then                      ~
                typedesc$ = "Purchased"
            if type$ > "489" and type$ < "500" then                      ~
                typedesc$ = "Purch. Tool"
            if type$ > "499" and type$ < "790" then                      ~
                typedesc$ = "Manufactured"
            if type$ > "789" and type$ < "800" then                      ~
                typedesc$ = "Manuf. Tool"
            if type$ > "799" and type$ < "1000" then                     ~
                typedesc$ = "Manufactured"
            if catcoder$ = "ALL" then L30870
            if catcode$ < catcoder$ then L30520
            if catcode$ > catto$ then L30520

L30870
*          IF TYPER$ = "ALL" THEN 30910
            if type$ < typer$ then L30520
            if type$ > typeto$ then L30520

*       * Accumulate quantities for part
            hit% = 1%
            toh,too,tbo,tco,tip = 0
            readkey$ = str(part$) & hex(00)
L30950:     call "PLOWNEXT" (#1, readkey$, 25%, f1%(1))
                if f1%(1) = 0 then return
            get #1 using L30980 , oh, bo, oo, co, ip
L30980:       FMT POS(69),PD(14,4), PD(14,4), PD(14,4), PD(14,4), PD(14,4)
            toh = toh + oh : too = too + oo
            tbo = tbo + bo : tco = tco + co : tip = tip + ip
            goto L30950

        tot_forcst /* Summary of total forecast (planned and unplanned) */
            init(hex(00))readkey$
            str (readkey$,1,1) = "4"
L31060:     call "PLOWALTS" (#13,readkey$, 0%, 1%, f1%(13))
                if f1%(13) <> 0 then L31110
            if str(readkey$,1,1) = "5" then return
            init (hex(00)) readkey$:str(readkey$,1,1) = "5"
            goto L31060
L31110:     get #13 using L31120 ,date_req$, parttest$, quantity$
L31120:         FMT XX(3), CH(6), XX(19), CH(25), CH(10)
            convert quantity$ to quantity,data goto L31310
L31140:     if parttest$ <> part$ then L31060
            if date_req$ >= week$(1) then L31190
                priorreq = priorreq + quantity
                pdemtotl = pdemtotl + quantity
                pdemsum  = pdemsum  + quantity
                goto L31060
L31190:     for x% = 1% to 13%
                if date_req$ >= week$(x% + 1%) then L31240
                    weekreq(x%) = weekreq(x%) + quantity
                    demreqt(x%) = demreqt(x%) + quantity
                    demsum(x%) = demsum(x%) + quantity
                    goto L31060
L31240:     next x%

            futurereq = futurereq + quantity
            fdemtotl = fdemtotl + quantity
            fdemsum =  fdemsum + quantity
            goto L31060

L31310:     quantity = 0
            goto L31140

        plan_forcst  /* Summary of forecasts that have been planned */
            init(hex(00))readkey$
            str (readkey$,1,1) = "4"
L31370:     call "PLOWALTS" (#13,readkey$, 0%, 1%, f1%(13))
                if f1%(13) <> 0 then L31420
            if str(readkey$,1,1) = "5" then return
                init (hex(00)) readkey$:str(readkey$,1,1) = "5"
            goto L31370
L31420:     get #13 using L31430 , parttest$, quantity$, date_pcd$
L31430:         FMT XX(28), CH(25), CH(10), XX(19), CH(6)
            convert quantity$ to quantity,data goto L31630
L31450:     if parttest$ <> part$ then L31370
            if date_pcd$ = " " or date_pcd$ = blankdate$ then L31370
            if date_pcd$ >= week$(1) then L31510
                priorreq1 = priorreq1 + quantity
                pplntotl = pplntotl + quantity
                pplnsum  = pplnsum  + quantity
                goto L31370
L31510:     for x% = 1% to 13%
                if date_pcd$ >= week$(x% + 1%) then L31560
                    weekreq1(x%) = weekreq1(x%) + quantity
                    plnreqt(x%) = plnreqt(x%) + quantity
                    plnsum(x%) = plnsum(x%) + quantity
                    goto L31370
L31560:     next x%

            futurereq1 = futurereq1 + quantity
            fplntotl = fplntotl + quantity
            fplnsum =  fplnsum + quantity
            goto L31370

L31630:     quantity = 0
            goto L31450

        tot_netsales  /* Summary of total sales (plnd and unplnd) */
            init(hex(0000))readkey$
            readkey$ = "1"
L31690:     call "PLOWALTS" (#13,readkey$, 0%, 1%, f1%(13))
                if f1%(13) = 0 then return
            get #13 using L31720 , date_req$, parttest$, quantity$
L31720:         FMT XX(3), CH(6), XX(19), CH(25), CH(10)
            convert quantity$ to quantity, data goto L31910
L31740:     if parttest$ <> part$ then L31690
            if date_req$ >= week$(1) then L31800
                priorreq2 = priorreq2 + quantity
                pdemtotl = pdemtotl + quantity
                pdemsum  = pdemsum  + quantity
                goto L31690
L31800:     for x% = 1% to 13%
                if date_req$ >= week$(x% + 1%) then L31850
                    weekreq2(x%) = weekreq2(x%) + quantity
                    demreqt(x%) = demreqt(x%) + quantity
                    demsum(x%) = demsum(x%) + quantity
                    goto L31690
L31850:     next x%
            futurereq2 = futurereq2 + quantity
            fdemtotl = fdemtotl + quantity
            fdemsum =  fdemsum + quantity
            goto L31690

L31910:     quantity = 0
            goto L31740

        pln_netsales   /* Summary of sales that have been planned */
            init(hex(0000))readkey$
            readkey$ = "1"
L31970:     call "PLOWALTS" (#13,readkey$, 0%, 1%, f1%(13))
                if f1%(13) = 0 then return
            get #13 using L32000 , parttest$, quantity$, date_pcd$
L32000:         FMT XX(28), CH(25), CH(10), XX(19), CH(6)
            convert quantity$ to quantity, data goto  L32200
L32020:     if parttest$ <> part$ then L31970
            if date_pcd$ = " " or date_pcd$ = blankdate$ then L31970
            if date_pcd$ >= week$(1) then L32090
                priorreq3 = priorreq3 + quantity
                pplntotl = pplntotl + quantity
                pplnsum  = pplnsum  + quantity
                goto L31970
L32090:     for x% = 1% to 13%
                if date_pcd$ >= week$(x% + 1%) then L32140
                weekreq3(x%) = weekreq3(x%) + quantity
                plnreqt(x%) = plnreqt(x%) + quantity
                plnsum(x%) = plnsum(x%) + quantity
                goto L31970
L32140:     next x%
            futurereq3 = futurereq3 + quantity
            fplntotl = fplntotl + quantity
            fplnsum =  fplnsum + quantity
            goto L31970

L32200:     quantity = 0
            goto L32020

        tot_nnetsales  /* Summary of total nonnettable sales */
            init(hex(0000))readkey$
            readkey$ = "2"
L32260:     call "PLOWALTS" (#13,readkey$, 0%, 1%, f1%(13))
                if f1%(13) = 0 then return
            get #13 using L32290 ,date_req$,parttest$, quantity$
L32290:         FMT XX(3), CH(6), XX(19), CH(25), CH(10)
            convert quantity$ to quantity, data goto L32480
L32310:     if parttest$ <> part$ then L32260
            if date_req$ >= week$(1) then L32370
                priorreq4 = priorreq4 + quantity
                pdemtotl = pdemtotl + quantity
                pdemsum  = pdemsum  + quantity
                goto L32260
L32370:     for x% = 1% to 13%
                if date_req$ >= week$(x% + 1%) then L32420
                weekreq4(x%) = weekreq4(x%) + quantity
                demreqt(x%) = demreqt(x%) + quantity
                demsum(x%) = demsum(x%) + quantity
                goto L32260
L32420:     next x%
            futurereq4 = futurereq4 + quantity
            fdemtotl = fdemtotl + quantity
            fdemsum =  fdemsum + quantity
            goto L32260

L32480:     quantity = 0
            goto L32310

        pln_nnetsales  /* Summary of non_net sales that have been plnd */
            init(hex(0000))readkey$
            readkey$ = "2"
L32540:     call "PLOWALTS" (#13,readkey$, 0%, 1%, f1%(13))
                if f1%(13) = 0 then return
            get #13 using L32570 , parttest$, quantity$, date_pcd$
L32570:         FMT XX(28), CH(25), CH(10), XX(19), CH(6)
            convert quantity$ to quantity, data goto L32770
L32590:     if parttest$ <> part$ then L32540
            if date_pcd$ = " " or date_pcd$ = blankdate$ then L32540
            if date_pcd$ >= week$(1) then L32660
                priorreq5 = priorreq5 + quantity
                pplntotl = pplntotl + quantity
                pplnsum  = pplnsum  + quantity
                goto L32540
L32660:     for x% = 1% to 13%
                if date_pcd$ >= week$(x% + 1%) then L32710
                weekreq5(x%) = weekreq5(x%) + quantity
                plnreqt(x%) = plnreqt(x%) + quantity
                plnsum(x%) = plnsum(x%) + quantity
                goto L32540
L32710:     next x%
            futurereq5 = futurereq5 + quantity
            fplntotl = fplntotl + quantity
            fplnsum =  fplnsum + quantity
            goto L32540

L32770:     quantity = 0
            goto L32590

        tot_miscadds  /* Summary of total misc additions (type 3, 7, 8) */
            init(hex(00))readkey$
            str (readkey$,1,1) = "3"
L32830:     call "PLOWALTS" (#13,readkey$, 0%, 1%, f1%(13))
                if f1%(13) <> 0 then L32880
            if str(readkey$,1,1) = "8" then return
                if str(readkey$,1,1) = "7" then L32860
                     init (hex(00)) readkey$ : str(readkey$,1,1) = "7"
                     goto L32830
L32860:     init (hex(00)) readkey$:str(readkey$,1,1) = "8"
            goto L32830
L32880:     get #13 using L32890 ,date_req$, parttest$, quantity$
L32890:         FMT XX(3), CH(6), XX(19), CH(25), CH(10)
            convert quantity$ to quantity, data goto L33080
L32910:     if parttest$ <> part$ then L32830
            if date_req$ >= week$(1) then L32970
                priorreq6 = priorreq6 + quantity
                pdemtotl = pdemtotl + quantity
                pdemsum  = pdemsum  + quantity
                goto L32830
L32970:     for x% = 1% to 13%
                if date_req$ >= week$(x% + 1%) then L33020
                weekreq6(x%) = weekreq6(x%) + quantity
                demreqt(x%) = demreqt(x%) + quantity
                demsum(x%) = demsum(x%) + quantity
                goto L32830
L33020:     next x%
            futurereq6 = futurereq6 + quantity
            fdemtotl = fdemtotl + quantity
            fdemsum =  fdemsum + quantity
            goto  L32830

L33080:     quantity = 0
            goto L32910

        pln_miscadds  /* Summary of misc addns(type 3 & 8)that are plnd */
            init(hex(00))readkey$
            str (readkey$,1,1) = "3"
L33140:     call "PLOWALTS" (#13,readkey$, 0%, 1%, f1%(13))
                if f1%(13) <> 0 then L33230
            if str(readkey$,1,1) = "8" then return
                if str(readkey$,1,1) = "7" then L33210
                    init (hex(00)) readkey$ : str(readkey$,1,1) = "7"
                    goto L33140
L33210:     init (hex(00)) readkey$:str(readkey$,1,1) = "8"
            goto L33140
L33230:     get #13 using L33240 , parttest$, quantity$, date_pcd$
L33240:         FMT XX(28), CH(25), CH(10), XX(19), CH(6)
            convert quantity$ to quantity, data goto L33440
L33260:     if parttest$ <> part$ then L33140
            if date_pcd$ = " " or date_pcd$ = blankdate$ then L33140
            if date_pcd$ >= week$(1) then L33330
                priorreq7 = priorreq7 + quantity
                pplntotl = pplntotl + quantity
                pplnsum  = pplnsum  + quantity
                goto L33140
L33330:     for x% = 1% to 13%
                if date_pcd$ >= week$(x% + 1%) then L33380
                weekreq7(x%) = weekreq7(x%) + quantity
                plnreqt(x%) = plnreqt(x%) + quantity
                plnsum(x%) = plnsum(x%) + quantity
                goto L33140
L33380:     next x%
            futurereq7 = futurereq7 + quantity
            fplntotl = fplntotl + quantity
            fplnsum =  fplnsum + quantity
            goto L33140

L33440:     quantity = 0
            goto L33260

        pip_out  /* Net uses of the part being reported   */
            str(readkey$,26,) = all(hex(00))
            str(readkey$,1,25)  = part$
L33500:     call "PLOWALTS" (#16,readkey$,1%,25%, f1%(16))
                if f1%(16) = 0 then return
            get #16 using L33530 , subdate%, quantity
L33530:         FMT XX(44), BI(04), XX(08), PD(14,4)
            subdate% = subdate% - 1%
            call "DATE" addr("G+",plbase$,subdate%,date_pcd$,ret%)
            if date_pcd$ >= week$(1) then L33600
                 priornru = priornru + quantity
                 prisumnru = prisumnru + quantity
                 goto L33500
L33600:     for x% = 1% to 13%
                if date_pcd$ >= week$(x% + 1%) then L33640
                weeknru(x%) = weeknru(x%) + quantity
                sumrnru(x%) = sumrnru(x%) + quantity
                goto L33500
L33640:     next x%
            futurenru = futurenru + quantity
            futsumnru = futsumnru + quantity
            goto L33500

            return

        rel_orders  /* Summary of JO's & PO's from PIPIN, on due date */
            init(hex(00))readkey$
            str(readkey$,1,25) = part$
L33740:     call "PLOWALTS" (#7, readkey$, 1%, 25%, f1%(7))
                if f1%(7) = 0 then return
            get #7 using L33770 , datein%, tag$, pipinqty
L33770:         FMT XX(25), BI(4), CH(19), PD(14,4)
            if str(tag$,1%,2%) = "PO" then L33840
            if str(tag$,1%,2%) = "QC" then L33840
            if str(tag$,1%,2%) = "JO" then L34050
            if str(tag$,1%,2%) = "BO" then L34260
            if str(tag$,1%,2%) = "RO" then L34260
            if str(tag$,1%,2%) = "WO" then L34470
            if str(tag$,1%,2%) = "BW" then L34470
            if str(tag$,1%,2%) = "RW" then L34470
            goto L33740

L33840
*       * Released purchases with PO or QC markers
            if datein% >= week%(1) then L33910
                priornrp = priornrp + pipinqty
                priornra = priornra + pipinqty
                prisumnrp = prisumnrp + pipinqty
                prisumnra = prisumnra + pipinqty
                goto L33740
L33910:     for x% = 1% to 13%
                if datein% >= week%(x% + 1%) then L33970
                weeknrp(x%) = weeknrp(x%) + pipinqty
                weeknra(x%) = weeknra(x%) + pipinqty
                sumrnrp(x%) = sumrnrp(x%) + pipinqty
                sumrnra(x%) = sumrnra(x%) + pipinqty
                goto L33740
L33970:     next x%
            futurenrp = futurenrp + pipinqty
            futurenra = futurenra + pipinqty
            futsumnrp = futsumnrp + pipinqty
            futsumnra = futsumnra + pipinqty
            goto L33740


L34050
*       * Release jobs with JO markers
            if datein% >= week%(1) then L34120
                priornrj = priornrj + pipinqty
                priornra = priornra + pipinqty
                prisumnrj = prisumnrj + pipinqty
                prisumnra = prisumnra + pipinqty
                goto L33740
L34120:     for x% = 1% to 13%
                if datein% >= week%(x% + 1%) then L34180
                weeknrj(x%) = weeknrj(x%) + pipinqty
                weeknra(x%) = weeknra(x%) + pipinqty
                sumrnrj(x%) = sumrnrj(x%) + pipinqty
                sumrnra(x%) = sumrnra(x%) + pipinqty
                goto L33740
L34180:    next x%
           futurenrj =  futurenrj + pipinqty
           futurenra =  futurenra + pipinqty
           futsumnrj =  futsumnrj + pipinqty
           futsumnra =  futsumnra + pipinqty
           goto  L33740


L34260
*       * Unreleased plans with BO or RO tags
            if datein% >= week%(1) then L34330
                priornrb = priornrb + pipinqty
                priornra = priornra + pipinqty
                prisumnrb = prisumnrb + pipinqty
                prisumnra = prisumnra + pipinqty
                goto L33740
L34330:     for x% = 1% to 13%
                if datein% >= week%(x% + 1%) then L34390
                weeknrb(x%) = weeknrb(x%) + pipinqty
                weeknra(x%) = weeknra(x%) + pipinqty
                sumrnrb(x%) = sumrnrb(x%) + pipinqty
                sumrnra(x%) = sumrnra(x%) + pipinqty
                goto L33740
L34390:     next x%
            futurenrb = futurenrb + pipinqty
            futurenra = futurenra + pipinqty
            futsumnrb = futsumnrb + pipinqty
            futsumnra = futsumnra + pipinqty
            goto L33740


L34470
*       * Unreleased plans with WO, BW, or RW tags
            if datein% >= week%(1) then L34540
                priornrw = priornrw + pipinqty
                priornra = priornra + pipinqty
                prisumnra = prisumnra + pipinqty
                prisumnrw = prisumnrw + pipinqty
                goto L33740
L34540:     for x% = 1% to 13%
                if datein% >= week%(x% + 1%) then L34600
                weeknrw(x%) = weeknrw(x%) + pipinqty
                weeknra(x%) = weeknra(x%) + pipinqty
                sumrnrw(x%) = sumrnrw(x%) + pipinqty
                sumrnra(x%) = sumrnra(x%) + pipinqty
                goto L33740
L34600:     next x%
            futurenrw = futurenrw + pipinqty
            futurenra = futurenra + pipinqty
            futsumnrw = futsumnrw + pipinqty
            futsumnra = futsumnra + pipinqty
            goto L33740


        net_uses
            init(hex(0000))readkey$
            str(readkey$,1,25)  = part$
            quantity = 0%

*       * First add all prior inventory transactions to prior bucket
L34735:     call "PLOWNEXT" (#17,readkey$,25%, f1%(17))
                if f1%(17) = 0 then L34860
            get #17 using L34750 , date_pcd$, quantity
L34750:         FMT XX(42), CH(06), XX(02), PD(14,4)
            if date_pcd$ >= week$(1) then L34775
                 priornra = priornra + quantity
                 prisumnra = prisumnra + quantity
                 goto L34735
L34775:     for x% = 1% to 13%
                if date_pcd$ < week$(x%) or                              ~
                   date_pcd$ >= week$(x% + 1%) then L34800
                weeknra(x%) = weeknra(x%) + quantity
                sumrnra(x%) = sumrnra(x%) + quantity
                goto L34735
L34800:     next x%
            if date_pcd$ < week$(14) then L34735
                futurenra = futurenra + quantity
                futsumnra = futsumnra + quantity
            goto L34735


L34860
*       * Now calculate the net usage and std cost of the additions
              priornra = priornra + toh
              prisumnra = prisumnra + toh
              priornrn = priornra - priornru
              prisumnrn = prisumnra - prisumnru
              weeknrn(1%) = (weeknra(1%) + priornrn) - weeknru(1%)
              sumrnrn(1%) = (sumrnra(1%) + prisumnrn) - sumrnru(1%)

              for i% = 1% to 12%
                  weekstds(1%,i%) = costs(i%) * weeknra(1%)
              next i%
              weekstds(1%,13%) = totcost * weeknra(1%)

        for x% = 2% to 13%
            weeknrn(x%) = (weeknra(x%) + weeknrn(x%-1%) - weeknru(x%))
            sumrnrn(x%) = (sumrnra(x%) + sumrnrn(x%-1%) - sumrnru(x%))
            for i% = 1% to 12%
                weekstds(x%,i%) = costs(i%) * weeknra(x%)
            next i%
            weekstds(x%,13%) = totcost * weeknra(x%)
        next x%

              futurenrn = (futurenra + weeknrn(13%)) - futurenru
              futsumnrn = (futsumnra + sumrnrn(13%)) - futsumnru
              return

        sources  /* PIPIN detail for part selected */
L35170:     init (" ") srctag$, remord$, complqty$
            datein%, remord, datest%, complqty = 0
            call "PLOWALTS" (#7, srckey$, 1%, 25%, f1%(7))
            if f1%(7) = 0 then srcend% = 1%
            if f1%(7) = 0 then return
            get #7 using L35230 , datein%, srctag$, remord, datest%
L35230:         FMT XX(25), BI(4), CH(19), PD(14,4), BI(4)
            if str(srctag$,1%,2%) = "JO" then goto  read_jbmastr2
            if str(srctag$,1%,2%) = "PO" then goto  read_vbk
            if str(srctag$,1%,2%) = "QC" then goto  read_vbk
            if str(srctag$,1%,2%) = "BO" or                              ~
               str(srctag$,1%,2%) = "RO" or                              ~
               str(srctag$,1%,2%) = "BW" or                              ~
               str(srctag$,1%,2%) = "RW" or                              ~
               str(srctag$,1%,2%) = "WO" then L35290
                    goto L35170
L35290:     return

        get_uses  /* DEMASTR detail for part selected  */
             if test% = 0 then L35350
                 call "PLOWNEXT" (#13,diskkey$,0%,f1%(13))
                 goto L35360
L35350:      call "READNEXT" (#13,f1%(13))
L35360:      if f1%(13) = 0 then return
             get #13 using L35400, status$, type$, date_req$, demcode$,   ~
                           demline$, parttest$, quantity$, date_pcd$
             if quantity$ = " " then quantity$ = "0"
L35400:          FMT CH(1), CH(1), XX(1), CH(6), CH(16), CH(3), CH(25),  ~
                     CH(10), XX(19), CH(6)
             if parttest$ <> part$ then L35350
             s$ = " "
             if status$ < "2" then s$ = "U"
             if status$ > "2" then s$ = "P"
             test% = 0
             write #14 using L35490 ,s$,date_pcd$,date_req$,cnt%,type$,   ~
             demcode$, demline$,quantity$
L35490:      FMT CH(1), CH(6), CH(6), BI(4), CH(1), CH(16), CH(3), CH(10)
             cnt% = cnt% + 1%
             goto L35350

        read_jbmastr2  /* Obtain quantity complete for this job order */
            job$ = str(srctag$,12,8)
            call "READ100" (#9, job$, f1%(9))
            get #9 using L35570 , complqty
L35570:         FMT XX(90), PD(14,4)
            return

        read_vbk  /* Determine quantity complete for this P.O. */
            po$ = str(srctag$,3,14) & " "

            call "REDALT0" (#10, po$, 1%, f1%(10))
            get #10 using L35650 , vend$
L35650:         FMT CH(9)
            readkey$ = str(vend$,1,9) & str(po$,1,16) & hex(00)
            call "PLOWNEXT" (#11, readkey$, 25%, f1%(1))
            get #11 using L35690 , ordqty
L35690:         FMT XX(92), PD(14,4)
            complqty = ordqty - remord
            return

*       *****************************************************************
*                           REPORT PRINT ROUTINES                       *
*       *****************************************************************

        print_std_part_data
            convert moq$ to moq, data goto L36060
L36030:     print using L53270 ,str(type$,1,3),moq,sstk,toh
            print using L53290 ,    catcode$,pansiz,too,tbo
            print using L53310 ,buyer1$,buyer2$,tco,tip
            print skip(2)
            linecnt% = linecnt% + 5%
            return
L36060:     keyhit% = 0
            call "ASKUSER" (keyhit%,"DATA ERROR", "There Has Been A Data ~
        ~Conversion Error","Data Is An MOQ In HNYMASTR",       "Press ENTE~
        ~R To Continue OR PF16 To Abort")
            if keyhit% = 16 then L10000
            moq = 0
            if keyhit% = 0 then goto L36030
            goto L36060

        print_summary_data
            for x% = 1% to 7%
                rptweek$(x%) = week$(x%)
                call "DATEFMT"(rptweek$(x%))
            next x%
            print using L54530
            if summary% = 1% then L36160
            print using L53950 , rptweek$(1),rptweek$(2),rptweek$(3),     ~
                  rptweek$(4),rptweek$(5),rptweek$(6)
            print using L53990
            return
L36160:     print using L53430 , rptweek$(1),rptweek$(2),rptweek$(3),     ~
                  rptweek$(4),rptweek$(5),rptweek$(6)
            print using L53990
            return

        print_first_six
            print using L54020
            print using L54050
            print using L54090 ,"FORECAST",    priorreq,weekreq(1),       ~
                  weekreq(2),weekreq(3),weekreq(4),weekreq(5),weekreq(6),~
                  date$

            print using L54120 ,"NET SALE",  priorreq2,weekreq2(1),       ~
                  weekreq2(2),weekreq2(3),weekreq2(4),weekreq2(5),       ~
                  weekreq2(6)

            print using L54150 ,"NON-NET SALE",  priorreq4,weekreq4(1),   ~
                  weekreq4(2), weekreq4(3),weekreq4(4),weekreq4(5),      ~
                  weekreq4(6)

            print using L54190 ,"REQUISITIONS", priorreq6,weekreq6(1),    ~
                  weekreq6(2),weekreq6(3),weekreq6(4),weekreq6(5),       ~
                  weekreq6(6)

            print using L54220
            print using L54250 ,"TOTAL DEMANDS",pdemtotl, demreqt(1),     ~
                   demreqt(2), demreqt(3), demreqt(4), demreqt(5),       ~
                   demreqt(6)

            print using L54290
            print using L54320
            print using L54350
            print using L54380 ,"FORECAST    ", priorreq1,weekreq1(1),    ~
                  weekreq1(2),weekreq1(3),weekreq1(4),weekreq1(5),       ~
                  weekreq1(6)

            print using L54410 ,"NET SALES ",priorreq3, weekreq3(1),      ~
                  weekreq3(2), weekreq3(3),weekreq3(4),weekreq3(5),      ~
                  weekreq3(6)

            print using L54440 ,"NON-NET SALE", priorreq5,weekreq5(1),    ~
                  weekreq5(2),weekreq5(3),weekreq5(4),weekreq5(5),       ~
                  weekreq5(6)

            print using L54470 ,"REQUISITIONS", priorreq7,weekreq7(1),    ~
                  weekreq7(2),weekreq7(3),weekreq7(4),weekreq7(5),       ~
                  weekreq7(6)
            print using L54500
            print using L53370 ,"TOTAL PLANS  ",pplntotl, plnreqt(1),     ~
                   plnreqt(2), plnreqt(3), plnreqt(4), plnreqt(5),       ~
                   plnreqt(6)

            print using L54530
            print using L53710
            print using L54530
            print using L53370 ,"BUY ADVICES",priornrb,weeknrb(1),        ~
                  weeknrb(2),weeknrb(3),weeknrb(4),weeknrb(5),weeknrb(6)

            print using L53370 ,"BLD. ADVICES",priornrw,weeknrw(1),       ~
                  weeknrw(2),weeknrw(3),weeknrw(4),weeknrw(5),weeknrw(6)

            print using L54530
            print using L53370 ,"OPEN JOBS ",priornrj,weeknrj(1),         ~
                  weeknrj(2),weeknrj(3),weeknrj(4),weeknrj(5),weeknrj(6)

            print using L53370 ,"OPEN PURCH.",priornrp,weeknrp(1),        ~
                  weeknrp(2),weeknrp(3),weeknrp(4),weeknrp(5),weeknrp(6)

            print using L54530
            print using L53370 ,"ADDS(note 1)",priornra,weeknra(1),       ~
                  weeknra(2),weeknra(3),weeknra(4),weeknra(5),weeknra(6)

            print using L53370 ,"USES(note 2)",    priornru,weeknru(1),   ~
                  weeknru(2),weeknru(3),weeknru(4),weeknru(5),weeknru(6)

            print using L54530
            print using L53370 ,"NET W/CARRY",priornrn,weeknrn(1),        ~
                  weeknrn(2),weeknrn(3),weeknrn(4),weeknrn(5),weeknrn(6)

            print using L54530
            linecnt% = 99%
            return

        print_detail
            if linecnt% > 55% then gosub print_header else L36590
               gosub print_detail_header
L36590
*       ** Convert dates to gregorian format
            if datest% = 0 then rlsdate$ = " "
            if datest% = 0 then L36615
               rlsdate$ = yymmdd$(datest%)
               call "DATEFMT"(rlsdate$)
L36615:     if datein% = 0 then duedate$ = " "
            if datein% = 0 then L36635
               duedate$ = yymmdd$(datein%)
               call "DATEFMT"(duedate$)
L36635:     convert remord to remord$, pic(#########)
            convert complqty to complqty$, pic(#########)
            if srctag$ = " " then remord$, complqty$ = " "

            init(" ") s$, type$,demcode$,demline$,date_req$,date_pcd$
            quantity$ = "0"
            call "PLOWNEXT" (#14,plowkey$,0%,f1%(14))
            if f1%(14) = 0 and srcend% = 1 then  L36740
            if f1%(14) = 0 then goto L36728
            get #14 using L36745 ,s$,date_pcd$,date_req$,cnt%,type$,      ~
                  demcode$,demline$,quantity$
            convert quantity$ to quantity, data goto L36755
L36695:     if date_pcd$ = " " then L36705
            call "DATEFMT"(date_pcd$)
L36705:     if date_req$ = " " then L36720
            call "DATEFMT"(date_req$)
            if s$ = "U" then date_pcd$ = "UNPLANNED"
L36720:     print using L53920 , srctag$,rlsdate$,duedate$,remord$,       ~
                  complqty$,type$,demcode$,demline$,date_req$,           ~
                  date_pcd$,quantity
            goto L36735
L36728:     print using L53920, srctag$, rlsdate$, duedate$, remord$,     ~
                  complqty$, " ", " ", " ", " ", " ", " "
L36735:     linecnt% = linecnt% + 1%
L36740:     if f1%(14) = 0 then useend% = 1
L36745:     FMT CH(1), CH(6), CH(6), BI(4), CH(1), CH(16), CH(3), CH(10)
            return
L36755:     keyhit% = 0
            call "ASKUSER" (keyhit%,"DATA ERROR", "There Has Been A Data ~
        ~Conversion Error","Data Is A Quantity From WORKFILE", "Press ENTE~
        ~R To Continue OR PF16 To Abort")
            if keyhit% = 16 then L10000
            quantity = 0
            if keyhit% = 0 then goto L36695
            goto L36755

*       *****************************************************************
*                           REPORT PRINT ROUTINES  PAGE 2               *
*       *****************************************************************

        print_summary_data_2
            if summary% = 1% then L36835
            gosub print_header
L36835:     for x% = 7% to 13%
                rptweek$(x%) = week$(x%)
                call "DATEFMT"(rptweek$(x%))
            next x%
            print using L54560
            print using L53490 ,  rptweek$(7),                            ~
                  rptweek$(8),rptweek$(9),rptweek$(10), rptweek$(11),    ~
                  rptweek$(12), rptweek$(13)
            print using L53550
            return

        print_next_seven
            print using L53670
            print using L54560
            print using L53520 ,"FORECAST",                               ~
                  weekreq(7),weekreq(8),weekreq(9),weekreq(10),          ~
                  weekreq(11),weekreq(12),weekreq(13),futurereq
            print using L53520 ,"NET SALE",                               ~
                              weekreq2(7),weekreq2(8),weekreq2(9),       ~
                  weekreq2(10),weekreq2(11),weekreq2(12), weekreq2(13),  ~
                  futurereq2
            print using L53520 ,"NON-NET SALE",                           ~
                               weekreq4(7),weekreq4(8),weekreq4(9),      ~
                  weekreq4(10),weekreq4(11),weekreq4(12),weekreq4(13),   ~
                  futurereq4
            print using L53520 ,"REQUISITIONS",                           ~
                              weekreq6(7),weekreq6(8),weekreq6(9),       ~
                  weekreq6(10),weekreq6(11),weekreq6(12),weekreq6(13),   ~
                  futurereq6
            print using L53550
            print using L53520 ,"TOTAL DEMANDS",                          ~
                               demreqt(7), demreqt(8), demreqt(9),       ~
                   demreqt(10), demreqt(11), demreqt(12), demreqt(13),   ~
                   fdemtotl
            print using L54560
            print using L53690
            print using L54560
            print using L53520 ,"FORECAST    ",                           ~
                              weekreq1(7),weekreq1(8),weekreq1(9),       ~
                  weekreq1(10),weekreq1(11),weekreq1(12),weekreq1(13),   ~
                  futurereq1
            print using L53520 ,"NET SALES ",                             ~
                               weekreq3(7),weekreq3(8),weekreq3(9),      ~
                  weekreq3(10), weekreq3(11),weekreq3(12),weekreq3(13),  ~
                  futurereq3
            print using L53520 ,"NON-NET SALE",                           ~
                              weekreq5(7),weekreq5(8),weekreq5(9),       ~
                  weekreq5(10),weekreq5(11),weekreq5(12),weekreq5(13),   ~
                  futurereq5
            print using L53520 ,"REQUISITIONS",                           ~
                              weekreq7(7),weekreq7(8),weekreq7(9),       ~
                  weekreq7(10),weekreq7(11),weekreq7(12),weekreq7(13),   ~
                  futurereq7
            print using L53550
            print using L53520 ,"TOTAL PLANS  ",                          ~
                               plnreqt(7), plnreqt(8), plnreqt(9),       ~
                   plnreqt(10), plnreqt(11), plnreqt(12), plnreqt(13),   ~
                   fplntotl
            print using L54560
            print using L53710
            print using L54560
            print using L53520 ,"BUY ADVICES",                            ~
                  weeknrb(7),weeknrb(8),weeknrb(9),weeknrb(10),          ~
                  weeknrb(11),weeknrb(12),weeknrb(13),futurenrb
            print using L53520 ,"BLD. ADVICES",                           ~
                  weeknrw(7),weeknrw(8),weeknrw(9),weeknrw(10),          ~
                  weeknrw(11),weeknrw(12),weeknrw(13),futurenrw
            print using L54560
            print using L53520 ,"OPEN JOBS ",                             ~
                  weeknrj(7),weeknrj(8),weeknrj(9),weeknrj(10),          ~
                  weeknrj(11),weeknrj(12),weeknrj(13),futurenrj
            print using L53520 ,"OPEN PURCH.",                            ~
                  weeknrp(7),weeknrp(8),weeknrp(9),weeknrp(10),          ~
                  weeknrp(11),weeknrp(12),weeknrp(13),futurenrp
            print using L54560
            print using L53520 ,"ADDS(note 1)",                           ~
                  weeknra(7),weeknra(8),weeknra(9),weeknra(10),          ~
                  weeknra(11),weeknra(12),weeknra(13),futurenra
            print using L53520 ,"USES(note 2)",                           ~
                  weeknru(7),weeknru(8),weeknru(9),weeknru(10),          ~
                  weeknru(11),weeknru(12),weeknru(13),futurenru
            print using L53550
            print using L53520 ,"NET W/CARRY",                            ~
                  weeknrn(7),weeknrn(8),weeknrn(9),weeknrn(10),          ~
                  weeknrn(11),weeknrn(12),weeknrn(13),futurenrn
            print using L54560
            linecnt% = 99%
            return

        print_standard_costs
            gosub print_header
            print skip(2)
            print using L54620, set$, setdescr$
            print skip(2)
            print using L54590, rptweek$(1), rptweek$(2), rptweek$(3),    ~
                               rptweek$(4), rptweek$(5), rptweek$(6)
            print using L53340
            for i% = 1% to buckets%
                print using L53400, bucketdescr$(i%), costs(i%),          ~
                      round(weekstds(1,i%),0), round(weekstds(2,i%),0),  ~
                      round(weekstds(3,i%),0), round(weekstds(4,i%),0),  ~
                      round(weekstds(5,i%),0), round(weekstds(6,i%),0)
            next i%

            print using L53460
            print using L53400, "TOTAL COST", totcost,                    ~
                        round(weekstds(1,13),0), round(weekstds(2,13),0),~
                        round(weekstds(3,13),0), round(weekstds(4,13),0),~
                        round(weekstds(5,13),0), round(weekstds(6,13),0)

            print using L53340

*       * Now next seven
            print skip(5)
            print using L54590, rptweek$(7), rptweek$(8), rptweek$(9),    ~
                               rptweek$(10), rptweek$(11), rptweek$(12)
            print using L53340
            for i% = 1% to buckets%
                print using L53400, bucketdescr$(i%), costs(i%),          ~
                      round(weekstds(7,i%),0), round(weekstds(8,i%),0),  ~
                      round(weekstds(9,i%),0), round(weekstds(10,i%),0), ~
                      round(weekstds(11,i%),0), round(weekstds(12,i%),0)
            next i%

            print using L53460
            print using L53400, "TOTAL COST", totcost,                    ~
                       round(weekstds(7,13),0), round(weekstds(8,13),0), ~
                       round(weekstds(9,13),0), round(weekstds(10,13),0),~
                       round(weekstds(11,13),0), round(weekstds(12,13),0)

            print using L53340
            return

        print_run_summary
            gosub print_header
            print using L53710
            gosub print_summary_data
            print using L54530
            print using L53370 ,"BUY ADVICES",prisumnrb,sumrnrb(1),       ~
                  sumrnrb(2),sumrnrb(3),sumrnrb(4),sumrnrb(5),sumrnrb(6)

            print using L53370 ,"BLD. ADVICES",prisumnrw,sumrnrw(1),      ~
                  sumrnrw(2),sumrnrw(3),sumrnrw(4),sumrnrw(5),sumrnrw(6)

            print using L54530
            print using L53370 ,"OPEN JOBS ",prisumnrj,sumrnrj(1),        ~
                  sumrnrj(2),sumrnrj(3),sumrnrj(4),sumrnrj(5),sumrnrj(6)

            print using L53370 ,"OPEN PURCH.",prisumnrp,sumrnrp(1),       ~
                  sumrnrp(2),sumrnrp(3),sumrnrp(4),sumrnrp(5),sumrnrp(6)

            print using L54530
            print using L53370 ,"ADDS(note 1)",prisumnra,sumrnra(1),      ~
                  sumrnra(2),sumrnra(3),sumrnra(4),sumrnra(5),sumrnra(6)

            print using L53370 ,"USES(note 2)",    prisumnru,sumrnru(1),  ~
                  sumrnru(2),sumrnru(3),sumrnru(4),sumrnru(5),sumrnru(6)

            print using L54530
            print using L53370 ,"NET W/CARRY",prisumnrn,sumrnrn(1),       ~
                  sumrnrn(2),sumrnrn(3),sumrnrn(4),sumrnrn(5),sumrnrn(6)

            print using L54530
            print skip(3)
            print using L53710
            gosub print_summary_data_2
            print using L54560
            print using L53520 ,"BUY ADVICES",                            ~
                  sumrnrb(7),sumrnrb(8),sumrnrb(9),sumrnrb(10),          ~
                  sumrnrb(11),sumrnrb(12),sumrnrb(13),futsumnrb
            print using L53520 ,"BLD. ADVICES",                           ~
                  sumrnrw(7),sumrnrw(8),sumrnrw(9),sumrnrw(10),          ~
                  sumrnrw(11),sumrnrw(12),sumrnrw(13),futsumnrw
            print using L54560
            print using L53520 ,"OPEN JOBS ",                             ~
                  sumrnrj(7),sumrnrj(8),sumrnrj(9),sumrnrj(10),          ~
                  sumrnrj(11),sumrnrj(12),sumrnrj(13),futsumnrj
            print using L53520 ,"OPEN PURCH.",                            ~
                  sumrnrp(7),sumrnrp(8),sumrnrp(9),sumrnrp(10),          ~
                  sumrnrp(11),sumrnrp(12),sumrnrp(13),futsumnrp
            print using L54560
            print using L53520 ,"ADDS(note 1)",                           ~
                  sumrnra(7),sumrnra(8),sumrnra(9),sumrnra(10),          ~
                  sumrnra(11),sumrnra(12),sumrnra(13),futsumnra
            print using L53520 ,"USES(note 2)",                           ~
                  sumrnru(7),sumrnru(8),sumrnru(9),sumrnru(10),          ~
                  sumrnru(11),sumrnru(12),sumrnru(13),futsumnru
            print using L54560
            print using L53520 ,"NET W/CARRY",                            ~
                  sumrnrn(7),sumrnrn(8),sumrnrn(9),sumrnrn(10),          ~
                  sumrnrn(11),sumrnrn(12),sumrnrn(13),futsumnrn
            print using L54560
            return

*       * Print selection information from screen
        print_params
            if linecnt% > 5% then print page
            print using L53100 , rpthead$
            print using L53120 , rundate$,str(runtime$,1,2),              ~
                               str(runtime$,3,2),ampm$,   0%
L37880:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then L37888
                str(i$(), i%, 1%) = hex(20)
                goto L37880
L37888:     print skip(3)
            print tab(37);
            print "--------------------- Report Selection Parameters ----~
        ~---------"
            for x% = 5% to 14%: print tab(37); i$(x%) : next x%
            print tab(37);
            print "------------------------------------------------------~
        ~---------"
            screend% = 1%
            pageno% = pageno% + 1%
            return

        print_header

            print page
            print using L53100 , rpthead$
            print using L53120 , rundate$,str(runtime$,1,2),              ~
                               str(runtime$,3,2),ampm$,   pageno%
            print using L53140 , freq$
            print using L53160 , dateline$

            print skip(1)
            if summary% = 1% then L38030
            print using L53180 , part$, partdesc$
            goto L38045
L38030:     print using L53210, date$,firstpart$, lastpart$
            print using L53230, genrefr$,gento$
            print using L53250, catcoder$,catto$,typer$,typeto$
L38045:     print skip(1)
            linecnt% = 7%
            pageno% = pageno% + 1%

            return

        print_detail_header
            if linecnt% > 55% then gosub print_header
            print using L53810
            print using L53880
            print using L53850
            print using L53880
            linecnt% = linecnt% + 4%
            return

        end_report
            print using L53880
            print skip (2)
            if sum$ = "O" then L38155
            print using L38140 ,part$
L38140: %   ***** END OF REPORT FOR: #########################
            gosub clear_variables

L38155
*       * Close workfile and get ready to quit
            call "FILEBGON" (#14)
            openflag = 0
            if hit% <> 0 then L38210
L38175:     call "ASKUSER" (keyhit%,"NO REPORT", "There Were No Eligible ~
        ~Parts In The Range Specified", "Press RETURN To Acknowledge"," ")
            gosub L52010
            call "FILEBGON" (#14)
            openflag = 0
            if keyhit% = 0 then L11000
            goto L38175
L38210:     if f1%(2) = 0 then L38220
            if part$ < limit$ then L30000
L38220:     if sum$ = "N" then L38235
            summary% = 1%
            gosub print_run_summary
L38235:     printerflag = 0
            time$ = " "  :  call "TIME" (time$)
            print skip(2)
            print using L54650, time$       /* End of report line */
            close printer
            call "SETPRNT" ("PLN003", " ", 0%, 1%)
            run% = 1%
            goto inputmode

        set_header_stuff
            runtime$ = time
            if str(runtime$,1,2)< "12" then ampm$ ="AM" else ampm$ = "PM"
            rundate$ = date
            call "DATEFMT"(rundate$)
            return

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  init(hex(84)) lfac$()
                  str(line2$,,60) = "Input report selections"
                  on fieldnr% gosub                                      ~
                                   L40230 ,         /* MONTH / WEEK     */~
                                   L40230 ,         /* START DATE       */~
                                   L40230 ,         /* PART             */~
                                   L40230 ,         /* GENERIC REF.     */~
                                   L40230 ,         /* CATEGORY         */~
                                   L40230 ,         /* TYPE             */~
                                   L40230 ,         /* SRC & USES       */~
                                   L40230 ,         /* STD COST         */~
                                   L40230           /* SUMMARY REPT.    */
                     goto L40300

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40230:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40300: accept                                                           ~
               at (01,02),                                               ~
        "Print Master Production Schedule",                              ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), "Weeks/Months:",                              ~
               at (06,18), fac(lfac$(1)), monweek$              , ch(01),~
               at (06,50), fac(hex(8c)),freq$                   , ch(25),~
               at (07,02), "Starting Date:",                             ~
               at (07,18), fac(lfac$(2)), scrdate$              , ch(08),~
               at (07,50), fac(hex(8c)), dateline$              , ch(30),~
               at (08,02), "Part Range",                                 ~
               at (08,18), fac(lfac$(3)), firstpart$            , ch(25),~
               at (08,50), fac(lfac$(3)), lastpart$             , ch(25),~
               at (09,02), "Generic Ref:",                               ~
               at (09,18), fac(lfac$(4)), genrefr$              , ch(16),~
               at (09,50), fac(lfac$(4)), gento$                , ch(16),~
               at (10,02), "Category:",                                  ~
               at (10,18), fac(lfac$(5)), catcoder$             , ch(04),~
               at (10,50), fac(lfac$(5)), catto$                , ch(04),~
               at (11,02), "Part Type : ",                               ~
               at (11,18), fac(lfac$(6)), typer$                , ch(03),~
               at (11,50), fac(lfac$(6)), typeto$               , ch(03),~
               at (12,02), "Incl. Detail?",                              ~
               at (12,18), fac(lfac$(7)), srcrpt$               , ch(01),~
               at (13,02), "Incl. Std Cost?",                            ~
               at (13,18), fac(lfac$(8)), stdcost$              , ch(01),~
               at (14,02), "Summary Rpt?",                               ~
               at (14,18), fac(lfac$(9)), sum$                  , ch(01),~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (22,21),                                               ~
                  fac(hex(8c)),pf4$,                                     ~
               at (23,21),                                               ~
                  fac(hex(84)),pf5$,                                     ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  fac(hex(8c)), pf16$,                            ch(16),~
                                                                         ~
               keys(hex(000104050d0f10)),                                ~
               key (keyhit%)

               if keyhit% <> 13 then L40840
                  call "MANUAL" ("PLNMSRPT")
                  goto L40300

L40840:        if keyhit% <> 15 then L40880
                  call "PRNTSCRN"
                  goto L40300

L40880:        close ws
               call "SCREEN" addr("C",u3%,"I",i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub                                      ~
                                   L50190 ,         /* MONTH / WEEK     */~
                                   L50430 ,         /* START DATE       */~
                                   L52010 ,         /* PART NUMBER      */~
                                   L52270 ,         /* GENERIC REF.     */~
                                   L52510 ,         /* CATEGORY         */~
                                   L52740 ,         /* PART TYPE        */~
                                   L52890 ,         /* SRC & USES       */~
                                   L52940 ,         /* STD COST         */~
                                   L52990           /* SUMMARY REPORT   */
                     return

L50190:     REM TEST FOR MONTH OR WEEK INPUT
                if monweek$ = "W" or monweek$ = "C"                      ~
                                  or monweek$ = "F" then L50250
                    errormsg$ = "'W' , 'C' and 'F'  Are The Only " &     ~
                                "Acceptable entries"
                    return
L50250:         if monweek$= "W" then freq$ = "     Report By Week      "
                if monweek$= "C" then freq$ = "Report By Calendar Months"
                if monweek$= "F" then freq$ = " Report By Fiscal Months "

        REM *** Obtain Planning Base Date
            call "READ100" (#12, "MONTHS OPEN", f1%(12))
                if f1%(12) = 0 then errormsg$ = "Months Open Error"
                if f1%(12) = 0 then return
            get #12 using L50340 , plbase$
L50340:         FMT XX(32), CH(6)
            call "DATE" addr("G+", plbase$, days%, highstart$, ret%)
            call "DAY"  addr(highstart$, days%)
            if days% = 1% then days% = 8%
            days% = 2% - days%
            call "DATE" addr("G+", highstart$, days%, highstart$, ret%)
            if editmode% = 1% then L50430
               return

L50430: REM Test Data For Starting Date
            fieldnr% = 2%
            if scrdate$ <> " " and ~
               scrdate$ <> blankdate$ then L50480
               if highstart$ < date then scrdate$ = highstart$           ~
                                    else scrdate$ = date$
L50480:     call "DATEOK" (scrdate$,date%,error$)
            date% = date%
            if error$ <> " " then errormsg$ = "Invalid Date Entry, " &   ~
                                              "Please Re-Enter"
            if errormsg$ <> " " then return

            scrdate1$ = scrdate$
            call "DATUNFMT"(scrdate1$)
            seldate$ = scrdate1$

        REM Determine 1St Monday Of Week Selected, Load 12 Subsequent    ~
                Mondays, Load Production Calendar

*       ** 1st Obtain Planning Base Date
            call "READ100" (#12, "MONTHS OPEN", f1%(12))
                if f1%(12) = 0 then errormsg$ = "Months Open Error"
                if f1%(12) = 0 then return
            get #12 using L50660 , plbase$
L50660:         FMT XX(32), CH(6)
                if seldate$ >= plbase$ then L50730
                msgdate$  = plbase$
                call "DATEFMT" (msgdate$)
                errormsg$ = "Starting Date Must Be After " & msgdate$
                return

L50730:     call "DAY" addr(seldate$,wkday%)

            for x% = 1% to 7%
                if x% = wkday% and x% > 1% then days% = -(wkday% - 2%)
                if x% = wkday% and x% = 1% then days% = -6%
            next x%

        REM Determine Date Of Monday Of Week Selected
            call "DATE" addr("G+",seldate$,days%,start$,ret%)
            if ret% <> 0 then errormsg$ = "INVALID DATE, Please Re-Enter"
            if ret% <> 0 then return
            if plbase$ <= start$ then L50930
            msgdate$ = start$
            call "DATE" addr("G+",msgdate$,7%,scrdate$,ret%)
            call "DATEFMT" (msgdate$)
            call "DATEFMT" (scrdate$)
            errormsg$ = "Previous Monday (" & msgdate$ & ") Is Before Pla~
        ~nning Calendar Start Date - Use " & scrdate$
            return

L50930: REM Now set up the bucketed dates
            call "DATE" addr("G-",plbase$,start$,subscr%,err%)
            subscr% = subscr% + 1%  : err% = err%
            gosub load_plan_cal
            week$(1%) = yymmdd$(subscr%)
            if monweek$ = "C" then gosub load_c_mon   /* C Months */
            if monweek$ = "F" then gosub load_f_mon   /* F Months */
            if monweek$ = "W" then gosub load_weeks   /* Weeks    */
            for i% = 1% to 14%
                call "PIPINDEX"(#12, week$(i%), week%(i%), err%)
                if err% = 0% then L51015
                for j% = i% to 14%
                    if monweek$ = "W" then                               ~
                       week%(j%) = week%(j% - 1%) + 7% else              ~
                       week%(j%) = week%(j% - 1%) + 30%
                next j%
                goto L51020
L51015:     next i%
L51020:     dateline$ = "From: XX/XX/XX  To: XX/XX/XX"
            str(dateline$,7,8) = week$(1)
            call "DATEFMT" (str(dateline$,7,8))
            str(dateline$,21,8) = week$(14)
            call "DATEFMT" (str(dateline$,21,8))
            return

        REM *** Load 14 Mondays For Report by Weeks
        load_weeks
            tdate$ = week$(1%)
            call "DATEFMT" ( tdate$, 0%, week$(1%) )
            for i% = 2% to 14%
                week$(i%) = week$(i%-1%)
                convert str(week$(i%),7%,2%) to da%, data goto L51140
L51140:         convert str(week$(i%),5%,2%) to mo%, data goto L51160
                convert str(week$(i%),1%,4%) to yr%, data goto L51160
L51160:         if mo% = 2% then L51240
                    if mo% = 4% or mo% = 6% or mo% = 9% or mo% = 11% then L51210
                        if da% < 25% then L51310
                            da% = da% - 31%
                            goto L51260
L51210:             if da% < 24% then L51310
                        da% = da% - 30%
                        goto L51260
L51240:         if da% < 22% then L51310   /* Doesn't Consider Leap Year */
                        da% = da% - 28%
L51260:         mo% = mo% + 1%
                if mo% < 13% then L51310
                    mo% = 1%
                    yr% = yr% + 1%

L51310:         da% = da% + 7%
                convert yr% to str(week$(i%),1%,4%), pic(0000)
                convert mo% to str(week$(i%),5%,2%), pic(00)
                convert da% to str(week$(i%),7%,2%), pic(00)
                call "DATECONV" (week$(i%-1%))
            next i%
            call "DATECONV" (week$(14%))
            return

        REM Get Fiscal Dates for Fiscal Month Report
        load_f_mon
            call "READ100" (#12, "FISCAL DATES", f1%(12))
            if f1%(12) = 1 then L51440
                errormsg$ = "No Fiscal Dates Record Available"
                return
L51440:     get #12, using L51450, fismonst$()
L51450:             FMT XX(22), 17*CH(08)
            for i% = 1% to 16%
                if fismonst$(i%) = " " or ~
                   fismonst$(i%) = blankdate$ then L51500
                if seldate$ >= fismonst$(i%) and                         ~
                   seldate$ <  fismonst$(i%+1%) then L51530
L51500:     next i%
                errormsg$ = "Starting Date is NOT Within the Fiscal " &  ~
                            "Calendar, Please Re-Select."
L51530:     k% = 0%
            for j% = i% to 17%
                if fismonst$(j%) = " " or ~
                   fismonst$(j%) = blankdate$ then L51590
                    k% = k% + 1%
                    week$(k%) = fismonst$(j%)
                    if k% = 14% then return
L51590:     next j%
            i% = 4%
            for j% = k% + 1% to 14%
                i% = i% + 1%
                if fismonst$(i%) = " " or ~
                   fismonst$(i%) = blankdate$ then i% = i% + 1%
                tdate$ = fismonst$(i%)
                call "DATEFMT" ( tdate$, 0%, week$(j%) )
                convert str(week$(j%),1%,4%) to yr%, data goto L51660
L51660:         yr% = yr% + 1%
                convert yr% to str(week$(j%),1%,4%), pic(0000)
                call "DATECONV" ( week$(j%) )
            next j%
            return

        REM *** Load In Production Calendar
        load_plan_cal
            call "READ100" (#6, "10", f1%(06))
            if f1%(06) <> 1 then goto L10000
            get #6, using L51760, str(yymmdd$(),1,1470)
L51760:         FMT XX(2), CH(1470)
            call "READ100" (#6, "11", f1%(06))
            get #6, using L51760, str(yymmdd$(),1471,1470)
            return

        REM *** Set up 13 Calendar Months
        load_c_mon
            tdate$ = yymmdd$(subscr%)
            call "DATEFMT" ( tdate$, 0%, week$(1%) )
            str(week$(1%),7%,2%) = "01"
            for i% = 2% to 14%
                week$(i%) = week$(i% - 1%)
                convert str(week$(i%),5%,2%) to mo%, data goto L51880
L51880:         if mo% < 12% then L51930
                    convert str(week$(i%),1%,4%) to yr%, data goto L51900
L51900:             yr% = yr% + 1%
                    convert yr% to str(week$(i%),1%,4%), pic(0000)
                    mo% = 0%
L51930:         mo% = mo% + 1%
                convert mo% to str(week$(i%),5%,2%), pic(00)
                call "DATECONV" (week$(i%-1%))
            next i%
            call "DATECONV" (week$(14%))
            return


L52010: REM Test The From Part Number
            if firstpart$ <> "?" then L52050
                call "GETCODE" (#2, firstpart$, " ", 0%, 0, f1%(2))
                if f1%(2) = 0% then firstpart$ = " "
L52050:     if lastpart$ <> "?" then L52090
                call "GETCODE" (#2, lastpart$, " ", 0%, 0, f1%(2))
                if f1%(2) = 0% then lastpart$ = " "

L52090:     call "TESTRNGE" (firstpart$, lastpart$, hnyplow$, limit$,    ~
                             errormsg$)
            if errormsg$ <> " " then return
                part$ = hnyplow$
                return

L52270: REM TEST GENERIC REFERENCE
             if genrefr$ = "ALL" then L52470
             if genrefr$ = "?" then L52310
             if genrefr$ <> " " then L52360
L52310:      str(genread$,1,16) = genrefr$
             str(genread$,17, ) = "   "
             call "PLOWCODE" (#18, genread$, " ", 16%,-1.3, f1%(18))
             if f1%(18) = 0% then L52490
             genrefr$ = str(genread$,1,16)
L52360:      if gento$ = " " then gento$ = genrefr$
             if gento$ = "ALL" then L52470
             if gento$ = "?" then L52400
             return
L52400:      str(genread$,1,16) = gento$
             str(genread$,17, ) = "    "
             call "PLOWCODE" (#18, genread$, " ", 16%,-1.3, f1%(18))
             gento$ = str(genread$,1,16)
             if gento$ < genrefr$ then errormsg$ ="To Can Not Be Greater ~
        ~Than From"
             return
L52470:      gento$ = genrefr$
             return
L52490:     errormsg$ = "Invalid Generic Reference"
            return
L52510: REM *** TEST CATEGORY ***************
                if catcoder$   = "ALL" then L52710
                if catcoder$ = "?" then L52550
                if catcoder$ <> " " then L52610
L52550:      call "GETCODE" (#15,catcoder$," ",0%,0,f1%(15))
                if f1%(15) = 1 then L52610
                   if catcoder$ = "?" then catcoder$ = " "
                   errormsg$ = "Blank From Category Not Allowed "
                   return

L52610:         if catto$ = " " then catto$ = catcoder$
                if catto$ = "?" then L52660
                if catto$ = "ALL" then catto$ = catcoder$
                return

L52660:         call "GETCODE" (#15,catto$," ",0%,0,f1%(15))
                if catto$ = "?" then catto$ = " "
                if catto$ < catcoder$ then errormsg$ = "To Can Not Be Gre~
        ~ater Than From"
                return
L52710:         catto$ = "ALL"
                return

L52740: REM ********* TEST TYPE ******************
            convert typer$ to type% , data goto L52870
            call "HNYTYPE" (type%,typedescr$,0%)
            if typedescr$ = "** Invalid **" then L52870
            convert type% to typer$,pic(000)
            if typeto$ = " " then typeto$ = typer$
            convert typeto$ to type%, data goto L52870
            call "HNYTYPE" (type%,typedescr$,0%)
            if typedescr$ ="** Invalid **" then L52870
            convert type% to typeto$,pic(000)
            if typeto$ >= typer$ then return
            errormsg$ = "To Must Be Greater Than From"
            return
L52870:     errormsg$ = "Part Type Must Be 000 to 999"
            return
L52890: REM Test Answers  Src Report
            if srcrpt$ = "Y" or srcrpt$ = "N" then return
                errormsg$ = "Enter 'Y' or 'N' Only"
                return

L52940: REM Test Answers For  Std Cost Report
            if stdcost$ = "Y" or stdcost$ = "N" then return
                errormsg$ = "Enter 'Y' or 'N' Only"
                return

L52990: REM Test For Summary Report Option
            if sum$ = "Y" or sum$ = "N" or sum$ = "O" then return
            errormsg$ = "'Y' - 'N' and 'O' Are The Only Valid Responses"
            return

*       *****************************************************************
*                      IMAGE STATEMENTS FOR REPORT                      *
*       *****************************************************************

L53100: %                                 ###############################~
        ~#############################                    PLNMSRPT: PLN003
L53120: %Run Date: ########  ##:## ##            M A S T E R    S C H E D~
        ~ U L E    R E P O R T                                Page:   ####
L53140: %                                        ----------##############~
        ~###########----------
L53160: %                                                ################~
        ~##############
L53180: %Part Number: #########################  Description: ###########~
        ~#####################
        %
L53210: %  * SUMMARY REPORT *  Run: ########  For Parts: ################~
        ~######### TO: #########################
L53230: %                      Generic Ref:  ################  To: ######~
        ~##########
L53250: %                      Category: ####  To: ####  Type: ###  To: #~
        ~##
L53270: %Part Type  =          ###        Min Ord. Qty  =  ########.##   ~
        ~     Safety Stock =  ########.##         On Hand   =   ########.##
L53290: %Cat  Code  =         ####        Pansize       =  ########.##   ~
        ~     On Order     =  ########.##         Back Ord. =   ########.##
L53310: %Buyer      =          ###        Scheduler                ###   ~
        ~     Committed    =  ########.##         In Qc     =   ########.##

L53340: %+---------------------------------------------------------------~
        ~--------------------------------------------+

L53370: %!#############  -#########  -#########  -#########  -#########  ~
        ~-#########  -#########  -######### !

L53400: %!####################   #####.####   #,###,###   #,###,###   #,#~
        ~##,###   #,###,###   #,###,###   #,###,###  !

L53430: %!                PRIOR TO >  ########    ########    ########   ~
        ~ ########    ########    ########  !

L53460: %!--------------------   ----------   ---------   ---------   ---~
        ~------   ---------   ---------   ---------  !

L53490: %!                ########    ########    ########    ########   ~
        ~ ########    ########    ########    ALL FUTURE !

L53520: %!#############  -#########  -#########  -#########  -#########  ~
        ~-#########  -#########  -#########  -#########  !

L53550: %!-------------------------  ----------  ----------  ----------  ~
        ~----------  ----------  ----------  ----------  !

        %!##############  #,###,###   #,###,###   #,###,###   #,###,###  ~
        ~ #,###,###   #,###,###   #,###,###              !

        %!-------------------------  ----------  ----------  ----------  ~
        ~----------  ----------  ---------- !

        %!------------   ----------  ----------  ----------  ----------  ~
        ~----------  ----------  ----------              !

L53670: %    * * * ALL  DEMANDS * * *

L53690: %    * * * PLANNED  DEMANDS * * *

L53710: %    * * * PLAN  RESULTS  * * *

        %    * * * STANDARD COST OF PLAN RESULTS * * *

        %    NOTES:  (1) All actual inventory adds and withdrawals PRIOR ~
        ~to ######## are included.   (2) Uses include UNPLANNED SALES.

        %            Demand Types: 1 Net Sales, 2 Non-Net Sale, 3-7-8 Req~
        ~uisition, 4 Forecast for Net Sales, 5 Increased Forecast

L53810: %                        *** SOURCES ***                         ~
        ~                          *** USES ***


L53850: %!     Tag Number       Rls Date  Due Date    Bal Due  Qty Comp !~
        ~! Type  Demand          Line    Date Req    Date Pln     Quantity~
        ~!
L53880: %+--------------------------------------------------------------!~
        ~!----------------------------------------------------------------~
        ~+

L53920: % ###################   ########  ########  ######### ######### !~
        ~!  #   ################  ###    ########    ######### ###########

L53950: %!                PRIOR TO >  ########    ########    ########   ~
        ~ ########    ########    ########  !   * * * * *   NOTES   * * * ~
        ~* *

L53990: %+-------------------------  ----------  ----------  ----------  ~
        ~----------  ----------  ---------- +                             ~

L54020: %    * * * ALL  DEMANDS * * *                                    ~
        ~                                       (1) All Actual Inventory A~
        ~dds
L54050: %!---------------------------------------------------------------~
        ~-----------------------------------!       and Withdrawals prior ~
        ~to

L54090: %!#############  -#########  -#########  -#########  -#########  ~
        ~-#########  -#########  -######### !       ######## are included.

L54120: %!#############  -#########  -#########  -#########  -#########  ~
        ~-#########  -#########  -######### !

L54150: %!#############  -#########  -#########  -#########  -#########  ~
        ~-#########  -#########  -######### !   (2) Uses include All Sales~
        ~.

L54190: %!#############  -#########  -#########  -#########  -#########  ~
        ~-#########  -#########  -######### !       Planned and Unplanned.

L54220: %+-------------  ----------  ----------  ----------  ----------  ~
        ~----------  -----------  --------- +

L54250: %!#############  -#########  -#########  -#########  -#########  ~
        ~-#########  -#########  -######### !   * * * * DEMAND TYPES * * *~
        ~ *

L54290: %+---------------------------------------------------------------~
        ~-----------------------------------!

L54320: %    * * * PLANNED  DEMANDS * * *                                ~
        ~                                           1 - Net Sales

L54350: %+---------------------------------------------------------------~
        ~-----------------------------------!       2 - Non Net Sales

L54380: %!#############  -#########  -#########  -#########  -#########  ~
        ~-#########  -#########  -######### !       3 - Requisition

L54410: %!#############  -#########  -#########  -#########  -#########  ~
        ~-#########  -#########  -######### !       4 - Forecast

L54440: %!#############  -#########  -#########  -#########  -#########  ~
        ~-#########  -#########  -######### !       5 - Forecast Increase

L54470: %!#############  -#########  -#########  -#########  -#########  ~
        ~-#########  -#########  -######### !       7 - Procurement

L54500: %!-------------------------  ----------  ----------  ----------  ~
        ~----------  ----------  ---------- !       8 - Procurement

L54530: %!---------------------------------------------------------------~
        ~-----------------------------------!

L54560: %!---------------------------------------------------------------~
        ~------------------------------------------------!

L54590: %                                     ########    ########    ###~
        ~#####    ########    ########    ########

L54620: %  CURRENT COST SET IS ########  ##############################

L54650:       %                            * * * * * * * * * *   E N D   ~
        ~O F   R E P O R T   @  ########  * * * * * * * * * *

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            call "SHOSTAT" ("ONE MOMENT PLEASE")

            call "SETPRNT" ("PLN003", " ", 0%, 1%)

            end
