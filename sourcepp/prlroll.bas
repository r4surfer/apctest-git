        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  PPPP   RRRR   L      RRRR    OOO   L      L              *~
            *  P   P  R   R  L      R   R  O   O  L      L              *~
            *  PPPP   RRRR   L      RRRR   O   O  L      L              *~
            *  P      R   R  L      R   R  O   O  L      L              *~
            *  P      R   R  LLLLL  R   R   OOO   LLLLL  LLLLL          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLROLL  - ALLOWS USER TO CLEAR MTD, QTD AND YTD ACCRAULS *~
            *            FOR ALL EMPLOYEES.                             *~
            *-----------------------------------------------------------*~
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
            * 12/22/83 ! ORIGINAL                                 ! HES *~
            * 10/13/87 ! Now allows purging of old check detail.  !     *~
            *          ! Converted SHOMESG to SHOSTAT             !     *~
            *          ! Converted STARTOVER to STARTOVR subroutn !     *~
            *          ! Cleaned Screen Headings                  ! DAW *~
            * 09/12/89 ! Now disallows purging of old check detail! JDH *~
            *          !   if check is not reconciled.            !     *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            * 03/02/95 ! Fixed delete of PRLCHK on UNIX. Thx Ken. ! JDH *~
            * 07/09/96 ! Corrected short arg list in call DELETE. ! JDH *~
            * 09/06/96 ! Millie date conversion                   ! DER *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            amt(3,2),                    /*                            */~
            ant(3),                      /*                            */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            emp_code$12,                 /* Employee Number            */~
            earnseq$3,                   /* Earnings Sequence Number   */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            last_purge$8,                /* Date of last purge         */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            line2$79,                    /* Screen information line    */~
            mtd$3,                       /* CLEAR THE MONTHLY ACCRUALS?*/~
            purge$3,                     /* Flag to PURGE Old Detail   */~
            purge_date$8,                /* Oldest Date to Start Purge */~
            purge_date_keep$8,           /* Oldest Date to Start Purge */~
            prl_check$8,                 /* Payroll Check Number       */~
            plowkey$25,                  /* Misc. Plow String          */~
            reconciled$1,                /* Reconciliation flag        */~
            record$(3)150,               /* WORK VARIABLE              */~
            tdate$8,                     /* Temporary Date Variable    */~
            udate$8,                     /* Temporary Date Variable    */~
            qtd$3,                       /* CLEAR THISQUARTERSACCRUALS?*/~
            ytd$3                        /* CLEAR THIS YEARS ACCRUALS? */~

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64),                     /* = 1 IF READ WAS SUCCESSFUL */~
            rslt$(64)20,                 /* TEXT FROM FILE OPENING     */~
            axd$(64)4                    /* ALT KEY POINTER FROM OPEN'G*/

        REM *************************************************************~
            *                 RELEASE VERSION ID SECTION                *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

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
            * #1  ! EMPDEDXN ! Employee deduction file                  *~
            * #2  ! EMPEARN1 ! Employee earnings file                   *~
            * #3  ! PRLCHK   ! Payroll Checks File                      *~
            * #4  ! PRLCHK2  ! Payroll Check Detail File                *~
            * #5  ! SYSFILE2 ! System info File                         *~
            *************************************************************

            select #1,  "EMPDEDXN",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  300,                                  ~
                        keypos =    1, keylen =  15,                     ~
                        alt key  1, keypos =   16, keylen =  18, dup

            select #2,  "EMPEARN1",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  200,                                  ~
                        keypos =    1, keylen =  15,                     ~
                        alt key  1, keypos =   16, keylen =  28

            select #3,  "PRLCHK",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  120,                                  ~
                        keypos =    1, keylen =  23,                     ~
                        alt key  1, keypos =   13, keylen =  11,         ~
                            key  2, keypos =   42, keylen =   9, dup

            select #4,  "PRLCHK2",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  120,                                  ~
                        keypos =    1, keylen = 25                       ~

            select #5,  "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos =    1, keylen = 20


*        Check to See if Payroll/Personnel is Active
            call "PRLEXTSB" ("PRL", prl%)
                if prl% = 99% then end (prl%)

            call "SHOSTAT" ("LINKING TO THE DATA BASE TO CLEAR EMPLOYEE A~
        ~CCRUALS")

            call "OPENFILE" (#1,  "IO   ", f2%(1 ), rslt$(1 ), axd$(1 ))
            call "OPENFILE" (#2,  "IO   ", f2%(2 ), rslt$(2 ), axd$(2 ))
            call "OPENFILE" (#3,  "IO   ", f2%(3 ), rslt$(3 ), axd$(3 ))
            call "OPENFILE" (#4,  "IO   ", f2%(4 ), rslt$(4 ), axd$(4 ))
            call "OPENFILE" (#5,  "SHARE", f2%(5 ), rslt$(5 ), axd$(5 ))

            if f2%(1) + f2%(2) + f2%(3) + f2%(4) <> 0 then stop "SOMEONE ~
        ~ELSE IS ACCESSING THE PAYROLL FILES... TRY LATER. (PRESS ENTER)"
            if f2%(1) + f2%(2) <> 0 then L65000

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            last_purge$ = " "
            call "DATEFMT" (date$, 0%, udate$)

            cur_mon$= str(udate$,5%,2%)
            convert str(udate$,1%,4%) to last_year%
            if cur_mon$ <> "12" then last_year% = last_year% - 1%
            convert last_year% to last_year$, pic (0000)
            purge_date$ = last_year$ & "1231"
            call "DATECONV" (purge_date$)
            call "DATEFMT"  (purge_date$)
            purge_date_keep$ = purge_date$

            str(line2$,62) = "PRLROLL : " & str(cms2v$,,8)

            call "READ100" (#5, "PAYROLL PURGE DATE", f1%(5))
              if f1%(5) = 0% then L09200
            get #5, using L09190, last_purge$
L09190:       FMT POS(21), CH(6)
L09200:     if last_purge$ = " " or ~
               last_purge$ = blankdate$ then last_purge$ = "NONE"
            if last_purge$ <> "NONE" then call "DATEFMT" (last_purge$)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$
            mtd$, qtd$, ytd$, purge$ = "NO"
            if purge_date$ = " " then purge_date$ = purge_date_keep$

            for fieldnr% = 1 to  3
                gosub'051(fieldnr%)   /* Default - enables */
                      if enabled% = 0 then L10190
L10130:         gosub'101(fieldnr%)   /* Display & accept */
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L10130
                gosub'151(fieldnr%)   /* Edit & test input data */
                      if errormsg$ <> " " then L10130
L10190:         next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

L10270:     inpmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (ENTER)."
            gosub'111(0%)          /* Display screen - no entry */
              if mtd$ <> "YES" and qtd$ <> "YES" and ytd$ <> "YES" then  ~
                     maxfield% = 3% else maxfield% = 5%

                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L10270
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% > maxfield% then L10270
            gosub'051(fieldnr%)    /* Default - Enables */
              if enabled% = 0% then L10270

L10410:     gosub'111(fieldnr%)    /* Dsiplay & accept screen */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L10410
            gosub'151(fieldnr%)    /* Edit & test input data */
                  if errormsg$ <> " " then L10410
            goto L10270

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            if mtd$ = "NO" and qtd$ = "NO" and ytd$ = "NO" then L65000
            gosub L30000   /* CLEAR EARNING ACCRUALS */
            gosub L31000   /* CLEAR DEDUCTION ACCRUALS */
            gosub L32000   /* Purge Old Check Detail Records */
            goto L65000

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 0
                  on fieldnr% gosub L20150,         /* MTD              */~
                                    L20180,         /* QTD              */~
                                    L20230,         /* YTD              */~
                                    L20280,         /* Purge detail     */~
                                    L20340          /* Purge date       */
                     return

L20150:     REM DEFAULT/ENABLE FOR CLEAR THE MONTHS ACCRUALS?
                enabled% = 1
                inpmessage$ = "Be sure you have all the MTD reports you n~
        ~eed before clearing the MTD accruals."
                return
L20180:     REM DEFAULT/ENABLE FOR CLEAR QUARTERS ACCRUALS?
                enabled% = 1
                inpmessage$ = "Be sure you have all the QTD reports you n~
        ~eed before clearing the QTD accruals."
                return
L20230:     REM DEFAULT/ENABLE FOR CLEAR YEARS ACCRUALS?
                enabled% = 1
                inpmessage$ = "Be sure you have all the YTD reports you n~
        ~eed before clearing the YTD accruals."
                return
L20280: REM Default/Enable for Purge Old Details?
                enabled% = 1
                purge$ = "NO"
                inpmessage$ = "Are you really sure that you want to elimi~
        ~nate old check detail records?"
                return
L20340: REM Default/Enable for Oldest Purge Date to Start Purge with
                enabled% = 1
                if purge_date$ = " " then purge_date$ = purge_date_keep$
                inpmessage$= "This is the oldest date with which to begin~
        ~ purging backwards from."
                return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * OR WILL RETURN USER BACK TO WHERE THEY WERE.  MUST PUSH   *~
            * TWO BUTTONS TO START OVER FOR SAFETY.                     *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode

L30000: REM *************************************************************~
            *    C L E A R    E A R N I N G S    A C C R U A L S        *~
            *                                                           *~
            * PLOWS EMPEARN1 AND ZEROES THE NEEDED BUCKETS.             *~
            *************************************************************

            call "SHOSTAT" ("Clearing employee's earning accruals")

L30080:     call "PLOWNXT1" (#2, readkey$, 0%, f1%(2))
                if f1%(2) = 0 then return

            get #2, using L30240, record$(1), amt(1,1), amt(1,2),         ~
                                 amt(2,1), amt(2,2), amt(3,1), amt(3,2), ~
                                 record$(2)

            if mtd$ = "YES" then amt(1,1), amt(1,2) = 0
            if qtd$ = "YES" then amt(2,1), amt(2,2) = 0
            if ytd$ = "YES" then amt(3,1), amt(3,2) = 0

          rewrite #2,using L30240, str(record$(1),,116),amt(1,1),amt(1,2),~
                                 amt(2,1), amt(2,2), amt(3,1), amt(3,2), ~
                                 str(record$(2),,36)
            goto L30080

L30240: FMT                      /* FILE: EMPEARN1                     */~
            CH(116),             /* FIRST PART OF RECORD               */~
            PD(14,4),            /* month to date units                */~
            PD(14,4),            /* month to date amount               */~
            PD(14,4),            /* quarter to date units              */~
            PD(14,4),            /* quarter to date amount             */~
            PD(14,4),            /* year to date units                 */~
            PD(14,4),            /* year to date amount                */~
            CH(36)               /* REST OF RECORD                     */~

L31000: REM *************************************************************~
            *    C L E A R    E A R N I N G S    A C C R U A L S        *~
            *                                                           *~
            * PLOWS EMPEARN1 AND ZEROES THE NEEDED BUCKETS.             *~
            *************************************************************

            call "SHOSTAT" ("Clearing employee's deduction accruals")
            readkey$ = " "

L31090:     call "PLOWNXT1" (#1, readkey$, 0%, f1%(1))
                if f1%(1) = 0 then return

            get #1, using L31270, record$(1), ant(1), ant(2), ant(3),     ~
                                 record$(2), amt(1,1), amt(1,2),         ~
                                 amt(2,1), amt(2,2), amt(3,1), amt(3,2), ~
                                 record$(3)

            if mtd$ = "YES" then ant(1), amt(1,1), amt(1,2) = 0
            if qtd$ = "YES" then ant(2), amt(2,1), amt(2,2) = 0
            if ytd$ = "YES" then ant(3), amt(3,1), amt(3,2) = 0

          rewrite #1, using L31270, str(record$(1),,124), ant(1), ant(2), ~
                           ant(3), str(record$(2),,24),amt(1,1),amt(1,2),~
                                   amt(2,1), amt(2,2), amt(3,1),amt(3,2),~
                                   str(record$(3),,80)
            goto L31090

L31270: FMT                      /* FILE: EMPDEDXN                     */~
            CH(124),             /* FIRST PART OF RECORD               */~
            PD(14,4),            /* month to date amount               */~
            PD(14,4),            /* quarter to date amount             */~
            PD(14,4),            /* year to date amount                */~
            CH(24),              /* MIDDLE                             */~
            PD(14,4),            /* Month To Date Units Subject        */~
            PD(14,4),            /* Month To Date Dollars Subject      */~
            PD(14,4),            /* Quarter To Date Units Subject      */~
            PD(14,4),            /* Quarter To Date Dollars Subject    */~
            PD(14,4),            /* Year To Date Units subject         */~
            PD(14,4),            /* Year To Date Dollars Subject       */~
            CH(80)               /* LAST (BUT NOT LEAST)               */~

L32000: REM *************************************************************~
            *  THIS SECTION PURGES OUT OLD CHECK DETAIL RECORDS         *~
            *************************************************************
           if purge$ <> "YES" then return

           call "SHOSTAT" ("Purging Old Check Detail Records")

           call "DATUNFMT" (purge_date$)
           plowkey$ = all(hex(00))
           call "PLOWNXT1" (#3, plowkey$, 0%, f1%(3))
           if f1%(3) = 0% then return else goto L32150

L32120:    call "READNXT1" (#3, f1%(3))
             if f1%(3) = 0% then write_date

L32150:    get #3, using L32160, emp_code$,prl_check$,earnseq$,           ~
                                check_date$, reconciled$
L32160:      FMT CH(12), CH(8), CH(3), POS(24), CH(6), POS(67), CH(1)
           if reconciled$ = "N" then L32120
           if check_date$ <= purge_date$ then L32200
           goto L32120

L32200:    plowkey$ = str(emp_code$,,12) & str(prl_check$,,8) &          ~
             str(earnseq$,,3) & hex(0000)
           call "DELETE" (#4, plowkey$, 23%)
           call "DELETE" (#3, key(#3), 23%)
           goto L32120

        write_date
        REM (Re)Write unformatted last date of purge to SYSFILE2
            call "READ101" (#5, "PAYROLL PURGE DATE", f1%(5))
              if f1%(5) = 0% then L32320
            get #5, using L32310, last_purge$
L32310:       FMT POS(21), CH(6)
            delete #5
L32320:     last_purge$ = purge_date$
            write #5, using L32350, "PAYROLL PURGE DATE", last_purge$,    ~
                     " ", " "
L32350:       FMT CH(20), CH(6), CH(250), CH(224)
            return

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L40130,         /* MTD              */~
                                    L40130,         /* QTD              */~
                                    L40130          /* YTD              */
                     goto L40170

L40130:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return

L40170:     accept                                                       ~
               at (01,02),                                               ~
                  "CLEAR EMPLOYEE ACCRUALS and/or PURGE OLD DETAIL",     ~
               at (01,66), "TODAY:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "CLEAR THE MONTHS ACCRUALS?",                          ~
               at (06,31), fac(lfac$( 1)), mtd$                 , ch(03),~
               at (07,02),                                               ~
                  "CLEAR THE QUARTERS ACCRUALS?",                        ~
               at (07,31), fac(lfac$( 2)), qtd$                 , ch(03),~
               at (08,02),                                               ~
                  "CLEAR THE YEARS ACCRUALS?",                           ~
               at (08,31), fac(lfac$( 3)), ytd$                 , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02),                                               ~
                  "P.F. KEYS ACTIVE:",                                   ~
               at (23,02),                                               ~
                  "(1)START OVER",                                       ~
               at (23,35),                                               ~
                  "(13)INSTRUCTIONS",                                    ~
               at (23,60),                                               ~
                  "(15)PRINT SCREEN",                                    ~
               at (24,60),                                               ~
                  "(16)EXIT PROGRAM",                                    ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L40530
                  call "MANUAL" ("PRLROLL ")
                  goto L40170

L40530:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40170

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 1 OF DOCUMENT.                    *~
            *************************************************************

            deffn'111(fieldnr%)
                  init(hex(84)) lfac$()
                  if mtd$ <> "YES" and qtd$ <> "YES" and ytd$ <> "YES"   ~
                    then lfac$(4), lfac$(5) = hex(9c)
                  on fieldnr% gosub L41170,         /* MTD              */~
                                    L41170,         /* QTD              */~
                                    L41170,         /* YTD              */~
                                    L41170,         /* Purge detail     */~
                                    L41170          /* Purge date       */
                     goto L41210

L41170:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return

L41210:     accept                                                       ~
               at (01,02),                                               ~
                  "CLEAR EMPLOYEE ACCRUALS and/or PURGE OLD DETAIL",     ~
               at (01,66), "TODAY:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "CLEAR THE MONTHS ACCRUALS?",                          ~
               at (06,31), fac(lfac$( 1)), mtd$                 , ch(03),~
               at (07,02),                                               ~
                  "CLEAR THE QUARTERS ACCRUALS?",                        ~
               at (07,31), fac(lfac$( 2)), qtd$                 , ch(03),~
               at (08,02),                                               ~
                  "CLEAR THE YEARS ACCRUALS?",                           ~
               at (08,31), fac(lfac$( 3)), ytd$                 , ch(03),~
               at (09,02),                                               ~
                  "PURGE OLD CHECK DETAIL?",                             ~
               at (09,31), fac(lfac$( 4)), purge$               , ch(03),~
               at (09,45),                                               ~
                  "DATE OF LAST PAYROLL PURGE:",                         ~
               at (10,02),                                               ~
                  "PURGE BACKWARDS FROM... DATE",                        ~
               at (10,31), fac(lfac$( 5)), purge_date$          , ch(08),~
               at (10,50), fac(hex(8d)), last_purge$            , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02),                                               ~
                  "P.F. KEYS ACTIVE:",                                   ~
               at (23,02),                                               ~
                  "(1)START OVER",                                       ~
               at (23,35),                                               ~
                  "(13)INSTRUCTIONS",                                    ~
               at (23,60),                                               ~
                  "(15)PRINT SCREEN",                                    ~
               at (24,60),                                               ~
                  "(16)BEGIN PROCESSING",                                ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L41660
                  call "MANUAL" ("PRLROLL ")
                  goto L41210

L41660:        if keyhit% <> 15 then L41700
                  call "PRNTSCRN"
                  goto L41210

L41700:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50150,         /* MTD              */~
                                    L50200,         /* QTD              */~
                                    L50250,         /* YTD              */~
                                    L50300,         /* Purge detail     */~
                                    L50350          /* Purge date       */
                     return

L50150:     REM TEST DATA FOR CLEAR THE MONTHLS ACCRUALS?
                if mtd$ = "YES" or mtd$ = "NO" then return
                errormsg$ = "PLEASE ENTER 'YES' OR 'NO'"
                return

L50200:     REM TEST DATA FOR CLEAR THE QUARTERS ACCRUALS?
                if qtd$ = "YES" or qtd$ = "NO" then return
                errormsg$ = "PLEASE ENTER 'YES' OR 'NO'"
                return

L50250:     REM TEST DATA FOR CLEAR THE YEARS ACCRUALS?
                if ytd$ = "YES" or ytd$ = "NO" then return
                errormsg$ = "PLEASE ENTER 'YES' OR 'NO'"
                return

L50300: REM Test data for purge old detail?
                if purge$ = "YES" or purge$ = "NO" then return
                errormsg$ = "PLEASE ENTER 'YES' OR 'NO'"
                return

L50350: REM Test data for purge date
                call "DATEOK" (purge_date$, 1%, errormsg$)
                return

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

            call "SHOSTAT" ("One Moment Please...")

            end
