        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  PPPP   RRRR   L       AAA    CCC   TTTTT  Y   Y  PPPP    *~
            *  P   P  R   R  L      A   A  C   C    T    Y   Y  P   P   *~
            *  PPPP   RRRR   L      AAAAA  C        T     YYY   PPPP    *~
            *  P      R   R  L      A   A  C   C    T      Y    P       *~
            *  P      R   R  LLLLL  A   A   CCC     T      Y    P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLACTYP - MANAGES THE SICK AND VACATION ACCRUAL CONTROL  *~
            *            RECORDS.  THESE RECORDS DEFFINE A 'METHOD' OF  *~
            *            ACCRUAL, AND ARE ACCESSED BY PROGRAM(S) THAT   *~
            *            NEED TO CALCULATE THE AMOUNT TO ADD TO THE     *~
            *            EMPLOYEES ACCUALS                              *~
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
            * 04/05/84 ! ORIGINAL                                 ! HES *~
            * 03/17/86 ! ADDED PRINT LIST ROUTINE                 ! SGA *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            blankline$79,                /* BLANK LINE FOR SCREEN      */~
            cmpname$60,                  /* COMPANY NAME FOR REPORT    */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            descr$30,                    /* METHOD DESCRIPTION         */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            filler$92,                   /* FILLER AT END OF RECORD    */~
            firstcode$3,                 /* FIRST METHOD CODE          */~
            hours$10,                    /* HOURS TO ADD               */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            intdescr$30,                 /* INTERNAL DESCRIPTION       */~
            lastcode$2,                  /* LAST METHOD IN RANGE       */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            maximum$10,                  /* MAXIMUM HOURS TO ACCRUE    */~
            method$2,                    /* METHOD OF ACCRUAL          */~
            minhours$10,                 /* MINIMUM HOURS TO WORK FOR  */~
            prgm$8,                      /* PROGRAM NAME               */~
            prgmid$79,                   /* PROGRAM ID FOR SCREEN      */~
            rptid$6,                     /* REPORT ID NUMBER           */~
            tempcode$2,                  /* FIRST CODE WITH 2 CHAR.    */~
            title$60,                    /* REPORT TITLE               */~
            type$1,                      /* 'U' = PER UNIT WORK, 'P' = */~
            vested$1                     /* IF 'N' THEN VESTED AS ACCRD*/~

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64)                      /* = 1 IF READ WAS SUCCESSFUL */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.01 11/05/92 Payroll Switch & Other          "
        REM *************************************************************
            mat f2% = con : u3% = 0%

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* THE FILE OPENING ROUTINES.                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! PRLACRLS ! PAYROLL SICK & VACATION ACCRUAL METHODS  *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1, "PRLACRLS",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos = 1, keylen = 2


*        Check to See if Payroll/Personnel is Active
            call "PRLEXTSB" ("PRL", prl%)
                if prl% = 99% then end (prl%)

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (# 1, 0%, f2%( 1), 100%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (ENTER)."

            prgm$ = "PRLACTYP"
            rptid$ = "PRL003"
            call "COMPNAME" (12%, cmpname$, ret%)
            ret% = 0
            str(prgmid$,62,9) = "PRLACTYP:"
            str(prgmid$,72,8) = str(cms2v$,1,8)
            title$ = "VACATION AND SICK LEAVE ACCRUAL METHOD LISTING"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, method$, hours$, type$,    ~
                      maximum$, minhours$, intdescr$, vested$

            for fieldnr% = 1 to  7
                gosub'051(fieldnr%)
                      if enabled% = 0 then L10180
L10120:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  =  3 and fieldnr% = 1 then L12000
                      if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L10120
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10120
L10180:         next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

        editmode
L11060:     gosub'111(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L11060
            fieldnr% = cursor%(1) - 6
            if fieldnr% < 2 or fieldnr% >  7 then L11060

L11130:     gosub'111(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11130
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11130
            goto L11060


L12000: REM *************************************************************~
            *        P R I N T   M O D E   I N P U T   R A N G E        *~
            *                                                           *~
            * MAIN PROGRAM FOR INPUTING RANGE OF ACCRL. TYPES TO PRINT. *~
            *************************************************************

            init(" ") errormsg$, firstcode$, lastcode$, thiscode$,       ~
                      blankline$, tempcode$
            firstcode$ = "ALL"
            blankline$ = "ENTER THE RANGE OF METHODS TO LIST."
L12010:     gosub L42000
                  if keyhit%  =  1 then       inputmode
                  if keyhit%  = 16 then       L65000
            gosub L51000
                  if errormsg$ <> " " then L12010

            call "SHOSTAT" ("PRINTING ACCRUAL METHOD LISTING")
            gosub L21000

              REM RETURN FROM ROUTINE
                   close printer
                    goto L12000

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave

            convert hours$ to hours
            convert maximum$ to hmax
            convert minhours$ to hmin

            call "READ101" (#1, method$, f1%(1))
            if f1%(1) = 1 then delete #1
            write #1,using L19180, method$, intdescr$, hours, type$, hmax,~
                              hmin, vested$, filler$
        goto inputmode

L19180:     FMT CH(2),                   /* METHOD CODE                */~
                CH(30),                  /* INTERNAL DESCRIPTION       */~
                PD(14,6),                /* HOURS TO ACCRUE            */~
                CH(1),                   /* TYPE ('U' OR 'P')          */~
                PD(14,4),                /* MAXIMUM HOURS TO ACCRUE    */~
                PD(14,4),                /* MINIMUM HOURS TO CONSIDER  */~
                CH(1),                   /* N=VESTED AS ACCRD,Y=ANNUALY*/~
                CH(92)                   /* FILLER                     */

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1
                  on fieldnr% gosub L20180,         /* METHOD OF ACCRUAL*/~
                                    L20230,         /* HOURS TO ADD     */~
                                    L20280,         /* BASIC TYPE       */~
                                    L20330,         /* MAXIMUM          */~
                                    L20380,         /* MINIMUM          */~
                                    L20430,         /* DESCRIPTION      */~
                                    L20560          /* VESTING FLAG     */
                     return
L20180:     REM DEFAULT/ENABLE FOR METHOD OF ACCRUAL
                inpmessage$ = "Leave Method Blank And Press (ENTER) To Se~
        ~e Methods On File."
                return

L20230:     REM DEFAULT/ENABLE FOR HOURS TO ADD
                inpmessage$ = "Enter The Number Of Units (such as hours) ~
        ~to Be Added to Accruals"
                return

L20280:     REM DEFAULT/ENABLE FOR 'U' = PER UNIT WORK, 'P' =
                inpmessage$ = "P = These Are Added Once Every Pay Period,~
        ~ U = Add For Every Unit Worked"
                return

L20330:     REM DEFAULT/ENABLE FOR MAXIMUM HOURS TO ACCRUE
                inpmessage$ = "Enter The Maximum Units An Employee Can Ac~
        ~crue. Excess is saved for reporting."
                return

L20380:     REM DEFAULT/ENABLE FOR MINIMUM HOURS TO WORK FOR
                inpmessage$ = "Enter The Minimum Hours An Employee Must W~
        ~ork To Accrue The Benefit."
                return

L20430:     REM DEFAULT/ENABLE FOR ACCRUAL METHOD DESCRIPTION
            convert hours$ to hours
            call "NUMTEST" (hours$, .0001, 9e9, errormsg$, 0.4, hours)
            if errormsg$ <> " " then L20520
            if type$ = "P" then intdescr$ = "ADD " & hours$ &            ~
                     " UNITS EACH PERIOD"
            if type$ = "U" then intdescr$ = "ADD " & hours$ &            ~
                     " UNITS FOR ONE WORKED"

L20520:         inpmessage$ = "Enter The description you would like to re~
        ~ference this method by."
                return

L20560:     REM DEFAULT/ENABLE FOR VESTING FLAG
                inpmessage$ = "If units are vested AS ACCRUED, then enter~
        ~ 'N'. If vested annually enter 'Y'."
                vested$ = "N"
                return

L21000: REM *************************************************************~
            *        P R I N T   A   R A N G E   O F   C O D E S        *~
            *                                                           *~
            * PRINT A RANGE OF ACCRUAL METHOD CODES.                    *~
            *************************************************************

            thiscode$ = tempcode$
            page%, total% = 0
            line% = 1000

L21100:     call "PLOWNEXT" (#1, thiscode$, 0%, f1%(1))
                 if f1%(1) = 0 then L21190
            if thiscode$ > lastcode$ then L21190
            gosub L32000                            /* GET TITLE RECORD */
            gosub L21250                            /* PAGE HEADER CHECK*/
            print using L21480, method$, descr$
            total% = total% + 1
            goto L21100

L21190:     REM END ROUTINE--PRINT STATS, AND RETURN TO CALLER.
                if total% = 0 then return
                   print using L21420
                   print
                   print using L21510, total%
                   return

L21250:     select printer(132)
            line% = line%+1
            if line% < 60 then return
               print page
               page% = page% + 1
               print using L21390, date$, cmpname$, page%
               print using L21400, prgm$, title$, rptid$
               print
               print using L21420
               print using L21450
               print using L21420
               line% = 5
               return

L21390: %########                           #############################~
        ~###############################                       PAGE: #####~
        ~#
L21400: %########                                  ######################~
        ~########################                            REPORT: #####~
        ~#
L21420: %                                          +----------+----------~
        ~----------------------+

L21450: %                                          !  METHOD  !          ~
        ~ DESCRIPTION          !

L21480: %                                          !   ####   ! #########~
        ~##################### !

L21510: %                                          ***** TOTAL OF #####  ~
        ~   METHODS LISTED *****

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

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

            keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return

            return clear all
            goto inputmode

L30000: REM *************************************************************~
            *         L O A D   R E C O R D   F O R   E D I T           *~
            *                                                           *~
            * LOADS THE PRLACRLS MASTER RECORD.                         *~
            *************************************************************

          get  #1, using L30160, method$, intdescr$, hours, type$, hmax,  ~
                               hmin, vested$, filler$

            call "CONVERT" (hours, 2.4, hours$)
            call "CONVERT" (hmax, 2.4, maximum$)
            call "CONVERT" (hmin, 2.4, minhours$)
            return

L30160:     FMT CH(2),                   /* METHOD CODE                */~
                CH(30),                  /* INTERNAL DESCRIPTION       */~
                PD(14,6),                /* HOURS TO ACCRUE            */~
                CH(1),                   /* TYPE ('U' OR 'P')          */~
                PD(14,4),                /* MAXIMUM HOURS TO ACCRUE    */~
                PD(14,4),                /* MINIMUM HOURS TO CONSIDER  */~
                CH(1),                   /* VESTED FLAG                */~
                CH(92)                   /* FILLER                     */

L32000: REM *************************************************************~
            *   G E T   A   R E C O R D   F O R   P R I N T   M O D E   *~
            *                                                           *~
            *GETS A RECORD FROM THE ACCRL METHOD CODE FILE FOR PRINTING.*~
            *************************************************************

            get #1, using L32090, method$, descr$
            return

L32090:     FMT CH(2),                   /* METHOD CODE                */~
                CH(30)                   /* METHOD DESCRIPTION         */~

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L40220,         /* METHOD OF ACCRUAL*/~
                                    L40250,         /* HOURS TO ADD     */~
                                    L40220,         /* BASIC TYPE       */~
                                    L40250,         /* MAXIMUM          */~
                                    L40250,         /* MINIMUM          */~
                                    L40220,         /* description      */~
                                    L40220          /* VESTED & NONVESTD*/
                     goto L40290

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40220:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L40250:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40290:     accept                                                       ~
               at (01,02),                                               ~
                  "Manage Payroll Sick and Vacation Accrual Methods",    ~
               at (01,67), "DATE:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), prgmid$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "METHOD OF ACCRUAL",                                   ~
               at (06,30), fac(lfac$( 1)), method$              , ch(02),~
               at (08,02),                                               ~
                  "NUMBER OF UNITS TO ACCRUE",                           ~
               at (08,30), fac(lfac$( 2)), hours$               , ch(10),~
               at (09,02),                                               ~
                  "ACCRUAL FREQUENCY",                                   ~
               at (09,30), fac(lfac$( 3)), type$                , ch(01),~
               at (10,02),                                               ~
                  "MAXIMUM HOURS TO ACCRUE",                             ~
               at (10,30), fac(lfac$( 4)), maximum$             , ch(10),~
               at (11,02),                                               ~
                  "MINIMUM UNITS TO WORK",                               ~
               at (11,30), fac(lfac$( 5)), minhours$            , ch(10),~
               at (12,02),                                               ~
                  "METHOD DESCRIPTION",                                  ~
               at (12,30), fac(lfac$( 6)), intdescr$            , ch(30),~
               at (13,02),                                               ~
                  "ARE UNITS VESTED ANNUALLY?",                          ~
               at (13,30), fac(lfac$( 7)), vested$              , ch(01),~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,20),                                               ~
                  "(3)Print Methods",                                    ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)Exit Program",                                    ~
                                                                         ~
               keys(hex(0001030d0f10)),                                  ~
               key (keyhit%)

               if keyhit% <> 13 then L40870
                  call "MANUAL" ("PRLACTYP")
                  goto L40290

L40870:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40290

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 1 OF DOCUMENT.                    *~
            *************************************************************

            deffn'111(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L41220,         /* METHOD OF ACCRUAL*/~
                                    L41250,         /* HOURS TO ADD     */~
                                    L41220,         /* BASIC TYPE       */~
                                    L41250,         /* MAXIMUM          */~
                                    L41250,         /* MINIMUM          */~
                                    L41220,         /* description      */~
                                    L40220          /* VESTED & NONVESTD*/
                     goto L41290

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L41220:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L41250:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L41290:     accept                                                       ~
               at (01,02),                                               ~
                  "Manage Payroll Sick and Vacation Accrual Methods",    ~
               at (01,67), "DATE:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), prgmid$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "METHOD OF ACCRUAL",                                   ~
               at (06,30), fac(lfac$( 1)), method$              , ch(02),~
               at (08,02),                                               ~
                  "NUMBER OF UNITS TO ACCRUE",                           ~
               at (08,30), fac(lfac$( 2)), hours$               , ch(10),~
               at (09,02),                                               ~
                  "ACCRUAL FREQUENCY",                                   ~
               at (09,30), fac(lfac$( 3)), type$                , ch(01),~
               at (10,02),                                               ~
                  "MAXIMUM HOURS TO ACCRUE",                             ~
               at (10,30), fac(lfac$( 4)), maximum$             , ch(10),~
               at (11,02),                                               ~
                  "MINIMUM UNITS TO WORK",                               ~
               at (11,30), fac(lfac$( 5)), minhours$            , ch(10),~
               at (12,02),                                               ~
                  "METHOD DESCRIPTION",                                  ~
               at (12,30), fac(lfac$( 6)), intdescr$            , ch(30),~
               at (13,02),                                               ~
                  "ARE UNITS VESTED ANNUALLY?",                          ~
               at (13,30), fac(lfac$( 7)), vested$              , ch(01),~
               at (21,02), fac(hex(a4)),   edtmessage$          , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)Save Data",                                       ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L41870
                  call "MANUAL" ("PRLACTYP")
                  goto L41290

L41870:        if keyhit% <> 15 then L41910
                  call "PRNTSCRN"
                  goto L41290

L41910:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

L42000: REM *************************************************************~
            *            I N P U T   R A N G E   S C R E E N            *~
            *                                                           *~
            * INPUT VALUES FOR THE RANGE OF DEPARTMENT CODES TO PRINT.  *~
            *************************************************************

L42360:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Vacation and Sick Leave Accrual Methods",       ~
               at (01,67), "DATE:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), prgmid$                , ch(79),~
               at (04,02), fac(hex(84)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "FIRST METHOD CODE",                                   ~
               at (06,30), fac(hex(81)), firstcode$             , ch(03),~
               at (07,02),                                               ~
                  "LAST METHOD CODE",                                    ~
               at (07,30), fac(hex(81)), lastcode$              , ch(02),~
               at (21,02), fac(hex(a4)), blankline$             , ch(79),~
               at (22,02),                                               ~
                  "(1)Input Mode",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)PRINT SCREEN",                                    ~
               at (24,65),                                               ~
                  "(16)Exit Program",                                    ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

            if keyhit% <> 13 then L44280
                call "MANUAL" ("PRLACTYP")
                goto L42360

L44280:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50180,         /* METHOD OF ACCRUAL*/~
                                    L50300,         /* HOURS TO ADD     */~
                                    L50330,         /* BASIC TYPE       */~
                                    L50360,         /* MAXIMUM          */~
                                    L50400,         /* MINIMUM          */~
                                    L50440,         /* METHOD DECRIPTION*/~
                                    L50470          /* VESTING FLAG     */
                     return
L50180:     REM TEST DATA FOR METHOD OF ACCRUAL
            if method$ <> " " then L50240
                call "GETCODE" (#1, method$, " ", 0%, 0, f1%(1))
                     if f1%(1) <> 0 then L50260
                     errormsg$ = hex(00)
                     return
L50240:         call "READ100" (#1, method$,  f1%(1))
                     if f1%(1) = 0 then return
L50260:         gosub L30000
                return clear
                return clear
                goto editmode
L50300:     REM TEST DATA FOR HOURS TO ADD
                call "NUMTEST" (hours$, .0001, 9e9, errormsg$, 0.6, hours)
                return
L50330:     REM TEST DATA FOR 'U' = PER UNIT WORK, 'P' =
                if pos("PU" =type$)=0 then errormsg$="MUST BE 'U' OR 'P'"
                return
L50360:     REM TEST DATA FOR MAXIMUM HOURS TO ACCRUE
                if maximum$ = " " then maximum$ = "9999"
                call "NUMTEST" (maximum$, 1, 9e9, errormsg$, 0.2, hmax)
                return
L50400:     REM TEST DATA FOR MINIMUM HOURS TO WORK FOR
                call "NUMTEST" (minhours$, 0, 9e9, errormsg$, 0.2, hmin)
                return
L50440:     REM TEST DATA FOR INTERNAL DESCRIPTION
                if intdescr$ = " " then errormsg$ = "CAN'T BE BLANK"
                return
L50470:     REM TEST DATA FOR VESTING FLAG
                if pos("YN"=vested$)=0 then errormsg$ = "ENTER 'Y' OR 'N'"
                return

L51000: REM *************************************************************~
            *       V A L I D A T E   R A N G E   T O   P R I N T       *~
            *                                                           *~
            * VALIDATES THE RANGE TO BE PRINTED.                        *~
            *************************************************************


             errormsg$ = " "
             REM HANDLES CASE FOR "ALL" METHOD CODES
                 if firstcode$ <> "ALL" then L51130
                    init(hex(00)) tempcode$
                    init(hex(ff)) lastcode$
                    return
L51130:      REM HANDLES CASE FOR SINGLE CODE
                 if lastcode$ <> " " then L51190
                    tempcode$ = str(firstcode$,1,2)
                    lastcode$ = tempcode$
                    tempcode$ =  add(hex(ff))
                    return
L51190:      REM HANDLES CASE FOR A RANGE OF CODES
                 if lastcode$ < firstcode$ then L51230
                    tempcode$ = firstcode$ addc hex(fffffffe)
                    return
L51230:      REM HANDLES ERROR MESSAGE -- LAST < FIRST.
                 errormsg$ = "ILLEGAL RANGE!  Please Respecify."
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

            call "SHOSTAT" ("Data Base Integrity Check In Progress")

            end
