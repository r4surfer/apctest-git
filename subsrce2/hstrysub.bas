        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  H   H   SSS   TTTTT  RRRR   Y   Y   SSS   U   U  BBBB    *~
            *  H   H  S        T    R   R  Y   Y  S      U   U  B   B   *~
            *  HHHHH   SSS     T    RRRR    YYY    SSS   U   U  BBBB    *~
            *  H   H      S    T    R   R    Y        S  U   U  B   B   *~
            *  H   H   SSS     T    R   R    Y     SSS    UUU   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HSTRYSUB - SUBROUTINE VERSION OF HISINPUT.  MANAGE        *~
            *            EMPLOYMENT HISTORY WITH COMPANY.  WHAT JOBS    *~
            *            WERE HELD WHEN, RATE OF PAY, ETC.              *~
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
            * 12/01/83 ! ORIGINAL                                 ! GLW *~
            * 02/23/88 ! Fixed bug in screen handling of detail   ! LKM *~
            *          ! and added ASKUSER for deleting records.  !     *~
            * 07/16/96 ! Millie date conversion                   ! DXL *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~

        sub "HSTRYSUB" (#1,#2,#3,#4,#5,#6,#7,#8,#9,#10,#11,employee$)

        dim                                                              ~
            blankdate$8,                 /* Blank date for comparison  */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            filler$(100)171,             /* FILLER FOR END OF RECORD   */~
            fromdate$(100)8,             /* EMPLOYMENT DATES, FROM/TO  */~
            todate$(100)8,               /* EMPLOYMENT DATES, FROM/TO  */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            jobclass$(100)16,            /* JOB CLASSIFICATION - EEOC  */~
            pass1$       162,            /* FOR COMSUB                 */~
            jobtitle$(100)16,            /* JOB TITLE HELD             */~
            pass$        162,            /* FOR COMSUB                 */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            lrby$(100)30,                /* LAST REVIEW BY             */~
            lrcoment$(2,100)50,          /*      REVIEWERS COMMENTS    */~
            lrdate$(100)8,               /* LAST REVIEW DATE           */~
            lrincamt$(100)10,            /* LAST REVIEW INCREASE AMOUNT*/~
            lrincfor$(100)16,            /* LAST REVIEW INCREASE REASON*/~
            lrincpct$(100)10,            /* LAST REVIEW INCREASE PERCNT*/~
            manager$(100)30,             /* NAME OF MANAGER            */~
            name$30,                     /* NAME OF EMPLOYEE           */~
            nrdate$(100)8,               /* NEXT REVIEW DATE           */~
            nrsent$(100)8,               /* DATE REV NOTICE SENT TO SUP*/~
            pay$(100)10,                 /* RATE OF PAY ($ & PER)      */~
            payper$(100)16,              /* RATE OF PAY ($ & PER)      */~
            payfor$(100)32,              /* RATE OF PAY ($ & PER)      */~
            readkey$100,                 /* GENERAL PURPOSE FILE KEY   */~
            supervisor$(100)30           /* NAME OF SUPERVISOR         */~

        dim f1%(64)                      /* = 1 IF READ WAS SUCCESSFUL */

        REM *************************************************************~
            *         P A S S   I N   F I L E S                         *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! APLSKILL ! Applicant skills inventory - personnel s *~
            * #02 ! REQSKILL ! Skills required for a requisition - pers *~
            * #03 ! EMPSKILL ! Employee skills inventory - personnel sy *~
            * #04 ! COMTERM  ! File of common terms for personel.       *~
            * #05 ! APLMASTR ! Applicant master file - part of personne *~
            * #06 ! REQMASTR ! Requisition master file - personnel syst *~
            * #07 ! EMPMASTR ! Employee master file                     *~
            * #08 ! PERMASTR ! Personnel master file-ties to EMPMASTR i *~
            * #09 ! PERFRNGE ! Fringe benefit file - personnel system   *~
            * #10 ! INSMASTR ! INSURANCE MASTER FILE - PERSONNEL SYSTEM *~
            * #11 ! EMPHSTRY ! Employment history file - personnel syst *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto   L09062
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
L09062: REM *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date
            call "DATEFMT" (date$)

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (ENTER)."

        REM *************************************************************~
            *       S T A R T I N G   P O I N T                         *~
            *                                                           *~
            *************************************************************

        init(" ")                                                        ~
            todate$(),                                                   ~
            edtmessage$  ,               /* EDIT SCREEN MESSAGE        */~
            errormsg$  ,                 /* ERROR MESSAGE              */~
            filler$()     ,              /* FILLER FOR END OF RECORD   */~
            fromdate$()   ,              /* EMPLOYMENT DATES, FROM/TO  */~
            inpmessage$  ,               /* INPUT MESSAGE              */~
            jobclass$()    ,             /* JOB CLASSIFICATION - EEOC  */~
            pass1$          ,            /* FOR COMSUB                 */~
            jobtitle$()    ,             /* JOB TITLE HELD             */~
            pass$           ,            /* FOR COMSUB                 */~
            lrby$()    ,                 /* LAST REVIEW BY             */~
            lrcoment$()       ,          /*      REVIEWERS COMMENTS    */~
            lrdate$()   ,                /* LAST REVIEW DATE           */~
            lrincamt$()    ,             /* LAST REVIEW INCREASE AMOUNT*/~
            lrincfor$()    ,             /* LAST REVIEW INCREASE REASON*/~
            lrincpct$()    ,             /* LAST REVIEW INCREASE PERCNT*/~
            manager$()    ,              /* NAME OF MANAGER            */~
            name$  ,                     /* NAME OF EMPLOYEE           */~
            nrdate$()   ,                /* NEXT REVIEW DATE           */~
            nrsent$()   ,                /* DATE REV NOTICE SENT TO SUP*/~
            pay$()    ,                  /* RATE OF PAY ($ & PER)      */~
            payfor$()    ,               /* RATE OF PAY ($ & PER)      */~
            readkey$   ,                 /* GENERAL PURPOSE FILE KEY   */~
            supervisor$()                /* NAME OF SUPERVISOR         */~

                     call "READ100" (#8, employee$, f1%(8))
                     if f1%(8) <> 1% then goto L65000
                     get #8, using L09925 , lname$, fname$, mname$
L09925:              FMT XX(1), CH(15), CH(10), CH(1)
                     name$ = str(fname$,1,len(fname$))
                     str(name$,len(fname$) + 2, 1) = mname$
                     str(name$,len(fname$) + 4) = lname$
                     init(" ") errormsg$

           gosub L31000

           goto summaryscreen


        inputmode
            init(" ") pass$ , pass1$
            for fieldnr% = 1 to 15
L10070:         gosub'051(fieldnr%)
                      if enabled% = 0 then L10110
L10080:         gosub'101(fieldnr%)
                      if keyhit% = 4 then goto L10115
                      if keyhit%  = 16 then goto L42230
                      if keyhit% <>  0 then       L10080
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10080
L10110:         if keyhit% <> 4% then goto L10125
L10115:               fieldnr% = max(1%, fieldnr% - 1%)
                      goto L10070
L10125:         next fieldnr%


        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

        editmode

L11060:     gosub'111(0%)
                  if keyhit%  = 16 then  summaryscreen
                  if keyhit% <>  0 then       L11060
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% > 15 then L11060

L11130:     gosub'111(fieldnr%)
                  if keyhit% <>  0 then L11130
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11130
            goto L11060

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub L30000
            goto  L65000

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20100,         /* FROM/TO DATES    */~
                                    L20150,         /* JOB TITLE HELD   */~
                                    L20200,         /* JOB CLASS        */~
                                    L20250,         /* RATE OF PAY      */~
                                    L20300,         /* SUPERVISOR       */~
                                    L20350,         /* NAME OF MANAGER  */~
                                    L20400,         /* LAST REVIEW DATE */~
                                    L20450,         /* LAST REVIEW BY   */~
                                    L20500,         /* LAST INCREASE    */~
                                    L20550,         /* LAST PERCENT     */~
                                    L20600,         /* LAST REASON      */~
                                    L20650,         /* COMMENTS         */~
                                    L20700,         /* COMMENTS         */~
                                    L20750,         /* NEXT REVIEW DATE */~
                                    L20800          /* DATE SENT TO SUPR*/
                     return
L20100:     REM DEFAULT/ENABLE FOR EMPLOYMENT DATES, FROM/TO
           inpmessage$ = "ENTER THE DATES FOR THIS JOB/PAY RECORD        ~
        ~                                "
           call "RJUSTIFY" (inpmessage$)
                return
L20150:     REM DEFAULT/ENABLE FOR JOB TITLE HELD
           inpmessage$ = "ENTER THE JOB TITLE, IF YOU DON'T KNOW, LEAVE I~
        ~T BLANK FOR THE SYSTEM'S HELP   "
           call "RJUSTIFY" (inpmessage$)
                return
L20200:     REM DEFAULT/ENABLE FOR JOB CLASSIFICATION - EEOC
           inpmessage$ = "ENTER THE EEOC CLASSIFICATION, IF YOU DON'T KNO~
        ~W, LEAVE BLANK & SYST WILL HELP "
           call "RJUSTIFY" (inpmessage$)
                return
L20250:     REM DEFAULT/ENABLE FOR RATE OF PAY ($ & PER)
           inpmessage$ = "ENTER BOTH THE RATE OF PAY AND THE PERIODICITY ~
        ~                                "
           call "RJUSTIFY" (inpmessage$)
                return
L20300:     REM DEFAULT/ENABLE FOR NAME OF SUPERVISOR
           inpmessage$ = "ENTER THE NAME OF THE EMPLOYEES CURRENT SUPERVI~
        ~SOR                             "
           call "RJUSTIFY" (inpmessage$)
                return
L20350:     REM DEFAULT/ENABLE FOR NAME OF MANAGER
           inpmessage$ = "ENTER THE NAME OF THE EMPLOYEES CURRENT MANAGER~
        ~                                "
           call "RJUSTIFY" (inpmessage$)
                return
L20400:     REM DEFAULT/ENABLE FOR LAST REVIEW DATE
           inpmessage$ = "ENTER THE DATE ON WHICH THE LAST REVIEW TOOK PL~
        ~ACE                             "
           call "RJUSTIFY" (inpmessage$)
                return
L20450:     REM DEFAULT/ENABLE FOR LAST REVIEW BY
           inpmessage$ = "WHO PERFORMED THE LAST REVIEW?                 ~
        ~                                "
           call "RJUSTIFY" (inpmessage$)
                return
L20500:     REM DEFAULT/ENABLE FOR LAST REVIEW INCREASE AMOUNT
           inpmessage$ = "WHAT WAS THE DOLLAR AMOUNT OF THE INCREASE?  US~
        ~E THE SAME UNITS AS FOR PAY     "
           call "RJUSTIFY" (inpmessage$)
                return
L20550:     REM DEFAULT/ENABLE FOR LAST REVIEW INCREASE PERCNT
           inpmessage$ = "WHAT PERCENTAGE WAS THIS INCREASE?             ~
        ~                                "
           call "RJUSTIFY" (inpmessage$)
                return
L20600:     REM DEFAULT/ENABLE FOR LAST REVIEW INCREASE REASON
           inpmessage$ = "WHY WAS THE INCREASE GIVEN?                    ~
        ~                                "
           call "RJUSTIFY" (inpmessage$)
                return
L20650:     REM DEFAULT/ENABLE FOR      REVIEWERS COMMENTS
           inpmessage$ = "RECORD THE REVIEWERS COMMENTS ABOUT THIS REVIEW~
        ~                                "
           call "RJUSTIFY" (inpmessage$)
                return
L20700:     REM DEFAULT/ENABLE FOR      REVIEWERS COMMENTS
            if lrcoment$(1,a%) = " " then enabled% = 0%
                return
L20750:     REM DEFAULT/ENABLE FOR NEXT REVIEW DATE
           inpmessage$ = "WHAT IS THE DATE OF THE NEXT SCHEDULED REVIEW? ~
        ~                                "
           call "RJUSTIFY" (inpmessage$)
                return
L20800:     REM DEFAULT/ENABLE FOR DATE REV NOTICE SENT TO SUP
           inpmessage$ = "IF A NOTICE FOR REVIEW HAS BEEN SENT TO THE SUP~
        ~ERVISOR, ENTER THE DATE SENT    "
           call "RJUSTIFY" (inpmessage$)
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

L30000: REM WRITE DATA TO EMPHSTRY FILE

           readkey$ = str(employee$,1,12) & " "
           call "DELETE" (#11, readkey$, 12%)

           for i% = 1% to maxi%
           if jobtitle$(i%) = " " then goto L30330
                     convert i% to seqnr$, pic(###)
                     call "DATUNFMT" (fromdate$(i%))
                     call "DATUNFMT" (   todate$(i%))
                     call "DATUNFMT" (nrdate$(i%))
                     call "DATUNFMT" (lrdate$(i%))
                     call "DATUNFMT" (nrsent$(i%))

        put #11, using L35030,    /* FILE: EMPHSTRY                     */~
            nrdate$(i%),         /* Date of next review                */~
            jobclass$(i%),       /* Job classification as per EEO.     */~
            jobtitle$(i%),       /* job title                          */~
            employee$,           /* employee code                      */~
            seqnr$,              /* General purpose sequence number    */~
            fromdate$(i%),       /* Date from                          */~
            todate$(i%),         /* Date to                            */~
            pay$(i%),            /* Rate of pay in dollars and cents   */~
            payper$(i%),         /* Frequency of pay description       */~
            supervisor$(i%),     /* Name of supervisor                 */~
            manager$(i%),        /* Name of manager                    */~
            lrdate$(i%),         /* Last review date                   */~
            lrby$(i%),           /* Last review by                     */~
            lrincamt$(i%),       /* Last review increase amount        */~
            lrincpct$(i%),       /* Last review increase percentage    */~
            lrincfor$(i%),       /* Last review increase for descripti */~
            lrcoment$(1%, i%),   /* Last reviewer's comments           */~
            lrcoment$(2%, i%),   /* Last reviewer's comments           */~
            nrsent$(i%),         /* Date next review notice sent to su */~
            filler$(i%)          /* filler for rest of record or inter */~

           write #11
L30330:    next i%
           return


L31000: REM GET DATA FROM EMPHSTRY FILE


           i%, maxi% = 0%
           readkey$   = str(employee$,1,12) & " "
L31050:    call "PLOWNEXT" (#11, readkey$  , 12%, f1%(11))
           if f1%(11) <> 1% then return
           i%, maxi% = i% + 1%

        get #11, using L35030,    /* FILE: EMPHSTRY                     */~
            nrdate$(i%),         /* Date of next review                */~
            jobclass$(i%),       /* Job classification as per EEO.     */~
            jobtitle$(i%),       /* job title                          */~
            employee$,           /* employee code                      */~
            seqnr$,              /* General purpose sequence number    */~
            fromdate$(i%),       /* Date from                          */~
            todate$(i%),         /* Date to                            */~
            pay$(i%),            /* Rate of pay in dollars and cents   */~
            payper$(i%),         /* Frequency of pay description       */~
            supervisor$(i%),     /* Name of supervisor                 */~
            manager$(i%),        /* Name of manager                    */~
            lrdate$(i%),         /* Last review date                   */~
            lrby$(i%),           /* Last review by                     */~
            lrincamt$(i%),       /* Last review increase amount        */~
            lrincpct$(i%),       /* Last review increase percentage    */~
            lrincfor$(i%),       /* Last review increase for descripti */~
            lrcoment$(1%, i%),   /* Last reviewer's comments           */~
            lrcoment$(2%, i%),   /* Last reviewer's comments           */~
            nrsent$(i%),         /* Date next review notice sent to su */~
            filler$(i%)          /* filler for rest of record or inter */~

                     call "DATEFMT" (fromdate$(i%))
                     call "DATEFMT" (   todate$(i%))
                     call "DATEFMT" (nrdate$(i%))
                     call "DATEFMT" (lrdate$(i%))
                     call "DATEFMT" (nrsent$(i%))

           if i% = 99% then return   /* ARRAYS ARE FULL               */
           goto L31050

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *                                                           *~
            * FORMAT STATEMENTS FOR DATA FILES.                         *~
            *************************************************************

L35030: FMT                      /* FILE: EMPHSTRY                     */~
            CH(6),               /* Date of next review                */~
            CH(16),              /* Job classification as per EEO.     */~
            CH(16),              /* job title                          */~
            CH(12),              /* employee code                      */~
            CH(3),               /* General purpose sequence number    */~
            CH(6),               /* Date from                          */~
            CH(6),               /* Date to                            */~
            CH(10),              /* Rate of pay in dollars and cents   */~
            CH(16),              /* Frequency of pay description       */~
            CH(30),              /* Name of supervisor                 */~
            CH(30),              /* Name of manager                    */~
            CH(6),               /* Last review date                   */~
            CH(30),              /* Last review by                     */~
            CH(10),              /* Last review increase amount        */~
            CH(10),              /* Last review increase percentage    */~
            CH(16),              /* Last review increase for descripti */~
            2*CH(50),            /* Last reviewer's comments           */~
            CH(6),               /* Date next review notice sent to su */~
            CH(171)              /* filler for rest of record or inter */~

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L40140,         /* FROM/TO DATES    */~
                                    L40140,         /* JOB TITLE HELD   */~
                                    L40140,         /* JOB CLASS        */~
                                    L40140,         /* RATE OF PAY      */~
                                    L40140,         /* SUPERVISOR       */~
                                    L40140,         /* NAME OF MANAGER  */~
                                    L40140,         /* LAST REVIEW DATE */~
                                    L40140,         /* LAST REVIEW BY   */~
                                    L40140,         /* LAST INCREASE    */~
                                    L40140,         /* LAST PERCENT     */~
                                    L40140,         /* LAST REASON      */~
                                    L40125,         /* COMMENTS         */~
                                    L40125,         /* COMMENTS         */~
                                    L40140,         /* NEXT REVIEW DATE */~
                                    L40140          /* DATE SENT TO SUPR*/
                     goto L40175

L40125:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40140:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40175:     accept                                                       ~
               at (01,02),                                               ~
                  "EMPLOYMENT HISTORY FOR",                              ~
               at (01,65), "Job notes below:",                           ~
               at (01,25),                                               ~
                  fac(hex(84)), name$                          , ch(30), ~
               at (02,30),                                               ~
                  fac(hex(8c)), str(pass$,63,50)                , ch(50),~
               at (03,30),                                               ~
                  fac(hex(8c)), str(pass$,113,50)               , ch(50),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "EMPLOYMENT DATES, FROM/TO",                           ~
               at (06,30), fac(lfac$( 1)), fromdate$(a%)        , ch(08),~
               at (06,40), fac(lfac$( 1)), todate$(a%)          , ch(08),~
               at (07,02),                                               ~
                  "JOB TITLE HELD",                                      ~
               at (07,30), fac(lfac$( 2)), jobtitle$(a%)        , ch(16),~
               at (07,49), fac(hex(8c)),str(pass$,17,30)        , ch(30),~
               at (08,02),                                               ~
                  "JOB CLASSIFICATION - EEOC",                           ~
               at (08,30), fac(lfac$( 3)), jobclass$(a%)        , ch(16),~
               at (08,49), fac(hex(8c)),str(pass1$,17,30)       , ch(30),~
               at (09,02),                                               ~
                  "RATE OF PAY ($ & PER)",                               ~
               at (09,30), fac(lfac$( 4)), pay$(a%)             , ch(10),~
               at (09,45), fac(lfac$( 4)), payfor$(a%)          , ch(30),~
               at (10,02),                                               ~
                  "NAME OF SUPERVISOR",                                  ~
               at (10,30), fac(lfac$( 5)), supervisor$(a%)      , ch(30),~
               at (11,02),                                               ~
                  "NAME OF MANAGER",                                     ~
               at (11,30), fac(lfac$( 6)), manager$(a%)         , ch(30),~
               at (12,02),                                               ~
                  "LAST REVIEW DATE",                                    ~
               at (12,30), fac(lfac$( 7)), lrdate$(a%)          , ch(08),~
               at (13,02),                                               ~
                  "LAST REVIEW BY",                                      ~
               at (13,30), fac(lfac$( 8)), lrby$(a%)            , ch(30),~
               at (14,02),                                               ~
                  "LAST REVIEW INCREASE AMOUNT",                         ~
               at (14,30), fac(lfac$( 9)), lrincamt$(a%)        , ch(10),~
               at (15,02),                                               ~
                  "LAST REVIEW INCREASE PERCNT",                         ~
               at (15,30), fac(lfac$(10)), lrincpct$(a%)        , ch(10),~
               at (16,02),                                               ~
                  "LAST REVIEW INCREASE REASON",                         ~
               at (16,30), fac(lfac$(11)), lrincfor$(a%)        , ch(16),~
               at (17,02),                                               ~
                  "     REVIEWERS COMMENTS",                             ~
               at (17,30), fac(lfac$(12)), lrcoment$(1,a%)      , ch(50),~
               at (18,02),                                               ~
                  "     REVIEWERS COMMENTS",                             ~
               at (18,30), fac(lfac$(13)), lrcoment$(2,a%)      , ch(50),~
               at (19,02),                                               ~
                  "NEXT REVIEW DATE",                                    ~
               at (19,30), fac(lfac$(14)), nrdate$(a%)          , ch(08),~
               at (20,02),                                               ~
                  "DATE REV NOTICE SENT TO SUP",                         ~
               at (20,30), fac(lfac$(15)), nrsent$(a%)          , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02),                                               ~
                  "P.F. KEYS ACTIVE:",                                   ~
               at (23,02),                                               ~
                  "(13)INSTRUCTIONS        (4)PREVIOUS FIELD",           ~
               at (23,65),                                               ~
                  "(15)PRINT SCREEN",                                    ~
               at (24,65),                                               ~
                  "(16)TO SUMMARY  ",                                    ~
                                                                         ~
               keys(hex(00040d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L40560
                  call "MANUAL" ("HISINPUT")
                  goto L40175

L40560:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40175

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 1 OF DOCUMENT.                    *~
            *************************************************************

            deffn'111(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L41280,         /* FROM/TO DATES    */~
                                    L41280,         /* JOB TITLE HELD   */~
                                    L41280,         /* JOB CLASS        */~
                                    L41280,         /* RATE OF PAY      */~
                                    L41280,         /* SUPERVISOR       */~
                                    L41280,         /* NAME OF MANAGER  */~
                                    L41280,         /* LAST REVIEW DATE */~
                                    L41280,         /* LAST REVIEW BY   */~
                                    L41280,         /* LAST INCREASE    */~
                                    L41280,         /* LAST PERCENT     */~
                                    L41280,         /* LAST REASON      */~
                                    L41250,         /* COMMENTS         */~
                                    L41250,         /* COMMENTS         */~
                                    L41280,         /* NEXT REVIEW DATE */~
                                    L41280          /* DATE SENT TO SUPR*/
                     goto L41350

L41250:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L41280:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L41350:     accept                                                       ~
               at (01,02),                                               ~
                  "EMPLOYMENT HISTORY FOR",                              ~
               at (01,25),                                               ~
                  fac(hex(84)), name$                          , ch(30), ~
               at (01,65), "Job notes below:",                           ~
               at (02,30),                                               ~
                  fac(hex(8c)), str(pass$,63,50)                , ch(50),~
               at (03,30),                                               ~
                  fac(hex(8c)), str(pass$,113,50)               , ch(50),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "EMPLOYMENT DATES, FROM/TO",                           ~
               at (06,30), fac(lfac$( 1)), fromdate$(a%)        , ch(08),~
               at (06,40), fac(lfac$( 1)), todate$(a%)          , ch(08),~
               at (07,02),                                               ~
                  "JOB TITLE HELD",                                      ~
               at (07,30), fac(lfac$( 2)), jobtitle$(a%)        , ch(16),~
               at (07,49), fac(hex(8c)),str(pass$,17,30)        , ch(30),~
               at (08,02),                                               ~
                  "JOB CLASSIFICATION - EEOC",                           ~
               at (08,30), fac(lfac$( 3)), jobclass$(a%)        , ch(16),~
               at (08,49), fac(hex(8c)),str(pass1$,17,30)       , ch(30),~
               at (09,02),                                               ~
                  "RATE OF PAY ($ & PER)",                               ~
               at (09,30), fac(lfac$( 4)), pay$(a%)             , ch(10),~
               at (09,45), fac(lfac$( 4)), payfor$(a%)          , ch(30),~
               at (10,02),                                               ~
                  "NAME OF SUPERVISOR",                                  ~
               at (10,30), fac(lfac$( 5)), supervisor$(a%)      , ch(30),~
               at (11,02),                                               ~
                  "NAME OF MANAGER",                                     ~
               at (11,30), fac(lfac$( 6)), manager$(a%)         , ch(30),~
               at (12,02),                                               ~
                  "LAST REVIEW DATE",                                    ~
               at (12,30), fac(lfac$( 7)), lrdate$(a%)          , ch(08),~
               at (13,02),                                               ~
                  "LAST REVIEW BY",                                      ~
               at (13,30), fac(lfac$( 8)), lrby$(a%)            , ch(30),~
               at (14,02),                                               ~
                  "LAST REVIEW INCREASE AMOUNT",                         ~
               at (14,30), fac(lfac$( 9)), lrincamt$(a%)        , ch(10),~
               at (15,02),                                               ~
                  "LAST REVIEW INCREASE PERCNT",                         ~
               at (15,30), fac(lfac$(10)), lrincpct$(a%)        , ch(10),~
               at (16,02),                                               ~
                  "LAST REVIEW INCREASE REASON",                         ~
               at (16,30), fac(lfac$(11)), lrincfor$(a%)        , ch(16),~
               at (17,02),                                               ~
                  "     REVIEWERS COMMENTS",                             ~
               at (17,30), fac(lfac$(12)), lrcoment$(1,a%)      , ch(50),~
               at (18,02),                                               ~
                  "     REVIEWERS COMMENTS",                             ~
               at (18,30), fac(lfac$(13)), lrcoment$(2,a%)      , ch(50),~
               at (19,02),                                               ~
                  "NEXT REVIEW DATE",                                    ~
               at (19,30), fac(lfac$(14)), nrdate$(a%)          , ch(08),~
               at (20,02),                                               ~
                  "DATE REV NOTICE SENT TO SUP",                         ~
               at (20,30), fac(lfac$(15)), nrsent$(a%)          , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)),   edtmessage$          , ch(79),~
               at (22,02),                                               ~
                  "P.F. KEYS ACTIVE:",                                   ~
               at (23,02),                                               ~
                  "(12)DELETE RECORD           (13)INSTRUCTIONS",        ~
               at (23,65),                                               ~
                  "(15)PRINT SCREEN",                                    ~
               at (24,68),                                               ~
                  "(16)SAVE DATA",                                       ~
                                                                         ~
               keys(hex(000c0d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 12% then L42110
                  kh% = 2
                  call "ASKUSER" (kh%, "DELETE RECORD?", "Press RETURN to~
        ~Delete Record", "- or -", "Press PF16 to Exit")
                  if kh% = 16 then L41350
                  if kh% <> 0 then L42230
                     goto L42230

L42110:        if keyhit% <> 13 then L42150
                  call "MANUAL" ("HISINPUT")
                  goto L41350

L42150:        if keyhit% <> 15 then L42190
                  call "PRNTSCRN"
                  goto L41350

L42190:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

L42230: for i% = a% to maxi%
            nrdate$(i%) = nrdate$(i%+1)
            jobclass$(i%) = jobclass$(i%+1)
            jobtitle$(i%) = jobtitle$(i%+1)
            fromdate$(i%) = fromdate$(i%+1)
            todate$(i%)   = todate$(i%+1)
            pay$(i%)      = pay$(i%+1)
            payper$(i%)   = payper$(i%+1)
            supervisor$(i%) = supervisor$(i%+1)
            manager$(i%)  = manager$(i%+1)
            lrdate$(i%)   = lrdate$(i%+1)
            lrby$(i%)     = lrby$(i%+1)
            lrincamt$(i%) = lrincamt$(i%+1)
            lrincpct$(i%) = lrincpct$(i%+1)
            lrincfor$(i%) = lrincfor$(i%+1)
            lrcoment$(1%, i%) = lrcoment$(1%, i%+1)
            lrcoment$(2%, i%) = lrcoment$(2%, i%+1)
            nrsent$(i%)   = nrsent$(i%+1)
            filler$(i%)   = filler$(i%+1)
        next i%

           maxi% = maxi% - 1
           goto summaryscreen

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100,         /* FROM/TO DATES    */~
                                    L50150,         /* JOB TITLE HELD   */~
                                    L50200,         /* JOB CLASS        */~
                                    L50250,         /* RATE OF PAY      */~
                                    L50300,         /* SUPERVISOR       */~
                                    L50350,         /* NAME OF MANAGER  */~
                                    L50400,         /* LAST REVIEW DATE */~
                                    L50450,         /* LAST REVIEW BY   */~
                                    L50500,         /* LAST INCREASE    */~
                                    L50550,         /* LAST PERCENT     */~
                                    L50600,         /* LAST REASON      */~
                                    L50650,         /* COMMENTS         */~
                                    L50700,         /* COMMENTS         */~
                                    L50750,         /* NEXT REVIEW DATE */~
                                    L50800          /* DATE SENT TO SUPR*/
                     return
L50100:     REM TEST DATA FOR EMPLOYMENT DATES, FROM/TO
           if fromdate$(a%) = " " or fromdate$(a%) = blankdate$ then L50121
            call "DATEOK" (fromdate$(a%), err%, errormsg$)
           call "RJUSTIFY" (errormsg$)
L50121:    if todate$(a%) = " " or todate$(a%) = blankdate$ then return
            call "DATEOK" (  todate$(a%), err%, errormsg$)
           call "RJUSTIFY" (errormsg$)
                return
L50150:     REM TEST DATA FOR JOB TITLE HELD
           init(" ") pass$
           str(pass$,47,16) = str(jobtitle$(a%) ,1,16)
L50156:    call "COMSUB" (#4, 1%, pass$, 1%, rc%)
           if rc% =  0% then goto L50156
           if str(pass$,1,16) =                                          ~
                        "JOB TITLE       " then goto L50174
        errormsg$ = "THE TERM " & str(jobtitle$(a%),1,len(jobtitle$(a%)))~
             & " IS NOT A JOB TITLE, PLEASE REENTER"
           init(" ") jobtitle$(a%)
           call "RJUSTIFY" (errormsg$)
                     return
L50174:    str(jobtitle$(a%) ,1,16) = str(pass$,47,16)
           call "RJUSTIFY" (str(pass$,63,50))
           call "RJUSTIFY" (str(pass$,113,50))
                return
L50200:     REM TEST DATA FOR JOB CLASSIFICATION - EEOC
           init(" ") pass1$
           str(pass1$,47,16) = str(jobclass$(a%) ,1,16)
L50206:    call "COMSUB" (#4, 1%, pass1$, 1%, rc%)
           if rc% =  0% then goto L50206
           if str(pass1$,1,16) =                                         ~
                        "EEO JOB CLASS   " then goto L50224
        errormsg$ = "THE TERM " & str(jobclass$(a%),1,len(jobclass$(a%)))~
             & " IS NOT AN EEO CLASS, PLEASE REENTER"
           init(" ") jobclass$(a%)
           call "RJUSTIFY" (errormsg$)
                     return
L50224:    str(jobclass$(a%) ,1,16) = str(pass1$,47,16)
                return
L50250:     REM TEST DATA FOR RATE OF PAY ($ & PER)
            if pay$(a%) = " " then pay$(a%) = "0"
            convert pay$(a%)  to  d, data goto L50296
            convert d to pay$(a%), pic($######.##)
                return
L50296:    errormsg$ = "PLEASE ENTER PAY AS DOLLARS AND CENTS"
                return
L50300:     REM TEST DATA FOR NAME OF SUPERVISOR
                return
L50350:     REM TEST DATA FOR NAME OF MANAGER
                return
L50400:     REM TEST DATA FOR LAST REVIEW DATE
           if lrdate$(a%) = " " or lrdate$(a%) = blankdate$ then return
            call "DATEOK" (lrdate$(a%), err%, errormsg$)
           call "RJUSTIFY" (errormsg$)
                return
L50450:     REM TEST DATA FOR LAST REVIEW BY
                return
L50500:     REM TEST DATA FOR LAST REVIEW INCREASE AMOUNT
            if lrincamt$(a%) = " " then lrincamt$(a%) = "0"
            convert lrincamt$(a%)  to  d, data goto L50525
            convert d to lrincamt$(a%), pic($######.##)
                return
L50525:    errormsg$ = "PLEASE ENTER AMOUNT AS DOLLARS AND CENTS"
                return
L50550:     REM TEST DATA FOR LAST REVIEW INCREASE PERCNT
            if lrincpct$(a%) = " " then lrincpct$(a%) = "0"
            convert lrincpct$(a%)  to  d, data goto L50575
            convert d to lrincpct$(a%), pic(#######.##)
                return
L50575:    errormsg$ = "PLEASE ENTER PERCENTAGE AS NUMBERS"
                return
L50600:     REM TEST DATA FOR LAST REVIEW INCREASE REASON
                return
L50650:     REM TEST DATA FOR      REVIEWERS COMMENTS
                return
L50700:     REM TEST DATA FOR      REVIEWERS COMMENTS
                return
L50750:     REM TEST DATA FOR NEXT REVIEW DATE
           if nrdate$(a%) = " " or nrdate$(a%) = blankdate$ then  return
            call "DATEOK" (nrdate$(a%), err%, errormsg$)
           call "RJUSTIFY" (errormsg$)
                return
L50800:     REM TEST DATA FOR DATE REV NOTICE SENT TO SUP
           if nrsent$(a%) = " " or nrsent$(a%) = blankdate$ then  return
            call "DATEOK" (nrsent$(a%), err%, errormsg$)
           call "RJUSTIFY" (errormsg$)
                return

        summaryscreen

           init(" ") errormsg$
           screen% = 0%


L51055: accept                                                           ~
               at (01,03),                                               ~
        "+---------------------------------------------------------------~
        ~------------+",                                                  ~
               at (02,03),                                               ~
        "! EMPLOYMENT HISTORY FOR ",                                     ~
               at (02,28),                                               ~
         fac(hex(84)), name$, ch(30) ,                                   ~
               at (02,60),                                               ~
        "NEXT REV",                                                      ~
               at (02,69),                                               ~
         fac(hex(84)), nrdate$(maxi%),      ch(8),                       ~
               at (02,79),                                               ~
        "!",                                                             ~
               at (03,03),                                               ~
        "+---------------------------------------------------------------~
        ~------------+",                                                  ~
               at (04,03),                                               ~
        "!  JOB TITLE HELD     FROM DATE      TO DATE   RATE OF PAY    EE~
        ~O JOB CLASS !",                                                  ~
               at (05,03),                                               ~
        "!                                                               ~
        ~            !",                                                  ~
               at (06,03),                                               ~
        "!",                                                             ~
               at (06,79),                                               ~
        "!",                                                             ~
               at (07,03),                                               ~
        "!",                                                             ~
               at (07,79),                                               ~
        "!",                                                             ~
               at (08,03),                                               ~
        "!",                                                             ~
               at (08,79),                                               ~
        "!",                                                             ~
               at (09,03),                                               ~
        "!",                                                             ~
               at (09,79),                                               ~
        "!",                                                             ~
               at (10,03),                                               ~
        "!",                                                             ~
               at (10,79),                                               ~
        "!",                                                             ~
               at (11,03),                                               ~
        "!",                                                             ~
               at (11,79),                                               ~
        "!",                                                             ~
               at (12,03),                                               ~
        "!",                                                             ~
               at (12,79),                                               ~
        "!",                                                             ~
               at (13,03),                                               ~
        "!",                                                             ~
               at (13,79),                                               ~
        "!",                                                             ~
               at (14,03),                                               ~
        "!",                                                             ~
               at (14,79),                                               ~
        "!",                                                             ~
               at (15,03),                                               ~
        "!",                                                             ~
               at (15,79),                                               ~
        "!",                                                             ~
               at (16,03),                                               ~
        "!",                                                             ~
               at (16,79),                                               ~
        "!",                                                             ~
               at (17,03),                                               ~
        "!",                                                             ~
               at (17,79),                                               ~
        "!",                                                             ~
               at (18,03),                                               ~
        "!",                                                             ~
               at (18,79),                                               ~
        "!",                                                             ~
               at (19,03),                                               ~
        "!",                                                             ~
               at (19,79),                                               ~
        "!",                                                             ~
               at (20,03),                                               ~
        "!",                                                             ~
               at (20,79),                                               ~
        "!",                                                             ~
               at (21,03),                                               ~
        "!",                                                             ~
               at (21,79),                                               ~
        "!",                                                             ~
               at (22,03),                                               ~
        "+---------------------------------------------------------------~
        ~------------+",                                                  ~
               at (23,03),                                               ~
        "CURSOR TO LINE & (ENTER)TO EDIT/DELETE JOB RECORDS     (1)FIRST ~
        ~ (5)NEXT PAGE",                                                  ~
               at (24,03),                                               ~
        "(8)TO ADD ANOTHER JOB RECORD   (15)PRINT SCREEN       (16)SAVE J~
        ~OB RECS SHOWN",                                                  ~
           at(05,05), fac(hex(94)), errormsg$                   , ch(73),~
           at(06,06), fac(hex(8c)), jobtitle$    (screen% +  1%), ch(16),~
           at(06,26), fac(hex(8c)), fromdate$    (screen% +  1%), ch(08),~
           at(06,39), fac(hex(8c)), todate$      (screen% +  1%), ch(08),~
           at(06,50), fac(hex(8c)), pay$         (screen% +  1%), ch(10),~
           at(06,66), fac(hex(8c)), jobclass$    (screen% +  1%), ch(08),~
           at(07,06), fac(hex(8c)), jobtitle$    (screen% +  2%), ch(16),~
           at(07,26), fac(hex(8c)), fromdate$    (screen% +  2%), ch(08),~
           at(07,39), fac(hex(8c)), todate$      (screen% +  2%), ch(08),~
           at(07,50), fac(hex(8c)), pay$         (screen% +  2%), ch(10),~
           at(07,66), fac(hex(8c)), jobclass$    (screen% +  2%), ch(08),~
           at(08,06), fac(hex(8c)), jobtitle$    (screen% +  3%), ch(16),~
           at(08,26), fac(hex(8c)), fromdate$    (screen% +  3%), ch(08),~
           at(08,39), fac(hex(8c)), todate$      (screen% +  3%), ch(08),~
           at(08,50), fac(hex(8c)), pay$         (screen% +  3%), ch(10),~
           at(08,66), fac(hex(8c)), jobclass$    (screen% +  3%), ch(08),~
           at(09,06), fac(hex(8c)), jobtitle$    (screen% +  4%), ch(16),~
           at(09,26), fac(hex(8c)), fromdate$    (screen% +  4%), ch(08),~
           at(09,39), fac(hex(8c)), todate$      (screen% +  4%), ch(08),~
           at(09,50), fac(hex(8c)), pay$         (screen% +  4%), ch(10),~
           at(09,66), fac(hex(8c)), jobclass$    (screen% +  4%), ch(08),~
           at(10,06), fac(hex(8c)), jobtitle$    (screen% +  5%), ch(16),~
           at(10,26), fac(hex(8c)), fromdate$    (screen% +  5%), ch(08),~
           at(10,39), fac(hex(8c)), todate$      (screen% +  5%), ch(08),~
           at(10,50), fac(hex(8c)), pay$         (screen% +  5%), ch(10),~
           at(10,66), fac(hex(8c)), jobclass$    (screen% +  5%), ch(08),~
           at(11,06), fac(hex(8c)), jobtitle$    (screen% +  6%), ch(16),~
           at(11,26), fac(hex(8c)), fromdate$    (screen% +  6%), ch(08),~
           at(11,39), fac(hex(8c)), todate$      (screen% +  6%), ch(08),~
           at(11,50), fac(hex(8c)), pay$         (screen% +  6%), ch(10),~
           at(11,66), fac(hex(8c)), jobclass$    (screen% +  6%), ch(08),~
           at(12,06), fac(hex(8c)), jobtitle$    (screen% +  7%), ch(16),~
           at(12,26), fac(hex(8c)), fromdate$    (screen% +  7%), ch(08),~
           at(12,39), fac(hex(8c)), todate$      (screen% +  7%), ch(08),~
           at(12,50), fac(hex(8c)), pay$         (screen% +  7%), ch(10),~
           at(12,66), fac(hex(8c)), jobclass$    (screen% +  7%), ch(08),~
           at(13,06), fac(hex(8c)), jobtitle$    (screen% +  8%), ch(16),~
           at(13,26), fac(hex(8c)), fromdate$    (screen% +  8%), ch(08),~
           at(13,39), fac(hex(8c)), todate$      (screen% +  8%), ch(08),~
           at(13,50), fac(hex(8c)), pay$         (screen% +  8%), ch(10),~
           at(13,66), fac(hex(8c)), jobclass$    (screen% +  8%), ch(08),~
           at(14,06), fac(hex(8c)), jobtitle$    (screen% +  9%), ch(16),~
           at(14,26), fac(hex(8c)), fromdate$    (screen% +  9%), ch(08),~
           at(14,39), fac(hex(8c)), todate$      (screen% +  9%), ch(08),~
           at(14,50), fac(hex(8c)), pay$         (screen% +  9%), ch(10),~
           at(14,66), fac(hex(8c)), jobclass$    (screen% +  9%), ch(08),~
           at(15,06), fac(hex(8c)), jobtitle$    (screen% + 10%), ch(16),~
           at(15,26), fac(hex(8c)), fromdate$    (screen% + 10%), ch(08),~
           at(15,39), fac(hex(8c)), todate$      (screen% + 10%), ch(08),~
           at(15,50), fac(hex(8c)), pay$         (screen% + 10%), ch(10),~
           at(15,66), fac(hex(8c)), jobclass$    (screen% + 10%), ch(08),~
           at(16,06), fac(hex(8c)), jobtitle$    (screen% + 11%), ch(16),~
           at(16,26), fac(hex(8c)), fromdate$    (screen% + 11%), ch(08),~
           at(16,39), fac(hex(8c)), todate$      (screen% + 11%), ch(08),~
           at(16,50), fac(hex(8c)), pay$         (screen% + 11%), ch(10),~
           at(16,66), fac(hex(8c)), jobclass$    (screen% + 11%), ch(08),~
           at(17,06), fac(hex(8c)), jobtitle$    (screen% + 12%), ch(16),~
           at(17,26), fac(hex(8c)), fromdate$    (screen% + 12%), ch(08),~
           at(17,39), fac(hex(8c)), todate$      (screen% + 12%), ch(08),~
           at(17,50), fac(hex(8c)), pay$         (screen% + 12%), ch(10),~
           at(17,66), fac(hex(8c)), jobclass$    (screen% + 12%), ch(08),~
           at(18,06), fac(hex(8c)), jobtitle$    (screen% + 13%), ch(16),~
           at(18,26), fac(hex(8c)), fromdate$    (screen% + 13%), ch(08),~
           at(18,39), fac(hex(8c)), todate$      (screen% + 13%), ch(08),~
           at(18,50), fac(hex(8c)), pay$         (screen% + 13%), ch(10),~
           at(18,66), fac(hex(8c)), jobclass$    (screen% + 13%), ch(08),~
           at(19,06), fac(hex(8c)), jobtitle$    (screen% + 14%), ch(16),~
           at(19,26), fac(hex(8c)), fromdate$    (screen% + 14%), ch(08),~
           at(19,39), fac(hex(8c)), todate$      (screen% + 14%), ch(08),~
           at(19,50), fac(hex(8c)), pay$         (screen% + 14%), ch(10),~
           at(19,66), fac(hex(8c)), jobclass$    (screen% + 14%), ch(08),~
           at(20,06), fac(hex(8c)), jobtitle$    (screen% + 15%), ch(16),~
           at(20,26), fac(hex(8c)), fromdate$    (screen% + 15%), ch(08),~
           at(20,39), fac(hex(8c)), todate$      (screen% + 15%), ch(08),~
           at(20,50), fac(hex(8c)), pay$         (screen% + 15%), ch(10),~
           at(20,66), fac(hex(8c)), jobclass$    (screen% + 15%), ch(08),~
           at(21,06), fac(hex(8c)), jobtitle$    (screen% + 16%), ch(16),~
           at(21,26), fac(hex(8c)), fromdate$    (screen% + 16%), ch(08),~
           at(21,39), fac(hex(8c)), todate$      (screen% + 16%), ch(08),~
           at(21,50), fac(hex(8c)), pay$         (screen% + 16%), ch(10),~
           at(21,66), fac(hex(8c)), jobclass$    (screen% + 16%), ch(08),~
           keys(hex(000105080f10)), key(hitkey%)

           init(" ") errormsg$

           if hitkey% <> 15% then goto L52865
                     call "PRNTSCRN"
                     goto L51055

L52865:    if hitkey% <> 16% then goto L52895
                     goto datasave

L52895:    if hitkey% <> 1% then goto L52935
                     screen% = 0%
                     goto L51055

L52935:    if hitkey% <> 5% then goto L52985
                     screen% = screen% + 16%
                     screen% = min(screen%, max(0%, maxi% - 16%))
                     goto L51055

L52985:    if hitkey% <> 8% then goto L53065
                     if maxi% < 99% then goto L53035
                     errormsg$ = "YOU HAVE ALREADY ENTERED THE MAXIMUM OF~
        ~ 99 JOB RECORDS FOR THIS EMPLOYEE"
                     goto L51055
L53035:              maxi%, a% = maxi% + 1%
                     goto inputmode

L53065:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())

           a% = cursor%(1%) - 5% + screen%
           if a% < screen% + 1% or a% > screen% + 16% then goto L51055
           if a% > maxi% then goto L51055
           init(" ") pass$
           str(pass$,47,16) = str(jobtitle$(a%) ,1,16)
           call "COMSUB" (#4, 9%, pass$, 0%, rc%)
           call "RJUSTIFY" (str(pass$,63,50))
           call "RJUSTIFY" (str(pass$,113,50))
           goto editmode

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

            end
