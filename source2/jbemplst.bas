        REM *************************************************************~
            *                                                           *~
            *  JJJJJ  BBBB   EEEEE  M   M  PPPP   L       SSS   TTTTT   *~
            *    J    B   B  E      MM MM  P   P  L      S        T     *~
            *    J    BBBB   EEEE   M M M  PPPP   L       SSS     T     *~
            *  J J    B   B  E      M   M  P      L          S    T     *~
            *   J     BBBB   EEEEE  M   M  P      LLLLL   SSS     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBEMPLST - LIST SFC EMPLOYEE MASTER INFORMATION, AND      *~
            *            EARNING TYPES                                  *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and proprie- *~
            * tary assets of CAELUS, INCORPORATED, Spokane, WA,    em-  *~
            * bodying substantial creative efforts  and confidential    *~
            * information.  Unauthorized use, copying, decompiling,     *~
            * translating, disclosure, or transfer of it is prohibited. *~
            * Copyright (c) 1983, an unpublished work by CAELUS,        *~
            * INCORPORATED, Spokane, Wa.  All rights reserved.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/28/92 ! ORIGINAL (Largely Cloned From PRLELIST)  ! JBK *~
            * 11/30/92 ! PRR 12687  Only one CMS2V$ please.       ! JDH *~
            * 12/21/93 ! Minor mods for UNIX printer.             ! JDH *~
            CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL

        dim                                                              ~
            cursor%(2),                  /* CURSOR COLUMN LOCATION     */~
            cmpname$60,                  /* COMPANY NAME FOR REPORT    */~
            date$8,                      /* SCREEN DATE                */~
            dept$4,                      /* DEPARTMENT CODE            */~
            deptcode$4,                  /* DITTO                      */~
            descr$(5)32,                 /* DESCRIPTIONS               */~
            empcode$12,                  /* EMPLOYEE CODE              */~
            empname$90,                  /* EMPLOYEE NAME FORMATTED    */~
            erntype$12,                  /* EARNINGS TYPE              */~
            errormsg$79,                 /* ERROR MESSAGE TEXT         */~
            expacct$16,                  /* EXPENSE ACCOUNT            */~
            firstone$12, firstemp$12,    /* FIRST NAME IN LIST         */~
            fname$10,                    /* Name - First               */~
            i$(24)80,                    /* SCREEN IMAGE WORK AREA     */~
            inpmessage$79,               /* INPUT MESSAGE TEXT         */~
            jrate$10,                    /* Employee's Rate Chg to Job */~
            keydescr$50,                 /* Plowcode Header            */~
            lastone$12, lastemp$12,      /* LAST EMPLOYEE CODE LISTED  */~
            lfac$(20)1,                  /* FIELD DISPLAY FACS         */~
            line2$79,                    /* Screen Line #2             */~
            lname$15,                    /* Name - Last                */~
            mname$2,                     /* Name - Middle              */~
            overacct$12,                 /* Applied Overhead Account   */~
            overacctdescr$32,            /* Applied Overhead Acct Descr*/~
            pf16$16,                     /* PF KEY 16 VARIABLE         */~
            plowkey$12,                  /* PLOW VARIABLE              */~
            plowkey2$50,                 /* ANOTHER PLOW VARIABLE      */~
            rate$10,                     /* PAY RATE                   */~
            readkey$50,                  /* A Read Key                 */~
            rpttitle$80,                 /* REPORT TITLE               */~
            selected$1,                  /* SELECTED EMPLOYMENT STATUS */~
            shift$1,                     /* Employee's Shift           */~
            sortby$1,                    /* Sort Parameter             */~
            ssnum$15,                    /* SOCIAL SECURITY NUMBER     */~
            status$1,                    /* EMPLOYEE STATUS            */~
            statusdescr$30,              /* STATUS DESCRIPTION         */~
            time$8,                      /* System Time                */~
            title$16,                    /* JOB TITLE                  */~
            unit$6,                      /* UNIT DESCRIPTION           */~
            wrkstn$4                     /* LABOR CLASS CODE           */~

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64)                      /* RECORD-ON-FILE FLAGS       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLE  F2%()            SHOULD NOT BE   */
                     /* MODIFIED.     IT  IS AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 2 ! EMPMASTR ! EMPLOYEE FILE MASTER RECORDS.            *~
            * # 3 ! EMPEARN1 ! EMPLOYEE EARNINGS DETAILS FILE           *~
            * # 5 ! GLMAIN   ! GENERAL LEDGER MAIN ACCOUNT FILE.        *~
            * # 6 ! PRLDEPTF ! PAYROLL DEPARTMENT CODE FILE             *~
            * # 8 ! GENCODES ! General Codes File                       *~
            * #14 ! PERMASTR ! PERSONNEL EMPLOYEE MASTER FILE           *~
            *************************************************************

            select # 2, "EMPMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 136,                                  ~
                         keypos = 1, keylen = 12,                        ~
                         alt key  1, keypos = 70, keylen =  1, dup

            select # 3, "EMPEARN1",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 200,                                  ~
                         keypos = 1, keylen = 15,                        ~
                         alternate key  1, keypos =  16, keylen = 28

            select # 5, "GLMAIN",                                        ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 300,                                  ~
                         keypos = 1, keylen = 9

            select # 6, "PRLDEPTF",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 300,                                  ~
                         keypos = 1, keylen = 4

            select # 8, "GENCODES",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 128,                                  ~
                         keypos = 1, keylen = 24

            select #14, "PERMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 950,                                   ~
                        keypos = 39, keylen = 12,                        ~
                        alt key  1, keypos =  28, keylen = 23,           ~
                            key  2, keypos =   2, keylen = 49,           ~
                            key  3, keypos =   1, keylen = 50

            call "PRLEXTSB" ("SFC", ret%)
                if ret% = 99% then L65000

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (# 2, 0%, f2%( 2),   0%, " ")
            call "OPENCHCK" (# 3, 0%, f2%( 3),   0%, " ")
            call "OPENCHCK" (# 5, 0%, f2%( 5),   0%, " ")
            call "OPENCHCK" (# 6, 0%, f2%( 6),   0%, " ")
            call "OPENCHCK" (# 8, 0%, f2%( 8),   0%, " ")
            call "OPENCHCK" (#14, 0%, f2%(14),   0%, " ")

        REM *************************************************************~
            *      I N I T I A L I Z A T I O N                          *~
            *                                                           *~
            * SET SYSTEM DATE                                           *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)

            str(line2$,62,9) = "JBEMPLST:"
            str(line2$,72,8) = str(cms2v$,1,8)

            call "COMPNAME" (12%, cmpname$, ret%)
            ret% = 0
            rpttitle$ = "S F C  E M P L O Y E E  M A S T E R  L I S T I N~
        ~ G"
L10000: REM *************************************************************~
            *         M A I N   P R O G R A M                           *~
            *                                                           *~
            * MAIN PROGRAM CONTROLLER SECTION                           *~
            *************************************************************

            init(" ") firstone$, lastone$, selected$, sortby$, errormsg$,~
                      lastemp$
            editmode% = 0
            u3% = 0
            firstemp$ = "ALL"

L10110:     for fieldnr% = 1 to 3
              gosub'051(fieldnr%)
                if enabled% = 0 then L10110
L10140:       gosub'101(fieldnr%)
                if keyhit% = 1  then gosub startover
                if keyhit% = 16 then L65000
                if keyhit% <> 0 then L10140
              gosub'151(fieldnr%)
                if errormsg$ <> " " then L10140
              next fieldnr%


L10230:      errormsg$ = " "
         inpmessage$ = "To Modify Displayed Values, Position Cursor To De~
        ~sired Value And Press RETURN."
            editmode% = 1

L10280:     gosub'111(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       main_report
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% > 3 then L10280

            gosub'051(fieldnr%)
                  if enabled% =  0 then L10230
L10360:     gosub'111(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then       L10360
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L10360
            goto L10230

        main_report
            REM INITIALIZE VARIABLES
                pkey% = 0
                if sortby$ = "N" then pkey% = 2%
                if sortby$ = "S" then pkey% = 3%
                select printer(134)
                pagenumber% = -1
                time$ = " "  :  call "TIME" (time$)
                call "SETPRNT" ("JB0012", " ", 0%, 0%)

        REM Set Read Key for files
            if pos(firstone$ <> hex(00)) <> 0%  then                     ~
                                         firstone$ = addc all(hex(ff))
            plowkey$ = firstone$
            if sortby$ <> "E" then plowkey$ = all(hex(00))

            call "SHOSTAT" ("Printing Employee Listing")
            call "PLOWALTS" (#14, plowkey$, pkey%, 0%, f1%(14))
                if f1%(14) = 0 then L10920
            goto L10640

L10610:     REM PLOW THROUGH PERSONNEL MASTER FILE
                call "READNEXT" (#14, f1%(14))
                     if f1%(14) = 0 then L10920
L10640:         if selected$ = "A" then L10670
                     get #14, status$
                     if status$ <> selected$ then L10610
L10670:         plowkey$ = key(#14, 0)
                if sortby$ = "E" and plowkey$ > lastone$ then L10920
                if plowkey$ > lastone$ then L10610
                if plowkey$ < firstone$ then L10610
                call "READ100" (#2, plowkey$, f1%(2))
                     if f1%(2) = 0 then L10610

            REM MASTER INFORMATION
                gosub L11000

            REM EARNINGS TYPES
                earncount% = 0%
                gosub L13000

            REM NEXT EMPLOYEE
                go to L10610

L10920:     REM DONE WITH EMPLOYEES
                empname$ = " "
                if pagenumber% <  0% then gosub page_heading
                if pageline%   > 55% then gosub page_heading
                    print skip(2)
                    time$ = " "  :  call "TIME" (time$)
                    print using L61040, time$
                    close printer
                    call "SETPRNT" ("JB0012", " ", 0%, 1%)
                    goto L65000

L11000: REM *************************************************************~
            *         M A S T E R   P R I N T E R                       *~
            *                                                           *~
            * GET AND PRINT MASTER INFORMATION                          *~
            *************************************************************

            REM GET EMPLOYEE INFO
                gosub L30000

            gosub page_heading
            print using L60980, "EMPLOYEE CODE                 ",         ~
                               empcode$
            pageline% = pageline% + 1
            print using L60980, "EMPLOYEE NAME:  FIRST         ",         ~
                              fname$
            if mname$ = " " then L11190
            pageline% = pageline% + 1
            print using L60980, "                MIDDLE        ",         ~
                              mname$
L11190:     pageline% = pageline% + 1
            print using L60980, "                LAST          ",         ~
                              lname$
            pageline% = pageline% + 1
            print using L60980, "SOCIAL SECURITY NUMBER        ",         ~
                               ssnum$
            pageline% = pageline% + 1
            print using L61010, "DEPARTMENT                    ",         ~
                               dept$, descr$(1)
            if title$ = " " then L11690
            pageline% = pageline% + 1
            print using L60980, "JOB  TITLE                    ",         ~
                               title$
L11690:     if wrkstn$ = " " then L11770
            pageline% = pageline% + 1
            print using L61010, "NORMAL LABOR CLASS (JOBCOSTNG)",         ~
                               wrkstn$, descr$(3)
L11770:     pageline% = pageline% + 1
            print using L61010, "EMPLOYEE STATUS               ",         ~
                               status$, statusdescr$
            pageline% = pageline% + 1
            if overacct$ = " " then L11860
            print using L61010, "APPLIED OVERHEAD ACCOUNT      ",         ~
                               overacct$, overacctdescr$
            pageline% = pageline% + 1
L11860:     if jrate = 0 then L11880
            print using L60980, "RATE FOR EXTERNAL JOB POSTING  ",        ~
                               jrate$
            pageline% = pageline% + 1
L11880:     if shift = 0 then L11900
            print using L60980, "SHIFT                          ",        ~
                               shift$
            pageline% = pageline% + 1

L11900:     print  :  print
            pageline% = pageline% + 2%

            return

L13000: REM *************************************************************~
            *     E A R N I N G S   T Y P E   P R I N T E R             *~
            *                                                           *~
            * GET AND PRINT EARNINGS TYPES                              *~
            *************************************************************

            REM SET PLOW VARIABLE
                plowkey2$ = empcode$

            REM PLOW DOWN EARNINGS
L13100:         call "PLOWNEXT" (#3, plowkey2$, 12%, f1%(3))
                                 if f1%(3) = 0 then L13250
            REM GET INFO
                gosub L32000

            REM HEADER AND THEN PRINT
                gosub earn_heading_check
                print using L60860, erntype$, deptcode$, unit$, rate$,    ~
                                   expacct$
                pageline% = pageline% + 1%
                earncount% = earncount% + 1%

            REM LOOP AROUND
                go to L13100

L13250: REM END OF EARNINGS
            if earncount% > 0% then gosub earn_trailer
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets defaults and enables fields for page 1 of the screen *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 0
                  on fieldnr% gosub L20140,         /* Employee Range   */~
                                    L20240,         /* Selected Status  */~
                                    L20310          /* Sort Option      */
                     return

L20140:     REM DEFAULT/ENABLE FOR EMPLOYEE SELECT RANGE
                inpmessage$ = "Enter starting & ending Employee "  &     ~
                   "Codes, or 'ALL' to list All Employees"
                enabled% = 1
                return

L20240:     REM DEFAULT/ENABLE FOR SELECTED EMPLOYMENT STATUS
                inpmessage$ = "A=All C=Current P=Previous L=Leave of Abse~
        ~nce M=Military Leave N=Terminated"
                if selected$ = " " then selected$ = "A"
                enabled% = 1
                return

L20310:     REM DEFAULT/ENABLE FOR SELECTED Sort Option
                inpmessage$ = "E=By Employee Number, N=By Employee Name, ~
        ~S=By Status."
                if sortby$ = " " then sortby$ = "E"
                enabled% = 1
                return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

            ask% = 2%
            call "STARTOVR" (ask%)
            if ask% = 1% then return

            return clear all
            goto L10000

L30000: REM *************************************************************~
            *  G E T   M A S T E R  I N F O R M A T I O N               *~
            *                                                           *~
            * GET MASTER INFORMATION, AND FORMAT DATA                   *~
            *************************************************************

            overacct$ = " "  :  jrate, shift = 0

            get #2, using L30155, empcode$, wrkstn$, dept$, overacct$,    ~
                                 jrate, shift

L30155:     FMT CH(12),                  /* Employee Code              */~
                CH(4),                   /* LABOR CLASS CODE           */~
                XX(75),                  /* Filler                     */~
                CH(4),                   /* DEPARTMENT                 */~
                CH(9),                   /* Overhead Account           */~
                XX(16),                  /* Filler                     */~
                PD(14,4),                /* Rate for extrnal jb posting*/~
                BI(1),                   /* Shift                      */~
                XX(7)                    /* Filler                     */

            status$,             /* General purpose status indicator   */~
            ssnum$,              /* Social security number             */~
            title$               /* Current job title - personnel syst */~
                       = all("*")

            lname$ = "NOT IN PERSONNEL"  /* Really can't happen */
            fname$ = "FILE"
            mname$ = " "

         if f1%(14) = 1 then get #14, using L30550,   /* FILE: PERMASTR */~
            status$,             /* General purpose status indicator   */~
            lname$,              /* Last name of person - part of pers */~
            fname$,              /* First name of person               */~
            mname$,              /* Middle name of person              */~
            ssnum$,              /* Social security number             */~
            title$               /* Current job title - personnel syst */

L30550: FMT                      /* FILE: PERMASTR                     */~
            CH(1),               /* General purpose status indicator   */~
            CH(15),              /* Last name of person - part of pers */~
            CH(10),              /* First name of person               */~
            CH(1),               /* Middle name of person              */~
            CH(11),              /* Social security number             */~
            XX(12),              /* employee code                      */~
            XX(250),             /* Filler                             */~
            XX(36),              /* Filler                             */~
            CH(16),              /* Current job title - personnel syst */~
            XX(16),              /* Filler                             */~
            XX(4),               /* Current department                 */~
            XX(87),              /* Filler                             */~
            XX(200),             /* Variable Fields                    */~
            XX(250),             /* Filler                             */~
            XX(41)               /* Filler                             */

            REM FORMAT THE NAME
            if mname$ <> " " then str(mname$,2) = "."
            if mname$ <> " " then empname$=lname$&", "&fname$&" " &mname$~
                                 else  empname$ = lname$ & " " &  fname$

            REM FORMAT DEPARTMENT and  LABOR CLASS
                call "DESCRIBE" (#6, dept$,  descr$(1), 1%, f1%(6))
                readkey$ = "LBR CLASS" & wrkstn$
                call "DESCRIBE" (#8, readkey$, descr$(3), 1%, f1%(8))

            REM FORMAT STATUS
                statusdescr$ = "(Inactive)"
                if status$ = "C" then statusdescr$ = "(Active)"
                if status$ = "L" then statusdescr$ = "(Leave of Absence)"
                if status$ = "M" then statusdescr$ = "(Military Leave)"
                if status$ = "P" then statusdescr$ = "(Previous Employee)"
                if status$ = "N" then statusdescr$ = "(Terminated)"

        REM FORMAT GL ACCTS
            call "DESCRIBE" (#5, overacct$, overacctdescr$, 1%, f1%(5))
            call "GLFMT" (overacct$)

        REM FORMAT RATE AND SHIFT
            call "CONVERT" (jrate, -2.4, jrate$)
            convert shift to shift$, pic(#)

            REM AND RETURN
                return

L32000: REM *************************************************************~
            *        G E T   E A R N I N G   R E C O R D                *~
            *                                                           *~
            * RETRIEVE EARNINGS RECORD FROM BUFFER                      *~
            *************************************************************

            get #3, using L32100, erntype$, deptcode$, unit$, rate,       ~
                                 expacct$
            call "GLFMT" (expacct$)

L32100:              FMT       XX(27),             /* SKIP BEGIN       */~
                               CH(12),             /* EARNINGS TYPE    */~
                               CH(4),              /* DEPARTMENT CODE  */~
                               XX(2),              /* Filler           */~
                               CH(6),              /* UNITS DESCRIP    */~
                               PD(14,4),           /* UNIT RATE        */~
                               CH(9),              /* EXPENSE ACCT     */~
                               XX(132)             /* Filler           */

            call "CONVERT" (rate, 2.4, rate$)

            return

        REM *************************************************************~
            *   S C R E E N   F O R   R A N G E   O F   E M P L O Y E ES*~
            *                                                           *~
            * ACCEPT SCREEN FOR RANGE OF EMPLOYEES                      *~
            *************************************************************

            deffn'101(fieldnr%)
              pf16$ = "(16)Exit Program"
              goto L40069
            deffn'111(fieldnr%)
              pf16$ = "(16)Print Report"
L40069:         init(hex(8c)) lfac$()
                  on fieldnr% gosub L40140,         /* Employee Range   */~
                                    L40140,         /* Selected Status  */~
                                    L40140          /* Sort Option      */
            goto L40200

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40140:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return
L40200:     accept                                                       ~
               at (01,02),                                               ~
                  "Print SFC Employee Listing",                          ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Employee Range        From:",                         ~
               at (06,30), fac(lfac$( 1)), firstemp$            , ch(12),~
               at (06,46), "To:",                                        ~
               at (06,50), fac(lfac$( 1)), lastemp$             , ch(12),~
               at (07,02),                                               ~
                  "Employment Status",                                   ~
               at (07,30), fac(lfac$( 2)), selected$            ,  ch(1),~
               at (08,02),                                               ~
                  "Sort Order (E, N or S)",                              ~
               at (08,30), fac(lfac$( 3)), sortby$              ,  ch(1),~
                                                                         ~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65), fac(hex(8c)), pf16$                  , ch(16),~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key(keyhit%)

            if keyhit% <> 13 then L40550
                call "MANUAL" ("JBEMPLST")
                goto L40200

L40550:        if keyhit% <> 15 then L40590
                  call "PRNTSCRN"
                  go to L40200

L40590:        if editmode% = 0 then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *          T E S T   P R I N T   R A N G E                  *~
            *                                                           *~
            * TEST PRINT RANGE ENTERED                                  *~
            *************************************************************
            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50140,         /* Employee Range   */~
                                    L51000,         /* Selected Status  */~
                                    L51050          /* Sort Option      */
                    return

L50140: REM Test Data for Employee Range

        REM HANDLES CASE FOR "ALL" EMPLOYEESS
            if firstemp$ <> "ALL" then L50290
                init(hex(00)) firstone$
                init(hex(ff)) lastone$
                return

L50290: REM HANDLES CASE FOR "?"
            if firstemp$ <> "?" then L50380
                firstemp$ = " "
                gosub L50610
                if errormsg$ <> " " then return
                     lastemp$  = firstemp$
                     firstone$ = firstemp$  :  lastone$ = lastemp$
                     return

L50380: REM "HANDLES CASE FOR blank (" ")
            if firstemp$ <> " " then L50450
            gosub L50610
            if errormsg$ <> " " then return
            firstone$ = firstemp$
            goto L50500

L50450: REM "HANDLES CASE FOR non-blank CODE
            gosub L50610
            firstone$ = firstemp$
            if errormsg$ <> " " then return

L50500: REM "CHECK LAST CODE
            keydescr$ = hex(06) & "Select Last Employee to Print"
            if lastemp$ <> " " then keydescr$ = keydescr$ &              ~
                                    " or PF(16) to RETURN"
            call "GETEMPL" (#14, lastemp$, keydescr$, 0%, f1%(14))
                if f1%(14) <> 1% then L50540
                     lastone$ = lastemp$
L50540:     if lastemp$ <> " " then L50570
                lastemp$, lastone$ = firstemp$
                return
L50570:     if lastemp$ >= firstemp$ then return
                errormsg$ = "Illegal Range!  Please Re-enter."
                return

L50610: REM CHECK OUT FIRST CODE
            keydescr$ = hex(06) & "Select 1st Employee to Print"
            if firstemp$ <> " " then keydescr$ = keydescr$ &             ~
                                     " or PF(16) to RETURN"
            call "GETEMPL" (#14, firstemp$, keydescr$, 0%, f1%(14))
                if f1%(14) <> 1% then L50650
                     firstone$ = firstemp$
L50650:         if firstemp$ <> " " then return
                     errormsg$ = "First Employee Code CANNOT be Blank."
                     return
L51000:      REM TESTS EMPLOYMENT STATUS SELECTED
                 if pos("ACPLMN" = selected$) <> 0 then return
                 errormsg$ = "INVALID STATUS!  Please Respecify."
                 return

L51050:      REM TESTS SORT OPTION
                 if pos("ENS" = sortby$) <> 0 then return
                 errormsg$ = "Please Enter 'E', 'N' or 'S'."
                 return

        REM *************************************************************~
            *      H E A D E R    C O N T R O L L E R                   *~
            *                                                           *~
            * CONTROLS THE HEADER FOR THE MASTER INFO, THE EARNINGS     *~
            * TYPES RECORDS.                                            *~
            *************************************************************

        page_heading
            if pagenumber% < 0% then gosub print_params
            print page
            pagenumber% = pagenumber% + 1
            print using L60730, date$, time$, cmpname$, "JBEMPLST"
            print using L60750, rpttitle$, pagenumber%
            print
            print using L60770, empname$
            print
            pageline% = 5
            return

        earn_heading_check
            if pageline% < 56% then L60340
                if earncount% > 0% then gosub earn_trailer
                gosub page_heading
                gosub earn_head
                return

L60340:     if earncount% = 0% then gosub earn_head
            return

        earn_head
            print using L60800
            print using L60830
            print using L60800
            pageline% = pageline% + 3%
            return

        earn_trailer
            print using L60800
            pageline% = pageline% + 1%
            return

        print_params           /* Print Page Zero */
            print page
            pagenumber% = pagenumber% + 1%
            print using L60730, date$, time$, cmpname$, "JBERNDFT"
            print using L60750, rpttitle$, pagenumber%
L60542:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then L60560
                str(i$(), i%, 1%) = hex(20)
                goto L60542
L60560:     print skip(3)
            print tab(26);
            print "------------------------- Report Selection Parameters ~
        ~--------------------------"
            print
            for x% = 6% to 17% : print tab(26); i$(x%) : next x%
            print tab(26);
            print "------------------------------------------------------~
        ~--------------------------"
            return

L60730: %RUN ########   ########              ###########################~
        ~#################################                ########: JB0012
L60750: %                                         #######################~
        ~###########################                          PAGE:   ####

L60770: %          FOR EMPLOYEE #########################################~
        ~#########################

L60800: %  +-------------+------+------------------+-----------+---------~
        ~--------+

L60830: %  !EARNINGS TYPE! DEPT ! UNIT DESCRIPTION ! UNIT RATE ! GL EXPEN~
        ~SE ACCT !

L60860: %  ! ############! #### !       ######     !########## ! ########~
        ~######  !

L60980: %                  ##############################      ##########~
        ~####################

L61010: %                  ##############################      ##########~
        ~##   ########################################

L61040: %                        * * * * * * * * * *   E N D   O F   R E ~
        ~P O R T   A T   ########   * * * * * * * * * *

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")
            end
