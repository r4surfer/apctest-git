        REM *************************************************************~
            *                                                           *~
            *  JJJJJ  BBBB   TTTTT   CCC   DDDD   PPPP   TTTTT   SSS    *~
            *    J    B   B    T    C   C  D   D  P   P    T    S       *~
            *    J    BBBB     T    C      D   D  PPPP     T     SSS    *~
            *  J J    B   B    T    C   C  D   D  P        T        S   *~
            *  JJJ    BBBB     T     CCC   DDDD   P        T     SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBTCDPTS - Shop Floor Payroll Department Management       *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/25/86 ! ORIGINAL                                 ! HES *~
            * 04/14/87 ! Standard Costing Changes                 ! ERN *~
            * 04/13/88 ! Added Department Report Listing          ! MJB *~
            * 09/14/88 ! Added Lunch Time Per Shift, Lunch Task   ! BPN *~
            * 11/10/88 ! Changed definition of differentials      ! ERN *~
            *************************************************************

        dim clearacct$12,                /* WIP Clearing Account       */~
            company$60,                  /* Company Name for report    */~
            defltdif$(4)7,               /* Defaults for Differentials */~
            dept$4,                      /* Deptartment Number         */~
            descr$30,                    /* Deptartment Description    */~
            drate$1,                     /* Deptartment Rate Flag      */~
            cursor%(2),                  /* CURSOR LOCATION            */~
            date$8,                      /* SCREEN DATE                */~
            errormsg$79,                 /* ERROR MESSAGE TEXT         */~
            holdif(4),                   /* Holiday differentials      */~
            holdif$(4)7,                 /* Holiday differentials      */~
            hoverdif(4),                 /* Holiday O.T. differentials */~
            hoverdif$(4)7,               /* Holiday O.T. differentials */~
            i$(24)80,                    /* SCREEN IMAGE (NOT USED)    */~
            in$4,                        /* Clock-in fudge factor      */~
            lfac$(20)1,                  /* FAC FOR INPUT              */~
            line2$79,                    /* Screen Underline           */~
            ltask$6,                     /* Lunch Task Code            */~
            lunch$(4,2)5,                /* Lunch Time Definition      */~
            message$79,                  /* INSTRUCTION FOR INPUT      */~
            oclass$4,                    /* Overhead Labor Class       */~
            over8$1,                     /* Overtime if over 8/day?    */~
            over40$1,                    /* Overtime if over 40/week?  */~
            overdif(4),                  /* Overtime differentials     */~
            overdif$(4)7,                /* Overtime differentials     */~
            ovhd$8,                      /* Overhead percentage        */~
            ovhdacct$12,                 /* Overhead Account           */~
            pfdescr$(2)79,               /* PF Literal values          */~
            pfkeys$16,                   /* PF Actual values           */~
            rate$10,                     /* Department Pay Rate        */~
            shift$(4,2)5,                /* Shift Time Definition      */~
            shiftdif(4),                 /* Shift differentials        */~
            shiftdif$(4)7,               /* Shift differentials        */~
            yn$(3)3                      /* Yes / No literals          */

        dim f1%(10)                      /* RECORD-ON-FILE FLAGS       */


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.00.00 01/19/90 CMS2 / CMS-I Merge              "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! PRLDEPTF ! Payroll Department Master File           *~
            * # 2 ! GLMAIN   ! General Ledger Main File                 *~
            * # 3 ! GENCODES ! General Codes File                       *~
            * # 4 ! SYSFILE2 ! CMS System Information File              *~
            * # 5 ! STCLABOR ! Standard Labor Rates                     *~
            * # 6 ! JBTCCDES ! Time Card Task Codes                     *~
            *************************************************************

            select  #1, "PRLDEPTF",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 4

            select  #2, "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 9

            select  #3, "GENCODES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 128,                                   ~
                        keypos = 1, keylen = 24

            select #4, "SYSFILE2",                                       ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =  1,   keylen = 20

            select #5, "STCLABOR",                                       ~
                        varc,     indexed,  recsize =  323,              ~
                        keypos =  1,   keylen = 4

            select #6,  "JBTCCDES",                                      ~
                        varc, indexed,  recsize = 100,                   ~
                        keypos = 1, keylen = 6

            call "SHOSTAT"  ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, 0%, 0%, 100%, " ") /*Create if need be*/
            call "OPENCHCK" (#2, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#3, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#4, 0%, 0%, 0%, " ")
            call "STCFOPEN" (" ", "   S  ", #4, errormsg$, #5, #5, #5,   ~
                                                           #5, #5, #5)
            call "OPENCHCK" (#6, 0%, 0%, 0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)
            call "COMPNAME" (12%, company$, ret%)  :  ret% = 0%

            for x% = 1% to 4%
                defltdif$(x%) = " 1.0000"
            next x%


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * INPUT MODE MAIN PROGRAM.                                  *~
            *************************************************************

        inputmode
            errormsg$, message$, dept$, descr$, rate$, clearacct$, ovhd$,~
            ovhdacct$, shift$(), shiftdif$(), overdif$(), holdif$(), in$,~
            hoverdif$(), over8$, over40$, oclass$, drate$,               ~
            lunch$(), ltask$ = " "
            mat shiftdif = con : mat overdif  = con
            mat holdif   = con : mat hoverdif = con
            editmode%, rate, in, out = 0

            for fieldnr% = 1 to 15
                gosub'161(fieldnr%)
                      if enabled% = 0 then L10280
L10170:         gosub'201(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then L10235
L10200:                  fieldnr% = max(2%, fieldnr%-1%)
                         gosub'161(fieldnr%)
                         if enabled% = 0 then L10200
                         goto L10170
L10235:               if keyhit%  = 14 and fieldnr% = 1 then report_it
                      if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L10170
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10170
L10280:     next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR LINEAR SCREENS.        *~
            *************************************************************

        editmode
            editmode% = 1
            message$ = "To Modify Displayed Values, Position Cursor to De~
        ~sired Field and Press RETURN."
L11100:     gosub'201(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L11100
            fieldnr% = cursor%(1) - 4
            if fieldnr% <  2 or fieldnr% > 17 then L11100
            if fieldnr% =  6 or fieldnr% =  7 then L11100
            if fieldnr% = 14 then L11100

            if fieldnr% >= 15 then fieldnr% = fieldnr% - 1
            if fieldnr%  =  5 then fieldnr% = 6
            if fieldnr%  =  8 then fieldnr% = 9
            if fieldnr%  <  3  or  fieldnr% > 6 then L11270

            if fieldnr%   >  3 then fieldnr% = fieldnr% + 1
            if cursor%(2) > 40 then fieldnr% = fieldnr% + 1

L11270:     if fieldnr% < 1 or fieldnr% > 15 then L11100

            gosub'161(fieldnr%)
                  if enabled% = 0 then editmode
L11310:     gosub'201(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11310
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11310
            goto editmode

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            REM FIRST DELETE OLD ENTRY
                call "DELETE" (#1, dept$, 4%)

            REM NOW GO SAVE...
                gosub L31000
                lastdept$ = dept$
                goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'161(fieldnr%)
                  enabled% = 1
                  message$ = " "
                  on fieldnr% gosub L20240,         /* Department       */~
                                    L20280,         /* Description      */~
                                    L20310,         /* Department Rate  */~
                                    L20350,         /* WIP Clear Acct   */~
                                    L20390,         /* Overhead Labr Cls*/~
                                    L20430,         /* Overhead Account */~
                                    L20460,         /* Use Dprtmnt Rate?*/~
                                    L20492,         /* Lunch Task Code  */~
                                    L20500,         /* Shift Start/Stop */~
                                    L20540,         /* Shift Diff.      */~
                                    L20590,         /* Overtime Diff.   */~
                                    L20640,         /* Holiday Diff.    */~
                                    L20690,         /* Holiday OT Diff. */~
                                    L20740,         /* Clock-in Window  */~
                                    L20800          /* When Overtime?   */
                     return
L20240:     REM DEFAULT/ENABLE FOR DEPARTMENT CODE
                message$ = "Enter Department Code To Manage.  Leave Blank~
        ~ And Press (RETURN) to Search File"
                return
L20280:     REM DEFAULT/ENABLE FOR DEPARTMENT DESCRIPTION
                message$ = "Enter Department Title For Reference."
                return
L20310:     REM DEFAULT/ENABLE FOR DEPARTMENT BASE RATE
                message$ = "Enter Department Rate If You Intended To Char~
        ~ge Jobs At Department Rate"
                return
L20350:     REM DEFAULT/ENABLE FOR WIP CLEARING ACCOUNT
                message$ = "Enter Account To Offset WIP When Posting Cost~
        ~ To Jobs."
                return
L20390:     REM DEFAULT/ENABLE FOR OVERHEAD LABOR CLASS
                message$ = "If Overhead Is To Be Calculated, Enter Labor ~
        ~Class To Define Overhead Rate."
                return
L20430:     REM DEFAULT/ENABLE FOR OVERHEAD Account Number
                message$ = "Enter Overhead G/L Account Number."
                return
L20460:     REM DEFAULT/ENABLE DEPARTMENT RATE FLAG
                if drate$ = " " then drate$ = "N"
                message$ = "Enter 'Y' To Post Job Using The Department Ra~
        ~te Rather Than Employees Rate."
                return
L20492:     REM DEFAULT/ENABLE LUNCH TASK CODE
                message$ = "Enter Lunch Task Code."
                return
L20500:     REM DEFAULT/ENABLE FOR SHIFT START AND STOP TIMES
                message$ = "Enter Shift/Lunch Start And Stop Times (Up To~
        ~ 4 Shifts Can Be Defined)."
                return
L20540:     REM DEFAULT/ENABLE FOR SHIFT DIFERENTIALS
                message$ = "Enter Shift Differential (1 = same as base)."
                if shiftdif$() = " " then shiftdif$() = defltdif$()
                return
L20590:     REM DEFAULT/ENABLE FOR OVERTIME DIFERENTIALS
                message$ = "Enter Overtime Differential (e.g. 1.5)."
                if overdif$() = " " then overdif$() = defltdif$()
                return
L20640:     REM DEFAULT/ENABLE FOR HOLIDAY DIFERENTIALS
                message$ = "Enter Holiday Differential (e.g. 2.0)."
                if holdif$() = " " then holdif$() = defltdif$()
                return
L20690:     REM DEFAULT/ENABLE FOR HOLIDAY OVERTIME DIFERENTIALS
                message$ = "Enter Differential for overtime on a "     & ~
                           "holiday (e.g., 3.0 = triple time)."
                if hoverdif$() = " " then hoverdif$() = defltdif$()
                return
L20740:     REM DEFAULT/ENABLE FOR CLOCK-IN WINDOW
                if shift$() = " " then enabled% = 0%
                if in$ = " " and enabled% = 1% then in$ = "6"
                message$ = "Enter, in minutes, clock-in window (adjusts"&~
                           " CDA clock-in time)."
                return
L20800:     REM DEFAULT/ENABLE FOR WHEN OVERTIME?
                if over8$ = " " then over8$, over40$ = "Y"
                message$ = "Is overtime payed for more than 8 hrs per" & ~
                           " day and/or 40 hours per week (Y/N)?"
                return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.
            k% = 2%
            call "STARTOVR" (k%)
            on k%+1% goto L29942, L29948
            return

L29942:        REM START OVER            (ENTER)
                   return clear
                   goto inputmode
L29948:        REM RETURN TO DISPLAY.    (P.F. KEY 1)
                   return

L30000: REM *************************************************************~
            *           R E C A L L   O L D   D E F A U L T S           *~
            *                                                           *~
            * LOADS EXISTING DEFAULTS FROM FILE                         *~
            *************************************************************

            call "READ100" (#1, dept$, f1%(1))
            if f1%(1) = 0% then return

            get #1, using L31310, dept$, descr$, clearacct$, oclass$,     ~
                          ovhdacct$, drate$, shift$(), shiftdif(),       ~
                          overdif(), holdif(), hoverdif(), rate, in,     ~
                          over8$, over40$, lunch$(), ltask$

            REM FORMAT DATA FOR SCREENS
            call "CONVERT" (rate, -2.4, rate$)
            for i% = 1 to 4
                if shiftdif(i%) = 0 then shiftdif(i%) = 1
                if overdif (i%) = 0 then overdif (i%) = 1
                if holdif  (i%) = 0 then holdif  (i%) = 1
                if hoverdif(i%) = 0 then hoverdif(i%) = 1
                call "CONVERT" (shiftdif(i%), 4.4, shiftdif$(i%))
                call "CONVERT" (overdif (i%), 4.4, overdif$ (i%))
                call "CONVERT" (holdif  (i%), 4.4, holdif$  (i%))
                call "CONVERT" (hoverdif(i%), 4.4, hoverdif$(i%))
            next i%
            call "CONVERT" (in, -0.01, in$)
            call "GLFMT" (clearacct$)
            call "GLFMT" (ovhdacct$)

*        Format Overhead Percentage
            ovhd = 0
            if oclass$ = " " then L30340
                call "READ100" (#5, oclass$, f1%(5))
                if f1%(5) = 1% then get #5, using L30330, ovhd
L30330:                   FMT POS(61), PD(14,4)
L30340:     call "CONVERT" (ovhd, -0.4, ovhd$)
            ovhd$ = "(" & ovhd$ & "%)"
            return

L31000: REM *************************************************************~
            *            S A V E   N E W   D E F A U L T S              *~
            *                                                           *~
            * SAVES ENTERED DATA ON FILE                                *~
            *************************************************************

            rate, in, out = 0

            REM UNFORMAT DATA FOR DISK
            convert rate$ to rate, data goto L31140
L31140:     convert in$ to in, data goto L31150
L31150:     call "GLUNFMT" (clearacct$)
            call "GLUNFMT" (ovhdacct$)

            for i% = 1 to 4
                convert shiftdif$(i%) to shiftdif(i%), data goto L31200
L31200:         convert overdif$ (i%) to overdif (i%), data goto L31210
L31210:         convert holdif$  (i%) to holdif  (i%), data goto L31220
L31220:         convert hoverdif$(i%) to hoverdif(i%), data goto L31230
L31230:     next i%

            write #1, using L31310, dept$, descr$, clearacct$, oclass$,   ~
                            ovhdacct$, drate$, shift$(), shiftdif(),     ~
                            overdif(), holdif(), hoverdif(), rate, in,   ~
                            over8$, over40$, lunch$(), ltask$, " "
            return

L31310:     FMT CH(4),                   /* Department                 */~
                CH(30),                  /* Description                */~
                CH(9),                   /* WIP Clearing Account       */~
                CH(4),                   /* Overhead Labor Class       */~
                CH(9),                   /* Overhead Account           */~
                CH(1),                   /* Use Dept Rate For Job Post?*/~
                8*CH(5),                 /* Shift Start/Stop Times     */~
                4*PD(14,6),              /* Shift Differentials        */~
                4*PD(14,6),              /* Overtime Differentials     */~
                4*PD(14,6),              /* Holiday Differentials      */~
                4*PD(14,6),              /* Holiday OT Differentials   */~
                PD(14,4),                /* Department Rate            */~
                BI(2),                   /* Clock-in Window            */~
                CH(1),                   /* Overtime If Over 8/Day?    */~
                CH(1),                   /* Overtime If Over 40/Week?  */~
                8*CH(5),                 /* Lunch Start/Stop Times     */~
                CH(6),                   /* Lunch Task Code            */~
                CH(17)                   /* Filler                     */

        REM *************************************************************~
            *      P R I N T   R E P O R T   S E C T I O N              *~
            *-----------------------------------------------------------*~
            * Print Department Report Section                           *~
            *************************************************************
        report_it
            call "SHOSTAT" ("Printing Department Report...")
            select printer(134)
            pcntr% = 0%  :  half% = 1%
            init(hex(00)) dept$
            call "READ104" (#1, dept$, f1%(1))
            goto L35150

        read_loop
            call "READNEXT" (#1, f1%(1))
                if f1%(1) = 0% then end_report
L35150:     get #1, using L31310, dept$, descr$, clearacct$, oclass$,     ~
                          ovhdacct$, drate$, shift$(), shiftdif(),       ~
                          overdif(), holdif(), hoverdif(), rate, in,     ~
                          over8$, over40$, lunch$(), ltask$

            call "CONVERT" (rate, -2.4, rate$)
            for i% = 1 to 4
                call "CONVERT" (shiftdif(i%), 4.4, shiftdif$(i%))
                call "CONVERT" (overdif (i%), 4.4, overdif$ (i%))
                call "CONVERT" (holdif  (i%), 4.4, holdif$  (i%))
                call "CONVERT" (hoverdif(i%), 4.4, hoverdif$(i%))
            next i%
            yn$(1), yn$(2), yn$(3) = "No "
            if drate$  = "Y" then yn$(1) = "Yes"
            if over8$  = "Y" then yn$(2) = "Yes"
            if over40$ = "Y" then yn$(3) = "Yes"
            call "CONVERT" (in, -0.01, in$)
            call "GLFMT" (clearacct$)
            call "GLFMT" (ovhdacct$)
*       ** Format Overhead Percentage
            ovhd = 0
            if oclass$ = " " then L35390
                call "READ100" (#5, oclass$, f1%(5))
                if f1%(5) = 1% then get #5, using L35380, ovhd
L35380:                   FMT POS(61), PD(14,4)
L35390:     call "CONVERT" (ovhd, -0.4, ovhd$)
            ovhd$ = "(" & ovhd$ & "%)"

*       ** Now print the department information
            if half% <> 1% then L35460
            print page
            pcntr% = pcntr% + 1%
            print using L60040, date$, company$, pcntr%
            print using L60070
L35460:     if half% = 2% then print skip(5) else print skip(2)
            print using L60100, dept$, descr$
            print skip(1)
            print using L60120, rate$, yn$(1), oclass$
            print skip(1)
            print using L60150, clearacct$, ovhdacct$
            print skip(1)
            print using L60180, yn$(2), yn$(3), in$
            print skip(2)
            print using L60210
            print using L60240
            print using L60270
            print using L60300, shift$(1,1), shift$(1,2), shiftdif$(1),   ~
                               overdif$(1), holdif$(1), hoverdif$(1)
            print using L60420, lunch$(1,1), lunch$(1,2)
            print skip(1)
            print using L60330, shift$(2,1), shift$(2,2), shiftdif$(2),   ~
                               overdif$(2), holdif$(2), hoverdif$(2)
            print using L60420, lunch$(2,1), lunch$(2,2)
            print skip(1)
            print using L60360, shift$(3,1), shift$(3,2), shiftdif$(3),   ~
                               overdif$(3), holdif$(3), hoverdif$(3)
            print using L60420, lunch$(3,1), lunch$(3,2)
            print skip(1)
            print using L60390, shift$(4,1), shift$(4,2), shiftdif$(4),   ~
                               overdif$(4), holdif$(4), hoverdif$(4)
            print using L60420, lunch$(4,1), lunch$(4,2)
            if half% = 1% then half% = 2% else half% = 1%
            goto read_loop

        end_report
            print skip(2)
            print using L64000
            close printer
            goto inputmode

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *-----------------------------------------------------------*~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'201(fieldnr%)
                init(hex(8c)) lfac$()
                if editmode% <> 0 then L40230
                pfdescr$(1) = "(1)Start Over     (4)Prev Field          (~
        ~13)Instructions      (15)Print Screen"
                pfdescr$(2) = "                                         (~
        ~14)Print Listing     (16)Exit Program"
                pfkeys$ = hex(0001040d0e0f10)
                if fieldnr% > 1% then pfdescr$(2) = " "
                if fieldnr% = 1% then str(pfdescr$(1),19,15) = " "
                line2$ = " "
                if lastdept$ <> " " then line2$ =                        ~
                                  "Last Department Managed: " & lastdept$
                str(line2$,62%) = "JBTCDPTS: " & cms2v$
                goto L40320

L40230:         REM Editmode logic...
                pfdescr$(1) = "(1)Start Over                            (~
        ~13)Instructions      (15)Print Screen"
                pfdescr$(2) = "                                          ~
        ~                     (16)Save Data   "
                pfkeys$ = hex(0001ff0d0f10)
                if fieldnr% = 0% then init(hex(86)) lfac$()
                if fieldnr% <> 0% then pfdescr$(2) = " "

L40320:           str(pfdescr$(2),63,1) = hex(84)
                  on fieldnr% gosub L40520,         /* Department       */~
                                    L40490,         /* Description      */~
                                    L40520,         /* Department Rate  */~
                                    L40520,         /* WIP Clear Acct   */~
                                    L40520,         /* Overhead Labr Cls*/~
                                    L40520,         /* Overhead Account */~
                                    L40520,         /* Use Dprtmnt Rate?*/~
                                    L40520,         /* Lunch Task Code  */~
                                    L40520,         /* Shift Start/Stop */~
                                    L40520,         /* Shift Diff.      */~
                                    L40520,         /* Overtime Diff.   */~
                                    L40520,         /* Holiday Diff.    */~
                                    L40520,         /* Holiday OT Diff. */~
                                    L40520,         /* Clock-in Window  */~
                                    L40520          /* When Overtime?   */
                     goto L40590

L40490:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40520:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40590:     accept                                                       ~
               at (01,02), "Manage Time Card Department Information",    ~
               at (01,60), "Todays Date:",                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Department Code",                            ~
               at (05,26), fac(lfac$(01)), dept$                , ch(04),~
               at (06,02), "Department Description",                     ~
               at (06,26), fac(lfac$(02)), descr$               , ch(30),~
               at (07,02), "Department Base Rate",                       ~
               at (07,26), fac(lfac$(03)), rate$                , ch(10),~
               at (07,45), "Applied Labor Account",                      ~
               at (07,67), fac(lfac$(04)), clearacct$           , ch(12),~
               at (08,02), "Overhead Labor Class",                       ~
               at (08,26), fac(lfac$(05)), oclass$              , ch(04),~
               at (08,31), fac(hex(8c)),   ovhd$                , ch(08),~
               at (08,45), "Applied Overhead Acct",                      ~
               at (08,67), fac(lfac$(06)), ovhdacct$            , ch(12),~
               at (09,02), "Post Jobs w/ Dept Rate?",                    ~
               at (09,26), fac(lfac$(07)), drate$               , ch(01),~
               at (09,45), "Lunch Task Code",                            ~
               at (09,67), fac(lfac$(08)), ltask$               , ch(06),~
                                                                         ~
               at (11,26), ".....(1)..... .....(2)..... .....(3)..... ...~
        ~..(4).....",                                                     ~
               at (12,03), "Shift Start/Stop Times",                     ~
               at (12,26), fac(lfac$(09)), shift$(1,1)          , ch(05),~
               at (12,32), "-",                                          ~
               at (12,34), fac(lfac$(09)), shift$(1,2)          , ch(05),~
               at (12,40), fac(lfac$(09)), shift$(2,1)          , ch(05),~
               at (12,46), "-",                                          ~
               at (12,48), fac(lfac$(09)), shift$(2,2)          , ch(05),~
               at (12,54), fac(lfac$(09)), shift$(3,1)          , ch(05),~
               at (12,60), "-",                                          ~
               at (12,62), fac(lfac$(09)), shift$(3,2)          , ch(05),~
               at (12,68), fac(lfac$(09)), shift$(4,1)          , ch(05),~
               at (12,74), "-",                                          ~
               at (12,76), fac(lfac$(09)), shift$(4,2)          , ch(05),~
                                                                         ~
               at (13,03), "Lunch Start/Stop Times",                     ~
               at (13,26), fac(lfac$(09)), lunch$(1,1)          , ch(05),~
               at (13,32), "-",                                          ~
               at (13,34), fac(lfac$(09)), lunch$(1,2)          , ch(05),~
               at (13,40), fac(lfac$(09)), lunch$(2,1)          , ch(05),~
               at (13,46), "-",                                          ~
               at (13,48), fac(lfac$(09)), lunch$(2,2)          , ch(05),~
               at (13,54), fac(lfac$(09)), lunch$(3,1)          , ch(05),~
               at (13,60), "-",                                          ~
               at (13,62), fac(lfac$(09)), lunch$(3,2)          , ch(05),~
               at (13,68), fac(lfac$(09)), lunch$(4,1)          , ch(05),~
               at (13,74), "-",                                          ~
               at (13,76), fac(lfac$(09)), lunch$(4,2)          , ch(05),~
                                                                         ~
               at (14,03), "Factors- Shift         ",                    ~
               at (14,28), fac(lfac$(10)), shiftdif$(1)         , ch(07),~
               at (14,42), fac(lfac$(10)), shiftdif$(2)         , ch(07),~
               at (14,56), fac(lfac$(10)), shiftdif$(3)         , ch(07),~
               at (14,70), fac(lfac$(10)), shiftdif$(4)         , ch(07),~
                                                                         ~
               at (15,03), "         Overtime     ",                     ~
               at (15,28), fac(lfac$(11)), overdif$(1)          , ch(07),~
               at (15,42), fac(lfac$(11)), overdif$(2)          , ch(07),~
               at (15,56), fac(lfac$(11)), overdif$(3)          , ch(07),~
               at (15,70), fac(lfac$(11)), overdif$(4)          , ch(07),~
                                                                         ~
               at (16,03), "         Holiday     ",                      ~
               at (16,28), fac(lfac$(12)), holdif$(1)           , ch(07),~
               at (16,42), fac(lfac$(12)), holdif$(2)           , ch(07),~
               at (16,56), fac(lfac$(12)), holdif$(3)           , ch(07),~
               at (16,70), fac(lfac$(12)), holdif$(4)           , ch(07),~
                                                                         ~
               at (17,03), "         OT & Holiday ",                     ~
               at (17,28), fac(lfac$(13)), hoverdif$(1)         , ch(07),~
               at (17,42), fac(lfac$(13)), hoverdif$(2)         , ch(07),~
               at (17,56), fac(lfac$(13)), hoverdif$(3)         , ch(07),~
               at (17,70), fac(lfac$(13)), hoverdif$(4)         , ch(07),~
                                                                         ~
               at (19,03), "Clock-in Window (Minutes)",                  ~
               at (19,30), fac(lfac$(14)), in$                  , ch(02),~
               at (19,36), "(For adjusting clock-in time)",              ~
                                                                         ~
               at (20,03), "Overtime If More Than 8 Hrs/Day?",           ~
               at (20,36), fac(lfac$(15)), over8$               , ch(01),~
               at (20,40), "40 Hrs/Week?",                               ~
               at (20,53), fac(lfac$(15)), over40$              , ch(01),~
                                                                         ~
               at (22,02), fac(hex(a4)),   message$             , ch(79),~
               at (23,02), fac(hex(8c)),   pfdescr$(1)          , ch(79),~
               at (24,02), fac(hex(8c)),   pfdescr$(2)          , ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 13 then L41540
                call "MANUAL" ("JBTCDPTS")
                goto L40590

L41540:        if keyhit% <> 15 then L41580
                  call "PRNTSCRN"
                  goto L40590

L41580:        if fieldnr% <> 0% then return
               close ws
               call "SCREEN" addr ("C", k%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50250,         /* Department       */~
                                    L50350,         /* Description      */~
                                    L50400,         /* Department Rate  */~
                                    L50440,         /* WIP Clear Acct   */~
                                    L50490,         /* Overhead Labr Cls*/~
                                    L50650,         /* Overhead Account */~
                                    L50710,         /* Use Dprtmnt Rate?*/~
                                    L50760,         /* Lunch Task Code  */~
                                    L50830,         /* Shift Start/Stop */~
                                    L51220,         /* Shift Diff.      */~
                                    L51300,         /* Overtime Diff.   */~
                                    L51380,         /* Holiday Diff.    */~
                                    L51460,         /* Holiday OT Diff. */~
                                    L51540,         /* Clock-in Window  */~
                                    L51580          /* When Overtime?   */
                     return

L50250:     REM TEST DATA FOR DEPARTMENT CODE
                if dept$ <> " " then L50300
                call "GETCODE" (#1, dept$, " ", 0%, 0, f1%(1))
                     if f1%(1) <> 0 then L50300
                     errormsg$ = hex(00) : return
L50300:         gosub L30000
                if f1%(1) = 0 then return
                return clear all
                goto editmode

L50350:     REM TEST DATA FOR DEPARTMENT DESCRIPTION
                if descr$ <> " " then return
                     errormsg$="Sorry, Description Can't Be Blank"
                     return

L50400:     REM TEST DATA FOR DEPARTMENT BASE RATE
                call "NUMTEST" (rate$, 0, 9999, errormsg$, 2.4, 0)
                return

L50440:     REM TEST FOR WIP CLEARING ACCOUNT NUMBER
                call "GETCODE" (#2, clearacct$, " ", 0%, 0, f1%(2))
                     if f1%(2) = 0 then errormsg$ = "Invalid Account #"
                return

L50490:     REM TEST FOR OVERHEAD PERCENTAGE
                ovhd = 0
                if oclass$ = " " then L50610
                readkey$ = "LBR CLASS" & oclass$
                call "PLOWCODE" (#3, readkey$, " ", 9%, 0.30, f1%(3))
                     if f1%(3) <> 0% then L50570
                     errormsg$ = "Invalid Labor Class"
                     return
L50570:         oclass$ = str(readkey$,10)
                call "READ100" (#5, oclass$, f1%(5))
                if f1%(5) = 1% then get #5 using L50600, ovhd
L50600:              FMT POS(61), PD(14,4)
L50610:         call "CONVERT" (ovhd, -0.4, ovhd$)
                ovhd$ = "(" & ovhd$ & "%)"
                return

L50650:     REM TEST FOR OVERHEAD ACCOUNT NUMBER
                call "GETCODE" (#2, ovhdacct$, " ", 0%, 0, f1%(2))
                if f1%(2) = 0% then errormsg$ = "Invalid Account #"
                return

L50710:     REM TEST FOR USE DEPARTMENT RATE FLAG
                if pos("YN"=drate$) > 0 then return
                     errormsg$ = "Please Enter 'Y' or 'N'"
                     return

L50760:     REM TEST DATA FOR LUNCH TASK CODE
                if ltask$ = " " then L50810
                call "GETCODE" (#6, ltask$, " ", 0%, 0, f1%(6))
                     if f1%(6) = 0 then errormsg$ =                      ~
                                 "Invalid Lunch Task Code"
L50810:         return

L50830
*        Test Data for SHIFT START AND STOP TIMES
            for i% = 1 to 4
              if shift$(i%,1) = " " and shift$(i%,2) = " " then L51010
                call "TIMEOK" (shift$(i%,1), clockin, errormsg$)
                if errormsg$ <> " " then L50980
                call "TIMEOK" (shift$(i%,2), clockout, errormsg$)
                if errormsg$ <> " " then L50980
                     if clockout < clockin then clockout = clockout + 24
                     hours = clockout - clockin
                     if hours >= 12 then errormsg$ =                     ~
                          "Sorry, shift must be less than 12 hours long"
                     if hours < 1 then errormsg$ =                       ~
                          "Sorry, shift must be at least 1 hour long"
                     if errormsg$ = " " then L51020
L50980:                   errormsg$=errormsg$&" (Shift "&bin(i%+48,1)&")"
                          return

L51010:     REM Test Data for LUNCH START AND STOP TIMES
L51020:       if lunch$(i%,1) = " " and lunch$(i%,2) = " " then L51190
                call "TIMEOK" (lunch$(i%,1), lunchin, errormsg$)
                if errormsg$ <> " " then L51160
                call "TIMEOK" (lunch$(i%,2), lunchout, errormsg$)
                if errormsg$ <> " " then L51160

                if lunchin >= clockin then L51065
                     lunchin  = lunchin  + 24
                     lunchout = lunchout + 24
L51065:         if lunchout < lunchin then lunchout = lunchout + 24
                if lunchin  < clockin or lunchout > clockout             ~
                          then errormsg$ = "Sorry, lunch must fall be" & ~
                               "tween Shift Start and Stop Times"
                hours = lunchout - lunchin
                if hours >= 2 then errormsg$ =                           ~
                          "Sorry, lunch must be less than 2 hours long"
                if hours < .25 then errormsg$ =                          ~
                          "Sorry, lunch must be at least 1/4 hours long"
                if errormsg$ = " " then L51190
L51160:              errormsg$ = errormsg$ &" (Lunch " & bin(i%+48,1)&")"
                     return
L51190:     next i%
            return

L51220:     REM TEST DATA FOR SHIFT DIFFERENTIALS
                for i% = 1 to 4
                     call "NUMTEST" (shiftdif$(i%),.5,99,errormsg$,-4.4,0)
                     if errormsg$ <> " " then return
                next i%
                return

L51300:     REM TEST DATA FOR OVERTIME DIFFERENTIALS
                for i% = 1 to 4
                     call "NUMTEST"(overdif$(i%),.5,99,errormsg$,-4.4,0)
                     if errormsg$ <> " " then return
                next i%
                return

L51380:     REM TEST DATA FOR HOLIDAY DIFFERENTIALS
                for i% = 1 to 4
                     call "NUMTEST" (holdif$(i%),.5,99,errormsg$,-4.4,0)
                     if errormsg$ <> " " then return
                next i%
                return

L51460:     REM TEST DATA FOR HOLIDAY OVERTIME DIFFERENTIALS
                for i% = 1 to 4
                     call "NUMTEST"(hoverdif$(i%),.5,99,errormsg$,-4.4,0)
                     if errormsg$ <> " " then return
                next i%
                return

L51540:     REM TEST DATA FOR CLOCK IN WINDOW
                call "NUMTEST" (in$, 0, 75, errormsg$, 0.0, 0)
                return

L51580:     REM TEST DATA FOR WHEN OVERTIME?
                if pos("YN"=over8$) > 0 then L51620
                     errormsg$ = "Please Enter 'Y' or 'N': " & over8$
                     return
L51620:         if pos("YN"=over40$) > 0 then return
                     errormsg$ = "Please Enter 'Y' or 'N': " & over40$
                     return

        REM *************************************************************~
            *        R E P O R T   I M A G E   S T A T E M E N T S      *~
            *************************************************************
*       ** First the header lines ...
L60040: %Run Date: ########                   ###########################~
        ~#################################                    Page: ###

L60070: %                                                      Payroll De~
        ~partment Listing                                 JBTCDPTS: PRL002

L60100: %Department Code: ####  ##############################

L60120: %     Base Rate is ##########     Post Job Using Department Rate?~
        ~ ###     Overhead Labor Class is ####

L60150: %     Applied Labor G/L Account is ############     Applied Labor~
        ~ Overhead G/L Account is ############

L60180: %     Overtime paid after 8 hrs/day? ###     after 40 hrs/wk? ###~
        ~     Clock-in Window is ## minutes

L60210: %                                                <---------------~
        ~- Differentials ---------------->

L60240: %                                  From   To      Shift %      Ov~
        ~ertime      Holiday    Holiday OT

L60270: %                                 -----  -----   ----------   ---~
        ~-------   ----------   ----------

L60300: %                First Shift      #####  #####     #######      #~
        ~######      #######      #######

L60330: %               Second Shift      #####  #####     #######      #~
        ~######      #######      #######

L60360: %                Third Shift      #####  #####     #######      #~
        ~######      #######      #######

L60390: %               Fourth Shift      #####  #####     #######      #~
        ~######      #######      #######

L60420: %               Lunch Period      #####  #####

L64000: %                                               ********** END OF~
        ~ REPORT **********

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Closes all the files currently open, and also displays    *~
            * a message (only if in foreground) while linking to the    *~
            * next program.                                             *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")
            end
