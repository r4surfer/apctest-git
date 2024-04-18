        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   GGG   L      BBBB    GGG   IIIII  PPPP   U   U  TTTTT   *~
            *  G      L      B   B  G        I    P   P  U   U    T     *~
            *  G GGG  L      BBBB   G GGG    I    PPPP   U   U    T     *~
            *  G   G  L      B   B  G   G    I    P      U   U    T     *~
            *   GGG   LLLLL  BBBB    GGG   IIIII  P       UUU     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GLBGIPUT - Allows entry of budget amounts for GL accounts.*~
            *            Current year and next year budgets can be      *~
            *            managed.  These budgets are rolled by the G/L  *~
            *            year end closing functions.                    *~
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
            * 02/27/85 ! ORIGINAL (Re-write)                      ! HES *~
            * 05/22/87 ! Support Of Obsolete Flag On GLAMIN       ! HES *~
            * 10/21/91 ! Standardized call to STARTOVR            ! MLJ *~
            * 02/15/93 ! PRR 12245, 12304 - Can now modify annual ! MLJ *~
            *          !   amts.  Amount entered is then dispersed!     *~
            *          !   throughout the periods.                !     *~
            *          ! PRR 12216 - Can now use actual Period Act!     *~
            *          !   as seed for Current Year budgets.      !     *~
            * 04/27/93 ! QC - PF(8) selection display now includes! MLJ *~
            *          !   period activity amounts.               !     *~
	    * 06/14/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**~

        dim                                                              ~
            account$16,                  /* G/L ACCOUNT NUMBER         */~
            budget$(28)13,               /* BUDGET AMOUNTS FOR ACCOUNT */~
            currpa(13),                  /* CURRENT YEAR PERIOD ACT    */~
            currpa$(13)13,               /* CURR YR PA - DISPLAY       */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            cy$(13)1,                    /* CURRENT YEAR PA SELECTION  */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            dates$(32)8,                 /* FISCAL DATE STRUCTURE      */~
            descr$32,                    /* REPORT DESCRIPTION         */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            fdate$(26)8,                 /* FMT'D FISCAL DATES PREV/CUR*/~
            filler$171,                  /* FILLER AT END OF RECORD    */~
            header$79,                   /* HEADER FOR SCREEN          */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            message$79,                  /* INPUT MESSAGE              */~
            name$(28)13,                 /* DATES FOR DISPLAY          */~
            obmsg$32,                    /* Obsolete Message           */~
            pfdescr$(2)79,               /* DESCRIPTION OF PFKEYS      */~
            pfkeys$32,                   /* PFKEYS Enabled List        */~
            prevpa(13),                  /* PREVIOUS YEAR PERIOD ACT   */~
            prevpa$(13)13,               /* PREV YR PA - DISPLAY       */~
            py$(13)1,                    /* PREVIOUS YEAR PA SELECTION */~
            pyear$112,                   /* LAST YEAR BUDGETS          */~
	    tdate$8,			 /* Temporary Date             */~
            text$(2)40,                  /* SCREEN TEXT                */~
            type$1,                      /* ACCOUTN TYPE (A,L,C,R,E)   */~
            typedescr$12,                /* ACCOUNT TYPE DESCRIPTION   */~
	    udate$8,			 /* Unformated Date String     */~
            work(26)                     /* WORK VARIABLE              */

        dim f1%(64)                      /* = 1 IF READ WAS SUCCESSFUL */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! GLMAIN   ! General Ledger Account Master File       *~
            * # 2 ! GLBUDGET ! General Ledger Account Budget Amounts    *~
            * # 3 ! SYSFILE2 ! System Info (Fiscal Dates Structure)     *~
            *************************************************************

            select #1,  "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 9

            select #2,  "GLBUDGET"                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 9

            select  #3, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            call "SHOSTAT" ("Opening Files, One Moment Please.")

            call "OPENCHCK" (#1, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#2, 0%, 0%, 100%, " ")
            call "OPENCHCK" (#3, 0%, 0%, 0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)

            call "READ100" (#3, "FISCAL DATES", f1%(3%))
                 if f1%(3%) <> 0% then L09140
                   accept "Error In Finding Fiscal Date Structure",      ~
                        "Press RETURN To Acknowledge And Exit Program"
                   goto L65000
L09140:     get #3, using L09150, periods%, dates$()
L09150:             FMT XX(20), BI(2), XX(136), XX(2), 32*CH(8)

        REM SET UP THE MONTH'S NAMES FOR THE ERRORMSG
            for i% = 15% to 27%
		tdate$ = dates$(i%+1%)
		call "DATEFMT" (tdate$, 0%, udate$)
                name$(i%), name$(i%-13%) = udate$
                convert str(name$(i%-13%),1%,4%) to temp%, data goto L09240
                convert temp% + 1% to str(name$(i%),1%,4%), pic(0000)
                call "DATEFMT" (name$(i%))
                call "DATEFMT" (name$(i%-13%))
L09240:     next i%
            if periods% = 12% then name$(14%), name$(27%) = " "
            name$(1%), name$(28%) = "Annual Budget"

            text$(1%) = "Current Year Budget"
            text$(2%) = "Next Year Budget"

        REM BUILD PREV/CURR YEAR PERIOD ACTIVITY DATES...
            fdate$() = " "
            for i% = 2% to 14%                   /* Previous Year Dates */
                fdate$(i% - 1%) = dates$(i%)
                call "DATEFMT" (fdate$(i% - 1%))
            next i%
                for i% = 16% to 28%              /* Current Year Dates  */
                    fdate$(i% - 2%) = dates$(i%)
                    call "DATEFMT" (fdate$(i% - 2%))
                next i%
            if periods% = 12% then fdate$(13%), fdate$(26%) = " "

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
            gosub L29000  /* Clear Variables For Input */

            for fieldnr% = 1% to 5%
                gosub'051(fieldnr%)
                      if enabled% = 0% then L10240
L10130:         gosub'101(1%, fieldnr%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  =  8% then gosub period_activity
                      if keyhit% <>  4% then L10210
L10160:                   if fieldnr% < 2%  then L10130
                          fieldnr% = fieldnr% - 1%
                          gosub'061(fieldnr%)
                          if enabled% <> 0% then L10130
                          goto L10160
L10210:               if keyhit%  = 16% and fieldnr% = 1% then L65000
                      if keyhit%  = 32% and fieldnr% = 1% then L65000
                      if keyhit% <>  0% then       L10130
L10240:         gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10130
                next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

        editmode
            message$ = "To Modify Displayed Values, Position Cursor To De~
        ~sired Value And Press (ENTER)."
L11090:     gosub'101(2%, 0%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       L11090
            if cursor%(1%) > 7% then L11146
                if cursor%(2%) > 50% then fieldnr% = 4%                  ~
                                     else fieldnr% = 2%
                goto L11170
L11146:     if cursor%(2%) > 50% then fieldnr% = 5%                      ~
                                 else fieldnr% = 3%

L11170:     gosub'061(fieldnr%)
            gosub'101(3%, fieldnr%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11170
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11170
            goto editmode

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            REM Delete old budgets from file first...
            lastaccount$ = account$
            call "GLUNFMT" (account$)
            call "READ101" (#2, account$, f1%(2%))
                if f1%(2%) = 1% then delete #2

            gosub L31000    /* Write New Budget info To Files */
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1% : message$=" "
                  on fieldnr% gosub L20250,         /* ACCOUNT NUMBER   */~
                                    L20290,         /* CUR ANNUAL BUDGET*/~
                                    L20340,         /* CUR BUDGET AMNTS */~
                                    L20400,         /* NXT ANNUAL BUDGET*/~
                                    L20430          /* NXT BUDGET AMNTS */
                  return

            deffn'061(fieldnr%)
*                MESSAGE$=" "
                  enabled% = 1%
                  on fieldnr% gosub L20260,         /* ACCOUNT NUMBER   */~
                                    L20300,         /* CUR ANNUAL BUDGET*/~
                                    L20350,         /* CUR BUDGET AMNTS */~
                                    L20410,         /* NXT ANNUAL BUDGET*/~
                                    L20440          /* NXT BUDGET AMNTS */
                     return

L20250:     REM DEFAULT/ENABLE FOR ACCOUNT NUMBER
L20260:         message$ = "Enter Account Or Leave Blank To Search."
                return
L20290:     REM DEFAULT/ENABLE FOR CURRENT ANNUAL BUDGET
L20300:         message$= "Enter Current Year Total Budget And Press RE"&~
                          "TURN To Distribute."
                return
L20340:     REM DEFAULT/ENABLE FOR CURRENT BUDGET AMOUNTS
L20350:         message$ = "Enter Budget For Each Period Of This Year."
                for i% = 2% to 1% + periods%
                     call "SPCSMASH" (budget$(i%))
                next i%
                return
L20400:     REM DEFAULT/ENABLE FOR NEXT YEARS ANNUAL BUDGET
L20410:         message$ = "Enter Next Year Total Budget And Press RETU"&~
                           "RN To Distribute."
                return
L20430:     REM DEFAULT/ENABLE FOR NEXT YEARS BUDGET AMOUNTS
L20440:         message$ = "Enter Budget For Each Period Of Next Year."
                for i% = 15% to 14% + periods%
                     call "SPCSMASH" (budget$(i%))
                next i%
                return

L29000: REM *************************************************************~
            * INITIALIZATION BLOCK (NEATER THAN CRAMMING AT 10000)      *~
            *************************************************************
            init(" ")                                                    ~
            account$,                    /* G/L ACCOUNT NUMBER         */~
            budget$(),                   /* BUDGET AMOUNTS FOR ACCOUNT */~
            cy$(),                       /* CURRENT YEAR PA SELECTIONS */~
            currpa$(),                   /* CURRENT YEAR PA AMOUNTS    */~
            descr$,                      /* REPORT DESCRIPTION         */~
            errormsg$,                   /* ERROR MESSAGE              */~
            message$,                    /* INPUT MESSAGE              */~
            obmsg$,                      /* Obsolete Message           */~
            py$(),                       /* PREVIOUS YEAR PA SELECTIONS*/~
            prevpa$(),                   /* PREVIOUS YEAR PA AMOUNTS   */~
            type$,                       /* ACCOUTN TYPE (A,L,C,R,E)   */~
            typedescr$                   /* ACCOUNT TYPE DESCRIPTION   */

            init(hex(00)) pyear$, filler$
            mat work = zer
            mat currpa = zer
            mat prevpa = zer
        return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * OR WILL RETURN USER BACK TO WHERE THEY WERE.  MUST PUSH   *~
            * TWO BUTTONS TO START OVER FOR SAFETY.                     *~
            *************************************************************

        startover

               keyhit1% = 2%
               call "STARTOVR"(keyhit1%)
               if keyhit1% = 1% then return
               return clear all
               goto inputmode

L30000: REM *************************************************************~
            *              L O A D   O L D   D A T A                    *~
            *                                                           *~
            * LOADS OLD BUDGET INFO FROM DISK.                          *~
            *************************************************************

            onfile% = 0%

            REM Try And Load Budgets...
            temp$ = account$
            call "GLUNFMT" (temp$)
            call "READ100" (#2, temp$, f1%(2%))
                if f1%(2%) = 0% then return

            onfile% = 1%
            get #2, using L30140, pyear$, work(), filler$
L30140:     FMT XX(9), CH(112), 26*PD(14,4), CH(171)

            annual = 0
            for i% = 1% to 13%
                convert work(i%) to budget$(i%+1%), pic(-#########.##)
                annual = annual + work(i%)
            next i%
            convert annual to budget$(1%), pic(-#########.##)

            annual = 0
            for i% = 14% to 26%
                convert work(i%) to budget$(i%+1%), pic(-#########.##)
                annual = annual + work(i%)
            next i%
            convert annual to budget$(28%), pic(-#########.##)
            if periods% = 12% then budget$(14%), budget$(27%) = " "
            return

L31000: REM *************************************************************~
            *              S A V E   N E W   D A T A                    *~
            *                                                           *~
            * SAVES NEW BUDGET INFO ONTO DISK.                          *~
            *************************************************************

            mat work = zer
            all_zero% = 0%
            for i% = 1% to 13%     /* Current Year Budgets */
                convert budget$(i%+1%) to work(i%), data goto L31120
                if abs(work(i%)) > .001 then all_zero% = 1%
L31120:     next i%

            for i% = 14% to 26%    /* Next Year Budgets */
                convert budget$(i%+1%) to work(i%), data goto L31180
                if abs(work(i%)) > .001 then all_zero% = 1%
L31180:     next i%

            if all_zero% = 0% then return
            write #2, using L31220, account$, pyear$, work(), filler$
L31220:     FMT CH(9), CH(112), 26*PD(14,4), CH(171)
            return

        REM *************************************************************~
            *          A C T U A L   P E R I O D   A C T I V I T Y      *~
            *                                                           *~
            * Populate period budgets using actual prev year or curr    *~
            * year period activity.                                     *~
            *************************************************************

        period_activity
            colpos% = cursor%(2%)                  /* Save Which Column */
            readkey$ = str(account$) & hex(00)
            call "READ100" (#1, readkey$, f1%(1%))
                if f1%(1%) = 1% then L32140
                     errormsg$ = "Account Number is NOT valid"
                     return
L32140:     get #1 using L32150, prevpa(), currpa()
L32150:         FMT POS(53), 13*PD(14,4), POS(165), 13*PD(14,4)

            for i% = 1% to 13%
                convert prevpa(i%) to prevpa$(i%), pic(-#########.##)
                convert currpa(i%) to currpa$(i%), pic(-#########.##)
            next i%


L32230:     gosub L42000                /*  Prev/Curr Year PA activity   */
                if keyhit%  = 1% then gosub startover
                if keyhit%  = 16% then L32640
                if keyhit% <> 0% then goto L32230
            annual = 0

            for i% = 1% to 13%                          /*  Simple edit */
                if py$(i%) <> " " and cy$(i%) <> " " then L32320          ~
                    else L32350
L32320:         errormsg$ = "You CANNOT select the same period from bot"&~
                            "h Previous and Current Year columns"
                goto L32230
L32350:     next i%

            for i% = 1% to 13%                        /* Get Prev Yr PA */
                if py$(i%) = " " then L32450
                if colpos% > 50% then                                    ~
                    convert prevpa(i%) to budget$(i%+14%),               ~
                                                      pic(-#########.##) ~
                    else convert prevpa(i%) to budget$(i%+1%),           ~
                                                      pic(-#########.##)
                annual = annual + prevpa(i%)
L32450:     next i%

            for i% = 1% to 13%                        /* Get Curr Yr PA */
                if cy$(i%) = " " then L32550
                if colpos% > 50% then                                    ~
                convert currpa(i%) to budget$(i%+14%),                   ~
                                                      pic(-#########.##) ~
                    else convert currpa(i%) to budget$(i%+1%),           ~
                                                      pic(-#########.##)
                annual = annual + currpa(i%)
L32550:     next i%

            if colpos% > 50% then L32670

            for i% = 2% to 14%                         /* PA To Curr Yr */
                if budget$(i%) <> " " then L32640
                    budget$(i%) = "0.00"
                call "STRING" addr("RJ", budget$(i%), 13%)
            next i%
L32640:     convert annual to budget$(1%), pic(-#########.##)
            goto L32740

L32670:     for i% = 15% to 27%                        /* PA To Next Yr */
                if budget$(i%) <> " " then L32700
                    budget$(i%) = "0.00"
L32700:         call "STRING" addr("RJ", budget$(i%), 13%)
            next i%
            convert annual to budget$(28%), pic(-#########.##)

L32740:     init(" ") errormsg$, cy$(), py$()
            return

        REM *************************************************************~
            *      I N P U T   /   E D I T   M O D E   P A G E   1      *~
            *                                                           *~
            * SERVES INPUT LOOP AND EDIT MODE FOR PAGE ONE OF DOCUMENT. *~
            *************************************************************

            deffn'101(screen%, fieldnr%)
                  gosub set_keys
                  init(hex(8c)) lfac$()
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                                  else init(hex(86)) lfac$()
                  header$  = "Last Account Managed: XXXXXXXXX"
                  str(header$,23%,16%) = lastaccount$
                  str(header$,62%) = "GLBGIPUT: " & cms2v$

                  on fieldnr% gosub L40250,         /* ACCOUT NUMBER    */~
                                    L40220,         /* CUR ANNUAL BUDGET*/~
                                    L40310,         /* CUR BUDGET AMNTS */~
                                    L40220,         /* NXT ANNUAL BUDGET*/~
                                    L40350          /* NXT BUDGET AMNTS */
                     goto L40400

L40220:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40250:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      if screen% > 1% then lfac$(1%) = hex(8c)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return
L40310:           REM SET FAC'S FOR NUMERIC ONLY INPUT (SPECIAL)
                      lfac$(3%) = hex(82)
                      if periods% = 13% then lfac$(6%) = hex(82)
                      return
L40350:           REM SET FAC'S FOR NUMERIC ONLY INPUT (SPECIAL)
                      lfac$(5%) = hex(82)
                      if periods% = 13% then lfac$(7%) = hex(82)
                      return

L40400: accept                                                           ~
               at (01,02), "General Ledger Account Budgets Management",  ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                   ,ch(08),~
               at (02,02), fac(hex(ac)), header$                 ,ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "ACCOUNT NUMBER:",                            ~
               at (04,18), fac(lfac$(1%)), account$              ,ch(12),~
               at (04,31), fac(hex(8c)),  descr$                 ,ch(32),~
               at (04,65), fac(hex(8c)),  typedescr$             ,ch(12),~
               at (05,31), fac(hex(84)),  obmsg$                 ,ch(32),~
                                                                         ~
               at (06,02), fac(hex(ac)),  text$(1%)              ,ch(28),~
               at (07,02), fac(hex(8c)),  name$(1%)              ,ch(13),~
               at (07,17), fac(lfac$(2%)), budget$(1%)          , ch(13),~
               at (08,03), fac(hex(8c)),  name$(2%)              ,ch(13),~
               at (08,17), fac(lfac$(3%)), budget$(2%)          , ch(13),~
               at (09,03), fac(hex(8c)),  name$(3%)              ,ch(13),~
               at (09,17), fac(lfac$(3%)), budget$(3%)          , ch(13),~
               at (10,03), fac(hex(8c)),  name$(4%)              ,ch(13),~
               at (10,17), fac(lfac$(3%)), budget$(4%)          , ch(13),~
               at (11,03), fac(hex(8c)),  name$(5%)              ,ch(13),~
               at (11,17), fac(lfac$(3%)), budget$(5%)          , ch(13),~
               at (12,03), fac(hex(8c)),  name$(6%)              ,ch(13),~
               at (12,17), fac(lfac$(3%)), budget$(6%)          , ch(13),~
               at (13,03), fac(hex(8c)),  name$(7%)              ,ch(13),~
               at (13,17), fac(lfac$(3%)), budget$(7%)          , ch(13),~
               at (14,03), fac(hex(8c)),  name$(8%)              ,ch(13),~
               at (14,17), fac(lfac$(3%)), budget$(8%)          , ch(13),~
               at (15,03), fac(hex(8c)),  name$(9%)              ,ch(13),~
               at (15,17), fac(lfac$(3%)), budget$(9%)          , ch(13),~
               at (16,03), fac(hex(8c)),  name$(10%)             ,ch(13),~
               at (16,17), fac(lfac$(3%)), budget$(10%)         , ch(13),~
               at (17,03), fac(hex(8c)),  name$(11%)             ,ch(13),~
               at (17,17), fac(lfac$(3%)), budget$(11%)         , ch(13),~
               at (18,03), fac(hex(8c)),  name$(12%)             ,ch(13),~
               at (18,17), fac(lfac$(3%)), budget$(12%)         , ch(13),~
               at (19,03), fac(hex(8c)),  name$(13%)             ,ch(13),~
               at (19,17), fac(lfac$(3%)), budget$(13%)         , ch(13),~
               at (20,03), fac(hex(8c)),  name$(14%)             ,ch(13),~
               at (20,17), fac(lfac$(3%)), budget$(14%)         , ch(13),~
                                                                         ~
               at (06,39), fac(hex(ac)),  text$(2%)              ,ch(28),~
               at (07,39), fac(hex(8c)),  name$(28%)             ,ch(13),~
               at (07,54), fac(lfac$(4%)), budget$(28%)         , ch(13),~
               at (08,40), fac(hex(8c)),  name$(15%)             ,ch(13),~
               at (08,54), fac(lfac$(5%)), budget$(15%)         , ch(13),~
               at (09,40), fac(hex(8c)),  name$(16%)             ,ch(13),~
               at (09,54), fac(lfac$(5%)), budget$(16%)         , ch(13),~
               at (10,40), fac(hex(8c)),  name$(17%)             ,ch(13),~
               at (10,54), fac(lfac$(5%)), budget$(17%)         , ch(13),~
               at (11,40), fac(hex(8c)),  name$(18%)             ,ch(13),~
               at (11,54), fac(lfac$(5%)), budget$(18%)         , ch(13),~
               at (12,40), fac(hex(8c)),  name$(19%)             ,ch(13),~
               at (12,54), fac(lfac$(5%)), budget$(19%)         , ch(13),~
               at (13,40), fac(hex(8c)),  name$(20%)             ,ch(13),~
               at (13,54), fac(lfac$(5%)), budget$(20%)         , ch(13),~
               at (14,40), fac(hex(8c)),  name$(21%)             ,ch(13),~
               at (14,54), fac(lfac$(5%)), budget$(21%)         , ch(13),~
               at (15,40), fac(hex(8c)),  name$(22%)             ,ch(13),~
               at (15,54), fac(lfac$(5%)), budget$(22%)         , ch(13),~
               at (16,40), fac(hex(8c)),  name$(23%)             ,ch(13),~
               at (16,54), fac(lfac$(5%)), budget$(23%)         , ch(13),~
               at (17,40), fac(hex(8c)),  name$(24%)             ,ch(13),~
               at (17,54), fac(lfac$(5%)), budget$(24%)         , ch(13),~
               at (18,40), fac(hex(8c)),  name$(25%)             ,ch(13),~
               at (18,54), fac(lfac$(5%)), budget$(25%)         , ch(13),~
               at (19,40), fac(hex(8c)),  name$(26%)             ,ch(13),~
               at (19,54), fac(lfac$(5%)), budget$(26%)         , ch(13),~
               at (20,40), fac(hex(8c)),  name$(27%)             ,ch(13),~
               at (20,54), fac(lfac$(5%)), budget$(27%)         , ch(13),~
                                                                         ~
               at (22,02), fac(hex(a4)), message$                ,ch(79),~
               at (23,02), fac(hex(8c)), pfdescr$(1%)            ,ch(79),~
               at (24,02), fac(hex(8c)), pfdescr$(2%)            ,ch(79),~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 10% then L41250
                  budget$(28%) = budget$(1%)
                  for i% = 2% to 14%
                      budget$(i%+13%) = budget$(i%)
                  next i%
                  goto L40400

L41250:        if keyhit% <> 12% then L41330
                  annual = 0  :  budget$(1%) = "0.00"
                  call "STRING" addr("RJ", budget$(1%), 13%)
                  for i% = 2% to 14%
                      budget$(i%) = "0.00"
                      work(i%-1%) = 0
                      call "STRING" addr("RJ", budget$(i%), 13%)
                  next i%
                  goto L40400

L41330:        if keyhit% <> 14% then L41410
                  annual = 0  :  budget$(28%) = "0.00"
                  call "STRING" addr("RJ", budget$(28%), 13%)
                  for i% = 15% to 27%
                      budget$(i%) = "0.00"
                      work(i%-1%) = 0
                      call "STRING" addr("RJ", budget$(i%), 13%)
                  next i%
                  goto L40400

L41410:        if keyhit% <> 13% then L41450
                  call "MANUAL" ("GLBGIPUT")
                  goto L40400

L41450:        if keyhit% <> 15% then L41490
                  call "PRNTSCRN"
                  goto L40400

L41490:        close ws
               call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
               return

L42000: REM *************************************************************~
            *  I N P U T   /   E D I T   P E R I O D   A C T I V I T Y  *~
            *************************************************************
            header$ = "Period Activity Selection"
            str(header$,62%) = "GLBGIPUT: " & cms2v$
            pfdescr$(1%)= "(1)Start Over                          (13)Ins~
        ~tructions        (15)Print Screen"
            pfdescr$(2%)= "                                              ~
        ~                 (16)Return"
            if fieldnr% = 0% then message$ = "Position Cursor and press"&~
                " RETURN to Modify." else message$ = "Enter a non-blank"&~
                " beside each period's activity you want to use."
            if fieldnr% = 0% then init(hex(86)) lfac$()                  ~
                             else init(hex(80)) lfac$()
            init(hex(8c)) lfac$(3%)
            if periods% = 12% then lfac$(2%), lfac$(3%) = hex(90)
            pfkeys$ = hex(0001100d0fffffffffffffffffffffff)

L42100: accept                                                           ~
               at (01,02), "General Ledger Account Budgets Management",  ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                   ,ch(08),~
               at (02,02), fac(hex(ac)), header$                 ,ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "PREVIOUS YEAR PERIOD ACTIVITY",              ~
               at (04,40), "CURRENT YEAR PERIOD ACTIVITY",               ~
                                                                         ~
               at (06,02), fac(lfac$(1%)), py$(1%)               ,ch(01),~
               at (06,06), fac(hex(8c)),   fdate$(1%)            ,ch(08),~
               at (06,16), fac(hex(8c)),   prevpa$(1%)           ,ch(13),~
               at (07,02), fac(lfac$(1%)), py$(2%)               ,ch(01),~
               at (07,06), fac(hex(8c)),   fdate$(2%)            ,ch(08),~
               at (07,16), fac(hex(8c)),   prevpa$(2%)           ,ch(13),~
               at (08,02), fac(lfac$(1%)), py$(3%)               ,ch(01),~
               at (08,06), fac(hex(8c)),   fdate$(3%)            ,ch(08),~
               at (08,16), fac(hex(8c)),   prevpa$(3%)           ,ch(13),~
               at (09,02), fac(lfac$(1%)), py$(4%)               ,ch(01),~
               at (09,06), fac(hex(8c)),   fdate$(4%)            ,ch(08),~
               at (09,16), fac(hex(8c)),   prevpa$(4%)           ,ch(13),~
               at (10,02), fac(lfac$(1%)), py$(5%)               ,ch(01),~
               at (10,06), fac(hex(8c)),   fdate$(5%)            ,ch(08),~
               at (10,16), fac(hex(8c)),   prevpa$(5%)           ,ch(13),~
               at (11,02), fac(lfac$(1%)), py$(6%)               ,ch(01),~
               at (11,06), fac(hex(8c)),   fdate$(6%)            ,ch(08),~
               at (11,16), fac(hex(8c)),   prevpa$(6%)           ,ch(13),~
               at (12,02), fac(lfac$(1%)), py$(7%)               ,ch(01),~
               at (12,06), fac(hex(8c)),   fdate$(7%)            ,ch(08),~
               at (12,16), fac(hex(8c)),   prevpa$(7%)           ,ch(13),~
               at (13,02), fac(lfac$(1%)), py$(8%)               ,ch(01),~
               at (13,06), fac(hex(8c)),   fdate$(8%)            ,ch(08),~
               at (13,16), fac(hex(8c)),   prevpa$(8%)           ,ch(13),~
               at (14,02), fac(lfac$(1%)), py$(9%)               ,ch(01),~
               at (14,06), fac(hex(8c)),   fdate$(9%)            ,ch(08),~
               at (14,16), fac(hex(8c)),   prevpa$(9%)           ,ch(13),~
               at (15,02), fac(lfac$(1%)), py$(10%)              ,ch(01),~
               at (15,06), fac(hex(8c)),   fdate$(10%)           ,ch(08),~
               at (15,16), fac(hex(8c)),   prevpa$(10%)          ,ch(13),~
               at (16,02), fac(lfac$(1%)), py$(11%)              ,ch(01),~
               at (16,06), fac(hex(8c)),   fdate$(11%)           ,ch(08),~
               at (16,16), fac(hex(8c)),   prevpa$(11%)          ,ch(13),~
               at (17,02), fac(lfac$(1%)), py$(12%)              ,ch(01),~
               at (17,06), fac(hex(8c)),   fdate$(12%)           ,ch(08),~
               at (17,16), fac(hex(8c)),   prevpa$(12%)          ,ch(13),~
               at (18,02), fac(lfac$(2%)), py$(13%)              ,ch(01),~
               at (18,06), fac(lfac$(3%)), fdate$(13%)           ,ch(08),~
               at (18,16), fac(lfac$(3%)), prevpa$(13%)          ,ch(13),~
                                                                         ~
               at (06,40), fac(lfac$(1%)), cy$(1%)               ,ch(01),~
               at (06,44), fac(hex(8c)),   fdate$(14%)           ,ch(08),~
               at (06,54), fac(hex(8c)),   currpa$(1%)           ,ch(13),~
               at (07,40), fac(lfac$(1%)), cy$(2%)               ,ch(01),~
               at (07,44), fac(hex(8c)),   fdate$(15%)           ,ch(08),~
               at (07,54), fac(hex(8c)),   currpa$(2%)           ,ch(13),~
               at (08,40), fac(lfac$(1%)), cy$(3%)               ,ch(01),~
               at (08,44), fac(hex(8c)),   fdate$(16%)           ,ch(08),~
               at (08,54), fac(hex(8c)),   currpa$(3%)           ,ch(13),~
               at (09,40), fac(lfac$(1%)), cy$(4%)               ,ch(01),~
               at (09,44), fac(hex(8c)),   fdate$(17%)           ,ch(08),~
               at (09,54), fac(hex(8c)),   currpa$(4%)           ,ch(13),~
               at (10,40), fac(lfac$(1%)), cy$(5%)               ,ch(01),~
               at (10,44), fac(hex(8c)),   fdate$(18%)           ,ch(08),~
               at (10,54), fac(hex(8c)),   currpa$(5%)           ,ch(13),~
               at (11,40), fac(lfac$(1%)), cy$(6%)               ,ch(01),~
               at (11,44), fac(hex(8c)),   fdate$(19%)           ,ch(08),~
               at (11,54), fac(hex(8c)),   currpa$(6%)           ,ch(13),~
               at (12,40), fac(lfac$(1%)), cy$(7%)               ,ch(01),~
               at (12,44), fac(hex(8c)),   fdate$(20%)           ,ch(08),~
               at (12,54), fac(hex(8c)),   currpa$(7%)           ,ch(13),~
               at (13,40), fac(lfac$(1%)), cy$(8%)               ,ch(01),~
               at (13,44), fac(hex(8c)),   fdate$(21%)           ,ch(08),~
               at (13,54), fac(hex(8c)),   currpa$(8%)           ,ch(13),~
               at (14,40), fac(lfac$(1%)), cy$(9%)               ,ch(01),~
               at (14,44), fac(hex(8c)),   fdate$(22%)           ,ch(08),~
               at (14,54), fac(hex(8c)),   currpa$(9%)           ,ch(13),~
               at (15,40), fac(lfac$(1%)), cy$(10%)              ,ch(01),~
               at (15,44), fac(hex(8c)),   fdate$(23%)           ,ch(08),~
               at (15,54), fac(hex(8c)),   currpa$(10%)          ,ch(13),~
               at (16,40), fac(lfac$(1%)), cy$(11%)              ,ch(01),~
               at (16,44), fac(hex(8c)),   fdate$(24%)           ,ch(08),~
               at (16,54), fac(hex(8c)),   currpa$(11%)          ,ch(13),~
               at (17,40), fac(lfac$(1%)), cy$(12%)              ,ch(01),~
               at (17,44), fac(hex(8c)),   fdate$(25%)           ,ch(08),~
               at (17,54), fac(hex(8c)),   currpa$(12%)          ,ch(13),~
               at (18,40), fac(lfac$(2%)), cy$(13%)              ,ch(01),~
               at (18,44), fac(lfac$(3%)), fdate$(26%)           ,ch(08),~
               at (18,54), fac(lfac$(3%)), currpa$(13%)          ,ch(13),~
                                                                         ~
               at (21,02), fac(hex(a4)), message$                ,ch(79),~
               at (22,02), fac(hex(8c)), pfdescr$(1%)            ,ch(79),~
               at (23,02), fac(hex(8c)), pfdescr$(2%)            ,ch(79),~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 13% then L42950
                   call "MANUAL" ("GLBGIPUT")
                   goto L42100

L42950:        if keyhit% <> 15 then L42990
                   call "PRNTSCRN"
                   goto L42100

L42990:        close ws
               call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                    S E T   K E Y S                        *~
            *                                                           *~
            * Sets PF Keys & Descriptions Based On SCREEN%...           *~
            *************************************************************

        set_keys                 /* Cleansliness Is Next To Godliness? */
            on screen% goto L49110,         /* Input Mode               */~
                            L49280,         /* Edit Mode                */~
                            L49370          /* Field Edit               */

L49110: REM  =-=-=-=-=-=-=-=-=-= Header, Input mode =-=-=-=-=-=-=-=-=-=-=
            pfdescr$(1%)= "(1)Start Over    (4)Previous Field     (13)Ins~
        ~tructions        (15)Print Screen"
            pfdescr$(2%)= "       (8)See Period Activity               (1~
        ~0)Copy           (16)Exit Program"
            pfkeys$ = hex(000104080a0d0f10ffffffffffffffff)
            str(pfdescr$(2%),63%,1%) = hex(84)

*        Flip Off Appropriate Fields
            if fieldnr% = 1% then L49240
                str(pfdescr$(2%),63%)    = " "  /* Shut Off Exit Optn  */
                str(pfkeys$,8%,1%) = hex(ff)
                goto L49260
L49240:     str(pfdescr$(1%),,35%) = " "        /* Shut Off Prev Field */
            str(pfkeys$,3%,1%) = hex(ff)
            str(pfdescr$(2%),1,55%) = " "       /* Shut Off PA and Copy*/
            str(pfkeys$,4%,2%) = hex(ffff)
L49260: return

L49280: REM =-=-=-=-=-=-=-=-=-= Header, Edit mode =-=-=-=-=-=-=-=-=-=-=-=
            pfdescr$(1%)= "(1)Start Over                          (13)Ins~
        ~tructions        (15)Print Screen"
            pfdescr$(2%)= "(10)Copy       (12)Clear Curr          (14)Cle~
        ~ar Next          (16)Save Budgets"
            pfkeys$ = hex(00010a0c0d0e0f10ffffffffffffffff)
            str(pfdescr$(2%),63%,1%) = hex(84)
        return

L49370: REM =-=-=-=-=-=-=-= All Screens, Field Edit =-=-=-=-=-=-=-=-=-=
            pfdescr$(1%)= "(1)Start Over                          (13)Ins~
        ~tructions        (15)Print Screen"
            pfdescr$(2%)= "(ENTER) Validate Modification(s)              ~
        ~                                 "
            pfkeys$ = hex(00010d0e0fffffffffffffffffffffff)
        return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50150,         /* ACCOUNT NUMBER   */~
                                    L50380,         /* CUR ANNUAL BUDGET*/~
                                    L50490,         /* CUR BUDGET AMNTS */~
                                    L50590,         /* NXT ANNUAL BUDGET*/~
                                    L50700          /* NXT BUDGET AMNTS */
                     return

L50150:     REM TEST DATA FOR ACCOUNT NUMBER
                obmsg$ = " "
                f1%(1%) = -999%  /* Include Obsolete Accounts */
                call "GETCODE" (#1, account$, descr$, 1%, 0, f1%(1%))
                if f1%(1%) <> 0% then L50200
                    errormsg$ = hex(00)
                    return
L50200:         get #1, using L50210, type$, ob%
L50210:         FMT XX(39), CH(1), BI(1)
                typedescr$ = "Unknown"
                if type$ = "A" then typedescr$ = "Asset"
                if type$ = "$" then typedescr$ = "Cash"
                if type$ = "L" then typedescr$ = "Liability"
                if type$ = "C" then typedescr$ = "Capital"
                if type$ = "R" then typedescr$ = "Revenue"
                if type$ = "E" then typedescr$ = "Expense"
                call "PUTPAREN" (typedescr$)
                if ob%=1% or ob%=2% then obmsg$ = "(Obsolete Account)"
                gosub L30000
                if onfile% <> 0% then L50350
                   if type$ <> "L" and type$ <> "R" then return
                   budget$(1%) = "-XX.XX"
                   return
L50350:         return clear all
                goto editmode

L50380:     REM TEST DATA FOR CURRENT YEARS ANNUAL BUDGET
            call "NUMTEST" (budget$(1%), -9e10,9e10, errormsg$,-2.2, temp)
            if abs(temp) < .005 then L50460
                temp = temp/periods%
                for i% = 2% to 1% + periods%
                    call "CONVERT" (temp, 2.2, budget$(i%))
                next i%
                return
L50460:     for i% = 1% to 14%
                budget$(i%) = "0.00"
                call "STRING" addr("RJ", budget$(i%), 13%)
            next i%
            return

L50490:     REM TEST DATA FOR CURRENT YEARS BUDGET AMOUNTS
            annual = 0
            for i% = 2% to 1% + periods%
                call"NUMTEST"(budget$(i%),-9e10,9e10,errormsg$,-2.2,temp)
                if errormsg$ <> " " then return
            annual = annual + temp
            next i%
            convert annual to budget$(1%), pic(-#########.##)
            return

L50590:     REM TEST DATA FOR NEXT YEARS ANNUAL BUDGET
            call "NUMTEST" (budget$(28%), -9e10,9e10, errormsg$,-2.2,temp)
            if abs(temp) < .005 then L50670
                temp = temp/periods%
                for i% = 15% to 14% + periods%
                    call "CONVERT" (temp, 2.2, budget$(i%))
                next i%
                return
L50670:     for i% = 15% to 28%
                budget$(i%) = "0.00"
                call "STRING" addr("RJ", budget$(i%), 13%)
            next i%
            return

L50700:     REM TEST DATA FOR NEXT YEARS BUDGET AMOUNTS
            annual = 0
            for i% = 15% to 14% + periods%
                call"NUMTEST"(budget$(i%),-9e10,9e10,errormsg$,-2.2,temp)
                if errormsg$ <> " " then return
            annual = annual + temp
            next i%
            convert annual to budget$(28%), pic(-#########.##)
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

            call "SHOSTAT" ("One Moment Please")
            end
