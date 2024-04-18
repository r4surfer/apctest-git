        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   GGG   L      BBBB    GGG   RRRR   PPPP   TTTTT          *~
            *  G      L      B   B  G      R   R  P   P    T            *~
            *  G GGG  L      BBBB   G GGG  RRRR   PPPP     T            *~
            *  G   G  L      B   B  G   G  R   R  P        T            *~
            *   GGG   LLLLL  BBBB    GGG   R   R  P        T            *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GLBGRPT  - Program prints a report of the budgets entered *~
            *            for the general ledger accounts.  The user can *~
            *            select and print a range of accounts for a     *~
            *            range of periods for the current, previous or  *~
            *            next fiscal year.                              *~
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
            * 07/19/85 ! ORIGINAL                                 ! SGA *~
            * 03/04/86 ! Change for unformatted Fiscal Dates      ! ERN *~
            * 11/11/86 ! Proper handling of encrypted account #s  ! HES *~
            * 08/03/87 ! Proper Clearing of Report Totals,        !     *~
            *          ! Fixed EDIT Mode for Correct Operation,   !     *~
            *          ! Added GLVALID to format Account Numbers. ! DAW *~
            * 10/21/91 ! Chg'd "PERIOD" to "YEAR" on input as to  ! MLJ *~
            *          !   reflect actual report contents, also   !     *~
            *          !   added SELECT PRINTER(134) & SETPRNT.   !     *~
            * 01/11/93 ! Page 0 Facs,         & End Report Time.  ! RJH *~
	    * 06/14/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            account$12,                  /* G/L ACCOUNT NUMBER         */~
            amount(42),                  /* AMOUNT ARRAY FOR BUDGET    */~
            amount$(42)10,               /* AMOUNT ARRAY FOR BUDGET    */~
            current(13),                 /* AMOUNT ARRAY FOR CURRENT BU*/~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            compname$60,                 /* Company name for header    */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            dates$(32)8,                 /* FISCAL DATE STRUCTURE      */~
            descrp$30,                   /* ACCOUNT DESCRIPTION        */~
            end_date$8,                  /* ENDING REPORT DATE         */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            first_account$12,            /* Account Range              */~
            future(13),                  /* BUDGET FUTURE AMOUNT       */~
            grand_total$13,              /* GRAND TOTAL FOR REPORT     */~
            hdr$60,                      /* Header for ASKUSER         */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            last_account$12,             /* Account Range              */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            line2$79,                    /* Screen line #2             */~
            message$79,                  /* INPUT MESSAGE              */~
            name$(42)13,                 /* DATES FOR DISPLAY          */~
            period$(13)8,                /* PERIOD END DATES           */~
            previous(13),                /* Previous Budget amount     */~
            report_total(13),            /* TOTAL ARRAY FOR REPORT     */~
            report_total$(13)10,         /* TOTALS FOR REPORT          */~
            report_type$1,               /* Period for Budget Report   */~
	    tdate$8,                     /* Temporary Date             */~
            text$(2)40,                  /* SCREEN TEXT                */~
            time$8,                      /* System Time                */~
            total$13,                    /* LINE TOTAL AMOUNT          */~
            type$20,                     /* REPORT DESCRIPTION         */~
	    udate$8 			 /* Unformated text date       */

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
            * #1  ! GLMAIN   ! General Ledger Main File                 *~
            * #2  ! GLBUDGET ! General ledger budget file               *~
            * #3  ! SYSFILE2 ! Caelus Management System Information     *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  300,                                  ~
                        keypos =    1, keylen =   9                      ~

            select #2,  "GLBUDGET",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos =    1, keylen =   9                      ~

            select #3,  "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos =    1, keylen =  20                      ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENFILE" (#1,  "SHARE", f2%(1 ), rslt$(1 ), axd$(1 ))
            call "OPENFILE" (#2,  "SHARE", f2%(2 ), rslt$(2 ), axd$(2 ))
            call "OPENFILE" (#3,  "SHARE", f2%(3 ), rslt$(3 ), axd$(3 ))
            if f2%(1) = 0 then L02380
                nofile$ = "GLMAIN"
                goto L02410
L02380:     if f2%(3) = 0 then L09000      /* Initialize                 */
                nofile$ = "SYSFILE2"

L02410:     ask% = 2%
            call "ASKUSER" (ask%, "***** MISSING FILE *****",            ~
                 "The file " & nofile$ & " is either missing",           ~
                 " or Unavailable at this time",                         ~
                 "Press RETURN to exit and correct this problem")
            if ask% <> 0% then L02410
            goto L65000

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            select printer(134)
            date$ = date
            call "DATEFMT" (date$)
            call "TIME" (time$)
            call "COMPNAME" (12%, compname$, 0%)

            message$ = "To Modify Displayed Values, Position Cursor To De~
        ~sired Value And Press (RETURN)."

            call "READ100" (#3, "FISCAL DATES", f1%(3))
            if f1%(3) <> 0 then L09250
L09170:         ask% = 2%
                hdr$ = "***** FISCAL DATE ERROR *****"
                call "ASKUSER" (ask%, hdr$, "An error has occurred in Fin~
        ~ding the Fiscal Dates", "Press 'RETURN' to EXIT this program",   ~
                "And Correct the Problem")
                if ask% <> 0% then L09170
                goto L65000

L09250:     get #3, using L09260, periods%, dates$()
L09260:             FMT XX(20), BI(2), XX(136), XX(2), 32*CH(8)

        REM SET UP THE MONTH'S NAMES FOR THE ERRORMSG
            for i% = 15% to 27%
		tdate$ = dates$(i%+1%)
		call "DATEFMT" (tdate$, 0%, udate$)
                name$(i%), name$(i%-13%) = udate$
                convert str(name$(i%-13%),1%,4%) to temp%, data goto L09350
                convert temp% + 1% to str(name$(i%),1%,4%), pic(0000)
                call "DATEFMT" (name$(i%))
                call "DATEFMT" (name$(i%-13%))
L09350:     next i%
            for i% = 2% to 14%
                name$(i%+28%) = dates$(i%)
                call "DATEFMT" (name$(i%+28%))
            next i%
            if periods% = 12% then name$(14%), name$(27%) = " "
            name$(1%), name$(28%) = "Annual Budget"

            text$(1) = "Current Year Budget"
            text$(2) = "Next Year Budget"
            str(line2$,62) = " GLBGRPT: " & str(cms2v$,1,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
            inp% = 1%
            gosub L29000  /* Clear Variables */
            call "SETPRNT" ("G/L004", " ", 0%, 0%)

            for fieldnr% = 1 to  3
                gosub'051(fieldnr%)      /* Default, Enables Page 1    */
                      if enabled% = 0 then L10180
L10120:         gosub'101(fieldnr%)      /* Input Mode Page 1          */
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L10120
                gosub'151(fieldnr%)      /* Test Data Input            */
                      if errormsg$ <> " " then L10120
L10180:     next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

L11052:     message$ = "To Modify Displayed Values, Position Cursor to De~
        ~sired Value and Press (RETURN)"
            lastfieldnr% = 0%
            inp% = 0%
L11060:     gosub'111(0%)                /* Edit Mode Screen Page 1    */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L11060
L11100:     fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% >  3 then L11052
            if fieldnr% = lastfieldnr% then L11052
            gosub'051(fieldnr%)          /* Set Input Messages         */
L11130:     gosub'111(fieldnr%)          /* Edit Mode Screen           */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11130
            gosub'151(fieldnr%)          /* Test Data Entered          */
                  if errormsg$ <> " " then L11130
                     lastfieldnr% = fieldnr%
            goto L11100

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            goto L30000                   /* Load Data for Report       */

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)          /* Default, Enables Page 1    */
                  enabled% = 1%
                  on fieldnr% gosub L20100,         /* Period for Report*/~
                                    L20300,         /* From Account     */~
                                    L20400          /* To Account       */
                     return
L20100: REM DEFAULT/ENABLE FOR Year for Budget Report
                message$ = "Enter 'P' for previous, 'C' for current or 'N~
        ~' for next year."
                return
L20300: REM DEFAULT/ENABLE FOR Account Range
                message$ = "Enter beginning account number or leave blank~
        ~ to search for desired account."
                return
L20400: REM DEFAULT/ENABLE FOR Account Range
                message$ = "Enter ending account number or leave blank to~
        ~ search for desired account."
                return

L29000: REM *************************************************************~
            * INITIALIZATION BLOCK (NEATER THAN CRAMMING AT 10000)      *~
            *************************************************************

            report_type$,                /* PERIOD OF REPORT           */~
            first_account$,              /* FIRST GL ACCT FOR REPORT   */~
            last_account$,               /* LAST GL ACCT FOR REPORT    */~
            type$ = " "                  /* REPORT TYPE DESCRIPTION    */
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

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

            keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return

            return clear all
            goto inputmode

L30000: REM *************************************************************~
            *                  LOAD DATA FOR REPORT                     *~
            *************************************************************

            call "SHOSTAT" ("Compiling Budget Report, One Moment Please")

            pagenumber% = -1%
            linecount% = 1000%
            select printer(134)
            call "SETPRNT" ("G/L004", " ", 0%, 0%)
            acctplow$  = first_account$
            call "GLUNFMT" (acctplow$)
            str(acctplow$,,9) = addc all(hex(ff))
            if first_account$ <> "ALL" then L30130
                acctplow$  = all(hex(00))
                last_account$ = all(hex(ff))
L30130:     total = 0
            call "PLOWNEXT" (#1, acctplow$, 0%, f1%(1))
                  if f1%(1) = 0% then L30670
            get #1, using L30170, account$, descrp$
L30170:         FMT CH(9), CH(30), XX(261)
            call "GLFMT" (account$)
            if account$ > last_account$ then L30670

            mat previous = zer
            mat current  = zer
            mat future   = zer
            call "READ100" (#2, acctplow$, f1%(2))
                  if f1%(2) = 0% then L30300
            get #2, using L30270, previous(), current(), future()
L30270:        FMT XX(17), 13*PD(14,4), 13*PD(14,4), 13*PD(14,4)


L30300:     if report_type$ <> "C" then L30400      /* Check for 'N'    */
            for i% = 2 to 14
                period$(i%-1) = name$(i%)
                amount(i%-1) = current(i%-1)
                total= total + amount(i%-1)
                call "CONVERT" (amount(i%-1), 2.2, amount$(i%-1))
                next i%
                call "CONVERT" (total, 2.2, total$)
                goto L30600     /* Got a Good One                       */

L30400:     if report_type$ <> "N" then L30500      /* Check for 'P'    */
            for i% = 15 to 27
                period$(i%-14) = name$(i%)
                amount(i%-14) = future(i%-14)
                total = total + amount(i%-14)
                call "CONVERT" (amount(i%-14), 2.2, amount$(i%-14))
                next i%
                call "CONVERT" (total, 2.2, total$)
                goto L30600     /* Got a Good One                       */

L30500:     if report_type$ <> "P" then L65000
            for i% = 30 to 42
                period$(i%-29) = name$(i%)
                amount(i%-29) = previous(i%-29)
                total = total + amount(i%-29)
                call "CONVERT" (amount(i%-29), 2.2, amount$(i%-29))
                next i%
                call "CONVERT" (total, 2.2, total$)

L30600:     if period$(13) <> " " then end_date$ = period$(13) else      ~
                                       end_date$ = period$(12)
            if period$(13) = " " then amount$(13) = " "
            if pagenumber% < 0 then gosub print_params
            if linecount% > 55 then gosub new_page
            gosub print_line
            goto L30130    /* Plow Again                                */

L30670:     if linecount% > 55 then gosub new_page
            gosub print_range_totals
            print skip(2)
            time$ = " "  :  call "TIME" (time$)
            print using L31770, time$       /* End of report line */
            close printer
            call "SETPRNT" ("G/L004", " ", 0%, 1%)
            goto inputmode

        new_page

            print page
            pagenumber% = pagenumber% + 1
            print using L31720, date$, time$, compname$
            print using L31480, end_date$, pagenumber%
            print
            print using L31500
            print using L31530
            print using L31560
            print using L31500
            linecount% = 7
            return

        print_line
            print using L31620, " ", " ", period$(01), amount$(01),       ~
                                         period$(04), amount$(04),       ~
                                         period$(07), amount$(07),       ~
                                         period$(10), amount$(10)

            print using L31620, account$, descrp$,                        ~
                               period$(02), amount$(02),                 ~
                               period$(05), amount$(05),                 ~
                               period$(08), amount$(08),                 ~
                               period$(11), amount$(11)

            print using L31620, " ", " ", period$(03), amount$(03),       ~
                                         period$(06), amount$(06),       ~
                                         period$(09), amount$(09),       ~
                                         period$(12), amount$(12)

            print using L31650, period$(13), amount$(13),                 ~
                               total$

            print using L31590

            for t% = 1 to 13
             report_total(t%) = report_total(t%) + amount(t%)
             next t%
            linecount% = linecount% + 5
            return

        print_range_totals
            for t% = 1 to 13
             grand_total = grand_total + report_total(t%)
             call "CONVERT" (report_total(t%), 2.2, report_total$(t%))
             next t%
             call "CONVERT" (grand_total, 2.2, grand_total$)

            if period$(13) = " " then report_total$(13) = " "

            print
            print using L31500
            print using L31620, " ", " ", period$(01), report_total$(01), ~
                                         period$(04), report_total$(04), ~
                                         period$(07), report_total$(07), ~
                                         period$(10), report_total$(10)

            print using L31620, "TOTALS","TOTALS FOR REPORT RANGE",       ~
                               period$(02), report_total$(02),           ~
                               period$(05), report_total$(05),           ~
                               period$(08), report_total$(08),           ~
                               period$(11), report_total$(11)

            print using L31620, " ", " ", period$(03), report_total$(03), ~
                                         period$(06), report_total$(06), ~
                                         period$(09), report_total$(09), ~
                                         period$(12), report_total$(12)

            print using L31650, period$(13), report_total$(13),           ~
                               grand_total$

            print using L31500
            for t% = 1% to 13%
                report_total(t%) = 0%
            next t%
            grand_total = 0%
            return

L31480: %                                            BUDGET REPORT FOR TH~
        ~E YEAR ENDING ########                               PAGE: ####
L31500: %+------------+------------------------------+--------+----------~
        ~-+--------+-----------+--------+-----------+--------+------------~
        ~+
L31530: %!  ACCOUNT   !           ACCOUNT            !        !          ~
        ~ !        !           !        !           !        !            ~
        ~!
L31560: %!  NUMBER    !         DESCRIPTION          !  DATE  !  AMOUNT  ~
        ~ !  DATE  !   AMOUNT  ! DATE   !   AMOUNT  !  DATE  !   AMOUNT   ~
        ~!
L31590: %!------------!------------------------------!--------!----------~
        ~-!--------!-----------!--------!-----------!--------!------------~
        ~!
L31620: %!############!##############################!########!##########~
        ~ !########!########## !########!########## !########! ###########~
        ~!
L31650: %!            !                              !########!##########~
        ~ !                     *******************  TOTAL  ##############~
        ~!
        %!            !                              !--------!----------~
        ~-!--------!-----------!--------!-----------!--------!------------~
        ~!

L31720: % RUN ######## ########              ############################~
        ~##################################                GLBGRPT: G/L004

L31770:       %                            * * * * * * * * * *   E N D   ~
        ~O F   R E P O R T   @  ########  * * * * * * * * * *

        print_params
            pagenumber% = pagenumber% + 1
            print page
            print using L31720, date$, time$, compname$
            print using L31480, end_date$, pagenumber%
            print skip(2)
L32022:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then L32040
                str(i$(), i%, 1%) = hex(20)
                goto L32022
L32040:     print skip(3)
            print tab(37);
            print "--------------------- Report Selection Parameters ----~
        ~---------"
            for x% = 6% to 17%: print tab(37); i$(x%) : next x%
            print tab(37);
            print "------------------------------------------------------~
        ~---------"

            return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *                                                           *~
            * FORMAT STATEMENTS FOR DATA FILES.                         *~
            *************************************************************

        FMT                 /* FILE: GLMAIN                            */~
            CH(9),          /* general ledger account number           */~
            CH(30),         /* general ledger account description      */~
            CH(1),          /* general ledger account type (a,l,c,r,e,$*/~
            BI(4),          /* next detail sequence number - gldetail  */~
            PD(14,4),       /* Balance forward (two years back)        */~
            13*PD(14,4),    /* same as current except one year back    */~
            PD(14,4),       /* Closing entries bucket for current year */~
            13*PD(14,4),    /* 13 period activity.1=first period in FY,*/~
            4*PD(14,4)      /* Place to put new years active if old yea*/~

        FMT                 /* FILE: GLBUDGET                          */~
            CH(9),          /* G/L account number.                     */~
            CH(8),          /* Filler For Rest of Record or Internal Sp*/~
            13*PD(14,4),    /* same as current except one year back    */~
            13*PD(14,4),    /* 13 period activity.1=first period in FY,*/~
            13*PD(14,4),    /* 13 period Budgets For Next Year         */~
            CH(171)         /* Filler For Rest of Record or Internal Sp*/~

        FMT                 /* FILE: SYSFILE2                          */~
            CH(20),         /* Acts as variable text key to SYSFILE2 sy*/~
            CH(480)         /* Filler For Rest of Record or Internal Sp*/

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L40160,         /* Type of Report   */~
                                    L40160,         /* Account Range    */~
                                    L40160          /* Account Range    */
                     goto L40230

L40160:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return

L40230:     accept                                                       ~
               at (01,02),                                               ~
                  "General Ledger Budget Report",                        ~
               at (01,67),                                               ~
                  "Date:",                                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Year for Budget Report",                              ~
               at (06,30), fac(lfac$( 1)), report_type$         , ch(01),~
               at (06,43), fac(hex(8c)), type$                  , ch(20),~
               at (07,02),                                               ~
                  "Account Range From:",                                 ~
               at (07,30), fac(lfac$( 2)), first_account$       , ch(12),~
               at (08,18), "To:",                                        ~
               at (08,30), fac(lfac$( 3)), last_account$        , ch(12),~
                                                                         ~
               at (21,02), fac(hex(a4)),   message$             , ch(79),~
               at (23,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)Exit Program",                                    ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L40610
                  call "MANUAL" ("GLBGRPT")
                  goto L40230

L40610:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40230

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 1 OF DOCUMENT.                    *~
            *************************************************************

            deffn'111(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L41160,         /* Type of Report   */~
                                    L41160,         /* Account Range    */~
                                    L41160          /* Account Range    */
                     goto L41230

L41160:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return

L41230:     accept                                                       ~
               at (01,02),                                               ~
                  "General Ledger Budget Report",                        ~
               at (01,67),                                               ~
                  "Date:",                                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Year for Budget Report",                              ~
               at (06,30), fac(lfac$( 1)), report_type$         , ch(01),~
               at (06,43), fac(hex(8c)), type$                  , ch(20),~
               at (07,02),                                               ~
                  "Account Range From:",                                 ~
               at (07,30), fac(lfac$( 2)), first_account$       , ch(12),~
               at (08,18), "To:",                                        ~
               at (08,30), fac(lfac$( 3)), last_account$        , ch(12),~
                                                                         ~
               at (21,02), fac(hex(a4)),      message$          , ch(79),~
               at (23,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)Print Report",                                    ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L41610
                  call "MANUAL" ("GLBGRPT")
                  goto L41230

L41610:        if keyhit% <> 15 then L41650
                  call "PRNTSCRN"
                  goto L41230

L41650:        close ws
               call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50120,         /* Type of Report   */~
                                    L50210,         /* Account Range    */~
                                    L50310          /* Account Range    */
                     return
L50120: REM TEST DATA FOR Period for Budget Report
                if report_type$ = "C" then type$ = "Current Year"
                if report_type$ = "C" then L50200
                if report_type$ = "N" then type$ = "Next Year"
                if report_type$ = "N" then L50200
                if report_type$ = "P" then type$ = "Previous Year"
                if report_type$ = "P" then L50200
                errormsg$ = "Invalid Selection, Please respecify."
L50200:         return
L50210: REM Test Data for Account Range
                if first_account$ = "ALL" then return
*        The -999% allows for obsolete codes to be entered
                f1%(1) = -999%
                descrp$ = hex(0694) & "Select STARTING Account #"
                call "GETCODE" (#1, first_account$,descrp$, 1%, 0, f1%(1))
                if f1%(1) <> 0 then goto L50300
                    call "GLVALID" (first_account$)
                    if errormsg$ <> " " then return
*        If INP% = 1%, you got here from INPUT, so go back for next field
L50300:         if inp% = 1% then return
L50310: REM Test Date for Account Range
*        The -999% allows for obsolete codes to be entered
                f1%(1) = -999%
                descrp$ = hex(0694) & "Select ENDING Account #"
                call "GETCODE" (#1, last_account$, descrp$, 1%, 0, f1%(1))
                if f1%(1) <> 0 then return
                    call "GLVALID" (last_account$)
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
