        REM *************************************************************~
            *                                                           *~
            *   GGG   L      BBBB    AAA   L       SSS   H   H  TTTTT   *~
            *  G      L      B   B  A   A  L      S      H   H    T     *~
            *  G GGG  L      BBBB   AAAAA  L       SSS   HHHHH    T     *~
            *  G   G  L      B   B  A   A  L          S  H   H    T     *~
            *   GGG   LLLLL  BBBB   A   A  LLLLL   SSS   H   H    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GLBALSHT - THIS PROGRAM PRINTS THE BALANCE SHEET. IT SORTS*~
            *            THE CASH AND CAPITAL AND ASSET ACCOUNTS AND    *~
            *            PUTS THEM UNDER ONE HEADING. THEN SORTS THE    *~
            *            LIABILITY ACCOUNTS AND PUTS THEM UNDER ANOTHER.*~
            *            THIS IS THE *BASIC* BALANCE SHEET....          *~
            *            PROFIT/(LOSS) IS CALCLUATED AS TOTAL ASSETS -  *~
            *            TOTAL LIABILITIES.                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/31/81 ! ORIGINAL                                 ! TOM *~
            * 11/11/82 ! NEW BALANCE FORMAT (), CLEANUP           ! ECR *~
            * 03/04/86 ! Changes due to Fiscal Dates reformat     ! ERN *~
            * 08/24/87 ! W.R. Grace accountancy (dual books) mods.! JIM *~
            * 08/09/88 ! Dual Books depends on a SYSFILE2 flag.   ! JIM *~
            * 06/15/90 ! Changed Report ID to G/L008              ! MJB *~
	    * 06/14/96 ! Changes for the year 2000                ! DXL *~
            *************************************************************

        dim                                                              ~
            account$16,                  /* For print                  */~
            acctdescr$32,                /* GL ACCOUNT DESCRIPTION     */~
            accttype$1,                  /* GL ACCOUNT TYPE            */~
            coname$50,                   /* Company name               */~
            currentbal$20,               /* CURRENT BALANCE FORMATTED  */~
            date$8,                      /* FORMATTED G/L DATE         */~
            dates$(32)8,                 /* FISCAL DATES               */~
            dual_books$1,                /* Dual books in effect?      */~
            errormsg$79,                 /* ERROR MESSAGE FOR DISPLAY  */~
            fac$(2)1,                    /* Field FACs                 */~
            heading$50,                  /* BALANCE SHEET HEADING      */~
            letsdo$10,                   /* SWITCH FOR ACCOUNT TYPES   */~
            line2$79,                    /* Screen line 2              */~
            line21$79,                   /* Screen line 21             */~
            months$36,                   /* MONTHS LIST AVAILABLE      */~
            mdate$(3)12,                 /* DATES OF AVAILABLE MONTHS  */~
            readkey$50,                  /* PLOW KEY                   */~
            refdates$(3)8,               /* REFERENCE                  */~
            retained$20,                 /* RETAINED EARNINGS          */~
            rptid$6,                     /* Report ID                  */~
            search%(1),                  /*                            */~
            set$1, setdescr$30, sethdr$50,/* Which set of books to use */~
            setmsg$35,                   /* Screen message for SET     */~
            sortkey$64,                  /* SORT KEY FOR SORTING       */~
            subhdr$50,                   /* Sub header                 */~
            tasktype$1,                  /* FOREGROUND OR BACKGROUND   */~
	    tdate$8,			 /* Temporary Date             */~
            time$8,                      /* Time of day                */~
            title$20,                    /* KEY TO SYSFILE2            */~
            totalytdass$20,              /* TOTAL YTD ASSETS           */~
	    udate$8,			 /* Unformated Date String     */~
            userid$3,                    /* USER ID CURRENT USER       */~
            which$1,                     /* WHICH MONTH THIS TRIAL     */~
            ytdbal$20                    /* YEAR TO DATE FORMATTED     */~

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20                  /* RETURN CODE FROM "FILEOPEN"*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con

        REM *************************************************************~
            *                    S E L E C T   F I L E S                *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !  DESCRIPTION                             *~
            *-----+----------+------------------------------------------*~
            * #01 ! GLMAIN   ! GENERAL LEDGER MAIN FILE                 *~
            * #04 ! SYSFILE2 ! SYSTEM INFORMATION (WHICH MONTHS OPEN)   *~
            * #09 ! WORKFILE ! WORKFILE FOR SORTED FILE                 *~
            * #10 ! GLMAIN2  ! G. L. chart of accounts for local auth.  *~
            *************************************************************

            select #01, "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 9

            select  #04, "SYSFILE2",                                     ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select  #09, "WORKFILE",                                     ~
                        consec,                                          ~
                        recsize = 100

            select #10, "GLMAIN2",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 9

            call "SHOSTAT" ("Opening files, one moment please")

            call "OPENCHCK" (#01, 0%, f2%( 1), 100%, rslt$( 1))
            call "OPENCHCK" (#04, 0%, f2%( 4), 100%, rslt$( 4))
            dual_books$ = "N"                        /* Default to 'no' */
            call "READ100" (#04, "SWITCHS.GL", f1%(4))
                if f1%(4) = 0% then goto L09000
            get #04 using L02360, dual_books$
L02360:         FMT POS(21), CH(1)
            if dual_books$ <> "Y" then goto L09000
                call "OPENCHCK" (#10, 0%, f2%(10), 100%, rslt$(10))

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES CONTROL VARIABLES, SETS AVAILABLE DATES, ETC. *~
            *************************************************************

            call "EXTRACT" addr ("ID", userid$, "TT", tasktype$)
            call "COMPNAME" (12%, coname$, u3%)
            date$ = date : call "DATEFMT" (date$)
            rptid$ = "G/L008"
            currentbal = 0
            title$ = "Profit /(Loss)   "
        REM FORMAT DATES FOR HISTORY FORMATTING
            months$ = "JanFebMarAprMayJunJulAugSepOctNovDec"
                call "READ100" (#04, "FISCAL DATES", f1%(4))
                     if f1%(4) <> 0 then L09200
L09161:                 if tasktype$ = "B" then goto L65000
                        u3% = 2%
                        call "ASKUSER" (u3%,"*** FISCAL DATE ERROR ***", ~
                        "GLBALSHT can't find Fiscal dates available"," ",~
                        "Press (RETURN) to acknowledge and exit")
                        goto L65000

L09200:         get #04, using L09210, periods%, monthopen%, dates$()
L09210:                 FMT XX(20), BI(2), XX(136), BI(2), 32*CH(8)

                mdate$(1) = dates$(monthopen%+16)
                if monthopen% = 12 then mdate$(1) = dates$(41-periods%)
                if monthopen% = 17 then mdate$(1) = " "

                mdate$(2) = dates$(monthopen%+15)

                mdate$(3) = dates$(monthopen%+14)
                if monthopen% = 1 then mdate$(3) = dates$(periods%+1)
                if monthopen% = 14 then mdate$(3) = dates$(periods%+15)

                for temp% = 1 to 3
                    refdates$(temp%), tdate$ = mdate$(temp%)
		    call "DATEFMT" (tdate$, 0%, udate$)
                    convert str(udate$,5%,2%)to month%,data goto L09161
                    m% = month%*3%-2%
                    mdate$(temp%) = str(months$,m%,3%) & " " &            ~
                          str(udate$,7%,2%) & ", " & str(udate$,1%,4%)
                next temp%

            if dual_books$ =  "Y" then setmsg$ = "Enter code for set of"&~
                " books to use:"

L10000: REM *************************************************************~
            *         I N P U T   W H I C H   M O N T H                 *~
            *                                                           *~
            * CONTROLING SECTION FOR WHICH MONTH TO PRINT               *~
            *************************************************************

        REM GET MONTH FOR WHICH TO PRINT BALANCE SHEET
            init(" ") which$, errormsg$, set$
            for x% = 1% to 2%
L10080:         gosub L41000
                    if keyhit%  =   1 then gosub startover
                    if keyhit%  =  16 then       L65000
                    if keyhit% <>   0 then       L10080
                gosub L51000
                    if errormsg$ <> " " then L10080
            next x%

        REM *************************************************************~
            *   P L O W   T H R O U G H   F I L E   A N D   P R I N T   *~
            *                                                           *~
            * PLOWS THROUGH FILE OF ACCOUNTS AND SORTS THE FILE TYPES   *~
            * FOR THE APPRPRIATE ACCOUNT TYPES.                         *~
            * PUTS THESE TWO ACCOUNT TYPES IN THE WORKFILE AND SORTS    *~
            * THEN FOR OUTPUT TO THE PRINTER. SET ASS ACCOUNT TYPE TO   *~
            * '1' AND LIB ACCOUNT TYPE TO '2' SO THEY CAN BE SORTED IN  *~
            * THAT ORDER. IF YOU WANT TO GET FANCIER, BREAK ALL TYPES   *~
            * OUT BY ASSIGNUNG 1,2,3, ETC....                           *~
            *************************************************************

            call "SHOSTAT" ("Printing Balance Sheet for period ending " &~
                mdate$(whichmonth%) )

        REM PLOW ROUTINE FOR ACCOUNTS.
            retained, totalytdass, totalytdlib = 0
            letsdo$, heading$ = "ASSETS"
            call "FMTTITLE" (heading$, " ", 12%)
            call "WORKOPEN" (#09, "OUTPT", 1000%, f1%(9))
            readkey$ = all(hex(00))

L12210:     call "PLOWNEXT" (#main%, readkey$, 0%, f1%(main%))
                if f1%(main%) = 0 then L13000
            get #main%, using L12240, accttype$ /* GET ACCOUNT TYPE */
L12240:         FMT XX(39), CH(1)
            if accttype$ = "$" then L12310
            if accttype$ = "A" then L12310
            if accttype$ = "C" then L12400
            if accttype$ = "L" then L12400
            go to L12210

L12310: REM CHANGE TYPE FROM (ASSETS )TO "1" FOR SORTING PURPOSES
            accttype$ = "1"
            gosub L12500
            go to L12210

L12400: REM CHANGE TYPE FROM L TO "2" FOR SORTING
            accttype$ = "2"
            gosub L12500
            go to L12210

L12500: REM SAVE THE RECORD IN WORKFILE AND GO TO PLOWNEXT
            str(sortkey$, 1, 1) = accttype$
            str(sortkey$, 2, 9) = str(readkey$, 1, 9)
            write #09, using L12550, sortkey$, readkey$
L12550:         FMT CH(64), CH(36)
            return

L13000: REM *************************************************************~
            *  S O R T   T H E  W O R K F I L E   A N D  P R I N T      *~
            *                                                           *~
            * SORT THE WORKFILE AND TOTAL THE ASSESTS AND LIABILITIES   *~
            * AND PRINT THE TOTALS ON A PIECE OF PAPER THE CIVILIZED    *~
            * WORLD NOW CALLS OUTPUT, HARDCOPY, RESULTS?????            *~
            * THINGS SURE ARE IMPORTANT THESE DAYS.                     *~
            *************************************************************

            call "SLCTSORT" (#09, 10%)
            pageline% = 1000% : count%, pagenumber% = 0%

L13110:     read #09, using L13120, sortkey$, readkey$, eod goto L19000
L13120:         FMT CH(64), CH(36)
            call "BOTMLIN2" (str(readkey$, 1, 9), acctdescr$, currentbal,~
                ytdbal, currentbal$, ytdbal$, 3%, thismonth%, #main%,    ~
                f2%(main%), u3%)
            if ytdbal = 0 then L13110
            account$ = str(readkey$,,9)
            if set = 1                                                   ~
                then call "GLFMT" (account$)                             ~
                else call "GLFMT2" (account$)
            count% = count% + 1%
            if letsdo$ = "LIABILITY" then L13300
            if str(sortkey$, 1,1) = "2" then gosub L13390
            if letsdo$ = "LIABILITY" then L13300
            totalytdass = totalytdass + ytdbal
            goto L13340

L13300: REM DO THE LIABILY ACCOUNT ACCOUNTING HERE
            totalytdlib = totalytdlib + ytdbal
L13340:     gosub L21000
            print using L60150, account$, acctdescr$, ytdbal$
            go to L13110

L13390: REM TOTAL THE ASSET ACCOUNTS AND GO ON TO THE LIABILITIES
            gosub L19231
            letsdo$ = "LIABILITY"
            heading$ = "LIABILITIES AND STOCKHOLDER'S EQUITY"
            call "FMTTITLE" (heading$, " ", 12%)
            pageline% = 1000
            return

L19000: REM *************************************************************~
            *    E N D   I T  H E R E                                   *~
            *                                                           *~
            *                                                           *~
            *                                                           *~
            *************************************************************

        REM RETAINED EARNINGS AS PART OF EQUITITIES   (PROFIT OR LOSS)
            retained = round(totalytdass + totalytdlib, 2%)
            temp = abs(retained)
            convert temp to retained$, pic(###,###,###.##)
            call "NUMPAREN" (retained, retained$)
            gosub L21000
            print using L60150, " ", title$, retained$

        REM TOTAL THE EXPENSE ACCOUNTS AND GO ON TO THE END
        REM REMEMBER THE DUAL ASPECT OF ACCOUNTING
            gosub L19231
            print
            print using L60170, count%
            call "FILEBGON" (#09)
            close printer
            call "SETPRNT" (rptid$, " ", 0%, 1%)
            goto L10000     /* Permit multiple reports */

L19231: REM ROUTINE PRINTS TOTAL ASSETS AND FINAL TOTAL
            totalytdass = round(totalytdass, 2%)
            convert abs(totalytdass) to totalytdass$, pic(###,###,###.##)
            call "NUMPAREN" (totalytdass, totalytdass$)
            gosub L21000
            print using L60150, " ", " ", "===================="
            print using L60150, " ", " ", totalytdass$
            return

L21000: REM *************************************************************~
            *        P A G E   C O N T R O L   R O U T I N E            *~
            *                                                           *~
            * CONTROLS THE PAGING                                       *~
            *************************************************************

            select printer (87)
            if pagenumber% <> 0% then goto L21070
                time$ = " " : call "TIME" (time$)
                call "SETPRNT" (rptid$, " ", 0%, 0%)
                subhdr$ = " "
                subhdr$ = "BALANCE SHEET FOR"
                call "FMTTITLE" (subhdr$, mdate$(whichmonth%), 2%)
L21070:     pageline% = pageline% + 1
            if pageline% < 55 then return
            print page
            pagenumber% = pagenumber% + 1
            print using L60030, date$, coname$, "-" & rptid$
            print using L60050, time$, subhdr$, pagenumber%
            print using L60065, sethdr$
            print using L60065, heading$
            print
            print using L60090, "#"
            print using L60110
            print
            pageline% = 8
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

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
            goto L10000

L41000: REM *************************************************************~
            *   I N P U T   W H I C H   M O N T H                       *~
            *                                                           *~
            * INDICATE WHICH MONTH YOU DESIRE TRIAL BALANCE FOR         *~
            *************************************************************

            line2$ = " "
            str(line2$,62%) = "GLBALSHT: " & str(cms2v$,,8%)
            init (hex(8c)) fac$()
            on x% goto L41100, L41130
L41100:     line21$ = "Enter the number of the month for which you want"&~
                " a Balance Sheet"
            fac$(1) = hex(82) : goto L41170
L41130:     if dual_books$ <> "Y" then return
            line21$ = "Enter '1' to use Statutory books; '2' to use Loc"&~
                "al Authority books"
            fac$(2) = hex(82)
L41170:     accept                                                       ~
               at (01,02), "Print Balance Sheet",                        ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02), "Enter number of desired month (1 - 3):",     ~
               at (05,42), fac(fac$(1)), which$                 , ch(01),~
               at (07,05), "Choose from this list:",                     ~
               at (07,34), "1.",                                         ~
               at (07,37), fac(hex(8c)), mdate$(1)              , ch(12),~
               at (08,34), "2.",                                         ~
               at (08,37), fac(hex(8c)), mdate$(2)              , ch(12),~
               at (09,34), "3.",                                         ~
               at (09,37), fac(hex(8c)), mdate$(3)              , ch(12),~
               at (12,02), fac(hex(8c)), setmsg$                , ch(35),~
               at (12,42), fac(fac$(2)), set$                   , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)), line21$                , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)EXIT PROGRAM",                           ~
                                                                         ~
               keys(hex(00010d0f10)), key (keyhit%)

               if keyhit% <> 13 then L41470
                  call "MANUAL" ("GLBALSHT")
                  goto L41170

L41470:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L41170


L51000: REM *************************************************************~
            *      T E S T   W H I C H   M O N T H  E N T E R E D       *~
            *                                                           *~
            * TEST THE MONTH ENTERED TO ENSURE THAT IT IS WITHIN RANGE  *~
            *************************************************************

            errormsg$ = " "
            on x% goto L51080, L51160
L51080:     call "NUMTEST" (which$, 1, 3, errormsg$, 0, which)
            if errormsg$ <> " " then return
            whichmonth% = which
            search dates$() = refdates$(whichmonth%) to search%()
                if search%(1) = 0 then L09161
            thismonth% = (search%(1) + 7)/8 - 15
            return

L51160:     set = 1 : main% = 1% /* Set default values */
            if dual_books$ <> "Y" then return
            call "NUMTEST" (set$, 1, 2, errormsg$, 0, set)
            if errormsg$ <> " " then return
            main% = 1%
            setdescr$ = "Statutory"
            if set = 1 then goto L51230
                main% = 10%
                setdescr$ = "Local Authority"
L51230:     sethdr$ = setdescr$
            call "PUTPAREN" (setdescr$)
            call "FMTTITLE" (sethdr$, "G/L SYSTEM", 2%)
            return

        REM *************************************************************~
            *    I M A G E   S T A T E M E N T S   F O R   P R I N T    *~
            *************************************************************

L60030: %DATE: ########   ###############################################~
        ~###   GLBALSHT#######
L60050: %TIME: ########   ###############################################~
        ~###        PAGE: ####
L60065: %                 ###############################################~
        ~###
L60090: %     ACCOUNT #     DESCRIPTION                             BALAN~
        ~CE
L60110: %     ------------  ------------------------------  -------------~
        ~-------
L60150: %     ############  ##############################  #############~
        ~#######
L60170: %*** END OF LISTING ***          NUMBER OF ACCOUNTS PRINTED: ####~
        ~######

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND, JUST FOR EFFECT,*~
            * CLEVERLY DISPLAYS A MESSAGE WHILE WE LINK TO THE NEXT     *~
            * PROGRAM.                                                  *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")
            end
