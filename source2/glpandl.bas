        REM *************************************************************~
            *                                                           *~
            *   GGG   L      PPPP    AAA   N   N  DDDD   L              *~
            *  G      L      P   P  A   A  NN  N  D   D  L              *~
            *  G GGG  L      PPPP   AAAAA  N N N  D   D  L              *~
            *  G   G  L      P      A   A  N  NN  D   D  L              *~
            *   GGG   LLLLL  P      A   A  N   N  DDDD   LLLLL          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GLPANDL  - THIS PROGRAM PRINTS THE PROFIT AND LOSS STATEME*~
            *            NT. IT SORTS ALL GL ACCOUNTS LOOKING FOR EXPENS*~
            *            E AND REVENUE ACCOUNTS. THEN PRINTS THEM OUT   *~
            *            AND CALCULATES THE DIFF. (RETAINED EARNINGS).  *~
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
            * 08/20/81 ! ORIGINAL                                 ! TOM *~
            * 10/06/81 ! USE OF FISCAL YEAR                       ! TEM *~
            * 11/09/82 ! REMOVE BUDGET, ADD PERCENT OF SALES      ! ECR *~
            * 09/16/87 ! W.R. Grace accountancy (dual books) mods.! JIM *~
            * 08/10/88 ! Dual Books depends on a SYSFILE2 flag.   ! JIM *~
            * 04/07/89 ! Initialized LETSDO$ for 2nd run          ! GGO *~
            * 08/24/89 ! Removed writing Retained Earnings to     ! JDH *~
            *          !   SYSFILE2.  It's not used anywhere!     !     *~
            * 01/12/93 ! Page 0 Facs fix,     & End Report Time.  ! RJH *~
	    * 06/24/96 ! Changes for the year 2000.               ! DXL *~
            *************************************************************

        dim                                                              ~
            acct$16,                     /* GL ACCOUNT FROM8063        */~
            acctdescr$32,                /* GL ACCOUNT DESCRIPTION     */~
            accttype$1,                  /* GL ACCOUNT TYPE            */~
            coname$60,                   /* Company name               */~
            currentbal$20,               /* CURRENT BALANCE FORMATTED  */~
            currentper$8,                /* CURRENT % OF SALES         */~
            cursor%(2),                                                  ~
            date$8,                      /* FORMATTED G/L DATE         */~
            dates$(32)8,                 /* FISCAL DATE STRUCTURE      */~
            dual_books$1,                /* Dual books in effect?      */~
            edtmessage$79,                                               ~
            errormsg$79,                 /* ERROR MESSAGE FOR DISPLAY  */~
            heading$32,                  /* PAGE HEADING               */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,                                               ~
            letsdo$8,                    /* SWITCH FOR EXP OR REV.     */~
            lfac$(3)1,                   /* Screen FACs                */~
            line2$79,                    /* Screen line 2              */~
            months$36,                   /* MONTHS LIST AVAILABLE      */~
            mdate$(4)12, mdate$14,       /* DATES OF AVAILABLE MONTHS  */~
            pf16msg$16, pf16fac$1, pf16key$1,                            ~
            profit$20,                   /* CURRENT PROFIT             */~
            refdates$(4)8,               /* FISCAL DATE STRUCTURE      */~
            ret$20,                      /* RETAINED EARNINGS          */~
            readkey$50,                  /* PLOW KEY                   */~
            rptid$6,                     /* Report ID                  */~
            search%(1),                  /* FOR SEARCH FUNTION         */~
            set$1, setdescr$32, sethdr$60,/* Set of books to use       */~
            setmsg$18,                   /* Screen message for SET     */~
            sortkey$64,                  /* SORT KEY FOR SORTING       */~
            tasktype$1,                  /* FOREGROUND OR BACKGROUND   */~
            tcs$20,                      /* TOTAL CURRENT SALES        */~
	    tdate$8,                     /* Temp. Date                 */~
            time$8,                      /* Time of day                */~
            title$20,                    /* KEY TO SYSFILE2            */~
            totalcurexp$20,              /* TOTAL CURRENT      EXPENSES*/~
            totalcurrev$20,              /* TOTAL CURRENT REVENUE      */~
            totalytdexp$20,              /* TOTAL YTD EXPENSES         */~
            totalytdrev$20,              /* TOTAL YTD REVENUE          */~
            tys$20,                      /* TOTAL YTD SALES            */~
	    udate$8,                     /* Unformated temp. date      */~
            userid$3,                    /* USER ID CURRENT USER       */~
            which$1,                     /* WHICH MONTH THIS TRIAL     */~
            ytdbal$20,                   /* YEAR TO DATE FORMATTED     */~
            ytdper$8                     /* YEAR TO DATE % OF SALES    */~

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            fs%(64),                     /* FOR OPENCHCK               */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20                  /* RETURN CODE FROM "FILEOPEN"*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */


        REM *************************************************************~
            *                    S E L E C T   F I L E S                *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !  DESCRIPTION                             *~
            *-----+----------+------------------------------------------*~
            * #01 ! GLMAIN   ! GENERAL LEDGER MAIN FILE                 *~
            * #04 ! SYSFILE2 ! SYSTEM INFORMATION (WHICH MONTHS OPEN)   *~
            * #09 ! WORKFILE ! WORKFILE FOR SORTED FILE                 *~
            * #11 ! GLMAIN2  ! G. L. chart of accounts for local auth.  *~
            *************************************************************

            select #01, "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        KEYPos = 1, keylen = 9

            select  #04, "SYSFILE2",                                     ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select  #09, "WORKFILE",                                     ~
                        consec,                                          ~
                        recsize = 100

            select #11, "GLMAIN2",                                       ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   9

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01%, fs%( 1), f2%( 1), 0%, rslt$( 1))
            call "OPENCHCK" (#04%, fs%( 4), f2%( 4), 0%, rslt$( 4))
            dual_books$ = "N"                        /* Default to 'no' */
            call "READ100" (#04, "SWITCHS.GL", f1%(4))
                if f1%(4) = 0% then goto L09000
            get #04 using L02345, dual_books$
L02345:         FMT POS(21), CH(1)
            if dual_books$ <> "Y" then goto L09000
                call "OPENCHCK" (#11%, fs%(11), f2%(11), 0%, rslt$(11))

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES CONTROL VARIABLES, SETS AVAILABLE DATES, ETC. *~
            *************************************************************

            if dual_books$ = "Y" then setmsg$ = "G/L System to use:"
            if dual_books$ = "Y" then lo% = 1% else lo% = 2%
            letsdo$ = "REVENUES"
            call "EXTRACT" addr ("ID", userid$, "TT", tasktype$)
            date$ = date : call "DATEFMT" (date$)
            title$ = "RETAINED EARNINGS"
            call "COMPNAME" (12%, coname$, 0%)
            str(line2$,63) = "GLPANDL: " & str(cms2v$,1,8)
            rptid$ = "G/L013"
            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (RETURN)."

        REM FORMAT DATES FOR HISTORY FORMATTING
            months$ = "JanFebMarAprMayJunJulAugSepOctNovDec"

            call "READ100" (#04, "FISCAL DATES        ", f1%(4))
                if f1%(4) <> 0 then L09270
L09200:     err% = 0%
            call "ASKUSER" (err%, "ERROR ON SETTING DATE RANGE",         ~
                            "Review your fiscal date structure",         ~
                           "Press PF-16 to Acknowledge and Exit Program",~
                           " ")
            goto L65000

L09270:     get #04, using L09280, periods%, mdate$(4), monthopen%,dates$()
L09280:            FMT XX(20), BI(2), CH(8), XX(128), BI(2), 32*CH(8)

                mdate$(1) = dates$(monthopen%+16)
                if monthopen% = 12 then mdate$(1) = dates$(41-periods%)
                if monthopen% = 17 then mdate$(1) = " "

                mdate$(2%) = dates$(monthopen%+15%)

                mdate$(3%) = dates$(monthopen%+14%)
                if monthopen% = 1% then mdate$(3%) = dates$(periods%+1%)
                if monthopen% = 14% then mdate$(3%) = dates$(periods%+15%)

                for temp% = 1% to 4%
		    tdate$ = mdate$(temp%)
		    call "DATEFMT" (tdate$, 0%, udate$)
                    convert str(udate$,5%,2%)to month%,data goto L09200
                    m% = month%*3%-2%
                    refdates$(temp%) = mdate$(temp%)
		    temp$ = udate$
                    mdate$(temp%) = str(months$,m%,3%) & " " &            ~
                          str(temp$,7%,2%) & ", " & str(temp$,1%,4%)
                next temp%

L10000: REM *************************************************************~
            *         I N P U T   W H I C H   M O N T H                 *~
            *                                                           *~
            * CONTROLING SECTION FOR WHICH MONTH TO PRINT               *~
            *************************************************************

            init(" ") which$, errormsg$, set$, setdescr$, sethdr$, mdate$
            letsdo$ = "REVENUES"
            pagenumber% = 0
            pageline% = 1000
            pf16msg$="(16)EXIT PROGRAM":pf16fac$=hex(84):pf16key$=hex(10)
            set = 1 : main% = 1% /* Set defaults */

            for fieldnr% = lo% to 2%
L10120:         gosub'101(fieldnr%)
                    if keyhit%  =   1 then gosub startover
                    if keyhit%  =  16 then       L65000
                    if keyhit% <>   0 then       L10120
                gosub'151(fieldnr%)
                    if errormsg$ <> " " then L10120
            next fieldnr%

        REM *************************************************************~
            *   EDIT THE FIELDS CAPTURED ABOVE.                         *~
            *************************************************************

L11040:     inpmessage$ = edtmessage$
            pf16msg$="(16)PRINT P & L":pf16fac$=hex(84):pf16key$=hex(10)
            gosub'101(0%)
                if keyhit%  =  1 then gosub startover
                if keyhit%  = 16 then L11230
                if keyhit% <>  0 then L11040
            fieldnr% = 0%
            if cursor%(1) =  6% and dual_books$ = "Y" then fieldnr% = 1%
            if cursor%(1) =  8% then fieldnr% = 2%
            if fieldnr% = 0% then goto L11040
            pf16fac$=hex(9c):pf16key$=hex(ff)
L11160:     gosub'101(fieldnr%)
                if keyhit%  =  1 then gosub startover
                if keyhit% <>  0 then L11160
            gosub'151(fieldnr%)
                if errormsg$ <> " " then L11160
            goto L11040

L11230: REM Print the Profit & Loss statement here
            convert set to set$, pic (#)
            first$ = mdate$(4)
            second$ = mdate$(whichmonth%)
            select printer (134)
            call "SETPRNT" (rptid$, " ", 0%, 0%)
            gosub L12000
            gosub L19000
            print
            time$ = " "  :  call "TIME" (time$)
            print using L20221, time$           /* End of Report */
            close printer
            call "SETPRNT" (rptid$, " ", 0%, 1%)
            call "FILEBGON" (#09)
            go to L10000

L12000: REM *************************************************************~
            *   P L O W   T H R O U G H   F I L E   A N D   P R I N T   *~
            *                                                           *~
            * PLOWS THROUGH FILE OF ACCOUNTS AND SORTS THE FILE TYPES   *~
            * FOR REVENUE AND EXPENSE ACCOUNTS ONLY.                    *~
            * PUTS THESE TWO ACCOUNT TYPES IN THE WORKFILE AND SORTS    *~
            * THEN FOR OUTPUT TO THE PRINTER. SET REV ACCOUNT TYPE TO   *~
            * '1' AND EXP ACCOUNT TYPE TO '2' SO THEY CAN BE SORTED IN  *~
            * THAT ORDER.                                               *~
            *************************************************************

            call "SHOSTAT" ("Processing Accounts for Profit and Loss Stat~
        ~ement")

        REM PLOW ROUTINE FOR ACCOUNTS.
            totalytdexp, totalcurexp = 0
            totalytdrev, totalcurrev = 0
            call "WORKOPEN" (#09, "OUTPT", 1000%, f1%(9))
            readkey$ = all(hex(00))
L12210:     call "PLOWNEXT" (#main%, readkey$, 0%, f1%(main%))
                if f1%(main%) = 0 then L13000
            get #main%, using L12240, accttype$ /* GET ACCOUNT TYPE */
L12240:         FMT XX(39), CH(1)
            if accttype$ = "R" then L12290  /* IF NOT A EXP OR  */
            if accttype$ = "E" then L12350  /* REV THEN NEXT ACC*/
            go to L12210

L12290: REM CHANGE TYPE FROM R TO "1" FOR SORTING PURPOSES
            accttype$ = "1"
            gosub L12410
            go to L12210

L12350: REM CHANGE TYPE FROM E TO "2" FOR SORTING PORPOISES
            accttype$ = "2"
            gosub L12410
            go to L12210

L12410: REM SAVE THE RECORD IN WORKFILE AND GO TO PLOWNEXT
            str(sortkey$, 1, 1) = accttype$
            str(sortkey$, 2, 9) = str(readkey$, 1, 9)
            write #09, using L12460, sortkey$, readkey$
L12460:         FMT CH(64), CH(36)
            return

L13000: REM *************************************************************~
            *  S O R T   T H E  W O R K F I L E   A N D  P R I N T      *~
            *                                                           *~
            * SORT THE WORKFILE AND TOTAL THE REVENUE AND EXPENSES      *~
            * AND PRINT THE TOTALS ON A PIECE OF PAPER THE CIVILIZED    *~
            * WORLD NOW CALLS OUTPUT, HARDCOPY, RESULTS?????            *~
            * THINGS SURE ARE IMPORTANT THESE DAYS...                   *~
            *************************************************************

            call "SLCTSORT" (#09, 10%)
            gosub L53000                  /* COMPUTE TOTAL SALES        */
            call "SHOSTAT" ("Printing Profit and Loss for period ending "~
                &  mdate$(whichmonth%) )
            heading$ = "R E V E N U E   A C C O U N T S"

L13180:     read #09, using L13190, sortkey$, readkey$, eod goto L15000
L13190:         FMT CH(64), CH(36)
            call "BOTMLIN2" (str(readkey$, 1, 9), acctdescr$,currentbal, ~
                ytdbal, currentbal$, ytdbal$, 3%, thismonth%, #main%,    ~
                f2%(1), u3%)
            if letsdo$ = "EXPENSES" then L13430
            if str(sortkey$, 1,1) = "2" then gosub L13580
            if letsdo$ = "EXPENSES" then L13430
            totalytdrev = totalytdrev + ytdbal
            totalcurrev = totalcurrev + currentbal
            gosub'100(ytdbal, currentbal, 1) /* %    OF SALES    */
            gosub L19000            /* PAGE CONTROL              */
            acct$ = str(readkey$,,9)
            if set = 1                                                   ~
                then call "GLFMT" (acct$)                                ~
                else call "GLFMT2" (acct$)
            print using L20140, acct$, acctdescr$, currentbal$,           ~
                currentper$, ytdbal$, ytdper$
            go to L13180

L13430: REM DO THE EXPENSE ACCOUNT ACCOUNTING HERE
            if str(sortkey$, 1, 1) = "1" then L13180
            totalytdexp = totalytdexp + ytdbal
            totalcurexp = totalcurexp + currentbal
            gosub'100(ytdbal, currentbal, -1)/* % OF SALES       */
            gosub L19000            /* PAGE CONTROL               */
            acct$ = str(readkey$,,9)
            if set = 1                                                   ~
                then call "GLFMT" (acct$)                                ~
                else call "GLFMT2" (acct$)
            print using L20140, acct$, acctdescr$, currentbal$,           ~
                currentper$, ytdbal$, ytdper$
            go to L13180

L13580: REM TOTAL THE REVENUE ACCOUNTS AND GO ON TO THE EXPENSES
            temp = -1 * totalytdrev
            temp = round(temp, 2)
            convert abs(temp) to totalytdrev$, pic(###,###,###.##)
            call "NUMPAREN" (temp, totalytdrev$)
            temp = -1 * totalcurrev
            temp = round(temp, 2%)
            convert abs(temp) to totalcurrev$, pic(###,###,###.##)
            call "NUMPAREN" (temp, totalcurrev$)
            gosub'100(totalytdrev, totalcurrev, 1)    /* %  OF SALES   */
            print using L20160
            print using L20180, totalcurrev$, currentper$, totalytdrev$,  ~
                ytdper$
            letsdo$ = "EXPENSES"
            heading$ = "E X P E N S E   A C C O U N T S"
            pageline% = 1000
            return

L15000: REM *************************************************************~
            *    E N D   I T  H E R E                                   *~
            * ADD THE TOTAL EXPENSES AND PRINT THEM, THEN CALCULATE     *~
            * RETAINED EARNINGS AND SAVE THE RESULT ALONG WITH THE      *~
            * RETAINED EARNINGS ACCOUNT NUMBER IN SYSFILE2              *~
            *************************************************************

        REM TOTAL THE EXPENSE ACCOUNTS AND GO ON TO THE END
            totalytdexp = round(totalytdexp, 2)
            convert abs(totalytdexp) to totalytdexp$, pic(###,###,###.##)
            call "NUMPAREN" (totalytdexp, totalytdexp$)
            totalcurexp = round(totalcurexp, 2)
            convert abs(totalcurexp) to totalcurexp$, pic(###,###,###.##)
            call "NUMPAREN" (totalcurexp, totalcurexp$)
            gosub'100(totalytdexp, totalcurexp, -1)   /* % OF SALES    */
            print using L20160
            print using L20180, totalcurexp$, currentper$, totalytdexp$,  ~
                ytdper$
            profit = -1 * (totalcurexp + totalcurrev)
            profit = round( profit, 2)
            convert abs(profit) to profit$, pic(###,###,###.##)
            call "NUMPAREN" (profit, profit$)
            retainedearnings = -1 * (totalytdexp + totalytdrev)
            retainedearnings = round( retainedearnings, 2)
            convert abs(retainedearnings) to ret$, pic(###,###,###.##)
            call "NUMPAREN" (retainedearnings, ret$)
            temp = -1 * totalcurrsales
            convert abs(temp) to tcs$, pic(###,###,###.##)
            call "NUMPAREN" (temp, tcs$)
            temp = -1 * totalytdsales
            convert abs(temp) to tys$, pic(###,###,###.##)
            call "NUMPAREN" (temp, tys$)

        REM  PRINT PROFIT/LOSS ON A NEW PAGE
            heading$ = "P R O F I T  /  L O S S"
            pageline% = 1000%
            gosub L19000             /* PAGE CONTROL               */
            gosub'100(totalytdsales, totalcurrsales, 1)
            print using L20140, " ", "Total Net Sales", tcs$, currentper$,~
                tys$, ytdper$
            gosub'100(retainedearnings, profit, -1)
            print using L20140, " ", "Profit/(Loss)", profit$,            ~
                currentper$, ret$, ytdper$
            return

        REM *************************************************************~
            *  C O M P U T E   A N D   F O R M A T  %   O F  S A L E S  *~
            *                                                           *~
            *************************************************************

           deffn'100(ytd1, curr1, mult)
            ytdper$     = " "
            currentper$ = " "
            if totalytdsales <> 0                                        ~
                then ytdper = (ytd1  / totalytdsales)  * 100 * mult      ~
                else ytdper = 0
            if totalcurrsales <> 0                                       ~
                then currentper = (curr1 / totalcurrsales) * 100 * mult  ~
                else currentper = 0
            ytemp = abs(ytdper)
            call "CONVERT" (ytemp, 2.2, str(ytdper$, 1, 6))
            ctemp = abs(currentper)
            call "CONVERT" (ctemp, 2.2, str(currentper$, 1, 6))
            call "NUMPAREN" (ytdper, ytdper$)
            call "NUMPAREN" (currentper, currentper$)
            if ytemp = 0 then ytdper$ = " "
            if ctemp = 0 then currentper$ = " "
            return

L19000: REM *************************************************************~
            *        P A G E   C O N T R O L   R O U T I N E            *~
            *                                                           *~
            * CONTROLS THE PAGING OF P AND L PRINTING                   *~
            *************************************************************

            pageline% = pageline% + 1
            if pageline% < 55 then return
            if pagenumber% <> 0% then goto L19175
                time$ = " " : call "TIME" (time$)
*        ** Fix Facs **
L19092:         i% = pos(str(i$()) > hex(7f))
                if i% = 0% then L19100
                    str(i$(), i%, 1%) = hex(20)
                    goto L19092

L19100:         pagesw% = 1%
                gosub L19180
                pagesw% = 0%
                print skip (2)
                print using L20210, "R E P O R T   S E L E C T I O N S:"
                print
                for x% = 4% to 20%
                    print using L20210, i$(x%)
                next x%
L19175:     pagenumber% = pagenumber% + 1
L19180:     print page
            print using L20000, date$, time$, coname$, "-" & rptid$
            print using L20030, pagenumber%
            print using L20060, sethdr$
            if pagesw% <> 0% then return
            print
            print using L20080, first$, second$, second$
            print
            print using L20110, heading$
            print using L20120, "#"
            print
            pageline% = 9
            return

L20000: %RUN: ######## @ ########            ############################~
        ~################################                      GLPANDL####~
        ~###
L20030: %                                         P R O F I T   A N D   L~
        ~ O S S   S T A T E M E N T                                PAGE: #~
        ~###
L20060: %                                    ############################~
        ~################################
L20080: %       F O R   P E R I O D   ############   T O   ############. ~
        ~   CURRENT REFERS TO PERIOD ENDING ############
L20110: %           ################################
L20120: % ACCOUNT #       D E S C R I P T I O N                 CURRENT  ~
        ~      % OF SALES      YEAR TO DATE       % OF SALES
L20140: %############  ################################ #################~
        ~###   ########    ####################   ########
L20160: %                                               =================~
        ~==================================================
L20180: %                                               #################~
        ~###   ########    ####################   ########
L20210: %                          ######################################~
        ~##########################################
L20221:       %                                                 *** END O~
        ~F REPORT  @ ########  ***

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.
            ask% = 2%
            call "STARTOVR" (ask%)
            if ask% = 1% then return
            return clear all
            goto L10000

        REM *************************************************************~
            *  CAPTURE THE DATA FIELDS NECESSARY FOR RUNNING THE P & L. *~
            *************************************************************

        deffn'101(fieldnr%)
            init (hex(8c)) lfac$()
            if fieldnr% <> 0% then goto L40050
                init (hex(84)) lfac$()
                if dual_books$ <> "Y" then lfac$(1) = hex(9c)
                goto L40210

L40050:     on fieldnr% goto L40070, L40120, L40170

L40070:     inpmessage$ = "Enter '1' for Statutory G/L set of books; '2"&~
                "' for Local Authority."
            lfac$(1) = hex(82)
            goto L40210

L40120:     inpmessage$ = "Select by number ('1' thru '3') the month yo"&~
                "u wish to print."
            lfac$(2) = hex(82)
            goto L40210

L40170:     inpmessage$ = "Enter the account number for Retained Earnin"&~
                "gs or blanks to see list."
            lfac$(3) = hex(81)

L40210:     accept                                                       ~
               at (01,02), "Print Profit and Loss Statement(s)",         ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), fac(hex(8c)),   setmsg$              , ch(18),~
               at (06,35), fac(lfac$( 1)), set$                 , ch(01),~
               at (06,48), fac(hex(8c)),   setdescr$            , ch(30),~
                                                                         ~
               at (08,02), "Select a Period-Ending Date:",               ~
               at (08,35), fac(lfac$( 2)), which$               , ch(01),~
               at (08,48), fac(hex(8c)),   mdate$               , ch(14),~
               at (09,10), "1=",                                         ~
               at (09,13), fac(hex(8c)), mdate$(1)              , ch(12),~
               at (10,10), "2=",                                         ~
               at (10,13), fac(hex(8c)), mdate$(2)              , ch(12),~
               at (11,10), "3=",                                         ~
               at (11,13), fac(hex(8c)), mdate$(3)              , ch(12),~
                                                                         ~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(pf16fac$), pf16msg$              , ch(16),~
                                                                         ~
               keys(hex(00010d0f) & pf16key$), key (keyhit%)

               if keyhit% <> 13 then L40650
                  call "MANUAL" ("GLPANDL")
                  goto L40210

L40650:        if keyhit% <> 15 then goto L40690
                  call "PRNTSCRN"
                  goto L40210

L40690:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% goto L50100, L50250

L50100:     call "NUMTEST" (set$, 1, 2, errormsg$, 0, set)
            if errormsg$ = " " then goto L50150
                errormsg$ = "G/L System code must be '1' (Statutory) or"&~
                    " '2' (Local Authority)"
                return
L50150:     setdescr$ = "Statutory"
            main% = 1%
            if set = 1 then goto L50200
                setdescr$ = "Local Authority"
                main% = 11%
L50200:     sethdr$ = setdescr$
            call "FMTTITLE" (sethdr$, "G/L SYSTEM", 12%)
            call "PUTPAREN" (setdescr$)
            return

L50250:     call "NUMTEST" (which$, 1, 3, errormsg$, 0, whichmonth)
            if errormsg$ = " " then goto L50300
                errormsg$ = "You must choose a period-ending month with"&~
                    " a number from '1' to '3'"
                return
L50300:     whichmonth% = whichmonth
            search dates$() = refdates$(whichmonth%) to search%()
            if search%(1) = 0 then L09200
                thismonth% = (search%(1) + 7)/8 - 15
                mdate$ = mdate$(whichmonth%)
                call "PUTPAREN" (mdate$)
                return

L53000: REM *************************************************************~
            *      C O M P U T E   T O T A L   S A L E S                *~
            *                                                           *~
            * PLOW THROUGH SYSFILE2 TO PICK UP THE SALES ACCOUNTS.  GET *~
            * THE BALANCES FOR THOSE ACCOUNTS AND ADD THEM UP.          *~
            *************************************************************

            call "SHOSTAT" ("Computing Sales For Period Ending " &       ~
                mdate$(whichmonth%))

            readkey$ = "SALES" & set$
            str(readkey$,7) = all(hex(00))
            totalytdsales = 0
            totalcurrsales = 0

L53140:     call "PLOWNEXT" (#04, readkey$, 6%, f1%(4))
            if f1%(4) <> 1 then L53260

            call "BOTMLIN2" (str(readkey$, 7), acctdescr$, currentbal,   ~
                ytdbal, currentbal$, ytdbal$, 1%, thismonth%, #main%,    ~
                f2%(main%), u3%)

            totalytdsales  = totalytdsales  + ytdbal
            totalcurrsales = totalcurrsales + currentbal
            goto L53140

L53260:     totalytdsales  = round(totalytdsales,  2)
            totalcurrsales = round(totalcurrsales, 2)
            return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN.                      *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")
            end
