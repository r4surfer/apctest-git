        REM *************************************************************~
            *                                                           *~
            *   GGG   N   N  L      EEEEE  DDDD    GGG   EEEEE  RRRR    *~
            *  G      NN  N  L      E      D   D  G      E      R   R   *~
            *  G GGG  N N N  L      EEEE   D   D  G GGG  EEEE   RRRR    *~
            *  G   G  N  NN  L      E      D   D  G   G  E      R   R   *~
            *   GGG   N   N  LLLLL  EEEEE  DDDD    GGG   EEEEE  R   R   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GNLEDGER - Prints period or YTD G/L detail either as an   *~
            *            edit report, or as a period ending hard copy   *~
            *            activity log. Shows the opening balance, sums  *~
            *            detail found, and shows the ending balance.    *~
            *            If the calculated ending balance doesn't equal *~
            *            the ending balance from GLMAIN, it tells you.  *~
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
            * 04/08/85 ! ORIGINAL                                 ! RCA *~
            * 07/30/85 ! Misc                                     ! HES *~
            * 03/04/86 ! Change for unformatted Fiscal Dates      ! ERN *~
            * 04/21/86 ! Misc                                     ! HES *~
            * 08/11/86 ! Correct opening of printer for month end ! RAC *~
            * 09/23/87 ! W.R. Grace accountancy (dual books) mods.! JIM *~
            * 08/12/88 ! Dual Books depends on a SYSFILE2 flag.   ! JIM *~
            * 10/08/88 ! If run from PROCE7 and 'dual books' flag ! JDH *~
            *          ! is yes, then reports on both set of books!     *~
            * 05/16/89 ! Modified to include Domestic prod. chgs  ! MLJ *~
            *          !   (1) Added JNL Seq # to report          !     *~
            *          !   (2) Acct # now part of page heading    !     *~
            *          !   (3) Report format now that of domestic !     *~
            * 04/09/92 ! PRR 11629 - Corrected period labeling on ! MLJ *~
            *          !   stand-alone YTD reporting.             !     *~
            * 03/31/93 ! Pass 5000% into SETPRNT for # of records.! JDH *~
            * 11/16/94 ! PRR 13312 - Don't print Account # prior  ! RJH *~
            *          !  to printing ledger balance if page break!     *~
	    * 06/26/96 ! Changes for the year 2000.               ! DXL *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        dim                                                              ~
            aid$1,                       /* AID CHARACTER IN GETPARM   */~
            bal(32),                     /* BALANCES FROM G/L MAIN FILE*/~
            begdate$8,                   /* BEGINNING ACCOUNT BAL DATE */~
            blankdate$8,                 /* Blank date for comparison. */~
            compname$60,                 /* PRINT COMPANY NAME         */~
            cursor%(2),                  /* CURSOR POSITION            */~
            date$8,                      /* FORMATTED G/L DATE         */~
            dates$(32)8,                 /* FISCAL DATES ARRAY         */~
            day1$(18)8,                  /* FIRST OF RELEVENAT MONTH   */~
            descr$56,                    /* GL DETAIL DESCRIPTION      */~
            dual_books$1,                /* Dual books in effect?      */~
            detaildate$6,                /* DATE FROM DETAIL FOR COMP  */~
            detprtdate$8,                /* DETAIL PRINT DATE          */~
            edtmessage$79,                                               ~
            enddate$8,                   /* END DATE RANGE             */~
            endprtdate$8,                /* END ACCOUNT BALANCE DATE   */~
            errormsg$79,                 /* ERROR MESSAGE FOR DISPLAY  */~
            firstaccount$16,             /* FIRST ACCOUNT OF RANGE     */~
            firstacct$9,                 /* FIRST ACCOUNT OF RANGE     */~
            i$(24)80,                    /* Screen for cursor position */~
            inpmessage$79,                                               ~
            jnlid$3,                     /* JOURNAL ID                 */~
            lastaccount$16,              /* LAST ACCOUNT NUMBER        */~
            lastacct$9,                  /* LAST ACCOUNT NUMBER        */~
            linefac$(3)1,                /* FAC'S FOR LINEAR INPUT     */~
            look%(1),                    /* CATCH ARRAY  FOR SEARCH    */~
            months$36,                   /* MONTHS LIST AVAILABLE      */~
            monthe$3,                    /* MONTH END?                 */~
            mdate$(18)12,                /* DATES OF AVAILABLE MONTHS  */~
            oldreadkey$50,               /* OLD KEY FOR PLOW ROUTINES  */~
            pagebrk$3,                   /* PAGE BREAK INDICATOR       */~
            periods$2,                   /* FISCAL PERIODS DISPLAY     */~
            prgmver$79,                  /* Program and version        */~
            prtaccount$16,               /* PRINT ACCOUNT NUMBER       */~
            prtacctdescr$32,             /* ACCOUNT DESCRIPTION        */~
            prtaccttitle$49,             /* ACCOUNT & DESCRIPTION      */~
            prtacctype$9,                /* ACCOUNT TYPE               */~
            prtbal$(4)13,                /* BALANCES TO PRINT          */~
            prtdescr$(3)30,              /* SUMMARY LINE DESCRIPTIONS  */~
            purgedate$6,                 /* LAST DETAIL PURGE DATE     */~
            range$70,                    /* MESSAGE OF RANGE PRINTED   */~
            rptid$6,                     /* Report ID                  */~
            perioddate$8,                /* PERIOD DATE                */~
            searchdate$8,                /* DATE FOR SEARCHES          */~
            set$1, setdescr$30, sethdr$60,/* Set of books to use       */~
            setmsg$18,                   /* Screen message for SET     */~
            seq$6,                       /* Formatted JNL Sequence #   */~
            startdate$8,                 /* START DATE RANGE           */~
            tasktype$1,                  /* FOREGROUND OR BACKGROUND   */~
            tdate$8,                     /* Temp. Date                 */~
            time$8,                      /* Time of day                */~
            type$2,                      /* TRANSACTION TYPE FOR DETAIL*/~
            udate$8,                     /* Unformatted Date           */~
            userid$3,                    /* USER ID CURRENT USER       */~
            ref1$32,                     /* GL DETAIL REFERENCE 1      */~
            ref2$32,                     /* GL DETAIL REFERENCE 2      */~
            ytdrep$3,                    /* YTD REPORT INDICATOR       */~
            which$2                      /* WHICH MONTH THIS TRIAL     */~

        dim f1%(64)                      /* RECORD-ON-FILE FLAGS       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

        REM *************************************************************~
            *                    S E L E C T   F I L E S                *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !  DESCRIPTION                             *~
            *-----+----------+------------------------------------------*~
            * #01 ! GLMAIN   ! GENERAL LEDGER MAIN FILE                 *~
            * #02 ! GLDETAIL ! GENERAL LEDGER DETAIL FILE               *~
            * #04 ! SYSFILE2 ! SYSTEM INFORMATION (WHICH MONTHS OPEN)   *~
            * #11 ! GLMAIN2  ! G. L. chart of accounts for local auth.  *~
            * #12 ! GLDETAL2 ! G. L. detail records for local authority *~
            *************************************************************

            select #01, "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 9

            select  #02, "GLDETAIL",     /* GENERAL LEDGER DETAILS     */~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 160,                                   ~
                        keypos = 1, keylen = 26

            select  #04, "SYSFILE2",                                     ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select #11, "GLMAIN2",                                       ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   9

            select #12, "GLDETAL2",                                      ~
                        varc,     indexed,  recsize = 160,               ~
                        keypos = 1,    keylen = 26

            call "SHOSTAT" ("Opening files, one moment please")

            call "OPENCHCK" (#01, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#02, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#04, 0%, 0%, 0%, " ")
            dual_books$ = "N"                        /* Default to 'no' */
            call "READ100" (#04, "SWITCHS.GL", f1%(4))
                if f1%(4) = 0% then goto L09000
            get #04 using L02445, dual_books$
L02445:         FMT POS(21), CH(1)
            if dual_books$ <> "Y" then goto L09000
                call "OPENCHCK" (#11, 0%, 0%, 0%, " ")
                call "OPENCHCK" (#12, 0%, 0%, 0%, " ")

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES CONTROL VARIABLES, SETS AVAILABLE DATES, ETC. *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)
            standalone% = 0%
            if dual_books$ = "Y" then setmsg$ = "G/L System to use:"
            if dual_books$ = "Y" then lo% = 1% else lo% = 2%
            call "TIME" (time$)
            call "COMPNAME" (12%, compname$, 0%)
            str(prgmver$,62) = "GNLEDGER: " & str(cms2v$,1,8)
            call "EXTRACT" addr ("ID", userid$, "TT", tasktype$)
            date$ = date : call "DATEFMT" (date$)
            prtdescr$(1) ="***** Beginning Balance"
            prtdescr$(2) ="***** Ending Balance"
            monthe$ = " "
            endacctflg% = 0%
            rptid$ = "G/L001"
            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (ENTER)."

        REM GET LAST PURGE DATE FOR G/L
            call "READ100" (#04, "LAST PURGE DATES", f1%(4))
                if f1%(4) <> 0 then get #04, using L09230, purgedate$
L09230:     FMT XX(50), CH(6)

        REM FORMAT DATES FOR HISTORY FORMATTING
            months$ = "JanFebMarAprMayJunJulAugSepOctNovDec"
            init(" ") day1$()
                call "READ100" (#04, "FISCAL DATES", f1%(4))
                     if f1%(4) <> 0 then L09320
                     goto L09840

L09320:          get #04, using L09330, periods%, monthopen%, dates$()
L09330:                  FMT XX(20), BI(2), XX(136), BI(2), 32*CH(8)

                p%, p1% = 16
                if monthopen% = 12 and periods% = 12 then p1% = 17
                str(dates$(), (monthopen%+p1%)*8+1), mdate$() =" "
                temp1% = 0
                for temp% = 32% to  p% - 1  step-1
                if dates$(temp%) = " " or ~
                   dates$(temp%) = blankdate$ then L09510
                    temp1% = temp1% + 1
                    if temp1% > monthopen% + 2 then L09520
                    day1$(temp1%) = dates$(temp%)
                    if temp% <> 15 then L09480
                       maxmonths% = temp1%
                       mdate$(temp1%) = str(dates$(temp%),,8)
                       goto L09510
L09480:             tdate$ = dates$(temp%)
		    call "DATEFMT" (tdate$, tdate%, udate$)
                    convert str(udate$,5%,2%) to month%
                    mdate$(temp1%) = str(months$, 3%*month%-2%, 3%) & " " & ~
                    str(udate$,7%,2%) &", "& str(udate$,1%,4%)
L09510:         next temp%
L09520: REM DO A GETPARM TO FIND IF WE ARE AT MONTH END
                call "GETPARM" addr ("ID", "S", "MONTHEND", aid$, "0001",~
                                     "GLTRAL", "Are We at Month End?",   ~
                                     20%, "K", "MONTHEND", monthe$, 3%,  ~
                                     5%, 32%, "A")

                if monthe$ <> "YES" then L10000
                standalone% = 1%          /* Run from PROC */
                set = 0
                whichmonth% = 2
                ytdflg% = 0
                ytdrep$ = "NO"
                pagebrk$ = "NO"
                firstaccount$ = "ALL"
                lastaccount$  = " "
                init(hex(00)) firstacct$
                init(hex(ff)) lastacct$
            enddate$, searchdate$ = day1$(whichmonth%)
            endprtdate$ = enddate$
            call "DATEFMT" (endprtdate$)
            startdate$ = day1$(whichmonth%+1)
            if startdate$ = "CLOSING" then startdate$=dates$(periods% + 1)
            call "DATE" addr ("G+", startdate$, 1%, startdate$, u3%)
            begdate$ = startdate$
            call "DATEFMT" (begdate$)
            if u3% <> 0 then L09840
L09760:     bigtotaldebits, bigtotalcredits, bigbalance = 0
            pagenumber% = 0
            pageline% = 1000
            set = set + 1 : gosub setup_set_values /* Force 'Statutory' */
              if set > 2 then L65000
                gosub L19000
                close printer
                go to L65000

L09840:     err% = 0%
            call "ASKUSER" (err%, "ERROR ON SETTING DATE RANGE",         ~
                            "Review your fiscal date structure",         ~
                           "Press PF-16 to Acknowledge and Exit Program",~
                           " ")
            if err% <> 16% then L09840
            goto L65000

L10000: REM *************************************************************~
            *         I N P U T   W H I C H   M O N T H                 *~
            *                                                           *~
            * CONTROLING SECTION FOR WHICH MONTH TO PRINT               *~
            *************************************************************
        inputmode
            init(" ") firstaccount$, lastaccount$, errormsg$, set$,      ~
                which$, errormsg$, ytdrep$, pagebrk$, setdescr$, mondescr$

            bigtotaldebits, bigtotalcredits, bigbalance, perioddebits,   ~
                periodcredits, totaladjustbal = 0
            firstaccount$ = "ALL"
            pagenumber%, end_report% = 0%
            pageline% = 1000%

            for fieldnr% = 1 to 2
L10140:         gosub'101(fieldnr%)
                      if enabled% = 0 then L10180
                gosub'201(fieldnr%)
                      if keyhit%  =   1 then gosub startover
                      if keyhit%  =  16 then L65000
                      if keyhit% <>   0 then L10140
L10180:         gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10140
            next fieldnr%

            for fieldnr% = 1 to 3
            gosub'102(fieldnr%)
            if enabled% = 0 then L10300
L10240:     gosub'202(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then       L10240
            gosub'152(fieldnr%)
                  if errormsg$ <> " " then L10240
L10300:     next fieldnr%

        REM *************************************************************~
            *               E D I T  M O D E                            *~
            *************************************************************

        editmode
            inpmessage$ = edtmessage$
            gosub'211(0%)
                if keyhit%  =   1 then gosub startover
                if keyhit%  =   5 then editmode2
                if keyhit%  =  16 then datasave
                if keyhit% <>   0 then editmode
             fieldnr% = cursor%(1) - 4%
             if fieldnr% < lo% or fieldnr% > 2% then editmode

            gosub'101(fieldnr%)
L11180:     gosub'211(fieldnr%)
                if keyhit%  =   1 then gosub startover
                if keyhit% <>   0 then L11180
            gosub'151(fieldnr%)
                if errormsg$ <> " " then L11180
            goto editmode

        editmode2
            inpmessage$ = edtmessage$
            gosub'212(0%)
                if keyhit%  =   1 then gosub startover
                if keyhit%  =   4 then editmode
                if keyhit%  =  16 then datasave
                if keyhit% <>   0 then editmode2
             fieldnr% = cursor%(1) - 5%
             if fieldnr% < 1% or fieldnr% > 3% then editmode2

            gosub'102(fieldnr%)
L11340:     gosub'212(fieldnr%)
                if keyhit%  =   1 then gosub startover
                if keyhit% <>   0 then L11340
            gosub'152(fieldnr%)
                if errormsg$ <> " " then L11340
            goto editmode2

L12000: REM *************************************************************~
            *   P L O W   T H R O U G H   F I L E   A N D   P R I N T   *~
            *                                                           *~
            * PLOWS THROUGH FILE OF ACCOUNTS AND THEN PLOW THRU DETAILS *~
            * TO GENERATE TOTALS BELOW.                                 *~
            * NOTE THAT WE DON'T INCLUDE DATA  IN THE TOTAL THAT IS     *~
            * MODULE DATED BEFORE THE FIRST OF THE CURRENT MONTH OR     *~
            * AFTER THE CURRENT G/L DATE.  THIS IS SO THAT WE CAN GET A *~
            * TRIAL BALANCE FOR APRIL IF THE CURRENT MONTH OPEN IS JUNE.*~
            *************************************************************

            oldacctkey$ = firstacct$
            call "SHOSTAT" ("Processing General Ledger Report For Period ~
        ~Ending " & mdate$(whichmonth%) )
            if purgedate$ > searchdate$ then searchdate$ = purgedate$

L12160: REM PLOW ROUTINE FOR ACCOUNTS.
            if endacctflg% = 1% and pageline% < 54% then endacctflg% = 0%
            search dates$() = searchdate$ to look%() step 8
                if look%(1) <> 0 then L12200
                goto L09840
L12200:     bucketpointer%, priorbalpointer% = (look%(1)+7)/8
            if ytdrep$ <> "YES" then L12270
            if ytdflg% = 0 then priorbalpointer% = 16%
            if ytdflg% = 1% then priorbalpointer% = 2%
            if ytdflg% = 1% then bucketpointer% = 15%
            if ytdflg% = 3% then priorbalpointer% = 16% + periods%

L12270:     call "READ102" (#main%, oldacctkey$, f1%(main%))
                 if f1%(main%) = 0 then L12640
            get #main% using L12410, newacctkey$, prtacctdescr$,          ~
                prtacctype$, bal()
            if newacctkey$ > lastacct$ then L12640 /* End Report */

            prtaccount$ = newacctkey$
            if set = 1                                                   ~
                then call "GLFMT" (prtaccount$)                          ~
                else call "GLFMT2" (prtaccount$)
            call "PUTPAREN" (prtacctdescr$)
            prtaccttitle$ = prtaccount$ & " " & prtacctdescr$
            if prtacctype$ = "$" then prtacctype$ = "Cash"
            if prtacctype$ = "A" then prtacctype$ = "Asset"
            if prtacctype$ = "L" then prtacctype$ = "Liability"
            if prtacctype$ = "C" then prtacctype$ = "Capital"
            if prtacctype$ = "R" then prtacctype$ = "Revenue"
            if prtacctype$ = "E" then prtacctype$ = "Expense"

L12410:             FMT CH(9),           /* ACCOUNT NUMBER             */~
                        CH(30),          /* DESCRIPTION                */~
                        CH(1),           /* ACCOUNT TYPE               */~
                        XX(4),           /* DETAIL SEQUENCE NUMBER     */~
                        32*PD(14,4)      /* BALANCE STUFF.             */

            oldacctkey$ = newacctkey$
            priorbal, currentbal, totalcredits, totaldebits = 0
            for i% = 1 to priorbalpointer% - 1
                priorbal = round(priorbal + bal(i%), 2)
            next i%
            for i% = 1 to bucketpointer%
                currentbal = round(currentbal + bal(i%), 2)
            next i%
            call "CONVERT" (priorbal, 2.2, prtbal$(1))
            gosub L17000  /* Print account subtitle */
            gosub L14000  /* Print account details */
            bigtotaldebits  = bigtotaldebits  + totaldebits
            bigtotalcredits = bigtotalcredits + totalcredits
            bigbalance      = bigbalance + currentbal
            goto L12160

L12640: REM WRAP UP PLOW ROUTINE HERE IF END OF FILE FOUND.
        REM PRINT TOTAL LINE FOR THE WHOLE MESS...
            call "CONVERT" (bigtotaldebits,  2.2, prtbal$(2))
            call "CONVERT" (bigtotalcredits, 2.2, prtbal$(3))
            call "CONVERT" (bigbalance,      2.2, prtbal$(4))
            call "CONVERT" (totaladjustbal,  2.2, prtbal$(1))
*          PRTDESCR$(3) = "** Detail Missing **"
            if totaladjustbal=0 then prtbal$(1), prtdescr$(3) = " "
            end_report% = 1%
            gosub L16000
            print using L18170
            print skip(1)
            print using L18440, prtdescr$(3), prtbal$(1), prtbal$(2),     ~
                prtbal$(3), prtbal$(4)
            print using L18170

        REM CLOSE PRINTER AND SEND PRINT TO QUEUE
            if pageline% < 56% then L12810
            pageline% = 75%  :  gosub L16000
L12810:     print skip(1)
            print using L12850
            return

L12850: %                                      **********   E N D   O F  ~
        ~ R E P O R T   **********

        %!                                                               ~
        ~                                                                 ~
        ~  !

L14000: REM *************************************************************~
            * A C T U A L L Y   P R I N T   A C C O U N T   T O T A L S *~
            *                                                           *~
            * PRINTS ACCOUNT DESCRIPTION, PRIOR BAL, DEBITS, CREDITS,   *~
            *  AND BALANCE TO DATE THIS MONTH.                          *~
            *                       NOTE THAT THE COMPUTER ASSUMES THAT *~
            * THE BALANCE AT THE BEGINNING OF THE MONTH WAS CORRECT.    *~
            * IF IT TURNS OUT TO BE WRONG (IE THE G/L DOES NOT BALANCE),*~
            * THEN YOU USE THE G/LSTOMP PROGRAM TO ADJUST THE BALANCE   *~
            * DIRECTLY.                                                 *~
            *************************************************************

            init(" ") prtbal$()
            perioddate$ = enddate$
            if perioddate$ <> "CLOSING" then L14160
            perioddate$ = dates$(periods% + 1)
L14160:     if ytdrep$ <> "YES" then L14210
            if ytdflg%=0 then period% = monthopen% + 1% else period%=14
            if ytdflg%=3 then period% = monthopen% - periods%
            perioddate$ = day1$(period%)
            if perioddate$="CLOSING" then perioddate$=dates$(periods%+1)
L14210:     periodcredits, perioddebits = 0
            init (hex(00)) oldreadkey$
            str(oldreadkey$,,16)= oldacctkey$
            str(oldreadkey$,17,6)= str(startdate$,,6)
            call "READ100" (#detl%, oldreadkey$, f1%(detl%))
                if f1%(detl%) = 1 then L14300
L14280:     call "PLOWNEXT" (#detl%, oldreadkey$, 16%, f1%(detl%))
                if f1%(detl%) = 0 then gosub L14690
L14300:     get #detl%, using L14560, detaildate$, type$, debit, credit,  ~
                ref1$, ref2$, descr$, jnlid$, seq%
            if enddate$="CLOSING" and type$ <> "99" then L14280

        REM TEST TO MAKE SURE DETAIL DATE IS IN REPORT PERIOD
            if detaildate$ > perioddate$ then gosub L14690
            call "CONVERT" (debit, 2.2, prtbal$(2))
            call "CONVERT" (credit, 2.2, prtbal$(3))
            if pageline% < 59% then L14400
            pageline% = 75%  :  gosub L16000
L14400:     pageline% = pageline% + 1%
            if debit = 0 then prtbal$(2) = " "
            if credit = 0 then prtbal$(3) = " "
            detprtdate$ = detaildate$
            if detprtdate$ = "CLOSING" then L14460
            call "DATEFMT" (detprtdate$)
            seq = seq%  :  call "CONVERT" (seq, -0.01, seq$)
L14460:     print using L18350, detprtdate$, type$, jnlid$, seq$, ref1$,  ~
                        ref2$, descr$, prtbal$(2), prtbal$(3), " "

        REM ADD DEBIT & CREDIT AMOUNTS TO ACCUMULATORS.
            totaldebits  = totaldebits  + debit
            totalcredits = totalcredits + credit
            perioddebits = perioddebits + debit
            periodcredits= periodcredits + credit
            goto L14280

L14560:         FMT XX(16),              /* ACCOUNT NUMBER             */~
                    CH(6),               /* MODULE DATE POSTED         */~
                    XX(4),               /* SEQUENCE NUMBER STRING     */~
                    CH(2),               /* JOURNAL TYPE               */~
                    2*PD(14,4),          /* DEBIT, CREDIT AMOUNTS.     */~
                    CH(30),              /* REFERENCE 1 TEXT           */~
                    CH(34),              /* REFERENCE 2 TEXT           */~
                    XX(4),               /* ADJUSTMENT CODE            */~
                    CH(32),              /* DESCRIPTION                */~
                    CH(3),               /* JOURNAL ID                 */~
                    BI(4),               /* SEQUENCE NUMBER            */~
                    XX(3),               /* OPERATOR ID OF CURRENT USER*/~
                    XX(6)                /* SYSTEM DATE WHEN WRITTEN.  */

L14690:     if ytdrep$ <> "YES" then L15040

        REM Print Period Sub Totals...
            if perioddebits = 0 and periodcredits = 0 then L14920
            periodtotal = round(perioddebits - periodcredits, 2)
            if periodtotal < 0 then L14780
            call "CONVERT" (periodtotal, 2.2, prtbal$(2))
            prtbal$(3) = " "
            goto L14810
L14780:     call "CONVERT" (abs(periodtotal), 2.2, prtbal$(3))
            prtbal$(2) = " "
L14810:     if ytdflg% = 3 then convert                                  ~
                (monthopen% - period% + 1 - periods%) to periods$,pic(##)
            if ytdflg% <> 0% then L14850
                if periods% = 13% then L14838
                    if monthopen% > 12% then L14842
L14838:         convert (monthopen% - period% + 2) to periods$, pic(##)
                goto L14870
L14842:         convert (monthopen% - period% + 1) to periods$, pic(##)
                goto L14870
L14850:     if ytdflg% = 1 then convert                                  ~
                (periods% - period% + 2%) to periods$, pic(##)
L14870:     if pageline% < 58% then L14880
            pageline% = 75%  :  gosub L16000
L14880:     pageline% = pageline% + 3%
            print skip(1)
            print using L18410, periods$, prtbal$(2), prtbal$(3)
            print skip(1)
L14920:     if f1%(detl%) = 0 then L15040
            if detaildate$ > enddate$ then L15040
            for newperiod% = period% to whichmonth% step -1
                perioddate$ = day1$(newperiod%)
                if perioddate$ = "CLOSING"                               ~
                   then perioddate$ = dates$(periods% + 1)
                if detaildate$ <= perioddate$ then L15000
            next newperiod%
L15000:     period% = newperiod%
            perioddebits, periodcredits = 0
            return

L15040: REM Print Account Totals, Then Move on To New Account...
            return clear
            adjustbal = currentbal-priorbal-totaldebits+totalcredits
            if pageline% < 58% then L15090
            pageline% = 75%  :  gosub L16000
L15090:     call "CONVERT" (totaldebits,  2.2, prtbal$(2))
            call "CONVERT" (totalcredits, 2.2, prtbal$(3))
            call "CONVERT" (currentbal,   2.2, prtbal$(4))
            call "CONVERT" (adjustbal,    2.2, prtbal$(1))
*          PRTDESCR$(3) = "** Detail Missing **"
            if adjustbal = 0 then prtbal$(1),prtdescr$(3) = " "
            totaladjustbal = totaladjustbal + adjustbal
            print using L18380, endprtdate$, prtdescr$(2), prtdescr$(3),  ~
                           prtbal$(1), prtbal$(2), prtbal$(3), prtbal$(4)
            print using L18260
            pageline% = pageline% + 2%
            endacctflg% = 1%
            return

L16000: REM *************************************************************~
            *        P A G E   C O N T R O L   R O U T I N E            *~
            *************************************************************

                if pageline% < 58% then return
                   if pagenumber% = 0% or endacctflg% = 1% then L16080
                   print using L18170
L16080:            print page
                   pagenumber% = pagenumber% + 1
                   if firstaccount$ <> "ALL" then L16130
                      range$ = "ALL Accounts"      : go to L16172
L16130:            if lastaccount$ <> " " then L16160
                      range$ = "For Account " & firstaccount$ & " ONLY"
                         go to L16172
L16160:               range$ = "From Account " & firstaccount$ &         ~
                                 " To Account " & lastaccount$
L16172:            print using L18052, date$, time$, compname$,           ~
                        "-" & rptid$
                   print using L18060, pagenumber%
                   print using L18081, sethdr$
                   if ytdrep$ = "YES" then                               ~
                           print using L18120, mdate$(whichmonth%)        ~
                      else print using L18090, mdate$(whichmonth%)
                   print skip( 1)
                   print using L18150, range$
                   print using L18260
                   print using L18320
                   print using L18260
                   if end_report% = 0% then                              ~
                       print using L18230, prtaccttitle$, prtacctype$
                   pageline% = 10%
                   endacctflg% = 0%
                   return

L17000: REM *************************************************************~
            *             S U B  H E A D I N G  R O U T I N E           *~
            *                                                           *~
            * PRINTS NEW ACCOUNT INFORMATION                            *~
            *************************************************************

            if pagebrk$ = "YES" then pageline% = 100
            if pageline% < 54% then L17090
            pageline% = 75%  :  gosub L16000
L17090:     pageline% = pageline% + 4
            if pageline% <> 14 then                                      ~
                print using L18230, prtaccttitle$, prtacctype$
            print skip(1)
            print using L18350, begdate$," ", " ", " ", prtdescr$(1),     ~
                               " ", " ", " ", " ", prtbal$(1)
            print skip(1)
            return

        REM *************************************************************~
            *                  P R I N T  L I N E S                     *~
            *************************************************************

L18052: %RUN ######## @ #########           #############################~
        ~###############################                      GNLEDGER####~
        ~###
L18060: %                                                   G E N E R A L~
        ~   L E D G E R                                            PAGE: #~
        ~###
L18081: %                                   #############################~
        ~###############################
L18090: %                                            P E R I O D   E N D ~
        ~I N G   ############

L18120: %                                         Y E A R - T O - D A T E~
        ~   T H R U   ############

L18150: % ** Report Range is ############################################~
        ~###########################
L18170: %================================================================~
        ~=================================================================~
        ~===
        %!                                                               ~
        ~                                                                 ~

L18230: % ACCOUNT: ################################################# ACCO~
        ~UNT TYPE: ##########                                             ~

L18260: %----------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~---

L18320: %Date     MD JNL/Seq    Reference-1              Reference-2     ~
        ~         Description              Debits       Credits       Bala~
        ~nce
L18350: %######## ## ### ###### ######################## ################~
        ~######## ################# ############# ############# ##########~
        ~###
L18380: %########               ######################## ################~
        ~########     ############# ############# ############# ##########~
        ~###
L18410: %                 PERIOD ## ACTIVITY                             ~
        ~                         ############# #############             ~

L18440: %                **** LEDGER BALANCE ****  ######################~
        ~###          ############# ############# ############# ##########~
        ~###

L19000: REM *************************************************************~
            *                  P R I N T  R E P O R T                   *~
            *                                                           *~
            * Prints period to date or year to date report              *~
            *************************************************************

        datasave
            gosub L55000                  /* SET PRINT PARAMETERS       */
            select printer (134)
            call "SETPRNT" ("G/L001", " ", 5000%, 0%)
            gosub L12000                  /* PRINT ACCOUNTS             */
            close printer
            call "SETPRNT" ("G/L001", " ", 0%, 1%)
                if standalone% = 1% and dual_books$ = "Y" then L09760
            goto L65000                   /* Exit Program               */

        REM *************************************************************~
            *       D E F A U L T / E N A B L E  F O R  P A G E  1      *~
            *                                                           *~
            *  SETS DEFAULT AND ENABLES FIELDS FOR PAGE 1 OF THE INPUT  *~
            *************************************************************

        deffn'101(fieldnr%)
            enabled% = 1
            on fieldnr% gosub L20110, L20160 /* G/L set; Period to report*/
            return

L20110: REM ENABLE FOR G/L set to use
            if dual_books$ <> "Y" then enabled% = 0%
            inpmessage$ = "Enter '1' for Statutory G/L set of books; '2"&~
                "' for Local Authority."
            return

L20160: REM ENABLE FOR PERIOD
            inpmessage$ = "Enter the number beside the Period-End date "&~
                "you wish to print"
            return

        REM *************************************************************~
            *       D E F A U L T / E N A B L E  F O R  P A G E  2      *~
            *                                                           *~
            *  SETS DEFAULT AND ENABLES FIELDS FOR PAGE 2 OF THE INPUT  *~
            *************************************************************

        deffn'102(fieldnr%)
            enabled% = 1
            on fieldnr% gosub L21120,     /* ACCOUNT RANGE              */~
                              L21150,     /* YTD REPORT                 */~
                              L21180      /* PAGE BREAK                 */
            return

L21120: REM ENABLE FOR RANGE OF ACCOUNTS
            inpmessage$ = "Enter a range of Account #s to print or 'ALL'"
            return

L21150: REM ENABLE FOR YTDREP$
            ytdrep$ = "NO"
            inpmessage$ = "Do you want a Year-to-Date report? 'YES' or "&~
                "'NO'"
            return

L21180: REM ENABLE FOR PAGEBRK$
            pagebrk$ = "NO"
            inpmessage$ = "Do you want page breaks for each acct? 'YES'"&~
                " or 'NO'"
            return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.
            keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return
            return clear all
            goto inputmode

        REM *************************************************************~
            *       I N P U T   R A N G E   O F   A C C O U N T S       *~
            *                                                           *~
            * INPUT RANGE OF ACCOUNTS TO PRINT TRIAL BALANCE FOR.       *~
            *************************************************************

        deffn'202(fieldnr%)
            init (hex(8c)) linefac$()
            on fieldnr% gosub L40057,     /*ACCOUNT RANGE               */~
                              L40057,     /*YEAR TO DATE INDICATOR      */~
                              L40057      /*PAGE BREAK INDICATOR        */
            goto L40062
            linefac$(fieldnr%) = hex(80) /*UPPER/LOWER CASE INPUT*/
            return
L40057:     linefac$(fieldnr%) = hex(81) /*UPPER CASE ONLY INPUT */
            return
            linefac$(fieldnr%) = hex(82) /*NUMERIC INPUT ONLY    */
            return

L40062:     str(prgmver$,1,50) = "Enter Print Parameters"
L40070:     accept                                                       ~
               at (01,02), "Print General Ledger Report",                ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), prgmver$               , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), "Account Range FROM",                         ~
               at (06,30), fac(linefac$( 1)), firstaccount$     , ch(12),~
               at (06,44), "TO",                                         ~
               at (06,48), fac(linefac$( 1)), lastaccount$      , ch(12),~
               at (07,02), "Year-To-Date Report ?",                      ~
               at (07,30), fac(linefac$( 2)), ytdrep$           , ch(03),~
               at (08,02), "Page Break On New Account ?",                ~
               at (08,30), fac(linefac$( 3)), pagebrk$          , ch(03),~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65),  "(13)Instructions",                          ~
               at (23,65),  "(15)Print Screen",                          ~
                                                                         ~
               keys(hex(00010d0f)),                                      ~
               key (keyhit%)

               if keyhit% <> 13 then L40380
                  call "MANUAL" ("GNLEDGER")
                  goto L40070

L40380:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40070

        REM *************************************************************~
            *   I N P U T   W H I C H   M O N T H                       *~
            *                                                           *~
            * INDICATE WHICH MONTH YOU DESIRE TRIAL BALANCE FOR         *~
            *************************************************************

        deffn'201(fieldnr%)
            init (hex(8c)) linefac$()
            on fieldnr% gosub L41130, L41130  /* G/L set, Period     */
            goto L41180
            linefac$(fieldnr%) = hex(80) /*UPPER/LOWER CASE INPUT*/
            return
            linefac$(fieldnr%) = hex(81) /*UPPER CASE ONLY INPUT */
            return
L41130:     linefac$(fieldnr%) = hex(82) /*NUMERIC INPUT ONLY    */
            return

L41180:     if dual_books$ = "Y" then str(prgmver$,1,50) =               ~
                "Choose G/L Set and Period for Report"                   ~
                else str(prgmver$,1,50) = "Choose Period for Report"
L41190:     accept                                                       ~
               at (01,02), "Print General Ledger Report",                ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), prgmver$               , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), fac(hex(8c)),      setmsg$           , ch(18),~
               at (05,30), fac(linefac$( 1)), set$              , ch(01),~
               at (05,33), fac(hex(8c)),      setdescr$         , ch(32),~
                                                                         ~
               at (06,02), "Enter Number beside Period:",                ~
               at (06,30), fac(linefac$( 2)), which$            , ch(02),~
               at (06,33), fac(hex(8c)),      mondescr$         , ch(14),~
               at (08,02), "The following Period-Ending dates will show a~
        ~ctivity:",                                                       ~
             at (10,15),"01 =",at (10,20),fac(hex(8c)),mdate$(01),ch(12),~
             at (11,15),"02 =",at (11,20),fac(hex(8c)),mdate$(02),ch(12),~
             at (12,15),"03 =",at (12,20),fac(hex(8c)),mdate$(03),ch(12),~
             at (13,15),"04 =",at (13,20),fac(hex(8c)),mdate$(04),ch(12),~
             at (14,15),"05 =",at (14,20),fac(hex(8c)),mdate$(05),ch(12),~
             at (15,15),"06 =",at (15,20),fac(hex(8c)),mdate$(06),ch(12),~
             at (16,15),"07 =",at (16,20),fac(hex(8c)),mdate$(07),ch(12),~
             at (17,15),"08 =",at (17,20),fac(hex(8c)),mdate$(08),ch(12),~
             at (18,15),"09 =",at (18,20),fac(hex(8c)),mdate$(09),ch(12),~
             at (10,45),"10 =",at (10,50),fac(hex(8c)),mdate$(10),ch(12),~
             at (11,45),"11 =",at (11,50),fac(hex(8c)),mdate$(11),ch(12),~
             at (12,45),"12 =",at (12,50),fac(hex(8c)),mdate$(12),ch(12),~
             at (13,45),"13 =",at (13,50),fac(hex(8c)),mdate$(13),ch(12),~
             at (14,45),"14 =",at (14,50),fac(hex(8c)),mdate$(14),ch(12),~
             at (15,45),"15 =",at (15,50),fac(hex(8c)),mdate$(15),ch(12),~
             at (16,45),"16 =",at (16,50),fac(hex(8c)),mdate$(16),ch(12),~
             at (17,45),"17 =",at (17,50),fac(hex(8c)),mdate$(17),ch(12),~
             at (18,45),"18 =",at (18,50),fac(hex(8c)),mdate$(18),ch(12),~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)EXIT PROGRAM",                           ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L41660
                  call "MANUAL" ("GNLEDGER")
                  goto L41190

L41660:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L41190

        REM *************************************************************~
            *         E D I T   P A G E  2                              *~
            *                                                           *~
            *  EDIT RANGE OF ACCOUNTS TO PRINT TRIAL BALANCE FOR.       *~
            *************************************************************

        deffn'212(fieldnr%)
            if fieldnr% = 0%                                             ~
                then init (hex(84)) linefac$()                           ~
                else init (hex(8c)) linefac$()
            on fieldnr% gosub L42130,     /*ACCOUNT RANGE               */~
                              L42130,     /*YEAR TO DATE INDICATOR      */~
                              L42130      /*PAGE BREAK INDICATOR        */
            goto L42181
            linefac$(fieldnr%) = hex(80) /*UPPER/LOWER CASE INPUT*/
            return
L42130:     linefac$(fieldnr%) = hex(81) /*UPPER CASE ONLY INPUT */
            return
            linefac$(fieldnr%) = hex(82) /*NUMERIC INPUT ONLY    */
            return

L42181:     str(prgmver$,1,50) = "Edit Print Parameters"
L42190:     accept                                                       ~
               at (01,02), "Print General Ledger Report",                ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), prgmver$               , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), "Account Range FROM",                         ~
               at (06,30), fac(linefac$( 1)), firstaccount$     , ch(12),~
               at (06,44), "TO",                                         ~
               at (06,48), fac(linefac$( 1)), lastaccount$      , ch(12),~
               at (07,02), "Year-To-Date Report ?",                      ~
               at (07,30), fac(linefac$( 2)), ytdrep$           , ch(03),~
               at (08,02), "Page Break on New Account ?",                ~
               at (08,30), fac(linefac$( 3)), pagebrk$          , ch(03),~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (23,16), "(4)Previous Screen",                         ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)PRINT REPORT",                           ~
                                                                         ~
               keys(hex(0001040d0f10)),                                  ~
               key (keyhit%)

               if keyhit% <> 13 then L42550
                  call "MANUAL" ("GNLEDGER")
                  goto L42190

L42550:        if keyhit% <> 15 then L42590
                  call "PRNTSCRN"
                  goto L42190

L42590:        REM GET CURSOR POSITION ON SCREEN FOR EDIT.
                   close ws
                   call "SCREEN" addr("C",u3%,"I",i$(),cursor%())
                   return

        REM *************************************************************~
            *   I N P U T   W H I C H   M O N T H                       *~
            *                                                           *~
            * INDICATE WHICH MONTH YOU DESIRE TRIAL BALANCE FOR         *~
            *************************************************************

        deffn'211(fieldnr%)
            if fieldnr% = 0%                                             ~
                then init (hex(84)) linefac$()                           ~
                else init (hex(8c)) linefac$()
            on fieldnr% gosub L43121, L43130 /* G/L set, PERIOD */
            goto L43155
            linefac$(fieldnr%) = hex(80) /*UPPER/LOWER CASE INPUT*/
            return
            linefac$(fieldnr%) = hex(81) /*UPPER CASE ONLY INPUT */
            return
L43121:     if dual_books$ = "Y" then goto L43130
                linefac$(1%) = hex(9c)
                return
L43130:     linefac$(fieldnr%) = hex(82) /*NUMERIC INPUT ONLY    */
            return

L43155:     if dual_books$ = "Y" then str(prgmver$,1,50) =               ~
                "Choose G/L Set and Period for Report"                   ~
                else str(prgmver$,1,50) = "Choose Period for Report"
L43160:     accept                                                       ~
               at (01,02), "Print General Ledger Report",                ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), prgmver$               , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), fac(hex(8c)),      setmsg$           , ch(18),~
               at (05,30), fac(linefac$( 1)), set$              , ch(01),~
               at (05,33), fac(hex(8c)),      setdescr$         , ch(32),~
                                                                         ~
               at (06,02), "Enter Number beside Period:",                ~
               at (06,30), fac(linefac$( 2)), which$            , ch(02),~
               at (06,33), fac(hex(8c)),      mondescr$         , ch(14),~
               at (08,02), "The following Period-Ending dates will show a~
        ~ctivity:",                                                       ~
              at (10,16),"01=",at (10,20),fac(hex(8c)),mdate$(01),ch(12),~
              at (11,16),"02=",at (11,20),fac(hex(8c)),mdate$(02),ch(12),~
              at (12,16),"03=",at (12,20),fac(hex(8c)),mdate$(03),ch(12),~
              at (13,16),"04=",at (13,20),fac(hex(8c)),mdate$(04),ch(12),~
              at (14,16),"05=",at (14,20),fac(hex(8c)),mdate$(05),ch(12),~
              at (15,16),"06=",at (15,20),fac(hex(8c)),mdate$(06),ch(12),~
              at (16,16),"07=",at (16,20),fac(hex(8c)),mdate$(07),ch(12),~
              at (17,16),"08=",at (17,20),fac(hex(8c)),mdate$(08),ch(12),~
              at (18,16),"09=",at (18,20),fac(hex(8c)),mdate$(09),ch(12),~
              at (10,46),"10=",at (10,50),fac(hex(8c)),mdate$(10),ch(12),~
              at (11,46),"11=",at (11,50),fac(hex(8c)),mdate$(11),ch(12),~
              at (12,46),"12=",at (12,50),fac(hex(8c)),mdate$(12),ch(12),~
              at (13,46),"13=",at (13,50),fac(hex(8c)),mdate$(13),ch(12),~
              at (14,46),"14=",at (14,50),fac(hex(8c)),mdate$(14),ch(12),~
              at (15,46),"15=",at (15,50),fac(hex(8c)),mdate$(15),ch(12),~
              at (16,46),"16=",at (16,50),fac(hex(8c)),mdate$(16),ch(12),~
              at (17,46),"17=",at (17,50),fac(hex(8c)),mdate$(17),ch(12),~
              at (18,46),"18=",at (18,50),fac(hex(8c)),mdate$(18),ch(12),~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (23,16), "(5)Next Screen",                             ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)PRINT REPORT",                           ~
                                                                         ~
               keys(hex(0001050d0f10)),                                  ~
               key (keyhit%)

               if keyhit% <> 13 then L43670
                  call "MANUAL" ("GNLEDGER")
                  goto L43160

L43670:        if keyhit% <> 15 then L43720
                  call "PRNTSCRN"
                  goto L43160


L43720:        REM GET CURSOR POSITION ON SCREEN FOR EDIT.
                   close ws
                   call "SCREEN" addr("C",u3%,"I",i$(),cursor%())
                   return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA--MAKES SURE THE RANGE OF ACCOUNTS IS OK,       *~
            * NORMALIZES THE KEYS FOR PLOW ROUTINE.                     *~
            *************************************************************

        deffn'152(fieldnr%)
             errormsg$ = " "
             on fieldnr% gosub L50077,    /* RANGE OF ACCOUNTS          */~
                               L50260,    /* YTD REPORT                 */~
                               L50310     /* PAGE BREAK                 */
             return

L50077:      REM HANDLES CASE FOR "ALL" ACCOUNTS
                 if firstaccount$ <> "ALL" then L50095
                    init(hex(00)) firstacct$
                    init(hex(ff)) lastacct$
                    return
L50095: REM HANDLES CASE FOR SINGLE ACCOUNT
            if set = 1                                                   ~
               then call "GLVALID" (firstaccount$, firstacct$, errormsg$)~
               else call "GLVALD2" (firstaccount$, firstacct$, errormsg$)
            if errormsg$ = " " then L50120
                errormsg$ = errormsg$ & ": " & firstaccount$
                return
L50120:     if lastaccount$ <> " " then L50135
                lastacct$ = firstacct$
                goto L50165
L50135: REM HANDLES CASE FOR A RANGE OF ACCOUNTS
            if set = 1                                                   ~
                then call "GLVALID" (lastaccount$, lastacct$, errormsg$) ~
                else call "GLVALD2" (lastaccount$, lastacct$, errormsg$)
            if errormsg$ = " " then L50160
                errormsg$ = errormsg$ & ": " & lastaccount$
                return
L50160:     if lastacct$ < firstacct$ then L50175
L50165:        str(firstacct$) = str(firstacct$) addc all(hex(ff))
               return
L50175: REM HANDLES ERROR MESSAGE -- LAST < FIRST.
            errormsg$ = "Invalid Range!  Please Respecify."
            return

L50260: REM TEST DATA FOR YTDREP$
            if ytdrep$ ="YES" or ytdrep$ = "NO" then L50290
            errormsg$ = "Enter 'YES' or 'NO' for Year to Date Report"
L50290:     return

L50310: REM TEST DATA FOR PAGEBRK$
            if pagebrk$ ="YES" or pagebrk$ ="NO" then L50340
            errormsg$ = "Enter 'YES' or 'NO' for Year to Date Report"
L50340:     return

        REM *************************************************************~
            *      T E S T   W H I C H   M O N T H  E N T E R E D       *~
            *                                                           *~
            * TEST THE MONTH ENTERED TO ENSURE THAT IT IS WITHIN RANGE  *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L51100, L52000 /* Test for G/L set, Period */
            return

L51100: REM TEST DATA FOR G/L set of books
            if dual_books$ = "Y" then goto L51134
                main% = 1% : detl% = 2% : set = 1 : set$, setdescr$ = " "
                return
            if dual_books$ <> "Y" then return
L51134:     call "NUMTEST" (set$, 1, 2, errormsg$, 0, set)
            if errormsg$ = " " then goto L51310
                errormsg$ = "G/L System code must be '1' (Statutory) or"&~
                    " '2' (Local Authority)"
                return

L51310: setup_set_values
            if dual_books$ <> "Y" then L51100
            setdescr$ = "Statutory"
            main% = 1% : detl% = 2%
            if set = 1 then goto L51636
                setdescr$ = "Local Authority"
                main% = 11% : detl% = 12%
L51636:     sethdr$ = setdescr$
            call "FMTTITLE" (sethdr$, "G/L SYSTEM", 12%)
            call "PUTPAREN" (setdescr$)
            return

L52000:     maxmonths = maxmonths%
            call "NUMTEST" (which$, 1, maxmonths, errormsg$, 0.0,        ~
                whichmonth)
            if errormsg$ <> " " then return
            whichmonth% = whichmonth

            if whichmonth% < maxmonths% + 1 then L52110
               convert maxmonths% to maxmonths$, pic(##)
               errormsg$ = "Choice cannot be greater than " & maxmonths$
               return

L52110:        if whichmonth% <= monthopen% + 2 then L52160
               convert monthopen%+2 to periods$, pic(##)
               errormsg$ = "Choice cannot be greater than " & periods$
               return

L52160:     mondescr$ = mdate$(whichmonth%)
            call "PUTPAREN" (mondescr$)
            convert whichmonth to which$, pic (00)
            return

L55000: REM SET PERIOD DATES FOR REPORT
            if ytdrep$ <> "YES" then L55030
            if whichmonth% <= monthopen% + 2 then L55030
            return
L55030:     enddate$, searchdate$ = day1$(whichmonth%)
            if enddate$ = "CLOSING" and ytdrep$ = "YES" then L55240
            if enddate$ = "CLOSING" then startdate$=dates$(periods% + 1)
            if enddate$ = "CLOSING" then L55150
            endprtdate$ = enddate$
            call "DATEFMT" (endprtdate$)
            startdate$ = day1$(whichmonth%+1)
            if ytdrep$ <> "YES" then L55150
            if monthopen% - whichmonth% + 1 <=  periods%                 ~
                    then startdate$ = dates$(periods% + 1)               ~
               else startdate$ = dates$(periods% + 15%)
            if monthopen% - whichmonth% + 1 > periods% then ytdflg% = 3
L55150:     if startdate$ = "CLOSING" then startdate$=dates$(periods% + 1)
            if enddate$ = "CLOSING" then endprtdate$ = "CLOSING"
            if enddate$ = "CLOSING" then L55190
            call "DATE" addr ("G+", startdate$, 1%, startdate$, u3%)
L55190:     begdate$ = startdate$
            call "DATEFMT" (begdate$)
            if u3% <> 0 then L09840
            return

L55240: REM *************************************************************~
            *          P R E V I O U S  Y E A R  R O U T I N E          *~
            *                                                           *~
            * IF 'CLOSING' WAS SPECIFIED AND YTDREP$ = YES THEN THE     *~
            * PREVIOUS YEAR IS PRINTED.  THE ABOVE OPTION SHOULD BE     *~
            * REQUESTED AFTER ALL CLOSING ROUTINES ARE POSTED BUT BEFORE*~
            * PURGING OF THE PREVIOUS YEAR HISTORY FOR A COMPLETE AUDIT *~
            * TRAIL THROUGH CLOSING ROUTINES                            *~
            *************************************************************

                 get #04, using L55350, periods%, monthopen%, dates$()
L55350:                  FMT XX(20), BI(2), XX(136), BI(2), 32*CH(8)

                temp1% = 0
                for temp% = 15% to 1% step-1
                if dates$(temp%) = " " or ~
                   dates$(temp%) = blankdate$ then L55490
                    temp1% = temp1% + 1
                    if temp1% > 15 then L55510
                    day1$(temp1%) = dates$(temp%)
                    if temp% <> 15 then L55460
                       mdate$(temp1%) = dates$(temp%)
                       goto L55490
L55460:             tdate$ = dates$(temp%)
                    call "DATEFMT" ( tdate$, tdate%, udate$ )
                    convert str(udate$,5%,2%) to month%
                    mdate$(temp1%) = str(months$, 3%*month%-2%, 3%) & " " & ~
                    str(udate$,7%,2%) &", " & str(udate$,1%,4%)
L55490:         next temp%

L55510:     enddate$, searchdate$ = dates$(periods% + 1)
            endprtdate$ = enddate$
            call "DATEFMT" (endprtdate$)
            startdate$ = dates$(1)
            call "DATE" addr("G+", startdate$, 1%, startdate$, u3%)
            begdate$ = startdate$
            call "DATEFMT" (begdate$)
            if u3% <> 0 then L09840
            monthopen% = periods%
            whichmonth% = 1
            ytdflg% = 1
            return

        REM *************************************************************~
            *          B A C K G R O U N D   M O D E   S E T U P        *~
            *                                                           *~
            * IF THIS PROGRAM IS RUNNING IN BACKGROUND MODE, THEN THE   *~
            * PROGRAM PRINTS THE TRIAL BALANCE FOR THE ENTIRE G/L.      *~
            *************************************************************

            init(hex(00)) firstaccount$
            init(hex(ff)) lastaccount$
            gosub L12000                  /* PRINT RANGE OF STUFF       */
            goto L65000

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN.                      *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")
            end
