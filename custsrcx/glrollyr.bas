        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   GGG   L      RRRR    OOO   L      L      Y   Y  RRRR    *~
            *  G      L      R   R  O   O  L      L      Y   Y  R   R   *~
            *  G GGG  L      RRRR   O   O  L      L       YYY   RRRR    *~
            *  G   G  L      R   R  O   O  L      L        Y    R   R   *~
            *   GGG   LLLLL  R   R   OOO   LLLLL  LLLLL    Y    R   R   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GLROLLYR - DOES ALL OF THE YEAR PROCESSING NEEDED FOR G/L *~
            *            YEAR END.  WRITES PREVIOUS YEAR DATA TO HISTORY*~
            *            FILE, THEN REPLACES PREVIOUS YEAR WHITH CURRENT*~
            *            YEAR, THEN CLEARS CURRENT YEAR BUCKETS, THEN   *~
            *            PURGES OLD CLOSING ENTRIES (THEY ARE MAINTAINED*~
            *            SHOULD BE RUN BE A PROC, THEN FISCAL YEAR CAN  *~
            *            BE RUN WHEN THIS IS COMPLETE.  THEN THE USER   *~
            *            CAN SEE THE NEW FISCAL DATE STRUCTURE TO VERIFY*~
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
            * 11/18/83 ! ORIGINAL                                 ! HES *~
            * 02/27/85 ! Made It Also Roll Budget File            ! HES *~
            * 05/10/85 ! MODIFIED FOR GLDETAIL RECORD EXPANSION - ! RAC *~
            * 04/10/86 ! Retain user defined next start dates     ! HES *~
            * 08/26/86 ! Change Create on GLHISTRY to size GLMAIN ! RAC *~
            * 09/21/87 ! W.R. Grace accountancy (dual books) mods.! JIM *~
            * 08/10/88 ! Dual Books depends on a SYSFILE2 flag.   ! JIM *~
            * 02/24/93 ! PRR 12790  Option to leave bugets in tact! JDH *~
            * 04/16/93 ! PRR 12817 Fixed Calc of prev yr end dates! JDH *~
            * 06/25/96 ! Changes for the year 2000.               ! DXL *~
            * 04/06/98 ! Y2K and 60403 Changes Merged             ! DJD *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            account$9,                   /* ACCOUNT NUMBER FOR PLOWS   */~
            bal(32),                     /* ACOUNTS BALANCES           */~
            bgtbal(26),                  /* ACOUNTS BUDGETS            */~
            bgtfiller$104,               /* ACOUNTS BUDGETS FOR NEW YER*/~
            blankline$79,                /*                   (get it?)*/~
            description$30,              /* ACCOUNT DESCRIPTION        */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            dates$(17)8,                 /* PERIOD START DATES         */~
            dual_books$1,                /* Dual books in effect?      */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            fmted$(32)8,                 /* PERIOD ENDING DATES+PRIORYR*/~
            hisbal(15),                  /* ACOUNTS BALANCES FOR HISTRY*/~
            inpmessage$79,               /* INPUT MESSAGE              */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            line2$79,                    /* Screen Line 2              */~
            newbal(32),                  /* NEW ACOUNTS BALANCES       */~
            perd$4,                      /* PERIOD FOR HISTORY RECORD  */~
            readkey$26,                  /* READKEY                    */~
            tdate$8,                     /* Temp. Date                 */~
            type$1,                      /* ACCOUNT TYPE               */~
            udate$10,                    /* Unformated date for calcs  */~
            yes$3,                       /* YES/NO PROMT               */~
            zero_ny_budgets$3            /* Option for budgets         */

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
            * #01 ! SYSFILE2 ! Caelus Management System Information     *~
            * #02 ! GLMAIN   ! General Ledger Main File                 *~
            * #03 ! GLDETAIL ! General ledger detail file               *~
            * #04 ! GLHISTRY ! GL HISTORY FILE                          *~
            * #05 ! GLBUDGET ! General Ledger Budget File               *~
            * #12 ! GLMAIN2  ! G. L. chart of accounts for local auth.  *~
            * #13 ! GLDETAL2 ! G. L. detail records for local authority *~
            * #14 ! GLHISTY2 ! GL HISTORY FILE (Local Authority)        *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #01,  "SYSFILE2",                                     ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos =    1, keylen =  20

            select #02,  "GLMAIN",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  300,                                  ~
                        keypos =    1, keylen =   9

            select #03,  "GLDETAIL",                                     ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  160,                                  ~
                        keypos =    1, keylen =  26

            select #04,  "GLHISTRY",                                     ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  190,                                  ~
                        keypos =    1, keylen =  13

            select #05,  "GLBUDGET",                                     ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos =    1, keylen =  9

            select #12, "GLMAIN2",                                       ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   9

            select #13, "GLDETAL2",                                      ~
                        varc,     indexed,  recsize = 160,               ~
                        keypos = 1,    keylen = 26

            select #14,  "GLHISTY2",                                     ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  190,                                  ~
                        keypos =    1, keylen =  13

            call "SHOSTAT" ("Preparing for Year-end G/L processing")

            call "OPENFILE" (#01, "IO   ", f2%(1 ), rslt$(1 ), axd$(1 ))
            call "OPENFILE" (#02, "IO   ", f2%(2 ), rslt$(2 ), axd$(2 ))
            call "OPENFILE" (#03, "IO   ", f2%(3 ), rslt$(3 ), axd$(3 ))
            call "OPENFILE" (#04, "IO   ", f2%(4 ), rslt$(4 ), axd$(4 ))
            call "OPENFILE" (#05, "IO   ", f2%(5 ), rslt$(5 ), axd$(5 ))
            dual_books$ = "N"                        /* Default to 'no' */
            call "READ100" (#01, "SWITCHS.GL", f1%(1))
                if f1%(1) = 0% then goto L02640
            get #01 using L02600, dual_books$
L02600:         FMT POS(21), CH(1)
            if dual_books$ <> "Y" then goto L02640
            call "OPENFILE" (#12, "IO   ", f2%(12), rslt$(12), axd$(12))
            call "OPENFILE" (#13, "IO   ", f2%(13), rslt$(13), axd$(13))
            call "OPENFILE" (#14, "IO   ", f2%(14), rslt$(14), axd$(14))

L02640:     if f2%(1) + f2%(2) + f2%(3) = 0 then L02700
L02650:       call "ASKUSER" (0%, "*** MISSING FILES ***",               ~
                  "SYSFILE2, GLMAIN, GLDETAIL and GLHSTRY are Required "&~
                  "to Continue.", "One or more of these files is missin"&~
                  "g or is in use.", "Press (RETURN) To Exit")
                goto L65000

L02700:     if f2%(4) = 0 then L02760
                get rslt$(2), using L02720, record%
L02720:             FMT XX(16), BI(4)
                call "OPENCHCK" (#04, 0%, f2%(4), record%, rslt$(4))
                if f2%(4) <> 0% then L02650

L02760:     if dual_books$ <> "Y" then goto L09000
            if f2%(14) = 0 then L09000
                get rslt$(12), using L02780, record%
L02780:             FMT XX(16), BI(4)
                call "OPENCHCK" (#14, 0%, f2%(14), record%, rslt$(14))
                if f2%(14) <> 0% then L02650

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (ENTER)."

            call "READ100" (#01, "FISCAL DATES", f1%(1))
                 if f1%(1) = 0 then L65000
            get #01, using L09340, periods%, dates$(), monthopen%,         ~
                                fmted$(), adjacct$
L09340:             FMT XX(20), BI(2), 17*CH(8), BI(2), 32*CH(8), CH(16)
            perd$ = str(fmted$(1%),,2) & str(fmted$(1%+periods%),,2)
            closdate$ = fmted$(1%+periods%)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, yes$, zero_ny_budgets$

            for fieldnr% = 1 to  1
                gosub'051(fieldnr%)
                      if enabled% = 0 then L10180
L10120:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 then L65000
                      if keyhit% <>  0 then L10120
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10120
L10180:         next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

            yes$ = " "
L11060:     gosub L41000
                  if keyhit%  =  1 then inputmode
                  if keyhit%  = 16 then datasave
                  if keyhit% <>  0 then L11060
            gosub'151(2%)

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            if monthopen% >= 14 then L18150

            askkey% = 2%
            call "ASKUSER" (askkey%, "*** CLOSING ERROR ***",            ~
               "According to your FISCAL DATES record, your fiscal year",~
               "is not over yet, so you may NOT run this program.",      ~
               "Press (RETURN) to acknowledge and exit.")
            goto L65000 /*DON'T WANT TO CLOSE IF NOT AT YEAR END!*/

L18150:     gosub clear_closing_journal

        REM First, roll the Statutory (or only) set of G/L accounts
            if dual_books$ =  "Y"                                        ~
            then call "SHOSTAT" ("Rolling Account balances -- Statutory")~
            else call "SHOSTAT" ("Rolling Account balances")
            return% = 99
            init (hex(00)) account$

L18200:     call "PLOWNXT1" (#02, account$, 0%, f1%(2)) /* NEXT ACCOUNT */
                if f1%(2) = 0 then L18440
            get #02, using L18250, description$, type$, seqnr%, bal()
            mat newbal = zer
            prevbal = 0
L18250:         FMT XX(9), CH(30), CH(1), BI(4), 32*PD(14,4)
            for temp% = 1  to 15
                prevbal = prevbal + bal(temp%)
                hisbal(temp%) = bal(temp%)
            next temp%
            newbal(1%) = prevbal
            for temp% = 2  to 14                     /*   MOVE     */
                newbal(temp%) = bal(temp% + 14%)     /*  CURRENT   */
            next temp%                               /*  BUCKETS   */
                                                     /*    TO      */
            for temp% = 16  to 19                    /*  PREVIOUS  */
                newbal(temp%) = bal(temp% + 13%)     /*   YEAR     */
            next temp%                               /*  BUCKETS   */
            rewrite #02, using L18720, account$, description$, type$,     ~
                seqnr%, newbal()   /* RE-SAVE */

	    /* CHANGE 1 - ADDED EOD CLAUSE TO THE CODE */
            write #04, using L18780, account$, perd$, description$, type$,~
                      hisbal(), prevbal, " ", eod goto L18403
            goto L18200

	    /* CHANGE 2 - added the code below here  */
L18403:         str(rhh$,1%,9%)  = account$
                str(rhh$,10%,4%) = perd$
                read #4,hold,key = rhh$, eod goto L18421
                delete #4
              put #04, using L18780, account$, perd$, description$, type$,~
                      hisbal(), prevbal, " "
              write #4
              goto L18200
L18421:             stop " (ERROR) ABORT YEAR CLOSE-OUT - RHH "
                    goto L65000
	    /* END CHANGE 2 */


L18440: REM Next, roll the Local Authority set of G/L books, if in effect
            if dual_books$ <> "Y" then goto L18860
            call "SHOSTAT" ("Rolling Account balances -- Local Authority")
            return% = 99
            init (hex(00)) account$

L18480:     call "PLOWNXT1" (#12, account$, 0%, f1%(12)) /*NEXT ACCOUNT */
                if f1%(12) = 0 then L18860
            get #12, using L18530, description$, type$, seqnr%, bal()
            mat newbal = zer
            prevbal = 0
L18530:         FMT XX(9), CH(30), CH(1), BI(4), 32*PD(14,4)
            for temp% = 1  to 15
                prevbal = prevbal + bal(temp%)
                hisbal(temp%) = bal(temp%)
            next temp%
            newbal(1%) = prevbal
            for temp% = 2  to 14                     /*   MOVE     */
                newbal(temp%) = bal(temp% + 14%)     /*  CURRENT   */
            next temp%                               /*  BUCKETS   */
                                                     /*    TO      */
            for temp% = 16  to 19                    /*  PREVIOUS  */
                newbal(temp%) = bal(temp% + 13%)     /*   YEAR     */
            next temp%                               /*  BUCKETS   */
            rewrite #12, using L18720, account$, description$, type$,     ~
                seqnr%, newbal()   /* RE-SAVE */
            write #14, using L18780, account$, perd$, description$, type$,~
                      hisbal(), prevbal, " " /* WRITE HISTORY */
          goto L18480

L18720:     FMT CH(9),                   /* ACCOUNT NUMBER             */~
                CH(30),                  /* DESCRIPTION                */~
                CH(1),                   /* ACCOUNT TYPE               */~
                BI(4),                   /* SEQUENCE #                 */~
                32*PD(14,4)              /* BALANCES                   */

L18780:     FMT CH(9),                   /* ACCOUNT NUMBER             */~
                CH(4),                   /* PERIOD INDICATOR           */~
                CH(30),                  /* DESCRIPTION                */~
                CH(1),                   /* ACCOUNT TYPE               */~
                15*PD(14,4),             /* BALANCES                   */~
                PD(14,4),                /* ENDING BALANCE             */~
                CH(18)                   /* FILLER                     */

L18860: REM Build New Fiscal Dates Record...
            str(dates$(),,32) =  str(dates$(),105,32)
            for temp% = 5% to 17%
                tdate$ = dates$(temp%)
		call "DATEFMT" (tdate$, 0%, udate$)
                convert str(udate$,1%,4%) to y%, data goto L18910
                convert y% + 1 to str(udate$,1%,4%), pic(####)
                call "DATFMTC" (udate$)
                call "DATUFMTC" (udate$)
		dates$(temp%) = udate$
L18910:     next temp%
            monthopen% = monthopen% - 13%
            gosub set_period_ending_dates
            call "READ101" (#01, "FISCAL DATES", f1%(1))
                if f1%(1) = 0 then L19000
            rewrite #01, using L18980, "FISCAL DATES", periods%, dates$(),~
                monthopen%,  fmted$(), adjacct$, " "
L18980:       FMT CH(20), BI(2), 17*CH(8), BI(2), 32*CH(8), CH(16), CH(68)

L19000: REM Roll Budget Data
            if dual_books$ = "Y"                                         ~
            then call "SHOSTAT" ("Rolling Budgets -- Statutory only")    ~
            else call "SHOSTAT" ("Rolling Budgets")
            init (hex(00)) account$, bgtfiller$
L19030:     call "PLOWNXT1" (#05, account$, 0%, f1%(5%)) /*NEXT ACCOUNT*/
                if f1%(5%) = 0% then L65000
            get #05, using L19060, bgtbal()
L19060:         FMT POS(122), 26*PD(14,4)
            if zero_ny_budgets$ = "YES" then L19070
                put #05, using L19066, bgtbal()
L19066:              FMT POS(18), 26*PD(14,4)
                goto L19090
L19070:     put #05, using L19080, bgtbal(), bgtfiller$
L19080:         FMT POS(18), 26*PD(14,4), CH(104)
L19090:     rewrite #05
            goto L19030

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 0
                  on fieldnr% gosub L20100          /* YES/NO PROMPT    */
                     return
L20100:     REM DEFAULT/ENABLE FOR YES/NO PROMT
                  enabled% = 1
                  if zero_ny_budgets$ = " " then zero_ny_budgets$ = "YES"
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
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.
            k% = 0%
            call "STARTOVR" (k%)
            on k%+1% goto L29928, L29934
            return

L29928:        REM START OVER            (ENTER)
                   return clear
                   goto inputmode
L29934:        REM RETURN TO DISPLAY.    (P.F. KEY 1)
                   return

        REM *************************************************************~
            *  P U R G E   G E N E R A L   L E D G E R   D E T A I L S  *~
            *                                                           *~
            * PURGES OUT OLD CLOSING ENTRIES.  (IF THERE IS ANY CURRENT *~
            * CLOSING ENTRIES, THERE WON'T BE WHEN THIS IS DONE!)       *~
            * WE HAVE TO DO THIS BECAUSE CLOSING ENTRIES ARN'T PURGED   *~
            * ALL YEAR, ONLY HERE.                                      *~
            *************************************************************

        clear_closing_journal
            if dual_books$ = "Y"                                         ~
            then call "SHOSTAT" ("Purging old General Ledger closing de"&~
                "tails -- Statutory")                                    ~
            else call "SHOSTAT" ("Purging old General Ledger closing de"&~
                "tails")
            readkey$ = all(hex(00))

        REM Plow through looking for General Ledger details to kill.  Do ~
            Statutory G/L set of books first.
L30150:     call "PLOWNEXT" (#02, readkey$, 0%, f1%(2))
            if f1%(2) = 0 then goto L30270
                str(readkey$,17) = closdate$
                put str(readkey$,23,4) using L30185, 9e7
L30185:             FMT BI(4)
L30190:         call "PLOWNXT1" (#03, readkey$, 23%, f1%(3))
                if f1%(3) = 0 then L30150
                    get #03, using L30220, mtype$
L30220:                 FMT XX(26), CH(2)
                    if mtype$ <> "99" then L30190 /*ONLY CLOSING ENTRIES*/
                    delete #03
                    goto L30190

L30270:     if dual_books$ <> "Y" then return
            call "SHOSTAT" ("Purging old General Ledger closing details"&~
                " -- Local Authority")
            readkey$ = all(hex(00))

        REM Do Local Authority G/L set of books next.
L30330:     call "PLOWNEXT" (#12, readkey$, 0%, f1%(12))
            if f1%(12) = 0 then return
                str(readkey$,17) = closdate$
                put str(readkey$,23,4) using L30370, 9e7
L30370:             FMT BI(4)
L30380:         call "PLOWNXT1" (#13, readkey$, 23%, f1%(13))
                if f1%(13) = 0 then L30330
                    get #13, using L30410, mtype$
L30410:                 FMT XX(26), CH(2)
                    if mtype$ <> "99" then L30380 /*ONLY CLOSING ENTRIES*/
                    delete #13
                    goto L30380

        REM *************************************************************~
            *       S E T   P E R I O D   E N D I N G    D A T E S      *~
            * CREATES AN ARRAY CONTAINING PERIOD ENDING DATES RATHER    *~
            * THEN STARTING DATES. DATES START WITH WHAT WOULD BE THE   *~
            * THE PREVIOUS YEAR.  ELEMENT 16 IS THE ENDING DATE OF THE  *~
            * FIRSTPERIOD IN THE CURRENT YEAR... YEAR DATE CORROSPONDS  *~
            * DIRECTLY WITH A BUCKET IN THE GL MAIN RECORD.             *~
            *************************************************************

        set_period_ending_dates
*        1st bring current year period end dates forward to previous year
            fmted$(1%) = fmted$(14%)
            for u3% = 2% to 14%
                fmted$(u3%) = fmted$(u3% + 14%)
            next u3%

*        Bucket 15 is always CLOSING
            fmted$(15%) = "CLOSING"

*        Now calculate period ending dates for the new current year
            str(fmted$(),15% * 8% + 1%) = str(dates$(),9%)
            fmted$(32%) = fmted$(19%)
            tdate$ = fmted$(32%)
            call "DATEFMT" ( tdate$, 0%, udate$ )
            convert str(udate$,1%,4%) to year%
            convert year% + 1% to str(udate$,1%,4%), pic(####)
            call "DATECONV" ( udate$ )
            fmted$(32%) = udate$

            for u3% = 16% to 32%
                if u3% = 15% then L32310
                if u3% = 13% and periods% = 12% then L32310
                if u3% = 27% and periods% = 12% then L32310
                     call "DATE" addr ("G+", str(fmted$(u3%),,6%), -1%,  ~
                                             str(fmted$(u3%),,6%), err%)
                     if err% <> 0% then L32391
L32310:     next u3%

            if periods% = 13% then L32370
                fmted$(27%) = fmted$(28%)
                fmted$(28) = " "
L32370:
        return

L32391:     u3% = 0%
            call "ASKUSER" (u3%, "*** FISCAL CALENDAR ERROR ***",        ~
                "Be sure to review and resave new calender before any G"&~
                "/L postings.", " ", "Press (RETURN) to acknowledge")
            return

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L40140          /* YES/NO PROMPT    */
                     goto L40202

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40140:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40202:     line2$ = " "
            str(line2$,62,10) = "GLROLLYR:"
            str(line2$,72, 8) = str(cms2v$,1,8)

L40210: accept                                                           ~
               at (01,02), "G/L Year-End Closing",                       ~
               at (01,59), "Today's Date:",                              ~
               at (01,73), fac(hex(8c)), date$,                          ~
               at (02,02), fac(hex(ac)), line2$,                         ~
               at (04,27),                                               ~
        "**PLEASE READ THE FOLLOWING**",                                 ~
               at (06,05),                                               ~
        "This program will roll all of your current year amounts into the~
        ~ previous",                                                      ~
               at (07,06),                                               ~
        "year buckets, and move all of your previous year amounts into a ~
        ~history",                                                        ~
               at (08,06),                                               ~
        "file.  Once this process is complete, posting to periods in the ~
        ~closed",                                                         ~
               at (09,11),                                               ~
        "year will NOT be possible, except by manual entries using the", ~
               at (10,17),                                               ~
        "Direct Journal Entry program (Management Version).",            ~
               at (12,07),                                               ~
        "It is recommended a year not be closed until ALL POSTING to that~
        ~ year",                                                          ~
               at (13,11),                                               ~
        "has been completed.  This includes month/year end adjustments", ~
               at (14,13),                                               ~
        "and back-dated postings. You have a four period buffer in",     ~
               at (15,16),                                               ~
        "which to complete these, ie., the year doesn't have",           ~
               at (16,15),                                               ~
        "to be closed until just prior to the beginning of the",         ~
               at (17,11),                                               ~
        "fifth period of the new year (at that point it is mandatory).", ~
               at (18,07),                                               ~
        "*Be sure NOT to make closing entries until AFTER the year is clo~
        ~sed.*",                                                          ~
               at (19,30),                                               ~
        "Did you make a backup??",                                       ~
               at (21,23), "Is it ok to close the fiscal year?",         ~
               at (21,61), fac(lfac$(1)), yes$,                    ch(3),~
               at (22,20), "Do you want to zero next year's budgets?",   ~
               at (22,61), fac(lfac$(1)), zero_ny_budgets$,        ch(3),~
               at (23,02), fac(hex(a4)), blankline$,              ch(79),~
               at (24,03),                                               ~
        "(13)Instructions              (15)Print Screen              (16)~
        ~EXIT PROGRAM",                                                   ~
               keys(hex(000d0f10)), key (keyhit%)

               if keyhit% <> 13 then L40710
                  call "MANUAL" ("GLROLLYR")
                  goto L40210

L40710:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40210

L41000: REM *************************************************************~
            *            S A F E G U A R D   S C R E E N   1            *~
            *                                                           *~
            * SAFEGUARDS THE ACCIDENTAL PURGING OF DATA BY ASKING IF HE *~
            * REALLY WANTS TO PURGE IT.                                 *~
            *************************************************************

            str(line2$,1,15) = "Enter SAFEGUARD"
            str(line2$,62,10) = "GLROLLYR:"
            str(line2$,72, 8) = str(cms2v$,1,8)

L41078: accept                                                           ~
               at (01,02), "G/L Year-End Closing",                       ~
               at (01,59), "Today's Date:",                              ~
               at (01,73), fac(hex(8c)), date$,                          ~
               at (02,02), fac(hex(ac)), line2$,                         ~
               at (08,23), "***************************************",    ~
               at (09,23), "* YEAR END CLOSING IS ABOUT TO BEGIN. *",    ~
               at (10,23), "*       DO YOU WISH TO PROCEED?       *",    ~
               at (11,23), "*                                     *",    ~
               at (12,23), "*                                     *",    ~
               at (13,23), "*                                     *",    ~
               at (14,23), "*     RESPONSES OTHER THAN 'YES'      *",    ~
               at (15,23), "*  WILL RETURN YOU TO PREVIOUS SCREEN *",    ~
               at (16,23), "***************************************",    ~
                                                                         ~
               at (12,40), fac(hex(81)), yes$                   , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)), blankline$             , ch(79),~
                                                                         ~
               at (22,02), "(1)Previous Screen",                         ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
                                                                         ~
               keys(hex(00010d0f)), key (keyhit%)

               if keyhit% <> 13 then L41400
                  call "MANUAL" ("GLROLL")
                  goto L41078

L41400:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L41078

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50110,         /* YES/NO PROMPT    */~
                                    L50140          /* YES/NO PROMPT    */
                     return
L50110:     REM TEST DATA FOR YES/NO PROMT
            if yes$ <> "YES" then L65000
            if zero_ny_budgets$ <> "YES" then zero_ny_budgets$ = "NO "
                return
L50140:     REM TEST DATA FOR YES/NO PROMT SCREEN TWO
            if yes$ = "YES" then return
                return clear
                goto inputmode

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
            end return%
