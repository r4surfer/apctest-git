        REM *************************************************************~
            *                                                           *~
            *   GGG   L      TTTTT  RRRR   IIIII   AAA   L              *~
            *  G      L        T    R   R    I    A   A  L              *~
            *  G GGG  L        T    RRRR     I    AAAAA  L              *~
            *  G   G  L        T    R   R    I    A   A  L              *~
            *   GGG   LLLLL    T    R   R  IIIII  A   A  LLLLL          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GLTRIAL  - PRINTS TRIAL BALANCE FOR A RANGE OF ACCTS, FOR *~
            *            ANY OF THE FIFTEEN MONTHS ON FILE.             *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/16/80 ! ORIGINAL                                 ! BCW *~
            * 03/17/81 ! TRIAL BALANCE FOR ALL 15 MONTHS ON FILE  ! TEM *~
            * 07/31/81 ! MONTH END CLOSING GETPARM                ! TEM *~
            * 10/04/82 ! REDEFINE GLMAIN TO INCL FWD & 13TH MONTHS! ECR *~
            * 11/15/83 ! REVISED TO CONFORM TO NEW GLMAIN LAYOUT  ! HES *~
            * 05/10/85 ! MODIFIED FOR GLDETAIL RECORD EXPANSION - ! RAC *~
            *          ! CHANGES INCLUDED GLPOST, HNYPOST, JNLINFO!     *~
            *          !(NEW),GLPRTSUB(NEW), INCREASE IN GL BUFFER!     *~
            *          ! INTERFACE FILE RECORD SIZE FOR PASSING   !     *~
            *          ! MODULE, JOURNAL, AND POSTING SEQUENCE    !     *~
            * 03/04/86 ! Change for unformatted Fiscal Dates      ! ERN *~
            * 08/20/87 ! W.R. Grace accountancy (dual books) mods.! JIM *~
            * 08/12/88 ! Dual Books depends on GETPARMs LAUTHY$...! JIM *~
            *          !   ... as well as the flag in SYSFILE2.   !     *~
            * 10/26/88 ! Changed 'month' to 'period'.             ! JDH *~
            * 05/17/89 ! Added re-print of acct & descr on page br! MLJ *~
            * 11/07/89 ! Removed dual books line from heading for ! MJB *~
            *          !  systems where dual books not active.    !     *~
            * 04/06/92 ! PRR 11429 - Fixed array overflow on 16th ! MLJ *~
            *          !   and 17th period (#9272 - 9274)         !     *~
            *          ! PRR 12006 - Chg'd GETPARM (#9570) to type!     *~
            *          !   'ID' obsoleteing PROC3D3.  PROC3D3L is !     *~
            *          !   still used for Local Authority = YES.  !     *~
            * 07/18/94 ! PRR 13162 - Added ASKUSER as to whether  ! MLJ *~
            *          !   module details should be printed when  !     *~
            *          !   period is outside the currrent window. !     *~
            *          ! PRR 13165 - Added TESTRNGE with channel  !     *~
            *          !   number to G/L acct edit for entry of   !     *~
            *          !   partial account numbers.               !     *~
            * 06/25/96 ! Changes for the year 2000.               ! DXL *~
            * 12/07/00 ! Mod so that Kathy P. can choose detail or! CMG *~
            *          !   summary for any month (EWD001)         !     *~
            *************************************************************

        dim                                                              ~
            aid$1,                       /* AID CHARACTER IN GETPARM   */~
            bal(32),                     /* BALANCES FROM G/L MAIN FILE*/~
            blankdate$8,                 /* Blank date for comparison  */~
            coname$60,                   /* Company name               */~
            date$8,                      /* FORMATTED G/L DATE         */~
            dates$(32)8,                 /* FISCAL DATES ARRAY         */~
            day1$(15)8,                  /* FIRST OF RELEVENAT MONTH   */~
            detaildate$6,                /* DATE FROM DETAIL FOR COMP  */~
            dual_books$1,                /* Dual books in effect?      */~
            enddate$8,                   /* END DATE RANGE             */~
            errormsg$79,                 /* ERROR MESSAGE FOR DISPLAY  */~
            firstaccount$16,             /* FIRST ACCOUNT OF RANGE     */~
            firstacct$9,                 /* FIRST ACCOUNT OF RANGE     */~
            fromaccount$16,              /* FOR TESTRNGE               */~
            lastaccount$16,              /* LAST ACCOUNT NUMBER        */~
            lastacct$9,                  /* LAST ACCOUNT NUMBER        */~
            lauthy$3,                    /* Local Authority books ?    */~
            line2$79,                    /* Line 2 of displays         */~
            line21$79,                   /* LINE UNDERLINED IN SCREEN  */~
            look%(1),                    /* CATCH ARRAY  FOR SEARCH    */~
            months$36,                   /* MONTHS LIST AVAILABLE      */~
            monthe$3,                    /* MONTH END?                 */~
            mdate$(15)12,                /* DATES OF AVAILABLE MONTHS  */~
            oldreadkey$50,               /* OLD KEY FOR PLOW ROUTINES  */~
            prtaccount$16,               /* PRINT ACCOUNT NUMBER       */~
            prtacctdescr$30,             /* ACCOUNT DESCRIPTION        */~
            prtbal$(4)15,                /* BALANCES TO PRINT          */~
            prtfield$30,                 /* A VARIABLE                 */~
            rnghdr$60,                   /* Range of accounts header   */~
            rptid$6,                     /* Report ID                  */~
            searchdate$8,                /* DATE FOR SEARCHES          */~
            setdescr$30, sethdr$60,      /* Descr of set of books      */~
            startdate$8,                 /* START DATE RANGE           */~
            subhdr$60,                   /* Sub header                 */~
            tasktype$1,                  /* FOREGROUND OR BACKGROUND   */~
            tdate$8,                     /* Temp. Date                 */~
            time$8,                      /* Time of day                */~
            total$30,                    /* Literal 'TOTAL'            */~
            toaccount$16,                /* FOR TESTRNGE               */~
            transtypes$50,               /* TRANSACTION TYPES          */~
            transhead$(25)30,            /* TRANSACTION HEADINGS       */~
            trans(25,2),                 /* TRANSACTION DEBIT AND CREDS*/~
            transtotal(25,2),            /* TOTAL TRANS DEBITS/CREDITS */~
            type$2,                      /* TRANSACTION TYPE FOR DETAIL*/~
            udate$8,                     /* Unformatted date for calcs */~
            userid$3,                    /* USER ID CURRENT USER       */~
            which$2                      /* WHICH MONTH THIS TRIAL     */~

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64)                      /* RECORD-ON-FILE FLAGS       */

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

            select #02, "GLDETAIL",      /* GENERAL LEDGER DETAILS     */~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 160,                                   ~
                        keypos = 1, keylen = 26

            select #04, "SYSFILE2",                                      ~
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

            call "OPENCHCK" (#01, 0%, f2%( 1), 100%, " ")
            call "OPENCHCK" (#02, 0%, f2%( 2), 100%, " ")
            call "OPENCHCK" (#04, 0%, f2%( 4), 100%, " ")
            dual_books$ = "N"                        /* Default to 'no' */
            call "READ100" (#04, "SWITCHS.GL", f1%(4))
                if f1%(4) = 0% then goto L09000
            get #04 using L02472, dual_books$
L02472:         FMT POS(21), CH(1)

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES CONTROL VARIABLES, SETS AVAILABLE DATES, ETC. *~
            *************************************************************

            call "EXTRACT" addr ("ID", userid$, "TT", tasktype$)
            date$ = date : call "DATEFMT" (date$)
            call "COMPNAME" (12%, coname$, u3%)
            rptid$ = "G/L006"
            total$ = "* TOTAL" : call "STRING" addr ("RJ", total$, 30%)

            REM FORMAT DATES FOR HISTORY FORMATTING
            months$ = "JanFebMarAprMayJunJulAugSepOctNovDec"
            init(" ") day1$()
                call "READ100" (#04, "FISCAL DATES", f1%(4))
                     if f1%(4) <> 0 then L09240
                         if tasktype$ = "B" then goto L65000
                         u3% = 2%
                         call "ASKUSER" (u3%,"*** SYSTEM DATE ERROR ***",~
                         "System dates available are in error", " ",     ~
                         "Press (RETURN) to acknowledge and exit")
                         goto L65000

L09240:          get #04, using L09250, periods%, monthopen%, dates$()
L09250:                  FMT XX(20), BI(2), XX(136), BI(2), 32*CH(8)

                p% = 16 : if monthopen% = 12 and periods% = 12 then p%=17
                init(" ") mdate$()
                if monthopen% >15 then goto L09290
                str(dates$(), (monthopen%+p%)*8+1), mdate$() = " "
L09290:         temp1% = 0
                blankdate$ = " "
		call "DATUFMTC" (blankdate$)
                for temp% = 32% to 1% step -1%
                if dates$(temp%) = " " or dates$(temp%) = blankdate$ then L09410
                    temp1% = temp1% + 1
                    if temp1% > 15 then L09430
                    day1$(temp1%) = dates$(temp%)
                    if temp% <> 15 then L09380
                       mdate$(temp1%) = dates$(temp%)
                       goto L09410
L09380:             tdate$ = dates$(temp%)
		    call "DATEFMT" (tdate$, 0%, udate$)
                    convert str(udate$,5%,2%) to month%
                    mdate$(temp1%) = str(months$, 3%*month%-2%, 3%) & " " & ~
                    str(udate$,7%,2%) & ", "& str(udate$,1%,4%)
L09410:         next temp%

L09430:     REM SET TRANSACTION TYPES
            i% = 0
            init(hex(ff)) transtypes$
            oldreadkey$ = "MODULENO:0000"
L09470:     call "PLOWNEXT" (#04, oldreadkey$, 11%, f1%(4))
                if f1%(4) = 0 then L09550
            i% = i% + 1
            call "DESCRIBE" (#04, oldreadkey$, transhead$(i%), 0%, f1%(4))
            call "STRING" addr ("RJ", transhead$(i%), 30%)
            str(transtypes$, 2*i%-1, 2) = str(oldreadkey$,12,2)
            goto L09470

L09550: REM GETPARM to find if we are at month end. Also find out which  ~
            set of books to use -- Statutory or Local Authority.
                monthe$ = "NO" : lauthy$ = "NO" : aid$ = hex(40)
            call "GETPARM" addr ("ID", "R", "MONTHEND", aid$, "0001",    ~
                "GLTRAL", 1%, "^Specifications for Trial Balance", 33%,  ~
                "K", "MONTHEND", monthe$, 3%, "A",  9%, "A", 32%, "A",   ~
                "K", "LOCLAUTH", lauthy$, 3%, "A", 11%, "A", 32%, "A")

            if dual_books$ = "Y" then ret% = 1%
            if lauthy$ = "YES" and dual_books$ <> "Y" then goto L65000
            setdescr$ = "Statutory" : set = 1 /* Set default values */
            main% = 1% : detl% = 2% /* Default files- GLMAIN & GLDETAIL */
            if lauthy$ <> "YES" then goto finish_headers
                call "OPENCHCK" (#11, 0%, f2%(11), 100%, " ")
                call "OPENCHCK" (#12, 0%, f2%(12), 100%, " ")
                main% = 11% : detl% = 12% /* GLMAIN2 & GLDETAL2 */
                setdescr$ = "Local Authority" : set = 2
        finish_headers
            sethdr$ = setdescr$ & " G/L System"
            if dual_books$ = "Y" then line2$ = sethdr$
            call "PUTPAREN" (setdescr$)
            call "FMTTITLE" (sethdr$, " ", 12%)

            if monthe$ <> "YES" then goto L10000
            whichmonth% = 2
            firstaccount$ = "ALL"
            lastaccount$ = " "
            init(hex(00)) firstacct$
            init(hex(ff)) lastacct$
            enddate$, searchdate$ = day1$(whichmonth%)
            startdate$ = day1$(whichmonth%+1)
            if startdate$ = "CLOSING" then startdate$=dates$(periods% + 1)
            if enddate$ <> "CLOSING" then L09900
                startdate$ = day1$(15)
                goto L09920
L09900:     call "DATE" addr ("G+", startdate$, 1%, startdate$, u3%)
            if u3% <> 0 then goto date_error_exit
L09920:     bigtotaldebits, bigtotalcredits, bigbalance = 0
            mat transtotal = zer
            pagenumber% = 0
            pageline% = 1000
                gosub L12000
                close printer
                go to L65000

L10000: REM *************************************************************~
            *         I N P U T   W H I C H   M O N T H                 *~
            *                                                           *~
            * CONTROLING SECTION FOR WHICH MONTH TO PRINT               *~
            *************************************************************

            REM GET MONTH FOR WHICH TO PRINT TRIAL BALANCE
                init(" ") which$, errormsg$
L10080:         gosub L41000
                      if keyhit%  =   1 then gosub startover
                      if keyhit%  =  16 then       L65000
                      if keyhit% <>   0 then       L10080
                gosub L51000
                      if errormsg$ <> " " then L10080

        REM *************************************************************~
            *         G E T   A C C O U N T S   T O   P R I N T         *~
            *                                                           *~
            * PRINT ACCOUNT NUMBERS FOR TRIAL BALANCE, BEEING CAREFUL TO*~
            * TOTAL UP THE STUFF THAT WE WILL NEED.  THEN EXIT PROGRAM. *~
            *************************************************************

            init(" ") firstaccount$, lastaccount$, errormsg$
            bigtotaldebits, bigtotalcredits, bigbalance = 0
            mat transtotal = zer
            firstaccount$ = "ALL"
            pagenumber% = 0
            pageline% = 1000

L11140:     gosub L40000
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       L65000
                  if keyhit% <>  0 then       L11140
            gosub L50000
                  if errormsg$ <> " " then L11140
            gosub L12000                  /* PRINT ACCOUNTS             */
            close printer
            goto L10000                   /* AND TRY ANOTHER ONE.       */

L12000: REM *************************************************************~
            *   P L O W   T H R O U G H   F I L E   A N D   P R I N T   *~
            *                                                           *~
            * PLOWS THROUGH FILE OF ACCOUNTS AND THEN PLOW THRU DETAILS *~
            * TO REGURGITATE TOTALS BELOW.                              *~
            * NOTE THAT WE DON'T INCLUDE STUFF IN THE TOTAL THAT IS     *~
            * MODULE DATED BEFORE THE FIRST OF THE CURRENT MONTH OR     *~
            * AFTER THE CURRENT G/L DATE.  THIS IS SO THAT WE CAN GET A *~
            * TRIAL BALANCE FOR APRIL IF THE CURRENT MONTH OPEN IS JUNE.*~
            *************************************************************

            oldacctkey$ = firstacct$
            count%, det% = 0%
/* (EWD001) Mod so can choose detail or summary for any period */
REM            if whichmonth% = 4% and enddate$ = "CLOSING" then L12132
REM                if whichmonth% <= 3% then L12132
L12118:     keyhit% = 2%
            call "ASKUSER" (keyhit%, "*** OUTSIDE CURRENT WINDOW ***",   ~
                 "Period " & which$ & " is outside the current window.", ~
                 "Press PF16 to include module detail in the report"    &~
                 " -OR- ", "press RETURN to omit detail from the report.")
            if keyhit% = 0% then L12136
                if keyhit% <> 16% then L12118
L12132:     det% = 1%                         /* Include Module Details */

L12136:     call "SHOSTAT" ("Processing Trial Balance For Period Ending "~
                & mdate$(whichmonth%) )

L12150: REM PLOW ROUTINE FOR ACCOUNTS.
            search dates$() = searchdate$ to look%() step 8
                if look%(1) <> 0 then L12220
                    if tasktype$ = "B" then goto L65000
                    u3% = 2%
                    call "ASKUSER" (u3%,"*** FISCAL DATE ERROR ***",     ~
                    "Error in Fiscal date structure", " ",               ~
                    "Press (RETURN) to acknowledge and exit")
                     goto L65000

L12220:     bucketpointer% = (look%(1)+7)/8
            totalcredits, totaldebits = 0
            mat trans = zer
            call "READ102" (#main%, oldacctkey$, f1%(main%))
                if f1%(main%) = 0 then L12400
            get #main%, using L12280, newacctkey$
L12280:         FMT CH(9)
            if newacctkey$ > lastacct$ then L12400
            count% = count% + 1%
            oldacctkey$ = newacctkey$
            if det% = 1% then L12330 else L12370

L12330:     gosub'47(oldacctkey$)              /* SUM UP DETAILS   */
            bigtotaldebits  = bigtotaldebits  + totaldebits
            bigtotalcredits = bigtotalcredits + totalcredits
            mat transtotal = transtotal + trans
L12370:     gosub L14000
            goto L12150

L12400: REM WRAP UP PLOW ROUTINE HERE IF END OF FILE FOUND.
        REM PRINT TOTAL LINE FOR THE WHOLE MESS...
            if pagenumber% = 0 then return
            bigtotaldebits  = round(bigtotaldebits, 2%)
            bigtotalcredits = round(bigtotalcredits, 2%)
            bigbalance      = round(bigbalance, 2%)
            gosub L15000
            print using L60160, " ", " ", total$, bigtotaldebits,         ~
                bigtotalcredits, bigbalance
            gosub L15000 : print
            if det% = 1% then L12530 else L12790

L12530: REM NOW FOR MODULE RECAP
            pageline% = 1000
            bigtotaldebits, bigtotalcredits = 0
            for temp% = 1 to 25
                transtotal(temp%, 1) = round(transtotal(temp%, 1), 2%)
                transtotal(temp%, 2) = round(transtotal(temp%, 2), 2%)
                    if abs(transtotal(temp%,1)) < .001 and               ~
                        abs(transtotal(temp%,2)) < .001 then L12670
                    gosub L15000
                    print using L60160, " ", " ", transhead$(temp%),      ~
                        transtotal(temp%,1), transtotal(temp%,2), " "
                   bigtotaldebits = bigtotaldebits + transtotal(temp%, 1)
                   bigtotalcredits = bigtotalcredits + transtotal(temp%,2)
L12670:         next temp%
                if pageline% = 1000 then L12790
                gosub L15000
                bigtotaldebits = round(bigtotaldebits, 2%)
                bigtotalcredits= round(bigtotalcredits, 2%)
                print using L60160, " ", " ", total$, bigtotaldebits,     ~
                    bigtotalcredits, " "

L12790: REM CLOSE PRINTER AND SEND PRINT TO QUEUE
            print : print using L60180, count%
            close printer
            call "SETPRNT" (rptid$, " ", 0%, 1%)
            return

        REM *************************************************************~
            *  T O T A L   U P   D E T A I L S   F O R   A N   A C C T  *~
            *                                                           *~
            * TOTAL ACCOUNT DETAILS FOR THE ACCOUNT SPECIFIED IN THE    *~
            * ARGUMENT FOR THE ROUTINE.  NOTE THAT THE TOTALS ARE       *~
            * RETURNED IN "TOTALDEBITS" AND "TOTALCREDITS".             *~
            * ALSO NOTE THAT THIS SELECTS ONLY THOSE RECS IN THIS PERIOD*~
            *************************************************************

            deffn'47(account$)
                  totalcredits, totaldebits = 0
                  mat trans = zer
                  init (hex(00)) oldreadkey$
                  str(oldreadkey$,1,16)= str(account$,1,16)
                  str(oldreadkey$,17,6)= str(startdate$,1,6)
L13150:           call "PLOWNEXT" (#detl%, oldreadkey$, 9%, f1%(detl%))
                       if f1%(detl%) = 0 then return
                  get #detl% using L13320, detaildate$,type$,debit,credit
                  if enddate$ = "CLOSING" and type$ <> "99" then L13150
                  REM TEST TO MAKE SURE DETAIL DATE IS IN REPORT PERIOD
                      if detaildate$ > enddate$ then return
                  REM ADD DEBIT & CREDIT AMOUNTS TO ACCUMULATORS.
                      totaldebits  = totaldebits  + debit
                      totalcredits = totalcredits + credit
                         search transtypes$ = type$ to look%() step 2
                         if look%(1) = 0 then L13150
                            type = (look%(1) + 1)/2
                            trans(type, 1) = trans(type, 1) + debit
                            trans(type, 2) = trans(type, 2) + credit
                  goto L13150

L13320:         FMT XX(16),              /* ACCOUNT NUMBER             */~
                    CH(6),               /* MODULE DATE POSTED         */~
                    XX(4),               /* SEQUENCE NUMBER STRING     */~
                    CH(2),               /* JOURNAL TYPE               */~
                    2*PD(14,4),          /* DEBIT, CREDIT AMOUNTS.     */~
                    XX(100),             /* ONE LONG STRING OF TEXT    */~
                    XX(3),               /* JOURNAL ID                 */~
                    XX(4),               /* SEQUENCE POSTING NUMBER    */~
                    XX(3),               /* OPERATOR ID OF CURRENT USER*/~
                    XX(6)                /* SYSTEM DATE WHEN WRITTEN.  */~

L14000: REM *************************************************************~
            * A C T U A L L Y   P R I N T   A C C O U N T   T O T A L S *~
            *                                                           *~
            * LET'S SEE IF WE CAN PRINT ACCOUNT DESCRIPTION, PRIOR BAL, *~
            * DEBITS, CREDITS, AND BALANCE TO DATE THIS MONTH WITHOUT   *~
            * THROWING UP OR WORSE.  NOTE THAT THE COMPUTER ASSUMES THAT*~
            * THE BALANCE AT THE BEGINNING OF THE MONTH WAS CORRECT.    *~
            * IF IT TURNS OUT TO BE WRONG (IE THE G/L DOES NOT BALANCE),*~
            * THEN YOU USE THE G/LSTOMP PROGRAM TO ADJUST THE BALANCE   *~
            * DIRECTLY.                                                 *~
            *************************************************************

            init(" ") prtbal$()
            prtaccount$ = oldacctkey$
            call "READ100" (#main%, prtaccount$, f1%(main%))
                 if f1%(main%) = 0 then return
            get #main%, using L14180, prtacctdescr$, bal()

L14180:             FMT XX(9),           /* SKIP ACCOUNT NUMBER        */~
                        CH(30),          /* DESCRIPTION                */~
                        XX(1),           /* ACCOUNT TYPE               */~
                        XX(4),           /* DETAIL SEQUENCE NUMBER     */~
                        32*PD(14,4)      /* BALANCE STUFF.             */~

            priorbal = 0
            for i% = 1 to bucketpointer% - 1
            priorbal = priorbal + bal(i%)
            next i%
            currentbal = bal(bucketpointer%)
            if det% = 1% then L14310 else L14680

L14310: REM FOR OPEN MONTHS ONLY (OR INCLUDE MODULE DETAIL)...
            currentbal = priorbal + totaldebits - totalcredits
            bigbalance = bigbalance + currentbal
            priorbal = round(priorbal, 2%)
            gosub L15000
            if lauthy$ <> "YES"                                          ~
                then call "GLFMT" (prtaccount$)                          ~
                else call "GLFMT2" (prtaccount$)
            convert priorbal to prtfield$, pic (-###,###,###.##)
            call "STRING" addr ("RJ", prtfield$, 30%)
            print using L60160, prtaccount$, prtacctdescr$, prtfield$,    ~
                " ", " ", " "
            for temp% = 1 to 10
                trans(temp%,1) = round(trans(temp%,1), 2%)
                trans(temp%,2) = round(trans(temp%,2), 2%)
                if abs(trans(temp%,1))+abs(trans(temp%,2)) = 0 then L14520
                gosub L15000
                if pageline% = 9 then                                    ~
                    print using L60160, prtaccount$, prtacctdescr$,       ~
                      transhead$(temp%), trans(temp%,1), trans(temp%,2)  ~
                else                                                     ~
                    print using L60160, " ", " ", transhead$(temp%),      ~
                      trans(temp%,1), trans(temp%,2)
L14520:     next temp%
            totaldebits = round(totaldebits, 2%)
            totalcredits= round(totalcredits, 2%)
            currentbal  = round(currentbal, 2%)
            gosub L15000
            if pageline% = 9 then                                        ~
                print using L60160, prtaccount$, prtacctdescr$, total$,   ~
                  totaldebits, totalcredits, currentbal                  ~
            else                                                         ~
                print using L60160, " ", " ", total$, totaldebits,        ~
                  totalcredits, currentbal
            gosub L15000 : print
            return

L14680: REM FOR NON OPEN MONTHS ONLY (EXCLUDE MODULE DETAIL)...
            priorbal = round (priorbal, 2%)
            currentbal = round (currentbal, 2%)
            bigbalance = bigbalance + currentbal + priorbal
            if lauthy$ <> "YES"                                          ~
                then call "GLFMT" (prtaccount$)                          ~
                else call "GLFMT2" (prtaccount$)
            convert priorbal to prtfield$, pic (-###,###,###.##)
            call "STRING" addr ("RJ", prtfield$, 30%)
            gosub L15000
            print using L60160, prtaccount$, prtacctdescr$, prtfield$,    ~
                " ", " ", priorbal + currentbal
            return

L15000: REM *************************************************************~
            *        P A G E   C O N T R O L   R O U T I N E            *~
            *                                                           *~
            * CONTROLS THE PAGING                                       *~
            *************************************************************

            select printer (134)
            if pagenumber% <> 0% then goto L15130
                time$ = " " : call "TIME" (time$)
                call "SETPRNT" (rptid$, " ", 0%, 0%)
                subhdr$ = " "
                subhdr$ = "TRIAL BALANCE FOR PERIOD ENDING"
                call "FMTTITLE" (subhdr$, mdate$(whichmonth%), 2%)
                rnghdr$ = "RANGE:"
                if firstaccount$ <> "ALL" then goto L15124
                    rnghdr$ = rnghdr$ & " ALL" : goto L15127
L15124:         rnghdr$ = rnghdr$ & " " & firstaccount$
                if lastaccount$ <> " " then rnghdr$ = rnghdr$ & " TO " & ~
                    lastaccount$
L15127:         call "FMTTITLE" (rnghdr$, " ", 2%)
L15130:     pageline% = pageline% + 1
            if pageline% < 55 then return
            print page
            pagenumber% = pagenumber% + 1
            print using L60040, date$, coname$, "-" & rptid$
            print using L60060, time$, subhdr$, pagenumber%
            if dual_books$ = "Y" then print using L60080, sethdr$
            print using L60080, rnghdr$
            print
            print using L60091
            print using L60100, "#"
            print using L60120
            print
            pageline% = 9
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

L40000: REM *************************************************************~
            *       I N P U T   R A N G E   O F   A C C O U N T S       *~
            *                                                           *~
            * INPUT RANGE OF ACCOUNTS TO PRINT TRIAL BALANCE FOR.       *~
            *************************************************************

            line21$ = "Enter first and last Account Numbers in desired "&~
                "range or 'ALL'"
            str(line2$,62%) = " GLTRIAL: " & str(cms2v$,,8%)
L40100:     accept                                                       ~
               at (01,02),                                               ~
                  "Print G/L Trial Balance for period ending",           ~
               at (01,44), fac(hex(8c)), mdate$(whichmonth%)    , ch(12),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), "Beginning Acct #",                           ~
               at (06,30), fac(hex(81)), firstaccount$          , ch(12),~
               at (07,02), "Ending Acct #",                              ~
               at (07,30), fac(hex(81)), lastaccount$           , ch(12),~
               at (21,02), fac(hex(a4)), line21$                , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)EXIT PROGRAM",                           ~
                                                                         ~
               keys(hex(00010d0f10)), key (keyhit%)

               if keyhit% <> 13 then L40340
                  call "MANUAL" ("GLTRIAL ")
                  goto L40100

L40340:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40100

L41000: REM *************************************************************~
            *   I N P U T   W H I C H   M O N T H                       *~
            *                                                           *~
            * INDICATE WHICH MONTH YOU DESIRE TRIAL BALANCE FOR         *~
            *************************************************************

            line21$ = "NOTE: The dates shown above are period ENDING da"&~
                "tes."
            str(line2$,62%) = " GLTRIAL: " & str(cms2v$,,8%)
L41080:     accept                                                       ~
               at (01,02),                                               ~
                  "Choose period for Trial Balance",                     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02), "Enter number of desired period (1 - 14):",   ~
               at (05,43), fac(hex(82)), which$                 , ch(02),~
               at (08,05), "These periods will show posting",            ~
               at (09,05), "detail by functional area:",                 ~
               at (09,34), "01",                                         ~
               at (09,37), fac(hex(8c)), mdate$(1)              , ch(12),~
               at (10,34), "02",                                         ~
               at (10,37), fac(hex(8c)), mdate$(2)              , ch(12),~
               at (11,34), "03",                                         ~
               at (11,37), fac(hex(8c)), mdate$(3)              , ch(12),~
               at (13,05), "But any of these are valid:",                ~
              at (14,15)," 04",at (14,20),fac(hex(8c)),mdate$(04),ch(12),~
              at (15,15)," 05",at (15,20),fac(hex(8c)),mdate$(05),ch(12),~
              at (16,15)," 06",at (16,20),fac(hex(8c)),mdate$(06),ch(12),~
              at (17,15)," 07",at (17,20),fac(hex(8c)),mdate$(07),ch(12),~
              at (18,15)," 08",at (18,20),fac(hex(8c)),mdate$(08),ch(12),~
              at (19,15)," 09",at (19,20),fac(hex(8c)),mdate$(09),ch(12),~
              at (14,45)," 10",at (14,50),fac(hex(8c)),mdate$(10),ch(12),~
              at (15,45)," 11",at (15,50),fac(hex(8c)),mdate$(11),ch(12),~
              at (16,45)," 12",at (16,50),fac(hex(8c)),mdate$(12),ch(12),~
              at (17,45)," 13",at (17,50),fac(hex(8c)),mdate$(13),ch(12),~
              at (18,45)," 14",at (18,50),fac(hex(8c)),mdate$(14),ch(12),~
              at (21,02), fac(hex(a4)), line21$                 , ch(79),~
                                                                         ~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)EXIT PROGRAM",                           ~
                                                                         ~
               keys(hex(00010d0f10)), key (keyhit%)

               if keyhit% <> 13 then L41530
                  call "MANUAL" ("GLTRIAL ")
                  goto L41080

L41530:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L41080


L50000: REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA--MAKES SURE THE RANGE OF ACCOUNTS IS OK,       *~
            * NORMALIZES THE KEYS FOR PLOW ROUTINE.                     *~
            *************************************************************

             errormsg$ = " "
             REM HANDLES CASE FOR "ALL" G/L ACCOUNTS
                 if firstaccount$ <> "ALL" then L50122
                    init(hex(00)) firstacct$
                    init(hex(ff)) lastacct$
                    return

L50122: REM SET CHANNEL FOR TESTRNGE
            if set = 1 then ch% = 1% else ch% = 11%
            call"TESTRNGE" (str(firstaccount$,1,9),str(lastaccount$,1,9),~
                             fromaccount$, toaccount$,                   ~
                             errormsg$, #ch%)
            if errormsg$ <> " " then return

        REM HANDLES CASE FOR SINGLE G/L ACCOUNT NUMBER
            if set = 1                                                   ~
               then call "GLVALID" (firstaccount$, firstacct$, errormsg$)~
               else call "GLVALD2" (firstaccount$, firstacct$, errormsg$)
            if errormsg$ = " " then L50180
                errormsg$ = errormsg$ & ": " & firstaccount$
                return
L50180:     if lastaccount$ <> " " then L50220
                lastacct$ = firstacct$
                goto L50280
L50220: REM HANDLES CASE FOR A RANGE OF ACCOUNT NUMBERS
            if set = 1                                                   ~
                then call "GLVALID" (lastaccount$, lastacct$, errormsg$) ~
                else call "GLVALD2" (lastaccount$, lastacct$, errormsg$)
            if errormsg$ = " " then L50270
                errormsg$ = errormsg$ & ": " & lastaccount$
                return
L50270:     if lastacct$ < firstacct$ then L50300
L50280:         str(firstacct$) = str(firstacct$) addc all(hex(ff))
                return
L50300: REM HANDLES ERROR MESSAGE -- LAST < FIRST.
            errormsg$ = "Beginning Account Number may not be greater th"&~
               "an Ending Account Number."
                 return

L51000: REM *************************************************************~
            *      T E S T   W H I C H   M O N T H  E N T E R E D       *~
            *                                                           *~
            * TEST THE MONTH ENTERED TO ENSURE THAT IT IS WITHIN RANGE  *~
            *************************************************************

            errormsg$ = " "
            call "NUMTEST" (which$, 1, 14, errormsg$, 0, which)
            if errormsg$ <> " " then return
            whichmonth% = which

            enddate$, searchdate$ = day1$(whichmonth%)
            if enddate$ = "CLOSING" then L51220
L51220:     startdate$ = day1$(whichmonth%+1)
            if startdate$ = "CLOSING" then startdate$=dates$(periods% + 1)
            call "DATE" addr ("G+", startdate$, 1%, startdate$, u3%)
            if u3% = 0 then return

        date_error_exit
            if tasktype$ = "B" then goto L65000
            u3% = 2%
            call "ASKUSER" (u3%, "*** ERROR SETTING DATE RANGE ***",     ~
                "GLTRIAL has been unable to set a valid date range",     ~
                " ", "Press (RETURN) to acknowledge and exit")
            goto L65000

        REM *************************************************************~
            *    I M A G E   S T A T E M E N T S   F O R   P R I N T    *~
            *************************************************************

L60040: %DATE: ########                ##################################~
        ~##########################                  GLTRIAL#######
L60060: %TIME: ########                ##################################~
        ~##########################                      PAGE: ####
L60080: %                              ##################################~
        ~##########################
L60091: %                                                         BEGINNI~
        ~NG BALANCE *------ P O S T I N G S ------*          ENDING
L60100: %ACCOUNT #    DESCRIPTION                               TRANSACTI~
        ~ON HEADERS          DEBITS         CREDITS         BALANCE
L60120: %------------ ------------------------------ --------------------~
        ~---------- --------------- --------------- ---------------
L60160: %############ ############################## ####################~
        ~########## -###,###,###.## -###,###,###.## -###,###,###.##
L60180: %*** END OF LISTING ***          NUMBER OF ACCOUNTS PRINTED: ####~
        ~######

        REM *************************************************************~
            *          B A C K G R O U N D   M O D E   S E T U P        *~
            *                                                           *~
            * IF THIS PROGRAM IS RUNNING IN BACKGROUND MODE, THEN THIS  *~
            * THING PRINTS THE TRIAL BALANCE FOR THE ENTIRE G/L.        *~
            *************************************************************

            init(hex(00)) firstaccount$
            init(hex(ff)) lastaccount$
            gosub L12000                  /* PRINT RANGE OF STUFF       */
            goto L65000

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND, JUST FOR EFFECT,*~
            * CLEVERLY DISPLAYS A MESSAGE WHILE WE LINK TO THE NEXT     *~
            * PROGRAM.                                                  *~
            *************************************************************

            call "SHOSTAT" ("One moment, please")
            end ret%