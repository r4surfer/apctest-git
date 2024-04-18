        REM *************************************************************~
            *                                                           *~
            *  FFFFF  RRRR     A     CCC    CCC   U   U  M   M          *~
            *  F      R   R   A A   C   C  C   C  U   U  MM MM          *~
            *  FFFF   RRRR   AAAAA  C      C      U   U  M M M          *~
            *  F      R RR   A   A  C   C  C   C  U   U  M   M          *~
            *  F      R   R  A   A   CCC    CCC    UUU   M   M          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * FRACCUM  - Used as part of G/L Financial Report generation*~
            *            module only.  Based on passed in data, this    *~
            *            subroutine builds a work file containing all   *~
            *            the amounts that any current report driver may *~
            *            need.  Accounts are sumarized where specified, *~
            *            and all of the report totaling is updated here.*~
            *            Basicaly this subroutine contains as much of   *~
            *            the reporting logic as can be consolidated, in *~
            *            an effort to make modifications easier. All of *~
            *            the range/selection logic is handled in here.  *~
            *            Numbers are also formatted as needed here.     *~
            *            Note the variable 'FREE%'. This is variable    *~
            *            is provided as a means of passing any special  *~
            *            instructions to this subroutine. It is puposely*~
            *            not used to allow for on site flexabilty.      *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/21/85 ! ORIGINAL (Re-write)                      ! HES *~
            * 12/08/85 ! FIXED ROUNDING BUG                       ! SGA *~
            * 12/13/85 ! Corrected handling of accts longer then 9! HES *~
            * 04/15/86 ! Reverse signs on totals if so indicated  ! HES *~
            * 10/12/87 ! W.R. Grace accountancy (dual books) mods.! JIM *~
            * 08/15/88 ! Dual Books depends on a SYSFILE2 flag.   ! JIM *~
            * 10/28/89 ! Added Reset of books flag if re-entering ! MJB *~
            * 10/22/91 ! Added previous/next year budget calcs.   ! MLJ *~
            * 02/25/93 ! Now supports period range selection,     ! MLJ *~
            *          !   changed implied integers.  Added dummy !     *~
            *          !   reads on #3 & #4 (#MAIN%) and #10 & #11!     *~
            *          !   (#LINE%) for SES Documentor.           !     *~
            * 03/01/93 ! PRR 12814 - Added call to GLFMT @ #10245.! MLJ *~
            * 03/04/93 ! Current Balance now correct for P & L    ! MLJ *~
            *          !   accounts when current period = 14.     !     *~
            * 03/05/93 ! Now gets correct 'last period' info when ! MLJ *~
            *          !   specifying ranges.                     !     *~
            * 02/22/94 ! Added BAL% to argument list to determine ! MLJ *~
            *          !   starting period used in curr bal calcs.!     *~
            * 03/22/95 ! Corrected calc of Period Opening bal on  ! MLJ *~
            *          !   'E' & 'R' accts when period > 14.      !     *~
            * 12/18/95 ! Corrected calc for 'YTD' budget when     ! MLJ *~
            *          !   selecting 'B' comparison and a single  !     *~
            *          !   period.                                !     *~
            ***********************************************************

        sub "FRACCUM"  (period1%,        /* Starting G/L Period        */~
                        period2%,        /* Ending G/L Period          */~
                        company$,        /* For Future Uses            */~
                        inst$,           /* Instruction Code From Formt*/~
                        group$,          /* Account Grouping Code      */~
                        acct1$,          /* From Account Number        */~
                        acct2$,          /* To Account Number          */~
                        zone%(),         /* Zone Definitions           */~
                        zonespec1$,      /* First Zone Selection       */~
                        zonespec2$,      /* Second Zone Selection      */~
                        zonespec3$,      /* Third Zone Selection       */~
                        zonespec4$,      /* Fourth Zone Selection      */~
                        zone_overide$,   /* Ignore Zoning Flag         */~
                        ieflag$(),       /* INCLUDE/EXCLUDE FLAGS      */~
                        print_if$,       /* Print/total Retriction Code*/~
                        reverse$,        /* Reverse Sign Indicator Y/N */~
                        print$,          /* Print Option (NO/ALL/SUM)  */~
                        paren$,          /* Put Parens Around Negatives*/~
                        round$,          /* Round To Even Dollars?     */~
                        free%,           /* Print Option (NO/ALL/SUM)  */~
                        total$,          /* Totaling Specifiaction     */~
                        totals(),        /* Totals Accumulator Array   */~
                        text$,           /* Text From Format Line      */~
                        directive%,      /* 0 = Don't Return Lines That*/~
                                         /*     Aren't To Be Printed.  */~
                                         /* 1 = Return Everything      */~
                        periods%,        /* Number OF Periods In F.Y.  */~
                        #09,             /* WORK FILE UFB ADDRESS      */~
                        set,             /* Statutory of Local Auth bks*/~
                        dual_books$,     /* Dual books in effect?      */~
                        returncode%,     /* 0 = Data Generated         */~
                                         /* 1 = Nothing Generated      */~
                                         /* 99 = Error condition hit   */~
                        bal%)            /*  1 = Start w/period 1      */~
                                         /* 14 = Start w/period 14     */

        dim account$12,                  /* WORK VARIABLE              */~
            acct1$9,                     /* FROM ACCOUNT               */~
            acct2$9,                     /* TO ACCOUNT                 */~
            accum(33),                   /* WORK ACCUMULATOR VARIABLE  */~
            company$3,                   /* FOR FUTURE USE             */~
            descr$45,                    /* ACCOUNT DESCRIPTION        */~
            dual_books$1,                /* Dual books in effect?      */~
            group$6,                     /* ACCOUNT GROUPING CODE      */~
            ieflag$(4)1,                 /* INCLUDE/EXCLUDE FLAGS      */~
            mainkey$20,                  /* WORK VARIABLE              */~
            paren$3,                     /* PUT PARENS AROUND NEGATIVES*/~
            readkey$99,                  /* MISC READ KEY              */~
            record$(33)18,               /* FORMATED DATA TO PASS BACK */~
            print$3,                     /* PRINT OPTIONS              */~
            print_if$1,                  /* PRINT/TOTAL RETRICTION CODE*/~
            reverse$3,                   /* REVERSE SIGN INDICATOR     */~
            round$3,                     /* ROUND TO EVEN DOLLARS?     */~
            saveacct$16,                 /* WORK VARIABLE              */~
            tempbal(32),                 /* WORK VARIABLE              */~
            tempbgt(39),                 /* WORK VARIABLE              */~
            text$45,                     /* TEXT FROM FORMAT LINE      */~
            total$10,                    /* TOTALING SPECIFIACTION     */~
            totals(10,33),               /* REPORT TOTALS ACCUMULATOR  */~
            zone%(4,2),                  /* START, LENGTH IN ACCOUNT   */~
            zone_overide$3,              /* IGNORE ZONING FLAG         */~
            zonespec$16,                 /* WORK VARIABLE              */~
            zonespec1$16,                /* USERS SELECTION FOR ZONE 1 */~
            zonespec2$16,                /* USERS SELECTION FOR ZONE 2 */~
            zonespec3$16,                /* USERS SELECTION FOR ZONE 3 */~
            zonespec4$16                 /* USERS SELECTION FOR ZONE 4 */

        dim f1%(64)                      /* = 1 IF READ WAS SUCCESSFUL */
        dim f2%(64)                      /* = 1 IF FILE IS OPEN        */
        dim fs%(64)                      /* = FILE STATUS CODES        */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.03 08/12/96 Last Wang Release               "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #02 ! GLBUDGET ! General Ledger Budget File               *~
            * #03 ! GLMAIN   ! GENERAL LEDGER MAIN FILE                 *~
            * #04 ! GLMAIN2  ! G. L. chart of accounts for local auth.  *~
            * #10 ! FRGRPLIN ! G/L Grouping Codes Detail File (Stat.)   *~
            * #11 ! FRGRPLI2 ! G/L Grouping Codes Detail File (L. A.)   *~
            *************************************************************

            select #02, "GLBUDGET",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 9

            select #03,  "GLMAIN",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 9

            select #04, "GLMAIN2",                                       ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   9

            select #10, "FRGRPLIN", varc, indexed, recsize = 60,         ~
                        keypos = 1, keylen = 22

            select #11, "FRGRPLI2", varc, indexed, recsize = 60,         ~
                        keypos = 1, keylen = 22

        REM Dummy Reads for SES Documentor...
            readkey$ = all(hex(00))
                call "READ104" (#3, readkey$, f1%(3%))     /* GLMAIN   */
            readkey$ = all(hex(00))
                call "READ104" (#4, readkey$, f1%(4%))     /* GLMAIN2  */
            readkey$ = all(hex(00))
                call "READ104" (#10, readkey$, f1%(10%))   /* FRGRPLIN */
            readkey$ = all(hex(00))
                call "READ104" (#11, readkey$, f1%(11%))   /* FRGRPLI2 */

            if beenherebefore% <> 0% then goto L08000
                beenherebefore% = 1%
                mat f2% = con
                main% = 3% : line% = 10%
                if set = 1 then goto L02410
                    main% = 4 : line% = 11%
L02410:         call "OPENCHCK" (#02, fs%( 2%), f2%( 2%), 0%, " ")
                call "OPENCHCK" (#03, fs%( 3%), f2%( 3%), 0%, " ")
                call "OPENCHCK" (#10, fs%(10%), f2%(10%), 0%, " ")
                if dual_books$ <> "Y" then goto L02450
                    call "OPENCHCK" (#04, fs%( 4%), f2%( 4%), 0%, " ")
                    call "OPENCHCK" (#11, fs%(11%), f2%(11%), 0%, " ")

L02450:     if (set = 1 and fs%(3%) = 1%) or (set = 2 and fs%(4%) = 1%)  ~
                    then goto L09000
                close printer
                call "ASKUSER" (0%, "*** G/L MASTER FILE ERROR ***",     ~
                    "G/L Master File Not Found", " ",                    ~
                    "Press (RETURN) To Exit")
                returncode% = 99%
                end

L08000:         main% = 3% : line% = 10%
                if set = 1 then goto L09000
                    main% = 4 : line% = 11%

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            mat accum = zer
            accounts_qualifying% = 0%
            if acct2$ = " " then acct2$ = acct1$
            mainkey$ = all(hex(00))
            call "DELETE" (#09, mainkey$, 0%)     /* Clear Work File */

            if str(inst$,,1%) <> "T" then L10000
                convert str(inst$,2%) to level%, data goto L09150
                goto L09220
L09150:         close printer
                    call "ASKUSER" (0%, "Please Note",                   ~
                         "Invalid Total Level Found In Format.",         ~
                         "Press (RETURN) To Exit", " ")
                    returncode% = 99%
                    end

L09220: REM Return Totals Logic...
                if print$ <> "NO" then print$ = "SUM"
                for i% = 1% to 33%
                    if reverse$ = "YES" then totals(level%+1%,i%) =      ~
                                             totals(level%+1%,i%) * (-1%)
                    accum(i%) = totals(level%+1%,i%)
                next i%
                accounts_qualifying% = 1%
                goto L13000

L10000: REM *************************************************************~
            *           M A I N   P R O C E S S I N G   L O O P         *~
            *                                                           *~
            * GETS ACCOUNTS SPECIFIED, THEN PASSES TO CRUNCH SECTIONS.  *~
            *************************************************************

            branch% = 2%
            str(mainkey$,,9%) = str(acct1$,,9%) addc all(hex(ff))
            if group$ = " " then next_account
                branch% = 1%
                mainkey$ = str(group$,,6%) & hex(000000)

        next_account
            on branch% goto L10150, L10320

L10150: REM Load accounts by Group Code...
                call "PLOWNEXT" (#line%, mainkey$, 6%, f1%(line%))
                    if f1%(line%) = 1% then L10210
                    if print$ = "SUM" then L13000
                    if print$ = "NO " then L13000
                    goto L65000
L10210:         account$ = str(mainkey$,7%)
                call "READ100" (#main%, account$, f1%(main%))
                     if f1%(main%) = 1% then L10400
                     close printer
                     call "GLFMT" (account$)
                     call "ASKUSER" (0%, "Please Note",                  ~
                     "Group Code: " & group$ & "  Account: " & account$, ~
                     "Invalid Account Found in Above Group Code",        ~
                     "Press (RETURN) To Exit")
                     returncode% = 99%
                     end

L10320: REM Load accounts By Account Range...
                call "PLOWNEXT" (#main%, mainkey$, 0%, f1%(main%))
                    if f1%(main%) = 1% and str(mainkey$,,9%) <= acct2$   ~
                        then L10380
                    if print$ = "SUM" then L13000
                    if print$ = "NO " then L13000
                    goto L65000
L10380:         account$ = str(mainkey$,,9%)

L10400: REM Process Accounts. First screen accounts based on zones...
            if zone_overide$ = "YES" then L10600
            saveacct$ = account$
            if set = 1                                                   ~
                then call "GLFMT" (account$)                             ~
                else call "GLFMT2" (account$)
            for i% = 1% to 4%
                if zone%(i%,1%) = 0% then L10590
                s% = zone%(i%,1%) : l% = zone%(i%,2%)
                on i% goto L10480, L10490, L10500, L10510
L10480:              zonespec$ = zonespec1$ : goto L10530
L10490:              zonespec$ = zonespec2$ : goto L10530
L10500:              zonespec$ = zonespec3$ : goto L10530
L10510:              zonespec$ = zonespec4$

L10530: REM Test Zone To See If Account Qualifies...
                if zonespec$ = " " then L10590
                if str(zonespec$,,l%) <> str(account$,s%,l%) and         ~
                                     ieflag$(i%) = "O" then next_account
                if str(zonespec$,,l%) = str(account$,s%,l%) and          ~
                                     ieflag$(i%) = "X" then next_account
L10590:     next i%
L10600:     accounts_qualifying% = 1%

        REM Load up account's balances...
            get #main%, using L10640, descr$, type$, tempbal()
L10640:         FMT XX(9), CH(30), CH(1), XX(4), 32*PD(14,4)
            if reverse$ = "YES" then mat tempbal = (-1%)*tempbal

        REM Retrieve info from Budget File...
            mat tempbgt = zer
            if set = 2 then goto L11000 /* No budget for Local Authority */
                call "READ100" (#02, saveacct$, f1%(2%))
                if f1%(2%) = 0%  then L11000
                    get #02, using L10720, tempbgt()
L10720:                 FMT XX(17), 39*PD(14,4)
                    if reverse$ = "YES" then mat tempbgt = (-1%)*tempbgt

L11000: REM *************************************************************~
            *   Add to accumulator array                                 ~
            *************************************************************

        REM GET CURRENT YEAR DATA FIRST, ADJUST IF PERIOD% > 13%...

        REM Current Year Opening Balance...
            period% = period1%
            offset% = 0% : yearo = 0
            if period% > 13% then offset% = 13%
            if type$ = "R" or type$ = "E" then L11140
            for i% = 1% to 15% + offset%
                accum(1%) = accum(1%) + tempbal(i%)
                yearo    = yearo    + tempbal(i%)
            next i%

L11140: REM Current Period Opening Balance...
            period% = period1%
            offset% = 1% : periodo = 0
            if type$ <> "R" and type$ <> "E" then L11190    /* P&L accts*/
                if period% < 15% then L11190
                    offset% = 16%
L11190:     for i% = offset% to 14% + period%
                accum(2%) = accum(2%) + tempbal(i%)
                periodo  = periodo  + tempbal(i%)
            next i%

        REM Current Period Activity...
            period% = period1%
L11244:     accum(4%) = accum(4%) + tempbal(15%+period%)
            period% = period% + 1%
            if period% > period2% then L11270
                goto L11244

L11270: REM Current Year Previous Period Activity...
            np% = period2% - period1% + 1%              /* # of Periods */
            temp% = 0%
            for i% = period1% + 14% to period1% + 15% - np% step -1%
                if i% = 15% then temp% = temp% + 1%
                if i% - temp% = 14% and periods% <> 13%                  ~
                    then temp% = temp% + 1%
                if i% = 14% and periods% <> 13% then temp% = temp% + 1%
                if i% = 28% and periods% <> 13% then temp% = temp% + 1%
                if temp% > 2% then temp% = 2%
                accum(17%) = accum(17%) + tempbal(i% - temp%)
            next i%

        REM Variance - Current Period From Last Period...
            accum(18%) = accum(4%) - accum(17%)

        REM Current Period Balance...
            period% = period1%
            if bal% = 1% then currentbalance=periodo+tempbal(15%+period%)~
                         else currentbalance=tempbal(15%+period%)
L11386:     period% = period% + 1%
            if period% > period2% then L11394
                currentbalance = currentbalance + tempbal(15%+period%)
                goto L11386
L11394:     accum(6%) = accum(6%) + currentbalance

        REM Current Year YTD Activity...
            if bal% = 1% then accum(8%)=accum(8%)+(currentbalance-yearo) ~
                         else accum(8%)=accum(8%)+currentbalance

        REM NOW GET LAST YEAR DATA, ADJUST IF PERIOD% > 13%...

        REM Prior Year Opening Balance...
            period% = period1%
            offset% = 1% : yearo = 0
            if period% > 13% then offset% = 15%
            for i% = 1% to offset%
                accum(9%) = accum(9%) + tempbal(i%)
                yearo    = yearo    + tempbal(i%)
            next i%

        REM Prior Year Period Opening Balance...
            period% = period1%
            offset% = 0% : periodo = 0
            if period% > 13% then offset% = 1%
            for i% = 1% to period% + offset%
                accum(10%) = accum(10%) + tempbal(i%)
                periodo   = periodo   + tempbal(i%)
            next i%

        REM Prior Year Current Period Activity...
            period% = period1%
L11624:     offset% = 0%
            if period% > 13% then offset% = 1%
            accum(12%) = accum(12%) + tempbal(1%+period%+offset%)
            period% = period% + 1%
            if period% > period2% then L11650
                goto L11624

L11650: REM Prior Year Current Period Balance...
            period% = period1%  : currentbalance = 0
            offset% = 0%
            if period% > 13% then offset% = 1%
            if bal% = 1% then currentbalance =                           ~
                                    periodo + tempbal(1%+period%+offset%)~
                         else currentbalance = tempbal(1%+period%+offset%)
L11660:     period% = period% + 1%
            if period% > period2% then L11670
            if period% > 13% then offset% = 1%
            currentbalance = currentbalance + tempbal(1%+period%+offset%)
            goto L11660
L11670:     accum(14%) = accum(14%) + currentbalance

        REM Prior Year YTD Activity...
            if bal% = 1% then accum(16%) =                               ~
                                    accum(16%) + (currentbalance - yearo)~
                         else accum(16%) = accum(16%) + currentbalance
        REM Variance - Current Balance From Last Year...
            accum(19%) = accum(6%) - accum(14%)

        REM Variance - Period Activity From Last year...
            accum(21%) = accum(4%) - accum(12%)

        REM Budget Calculations...
            if period1% >= 14% then period% = bal%                       ~
                               else period% = period1%
            for i% = period% to period2%
                if i% < period1% then L11930
                    accum(5%)  = accum(5%)  + tempbgt(13%+i%)
*                  ACCUM(7%)  = ACCUM(7%)  + TEMPBGT(13%+I%)
                    accum(23%) = accum(23%) + tempbgt(26%+i%)
L11930:         accum(3%)  = accum(3%)  + tempbgt(13%+i%)
                accum(11%) = accum(11%) + tempbgt(i%)
                accum(13%) = accum(13%) + tempbgt(i%)
                accum(25%) = accum(25%) + tempbgt(26%+i%)
            next i%

            for i% = 1% to period2%
                accum(7%)  = accum(7%)  + tempbgt(13%+i%)
                accum(15%) = accum(15%) + tempbgt(i%)
                accum(24%) = accum(24%) + tempbgt(26%+i%)
                if i% = period1% then L11972
                    accum(27%) = accum(27%) + tempbgt(13%+i%)
L11972:     next i%

            accum(26%) = accum(26%) + tempbgt(13%+period1%-1%)

            accum(22%) = accum(4%)  - accum(5%)
            accum(20%) = accum(6%)  - accum(7%)
            accum(28%) = accum(5%)  - accum(13%)
            accum(29%) = accum(5%)  - accum(23%)
            accum(30%) = accum(5%)  - accum(26%)
            accum(31%) = accum(7%)  - accum(15%)
            accum(32%) = accum(7%)  - accum(24%)
            accum(33%) = accum(7%)  - accum(27%)

            if print$ = "SUM" then next_account
            if print$ = "NO " then next_account

L13000: REM *************************************************************~
            *                W R I T E   R E C O R D                    *~
            *                                                           *~
            * Writes records to print tickler file, adds to totals.     *~
            *************************************************************

        REM Find Anything?...
            if accounts_qualifying% = 0% then L13360

        REM Check 'PRINT_IF' constraint...
            if print_if$ <> "+" then L13170
                if inst$ = "CB" and accum(6%) < .001 then move_on
                if inst$ = "PA" and accum(4%) < .001 then move_on
                if inst$ = "YO" and accum(1%) < .001 then move_on
                if inst$ = "PO" and accum(2%) < .001 then move_on
                if inst$ = "YA" and accum(8%) < .001 then move_on

L13170:     if print_if$ <> "-" then L13240
                if inst$ = "CB" and accum(6%) > -.001 then move_on
                if inst$ = "PA" and accum(4%) > -.001 then move_on
                if inst$ = "YO" and accum(1%) > -.001 then move_on
                if inst$ = "PO" and accum(2%) > -.001 then move_on
                if inst$ = "YA" and accum(8%) > -.001 then move_on

L13240: REM Update Specified Total Buckets...
            for i% = 10% to 1% step -1%
                if str(total$,i%,1%) = " " then L13340
                if str(total$,i%,1%) = "0" then L13340
                for j% = 1% to 33%
                     accum(j%) = round(accum(j%), 2%)
                     if round$ = "YES" then accum(j%)=round(accum(j%), 0%)
                     if str(total$,i%,1%) = "+" then totals(i%,j%) =     ~
                                      round(totals(i%,j%) + accum(j%), 2%)
                     if str(total$,i%,1%) = "-" then totals(i%,j%) =     ~
                                      round(totals(i%,j%) - accum(j%), 2%)
                next j%
L13340:     next i%

L13360: REM Is this data to be printed?...
            if accounts_qualifying% = 0% then print$ = "NO "
            if print$ = "NO " and directive% = 0% then move_on
            returncode% = 0%
            if text$ <> " " then descr$ = text$
            if print$ = "SUM" then descr$ = text$

            for i% = 1% to 33%
                accum(i%) = round(accum(i%), 2%)
                if round$ = "YES" then accum(i%) = round(accum(i%), 0%)
                if round$ = "YES" and paren$ = "YES" then                ~
                                  put record$(i%), using L13470, accum(i%)
L13470:                               %#,###,###,###,###
                if round$ = "YES" and paren$ <> "YES" then               ~
                                  put record$(i%), using L13500, accum(i%)
L13500:                               % -###,###,###,###
                if round$ <> "YES" and paren$ = "YES" then               ~
                                  put record$(i%), using L13530, accum(i%)
L13530:                               %##,###,###,###.##
                if round$ <> "YES" and paren$ <> "YES" then              ~
                                  put record$(i%), using L13560, accum(i%)
L13560:                               %-#,###,###,###.##
                if paren$ = "YES" then str(record$(i%),18%) = " "
                if paren$ = "YES" and accum(i%) < -.005 then             ~
                      str(record$(i%), pos(record$(i%)<>" ")-1%,1%) = "("
                if paren$ = "YES" and accum(i%) < -.005 then             ~
                                               str(record$(i%),18%) = ")"
            next i%
            if print$ = "SUM" then account$ = "SUMMARY"
            if print$ = "NO " then account$ = "NO PRINT"
            write #09, using L13670, account$, descr$, record$()

L13670:     FMT CH(16),     /* Account Number                          */~
                CH(45),     /* Text Or Account Description             */~
                CH(18),     /* Current Year - Opening Balance          */~
                CH(18),     /* Current Period - Opening Balance        */~
                CH(18),     /* Current Period - Budget Opening Balance */~
                CH(18),     /* Current Period  - Activity              */~
                CH(18),     /* Current Period  - Budget Activity       */~
                CH(18),     /* Current Period  - Balance               */~
                CH(18),     /* Current Period  - YTD Budget            */~
                CH(18),     /* Current Year - YTD Activity             */~
                CH(18),     /* Prior year - Opening Balance            */~
                CH(18),     /* Prior Year - Period Opening Balance     */~
                CH(18),     /* Prior Year - Period Budget Opening Bal  */~
                CH(18),     /* Prior Year - Current Period Activity    */~
                CH(18),     /* Prior Year - Current Period Budget Act  */~
                CH(18),     /* Prior Year - Current Period Balance     */~
                CH(18),     /* Prior Year - Current Period YTD Budget  */~
                CH(18),     /* Prior Year - YTD Activity               */~
                CH(18),     /* Current Year - Previous Period Activity */~
                CH(18),     /* Variance - Current Period From Last Per */~
                CH(18),     /* Variance - Current Balance From Last Yr */~
                CH(18),     /* Variance - Current Balance From Budget  */~
                CH(18),     /* Variance - Period Activity From Last Yr */~
                CH(18),     /* Variance - Period Activity From Budget  */~
                CH(18),     /* Next Year - Period Budget Activity      */~
                CH(18),     /* Next Year - Budget Current Balance      */~
                CH(18),     /* Next Year - Period Budget Opening Bal   */~
                CH(18),     /* Current Year - Last Period Budget Act   */~
                CH(18),     /* Current Year - Last Period Budget YTD   */~
                CH(18),     /* Variance - Curr Per Bgt Act From Last Yr*/~
                CH(18),     /* Variance - Curr Per Bgt Act From Next Yr*/~
                CH(18),     /* Variance - Curr Per Bgt Act From Last Pr*/~
                CH(18),     /* Variance - Curr Bgt Bal From Last Year  */~
                CH(18),     /* Variance - Curr Bgt Bal From Next Year  */~
                CH(18)      /* Variance - Curr Bgt Bal From Last Per   */

        move_on
            if print$ = "SUM" then L65000
            if print$ = "NO " then L65000
            mat accum = zer
            goto next_account

L65000: REM *************************************************************~
            *                E X I T  S U B R O U T I N E               *~
            *                                                           *~
            * Returns Control To Caller, Will Zero Indicated Totals.    *~
            *************************************************************

        REM Zero Totals As Indicated...
            for i% = 10% to 1% step -1%
                if str(total$,i%,1%) <> "0" then L65120
                for j% = 1% to 33%
                    totals(i%,j%) = 0
                next j%
L65120:     next i%
            end
