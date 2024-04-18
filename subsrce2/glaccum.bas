        REM *************************************************************~
            *                                                           *~
            *   GGG   L        A     CCC    CCC   U   U  M   M          *~
            *  G      L       A A   C   C  C   C  U   U  MM MM          *~
            *  G  GG  L      AAAAA  C      C      U   U  M M M          *~
            *  G   G  L      A   A  C   C  C   C  U   U  M   M          *~
            *   GGG   LLLLL  A   A   CCC    CCC    UUU   M   M          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GLACCUM  - Used as part of G/L Financial Report generation*~
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
            * 09/23/87 ! ORIGINAL (Copy from FRACCUM)             ! RAC *~
            * 04/04/88 ! Moved file SELECT & OPENs to GLPRINT.    ! DAW *~
            * 12/08/89 ! Added Local Authority, SELECT & OPEN for ! MLJ *~
            *          !   GLMAIN, GLBUDGET, FRGRPMAS, FRGRPMA2.  !     *~
            *          !   Mainly got it to execute.              !     *~
            * 06/18/90 ! Fixed Group Code processing by changing  ! MLJ *~
            *          !   access from Master files to Detail     !     *~
            *          !   on #1 and #4.  Also corrected calc     !     *~
            *          !   on percentages.                        !     *~
            * 12/05/91 ! Added Prev/Next year budget exposure.    !     *~
            * 06/25/92 ! PRR 12494 - Corrected totalling problem. ! MLJ *~
            * 09/04/92 ! Fixed % calc. It is now and was in the   ! JDH *~
            *          !   past but might not be foreverafter, the!     *~
            *          !   percent differential of the 1st column !     *~
            *          !   compared to the 2nd column. Thx Frank. !     *~
            * 02/10/93 ! PRR 12390 - When using parens, positive  ! MLJ *~
            *          !   amts align w/right most digit, not the !     *~
            *          !   right paren. Fixed implied integers.   !     *~
            * 02/15/93 ! Added GLMAIN2 for Local Authority Accts. ! MLJ *~
            *          !   #1 & #4 now GP%, #3 & #5 now AC%,      !     *~
            *          !   minimizing code. Also added dummy reads!     *~
            *          !   for SES documentor.                    !     *~
            *          ! PRR 12739 - Corrected Last Year's Period !     *~
            *          !   Activity for periods > 13.             ! MLJ *~
            * 02/24/93 ! Modified to allow accumulation for a     ! MLJ *~
            *          !   range of G/L periods.                  !     *~
            * 03/02/93 ! PRR 12814 - Added call to GLFMT at #10245! MLJ *~
            * 03/05/93 ! Balance now correct when current period  ! MLJ *~
            *          !   equal 14.                              !     *~
            * 03/09/93 ! Now gets correct last period balance on  ! MLJ *~
            *          !   range of periods.                      !     *~
            * 06/23/93 ! PRR 12975 - "NO" to print lines when all ! MLJ *~
            *          !   zeroes now works as stated.            !     *~
            * 02/07/94 ! PRR 13073 - Added BAL% to argument list  ! MLJ *~
            *          !   to determine starting period used in   !     *~
            *          !   current bal calcs (CB,YCB,YA,LYA).     !     *~
            * 04/04/95 ! PRR 13379 - No multiple format. Thx Jeff.! JDH *~
            * 04/12/94 ! Corrected budget calc errors introduced  ! MLJ *~
            *          !   during PRR 13073 fix.                  !     *~
            *************************************************************


        sub "GLACCUM"  (period1%,        /* Starting G/L Period        */~
                        period2%,        /* Ending G/L Period          */~
                        company$,        /* For Future Uses            */~
                        inst$(),         /* Instruction Code From Formt*/~
                        group$(),        /* Account Grouping Code      */~
                        acct1$(),        /* From Account Number        */~
                        acct2$(),        /* To Account Number          */~
                        zone%(),         /* Zone Definitions           */~
                        zonespec1$,      /* First Zone Selection       */~
                        zonespec2$,      /* Second Zone Selection      */~
                        zonespec3$,      /* Third Zone Selection       */~
                        zonespec4$,      /* Fourth Zone Selection      */~
                        zone_overide$(), /* Ignore Zoning Flag         */~
                        ieflag$(),       /* INCLUDE/EXCLUDE FLAGS      */~
                        print_if$(),     /* Print/total Retriction Code*/~
                        reverse$(),      /* Reverse Sign Indicator Y/N */~
                        print$(),        /* Print Option (NO/ALL/SUM)  */~
                        paren$,          /* Put Parens Around Negatives*/~
                        round$,          /* Round To Even Dollars?     */~
                        zero$,           /* Print if All Zeros?        */~
                        total$(),        /* Totaling Specifiaction     */~
                        totals(),        /* Totals Accumulator Array   */~
                        text$(),         /* Text From Format Line      */~
                        columns%,        /* Number of Report Columns   */~
                        colpos%(),       /* Column Positions           */~
                        collgth%(),      /* Column Lengths             */~
                        colfmt$(),       /* Column Formats             */~
                        colcom$(),       /* Column Commas?             */~
                        dolprt$(),       /* Column Formats             */~
                        directive%,      /* 0 = Don't Return Lines That*/~
                                         /*     Aren't To Be Printed.  */~
                                         /* 1 = Return Everything      */~
                        periods%,        /* Number OF Periods In F.Y.  */~
                        #9,              /* WORK FILE UFB ADDRESS      */~
                        set$,            /* Set of G/L books to use    */~
                        returncode%,     /* 0 = Data Generated         */~
                                         /* 1 = Nothing Generated      */~
                                         /* 99 = Error condition hit   */~
                        bal%)            /* 1%  = Start w/period 1     */~
                                         /* 14% = Start w/period 14    */

        dim account$12,                  /* WORK VARIABLE              */~
            acct1$(10)9,                 /* FROM ACCOUNT               */~
            acct2$(10)9,                 /* TO ACCOUNT                 */~
            accum(10),                   /* Accum for each Column      */~
            colcom$(10)3,                /* Column Commas?             */~
            colfmt$(10)1,                /* Column Format              */~
            collgth%(10),                /* Column Lengths             */~
            colpos%(10),                 /* Column Positions           */~
            company$3,                   /* FOR FUTURE USE             */~
            descr$(10)45,                /* ACCOUNT DESCRIPTION        */~
            dol%(10),                    /* Dollar Flag                */~
            dolprt$(10)1,                /* Dollar Sign Print Switch   */~
            group$(10)6,                 /* ACCOUNT GROUPING CODE      */~
            ieflag$(4)1,                 /* INCLUDE/EXCLUDE FLAGS      */~
            inst$(10)4,                  /* INSTRUCTION CODES          */~
                                         /*   Note: No formatting codes*/~
                                         /* (H,D,DC,U,DU or P) are     */~
                                         /* passed to this sub.        */~
            mainkey$20,                  /* WORK VARIABLE              */~
            paren$3,                     /* PUT PARENS AROUND NEGATIVES*/~
            readkey$99,                  /* MISC READ KEY              */~
            record$132,                  /* FORMATED DATA TO PASS BACK */~
            print$(10)3,                 /* PRINT OPTIONS              */~
            print_if$(10)1,              /* PRINT/TOTAL RETRICTION CODE*/~
            reverse$(10)3,               /* REVERSE SIGN INDICATOR     */~
            round$3,                     /* ROUND TO EVEN DOLLARS?     */~
            saveacct$16,                 /* WORK VARIABLE              */~
            set$1,                       /* SET OF G/L BOOKS TO USE    */~
            tempbal(32),                 /* WORK VARIABLE              */~
            tempbgt(39),                 /* WORK VARIABLE              */~
            tempprt$17,                  /* WORK VARIABLE              */~
            text$(10)45,                 /* TEXT FROM FORMAT LINE      */~
            total$(10)20,                /* TOTALING SPECIFIACTION     */~
            totals(20),                  /* REPORT TOTALS ACCUMULATOR  */~
            zero$3,                      /* Print if All Zeros         */~
            zone%(4,2),                  /* START, LENGTH IN ACCOUNT   */~
            zone_overide$3,              /* IGNORE ZONING FLAG         */~
            zonespec$16,                 /* WORK VARIABLE              */~
            zonespec1$16,                /* USERS SELECTION FOR ZONE 1 */~
            zonespec2$16,                /* USERS SELECTION FOR ZONE 2 */~
            zonespec3$16,                /* USERS SELECTION FOR ZONE 3 */~
            zonespec4$16                 /* USERS SELECTION FOR ZONE 4 */

        dim f1%(64)                      /* = 1 IF READ WAS SUCCESSFUL */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.01 06/23/95 Patch Finalization of R6.04.01  "

        REM *************************************************************~
            *                S E L E C T   F I L E S                    *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! FRGRPLIN ! G/L Group Codes Detail - Statutory       *~
            * # 2 ! GLBUDGET ! G/L Budget File                          *~
            * # 3 ! GLMAIN   ! G/L Main File                            *~
            * # 4 ! FRGRPLI2 ! G/L Group Codes Detail - Local Authority *~
            * # 5 ! GLAMIN2  ! G. L. chart of accounts for local auth.  *~
            *************************************************************

            select #1,  "FRGRPLIN",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 60,                                    ~
                        keypos = 1, keylen = 22,                         ~
                        alt key 1, keypos = 7, keylen = 16, dup

            select #2,  "GLBUDGET",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 9

            select  #3, "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 9

            select  #4, "FRGRPLI2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 60,                                    ~
                        keypos = 1, keylen = 22,                         ~
                        alt key 1, keypos = 7, keylen = 16, dup

            select #5,  "GLMAIN2",                                       ~
                        varc, indexed, recsize = 300,                    ~
                        keypos = 1, keylen = 9

            call "OPENCHCK" (#2, 0%, 0%, 0%, " ")

            if set$ <> "1" then L02406
                call "OPENCHCK" (#1, 0%, 0%, 0%, " ")       /* FRGRPLIN */
                call "OPENCHCK" (#3, 0%, 0%, 0%, " ")       /* GLMAIN   */
                gp% = 1%  :  ac% = 3%  :  goto L02500
L02406:     call "OPENCHCK" (#4, 0%, 0%, 0%, " ")           /* FRGRPLI2 */
            call "OPENCHCK" (#5, 0%, 0%, 0%, " ")           /* GLMAIN2  */
            gp% = 4%  :  ac% = 5%

L02500: REM Dummy Reads for SES Documentor...
            readkey$ = all(hex(00))
                call "READ104" (#1, readkey$, f1%(1%))
            readkey$ = all(hex(00))
                call "READ104" (#3, readkey$, f1%(3%))
            readkey$ = all(hex(00))
                call "READ104" (#4, readkey$, f1%(4%))
            readkey$ = all(hex(00))
                call "READ104" (#5, readkey$, f1%(5%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            mat accum  = zer
            init (hex(20)) descr$(), record$
            printsum%, seq%, accounts_qualifying% = 0%
            mainkey$ = all(hex(00))

            call "DELETE" (#9, mainkey$, 0%)     /* Clear Work File */
            for r% = 1% to columns%
                if inst$(r%) = " " then L09160
                if acct2$(r%) = " " then acct2$(r%) = acct1$(r%)
                if str(inst$(r%),,1%) = "T" then gosub L09190              ~
                   else gosub L10000
                if print$(r%) = "ALL" then L65000
L09160:     next r%
            gosub loop
            goto L65000

L09190:         convert str(inst$(r%),2%) to level%, data goto L09210
                goto L09280

L09210:         close printer
                    call "ASKUSER" (0%, "Please Note",                   ~
                         "Invalid Total Level Found In Format.",         ~
                         "Press (RETURN) To Exit", " ")
                    returncode% = 99%
                    end

L09280: REM Return Totals Logic...
                if print$(r%) <> "NO" then print$(r%) = "SUM"
                    if reverse$(r%) = "YES" then totals(level%+1%)  =    ~
                                             totals(level%+1%) * (-1)
                    accum(r%) = totals(level%+1%)
                accounts_qualifying% = 1%
                printsum% = 1%
                gosub L13000
                return

L10000: REM *************************************************************~
            *           M A I N   P R O C E S S I N G   L O O P         *~
            *                                                           *~
            * GETS ACCOUNTS SPECIFIED, THEN PASSES TO CRUNCH SECTIONS.  *~
            *************************************************************

            if str(inst$(r%),1%,1%) = "N" and str(inst$(r%),3%,1%) = " " ~
                then L12500
            if str(inst$(r%),1%,1%) = "U" then L13980
            if str(inst$(r%),1%,2%) = "DU" then L14000
            branch% = 2%
            accounts_qualifying%, printsum% = 0%
            str(mainkey$,,9%) = str(acct1$(r%),,9%) addc all(hex(ff))
            if group$(r%) = " " then next_account
                branch% = 1%
                mainkey$ = str(group$(r%),,6%) & hex(000000)

        next_account
            on branch% goto L10150, L10320

L10150: REM Load accounts by Group Code...
            call "PLOWNEXT" (#gp%, mainkey$, 6%, f1%(gp%))
                if f1%(gp%) = 1% then L10210
                    if print$(r%) = "ALL" then return
                        gosub L13080
                        return
L10210:     account$ = str(mainkey$,7%)
            call "READ100" (#ac%, account$, f1%(ac%))
                 if f1%(ac%) = 1% then L10400
            close printer
                     call "GLFMT" (account$)
                     call "ASKUSER" (0%, "Please Note",                  ~
                     "Group Code: " & group$(r%)&"  Account: "&account$, ~
                     "Invalid Account Found in Above Group Code",        ~
                     "Press (RETURN) To Exit")
                     returncode% = 99%
                     end

L10320: REM Load accounts By Account Range...
            call "PLOWNEXT" (#ac%, mainkey$, 0%, f1%(ac%))
                if f1%(ac%) = 1% and str(mainkey$,,9%) <= acct2$(r%)     ~
                    then L10380
                if print$(r%) = "ALL" then return
                    gosub L13080
                    return
L10380:     account$ = str(mainkey$,,9%)

L10400: REM Process Accounts. First screen accounts based on zones...
            if print$(r%) = "ALL" then L10450
            gosub L10500
            goto next_account

L10450:     j% = r%
            for r% = j% to columns%
                if inst$(r%) = " " then L10480
                if acct2$(r%) = " " then acct2$(r%) = acct1$(r%)
                gosub L10500
L10480:     next r%
            r% = j%
            gosub loop
            goto next_account

L10500:     if str(inst$(r%),,1%)  = "N" and                             ~
               str(inst$(r%),3%,1%) = " " then L12500
            if str(inst$(r%),1%,1%)  = "D" then L13980
            if str(inst$(r%),1%,2%)  = "DU" then L14000
            if zone_overide$ = "YES" then L10710
            if print$(r%) = "ALL" and r% <> j% then L10550
                saveacct$ = account$
                call "GLFMT" (account$)

L10550:     for i% = 1% to 4%
                if zone%(i%,1%) = 0% then L10700
                s% = zone%(i%,1%) : l% = zone%(i%,2%)
                on i% goto L10590, L10600, L10610, L10620
L10590:              zonespec$ = zonespec1$ : goto L10640
L10600:              zonespec$ = zonespec2$ : goto L10640
L10610:              zonespec$ = zonespec3$ : goto L10640
L10620:              zonespec$ = zonespec4$

L10640: REM Test Zone qualification...
                if zonespec$ = " " then L10700
                if str(zonespec$,,l%) <> str(account$,s%,l%) and         ~
                   ieflag$(i%) = "O" then return
                if str(zonespec$,,l%) = str(account$,s%,l%) and          ~
                   ieflag$(i%) = "X" then return
L10700:     next i%
L10710:     accounts_qualifying% = 1%

        REM Load Account Balances...
            get #3, using L10750, descr$(r%), type$, tempbal()
L10750:     FMT XX(9), CH(30), CH(1), XX(4), 32*PD(14,4)
            if reverse$(r%) = "YES" then mat tempbal = (-1%)*tempbal

        REM Retrieve BUDGET information...
            mat tempbgt = zer
            call "READ100" (#2, saveacct$, f1%(2%))
                if f1%(2%) = 0%  then L11040
            get #2, using L10830, tempbgt()
L10830:     FMT XX(17), 39*PD(14,4)
            if reverse$(r%) = "YES" then mat tempbgt = (-1%)*tempbgt

        REM *************************************************************~
            *             Add to accumulator array                      *~
            *************************************************************

L11040: REM Get CURRENT YEAR data, adjust if PERIOD% > 13...

        REM Current Year Opening Balance...
            period% = period1%
            yearo = 0
            offset% = 0%
            if period% > 13% then offset% = 13%
            if type$ = "R" or type$ = "E" then L11140
            for i% = 1% to 15% + offset%
                if inst$(r%)="YO" then accum(r%)=accum(r%) + tempbal(i%)
                yearo = yearo + tempbal(i%)
            next i%

L11140: REM Current Period Opening Balance...
            periodo = 0
            period% = period1%
            offset% = 1%
            if type$ <> "R" and type$ <> "E" then L11190   /* P&L accts*/
*              IF PERIOD% = 14% THEN 11240          /* Implied Zero  */
                if period% > 14% then offset% = 29%  /* Year Open Bal */
L11190:     for i% = offset% to 14% + period%
                if inst$(r%)="PO" then accum(r%)=accum(r%) + tempbal(i%)
                periodo  = periodo  + tempbal(i%)
            next i%

        REM Current Period Activity...
            period% = period1%
L11250:     if inst$(r%)="PA" then                                       ~
                             accum(r%) = accum(r%) + tempbal(15%+period%)
            period% = period% + 1%
            if period% > period2% then L11270
                goto L11250

L11270: REM Previous Period Activity...
            np% = period2% - period1% + 1%              /* # of Periods */
            temp% = 0%
            for i% = period1% + 14% to period1% + 15% - np% step -1%
                if i% = 15% then temp% = temp% + 1%
                if i% - temp% = 14% and periods% <> 13%                  ~
                    then temp% = temp% + 1%
                if i% = 14% and periods% <> 13% then temp% = temp% + 1%
                if i% = 28% and periods% <> 13% then temp% = temp% + 1%
                if temp% > 2% then temp% = 2%
                if inst$(r%)="PPA" then                                  ~
                               accum(r%) = accum(r%) + tempbal(i% - temp%)
                next i%

        REM Previous Period Opening Balance...
            period% = period1%
            temp% = period% - 1%
            if temp% = 0% then temp% = -1%
            if temp% <> 13% and temp% <> -1% then L11312
                if periods% = 12% then temp% = temp% - 1%
L11312:     if inst$(r%)="PPO" then                                      ~
                     accum(r%) = accum(r%) + periodo - tempbal(15%+temp%)


        REM Current Period Current Balance...
            period% = period1%
            if bal% = 1% then currentbalance=periodo+tempbal(15%+period%)~
                       else currentbalance=tempbal(15%+period%)
L11400:     period% = period% + 1%
            if period% > period2% then L11408
                currentbalance = currentbalance + tempbal(15%+period%)
                 goto L11400
L11408:     if inst$(r%)="CB" then accum(r%) = accum(r%) + currentbalance

        REM Current Year Activity...
            if inst$(r%) <> "YA" then L11450
                if bal% = 1% then accum(r%) =                            ~
                                     accum(r%) + (currentbalance - yearo)~
                             else accum(r%) = accum(r%) + currentbalance

L11450: REM Get Prior Year data, adjust if PERIOD% > 13%...

        REM Previous Year Opening Balance...
            period% = period1%
            yearo = 0
            offset% = 1%
            if period% > 13% then offset% = 15%
            for i% = 1% to offset%
                if inst$(r%)="LYO" then accum(r%)=accum(r%)+tempbal(i%)
                yearo = yearo + tempbal(i%)
            next i%

        REM Previous Year Activity...
            period% = period1%
L11531:     offset% = 1%
            if period% > 13% then offset% = 2%
            if inst$(r%)="LPA" then                                      ~
                       accum(r%) = accum(r%) + tempbal(offset% + period%)
            period% = period% + 1%
            if period% > period2% then L11540
                goto L11531

L11540: REM Previous Year Period Opening Balance...
            period% = period1%
            periodo = 0
            offset% = 0%
            if period% > 13% then offset% = 1%
            for i% = 1% to period% + offset%
                if inst$(r%)="LPO" then accum(r%)=accum(r%) + tempbal(i%)
                periodo = periodo + tempbal(i%)
            next i%

        REM Previous Year Current Balance...
            period% = period1%  : currentbalance = 0
            offset% = 0%
            if period% > 13% then offset% = 1%
            if bal% = 1% then currentbalance =                           ~
                                    periodo + tempbal(1%+period%+offset%)~
                         else currentbalance = tempbal(1%+period%+offset%)

L11632:     period% = period% + 1%
            if period% > period2% then L11642
                if period% > 13% then offset% = 1%
            currentbalance = currentbalance + tempbal(1%+period%+offset%)
            goto L11632
L11642:     if inst$(r%)="LCB" then accum(r%)=accum(r%)+currentbalance

        REM Previous Year Activity...
            if inst$(r%) <> "LYA" then L11720
                if bal% = 1% then accum(r%) =                            ~
                                     accum(r%) + (currentbalance - yearo)~
                             else accum(r%) = accum(r%) + currentbalance
L11720:     if str(inst$(r%),,1%)  = "B"  or                             ~
               str(inst$(r%),1%,2%) = "PB" or                            ~
               str(inst$(r%),1%,2%) = "NB" then L11800 else goto L13000

L11800: REM BUDGET Calculations...
            if inst$(r%) = "PBPO" or inst$(r%) = "BPO" or                ~
               inst$(r%) = "NBPO" then period% = period1% else           ~
                                       period% = period2%
            offset% = 1%
            if period% > 13% then offset% = 14%
            for i% = offset% to period%
                if inst$(r%) = "PBPO" or inst$(r%) = "PBCB" or           ~
                   inst$(r%) = "PBYA" then                               ~
                                  accum(r%) = accum(r%) + tempbgt(i%)
                if inst$(r%) = "BPO" or inst$(r%) = "BCB" or             ~
                   inst$(r%) = "BYA" then                                ~
                                  accum(r%) = accum(r%) + tempbgt(13%+i%)
                if inst$(r%) = "NBPO" or inst$(r%) = "NBCB" or           ~
                   inst$(r%) = "NBYA" then                               ~
                             accum(r%) = accum(r%) + tempbgt(26%+i%)
            next i%

            period% = period1%
L11932:         if inst$(r%) = "PBPA" then                               ~
                                accum(r%) = accum(r%) + tempbgt(period%)
                if inst$(r%) = "BPA" then                                ~
                            accum(r%) = accum(r%) + tempbgt(13%+period%)
                if inst$(r%) = "NBPA" then                               ~
                            accum(r%) = accum(r%) + tempbgt(26%+period%)
            period% = period% + 1%
            if period% > period2% then L11952
                 goto L11932

L11952:     if inst$(r%) <> "PBYE" and inst$(r%) <> "BYE" then L12100
            period% = period2%
            offset% = 1%
            if period% > 13% then offset% = 14%
            for i% = offset% to offset% + 12%
                if inst$(r%) = "PBYE" then                               ~
                       accum(r%) = accum(r%) + tempbgt(i%)
                if inst$(r%) = "BYE" then                                ~
                       accum(r%) = accum(r%) + tempbgt(13%+i%)
             next i%
L12100:      goto L13000

L12500: REM *************************************************************~
            *          C A L C U L A T E   I N S T R U C T I O N        *~
            *                                                           *~
            * Calculates value                                          *~
            *************************************************************~

            type% = pos("+-/*%"=str(inst$(r%),2%,1%))
               if type% = 0% then return  /* Nothing to do? */

            on type% gosub L12630, L12680, L12730, L12780, L12830
            accounts_qualifying% = 1%
            if print$(r%) <> "NO" then print$(r%) = "SUM"
            gosub L13080
            return

L12630: REM ADD Function...
            gosub get_fields
            accum(r%) = round(accum(r%) + field1 + field2, 2%)
            return

L12680: REM SUBTRACT Function...
            gosub get_fields
            accum(r%) = round(accum(r%) + field1 - field2, 2%)
            return

L12730: REM DIVIDE Function...
            gosub get_fields
            if field2 = 0 then L12760
            accum(r%) = round(accum(r%) + (field1/field2), 2%)
L12760:     return

L12780: REM MULTIPLY Function...
            gosub get_fields
            accum(r%) = round(accum(r%) + (field1*field2), 2%)
            return

L12830: REM PERCENT Function...  This is for 'this year compared to last
*                               year' kinds of stuff.
            gosub get_fields
            if field2 = 0 then L12860
                field3 = field1 - field2
                if field3 = 0 then L12860
            accum(r%) = round(accum(r%) + (field3 / field2 * 100%), 2%)
L12860:     return

        get_fields
            if str(acct1$(r%),,1%) = "C" or str(acct1$(r%),,1%) = "T"    ~
               then L12930
            convert acct1$(r%) to field1
            goto L12955
L12930:     convert str(acct1$(r%),2%) to c%
            if str(acct1$(r%),,1%) = "C" then field1 = accum(c%)
            if str(acct1$(r%),,1%) = "T" then field1 = totals(c%+1%)
L12955:     if str(acct2$(r%),,1%) = "C" or str(acct2$(r%),,1%) = "T"    ~
               then L12975
            convert acct2$(r%) to field2
            return

L12975:     convert str(acct2$(r%),2%) to c%
            if str(acct2$(r%),,1%) = "C" then field2 = accum(c%)
            if str(acct2$(r%),,1%) = "T" then field2 = totals(c%+1%)
            return

L13000: REM *************************************************************~
            *                W R I T E   R E C O R D                    *~
            *                                                           *~
            * Writes accum1s to print tickler file, adds to totals.     *~
            *************************************************************

        REM Find Anything?...
            goto L13090
L13080:     printsum% = 1%
L13090:     if accounts_qualifying% = 0% then move_on

        REM 'PRINT_IF' constraint...
            if print_if$(r%) <> "+" then L13150
                if accum(r%)  < .001 then move_on

L13150:     if print_if$(r%) <> "-" then L13180
                if accum(r%)  > -.001 then move_on

L13180: REM Update Total Buckets...
            if print$(r%) = "SUM" and printsum% = 0% then L13310
            accum(r%) = round(accum(r%), 2%)

            for i% = 20% to 1% step -1%
                if str(total$(),((r%-1%)*20%)+i%,1%) = " " then L13290
                if str(total$(),((r%-1%)*20%)+i%,1%) = "0" then L13290
                     if round$ = "YES" then accum(r%)=round(accum(r%), 0%)
                     if str(total$(),((r%-1%)*20%)+i%,1%) = "+"          ~
                        then totals(i%) =round(totals(i%) + accum(r%), 2%)
                     if str(total$(),((r%-1%)*20%)+i%,1%) = "-"          ~
                        then totals(i%) =round(totals(i%) - accum(r%), 2%)
L13290:     next i%

L13310: REM Is this to be printed?...
            if print$(r%) = "NO" then accum(r%) = 0
            if accounts_qualifying% = 0% then print$(r%) = "NO "
            if print$(r%) = "NO " and directive% = 0% then move_on

            if str(record$,3%,colpos%(1%)-1%) <> " " then L13410
            if str(inst$(r%),1%,1%)="N" and str(inst$(r%),3%,1%)=" " and ~
                 print$(r%) = "ALL" then L13410
            if text$(r%)=" "then str(record$,3%,colpos%(1%)-1)=descr$(r%)~
                            else str(record$,3%,colpos%(1%)-1%)= text$(r%)
            if text$(r%) = " " then L13410
            if print$(r%)="SUM" then str(record$,3,colpos%(1)-1)=text$(r%)
L13410:     if print$(r%) = "SUM" and printsum% = 0% then return
            if print$(r%) = "NO " then return
            if round$ = "YES" then accum(r%) = round(accum(r%), 0%)
            if zero$ = "NO" and str(inst$(r%),1%,1%) <> "T"              ~
               and accum(r%) = 0 then move_on
            tempprt$ = " "
        REM Format accumulated value...
            if round$ = "YES" then accum(r%) = round(accum(r%), 0%)
            if round$ = "YES" and paren$="YES" and colcom$(r%) = "YES"   ~
               then put tempprt$ using L13620, accum(r%)
L13620:            % ####,###,###,###
            if round$ = "YES" and paren$ = "NO" and colcom$(r%) = "YES"  ~
               then put tempprt$ using L13650, accum(r%)
L13650:            % -###,###,###,###
            if round$ = "YES" and paren$ = "YES" and colcom$(r%) = "NO"  ~
               then put tempprt$ using L13680, accum(r%)
L13680:            %    #############
            if round$ = "YES" and paren$ = "NO" and colcom$(r%) = "NO"   ~
               then put tempprt$ using L13710, accum(r%)
L13710:            %    -############
            if round$ = "NO" and paren$ = "YES" and colcom$(r%) = "YES"  ~
               then put tempprt$ using L13750, accum(r%)
L13750:            %##,###,###,###.##
            if round$ = "NO" and paren$ = "NO" and colcom$(r%) = "YES"   ~
               then put tempprt$ using L13780, accum(r%)
L13780:            %-#,###,###,###.##
            if round$ = "NO" and paren$ = "YES" and colcom$(r%) = "NO"   ~
               then put tempprt$ using L13810, accum(r%)
L13810:            %   ###########.##
            if round$ = "NO" and paren$ = "NO" and colcom$(r%) = "NO"    ~
               then put tempprt$ using L13840, accum(r%)
L13840:            %   -##########.##
        REM Add parenthesis to formatted value...
            pu% = 0%                            /* Init Paren Flag    */
            if paren$ = "NO" then L13920
            if accum(r%) < -.005 then L13906 else L13920
L13906:         call "STRING" addr("LJ", tempprt$, 17%)
                call "PUTPAREN" (tempprt$)
                call "STRING" addr("RJ", tempprt$, 17%)
                pu% = 1%                        /* This Amt Has Parens */
L13920: REM Add '%' or '$' signs to formatted value and SQUISH...
            if str(inst$(r%),1%,1%) = "T" then dol%(r%) = 0%
            if colfmt$(r%) = "$" and dol%(r%) = 0% then                  ~
                str(tempprt$,1%,1%) = "$"
            if colfmt$(r%) = "%" and dol%(r%) = 0% then                  ~
                str(tempprt$,1%,1%) = "%"
            if str(inst$(r%),1%,1%) <> "T" then dol%(r%) = 1%
            call "SPCSMASH" (tempprt$)
            if paren$ = "YES" and pu% = 0%                               ~
                then call "STRING" addr("RJ", tempprt$, 16%)             ~
                else call "STRING" addr("RJ", tempprt$, 17%)

        REM Put formatted value to RECORD$...
            str(record$,colpos%(r%),collgth%(r%)) =                      ~
                                    str(tempprt$, 18% - collgth%(r%))
            goto move_on

L13980:     if inst$(r%)="U" then str(record$,colpos%(r%),collgth%(r%))  ~
                   = all("-")
L14000:     if inst$(r%)="DU"then str(record$,colpos%(r%),collgth%(r%))  ~
                   = all("=")

L14030:     FMT BI(4),      /* Sequence Number                         */~
                CH(132)     /* Print Record                            */

        move_on
            accounts_qualifying% = 0%
            return

        loop
            if directive%=0 and str(record$,colpos%(1%),) = " " then L14150
            if inst$(1%) = "H   " or inst$(1%) = "P    " or              ~
               inst$(1%) = "D   " or inst$(1%) = "DC   " or              ~
               inst$(1%) = "U   " or inst$(1%) = "DU   " then L14120
            for i% = 1% to columns%
                if accum(i%) <> 0 then L14120
            next i%
                if zero$ = "NO" then L14150
L14120:     seq% = seq% + 1%
            write #9, using L14030, seq%, record$
            returncode% = 0%
L14150:     record$ = " "
            mat accum  = zer
            return

L65000: REM *************************************************************~
            *                E X I T  S U B R O U T I N E               *~
            *                                                           *~
            * Returns Control To Caller, Will Zero Indicated Totals.    *~
            *************************************************************

        REM Zero Totals As Required...
            for r% = 1% to columns%
              for i% = 20% to 1% step -1%
                if str(total$(),((r%-1)*20%)+i%,1%) <> "0" then L65120
                totals(i%) = 0
L65120:       next i%
            next r%
            end
