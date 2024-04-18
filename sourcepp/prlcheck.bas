        REM *************************************************************~
            *                                                           *~
            *  PPPP   RRRR   L       CCC   H   H  EEEEE   CCC   K   K   *~
            *  P   P  R   R  L      C   C  H   H  E      C   C  K  K    *~
            *  PPPP   RRRR   L      C      HHHHH  EEEE   C      KKK     *~
            *  P      R   R  L      C   C  H   H  E      C   C  K  K    *~
            *  P      R   R  LLLLL   CCC   H   H  EEEEE   CCC   K   K   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLCHECK - Prints Payroll Checks And Direct Deposit Slips.*~
            *            Handles forms line up and next item number.    *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/27/81 ! ORIGINAL                                 ! BCW *~
            * 05/18/81 ! Pay Period Dates From SYSFILE2           ! TEM *~
            * 08/20/81 ! DATE ON CHECKS FROM SYSFILE2             ! TEM *~
            * 09/01/81 ! Rounding on cents in check amount        ! TEM *~
            * 10/19/81 ! USE RANGE TO PROCESS                     ! JAM *~
            * 10/06/86 ! Support Standard Forms & Direct Deposit  ! HES *~
            * 05/13/88 ! Added Sorting Option & misc cleanup      ! RJM *~
            * 06/24/88 ! Corrected city, state, zip string        ! MJB *~
            * 07/08/88 ! Modified to allow reporting of pay rates ! MDE *~
            *          !   & fix non-cur period accum to OTHER    !     *~
            * 10/21/88 ! Fix for deduction stacks.                ! JDH *~
            * 01/15/89 ! Fix Deposit Slips, Trucation, Lost       ! kab *~
            *          ! Deposit Slips. Print Earnings when <> 0  !     *~
            * 08/03/89 ! Modified to print Ded YTD if current = 0 ! MJB *~
            * 12/09/91 ! PRR 12144- Dept/Shift/Employee option not! JBK *~
            *          ! printing deposit slips                   !     *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            * 09/25/95 ! PRR 13448.  Added Print Mode 'P' for     ! JBK *~
            *          !  UNIX platforms only.                    !     *~
            * 09/29/97 ! Changes for the year 2000.               ! DXL *~
            *************************************************************

        dim                                                              ~
            answer$1,                    /* AOK ON PRINTING?           */~
            address$(3)30,               /* EMPLOYEE'S HOME ADDRESS    */~
            autopay$1,                   /* AUTOMATIC PAYROLL FLAG     */~
            bankacct$20,                 /* BANK Account Number        */~
            cash$1,                      /* PAID IN CASH? FLAG         */~
            cashacct$9,                  /* CASH IN BANK ACCOUNT       */~
            checkdate$10,                /* CHECK DATE (POSTING DATE)  */~
            checkdateu$6,                /* CHECK DATE (POSTING DATE)  */~
            checknr$8,                   /* CHECK NUMBER THIS CHECK    */~
            city$20,                     /* Employee City              */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            dedamt(10,2),                /* DEDUCTION AMOUNTS          */~
            dedempflag$1,                /* EMPLOYEE PAYS THIS ONE?    */~
            dedmethod$6,                 /* METHOD OF DEDUCTION        */~
            deductflag$1,                /* PROCESS ON THIS RUN?       */~
            deduction$12,                /* DEDUCTION DESCRIPTION      */~
            deduction$(10)12,            /* DEDUCTION DESCRIPTIONS STK */~
            department$4,                /* DEPARTMENT CODE THIS GUY   */~
            dept$(2)4,                   /* DEPARTMENT RANGE           */~
            direct$(5)17,                /* DIRECT DEPOSIT ACCOUNTS    */~
            diramt(6,2),                 /* DIRECT DEPOSIT AMOUNTS     */~
            dirbank$(5)4,                /* DIRECT DEPOSIT BANK CODES  */~
            earnamt(9,4),                /* EARNINGS AMOUNTS AS LOADED */~
            earntype$(9)12,              /* EARNINGS TYPES FOR PRINT   */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            empcode$12,                  /* EMPLOYEE CODE INFO         */~
            empcode$(2)12,               /* EMPLOYEE CODE RANGE        */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            firstcheck$8,                /* FIRST CHECK NUMBER         */~
            firstpaydate$8,              /* FIRST DATE PAY PERIOD      */~
            firstslip$7,                 /* FIRST DEPOSIT SLIP NUMBER  */~
            fstatus$1,                   /* FEDERAL FILING STATUS      */~
            grossacct$9,                 /* GROSS PAY ACCOUNT          */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            junk$8,                      /* WORK VARIABLE              */~
            lastpaydate$8,               /* LAST DATE THIS PAY PERIOD  */~
            lastcheck$8,                 /* LAST CHECK TO PRINT        */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            line2$79,                    /* Screen Line 2              */~
            masks$(2)2,                  /* NUMBER OF MASKS TO PRINT   */~
            maskmsg$36,                  /* CURRENT PRINT MODE MESSAGE */~
            name$62,                     /* NAME OF EMPLOYEE           */~
            nextcheck$8,                 /* NEXT AVAILABLE CHECK NO.   */~
            nextslip$8,                  /* NEXT DEPOSIT SLIP NUMBER   */~
            normalhrs$2,                 /* NORMAL HOURS PER DAY       */~
            normweekhrs$2,               /* NORMAL HOURS PER WEEK      */~
            payfreq$1,                   /* PAY FREQUENCY (1-7)        */~
            payfreq$(2)1,                /* PAY FREQUENCY RANGE        */~
            paymode$1,                   /* HOW THIS GUY IS PAID       */~
            period$(10)10,               /* PAY PERIOD DATES           */~
            pf16$16,                                                     ~
            primstate$2,                 /* PRIMARY STATE OF EMPLOYMENT*/~
            printmode$1,                 /* DEFAULT PRINT MODE         */~
            rateflag$1,                  /* Print pay rates flag       */~
            readkey$50,                  /* KEY FOR PLOW ROUTINES      */~
            status$1,                    /* EMPLOYEE'S STATUS CODE     */~
            shiftcode$1,                 /* TIME CARD SHIFT CODE       */~
            sstatus$1,                   /* STATE FILING STATUS        */~
            state$2,                     /* Employee State             */~
            txbl$1,                      /* TAXABLE? THIS EARNINGS REC */~
            type$12,                     /* EARNINGS TYPE CODE         */~
            wkcomdef$10,                 /* WORK COMP CODE             */~
            wkstncode$4,                 /* WORKSTATION CODE           */~
            zip$9,                       /* Employee Zip Code          */~
            userid$3                     /* USERID OF CURRENT USER     */~

        dim                      /* PERSONNEL STUFF                    */~
            lname$15,            /* Last name of person - part of pers */~
            fname$10,            /* First name of person               */~
            mname$1,             /* Middle name of person              */~
            ssnumber$11,         /* Social security number             */~
            telephone$10,        /* Telephone number                   */~
            county$20,           /* County in address                  */~
            union$16,            /* Union status - personnel system    */~
            curjob$16,           /* Current job title - personnel syst */~
            curjobeeo$16         /* EEO class of current job - personn */

        dim file$8,                      /* FILE NAME                  */~
            lib$8,                       /* LIB  NAME                  */~
            vol$6,                       /* VOLUME                     */~
            record$64,                   /* FOR SORT FILE              */~
            sort$116                     /* FOR SORTCALL               */

        dim f1%(64)                      /* RECORD-ON-FILE FLAGS       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !  DESCRIPTION                             *~
            *-----+----------+------------------------------------------*~
            * # 1 ! SYSFILE2 ! SYSTEM INFORMATION FILE (PAYROLL DEFAULT)*~
            * # 2 ! USERINFO ! USER INFORMATION FILE (PAYROLL DATE)     *~
            * # 3 ! EMPMASTR ! EMPLOYEE FILE MASTER RECORDS.            *~
            * # 4 ! EMPEARN1 ! EMPLOYEE EARNINGS DETAILS FILE           *~
            * # 5 ! EMPDEDXN ! EMPLOYEE DEDUCTIONS FILE.                *~
            * # 6 ! PRLCHTIF ! INTERIM FILE CONTAINING CHECK NUMBERS    *~
            * # 7 ! PRLCHREF ! Earntype cross reference for check printi*~
            * #14 ! PERMASTR ! PERRSONNEL MASTER FILE                   *~
            * #15 ! PRLCHK   ! Payroll Checks Master File               *~
            * #60 ! SORTFILE ! File for sorting pay checks              *~
            *************************************************************

            select #1,  "SYSFILE2",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 500,                                  ~
                         keypos = 1, keylen = 20

            select #2,  "USERINFO",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 150,                                  ~
                         keypos = 1, keylen = 3

            select #3,  "EMPMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 136,                                  ~
                         keypos = 1, keylen = 12,                        ~
                         alt key  1, keypos = 70, keylen =  1, dup

            select # 4, "EMPEARN1",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 200,                                  ~
                         keypos = 1, keylen = 15,                        ~
                         alternate key  1, keypos =  16, keylen = 28

            select # 5, "EMPDEDXN",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 500,                                  ~
                         keypos = 1, keylen = 15,                        ~
                         alternate key  1, keypos =  16, keylen = 18, dup

            select # 6, "PRLCHTIF",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 50,                                    ~
                        keypos = 1, keylen = 12

            select # 7, "PRLCHREF",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 50,                                    ~
                        keypos = 1, keylen = 14,                         ~
                        alt key 1, keypos = 15, keylen = 12

            select #14, "PERMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 950,                                   ~
                        keypos = 39, keylen = 12,                        ~
                        alt key  1, keypos =  28, keylen = 23,           ~
                            key  2, keypos =   2, keylen = 49,           ~
                            key  3, keypos =   1, keylen = 50

            select #15, "PRLCHK",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 120,                                   ~
                        keypos =   1, keylen = 23,                       ~
                        alt key  1, keypos =  13, keylen =  11,          ~
                            key  2, keypos =  42, keylen =   9, dup

            select #60, "SORTFILE",                                      ~
                         varc,                                           ~
                         consec,                                         ~
                         recsize = 64                                    ~


*        Check to See if Payroll/Personnel is Active
            call "PRLEXTSB" ("PRL", prl%)
                if prl% = 99% then end (prl%)

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#2, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#3, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#4, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#5, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#6, 0%, 0%, 100%, " ")
            call "OPENCHCK" (#7, 0%, 0%, 100%, " ")
            call "OPENCHCK" (#14, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#15, 0%, 0%, 0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * ABOUT ALL WE HAVE TO SET HERE IS USERID FOR THE PLOW      *~
            * ROUTINE, AND SELECT PRINTED OUTPUT TO THE PRINTER.        *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)
            call "EXTRACT" addr("ID", userid$)

            REM Get Users Payroll Date For Use As Check Date...
                call "READ100" (#2, userid$, f1%(2))
                     if f1%(2) <> 0 then L09170
                     call "ASKUSER" (0%, "Sorry", "Your Are Not Set Up To~
        ~ Run In This Data Base", " ", "Press ENTER To Exit")
                     goto L65000
L09170:         get #2, using L09180, checkdate$
L09180:         FMT XX(15), CH(6)
                checkdateu$ = checkdate$
                call "DATFMTC" (checkdate$)

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press RETURN."

            str(line2$,62,18) = "PRLCHECK: " & str(cms2v$,1,8)

            REM GET PAYROLL PERIOD DATES
                call "READ100" (#1, "PAY PERIODS", f1%(1))
                      if f1%(1) = 0 then L09320
                get #1, using L09300 , str(period$(), 1)
L09300:                 FMT XX(20), CH(91)

L09320:     REM GET RANGE TO PROCESS
                gosub L34000

            REM SET-UP FOR SORT OPTION
                call "GETNAMES" addr(#3, file$, lib$, vol$)
                call "READFDR" addr(file$, lib$, vol$, 0%, "RC",         ~
                                    records%, ret%)

        inputmode

            nextcheck$, firstcheck$, lastcheck$, nextslip$, firstslip$,  ~
            masks$(), rateflag$ = " "

            select printer (134)
            call "SETPRNT" ("PRL011", " ", 0%, 0%)
            call "EXTRACT" addr("PM", printmode$)
            close printer
            call "SETPRNT" (" ", " ", 0%, 1%)

            on pos("SOHKP" = printmode$) goto L09490, L09510, L09540, L09560, L09522
               goto L65000

L09490:     maskmsg$ = "Current Print Mode is 'SPOOLED'    "
                goto L12000
L09510:     maskmsg$ = "Current Print Mode is 'ON-LINE'    "
                goto L12000
L09522:     maskmsg$ = "Current Print Mode is 'PRINT-KEEP' "
                goto L12000

L09540:     maskmsg$ = "Current Print Mode is 'HOLD'    "
                goto L10000
L09560:     maskmsg$ = "Current Print Mode is 'KEEP'    "
                goto L10000

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            * SECTION FOR HOLD OR KEEP                                  *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

            init(" ") errormsg$, inpmessage$

            for fieldnr% = 1% to  6%
                gosub'051(fieldnr%)
                      if enabled% = 0% then L10150
L10110:         gosub'101(fieldnr%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  = 16% and fieldnr% = 1% then L65000
                      if keyhit% <>  0% then       L10110
L10150:         gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10110
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

            inpmessage$ = edtmessage$
L11060:     gosub'101(0%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       L11220
                  if keyhit% <>  0% then       L11060
            fieldnr% = cursor%(1) - 5%
            if fieldnr% < 1% or fieldnr% >  8% then L11060
               if fieldnr% > 2% then fieldnr% = fieldnr% - 1%
               if fieldnr% > 2% then fieldnr% = fieldnr% - 1%

L11150:     gosub'101(fieldnr%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11150
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11150
            goto L11060

L11220: REM HANDLE MASK REQUESTS
            convert masks$(1%) to masks%
            if masks% = 0% then L11280
            masktype% = -1%
            for i% = 1% to masks%
                gosub print_mask
            next i%
L11280:     goto print_checks

L12000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            * SECTION FOR SPOOLED OR ON-LINE                            *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

            init(" ") errormsg$, inpmessage$

            for fieldnr% = 1% to  6%
                gosub'052(fieldnr%)
                      if enabled% = 0% then L12170
L12110:         gosub'102(fieldnr%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  = 16% and fieldnr% = 1% then L65000
                      if keyhit% <>  0% then       L12110
                gosub'152(fieldnr%)
                      if errormsg$ <> " " then L12110
L12170:     next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

            inpmessage$ = edtmessage$
L13060:     gosub'102(0%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       print_checks
                  if keyhit% <>  0% then       L13060
            fieldnr% = cursor%(1) - 5%
            if fieldnr% < 1% or fieldnr% >  7% then L13060
               if fieldnr% > 2% then fieldnr% = fieldnr% - 1%
               if fieldnr% > 2% then fieldnr% = fieldnr% - 1%

L13140:     gosub'102(fieldnr%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L13140
            gosub'152(fieldnr%)
                  if errormsg$ <> " " then L13140
            goto L13060

        REM *************************************************************~
            * MASK PRINTING CONTROL SECTION                             *~
            *************************************************************

        print_mask
                call "PRLCHPRT" (masktype%, empcode$, checknr$,          ~
                            junk$, earntype$(), earnamt(), deduction$(), ~
                            dedamt(), name$, address$(), firstpaydate$,  ~
                            lastpaydate$, ssnumber$, curjob$, curjobeeo$,~
                            union$, direct$(), dirbank$(), diramt(),     ~
                            rateflag$)
            close printer
            call "SETPRNT" (" ", " ", 0%, 1%)
            return

        REM *************************************************************~
            *                   M A I N   L O O P                       *~
            *                                                           *~
            * PRINT D DATA.                                             *~
            *************************************************************~

        print_checks
            call "SHOSTAT" ("Printing Payroll Checks")
            call "DELETE" (#6, hex(00), 0%)

        REM First Print Checks...
            empcode$ = " "
            mode% = 0
            nextcheck% = firstcheck% - 1
            gosub L16000
            close printer
            call "SETPRNT" (" ", " ", 0%, 1%)
            if checks% = 0 then L15310

        REM Update last check number used...
            convert nextcheck% + 1 to nextcheck$, pic(########)
            nextslip$ = "0000001"
            call "READ101" (#1, "NEXT PAYROLL CHECK #", f1%(1))
                if f1%(1) = 0 then L15270
            get #1, using L15250, nextslip$
L15250:     FMT XX(28), CH(7)
            delete #1
L15270:     write #1, using L15290, "NEXT PAYROLL CHECK #", nextcheck$,   ~
                                                      nextslip$, " ", " "
L15290:     FMT CH(20), CH(8), CH(7), CH(215), CH(250)

L15310: REM Now Print Direct Deposit Slips...
            if direct$ = " " then L65000
            call "SHOSTAT" ("Printing Direct Deposit Slips")
            convert masks$(2) to masks%
            if masks% = 0% then L15400
               masktype% = -2
               for i% = 1% to masks%
                   gosub print_mask
               next i%
L15400:     empcode$ = " "
            mode% = 1
            nextslip% = firstslip% - 1
            reopen% = 1%
            gosub L16000
            close printer
            call "SETPRNT" (" ", " ", 0%, 1%)
            if slips% = 0 then L65000    /* Shouldn't Happen */

            REM Update last slip number used...
            convert nextslip% + 1 to nextslip$, pic(0000000)
            nextcheck$ = "       1"
            call "READ101" (#1, "NEXT PAYROLL CHECK #", f1%(1))
                if f1%(1) = 0 then L15560
            get #1, using L15540, nextcheck$
L15540:     FMT XX(20), CH(8)
            delete #1
L15560:     write #1, using L15580, "NEXT PAYROLL CHECK #", nextcheck$,   ~
                                                      nextslip$, " ", " "
L15580:     FMT CH(20), CH(8), CH(7), CH(215), CH(250)
            goto L65000

L16000: REM *************************************************************~
            *                  P R O C E S S I N G                      *~
            *                                                           *~
            * PLOWS THROUGH EMPLOYEES WITH THE EARNINGS FLAG SET.       *~
            * SINCE THIS IS THE CHECK PRINTING RUN, WE PRINT FOR ALL    *~
            * THAT HAVE THIS FLAG.  THEN WE LATER ON IN THE CHAIN RESET *~
            * THE FLAG.  NOTE ALSO THAT WE PLOW DOWN THE EMPLOYEE INDEX *~
            * RATHER THAN THE DEDUCTION FLAG ALT KEY, SO THAT THE CHECKS*~
            * COME OUT IN EMPLOYEE NUMBER ORDER.                        *~
            *************************************************************

            REM INITIALIZE STACKS FOR THIS EMPLOYEE
                earntype$(), deduction$(), direct$(), dirbank$() = " "
                mat earnamt = zer  : mat dedamt = zer : mat diramt = zer
                dir%, eptr%, dptr% = 0

            REM GET NEXT EMPLOYEE CODE. IF FLAG SET, PROCESS, ELSE SKIP.
                if sort_option% = 0% then gosub L17900  else gosub L17000
                     if f1%(3) = 0 then return

            REM LOAD EMPLOYEE EARNINGS RECORDS AND POST TO STACK
                net, ytdnet, ytd_other = 0
                readkey$ = empcode$

L16330:         call "PLOWNEXT" (#4, readkey$, 12%, f1%(4))
                     if f1%(4) = 0 then L16450
                gosub L31000              /* LOAD EARNINGS RECORD.      */
                if cash$ <> "Y" then L16330
                if amount <> 0 then L16400
                   ytd_other = ytd_other + ytdamount
                if ytdamount = 0 then L16420
L16400:         gosub'162(type$, units, amount, ytdamount)
                net = net + amount
L16420:         ytdnet = ytdnet + ytdamount + amount
                goto L16330

L16450:     REM LOAD EMPLOYEE DEDUCTIONS RECORDS AND POST TO STACK.
                if net <= 0 then L16000  /* NO EARNINGS */
                convert payfreq$ to temp%, data goto L16000
                firstpaydate$ = str(period$(), 2 + (temp%-1)*13, 6)
                lastpaydate$  = str(period$(), 8 + (temp%-1)*13, 6)
*               call "DATEFMT" (firstpaydate$)
*               call "DATEFMT" (lastpaydate$)
        REM     IF YTD_OTHER = 0 THEN 16550             /* YTD info   */
        REM        GOSUB'162("OTHER", 0, 0, YTD_OTHER)  /* for things */
                                                        /* not paid   */
                readkey$ = empcode$
                ytd_other = 0
                on mode% goto L18000

L16590:         call "PLOWNEXT" (#5, readkey$, 12%, f1%(5))
                     if f1%(5) = 0 then L16710      /* IF END OF RECORD */
                gosub L32000              /* LOAD DEDUCTIONS RECORD     */
                if dedempflag$ <> "Y" then L16590   /* IF EMPLOYER PAYS */
                if amount > 0 then L16660
                   ytd_other = ytd_other + ytdamount
        REM        GOTO 16590
L16660:         if dedmethod$ = "DIRECT" then direct$ = "X"
                gosub'163(deduction$, amount, ytdamount)
                net = net - amount
                goto L16590

L16710:     REM NOW, IF NET PAY DUE, PRINT CHECK
                if net <= 0 then L16000
        REM     IF YTD_OTHER <> 0 THEN GOSUB'163("OTHER", 0, YTD_OTHER)
                nextcheck% = nextcheck% + 1       /* NEXT CHECK NUMBER*/
                convert nextcheck% to checknr$, pic(########)

                call "PRLCHPRT" (1%, empcode$, checknr$, checkdateu$,     ~
                            earntype$(), earnamt(), deduction$(),        ~
                            dedamt(), name$, address$(), firstpaydate$,  ~
                            lastpaydate$, ssnumber$, curjob$, curjobeeo$,~
                            union$, direct$(), dirbank$(), diramt(),     ~
                            rateflag$)
                write #6, using L16830, empcode$, checknr$, " "
L16830:         FMT CH(12), CH(8), CH(30)
                checks% = 1
                if nextcheck% = lastcheck% then empcode$(2) = empcode$
                goto L16000            /* PROCESS NEXT EMPLOYEE      */

L17000: REM *************************************************************~
            *          S E L E C T   N E X T   E M P L O Y E E          *~
            *                                                           *~
            * SPECIAL HANDLING OF SORT OPTIONS.                         *~
            *************************************************************

            if scount% > 0% then L17352
        REM ----  SORT EMPLOYEE LIST BY DEPT,SHIFT,NAME,EMPLOYEE CODE
            sort$ = " "
            scount% = 0%
            call "WORKOPN2" (#60, "OUTPT", records%, f1%(60))
L17120:     gosub L17900
                 if f1%(3%) = 0% then L17160
            on sort_option% gosub L17500
            goto L17120
L17160:     close #60

            call "GETNAMES" addr(#60, file$, lib$, vol$)
            str(sort$,1%,8%) = file$
            str(sort$,9%,8%) = lib$
            str(sort$,17%,6%) = vol$
            str(sort$,23%,22%) = str(sort$,,22%)
            call "SORTCALL" addr(sort$, ret%)
            if ret% = 0% then L17360
               k% = 2%
               call "ASKUSER" (k%, "*** SORT FAILURE ***",               ~
                "Either unable to link to SORT (not found or protected)",~
                "or SORT failed for some other reason (not enuff space)."~
                ,"Press RETURN to acknowlege & exit.")
               checks% = 0%
               goto exit_program

L17352:     if mode%    = 0% then L17400
            if reopen% <> 1% then L17400
            close #60
            reopen%     = 0%

L17360:     call "WORKOPN2" (#60, "INPUT", records%, f1%(60))

L17400:     call "READNEXT" (#60, f1%(60))
                if f1%(60%) <> 0% then L17440
                     f1%(3%) = 0%
                     return              /* ALL DONE */
L17440:     get #60, using L17445, empcode$
L17445:         FMT XX(31), CH(12)
            call "READ100" (#3, empcode$, f1%(3%))
                if f1%(3%) = 0% then L17400       /* BETTER NOT HAPPEN */
            gosub L30000                /* LOAD EMPLOYEE MASTER RECORD */
            gosub load_per_stuff
        return

L17500:     REM WRITE TO SORT FILE W/ DEPT, SHIFT, NAME & EMPLOYEE CODE
                shiftcode$ = " "
                convert shiftcode% to shiftcode$, pic(#)
                record$ = department$
                str(record$, 5, 1) = shiftcode$
                str(record$, 6,15) = lname$
                str(record$,21,10) = fname$
                str(record$,31, 1) = mname$
                str(record$,32,12) = empcode$
                if scount% > 0% then L17640
                     str(sort$,45%,9%) = "0001004CA"  /* Department    */
                     str(sort$,54%,9%) = "0005001CA"  /* Shift code    */
                     str(sort$,63%,9%) = "0006015CA"  /* Last Name     */
                     str(sort$,72%,9%) = "0021010CA"  /* First Name    */
                     str(sort$,81%,9%) = "0031001CA"  /* Middle Initial*/
                     str(sort$,90%,9%) = "0032012CA"  /* Employee Code */
L17640:         write #60, record$
                scount% = scount% + 1%
            return

L17900:     REM GET NEXT EMPLOYEE CODE. IF FLAG SET, PROCESS, ELSE SKIP.
                call "PLOWNEXT" (#3, empcode$, 0%, f1%(3%))
                     if f1%(3%) = 0% then return
                gosub L30000              /* LOAD EMPLOYEE MASTER RECORD*/
                if deductflag$ <> "Y" then L17900   /*  NO EARNINGS   */
                if empcode$ <= empcode$(1%) then L17900
                gosub load_per_stuff
                     if f1%(14%) = 0% then L17900   /* NO PER DATA    */
                if empcode$ <= empcode$(2%) then L17940
                     f1%(3%) = 0%
                     return
L17940:         if department$ <= dept$(1%) then L17900
                if department$ >  dept$(2%) then L17900
                if payfreq$ <= payfreq$(1%) then L17900
                if payfreq$ >  payfreq$(2%) then L17900
            return

L18000: REM *************************************************************~
            *               D E P O S I T   S L I P S                   *~
            *                                                           *~
            * SPECIAL HANDLING OF DEDUCTIONS STACK, ALONG WITH A        *~
            * SPECIAL CALL TO PRINT SUB IS ALL THATS REQUIRED TO GET    *~
            * THE SLIP PRINTED ON THE CAELUS STANDARD FORM              *~
            *************************************************************

                direct$ = "X"
L18090:         call "PLOWNEXT" (#5, readkey$, 12%, f1%(5))
                     if f1%(5) = 0 then L18320      /* IF END OF RECORD */
                gosub L32000              /* LOAD DEDUCTIONS RECORD     */
                if dedempflag$ <> "Y" then L18090   /* IF EMPLOYER PAYS */
                if amount > 0 then L18190
                   if dedmethod$ <> "DIRECT" then L18270
                      diramt(6,2) = diramt(6,2) + ytdamount
                      goto L18290
                   ytd_other = ytd_other + ytdamount
                   goto L18290
L18190:         if dedmethod$ <> "DIRECT" then L18270
                   direct$ = "Y"
                   dir% = min(dir%+1, 5) /* Don't need MIN in theory */
                   direct$(dir%) = bankacct$
                   dirbank$(dir%) = bank$
                   diramt(dir%,1) = amount
                   diramt(dir%,2) = ytdamount + amount
                   goto L18280
L18270:         gosub'163(deduction$, amount, ytdamount)
L18280:         net = net - amount
L18290:         ytdnet = ytdnet - (ytdamount + amount)
                goto L18090

L18320:     REM NOW, IF DEPOSITS WILL BE MADE, PRINT DEPOSIT SLIP
                if direct$ <> "Y" then L16000         /* No Deposits */
                if net = 0 then L18560
                   temp$ = " "
                   dptr% = dptr% + 1
                   if dptr% < 2 then L18480
                   if dptr% < 11 then L18430
                     dedamt(10,1) = dedamt(10,1) + dedamt(9,1)
                     dedamt(10,2) = dedamt(10,2) + dedamt(9,2)
                     deduction$(10) = "OTHER"
                     dptr% = 9
L18430:            for i% = dptr% to 2% step -1%
                     deduction$(i%) = deduction$(i%-1)
                     dedamt(i%,1) = dedamt(i%-1,1)
                     dedamt(i%,2) = dedamt(i%-1,2)
                   next i%
L18480:            call "READ101" (#6, empcode$, f1%(6))
                     if f1%(6) = 0 then L18520
                   get #6, using L18510, temp$
L18510:            FMT POS(13), CH(8)
L18520:            deduction$(1) = "CHK#" & temp$
                   call "SPCESMSH" (deduction$(1), 1%)
                   dedamt(1,1) = net
                   dedamt(1,2) = ytdnet
L18560: REM     IF YTD_OTHER <> 0 THEN GOSUB'163("OTHER", 0, YTD_OTHER)
                nextslip% = nextslip% + 1       /* NEXT SLIP NUMBER*/
                if nextslip% = 10000000% then nextslip% = 1%
                convert nextslip% to nextslip$, pic(0000000)
                checknr$ = "D" & str(nextslip$,,7)
                call "REDALT0" (#15, checknr$, 1%, f1%(15))
                     if f1%(15) <> 0% then L18560

                call "PRLCHPRT" (2%, empcode$, checknr$, checkdateu$,     ~
                            earntype$(), earnamt(), deduction$(),        ~
                            dedamt(), name$, address$(), firstpaydate$,  ~
                            lastpaydate$, ssnumber$, curjob$, curjobeeo$,~
                            union$, direct$(), dirbank$(), diramt(),     ~
                            rateflag$)
                checks%, slips% = 1
                call "READ101" (#6, empcode$, f1%(6))
                     if f1%(6) = 0 then L18770
                put #6, using L18730, checknr$
L18730:         FMT POS(21), CH(30)
                rewrite #6
                goto L16000            /* PROCESS NEXT EMPLOYEE      */

L18770:         write #6, using L18780, empcode$, " ", checknr$
L18780:         FMT CH(12), CH(8), CH(30)
                goto L16000            /* PROCESS NEXT EMPLOYEE      */

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20130,         /* No. of Masks     */~
                                    L21170,         /* Check # Range    */~
                                    L21480,         /* No. of Masks     */~
                                    L21520,         /* Slip # Range     */~
                                    L21650,         /* Sort Option      */~
                                    L21690          /* Rate Flag        */
                     return
L20130:     REM DEFAULT/ENABLE FOR Number of Check Masks to Print
                masks$(1) = "1"
                return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 2 OF INPUT. *~
            *************************************************************

            deffn'052(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L21130,         /* Print Mask       */~
                                    L21170,         /* Check # Range    */~
                                    L21480,         /* No. of Masks     */~
                                    L21520,         /* Slip # Range     */~
                                    L21650,         /* Sort Option      */~
                                    L21690          /* Rate Flag        */
                     return
L21130:     REM DEFAULT/ENABLE FOR Print a Check Mask?
                answer$ = "Y"
                return

L21170:     REM DEFAULT/ENABLE FOR Check Number Range
                firstcheck%, firstslip% = 1%
                call "READ100" (#1, "NEXT PAYROLL CHECK #", f1%(1))
                     if f1%(1) = 0 then L21270      /* ERROR MESSAGE    */
                get #1, using L21220, firstcheck$, firstslip$
L21220:         FMT XX(20), CH(8), CH(7)
                convert firstcheck$ to firstcheck%, data goto L21240
L21240:         convert firstslip$ to firstslip%, data goto L21250
L21250:         firstslip$ = " "

L21270:         convert firstcheck% to nextcheck$, pic(########)

                call "REDALT0" (#15, nextcheck$, 1%, f1%(15))
                     if f1%(15) = 0% then L21350
L21310:         firstcheck% = firstcheck% + 1%
                if firstcheck% > 99999999% then firstcheck% = 1
                goto L21270

L21350:         readkey$ = str(nextcheck$,,8) & hex(ffffff)
                lastcheck% = 99999999%
                call "PLOWALTS" (#15, readkey$, 1%, 0%, f1%(15))
                     if f1%(15) = 0% then L21400
                convert str(readkey$,,8) to lastcheck%, data goto L21400
L21400:         lastcheck% =max(firstcheck%,min(lastcheck%-1%,99999999%))
                if firstcheck% = lastcheck% then L21310
                convert firstcheck% to firstcheck$, pic(########)
                convert lastcheck% to lastcheck$, pic(########)
                call "STRING" addr ("LJ", firstcheck$, 8%)
                call "STRING" addr ("LJ", lastcheck$, 8%)
                return

L21480:     REM DEFAULT/ENABLE FOR Number of Slip Masks to Print
                masks$(2) = "1"
                return

L21520:     REM DEFAULT/ENABLE FOR Deposit Slip Number Range
L21530:         convert firstslip% to str(nextslip$,2), pic(0000000)
                str(nextslip$,,1) = "D"

                call "REDALT0" (#15, nextslip$, 1%, f1%(15))
                     if f1%(15) = 0% then L21620
                firstslip% = firstslip% + 1%
                if firstslip% > 9999999% then firstslip% = 1
                goto L21530

L21620:         firstslip$ = str(nextslip$,2)
                return

L21650:     REM DEFAULT/ENABLE FOR SORT OPTION SELECTION
                sort_option% = 0%
                inpmessage$ = "Enter '0' to print by Employee code, '1' t~
        ~o print by Dept/Shift/Employee Name"
                return

L21690:     REM DEFAULT/ENABLE FOR Printing of Rate of Pay
                rateflag$ = "N"
                inpmessage$ = "Enter 'Y' if Rate of Pay should be printed~
        ~ on checks."
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
            *          L O A D   E M P L O Y E E   R E C O R D          *~
            *                                                           *~
            * LOADS THE EMPLOYEE MASTER RECORD FROM THE DEPTHS OF THE   *~
            * DISK DRIVE SO THAT WE CAN ACCESS ALL OF THE SHADOWY       *~
            * DELIGHTS HIDDEN WITHIN.                                   *~
            *************************************************************


          get  #3, using L30200, empcode$, wkstncode$, fstatus$, sstatus$,~
                       payfreq$, paymode$,                               ~
                       cashacct$, grossacct$, normalhrs$, autopay$,      ~
                       firstpaydate$, lastpaydate$, deductflag$,         ~
                       normweekhrs$, wkcomdef$, primstate$, department$, ~
                       shiftcode%
        return

L30200:     FMT CH(12),                  /* EMPLOYEE CODE              */~
                CH(4),                   /* WORKSTATION CODE           */~
                CH(1),                   /* FED FILING STATUS          */~
                CH(1),                   /* STATE FILING STATUS        */~
                CH(1),                   /* PAY FREQUENCY              */~
                CH(1),                   /* MODE OF PAYMENT            */~
                XX(16),                  /* Filler                     */~
                CH(9),                   /* CASH IN BANK ACCOUNT       */~
                CH(9),                   /* GROSS PAYROLL ACCOUNT      */~
                CH(2),                   /* NORMAL HRS/DAY ASCII(##)   */~
                CH(1),                   /* AUTOPAYROLL FLAG           */~
                CH(6),                   /* FIRST DATE PAY PERIOD      */~
                CH(6),                   /* LAST DATE PAY PERIOD       */~
                CH(1),                   /* DEDUCT ON THIS PRLDDUCT RUN*/~
                CH(02),                  /* NORMAL HRS/WEEK ASCII(##)  */~
                CH(10),                  /* WORKMANS' COMP. CODE DEF   */~
                XX(7),                                                   ~
                CH(2),                   /* PRIMARY STATE CODE         */~
                CH(4),                   /* DEPARTMENT CODE            */~
                XX(33),                                                  ~
                BI(1)                    /* SHIFT CODE                 */

L31000: REM *************************************************************~
            *          L O A D   E A R N I N G S   R E C O R D          *~
            *                                                           *~
            * LOADS ONE EARNINGS RECORD FROM THE EARNINGS FILE.  THIS   *~
            * JUST FOLLOWS WHAT EVERYONE ELSE DOES.  WE THEN PUSH THIS  *~
            * TO THE EARNINGS STACK FOR A ROLLICKING GOOD TIME.         *~
            *************************************************************

            get   #4, using L31140, type$, cash$, txbl$, units,           ~
                           amount, ytdamount
            call "REDALT0" (#7, type$, 1%, f1%(7))
                if f1%(7) <> 0 then get #7, type$
            return

L31140:     FMT XX(12),                  /* EMPLOYEE CODE              */~
                XX(3),                   /* SEQUENCE NUMBER            */~
                XX(12),                  /* EMPLOYEE CODE (AGAIN)      */~
                CH(12),                  /* EARNINGS TYPE              */~
                XX(4),                   /* DEPARTMENT CODE            */~
                CH(1),                   /* PAID IN CASH? FLAG         */~
                CH(1),                   /* IS THIS TAXABLE?           */~
                XX(6),                   /* UNIT DESCRIPTION           */~
                XX(8),                   /* UNIT RATE                  */~
                XX(9),                   /* EXPENSE ACCOUNT NUMBER     */~
                                                                         ~
                XX(8),                   /* USUAL PAYROLL   UNITS      */~
                XX(8),                   /* USUAL PAYROLL   AMOUNT     */~
                XX(8),                   /* BUFFER ADD      UNITS      */~
                XX(8),                   /* BUFFER ADD      AMOUNT     */~
                PD(14,4),                /* CURRENT CHECK   UNITS      */~
                PD(14,4),                /* CURRENT CHECK   AMOUNT     */~
                XX(8),                   /* MONTH   TO DATE UNITS      */~
                XX(8),                   /* MONTH   TO DATE AMOUNT     */~
                XX(8),                   /* QUARTER TO DATE UNITS      */~
                XX(8),                   /* QUARTER TO DATE AMOUNT     */~
                XX(8),                   /* YEAR    TO DATE UNITS      */~
                PD(14,4)                 /* YEAR    TO DATE AMOUNT     */~

L32000: REM *************************************************************~
            *         L O A D   D E D U C T I O N   R E C O R D         *~
            *                                                           *~
            * LOADS THE DEDUCTION RECORD FROM THE DEDUCTION LINE ITEMS  *~
            * FILE.  THIS IS STANDARD FARE.                             *~
            *************************************************************

            get   # 5, using L32130, dedmethod$, deduction$, dedempflag$, ~
                                    amount, ytdamount, bankacct$, bank$
            return

L32130:     FMT XX(12),                  /* EMPLOYEE CODE              */~
                XX(3),                   /* SEQUENCE NUMBER            */~
                XX(12),                  /* EMPLOYEE NUMBER            */~
                XX(6),                   /* DEDUCTION CATEGORY (LOC?!) */~
                CH(6),                   /* METHOD OF DEDUCTION (DDT)  */~
                CH(12),                  /* DEDUCTION DESCRIPTION      */~
                CH(1),                   /* EMPLOYEE PAYS THIS? FLAG   */~
                XX(9),                   /* CREDIT ACCOUNT             */~
                XX(9),                   /* DEBIT ACCOUNT              */~
                XX(6),                   /* APPLIES FIELD (123456)     */~
                XX(8),                   /* AMOUNT 1 OR 0 IF NOT USED  */~
                XX(8),                   /* AMOUNT 2 OR 0 IF NOT USED  */~
                XX(8),                   /* AMOUNT 3 OR 0 IF NOT USED  */~
                XX(8),                   /* AMOUNT 4 OR 0 IF NOT USED  */~
                XX(8),                   /* GOAL                       */~
                PD(14,4),                /* CURRENT DEDUCTION AMOUNT   */~
                XX(8),                   /* MTD         "        "     */~
                XX(8),                   /* QTD         "        "     */~
                PD(14,4),                /* YTD         "        "     */~
                POS(221), CH(20), CH(4)  /* DIRECT DEP ACCOUNT NUMBER  */

L34000: REM *************************************************************~
            *  GET PROCESSING RANGES                                    *~
            *************************************************************

            init(hex(00)) dept$(1), empcode$(1), payfreq$(1)
            init(hex(ff)) dept$(2), empcode$(2), payfreq$(2)

            call "READ100" (#1, "PRL PROCESSING RANGE", f1%(1))
                     if f1%(1) = 0 then return
                     get #1, using L34340, str(dept$(), 1),               ~
                                          str(empcode$(), 1),            ~
                                          str(payfreq$(), 1)

            if dept$(1) <> "ALL" then L34170
               init(hex(00)) dept$(1)
               init(hex(ff)) dept$(2)
               go to L34210
L34170:     if dept$(2) <> " " then L34190
               dept$(2) = dept$(1)
L34190:        dept$(1) = dept$(1) addc all(hex(ff))

L34210:     if empcode$(1) <> "ALL" then L34250
               init(hex(00)) empcode$(1)
               init(hex(ff)) empcode$(2)
               go to L34290
L34250:     if empcode$(2) <> " " then L34270
               empcode$(2) = empcode$(1)
L34270:        empcode$(1) = empcode$(1) addc all(hex(ff))

L34290:     if payfreq$(2) <> " " then L34310
               payfreq$(2) = payfreq$(1)
L34310:        payfreq$(1) = payfreq$(1) addc all(hex(ff))
               return

L34340:     FMT XX(20), CH(8), CH(24), CH(2)

        REM *************************************************************~
            *        G E T   F R O M         P E R M A S T R            *~
            *                                                           *~
            *************************************************************

        load_per_stuff

        call "READ101" (#14, empcode$, f1%(14))
              if f1%(14) = 0% then return

        get #14, using L35340,    /* FILE: PERMASTR                     */~
            status$,             /* General purpose status indicator   */~
            lname$,              /* Last name of person - part of pers */~
            fname$,              /* First name of person               */~
            mname$,              /* Middle name of person              */~
            ssnumber$,           /* Social security number             */~
            telephone$,          /* Telephone number                   */~
            address$(1),         /* Street address line 1              */~
            address$(2),         /* Street address line 2              */~
            city$,               /* City in address                    */~
            county$,             /* County in address                  */~
            state$,              /* State in address                   */~
            zip$,                /* Zipper in address                  */~
            union$,              /* Union status - personnel system    */~
            curjob$,             /* Current job title - personnel syst */~
            curjobeeo$           /* EEO class of current job - personn */

            address$(3) = city$ & ", " & state$ & "  " & zip$
            call "LINSMASH" (address$())
            if mname$ <> " " then name$=fname$&" "&mname$&". " & lname$  ~
                                 else  name$ = fname$ & " " &  lname$
        return

L35340: FMT                      /* FILE: PERMASTR                     */~
            CH(1),               /* General purpose status indicator   */~
            CH(15),              /* Last name of person - part of pers */~
            CH(10),              /* First name of person               */~
            CH(1),               /* Middle name of person              */~
            CH(11),              /* Social security number             */~
            XX(12),              /* employee code                      */~
            CH(10),              /* Telephone number                   */~
            CH(30),              /* Street address line 1              */~
            CH(30),              /* Street address line 2              */~
            CH(20),              /* City in address                    */~
            CH(20),              /* County in address                  */~
            CH(2),               /* State in address                   */~
            CH(9),               /* Zip code in address                */~
            XX(30),              /* emergency contact                  */~
            XX(10),              /* emergency contact's phone number   */~
            XX(16),              /* Emergency contacts relationship to */~
            XX(1),               /* Gender of a person                 */~
            XX(6),               /* birth date                         */~
            XX(3),               /* minority (eeoc compliance) code    */~
            XX(1),               /* Marital status - personnel system  */~
            XX(2),               /* Number of dependants - personnel s */~
            XX(16),              /* A persons physical status - handyc */~
            XX(16),              /* A persons military status - vet, r */~
            XX(16),              /* Citizenship status - personnel sys */~
            XX(16),              /* Passport status - personnel system */~
            CH(16),              /* Union status - personnel system    */~
            XX(16),              /* Bonding status - personnel system  */~
            CH(16),              /* Current job title - personnel syst */~
            CH(16),              /* EEO class of current job - personn */~
            XX(4),               /* Current department                 */~
            XX(1),               /* Current shift worked               */~
            XX(16),              /* Current supervisor                 */~
            XX(6),               /* Original date hired                */~
            XX(6),               /* Seniority date                     */~
            XX(6),               /* Last date rehired                  */~
            XX(6),               /* Last date terminated               */~
            XX(30),              /* Reason for last termination        */~
            XX(16),              /* Job held when last terminated      */~
            XX(250),             /* filler for rest of record or inter */~
            XX(241)              /* filler for rest of record or inter */~

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  pf16$ = " "
                  if fieldnr% = 0% then pf16$ = "(16)PRINT CHECKS"       ~
                                   else pf16$ = "(16)EXIT PROGRAM"
                  init(hex(84)) lfac$()
                  if fieldnr% = 0% then init(hex(86)) lfac$()
                  on fieldnr% gosub L40170,         /* No. of Masks     */~
                                    L40170,         /* Check # Range    */~
                                    L40170,         /* No. of Masks     */~
                                    L40170,         /* Slip # Range     */~
                                    L40200,         /* Sort Option      */~
                                    L40170          /* Rate Flag        */
                     goto L40240

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40170:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L40200:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40240:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Payroll Checks And Deposit Slips",              ~
               at (01,60),                                               ~
                  "Todays Date:",                                        ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "No. of Check Masks to Print",                         ~
               at (06,30), fac(lfac$( 1)), masks$(1)            , ch(02),~
               at (06,43), fac(hex(8c))  , maskmsg$             , ch(36),~
               at (07,02),                                               ~
                  "First Check Number",                                  ~
               at (07,30), fac(lfac$( 2)), firstcheck$          , ch(08),~
               at (08,02),                                               ~
                  "Last Check Number",                                   ~
               at (08,30), fac(lfac$( 2)), lastcheck$           , ch(08),~
               at (10,02),                                               ~
                  "No. of Deposit Slip Masks",                           ~
               at (10,30), fac(lfac$( 3)), masks$(2)            , ch(02),~
           /*  AT (10,49), "Forced Print Mode of 'HOLD'", */             ~
               at (11,02),                                               ~
                  "First Deposit Slip Number",                           ~
               at (11,30), fac(lfac$( 4)), firstslip$           , ch(07),~
               at (12,02),                                               ~
                  "Select Printing Sequence",                            ~
               at (12,30), fac(lfac$( 5)), sort_option%         , pic(#),~
               at (13,02), "Print Rate Of Pay On Check?",                ~
               at (13,30), fac(lfac$( 6)), rateflag$            , ch(01),~
               at (14,02),                                               ~
                  "Check Date (Post Date)",                              ~
               at (14,30), fac(hex(84)), checkdate$             , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (23,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65), fac(hex(8c)), pf16$                  , ch(16),~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13% then L40700
                  call "MANUAL" ("PRLCHECK")
                  goto L40240

L40700:        if keyhit% <> 15% then L40740
                  call "PRNTSCRN"
                  goto L40240

L40740:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   2      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'102(fieldnr%)
                  pf16$ = " "
                  if fieldnr% = 0% then pf16$ = "(16)PRINT CHECKS"       ~
                                   else pf16$ = "(16)EXIT PROGRAM"
                  init(hex(84)) lfac$()
                  if fieldnr% = 0% then init(hex(86)) lfac$()
                  on fieldnr% gosub L41170,         /* Print Mask       */~
                                    L41170,         /* Check # Range    */~
                                    L41170,         /* Print Mask       */~
                                    L41170,         /* Slip # Range     */~
                                    L41200,         /* Sort Option      */~
                                    L41170          /* Rate Flag        */
                     goto L41240

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L41170:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L41200:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L41240:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Payroll Checks And Deposit Slips",              ~
               at (01,60),                                               ~
                  "Todays Date:",                                        ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Print a Check Mask?",                                 ~
               at (06,30), fac(lfac$( 1)), answer$              , ch(01),~
               at (06,43), fac(hex(8c))  , maskmsg$             , ch(36),~
               at (07,02),                                               ~
                  "First Check Number",                                  ~
               at (07,30), fac(lfac$( 2)), firstcheck$          , ch(08),~
               at (08,02),                                               ~
                  "Last Check Number",                                   ~
               at (08,30), fac(lfac$( 2)), lastcheck$           , ch(08),~
               at (10,02),                                               ~
                  "No. of Deposit Slip Masks",                           ~
               at (10,30), fac(lfac$( 3)), masks$(2)            , ch(02),~
               at (10,49), "Forced Print Mode of 'HOLD'",                ~
               at (11,02),                                               ~
                  "First Deposit Slip Number",                           ~
               at (11,30), fac(lfac$( 4)), firstslip$           , ch(07),~
               at (12,02),                                               ~
                  "Select Printing Sequence",                            ~
               at (12,30), fac(lfac$( 5)), sort_option%         , pic(#),~
               at (13,02), "Print Rate Of Pay On Checks",                ~
               at (13,30), fac(lfac$( 6)), rateflag$            , ch(01),~
               at (14,02),                                               ~
                  "Check Date (Post Date)",                              ~
               at (14,30), fac(hex(84)), checkdate$             , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (23,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65), fac(hex(8c)), pf16$                  , ch(16),~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13% then L41700
                  call "MANUAL" ("PRLCHECK")
                  goto L41240

L41700:        if keyhit% <> 15% then L41740
                  call "PRNTSCRN"
                  goto L41240

L41740:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50130,         /* No. of Masks     */~
                                    L51230,         /* Check # Range    */~
                                    L51520,         /* No. of Masks     */~
                                    L51560,         /* Slip # Range     */~
                                    L52100,         /* Sort Option      */~
                                    L52130          /* Rate Flag        */
                     return
L50130:     REM TEST DATA FOR Number of Check Masks to Print
                call "NUMTEST" (masks$(1), 0, 99, errormsg$, 0.0, 0)
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 2.                       *~
            *************************************************************

            deffn'152(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L51130,         /* Print Mask       */~
                                    L51230,         /* Check # Range    */~
                                    L51520,         /* No. of Masks     */~
                                    L51560,         /* Slip # Range     */~
                                    L52100,         /* Sort Option      */~
                                    L52130          /* Rate Flag        */
                     return
L51130:     REM TEST DATA FOR Print a Check Mask?
                if answer$  = "N" then return
                if answer$ <> "Y" then L51200
                   masktype% = -1
                   gosub print_mask
                errormsg$ = "Mask has been Printed, Do You Want Another?"
                return
L51200:            errormsg$ = "Answer 'Y' or 'N' Please"
                   return

L51230:     REM TEST DATA FOR Check Number Range
                convert firstcheck$ to firstcheck%, data goto L51300
                convert firstcheck% to firstcheck$, pic(########)
                nextcheck$ = firstcheck$
                convert lastcheck$ to lastcheck%, data goto L51300
                convert lastcheck% to lastcheck$, pic(########)
                if lastcheck% >= firstcheck% then L51330
L51300:            errormsg$ = "Invalid entry for Check Number Range."
                   return

L51330:         call "REDALT0" (#15, nextcheck$, 1%, f1%(15))
                     if f1%(15)  = 0% then L51380
                     errormsg$ = "First Check Number Has Been Assigned."
                     return

L51380:         readkey$ = str(nextcheck$,,8) & hex(ffffff)
                nextcheck% = 99999999%
                call "PLOWALTS" (#15, readkey$, 1%, 0%, f1%(15))
                     if f1%(15) = 0% then L51430
                convert str(readkey$,,8) to nextcheck%, data goto L51430
L51430:         nextcheck% =max(firstcheck%,min(nextcheck%-1%,99999999%))

                if nextcheck% >= lastcheck% then return

                convert nextcheck% to nextcheck$, pic(########)
                errormsg$ = "Highest LAST Check Number Available for Cont~
        ~inuity is # " & nextcheck$
                return

L51520:     REM TEST DATA FOR Number of Slip Masks to Print
                call "NUMTEST" (masks$(2), 0, 99, errormsg$, 0.0, 0)
                return

L51560:     REM TEST DATA FOR Deposit Slip Number Range
                convert firstslip$ to firstslip%, data goto L52010
                convert firstslip% to firstslip$, pic(0000000)
                nextslip$ = "D" & str(firstslip$)
                goto L52040
L52010:            errormsg$ = "Invalid entry for Deposit Number Range."
                   return

L52040:         call "REDALT0" (#15, nextslip$, 1%, f1%(15))
                     if f1%(15)  = 0% then return
                   errormsg$ = "First Deposit Number Has Been Assigned."
                   return

            REM TEST FOR SORT OPTION
L52100:         if sort_option% > 1% then errormsg$ = "Invalid Option!"
                return

L52130:     REM Rateflag for check printing
                if rateflag$ = "N" then return
                if rateflag$ <> "Y" then L52170
                return
L52170:         errormsg$ = "Must Enter 'Y' or 'N'"
                return

        REM *************************************************************~
            *        S T A C K   P U S H   F O R   A M O U N T S        *~
            *                                                           *~
            * WE NEED TO PUSH EARNINGS AND DEDUCTIONS AMOUNTS ONTO THE  *~
            * STACKS FOR POSSIBLE CONSOLIDATION.                        *~
            *************************************************************

        REM -------------- These Stacks MUST be accurate ----------------~
              *       *      *          *         *         *       *    ~
                The print subroutine totals them to determine net pay    ~
              *       *      *          *         *         *       *


            deffn'162(type$, units, amount, ytdamount)
                  search earntype$() = str(type$) to cursor%() step 12
                  if cursor%(1) = 0 then L55220
                     temp% = (cursor%(1)+11)/12
L55170:              earnamt(temp%,1) = earnamt(temp%,1) + units
                     earnamt(temp%,2) = earnamt(temp%,2) + amount
                     earnamt(temp%,3) = earnamt(temp%,3) + amount
                     earnamt(temp%,3) = earnamt(temp%,3) + ytdamount
                     return
L55220:           REM PUSH NEW ITEM ONTO STACK.
                      eptr% = eptr% + 1
                      if rateflag$ <> "Y" then lim% =  9% else lim% = 4%
                      if eptr% < lim% + 1% then L55280
                         earntype$(lim%) = "OTHER"
                         temp% = lim%
                         goto L55170
L55280:               earntype$(eptr%) = type$
                      earnamt(eptr%,1) = units
                      earnamt(eptr%,2) = amount
                      earnamt(eptr%,3) = ytdamount + amount
                      return

            deffn'163(deduction$, amount, ytdamount)
        REM       SEARCH DEDUCTION$()=STR(DEDUCTION$) TO CURSOR%() STEP 12
        REM       IF CURSOR%(1) = 0 THEN 55420
        REM          TEMP% = (CURSOR%(1)+11)/12
        REM          DEDAMT(TEMP%,1) = DEDAMT(TEMP%,1) + AMOUNT
        REM          DEDAMT(TEMP%,2) = DEDAMT(TEMP%,2) + AMOUNT
        REM          DEDAMT(TEMP%,2) = DEDAMT(TEMP%,2) + YTDAMOUNT
        REM          RETURN
        REM PUSH NEW ENTRY ONTO STACK.
                   dptr% = dptr% + 1%
                   if dptr% < 11% then L55480
                      deduction$(10) = "OTHER"
                      dptr% = 10%
                      goto L55490
L55480:            deduction$(dptr%) = deduction$
L55490:            dedamt(dptr%,1) = dedamt(dptr%,1) + amount
                   dedamt(dptr%,2) = dedamt(dptr%,2) + ytdamount + amount
                     if dedamt(dptr%,2) <> 0 then return
                     if dedamt(dptr%,1) <> 0 then return
                        deduction$(dptr%) = " "
                        dptr% = dptr% - 1%
                        return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************
        exit_program
            call "SHOSTAT" ("One Moment Please")
            call "FILEBGON" (#60)
            end checks%

