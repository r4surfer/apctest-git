        REM *************************************************************~
            *                                                           *~
            *  PPPP   RRRR   L      JJJJJ  U   U  RRRR   N   N    0     *~
            *  P   P  R   R  L        J    U   U  R   R  NN  N   0 0    *~
            *  PPPP   RRRR   L        J    U   U  RRRR   N N N  0   0   *~
            *  P      R   R  L      J J    U   U  R   R  N  NN   0 0    *~
            *  P      R   R  LLLLL   J      UUU   R   R  N   N    0     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLJURN0 - PRINTS THE PAYROLL JOURNAL AND PROOF PAYROLL   *~
            *            JOURNAL.  THE DIFFERENTIATION IS MADE WITH A   *~
            *            SNEAKY GETPARM--PROOFME = YES/NO...            *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/16/81 ! ORIGINAL                                 ! BCW *~
            * 08/17/81 ! ADD TOTALS FOR EARNINGS/DEDUCTIONS       ! TEM *~
            * 08/20/81 ! HANDLES CHECKS (NEGATIVES AND NUMBERS)   ! TEM *~
            * 10/19/81 ! USE RANGE TO PROCESS                     ! JAM *~
            * 09/02/86 ! Added Sure Pay Report For Direct Deposit ! HES *~
            * 12/21/88 ! Added tape processing for Direct Deposit ! ERN *~
            * 01/15/89 ! Truncation Errors In Print Out           ! KAB *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            * 12/05/92 ! PRR 12530 - Correct Position of 'Will not! JBK *~
            *          !  Process Message' for Net Pay < 0.  Added!     *~
            *          !  'Deduction Maybe Under Withheld' message!     *~
            *          !  for Check with Net Pay = 0.             !     *~
            *          ! PRR 12542 - Add Direct Deposit Totals by !     *~
            *          !   Bank and Page Break on Bank Change.    !     *~
            *          ! Standard Report Headings added, SHOWMSG's!     *~
            *          !  Removed.                                !     *~
            * 08/27/96 ! Changes for the year 2000.               ! DXL *~
            *************************************************************

        dim                                                              ~
            acct$9,                      /* EARNINGS ACCOUNT NUMBER    */~
            aid$1,                       /* AID CHARACTER FROM GETPARM */~
            amount(5),                   /* 5 DEDUCTION PARAMETERS     */~
            amount$10,                   /* Formatted amount           */~
            bankacct$20,                 /* Direct Dep Bank Acct No.   */~
            bankcode$4,                  /* Direct Dep Bank Code       */~
            bankid$9,                    /* Direct Dep Bank ACH #      */~
            banksp$2,                    /* Direct Dep Sure Pay Code   */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            cash$1,                      /* PAID IN CASH? EARNING FLAG */~
            company$60,                  /* COMPANY FOR HEADERS        */~
            date$8,                      /* TODAYS DATE                */~
            dedamt(4),                   /* CURR, MTD, QTD, YTD DEDXNS */~
            dedapplies$6,                /* DEDUCTION APPLIES INFO     */~
            dedcat$6,                    /* DEDUCTION CATEGORY INFO    */~
            dedcracct$9,                 /* DEDUCTION CREDIT ACCOUNT   */~
            deddbacct$9,                 /* DEDUCTION DEBIT ACCOUNT    */~
            dedempflag$1,                /* DEDUCTION EMPLOYEE PAYS?   */~
            dedmethod$6,                 /* DEDUCTION METHOD           */~
            deductflag$1,                /* DO WE PROCESS THIS GUY?    */~
            deduction$12,                /* DEDUCTION DESCRIPTION      */~
            dedxnstak$(100)15,           /* DEDUCTION DESCRIPTION STACK*/~
            dedxnstak(100,3),            /* DEDUCTION AMOUNT STACK     */~
            dedxxstak$(100)15,           /* DEDUCTION DESCRIPTION STACK*/~
            dept$4,                      /* EARNINGS DEPARTMENT CODE   */~
            dept$(2)4,                   /* DEPARTMENT RANGE TO PROCESS*/~
            department$4,                /* EMP. MASTER DEPARTMENT CODE*/~
            direct_deposit_flag$1,       /* Employee has a Direct Depos*/~
            dpt$4,                       /* DEPARTMENT CODE            */~
            dstack$(999)20,              /* DEDUCTION STACK            */~
            dstak(999),                  /* DITTO WITH NUMBERS         */~
            dtstamp$7,                   /* Date/Time Stamp            */~
            earn(4,2),                   /* AMOUNTS THIS EARNINGS REC  */~
            earnstak$(100)12,            /* EARNINGS TYPE STACK        */~
            earnstak(100,2),             /* EARNINGS UNITS STACK       */~
            empcode$12,                  /* EMPLOYEE CODE INFO         */~
            empcode$(2)12,               /* EMPLOYEE CODE RANGE        */~
            emprstack$(999)20,           /* EMPLOYERS SHARE STACK      */~
            emprstak(999),               /* DITTO WITH NUMBERS         */~
            estack$(999)20,              /* EARNINGS STACK             */~
            estak1(999),                 /* DITTO WITH NUMBERS         */~
            estak2(999),                 /* DITTO     DITTO            */~
            etype$12,                    /* EARNINGS TYPE DESCRIPTION  */~
            firstpaydate$8,              /* FIRST DATE PAY PERIOD      */~
            lastpaydate$8,               /* LAST DATE THIS PAY PERIOD  */~
            linenumber%(6),              /* LINE POINTERS FOR PRINT    */~
            location$2,                  /* LOCATOR ARRAY FOR MATSEARCH*/~
            name$(3)15,                  /* EMPLOYEE'S 3 NAMES         */~
            nameb$20,                    /* EMPLOYEE'S NAME FORMATTED  */~
            namef$28,                    /* EMPLOYEE'S NAME FORMATTED  */~
            payfreq$1,                   /* PAY FREQUENCY (1-7)        */~
            payfreq$(2)1,                /* PAY FREQUENCY RANGE        */~
            period$(10)10,               /* PAY PERIOD DATES           */~
            pointer%(2),                 /* POINTERS FOR STACK PRINTS  */~
            print$(11)20,                /* PRINT COLUMN ENTRIES       */~
            proof$3,                     /* PROOF JOURNAL? (Y/N)       */~
            readkey$50,                  /* READ KEY FOR FILE INFO     */~
            reporthd$(4)60,              /* Report Headings - 4 Reports*/~
            rptid$6,                     /* Report ID                  */~
            route$9,                     /* Direct Dep Bank Routing Id.*/~
            seqnr$3,                     /* EARNINGS SEQUENCE NUMBER   */~
            ssn$11,                      /* SOCIAL SECURITY NUMBER     */~
            string$50,                   /* SEARCH STRING IN STACK     */~
            txbl$1,                      /* IS THIS EARNING TAXABLE?   */~
            type$12,                     /* EARNINGS TYPE DESCRIPTION  */~
            units$6,                     /* EARNINGS UNIT OF MEASURE   */~
            userid$3                     /* Operators Logon Id.        */

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64)                      /* RECORD-ON-FILE FLAGS       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() SHOULD NOT BE MODIFIED.    */
                     /* ITS AN INTRINSIC PART OF THE OPEN SUBROUTINE.  */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! SYSFILE2 ! SYSTEM CONTROL FILE (CHECK # AND DATE    *~
            * # 2 ! USERINFO ! USER INFORMATION FILE (PAYROLL DATE)     *~
            * # 3 ! EMPMASTR ! EMPLOYEE FILE MASTER RECORDS.            *~
            * # 4 ! EMPEARN1 ! EMPLOYEE EARNINGS DETAILS FILE           *~
            * # 5 ! EMPDEDXN ! EMPLOYEE DEDUCTIONS FILE.                *~
            * # 6 ! WORKFILE ! EMPLOYEE DEDUCTIONS FILE.                *~
            * # 7 ! PRLBANKF ! PAYROLL BANK CODE NUMBER FILE            *~
            * # 8 ! PRLCHTIF ! INTERIM FILE CONTAINING CHECK NUMBERS    *~
            * # 9 ! PRLDDTIF ! Direct Deposit Transactions              *~
            * #14 ! PERMASTR ! Personnel Master File.                   *~
            *************************************************************

            select #1,  "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select #2,  "USERINFO",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos = 1, keylen = 3

            select #3,  "EMPMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 136,                                  ~
                         keypos = 1, keylen = 12,                        ~
                         alt key  1, keypos = 70, keylen =  1, dup

            select #4,  "EMPEARN1",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 200,                                  ~
                         keypos = 1, keylen = 15,                        ~
                         alternate key  1, keypos =  16, keylen = 28

            select #5,  "EMPDEDXN",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 300,                                  ~
                         keypos = 1, keylen = 15,                        ~
                         alternate key  1, keypos =  16, keylen = 18, dup

            select #6,  "WORKFILE",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 95,                                   ~
                         keypos = 1, keylen = 43

            select #7,  "PRLBANKF",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 256,                                  ~
                         keypos = 1, keylen = 4,                         ~
                         alt key 1, keypos = 5, keylen = 30, dup

            select #8,  "PRLCHTIF",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 50,                                   ~
                         keypos = 1, keylen = 12

            select #9,  "PRLDDTIF",                                      ~
                         varc, indexed, recsize = 108,                   ~
                         keypos = 102, keylen = 7,                       ~
                         alt key 1, keypos = 95, keylen = 14

            select #14, "PERMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 950,                                   ~
                        keypos = 39, keylen = 12,                        ~
                        alt key  1, keypos =  28, keylen = 23,           ~
                            key  2, keypos =   2, keylen = 49,           ~
                            key  3, keypos =   1, keylen = 50


*        Check to See if Payroll/Personnel is Active
            call "PRLEXTSB" ("PRL", prl%)
                if prl% = 99% then end (prl%)

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, 0%, f2%(1), 0%, " ")
            call "OPENCHCK" (#2, 0%, f2%(1), 0%, " ")
            call "OPENCHCK" (#3, 0%, f2%(3), 0%, " ")
            call "OPENCHCK" (#4, 0%, f2%(4), 0%, " ")
            call "OPENCHCK" (#5, 0%, f2%(5), 0%, " ")
            call "WORKOPEN" (#6, "IO   ", 100%, f2%(6))
            call "OPENCHCK" (#7, 0%, f2%(7), 0%, " ")
            call "OPENCHCK" (#8, 0%, f2%(8), 100%, " ")
            call "OPENCHCK" (#14, 0%, f2%(14), 0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES NECESSARY VARIABLES.  ALSO DOES GETPARM FOR   *~
            * FIGURNING OUT WHETHER THIS IS A PROOF PAYROLL JOURNAL OR  *~
            * THE "REAL THING."                                         *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date
            call "DATEFMT" (date$)
            call "EXTRACT" addr("ID", userid$)
            call "COMPNAME" (12%, company$, 0%)
            lastdpt$ = all(hex(ff))

            REM DO A GETPARM TO FIND IF THIS BATCH GETS POSTED TO G/L.
                call "GETPARM" addr ("I ", "R", "PRLJURN0", aid$, "0001",~
                                     "PRLJRN",                           ~
                                     "Is This A Proof Payroll Journal?", ~
                                     32%, "K", "PROOFME ", proof$, 3%,   ~
                                     5%, 32%, "A")

            if str(proof$,,1) = "Y" then L09240
               call "SHOSTAT" ("Printing Payroll Journal")
               go to L09260
L09240:        call "SHOSTAT" ("Printing Proof Payroll Journal")

L09260:     REM INITIALIZE FOR NEXT WHICH IS A PLOW ROUTINE.
                line% = 1000
                page% = 0
                init(hex(00)) empcode$

            REM INITIALIZE STACKS
                init(hex(ff)) emprstack$(), estack$(), dstack$()
                mat estak1 = zer
                mat estak2 = zer
                mat dstak  = zer
                eptr%, dptr%, emprptr% = 0

            REM GET RANGE TO PROCESS
                gosub L35000
                call "READ100" (#1, "PAY PERIODS", f1%(1))
                     if f1%(1) = 0 then L09450
                get #1, using L09430, str(period$(), 1)
L09430:         FMT XX(20), CH(91)

L09450:     if str(proof$,,1) = "Y" then L09800
            REM Get Users Payroll Date For Use As Check Date...
                call "READ100" (#2, userid$, f1%(2))
                     if f1%(2) = 0% then L09520
                get #2 using L09500, checkdate$
L09500:             FMT XX(15), CH(6)
                call "DATEFMT" (checkdate$)
L09520:     REM Determine if ACH Tape Sub-Module is active or not...
                call "READ100" (#1, "PRLDD.INFORMATION", tape%)
                if tape% = 0% then L09800
                     temp$ = checkdate$ : call "DATUNFMT" (temp$)
                     call "OPENCHCK" (#9, 0%, f2%(9), 500%, " ")
                     write #1 using L09580, "PRLDD.P/R.DATES", " ",        ~
                                           eod goto L09590
L09580:                   FMT 2*CH(250)
L09590:              call "READ101" (#1, "PRLDD.P/R.DATES", f1%(1))
                     get #1 using L09610, temp1$, temp2$
L09610:                   FMT POS(21), 2*CH(6)
                     if temp1$ <> " " and temp1$ <> blankdate$ then L09650
                          temp1$ = checkdate$
                          goto L09680
L09650:              if temp2$ = " " or temp2$ = blankdate$ then temp2$ = temp1$
                     if temp$ < temp1$ then temp1$ = temp$
                     if temp$ > temp2$ then temp2$ = temp$
L09680:              put #1 using L09610, temp1$, temp2$
                     rewrite #1


L09800
*        Set-up Report Headings based on Proof (Y or N)
            reporthd$(1%) = "P A Y R O L L   J O U R N A L"
            reporthd$(2%) = "D E P A R T M E N T   R E C A P"
            reporthd$(3%) = "C O M P A N Y   R E C A P"
            reporthd$(4%) = "D I R E C T   D E P O S I T   L I S T I N G"

            if str(proof$,,1%) <> "Y" then L09900
                for i% = 1% to 4%
                     reporthd$(i%) = "P R O O F   " & reporthd$(i%)
                next i%
L09900:     for i% = 1% to 4%
               call "STRING" addr("CT", reporthd$(i%), 60%, reporthd$(i%))
            next i%

            rptid$ = "PRL015"
            line% = 99%
            reportid% = 1%

L10000: REM *************************************************************~
            *                  M A I N   P R O G R A M                  *~
            *                                                           *~
            * PLOWS DOWN EMPLOYEE LIST IN EMPLOYEE CODE ORDER AND GOSUBS*~
            * TO THE PROCESSING ROUTINE FOR THOSE WITH THE EARNINGS FLAG*~
            * SET.  NOTICE THAT WE CANNOT (*WHIMPER*) PLOW DOWN THE ALT *~
            * KEY SINCE THOSE ARE NOT IN EMPLOYEE CODE ORDER.           *~
            *************************************************************

            direct_deposit_flag$ = " "
            if str(proof$,,1) = "Y" then L10230
            REM By Employee Payed...
            checknr$, slipnr$ = " "
            call "PLOWNEXT" (#8, empcode$, 0%, f1%(8))
                 if f1%(8) = 0 then L19000
            get #8, using L10150, checknr$, slipnr$
L10150:     FMT XX(12), CH(8), CH(8)
            call "READ100" (#3, empcode$, f1%(3))
                 if f1%(3) = 0 then L19000
            call "READ100" (#14, empcode$, f1%(14))
            gosub L30000                  /* EMPLOYEE MASTER RECORDS    */
            goto L10370

L10220:     REM By Employee...
L10230:     call "PLOWNEXT" (#3, empcode$, 0%, f1%(3))
                 if f1%(3) = 0 then L19000
            call "READ100" (#14, empcode$, f1%(14))
            gosub L30000                  /* EMPLOYEE MASTER RECORDS    */

            REM in Range?
            if deductflag$ <> "Y" then L10220
            if empcode$ <= empcode$(1) then L10220
            if empcode$ >  empcode$(2) then L19000
            if department$ <= dept$(1) then L10220
            if department$ >  dept$(2) then L10220
            if payfreq$ <= payfreq$(1) then L10220
            if payfreq$ >  payfreq$(2) then L10220

L10370:     REM Good Stuff Begins Here...
            init(hex(ff)) dedxnstak$(), earnstak$()
            mat earnstak = zer
            mat dedxnstak = zer
            earnptr%, dedxnptr% = 0

            REM NOW LOAD NONZERO EARNINGS RECORDS INTO STACK.
            readkey$ = empcode$
            gross, flag% = 0
L10460:     call "PLOWNEXT" (#4, readkey$, 12%, f1%(4))
                if f1%(4) = 0 then L10560
            gosub L31000           /* LOAD EARNINGS RECORD       */
            if earn(1,1) = 0 and earn(1,2) = 0 then L10460
            flag% = 1
            gross = gross + earn(1,2)
            gosub'162(type$, earn(1,1), earn(1,2))
            gosub'172(department$, type$, earn(1,1), earn(1,2))
            goto L10460

L10560:     REM NOW LOAD NONZERO DEDUCTIONS RECORDS INTO STACK.
            if flag% = 0 and str(proof$,,1) = "Y" then L10000 /*ZIP OUT!*/
            readkey$ = empcode$
L10590:     call "PLOWNEXT" (#5, readkey$, 12%, f1%(5))
                if f1%(5) = 0 then L10800
            gosub L32000           /* LOAD DEDUCTIONS RECORD     */
            if dedamt(1) = 0 then L10590
            if dedmethod$ <> "DIRECT" then L10730
                if dedempflag$ <> "Y" then L10730 /* ?? */
                route$ = "UNKNOWN"  :  direct_deposit_flag$ = "Y"
                call "READ100" (#7, bankcode$, f1%(7))
                     if f1%(7) <> 0 then get #7 using L10680, route$
L10680:         FMT XX(124), CH(9)
                seq% = seq% + 1
                write #6, using L10720, route$, empcode$, bankacct$, seq%,~
                                       banksp$, dedamt(1), nameb$
L10720:         FMT CH(9), CH(12), CH(20), BI(2), CH(02), PD(14,4),CH(20)
L10730:     if dedempflag$ <> "Y" then                                   ~
                            gosub'174(department$, deduction$, dedamt(1))
            gosub'163(deduction$, dedamt(1))
            if dedempflag$ <> "Y" then L10590
            gosub'173(department$, deduction$, dedamt(1))
            goto L10590

L10800:     REM NOW PRINT THE JOURNAL ENTRIES
            gosub L20000
            goto  L10000

L19000: REM *************************************************************~
            *    P R I N T   D E P A R T M E N T   R E C A P            *~
            *************************************************************

            if dptr% + eptr% + emprptr% = 0 then L65000
            if eptr% > 0 then                                            ~
            call "SORT" addr(str(estack$(), 1), eptr%, 20%,              ~
                             str(estack$(), 1), 1%, 16%, "A", "S")

            if dptr% > 0 then                                            ~
            call "SORT" addr(str(dstack$(), 1), dptr%, 20%,              ~
                             str(dstack$(), 1), 1%, 16%, "A", "S")

            if emprptr% > 0 then                                         ~
            call "SORT" addr(str(emprstack$(), 1), emprptr%, 20%,        ~
                             str(emprstack$(), 1), 1%, 16%, "A", "S")
            gosub L40000    /* Payroll Department Recap */
            print$() = " "
            print$(1) = "****"
            print$(2) = "***TOTALS***"

            call "CONVERT" (tot1, 0.4, str(print$(3),,16))
            call "CONVERT" (tot2, 2.2, str(print$(4),,15))
            call "CONVERT" (tot3, 2.2, str(print$(7),,15))
            call "CONVERT" (tot4, 2.2, str(print$(10),,15))
            print using L63100, print$(1), print$(2), print$(3),print$(4),~
                               print$(1), print$(2), print$(7),          ~
                               print$(1), print$(2), print$(10)
            print using L62980

            REM Now print Company payroll summary totals...
            if departments% <= 1% then L19530 /* Redundant with above? */
            if dedxnptr% > 0 then                                        ~
            call "SORT" addr(str(dedxnstak$(),1), dedxnptr%, 15%,        ~
                             str(dedxnstak$(),1), 1%, 15%, "A", "S")

            if dedxxptr% > 0 then                                        ~
            call "SORT" addr(str(dedxxstak$(),1), dedxxptr%, 15%,        ~
                             str(dedxxstak$(),1), 1%, 15%, "A", "S")
            gosub L42000    /* Payroll Summary Recap */
            print$() = " "
            print$(1) = "****"
            print$(2) = "***TOTALS***"

            call "CONVERT" (tot1, 0.4, str(print$(3),,16))
            call "CONVERT" (tot2, 2.2, str(print$(4),,15))
            call "CONVERT" (tot3, 2.2, str(print$(7),,15))
            call "CONVERT" (tot4, 2.2, str(print$(10),,15))
            print using L63100, print$(1), print$(2), print$(3),print$(4),~
                               print$(1), print$(2), print$(7),          ~
                               print$(1), print$(2), print$(10)
            print using L62980

L19530:     REM Finally, if direct deposits, print sure pay report...
            gosub L50000
            go to L65000 /* We're done */

L20000: REM *************************************************************~
            *    R E P O R T   P R I N T I N G   S U B R O U T I N E    *~
            *                                                           *~
            * PRINTS THE PAYROLL JOURNAL FOR A SINGLE EMPLOYEE.  NOTE   *~
            * THAT THIS IS THE STANDARD FIVE-COLUMN TRIP.               *~
            *************************************************************

            colsdone% = 0
            mat linenumber% = con

*        Determine Net pay or Takehome before printing the employee
            REM Total Gross Pay
                gunits, gdollars, deductions, employer = 0
                if earnptr% = 0% then L20100
                     for temp% = 1% to earnptr%
                          gdollars = gdollars + earnstak(temp%,2%)
                          gunits   = gunits   + earnstak(temp%,1%)
                     next temp%
L20100:     REM Total Net Pay
                if dedxnptr% = 0% then L20108
                     for temp% = 1% to dedxnptr%
                          if str(dedxnstak$(temp%),,1%) <> "Y" then L20106
                            deductions = deductions + dedxnstak(temp%,3%)
                            goto L20107
L20106:                   employer = employer + dedxnstak(temp%,3%)
L20107:              next temp%
L20108:         takehome = gdollars - deductions

            if dedxnptr% > 0 then                                        ~
            call "SORT" addr(str(dedxnstak$(), 1), dedxnptr%, 15%,       ~
                             str(dedxnstak$(), 1), 1%, 3%, "A", "S")

            REM LOOP THROUGH COMPUTING AND PRINTING LINES UNTIL DONE.
L20140:         for column% = 1% to 6%
                    on column% gosub L21000, L22000, L23000, L24000, L25000,  ~
                                     L26000
                    next column%
                if colsdone% < 6% then L20260
                   REM EXIT ROUTINE FOR FINISHED.
                       if tagprinted% <> 0% then return
                          gosub L28000
                          if tagprinted% <> 0% then return
                             print using L62620
                             tagprinted% = 1%
                             return
L20260:         gosub L28000              /* PAGE HEADING, IF NECCESSARY*/
                print using L62650, print$(1), print$(2), print$(3),      ~
                                   print$(4), print$(5), print$(6),      ~
                                   print$(7), print$(8), print$(9),      ~
                                   print$(10), print$(11)
                tagprinted% = 0%
                goto L20140

L21000:     REM FIRST  COLUMN--EMPLOYEE CODE, ETC.
                on linenumber%(1) gosub L21100, L21200, L21300, L21400,      ~
                                        L21500, L21600,        L21800
                   return
L21100:         REM HANDLES FIRST  CASE--PRINT EMPLOYEE INFORMATION
                    print$(1) = empcode$
                    linenumber%(1) = 2
                    return
L21200:         REM HANDLES SECOND CASE--DEPARTMENT CODE
                    print$(1) = "DEPT: " & department$
                    print$(10) = " "
                    if takehome < 0 then L21250
                    linenumber%(1) = 7
                    return
L21250:             print$(10) = "*********"
                    linenumber%(1) = 3
                    return
L21300:         REM HANDLES THIRD CASE--NEGATIVE CHECK MESSAGE
                    print$(1) = " "
                    print$(10) = "*WILL   *"
                    linenumber%(1) = 4
                    return
L21400:         REM HANDLES FOURTH CASE--NEGATIVE CHECK
                    print$(10) = "*NOT    *"
                    linenumber%(1) = 5
                    return
L21500:         REM HANDLES FIFTH CASE--NEGATIVE CHECK
                    print$(10) = "*PROCESS*"
                    linenumber%(1) = 6
                    return
L21600:         REM HANDLES SIXTH CASE--NEGATIVE CHECK
                    print$(10) = "*********"
                    linenumber%(1) = 7
                    return
L21800:         REM HANDLES SEVENTH CASE--ZAP VARIABLES
                    print$(1), print$(10) = " "
                    linenumber%(1) = 8
                    colsdone% = colsdone% + 1
                    return
L22000:     REM SECOND COLUMN--EMPLOYEE NAME
                on linenumber%(2) gosub L22100, L22200, L22300, L22400
                   return
L22100:         REM HANDLES FIRST  CASE--LAST NAME IF NONBLANK
                    if name$(3) = " " then L22200
                       print$(2) = name$(3)
                       linenumber%(2) = 2
                       return
L22200:         REM HANDLES SECOND CASE--FIRST NAME (MAYBE INITIAL, TOO)
                    if name$(1) = " " then L22300
                       print$(2) = name$(1)
                       if name$(2) <> " " and len(name$(2)) = 1          ~
                          then str(print$(2), len(print$(2)) + 2) =      ~
                                         name$(2)
                       linenumber%(2) = 3
                       return
L22300:         REM HANDLES THIRD  CASE--MIDDLE NAME, IF LEN > 1
                    if name$(2) = " " then L22400
                       if len(name$(2)) <= 1 then L22400      /* INITIAL*/
                          print$(2) = name$(2)
                          linenumber%(2) = 4
                          return
L22400:         REM HANDLES FOURTH CASE--ZAP VARIABLES
                    print$(2) = " "
                    linenumber%(2) = 5
                    colsdone% = colsdone% + 1
                    return

L23000:     REM THIRD  COLUMN--EARNINGS INFORMATION
                on linenumber%(3) gosub L23030, L23050
                   return
L23030:         REM HANDLES FIRST  CASE--INITIALIZATION FOR PROCESSING
                    pointer%(1) = 0
L23050:         REM HANDLES SECOND CASE--PRINT AN EARNINGS LINE
                    pointer%(1) = pointer%(1) + 1
                    if pointer%(1) > earnptr% then L23150
                       print$(3) = earnstak$(pointer%(1))
                       call "CONVERT" (earnstak(pointer%(1),1), 0.2,     ~
                                                       str(print$(4),,6))
                       call "CONVERT" (earnstak(pointer%(1),2), 2.2,     ~
                                                       str(print$(5),,9))
                       linenumber%(3) = 2
                       return
L23150:         REM HANDLES THIRD  CASE--ZAP VARIABLES
                    print$(3), print$(4), print$(5) = " "
                    linenumber%(3) = 3
                    colsdone% = colsdone% + 1
                    return

L24000:     REM FOURTH COLUMN--DEDUCTIONS INFORMATION
                on linenumber%(4) gosub L24030, L24050
                   return
L24030:         REM HANDLES FIRST  CASE--INITIALIZATION FOR PROCESSING
                    pointer%(2) = 0
L24050:         REM HANDLES SECOND CASE--PRINT A DEDUCTIONS LINE
                    pointer%(2) = pointer%(2) + 1
                    if pointer%(2) > dedxnptr% then L24220
                       print$(6) = str(dedxnstak$(pointer%(2)),4,12)
                       print$(7), print$(8), print$(9) = " "
                       temp% = val(str(dedxnstak$(pointer%(2)),2,2),2)
                       temp = dedxnstak(temp%,2)
                       if temp = gross then print$(7) = "      all" else ~
                            call "CONVERT" (temp, 2.2, str(print$(7),,9))
                       temp = dedxnstak(temp%,3)
                       print$(8), print$(9) = " "
                       if str(dedxnstak$(pointer%(2)),,1)<>"Y" then L24190
                          call "CONVERT" (temp, 2.2, str(print$(8),,9))
                          goto L24200
L24190:                call "CONVERT" (temp, 2.2, str(print$(9),,9))
L24200:             linenumber%(4) = 2
                    return
L24220:         REM HANDLES FOURTH CASE--ZAP VARIABLES
                    print$(6), print$(7), print$(8), print$(9) = " "
                    linenumber%(4) = 3
                    colsdone% = colsdone% + 1
                    return

L25000:     REM FIFTH  COLUMN--"OTHER" INFORMATION
                on linenumber%(5%) gosub L25040, L25090, L25160, L25210,     ~
                                         L25250, L25340, L25430, L25490,     ~
                                         L25530, L25570, L25610, L25650
                   return
L25040:         REM HANDLES FIRST CASE--SOCIAL SECURITY NUMBER
                    if ssn$ = " " then L25090
                       print$(11) = "SS#:" & ssn$
                       linenumber%(5) = 2
                       return
L25090:         REM HANDLES SECOND CASE--CHECK NUMBER IF EXTANT
                    if str(proof$,,1) = "Y" then L25430
                    print$(11) = "CHECK: " & checknr$
                    if checknr$=" " then print$(11) = "DSLIP: " & slipnr$
                    if checknr$=" " then slipnr$ = " "
                    linenumber%(5) = 3
                    return
L25160:         REM HANDLES THIRD CASE--DEPOSIT SLIP NUMBER IF EXTANT
                    if slipnr$ = " " then L25210
                    print$(11) = "DSLIP: " & slipnr$
                    linenumber%(5) = 4
                    return
L25210:         REM HANDLES FOURTH CASE A -- CHECK DATE
                    print$(11) = "DATED: " & checkdate$
                    linenumber%(5) = 5
                    return
L25250:         REM HANDLES FIFTH CASE--FIRST DATE PAY PERIOD
                    convert payfreq$ to temp%
                    firstpaydate$ = str(period$(), 2 + (temp%-1)*13, 6)
                    if firstpaydate$ = " " or ~
                       firstpaydate$ = blankdate$ then L25340
                       print$(11) = "FROM:          "
                       str(print$(11), 7) = firstpaydate$
                       call "DATEFMT" (str(print$(11), 7))
                       linenumber%(5) = 6
                       return
L25340:         REM HANDLES SIXTH CASE--LAST DATE PAY PERIOD
                    convert payfreq$ to temp%
                    lastpaydate$ = str(period$(), 8 + (temp%-1)*13, 6)
                    if lastpaydate$ = " " or ~
                       lastpaydate$ = blankdate$ then L25430
                       print$(11) = "TO:            "
                       str(print$(11), 7) = lastpaydate$
                       call "DATEFMT" (str(print$(11), 7))
                       linenumber%(5) = 7
                       return
L25430:         REM Handles Seventh Case--TAKEHOME = 0 and No Dir Doposit
                    if takehome <> 0 then L25650
                    if direct_deposit_flag$ = "Y" then L25650
                       print$(11%) = "***************"
                       linenumber%(5%) = 8%
                       return
L25490:         REM Handles Eighth Case--TAKEHOME = 0 and No Dir Doposit
                    print$(11%) = "*Deductions   *"
                    linenumber%(5%) = 9%
                    return
L25530:         REM Handles Ninth Case--TAKEHOME = 0 and No Dir Doposit
                    print$(11%) = "*Maybe Under  *"
                    linenumber%(5%) = 10%
                    return
L25570:         REM Handles Tenth Case--TAKEHOME = 0 and No Dir Doposit
                    print$(11%) = "*Withheld     *"
                    linenumber%(5%) = 11%
                    return
L25610:         REM Handles Eleventh Case--TAKEHOME = 0 and No Dir Doposit
                    print$(11%) = "***************"
                    linenumber%(5%) = 12%
                    return
L25650:         REM Handles Twelfth Case--Zap Variables
                    print$(11) = " "
                    linenumber%(5%) = 13%
                    colsdone% = colsdone% + 1%
                    return

L26000:     REM FIFTH  COLUMN--TOTALS LINE
                on linenumber%(6) gosub L26030, L26340
                   return
L26030:         REM HANDLES FIRST CASE
                   if colsdone% < 5 then return
                REM HANDLES SECOND CASE PRINT TOTALS
                    print$(3) = "GROSS"
                    call "CONVERT" (gunits, 0.2, str(print$(4%),,6%))
                    call "CONVERT" (gdollars, 2.2, str(print$(5%),,9%))
                    print$(6) = "DEDUCTIONS"
                    call "CONVERT" (deductions, 2.2, str(print$(8),,9))
                    call "CONVERT" (employer, 2.2, str(print$(9),,9))
                    takehome = gdollars - deductions
                    call "CONVERT" (takehome, 2.2, str(print$(10),,9))
                    print$(2) = "EMPLOYEE TOTALS"
                    linenumber%(6) = 2
                    return
L26340:         REM HANDLES THIRD CASE--ZAP VARIABLES
                    linenumber%(6) = 3
                    print$() = " "
                    colsdone% = colsdone% + 1
                    return

L28000:     REM PAGE CONTROL SUBROUTINE
                line% = line% + 1%
                if line% < 56% then return

                   if page% = 0% then L28080
                      if tagprinted% <> 0% then L28080
                         print using L62620
L28080:                  line% = line% + 1%
                   gosub page_control
                   return

L30000: REM *************************************************************~
            *   L O A D   E M P L O Y E E   M A S T E R   R E C O R D   *~
            *                                                           *~
            * LOADS THE EMPLOYEE MASTER RECORD FROM THE EMPLOYEE MASTER *~
            * FILE.                                                     *~
            *************************************************************

            get   # 3, using L30100, empcode$, payfreq$,                  ~
                   firstpaydate$, lastpaydate$, deductflag$, department$

L30100:     FMT CH(12),                  /* EMPLOYEE CODE              */~
                XX(4),                   /* WORKSTATION CODE           */~
                XX(1),                   /* FED FILING STATUS          */~
                XX(1),                   /* STATE FILING STATUS        */~
                CH(1),                   /* PAY FREQUENCY              */~
                XX(1),                   /* MODE OF PAYMENT            */~
                XX(4),                   /* DIRECT DEPOSIT BANK CODE   */~
                XX(12),                  /* D.D. ACCOUNT NUMBER        */~
                XX(18),                  /* CASH IN BANK ACCOUNT       */~
                                         /* GROSS PAYROLL ACCOUNT      */~
                XX(2),                   /* NORMAL HRS/DAY ASCII(##)   */~
                XX(1),                   /* AUTOPAYROLL FLAG           */~
                CH(6),                   /* FIRST DATE PAY PERIOD      */~
                CH(6),                   /* LAST DATE PAY PERIOD       */~
                CH(1),                   /* DEDUCT ON THIS PRLDDUCT RUN*/~
                XX(02),                  /* NORMAL HRS/WEEK ASCII(##)  */~
                XX(10),                  /* WORKMANS' COMP. CODE DEF   */~
                XX(1),                   /* JOBCODE                    */~
                XX(1),                   /* SHIFT CODE                 */~
                XX(2),                   /* AREA CODE                  */~
                XX(3),                   /* O/H NUMBER                 */~
                XX(2),                   /* PRIMARY STATE CODE         */~
                CH(4),                   /* DEPARTMENT                 */~
                XX(41)                   /* FILLER FOR REST OF RECORD  */

            ssn$, name$() = " "

            name$(3) = "NOT IN PERSONEL"
            name$(1)  = "FILE"
            namef$, nameb$  = "NOT IN PERSONEL FILE"

            if f1%(14) = 0 then return
            get #14, using L30550,                    /* FILE: PERMASTR */~
            name$(3),            /* Last name of person - part of pers */~
            name$(1),            /* First name of person               */~
            name$(2),            /* Middle name of person              */~
            ssn$

            if name$(2) <> " " then str(name$(2),2) = "."
            namef$ = name$(1) & " " & name$(2)
            namef$ = namef$ & " " & name$(3)
            nameb$ = name$(3) & ", " & name$(1)
            nameb$ = nameb$ & " " & name$(2)
            return

L30550: FMT                      /* FILE: PERMASTR                     */~
            XX(1),               /* General purpose status indicator   */~
            CH(15),              /* Last name of person - part of pers */~
            CH(10),              /* First name of person               */~
            CH(1),               /* Middle name of person              */~
            CH(11),              /* Social security number             */~
            XX(12),              /* employee code                      */~
            XX(10),              /* Telephone number                   */~
            XX(30),              /* Street address line 1              */~
            XX(30),              /* Street address line 2              */~
            XX(20),              /* City in address                    */~
            XX(20),              /* County in address                  */~
            XX(2),               /* State in address                   */~
            XX(9),               /* Zip code in address                */~
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
            XX(16),              /* Union status - personnel system    */~
            XX(16),              /* Bonding status - personnel system  */~
            XX(16),              /* Current job title - personnel syst */~
            XX(16),              /* EEO class of current job - personn */~
            XX(4),               /* Current department                 */~
            XX(1),               /* Current shift worked               */~
            XX(16),              /* Current supervisor                 */~
            XX(6),               /* Original date hired                */~
            XX(6),               /* Seniority date                     */~
            XX(6)                /* Last date rehired                  */

L31000: REM *************************************************************~
            *          L O A D   E A R N I N G S   R E C O R D          *~
            *                                                           *~
            * LOADS AN EARNINGS RECORD SO THAT WE CAN PUSH IT ONTO THE  *~
            * EARNINGS STACK.  THE REASON WE HAVE AN EARNINGS STACK IS  *~
            * SO THAT WE CAN A.) COMPUTE THE GROSS PAY IN ADVANCE OF    *~
            * PRINTING IT, AND B.) ACCUMULATE ALL OF THE EARNINGS TYPES *~
            * IRRESPECTIVE OF DEPARTMENT.                               *~
            *************************************************************

            get   # 4, using L31220, seqnr$,                              ~
                       type$, dept$, cash$, txbl$, units$, rate, acct$,  ~
                       usunits, usbucks, addunits, addamount, earn(1,1), ~
                       earn(1,2), earn(2,1), earn(2,2), earn(3,1),       ~
                       earn(3,2), earn(4,1), earn(4,2)
                     u3% = rate          /* REMOVE COMPILER MESSAGE    */
                     u3% = usunits
                     u3% = usbucks
                     u3% = addunits
                     u3% = addamount
            return

L31220:     FMT XX(12),                  /* EMPLOYEE CODE              */~
                CH(3),                   /* SEQUENCE NUMBER            */~
                XX(12),                  /* EMPLOYEE CODE (AGAIN)      */~
                CH(12),                  /* EARNINGS TYPE              */~
                CH(4),                   /* DEPARTMENT CODE            */~
                CH(1),                   /* PAID IN CASH? FLAG         */~
                CH(1),                   /* IS THIS TAXABLE?           */~
                CH(6),                   /* UNIT DESCRIPTION           */~
                PD(14,4),                /* UNIT RATE                  */~
                CH(9),                   /* EXPENSE ACCOUNT NUMBER     */~
                                                                         ~
                PD(14,4),                /* USUAL PAYROLL   UNITS      */~
                PD(14,4),                /* USUAL PAYROLL   AMOUNT     */~
                PD(14,4),                /* BUFFER ADD      UNITS      */~
                PD(14,4),                /* BUFFER ADD      AMOUNT     */~
                PD(14,4),                /* CURRENT CHECK   UNITS      */~
                PD(14,4),                /* CURRENT CHECK   AMOUNT     */~
                PD(14,4),                /* MONTH   TO DATE UNITS      */~
                PD(14,4),                /* MONTH   TO DATE AMOUNT     */~
                PD(14,4),                /* QUARTER TO DATE UNITS      */~
                PD(14,4),                /* QUARTER TO DATE AMOUNT     */~
                PD(14,4),                /* YEAR    TO DATE UNITS      */~
                PD(14,4)                 /* YEAR    TO DATE AMOUNT     */~

L32000: REM *************************************************************~
            *         L O A D   D E D U C T I O N   R E C O R D         *~
            *                                                           *~
            * LOADS A DEDUCTION RECORD SO THAT WE CAN PUT IT IN ITS     *~
            * STACK.  NOTE THAT WE HAVE TO DO THAT INSTEAD OF PRINTING  *~
            * DIRECTLY SINCE WE NEED TO KNOW THE TOTAL DEDUCTIONS       *~
            * FOR PRINTING THE "NET PAY" FIELD.                         *~
            *************************************************************

            get   # 5, using L32180,                                      ~
                       dedcat$, dedmethod$, deduction$, dedempflag$,     ~
                       dedcracct$, deddbacct$, dedapplies$, amount(1),   ~
                       amount(2), amount(3), amount(4), amount(5),       ~
                       dedamt(1), dedamt(2), dedamt(3), dedamt(4),       ~
                       unitbase, dollarbase, bankacct$, bankcode$,       ~
                       banksp$
            return

L32180:     FMT XX(12),                  /* EMPLOYEE CODE              */~
                XX(3),                   /* SEQUENCE NUMBER            */~
                XX(12),                  /* EMPLOYEE NUMBER            */~
                CH(6),                   /* DEDUCTION CATEGORY (LOC?!) */~
                CH(6),                   /* METHOD OF DEDUCTION (DDT)  */~
                CH(12),                  /* DEDUCTION DESCRIPTION      */~
                CH(1),                   /* EMPLOYEE PAYS THIS? FLAG   */~
                CH(9),                   /* CREDIT ACCOUNT             */~
                CH(9),                   /* DEBIT ACCOUNT              */~
                CH(6),                   /* APPLIES FIELD (123456)     */~
                PD(14,4),                /* AMOUNT 1 OR 0 IF NOT USED  */~
                PD(14,4),                /* AMOUNT 2 OR 0 IF NOT USED  */~
                PD(14,4),                /* AMOUNT 3 OR 0 IF NOT USED  */~
                PD(14,4),                /* AMOUNT 4 OR 0 IF NOT USED  */~
                PD(14,4),                /* GOAL                       */~
                PD(14,4),                /* CURRENT DEDUCTION AMOUNT   */~
                PD(14,4),                /* MTD         "        "     */~
                PD(14,4),                /* QTD         "        "     */~
                PD(14,4),                /* YTD         "        "     */~
                XX(8),                   /* SKIP                       */~
                PD(14,4),        /* Current Units Subject To Deduction */~
                PD(14,4),        /* Current Dollars Subject To Deducti */~
                XX(48),                  /* SKIP                       */~
                CH(20),                  /* BANK ACCOUNT NUMBER        */~
                CH(04),                  /* BANK CODE                  */~
                CH(02)                   /* SURE PAY CODE              */

L35000: REM *************************************************************~
            *  SET PROCESSING RANGES                                    *~
            *************************************************************

            init(hex(00)) dept$(1), empcode$(1), payfreq$(1)
            init(hex(ff)) dept$(2), empcode$(2), payfreq$(2)

            call "READ100" (#1, "PRL PROCESSING RANGE", f1%(1))
                     if f1%(1) = 0 then return
                     get #1, using L35340, str(dept$(), 1),               ~
                                          str(empcode$(), 1),            ~
                                          str(payfreq$(), 1)

            if dept$(1) <> "ALL" then L35170
               init(hex(00)) dept$(1)
               init(hex(ff)) dept$(2)
               go to L35210
L35170:     if dept$(2) <> " " then L35190
               dept$(2) = dept$(1)
L35190:        dept$(1) = dept$(1) addc all(hex(ff))

L35210:     if empcode$(1) <> "ALL" then L35250
               init(hex(00)) empcode$(1)
               init(hex(ff)) empcode$(2)
               go to L35290
L35250:     if empcode$(2) <> " " then L35270
               empcode$(2) = empcode$(1)
L35270:        empcode$(1) = empcode$(1) addc all(hex(ff))

L35290:     if payfreq$(2) <> " " then L35310
               payfreq$(2) = payfreq$(1)
L35310:        payfreq$(1) = payfreq$(1) addc all(hex(ff))
               return

L35340:              FMT       XX(20), CH(8), CH(24), CH(2)

L40000: REM *************************************************************~
            *    R E P O R T   P R I N T I N G   S U B R O U T I N E    *~
            *                                                           *~
            * PRINTS THE EARNINGS/DEDUCTION RECAP BY DEPARTMENT.        *~
            *************************************************************

            line% = 1000%
            page% = 0%
            reportid% = 2%
            init(hex(ff)) dedxnstak$(), dedxxstak$(), earnstak$()
            mat earnstak = zer
            mat dedxnstak = zer
            earnptr%, dedxnptr%, dedxxptr% = 0%

            colsdone% = 0%
            mat linenumber% = con
            go1%, go2%, go3%, e1, e2, d1, d2, tot1, tot2, tot3, tot4 = 0

            REM LOOP THROUGH COMPUTING AND PRINTING LINES UNTIL DONE.
L40180:         for column% = 1 to 3
                    on column% gosub L40320, L40790, L41200
                    next column%
                if colsdone% < 3 then L40250
                   REM EXIT ROUTINE FOR FINISHED.
                       return

L40250:         gosub L41620              /* PAGE HEADING, IF NECCESSARY*/
                print using L63100, print$(1), print$(2), print$(3),      ~
                                   print$(4), print$(5), print$(6),      ~
                                   print$(7), print$(8), print$(9),      ~
                                   print$(10)
                goto L40180

L40320:     REM FIRST  COLUMN--EARNINGS STACK
                on linenumber%(1) gosub L40350, L40520, L40640, L40730
                   return
L40350:         REM HANDLES FIRST  CASE--PRINT AN ENTRY
                    go1% = go1% + 1
                    if go1% > eptr% then L40520
                    if go1% = 1 then prevdept1$ = str(estack$(go1%),,4)
                    if prevdept1$ <> str(estack$(go1%),,4) then L40520
                    print$(1) = str(estack$(go1%),,4)
                    print$(2) = str(estack$(go1%), 5, 12)
                    convert str(estack$(go1%), 17, 4) to t%
                    print$(3), print$(4) = " "
                    call "CONVERT" (estak1(t%), 0.4, str(print$(3),,11))
                    call "CONVERT" (estak2(t%), 2.2, str(print$(4),,13))
                    e1 = e1 + estak1(t%)
                    e2 = e2 + estak2(t%)
                    gosub'200(str(estack$(go1%),5,12), estak1(t%),       ~
                                                              estak2(t%))
                    linenumber%(1) = 1
                    return
L40520:         REM HANDLES SECOND CASE--DEPARTMENT TOTAL
                    print$(1) = prevdept1$
                    print$(2) = "*SUB TOTAL*"
                    print$(3), print$(4) = " "
                    call "CONVERT" (e1, 0.4, str(print$(3),,11))
                    call "CONVERT" (e2, 2.2, str(print$(4),,13))
                    prevdept1$ = str(estack$(go1%), 1, 4)
                    tot1 = tot1 + e1
                    tot2 = tot2 + e2
                    e1, e2 = 0
                    linenumber%(1) = 3
                    return
L40640:         REM HANDLES THIRD CASE--TAG LINE
                    print$(1) = "----"
                    print$(2) = "------------"
                    print$(3) = "-----------------"
                    print$(4) = "----------------"
                    if go1% > eptr% then linenumber%(1) = 4              ~
                                    else linenumber%(1) = 1
                    go1% = go1% - 1
                    return
L40730:         REM HANDLES FOURTH CASE--ZAP VARIABLES
                    print$(1), print$(2), print$(3), print$(4) = " "
                    colsdone% = colsdone% + 1
                    linenumber%(1) = 5
                    return

L40790:     REM SECOND COLUMN--DEDUCTION STACK
                on linenumber%(2) gosub L40820, L40960, L41060, L41140
                   return
L40820:         REM HANDLES FIRST  CASE--PRINT AN ENTRY
                    go2% = go2% + 1
                    if go2% > dptr% then L40960
                    if go2% = 1 then prevdept2$ = str(dstack$(go2%), 1, 4)
                    if prevdept2$ <> str(dstack$(go2%), 1, 4) then L40960
                    print$(5) = str(dstack$(go2%), 1, 4)
                    print$(6) = str(dstack$(go2%), 5, 12)
                    convert str(dstack$(go2%), 17, 4) to t%
                    print$(7) = " "
                    call "CONVERT" (dstak(t%), 2.2, str(print$(7),,13))
                    d1 = d1 + dstak(t%)
                    gosub'201(str(dstack$(go2%),5,12), dstak(t%))
                    linenumber%(2) = 1
                    return
L40960:         REM HANDLES SECOND CASE--DEPARTMENT TOTAL
                    print$(5) = prevdept2$
                    print$(6) = "*SUB TOTAL*"
                    print$(7) = " "
                    call "CONVERT" (d1, 2.2, str(print$(7),,13))
                    prevdept2$ = str(dstack$(go2%), 1, 4)
                    tot3 = tot3 + d1
                    d1 = 0
                    linenumber%(2) = 3
                    return
L41060:         REM HANDLES THIRD CASE--TAG LINE
                    print$(5) = "----"
                    print$(6) = "------------"
                    print$(7) = "----------------"
                    if go2% > dptr% then linenumber%(2) = 4              ~
                                    else linenumber%(2) = 1
                    go2% = go2% - 1
                    return
L41140:         REM HANDLES FOURTH CASE--ZAP VARIABLES
                    print$(5), print$(6), print$(7) = " "
                    colsdone% = colsdone% + 1
                    linenumber%(2) = 5
                    return

L41200:     REM SECOND COLUMN--DEDUCTION STACK
                on linenumber%(3) gosub L41230, L41380, L41480, L41560
                   return
L41230:         REM HANDLES FIRST  CASE--PRINT AN ENTRY
                    go3% = go3% + 1
                    if emprptr% = 0 then L41480
                    if go3% > emprptr% then L41380
                    if go3% = 1 then prevdept3$ = str(emprstack$(go3%),,4)
                    if prevdept3$ <> str(emprstack$(go3%),,4) then L41380
                    print$(8) = str(emprstack$(go3%), 1, 4)
                    print$(9) = str(emprstack$(go3%), 5, 12)
                    convert str(emprstack$(go3%), 17, 4) to t%
                    print$(10) = " "
                    call "CONVERT" (emprstak(t%),2.2,str(print$(10),,13))
                    d2 = d2 + emprstak(t%)
                    gosub'202(str(emprstack$(go3%),5,12), emprstak(t%))
                    linenumber%(3) = 1
                    return
L41380:         REM HANDLES SECOND CASE--DEPARTMENT TOTAL
                    print$(8) = prevdept3$
                    print$(9) = "*SUB TOTAL*"
                    print$(10) = " "
                    call "CONVERT" (d2, 2.2, str(print$(10),,13))
                    prevdept3$ = str(emprstack$(go3%), 1, 4)
                    tot4 = tot4 + d2
                    d2 = 0
                    linenumber%(3) = 3
                    return
L41480:         REM HANDLES THIRD CASE--TAG LINE
                    print$(8) = "----"
                    print$(9) = "------------"
                    print$(10) = "----------------"
                    if go3% > emprptr% then linenumber%(3) = 4           ~
                                    else linenumber%(3) = 1
                    go3% = go3% - 1
                    return
L41560:         REM HANDLES FOURTH CASE--ZAP VARIABLES
                    print$(8), print$(9), print$(10) = " "
                    colsdone% = colsdone% + 1
                    linenumber%(3) = 5
                    return

L41620:     REM PAGE CONTROL SUBROUTINE

                line% = line% + 1%
                if line% < 56% then return
                   if page% = 0% then L41680
                     print using L62980
L41680:            gosub page_control
                   return

L42000: REM *************************************************************~
            *    R E P O R T   P R I N T I N G   S U B R O U T I N E    *~
            *                                                           *~
            * PRINTS THE EARNINGS/DEDUCTION RECAP BY DEPARTMENT.        *~
            *************************************************************

            line% = 1000%
            page% = 0%
            reportid% = 3%

            colsdone%, go1%, go2%, go3% = 0%
            mat linenumber% = con

            REM LOOP THROUGH COMPUTING AND PRINTING LINES UNTIL DONE.
L42170:         for column% = 1 to 3
                    on column% gosub L42330, L42510, L42690
                    next column%
                if colsdone% < 3 then L42260
                   REM EXIT ROUTINE FOR FINISHED.
                       gosub L42870
                       print using L62980
                       return

L42260:         gosub L42870              /* PAGE HEADING, IF NECCESSARY*/
                print using L63100, print$(1), print$(2), print$(3),      ~
                                   print$(4), print$(5), print$(6),      ~
                                   print$(7), print$(8), print$(9),      ~
                                   print$(10)
                goto L42170

L42330:     REM FIRST  COLUMN--EARNINGS STACK
                on linenumber%(1) gosub L42360, L42450
                   return
L42360:         REM HANDLES FIRST  CASE--PRINT AN ENTRY
                    if earnptr% <= 0% then L42430
                    t%, go1% = go1% + 1
                    print$(2) = str(earnstak$(t%),,12)
                    print$(1), print$(3), print$(4) = " "
                    call "CONVERT"(earnstak(t%,1),0.4,str(print$(3),,11))
                    call "CONVERT"(earnstak(t%,2),2.2,str(print$(4),,13))
                    linenumber%(1) = 1
L42430:             if go1% >= earnptr% then linenumber%(1) = 2
                    return
L42450:         REM HANDLES FOURTH CASE--ZAP VARIABLES
                    print$(1), print$(2), print$(3), print$(4) = " "
                    colsdone% = colsdone% + 1
                    linenumber%(1) = 3
                    return

L42510:     REM SECOND COLUMN--DEDUCTION STACK
                on linenumber%(2) gosub L42540, L42630
                   return
L42540:         REM HANDLES FIRST  CASE--PRINT AN ENTRY
                    if dedxnptr% <= 0% then L42610
                    go2% = go2% + 1
                    print$(6) = str(dedxnstak$(go2%),,12)
                    t% = val(str(dedxnstak$(go2%),13,2), 2)
                    print$(5), print$(7) = " "
                    call"CONVERT"(dedxnstak(t%,1),2.2,str(print$(7),,13))
                    linenumber%(2) = 1
L42610:             if go2% >= dedxnptr% then linenumber%(2) = 2
                    return
L42630:         REM HANDLES FOURTH CASE--ZAP VARIABLES
                    print$(5), print$(6), print$(7) = " "
                    colsdone% = colsdone% + 1
                    linenumber%(2) = 3
                    return

L42690:     REM SECOND COLUMN--DEDUCTION STACK EMPLOYER
                on linenumber%(3) gosub L42720, L42810
                   return
L42720:         REM HANDLES FIRST  CASE--PRINT AN ENTRY
                    if dedxxptr% <= 0% then L42790
                    go3% = go3% + 1
                    print$(9) = str(dedxxstak$(go3%),,12)
                    t% = val(str(dedxxstak$(go3%),13,2), 2)
                    print$(8), print$(10) = " "
                   call"CONVERT"(dedxnstak(t%,2),2.2,str(print$(10),,13))
                    linenumber%(3) = 1
L42790:             if go3% >= dedxxptr% then linenumber%(3) = 2
                    return
L42810:         REM HANDLES FOURTH CASE--ZAP VARIABLES
                    print$(8), print$(9), print$(10) = " "
                    colsdone% = colsdone% + 1
                    linenumber%(3) = 3
                    return

L42870:     REM PAGE CONTROL SUBROUTINE
                line% = line% + 1%
                if line% < 56% then return
                   if page% = 0% then L42940
                         print using L62980
L42940:            gosub page_control
                   return

L50000: REM *************************************************************~
            *    R E P O R T   P R I N T I N G   S U B R O U T I N E    *~
            *                                                           *~
            * PRINTS THE DIRECT DEPOSIT SURE PAY REPORT.                *~
            *************************************************************

            line% = 1000%
            page% = 0%
            pagetotal, banktotal, rpttotal = 0
            reportid% = 4%
            readkey$, string$, bankid$ = " "
            if str(proof$,,1) = "Y" then string$ = "** NOT NEGOTIABLE **"
L50100:     call "PLOWNEXT" (#6, readkey$, 0%, f1%(6))
                if f1%(6) <> 0 then L50190
                if page% = 0% then return
                gosub d_d_page_total
                gosub d_d_bank_total
                gosub page_control
                print
                print using L63430, "GRAND TOTAL", rpttotal, string$
                return

L50190:     get #6, using L50210, route$, empcode$, bankacct$, banksp$,   ~
                                 amount, nameb$
L50210:     FMT CH(9), CH(12), CH(20), XX(2), CH(02), PD(14,4), CH(20)

            if bankid$ = " " then gosub page_control
            if bankid$ = " " then bankid$ = route$
            if bankid$ = route$ then L50225
                gosub d_d_page_total
                gosub d_d_bank_total
                gosub page_control

L50225:     if line% < 56% then L50240
                gosub d_d_page_total
                gosub page_control

L50240:     amount = round(amount, 2)
            print using L63410, banksp$, route$, bankacct$, amount,       ~
                               empcode$, nameb$, string$
            pagetotal = pagetotal + amount
            line%     = line% + 1%
            if str(proof$,,1) ="Y" or tape% = 0% or amount = 0 then L50340
                convert amount * 100 to amount$, pic(0000000000)
L50280:         call "GETDTTM" addr(str(dtstamp$,,7))
                write #9 using L50320, "6", banksp$, route$, bankacct$,   ~
                                      amount$, empcode$, nameb$,         ~
                                      " ", "0", " ", " ", " ", dtstamp$, ~
                                      eod goto L50280
L50320:              FMT CH(1), CH(2), CH(9), CH(17), CH(10), CH(15),    ~
                         CH(22), CH(2), CH(1), CH(8), 3*CH(7)
L50340:     goto L50100

        d_d_page_total
            print using L63460
            print using L63430," PAGE TOTAL", pagetotal, string$
            print
            banktotal = banktotal + pagetotal
            pagetotal = 0
            return

        d_d_bank_total
            print using L63430," BANK TOTAL", banktotal, string$
            rpttotal = rpttotal + banktotal
            banktotal = 0
            bankid$ = route$
            return

        REM *************************************************************~
            *        S T A C K   P U S H I N G   R O U T I N E S        *~
            *                                                           *~
            * PUSHES THE INDICATED INFORMATION ONTO THE DESIRED STACK   *~
            * FOR LATER PROCESSING.                                     *~
            *************************************************************

            deffn'162(etype$, units, amount)
                  search str(earnstak$(),1) = str(etype$) to location$   ~
                                                                  step 12
                  if location$ = hex(0000) then L60150
                     temp% = int(val(location$,2)/12)+1
                     earnstak(temp%,1) = earnstak(temp%,1) + units
                     earnstak(temp%,2) = earnstak(temp%,2) + amount
                     return
L60150:           REM PUSH NEW ITEM ONTO STACK.
                      earnptr% = earnptr% + 1
                      earnstak$(earnptr%) = etype$
                      earnstak(earnptr%,1) = units
                      earnstak(earnptr%,2) = amount
                      return

            deffn'163(deduction$, amount)
                  if dedempflag$ <> "Y" then dedempflag$ = "N"
                  temp$ = dedempflag$
                  str(temp$,4) = str(deduction$)
                  REM PUSH NEW ENTRY ONTO SALES ACCOUNT STACK.
                      dedxnptr% = dedxnptr% + 1
                      dedxnstak$(dedxnptr%) = temp$
                      str(dedxnstak$(dedxnptr%),2,2) = bin(dedxnptr%,2)
                      dedxnstak(dedxnptr%,1) = unitbase
                      dedxnstak(dedxnptr%,2) = dollarbase
                      dedxnstak(dedxnptr%,3) = amount
                      return

            deffn'172(dpt$, etype$, units, amount)
                  if dpt$ <> lastdpt$ then departments%=departments% + 1
                  lastdpt$ = dpt$
                  string$ = dpt$
                  str(string$, 5) = etype$
                  search str(estack$(),1) = str(string$, 1, 16)          ~
                                         to location$ step 20
                  if location$ = hex(0000) then L60470
                     temp% = int(val(location$,2)/20)+1
                     estak1(temp%) = estak1(temp%) + units
                     estak2(temp%) = estak2(temp%) + amount
                     return
L60470:           REM PUSH NEW ITEM ONTO STACK.
                      eptr% = eptr% + 1
                      estack$(eptr%) = string$
                      convert eptr% to                                   ~
                               str(estack$(eptr%), 17, 4), pic(####)
                      estak1(eptr%) = units
                      estak2(eptr%) = amount
                      return

            deffn'173(dpt$, deduction$, amount)
                  if dpt$ <> lastdpt$ then departments%=departments% + 1
                  lastdpt$ = dpt$
                  string$ = dpt$
                  str(string$, 5) = deduction$
                  search str(dstack$(),1) = str(string$, 1, 16)          ~
                               to location$ step 20
                  if location$ = hex(0000) then L60670
                     temp% = int(val(location$,2)/20)+1
                     dstak(temp%) = dstak(temp%) + amount
                     return
L60670:           REM PUSH NEW ENTRY ONTO STACK.
                      dptr% = dptr% + 1
                      dstack$(dptr%) = string$
                      convert dptr% to                                   ~
                               str(dstack$(dptr%), 17, 4), pic(####)
                      dstak (dptr%) = amount
                      return

            deffn'174(dpt$, deduction$, amount)
                  if dpt$ <> lastdpt$ then departments%=departments% + 1
                  lastdpt$ = dpt$
                  string$ = dpt$
                  str(string$, 5) = deduction$
                  search str(emprstack$(),1) = str(string$, 1, 16)       ~
                               to location$ step 20
                  if location$ = hex(0000) then L60860
                     temp% = int(val(location$,2)/20)+1
                     emprstak(temp%) = emprstak(temp%) + amount
                     return
L60860:           REM PUSH NEW ENTRY ONTO STACK.
                      emprptr% = emprptr% + 1
                      emprstack$(emprptr%) = string$
                      convert emprptr% to                                ~
                            str(emprstack$(emprptr%), 17, 4), pic(####)
                      emprstak (emprptr%) = amount
                      return

            deffn'200(etype$, units, amount) /*Stack reused for Gtotals*/
                  search str(earnstak$(),1) = str(etype$) to location$   ~
                                                                  step 12
                  if location$ = hex(0000) then L61020
                     temp% = int(val(location$,2)/12)+1
                     earnstak(temp%,1) = earnstak(temp%,1) + units
                     earnstak(temp%,2) = earnstak(temp%,2) + amount
                     return
L61020:           REM PUSH NEW ITEM ONTO STACK.
                      earnptr% = earnptr% + 1
                      earnstak$(earnptr%) = etype$
                      earnstak(earnptr%,1) = units
                      earnstak(earnptr%,2) = amount
                      return

            deffn'201(deduction$, amount)    /*Stack reused for Gtotals*/
                  search str(dedxnstak$(),1) = str(deduction$) to        ~
                                                        location$ step 15
                  if location$ = hex(0000) then L61160
                     temp% = int(val(location$,2)/15)+1
L61140:              dedxnstak(temp%,1) = dedxnstak(temp%,1) + amount
                     return
L61160:           REM PUSH NEW ENTRY ONTO SALES ACCOUNT STACK.
                      temp% = dedxnptr% + 1
                      if temp% < 101% then L61210
                          temp% = 100%
                          goto L61140
L61210:               dedxnptr% = dedxnptr% + 1
                      dedxnstak$(dedxnptr%) = deduction$
                      str(dedxnstak$(dedxnptr%),13,2) = bin(dedxnptr%,2)
                      dedxnstak(dedxnptr%,1) = amount
                      return

            deffn'202(deduction$, amount)    /*Stack reused for Gtotals*/
                  search str(dedxxstak$(),1) = str(deduction$) to        ~
                                                        location$ step 15
                  if location$ = hex(0000) then L61340
                     temp% = int(val(location$,2)/15)+1
L61320:              dedxnstak(temp%,2) = dedxnstak(temp%,2) + amount
                     return
L61340:           REM PUSH NEW ENTRY ONTO SALES ACCOUNT STACK.
                      temp% = dedxxptr% + 1
                      if temp% < 101% then L61390
                          temp% = 100%
                          goto L61320
L61390:               dedxxptr% = dedxxptr% + 1
                      dedxxstak$(dedxxptr%) = deduction$
                      str(dedxxstak$(dedxxptr%),13,2) = bin(dedxxptr%,2)
                      dedxnstak(dedxxptr%,2) = amount
                      return

        page_control
            if printer% > 0% then L62080
                call "SETPRNT" (rptid$, " ", 0%, 0%)
                select printer (134)
                runtime$ = " "
                call "TIME" (runtime$)
                printer% = 1%

L62080:     print page
            page% = page% + 1%
            print using L62200, date$, runtime$, company$, "PRLJURN0",    ~
                               rptid$
            print using L62220, reporthd$(reportid%), page%
            print
            on reportid% goto L62400, L62800, L62800, L63200
            return

L62200: %RUN ######## @ ########              ###########################~
        ~#################################                 ########:######
L62220: %                                    ############################~
        ~################################                      Page: ####

L62400
*        Reportid% = 1, Individual employee report headings
            print using L62500
            print using L62530
            print using L62560, ":"
            print using L62590
            print using L62620
            line% = 8%                 /* SET STARTING LINE ON PAGE  */
            tagprinted% = 1%
            return

L62500: %+------------+-----------------+------------------------------+-~
        ~-------------------------------+---------+---------+-------------~
        ~--+
L62530: %!  EMPLOYEE  !        LAST     !  E A R N I N G S   I N F O   ! ~
        ~      D E D U C T I O N S      ! EMPLOYER! NET PAY !   O T H E R ~
        ~  !
L62560: %!            !  NAME# FIRST    +------------+-------+---------+-~
        ~-----------+-------------------+   PAID  !         !             ~
        ~  !
L62590: %!    CODE    !        MIDDLE   !    TYPE    ! UNITS !  AMOUNT !D~
        ~ESCRIPTIONS! SUBJECT !  AMOUNT !  AMOUNT !         !             ~
        ~  !
L62620: %+------------+-----------------+------------+-------+---------+-~
        ~-----------+---------+---------+---------+---------+-------------~
        ~--+
L62650: %!############!#################!############!#######!#########!#~
        ~###########!#########!#########!#########!#########!#############~
        ~##!

L62800
*        Reportid% = 2 or 3, Department or company recaps
            print using L62900
            print using L62940
            print using L62980
            if reportid% = 2% then print using L63020                     ~
                else print using L63060
            print using L62980
            line% = 8%                  /* SET STARTING LINE ON PAGE  */
            return


L62900: %+----------------------------------------------------+  +-------~
        ~---------------------------+  +----------------------------------~
        ~+

L62940: %!                  E A R N I N G S                   !  !       ~
        ~D E D U C T I O N S        !  !  E M P L O Y E R S    S H A R E  ~
        ~!

L62980: %!----+------------+-----------------+----------------!  !----+--~
        ~----------+----------------!  !----+------------+----------------~
        ~!

L63020: %!DEPT! TYPE       !      UNITS      !     DOLLARS    !  !DEPT!  ~
        ~TYPE      !     DOLLARS    !  !DEPT!  TYPE      !     DOLLARS    ~
        ~!

L63060: %!    ! TYPE       !      UNITS      !     DOLLARS    !  !    !  ~
        ~TYPE      !     DOLLARS    !  !    !  TYPE      !     DOLLARS    ~
        ~!

L63100: %!####!############!#################!################!  !####!##~
        ~##########!################!  !####!############!################~
        ~!

L63200
*        Reportid% = 4, Direct Deposits
                   print using L63340
                   print using L63350, "#"
                   print using L63370
                   print using L63390
                   line% = 7%             /* SET STARTING LINE ON PAGE  */
                   return

L63340: %TRAN  RECEIVING
L63350: %CODE   FRB/ANA    ACCOUNT NUMBER        AMOUNT INDIVIDUAL I.D.# ~
        ~           NAME
L63370: % 2-3     4-12          13-29            30-39       40-54       ~
        ~          55-76
L63390: %----------------------------------------------------------------~
        ~--------------------------
L63410: % ##   #########   ################# #######.##      ############~
        ~### #################### ####################
L63430: %      ############################ ########.##   ###############~
        ~#####

L63460: %                                   -----------

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            return_code% = 0

            REM Funs over, time to move on...
            if printer% > 0% then call "SETPRNT" (rptid$, " ", 0%, 1%)
            call "SHOSTAT" ("One Moment Please")
            call "FILEBGON" (#6)
            end return_code%
