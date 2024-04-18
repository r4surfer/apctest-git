        REM *************************************************************~
            *                                                           *~
            *  PPPP   RRRR   L      TTTTT   OOO    GGG   L              *~
            *  P   P  R   R  L        T    O   O  G      L              *~
            *  PPPP   RRRR   L        T    O   O  G GGG  L              *~
            *  P      R   R  L        T    O   O  G   G  L              *~
            *  P      R   R  LLLLL    T     OOO    GGG   LLLLL          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLTOGL  - POSTS THE DEDUCTIONS TO THE G/L AND ALSO       *~
            *            RELIEVES THE ACCRUED GROSS PAYROLL, CREDITING  *~
            *            CASH IN BANK PAYMENTS.                         *~
            *            UPDATES (ROLLS) THE MTD, QTD, AND YTD ACCRUALS *~
            *            STORED IN THE EARNINGS AND DEDUCTIONS RECORDS. *~
            *            ALSO UPDATES THE SICK & VACATION ACCRUALS.     *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/30/81 ! ORIGINAL                                 ! BCW *~
            * 10/19/81 ! USE RANGE TO PROCESS.  NO PRLERNRG.      ! TEM *~
            * 08/30/82 ! ADDED PRLERNG WITH RECONCILATION         ! ECR *~
            * 26/28/84 ! ADDED SICK AND VACATION ACCRUALS         ! HES *~
            * 05/10/85 ! MODIFIED FOR GLDETAIL RECORD EXPANSION   ! RAC *~
            * 12/10/85 ! CHANGED CHECK FILE LAYOUT.               ! HES *~
            * 09/03/86 ! Changes related to deirect deposits      ! HES *~
            * 12/26/90 ! Changed Call to JNLINFO for GL Batches,  ! RAC *~
            *          !    added Call to JNLCLOSE for GL Batches !     *~
            * 06/19/91 ! Modifications for GL Export File         ! JBK *~
            * 06/20/91 ! Fixed PRR 11933- Initialize variables    ! JBK *~
            *          ! Added CALL to ALLFREE                    !     *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            * 12/02/92 ! PRR 11845 - Vacation & Sick Accruals     ! JBK *~
            *          !  Report - Added calculation of units     !     *~
            *          !  added and taken dollar value, Added     !     *~
            *          !  totals for all columns, brought report  !     *~
            *          !  header up to standards.                 !     *~
            * 08/27/96 ! Changes for the year 2000.               ! DXL *~
            *************************************************************

        dim                                                              ~
            account$9,                   /* ACCOUNT ARG FOR STACKS     */~
            acct$9,                      /* EARNINGS ACCOUNT NUMBER    */~
            amount(5),                   /* AMOUNTS FOR DEDUCTIONS     */~
            bacct$17,                    /* Employee Bank Account No.  */~
            bacct$(100)17,               /* Employee Bank Account No.  */~
            bcode$4,                     /* Employee Bank Code         */~
            bcode$(100)4,                /* Employee Bank Code         */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            bsure$2,                     /* Employee Account Type      */~
            bsure$(100)2,                /* Employee Account Type      */~
            cash$1,                      /* EARNING PAID IN CASH? FLAG */~
            cashacct$9,                  /* CASH IN BANK ACCOUNT NUMBER*/~
            changedate$6,                /* ACCRUAL METHOD CHANGE DATE */~
            changeflag$2,                /* MTHD CHANGED FLAG FOR PRINT*/~
            checknr$8,                   /* CHECK NUMBER               */~
            company$60,                  /* Company or Division Name   */~
            creditsstk$(250)12,          /* CREDIT ACCOUNT RECAP STACK */~
            creditsstk(250),             /* PARALLEL STACK FOR AMOUNTS */~
            date$(2)6,                   /* TEMPORARY STORGE           */~
            debitsstk$(250)12,           /* DEBIT RECAP STACK          */~
            debitsstk(250),              /* PARALLEL STACK FOR AMOUNTS */~
            dedamt(4),                   /* DEDUCTIONS AMT ACCUMULATORS*/~
            dedunt(4),                   /* DEDUCTIONS UNITS SUBJECT AC*/~
            deddol(4),                   /* DEDUCTIONS DOLLARS SUBJECT */~
            dedapplies$6,                /* DEDUCTION APPLIES FIELD    */~
            dedcat$6,                    /* DEDUCTION CATEGORY         */~
            dedcracct$9,                 /* DEDUCTION CREDIT ACCOUNT # */~
            deddbacct$9,                 /* DEDUCTION DEBIT ACCOUNT #  */~
            dedempflag$1,                /* EMPLOYEE PAYS THIS? FLAG   */~
            dedforstak$18,               /* METHOD & DESR CONCANT      */~
            dedhold$8,                   /* NOT USED                   */~
            dedmethod$6,                 /* METHOD OF DEDUCTION        */~
            dedrecord$100,               /* FREE BYTES END OF DEDXN REC*/~
            deductflag$1,                /* PROCESS PAYROLL THIS GUY?  */~
            deduction$12,                /* DEDUCTION DESCRIPTION      */~
            dedxnstak(100,3),            /* DEDUCTION AMOUNTS STACK    */~
            dedxnstak$(100)18,           /* DEDUCTION DESCRIPTION STACK*/~
            dedxnstakcr$(100)9,          /* DEDUCTION CREDIT ACCOUNT   */~
            dedxnstakdb$(100)9,          /* DEDUCTION DEBIT ACCOUNT    */~
            dedxnstak1$(100)6,           /* DEDUCTION CATEGORY STACK   */~
            dedxnstak2$(100)1,           /* DEDUCTION PAID BY STACK    */~
            department$4,                /* DEPARTMENT CODE THIS GUY   */~
            dept$(2)4,                   /* DEPARTMENT RANGE           */~
            earn(4,2),                   /* EARNINGS AMOUNTS           */~
            earnrecord$32,               /* UNUSED EARNINGS REC. BYTES */~
            earnstak$(100)18,            /* EARNINGS DESCRIPTIONS/UNITS*/~
            earnstakdb$(100)9,           /* EARNINGS CREDIT DEBIT      */~
            earnstak1$(100)4,            /* EARNINGS CATEGORY STACK    */~
            earnstak(100,3),             /* EARNINGS AMOUNTS STACK     */~
            ecat$4,                      /* EARNINGS CATEGORY CODE     */~
            empcode$12,                  /* EMPLOYEE CODE THIS EMP.    */~
            empcode$(2)12,               /* EMPLOYEE CODE RANGE        */~
            emprecord$(50)10,            /* EMPLOYEE RECORD FOR UPDATES*/~
            firstpaydate$8,              /* FIRST DATE THIS PAY PERIOD */~
            grossacct$9,                 /* GROSS PAYROLL ACCOUNT      */~
            hdrdate$45,                  /* DATE FOR RECAP PRINTING    */~
            hiredate$8,                  /* DATE GUY WAS HIRED         */~
            jnlid$3,                     /* JOURNAL ID                 */~
            lastpaydate$8,               /* LAST DATE THIS PAY PERIOD  */~
            location$2,                  /* LOCATOR ARRAY-STACK PUSHER */~
            moduleno$20,                 /* MODULE NUMBER              */~
            name$(3)15,                  /* EMPLOYEE'S NAME            */~
            name1$29,                    /* EMPLOYEE NAME FOR GLDETAIL */~
            newcode$2,                   /* VACATION METHOD TO CHANGETO*/~
            payfreq$1,                   /* HOW OFTEN TO CUT HIS CHECKS*/~
            payfreq$(2)1,                /* PAY FREQUENCY RANGE        */~
            period$(10)10,               /* PAY PERIODS$               */~
            prldate$8,                   /* PAYROLL DATE INFORMATION   */~
            print$(3,8)10,               /* VARIABLES FOR TWO PRINT LNS*/~
            printline$(3)132,            /* VARIABLES FOR TWO PRINT LNS*/~
            rcpacct$(2)16,               /* DEBIT, CREDIT ACCS--RCP#1  */~
            rcpacctdescr$(2)30,          /* DESCRIPTIONS OF ACCOUNTS   */~
            rcpamt(2),                   /* AMOUNTS OF RECAP STUFF.    */~
            rcpline%(2),                 /* RECAP LINE NUMBERS         */~
            rcpptr%(2),                  /* POINTER INTO STACKS        */~
            readkey$50,                  /* KEY USED IN PLOW ROUTINES  */~
            runtime$8,                   /* Run Time for Vac & Sick Rpt*/~
            seqnr$3,                     /* SEQUENCE NUMBER INFORMATION*/~
            sick(5),                     /* EMPLY'S SICK ACCRUAL RECORD*/~
            sickcode$2,                  /* SICK ACCRUAL METHOD        */~
            sfill$147,                   /* FILLER                     */~
            sickflag$2,                  /* SICK ACCRUED THIS ERN REC? */~
            summary$1,                   /* SUMMARY INDICATOR          */~
            t(4),                        /* SOME TOTALS                */~
            tempprint$30,                /*                            */~
            text$100,                    /* G/L TEXT INFORMATION       */~
            title$60,                    /* JOURNAL TITLE              */~
            txbl$1,                      /* IS EARNING TAXABLE? FLAG   */~
            type$12,                     /* EARNINGS TYPE DESCRIPTION  */~
            units$6,                     /* EARNINGS UNITS DESCRIPTION */~
            userid$3,                    /* USERID OF CURRENT USER.    */~
            vaca(5),                     /* EMPS VACATION ACCRUAL RECRD*/~
            vacacode$2,                  /* VACATION ACCRUAL METHOD    */~
            vfill$147,                   /* FILLER                     */~
            vacaflag$2,                  /* VACATN ACCRUED THIS ERNREC?*/~
            vsrptid$6,                   /* Vacation & Sick Report ID  */~
            vs(2,8),                     /* Vacation & Sick Totals     */~
            vsdate$8,                    /* Date for Vac & Sick Report */~
            yeardate$8                   /* TODAY'S DATE BUT LAST YEAR */

        dim                              /* G/L Export Posting info    */~
            export_on$1,                 /* G/L Export File processing?*/~
            gl_post_info$(2)255,         /* G/L Export Posting Info    */~
            gl_check$8,                  /* G/L P/R Check number       */~
            gl_deduction$12,             /* G/L P/R Deduction Descr.   */~
            gl_depart$4,                 /* G/L P/R Employee Depart #  */~
            gl_employee$12,              /* G/L P/R Employee Number    */~
            tran_type$5                  /* G/L transaction type       */

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20                  /* RETURN CODE FROM "OPENCHCK"*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLE  F2%()            SHOULD NOT BE   */
                     /* MODIFIED.     IT  IS AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! SYSFILE2 ! PAYROLL DATE (TO POST TO G/L)            *~
            * # 2 ! GLMAIN   ! GENERAL LEDGER MAIN ACCOUNT FILE.        *~
            * # 3 ! EMPMASTR ! EMPLOYEE FILE MASTER RECORDS.            *~
            * # 4 ! EMPEARN1 ! EMPLOYEE EARNINGS DETAILS FILE           *~
            * # 5 ! EMPDEDXN ! EMPLOYEE DEDUCTIONS RECORDS FILE         *~
            * # 6 ! USERINFO ! SYSTEM USER INFO FILE (PAYROLL DATES)    *~
            * # 7 ! PRLCHK   ! SYSTEM EARNINGS REGISTER INFORMATION     *~
            * # 8 ! GLDETAIL ! GENERAL LEDGER TRANSACTION DETAIL FILE   *~
            * # 9 ! PRLACRLS ! PAYROLL SICK & VACATION ACCRUAL METHODS  *~
            * #10 ! EMPACRLS ! employee sick & vacation accrual ledger  *~
            * #11 ! PERMASTR ! Personnel master file-ties to EMPMASTR i *~
            * #12 ! PRLCHK2  ! SYSTEM EARNINGS REGISTER LINES           *~
            * #13 ! PRLCHTIF ! INTERIM FILE CONTAINING CHECK NUMBERS    *~
            *************************************************************

            select #1,  "SYSFILE2",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 500,                                  ~
                         keypos = 1, keylen = 20

            select # 2, "GLMAIN",                                        ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 300,                                  ~
                         keypos = 1, keylen = 9

            select # 3, "EMPMASTR",                                      ~
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
                         alt key  1, keypos =  16, keylen = 28

            select # 5, "EMPDEDXN",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 300,                                  ~
                         keypos = 1, keylen = 15,                        ~
                         alternate key  1, keypos =  16, keylen = 18, dup

            select # 6, "USERINFO",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos = 1, keylen = 3

            select # 7, "PRLCHK",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 120,                                   ~
                        keypos =   1, keylen = 23,                       ~
                        alt key  1, keypos =  13, keylen =  11,          ~
                            key  2, keypos =  42, keylen =   9, dup

            select # 8, "GLDETAIL",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 160,                                  ~
                         keypos = 1, keylen = 26

            select #9, "PRLACRLS",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos = 1, keylen = 2

            select #10, "EMPACRLS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 200,                                   ~
                        keypos = 1,    keylen = 13

            select #11, "PERMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  950,                                  ~
                        keypos =   39, keylen =  12,                     ~
                        alt key  1, keypos =   28, keylen =  23,         ~
                            key  2, keypos =    2, keylen =  49,         ~
                            key  3, keypos =    1, keylen =  50          ~

            select #12, "PRLCHK2",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 120,                                   ~
                        keypos =   1, keylen = 25

            select #13, "PRLCHTIF",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 50,                                    ~
                        keypos =   1, keylen = 12


*        Check to See if Payroll/Personnel is Active
            call "PRLEXTSB" ("PRL", prl%)
                if prl% = 99% then end (prl%)

            call "SHOSTAT" ("Preparing To Post Net Pay And Deductions To ~
        ~The General Ledger")

            call "OPENCHCK" (# 1, 0%, f2%( 1),   0%, rslt$( 1))
            call "OPENCHCK" (# 2, 0%, f2%( 2),   0%, rslt$( 2))
            call "OPENCHCK" (# 3, 0%, f2%( 3),   0%, rslt$( 3))
            call "OPENCHCK" (# 4, 0%, f2%( 4),   0%, rslt$( 4))
            call "OPENCHCK" (# 5, 0%, f2%( 5),   0%, rslt$( 5))
            call "OPENCHCK" (# 6, 0%, f2%( 6),   0%, rslt$( 6))
            call "OPENCHCK" (# 7, 0%, f2%( 7), 100%, rslt$( 7))
            call "OPENCHCK" (# 8, 0%, f2%( 8),   0%, rslt$( 8))
            call "OPENCHCK" (# 9, 0%, f2%( 9),   0%, rslt$( 9))
            call "OPENCHCK" (#10, 0%, f2%(10), 100%, rslt$(10))
            call "OPENCHCK" (#11, 0%, f2%(11),   0%, rslt$(11))
            call "OPENCHCK" (#12, 0%, f2%(12), 100%, rslt$(12))
            call "OPENCHCK" (#13, 0%, f2%(13),   0%, rslt$(13))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR GENERAL LEDGER      *~
            * PROCESSING AND BATCH RECAP PRINTING.                      *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            REM GET PAYROLL DATE FOR GENERAL LEDGER PROCESSING.
                call "EXTRACT" addr("ID", userid$)
                call "READ100" (#6, userid$, f1%(6))
                     if f1%(6) = 0 then L09162
                get #6, using L09120, prldate$
L09120:                 FMT XX(3), XX(12), CH(6)
                call "WHICHPER" (#1, prldate$, whichper%)
                     if whichper% <> 0 then L09180

L09162:     ask% = 2%
            call "ASKUSER" (ask%, "*** INVALID POSTING DATE ***", "Your P~
        ~osting Date Is INVALID", "Press RETURN To Exit This Program and C~
        ~orrect This Error", " ")
            if ask% <> 0% then L09162  :  goto L65000

L09180:     REM GET PAY PERIODS RECORD
                call "READ100" (#1, "PAY PERIODS", f1%(1))
                     if f1%(1) = 0 then L09370
                get #1, using L09350, str(period$(), 1)
L09350:                 FMT XX(20), CH(91)

*        See if G/L Export is on
            export_on$ = "N"
            call "READ100" (#1, "SWITCHS.GL", f1%(1))
            if f1%(1) = 1% then get #1 using L09362, export_on$
L09362:         FMT POS(22), CH(1)

L09370:     REM INITIALIZE STACKS
                gosub L60000

            REM INITIALIZE FLAGS FOR RECONCILIATION AND RECAP
                line% = 10000
                vspage% = 0%
                vsrptid$ = "PRL014"

            REM GET RANGE TO PROCESS
                gosub L39500

            call "DATEFMT" (date, 0%, yeardate$)
            convert str(yeardate$,1%,4%) to temp%
            convert temp% - 1% to str(yeardate$,1%,4%), pic(0000)
            call "DATECONV" ( yeardate$ )

L09510:      REM DO A GETPARM TO FIND JNLID$
                call "GETPARM" addr ("I ", "R", "JNLID   ",  " ", "0001",~
                                     "PRLTOG",                           ~
                                    "INPUT THE JOURNAL ID TO POST THRU ",~
                                      34%, "K", "JNLID   ", jnlid$, 3%,  ~
                                      5%, 32%, "A")
                if jnlid$ = " " then L09510

                returncode% = 0
                moduleno$ = "09"
                call "JNLINFO" (moduleno$, jnlid$, pstseq%, summary$,    ~
                                title$, prldate$, #1, f2%(1), returncode%)
                call "FMTTITLE" (title$, "RECAP",12%)

            ret% = 0%
            call "COMPNAME" (12%, company$, ret%)

            call "SHOSTAT" ("Posting Deductions to General Ledger")

L10000: REM *************************************************************~
            *    P L O W   T H R O U G H   E M P L O Y E E   F I L E    *~
            *                                                           *~
            * GETS THE NEXT EMPLOYEE AND CALLS THE ROUTINES THAT POST   *~
            * HIM TO THE GENERAL LEDGER AND ALSO THE ROUTINES THAT POST *~
            * HIM TO THE EARNINGS REGISTER.                             *~
            *     >>> NOTE <<< NOTE THAT WE ZAP THE FLAG IN THE EMPLOYEE*~
            * MASTER RECORD THAT TELLS US TO PROCESS HIM FIRST OFF SO   *~
            * THAT ANY OTHER USER PROCESSING PAYROLL WILL SEE THE FLAG  *~
            * UNSET FOR THAT EMPLOYEE AND THUS WILL NOT TRY TO PROCESS  *~
            * HIM.  THIS IS NICE BECAUSE IF THE PROGRAM BOMBS ON AN     *~
            * EMPLOYEE, WE WILL NOT HAVE A LOT OF DOUBLE POSTINGS TO DO.*~
            *************************************************************

            call "ALLFREE"

            call "PLOWNEXT" (#13, empcode$, 0%, f1%(13))
                 if f1%(13) = 0 then L10440          /* DONE.            */
            get #13, using L10170, checknr$, slipnr$
L10170:     FMT XX(12), CH(8), CH(8)
            call "READ100" (#3, empcode$, f1%(3))
                if f1%(3) = 0 then L10440 /* Better Not Happen */
            gosub L30000                  /* LOAD EMPLOYEE RECORD.      */

            REM INITIALIZE THE STACKS TO POST TO EARNINGS REGISTER.
                earnstak$(), dedxnstak$(), dedxnstak1$(), dedxnstak2$(), ~
                earnstakdb$(), dedxnstakdb$(), dedxnstakcr$(), bacct$(), ~
                bcode$(), bsure$() = " "
                direct$ = "N"
                earnptr%, dedxnptr% = 0
                mat earnstak = zer
                mat dedxnstak = zer
                name1$ = "!!! NOT IN FILE !!!"
                call "READ100" (#11, empcode$, f1%(11))
                     if f1%(11) = 0 then L10380
                get #11, using L10340, name$(1), name$(2), name$(3)
L10340:         FMT XX(1), CH(15), CH(10), CH(1)
                name1$ = name$(1) &", "& name$(2)
                if name$(3)<>" " then name1$ = name1$ &" "& name$(3) &"."

L10380:     REM ACTUALLY POST ALL THAT INFORMATION, CALLING NEEDED ROUTINE
                gosub L11000              /* POST TO G/L                */
                gosub L14000              /* POST TO EARNINGS REGISTER. */
                gosub L33000              /* S & V, RESET DEDUCT FLAG   */
                goto L10000

L10440:     REM PRINT OUT REMAINDER OF DAILY RECAP
                if vspage% = 0% then L10720
                  gosub vacation_sick_totals
                  print using L10590, t(1), t(2), t(3), t(4)

                  if vestrolled% = 0 then L10510
                      print
                      print using L10620
L10510:           if asterisk% = 0 then L10550
                      print
                      print using L10650

L10550:           if excess% = 0 then L10720
                      print
                      print using L10680

L10590:  %Total Estimated Liability  -$#######.##                        ~
        ~-$#######.##    -$#######.##                        -$#######.##

L10620: %*By Date Hired indicates employee's accrued vacation has switche~
        ~d from non-vested to vested.

L10650: %                                                                ~
        ~This employee's vacation accrual method changed this pay period  ~
        ~**
L10680: %                                                                ~
        ~    This employee's accruals are in excess of the defined limit  ~
        ~><

L10720:         gosub L40000
                goto L65000

L11000: REM *************************************************************~
            * R O U T I N E   T O   P O S T   G R O S S   P A Y R O L L *~
            *                                                           *~
            * THIS ROUTINE ACCOMPLISHES 3 THINGS.                       *~
            *                                                           *~
            * 1.) LOADS THE EARNINGS RECORDS AND POSTS THEM TO THE      *~
            *     STACK FOR USE BY THE EARNINGS REGISTER POSTING ROUTINE*~
            *     ALSO TOTALS THEM FOR GROSS PAY PURPOSES.     NOTE THAT*~
            *     THESE GUYS DON'T GET POSTED TO G/L SINCE PRLERNGL DID *~
            *     THAT ON EARNINGS INPUT.                               *~
            * 2.) LOADS ALL DEDUCTIONS RECORDS. POSTS THEM TO THE G/L.  *~
            *     IF EMPLOYEE PAYS THE DEDUCTION, PUSH ONTO EARNINGS    *~
            *     REGISTER RECAP STACKS AND ADD TO THE DEDUCTIONS       *~
            *     AMOUNT USED FOR GROSS PAY.  ALSO UPDATE MTD, QTD, YTD.*~
            * 3.) TAKE THE NET PAY AMOUNT COMPUTED AND CREDIT CASH IN   *~
            *     BANK, FOLLOWED BY A DEBIT TO ACCRUED GROSS PAYROLL.   *~
            *************************************************************

            readkey$ = empcode$

            REM LOAD EMPLOYEE EARNINGS RECORDS AND POST TO STACK
                grosspay, totsadded, totstaken, totvadded, totvtaken = 0
                accrues%, accruev% = 0
                readkey$ = empcode$

L11250:         call "PLOWNEXT" (#4, readkey$, 12%, f1%(4))
                     if f1%(4) = 0 then L11650
                gosub L31000              /* LOAD EARNINGS RECORD.      */

*        Does this add to, take from, or not effect sick accruals?
                if abs(sickflag%) <>  1 then L11360
                totsadded = round(totsadded + earn(1,1), 4)
                if sickflag% <> -1 then L11360
                totstaken = round(totstaken + earn(1,1), 4)

*        Does this add to, take from, or not effect vacation accruals?
L11360:         if abs(vacaflag%) <>  1 then L11410
                totvadded = round(totvadded + earn(1,1), 4)
                if vacaflag% <> -1 then L11410
                totvtaken = round(totvtaken + earn(1,1), 4)

L11410:         if earn(1,1) = 0 and earn(1,2) = 0 then L11250
                if cash$ <> "Y" then L11540   /* non-cash benefits */
                   grosspay = round(grosspay + earn(1,2), 2)
                   REM PUSH NEW ITEM ONTO STACK.
                      earnptr% = earnptr% + 1
                      earnstak$(earnptr%) = type$
                      str(earnstak$(earnptr%),13) = units$
                      str(earnstakdb$(earnptr%)) = acct$
                      earnstak1$(earnptr%) = ecat$
                      earnstak(earnptr%,1) = earn(1,1)
                      earnstak(earnptr%,2) = rate
                      earnstak(earnptr%,3) = earn(1,2)

L11540:         earn(2,1) = earn(2,1) + earn(1,1)
                earn(2,2) = earn(2,2) + earn(1,2)
                earn(3,1) = earn(3,1) + earn(1,1)
                earn(3,2) = earn(3,2) + earn(1,2)
                earn(4,1) = earn(4,1) + earn(1,1)
                earn(4,2) = earn(4,2) + earn(1,2)
                earn(1,1), earn(1,2) = 0 /* Clear 'current' buckets */
                call "READ101" (#4, readkey$, f1%(4))
                gosub L36000              /* Rewrite earnings record  */
                goto L11250

L11650:     REM LOAD EMPLOYEE DEDUCTIONS RECORDS AND POST TO STACK.
                deductions = 0
                readkey$ = empcode$

L11700:         call "PLOWNEXT" (#5, readkey$, 12%, f1%(5))
                     if f1%(5) = 0 then L12310      /* IF DONE          */
                gosub L32000              /* LOAD DEDUCTIONS RECORD     */
                if dedamt(1) <= 0 then L12150
                   gosub'162(deddbacct$, dedamt(1))
                   gosub'163(dedcracct$, dedamt(1))
                   if slipnr$ <> " " then temp$ = slipnr$                ~
                     else temp$ = checknr$
                   text$ = str(deduction$) & " (" & dedmethod$
                   text$ = text$ & ")"
                   str(text$,31) = str(empcode$) & str(temp$)
                   str(text$,69) = "PRL DED: " & name1$
                   if dedmethod$ <> "DIRECT" then L11860
                      REM CR account will be cash, DB is gross payroll...
*                    STR(TEXT$,1,30)= STR(EMPCODE$) & STR(DEDRECORD$,,17)
                      str(text$,1,30)= "DIRECT DEPOSIT "                 ~
                                       & str(dedrecord$,,17)
                      str(text$,65) = "    EMP: " & name1$
                      direct$ = "Y"

L11860:          if summary$ = "Y" then L11905

                     if export_on$   <> "Y" then L11870
                        tran_type$    = "EAC04"
                        gl_check$     = temp$
                        gl_deduction$ = deduction$
                        gl_depart$    = department$
                        gl_employee$  = empcode$
                        gl_postamt    = -dedamt(1)
                        gosub load_gl_info
L11870:            call "GLPOST2" (dedcracct$, 0, dedamt(1), prldate$,   ~
                                  0%, "09", text$, jnlid$,               ~
                                  pstseq%, userid$, #2, #8, #1,          ~
                                  err%, " ", gl_post_info$())
L11905
*                    STR(TEXT$,1,30)= "DIRECT DEPOSIT "                 ~
*                                     & STR(DEDRECORD$,,17)
                   if summary$ = "Y" then L11970

                       if export_on$ <> "Y" then L11920
                          tran_type$ = "EAC03"
                          if dedempflag$ = "Y" then                      ~
                             tran_type$ = "EAC01"
                          gl_postamt = dedamt(1)
                          gosub load_gl_info
L11920:            call "GLPOST2" (deddbacct$, dedamt(1), 0, prldate$,   ~
                                  0%, "09", text$, jnlid$,               ~
                                  pstseq%, userid$, #2, #8, #1,          ~
                                  err%, " ", gl_post_info$())

L11970:            REM PUSH ON EARN. REG STACK AND TOTAL IF EMPLOYEE PAYS.
                       dedforstak$ = str(deduction$) & str(dedmethod$)
                       dedxnptr% = dedxnptr% + 1
                       dedxnstak$(dedxnptr%) = dedforstak$
                       dedxnstakdb$(dedxnptr%) = deddbacct$
                       dedxnstakcr$(dedxnptr%) = dedcracct$
                       dedxnstak2$(dedxnptr%) = dedempflag$
                       dedxnstak1$(dedxnptr%) = dedcat$
                       dedxnstak(dedxnptr%,1) = dedunt(1)
                       dedxnstak(dedxnptr%,2) = deddol(1)
                       dedxnstak(dedxnptr%,3) = dedamt(1)
                       bacct$(dedxnptr%) = bacct$
                       bcode$(dedxnptr%) = bcode$
                       bsure$(dedxnptr%) = bsure$
                       if dedempflag$ <> "Y" then L12150
                          deductions = round(deductions + dedamt(1), 2)

*        Now update the accrual amounts on the deduction record.
L12150:         dedamt(2) = round(dedamt(2) + dedamt(1), 2)  /* amount */
                dedamt(3) = round(dedamt(3) + dedamt(1), 2) /* deducted */
                dedamt(4) = round(dedamt(4) + dedamt(1), 2)

                dedunt(2) = round(dedunt(2) + dedunt(1), 2)  /* units */
                dedunt(3) = round(dedunt(3) + dedunt(1), 2) /* subject */
                dedunt(4) = round(dedunt(4) + dedunt(1), 2)

                deddol(2) = round(deddol(2) + deddol(1), 2) /* dollars */
                deddol(3) = round(deddol(3) + deddol(1), 2) /* subject */
                deddol(4) = round(deddol(4) + deddol(1), 2)
                dedamt(1), dedunt(1), deddol(1) = 0   /* Clear Current */
                call "READ101" (#5, readkey$, f1%(5))
                   gosub L37000              /* Resave deduction record */
                   goto L11700

L12310:     REM NOW CREDIT CASH IN BANK AND ACCRUED GROSS WITH NET PAY
                if slipnr$ <> " " and checknr$ = " "  then               ~
                        temp$ = slipnr$                                  ~
                   else temp$ = checknr$
                text$ = str(empcode$) & temp$
                str(text$,31) = str(empcode$) & temp$
                str(text$,69) = "EMP: " & name1$
                netpay = round(grosspay - deductions, 2)
                 if summary$ = "Y" then L12410

                     if export_on$   <> "Y" then L12370
                        tran_type$    = "EAC02"
                        gl_check$     = temp$
                        gl_deduction$ = " "
                        gl_depart$    = department$
                        gl_employee$  = empcode$
                        gl_postamt    = -netpay
                        gosub load_gl_info
L12370:         call "GLPOST2" (cashacct$, 0, netpay, prldate$,          ~
                               0%, "09", text$, jnlid$,                  ~
                               pstseq%, userid$, #2, #8, #1,             ~
                               err%, " ", gl_post_info$())
L12410:         gosub'163(cashacct$, netpay)
                if summary$ = "Y" then L12470

                     if export_on$   <> "Y" then L12430
                        tran_type$    = "EAC01"
                        gl_postamt    = netpay
                        gosub load_gl_info
L12430:         call "GLPOST2" (grossacct$, netpay, 0, prldate$,         ~
                               0%, "09", text$, jnlid$,                  ~
                               pstseq%, userid$, #2, #8, #1,             ~
                               err%, " ", gl_post_info$())
L12470:         gosub'162(grossacct$, netpay)
                return

L14000: REM *************************************************************~
            *        P O S T   E A R N I N G S   R E G I S T E R        *~
            *                                                           *~
            * POSTS THE AMOUNTS IN THE STACKS TO THE EARNINGS REGISTER. *~
            * THE EARNINGS REGISTER RECORD IS WRITTEN ANEW EACH TIME,   *~
            * EVEN IF THE CHECK NUMBER ALREADY EXISTS.  THIS IS SO THAT *~
            * WE CAN HAVE A HISTORY OF MODIFICATION OF ANY CHECKS.      *~
            *************************************************************

            REM DETERMINE NEXT SEQUENCE NUMBER FOR THIS CHECK.
                readkey$ = empcode$
                str(readkey$, 13) = checknr$
                temp$=checknr$
                seqnr% = 1000
                call "PLOWALTS" (#7, temp$, 1%, 8%, f1%(7))
                     if f1%(7) = 0 then L14180
                get #7, using L14170, seqnr%
L14170:         FMT XX(20), PIC(###)
L14180:         convert seqnr% - 1 to seqnr$, pic(###)

            REM GET DATES
                convert payfreq$ to temp%
                firstpaydate$ = str(period$(), 2 + (temp%-1)*13, 6)
                lastpaydate$  = str(period$(), 8 + (temp%-1)*13, 6)

            REM NOW WRITE THE EARNINGS RECORD.
                netpay = round(grosspay - deductions, 2)
                gosub L38000              /* ACTUALLY DO THE WRITE.     */
                return

        load_gl_info

            put str(gl_post_info$(),,) using L15490,                      ~
                tran_type$,              /* Transaction Type CH(5)     */~
                " ",                     /* Currency code CH(4)        */~
                0,                       /* Currency Units per Book    */~
                gl_postamt,              /* Functional Currency amount */~
                0,                       /* Unit amount                */~
                " ",                     /* Customer code CH(9)        */~
                " ",                     /* Sales Order number CH(16)  */~
                " ",                     /* BOL number CH(3)           */~
                " ",                     /* Customer Type CH(2)        */~
                " ",                     /* State CH(2)                */~
                " ",                     /* Country CH(3)              */~
                " ",                     /* ZIP CH(9)                  */~
                " ",                     /* Sales Region CH(4)         */~
                " ",                     /* Sales Tax code CH(10)      */~
                " ",                     /* Shipping Region CH(4)      */~
                " ",                     /* Salesman code CH(4)        */~
                " ",                     /* Invoice Number CH(8)       */~
                " ",                     /* Part Number CH(25)         */~
                " ",                     /* Part Category CH(4)        */~
                " ",                     /* Part Class CH(4)           */~
                " ",                     /* Part Generic code CH(16)   */~
                " ",                     /* Part Type CH(3)            */~
                " ",                     /* Part UOM CH(4)             */~
                " ",                     /* Store Number CH(3)         */~
                " ",                     /* Check Receipt Number CH(8) */~
                " ",                     /* Vendor code CH(9)          */~
                " ",                     /* Vendor type CH(4)          */~
                " ",                     /* Purchase Order CH(16)      */~
                " ",                     /* Receiver Number CH(16)     */~
                " ",                     /* Vendor Invoice CH(16)      */~
                gl_check$,               /* Check Payment Number CH(8) */~
                " ",                     /* Project code CH(8)         */~
                " ",                     /* Job number CH(8)           */~
                " ",                     /* Work Center CH(4)          */~
                " ",                     /* Activity code CH(4)        */~
                gl_employee$,            /* Employee number CH(12)     */~
                gl_depart$,              /* Department code CH(4)      */~
                " ",                     /* Cost Center CH(4)          */~
                " ",                     /* Earnings Type CH(12)       */~
                gl_deduction$,           /* Deduction Type CH(12)      */~
                " ",                     /* P/R Category CH(4)         */~
                " ",                     /* Labor class CH(4)          */~
                " "                      /* Filler                     */

            return

L15490: FMT     CH(5),                   /* Transaction Type CH(5)     */~
                CH(4),                   /* Currency code CH(4)        */~
                PD(15,7),                /* Transaction Currency amount*/~
                PD(15,4),                /* Functional Currency amount */~
                PD(15,4),                /* Unit amount                */~
                CH(9),                   /* Customer code CH(9)        */~
                CH(16),                  /* Sales Order number CH(16)  */~
                CH(3),                   /* BOL number CH(3)           */~
                CH(2),                   /* Customer Type CH(2)        */~
                CH(2),                   /* State CH(2)                */~
                CH(3),                   /* Country CH(3)              */~
                CH(9),                   /* ZIP CH(9)                  */~
                CH(4),                   /* Sales Region CH(4)         */~
                CH(10),                  /* Sales Tax code CH(10)      */~
                CH(4),                   /* Shipping Region CH(4)      */~
                CH(4),                   /* Salesman code CH(4)        */~
                CH(8),                   /* Invoice Number CH(8)       */~
                CH(25),                  /* Part Number CH(25)         */~
                CH(4),                   /* Part Category CH(4)        */~
                CH(4),                   /* Part Class CH(4)           */~
                CH(16),                  /* Part Generic code CH(16)   */~
                CH(3),                   /* Part Type CH(3)            */~
                CH(4),                   /* Part UOM CH(4)             */~
                CH(3),                   /* Store Number CH(3)         */~
                CH(8),                   /* Check Receipt Number CH(8) */~
                CH(9),                   /* Vendor code CH(9)          */~
                CH(4),                   /* Vendor type CH(4)          */~
                CH(16),                  /* Purchase Order CH(16)      */~
                CH(16),                  /* Receiver Number CH(16)     */~
                CH(16),                  /* Vendor Invoice CH(16)      */~
                CH(8),                   /* Check Payment Number CH(8) */~
                CH(8),                   /* Project code CH(8)         */~
                CH(8),                   /* Job number CH(8)           */~
                CH(4),                   /* Work Center CH(4)          */~
                CH(4),                   /* Activity code CH(4)        */~
                CH(12),                  /* Employee number CH(12)     */~
                CH(4),                   /* Department code CH(4)      */~
                CH(4),                   /* Cost Center CH(4)          */~
                CH(12),                  /* Earnings Type CH(12)       */~
                CH(12),                  /* Deduction Type CH(12)      */~
                CH(4),                   /* P/R Category CH(4)         */~
                CH(4),                   /* Labor class CH(4)          */~
                CH(191)                  /* Filler                     */

        vacation_sick_totals
            init (" ")  print$()
            for i% = 1% to 2%
                for j% = 1% to 8%
                if vs(i%, j%) = 0 then L16060
                     call "CONVERT" (vs(i%, j%), 2.2, print$(i%, j%))
L16060:         next j%
            next i%

            tempprint$ = "Unit Totals"
            put printline$(1%), using L35700, tempprint$, print$(1%,1%),  ~
                                print$(1%,2%), print$(1%,3%),            ~
                                print$(1%,4%), print$(1%,5%),            ~
                                print$(1%,6%), print$(1%,7%),            ~
                                print$(1%,8%), " "

            tempprint$ = "Dollar Totals"
            put printline$(2%), using L35700, tempprint$, print$(2%,1%),  ~
                                print$(2%,2%), print$(2%,3%),            ~
                                print$(2%,4%), print$(2%,5%),            ~
                                print$(2%,6%), print$(2%,7%),            ~
                                print$(2%,8%), " "

            if line% > 55% then gosub accrual_form_control
            print printline$(1%)
            print printline$(2%)
            print using L35670
            line% = line% + 2%
            return

L30000: REM *************************************************************~
            *   L O A D   E M P L O Y E E   M A S T E R   R E C O R D   *~
            *                                                           *~
            * LOADS THE EMPLOYEE MASTER RECORD FROM THE EMPLOYEE MASTER *~
            * RECORD FILE.                                              *~
            *************************************************************

            get   # 3, using L30150, empcode$, payfreq$, cashacct$,       ~
                                   grossacct$, firstpaydate$,            ~
                                   lastpaydate$, deductflag$, vacacode$, ~
                                   sickcode$, department$, baserate,     ~
                                   newcode$, changedate$
            if baserate > 10000 then baserate = 0
            return

L30150:     FMT CH(12),                  /* EMPLOYEE CODE              */~
                XX(4),                   /* WORKSTATION CODE           */~
                XX(1),                   /* FED FILING STATUS          */~
                XX(1),                   /* STATE FILING STATUS        */~
                CH(1),                   /* PAY FREQUENCY              */~
                XX(1),                   /* MODE OF PAYMENT            */~
                XX(4),                   /* DIRECT DEPOSIT BANK CODE   */~
                XX(12),                  /* D.D. ACCOUNT NUMBER        */~
                CH(09),                  /* CASH IN BANK ACCOUNT       */~
                CH(09),                  /* GROSS PAYROLL ACCOUNT      */~
                XX(2),                   /* NORMAL HRS/DAY ASCII(##)   */~
                XX(1),                   /* AUTOPAYROLL FLAG           */~
                CH(6),                   /* FIRST DATE PAY PERIOD      */~
                CH(6),                   /* LAST DATE PAY PERIOD       */~
                CH(1),                   /* DEDUCT ON THIS PRLDDUCT RUN*/~
                XX(02),                  /* NORMAL HRS/WEEK ASCII(##)  */~
                XX(10),                  /* WORKMANS' COMP. CODE DEF   */~
                CH(2),                   /* VACATION ACCRUAL METHOD    */~
                CH(2),                   /* SICK ACCRUAL METHOD        */~
                XX(3),                   /* O/H NUMBER                 */~
                XX(2),                   /* PRIMARY STATE CODE         */~
                CH(4),                   /* DEPARTMENT                 */~
                XX(9),                                                   ~
                PD(14,4),                /*RATE USED TO EST ACRL LIBLTY*/~
                CH(2),                   /* NEXT VACATION ACCRUAL METHO*/~
                CH(6),                   /* VACATION CHANGE DATE       */~
                XX(16)                   /* FILLER FOR REST OF RECORD  */

L31000: REM *************************************************************~
            *           L O A D   E A R N I N G S   R E C O R D         *~
            *                                                           *~
            * LOADS THE EARNINGS RECORD FROM THE FILE SO THAT WE CAN    *~
            * POST TO THE EMPLOYEE FILE.                                *~
            *************************************************************

            get   # 4, using L31200, seqnr$,                              ~
                       type$, ecat$, cash$, txbl$, units$, rate,         ~
                       acct$, usunits, usbucks, addunits, addamount,     ~
                       earn(1,1), earn(1,2), earn(2,1), earn(2,2),       ~
                       earn(3,1), earn(3,2), earn(4,1), earn(4,2),       ~
                       sickflag$, vacaflag$, earnrecord$
            sickflag%, vacaflag% = 0
            convert sickflag$ to sickflag%, data goto L31150
L31150:     convert vacaflag$ to vacaflag%, data goto L31160
L31160:     if sickflag% = 1 then accrues% = 1
            if vacaflag% = 1 then accruev% = 1
            return

L31200:     FMT XX(12),                  /* EMPLOYEE CODE              */~
                CH(3),                   /* SEQUENCE NUMBER            */~
                XX(12),                  /* EMPLOYEE CODE (AGAIN)      */~
                CH(12),                  /* EARNINGS TYPE              */~
                CH(4),                   /* DEPARTMENT CODE            */~
                CH(1),                   /* PAID IN CASH? FLAG         */~
                CH(1),                   /* IS THIS TAXABLE?           */~
                CH(6),                   /* UNIT DESCRIPTION           */~
                PD(14,4),                /* UNIT RATE                  */~
                CH(9),                   /* EXPENSE ACCOUNT NUMBER     */~
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
                PD(14,4),                /* YEAR    TO DATE AMOUNT     */~
                CH(2),                   /* SICK ACCRUAL FLAG          */~
                CH(2),                   /* VACATION ACCRUAL FLAG      */~
                CH(32)                   /* EARNINGS RECORD TAG END    */~

L32000: REM *************************************************************~
            *         L O A D   D E D U C T I O N S   R E C O R D       *~
            *                                                           *~
            * LOADS THE DEDUCTIONS RECORD FROM THE DEDUCTIONS FILE FOR  *~
            * DEDUCTIONS PROCESSING.                                    *~
            *************************************************************

            get   #5,  using L32200, seqnr$,                              ~
                       dedcat$, dedmethod$, deduction$, dedempflag$,     ~
                       dedcracct$, deddbacct$, dedapplies$, amount(1),   ~
                       amount(2), amount(3), amount(4), amount(5),       ~
                       dedamt(1), dedamt(2), dedamt(3), dedamt(4),       ~
                       dedhold$,                                         ~
                       dedunt(1), deddol(1), dedunt(2), deddol(2),       ~
                       dedunt(3), deddol(3), dedunt(4), deddol(4),       ~
                       dedrecord$
            get dedrecord$, using L32170, bacct$, bcode$, bsure$
L32170:     FMT CH(20), CH(4), CH(2)
            return

L32200: FMT                      /* FILE: EMPDEDXN                     */~
            XX(12),              /* employee code                      */~
            CH(3),               /* empdedxn sequence number           */~
            XX(12),              /* repeat occurance of employee  code */~
            CH(6),               /* deduction category code            */~
            CH(6),               /* method of deduction                */~
            CH(12),              /* deduction descriptions             */~
            CH(1),               /* employer/employee pays flag        */~
            CH(9),               /* deductions credit account          */~
            CH(9),               /* deductions debit account           */~
            CH(6),               /* applies (123456) field             */~
            PD(14,4),            /* deduction amount 1                 */~
            PD(14,4),            /* deduction amount 2                 */~
            PD(14,4),            /* deduction amount 3                 */~
            PD(14,4),            /* deduction amount 4                 */~
            PD(14,4),            /* deduction goal                     */~
            PD(14,4),            /* current amount                     */~
            PD(14,4),            /* month to date amount               */~
            PD(14,4),            /* quarter to date amount             */~
            PD(14,4),            /* year to date amount                */~
            CH(8),               /* hold month-to-date amount          */~
            PD(14,4),            /* Current Units Subject To Deduction */~
            PD(14,4),            /* Current Dollars Subject To Deducti */~
            PD(14,4),            /* Month To Date Units Subject        */~
            PD(14,4),            /* Month To Date Dollars Subject      */~
            PD(14,4),            /* Quarter To Date Units Subject      */~
            PD(14,4),            /* Quarter To Date Dollars Subject    */~
            PD(14,4),            /* Year To Date Units subject         */~
            PD(14,4),            /* Year To Date Dollars Subject       */~
            CH(80)               /* filler for rest of record or inter */~

L33000: REM *************************************************************~
            *       R E W R I T E   E M P L O Y E E   M A S T E R       *~
            *          AND UPDATE THE VACATION & SICK ACCRUALS          *~
            *                                                           *~
            * REWRITE EMPLOYEE MASTER RECORD, RESETING THE DEDUCT FLAG. *~
            * THIS IS THE PLACE WHERE THE EMP'S SICK & VACATION ACCRUAL *~
            * LEDGERS ARE UPDATED.  A REPORT IS ALSO GENERATED, SHOWING *~
            * THE EMPLOYEES CURRENT STANDING IN TERMS OF UNITS ACCRUED  *~
            * AND UNITS USED.  NOTE THAT THE REPORT WILL GESTIMATE THE  *~
            * LIABILTY IN DOLLARS. THIS IS TO AVOID SOME VERY           *~
            * MESSY G/L ACCOUNT BALANCE MAINTENANCE LOGIC...            *~
            *************************************************************

            changeflag$ = " "
            call "READ101" (#3, empcode$, f1%(3))
                 if f1%(3) = 0 then return    /* ? */
            get #3, using L33290, str(emprecord$(), 1)
            str(emprecord$(), 70, 1) = "N"
            if newcode$ = " " or changedate$ = " " or changedate$ = blankdate$ ~
                                  then L33280
            if changedate$ > date then L33280
                changeflag$ = "**"                  /* This logic is    */
                vacacode$ = newcode$                /* intended to help */
                newcode$ = " "                      /* in keeping track */
                str(emprecord$(), 83,2) = vacacode$ /* of employee accrl*/
                str(emprecord$(), 113,2) = newcode$ /* rate changes.    */
                asterisk% = 1

L33280:     rewrite #3, using L33290, str(emprecord$(), 1)
L33290:     FMT CH(136)

*        Initialize variables...
            vtype$,stype$,vfill$,sfill$,vestdate$,vestflag$,hiredate$=" "
            mat vaca = zer : mat sick = zer
            vadd,sadd,vhours,shours,vmax,smax,vmin,smin,vest,nonvested = 0
            newvbal, newvval, newsbal, newsval = 0
            vestvval, saddval, totvtakenval, totstakenval = 0

*        Load up the accrual control records and employee detail records
            call "READ100" (#9, vacacode$, f1%(9)) /* Vacation first */
                if f1%(9) = 0 then L33470
            get  #9, using L33540, vhours, vtype$, vmax, vmin, vestflag$
            call "READ100" (#10, str(empcode$) & "V", f1%(10))
                if f1%(10) = 0 then L33470
            get #10, using L33650, vaca(), vfill$ /* VACAT LEDGER RECORD */
            get vfill$, using L33440, vestdate$
L33440:     FMT XX(37), CH(6)

*        Now sick...
L33470:     call "READ100" (#9, sickcode$, f1%(9))
                if f1%(9) = 0 then L33760
            get  #9, using L33540, shours, stype$, smax, smin
            call "READ100" (#10, str(empcode$) & "S", f1%(10))
                if f1%(10) = 0 then L33760
            get #10, using L33650, sick(), sfill$  /* SICK LEDGER RECORD */

L33540:     FMT XX(2),                   /* METHOD CODE                */~
                XX(30),                  /* INTERNAL DESCRIPTION       */~
                PD(14,6),                /* HOURS TO ACCRUE            */~
                CH(1),                   /* TYPE ('U' OR 'P')          */~
                PD(14,4),                /* MAXIMUM HOURS TO ACCRUE    */~
                PD(14,4),                /* MINIMUM HOURS TO CONSIDER  */~
                CH(1),                   /* N=VEST AS ACCRUED,Y=ANNUALY*/~
                XX(9),                   /* CREDIT ACCOUNT             */~
                XX(9),                   /* DEBIT ACCOUNT              */~
                XX(74)                   /* FILLER                     */

L33650: FMT                      /* FILE: EMPACRLS                     */~
            XX(13),              /* KEY                                */~
            5*PD(14,4),          /* NUMBERS                            */~
            CH(147)              /* MORE STUFF + FILLER                */

*        Figure out how many hours to add to the two ledgers (if any).
*        This is dependent on accrual method (& possibly # hours worked).
*        He must meet or exceed the minimum hours, if the minimum is used
*        ACCRUEV% & ACCRUES% Indicate wether or not the guy was payed
*        through any EARNTYPES that accrue vacation & sick respectively

L33760:     if vtype$ = " " and stype$ = " " then return  /*DON'T ACCRUE*/
            if totvadded < vmin or accruev% = 0 then L33830
            if vtype$ = "P" then vadd = vhours   /* X hours per period */
            if vtype$ = "U" then vadd = round(vhours * totvadded,2)
                                 /* U = accrue X hours per hour worked */

*        Now sick...
L33830:     if totsadded < smin or accrues% = 0 then L33880
            if stype$ = "P" then sadd = shours
            if stype$ = "U" then sadd = round(shours * totsadded,2)

*        Get employee's Hire Date from personnel file...
L33880:     call "READ100" (#11, empcode$, f1%(11))
                 if f1%(11) = 0 then L33970
            get #11, using L33910, hiredate$
L33910:     FMT XX(177), XX(212), CH(6)
            if vtype$ = " " then L34160

*        Calc next vesting date if needed. (happens when records created)
L33970:     if vestflag$ <> "Y" or ~
              (vestdate$ <> " " and vestdate$ <> blankdate$) then L34080
            tdate$ = hiredate$
            call "DATEFMT" (tdate$, 0%, vestdate$)
            call "DATEFMT" ( date, 0%, tdate$ )
            str(vestdate$,1%,4%) = str( tdate$, 1%, 4% )
            if vestdate$ >= tdate$ then L34080
            convert vestdate$ to v%, data goto L34080
            convert v% + 10000 to vestdate$, pic(########)

*        Calculate units to vest this period for vacation if any.
*        Vacation units to add are always added to non-vested, then
*        the non-vested are POSSIBLY added to the vested/available,
*        depending on the hire date/vest roll date and/or VESTFLAG$
L34080:     nonvested = round(vaca(5) + vadd, 4)
            if vestdate$ > date and vestflag$ = "Y" then L34160
                vest = nonvested
                nonvested = 0
            convert vestdate$ to v%, data goto L34160
            convert v% + 10000 to vestdate$, pic(########)
         call "DATECONV" (vestdate$)

*        We now know the amounts to add & take, so format the print lines
L34160:     print$() = " "
            if vtype$ = " " then L34310
            newvbal      = round(vaca(1) + vest - totvtaken, 4%)
            vestvval     = round(vest      * baserate, 2%)
            totvtakenval = round(totvtaken * baserate, 2%)
            newvval      = round(newvbal   * baserate, 2%)
            call "CONVERT" (vaca(1), 2.4, print$(1,1))
            if vest <> 0 then                                            ~
                call "CONVERT" (vest, 2.4, print$(1,2))
            if totvtaken <> 0 then                                       ~
                call "CONVERT" (totvtaken, 2.4, print$(1,3))
            call "CONVERT" (newvbal, 2.4, print$(1,4))
            convert vaca(2%)     to print$(2%,1%), pic(-$#####.##)
            if vestvval     <> 0 then convert vestvval     to            ~
                                    print$(2%,2%), pic(-$#####.##)
            if totvtakenval <> 0 then convert totvtakenval to            ~
                                    print$(2%,3%), pic(-$#####.##)
            convert newvval      to print$(2%,4%), pic(-$#####.##)
*        The array T() is the totals for the report
            t(1)=round(t(1) + vaca(2), 2) : t(2)=round(t(2) + newvval, 2)
            vs(1%,1%) = vs(1%,1%) + vaca(1%)
            vs(1%,2%) = vs(1%,2%) + vest
            vs(1%,3%) = vs(1%,3%) + totvtaken
            vs(1%,4%) = vs(1%,4%) + newvbal
            vs(2%,1%) = round(vs(2%,1%) + vaca(2%),     2%)
            vs(2%,2%) = round(vs(2%,2%) + vestvval,     2%)
            vs(2%,3%) = round(vs(2%,3%) + totvtakenval, 2%)
            vs(2%,4%) = round(vs(2%,4%) + newvval,      2%)

L34310:     if stype$ = " " then L34440
            newsbal      = round(sick(1%) + sadd - totstaken, 4%)
            saddval      = round(sadd      * baserate, 2%)
            totstakenval = round(totstaken * baserate, 2%)
            newsval      = round(newsbal   * baserate, 2%)
            call "CONVERT" (sick(1), 2.4, print$(1,5))
            if sadd <> 0 then                                            ~
                call "CONVERT" (sadd, 2.4, print$(1,6))
            if totstaken <> 0 then                                       ~
                call "CONVERT" (totstaken, 2.4, print$(1,7))
            call "CONVERT" (newsbal, 2.4, print$(1,8))
            convert sick(2%)     to print$(2%,5%), pic(-$#####.##)
            if saddval      <> 0 then convert saddval      to            ~
                                    print$(2%,6%), pic(-$#####.##)
            if totstakenval <> 0 then convert totstakenval to            ~
                                    print$(2%,7%), pic(-$#####.##)
            convert newsval      to print$(2%,8%), pic(-$#####.##)
            t(3)=round(t(3) + sick(2), 2) : t(4)=round(t(4) + newsval, 2)
            vs(1%,5%) = vs(1%,5%) + sick(1%)
            vs(1%,6%) = vs(1%,6%) + sadd
            vs(1%,7%) = vs(1%,7%) + totstaken
            vs(1%,8%) = vs(1%,8%) + newsbal
            vs(2%,5%) = round(vs(2%,5%) + sick(2%),     2%)
            vs(2%,6%) = round(vs(2%,6%) + saddval,      2%)
            vs(2%,7%) = round(vs(2%,7%) + totstakenval, 2%)
            vs(2%,8%) = round(vs(2%,8%) + newsval,      2%)

L34440:     printline$(3) = " "
            tempprint$ = empcode$
            call "DATEFMT" (hiredate$)
            str(tempprint$,18, 8) = hiredate$
            if abs(vest) < .0001 or vestflag$ <> "Y" then L34510
                vestrolled% = 1  : str(tempprint$,27,1) = "*"

L34510:     put printline$(1), using L35700, tempprint$, print$(1,1),     ~
                               print$(1,2), print$(1,3), print$(1,4),    ~
                               print$(1,5), print$(1,6), print$(1,7),    ~
                               print$(1,8), changeflag$

            changeflag$ = " "
            if abs(vmax) < .0001 then vmax = 9e9
            excess = max(0, newvbal-vmax )            /* ...Excess */
            if abs(excess) < .0001 then L34610
                excess% = 1
                changeflag$ = "><"

L34610:     put printline$(2), using L35700, name1$, print$(2,1),         ~
                               print$(2,2), print$(2,3), print$(2,4),    ~
                               print$(2,5), print$(2,6), print$(2,7),    ~
                               print$(2,8), changeflag$

            if vestflag$ <> "Y" then L34800
            if abs(vaca(5))   > .001 then L34700
            if abs(nonvested) > .001 then L34700
                goto L34810
L34700:     call "CONVERT" (vaca(5), 2.4, print$(3,1))
            if vadd <> 0 then                                            ~
                call "CONVERT" (vadd, 2.4, print$(3,2))
            if vest <> 0 then                                            ~
                call "CONVERT" (vest, 2.4, print$(3,3))
            call "CONVERT" (nonvested, 2.4, print$(3,4))

            put printline$(3), using L35740, print$(3,1), print$(3,2),    ~
                               print$(3,3), print$(3,4)

L34800:     if line% > 55% then gosub accrual_form_control  /* Heading */
L34810:     print printline$(1%)
            print printline$(2%)
            if printline$(3%) = " " then L34850
                print printline$(3%)  :  line% = line% + 1%
L34850:     print using L35670
            line% = line% + 3%

*        Update ledger records ... note that some of these fields are
*        either info only or for anticipated future uses...
            sick(1)=newsbal : vaca(1) = newvbal    /* Reset current hrs */
            sick(2)=newsval : vaca(2) = newvval    /* reset current $   */
            sick(3)=sick(3) + sadd : vaca(3)=vaca(3) + vadd    /* Add   */
            sick(4) = sick(4) + totstaken                    /* to the  */
            vaca(4) = vaca(4) + totvtaken                    /* history */
            vaca(5)=nonvested

*        Set modification dates and last added/taken. These are memoes
*          Sick first...
            get sfill$, using L35330, temp$, date$(1), temp, date$(2), temp
            if abs(sadd) > .0001 then date$(1) = date
            if abs(totstaken) > .0001 then date$(2) = date
            put sfill$, using L35330, temp$, date$(1), sadd, date$(2),    ~
                                                      totstaken, " ", " "
*          Then vacation...
            get vfill$, using L35330, temp$, date$(1), temp, date$(2), temp
            if abs(vadd) > .0001 then date$(1) = date
            if abs(totvtaken) > .0001 then date$(2) = date
            put vfill$, using L35330, temp$, date$(1), vadd, date$(2),    ~
                                              totvtaken, vestdate$, " "

*        And do the writes...
            call "DELETE" (#10, str(empcode$)&" ", 12%)
            x% = 0
            for i% = 1 to 5 : if abs(vaca(i%)) > .0001 then x%=1 : next i%
            if x% = 0 then L35160
            write #10, using L35230, empcode$, "V", vaca(), vfill$
L35160:     x% = 0
            for i% = 1 to 5 : if abs(sick(i%)) > .0001 then x%=1 : next i%
            if x% = 0 then L35200
            write #10, using L35230, empcode$, "S", sick(), sfill$
L35200: return

L35230: FMT                      /* FILE: EMPACRLS                     */~
            CH(12),              /* employee code                      */~
            CH(1),               /* 'S' FOR SICK, 'V' FOR VACATION     */~
            PD(14,4),            /* CURRENT BALANCE OF UNITS *AVAILABLE*/~
            PD(14,4),            /* ESTIMATED VALUE OF CURRENT UNITS   */~
            PD(14,4),            /* TOTAL UNITS ADDED TO DATE          */~
            PD(14,4),            /* TOTAL UNITS TAKEN TO DATE          */~
            PD(14,4),            /* NON VESTED UNITS BALANCE           */~
            CH(147)              /* FILLER    (37 bytes are desc below)*/

L35330: FMT                      /* FORMAT FOR ABOVE FILES FILLER      */~
            CH(9),               /* LAST DATE MODIFIED DIRECTLY        */~
                                 /* LAST USER TO DIRECTLY MODIFY RECRD */~
            CH(6),               /* LAST DATE SYSTEM ADDED UNITS       */~
            PD(14,4),            /* NUMBER OF UNITS LAST ADDED BY SYSTM*/~
            CH(6),               /* LAST DATE EMPLOYEE USED UNITS      */~
            PD(14,4),            /* NUMBER OF UNITS LAST USED BY EMPLYE*/~
            CH(6),               /* NEXT VESTING ROLL DATE             */~
            CH(104)              /* VESTDATE + FILLER                  */~

        accrual_form_control
            if vspage% > 0% then goto L35480
                call "SETPRNT" (vsrptid$, " ", 0%, 0%)
                select printer (134)
                runtime$ = " "
                call "TIME" (runtime$)
                vsdate$ = date
                call "DATEFMT" (vsdate$)
L35480:            print page
                   vspage% = vspage% + 1%
                   print using L35580, vsdate$, runtime$, company$,       ~
                                      " PRLTOGL",vsrptid$
                   print using L35600, vspage%
                   print
                   print using L35610
                   print using L35630
                   print using L35650
                   print using L35670
                   line% = 9 : return

L35580: %RUN ######## @ ########              ###########################~
        ~#################################                 ########:######
L35600: %                                  V A C A T I O N   A N D   S I ~
        ~C K   A C C R U A L S   S T A T U S                   Page: ####
L35610: %                                      * * * *  V A C A T I O N  ~
        ~* * * *                      * * * *  S I C K  * * * *
L35630: %Employee code                Beg. Units       Units       Units ~
        ~  End Units      Beg. Units       Units       Units   End Units
L35650: %Name           Date Hired      /Dollars     Accrued       Taken ~
        ~   /Dollars        /Dollars     Accrued       Taken    /Dollars
L35670: %----------------------------+-----------------------------------~
        ~---------------+------------------------------------------------+~
        ~--
L35700: %############################!##########  ##########  ########## ~
        ~  ##########    ! ##########  ##########  ##########  ########## ~
        ~ !##

L35740: %     Non-Vested Units Status:##########  ##########  ########## ~
        ~  ##########    !                                                ~
        ~!
L36000: REM *************************************************************~
            *       R E W R I T E   E A R N I N G S   R E C O R D S     *~
            *                                                           *~
            * REWRITES EARNINGS RECORDS FROM THE EARNINGS RECORD LOADED *~
            * IN THE ROUTINE ABOVE.  ACTUALLY, THIS ROUTINE IS FAIRLY   *~
            * SIMPLE SINCE IT SAVES A CONSTANT ZERO FOR THE CURRENT     *~
            * RECORD, AND TOTALS THE MTD, QTD, YTD ACCUMULATORS IN THE  *~
            * REWRITE STATEMENT.  THIS IS NICE SINCE WE CAN REALLY ZIP  *~
            * DOWN THE EMPLOYEE LIST WITHOUT WORRYING ABOUT EMPLOYEE    *~
            * FILE BOMBOUTS.                                            *~
            *************************************************************

            rewrite # 4, using L36210,                                    ~
                         empcode$, seqnr$, empcode$,                     ~
                         type$, ecat$, cash$, txbl$, units$, rate,       ~
                         acct$, usunits, usbucks, addunits, addamount,   ~
                         earn(1,1), earn(1,2), earn(2,1), earn(2,2),     ~
                         earn(3,1), earn(3,2), earn(4,1), earn(4,2),     ~
                         sickflag$, vacaflag$, earnrecord$
            return

L36210:     FMT CH(12),                  /* EMPLOYEE CODE              */~
                CH(3),                   /* SEQUENCE NUMBER            */~
                CH(12),                  /* EMPLOYEE CODE (AGAIN)      */~
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
                PD(14,4),                /* YEAR    TO DATE AMOUNT     */~
                CH(2),                   /* SICK ACCRUAL FLAG          */~
                CH(2),                   /* VACATION ACCRUAL FLAG      */~
                CH(32)                   /* EARNINGS RECORD TAG END    */~

L37000: REM *************************************************************~
            *     R E W R I T E   D E D U C T I O N S   R E C O R D     *~
            *                                                           *~
            * REWRITES THE DEDUCTIONS RECORD WITHOUT LOSING  ALL THE    *~
            * STACK INFORMATION.                                        *~
            *************************************************************

            if dedapplies$ = "ONCE" then dedapplies$ = " "
            rewrite #5,  using L37200,                                    ~
                         empcode$, seqnr$, empcode$,                     ~
                         dedcat$, dedmethod$, deduction$, dedempflag$,   ~
                         dedcracct$, deddbacct$, dedapplies$, amount(1), ~
                         amount(2), amount(3), amount(4), amount(5),     ~
                         dedamt(1), dedamt(2), dedamt(3), dedamt(4),     ~
                         dedhold$,                                       ~
                         dedunt(1), deddol(1), dedunt(2), deddol(2),     ~
                         dedunt(3), deddol(3), dedunt(4), deddol(4),     ~
                         str(dedrecord$,,80)
            return

L37200: FMT                      /* FILE: EMPDEDXN                     */~
            CH(12),              /* employee code                      */~
            CH(3),               /* empdedxn sequence number           */~
            CH(12),              /* repeat occurance of employee  code */~
            CH(6),               /* deduction category code            */~
            CH(6),               /* method of deduction                */~
            CH(12),              /* deduction descriptions             */~
            CH(1),               /* employer/employee pays flag        */~
            CH(9),               /* deductions credit account          */~
            CH(9),               /* deductions debit account           */~
            CH(6),               /* applies (123456) field             */~
            PD(14,4),            /* deduction amount 1                 */~
            PD(14,4),            /* deduction amount 2                 */~
            PD(14,4),            /* deduction amount 3                 */~
            PD(14,4),            /* deduction amount 4                 */~
            PD(14,4),            /* deduction goal                     */~
            PD(14,4),            /* current amount                     */~
            PD(14,4),            /* month to date amount               */~
            PD(14,4),            /* quarter to date amount             */~
            PD(14,4),            /* year to date amount                */~
            CH(8),               /* hold month-to-date amount          */~
            PD(14,4),            /* Current Units Subject To Deduction */~
            PD(14,4),            /* Current Dollars Subject To Deducti */~
            PD(14,4),            /* Month To Date Units Subject        */~
            PD(14,4),            /* Month To Date Dollars Subject      */~
            PD(14,4),            /* Quarter To Date Units Subject      */~
            PD(14,4),            /* Quarter To Date Dollars Subject    */~
            PD(14,4),            /* Year To Date Units subject         */~
            PD(14,4),            /* Year To Date Dollars Subject       */~
            CH(80)               /* filler for rest of record or inter */~

L38000: REM *************************************************************~
            * W R I T E   E A R N I N G   R E G I S T E R   R E C O R D *~
            *                                                           *~
            * Writes new check to file.                                 *~
            *************************************************************

            REM Write out check if required...
            if netpay > 0 then gosub L38500

            REM Write out dirct deposit receipt if required...
            if direct$ <> "Y" then L38760
            if netpay = 0 then L38350
                REM Add line to deductions to show check amount...
                dedxnptr% = dedxnptr% + 1
                if dedxnptr% < 2 then L38280
                for i% = dedxnptr% to 2% step -1%
                     dedxnstak$(i%)   = dedxnstak$(i%-1)
                     dedxnstak1$(i%)  = dedxnstak1$(i%-1)
                     dedxnstak2$(i%)  = dedxnstak2$(i%-1)
                     dedxnstakdb$(i%) = dedxnstakdb$(i%-1)
                     dedxnstakcr$(i%) = dedxnstakcr$(i%-1)
                     dedxnstak(i%,1)  = dedxnstak(i%-1,1)
                     dedxnstak(i%,2)  = dedxnstak(i%-1,2)
                     dedxnstak(i%,3)  = dedxnstak(i%-1,3)
                     bacct$(i%)       = bacct$(i%-1)
                     bcode$(i%)       = bcode$(i%-1)
                     bsure$(i%)       = bsure$(i%-1)
                next i%
L38280:         dedxnstak1$(1), dedxnstak2$(1), bacct$(1), bcode$(1),    ~
                dedxnstakdb$(1), dedxnstakcr$(1), bsure$(1) = " "
                dedxnstak(1,1), dedxnstak(2,1) = 0
                dedxnstak$(1) = "CHK#" & checknr$
                call "SPCESMSH" (dedxnstak$(1), 1%)
                dedxnstak(1,3) = netpay

L38350:     REM Calculate Total Of Deposit Slip...
            temp$ = checknr$
            checknr$ = slipnr$
            slipnr$ = temp$
            if netpay = 0 then slipnr$ = " "
            netpay = 0
            for temp% = 1 to dedxnptr%
                if dedxnstak2$(temp%) <> "Y" then L38450
                if str(dedxnstak$(temp%),13) <> "DIRECT" then L38450
                netpay = round(netpay + dedxnstak(temp%,3), 2)
L38450:     next temp%
            seqnr$ = "999"
            if netpay > 0 then gosub L38500
            goto L38760

L38500:     write # 7, using L38820,                                      ~
                       empcode$, checknr$, seqnr$, prldate$,             ~
                       firstpaydate$, lastpaydate$, cashacct$,           ~
                       grosspay, netpay, "N", " ", prldate$, department$,~
                       slipnr$, " "

            REM Write Check Details...
                for temp% = 1 to earnptr%
                     write #12, using L38980, empcode$, checknr$, seqnr$, ~
                                "P", temp%, " ", earnstak1$(temp%),      ~
                                earnstak$(temp%), earnstak(temp%,1),     ~
                                earnstak(temp%,2), earnstak(temp%,3),    ~
                                earnstakdb$(temp%), grossacct$, " ", " ",~
                                " ", " "
                     next temp%
                for temp% = 1 to dedxnptr%
                     write #12, using L38980, empcode$, checknr$, seqnr$, ~
                                "W", temp%, dedxnstak2$(temp%),          ~
                                dedxnstak1$(temp%), dedxnstak$(temp%),   ~
                                dedxnstak(temp%,1), dedxnstak(temp%,2),  ~
                                dedxnstak(temp%,3), dedxnstakdb$(temp%), ~
                                dedxnstakcr$(temp%), bacct$(temp%),      ~
                                bcode$(temp%), bsure$(temp%), " "
                next temp%
                return

L38760:     REM Clear Check Tiff, Move On...
            call "READ101" (#13, empcode$, f1%(13))
                 if f1%(13) = 0 then L38800 /* ?? */
            delete #13
L38800:     return

L38820:     FMT CH(12),                  /* EMPLOYEE CODE              */~
                CH(8),                   /* CHECK NUMBER               */~
                CH(3),                   /* REVERSE SEQUENCE NUMBER    */~
                CH(6),                   /* DATE OF CHECK              */~
                CH(6),                   /* FIRST DATE THIS PAY PERIOD */~
                CH(6),                   /* LAST DATE THIS PAY PERIOD  */~
                CH(9),                   /* CASH IN BANK ACCOUNT       */~
                PD(14,4),                /* GROSS PAY AMOUNT           */~
                PD(14,4),                /* NET PAY AMOUNT             */~
                CH(1),                   /* RECONCILIATION FLAG        */~
                CH(6),                   /* RECONCILIATION DATE        */~
                CH(6),                   /* POSTING DATE               */~
                CH(4),                   /* EMPLOYEE DEPARTMENT CODE   */~
                CH(8),                   /* Direct Deposit Slip Number */~
                CH(29)                   /* EARN. REGISTER FREE SPACE  */

L38980:     FMT CH(12),                  /* EMPLOYEE CODE              */~
                CH(8),                   /* CHECK NUMBER               */~
                CH(3),                   /* REVERSE SEQUENCE NUMBER    */~
                CH(1),                   /* Record Type (P=PAY, W=WHLD)*/~
                BI(1),                   /* SEQUENCE NUMBER            */~
                CH(1),                   /* Employee Paid? DED only    */~
                CH(6),                   /* CATEGORY                   */~
                CH(18),                  /* EARN/DEDUCTION TYPE        */~
                PD(14,4),                /* UNITS / UNITS SUBJECT      */~
                PD(14,4),                /* RATE / DOLLARS SUBJECT     */~
                PD(14,4),                /* AMOUNT                     */~
                CH(9),                   /* DEBIT ACCOUNT              */~
                CH(9),                   /* CREBIT ACCOUNT             */~
                CH(17),                  /* Direct deposit bank account*/~
                CH(4),                   /* Direct deposit bank id.    */~
                CH(2),                   /* bannk account type(Surepay)*/~
                CH(5)                    /* EARN. REGISTER FREE SPACE  */

L39500: REM *************************************************************~
            *  SET PROCESSING RANGES                                    *~
            *************************************************************

            init(hex(00)) dept$(1), empcode$(1), payfreq$(1)
            init(hex(ff)) dept$(2), empcode$(2), payfreq$(2)

            call "READ100" (#1, "PRL PROCESSING RANGE", f1%(1))
                     if f1%(1) = 0 then return
                     get #1, using L39840, str(dept$(), 1),               ~
                                          str(empcode$(), 1),            ~
                                          str(payfreq$(), 1)

            if dept$(1) <> "ALL" then L39670
               init(hex(00)) dept$(1)
               init(hex(ff)) dept$(2)
               go to L39710
L39670:     if dept$(2) <> " " then L39690
               dept$(2) = dept$(1)
L39690:        dept$(1) = dept$(1) addc all(hex(ff))

L39710:     if empcode$(1) <> "ALL" then L39750
               init(hex(00)) empcode$(1)
               init(hex(ff)) empcode$(2)
               go to L39790
L39750:     if empcode$(2) <> " " then L39770
               empcode$(2) = empcode$(1)
L39770:        empcode$(1) = empcode$(1) addc all(hex(ff))

L39790:     if payfreq$(2) <> " " then L39810
               payfreq$(2) = payfreq$(1)
L39810:        payfreq$(1) = payfreq$(1) addc all(hex(ff))
               return

L39840:              FMT       XX(20), CH(8), CH(24), CH(2)

L40000: REM *************************************************************~
            *        P R I N T   D A I L Y   R E C A P   I N F O        *~
            *                                                           *~
            * TAKES THE CONTENTS OF THE VARIOUS STACKS AND POSTS THEM TO*~
            * THE DAILY RECAP.  SHOWS OFF JUST FOR FUN.  WILL NOT PRINT *~
            * ANYTHING WHERE THE AMOUNT WAS ZERO OR THE STACK WAS EMPTY *~
            *************************************************************

            close printer
            text$ = "SUMMARY"
            str(text$,31) = "SUMMARY"
            str(text$,69) = "PAYROLL CHECK PRINT"
            if debitsptr% = 0 and creditsptr% = 0 then L65000
               call "SORT" addr(str(debitsstk$(),1), 250%, 12%)
               call "SORT" addr(str(creditsstk$(),1), 250%, 12%)
               totaldebits, totalcredits, colsdone% = 0
               mat rcpline% = con
               mat rcpptr%  = zer
               gosub L48000                     /* SKIP TO TOP OF PAGE  */

L40170:     for column% = 1 to 2
                on column% gosub L41000, L42000
                next column%
                print                    /* FREE UP LINE.              */
            if  colsdone% >= 2 then return         /* DONE W/ REPORT   */
                goto L40170

L41000:     REM HANDLES LEFT (DEBITS) COLUMN FOR REPORT
                on rcpline%(1) gosub L41100, L41200, L41300, L41400, L41500
                   return
L41100:         REM PRINT CONTENTS OF DEBITS STACK, IF ANY.
                    if debitsptr% = 0 then L41300
                    rcpptr%(1)  = rcpptr%(1) + 1
                    convert str(debitsstk$(rcpptr%(1)),10,3) to t%
                    rcpamt(1)   = debitsstk (t%)
                    rcpacct$(1) = str(debitsstk$(rcpptr%(1)),1,9)
                    if summary$ <> "Y" then L41130
                        if export_on$ <> "Y" then L41122
                           init (" ")  gl_check$, gl_deduction$,         ~
                                       gl_depart$, gl_employee$
                           tran_type$ = "EAC01" : gl_postamt = rcpamt(1)
                           gosub load_gl_info
L41122:            call "GLPOST2" (rcpacct$(1), rcpamt(1), 0, prldate$,  ~
                                  0%, "09", text$, jnlid$,               ~
                                  pstseq%, userid$, #2, #8, #1,          ~
                                  err%, " ", gl_post_info$())
L41130:             call "DESCRIBE" (#2, rcpacct$(1), rcpacctdescr$(1),  ~
                                                   0%, f1%(2))
                    call "GLFMT" (rcpacct$(1))
                    print using L49020, rcpacct$(1), rcpacctdescr$(1),    ~
                                         rcpamt(1);
                    totaldebits = totaldebits + debitsstk(t%)
                    if rcpptr%(1) < debitsptr% then return
                       rcpline%(1) = 2
                       return
L41200:         REM PRINTS SEPARATOR LINE.
                    print using L49010, "*--";
                    rcpline%(1) = 3
                    return
L41300:         REM PRINTS TOTAL LINE
                    totaldebits = round(totaldebits, 2)
                    print using L49030, totaldebits;
                    rcpline%(1) = 4
                    return
L41400:         REM PRINTS STARS
                    print using L49000, "*";
                    rcpline%(1) = 5
                    return
L41500:         REM SETS TO BLANKS AS COLUMN IS DONE.
                    rcpacct$(1), rcpacctdescr$(1) = " "
                    colsdone% = colsdone% + 1
                    rcpline%(1) = 6
                    return

L42000:     REM HANDLES RIGHT HAND COLUMN--CREDITS.
                print tab(70);
                on rcpline%(2) gosub L42100, L42200, L42300, L42400, L42500
                   return
L42100:         REM PRINT THE CREDITS STACK.
                    if creditsptr% = 0 then L42300
                    rcpptr%(2)  = rcpptr%(2) + 1
                    convert str(creditsstk$(rcpptr%(2)),10,3) to t%
                    rcpamt(2)   = creditsstk (t%)
                    rcpacct$(2) = str(creditsstk$(rcpptr%(2)),1,9)
                    if summary$ <> "Y" then L42129

                        if export_on$ <> "Y" then L42122
                           tran_type$ = "EAC04" : gl_postamt = -rcpamt(2)
                           gosub load_gl_info
L42122:            call "GLPOST2" (rcpacct$(2), 0, rcpamt(2), prldate$,  ~
                                  0%, "09", text$, jnlid$,               ~
                                  pstseq%, userid$, #2, #8, #1,          ~
                                  err%, " ", gl_post_info$())
L42129:             call "DESCRIBE" (#2, rcpacct$(2), rcpacctdescr$(2),  ~
                                             0%, f1%(2))
                    call "GLFMT" (rcpacct$(2))
                    print using L49020, rcpacct$(2),rcpacctdescr$(2),     ~
                               rcpamt(2);
                    totalcredits = totalcredits + creditsstk(t%)
                    if rcpptr%(2) < creditsptr% then return
                       rcpline%(2) = 2
                       return
L42200:         REM PRINT SEPARATOR LINE
                    print using L49010, "*--";
                    rcpline%(2) = 3
                    return
L42300:         REM PRINT TOTAL CREDITS LINE
                    totalcredits = round(totalcredits, 2)
                    print using L49040, totalcredits;
                    rcpline%(2) = 4
                    return
L42400:         REM PRINT STARS
                    print using L49000,"*";
                    rcpline%(2) = 5
                    return
L42500:         REM BLANK--PASS...
                    rcpline%(2) = 6
                    colsdone% = colsdone% + 1
                    rcpacct$(2), rcpacctdescr$(2) = " "
                    return

L48000:     REM PAGE CONTROL SUBROUTINE FOR PRINTING DAILY RECAP
                select printer (134)
                   call "DATE" addr ("HD", hdrdate$)
                   print page
                   rcppage% = rcppage% + 1%
                   print using L49060, rcppage%, title$, hdrdate$
                   print
                   print using L49100
                   print
                   print using L49140
                   print using L49180,"#","#"
                   print using L49220
                   return

L49000: %**************************#***********************************
L49010: %#--------------+--------------------------------+------------*
L49020: %* ############ ! ############################## !-#######.## *
L49030: %*              ! TOTAL DEBITS                   !-#######.## *
L49040: %*              ! TOTAL CREDITS                  !-#######.## *

L49060: %PAGE #####            ##########################################~
        ~###################    ##########################################~
        ~###

L49100: %=========================D E B I T S==========================  ~
        ~     ===========================C R E D I T S====================~
        ~==

L49140: %**************************************************************  ~
        ~     ************************************************************~
        ~**

L49180: %* ACCOUNT #    !     D E S C R I P T I O N      !   AMOUNT   *  ~
        ~     * ACCOUNT #    !     D E S C R I P T I O N      !   AMOUNT  ~
        ~ *

L49220: %*--------------+--------------------------------+------------*  ~
        ~     *--------------+--------------------------------+-----------~
        ~-*

L60000: REM *************************************************************~
            *        S T A C K   P U S H I N G   R O U T I N E S        *~
            *                                                           *~
            * PUSHES THE INDICATED INFORMATION ONTO THE DESIRED STACK   *~
            * FOR LATER PROCESSING.                                     *~
            *************************************************************

            REM INITIALIZE STACKS
                init(hex(ff)) debitsstk$(), creditsstk$()
                mat debitsstk = zer
                mat creditsstk = zer
                debitsptr%, creditsptr% = 0
                debitsptrmax%, creditsptrmax% = 250
                return

            deffn'162(account$, amount)  /* RECAP DEBITS ACCUMULATOR   */
                  search str(debitsstk$(),1) = account$                  ~
                               to location$ step 12 /* FIND ACCOUNT #   */
                  if location$ = hex(0000) then L60240  /* PUSH NEW ITEM*/
                     temp% = int(val(location$,2)/12)+1 /* WHICH CELL?  */
                     debitsstk(temp%) = round(debitsstk(temp%) + amount,2)
                               /* UPDATE AMOUNT FOR EXISTING ACCOUNT   */
                     return
L60240:           REM PUSH NEW ITEM ONTO STACK.
                      debitsptr% = debitsptr% + 1
                      if debitsptr% <= debitsptrmax% then L60290
                         gosub L40000   /* EMPTY STACK BY PRINTING RECAP*/
                         gosub L60000   /* RE-INITIALIZE BOTH STACKS    */
L60290:               debitsstk$(debitsptr%) = account$
                      convert debitsptr% to                              ~
                         str(debitsstk$(debitsptr%),10,3), pic(###)
                      debitsstk(debitsptr%) = amount
                      return

            deffn'163(account$, amount)  /* RECAP CREDITS ACCUMULATOR  */
                  search str(creditsstk$(),1) = account$                 ~
                               to location$ step 12
                  if location$ = hex(0000) then L60440  /* IF NO ==> NEW*/
                     temp% = int(val(location$,2)/12)+1 /* WHICH ELEMENT*/
                     creditsstk(temp%) = round(creditsstk(temp%)+amount,2)
                                         /* UPDATE AMT FOR THAT ELEMENT*/
                     return
L60440:           REM PUSH NEW ENTRY
                      creditsptr% = creditsptr% + 1
                      if creditsptr% <= creditsptrmax% then L60490
                         gosub L40000   /* EMPTY STACK BY PRINTING RECAP*/
                         gosub L60000   /* RE-INITIALIZE BOTH STACKS    */
L60490:               creditsstk$(creditsptr%) = account$
                      convert creditsptr% to                             ~
                          str(creditsstk$(creditsptr%),10,3), pic(###)
                      creditsstk(creditsptr%) = amount
                      return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "JNLCLOSE" (moduleno$, jnlid$, pstseq%, returncode%)
            call "SHOSTAT" ("Closing Files, One Moment Please")

        end
