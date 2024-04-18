        REM *************************************************************~
            *                                                           *~
            *  PPPP   RRRR   L      EEEEE  L      IIIII   SSS   TTTTT   *~
            *  P   P  R   R  L      E      L        I    S        T     *~
            *  PPPP   RRRR   L      EEEE   L        I     SSS     T     *~
            *  P      R   R  L      E      L        I        S    T     *~
            *  P      R   R  LLLLL  EEEEE  LLLLL  IIIII   SSS     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLELIST - LIST EMPLOYEE MASTER INFORMATION, EARNINGS AND *~
            *            DEDUCTION TYPES.                               *~
            *----------------------------------------------------------Q*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/29/81 ! ORIGINAL                                 ! TEM *~
            * 12/09/83 ! PREPARE FOR RELEASE                      ! HES *~
            * 03/26/86 ! Extended PRLDEPTF Record Length          ! HES *~
            * 02/11/87 ! Added Employement Status Selection       ! SGA *~
            * 03/10/87 ! Extended PRLBANKF Record Length          ! HES *~
            * 05/14/87 ! Standard Costing Changes.                ! ERN *~
            * 08/31/87 ! Correct Load Of Direct Deposit Data      ! HES *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            * 08/26/96 ! Changes for the year 2000.               ! DXL *~
            *************************************************************

        dim                                                              ~
            acct$(2)16,                  /* GL ACCOUNTS                */~
            address$(3)35,               /* ADDRESS                    */~
            amount$(5)10,                /* DEDUCTION AMOUNTS          */~
            amt(5),                      /* DITTO FROM FILE            */~
            amtdescr$(4)15,              /* AMOUNT DESCRIPTIONS FR. DDT*/~
            applies$6,                   /* APPLIES                    */~
            auto$3,                      /* AUTO PAYROLL               */~
            bankacct$(5)17,              /* CHECKING ACCOUNT NUMBER    */~
            bankcode$(5)4,               /* BANK I.D. CODE FOR DIRECTS */~
            bankdescr$(5)32,             /* BANK NAME FIELD            */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            cash$3,                      /* PAID IN CASH               */~
            category$6,                  /* DEDUCTION CATEGORY         */~
            city$20,                     /* CITY EMP LIVES IN          */~
            cmpcode$10,                  /* WORKMAN'S COMP. CODE       */~
            conphone$15,                 /* CONTACTS PHONE NUMBER      */~
            contact$40,                  /* EMERGENCY CONTACT          */~
            county$20,                   /* COUNTY EMP LIVES IN        */~
            cracct$16,                   /* CREDIT ACCOUNT             */~
            cursor%(2),                  /* CURSOR COLUMN LOCATION     */~
            date$8,                      /* SCREEN DATE                */~
            dayhrs$2,                    /* HOURS PER DAY              */~
            ddescr$12,                   /* DEDUCTION DESCRIPTION      */~
            dacct$16,                    /* DEBIT ACCOUNT              */~
            ddamount$(5)09,              /* DIRECT DEPOSIT AMOUNTS     */~
            dept$4,                      /* DEPARTMENT CODE            */~
            deptcode$4,                  /* DITTO                      */~
            descr$(5)32,                 /* DESCRIPTIONS               */~
            eeoc$3,                      /* EEOC CODE                  */~
            eeocdescr$50,                /* EEOC DESCRIPTION           */~
            empcode$12,                  /* EMPLOYEE CODE              */~
            empname$90,                  /* EMPLOYEE NAME FORMATTED    */~
            erntype$12,                  /* EARNINGS TYPE              */~
            errormsg$79,                 /* ERROR MESSAGE TEXT         */~
            expacct$16,                  /* EXPENSE ACCOUNT            */~
            firstone$12, firstemp$12,    /* FIRST NAME IN LIST         */~
            fstatusdescr$30,             /* FEDERAL FILING DESCRIPTION */~
            goal$10,                     /* ASCII GOAL                 */~
            hdrdate$45,                  /* PRINTER FORMATTED DATE     */~
            i$(24)80,                    /* SCREEN IMAGE WORK AREA     */~
            inpmessage$79,               /* INPUT MESSAGE TEXT         */~
            it$(2)30,                    /* ITS FOR AMOUNTS AND GOAL   */~
            lastone$12, lastemp$12,      /* LAST EMPLOYEE CODE LISTED  */~
            lfac$(20)1,                  /* FIELD DISPLAY FACS         */~
            line2$79,                    /* Screen Line #2             */~
            method$6,                    /* DEDUCTION METHOD           */~
            mstatusdescr$30,             /* MARITAL STATUS DESCRIPTION */~
            payfreq$1,                   /* PAY FREQUENCY              */~
            payfreqdescr$30,             /* PAY FREQUENCY DESCRIPTION  */~
            paymode$1,                   /* PAY MODE                   */~
            paymodedescr$30,             /* PAY MODE DESCRIPTION       */~
            pays$3,                      /* WHO PAYS DEDUCTION         */~
            pf16$16,                     /* PF KEY 16 VARIABLE         */~
            phone$15,                    /* EMPLOYEE PHONE NUMBER      */~
            plowkey$12,                  /* PLOW VARIABLE              */~
            plowkey2$50,                 /* ANOTHER PLOW VARIABLE      */~
            rate$10,                     /* PAY RATE                   */~
            readkey$50,                  /* A Read Key                 */~
            selected$1,                  /* SELECTED EMPLOYMENT STATUS */~
            surepay$(5)2,                /* SAVINGS OR CHECKING ACCOUNT*/~
            ssnum$15,                    /* SOCIAL SECURITY NUMBER     */~
            sstatus$1,                   /* STATE FILING STATUS        */~
            status$1,                    /* EMPLOYEE STATUS            */~
            statusdescr$30,              /* STATUS DESCRIPTION         */~
            tax$3,                       /* TAXABLE?                   */~
            title$16,                    /* JOB TITLE                  */~
            unit$6,                      /* UNIT DESCRIPTION           */~
            usamt$10,                    /* USUAL AMOUNT               */~
            usunit$10,                   /* USUAL UNITS                */~
            weekhrs$2,                   /* HOURS PER WEEK             */~
            wrkstn$4                     /* LABOR CLASS CODE           */~

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64)                      /* RECORD-ON-FILE FLAGS       */

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
            * # 2 ! EMPMASTR ! EMPLOYEE FILE MASTER RECORDS.            *~
            * # 3 ! EMPEARN1 ! EMPLOYEE EARNINGS DETAILS FILE           *~
            * # 4 ! EMPDEDXN ! EMPLOYEE DEDUCTIONS FILE.                *~
            * # 5 ! GLMAIN   ! GENERAL LEDGER MAIN ACCOUNT FILE.        *~
            * # 6 ! PRLDEPTF ! PAYROLL DEPARTMENT CODE FILE             *~
            * # 7 ! COMTERM  ! COMMON PERSONNEL TERMS                   *~
            * # 8 ! GENCODES ! General Codes File                       *~
            * # 9 ! PRLBANKF ! PAYROLL BANK CODE NUMBER FILE            *~
            * #10 ! EMPBANKS ! Employee direct deposit information      *~
            * #11 ! PRLDDT   ! PAYROLL DEDUCTION DEFINITION TABLE       *~
            * #14 ! PERMASTR ! PERSONNEL EMPLOYEE MASTER FILE           *~
            *************************************************************

            select # 2, "EMPMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 136,                                  ~
                         keypos = 1, keylen = 12,                        ~
                         alt key  1, keypos = 70, keylen =  1, dup

            select # 3, "EMPEARN1",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 200,                                  ~
                         keypos = 1, keylen = 15,                        ~
                         alternate key  1, keypos =  16, keylen = 28

            select # 4, "EMPDEDXN",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 300,                                  ~
                         keypos = 1, keylen = 15,                        ~
                         alternate key  1, keypos =  16, keylen = 18, dup

            select # 5, "GLMAIN",                                        ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 300,                                  ~
                         keypos = 1, keylen = 9

            select # 6, "PRLDEPTF",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 300,                                  ~
                         keypos = 1, keylen = 4

            select # 7, "COMTERM",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  162,                                  ~
                        keypos =   47, keylen =  16,                     ~
                        alt key  1, keypos =   17, keylen =  46,         ~
                            key  2, keypos =    1, keylen =  62

            select # 8, "GENCODES",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 128,                                  ~
                         keypos = 1, keylen = 24

            select # 9, "PRLBANKF",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 256,                                  ~
                         keypos = 1, keylen = 4

            select #10, "EMPBANKS",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 256,                                  ~
                         keypos = 1, keylen = 13

            select #11, "PRLDDT",                                        ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 400,                                  ~
                         keypos = 1, keylen = 6

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

            call "OPENCHCK" (# 2, 0%, f2%( 2),   0%, " ")
            call "OPENCHCK" (# 3, 0%, f2%( 3),   0%, " ")
            call "OPENCHCK" (# 4, 0%, f2%( 4),   0%, " ")
            call "OPENCHCK" (# 5, 0%, f2%( 5),   0%, " ")
            call "OPENCHCK" (# 6, 0%, f2%( 6),   0%, " ")
            call "OPENCHCK" (# 7, 0%, f2%( 7),   0%, " ")
            call "OPENCHCK" (# 8, 0%, f2%( 8),   0%, " ")
            call "OPENCHCK" (# 9, 0%, f2%( 9),   0%, " ")
            call "OPENCHCK" (#10, 0%, f2%(10),   0%, " ")
            call "OPENCHCK" (#11, 0%, f2%(11),   0%, " ")
            call "OPENCHCK" (#14, 0%, f2%(14),   0%, " ")

        REM *************************************************************~
            *      I N I T I A L I Z A T I O N                          *~
            *                                                           *~
            * SET SYSTEM DATE                                           *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date
            call "DATEFMT" (date$)

            str(line2$,62,9) = "PRLELIST:"
            str(line2$,72,8) = str(cms2v$,1,8)

L10000: REM *************************************************************~
            *         M A I N   P R O G R A M                           *~
            *                                                           *~
            * MAIN PROGRAM CONTROLLER SECTION                           *~
            *************************************************************

            init(" ") firstone$, lastone$, selected$, sortby$, errormsg$
            editmode% = 0
            u3% = 0
            firstemp$ = "ALL"

L10110:     for fieldnr% = 1 to 3
              gosub'051(fieldnr%)
                if enabled% = 0 then L10110
L10140:       gosub'101(fieldnr%)
                if keyhit% = 1  then gosub startover
                if keyhit% = 16 then L65000
                if keyhit% <> 0 then L10140
              gosub'151(fieldnr%)
                if errormsg$ <> " " then L10140
              next fieldnr%


L10230:      errormsg$ = " "
         inpmessage$ = "To Modify Displayed Values, Position Cursor To De~
        ~sired Value And Press RETURN."
            editmode% = 1

L10280:     gosub'111(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       main_report
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% > 3 then L10280

            gosub'051(fieldnr%)
                  if enabled% =  0 then L10230
L10360:     gosub'111(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then       L10360
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L10360
            goto L10230

        main_report
            REM INITIALIZE VARIABLES
                last%, pkey% = 0
                if sortby$ = "N" then pkey% = 2%
                if sortby$ = "S" then pkey% = 3%
        REM Set Read Key for files
            plowkey$ = firstone$
            if sortby$ <> "E" then plowkey$ = all(hex(00))

            call "SHOSTAT" ("Printing Employee Listing")
            call "PLOWALTS" (#14, plowkey$, pkey%, 0%, f1%(14))
                if f1%(14) = 0 then L10920
            goto L10640

L10610:     REM PLOW THROUGH PERSONNEL MASTER FILE
                call "READNEXT" (#14, f1%(14))
                     if f1%(14) = 0 then L10920
L10640:         if selected$ = "A" then L10670
                     get #14, status$
                     if status$ <> selected$ then L10610
L10670:         plowkey$ = key(#14, 0)
                if sortby$ = "E" and plowkey$ > lastone$ then L10920
                if plowkey$ > lastone$ then L10610
                if plowkey$ < firstone$ then L10610
                call "READ100" (#2, plowkey$, f1%(2))
                     if f1%(2) = 0 then L10610

            REM MASTER INFORMATION
                pageline% = 1000
                gosub L11000

            REM EARNINGS TYPES
                pagesave% = pageline%
                pageline% = 2000
                gosub L13000
                if pageline% = 2000 then pageline% = pagesave%

            REM DEDUCTION RECORDS
                pagesave% = pageline%
                pageline% = 2000
                gosub L14000

            REM NEXT EMPLOYEE
                go to L10610

L10920:     REM DONE WITH EMPLOYEES
                if pagenumber% = 0 then L65000
                if last% <= 1 then L65000
                if last% = 2 then print using L60800
                if last% = 3 then print using L60890
                go to L65000

L11000: REM *************************************************************~
            *         M A S T E R   P R I N T E R                       *~
            *                                                           *~
            * GET AND PRINT MASTER INFORMATION                          *~
            *************************************************************

            REM GET EMPLOYEE INFO
                gosub L30000

            gosub page_control
            print using L60980, "EMPLOYEE CODE                 ",         ~
                               empcode$
            pageline% = pageline% + 1
            print using L60980, "EMPLOYEE NAME:  FIRST         ",         ~
                              fname$
            if mname$ = " " then L11190
            pageline% = pageline% + 1
            print using L60980, "                MIDDLE        ",         ~
                              mname$
L11190:     pageline% = pageline% + 1
            print using L60980, "                LAST          ",         ~
                              lname$
            pageline% = pageline% + 1
            print using L60980, "ADDRESS                       ",         ~
                               address$(1)
            if address$(2) = " " then L11290
            pageline% = pageline% + 1
            print using L60980, "                              ",         ~
                               address$(2)
L11290:     if address$(3) = " " then L11330
            pageline% = pageline% + 1
            print using L60980, "                              ",         ~
                               address$(3)
L11330:     pageline% = pageline% + 1
            print using L60980, "PHONE NUMBER                  ",         ~
                               phone$
            if contact$ = " " then L11430
            pageline% = pageline% + 1
            print using L60980, "EMERGENCY CONTACT             ",         ~
                               contact$
            pageline% = pageline% + 1
            print using L60980, "EMERGENCY CONTACT'S PHONE NO. ",         ~
                               conphone$
L11430:     pageline% = pageline% + 1
            print using L60980, "SOCIAL SECURITY NUMBER        ",         ~
                               ssnum$
            if birthdate$ = " " or birthdate$ = blankdate$ then L11500
            pageline% = pageline% + 1
            print using L60980, "DATE OF BIRTH                 ",         ~
                         birthdate$
L11500:     pageline% = pageline% + 1
            print using L60980, "DATE HIRED                    ",         ~
                               origdate$
            if lastermdate$ = " " or lastermdate$ = blankdate$ then L11570
            pageline% = pageline% + 1
            print using L60980, "DATE LAST TERMINATED          ",         ~
                               lastermdate$
L11570:     if dept$ = " " then L11610
            pageline% = pageline% + 1
            print using L61010, "DEPARTMENT                    ",         ~
                               dept$, descr$(1)
L11610:     if title$ = " " then L11650
            pageline% = pageline% + 1
            print using L61010, "JOB  TITLE                    ",         ~
                               title$, descr$(2)
L11650:     if cursupr$ = " " then L11690
            pageline% = pageline% + 1
            print using L61010, "CURRENT SUPERVISOR            ",         ~
                             cursupr$
L11690:     if wrkstn$ = " " then L11730
            pageline% = pageline% + 1
            print using L61010, "NORMAL LABOR CLASS (JOBCOSTNG)",         ~
                               wrkstn$, descr$(3)
L11730:     if physical$ = " " then L11770
            pageline% = pageline% + 1
            print using L61010, "PHYSICAL STATUS               ",         ~
                              physical$
L11770:     pageline% = pageline% + 6
            print using L61010, "EEOC CLASSIFICATION           ",         ~
                               eeoc$, eeocdescr$
            print using L61010, "EMPLOYEE STATUS               ",         ~
                               status$, statusdescr$
            print using L61010, "ACTUAL MARITAL STATUS         ",         ~
                               marital$, mstatusdescr$
            print using L61010, "FEDERAL FILING STATUS         ",         ~
                               fstatus$, fstatusdescr$
            print using L61010, "PAY FREQUENCY                 ",         ~
                               payfreq$, payfreqdescr$
            print using L61010, "MODE OF PAYMENT               ",         ~
                               paymode$, paymodedescr$

            if bankcode$() = " " then L12000
            print skip(1)
            print using L61040
            print using L61062 : pageline% = pageline% + 3
            for i% = 1 to 5
                if bankcode$(i%) = " " then L11980
                print using L61070, bankcode$(i%), bankdescr$(i%),        ~
                               bankacct$(i%), surepay$(i%), ddamount$(i%)
                pageline% = pageline% + 1
L11980:     next i%
            print skip(1) : pageline% = pageline% + 1

L12000:     pageline% = pageline% + 9
            print using L61010, "CASH IN BANK ACCOUNT          ",         ~
                               acct$(1), descr$(4)
            print using L61010, "ACCRUED GROSS PAYROLL ACCOUNT ",         ~
                               acct$(2), descr$(5)
            print using L60980, "PAID BY AUTOMATIC PAYROLL?    ",         ~
                               auto$
            print using L60980, "NORMAL HOURS PER DAY          ",         ~
                               dayhrs$
            print using L60980, "NORMAL HOURS PER WEEK         ",         ~
                               weekhrs$
            print using L61010, "NORMAL WORKMAN'S COMP. CODE   ",         ~
                               cmpcode$
            print skip(3)
            return

L13000: REM *************************************************************~
            *     E A R N I N G S   T Y P E   P R I N T E R             *~
            *                                                           *~
            * GET AND PRINT EARNINGS TYPES                              *~
            *************************************************************

            REM SET PLOW VARIABLE
                plowkey2$ = empcode$

            REM PLOW DOWN EARNINGS
L13100:         call "PLOWNEXT" (#3, plowkey2$, 12%, f1%(3))
                                 if f1%(3) = 0 then return
            REM GET INFO
                gosub L32000

            REM HEADER AND THEN PRINT
                gosub'22
                print using L60860, erntype$, deptcode$, cash$,           ~
                                   unit$, rate$, expacct$, usunit$,      ~
                                   usamt$

            REM LOOP AROUND
                go to L13100

L14000: REM *************************************************************~
            *      D E D U C T I O N   P R I N T E R                    *~
            *                                                           *~
            * GETS AND PRINTS DEDUCTION INFO                            *~
            *************************************************************

            REM SET PLOW VARIABLE
                plowkey2$ = empcode$

            REM PLOW DOWN DEDUCTION RECORDS
L14100:         call "PLOWNEXT" (#4, plowkey2$, 12%, f1%(4))
                                if f1%(4) = 0 then return

            REM RETRIEVE INFO
                gosub L33000

            REM HEADER AND THEN PRINT, CAREFUL WITH AMOUNTS
                i% = 0: init(" ") it$()
                gosub L14310
                gosub'23
                print using L60950, category$, method$, ddescr$, pays$,   ~
                                   cracct$, dacct$, applies$, it$(1),    ~
                                   it$(2)
L14230:         if i% > 4 then L14100
                gosub L14310
                gosub'23
                print using L60950, " ", " ", " ", " ", " ", " ", " ",    ~
                                   it$(1), it$(2)
                go to L14230

L14310:         i% = i% + 1: if i% > 4 then L14355
                if amtdescr$(i%) = " " then L14310
                it$(1) = amtdescr$(i%)
                it$(2) = amount$(i%)
                return
L14355:         if goal = 0 then L14390
                it$(1) = "GOAL"
                it$(2) = goal$
                return
L14390:         it$(1), it$(2) = " "
                return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets defaults and enables fields for page 1 of the screen *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 0
                  on fieldnr% gosub L20140,         /* Employee Range   */~
                                    L20240,         /* Selected Status  */~
                                    L20310          /* Sort Option      */
                     return

L20140:     REM DEFAULT/ENABLE FOR EMPLOYEE SELECT RANGE
                inpmessage$ = "Enter starting & ending Employee "  &     ~
                   "Numbers, or 'ALL' to list All Employees"
                enabled% = 1
                return

L20240:     REM DEFAULT/ENABLE FOR SELECTED EMPLOYMENT STATUS
                inpmessage$ = "A=All C=Current P=Previous L=Leave of Abse~
        ~nce M=Military Leave N=Terminated"
                if selected$ = " " then selected$ = "A"
                enabled% = 1
                return

L20310:     REM DEFAULT/ENABLE FOR SELECTED Sort Option
                inpmessage$ = "E=By Employee Number, N=By Employee Name, ~
        ~S=By Status."
                if sortby$ = " " then sortby$ = "E"
                enabled% = 1
                return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

            ask% = 2%
            call "STARTOVR" (ask%)
            if ask% = 1% then return

            return clear all
            goto L10000

L30000: REM *************************************************************~
            *  G E T   M A S T E R  I N F O R M A T I O N               *~
            *                                                           *~
            * GET MASTER INFORMATION, AND FORMAT DATA                   *~
            *************************************************************

            bankcode$(), bankacct$(), surepay$(), ddamount$() = " "
            bankdescr$() = " "
            get #2, using L30155, empcode$, wrkstn$, fstatus$, sstatus$,  ~
                                 payfreq$, paymode$, acct$(1), acct$(2), ~
                                 dayhrs$, auto$, weekhrs$, cmpcode$,dept$

            plowkey2$ = empcode$ : u3% = 0
L30075:     call "PLOWNEXT" (#10, plowkey2$, 12%, f1%(10))
                if f1%(10) = 0% then L30280
            u3% = u3% + 1
            get #10 using L30120, bankcode$(u3%), bankacct$(u3%),         ~
                                 surepay$(u3%), amount
            call "DESCRIBE" (#9,bankcode$(u3%),bankdescr$(u3%),0%,f1%(9))
            call "CONVERT" (amount, 2.2, ddamount$(u3%))
            goto L30075

L30120: FMT                 /* FILE: EMPBANKS                          */~
            XX(13),         /* Skip key                                */~
            CH(4),          /* direct deposit bank code                */~
            CH(20),         /* Direct Deposit checking account number  */~
            CH(2),          /* Direct Deposit Code for Checking or Savi*/~
            PD(14,4)        /* Amount of something.                    */

L30155:     FMT CH(12),                  /* EMPLOYEE CODE              */~
                CH(4),                   /* LABOR CLASS CODE           */~
                CH(1),                   /* FED FILING STATUS          */~
                CH(1),                   /* STATE FILING STATUS        */~
                CH(1),                   /* PAY FREQUENCY              */~
                CH(1),                   /* MODE OF PAYMENT            */~
                XX(16),                  /* FILLER                     */~
                CH(9),                   /* CASH IN BANK ACCOUNT       */~
                CH(9),                   /* GROSS PAYROLL ACCOUNT      */~
                CH(2),                   /* NORMAL HRS/DAY ASCII(##)   */~
                CH(1),                   /* AUTOPAYROLL FLAG           */~
                XX(6),                   /* FIRST DATE PAY PERIOD      */~
                XX(6),                   /* LAST DATE PAY PERIOD       */~
                XX(1),                   /* DEDUCT ON THIS PRLDDUCT RUN*/~
                CH(02),                  /* NORMAL HRS/WEEK ASCII(##)  */~
                CH(10),                  /* WORKMANS' COMP. CODE DEF   */~
                XX(1),                   /* JOBCODE                    */~
                XX(1),                   /* SHIFT CODE                 */~
                XX(2),                   /* AREA CODE                  */~
                XX(3),                   /* O/H NUMBER                 */~
                XX(2),                   /* PRIMARY STATE CODE         */~
                CH(4),                   /* DEPARTMENT                 */~
                XX(41)                   /* FILLER FOR REST OF RECORD  */

L30280:     status$,             /* General purpose status indicator   */~
            ssnum$,              /* Social security number             */~
            phone$,              /* Telephone number                   */~
            address$(1),         /* Street address line 1              */~
            address$(2),         /* Street address line 2              */~
            city$,               /* City in address                    */~
            county$,             /* County in address                  */~
            state$,              /* State in address                   */~
            zip$,                /* Zip code in address                */~
            contact$,            /* emergency contact                  */~
            conphone$,           /* emergency contact's phone number   */~
            relationship$,       /* Emergency contacts relationship to */~
            gender$,             /* Gender of a person                 */~
            birthdate$,          /* birth date                         */~
            eeoc$,               /* minority (eeoc compliance) code    */~
            marital$,            /* Marital status - personnel system  */~
            dependants$,         /* Number of dependants - personnel s */~
            physical$,           /* A persons physical status - handyc */~
            title$,              /* Current job title - personnel syst */~
            cursupr$,            /* Current supervisor                 */~
            origdate$,           /* Original date hired                */~
            lastermdate$         /* Last date terminated */ = all("*")

            lname$ = "NOT IN PERSONNEL"  /* Really can't happen */
            fname$ = "FILE"
            mname$ = " "

         if f1%(14) = 1 then get #14, using L30550,   /* FILE: PERMASTR */~
            status$,             /* General purpose status indicator   */~
            lname$,              /* Last name of person - part of pers */~
            fname$,              /* First name of person               */~
            mname$,              /* Middle name of person              */~
            ssnum$,              /* Social security number             */~
            phone$,              /* Telephone number                   */~
            address$(1),         /* Street address line 1              */~
            address$(2),         /* Street address line 2              */~
            city$,               /* City in address                    */~
            county$,             /* County in address                  */~
            state$,              /* State in address                   */~
            zip$,                /* Zip code in address                */~
            contact$,            /* emergency contact                  */~
            conphone$,           /* emergency contact's phone number   */~
            relationship$,       /* Emergency contacts relationship to */~
        /*  GENDER$,             /* Gender of a person                 */~
            birthdate$,          /* birth date                         */~
            eeoc$,               /* minority (eeoc compliance) code    */~
            marital$,            /* Marital status - personnel system  */~
        /*  DEPENDANTS$,         /* Number of dependants - personnel s */~
            physical$,           /* A persons physical status - handyc */~
            title$,              /* Current job title - personnel syst */~
            cursupr$,            /* Current supervisor                 */~
            origdate$,           /* Original date hired                */~
            lastermdate$         /* Last date terminated               */~

L30550: FMT                      /* FILE: PERMASTR                     */~
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
            CH(30),              /* emergency contact                  */~
            CH(10),              /* emergency contact's phone number   */~
            CH(16),              /* Emergency contacts relationship to */~
            XX(1),               /* Gender of a person                 */~
            CH(6),               /* birth date                         */~
            CH(3),               /* minority (eeoc compliance) code    */~
            CH(1),               /* Marital status - personnel system  */~
            XX(2),               /* Number of dependants - personnel s */~
            CH(16),              /* A persons physical status - handyc */~
            XX(16),              /* A persons military status - vet, r */~
            XX(16),              /* Citizenship status - personnel sys */~
            XX(16),              /* Passport status - personnel system */~
            XX(16),              /* Union status - personnel system    */~
            XX(16),              /* Bonding status - personnel system  */~
            CH(16),              /* Current job title - personnel syst */~
            XX(16),              /* EEO class of current job - personn */~
            XX(4),               /* Current department                 */~
            XX(1),               /* Current shift worked               */~
            CH(16),              /* Current supervisor                 */~
            CH(6),               /* Original date hired                */~
            XX(6),               /* Seniority date                     */~
            XX(6),               /* Last date rehired                  */~
            CH(6)                /* Last date terminated               */

            REM FORMAT DATES
            call "DATFMTC" (birthdate$)
            call "DATFMTC" (lastermdate$)
            call "DATFMTC" (origdate$)

            REM FORMAT THE NAME
            if relationship$ <> " " and contact$ <> " " then             ~
                        contact$ = contact$ & " (" & relationship$ & ")"
            if mname$ <> " " then str(mname$,2) = "."
            if mname$ <> " " then empname$=lname$&", "&fname$&" " &mname$~
                                 else  empname$ = lname$ & " " &  fname$
            if state$ <> " " then state$ = state$ & "."
            address$(3) = city$ & " " & state$ & " " & zip$


            REM FORMAT PHONE NUMBERS
                temp$ = phone$
                phone$ = "(" & str(temp$, 1, 3) & ")" & str(temp$, 4, 3) ~
                         &  "-" & str(temp$, 7, 4)
                temp$ = conphone$
                conphone$ = "(" & str(temp$, 1, 3) & ")" &               ~
                             str(temp$, 4, 3) & "-" & str(temp$, 7, 4)


            REM FORMAT DEPARTMENT, JOB TITLE, LABOR CLASS
                call "DESCRIBE" (#6, dept$,  descr$(1), 1%, f1%(6))
                call "DESCRIBE" (#7, title$, descr$(2), 1%, f1%(7))
                readkey$ = "LBR CLASS" & wrkstn$
                call "DESCRIBE" (#8, readkey$, descr$(3), 1%, f1%(8))

            REM FORMAT MINORITY CODE
                call "PUTPAREN" (eeocdescr$)

            REM FORMAT STATUS
                statusdescr$ = "(Inactive)"
                if status$ = "C" then statusdescr$ = "(Active)"
                if status$ = "L" then statusdescr$ = "(Leave of Absence)"
                if status$ = "M" then statusdescr$ = "(Military Leave)"
                if status$ = "P" then statusdescr$ = "(Previous Employee)"
                if status$ = "N" then statusdescr$ = "(Terminated)"

            REM FORMAT ACTUAL MARITAL STATUS
                mstatusdescr$ = " "
                if marital$ = "S" then mstatusdescr$="(Single)"
                if marital$ = "M" then mstatusdescr$="(Married)"
                if marital$ = "H" then mstatusdescr$="(Head Of Household)"

            REM FORMAT FEDERAL FILING STATUS
                fstatusdescr$ = " "
                if fstatus$ = "S" then fstatusdescr$="(Single)"
                if fstatus$ = "M" then fstatusdescr$="(Married)"

            REM FORMAT PAY PREQUENCY
                payfreqdescr$ = " "
                if payfreq$ = "1" then payfreqdescr$ = "(Weekly)"
                if payfreq$ = "2" then payfreqdescr$ = "(Bi-Weekly)"
                if payfreq$ = "3" then payfreqdescr$ = "(Semi-Monthly)"
                if payfreq$ = "4" then payfreqdescr$ = "(Monthly)"
                if payfreq$ = "5" then payfreqdescr$ = "(Quarterly)"
                if payfreq$ = "6" then payfreqdescr$ = "(Semi-annually)"
                if payfreq$ = "7" then payfreqdescr$ = "(Annually)"

            REM FORMAT MODE OF PAYMENT
                paymodedescr$ = " "
                if paymode$ = "$" then paymodedescr$ = "(Cash)"
                if paymode$ = "C" then paymodedescr$ = "(Check)"
                if paymode$ = "D" then paymodedescr$ = "(Direct Deposit)"

            REM FORMAT GL ACCTS
                call "DESCRIBE" (#5, acct$(1), descr$(4), 1%, f1%(5))
                call "DESCRIBE" (#5, acct$(2), descr$(5), 1%, f1%(5))
                call "GLFMT" (acct$(1))
                call "GLFMT" (acct$(2))

            REM AUTO PAYROLL
                if auto$ = "Y" then auto$ = "YES"                        ~
                               else auto$ = "NO"

            REM AND RETURN
                return

L32000: REM *************************************************************~
            *        G E T   E A R N I N G   R E C O R D                *~
            *                                                           *~
            * RETRIEVE EARNINGS RECORD FROM BUFFER                      *~
            *************************************************************

            cash$, tax$ = " "
            get #3, using L32100, erntype$, deptcode$, cash$, unit$,      ~
                                 rate, expacct$, usunit, usamt
            call "GLFMT" (expacct$)

L32100:              FMT       XX(27),             /* SKIP BEGIN       */~
                               CH(12),             /* EARNINGS TYPE    */~
                               CH(4),              /* DEPARTMENT CODE  */~
                               CH(1),              /* PAID IN CASH     */~
                               XX(1),              /* DEAD SPACE       */~
                               CH(6),              /* UNITS DESCRIP    */~
                               PD(14,4),           /* UNIT RATE        */~
                               CH(9),              /* EXPENSE ACCT     */~
                               PD(14,4),           /* USUAL UNITS      */~
                               PD(14,4)            /* USUAL AMOUNT     */

            if str(tax$, 1, 1) = "Y" then tax$ = "YES"                   ~
                                     else tax$ = "NO"
            if str(cash$,1, 1) = "Y" then cash$ = "YES"                  ~
                                     else cash$ = "NO"
            call "CONVERT" (rate, 2.4, rate$)
            if usunit <> 0 then call "CONVERT" (usunit, 2.4, usunit$)    ~
                           else usunit$ = " "
            if usamt  <> 0 then call "CONVERT" (usamt , 2.4, usamt$ )    ~
                           else usamt$ = " "

            return

L33000: REM *************************************************************~
            *         G E T   D E C U C T I O N   R E C O R D           *~
            *                                                           *~
            * GET DEDUCTION INFORMATION                                 *~
            *************************************************************

            pays$ = " "
            get #4, using L33110, category$, method$, ddescr$, pays$,     ~
                                 cracct$, dacct$, applies$, amt(1),      ~
                                 amt(2), amt(3), amt(4), goal
            call "GLFMT" (cracct$)
            call "GLFMT" (dacct$)

L33110:              FMT       XX(27),             /* SKIP KEY STUFF   */~
                               CH(6),              /* CATEGORY         */~
                               CH(6),              /* METHOD           */~
                               CH(12),             /* DESCRIPTION      */~
                               CH(1),              /* EMPLOYEE PAYS    */~
                               CH(9),              /* CREDIT ACCOUNT   */~
                               CH(9),              /* DEBIT ACCOUTN    */~
                               CH(6),              /* APPLIES          */~
                               5*PD(14,4)          /* AMOUNTS + GOAL   */~

            REM FORMAT PAYS
                if pays$ = "Y" then pays$ = "YES"                        ~
                               else pays$ = "NO"

            REM FORMAT AMOUNTS AND DESCRIPTIONS
                init(" ") amount$()
                call "READ100" (#11, method$, f1%(11))
                     if f1%(11) = 0 then L33350
                     get #11, using L33300, str(amtdescr$(), 1)
L33300:                       FMT XX(43), CH(60)
                for i% = 1 to 4
                    if amtdescr$(i%) <> " " then                         ~
                       call "CONVERT" (amt(i%), 2.4, amount$(i%))
                next i%
L33350:         call "CONVERT" (goal, 2.2, goal$)

            REM AND RETURN
                return

        REM *************************************************************~
            *   S C R E E N   F O R   R A N G E   O F   E M P L O Y E ES*~
            *                                                           *~
            * ACCEPT SCREEN FOR RANGE OF EMPLOYEES                      *~
            *************************************************************

            deffn'101(fieldnr%)
              pf16$ = "(16)Exit Program"
              goto L40069
            deffn'111(fieldnr%)
              pf16$ = "(16)Print Report"
L40069:         init(hex(8c)) lfac$()
                  on fieldnr% gosub L40140,         /* Employee Range   */~
                                    L40140,         /* Selected Status  */~
                                    L40140          /* Sort Option      */
            goto L40200

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40140:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return
L40200:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Employee Listing",                              ~
               at (01,67),                                               ~
                  "DATE:",                                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Employee Range        from:",                         ~
               at (06,30), fac(lfac$( 1)), firstemp$            , ch(12),~
               at (06,46), "to:",                                        ~
               at (06,50), fac(lfac$( 1)), lastemp$             , ch(12),~
               at (07,02),                                               ~
                  "Employment Status",                                   ~
               at (07,30), fac(lfac$( 2)), selected$            ,  ch(1),~
               at (08,02),                                               ~
                  "Sort Order (E, N or S)",                              ~
               at (08,30), fac(lfac$( 3)), sortby$              ,  ch(1),~
                                                                         ~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65), fac(hex(8c)), pf16$                  , ch(16),~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key(keyhit%)

            if keyhit% <> 13 then L40550
                call "MANUAL" ("PRLELIST")
                goto L40200

L40550:        if keyhit% <> 15 then L40590
                  call "PRNTSCRN"
                  go to L40200

L40590:        if editmode% = 0 then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *          T E S T   P R I N T   R A N G E                  *~
            *                                                           *~
            * TEST PRINT RANGE ENTERED                                  *~
            *************************************************************
            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50140,         /* Employee Range   */~
                                    L50370,         /* Selected Status  */~
                                    L50410          /* Sort Option      */
                    return

L50140: REM Test Data for Employee Range
            call "TESTRNGE" (firstemp$, lastemp$, firstone$, lastone$,   ~
                             errormsg$)
            return

L50370:      REM TESTS EMPLOYMENT STATUS SELECTED
                 if pos("ACPLMN" = selected$) <> 0 then return
                 errormsg$ = "INVALID TYPE!  Please Respecify."
                 return

L50410:      REM TESTS SORT OPTION
                 if pos("ENS" = sortby$) <> 0 then return
                 errormsg$ = "Please Enter 'E', 'N' or 'S'."
                 return

        REM *************************************************************~
            *      H E A D E R    C O N T R O L L E R                   *~
            *                                                           *~
            * CONTROLS THE HEADER FOR THE MASTER INFO, THE EARNINGS     *~
            * TYPES, AND THE DEDUCTION RECORDS.                         *~
            *************************************************************

        page_control
            REM MASTER RECORD HEADER
                pageline% = pageline% + 1
                if pageline% < 60 then return
                select printer (134)
                if pagenumber% = 0 then L60170
                on last% + 1 go to L60170, L60170, L60150, L60160
                            go to L60170
L60150:            print using L60800 : go to L60170
L60160:            print using L60890 : go to L60170
L60170:         print page
                pagenumber% = pagenumber% + 1
                call "DATE" addr("HD", hdrdate$)
                print using L60730, pagenumber%, hdrdate$
                print using L60770, empname$
                print
                pageline% = 5
                last% = 1
                return

            deffn'22
            REM EARNINGS TYPES HEADER
                pageline% = pageline% + 1
                if pageline% < 60 then return
                if pageline% = 2001 then L60420
                if pagenumber% = 0 then L60370
                on last% + 1 go to L60370, L60370, L60350, L60360
                   go to L60370
L60350:            print using L60800 : go to L60370
L60360:            print using L60890 : go to L60370
L60370:         print page
                pagenumber% = pagenumber% + 1
                print using L60730, pagenumber%, hdrdate$
                print
                pagesave% = 0
L60420:         print using L60800
                print using L60830
                print using L60800
                pageline% = pagesave% + 6
                last% = 2
                return

            deffn'23
            REM DEDUCTIONS TYPE HEADER
                pageline% = pageline% + 1
                if pageline% < 60 then return
                if pagenumber% = 0 then L60610
                   on last% + 1 go to L60610, L60610, L60560, L60580
                      go to L60600
L60560:               print using L60800
                            print skip (2) : go to L60600
L60580:               print using L60890
                            print skip (2) : go to L60600
L60600:         if pageline% = 2001 then L60660
L60610:         print page
                pagenumber% = pagenumber% + 1
                print using L60730, pagenumber%, hdrdate$
                print
                pagesave% = 0
L60660:         print using L60890
                print using L60920
                print using L60890
                pageline% = pagesave% + 6
                last% = 3
                return

L60730: %PAGE #####               E M P L O Y E E   M A S T E R   L I S T~
        ~ I N G                 ##########################################~
        ~###

L60770: %          FOR EMPLOYEE #########################################~
        ~#########################

L60800: %  +-------------+------+-----+------+-----------+-----------+---~
        ~--------------+-------------+--------------+

L60830: %  !EARNINGS TYPE! DEPT !CASH?! UNIT DESCRIPTION ! UNIT RATE ! GL~
        ~ EXPENSE ACCT ! USUAL UNITS ! USUAL AMOUNT !

L60860: %  ! ############! #### ! ### !       ######     !########## !   ~
        ~############  !  ########## !  ##########  !

L60890: %  +--------+------+---------------+-------+-------------+-------~
        ~-----+---------+------------------------------+

L60920: %  !CATEGORY!METHOD! DEDUC. DESCR. !DEDUCT?! CREDIT ACCT ! DEBIT ~
        ~ACCT ! APPLIES !   DEDUCTION PARAMETERS       !

L60950: %  ! ###### !######!  ############ !  ###  !#############!#######~
        ~#####!  ###### ! ################  ########## !

L60980: %                  ##############################      ##########~
        ~####################

L61010: %                  ##############################      ##########~
        ~##   ########################################

L61040: %                  DIRECT DEPOSIT BREAK DOWN        BANK DESCRIPT~
        ~ION                     ACCOUNT NUMBER       SURE PAY      AMOUNT

L61062: %                                                   ---- --------~
        ~----------------------  -------------------- --------   ---------

L61070: %                                                   #### ########~
        ~######################  ####################    ##      #########

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")
            end
