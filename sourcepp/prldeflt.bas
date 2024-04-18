        REM *************************************************************~
            *                                                           *~
            *  PPPP   RRRR   L      DDDD   EEEEE  FFFFF  L      TTTTT   *~
            *  P   P  R   R  L      D   D  E      F      L        T     *~
            *  PPPP   RRRR   L      D   D  EEEE   FFFF   L        T     *~
            *  P      R   R  L      D   D  E      F      L        T     *~
            *  P      R   R  LLLLL  DDDD   EEEEE  F      LLLLL    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLDEFLT - SET EMPLOYEE RECORD DEFAULTS FOR PAYROLL.  THE *~
            *            DEDUCTION DEFINITION TABLE WILL HANDLE ALL THE *~
            *            DEFAULT DEDUCTIONS FOR PAYROLL.                *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/22/80 ! ORIGINAL                                 ! BCW *~
            * 12/09/80 ! RETROFIT OF DAYS BETWEEN REVIEWS FIELD   ! BCW *~
            * 06/23/81 ! ADD NUMBER OF DAYS PER WEEK              ! TEM *~
            * 10/13/86 ! ADDED DIRECT DEPOSIT BANK CODE           ! HES *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            *************************************************************

        dim                                                              ~
            bank$4,                      /* COMPANY BANK ACCOUNT       */~
            bankdescr$32,                /* COMPANY BANK ACCOUNT       */~
            cashacct$16,                 /* CASH IN BANK ACCOUNT       */~
            cashacctdescr$32,            /* CASH ACCOUNT DESCRIPTION   */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDITS  */~
            date$8,                      /* TODAY'S DATE DISPLAY.      */~
            edtmessage$79,               /* "TO MODIFY VALUES..." TEXT */~
            errormsg$79,                 /* ERROR MESSAGE TEXT         */~
            fac$(20)1,                   /* FIELD ATTRIBUTE CHARACTERS */~
            grsacct$16,                  /* GROSS PAY ACCOUNT          */~
            grsacctdescr$32,             /* GROSS PAY ACCT DESCRIPTION */~
            i$(24)80,                    /* EDIT SCREEN IMAGE (NOT USED*/~
            inpmessage$79,               /* MESSAGE FOR FIELD INPUT    */~
            line2$79,                    /* Screen Line #2             */~
            normalhrs$2,                 /* NORMAL HOURS TO WORK       */~
            normalwks$2,                 /* NORMAL HOURS PER WEEK      */~
            payfreq$1,                   /* PAY FREQUENCY (1-7)        */~
            payfreqdescr$32,             /* PAY FREQUENCY DESCRIPTION  */~
            paymode$1,                   /* PAY MODE (CASH, CHECK,...) */~
            paymodedescr$32,             /* TELLS ABOUT PAY MODE       */~
            reviewdays$10                /*                            */

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64)                      /* RECORD-ON-FILE FLAGS       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.01 11/05/92 Payroll Switch & Other          "
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
            * # 1 ! SYSFILE2 ! GET CUSTOMER DEFAULT INFO FROM HERE.     *~
            * # 2 ! GLMAIN   ! GENERAL LEDGER MAIN FILE                 *~
            * # 3 ! PRLBANKF ! Payroll bank information file            *~
            *************************************************************

            select #1,  "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select #2,  "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1 , keylen = 9

            select #3,  "PRLBANKF",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  256,                                  ~
                        keypos =    1, keylen =   4,                     ~
                        alt key 1, keypos = 5, keylen = 30, dup


*        Check to See if Payroll/Personnel is Active
            call "PRLEXTSB" ("PRL", prl%)
                if prl% = 99% then end (prl%)

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (# 1, 0%, f2%( 1), 100%, " ")
            call "OPENCHCK" (# 2, 0%, f2%( 2),   0%, " ")
            call "OPENCHCK" (# 3, 0%, f2%( 3),   0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZE SCREEN INFORMATION FOR HEADINGS.               *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press RETURN."

            str(line2$,61,9) = "PRLDEFLT:"
            str(line2$,71,8) = str(cms2v$,1,8)

            gosub L30000
            if f1%(1) = 0 then inputmode                                 ~
                          else L11000

        REM *************************************************************~
            *        I N P U T   P A Y R O L L   D E F A U L T S        *~
            *                                                           *~
            * MAIN ROUTINE FOR INPUTTING PAYROLL DEFAULTS.              *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$,                            ~
                      grsacct$, grsacctdescr$, cashacct$, cashacctdescr$,~
                      payfreq$, payfreqdescr$, paymode$, paymodedescr$,  ~
                      normalhrs$, normalwks$, bank$, bankdescr$

            for fieldnr% = 1 to 7
                gosub'161(fieldnr%)
                      if enabled% =  0 then L10210
L10150:         gosub'201(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then L10150
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10150
L10210:         next fieldnr%

L11000: REM *************************************************************~
            *        E D I T   M O D E   F O R   D E F A U L T S        *~
            *                                                           *~
            * MAIN ROUTINE FOR EDITING OF THE PAYROLL DEFAULTS.         *~
            *************************************************************

        editmode
L11070:     gosub'211(0%)
                  if keyhit% =  1 then gosub startover
                  if keyhit% = 16 then       datasave
                  if keyhit% <> 0 then       L11070
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% > 7 then L11070

            gosub'161(fieldnr%)
L11150:     gosub'211(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then       L11150
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11150
            goto editmode

        REM *************************************************************~
            *                    W R I T E   D A T A                    *~
            *                                                           *~
            * WRITES DATA FOR THE DEFAULTS.                             *~
            *************************************************************

        datasave
            gosub L31000
            goto  L65000

        REM *************************************************************~
            *       D E F A U L T / E N A B L E   D E F A U L T S       *~
            *                                                           *~
            * DEFAULT/ENABLE FOR PAYROLL DEFAULTS.  NOTE THAT MAINLY,   *~
            * THIS ROUTINE SETS INPMSG$ AT THE BOTTOM OF THE SCREEN     *~
            * TELLING WHAT WE ARE DOING.                                *~
            *************************************************************

            deffn'161(fieldnr%)
                  enabled% = 0
                  on fieldnr% gosub L20300,         /* PAY FREQUENCY    */~
                                    L20400,         /* MODE OF PAYMENT  */~
                                    L20200,         /* CASH IN BANK ACCT*/~
                                    L20100,         /* GROSS PAY ACCT   */~
                                    L20500,         /* NORMAL HRS/DAY   */~
                                    L20550,         /* NORMAL WKS/WEEK  */~
                                    L20575          /* BANK CODE        */

                     return
L20100:     REM DEFAULT/ENABLE FOR GROSS PAYROLL ACCOUNT
                enabled% = 1
                inpmessage$ = " "
                return
L20200:     REM DEFAULT/ENABLE FOR CASH IN BANK ACCOUNT
                enabled% = 1
                inpmessage$ = " "
                return
L20300:     REM DEFAULT/ENABLE FOR PAY FREQUENCY
                enabled% = 1
                inpmessage$ = "1=Weekly,2=Biweekly,3=Semimonthly,4=Monthl~
        ~y,5=Quarterly,6=Semiyearly,7=Yearly"
                return
L20400:     REM DEFAULT/ENABLE FOR MODE OF PAYMENT
                enabled% = 1
                inpmessage$ = "'C' = Check, 'D' = Direct Deposit, or '$' ~
        ~= Cash"
                return
L20500:     REM DEFAULT/ENABLE FOR NORMAL HOURS PER DAY
                enabled% = 1
                inpmessage$ = "Enter Normal Hours Worked In a Day"
                return
L20550:     REM DEFAULT/ENABLE FOR NORMAL HOURS PER WEEK
                enabled% = 1
                inpmessage$ = "Enter Normal Hours Worked In a Week"
                return
L20575:     REM DEFAULT/ENABLE FOR BANK CODE
                enabled% = 1
                inpmessage$ = "Enter Code To Identify YOUR Companies ACH ~
        ~Routing# (Direct Deposit Users Only)."
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

L30000: REM *************************************************************~
            *       L O A D   D E F A U L T S   F R O M   F I L E       *~
            *                                                           *~
            * LOADS DEFAULTS FROM FILE AND FORMATS ALL THE DESCRIPTIONS *~
            * BEFORE RETURNING.                                         *~
            *************************************************************

            call "READ101" (#1, "EMPLOYEE DEFAULTS", f1%(1))
                 if f1%(1) = 0 then return

            get #1, using L30250,                                         ~
                    grsacct$, cashacct$, payfreq$, paymode$, normalhrs$, ~
                    reviewdays, normalwks$, bank$
            call "DESCRIBE" (#3, bank$, bankdescr$, 1%, f1%(3))
            call "DESCRIBE" (#2, grsacct$,  grsacctdescr$,  1%, f1%(2))
                call "GLFMT" (grsacct$)
            call "DESCRIBE" (#2, cashacct$, cashacctdescr$, 1%, f1%(2))
                call "GLFMT" (cashacct$)
            if reviewdays <> 0                                           ~
               then call "CONVERT" (reviewdays, 0.0, reviewdays$)

            gosub L60000                  /* FORMAT PAY FREQUENCY CODE  */
            gosub L61000                  /* FORMAT PAY MODE CODE       */
            return

L30250:     FMT XX(20),                  /* KEY TO RECORD              */~
                CH(9),                   /* GROSS PAY ACCOUNT          */~
                CH(9),                   /* CASH IN BANK ACCOUNT       */~
                CH(1),                   /* PAY FREQUENCY              */~
                CH(1),                   /* PAY MODE (CASH, CHK, OR ?) */~
                CH(2),                   /* NORMAL HOURS PER DAY       */~
                PIC(####),               /* DAYS BETWEEN REVIEWS       */~
                CH(2),                   /* NORMAL HOURS POR WEEK      */~
                XX(9),                   /* FILLER                     */~
                CH(4)                    /* BANK CODE                  */

L31000: REM *************************************************************~
            *            W R I T E   D A T A   T O   F I L E            *~
            *                                                           *~
            * WRITE ALL THE DATA FROM THE DEFAULTS TO THE FILE.         *~
            *************************************************************

            if reviewdays$ <> " "                                        ~
               then convert reviewdays$ to reviewdays                    ~
               else reviewdays = 0

            call "GLUNFMT" (grsacct$)
            call "GLUNFMT" (cashacct$)

            put #1, using L31200, "EMPLOYEE DEFAULTS",                    ~
                    grsacct$, cashacct$, payfreq$, paymode$, normalhrs$, ~
                    reviewdays, normalwks$, " ", bank$, " "
            if f1%(1) = 0 then write   # 1                               ~
                          else rewrite # 1
            return

L31200:     FMT CH(20),                  /* KEY TO RECORD              */~
                CH(9),                   /* GROSS PAY ACCOUNT          */~
                CH(9),                   /* CASH IN BANK ACCOUNT       */~
                CH(1),                   /* PAY FREQUENCY              */~
                CH(1),                   /* PAY MODE (CASH, CHK, OR ?) */~
                CH(2),                   /* NORMAL HOURS PER DAY       */~
                PIC(####),               /* DAYS BETWEEN REVIEWS       */~
                CH(2),                   /* NORMAL HOURS PER WEEK      */~
                CH(9),                   /* FILLER                     */~
                CH(4),                   /* BANK CODE                  */~
                CH(439)                  /* SPARE CHANGE               */

        REM *************************************************************~
            *          I N P U T   D E F A U L T S   S C R E E N        *~
            *                                                           *~
            * INPUTS DEFAULTS.                                          *~
            *************************************************************

            deffn'201(fieldnr%)
                  init(hex(84)) fac$()
                  on fieldnr% gosub L40210,         /* PAY FREQUENCY    */~
                                    L40210,         /* MODE OF PAYMENT  */~
                                    L40210,         /* PAYROLL CASH ACCT*/~
                                    L40210,         /* GROSS PAY ACCT   */~
                                    L40210,         /* HOURS PER DAY    */~
                                    L40210,         /* HOURS PER WEEK   */~
                                    L40210          /* BANK CODE        */

                     goto L40280

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      fac$(fieldnr%) = hex(80)
                      return
L40210:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      fac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      fac$(fieldnr%) = hex(82)
                      return

L40280:     accept                                                       ~
               at (01,02),                                               ~
                  "Input Employee Defaults",                             ~
               at (01,60), "Todays Date:",                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)),  errormsg$             , ch(79),~
               at (06,02),                                               ~
                  "Pay Frequency",                                       ~
               at (06,30), fac(fac$( 1)), payfreq$              , ch(01),~
               at (06,49), fac(hex(8c)),  payfreqdescr$         , ch(32),~
               at (07,02),                                               ~
                  "Mode of Payment",                                     ~
               at (07,30), fac(fac$( 2)), paymode$              , ch(01),~
               at (07,49), fac(hex(8c)),  paymodedescr$         , ch(32),~
               at (08,02),                                               ~
                  "Cash in Bank Account",                                ~
               at (08,30), fac(fac$( 3)), cashacct$             , ch(12),~
               at (08,49), fac(hex(8c)),  cashacctdescr$        , ch(32),~
               at (09,02),                                               ~
                  "Payroll Accrual Account",                             ~
               at (09,30), fac(fac$( 4)), grsacct$              , ch(12),~
               at (09,49), fac(hex(8c)),  grsacctdescr$         , ch(32),~
               at (10,02),                                               ~
                  "Normal Hours Per Day",                                ~
               at (10,30), fac(fac$( 5)), normalhrs$            , ch(02),~
               at (11,02),                                               ~
                  "Normal Hours Per Week",                               ~
               at (11,30), fac(fac$( 6)), normalwks$            , ch(02),~
               at (12,02),                                               ~
                  "Your Bank Code",                                      ~
               at (12,30), fac(fac$( 7)), bank$                 , ch(04),~
               at (12,49), fac(hex(8c)),  bankdescr$            , ch(32),~
                                                                         ~
               at (21,02), fac(hex(a4)),  inpmessage$           , ch(79),~
               at (23,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)EXIT PROGRAM",                                    ~
                                                                         ~
               keys (hex(00010d0f10)),                                   ~
               key  (keyhit%)

               if keyhit% <> 13 then L40790
                  call "MANUAL" ("PRLDEFLT")
                  goto L40280

L40790:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40280

        REM *************************************************************~
            *         E D I T   P A Y R O L L   D E F A U L T S         *~
            *                                                           *~
            * EDITS PAYROLL DEFAULTS.                                   *~
            *************************************************************

            deffn'211(fieldnr%)
                  init(hex(84)) fac$()
                  if fieldnr% = 0 then init(hex(86)) fac$()
                  on fieldnr% gosub L41220,         /* PAY FREQUENCY    */~
                                    L41220,         /* MODE OF PAYMENT  */~
                                    L41220,         /* PAYROLL CASH ACCT*/~
                                    L41220,         /* GROSS PAY ACCT   */~
                                    L41220,         /* HOURS PER DAY    */~
                                    L41220,         /* HOURS PER WEEK   */~
                                    L41220          /* BANK CODE        */

                     goto L41290

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      fac$(fieldnr%) = hex(80)
                      return
L41220:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      fac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      fac$(fieldnr%) = hex(82)
                      return

L41290:     accept                                                       ~
               at (01,02),                                               ~
                  "Edit Employee Defaults",                              ~
               at (01,60), "Todays Date:",                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)),  errormsg$             , ch(79),~
               at (06,02),                                               ~
                  "Pay Frequency",                                       ~
               at (06,30), fac(fac$( 1)), payfreq$              , ch(01),~
               at (06,49), fac(hex(8c)),  payfreqdescr$         , ch(32),~
               at (07,02),                                               ~
                  "Mode of Payment",                                     ~
               at (07,30), fac(fac$( 2)), paymode$              , ch(01),~
               at (07,49), fac(hex(8c)),  paymodedescr$         , ch(32),~
               at (08,02),                                               ~
                  "Cash in Bank Account",                                ~
               at (08,30), fac(fac$( 3)), cashacct$             , ch(12),~
               at (08,49), fac(hex(8c)),  cashacctdescr$        , ch(32),~
               at (09,02),                                               ~
                  "Payroll Accrual Account",                             ~
               at (09,30), fac(fac$( 4)), grsacct$              , ch(12),~
               at (09,49), fac(hex(8c)),  grsacctdescr$         , ch(32),~
               at (10,02),                                               ~
                  "Normal Hours Per Day",                                ~
               at (10,30), fac(fac$( 5)), normalhrs$            , ch(02),~
               at (11,02),                                               ~
                  "Normal Hours Per Week",                               ~
               at (11,30), fac(fac$( 6)), normalwks$            , ch(02),~
               at (12,02),                                               ~
                  "Your Bank Code",                                      ~
               at (12,30), fac(fac$( 7)), bank$                 , ch(04),~
               at (12,49), fac(hex(8c)),  bankdescr$            , ch(32),~
                                                                         ~
               at (21,02), fac(hex(a4)),  edtmessage$           , ch(79),~
               at (23,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)SAVE DATA",                                       ~
                                                                         ~
               keys (hex(00010d0f10)),                                   ~
               key  (keyhit%)

               if keyhit% <> 13 then L41800
                  call "MANUAL" ("PRLDEFLT")
                  goto L41290

L41800:        if keyhit% <> 15 then L41840
                  call "PRNTSCRN"
                  goto L41290

L41840:        close ws
               call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TEST THE DATA FOR VALIDITY.                               *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50330,         /* PAY FREQUENCY    */~
                                    L50400,         /* MODE OF PAYMENT  */~
                                    L50250,         /* CASH IN BANK ACCT*/~
                                    L50170,         /* GROSS PRL ACCT   */~
                                    L50480,         /* NORMAL HOURS/DAY */~
                                    L50530,         /* NORMAL HOURS/WEEK*/~
                                    L50580          /* BANK CODE        */

                     return
L50170:     REM TEST DATA FOR GROSS PAYROLL ACCOUNT
                grsacctdescr$ = " "
                if grsacct$ = " " then return
                call "GETCODE"(#2, grsacct$,  grsacctdescr$, 1%,0,f1%(2))
                if f1%(2) <> 0 then return
                   errormsg$ = "Gross Payroll Account Not On File :"     ~
                                                   & grsacct$
                   return
L50250:     REM TEST DATA FOR CASH IN BANK ACCOUNT
                cashacctdescr$ = " "
                if cashacct$ = " " then return
                call "GETCODE"(#2, cashacct$, cashacctdescr$,1%,0,f1%(2))
                if f1%(2) <> 0 then return
                   errormsg$ = "Cash In Bank Account Not On File :"      ~
                                                   & cashacct$
                   return
L50330:     REM TEST DATA FOR PAY FREQUENCY
                payfreqdescr$ = " "
                if payfreq$ = " " then return
                   gosub L60000
                   if payfreqdescr$ <> " " then return
                errormsg$ = "Invalid Entry For Pay Frequency :" & payfreq$
                return
L50400:     REM TEST DATA FOR MODE OF PAYMENT
                paymodedescr$ = " "
                if paymode$ = " " then return
                   gosub L61000
                   if paymodedescr$ <> " " then return
                errormsg$ = "Invalid Entry For Mode Of Payment :"        ~
                                                   & paymode$
                return
L50480:     REM TEST DATA FOR NORMAL HOURS PER DAY
                if normalhrs$ = " " then return
                call "NUMTEST" (normalhrs$, 1, 24, errormsg$, -0.01, nn)
                if errormsg$ <> " " then return
                return
L50530:     REM TEST DATA FOR NORMAL HOURS PER WEEK
                if normalwks$ = " " then return
                call "NUMTEST" (normalwks$, 1, 80, errormsg$, -0.01, nn)
                if errormsg$ <> " " then return
                return
L50580:     REM TEST DATA FOR BANK CODE
                bankdescr$ = " "
                if bank$ = " " then return
                call "GETCODE" (#3, bank$,  bankdescr$, 1%, 1, f1%(3))
                     if f1%(3) = 0 then errormsg$= "Undeffined Bank Code"
                return

L60000: REM *************************************************************~
            *        D E S C R I B E   P A Y   F R E Q U E N C Y        *~
            *                                                           *~
            * DESCRIBE PAY FREQUENCY, MAPPING DESCRIPTION INTO          *~
            * PAYFREQDESCR$.                                            *~
            *************************************************************

            if payfreq$ = " " then return
            if payfreq$ = "1" then payfreqdescr$ = "(Weekly)"
            if payfreq$ = "2" then payfreqdescr$ = "(Bi-Weekly)"
            if payfreq$ = "3" then payfreqdescr$ = "(Semi-Monthly)"
            if payfreq$ = "4" then payfreqdescr$ = "(Monthly)"
            if payfreq$ = "5" then payfreqdescr$ = "(Quarterly)"
            if payfreq$ = "6" then payfreqdescr$ = "(Semi-Annually)"
            if payfreq$ = "7" then payfreqdescr$ = "(Annually)"
               return

L61000: REM *************************************************************~
            *      D E S C R I B E   M O D E   O F   P A Y M E N T      *~
            *                                                           *~
            * DESCRIBE THE MODE OF PAYMENT. (CASH, CHECK OR DIRECT DEP.)*~
            *************************************************************

            if paymode$ = " " then return
            if paymode$ = "C" then paymodedescr$ = "(Check)"
            if paymode$ = "$" then paymodedescr$ = "(Cash)"
            if paymode$ = "D" then paymodedescr$ = "(Direct Deposit)"

               return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("Closing Files, One Moment Please")

            end
