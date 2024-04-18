        REM *************************************************************~
            *                                                           *~
            *  PPPP     A    Y   Y  DDDD   EEEEE  FFFFF  L      TTTTT   *~
            *  P   P   A A   Y   Y  D   D  E      F      L        T     *~
            *  PPPP   AAAAA   YYY   D   D  EEE    FFF    L        T     *~
            *  P      A   A    Y    D   D  E      F      L        T     *~
            *  P      A   A    Y    DDDD   EEEEE  F      LLLLL    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PAYDEFLT - Accounts Payables System Defaults Management.  *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/16/86 ! ORIGINAL                                 ! HES *~
            * 05/09/95 ! PRR 13403.  Can't have blank Accounts.   ! JDH *~
            *          ! Added PF32 to exit without save.         !     *~
            *************************************************************

        dim                                                              ~
            billsdue$3,                  /* DAYS DUE DISCOUNT          */~
            discsdue$3,                  /* DAYS DUE REGULAR           */~
            cursor%(2),                  /* CURSOR LOCATION            */~
            date$8,                      /* SCREEN DATE                */~
            edtmessage$79,               /* EDIT INSTRUCTIONS          */~
            errormsg$79,                 /* ERROR MESSAGE TEXT         */~
            glaccts$(6)16,               /* A/P Accounts (defaults)    */~
            gldescrs$(6)30,              /* A/P Account Descriptions   */~
            i$(24)80,                    /* SCREEN IMAGE (NOT USED)    */~
            inpmessage$79,               /* INSTRUCTION FOR INPUT      */~
            lfac$(20)1,                  /* FAC FOR INPUT              */~
            line2$79,                    /* Screen Underline           */~
            nextcheck$7,                 /* NEXT AP CHECK NUMBER       */~
            pfdescr$(2)79,               /* PF Literal values          */~
            pfkeys$16                    /* PF Actual values           */

        dim f2%(04),                     /* FILE STATUS FLAGS FOR      */~
            f1%(04),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(04)20,                 /* RETURN CODE FROM "FILEOPEN"*/~
            axd$(04)4                    /* AXD POINTER FROM "FILEOPEN"*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.01 06/23/95 Patch finalization of R6.04.00  "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! SYSFILE2 ! SYSTEM CONTROL FILE                      *~
            * # 2 ! GLMAIN   ! GENERAL LEDGER MAIN FILE                 *~
            *************************************************************

            select  #1, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select  #2, "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 9

            call "SHOSTAT"  ("Opening Files, One Moment Please")

            call "OPENFILE" (#1, "SHARE", f2%( 1), rslt$( 1), axd$( 1))
            call "OPENFILE" (#2, "SHARE", f2%( 2), rslt$( 2), axd$( 2))

            REM IF SYSTEM CONTROL FILE NOT THERE THEN CREATE IT
                if f2%(1) = 0 then L09000
                call "OPENFILE" (#1, "OUTPT", f2%(1), rslt$(1), axd$(1))
                close #1
                call "OPENFILE" (#1, "SHARE", f2%(1), rslt$(1), axd$(1))

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)
            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (RETURN)."

            call "READ100" (#1, "MODULE.DEFAULTS.AP  ", f1%(1))
                 if f1%(1) = 0 then L10000
                 gosub L30000
                 go to edtpg1

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * INPUT MODE MAIN PROGRAM.                                  *~
            *************************************************************

        inputmode
            errormsg$, inpmessage$, billsdue$, discsdue$, nextcheck$,    ~
            glaccts$(), gldescrs$() = " "
            editmode% = 0

            for fieldnr% = 1 to 9
                gosub'161(fieldnr%)
                      if enabled% = 0 then L10250
L10140:         gosub'201(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then L10210
L10170:                  fieldnr% = max(1%, fieldnr%-1%)
                         gosub'161(fieldnr%)
                         if enabled% = 0 then L10170
                         goto L10140
L10210:               if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L10140
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10140
L10250:     next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR LINEAR SCREENS.        *~
            *************************************************************

        edtpg1
            editmode% = 1
            inpmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Field And Press (RETURN)."
L11100:     gosub'201(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit%  = 32% then      L65000
                  if keyhit% <>  0 then       L11100
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% > 10 then L11100
            if fieldnr% = 4 then L11100
            if fieldnr% > 4 then fieldnr% = fieldnr% - 1

            gosub'161(fieldnr%)
                  if enabled% = 0 then edtpg1
L11210:     gosub'201(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11210
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11210
            goto edtpg1

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            REM FIRST DELETE OLD ENTRY
                call "DELETE" (#1, "MODULE.DEFAULTS.AP  ", 20%)

            REM NOW GO SAVE...
                gosub L31000

            REM ... AND EXIT
                go to L65000

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'161(fieldnr%)
                  enabled% = 1
                  on fieldnr% gosub L20130,         /* BILL DAYS DUE    */~
                                    L20180,         /* DISCOUNT DUE     */~
                                    L20230,         /* NEXT CHECK #     */~
                                    L20280,         /* Purchases        */~
                                    L20312,         /* Price-Cost Var   */~
                                    L20320,         /* Interim Liability*/~
                                    L20360,         /* Accounts Payable */~
                                    L20400,         /* Cash-in-Bank     */~
                                    L20440          /* Discounts Taken  */
                     return
L20130:     REM DEFAULT/ENABLE FOR BILL DAYS DUE
                inpmessage$ = "Enter Default Number Of Days After Invoice~
        ~ That Payment Is Due."
                if billsdue$ = " " then billsdue$ = "30"
                return
L20180:     REM DEFAULT/ENABLE FOR DISCOUNT DUE
                inpmessage$ = "Enter Default Number Of Days After Invoice~
        ~ That Payment Is Due To Get Discount"
                if discsdue$ = " " then discsdue$ = "20"
                return
L20230:     REM DEFAULT/ENABLE FOR NEXT CHECK #
                inpmessage$ = "Enter Default Next Check Number For Accoun~
        ~ts Payables Check Printing."
                return
L20280:     REM DEFAULT/ENABLE FOR PURCHASES ACCOUNT
                inpmessage$ = "Enter Default Purchase Account.  Blank Is ~
        ~Valid Input."
                return
L20312:     REM DEFAULT/ENABLE FOR Price-Cost Variance Account
                inpmessage$ = "Enter Default Price-Cost Variance Account.~
        ~  Blank Is Valid Input."
                return
L20320:     REM DEFAULT/ENABLE FOR INTERIM LIABILITY ACCOUNT
                inpmessage$ = "Enter Default P.O. Interim Liability Accou~
        ~nt.  Blank Is Valid Input."
                return
L20360:     REM DEFAULT/ENABLE FOR AP ACCOUNT
                inpmessage$ = "Enter Default A/P Account.  Blank Is Valid~
        ~ Input."
                return
L20400:     REM DEFAULT/ENABLE FOR CASH ACCOUNT
                inpmessage$ = "Enter Default Cash Account.  Blank Is Vali~
        ~d Input."
                return
L20440:     REM DEFAULT/ENABLE FOR DISCOUNT ACCOUNT
                inpmessage$ = "Enter Default Discounts Taken Account.  Bl~
        ~ank Is Valid Input."
                return

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
            on k%+1% goto L29942, L29948
            return

L29942:        REM START OVER            (ENTER)
                   return clear
                   goto inputmode
L29948:        REM RETURN TO DISPLAY.    (P.F. KEY 1)
                   return

L30000: REM *************************************************************~
            *           R E C A L L   O L D   D E F A U L T S           *~
            *                                                           *~
            * LOADS EXISTING DEFAULTS FROM FILE                         *~
            *************************************************************

            get #1, using L30220, billsdue, discsdue, nextcheck$,         ~
                                 glaccts$()

            REM FORMAT DATA FOR SCREENS
                call "CONVERT" (abs(billsdue), 0.0, str(billsdue$,,2))
                     if billsdue < 0 then billsdue$=billsdue$ & "P"
                call "CONVERT" (abs(discsdue), 0.0, str(discsdue$,,2))
                     if discsdue < 0 then discsdue$=discsdue$ & "P"

            REM RETRIEVE ACCOUNT DESCRIPTIONS
            for i% = 1 to 6
                call "DESCRIBE" (#2,glaccts$(i%),gldescrs$(i%),0%,f1%(2))
                call "GLFMT" (glaccts$(i%))
            next i%
            return

L30220:     FMT      XX(20),             /* SKIP IDENTIFIER            */~
                     BI(4),              /* NORMALS DAYS DUE           */~
                     BI(4),              /* DISCOUNT DAYS DUE          */~
                     XX(1),              /* ALLWAYS '0'                */~
                     CH(7),              /* NEXT CHECK NUMBER (LAST 7) */~
                     6*CH(9)             /* GL Account Defaults        */

L31000: REM *************************************************************~
            *            S A V E   N E W   D E F A U L T S              *~
            *                                                           *~
            * SAVES ENTERED DEFAULTS ON FILE                            *~
            *************************************************************

            for i% = 1 to 6
                call "GLUNFMT" (glaccts$(i%))
            next i%

            write #1, using L31150, "MODULE.DEFAULTS.AP  ", billsdue,     ~
                                   discsdue, "0", nextcheck$, glaccts$(),~
                                   " ", " "
            return

L31150:     FMT      CH(20),             /* PUT IN INDENTIFIER         */~
                     BI(4),              /* NORMALS DAYS DUE           */~
                     BI(4),              /* DISCOUNT DAYS DUE          */~
                     CH(1),              /* ALLWAYS '0'                */~
                     CH(7),              /* NEXT CHECK NUMBER (LAST 7) */~
                     6*CH(9),            /* GL Account Defaults        */~
                     CH(201),            /* FILLER                     */~
                     CH(209)             /* FILLER                     */

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'201(fieldnr%)
                str(line2$,62%) = "PAYDEFLT: " & cms2v$
                init(hex(8c)) lfac$()
                if editmode% <> 0 then L40190
                pfdescr$(1) = "                  (4)Prev Field           ~
        ~                     (15)Print Screen"
                pfdescr$(2) = "                                          ~
        ~                     (16)Exit Program"
                pfkeys$ = hex(0001040d0f10ffffffffffffffffffff)
                if fieldnr% > 1% then pfdescr$(2) = " "
                if fieldnr% = 1% then str(pfdescr$(1),19,15) = " "
                goto L40290

L40190:         REM Editmode logic...
                pfdescr$(1) = "                                          ~
        ~                     (15)Print Screen"
                pfdescr$(2) = "                                          ~
        ~(32)Exit w/o Save    (16)Save Data   "
                pfkeys$ = hex(0001ff0d0f1020ffffffffffffffffff)
                if fieldnr% = 0% then init(hex(86)) lfac$()
                if fieldnr% <> 0% then pfdescr$(2) = " "

L40290:           on fieldnr% gosub L40420,         /* Bills due        */~
                                    L40420,         /* Discount Due     */~
                                    L40420,         /* Next Check #     */~
                                    L40420,         /* Purchases        */~
                                    L40420,         /* Price-Cost Var   */~
                                    L40420,         /* Interim Liability*/~
                                    L40420,         /* Accounts Payable */~
                                    L40420,         /* Cash-in-Bank     */~
                                    L40420          /* Discounts Taken  */
                     goto L40490

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40420:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40490:     accept                                                       ~
               at (01,02), "Manage Accounts Payables System Defaults",   ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Bills Due (Days) Default",                   ~
               at (06,30), fac(lfac$(1)), billsdue$             , ch(03),~
               at (07,02), "Discounts Due (Days)",                       ~
               at (07,30), fac(lfac$(2)), discsdue$             , ch(03),~
               at (08,02), "Next A/P Check Number",                      ~
               at (08,30), fac(lfac$(3)), nextcheck$            , ch(07),~
                                                                         ~
               at (10,02), "Default : Purchases",                        ~
               at (10,30), fac(lfac$(4)), glaccts$(1)           , ch(16),~
               at (10,49), fac(hex(8c)),  gldescrs$(1)          , ch(30),~
               at (11,02), "  G/L   : Prc-Cst Variance",                 ~
               at (11,30), fac(lfac$(5)), glaccts$(6)           , ch(16),~
               at (11,49), fac(hex(8c)),  gldescrs$(6)          , ch(30),~
               at (12,02), "Accounts: Interim Liability",                ~
               at (12,30), fac(lfac$(6)), glaccts$(2)           , ch(16),~
               at (12,49), fac(hex(8c)),  gldescrs$(2)          , ch(30),~
               at (13,02), "          Accounts Payable",                 ~
               at (13,30), fac(lfac$(7)), glaccts$(3)           , ch(16),~
               at (13,49), fac(hex(8c)),  gldescrs$(3)          , ch(30),~
               at (14,02), "          Cash-in-Bank",                     ~
               at (14,30), fac(lfac$(8)), glaccts$(4)           , ch(16),~
               at (14,49), fac(hex(8c)),  gldescrs$(4)          , ch(30),~
               at (15,02), "          Discounts Taken",                  ~
               at (15,30), fac(lfac$(9)), glaccts$(5)           , ch(16),~
               at (15,49), fac(hex(8c)),  gldescrs$(5)          , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,02), fac(hex(8c)),   pfdescr$(1)          , ch(79),~
               at (24,02), fac(hex(8c)),   pfdescr$(2)          , ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 13 then L40900
                call "MANUAL" ("PAYDEFLT")
                goto L40490

L40900:        if keyhit% <> 15 then L40940
                  call "PRNTSCRN"
                  goto L40490

L40940:        if fieldnr% <> 0% then return
               close ws
               call "SCREEN" addr ("C", k%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50180,         /* BILLS DUE DAYS   */~
                                    L50360,         /* DISCOUNT DUE DAYS*/~
                                    L50540,         /* NEXT CHECK #     */~
                                    L50630,         /* Purchases        */~
                                    L50662,         /* Prc-Cst Variance */~
                                    L50670,         /* Interim Liability*/~
                                    L50710,         /* Accounts Payable */~
                                    L50750,         /* Cash-in-Bank     */~
                                    L50790          /* Discounts Taken  */
                     return

L50180:     REM TEST BILLS DUE DAYS FIELD FOR NUMERICALLY VALID.
                if pos(billsdue$="P")=0 then L50320 /* HANDLE PROX.     */
                   if pos(billsdue$="P")=1 then L50240    /*P=1ST CHAR  */
                   temp$ = str(billsdue$,1,pos(billsdue$="P")-1)
                   call "NUMVALID" (temp$, err%, 1)
                        if err% = 0 then L50270
L50240:                    errormsg$ = "Illegal Number In Bills Due: "&  ~
                               billsdue$
                           return
L50270:            convert temp$ to billsdue
                   billsdue$ = temp$ & "P"
                           if billsdue>31 or billsdue < 1 then L50240
                   billsdue  = -(abs(billsdue))    /* FOR FILE LAYOUT. */
                   return
L50320:         REM COMPUTE FOR REGULAR (NON-PROX)
                call "NUMTEST" (billsdue$,0,9999,errormsg$,0.0,billsdue)
                      return

L50360:     REM TEST DISCOUNTS DUE DAYS FOR NUMERICALLY VALID.
                if pos(discsdue$="P")=0 then L50500 /* HANDLE PROX.     */
                if pos(discsdue$="P")=1 then L50420 /* P IS 1ST CHAR-BAD*/
                   temp$ = str(discsdue$,1,pos(discsdue$="P")-1)
                   call "NUMVALID" (temp$, err%, 1)
                        if err% = 0 then L50450
L50420:                    errormsg$="Invalid Number In Discounts Due: "&~
                               discsdue$
                           return
L50450:            convert temp$ to discsdue
                   discsdue$ = temp$ & "P"
                           if discsdue>31 or discsdue < 1 then L50420
                   discsdue  = -(abs(discsdue))    /* FOR FILE LAYOUT. */
                   return
L50500:         REM COMPUTE FOR REGULAR (NON-PROX)
                call "NUMTEST" (discsdue$,0,9999,errormsg$,0.0,discsdue)
                      return

L50540:      REM TEST FOR VALID STARTING CHECK NUMBER
                 if nextcheck$=" " then nextcheck$ = "0"
                 convert nextcheck$ to nextcheck%,data goto L50590
                 convert nextcheck% to nextcheck$, pic(0000000)
                 return
L50590:              errormsg$="Next Check Number Must Be Numeric" &     ~
                                                               nextcheck$
                 return

L50630:     REM TEST FOR VALID PURCHASES ACCOUNT
                gosub find_account
                return

L50662:     REM TEST FOR VALID Prc-Cst Variance Account
                gosub find_account
                return

L50670:     REM TEST FOR VALID INTERIM LIABILITY ACCOUNT
                gosub find_account
                return

L50710:     REM TEST FOR VALID AP ACCOUNT
                gosub find_account
                return

L50750:     REM TEST FOR VALID CASH ACCOUNT
                gosub find_account
                return

L50790:     REM TEST FOR VALID DISCOUNT ACCOUNT
                gosub find_account
                return

        find_account
            i% = fieldnr% - 4
            if i% = 1% then i% = 6%
            if i% = 0% then i% = 1%
            if glaccts$(i%) = " " then  L50900
            call "GETCODE" (#2,glaccts$(i%),gldescrs$(i%),0%,0.30,f1%(2))
                if f1%(2) = 0 then errormsg$ =                           ~
                     "Invalid G/L Account Number: " & glaccts$(i%)
        return
L50900:     errormsg$ = "A/P Default Account Numbers Cannot be Blank."
            return
L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")
            end
