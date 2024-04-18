        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  PPPP    AAA   Y   Y   AAA   DDDD   JJJJJ  IIIII  N   N   *~
            *  P   P  A   A  Y   Y  A   A  D   D    J      I    NN  N   *~
            *  PPPP   AAAAA   YYY   AAAAA  D   D    J      I    N N N   *~
            *  P      A   A    Y    A   A  D   D  J J      I    N  NN   *~
            *  P      A   A    Y    A   A  DDDD    J     IIIII  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PAYADJIN - USED TO DUMP ADJUSTMENTS INTO THE PAY BUFFER.  *~
            *            LOGIC IS SIMILAR TO 'PAYINPUT'.                *~
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
            * 09/26/83 ! ORIGINAL                                 ! HES *~
            * 12/11/84 ! MAKE PAYBUF2 RECORD SAME AS PAYINPUT     ! JWG *~
            * 12/16/85 ! Vendor file format changes               ! MJB *~
            * 01/17/85 ! Validate User, get new account defaults  ! HES *~
            * 05/13/86 ! Standards Mods   (screens, file fmts)    ! HES *~
            * 05/18/87 ! PAYBUF2 mods for Standard cost.          ! JIM *~
            * 09/16/88 ! Added PAYLINES File                      ! MJB *~
            * 04/01/94 ! Condition for background posting         ! KAB *~
            * 08/15/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            amount$10,                   /* AMOUNT                     */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            cost(12), cost$96,           /* Standard costs             */~
            date$8,                      /* SYSTEM DATE                */~
            datetime$7,                  /* DATE TIME STAMP            */~
            docid$16,                    /* DOCUMENT ID.               */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            freetext$20,                 /* FREE TEXT                  */~
            header$79,                   /* Screen Title               */~
            header$(2)79,                /* Plowcode Dummy variable    */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            inc(1),                      /* For The Elusive "PLOWCODE" */~
            inc$(1)1,                    /* For The Elusive "PLOWCODE" */~
            invdate$8,                   /* DOCUMENT DATE              */~
            invtype$1,                   /* ADJUSTMENT TAG = A         */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            message$79,                  /* GENERAL INPUT MESSAGE      */~
            origdate$6,                  /* DATE OF ORIGINAL INVOICE   */~
            origuserid$3,                /* USERID OF ORIGINAL INVOICE */~
            payacct$16,                  /* PAYABLES ACCOUNT           */~
            payacctdescr$30,             /* PAYABLES ACCOUNT DESCRIPTIO*/~
            payaccttype$1,               /* PAYABLES ACCOUNT TYPE      */~
            paydate$8,                   /* DATE EFFECTIVE             */~
            pfdescr$(3)79,               /* FUNCTION KEYS ENABLED LISTS*/~
            pfkeys$32,                   /* FUNCTION KEYS ENABLED LISTS*/~
            postdate$8,                  /* A/P POST DATE              */~
            puracct$16,                  /* PURCHASE ACCOUNT           */~
            puracctdescr$30,             /* PURCHASE ACCOUNT DESCRIPTIO*/~
            readkey$60,                  /* PLOW KEY                   */~
            sysacct$(5)9,                /* SYSTEM DEFAULT ACCOUNTS    */~
            ten99$4,                     /* 1099 CATEGORY              */~
            ten99descr$32,               /* 1099 CATEGORY              */~
            ten99key$50,                 /* 1099 CATEGORY              */~
            text$4,                      /* Document Text Id. Number   */~
            textmsg$79,                  /* Text Rtn Message           */~
            text$(113,1)70,              /* Free Text Array            */~
            vendor$9,                    /* VENDOR NUMBER              */~
            vendescr$32,                 /* VENDOR NUMBER              */~
            userid$3                     /* VENDOR NUMBER              */

        dim f1%(64)                      /* = 1 IF READ WAS SUCCESSFUL */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! VENDOR   ! Vendor Master File                       *~
            * #02 ! PAYBUFFR ! Payables buffer header file              *~
            * #03 ! PAYBUF2  ! Payables line inem buffer file           *~
            * #04 ! PAYMASTR ! Payables main file                       *~
            * #05 ! PAYLINES ! Payables Line Items File                 *~
            * #06 ! GLMAIN   ! General Ledger Main File                 *~
            * #07 ! GENCODES ! 1099 VALIDATION                          *~
            * #08 ! SYSFILE2 ! SYSTEM INFO (DEFAULT PAY DATES)          *~
            * #09 ! USERINFO ! USER INFORMATION (PAYABLES DATE)         *~
            * #10 ! TXTFILE  ! System Text File                         *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #01, "VENDOR",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  600,                                  ~
                        keypos =    1, keylen =   9,                     ~
                        alt key 1, keypos = 10, keylen = 30, dup

            select #02, "PAYBUFFR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  350,                                  ~
                        keypos =    1, keylen =  10,                     ~
                        alternate key 1, keypos = 11, keylen = 25

            select #03, "PAYBUF2",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 541,                                   ~
                        keypos = 36, keylen = 28,                        ~
                        alternate key 1, keypos = 1, keylen = 63,        ~
                                  key 2, keypos = 17, keylen = 47

            select #04, "PAYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  350,                                  ~
                        keypos =    1, keylen =  25                      ~

            select #05, "PAYLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 541,                                   ~
                        keypos = 36, keylen = 28,                        ~
                        alternate key 1, keypos = 1, keylen = 63,        ~
                                  key 2, keypos = 17, keylen = 47

            select #06, "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  300,                                  ~
                        keypos =    1, keylen =   9                      ~

            select #7,  "GENCODES", varc, indexed, recsize = 128,        ~
                        keypos = 1, keylen = 24

            select #8,  "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select #9,  "USERINFO",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos = 1, keylen = 3

            select #10, "TXTFILE",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos  = 1, keylen = 11

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#2, 0%, 0%, 100%, " ")
            call "OPENCHCK" (#3, 0%, 0%, 200%, " ")
            call "OPENCHCK" (#4, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#5, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#6, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#7, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#8, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#9, 0%, 0%, 0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "PAYBKCTL" (#2, ret%)
               if ret% <> 0% then end 0%

            date$ = date
            call "DATEFMT" (date$)
            call "EXTRACT" addr ("ID", userid$)

            REM SET DATES FOR THE SCREENS.
                call "READ100" (#9, userid$, f1%(9))
                     if f1%(9) = 0 then L65000
                get #9, using L09140  , postdate$
L09140:         FMT XX(9), CH(6)

            REM VALIDATE USERS POSTING DATE
                call "WHICHMON" (#8, postdate$, this%)
                  if this% <> 0 then L09240
                 call "ASKUSER" (keyhit%, "Sorry",                       ~
                      "Your Posting Date Is Outside The Posting Window", ~
                                         " ",  "Press (RETURN) To Exit.")
                goto L65000

L09240:     call "DATEFMT" (postdate$)

            REM SYSTEM DEFAULTS
                call "READ100" (#8, "MODULE.DEFAULTS.AP  ", f1%(8))
                     if f1%(8) = 0 then L10000
                get #8, using L09300  , sysacct$()
L09300:         FMT XX(36), 5*CH(9)

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
            errormsg$, message$, payacct$, puracct$, vendor$, vendescr$, ~
            invdate$, docid$, amount$, payacctdescr$, freetext$, ten99$, ~
            puracctdescr$, paydate$, ten99descr$, origdate$, origuserid$,~
            text$ = " "
            editmode% = 0
            call "TXTFUTIL" (#10, 0%, "INTL", text$)

            for fieldnr% = 1 to  9
                gosub'051(fieldnr%)
                      if enabled% = 0 then L10280
L10170:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
L10190:               if keyhit% <>  4 then L10240
                         fieldnr% = max(1%, fieldnr%-1%)
                         gosub'051(fieldnr%)
                         if enabled% = 0 then L10190
                         goto L10170
L10240:               if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L10170
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10170
L10280:         next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

        editmode
            editmode% = 1
            message$ = "To Modify Displayed Values, Position Cursor To De~
        ~sired Value And Press RETURN."
L11100:     gosub'111(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit%  = 25 then gosub edit_text
                  if keyhit% <>  0 then       L11100
            fieldnr% = cursor%(1) - 6
            if fieldnr% < 3 or fieldnr% >  9 then L11100

            gosub'051(fieldnr%)
                if enabled% = 0 then editmode
L11200:     gosub'111(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11200
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11200
            goto L11100

        edit_text
            textmsg$= "Vendor: " & vendor$ & ", Adjustment Id: " & docid$
            call "TXTINSUB" (#10, 0%, "011", textmsg$, text$, text$())
            return

        REM *************************************************************~
            *          W R I T E   D A T A   T O   B U F F E R          *~
            *                                                           *~
            * THIS ROUTINE DELETES THE OLD INVOICE FROM THE BUFFER IF   *~
            * THERE WAS ONE.  THEN IT GOES AND CALLS THE ROUTINE THAT   *~
            * WRITES THE NEW ONE OUT THERE.                             *~
            *************************************************************

        datasave
            REM FIRST TRY TO KILL THE INVOICE ON FILE.
                readkey$ = vendor$
                str(readkey$, 10) = docid$
                call "REDALT1" (#2, readkey$, 1%, f1%(2))
                      if f1%(2) <> 0 then delete #2
                call "DELETE"   (#3, readkey$, 25%)

            REM WRITE INVOICE.
                gosub L31000
                lastvendor$ = vendor$
                lastinvoice$ = docid$
                goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 0
                  message$ = " "
                  on fieldnr% gosub L20190,         /* VENDOR NUMBER    */~
                                    L20240,         /* DOCUMENT ID.     */~
                                    L20280,         /* DOCUMENT DATE    */~
                                    L20330,         /* PAYABLES ACCOUNT */~
                                    L20440,         /* PURCHASE ACCOUNT */~
                                    L20540,         /* AMOUNT           */~
                                    L20600,         /* EFFECTIVE DATE   */~
                                    L20640,         /* FREE TEXT        */~
                                    L20680          /* 1099 CATEGORY    */
                     return
L20190:     REM DEFAULT/ENABLE FOR VENDOR NUMBER
                enabled% = 1
                message$ = "Enter Vendor Number.  Leave Blank And Press (~
        ~RETURN) To Find An Existing Vendor."
                return
L20240:     REM DEFAULT/ENABLE FOR DOCUMENT ID.
                enabled% = 1
                message$ = "Enter Adjustment Document Id Number."
                return
L20280:     REM DEFAULT/ENABLE FOR DOCUMENT DATE
                if invdate$ = " " or invdate$ = blankdate$ then invdate$ = date$
                message$ = "Enter Date Of Adjustment"
                enabled% = 1
                return
L20330:     REM DEFAULT/ENABLE FOR PAYABLES ACCOUNT
                if payacct$ <> " " then L20400
                get #1, using L20360 , payacct$
L20360:                 FMT XX(258), CH(9)
                if payacct$ = " " then payacct$ = sysacct$(3)
                call "DESCRIBE" (#6, payacct$, payacctdescr$, 1%, f1%(6))
                call "GLFMT" (payacct$)
L20400:         enabled% = 1
                message$ = "Enter The G/L Accounts Payable Account Number~
        ~.  Blank To Search For Account."
                return
L20440:     REM DEFAULT/ENABLE FOR PURCHASE ACCOUNT
                if puracct$ <> " " then L20510
                get #1, using L20470 , puracct$
L20470:                 FMT XX(249), CH(9)
                if puracct$ = " " then puracct$ = sysacct$(1)
                call "DESCRIBE" (#6, puracct$, puracctdescr$, 1%, f1%(6))
                call "GLFMT" (puracct$)
L20510:         enabled% = 1
                message$ = "Enter The G/L Purchases Account Number."
                return
L20540:     REM DEFAULT/ENABLE FOR AMOUNT
                enabled% = 1
                call "SPCSMASH" (amount$)
                message$ = "Enter The Amount To Adjust Vendors Balance By~
        ~. Use Negative To Decrease Balance."
                return
L20600:     REM DEFAULT/ENABLE FOR PAYDATE
                if paydate$ = " " or paydate$ = blankdate$ ~
                                then paydate$ = invdate$
                enabled% = 1
                return
L20640:     REM DEFAULT/ENABLE FOR FREE TEXT
                enabled% = 1
                message$ = "Anything You Want Here."
                return
L20680:     REM DEFAULT ENABLE FOR 1099 CATEGORY
                if ten99$ <> " " then L20750
                get #1, using L20710 , ten99$
L20710:             FMT XX(514), CH(4)
                if ten99$ = " " then L20750
                   ten99key$ = "1099 CATS" & ten99$
                call "DESCRIBE" (#7, ten99key$, ten99descr$, 0%, f1%(7))
L20750:         if ten99$ <> " " then enabled% = 1
                message$ = "Enter The 1099 Category Of This Invoice."
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
            *     L O A D   O L D   I N V O I C E   O F F   F I L E     *~
            *                                                           *~
            *    PLOWS THROUGH BUFFER AND THEN THROUGH PAYABLES MASTER  *~
            * FILE LOOKING FOR THE GIVEN INVOICE.  IF ON FILE, LOAD AND *~
            * EDIT, OTHERWISE, JUST MAKE NEW INVOICE.                   *~
            *************************************************************

            REM SET OLDREADKEY, WHICH IS USED IN EITHER CASE.
                oldinvoiceonfile%, maxlines%, amount = 0
                readkey$ = vendor$
                str(readkey$, 10) = docid$

            REM FIRST TRY TO READ HEADER RECORD FROM BUFFER.
                file% = 3
                call "REDALT0" (#2, readkey$, 1%, f1%(2))
                   if f1%(2) = 0 then L30115
                call "SHOSTAT" ("Loading ADJUSTMENT from Buffer File")
                get #2, using L30490, invdate$, puracct$, payacct$,       ~
                        payaccttype$, paydate$, origdate$, origuserid$,  ~
                        freetext$, ten99$, invtype$, text$
                goto L30160               /* FORMAT, LOAD LINE ITEMS.   */

L30115:     REM IF NOT IN BUFFER TRY MAIN FILE.  IF NOT THERE, RETURN.
                file% = 5      /* FOR READING LINE ITEMS.              */
                call "READ100" (#4, readkey$, f1%(4))
                   if f1%(4) = 0 then return
                call "SHOSTAT"("Loading ADJUSTMENT From Payables Ledger")
                get #4, using L30315, invdate$, puracct$, payacct$,       ~
                        payaccttype$, paydate$, origdate$, origuserid$,  ~
                        freetext$, ten99$, invtype$, text$

L30160:     REM FORMAT ALL ITEMS IN HEADER FOR INPUT STUFF.
                if invtype$ <> "A" then L30285
                oldinvoiceonfile% = 1
                call "TXTFUTIL" (#10, 0%, "LOAD", text$)
                call "DESCRIBE" (#6, puracct$, puracctdescr$, 1%, f1%(6))
                   call "GLFMT" (puracct$)
                call "DESCRIBE" (#6, payacct$, payacctdescr$, 1%, f1%(6))
                   call "GLFMT" (payacct$)
                call "DATEFMT"  (invdate$)
                call "DATEFMT"  (paydate$)
            if ten99$ = " " then L30230
                ten99key$ = "1099 CATS" & ten99$
                call "DESCRIBE" (#7, ten99key$, ten99descr$, 0%, f1%(7))

L30230:     REM LOAD AND FORMAT INFORMATION IN LINE ITEMS.
                readkey$ = vendor$
                str(readkey$,10) = docid$

                call "PLOWNEXT" (#file%, readkey$, 25%, f1%(file%))
                     if f1%(file%) = 0 then L30270
                maxlines% = 1
                get #file%, using L30435 , amount
L30270:         call "CONVERT" (amount, -2.2, amount$)
                return

L30285:     errormsg$ = "Sorry, This Can't Be Used As An Adjustment Id: "~
                                                                 & docid$
            amount$, payacct$, puracct$, paydate$, invdate$, freetext$,  ~
            payacctdescr$, puracctdescr$, ten99$, text$, ten99descr$= " "
            return

L30315:     FMT XX(9),                   /* VENDOR CODE                */~
                XX(16),                  /* INVOICE NUMBER             */~
                XX(16),                  /* PURCHASE ORDER NUMBER      */~
                CH(6),                   /* INVOICE DATE               */~
                CH(9),                   /* PURCHASES ACCOUNT          */~
                CH(9),                   /* PAYABLES ACCOUNT           */~
                CH(1),                   /* PAYABLES ACCOUNT TYPE (A,E)*/~
                CH(6),                   /* PAY W/O DISCOUNT DATE      */~
                XX(6),                   /* PAY W/DISCOUNT DATE        */~
                XX(8),                   /* NON-DISCOUNTABLE AMOUNT    */~
                XX(6),                   /* LAST POST DATE             */~
                CH(6),                   /* ORIGINAL INPUT DATE        */~
                CH(3),                   /* ORIGINAL INPUT BY          */~
                XX(9),                   /* LAST INFO                  */~
                XX(8),                   /* ORIGINAL AMOUNT            */~
                XX(8),                   /* OPEN AMOUNT                */~
                CH(20),                  /* FREE TEXT FIELD            */~
                XX(16),                  /* SKIP DISCOUNT STUFF        */~
                CH(4),                   /* 1099 CATEGORY              */~
                XX(1),                   /* HOLD STATUS                */~
                CH(1),                   /* INVOICE TYPE (INTERNAL)    */~
                CH(4)                    /* TEXT ID                    */


L30435:     FMT XX(16),                  /* Receiver Number            */~
                XX(16),                  /* Purchase Order Number      */~
                XX(3),                   /* Purchase Order Line Number */~
                XX(9),                   /* VENDOR CODE                */~
                XX(16),                  /* INVOICE NUMBER             */~
                XX(3),                   /* SEQUENCE NUMBER            */~
                XX(9),                   /* PURCHASEACCOUNT NUMBER     */~
                XX(25),                  /* PART NUMBER                */~
                XX(8),                   /* QUANTITY                   */~
                PD(14,4)                 /* EXTENSION                  */

L30490:     FMT XX(10),                  /* BUFFER KEY                 */~
                XX(9),                   /* VENDOR CODE                */~
                XX(16),                  /* INVOICE NUMBER             */~
                XX(16),                  /* PURCHASE ORDER NUMBER      */~
                CH(6),                   /* INVOICE DATE               */~
                CH(9),                   /* PURCHASES ACCOUNT          */~
                CH(9),                   /* PAYABLES ACCOUNT           */~
                CH(1),                   /* PAYABLES ACCOUNT TYPE (A,E)*/~
                CH(6),                   /* PAY W/O DISCOUNT DATE      */~
                XX(6),                   /* PAY W/DISCOUNT DATE        */~
                XX(8),                   /* NON-DISCOUNTABLE AMOUNT    */~
                XX(6),                   /* LAST POST DATE             */~
                CH(6),                   /* ORIGINAL INPUT DATE        */~
                CH(3),                   /* ORIGINAL INPUT BY          */~
                XX(9),                   /* LAST INFO                  */~
                XX(8),                   /* ORIGINAL AMOUNT            */~
                XX(8),                   /* OPEN AMOUNT                */~
                CH(20),                  /* FREE TEXT FIELD            */~
                XX(16),                  /* SKIP DISCOUNT STUFF        */~
                CH(4),                   /* 1099 CATEGORY              */~
                XX(1),                   /* HOLD STATUS                */~
                CH(1),                   /* INVOICE TYPE (INTERNAL)    */~
                CH(4)                    /* TEXT ID                    */

L31000: REM *************************************************************~
            *       W R I T E   I N V O I C E   T O   B U F F E R       *~
            *                                                           *~
            * TOSSES THE CURRENT INVOICE TO THE BUFFER.  THE DATASAVE   *~
            * ROUTINE DELETED THE OLD INVOICE FROM THE BUFFER SO THAT   *~
            * WE CAN NOW SAVE THE NEW ONE.                              *~
            *************************************************************

            REM WRITE LINE ITEM INFORMATION TO FILE.
                call "SHOSTAT" ("Saving ADJUSTMENT In Buffer File")
                convert amount$ to amount, data goto L31120
L31120:         if amount = 0 then L31210
                call "GLUNFMT" (puracct$)
                call "GLUNFMT" (payacct$)
                mat cost = zer : cost(1) = amount
                call "PACKZERO" (cost(), cost$)
                    write #03, using L31700, " ", " ", " ",               ~
                               vendor$, docid$, "001", puracct$,         ~
                               freetext$, 1, amount, " ", " ", " ",      ~
                               amount, " ", " ", amount, cost$, " ", " "
                     goto L31270

L31210:         REM If Deleted Document Isn't In Master, Save Nothing...
                call "TXTFUTIL" (#10, 0%, "XOUT", text$)
                readkey$ = str(vendor$) & str(docid$)
                call "READ100" (#4, readkey$, f1%(4))
                     if f1%(4) = 0 then L31390

L31270:     REM WRITE HEADER INFORMATION TO FILE
                call "DATUNFMT" (invdate$)
                call "DATUNFMT" (paydate$)
                if origdate$ = " " or origdate$ = blankdate$ ~
                                 then origdate$ = date
                if origuserid$ = " " then origuserid$ = userid$

                call "GETDTTM" addr (datetime$)
                write #2, using L31420, userid$, datetime$, vendor$,      ~
                          docid$, " ", invdate$, puracct$, payacct$,     ~
                          payaccttype$, paydate$, " ", 0, " ", origdate$,~
                          origuserid$, date, userid$, amount, 0,         ~
                          freetext$, 0, 0, ten99$, "N", "A", text$, " "
L31390:         call "TXTFUTIL" (#10, 0%, "SAVE", text$)
                return

L31420:     FMT CH(3),                   /* USERID                     */~
                CH(7),                   /* DATE TIME STAMP            */~
                CH(9),                   /* VENDOR CODE                */~
                CH(16),                  /* INVOICE NUMBER             */~
                CH(16),                  /* PURCHASE ORDER NUMBER      */~
                CH(6),                   /* INVOICE DATE               */~
                CH(9),                   /* PURCHASES ACCOUNT          */~
                CH(9),                   /* PAYABLES ACCOUNT           */~
                CH(1),                   /* PAYABLES ACCOUNT TYPE      */~
                CH(6),                   /* PAY W/O DISCOUNT DATE      */~
                CH(6),                   /* PAY W/DISCOUNT DATE        */~
                PD(14,4),                /* NON-DISCOUNTABLE AMOUNT    */~
                CH(6),                   /* LAST POSTING DATE          */~
                CH(6),                   /* ORIGINAL INPUT DATE        */~
                CH(3),                   /* ORIGINAL INPUT BY          */~
                CH(6),                   /* LAST MOD DATE              */~
                CH(3),                   /* LAST MOD BY                */~
                PD(14,4),                /* INVOICE AMOUNT             */~
                PD(14,4),                /* INVOICE OPEN AMOUNT        */~
                CH(20),                  /* FREE TEXT FIELD            */~
                PD(14,4),                /* DISC PERCENT               */~
                PD(14,4),                /* DISCOUNT AMOUNT            */~
                CH(4),                   /* 1099 CATEGORY              */~
                CH(1),                   /* HOLD FLAG                  */~
                CH(1),                   /* INVOICE TYPE (INTERNAL)    */~
                CH(4),                   /* TEXT ID                    */~
                CH(168)                  /* FILLER                     */

L31700:     FMT CH(16),                  /* Receiver Number            */~
                CH(16),                  /* Purchase Order Number      */~
                CH(3),                   /* Purchase Order Line Number */~
                CH(9),                   /* VENDOR CODE                */~
                CH(16),                  /* INVOICE NUMBER             */~
                CH(3),                   /* SEQUENCE NUMBER            */~
                CH(9),                   /* PURCHASEACCOUNT NUMBER     */~
                CH(25),                  /* PART NUMBER                */~
                PD(14,4),                /* QUANTITY                   */~
                PD(14,4),                /* EXTENSION                  */~
                CH(8),                   /* JOB NUMBER                 */~
                CH(03),                  /* STORE NUMBER               */~
                CH(06),                  /* LOT NUMBER                 */~
                PD(14,7),                /* PRICE                      */~
                CH(03),                  /* OLDSEQUENCE NUMBER         */~
                CH(41),                  /* Units, UOM, Vdr part, Seq# */~
                PD(14,4),                /* Total Inventory cost       */~
                CH(96),                  /* Inventory cost array       */~
                CH(9),                   /* Price cost variance acct # */~
                CH(246)                  /* FILLER                     */

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                init(hex(8c)) lfac$()
                pfdescr$(1) = "(1)Start Over     (4)Previous Field       ~
        ~                     (13)Instructions"
                pfdescr$(2) = "                                          ~
        ~                     (15)Print Screen"
                pfdescr$(3) = "                                          ~
        ~                     (16)Exit Program"
                pfkeys$ = hex(0001040d0f10)

                REM Get appropriate fields...
                header$ = " "
                if fieldnr% > 1% then L40210
                str(pfdescr$(1),19,19) = " "
                str(pfkeys$,3,2) = hex(ffff)
L40210:         if fieldnr% > 2% then L40250
                if lastvendor$ <> " " then header$ = "Last Vendor: " &   ~
                     lastvendor$ & "  " & "Last Doc Id: " & lastinvoice$
                goto L40450
L40250:         pfdescr$(3) = " "
                str(pfkeys$,6,1) = hex(ff)
                goto L40450

            deffn'111(fieldnr%)
                REM Editmode logic...
                pfdescr$(1) = "(1)Start Over                             ~
        ~                     (13)Instructions"
                pfdescr$(2) = "                                          ~
        ~                     (15)Print Screen"
                pfdescr$(3) = "(25)Free Text                             ~
        ~                     (16)Save Data"
                pfkeys$ = hex(00010d0f1019)
                init(hex(8c)) lfac$()
                init(hex(86)) str(lfac$(),3)
                if fieldnr% = 0% then L40450
                     pfdescr$(3) = " "
                     str(pfkeys$,5) = all(hex(ff))
                     init(hex(8c)) lfac$()

L40450:           str(pfdescr$(3),63,1) = hex(84)
                  str(header$,62) = "PAYADJIN: " & cms2v$
                  on fieldnr% gosub L40610,         /* VENDOR NUMBER    */~
                                    L40610,         /* DOCUMENT ID.     */~
                                    L40610,         /* DOCUMENT DATE    */~
                                    L40610,         /* PAYABLES ACCOUNT */~
                                    L40610,         /* PURCHASE ACCOUNT */~
                                    L40640,         /* AMOUNT           */~
                                    L40610,         /* PAY DATE         */~
                                    L40610,         /* FREE TEXT        */~
                                    L40610          /* 1099 CATEGORY    */
                     goto L40680

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40610:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L40640:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40680:     accept                                                       ~
               at (01,02), "Manage Vendor Balance Adjustments",          ~
               at (01,38), "Post Date: XXXXXXXX  Today's Date:",         ~
               at (01,49), fac(hex(8c)),   postdate$            , ch(08),~
               at (01,73), fac(hex(8c)),   date$                , ch(08),~
               at (02,02), fac(hex(ac)),   header$              , ch(79),~
               at (04,02), fac(hex(94)),   errormsg$            , ch(79),~
                                                                         ~
               at (06,02), "Vendor Code",                                ~
               at (06,30), fac(lfac$(1)), vendor$               , ch(09),~
               at (06,49), fac(hex(8c)),  vendescr$             , ch(32),~
               at (07,02), "Document Id. Number",                        ~
               at (07,30), fac(lfac$(2)), docid$                , ch(16),~
                                                                         ~
               at (09,02), "Document Date",                              ~
               at (09,30), fac(lfac$(3)), invdate$              , ch(08),~
               at (10,02), "Payables Account",                           ~
               at (10,30), fac(lfac$(4)), payacct$              , ch(12),~
               at (10,45), fac(hex(8c)),  payacctdescr$         , ch(30),~
               at (11,02), "Purchase Account",                           ~
               at (11,30), fac(lfac$(5)), puracct$              , ch(12),~
               at (11,45), fac(hex(8c)),  puracctdescr$         , ch(30),~
               at (12,02), "Amount",                                     ~
               at (12,30), fac(lfac$(6)), amount$               , ch(10),~
               at (13,02), "Effective Date",                             ~
               at (13,30), fac(lfac$(7)), paydate$              , ch(08),~
               at (14,02), "Free Text Field",                            ~
               at (14,30), fac(lfac$(8)), freetext$             , ch(20),~
               at (15,02), "1099 Category",                              ~
               at (15,30), fac(lfac$(9)), ten99$                , ch(04),~
               at (15,49), fac(hex(8c)),  ten99descr$           , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   message$             , ch(79),~
               at (22,02), fac(hex(8c)),   pfdescr$(1)          , ch(79),~
               at (23,02), fac(hex(8c)),   pfdescr$(2)          , ch(79),~
               at (24,02), fac(hex(8c)),   pfdescr$(3)          , ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 13 then L41120
                  call "MANUAL" ("PAYADJIN")
                  goto L40680

L41120:        if keyhit% <> 15 then L41160
                  call "PRNTSCRN"
                  goto L40680

L41160:        if editmode% = 0 then return
               close ws
               call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50180,         /* VENDOR NUMBER    */~
                                    L50240,         /* DOCUMENT ID.     */~
                                    L50410,         /* DOCUMENT DATE    */~
                                    L50440,         /* PAYABLES ACCOUNT */~
                                    L50520,         /* PURCHASE ACCOUNT */~
                                    L50570,         /* AMOUNT           */~
                                    L50600,         /* PAY DATE         */~
                                    L50630,         /* FREE TEXT        */~
                                    L50670          /* 1099 CATEGORY    */
                     return
L50180:     REM TEST FOR VENDOR CODE ON FILE.
                call "GETCODE" (#1, vendor$, vendescr$, 1%, 1.3, f1%(1))
                     if f1%(1) = 0 then L50220
                     return
L50220:         errormsg$ = "Vendor Not On File: " & vendor$
                return
L50240:     REM TEST FOR VENDOR CODE/INVOICE ON FILE
                if docid$ <> " " then L50360
                readkey$ = vendor$
                errormsg$ = hex(06) & "Select Invoice For Edit?"
                header$() = " "
                inc(1) =  168.01 : inc$(1) = "A"
                call "PLOWCODE" (#4, readkey$, errormsg$, 5009%,         ~
                               0, f1%(4), header$(), 0, 0, inc(), inc$())
                     if f1%(4) <> 0 then L50350
                     errormsg$ = hex(00)
                     return
L50350:         docid$ = str(readkey$,10)
L50360:         errormsg$ = " "
                gosub L30000
                if oldinvoiceonfile% = 0 or errormsg$ <> " " then return
                   return clear all
                   goto editmode
L50410:     REM TEST FOR VALID INVOICE DATE
                call "DATEOK" (invdate$, temp%, errormsg$)
                return
L50440:     REM TEST FOR VALID PAYABLES ACCOUNT
                call "GETCODE" (#6,payacct$,payacctdescr$, 0%,0,f1%(6))
                call "PUTPAREN" (payacctdescr$)
                     if f1%(6) = 0 then L50500
                        get #6, using L50480 , payaccttype$
L50480:                         FMT XX(39), CH(1)
                        return
L50500:         errormsg$ = "Payables Account Not On File: " & payacct$
                     return
L50520:     REM TEST FOR VALID PURCHASES ACCOUNT DEFAULT
                call "GETCODE" (#6,puracct$,puracctdescr$,0%,0,f1%(6))
                call "PUTPAREN" (puracctdescr$)
                if f1%(6) <> 0 then return
                   errormsg$="Purchases Account Not On File: " & puracct$
                   return
L50570:     REM TEST FOR VALID AMOUNT
                call "NUMTEST" (amount$,-9e7,9e7,errormsg$,2.2,amount)
                return
L50600:     REM TEST FOR VALID PAY DATE
                call "DATEOK" (paydate$, temp%, errormsg$)
                return
L50630:     REM TEST FREE TEXT FIELD
                REM ACTUALLY, THERE'S NOTHING TO TEST...
                return

L50670:     REM 1099 CATEGORY VALIDATION
                ten99descr$ = " "
                if ten99$ = " " then return
                   ten99key$ = "1099 CATS" & ten99$
                   ten99descr$ = hex(06) & "Select 1099 Category"
            call "PLOWCODE" (#7, ten99key$, ten99descr$, 9%, 0.30, f1%(7))
               if f1%(7) = 1% then L50760
                  errormsg$ = "Invalid 1099 Category Code"
                  return
L50760:        ten99$ = str(ten99key$,10)
               return

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
            readkey$ = all(hex(00))
            str(readkey$,,3) = userid$
            call "PLOWNEXT" (#2, readkey$, 3%, f1%(2))
            end f1%(2)
