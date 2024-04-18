         rem*************************************************************~
            *                                                           *~
            *  pppp    aaa   y   y  iiiii  n   n  dddd     a    ttttt   *~
            *  p   p  a   a  y   y    i    nn  n  d   d   a a     t     *~
            *  pppp   aaaaa   yyy     i    n n n  d   d  aaaaa    t     *~
            *  p      a   a    y      i    n  nn  d   d  a   a    t     *~
            *  p      a   a    y    iiiii  n   n  dddd   a   a    t     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * payindat - allows edit of invoice dates and discount      *~
            *            percent only.  intended for users responsible  *~
            *            for generating checks.                         *~
            *-----------------------------------------------------------*~
            *                  m o d i f i c a t i o n s                *~
            *---when---+------------------what--------------------+-who-*~
            * 06/21/86 ! original                                 ! hes *~
            * 10/21/86 ! corrected write of current invoice amount! rac *~
            * 05/18/87 ! paylines record length mod- standard cost! jim *~
            * 12/07/92 ! prr 12385 - now writes last user id and  ! mlj *~
            *          !  modified date.                          !     *~
            * 08/15/96 ! Changes for the year 2000.               ! DXL *~
            *************************************************************

        dim                                                              ~
            blankdate$,                  /* Blank Date for Comparison  */~
            cursor%(2),                  /* CURSOR LOACATIONS FOR EDIT */~
            date$8,                      /* SCREEN DATE STRING STUFF   */~
            date1$8,                     /* DATE FOR PAY DATE COMPUTING*/~
            date2$8,                     /* ANOTHER DATE...            */~
            disc_amt$10,                 /* DISCOUNT AMOUNT            */~
            disc_pct$10,                 /* DISCOUNT PERCENT           */~
            discdate$8,                  /* PAY TAKING DISCOUNT DATE   */~
            errormsg$79,                 /* ERROR MESSAGE TEXT LINE    */~
            ext(100),                    /* EXTENSIONS AND STUFF       */~
            freetext$20,                 /* FREE TEXT INFORMATION      */~
            header$(2)79,                /* Screen Title               */~
            hold$1,                      /* Invoice On Hold Flag       */~
            i$(24)80,                    /* JUNK SCREEN IMAGE (NOT USED*/~
            inc(2),                      /* For The Elusive "PLOWCODE" */~
            inc$(2)28,                   /* For The Elusive "PLOWCODE" */~
            invdate$8,                   /* SCREEN INVOICE DATE        */~
            invnet$10,                   /* INVOICE TOTAL LESS DISCOUNT*/~
            invoicenr$16,                /* INVOICE NUMBER             */~
            invtype$,                    /* INVOICE TYPE (INTERNAL)    */~
            invtotal$10,                 /* INVOICE TOTAL              */~
            lastdate$6,                  /* LAST MODIFIED DATE         */~
            lastinvoice$16,              /* LAST INVOICE NUMBER INPUT  */~
            lastuserid$3,                /* LAST MODIFIED BY USER ID   */~
            lastvendor$9,                /* LAST VENDOR PROCESSED      */~
            lfac$(20)1,                  /* LINEAR INPUT FAC'S         */~
            lot$(4)100,                  /* PAYMASTR record            */~
            message$79,                  /* INPUT MESSAGE              */~
            nondisc$10,                  /* NON-DISCOUNTABLE AMOUNT    */~
            origdate$6,                  /* DATE OF ORIGINAL INVOICE   */~
            origuserid$3,                /* USERID OF ORIGINAL INVOICE */~
            payacct$16,                  /* PAY ACCOUNT NUMBER         */~
            paydate$8,                   /* PAYABLES DATE INFORMATION  */~
            pfdescr$(3)79,               /* FUNCTION KEYS ENABLED LISTS*/~
            pfkeys$32,                   /* FUNCTION KEYS ENABLED LISTS*/~
            puracct$16,                  /* PURCHASES ACCOUNT NUMBER   */~
            readkey$90,                  /* KEY FOR PLOW ROUTINES      */~
            receiver$16,                 /* DEFAULT REVEIVER NUMBER    */~
            regulardate$8,               /* REGULAR DATE INFORMATION   */~
            sysacct$(5)9,                /* SYSTEM DEFAULT ACCOUNTS    */~
            tdate$8,                     /* Temporary Date             */~
            ten99$4,                     /* 1099 CATEGORY              */~
            text$4,                      /* Document Text Id. Number   */~
            topline$79,                  /* Floating Program Title     */~
            userid$3,                    /* USERID THIS USER           */~
            venaddr$(6)30,               /* ADDRESS THIS VENDOR        */~
            vencode$9,                   /* VENDOR CODE THIS INVOICE   */~
            vendescr$32                  /* VENDOR DESCRIPTION         */

        dim f1%(64)                      /* RECORD-ON-FILE FLAGS       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !          D E S C R I P T I O N           *~
            *-----+----------+------------------------------------------*~
            * # 1 ! USERINFO ! USER INFORMATION (PAYABLES DATE)         *~
            * # 3 ! VENDOR   ! LOAD VENDOR MASTER INFORMATION HERE      *~
            * # 4 ! HNYMASTR ! INVENTORY MASTER FILE                    *~
            * # 5 ! PAYMASTR ! PAYABLES MAIN HEADER FILE.               *~
            * # 6 ! PAYLINES ! PAYABLES LINE ITEMS FILE                 *~
            * # 7 ! SYSFILE2 ! SYSTEM INFO (DEFAULT PAY DATES)          *~
            * # 9 ! PAYBUFFR ! PAYABLES INVOICE HEADER BUFFER           *~
            * #20 ! GENCODES ! General Purpose Code File                *~
            *************************************************************

            select # 1, "USERINFO",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos = 1, keylen = 3

            select #3,  "VENDOR",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 600,                                   ~
                        keypos=1, keylen=9,                              ~
                        alt key 1, keypos = 10, keylen = 30, dup

            select #4,  "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 900,                                   ~
                        keypos = 1, keylen = 25

            select  #5, "PAYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 350,                                   ~
                        keypos = 1, keylen = 25

            select  #6, "PAYLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 541,                                   ~
                        keypos = 36, keylen = 28,                        ~
                        alternate key 1, keypos = 1, keylen = 63,        ~
                                  key 2, keypos = 17, keylen = 47

            select #7,  "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select #9,  "PAYBUFFR",      /* INVOICE HEADER BUFFER      */~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 350,                                   ~
                        keypos = 1, keylen = 10,                         ~
                        alternate key 1, keypos = 11, keylen = 25

            select #20, "GENCODES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 128,                                   ~
                        keypos = 1, keylen = 24

            call "SHOSTAT"  ("Opening Files, One Moment Please.")

            call "OPENCHCK" (#1, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#3, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#4, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#5, 0%, 0%, 100%, " ")
            call "OPENCHCK" (#6, 0%, 0%, 200%, " ")
            call "OPENCHCK" (#7, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#9, 0%, 0%, 100%, " ")
            call "OPENCHCK" (#20, 0%, 0%, 0%, " ")

        REM *************************************************************~
            *                 I N I T I A L I Z A T I O N               *~
            *                                                           *~
            * SETS USER ID, DATES, CONTROL INFO  ETC...                 *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date
            lastdate$ = date$
            call "DATEFMT" (date$)
            call "EXTRACT" addr("ID", userid$)
            lastuserid$ = userid$

            REM Get Users Posting Date...
            call "READ100" (#1, userid$, f1%(1))
                if f1%(1) = 1 then L09180
                call "ASKUSER" (keyhit%, "Sorry",                        ~
                  "You're Not Listed As A Valid User In This Data Base", ~
                                           " ", "Press (ENTER) To Exit.")
                goto L65000

L09180:     REM Get A/P System Defaults...
                call "READ100" (#7, "MODULE.DEFAULTS.AP  ", f1%(7))
                     if f1%(7) = 0 then L09240
                get #7, using L09220,sysbillsdue%, sysdiscsdue%, sysacct$()
L09220:         FMT XX(20), 2*BI(4), XX(8), 5*CH(9)

L09240:     topline$ = "Manage Vendor Invoice Dates And Discount Percent ~
        ~        Today's Date: " & date$

        REM *************************************************************~
            *     I N P U T   I N V O I C E   H E A D E R   I N F O     *~
            *                                                           *~
            * GETS INVOICE HEADER INFORMATION AND THAT SORT OF THING.   *~
            *************************************************************

        inputmode
            vencode$, invoicenr$ = " "
            gosub init_data

            for fieldnr% = 1 to 2
                gosub'161(fieldnr%)
                      if enabled% = 0 then L10220
L10130:         gosub'201(fieldnr%)
L10140:               if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then L10200
                         fieldnr% = max(1%, fieldnr%-1%)
                         gosub'161(fieldnr%)
                         if enabled% = 0 then L10140
                         goto L10130
L10200:               if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L10130
L10220:         gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10130
                next fieldnr%

        REM *************************************************************~
            *           E D I T   I N V O I C E   H E A D E R           *~
            *                                                           *~
            * EDITS INVOICE HEADERS, PERMITTING ALL OF THE FIELDS TO BE *~
            * MODIFIED.                                                 *~
            *************************************************************

        editmode

            message$ = "To Modify Displayed Values, Position Cursor To De~
        ~sired Value And Press RETURN."
            gosub'211(0%)
                 if keyhit%  =  1 then gosub startover
                 if keyhit%  = 16 then       datasave
                 if keyhit% <>  0 then       editmode
            oldfieldnr% = 0
L11210:     fieldnr% = cursor%(1) - 12
            if fieldnr% < 3% or fieldnr% > 7% then editmode
            if fieldnr% > 6% then fieldnr% = fieldnr% + 1%
            if fieldnr% = 6% and cursor%(2) > 35 then fieldnr% = 7%
            if fieldnr% = oldfieldnr% then editmode
            oldfieldnr% = fieldnr%

            gosub'161(fieldnr%)
                 if enabled% = 0 then editmode
L11300:     gosub'211(fieldnr%)
                 if keyhit%  =  1 then gosub startover
                 if keyhit% <>  0 then       L11300
            gosub'151(fieldnr%)
                 if errormsg$ <> " " then L11300
            goto L11210

        REM *************************************************************~
            *      M I S C E L L A N E O U S   S U B R O U T I N E S    *~
            *                                                           *~
            * MISCELLANEOUS SUBROUTINES.                                *~
            *************************************************************

        total_up_invoice
            invamt, disc_amt = 0
            if maxlines% = 0% then L14140
            for i% = 1% to maxlines%
                invamt = round(invamt + ext(i%),2)
            next i%
            if invamt < 0 then L14180
L14140:     convert disc_amt$ to disc_amt, data goto L14150
L14150:     convert disc_pct$ to disc_pct, data goto L14160
L14160:     if disc_pct <> 0 then disc_amt =                             ~
                       round((invamt-min(invamt,nondisc))*disc_pct/100,2)
L14180:     call "CONVERT" (disc_amt, -2.2, disc_amt$)
            call "CONVERT" (invamt, -2.2, invtotal$)
            call "CONVERT" (invamt-disc_amt, -2.2, invnet$)
        return

        REM *************************************************************~
            *          W R I T E   D A T A   T O   M A S T E R          *~
            *                                                           *~
            * SAVE DATA.                                                *~
            *************************************************************

        datasave

            REM Set Last Document Input Information And Write Invoice.
                gosub L31000
                lastvendor$ = vencode$
                lastinvoice$ = invoicenr$
                goto inputmode

        REM *************************************************************~
            *  S E T   D E F A U L T S   F O R   H E A D E R   I N F O  *~
            *                                                           *~
            * SETS DEFAULTS FOR THE FIRST PAGE OF THE HEADER--ADDRESS,  *~
            * INVOICE DATE, DATE TO PAY WITH AND WITHOUT DISCOUNTS, AND *~
            * THAT SORT OF THING.                                       *~
            *************************************************************

            deffn'161(fieldnr%)
                  enabled% = 1
                  message$ = " "
                  on fieldnr% gosub L20200,         /* VENDOR CODE      */~
                                    L20240,         /* INVOICE NUMBER   */~
                                    L20270,         /* DATE OF INVOICE  */~
                                    L20310,         /* REGULAR DATE     */~
                                    L20350,         /* DISCOUNT DATE    */~
                                    L20400,         /* DISC_PCT         */~
                                    L20470,         /* DISC_AMT         */~
                                    L20500          /* NON-DISCOUNTABLE */
                return
L20200:     REM ENABLE STUFF FOR VENDOR CODE
                message$ = "Enter Vendor Number.  Leave Blank And Press (~
        ~RETURN) To Find An Existing Vendor."
                return
L20240:     REM INPUT ENABLE FOR INVOICE NUMBER
                message$ = "Enter Vendor's Invoice Number."
                return
L20270:     REM INPUT ENABLE FOR INVOICE DATE
                message$ = "Enter Date Of Invoice."
                if invdate$ = " " or invdate$ = blankdate$ ~
                                then invdate$ = paydate$
                return
L20310:     REM INPUT ENABLE FOR REGULAR DISBURSEMENT DATE
                message$ = "Enter Date This Invoice Should Be Paid By."
                if regulardate$ = " " or regulardate$ = blankdate$ ~
                                                 then gosub L56310
                return
L20350:     REM INPUT ENABLE FOR DISCOUNT DISBURSEMENT DATE
                if discdate$ = " " or blankdate$ = discdate$ then gosub L56650
                message$ = "Enter Date This Invoice Would Have To Be Paid~
        ~ By To Receive a Discount."
                return
L20400:     REM ENABLE DISCOUNT PERCENT
                message$ = "Enter Discount Percentage, If Any (Eg. Enter ~
        ~'2' If a 2% Discount Is Possible)."
                if disc_pct = 0 then return
                if disc_pct$ <> " " then return
                call "CONVERT" (disc_pct, -2.4, disc_pct$)
                return
L20470:     REM ENABLE DISCOUNT AMOUNT
                message$ = "Enter The Total Discount For This Invoice."
                return
L20500:     REM INPUT ENABLE FOR NON-DISCOUNTABLE AMOUNT
                message$ = "Enter Portion Of Total Invoice That Is Non Di~
        ~scountable."
                return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

            startover% = 2%
            call "STARTOVR" (startover%)
            if startover% = 1% then return

            return clear all
            goto inputmode

        init_data
            vendescr$, venaddr$(), invdate$, puracct$, origdate$,        ~
            payacct$, discdate$, invtype$, errormsg$, receiver$, ten99$, ~
            regulardate$, nondisc$, freetext$, disc_amt$, disc_pct$,     ~
            hold$, origuserid$, text$ = " "

            maxlines%, disc_pct, disc_amt, invamt = 0
            mat ext = zer
        return

L30000: REM *************************************************************~
            *     L O A D   O L D   I N V O I C E   O F F   F I L E     *~
            *                                                           *~
            * LOADS INVOICE FROM MASTER FILE ONLY.                      *~
            *************************************************************

            REM Prepare To Search Data Files...
                oldinvoiceonfile%, maxlines% = 0
                readkey$ = str(vencode$) & str(invoicenr$)

            REM First Try To Read Header Record From Buffer...
                call "REDALT0" (#9, readkey$, 1%, f1%(9))
                     if f1%(9) = 0 then L30180
                errormsg$ = "Sorry, User XXX has a chenge pending for thi~
        ~s Invoice."
                get #9, str(errormsg$,13,3)
                return

L30180:     REM Try Master File.
                call "READ101" (#5, readkey$, f1%(5))
                     if f1%(5) = 0 then return
                call "SHOSTAT" ("Loading Invoice From Payables " &       ~
                                                           "Master File")
            REM Actually get And Format Data Off Header...
                get #5, str(lot$(),,350)
                invdate$ = str(lot$(), 42, 6)
                hold$ = str(lot$(), 167, 1)
                regulardate$ = str(lot$(), 67, 6)
                invtype$ = str(lot$(), 168, 1)
                discdate$ = str(lot$(), 73, 6)
                get str(lot$(), 147, 8) using L30275, disc_pct
L30275:         FMT PD(14,4)
                get str(lot$(), 155, 8) using L30275, disc_amt
                get str(lot$(),  79, 8) using L30275, nondisc
                oldinvoiceonfile% = 1

                call "DATEFMT"  (invdate$)
                call "DATEFMT"  (regulardate$)
                if discdate$ = blankdate$ then discdate$ = "00/00/00" else ~
                   call "DATEFMT" (discdate$)
                call "CONVERT" (nondisc, -2.4, nondisc$)
                if disc_pct<>0 then call"CONVERT"(disc_pct,-2.4,disc_pct$)
                call "CONVERT"(disc_amt, -2.2, disc_amt$)

            REM Load Up And Format Line Item Details...
                readkey$ = vencode$
                str(readkey$, 10) = invoicenr$
L30410:         call "PLOWNEXT" (#6, readkey$, 25%, f1%(6))
                     if f1%(6) = 0 then L30480  /* Return */
                c%, maxlines% = maxlines% + 1
                get #6, using L30450, ext(c%)
L30450:         FMT POS(106), PD(14,4)
                goto L30410

L30480:     REM Total invoice, & return...
                REM Check other possible edit restrictions...
                i$(1) = " "
                if hold$ = "Y" then i$(1) =                              ~
                           "Sorry, Invoice Is On Hold, Can't Be Changed."
                if invtype$ = "A" then i$(1) =                           ~
                              "Adjustment Invoice, Can't Edit From Here."
                if invtype$ = "R" then i$(1) =                           ~
                        "Recurring Master Invoice, Can't Edit From Here."
                if i$(1) <> " " then gosub init_data
                if i$(1) <> " " then errormsg$ = i$(1)
                gosub total_up_invoice
                return

            FMT XX(9),                   /* VENDOR CODE                */~
                XX(16),                  /* INVOICE NUMBER             */~
                CH(16),                  /* RECEIVER NUMBER            */~
                CH(6),                   /* INVOICE DATE               */~
                CH(9),                   /* PURCHASES ACCOUNT          */~
                CH(9),                   /* PAYABLES ACCOUNT           */~
                CH(1),                   /* PAYABLES ACCOUNT TYPE (A,E)*/~
                CH(6),                   /* PAY W/O DISCOUNT DATE      */~
                CH(6),                   /* PAY W/DISCOUNT DATE        */~
                PD(14,4),                /* NON-DISCOUNTABLE AMOUNT    */~
                XX(6),                   /* LAST POSTING DATE          */~
                CH(6),                   /* ORIGINAL INPUT DATE        */~
                CH(3),                   /* ORIGINAL INPUT BY          */~
                XX(25),                  /* AUDIT INFO AND TOTALS      */~
                CH(20),                  /* FREE TEXT FIELD            */~
                PD(14,4),                /* DISCOUNT PERCENT           */~
                PD(14,4),                /* DISCOUNT AMOUNT            */~
                CH(4),                   /* 1099 CATEGORY              */~
                CH(1),                   /* HOLD STATUS                */~
                CH(1),                   /* INVOICE TYPE (INTERNAL)    */~
                CH(4)                    /* TEXT ID NUMBER             */

L31000: REM *************************************************************~
            *            R E W R I T E   I N V O I C E                  *~
            *                                                           *~
            * These changes are written straight to the PAYMASTR file.  *~
            *************************************************************

            REM WRITE LINE ITEM INFORMATION TO FILE.
                call "SHOSTAT" ("Writing Invoice To Master File")

            REM WRITE HEADER INFORMATION TO FILE
                gosub total_up_invoice
                call "DATUNFMT" (invdate$)
                call "DATUNFMT" (regulardate$)
                call "DATUNFMT" (discdate$)
                if origdate$ = " " or origdate$ = blankdate$ ~
                                 then origdate$ = date
                if origuserid$ = " " then origuserid$ = userid$
                convert nondisc$ to nondisc
                if invtype$ = " " then invtype$ = "N"

                REM WRITE OUT INVOICE STRAIGHT TO MASTER...
                str(lot$(), 42, 6) = invdate$
                str(lot$(), 67, 6) = regulardate$
                str(lot$(), 73, 6) = discdate$
                str(lot$(),102, 6) = lastdate$
                str(lot$(),108, 3) = lastuserid$
                put str(lot$(), 147, 8) using L31260, disc_pct
                put str(lot$(), 155, 8) using L31260, disc_amt
                put str(lot$(),  79, 8) using L31260, nondisc
L31260:         FMT PD(14,4)

                rewrite #5 using L31290, str(lot$(),1,25),                ~
                           str(lot$(),26,200), str(lot$(),226, 125)
L31290:         FMT CH(25), CH(200), CH(125)
                return

            FMT CH(9),                   /* VENDOR CODE                */~
                CH(16),                  /* INVOICE NUMBER             */~
                CH(16),                  /* RECEIVER NUMBER            */~
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
                CH(1),                   /* HOLD STATUS                */~
                CH(1),                   /* INVOICE TYPE (INTERNAL)    */~
                CH(4),                   /* TEXT ID NUMBER             */~
                CH(168)                  /* FILLER                     */

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'201(fieldnr%)
                init(hex(8c)) lfac$()
                pfdescr$(1) = "(1)Start Over     (4)Previous Field       ~
        ~                     (13)Instructions"
                pfdescr$(2) = "                                          ~
        ~                     (15)Print Screen"
                pfdescr$(3) = "                                          ~
        ~                     (16)EXIT PROGRAM"
                pfkeys$ = hex(0001040d0f10)

                REM Turn off appropriate fields...
                header$(1) = " "
                if fieldnr% > 1% then L40270
                str(pfdescr$(1),19,19) = " "    /* Shut Off Prev Field */
                str(pfkeys$,3,2) = hex(ffff)
L40270:         if fieldnr% > 2% then L40310
                if lastvendor$ <> " " then header$(1) = "Last Vendor: " &~
                     lastvendor$ & "  " & "Last Invoice: " & lastinvoice$
                goto L40540
L40310:         pfdescr$(3) = " "               /* Shut Off Exit       */
                str(pfkeys$,6,1),str(pfkeys$,8,1) = hex(ff)
                goto L40540

            deffn'211(fieldnr%)
                REM Editmode logic...
                pfdescr$(1) = "(1)Start Over                             ~
        ~                     (13)Instructions"
                pfdescr$(2) = "                                          ~
        ~                     (15)Print Screen"
                pfdescr$(3) = "                                          ~
        ~                     (16)SAVE DATA"
                pfkeys$ = hex(00010d0f10)
                header$(1) = "Invoice Total: " & invtotal$
                str(header$(1),27) = "Disc: " & disc_amt$
                str(header$(1),44) = "Net: " & invnet$
                init(hex(8c)) lfac$()
                init(hex(86)) str(lfac$(),3)
                if fieldnr% = 0% then L40540
                     pfdescr$(3) = " "
                     str(pfkeys$,5) = all(hex(ff))
                     init(hex(8c)) lfac$()

L40540:           str(pfdescr$(3),63,1) = hex(84)
                  str(header$(1),62) = "PAYINDAT" & ": " & cms2v$
                  on fieldnr% gosub L40690,         /* Vendor Code      */~
                                    L40690,         /* Invoice Number   */~
                                    L40690,         /* Invoice Date     */~
                                    L40690,         /* Regular Date     */~
                                    L40690,         /* Discount Date    */~
                                    L40690,         /* Discount Percent */~
                                    L40690,         /* Discount Amount  */~
                                    L40690          /* Non Disc Amount  */
                     goto L40760

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40690:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40760:     accept                                                       ~
               at (01,02), fac(hex(8c)),   topline$             , ch(79),~
               at (02,02), fac(hex(ac)),   header$(1)           , ch(79),~
               at (04,02), fac(hex(94)),   errormsg$            , ch(79),~
                                                                         ~
               at (06,02), "Vendor Code",                                ~
               at (06,30), fac(lfac$(1)), vencode$              , ch(09),~
               at (06,49), fac(hex(8c)),  vendescr$             , ch(32),~
               at (07,02), "Invoice Number",                             ~
               at (07,30), fac(lfac$(2)), invoicenr$            , ch(16),~
                                                                         ~
               at (09,02), "Vendor Name",                                ~
               at (09,30), fac(hex(8c)),  venaddr$(1)           , ch(30),~
               at (10,02), "   Address (Line 1)",                        ~
               at (10,30), fac(hex(8c)),  venaddr$(2)           , ch(30),~
               at (11,02), "           (Line 2)",                        ~
               at (11,30), fac(hex(8c)),  venaddr$(3)           , ch(30),~
               at (12,02), "           (Line 3)",                        ~
               at (12,30), fac(hex(8c)),  venaddr$(4)           , ch(30),~
               at (13,02), "           (Line 4)",                        ~
               at (13,30), fac(hex(8c)),  venaddr$(5)           , ch(30),~
               at (14,02), "   City, State, Zip",                        ~
               at (14,30), fac(hex(8c)),  venaddr$(6)           , ch(30),~
               at (15,02), "Invoice Date",                               ~
               at (15,30), fac(lfac$(3)),  invdate$             , ch(08),~
               at (16,02), "Regular Disbursement Date",                  ~
               at (16,30), fac(lfac$(4)),  regulardate$         , ch(08),~
               at (17,02), "Discount Disbursement Date",                 ~
               at (17,30), fac(lfac$(5)),  discdate$            , ch(08),~
               at (18,02), "Discount Percent",                           ~
               at (18,30), fac(lfac$(6)),  disc_pct$            , ch(10),~
               at (18,41), "And Amount",                                 ~
               at (18,52), fac(lfac$(7)),  disc_amt$            , ch(10),~
               at (19,02), "Non-Discountable Amount",                    ~
               at (19,30), fac(lfac$(8)),  nondisc$             , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   message$             , ch(79),~
               at (22,02), fac(hex(8c)),   pfdescr$(1)          , ch(79),~
               at (23,02), fac(hex(8c)),   pfdescr$(2)          , ch(79),~
               at (24,02), fac(hex(8c)),   pfdescr$(3)          , ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 13 then L41370
                  call "MANUAL" ("PAYINDAT")
                  goto L40760

L41370:        if keyhit% <> 15 then L41410
                  call "PRNTSCRN"
                  goto L40760

L41410:        close ws
               call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *         T E S T   D A T A   F R O M   H E A D E R         *~
            *                                                           *~
            * TESTS ALL THE DATA ON THE PAYABLES INVOICE HEADER.        *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50180,         /* VENDOR CODE      */~
                                    L50310,         /* INVOICE NUMBER   */~
                                    L50560,         /* DATE OF INVOICE  */~
                                    L50650,         /* PAY W/O DISC DATE*/~
                                    L50700,         /* PAY W/DISC DATE  */~
                                    L50760,         /* DISC PCT         */~
                                    L50840,         /* DISC AMOUNT      */~
                                    L50920          /* NON-DISCOUNTABLE */
                  return

L50180:     REM TEST FOR VENDOR CODE ON FILE.
                vendescr$ = hex(0684) & "Select Vendor"
                call "GETCODE" (#3, vencode$, vendescr$, 1%, 1.3, f1%(3))
                     if f1%(3) = 0 then L50290
                REM GET ADDRESS & STUFF, JUST FOR FUN.
                    get #3, using L50240, venaddr$()
L50240:             FMT XX(39), 6*CH(30)
                    get #3, using L50260, billsdue%, discsdue%, discpercent
L50260:                     FMT XX(285), 3*PD(14,4)
                     disc_pct = discpercent /* DEFAULT VALUE           */
                    return
L50290:         errormsg$ = "Vendor Not On File: " & vencode$
                return
L50310:     REM TEST FOR VENDOR CODE/INVOICE ON FILE
                readkey$ = str(vencode$) & invoicenr$
                errormsg$ = hex(06) & "Select Invoice For Edit?"
                header$(), inc$() = " "
                mat inc = zer
                inc(2) =  168.01 : inc$(2) = "N"
                call "PLOWCODE" (#5, readkey$, errormsg$, 5009%,         ~
                               0, f1%(5), header$(), 0, 0, inc(), inc$())
                     if f1%(5) <> 0 then L50450
L50430:              errormsg$ = "Sorry, Invoice Not On File"
                     return
L50450:         invoicenr$ = str(readkey$,10)
                errormsg$ = " "
                gosub L30000
                if errormsg$ <> " " then return
                if oldinvoiceonfile% = 0 then L50430
                   return clear all
                   goto editmode
L50560:     REM TEST FOR VALID INVOICE DATE
                if invdate$ <> " " and invdate$ <> blankdate$ then L50600
                   invdate$ = paydate$
L50600:         call "DATEOK" (invdate$, temp%, errormsg$)
                     if errormsg$ <> " " then return
                        gosub L56000                /* REGULAR DATE JUNK*/
                        gosub L56650                /* DISCOUNT DATE    */
                     return
L50650:     REM TEST FOR VALID "DISBURSEMENT WITHOUT DISCOUNT" DATE
                if regulardate$ = " " or regulardate$ = blankdate$ ~
                                                 then gosub L56310
                call "DATEOK" (regulardate$, temp%, errormsg$)
                     return
L50700:     REM TEST FOR VALID DISBURSEMENT WITH DISCOUNT DATE
                if discdate$ = " " or discdate$ = blankdate$ then gosub L56650
                if discdate$ = "00/00/00" then return
                call "DATEOK" (discdate$, temp%, errormsg$)
                     return
L50760:     REM TEST FOR VALID DISC PERCENT
                disc_pct  = 0
                if disc_pct$ = " " then return
                call"NUMTEST"(disc_pct$,-9e7,1000,errormsg$,2.4,disc_pct)
                disc_amt = round((invamt-nondisc)*disc_pct/100,2)
                call "CONVERT" (disc_amt, -2.2, disc_amt$)
                gosub total_up_invoice
                return
L50840:     REM TEST FOR VALID DISC AMOUNT
                call "NUMTEST"(disc_amt$,-9e7,9e7,errormsg$,2.2,disc_amt)
                     if errormsg$ <> " " then return
                if disc_amt = round((invamt-min(invamt,nondisc)) *       ~
                                             disc_pct/100,2) then L50900
                     disc_pct = 0 : disc_pct$ = " "
L50900:         gosub total_up_invoice
                return
L50920:     REM TEST FOR VALID NON-DISCOUNTABLE AMOUNT
                call "NUMTEST" (nondisc$, 0,9e7, errormsg$, 2.2, nondisc)
                gosub total_up_invoice
                return

L56000: REM *************************************************************~
            * D I S B U R S E M E N T   D A T E   C O M P U T A T I O N *~
            *                                                           *~
            * THESE TWO SUBROUTINES FIGURE OUT THE TWO DISBURSEMENT     *~
            * DATES FOR THE INVOICE USING THE VARIOUS DEFAULTS.         *~
            *-----------------------------------------------------------*~
            * FOR THE BILLS DUE WITHOUT DISCOUNT FIELD, THERE ARE 3 WAYS*~
            *     1.) IF THE VENDOR'S BILLS DUE FIELD = 0 THEN WE SET   *~
            *         THE DATE TO THE INVOICE DATE + THE SYSTEM DEFAULT *~
            *         BILLS DUE (DAYS) PARAMETER. (THIS IS COMMON...)   *~
            *     2.) IF THE VENDOR'S BILLS DUE FIELD > 0 THEN WE SET   *~
            *         THE DATE TO THE INVOICE DATE + THE VENDOR'S       *~
            *         DEFAULT BILLS DUE (DAYS) FIELD.                   *~
            *     3.) IF THE VENDOR'S BILLS DUE FIELD < 0 THEN WE SET   *~
            *         THE DATE ON THE PROX SYSTEM--DUE MONTH EQUAL TO   *~
            *         MONTH OF INVOICE + 1, CARRYING THE YEAR IF        *~
            *         APPROPRIATE.  THE PROXX DAY IS ALWAYS THE ABSOLUTE*~
            *         VALUE OF THE NUMBER RECALLED FROM THE VENDOR'S    *~
            *         BILLS DUE FIELD.                                  *~
            *-----------------------------------------------------------*~
            * FOR THE BILLS DUE WITH DISCOUNTS FIELD, THERE ARE 3 CASES *~
            *     1.) IF THE DISCOUNTS DUE FIELD = 0 THEN THE DISCOUNT  *~
            *         DATE FOR THE INVOICE GETS A NULL VALUE.           *~
            *     2.) IF THE DISCOUNTS DUE FIELD > 0 THEN THE DISCOUNT  *~
            *         DATE GETS THE INVOICE DATE + THE VENDOR'S DISCOUNT*~
            *         DUE FIELD.                                        *~
            *     3.) IF THE DISCOUNTS DUE FIELD < 0 THEN THE DISCOUNT  *~
            *         DATE IS COMPUTED ON THE "PROX" SYSTEM.  THE       *~
            *         CALCULATION IS AS IN (3) ABOVE.                   *~
            *************************************************************

L56310:     REM ROUTINE THAT COMPUTES REGULARDATE$
                date1$ = invdate$
                call "DATUNFMT" (date1$)
                date2$ = date1$

                on sgn (billsdue%)+2 gosub L56510, L56410, L56470
                   regulardate$ = date2$
                   call "DATEFMT" (regulardate$)
                   return

L56410:         REM CASE 1--IF BILLS DUE = 0 THEN USE SYSTEM DEFAULT
                    if sysbillsdue% = 0% then return
                       billsdue% = sysbillsdue%
                       on sgn (billsdue%)+2 goto L56510,,L56470
                       return

L56470:         REM CASE 2--IF BILLS DUE > 0 THEN USE INV DATE + DAYS
                    call "DATE" addr("G+",date1$,billsdue%,date2$,err%)
                    return

L56510:         REM CASE 3--IF BILLS DUE < 0 THEN SEE EXPLANATION ABOVE
                    tdate$ = date1$
                    call "DATEFMT" (tdate$, 0%, date1$)
                    convert str(date1$,5%,2%) to month%
                    convert str(date1$,1%,4%) to year%
                    month% = month% + 1%
                    if month% < 13% then L56580      /* CARRY YEAR       */
                       year% = year% + 1%
                       month% = 1%
L56580:             day% = abs(billsdue%)
                    REM CONVERT DATES BACK TO REGULAR DATE FORMAT.
                        convert year%  to str(date2$,1%,4%), pic(0000)
                        convert month% to str(date2$,5%,2%), pic(00)
                        convert day%   to str(date2$,7%,2%), pic(00)
                        call "DATECONV" (date1$)
                        call "DATECONV" (date2$)
                           return

L56650:     REM ROUTINE THAT COMPUTES DISCDATE$
                date1$ = invdate$
                call "DATUNFMT" (date1$)
                date2$ = "00/00/00"
                if discpercent = 0 then L56720

                   on sgn (discsdue%)+2 gosub L56860, L56760, L56820
L56720:               discdate$ = date2$
                      if date2$ <> "00/00/00" then call "DATEFMT" (discdate$)
                         return

L56760:         REM CASE 1--IF BILLS DUE = 0 THEN USE SYSTEM DEFAULT
                    if sysdiscsdue% = 0% then return
                       discsdue% = sysdiscsdue%
                       on sgn (discsdue%)+2 goto L56860,,L56820
                       return

L56820:         REM CASE 2--IF DISCOUNTS DUE > 0 THEN USE INV DATE + DAYS
                    call "DATE" addr("G+",date1$,discsdue%,date2$,err%)
                    return

L56860:         REM CASE 3--IF DISCOUNTS DUE < 0 THEN SEE EXPLANATION
                    tdate$ = date1$
                    call "DATEFMT"( tdate$, 0%, date1$)
                    convert str(date1$,5%,2%) to month%
                    convert str(date1$,1%,4%) to year%
                    month% = month% + 1%
                    if month% < 13% then L56930      /* CARRY YEAR       */
                       year% = year% + 1%
                       month% = 1%
L56930:             day% = abs(discsdue%)
                    REM CONVERT DATES BACK TO REGULAR DATE FORMAT.
                        convert year%  to str(date2$,1%,4%), pic(0000)
                        convert month% to str(date2$,5%,2%), pic(00)
                        convert day%   to str(date2$,7%,2%), pic(00)
                        call "DATECONV" (date1$)
                        call "DATECONV" (date2$)
                           return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM. SET RETURN CODE FOR DOCUMENTS IN BUFFER.    *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")
            end
