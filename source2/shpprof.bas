        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   SSS   H   H  PPPP   PPPP   RRRR    OOO   FFFFF          *~
            *  S      H   H  P   P  P   P  R   R  O   O  F              *~
            *   SSS   HHHHH  PPPP   PPPP   RRRR   O   O  FFFF           *~
            *      S  H   H  P      P      R   R  O   O  F              *~
            *   SSS   H   H  P      P      R   R   OOO   F              *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SHPPROF  - Print shipping proficiency report.             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 09/15/94 ! ORIGINAL                                 ! HES *~
            * 12/19/96 ! Millie date conversion - Add to Mod Block! DER *~
            *          ! on 08/26/96 - Blank Date                 !     *~
            *          ! on 09/23/96 - Date range check, inc. size!     *~
            *          ! 09/17/97 - fixed date shipped if "pd"    !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            age%(9),                     /* Aging Break Down For Summar*/~
            age$(7)3,                    /* Aging Break Down For Summar*/~
            age1%(9),                    /* Aging Accumulator Array    */~
            age1$(9)5,                   /* Aging Accumulator Array    */~
            age2%(9),                    /* Aging Accumulator Array    */~
            breaks$1,                    /* Print Page Breaks w/totals */~
            blankdate$6,                 /* blank unfmt date           */~
            cat$4,                       /* Category Code              */~
            company$60,                  /* Company Name Field         */~
            cursor%(2),                  /* Cursor Location For Edit   */~
            cuscode$9,                   /* Customer Code              */~
            currentcat$4,                /* Category For This Printline*/~
            currentcus$9,                /* Customer For This Printline*/~
            currentdays$3,               /* days late For This Printlin*/~
            currentdate$8,               /* Inv Date For This Printline*/~
            currentkey$25,               /* Current Record Key         */~
            currentpart$25,              /* Part For This Printline    */~
            currentstr$3,                /* Store No For This Printline*/~
            date$8,                      /* Date For Screen Display    */~
            days$3,                      /* Days Late (Binary)         */~
            descr$32,                    /* Part Description           */~
            displaycat$4,                /* Category To Print          */~
            displaycus$9,                /* Customer To Print          */~
            displaydate$8,               /* Inv Date To Print          */~
            displaydays$3,               /* days late To Print         */~
            displaypart$25,              /* Part Num To Print          */~
            displaystr$3,                /* Store No To Print          */~
            duedate$8,                   /* Required Shipment Date.    */~
            edtmessage$79,               /* Edit Screen Message        */~
            errormsg$79,                 /* Error Message              */~
            fdate$10,                    /* unfmt from date            */~
            firstdate$8,                 /* Earliest Date For Report   */~
            firstcat$4,                  /* First Category (For Screen)*/~
            firstcus$9,                  /* First Customer (For Screen)*/~
            firststr$3,                  /* First Store No (For Screen)*/~
            fcat$4,                      /* First Cat In Range         */~
            fcus$9,                      /* First Cus In Range         */~
            fpart$25,                    /* First Part In Range        */~
            fstr$3,                      /* First str In Range         */~
            firstpart$25,                /* First Part (For Screen)    */~
            fromdate$10,                 /* First Date For Workfile    */~
            i$(24)80,                    /* Screen Image               */~
            inlib$8,                     /* Data Base Name             */~
            invnumber$8,                 /* Invoice Number             */~
            invdate$8,                   /* Date Of Invoice            */~
            ivctype$1,                   /* Reclines Adj Records       */~
            lastcat$4,                   /* Last Category (For Screen) */~
            lastcus$9,                   /* Last Customer (For Screen) */~
            laststr$3,                   /* Last Store No (For Screen) */~
            lcat$4,                      /* Last Cat In Range          */~
            lcus$9,                      /* Last Cus In Range          */~
            lpart$25,                    /* Last Part In Range         */~
            lstr$3,                      /* Last Str In Range          */~
            lastpart$25,                 /* Last Part (For Screen)     */~
            line2$79,                    /* Screen Line #2             */~
            inpmessage$79,               /* Input Message              */~
            lastdate$8,                  /* Last Date For Report       */~
            net$8,                       /* Work Variable              */~
            lineplowkey$65,              /* Plowkey For Reclines       */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            netgross$1,                  /* Use Gross Or Net Price     */~
            part$25,                     /* Full Part Number           */~
            pct$(9)4,                    /* Aging Accumulator Array    */~
            pf$(3)79,                    /* PF Key Strings             */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$50,                  /* Plowkey For Recmastr       */~
            postdate$6,                  /* Invoice Post Date          */~
            prtdate$8,                   /* Ship Date For Print        */~
            qty$8,                       /* Work Variable              */~
            readkey$60,                  /* Work Variable              */~
            runmode$1,                   /* Report Run Mode            */~
            rundescr$30,                 /* Report Run Mode            */~
            seq$3,                       /* Reclines Sequence Number   */~
            shpdate$8,                   /* Invoice Ship Date          */~
            so$16,                       /* Sales Order Number         */~
            soline$3,                    /* Sales Order Line Number    */~
            sort1$4,                     /* Primary Sort Selected      */~
            sort2$4,                     /* Secondary Sort Selected    */~
            sortkey$65,                  /* For Work File              */~
            sortopt$22,                  /* Report title Field         */~
            srtdate$6,                   /* Work VAriable              */~
            str$3,                       /* Store Number               */~
            tdate$10,                    /* unfmt to date              */~
            time$8,                      /* Report Run Time            */~
            titl$(3)25,                  /* Screen title line          */~
            todate$10,                   /* Second Date For Workfile   */~
            totallit$70,                 /* Literal string for total   */~
            total(3,6),                  /* Totals                     */~
            total$(5)10,                 /* Totals                     */~
            tprtdate1$8,                 /* temp prtdate 1             */~
            tprtdate2$8,                 /* temp prtdate 2             */~
            userid$3                     /* Thouest Whom I Serve       */

        dim f2%(16),                     /* = 0 if the file is open    */~
            f1%(16),                     /* = 1 if READ was successful */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(16)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! ARIMASTR ! Invoice Master File                      *~
            * #2  ! ARILINES ! Invoice Line Items File                  *~
            * #3  ! SYSFILE2 ! Caelus Management System Information     *~
            * #4  ! CUSTOMER ! Customer Master File                     *~
            * #6  ! BCKLINES ! Back Log Line Item File                  *~
            * #8  ! WORKFILE ! Workfile built from ARILINES             *~
            * #9  ! WORKFIL1 ! Workfile built for summary               *~
            * #10 ! WORKFIL2 ! Workfile built for backorder control     *~
            * #11 ! CATEGORY ! Inventory category codes file            *~
            * #12 ! STORNAME ! Store Info File - Name/Address           *~
            * #13 ! HNYMASTR ! Inventory Master File                    *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "ARIMASTR",                                      ~
                        varc,     indexed,  recsize = 2000,              ~
                        keypos =    1, keylen =  17                      ~

            select #2,  "ARILINES",                                      ~
                        varc,     indexed,  recsize =  750,              ~
                        keypos =    1, keylen =  20                      ~

            select #3,  "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #4,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9

            select #6,  "BCKLINES",                                      ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =  10,  keylen = 19

            select #8,  "WORKFILE",                                      ~
                        varc, consec, recsize = 114

            select #9,  "WORKFIL1",                                      ~
                        varc, consec, recsize = 45

            select #10, "WORKFIL2",                                      ~
                        varc,     indexed,  recsize = 25,                ~
                        keypos =  1,   keylen = 19

            select #11, "CATEGORY",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    1, keylen =   4

            select #12, "STORNAME",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   3

            select #13,  "HNYMASTR",                                     ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25

            call "SHOSTAT" ("Opening files, One Moment Please")

            call "OPENCHCK" (#1,  0%, f2%(1%),  0%, rslt$(1%))
            call "OPENCHCK" (#2,  0%, f2%(2%),  0%, rslt$(2%))
            call "OPENCHCK" (#3,  0%, f2%(3%),  0%, rslt$(3%))
            call "OPENCHCK" (#4,  0%, f2%(4%),  0%, rslt$(4%))
            call "OPENCHCK" (#6,  0%, f2%(6%),  0%, rslt$(6%))
            call "OPENCHCK" (#11, 0%, f2%(11%), 0%, rslt$(11%))
            call "OPENCHCK" (#12, 0%, f2%(12%), 0%, rslt$(12%))
            call "OPENCHCK" (#13, 0%, f2%(13%), 0%, rslt$(13%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)
            blankdate$ = " "
            call "DATUNFMT" (blankdate$)
            call "COMPNAME" (12%, company$, u3%)
            str(line2$,62) = " SHPPROF: " & str(cms2v$,,8)
            call "EXTRACT" addr("ID", userid$, "IL", inlib$)

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press RETURN."

            titl$(1) = "Selection"
            titl$(2) = "Beginning Selection"
            titl$(3) = "Ending Selection"

        REM *************************************************************~
            *       I N P U T   M O D E   S C R E E N   O N E           *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub L29000

            for fieldnr% = 1 to 8
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0 then L10220
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10200
L10150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10120
                         if fieldnr% = 1% then L10100
                         goto L10150
L10200:               if keyhit% = 16 and fieldnr% = 1 then exit_program
                      if keyhit% <>  0 then L10120
L10220:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10120
            next fieldnr%


        REM *************************************************************~
            *        E D I T   M O D E   S C R E E N   O N E            *~
            *************************************************************

        editpg1
            editmode% = 1%
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       build_work_file
                  if keyhit% <>  0 then       editpg1
L11110:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% >  8% then editpg1
            if fieldnr% = lastfieldnr% then editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       editpg1
L11180:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11180
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11180
                  lastfieldnr% = fieldnr%
            goto L11110

        REM *************************************************************~
            *             B U I L D  T H E  W O R K  F I L E            *~
            * Build the workfile for the selected invoice lines         *~
            *************************************************************

        build_work_file
            recnbr% = max((val(str(rslt$(2),17,4),4)/50%), 3000%)
            call "WORKOPEN"(#8,  "OUTPT", recnbr%, f2%(8))
            call "WORKOPEN"(#9,  "OUTPT", 500%,    f2%(9))
            call "WORKOPEN"(#10, "IO   ", recnbr%, f2%(10))
            call "SHOSTAT" ("Selecting Invoice Details...")
            fdate$ = fromdate$ : tdate$ = todate$
            call "DATUFMTC" (fdate$) : call "DATUFMTC" (tdate$)
            sortopt$ = "SORT BY " & sort1$
            if sort2$ <> " " then sortopt$ = sortopt$ & " THEN " & sort2$
            call "RJUSTIFY" (sortopt$)
            plowkey$ = fcus$

*       *  Now march through the ARIMASTER file looking for records that
*       *  fall within the ranges selected.
L14200:     call "PLOWNEXT" (#1%, plowkey$, 0%, f1%(1%))
               if f1%(1%) = 0% then process_and_print
            if str(plowkey$,,9) > lcus$ then process_and_print

            REM Invoices...
            get #1 using L14280, cuscode$, invnumber$, so$, shpdate$,     ~
                                invdate$, postdate$, hdiscpct, str$,     ~
                                ivctype$
L14280:     FMT CH(9), CH(8), POS(34), CH(16), POS(413), CH(6), POS(521),~
              CH(6), POS(533), CH(6), POS(801), PD(14,4), POS(870),      ~
              CH(3), POS(891), CH(1)

            if ivctype$ <> "O" then L14200
            srtdate$ = shpdate$
            if srtdate$ = " " or srtdate$ = blankdate$ then ~
               srtdate$ = postdate$
            if srtdate$  < fdate$ or srtdate$  > tdate$ then L14200
            if str$      <=fstr$  or str$      > lstr$  then L14200
            if cuscode$  <=fcus$  or cuscode$  > lcus$  then L14200

*       * For each qualified master, load up the line items
            lineplowkey$ = str(plowkey$,,17%) & hex(00)

L14410:     call "PLOWNEXT" (#2%, lineplowkey$, 17%, f1%(2%))
               if f1%(2%) = 0 then L14200  /* on to the next master */

            REM Invoices Lines...
            get #2, using L14470, seq$, part$, descr$, cat$, quanship,    ~
                                 unitsell, disc, extension, soline$
L14470:     FMT XX(17), CH(3), POS(24), CH(25), CH(32), CH(4), POS(93),  ~
                PD(14,4), POS(109), PD(14,4), POS(141), PD(14,4),        ~
                POS(157), PD(14,4), POS(194),CH(3)
            if quanship = 0 then L14410
            if part$ = "  " then goto L14410  /* Don't want these memos */
            if part$ < fpart$ or part$ > lpart$ then L14410
            if cat$  < fcat$  or cat$  > lcat$  then L14410

            REM Channel 10 is used to insure we only show backorder once
            REM on the report when there are multi ships against same line
            oo_qty = 0
            readkey$ = str(so$) & soline$
            call "READ100" (#10, readkey$, f1%(10))
               if f1%(10) = 0% then L14650
            get #10, using L14620, duedate$
L14620:     FMT POS(20), CH(6)
            goto L14730

L14650:     REM Read Order Line Detail Record
            call "READ100" (#6, readkey$, f1%(6))
               if f1%(6) = 0% then L14410
            get #6, using L14690, oo_qty, duedate$
L14690:     FMT POS(109), PD(14,4), POS(212), CH(6)
            write #10, using L14710, readkey$, duedate$
L14710:     FMT CH(19), CH(6)

L14730:     REM Compute Backlog $ based on what it would have billed at,
            REM had it shipped with this shipment...
            if oo_qty <= 0 and runmode$ = "B" then L14410
            oo_amt = oo_qty * unitsell
            oo_amt = round((1 - (disc * .01)) * oo_amt, 2)

            REM Compute Days Late...
            call "DATE"addr("G-", str(duedate$,,6), srtdate$, days%, u3%)
            if days% < 1% and runmode$ = "L" then L14410
            days% = min(max(-999%, days%), 999%)
            for i% = 1% to 9%
                if days% <= age%(i%) then L14860
            next i%
L14860:     days$ = bin(age%(i%), 2%) /* for sort use only */
            days% = days% + 1000%

            REM Factor in header discount to show true net...
            extension = round((1 - (hdiscpct * .01)) * extension, 2)
            oo_amt    = round((1 - (hdiscpct * .01)) * oo_amt,    2)

            REM Set up sort area of key...
            extension = min(max(-99999999, extension), 99999999)
            quanship  = min(max(-99999999, quanship ), 99999999)
            extension = extension + 100000000  /* Deal with */
            quanship  = quanship  + 100000000  /* negatives.*/
            extension = 200000000  - extension   /* Reverse   */
            quanship  = 200000000  - quanship    /* Order.    */
            put net$ using L15020, extension
            put qty$ using L15020, quanship
L15020:     FMT PD(14,4)
            u1%, u2%, u3%, u4%, u5%, u6%, u7%, u8% = 1%
            if sort1$ <> "CAT"  then L15060
               sortkey$ = cat$     : next% =  5% : u1%=0%
L15060:     if sort1$ <> "PART" then L15080
               sortkey$ = part$    : next% = 26% : u2%=0%
L15080:     if sort1$ <> "CUS"  then L15100
               sortkey$ = cuscode$ : next% = 10% : u3%=0%
L15100:     if sort1$ <> "DATE" then L15120
               sortkey$ = srtdate$ : next% =  7% : u4%=0%
L15120:     if sort1$ <> "STOR" then L15140
               sortkey$ = str$     : next% =  4% : u5%=0%
L15140:     if sort1$ <> "DAYS" then L15160
               sortkey$ = days$    : next% =  3% : u6%=0%
L15160:     if sort2$ <> "CAT"  then L15180
               str(sortkey$,next%) = cat$     : next%=next% +  4%: u1%=0%
L15180:     if sort2$ <> "PART" then L15200
               str(sortkey$,next%) = part$    : next%=next% + 25%: u2%=0%
L15200:     if sort2$ <> "CUS"  then L15220
               str(sortkey$,next%) = cuscode$ : next%=next% +  9%: u3%=0%
L15220:     if sort2$ <> "DATE" then L15240
               str(sortkey$,next%) = srtdate$ : next%=next% +  6%: u4%=0%
L15240:     if sort2$ <> "STOR" then L15260
               str(sortkey$,next%) = str$     : next%=next% +  3%: u5%=0%
L15260:     if sort2$ <> "DAYS" then L15280
               str(sortkey$,next%) = days$    : next%=next% +  2%: u6%=0%
L15280:     if sort2$ <> "NET$" then L15300
               str(sortkey$,next%) = net$     : next%=next% +  8%: u7%=0%
L15300:     if sort2$ <> "QTY " then L15320
               str(sortkey$,next%) = qty$     : next%=next% +  8%: u8%=0%
L15320:     sortlen% = next% - 1%
            for i% = 1% to 8%
                if u1%=0% then L15360
                   str(sortkey$,next%)=cat$    :next%=next%+ 4% : u1%=0%
L15360:         if u2%=0% then L15380
                   str(sortkey$,next%)=part$   :next%=next%+25% : u2%=0%
L15380:         if u3%=0% then L15400
                   str(sortkey$,next%)=cuscode$:next%=next%+ 9% : u3%=0%
L15400:         if u4%=0% then L15420
                   str(sortkey$,next%)=srtdate$:next%=next%+ 6% : u4%=0%
L15420:         if u5%=0% then L15440
                   str(sortkey$,next%)=str$    :next%=next%+ 3% : u5%=0%
L15440:         if u6%=0% then L15460
                   str(sortkey$,next%)=days$   :next%=next%+ 2% : u6%=0%
L15460:         if u7%=0% then L15480
                   str(sortkey$,next%)=net$    :next%=next%+ 8% : u7%=0%
L15480:         if u8%=0% then L15500
                   str(sortkey$,next%)=qty$    :next%=next%+ 8% : u8%=0%
L15500:     next i%

            if shpdate$ = " " or shpdate$ = blankdate$ then ~
               shpdate$ = "pd" & postdate$
            write #8, using L15550, sortkey$, invnumber$, seq$, duedate$, ~
                      invdate$, shpdate$, oo_qty, oo_amt, days%
L15550:     FMT CH(65), CH(8), CH(3), 2*CH(6), CH(8), 2*PD(14,4), BI(2)
            wfrec% = wfrec% + 1
            goto L14410    /* Go plow for next line item */

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            * Sets defaults and enables fields for page 1 of input.     *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20180,         /* Date Range       */~
                                    L20240,         /* Customer Range   */~
                                    L20280,         /* Category Range   */~
                                    L20320,         /* Part Range       */~
                                    L20360,         /* Store Range      */~
                                    L20400,         /* Report Run Mode  */~
                                    L20440,         /* Aging Days       */~
                                    L20480          /* Sort Options     */

                  return

L20180: REM Default/enable for date range to report on
            if fromdate$ <> " " and fromdate$ <> blankdate$ then return
            fromdate$ = "19010101"
            call "DATECONV" (fromdate$)
            call "DATFMTC"  (fromdate$)
            todate$ = date
            call "DATFMTC"  (todate$)
            return

L20240: REM Default/enable for Customer Range
            if firstcus$ = " " then firstcus$ = "ALL"
            return

L20280: REM Default/enable for Category Range
            if firstcat$ = " " then firstcat$ = "ALL"
            return

L20320: REM Default/enable for Part Number Range
            if firstpart$ = " " then firstpart$ = "ALL"
            return

L20360: REM Default/enable for Store Number Range
            if firststr$ = " " then firststr$ = "ALL"
            return

L20400: REM Default/enable for Report Run Mode
            if runmode$ = " " then runmode$ = "D"
            return

L20440: REM Default/enable for Aging Days For Summary
            if age$() = " " then age$() = "  1  2  3  7 14 30 60"
            return

L20480: REM Default/enable for Sort Options
            if sort1$ <> " " then return
               sort1$ = "CAT"
               sort2$ = "PART"
               breaks$ = "N"
               return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28110
                inpmessage$ = edtmessage$
                return

L28110
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Beginning and Ending Date for Selection                ",~
         "Enter Beginning and Ending Customer Code or 'ALL'            ",~
         "Enter Beginning and Ending Category Code or 'ALL'            ",~
         "Enter Beginning and Ending Part Number or 'ALL'              ",~
         "Enter Beginning and Ending Store Number or 'ALL'             ",~
         "D) all DETAIL, L) LATE Only, B) BACKORDER Only, or S) SUMMARY P~
        ~ages Only.",                                                     ~
         "All buckets must contain values between 1 and 999, incrementaly~
        ~.",                                                              ~
         "Sortby: CAT, PART, CUS, DATE, STOR, or DAYS. Thenby: same, plus~
        ~ NET$ or QTY"

L29000: REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

            init(" ") errormsg$, inpmessage$, firstcat$, sort1$, age$(), ~
                      firstpart$, netgross$, lastcat$, firstcus$, sort2$,~
                      firststr$, laststr$, lastcus$, runmode$, lastdate$,~
                      lastpart$, lastdate$, firstdate$, breaks$,         ~
                      rundescr$

            call "FILEBGON" (#8)
            call "FILEBGON" (#9)
            call "FILEBGON" (#10)
            mat total = zer
            mat age1% = zer
            mat age2% = zer
            editmode% = 0%
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode

        REM *************************************************************~
            *   Read the workfile, print the report and accumulate      *~
            *   part and category totals and averages.  The report      *~
            *   headers vary with summary or detail reports.            *~
            *************************************************************

        process_and_print
            if wfrec% > 0% then L30130
               call "ASKUSER" (0%, "Sorry", "No Data Was Found That Met",~
                               "Your Selection Criteria.",               ~
                               "Press RETURN To Acknowledge")
               goto inputmode

L30130:     call "SHOSTAT" ("Sorting Selected Records...")
            call "SLCTSORT" (#8, sortlen%)
            call "SHOSTAT" ("Printing Shipping Proficiency Report")
            select printer (134)
            call "SETPRNT" ("SHP011", "SHPPROF", wfrec%, 0%)
            close #8
            call "WORKOPN2" (#8, "INPUT", 0%, f2%(8))

        REM SET THE COUNTERS, ETC.
            summary%, page% = -1%
            gosub print_params
            summary% = 0%
            line% = 99%
            printed% = 0%  /* Set to 1 if we printed anything */

*       * Now begin plowing the work file for the report
L30290:     call "READNEXT" (#8, f1%(8))
               if f1%(8) = 0 then exit_path
            get #8, using L15550, sortkey$, invnumber$, seq$, duedate$,   ~
                    invdate$, prtdate$, oo_qty, oo_amt, days%

            REM Extract sort area of key...
            u1%, u2%, u3%, u4%, u5%, u6%, u7%, u8% = 1%
            if sort1$ <> "CAT"  then L30380
               cat$     = sortkey$ : next% =  5% : u1%=0%
L30380:     if sort1$ <> "PART" then L30400
               part$    = sortkey$ : next% = 26% : u2%=0%
L30400:     if sort1$ <> "CUS"  then L30420
               cuscode$ = sortkey$ : next% = 10% : u3%=0%
L30420:     if sort1$ <> "DATE" then L30440
               shpdate$ = sortkey$ : next% =  7% : u4%=0%
L30440:     if sort1$ <> "STOR" then L30460
               str$     = sortkey$ : next% =  4% : u5%=0%
L30460:     if sort1$ <> "DAYS" then L30480
               days$    = sortkey$ : next% =  3% : u6%=0%
L30480:     big_sort% = next% - 1%
            if sort2$ <> "CAT"  then L30510
               cat$     = str(sortkey$,next%) : next%=next% +  4%: u1%=0%
L30510:     if sort2$ <> "PART" then L30530
               part$    = str(sortkey$,next%) : next%=next% + 25%: u2%=0%
L30530:     if sort2$ <> "CUS"  then L30550
               cuscode$ = str(sortkey$,next%) : next%=next% +  9%: u3%=0%
L30550:     if sort2$ <> "DATE" then L30570
               shpdate$ = str(sortkey$,next%) : next%=next% +  6%: u4%=0%
L30570:     if sort2$ <> "STOR" then L30590
               str$     = str(sortkey$,next%) : next%=next% +  3%: u5%=0%
L30590:     if sort2$ <> "DAYS" then L30610
               days$    = str(sortkey$,next%) : next%=next% +  2%: u6%=0%
L30610:     if sort2$ <> "NET$" then L30630
               net$     = str(sortkey$,next%) : next%=next% +  8%: u7%=0%
L30630:     if sort2$ <> "QTY " then L30650
               qty$     = str(sortkey$,next%) : next%=next% +  8%: u8%=0%
L30650:     for i% = 1% to 8%
                if u1%=0% then L30680
                   cat$    =str(sortkey$,next%):next%=next%+ 4% : u1%=0%
L30680:         if u2%=0% then L30700
                   part$   =str(sortkey$,next%):next%=next%+25% : u2%=0%
L30700:         if u3%=0% then L30720
                   cuscode$=str(sortkey$,next%):next%=next%+ 9% : u3%=0%
L30720:         if u4%=0% then L30740
                   shpdate$=str(sortkey$,next%):next%=next%+ 6% : u4%=0%
L30740:         if u5%=0% then L30760
                   str$    =str(sortkey$,next%):next%=next%+ 3% : u5%=0%
L30760:         if u6%=0% then L30780
                   days$   =str(sortkey$,next%):next%=next%+ 2% : u6%=0%
L30780:         if u7%=0% then L30800
                   net$    =str(sortkey$,next%):next%=next%+ 8% : u7%=0%
L30800:         if u8%=0% then L30820
                   qty$    =str(sortkey$,next%):next%=next%+ 8% : u8%=0%
L30820:     next i%
            get net$ using L15020, extension
            get qty$ using L15020, quanship
            extension = abs(extension - 200000000)
            quanship  = abs(quanship  - 200000000)
            extension = extension - 100000000
            quanship  = quanship  - 100000000
            days% = days% - 1000%
            for age% = 1% to 9%
                if days% <= age%(age%) then L30940
            next age%

L30940:     call "DATEFMT" (shpdate$)
            call "DATEFMT" (duedate$)

            tprtdate1$ = prtdate$
            if str(prtdate$, 1%, 2%) = "pd" then tprtdate1$ = str(prtdate$, 3%, 6%)
            call "DATEFMT" (tprtdate1$, tprtdate%, tprtdate2$)
            /* no error */
            tprtdate% = tprtdate%
            if str(prtdate$, 1%, 2%) = "pd" ~
               then prtdate$ = "pd" & str(tprtdate2$, 3%, 6%) ~
               else prtdate$ = tprtdate1$

            /* if str(prtdate$,,2%) <> "pd" then call "DATEFMT" (prtdate$) */

            call "DATEFMT" (invdate$)
            convert val(days$,2%) to days$, pic(##0)
            if printed% > 0% then L31080
               REM Don't trigger subtotal on very first record
               currentkey$  = sortkey$
               currentpart$ = part$
               currentcat$  = cat$
               currentcus$  = cuscode$
               currentdate$ = shpdate$
               currentstr$  = str$
               currentdays$ = days$
L31080:     REM See if we need to subtotal...
            if sort1$ = "CAT"  and currentcat$  <> cat$     then L31220
            if sort1$ = "CUS"  and currentcus$  <> cuscode$ then L31220
            if sort1$ = "PART" and currentpart$ <> part$    then L31220
            if sort1$ = "DATE" and currentdate$ <> shpdate$ then L31220
            if sort1$ = "STOR" and currentstr$  <> str$     then L31220
            if sort1$ = "DAYS" and currentdays$ <> days$    then L31220
            if sort2$ = "CAT"  and currentcat$  <> cat$     then L31220
            if sort2$ = "CUS"  and currentcus$  <> cuscode$ then L31220
            if sort2$ = "PART" and currentpart$ <> part$    then L31220
            if sort2$ = "DATE" and currentdate$ <> shpdate$ then L31220
            if sort2$ = "STOR" and currentstr$  <> str$     then L31220
            if sort2$ = "DAYS" and currentdays$ <> days$    then L31220
            goto L31240
L31220:        REM Need to print totals...
               gosub print_sub_totals
L31240:     printed% = 1%
            currentkey$  = sortkey$
            currentpart$ = part$
            currentcat$  = cat$
            currentcus$  = cuscode$
            currentdate$ = shpdate$
            currentstr$  = str$
            currentdays$ = days$

*       * Increment Subtotals
            total(1%,1%) = total(1%,1%) + oo_qty
            total(1%,2%) = total(1%,2%) + quanship
            total(1%,3%) = total(1%,3%) + extension
            total(1%,4%) = total(1%,4%) + oo_amt
            total(1%,5%) = total(1%,5%) + 1

*       * Increment Intermediate Totals
            total(2%,1%) = total(2%,1%) + oo_qty
            total(2%,2%) = total(2%,2%) + quanship
            total(2%,3%) = total(2%,3%) + extension
            total(2%,4%) = total(2%,4%) + oo_amt
            total(2%,5%) = total(2%,5%) + 1
            age1%(age%)  = age1%(age%)  + 1%

*       * Increment grand totals
            total(3%,1%) = total(3%,1%) + oo_qty
            total(3%,2%) = total(3%,2%) + quanship
            total(3%,3%) = total(3%,3%) + extension
            total(3%,4%) = total(3%,4%) + oo_amt
            total(3%,5%) = total(3%,5%) + 1

            REM Print the detail record...
            total$() = " "
            if oo_amt = 0 then L31600
               call "CONVERT" (oo_qty,    0.2, total$(1%))
               call "CONVERT" (oo_amt,    2.2, total$(4%))
L31600:     call "CONVERT" (quanship,  0.2, total$(2%))
            call "CONVERT" (extension, 2.2, total$(3%))

            if line% > 55% then gosub print_header
            if runmode$ = "S" then L31680
            print using L60270, cat$, part$, cuscode$, prtdate$, str$,    ~
                               invnumber$, invdate$, duedate$, total$(1),~
                               total$(2), total$(3), total$(4), days%
L31680:     line% = line% + 1%
            goto L30290       /* Get another one    */

        print_sub_totals
            if sort2$ = " "    then L33070     /*  Don't   */
            if sort2$ = "NET$" then L33070     /* Subtotal */
            if sort2$ = "QTY " then L33070     /*  These   */
            i% = 1%
            gosub print_total_line

L33070:     if sort1$ = "CAT"  and currentcat$  = cat$     then return
            if sort1$ = "CUS"  and currentcus$  = cuscode$ then return
            if sort1$ = "PART" and currentpart$ = part$    then return
            if sort1$ = "DATE" and currentdate$ = shpdate$ then return
            if sort1$ = "STOR" and currentstr$  = str$     then return
            if sort1$ = "DAYS" and currentdays$ = days$    then return
            gosub print_mid_totals
            return

*       * Print routine for category totals
        print_mid_totals
            i% = 2%
            gosub print_total_line
            if breaks$ = "Y" then line% = 99%
            return

        print_total_line
            call "CONVERT" (total(i%, 1%), 0.0, total$(1))
            call "CONVERT" (total(i%, 2%), 0.0, total$(2))
            call "CONVERT" (total(i%, 3%), 0.0, total$(3))
            call "CONVERT" (total(i%, 4%), 0.0, total$(4))

            if line% > 53% then gosub print_header
            REM Don't total if only one detail
            line% = line% + 1%
            if total(i%,5%) = 1 then L33670
               if runmode$ <> "S" then print using L60300

            if line% > 53% then gosub print_header
            totallit$, displaycat$, displaycus$, displaypart$ = " "
            displaydate$, displaystr$, displaydays$ = " "
            if sort1$ = "CAT"  then displaycat$  = currentcat$
            if sort1$ = "CUS"  then displaycus$  = currentcus$
            if sort1$ = "PART" then displaypart$ = currentpart$
            if sort1$ = "DATE" then displaydate$ = currentdate$
            if sort1$ = "STOR" then displaystr$  = currentstr$
            if sort1$ = "DAYS" then displaydays$ = currentdays$
            call "CONVERT" (total(i%,5%), -0.01, str(totallit$,,4%))
            if i% = 1% then L33530
               if sort2$ = "CAT"  then displaycat$  = all("*")
               if sort2$ = "CUS"  then displaycus$  = all("*")
               if sort2$ = "PART" then displaypart$ = all("*")
               if sort2$ = "DATE" then displaydate$ = all("*")
               if sort2$ = "STOR" then displaystr$  = all("*")
               if sort2$ = "DAYS" then displaydays$ = all("*")
               goto L33590
L33530:     if sort2$ = "CAT"  then displaycat$  = currentcat$
            if sort2$ = "CUS"  then displaycus$  = currentcus$
            if sort2$ = "PART" then displaypart$ = currentpart$
            if sort2$ = "DATE" then displaydate$ = currentdate$
            if sort2$ = "STOR" then displaystr$  = currentstr$

L33590:     totallit$ = totallit$ & " SHPMTS"
            if runmode$ = "S" then L33650
            print using L60270, displaycat$, displaypart$, displaycus$,   ~
                               displaydate$, displaystr$, totallit$,     ~
                               " ", " ", total$(1%), total$(2%),         ~
                               total$(3%), total$(4%), displaydays$
L33650:     line% = line% + 2%

L33670:     if runmode$ <> "S" then print skip(1)
            total(i%,1%), total(i%,2%), total(i%,3%), total(i%,4%) = 0
            total(i%,6%) = total(i%,6%) + 1%
            if i% <> 2% then L33760
               REM Accumulate Summary Info For Summary Pages
               write #9, using L33740, str(currentkey$,,big_sort%),       ~
                                      age1%(), total(2%,5%)
L33740:        FMT CH(25), 10*BI(2)
               mat age1% = zer
L33760:     total(i%,5%) = 0
            return

        exit_path
            init (hex(ff)) cat$, part$, cuscode$, shpdate$, str$, days$
            gosub print_sub_totals
            gosub print_grand_totals
            gosub print_summary
            print skip(2)
            print using L60580   /* END OF REPORT */
            close printer
            goto inputmode

        print_grand_totals
            if runmode$ = "S" then return
            i% = 3%

            call "CONVERT" (total(i%, 1%), 0.0, total$(1%))
            call "CONVERT" (total(i%, 2%), 0.0, total$(2%))
            call "CONVERT" (total(i%, 3%), 0.0, total$(3%))
            call "CONVERT" (total(i%, 4%), 0.0, total$(4%))

            call "CONVERT" (total(i%,5%), -0.001, total$(5%))
            totallit$ = "***** REPORT TOTALS: " & total$(5%)
            call "CONVERT" (total(1%,6%), -0.001, total$(5%))
            totallit$ = totallit$ & " SHPMTS FOR "
            if sort2$ = "CAT"  then temp$ = "CATEGORIES"
            if sort2$ = "CUS"  then temp$ = "CUSTOMERS"
            if sort2$ = "PART" then temp$ = "PARTS"
            if sort2$ = "DATE" then temp$ = "INV DATES"
            if sort2$ = "STOR" then temp$ = "STORES"
            if sort2$ = "DAYS" then temp$ = "DAY BUCKETS"
            if sort2$ = " "    then L34330
            if sort2$ = "DAYS" then L34330
            if sort2$ = "NET$" then L34330
            totallit$ = totallit$ & " " & total$(5%)
            totallit$ = totallit$ & " " & temp$ & " WITHIN"
L34330:     call "CONVERT" (total(2%,6%), -0.001, total$(5%))
            if sort1$ = "CAT"  then temp$ = "CATEGORIES"
            if sort1$ = "CUS"  then temp$ = "CUSTOMERS"
            if sort1$ = "PART" then temp$ = "PARTS"
            if sort1$ = "DATE" then temp$ = "IVC DATES"
            if sort1$ = "STOR" then temp$ = "STORES"
            if sort1$ = "DAYS" then temp$ = "DAY BUCKETS"
            totallit$ = totallit$ & " " & total$(5%) & " " & temp$

            if line% > 53 then gosub print_header
            print skip(1)
            print using L60340
            print using L60370, totallit$, total$(1%), total$(2%),        ~
                               total$(3%), total$(4%)
            return

        print_summary
            close #9
            call "WORKOPN2" (#9, "INPUT", 0%, f2%(9))
            line% = 99% : summary% = 1% : total% = 0%

            REM plow the work file for the summary records...
L34550:     call "READNEXT" (#9, f1%(9))
               if f1%(9) = 0 then L34680
            get #9, using L33740, sortkey$, age1%(), u3%
            if sort1$ = "DATE" then call "DATEFMT" (str(sortkey$,,8%))
            if sort1$ = "DAYS" then convert val(str(sortkey$,,2%),2%)    ~
                                    to str(sortkey$,7%), pic(##0)
            if sort1$ = "DAYS" then str(sortkey$,,6%) = "DAYS: "
            gosub print_a_sum_line
            mat age2% = age2% + age1%
            total% = total% + u3%
            goto L34550

            REM OK, wrap things up...
L34680:     mat age1% = age2%
            u3% = total%
            sortkey$ = "    **** TOTALS ****"
            print using L60480
            gosub print_a_sum_line
            print using L60480
            print skip(1)
            print using L60520
            print using L60550
            return

        print_a_sum_line
            if line% > 54 then gosub print_header
            age1$(), pct$() = " "
            for i% = 1% to 9%
                if age1%(i%) = 0 then L34900
                   temp = age1%(i%) : temp1 = u3%
                   pct = round((temp/temp1)*100, 0%)
                   convert pct to pct$(i%), pic(##0)
                   str(pct$(i%),4) = "%"
                   convert age1%(i%) to age1$(i%), pic(####0)
                   goto L34920
L34900:         if mod(line%,2%) = 0% then L34920
                   age1$(i%) = "    -" : pct$(i%) = "  -"
L34920:     next i%
            print using L60440,sortkey$, u3%, age1$(1), pct$(1), age1$(2),~
                 pct$(2), age1$(3), pct$(3), age1$(4), pct$(4), age1$(5),~
                 pct$(5), age1$(6), pct$(6), age1$(7), pct$(7), age1$(8),~
                 pct$(8), age1$(9), pct$(9)
            line% = line% + 1%
        return

        print_header
            if runmode$ = "S" and summary% = 0% then return
            print page
            page% = page% + 1%
            line% = 7%
            time$ = " " : call "TIME" (time$)
            print using L60060, date$, time$, company$
            print using L60100, userid$, inlib$, page%
            print using L60140, sortopt$
            print skip(1)
            if summary% <> 1% then L36160
               print using L60480
               print using L60400, age%(1), age%(2), age%(3), age%(4),    ~
                           age%(5), age%(6), age%(7), age%(8), age%(9)
               print using L60480
               return
L36160:     print using L60180
            print using L60210
            print using L60240
        return

        print_params
            gosub print_header
L36230:     x% = pos(str(i$()) > hex(7f))
            if x% = 0% then L36270
               str(i$(),x%,1%) = " "
               goto L36230
L36270:     print skip(3)
            print tab(37);
            print "--------------------- Report Selection Parameters ----~
        ~---------"
            for x% = 5% to 19%: print tab(37); i$(x%) : next x%
            print tab(37);
            print "------------------------------------------------------~
        ~---------"
        return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              gosub set_pf1
              str(pf$(3),63,1) = hex(84)
              str(line2$,,60) = "Report Range and Calculation Parameters"
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()

              on fieldnr% gosub L40270,         /* Date Range        */   ~
                                L40270,         /* Customer Range    */   ~
                                L40270,         /* Category Range    */   ~
                                L40270,         /* Part Range        */   ~
                                L40270,         /* Store Range       */   ~
                                L40270,         /* Run Mode          */   ~
                                L40270,         /* Aging Days        */   ~
                                L40270          /* Sort Options      */
              goto L40300

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40270:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40300:     accept                                                       ~
               at (01,02),                                               ~
                  "Shipping Proficiency Reporting",                      ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), fac(hex(ac)), titl$(1)               , ch(21),~
               at (05,27), fac(hex(ac)), titl$(2)               , ch(25),~
               at (05,55), fac(hex(ac)), titl$(3)               , ch(25),~
                                                                         ~
               at (06,02), "Shipment Date Range",                        ~
               at (06,27), fac(lfac$( 1)), fromdate$            , ch(10),~
               at (06,55), fac(lfac$( 1)), todate$              , ch(10),~
                                                                         ~
               at (07,02), "Customer Range",                             ~
               at (07,27), fac(lfac$( 2)), firstcus$            , ch(09),~
               at (07,55), fac(lfac$( 2)), lastcus$             , ch(09),~
                                                                         ~
               at ( 8,02), "Category Range",                             ~
               at ( 8,27), fac(lfac$( 3)), firstcat$            , ch(04),~
               at ( 8,55), fac(lfac$( 3)), lastcat$             , ch(04),~
                                                                         ~
               at ( 9,02), "Part Number Range",                          ~
               at ( 9,27), fac(lfac$( 4)), firstpart$           , ch(25),~
               at ( 9,55), fac(lfac$( 4)), lastpart$            , ch(25),~
                                                                         ~
               at (10,02), "Store Number Range",                         ~
               at (10,27), fac(lfac$( 5)), firststr$            , ch(03),~
               at (10,55), fac(lfac$( 5)), laststr$             , ch(03),~
                                                                         ~
               at (11,02), "Report Run Mode",                            ~
               at (11,27), fac(lfac$( 6)), runmode$             , ch(01),~
               at (11,30), fac(hex(84)),   rundescr$            , ch(30),~
                                                                         ~
               at (12,02), "Aging Days For Summary",                     ~
               at (12,27), fac(lfac$( 7)), age$(1)              , ch(03),~
                           fac(lfac$( 7)), age$(2)              , ch(03),~
                           fac(lfac$( 7)), age$(3)              , ch(03),~
                           fac(lfac$( 7)), age$(4)              , ch(03),~
                           fac(lfac$( 7)), age$(5)              , ch(03),~
                           fac(lfac$( 7)), age$(6)              , ch(03),~
                           fac(lfac$( 7)), age$(7)              , ch(03),~
                                                                         ~
               at (13,02), "Sort By #### ,  Then By ####     Page Break",~
               at (13,10), fac(lfac$( 8)), sort1$               , ch(04),~
               at (13,27), fac(lfac$( 8)), sort2$               , ch(04),~
               at (13,47), fac(lfac$( 8)), breaks$              , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40910
                  call "MANUAL" ("SHPPROF ") : goto L40300

L40910:        if keyhit% <> 15 then L40940
                  call "PRNTSCRN" : goto L40300

L40940:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L41130     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L41090
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L41090:     if fieldnr% > 1% then L41110
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L41110:     return

L41130: if fieldnr% > 0% then L41220  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Print Report"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L41220:                              /*  Edit Mode - Enabled    */       ~
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return
                                                                         ~
        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub      L50180,          /* Date Range       */~
                                   L50310,          /* Customer Range   */~
                                   L50360,          /* Category Range   */~
                                   L50410,          /* Part Range       */~
                                   L50460,          /* Store Range      */~
                                   L50510,          /* Report Run Mode  */~
                                   L50620,          /* Aging Days       */~
                                   L50750           /* Sort Option      */
                     return

L50180: REM Test data for Date Range
            if fromdate$ <> "ALL" then L50220
            fromdate$ = "19010101"
            todate$   = "20991231"
L50220:     call "DATEOKC" (fromdate$, firstdate%, errormsg$)
                if errormsg$ <> " " then return
            call "DATEOKC" (todate$, lastdate%, errormsg$)
                if errormsg$ <> " " then return
            if firstdate% <= lastdate% then return
                errormsg$ = "Beginning Date must be earlier than Ending D~
        ~ate"
                return

L50310: REM Test data for Customer Code Range
              call "TESTRNGE" (firstcus$, lastcus$, fcus$, lcus$,        ~
                               errormsg$, #4)
              return

L50360: REM Test data for Category Code Range
              call "TESTRNGE" (firstcat$, lastcat$, fcat$, lcat$,        ~
                               errormsg$, #11)
              return

L50410: REM Test data for Part Number Range
            call "TESTRNGE" (firstpart$, lastpart$, fpart$, lpart$,      ~
                             errormsg$, #13)
            return

L50460: REM Test data for Store Number Range
            call "TESTRNGE" (firststr$, laststr$, fstr$, lstr$,          ~
                             errormsg$, #12)
            return

L50510: REM Test data for Report Run Mode
            rundescr$ = " "
            if pos("DLSB" = runmode$) <> 0% then L50560
               errormsg$ = "Please Enter 'D', 'L', 'B' or 'S'"
               return
L50560:     if runmode$ = "D" then rundescr$ = "(Full Detail Report)"
            if runmode$ = "L" then rundescr$ = "(Detailed, Late Only)"
            if runmode$ = "S" then rundescr$ = "(Summary Pages Only)"
            if runmode$ = "B" then rundescr$ = "(Partial Shipments Only)"
            return

L50620: REM Test data for Aging Days For Summary
            mat age% = zer
            for i% = 1% to 7%
                call "NUMTEST" (age$(i%), 1, 998, errormsg$, 0.0, age)
                if errormsg$ <> " " then return
                   if age > age%(i%) then L50700
                      errormsg$ = "Invalid Entry: " & age$(i%)
                      return
L50700:            age%(i%+1%) = age
            next i%
            age%(9%) = 999%
            return

L50750: REM Test data for Sort Options
            if sort1$ = "CAT " then L50850
            if sort1$ = "PART" then L50850
            if sort1$ = "CUS " then L50850
            if sort1$ = "DATE" then L50850
            if sort1$ = "STOR" then L50850
            if sort1$ = "DAYS" then L50850
               errormsg$ = "SORT BY must be 'CAT', 'PART', 'CUS'" &      ~
                           ", 'DATE', 'STOR' or 'DAYS'"
               return
L50850:     if sort2$ = " "    then L50970
            if sort2$ = "CAT " then L50970
            if sort2$ = "PART" then L50970
            if sort2$ = "CUS " then L50970
            if sort2$ = "DATE" then L50970
            if sort2$ = "STOR" then L50970
            if sort2$ = "DAYS" then L50970
            if sort2$ = "NET$" then L50970
            if sort2$ = "QTY " then L50970
               errormsg$ = "THEN BY must be 'CAT', 'PART', 'CUS'" &      ~
                           ", 'DATE', 'STOR', 'DAYS', 'NET$' or 'QTY'"
               return
L50970:     if sort1$ <> sort2$ then L51000
               errormsg$ = "SORT BY and THEN BY can't be same"
               return
L51000:     if sort1$ <> "PART" or sort2$ <> "CAT" then L51030
               errormsg$ = "You Can't Sort By PART then by CAT."
               return
L51030:     if pos("YN" = breaks$) <> 0% then return
               errormsg$ = "Page Break Option Must Be 'Y' or 'N'"
               return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements                                          *~
            *************************************************************

L60060: %RUN: ########  ########             ############################~
        ~################################                      SHPPROF:SHP~
        ~011

L60100: %BY:  ###   DB: ########                        **** Shipping Pro~
        ~ficiency Report ****                                     PAGE: ##~
        ~##

L60140: %                                                                ~
        ~                                              ###################~
        ~###

L60180: %                                         DATE         INVOICE   ~
        ~ INVOICE            BACKORDER   QUANTITY    BILLING  BACKORDER DA~
        ~YS
L60210: %CATG PART NUMBER               CUSTOMER  SHIPPED  STR NUMBER    ~
        ~ DATE     DUE DATE   QUANTITY    SHIPPED     AMOUNT     AMOUNT LA~
        ~TE
L60240: %---- ------------------------- --------- -------- --- ----------~
        ~ -------- -------- ---------- ---------- ---------- ---------- --~
        ~--
L60270: %#### ######################### ######### ######## ### ##########~
        ~ ######## ######## ########## ########## ########## ########## ##~
        ~#-
L60300: %==== ========================= ========= ======== === ==========~
        ~ ======== ======== ========== ========== ========== ========== ==~
        ~==

L60340: %                                                                ~
        ~                   ========== ========== ========== ==========

L60370: %################################################################~
        ~####               ########## ########## ########## ##########

L60400: %                         !TOTAL !### DAYS  !### DAYS  !### DAYS ~
        ~ !### DAYS  !### DAYS  !### DAYS  !### DAYS  !### DAYS  !### DAYS~
        ~  !

L60440: %#########################!##### !##### ####!##### ####!##### ###~
        ~#!##### ####!##### ####!##### ####!##### ####!##### ####!##### ##~
        ~##!

L60480: %                         +------+----------+----------+---------~
        ~-+----------+----------+----------+----------+----------+--------~
        ~--+

L60520: %NOTES:  1) If a Ship Date was not specified on the Invoice, then~
        ~ the Invoice POST DATE (pdYYMMDD) was used to compute the Late Da~
        ~ys.
L60550: %        2) Only "On Order" Invoice details are included on this ~
        ~report.

L60580: %                                               * * * * * END OF ~
        ~REPORT * * * * *

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
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
            * COPYRIGHT (C) 1986, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

        exit_program

            call "SHOSTAT" ("Closing Files, One Moment Please")
            call "SETPRNT" ("  ", "   " , 0%, 1%)
            call "FILEBGON" (#8)
            call "FILEBGON" (#9)
            call "FILEBGON" (#10)

            end
