        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   SSS    AAA    GGG   PPPP   RRRR   PPPP   TTTTT          *~
            *  S      A   A  G      P   P  R   R  P   P    T            *~
            *   SSS   AAAAA  G GGG  PPPP   RRRR   PPPP     T            *~
            *      S  A   A  G   G  P      R   R  P        T            *~
            *   SSS   A   A   GGG   P      R   R  P        T            *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SAGPRPT  - Print a Gross Margin or Markup report based on *~
            *            invoiced sales over a selected date range.     *~
            *            User can choose method of calc of profit and   *~
            *            summary or detail.                             *~
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
            * 03/07/86 ! ORIGINAL                                 ! WPH *~
            * 06/22/87 ! Resurrected to R5.00.00                  ! MJB *~
            * 02/02/88 ! Corrected cost extension calculation     ! HES *~
            * 12/01/88 ! Mod to skip records with cost < 0        ! MJB *~
            * 04/03/89 ! Mod to control break if category changes ! RJM *~
            *          !   and Part is the same (>1 category/part)!     *~
            *          !   Extended size of totals fields by 1.   !     *~
            * 04/07/89 ! Since units shipped is in stock UOM,     ! MJB *~
            *          !  changed unit price to price at stk UOM. !     *~
            * 06/12/89 ! Added Customer Code to key of work file  ! MJB *~
            *          !  to quarantee record uniqueness on dup   !     *~
            *          !  invoice, part, cat combinations.        !     *~
            * 02/12/93 ! PRR 11695 CH(8) now CH(6) @ LINE 14180.  ! JIM *~
            * 02/12/93 ! PRR 12460 Option to net/include Cr Memos.! JIM *~
            * 02/12/93 ! PRR 11031 Now prints Part Description.   ! JIM *~
            * 02/12/93 ! PRR 11031 Squoze Customer # on Detail rpt! JIM *~
            * 05/25/94 ! Essentially a rewrite, many new options. ! HES *~
            *       1) ! Run for invoices OR SALES ORDERS.        !     *~
            *          ! Has three modes now - A) Billings        !     *~
            *          !                       B) Bookings        !     *~
            *          !                       C) Backlog         !     *~
            *       2) ! Includ hdr disc in calc to make true net.!     *~
            *       3) ! Added Customer Range selection           !     *~
            *       4) ! Added option to get costs from STD files !     *~
            *       5) ! Can now select TYPE of invoices to includ!     *~
            *       6) ! Added BUNCH of Sort selections           !     *~
            *       7) ! Optional extended detail mode.           !     *~
            *       8) ! Uses CONSEC rather then INDEXED workfile,!     *~
            *          !  will run much faster.                   !     *~
            *       9) ! Major cleanup, internal & external,      !     *~
            *          !  including shortening report, verbage.   !     *~
            * 11/28/95 ! PRR 13438.  Added Document Range.        ! JDH *~
            *          ! PRR 13532.  Fixed Sub-total printing.    !     *~
            * 09/05/96 ! Millie date conversion                   ! DER *~
            * 04/02/97 ! Added return code to PUTPARM call for NT ! LDJ *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            blankdate$8,                 /* blank unfmt date           */~
            breaks$1,                    /* Print Page Breaks w/totals */~
            cat$4,                       /* Category Code              */~
            cursor%(2),                  /* Cursor Location For Edit   */~
            cuscode$9,                   /* Customer Code              */~
            cusdescr$30,                 /* Customer Description       */~
            currentacct$12,              /* sls acct For This Printline*/~
            currentcat$4,                /* Category For This Printline*/~
            currentcus$9,                /* Customer For This Printline*/~
            currentdate$8,               /* Inv Date For This Printline*/~
            currentpart$25,              /* Part For This Printline    */~
            currentstr$3,                /* Store No For This Printline*/~
            date$8,                      /* Date For Screen Display    */~
            descr$32,                    /* Part Description           */~
            detail$1,                    /* Show Inv Lines Yes Or No   */~
            displayacct$12,              /* sls acct To Print          */~
            displaycat$4,                /* Category To Print          */~
            displaycus$9,                /* Customer To Print          */~
            displaydate$8,               /* Inv Date To Print          */~
            displaypart$25,              /* Part Num To Print          */~
            displaystr$3,                /* Store No To Print          */~
            dtype$1,                     /* Data Source                */~
            dtyped1$7,                   /* Data Source Description    */~
            dtyped2$8,                   /* Data Source Description    */~
            edtmessage$79,               /* Edit Screen Message        */~
            errormsg$79,                 /* Error Message              */~
            firstdate$8,                 /* Earliest Date For Report   */~
            firstcat$4,                  /* First Category (For Screen)*/~
            firstcus$9,                  /* First Customer (For Screen)*/~
            firstdoc$(16,1)1,            /* First Document (For Screen)*/~
            firststr$3,                  /* First Store No (For Screen)*/~
            fcat$4,                      /* First Cat In Range         */~
            fcus$9,                      /* First Cus In Range         */~
            fdate$10,                    /* Fromdate unformatted       */~
            fdoc$16,                     /* First Doc In Range         */~
            fpart$25,                    /* First Part In Range        */~
            fstr$3,                      /* First str In Range         */~
            firstpart$25,                /* First Part (For Screen)    */~
            fromdate$10,                 /* First Date For Workfile    */~
            gppercent$6,                 /* Gross Profit Percent       */~
            gpct$8,                      /* Work Variable              */~
            gpdl$8,                      /* Work Variable              */~
            grs$8,                       /* Work Variable              */~
            i$(24)80,                    /* Screen Image               */~
            in_file$8,                   /* Cost Set File Name         */~
            invnumber$10,                /* Invoice Number             */~
            invdate$8,                   /* Date Of Invoice            */~
            ivctype$1,                   /* Reclines Adj Records       */~
            lastcat$4,                   /* Last Category (For Screen) */~
            lastcus$9,                   /* Last Customer (For Screen) */~
            lastdoc$(16,1)1,             /* Last Document (For Screen) */~
            laststr$3,                   /* Last Store No (For Screen) */~
            lcat$4,                      /* Last Cat In Range          */~
            lcus$9,                      /* Last Cus In Range          */~
            ldoc$16,                     /* Last Doc In Range          */~
            lpart$25,                    /* Last Part In Range         */~
            lstr$3,                      /* Last Str In Range          */~
            lastpart$25,                 /* Last Part (For Screen)     */~
            line2$79,                    /* Screen Line #2             */~
            inpmessage$79,               /* Input Message              */~
            lastdate$8,                  /* Last Date For Report       */~
            name$60,                     /* Company Name Field         */~
            net$8,                       /* Work Variable              */~
            lineplowkey$65,              /* Plowkey For Reclines       */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            netgross$1,                  /* Use Gross Or Net Price     */~
            part$25,                     /* Full Part Number           */~
            pf$(3)79,                    /* PF Key Strings             */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$50,                  /* Plowkey For Recmastr       */~
            profitflag$1,                /* Use Margin Or Markup       */~
            prompt$(4)24,                /* Variable Screen Prompts    */~
            rpttitle$80,                 /* Title For Report Header    */~
            qtyname$8,                   /* Title For Report Header    */~
            scrnmsg$(3)50,               /* Select descriptions        */~
            seq$3,                       /* Reclines Sequence Number   */~
            set$8,                       /* Override Cost Set          */~
            setdescr$32,                 /* Override Cost Set Decsriptn*/~
            slsacct$12,                  /* Sales Account              */~
            sort1$4,                     /* Primary Sort Selected      */~
            sort2$4,                     /* Secondary Sort Selected    */~
            sortkey$88,                  /* For Work File              */~
            sortopt$21,                  /* Report title Field         */~
            str$3,                       /* Store Number               */~
            subhead$90,                  /* Sub Heading                */~
            time$8,                      /* Report Run Time            */~
            titl$(3)25,                  /* Screen title line          */~
            todate$10,                   /* Second Date For Workfile   */~
            totallit$70,                 /* Literal string for total   */~
            total(4,8),                  /* Totals                     */~
            total$(6)10,                 /* Totals                     */~
            typ$(9,1)1,                  /* Invoice Types To Include   */~
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
            * #5  ! BCKMASTR ! Backlog master file                      *~
            * #6  ! BCKLINES ! Back Log Line Item File                  *~
            * #8  ! WORKFILE ! Workfile built from ARILINES             *~
            * #10 ! STCHNY   ! Standard Cost Set- Inventory Standards   *~
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

            select #5,  "BCKMASTR",                                      ~
                        varc,     indexed,  recsize =  1000,             ~
                        keypos =    1, keylen =  25

            select #6,  "BCKLINES",                                      ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =  10,  keylen = 19

            select #8,  "WORKFILE",                                      ~
                        varc, consec, recsize = 173

            select #10, "STCHNY",                                        ~
                        varc,     indexed,  recsize = 500,               ~
                        keypos = 1,    keylen = 25

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

            call "OPENCHCK" (#1,  0%, f2%(6%),  0%, rslt$(1%))
            call "OPENCHCK" (#2,  0%, f2%(6%),  0%, rslt$(2%))
            call "OPENCHCK" (#3,  0%, f2%(6%),  0%, rslt$(3%))
            call "OPENCHCK" (#4,  0%, f2%(6%),  0%, rslt$(4%))
            call "OPENCHCK" (#5,  0%, f2%(6%),  0%, rslt$(5%))
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
            call "COMPNAME" (12%, name$, u3%)
            str(line2$,62) = " SAGPRPT: " & str(cms2v$,,8)
            call "EXTRACT" addr("ID", userid$)

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

            for fieldnr% = 1 to 13
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
                  errormsg$ = " "
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 14 then       build_work_file
                  if keyhit%  = 16 then       build_work_file
                  if keyhit% <>  0 then       editpg1
L11110:     fieldnr% = cursor%(1%) - 4%
            if fieldnr% < 1% or fieldnr% > 15% then editpg1
            if fieldnr% = 2% or fieldnr% =  3% then editpg1
            if fieldnr% > 1% then fieldnr% = fieldnr% - 2%
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
            if keyhit% = 14% then view% = 1% else view% = 0%
            recnbr% = max((val(str(rslt$(2),17,4),4)/50%), 1000%)
            call "WORKOPEN"(#8, "OUTPT", recnbr%, f2%(8))
            call "SHOSTAT" ("Selecting " & dtyped1$ & " Details...")
            fdate$ = fromdate$ : tdate$ = todate$
            call "DATUFMTC" (fdate$) : call "DATUFMTC" (tdate$)
            sortopt$ = "SORT BY " & sort1$
            if sort2$ = " " then L14140
               sortopt$ = sortopt$ & " AND " & sort2$
L14140:     call "RJUSTIFY" (sortopt$)
            dswap% = 0%
            if sort1$ <> "DESC" then L14180
               dswap% = 1% : sort1$ = "PART"
L14180:     if sort2$ <> "DESC" then L14200
               dswap% = 1% : sort2$ = "PART"
L14200:     plowkey$ = fcus$
            fm% = 1% : if dtype$ = "O" then fm% = 5%
            fl% = 2% : if dtype$ = "O" then fl% = 6%

*       *  Now march through the MASTER file looking for records that
*       *  fall within the ranges selected.

L14270:     call "PLOWNEXT" (#fm%, plowkey$, 0%, f1%(fm%))
               if f1%(fm%) = 0% then process_and_print
            if str(plowkey$,,9) > lcus$ then process_and_print
            if fm% = 5% then L14410

            REM Invoices...
            get #1 using L14350, cuscode$, invnumber$, invdate$, hdiscpct,~
                                str$, ivctype$
L14350:     FMT CH(9), CH(8), POS(521), CH(6), POS(801), PD(14,4),       ~
                POS(870), CH(3), POS(891), CH(1)
            if pos(str(typ$())=ivctype$) = 0% then L14270
            if invdate$ < fdate$ or invdate$ > tdate$ then L14270
            goto L14510

L14410:     REM Sales Orders...
            get #5 using L14440, cuscode$, sonumber$, str$, hdiscpct,     ~
                                openamt
L14440:     FMT CH(9), CH(16), POS(803), CH(3), POS(859), 2*PD(14,4)
            if typ$(1,1) = "N" and openamt = 0 then L14270
            invnumber$ = sonumber$
            i% = len(sonumber$)
            if i% < 11% then L14510
               invnumber$ = str(sonumber$,i%-9%) /*Get signfcnt digits*/

L14510:     REM Things cum together here...
            if str$       <= fstr$  or str$       > lstr$  then L14270
            if cuscode$   <= fcus$  or cuscode$   > lcus$  then L14270
            if invnumber$ <= fdoc$  or invnumber$ > ldoc$  then L14270

*       * For each qualified master, load up the line items

            lineplowkey$ = str(plowkey$,,17%) & hex(00): br% = 17%
            if fl%=6% then lineplowkey$ = str(plowkey$,10%,16%) & hex(00)
            if fl%=6% then br% = 16%

L14610:     call "PLOWNEXT" (#fl%, lineplowkey$, br%, f1%(fl%))
               if f1%(fl%) = 0 then L14270  /* on to the next master */
            if fl% = 6% then L14750

            REM Invoices Lines...
            get #2, using L14690, seq$, part$, descr$, cat$, quanship,    ~
                                 unitsell, discamt, extension, slsacct$, ~
                                 extcost
L14690:         FMT XX(17), CH(3), POS(24), CH(25), CH(32), CH(4),       ~
                    POS(93), PD(14,4), POS(109), PD(14,4), POS(149),     ~
                    2*PD(14,4), XX(1), CH(9), POS(630), PD(14,4)
            if quanship = 0 then L14610
            goto L14910

L14750:     REM Sales Order Lines...
            get #6, using L14790, seq$, part$, descr$, cat$, orderqty,    ~
                                 quanship, unitsell, discamt, slsacct$,  ~
                                 invdate$
L14790:         FMT XX(28), CH(3), CH(25), CH(32), CH(4), PD(14,4),      ~
                    XX(8), PD(14,4), XX(24), PD(14,4), XX(24), PD(14,4), ~
                    XX(1), CH(9), POS(212), CH(6)
            if invdate$ < fdate$ or invdate$ > tdate$ then L14610
            if typ$(1,1) = "N" and  quanship = 0 then L14610
            if typ$(1,1) = "Y" then quanship = orderqty
            extension = quanship * unitsell
            if discamt = 0 then L14890 /* really a % at this point */
              discamt = -(extension * discamt) / 100
              extension = extension + discamt
L14890:     extcost = 0

L14910:     REM We cum together here...
            if part$ = "  " then goto L14610  /* Don't want these memos */
            if part$ <= fpart$ or part$ > lpart$ then L14610
            if cat$  <= fcat$  or cat$  > lcat$  then L14610
            if set$ = " " then L15000
               REM They Chose To Override Costs...
               call "STCCOSTS" (part$, set$, #3, 1%, stdcost)
               extcost = stdcost * quanship

L15000:     if dswap% = 0% then L15050
               inpmessage$ = descr$
               descr$ = part$
               part$ = inpmessage$

L15050:     REM Factor in header discount to show true net...
            hdiscamt  = round(hdiscpct * extension * .01, 2)
            discamt   = discamt   - hdiscamt
            extension = extension - hdiscamt
            gross = quanship * unitsell

            total(1%,2%) = extcost
            total(1%,3%) = extension
            total(1%,5%) = gross
            i% = 1% : gosub compute_gp

            REM Set up sort area of key...
            gppercent = min(max(-99999999, gppercent), 99999999)
            gpdollars = min(max(-99999999, gpdollars), 99999999)
            gross     = min(max(-99999999, gross    ), 99999999)
            extension = min(max(-99999999, extension), 99999999)
            gppercent = gppercent + 100000000
            gpdollars = gpdollars + 100000000  /* Deal with */
            gross     = gross     + 100000000  /* Negatives */
            extension = extension + 100000000
            gross     = 200000000  - gross       /* Reverese  */
            extension = 200000000  - extension   /* Order.    */
            put gpct$ using L15310, gppercent
            put gpdl$ using L15310, gpdollars
            put grs$  using L15310, gross
            put net$  using L15310, extension
L15310:     FMT PD(14,4)
            u1%, u2%, u3%, u4%, u5%, u6%, u7%, u8%, u9%, u0% = 1%
            if sort1$ <> "CAT"  then L15350
               sortkey$ = cat$     : next% =  5% : u1%=0%
L15350:     if sort1$ <> "PART" then L15370
               sortkey$ = part$    : next% = 26% : u2%=0%
L15370:     if sort1$ <> "CUS"  then L15390
               sortkey$ = cuscode$ : next% = 10% : u3%=0%
L15390:     if sort1$ <> "DATE" then L15410
               sortkey$ = invdate$ : next% =  7% : u4%=0%
L15410:     if sort1$ <> "STOR" then L15430
               sortkey$ = str$     : next% =  4% : u5%=0%
L15430:     if sort1$ <> "ACCT" then L15450
               sortkey$ = slsacct$ : next% = 10% : u0%=0%
L15450:     if sort2$ <> "CAT"  then L15470
               str(sortkey$,next%) = cat$     : next%=next% +  4%: u1%=0%
L15470:     if sort2$ <> "PART" then L15490
               str(sortkey$,next%) = part$    : next%=next% + 25%: u2%=0%
L15490:     if sort2$ <> "CUS"  then L15510
               str(sortkey$,next%) = cuscode$ : next%=next% +  9%: u3%=0%
L15510:     if sort2$ <> "DATE" then L15530
               str(sortkey$,next%) = invdate$ : next%=next% +  6%: u4%=0%
L15530:     if sort2$ <> "STOR" then L15550
               str(sortkey$,next%) = str$     : next%=next% +  3%: u5%=0%
L15550:     if sort2$ <> "GP% " then L15570
               str(sortkey$,next%) = gpct$    : next%=next% +  8%: u6%=0%
L15570:     if sort2$ <> "GP$ " then L15590
               str(sortkey$,next%) = gpdl$    : next%=next% +  8%: u7%=0%
L15590:     if sort2$ <> "GRS$" then L15610
               str(sortkey$,next%) = grs$     : next%=next% +  8%: u8%=0%
L15610:     if sort2$ <> "NET$" then L15630
               str(sortkey$,next%) = net$     : next%=next% +  8%: u9%=0%
L15630:     if sort2$ <> "ACCT" then L15650
               str(sortkey$,next%) = slsacct$ : next%=next% +  9%: u0%=0%
L15650:     sortlen% = next% - 1%
            for i% = 1% to 8%
                if u1%=0% then L15690
                   str(sortkey$,next%)=cat$    :next%=next%+ 4% : u1%=0%
L15690:         if u2%=0% then L15710
                   str(sortkey$,next%)=part$   :next%=next%+25% : u2%=0%
L15710:         if u3%=0% then L15730
                   str(sortkey$,next%)=cuscode$:next%=next%+ 9% : u3%=0%
L15730:         if u4%=0% then L15750
                   str(sortkey$,next%)=invdate$:next%=next%+ 6% : u4%=0%
L15750:         if u5%=0% then L15770
                   str(sortkey$,next%)=str$    :next%=next%+ 3% : u5%=0%
L15770:         if u6%=0% then L15790
                   str(sortkey$,next%)=gpct$   :next%=next%+ 8% : u6%=0%
L15790:         if u7%=0% then L15810
                   str(sortkey$,next%)=gpdl$   :next%=next%+ 8% : u7%=0%
L15810:         if u8%=0% then L15830
                   str(sortkey$,next%)=grs$    :next%=next%+ 8% : u8%=0%
L15830:         if u9%=0% then L15870
                   str(sortkey$,next%)=net$    :next%=next%+ 8% : u9%=0%
                if u0%=0% then L15870
                   str(sortkey$,next%)=slsacct$:next%=next%+ 9% : u0%=0%
L15870:     next i%

            write #8, using L15910, sortkey$, invnumber$, seq$, quanship, ~
                            unitsell, discamt, extcost, descr$
L15910:     FMT CH(88), CH(10), CH(3), 4*PD(14,4), CH(32)
            wfrec% = wfrec% + 1
            goto L14610    /* Go plow for next line item */

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            * Sets defaults and enables fields for page 1 of input.     *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20250,         /* Orders Or Invoice*/~
                                    L20300,         /* Date Range       */~
                                    L20850,         /* Document Range   */~
                                    L20360,         /* Customer Range   */~
                                    L20400,         /* Category Range   */~
                                    L20440,         /* Part Range       */~
                                    L20480,         /* Store Range      */~
                                    L20520,         /* Show detail?     */~
                                    L20570,         /* % Calc flag      */~
                                    L20610,         /* Which profit     */~
                                    L20650,         /* Override Cost Set*/~
                                    L20700,         /* Inv Types To Incl*/~
                                    L20750          /* Sort Options     */

                  if enabled% <> 0% then return
                     errormsg$ = hex(84) & "Edit on selected field not al~
        ~lowed - To Change, START OVER"
                  return

L20250: REM Default/enable for Orders Or Invoices
            if dtype$ = " " then dtype$ = "I"
            if editmode% = 1% then enabled% = 0%  /*bcause of othr opts*/
            return

L20300: REM Default/enable for date range to report on
            if fromdate$ <> " " and fromdate$ <> blankdate$ then return
            fromdate$ = "19010101"
            call "DATECONV" (fromdate$)
            call "DATFMTC"  (fromdate$)
            todate$ = date
            call "DATFMTC"  (todate$)
            return

L20360: REM Default/enable for Customer Range
            if firstcus$ = " " then firstcus$ = "ALL"
            return

L20400: REM Default/enable for Category Range
            if firstcat$ = " " then firstcat$ = "ALL"
            return

L20440: REM Default/enable for Part Number Range
            if firstpart$ = " " then firstpart$ = "ALL"
            return

L20480: REM Default/enable for Store Number Range
            if firststr$ = " " then firststr$ = "ALL"
            return

L20520: REM Default/enable for show detail (y or n)
            if editmode% = 1% then enabled% = 0%  /*cause of sort opts*/
            if detail$ = " " then detail$ = "Y"
            return

L20570: REM Default/enable for calc. profit as marg or m/u
            if profitflag$ = " " then profitflag$ = "S"
            return

L20610: REM Default/enable for how to calculate profit
            if netgross$ = " " then netgross$ = "N"
            return

L20650: REM Default/enable for Override Cost Set
            if dtype$ = "O" and set$ = " " then                          ~
               call "STCSETID" (1%, #3, set$, " ")
            return

L20700: REM Default/enable for Invoice Types To Include
            if dtype$ = "O" and typ$(1,1) = " " then typ$() = "N"
            if typ$() = " " then typ$() = "OMDACX"
            return

L20750: REM Default/enable for Include adjustments
            if sort1$ <> " " then return
               sort1$ = "CAT"
               sort2$ = "PART"
               breaks$ = "Y"
               if detail$ = "N" then breaks$ = "N"
               return

L20850: REM Default/enable for Document Range
            if str(firstdoc$()) = " " then str(firstdoc$()) = "ALL"
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
            goto L28360

        scrn1_msg  :  data                                               ~
         "Enter 'O' for Orders (Bookings) or 'I' for Invoices (Billings) ~
        ~Analysis.",                                                      ~
         "Enter Beginning and Ending Date for Selection                ",~
         "Enter Beginning and Ending Document Number Selection or 'ALL'",~
         "Enter Beginning and Ending Customer Code or 'ALL'            ",~
         "Enter Beginning and Ending Category Code or 'ALL'            ",~
         "Enter Beginning and Ending Part Number or 'ALL'              ",~
         "Enter Beginning and Ending Store Number or 'ALL'             ",~
         "'Y' to Print Details, 'N' for Summary, 'E' For Extra Detail. ",~
         "Should Profit % be a function of 'S' Selling Price or 'C' Cost"~
        ,                                                                ~
         "Should Profit be calculated from 'N' net price or 'G' gross sal~
        ~e?",                                                             ~
         "Instead of Using Cost On Invoice, You Can OPTIONALY Use Cost Fr~
        ~om a Cost Set",                                                  ~
         "O=OnOrder,M=Manual,D=Direct,A=Adjust,C=Credit,X=Export,G=Genera~
        ~ted,F=FinancChrg",                                               ~
         "Sortby: CAT PART CUS DATE STOR DESC ACCT. Thenby: same+ GP% GP$~
        ~ GRS$ NET$ blank"

L28360:     if dtype$ <> "O" then return
            if fieldnr% = 11% then inpmessage$ =                         ~
              "Enter Standard Cost Set For Cost Determination."
            if fieldnr% = 12% then inpmessage$ =                         ~
              "Enter 'Y' For Bookings Analysis, or 'N' For Backlog Only"
        return

L29000: REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

            init(" ") errormsg$, inpmessage$, detail$, firstcat$, sort1$,~
                      firstpart$, netgross$, profitflag$, lastcat$, set$,~
                      lastpart$, lastdate$, firstdate$, lastdate$,       ~
                      scrnmsg$(), firstcus$, lastcus$, setdescr$, sort2$,~
                      firststr$, laststr$, breaks$, prompt$(), typ$(),   ~
                      fromdate$, todate$, firstdoc$(), lastdoc$()
            call "FILEBGON" (#8)
            mat total = zer
            last_cuscode$ = all(hex(ff))
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
            call "SHOSTAT" ("Printing Gross Profit Report")
            select printer (134)
            call "SETPRNT" ("SA0001", "SAGPRPT", wfrec%, 0%)
            if view% = 1% then call "SET" addr("PM", "K")

            subhead$ = "***** PROFIT CALCULATED USING"
            if netgross$ = "N" then subhead$ = subhead$ & " NET SALES"   ~
                               else subhead$ = subhead$ & " GROSS SALES"
            subhead$ = subhead$ & " *****"
            call "STRING" addr("CT", subhead$, 90%)
            rpttitle$ = "*** DETAIL"
            if detail$ = "N" then rpttitle$ = "*** SUMMARY"
            rpttitle$ = rpttitle$ & " " & dtyped2$ & " GROSS PROFIT ***"
            call "FMTTITLE" (rpttitle$, " ", 12%)
            qtyname$ = "QTY SOLD"
            if dtype$= "O" and typ$(1,1) = "N" then qtyname$ = "QTY OPEN"
            if dtype$= "O" and typ$(1,1) = "Y" then qtyname$ = "ORDR QTY"
            close #8
            call "WORKOPN2" (#8, "INPUT", 0%, f2%(8))

        REM SET THE COUNTERS, ETC.
            page% = -1%
            gosub print_params
            line% = 99%
            printed% = 0%  /* Set to 1 if we printed anything */

*       * Now begin plowing the work file for the report
L30410:     call "READNEXT" (#8, f1%(8))
               if f1%(8) = 0 then exit_path
            get #8, using L15910, sortkey$, invnumber$, seq$, quanship,   ~
                          unitsell, discamt, extcost, descr$

            REM Extract sort area of key...
            u1%, u2%, u3%, u4%, u5%, u6%, u7%, u8%, u9%, u0% = 1%
            if sort1$ <> "CAT"  then L30500
               cat$     = sortkey$ : next% =  5% : u1%=0%
L30500:     if sort1$ <> "PART" then L30520
               part$    = sortkey$ : next% = 26% : u2%=0%
L30520:     if sort1$ <> "CUS"  then L30540
               cuscode$ = sortkey$ : next% = 10% : u3%=0%
L30540:     if sort1$ <> "DATE" then L30560
               invdate$ = sortkey$ : next% =  7% : u4%=0%
L30560:     if sort1$ <> "STOR" then L30580
               str$     = sortkey$ : next% =  4% : u5%=0%
L30580:     if sort1$ <> "ACCT" then L30600
               slsacct$ = str(sortkey$,,9%): next% = 10% : u0%=0%
L30600:     if sort2$ <> "CAT"  then L30620
               cat$     = str(sortkey$,next%) : next%=next% +  4%: u1%=0%
L30620:     if sort2$ <> "PART" then L30640
               part$    = str(sortkey$,next%) : next%=next% + 25%: u2%=0%
L30640:     if sort2$ <> "CUS"  then L30660
               cuscode$ = str(sortkey$,next%) : next%=next% +  9%: u3%=0%
L30660:     if sort2$ <> "DATE" then L30680
               invdate$ = str(sortkey$,next%) : next%=next% +  6%: u4%=0%
L30680:     if sort2$ <> "STOR" then L30700
               str$     = str(sortkey$,next%) : next%=next% +  3%: u5%=0%
L30700:     if sort2$ <> "GP% " then L30720
               gpct$    = str(sortkey$,next%) : next%=next% +  8%: u6%=0%
L30720:     if sort2$ <> "GP$ " then L30740
               gpdl$    = str(sortkey$,next%) : next%=next% +  8%: u7%=0%
L30740:     if sort2$ <> "GRS$" then L30760
               grs$     = str(sortkey$,next%) : next%=next% +  8%: u8%=0%
L30760:     if sort2$ <> "NET$" then L30780
               net$     = str(sortkey$,next%) : next%=next% +  8%: u9%=0%
L30780:     if sort2$ <> "ACCT" then L30800
               slsacct$ = str(sortkey$,next%) : next%=next% +  9%: u0%=0%
L30800:     for i% = 1% to 8%
                if u1%=0% then L30830
                   cat$    =str(sortkey$,next%):next%=next%+ 4% : u1%=0%
L30830:         if u2%=0% then L30850
                   part$   =str(sortkey$,next%):next%=next%+25% : u2%=0%
L30850:         if u3%=0% then L30870
                   cuscode$=str(sortkey$,next%):next%=next%+ 9% : u3%=0%
L30870:         if u4%=0% then L30890
                   invdate$=str(sortkey$,next%):next%=next%+ 6% : u4%=0%
L30890:         if u5%=0% then L30910
                   str$    =str(sortkey$,next%):next%=next%+ 3% : u5%=0%
L30910:         if u6%=0% then L30930
                   gpct$   =str(sortkey$,next%):next%=next%+ 8% : u6%=0%
L30930:         if u7%=0% then L30950
                   gpdl$   =str(sortkey$,next%):next%=next%+ 8% : u7%=0%
L30950:         if u8%=0% then L30970
                   grs$    =str(sortkey$,next%):next%=next%+ 8% : u8%=0%
L30970:         if u9%=0% then L30990
                   net$    =str(sortkey$,next%):next%=next%+ 8% : u9%=0%
L30990:         if u0%=0% then L31010
                   slsacct$=str(sortkey$,next%):next%=next%+ 9% : u0%=0%
L31010:     next i%
            get gpct$ using L15310, gppercent1
            get gpdl$ using L15310, gpdollars1
            get grs$  using L15310, gross
            get net$  using L15310, extension
            gross      = abs(gross      - 200000000)
            extension  = abs(extension  - 200000000)
            gppercent1 = gppercent1 - 100000000
            gpdollars1 = gpdollars1 - 100000000
            gross      = gross      - 100000000
            extension  = extension  - 100000000

*       * We caught one so let's do it to it
            call "DATEFMT" (invdate$)
            call "GLFMT" (slsacct$)
            if printed% > 0% then L31260
               REM Don't trigger subtotal on very first record
               currentpart$ = part$
               currentcat$  = cat$
               currentcus$  = cuscode$
               currentdate$ = invdate$
               currentstr$  = str$
               currentacct$ = slsacct$
L31260:     REM See if we need to subtotal...
            if sort1$ = "CAT"  and currentcat$  <> cat$     then L31400
            if sort1$ = "CUS"  and currentcus$  <> cuscode$ then L31400
            if sort1$ = "PART" and currentpart$ <> part$    then L31400
            if sort1$ = "DATE" and currentdate$ <> invdate$ then L31400
            if sort1$ = "STOR" and currentstr$  <> str$     then L31400
            if sort1$ = "ACCT" and currentacct$ <> slsacct$ then L31400
            if sort2$ = "CAT"  and currentcat$  <> cat$     then L31400
            if sort2$ = "CUS"  and currentcus$  <> cuscode$ then L31400
            if sort2$ = "PART" and currentpart$ <> part$    then L31400
            if sort2$ = "DATE" and currentdate$ <> invdate$ then L31400
            if sort2$ = "STOR" and currentstr$  <> str$     then L31400
            if sort2$ = "ACCT" and currentacct$ <> slsacct$ then L31400
            goto L31420
L31400:        REM Need to print totals...
               gosub print_sub_totals
L31420:     printed% = 1%
            currentpart$ = part$
            currentcat$  = cat$
            currentcus$  = cuscode$
            currentdate$ = invdate$
            currentstr$  = str$
            currentacct$ = slsacct$

*       * Increment part totals
            total(2%,1%) = total(2%,1%) + quanship
            total(2%,2%) = total(2%,2%) + extcost
            total(2%,3%) = total(2%,3%) + extension
            total(2%,4%) = total(2%,4%) + gpdollars
            total(2%,5%) = total(2%,5%) + gross
            total(2%,6%) = total(2%,6%) + discamt
            total(2%,7%) = total(2%,7%) + 1

*       * Increment category totals
            total(3%,1%) = total(3%,1%) + quanship
            total(3%,2%) = total(3%,2%) + extcost
            total(3%,3%) = total(3%,3%) + extension
            total(3%,4%) = total(3%,4%) + gpdollars
            total(3%,5%) = total(3%,5%) + gross
            total(3%,6%) = total(3%,6%) + discamt
            total(3%,7%) = total(3%,7%) + 1

*       * Increment grand totals
            total(4%,1%) = total(4%,1%) + quanship
            total(4%,2%) = total(4%,2%) + extcost
            total(4%,3%) = total(4%,3%) + extension
            total(4%,4%) = total(4%,4%) + gpdollars
            total(4%,5%) = total(4%,5%) + gross
            total(4%,6%) = total(4%,6%) + discamt
            total(4%,7%) = total(4%,7%) + 1

            if detail$ = "N" then  L30410

            REM Print the detail record...
            total$() = " "
            call "CONVERT" (quanship,   0.2, str(total$(1%),,8%))
            call "CONVERT" (extcost,    2.2, total$(2%))
            call "CONVERT" (extension,  2.2, total$(3%))
            call "CONVERT" (gpdollars1, 2.2, str(total$(4%),,9%))
            call "CONVERT" (gross,      2.2, total$(5%))
            call "CONVERT" (discamt,    2.2, str(total$(6%),,8%))
            call "CONVERT" (gppercent1, 1.1, gppercent$)

            if line% > 55% then gosub print_header
            print using L60350, cat$, part$, cuscode$, invdate$, str$,    ~
                               invnumber$, total$(1), total$(5),         ~
                               total$(6), total$(3), total$(2),          ~
                               total$(4), gppercent$
            if detail$ <> "E" then L32020
               line% = line% + 2%
               if cuscode$ = last_cuscode$ then L32000
                  call "DESCRIBE" (#4, cuscode$, cusdescr$, 0%, f1%(4))
                  if f1%(4) = 0% then cusdescr$ = "*UNDEFINED CUS CODE*"
                  last_cuscode$ = cuscode$
L32000:        print using L60390, descr$, seq$, slsacct$, cusdescr$
               print skip(1)
L32020:     line% = line% + 1%
            goto L30410       /* Get another one    */

*       * Now print part totals as required
        print_sub_totals
            if sort2$ = " "    then L33110
            if sort2$ = "GP%"  then L33110
            if sort2$ = "GP$"  then L33110
            if sort2$ = "GRS$" then L33110
            if sort2$ = "NET$" then L33110
            i% = 2%
            gosub compute_gp
            gosub print_total_line

L33110:     if sort1$ = "CAT"  and currentcat$  = cat$     then return
            if sort1$ = "CUS"  and currentcus$  = cuscode$ then return
            if sort1$ = "PART" and currentpart$ = part$    then return
            if sort1$ = "DATE" and currentdate$ = invdate$ then return
            if sort1$ = "STOR" and currentstr$  = str$     then return
            if sort1$ = "ACCT" and currentacct$ = slsacct$ then return
            gosub print_mid_totals
            return

*       * Print routine for category totals
        print_mid_totals
            i% = 3%
            gosub compute_gp
            gosub print_total_line
            if breaks$ = "Y" then line% = 99%
            return

        print_grand_totals
            i% = 4%
            gosub compute_gp

            call "CONVERT" (total(i%, 1%), 0.0, str(total$(1%),,8%))
            call "CONVERT" (total(i%, 2%), 0.0, total$(2%))
            call "CONVERT" (total(i%, 3%), 0.0, total$(3%))
            call "CONVERT" (gpdollars,     0.0, str(total$(4%),,9%))
            call "CONVERT" (total(i%, 5%), 0.0, total$(5%))
            call "CONVERT" (total(i%, 6%), 0.0, str(total$(6%),,8%))
            call "CONVERT" (gppercent,     1.1, gppercent$)

            call "CONVERT" (total(4%,7%), -0.001, total$(1%))
            totallit$ = "***** REPORT TOTALS: " & total$(1%)
            call "CONVERT" (total(2%,8%), -0.001, total$(1%))
            totallit$ = totallit$ & " SALES FOR "
            if sort2$ = "CAT"  then temp$ = "CATEGORIES"
            if sort2$ = "CUS"  then temp$ = "CUSTOMERS"
            if sort2$ = "PART" then temp$ = "PARTS"
            if sort2$ = "DATE" then temp$ = "INV DATES"
            if sort2$ = "STOR" then temp$ = "STORES"
            if sort2$ = "ACCT" then temp$ = "SALESACCTS"
            if sort2$ = " "    then L33570
            if sort2$ = "GP% " then L33570
            if sort2$ = "GP$ " then L33570
            if sort2$ = "GRS$" then L33570
            if sort2$ = "NET$" then L33570
            totallit$ = totallit$ & " " & total$(1%)
            totallit$ = totallit$ & " " & temp$ & " IN"
L33570:     call "CONVERT" (total(3%,8%), -0.001, total$(1%))
            if sort1$ = "CAT"  then temp$ = "CATEGORIES"
            if sort1$ = "CUS"  then temp$ = "CUSTOMERS"
            if sort1$ = "PART" then temp$ = "PARTS"
            if sort1$ = "DATE" then temp$ = "IVC DATES"
            if sort1$ = "STOR" then temp$ = "STORES"
            if sort1$ = "ACCT" then temp$ = "SALESACCTS"
            totallit$ = totallit$ & " " & total$(1%) & " " & temp$

            if line% > 53 then gosub print_header
            print skip(1)
            print using L60530
            print using L60570, totallit$, total$(5%), total$(6%),        ~
                               total$(3%), total$(2%), total$(4%),       ~
                               gppercent$
            return

        print_total_line
            total(i%,4%) = gpdollars
            call "CONVERT" (total(i%, 1%), 0.0, str(total$(1),,8%))
            call "CONVERT" (total(i%, 2%), 0.0, total$(2%))
            call "CONVERT" (total(i%, 3%), 0.0, total$(3%))
            call "CONVERT" (total(i%, 4%), 0.0, str(total$(4%),,9%))
            call "CONVERT" (total(i%, 5%), 0.0, total$(5%))
            call "CONVERT" (total(i%, 6%), 0.0, str(total$(6),,8%))
            call "CONVERT" (gppercent,     1.1, gppercent$)

            if detail$ = "N" and i% = 2% then L33920
            if detail$ = "N" and i% = 3% and sort2$ = " " then L33920
               if line% > 52% then gosub print_header
               REM Don't total if only one detail
               if total(i%,7%) = 1 and detail$ <> "N" then L34300
                  line% = line% + 1%
                  print using L60490

L33920:     if line% > 53% then gosub print_header
            totallit$, displaycat$, displaycus$, displaypart$ = " "
            displaydate$, displaystr$, displayacct$ = " "
            if sort1$ = "CAT"  then displaycat$  = currentcat$
            if sort1$ = "CUS"  then displaycus$  = currentcus$
            if sort1$ = "PART" then displaypart$ = currentpart$
            if sort1$ = "DATE" then displaydate$ = currentdate$
            if sort1$ = "STOR" then displaystr$  = currentstr$
            if sort1$ = "ACCT" then displayacct$ = currentacct$
            call "CONVERT" (total(i%,7%), -0.01, str(totallit$,,4%))
            if i% = 2% then L34100
               if sort2$ = "CAT"  then displaycat$  = all("*")
               if sort2$ = "CUS"  then displaycus$  = all("*")
               if sort2$ = "PART" then displaypart$ = all("*")
               if sort2$ = "DATE" then displaydate$ = all("*")
               if sort2$ = "STOR" then displaystr$  = all("*")
               if sort2$ = "ACCT" then displayacct$ = all("*")
               goto L34170
L34100:     if sort2$ = "CAT"  then displaycat$  = currentcat$
            if sort2$ = "CUS"  then displaycus$  = currentcus$
            if sort2$ = "PART" then displaypart$ = currentpart$
            if sort2$ = "DATE" then displaydate$ = currentdate$
            if sort2$ = "STOR" then displaystr$  = currentstr$
            if sort2$ = "ACCT" then displayacct$ = currentacct$

L34170:     totallit$ = totallit$ & " SALES"
            print using L60350, displaycat$, displaypart$, displaycus$,   ~
                               displaydate$, displaystr$, totallit$,     ~
                               total$(1%), total$(5%), total$(6%),       ~
                               total$(3%), total$(2%), total$(4%),       ~
                               gppercent$
            line% = line% + 1%
            if displayacct$ = " " then L34280
               print using L60390, " ", " ", displayacct$
               line% = line% + 1%    /* DETAIL$ must be 'E' 2b here */

L34280:     if detail$ = "N" and i% = 2% then L34320
            if detail$ = "N" and i% = 3% and sort2$ = " " then L34320
L34300:        line% = line% + 1%
               print skip(1)
L34320:     total(i%,1%), total(i%,2%), total(i%,3%), total(i%,4%) = 0
            total(i%,5%), total(i%,6%), total(i%,7%) = 0
            total(i%,8%) = total(i%,8%) + 1%
        return

        print_header
            print page
            page% = page% + 1%
            line% = 7%
            if set$ = " " then set$ = "n/a"
            time$ = " " : call "TIME" (time$)
            print using L60060, date$, time$, name$
            print using L60100, userid$, rpttitle$, page%
            print using L60140, set$, subhead$, sortopt$
            print skip(1)
            mtype$ = "MARKUP"
            if profitflag$ = "S" then mtype$ = "MARGIN"
            if detail$ = "N" then goto L35250
            if detail$ = "E" then goto L35200

            print using L60170,dtyped1$,dtyped3$ /*Detail column header*/
            print using L60200, qtyname$, mtype$
            print using L60310
            return

L35200:     print using L60240,dtyped1$,dtyped3$ /*Extra Detail clmn hdr*/
            print using L60270, qtyname$, mtype$
            print using L60310
            return

L35250:     print using L60420, dtyped1$ /* Summary column headers    */
            print using L60450, qtyname$, mtype$
            print using L60310
            return

        compute_gp
            REM Compute Percentage, as requested...
            bases = total(i%,3%)
            if netgross$ <> "N" then bases = total(i%,5%)
            gpdollars = bases - total(i%,2%)
            gppercent = 0

            if profitflag$ <> "S" then L35420
               REM Margin...
               if bases <> 0 then gppercent = (gpdollars/bases) * 100
               goto L35450

L35420:     REM Markup...
            if total(i%,2%)<>0 then gppercent=(gpdollars/total(i%,2))*100

L35450:     REM Suppress potential pound signs...
            if bases < 0 then gppercent = gppercent * (-1)
            if gppercent < -999.9 then gppercent = -999.9
            if gppercent > 9999.9 then gppercent = 9999.9
        return

        exit_path
            init (hex(ff)) cat$, part$, cuscode$, invdate$, str$, slsacct$
            gosub print_sub_totals
            gosub print_grand_totals
            print skip(2)
            print using L60610   /* END OF REPORT */
            if view% = 1% then call "GETPRTNM" addr(file$, lib$, vol$)
            close printer
            if view% = 1% then gosub view_report
            goto inputmode

        print_params
            if view% <> 1% then L35610
                page% = page% + 1%
                return
L35610:     gosub print_header
L35612:     x% = pos(str(i$()) > hex(7f))
            if x% = 0% then L35630
               str(i$(),x%,1%) = " "
               goto L35612
L35630:     print skip(3)
            print tab(37);
            print "--------------------- Report Selection Parameters ----~
        ~---------"
            for x% = 5% to 19%: print tab(37); i$(x%) : next x%
            print tab(37);
            print "------------------------------------------------------~
        ~---------"
        return

        view_report
            REM Display printed file...
            close ws
            call "PUTPARM" addr("E", "INPUT   ",4%,                      ~
                     "FILE    ", file$, 8%, "LIBRARY ", lib$, 8%,        ~
                     "VOLUME  ", vol$, 6%, "ACCESS  ", "PRINT ", 6%, "@",ret%)
            call"LINK"addr("DISPLAY ","S"," "," ",0%," "," ",0%,"N",0%,ret%)
            call "SCRATCH" addr("F", file$, lib$, vol$, " ", " ", 0%)
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

              on fieldnr% gosub L40270,         /* Orders or invoices*/   ~
                                L40270,         /* Date Range        */   ~
                                L40270,         /* Document Range    */   ~
                                L40270,         /* Customer Range    */   ~
                                L40270,         /* Category Range    */   ~
                                L40270,         /* Part Range        */   ~
                                L40270,         /* Store Range       */   ~
                                L40270,         /* Show Detail?      */   ~
                                L40270,         /* Profit % Method   */   ~
                                L40270,         /* Which Profit      */   ~
                                L40270,         /* Override Costs?   */   ~
                                L40270,         /* Inv Types To Inclu*/   ~
                                L40270          /* Sort Options      */
              goto L40300

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40270:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40300:     accept                                                       ~
               at (01,02),                                               ~
                  "Gross Profit Reporting",                              ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "ORDERS or INVOICES?",                        ~
               at (05,27), fac(lfac$( 1)), dtype$               , ch(01),~
                                                                         ~
               at (07,02), fac(hex(ac)), titl$(1)               , ch(21),~
               at (07,27), fac(hex(ac)), titl$(2)               , ch(25),~
               at (07,55), fac(hex(ac)), titl$(3)               , ch(25),~
                                                                         ~
               at (08,02), fac(hex(8c)),   prompt$(1)           , ch(24),~
               at (08,27), fac(lfac$( 2)), fromdate$            , ch(10),~
               at (08,55), fac(lfac$( 2)), todate$              , ch(10),~
                                                                         ~
               at (09,02), fac(hex(8c)),   prompt$(4)           , ch(24),~
               at (09,27), fac(lfac$( 3)), str(firstdoc$())             ,~
               at (09,55), fac(lfac$( 3)), str(lastdoc$())              ,~
                                                                         ~
               at (10,02), "Customer Range",                             ~
               at (10,27), fac(lfac$( 4)), firstcus$            , ch(09),~
               at (10,55), fac(lfac$( 4)), lastcus$             , ch(09),~
                                                                         ~
               at (11,02), "Category Range",                             ~
               at (11,27), fac(lfac$( 5)), firstcat$            , ch(04),~
               at (11,55), fac(lfac$( 5)), lastcat$             , ch(04),~
                                                                         ~
               at (12,02), "Part Number Range",                          ~
               at (12,27), fac(lfac$( 6)), firstpart$           , ch(25),~
               at (12,55), fac(lfac$( 6)), lastpart$            , ch(25),~
                                                                         ~
               at (13,02), "Store Number Range",                         ~
               at (13,27), fac(lfac$( 7)), firststr$            , ch(03),~
               at (13,55), fac(lfac$( 7)), laststr$             , ch(03),~
                                                                         ~
               at (14,02), "Print Details?",                             ~
               at (14,27), fac(lfac$( 8)), detail$              , ch(01),~
               at (14,30), fac(hex(8c)),   scrnmsg$(1)          , ch(50),~
                                                                         ~
               at (15,02), "Profit % Calc Method",                       ~
               at (15,27), fac(lfac$( 9)), profitflag$          , ch(01),~
               at (15,30), fac(hex(8c)),   scrnmsg$(2)          , ch(50),~
                                                                         ~
               at (16,02), "Which Profit Base",                          ~
               at (16,27), fac(lfac$(10)), netgross$            , ch(01),~
               at (16,30), fac(hex(8c)),   scrnmsg$(3)          , ch(50),~
                                                                         ~
               at (17,02), fac(hex(8c)),   prompt$(2)           , ch(24),~
               at (17,27), fac(lfac$(11)), set$                 , ch(08),~
               at (17,37), fac(hex(8c)),   setdescr$            , ch(32),~
                                                                         ~
               at (18,02), fac(hex(8c)),   prompt$(3)           , ch(24),~
               at (18,27), fac(lfac$(12)), str(typ$()),                  ~
                                                                         ~
               at (19,02), "Sort By #### ,  Then By ####     Page Break",~
               at (19,10), fac(lfac$(13)), sort1$               , ch(04),~
               at (19,27), fac(lfac$(13)), sort2$               , ch(04),~
               at (19,47), fac(lfac$(13)), breaks$              , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40910
                  call "MANUAL" ("SAGPRPT ") : goto L40300

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
L41090:     if fieldnr% > 2% then L41110
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L41110:     return

L41130: if fieldnr% > 0% then L41220  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "    (14) View Report   (16)Print Report"
            pfkeys$ = hex(01ffffffffffffffffffffff0d0e0f1000)
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
            on fieldnr% gosub      L50220,          /* Orders or invoics*/~
                                   L50390,          /* Date Range       */~
                                   L52050,          /* Document Range   */~
                                   L50530,          /* Customer Range   */~
                                   L50580,          /* Category Range   */~
                                   L50630,          /* Part Range       */~
                                   L50680,          /* Store Range      */~
                                   L50730,          /* Show Detail      */~
                                   L50860,          /* Profit Calc ?    */~
                                   L50970,          /* Which Profit     */~
                                   L51080,          /* Override Cost set*/~
                                   L51340,          /* Inv Types To Incl*/~
                                   L51510           /* Sort Option      */
                     return

L50220: REM Test data for Orders Or Invoices
            if pos("IO" = dtype$) <> 0% then L50260
               errormsg$ = "Enter 'O' for Orders, or 'I' for invoices"
               return
L50260:     dtyped1$ = "INVOICE" : dtyped2$ = "BILLINGS"
            dtyped3$ = "INVOICE"
            prompt$(1) = "Invoice Date Range"
            prompt$(2) = "Override Costs? (SET)"
            prompt$(3) = "Invoice Types To Include"
            prompt$(4) = "Invoice Number Range"
            mat redim typ$(9%,1%)1%
            mat redim firstdoc$(8%,1%)1%
            mat redim  lastdoc$(8%,1%)1%
            if dtype$ = "I" then return
               dtyped1$ = "REQ DUE" : dtyped2$ = "BOOKINGS"
               dtyped3$ = "ORDER"
               prompt$(1) = "Order Due Date Range"
               prompt$(2) = "Cost Set"
               prompt$(3) = "Include Shipped Orders?"
               prompt$(4) = "Order Number Range"
               mat redim typ$(1%,1%)1%
               mat redim firstdoc$(16%,1%)1%
               mat redim  lastdoc$(16%,1%)1%
               return

L50390: REM Test data for Date Range
            if fromdate$ <> "ALL" then L50430
            fromdate$ = "19010101"
            todate$   = "20991231"
            call "DATECONV" (fromdate$)
            call "DATECONV" (todate$)

L50430:     call "DATEOKC" (fromdate$, firstdate%, errormsg$)
                if errormsg$ <> " " then return
            if todate$ = " " or todate$ = blankdate$ then ~
               todate$ = fromdate$
            call "DATEOKC" (todate$, lastdate%, errormsg$)
                if errormsg$ <> " " then return
            if firstdate% <= lastdate% then return
                errormsg$ = "Beginning Date must be earlier than Ending D~
        ~ate"
                return

L50530: REM Test data for Customer Code Range
              call "TESTRNGE" (firstcus$, lastcus$, fcus$, lcus$,        ~
                               errormsg$, #4)
              return

L50580: REM Test data for Category Code Range
              call "TESTRNGE" (firstcat$, lastcat$, fcat$, lcat$,        ~
                               errormsg$, #11)
              return

L50630: REM Test data for Part Number Range
            call "TESTRNGE" (firstpart$, lastpart$, fpart$, lpart$,      ~
                             errormsg$, #13)
            return

L50680: REM Test data for Store Number Range
            call "TESTRNGE" (firststr$, laststr$, fstr$, lstr$,          ~
                             errormsg$, #12)
            return

L50730: REM Test data show detail Y or N
            if detail$ <> "E" then L50770
                scrnmsg$(1) = "(Full Detailed Report)"
                return
L50770:     if detail$ <> "Y" then L50800
                scrnmsg$(1) = "(Detailed Report)"
                return
L50800:     if detail$ <> "N" then L50830
                scrnmsg$(1) = "(Summary Report Only)"
                return
L50830:     errormsg$ = "Please Enter 'Y', 'N' or 'E'."
            return

L50860: REM Test data for profit Percentage calculation method
            if profitflag$ <> "S" then L50900
                scrnmsg$(2) = "(MARGIN, i.e. function of selling Price)"
                return
L50900:     if profitflag$ <> "C" then L50940
                scrnmsg$(2) = "(MARKUP, i.e. function of cost)"
                return

L50940:     errormsg$ = "Please Enter 'S' or 'C'."
            return

L50970: REM Test data for which price to use
            if netgross$ <> "N" then L51010
                scrnmsg$(3) = "(Profit Calculated from Net Sale)"
                return
L51010:     if netgross$ <> "G" then L51050
                scrnmsg$(3) = "(Profit Calculated from Gross Sale)"
                return

L51050:     errormsg$ = "Please Enter 'N' or 'G'."
            return

L51080: REM Test for COST SET ID                  SET$
            if dtype$ = "I" and set$ = " " then return
            plowkey$   = "STC.HDR." & set$
            set_descr$ = hex(06) & "Select Cost Set"
            call "PLOWCODE" (#3, plowkey$, setdescr$, 8%, 0.30, onfile%)
            if onfile% = 1% then L51180
                errormsg$ = "Select Set, or blank to use cost on invoice"
                if dtype$ = "O" then errormsg$ = "Invalid Cost Set"
                set_descr$ = " "
                return
L51180:     set$ = str(plowkey$,9)
            call "PUTPAREN" (setdescr$)
            get #3 using L51210, in_file$
L51210:         FMT POS(51), CH(4)
            in_file$ = "STC" & in_file$ & "H"
            call "PUTPRNAM" addr(#10%, in_file$)
            f2%(10) = 1%
            call "OPENFILE" (#10%, "VALID", f2%(10), rslt$(10), " ")
            if f2%(10%) = 0% then return
                call "ASKUSER" (0%, "COST SET ARCHIVED",                 ~
                       "This Cost Set is not on disk and is assumed to", ~
                       "have been archived.  Press RETURN",              ~
                       "to select another Set.")
                     errormsg$ = "Cost Set Archived or Unavailable"
                     return

L51340: REM Test data for Types To Include...
            if dtype$ = "O" then L51470
               call "SPCESMSH" (typ$(), 0%) : i% = 1%
               if typ$() = " " then L51430
                  temp$ = " OMDACFXG"
                  for i% = 1% to 9%
                      if pos(temp$ = typ$(i%,1%)) = 0% then L51430
                  next i%
                  return
L51430:        errormsg$="Enter any combo of O, M, D, A, C, F, X or G: " ~
                          & typ$(i%,1%)
               return

L51470:     if typ$(1%,1%) = "N" or typ$(1%,1%) = "Y" then return
               errormsg$ = "Please enter 'Y' or 'N': " & typ$(i%,1%)
               return

L51510: REM Test data for Sort Options
            if sort1$ = "CAT " then L51630
            if sort1$ = "PART" then L51630
            if sort1$ = "CUS " then L51630
            if sort1$ = "DATE" then L51630
            if sort1$ = "STOR" then L51630
            if sort1$ = "DESC" then L51630
            if sort1$ = "ACCT" then L51630
            if sort1$ = "DESC" then L51630
               errormsg$ = "SORT BY must be 'CAT', 'PART', 'CUS'" &      ~
                           ", 'DATE', 'DESC', 'ACCT' or 'STOR'"
               return
L51630:     if sort1$ <> "ACCT" or detail$ = "E" then L51670
L51640:        errormsg$ = "Can only sort by Sales Account If Detail" &  ~
                           " Option is 'E'"
               return
L51670:     if sort2$ = " "    then L51820
            if sort2$ = "CAT " then L51820
            if sort2$ = "PART" then L51820
            if sort2$ = "CUS " then L51820
            if sort2$ = "DATE" then L51820
            if sort2$ = "STOR" then L51820
            if sort2$ = "GP% " then L51820
            if sort2$ = "GP$ " then L51820
            if sort2$ = "GRS$" then L51820
            if sort2$ = "NET$" then L51820
            if sort2$ = "DESC" then L51820
            if sort2$ = "ACCT" then L51820
               errormsg$ = "THEN BY m/be CAT, PART, CUS, DATE, ACCT" &   ~
                           ", STOR, DESC, GP%, GP$, GRS$, NET$,blank"
               return
L51820:     if sort2$ = "ACCT" and detail$ <> "E" then L51640
            if sort1$ <> sort2$ then L51860
               errormsg$ = "SORT BY and THEN BY can't be same"
               return
L51860:     if sort1$ <> "PART" or sort2$ <> "CAT" then L51890
               errormsg$ = "You Can't Sort By PART then by CAT."
               return
L51890:     if sort1$ <> "DESC" or sort2$ <> "CAT" then L51920
               errormsg$ = "You Can't Sort By DESC then by CAT."
               return
L51920:     if sort2$ = "GP$ " and detail$  = "N" then L51960
            if sort2$ = "GRS$" and detail$  = "N" then L51960
            if sort2$ = "NET$" and detail$  = "N" then L51960
            if sort2$ <> "GP% " or detail$ <> "N" then L51990
L51960:        errormsg$ = "You Can't Sub-Sort By GP%, GP$, GRS$ or NET$"~
                           & " If Running Summary Report: " & sort2$
               return
L51990:     if pos("YN" = breaks$) <> 0% then return
               errormsg$ = "Page Break Option Must Be 'Y' or 'N'"
               return

L52050: REM Test data for Document Number Range
            call "TESTRNGE" (str(firstdoc$()), str(lastdoc$()), fdoc$,   ~
                             ldoc$, errormsg$)
            return



        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements                                          *~
            *************************************************************

L60060: %RUN: ########  ########             ############################~
        ~################################                      SAGPRPT:SA0~
        ~001

L60100: %BY:  ###                ########################################~
        ~########################################                 PAGE: ##~
        ~##

L60140: %COST SET: ######## #############################################~
        ~#############################################  ##################~
        ~###
L60170: %                                         #######      #######   ~
        ~                        TOTAL                          PROFIT

L60200: %CATG PART NUMBER               CUSTOMER  DATE     STR NUMBER    ~
        ~ ######## GROSS SALE DISC AMT   NET SALE       COST   DOLLARS ###~
        ~###

L60240: %                                         #######  STR #######/  ~
        ~                        TOTAL                          PROFIT

L60270: %CATG PART NUMBER/DESCRIPTION   CUSTOMER  DATE     LIN SALES ACCT~
        ~ ######## GROSS SALE DISC AMT   NET SALE       COST   DOLLARS ###~
        ~###

L60310: %---- ------------------------- --------- -------- --- ----------~
        ~ -------- ---------- -------- ---------- ---------- --------- ---~
        ~---

L60350: %#### ######################### ######### ######## ### ##########~
        ~ ######## ########## ######## ########## ########## ######### ###~
        ~###

L60390: %     ##################################           ### ##########~
        ~##  ##############################

L60420: %                                         #######                ~
        ~                        TOTAL                          PROFIT

L60450: %CATG PART NUMBER               CUSTOMER  DATE     STR           ~
        ~ ######## GROSS SALE DISC AMT   NET SALE       COST   DOLLARS ###~
        ~###

L60490: %==== ========================= ========= ======== === ==========~
        ~ ======== ========== ======== ========== ========== ========= ===~
        ~===

L60530: %                                                                ~
        ~          ========== ======== ========== ========== ========= ===~
        ~===

L60570: %################################################################~
        ~####      ########## ######## ########## ########## ######### ###~
        ~###

L60610: %                                               * * * * * END OF ~
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

            call "SETPRNT" ("  ", "   " , 0%, 1%)
            call "FILEBGON" (#8)
            call "SHOSTAT" ("Closing Files, One Moment Please")

            end
