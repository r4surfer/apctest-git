        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  V   V  BBBB   K   K  DDDD    SSS   PPPP   L      Y   Y   *~
            *  V   V  B   B  K  K   D   D  S      P   P  L      Y   Y   *~
            *  V   V  BBBB   KKK    D   D   SSS   PPPP   L       YYY    *~
            *   V V   B   B  K  K   D   D      S  P      L        Y     *~
            *    V    BBBB   K   K  DDDD    SSS   P      LLLLL    Y     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * VBKDSPLY - SUMMARY - HISTORY - DETAIL PROGRAM FOR VENDOR  *~
            *            ACCOUNTS PAYABLE MODULE                        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/08/80 ! ORIGINAL                                 ! BEV *~
            * 04/11/83 ! GENERAL CLEAN UP                         ! ECR *~
            * 09/19/84 ! DISPLAYS PONUMBER HISTORY OF RECEIPTS    ! JWG *~
            * 12/13/85 ! Vendor file format changes               ! MJB *~
            * 06/04/86 ! Replaced POAPLINES with RCVMASTR         ! HES *~
            * 02/10/89 ! Fixed Range Selection                    ! MJB *~
            * 08/28/89 ! Added Multi-Currency Visability          ! MLJ *~
            * 06/08/90 ! Fixed PRRs'                              ! SID *~
            *          !    10963- Added PO date on Display/Report!     *~
            * 07/23/90 ! Fixed PRRs' 11592/11593 Vendor/PO# DSPLY ! SID *~
            * 05/07/91 ! Added prompt for history files.          ! JDH *~
            * 06/13/91 ! Removed FNX and used ROUND.              ! JDH *~
            * 04/02/92 ! Minor mods for DEC Compatability.        ! MJB *~
            * 11/23/92 ! PRR 12355 - Print now shows same amts as ! MLJ *~
            *          !  displayed when MC on and tran same as   !     *~
            *          !  stat currency.  Fixed alignment of line !     *~
            *          !  header on VBK003.                       !     *~
            *          ! PRR 12223 - Added date range selection to!     *~
            *          !  to VBK003, added time at end of report. !     *~
            *          !  Standardized headings on VBK002, VBK003.!     *~
            * 10/11/93 ! Fixed alignment of Job # & Rcv'd date on ! MLJ *~
            *          !  report. Fixed trunctation of rightmost  !     *~
            *          !  pos of Job # on line item display.      !     *~
            * 08/27/96 ! Millie date conversion                   ! DER *~
            * 01/27/97 ! From Date defaults to 'ALL'. PRR 13686.  ! DXL *~
            *************************************************************
        dim                                                              ~
            acct$16,                     /* ACCOUNT NUMBER             */~
            acct$(6)16,                  /* LINE ITEM SALES ACCOUNT    */~
            address$(3)30,               /* VENDOR   ADDRESS           */~
            amount$(20)10,               /* AMOUNTS FOR LINE ITEMS     */~
            amount(18),                  /* AMT ACCUMULATORS FOR AGING.*/~
            backext$10,                  /* BACKLOG TOTAL              */~
            backqty$10,                  /* TOTAL BACKLOG QTY THIS REPT*/~
            bckline$(21)79,              /* HOLD BACKLOG DISPLAY SCREEN*/~
            bckvencode$9,                /* HOLD BACKLOG CUSCODE       */~
            bckponumber$16,              /* HOLD BACKLOG PO NUMBER     */~
            bcksonumber$16,              /* HOLD BACKLOG SALES ORDER # */~
            bckstore$,                   /* HOLD BACKLOG STORE NUMBER  */~
            bk_lines$(21)79,             /* BACKLOG LINES DISPLAY      */~
            blankdate$8,                 /* blank unfmt date           */~
            blankline$79,                /* BLANK LINE INFORMATION     */~
            canceldate$8,                /* DATE TO CANCEL ORDER ON    */~
            cat$4,                       /* CATEGORY CODE INFORMATION  */~
            cat$(6)4,                    /* CATEGOTY CODE FOR DETAIL   */~
            ccyymmdd$8,                  /* ccyymmdd                   */~
            compdate$(18)6,              /* DATES FOR AGING THE BACKLOG*/~
            conname$20,                  /* VENDOR CONTACT NAME        */~
            curr$1,                      /* MULTI-CURRENCY USAGE FLAG  */~
            current$12,                  /* CURRENT BACKLOG TOTAL DSPLY*/~
            currkey$4,                   /* CURMASTR READ KEY          */~
            cursor%(2),                  /* CURSOR POSITION            */~
            cusdisplay$79,               /* VENDOR   NAME FOR DISPLAY  */~
            cusname$30,                  /* NAME OF THIS VENDOR        */~
            date$8,                      /* CALENDAR DATE (FOR AGING)  */~
            dspdate$(18)8,               /* SCREEN-F0RMAT AGING DATES  */~
            datedue$8,                   /* DATE DUE FROM LINE ITEMS   */~
            datedue$(6)8,                /* DATE DUE FOR DETAIL SCREEN */~
            date_msg$38,                 /* REPORT - DATE RANGE        */~
            datenext$8,                  /* date next                  */~
            datenext$(6)8,               /* LINE ITEM DATE PROMISED    */~
            dateprmsd$8,                 /* DATE PROMISED              */~
            daterecd$8,                  /* DATE RELEASED (LINE ITEMS) */~
            daterecd$(6)8,               /* DATE RELEASED (LINE ITEMS) */~
            descr$32,                    /* DESCRIPTION OF STUFF       */~
            descr$(6)32,                 /* PART DESCRIPTION FOR DETAIL*/~
            disp_lit3$(6)4,              /* PO DISPLAY LINE CURR CODE  */~
            disp_price$(6)10,            /* PO DISPLAY LINE PRICE      */~
            errormsg$79,                 /* CRUEL REMINDER OF MISTAKES */~
            extensiontotal$10,           /* EXTENSION FOR BCK BY CUS   */~
            extension$(6)10,             /* LINE ITEM EXTENSION        */~
            firstcust$9,                 /* FIRST VENDOR   CODE IN RNGE*/~
            fromcust$9,                  /* FIRST VENDOR for plow      */~
            fromdate$10,                 /* START DATE IN RANGE        */~
            futureamount$10,             /* FUTURE BACKLOG TOTAL       */~
            futuredate$8,                /* DATE PAST 18-MONTH AGING.  */~
            header$(3)79,                /* PLOWCODE Headers Var Arrary*/~
            hist_msg$7,                  /* Current or History Message */~
            history$1,                   /* Display from History Files?*/~
            i$(24)80,                    /* USED FOR CURSOR POSITION   */~
            incl_excl(1),                /* PLOWCODE Extended Arguement*/~
            incl_excl$(1)20,             /* PLOWCODE Extended Arguement*/~
            item$3,                      /* ITEM INFORMATION           */~
            item$(6)3,                   /*  "       "    FOR DETAIL   */~
            job$8,                       /* JOB NUMBER (FROM LINE ITEM)*/~
            jobdescr$32,                 /* JOB DESCRIPTION            */~
            jobnr$8,                     /* JOB NUMBER                 */~
            job$(6)8,                    /*  "    "       FOR DETAIL   */~
            lfac$(15)1,                  /* LINE FAC                   */~
            lastinvoice$16,              /* LAST INVOICE NUMBER        */~
            lastcust$9,                  /* LAST VENDOR   FOR PLOW RTN */~
            lastdate$8,                  /* LAST DATE PO MODIFIED      */~
            lastuserid$3,                /* LAST MODIFIED BY...        */~
            line2$79,                    /* Screen Line #2             */~
            line$(21)79,                 /* STATUTORY - BACKLOG LINES  */~
            linekey$(24)16,              /* PO NUMBER THIS SCREEN LINE */~
            linfac$(20)1,                /* FAC'S FOR DISPLAY WORK     */~
            literal1$15,                 /* STATUTORY NOTATION LABEL   */~
            literal2$32,                 /* PO CURRENCY DISPLAY HEADER */~
            literal3$4,                  /* PO CURRENCY DISPLAY CODE   */~
            lit3$(6)4,                   /* STATUTORY CODE ARRAY       */~
            location$2,                  /* LOCATOR ARRAY FOR AGINGS   */~
            lot$6,                       /* LOT NUMBER INFORMATION     */~
            lot$(6)6,                    /* LOT NUMBER                 */~
            maxdate$8,                   /* HIGH DATE FOR AGING STUFF  */~
            mc_display$1,                /* Multi-Currency Display flag*/~
            months$36,                   /* "JANFEBMARAPR..."          */~
            tocust$9,                    /* TO CUST for Plow           */~
            nextpo$50,                   /* NEXT PO NUMBER TO PLOW FOR */~
            nextpokey$50,                /* NEXT PO NUMBER FOR PLOW    */~
            oh$1,                        /* ON HAND POSTING OPTION     */~
            oh$(6)1,                     /* ON HAND POSTING OPTION     */~
            open$1,                      /* OPEN ONLY FLAG             */~
            orderdate$8,                 /* DATE OF ORDER (FROM HEADER)*/~
            origdate$8,                  /* DATE PO ORIGINALLY POSTED  */~
            origuserid$,                 /* ORIGINALLY POSTED BY...    */~
            part$26,                     /* PART NUMBER                */~
            part$(6)26,                  /* PART NUMBER FOR DETAIL SHOW*/~
            pastdue$12,                  /* PAST DUE TOTAL DISPLAY VAR */~
            pastdue2$10,                 /* PAST DUE DISPLAY FOR AGING */~
            phone$12,                    /* VENDOR   PHONE NUMBER      */~
            pf$(3)79,                    /* PFKEY LINES                */~
            pfkey$79,                    /* FORMATTED LIST OF PKKEYS   */~
            pfkeys$20,                   /* PFKEYS                     */~
            ponumber$16,                 /* PURCHASE ORDER NUMBER      */~
            po_tran_net$12,              /* PO TRANSACTION DISPLAY AMT */~
            plowkey$79,                  /* PLOW KEY                   */~
            ponum$16,                    /* INPUT SELECTION PO NUMBER  */~
            price$(6)10,                 /* STATUTORY PRICE ARRAY      */~
            prtvencode$9,                /* VENDOR   CODE TO PRINT     */~
            prtcusname$30,               /* VENDOR   NAME TO PRINT     */~
            prtdescr$32,                 /* DESCRIPTION FOR PRINT MODE */~
            prtext$10,                   /* PRINT VARIABLE             */~
            prtqty$10,                   /* PRINT VARIABLE             */~
            qtytotal$10,                 /* PRINT VARIABLE             */~
            readkey$50,                  /* KEY FOR PLOW ROUTINES      */~
            readkey2$50,                 /* ANOTHER KEY FOR PLOWS      */~
            runtime$8,                   /* SYSTEM RUN TIME            */~
            separator$(5)79,             /* INTER-LINE SEPERATOR TEXT  */~
            screendate$8,                /* PAYABLES DATE MM/DD/YY     */~
            shipto$(3)30,                /* SHIP TO INFORMATION        */~
            slsacct$16,                  /* SALES ACCOUNT NUMBER       */~
            slsacctdescr$32,             /* SALES ACCOUNT DESCRIPTION  */~
            sonumber$16,                 /* SALES ORDER NUMBER         */~
            stat_curr_code$4,            /* STATUTORY CURRENCY CODE    */~
            stat_curr_desc$30,           /* STATUTORY CURRENCY DESCR   */~
            store$(6)3,                  /* STORE NUMBER               */~
            store$3,                     /* STORE NUMBER               */~
            storedescr$32,               /* STORE DESSCRIPTION         */~
            tempcus$11,                  /* CUS CODE FOR DISPLAY       */~
            tempdate1$8,                 /* DATE INFORMATION           */~
            tempdate2$8,                 /* tempdate1 in ccyymmdd      */~
            tempmsg$79,                  /* MESSAGE FOR DISPLAYS       */~
            title$79,                    /* TITLE OF LINE ITEM COLUMNS */~
            todate$10,                   /* END DATE IN RANGE          */~
            today$8,                     /* TODAY'S DATE               */~
            total$12,                    /* TOTAL BACKLOG DISPLAY VAR. */~
            tr_lines$(21)79,             /* TRANSACTION - BACKLOG LINES*/~
            tr_ext$10,                   /* TRANSACTION- EXTENSION DISP*/~
            tr_lit3$(6)4,                /* TRANSACTION - CURR ARRAY   */~
            tr_price$(6)10,              /* TRANSACTION - LINE PRICE   */~
            tr_unit$(6)10,               /* TRANSACTION - UNIT PRICE   */~
            tran_curr_code$4,            /* TRANSACTION - CURRENCY CODE*/~
            tran_curr_desc$30,           /* TRANSACTION - CURRENCY DESC*/~
            qtyrecd$(6)10,               /* QUANTITY RECEIVED          */~
            qtyorig$(6)10,               /* QUANTITY ORIGINAL          */~
            qtyonord$(6)10,              /* QUANTITY BACKORDERED       */~
            userid$3,                    /* USERID OF CURRENT USER     */~
            variable$(10)20,             /* VARIABLE FIELDS INFORMATION*/~
            vbklnkey$28,                 /* VBKLNCUR READ KEY          */~
            vencode$9                    /* VENDOR    CODE WE'RE DOING */

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
            *   1 ! USERINFO ! SYSTEM USER INFORMATION...MODULE DATES...*~
            *   2 ! GLMAIN   ! GENERAL LEDGER MAIN FILE                 *~
            *   3 ! VENDOR   ! VENDOR   MASTER RECORDS FILE.            *~
            *   4 ! TXTFILE  ! System Text File                         *~
            *   5 ! VBKMASTR ! VENDOR   BACKLOG MASTER FILE             *~
            *   6 ! VBKLINES ! BACKLOG LINE ITEMS FILE                  *~
            *   8 ! RCVLINES ! Receiver Line Items                      *~
            *   9 ! STORNAME ! STORE INFORMATION FILE                   *~
            *  10 ! JOBMASTR ! JOB MASTER FILE                          *~
            *  11 ! PAYLINES ! PAYABLES LINE ITEMS FILE                 *~
            *  12 ! PAYMASTR ! PAYABLES MASTER FILE                     *~
            *  13 ! SYSFILE2 ! SYSTEM FILE                              *~
            *  15 ! VBKHMSTR ! VENDOR   BACKLOG HISTORY FILE            *~
            *  16 ! VBKHLNES ! BACKLOG LINE ITEMS HISTORY FILE          *~
            *  40 ! VBKLNCUR ! SHADOW - VBKLINES                        *~
            *  41 ! CURMASTR ! CURRENCY MASTER FILE                     *~
            *  42 ! VBKHCLNS ! SHADOW - VBKHLNES                        *~
            *************************************************************

            select # 1, "USERINFO",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos = 1 , keylen = 3

            select #2, "GLMAIN",                                         ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 300,                                    ~
                       keypos = 1, keylen = 9

            select #3,  "VENDOR",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 600,                                   ~
                        keypos=1, keylen=9,                              ~
                        alt key 1, keypos = 10, keylen = 30, dup

            select  #4, "TXTFILE",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos  = 1, keylen = 11

            select  #5, "VBKMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1030,                                  ~
                        keypos = 1, keylen = 25,                         ~
                        alt key 1, keypos = 10, keylen = 16

            select #6, "VBKLINES",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 700,                                    ~
                       keypos = 1, keylen = 28

            select # 8, "RCVLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 800,                                   ~
                        keypos= 26, keylen = 52,                         ~
                        alt key 1, keypos =  1, keylen = 69,             ~
                            key 2, keypos = 42, keylen = 36,             ~
                            key 3, keypos =128, keylen = 24

            select #9, "STORNAME",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 300,                                    ~
                       keypos = 1, keylen = 3

            select #10, "JOBMASTR",                                      ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 700,                                    ~
                       keypos  = 1, keylen = 8

            select #11, "PAYLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 541,                                   ~
                        keypos = 36, keylen = 28,                        ~
                        alternate key 1, keypos = 1, keylen = 63,        ~
                                  key 2, keypos = 17, keylen = 47

            select #12, "PAYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 350,                                   ~
                        keypos = 1, keylen = 25


            select #13, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select #15, "VBKHMSTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1030,                                  ~
                        keypos = 1, keylen = 25,                         ~
                        alt key 1, keypos = 10, keylen = 16

            select #16, "VBKHLNES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 700,                                   ~
                        keypos = 1, keylen = 28

            select #40, "VBKLNCUR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 5, keylen = 28

            select #41, "CURMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 256,                                   ~
                        keypos = 1, keylen = 4

            select #42, "VBKHCLNS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 5, keylen = 28

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#2, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#3, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#4, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#5, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#6, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#8, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#9, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#10, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#11, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#12, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#13, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#15, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#16, 0%, 0%, 0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * SETS UP ROUNDING FUNCTION, GETS PAYABLES    DATE, ...     *~
            *************************************************************

            open$ = "Y"
            history$ = "N"

            REM GET PAYABLES    DATE.
                call "EXTRACT" addr("ID", userid$)
                call "READ100" (#1, userid$, f1%(1))
                     if f1%(1) = 0 then L65000
                get #1, using L09124, paydate$    /* PAYABLES DATE!!  */
L09124:                 FMT XX(9), CH(6)

*        Check for Multi-Currency usage & get STATUTORY curr & descr
            curr$ = "N"
            stat_curr_code$ = " "
            call "READ100" (#13, "SWITCHS.CUR", f1%(13))
            if f1%(13) = 0% then L09150    /* Multi-Currency not used */
                get #13 using L09132, curr$, stat_curr_code$
L09132:              FMT POS(21), CH(1), CH(4)
            call "OPENCHCK" (#40, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#41, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#42, 0%, 0%, 0%, " ")
            currkey$ = str(stat_curr_code$)
            call "READ100" (#41, currkey$, f1%(41))
                if f1%(41) = 0% then L09150
                     get #41 using L09140, stat_curr_desc$
L09140:                   FMT POS(05), CH(30)

L09150:     date$ = date
            call "DATEFMT" (date$, ccyymmdd%, ccyymmdd$)
            today$ = date$
            blankdate$ = " "
            call "DATUNFMT" (blankdate$)
            screendate$ = paydate$
            call "DATEFMT" (screendate$)

            months$="JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC"

            str(line2$,62,18) = "VBKDSPLY: " & str(cms2v$,1,8)

            mc% = 5%   /* Channel variables - Current vs History */
            lc% = 6%   /* Defaults here to Current Files         */
            cc% = 40%

        REM *************************************************************~
            *           I N P U T   V E N D O R       C O D E           *~
            *                                                           *~
            * INPUT VENDOR   CODE, AND GO TO RANGE PRINT OPERATION IF   *~
            * DESIRED.  DETERMINES IF THE VENDOR   IS ON FILE           *~
            *************************************************************

        inputmode
            init(" ") vencode$, cusname$, address$(), errormsg$, phone$, ~
                      ponum$
            blankline$ = "Enter 'Open' Option, Vendor Code, and/or "     ~
                       & "PO Number."
            fromdate$ = "ALL"

L10120:     for fieldnr% = 1 to 1
L10130:         gosub'200(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  =  9 then rangeprint
                      if keyhit%  = 16 then L65000
                      if keyhit% <>  0 then L10130
                gosub'150(fieldnr%)
                      if errormsg$ <> " " then L10130
                next fieldnr%

L11000: REM *************************************************************~
            *   D I S P L A Y   R O U T I N E   F O R   B A C K L O G   *~
            *                                                           *~
            * PLOWS THROUGH THE BACKLOG FILE AND LOADS THE LINE ITEMS.  *~
            *************************************************************

            displayline% = 0
            init(" ") line$(), linekey$(), bk_lines$(), tr_lines$()
            flag1% = 0                    /* LINES PRINTED THIS CUST.  */

            if ponum$ = " " then L11096
                nextpo$ = str(vencode$) & ponum$
                call "READ100" (#mc%, nextpo$, f1%(mc%))
                     if f1%(mc%) = 0 then exitplow1
                goto L11181

L11096:     nextpo$ = vencode$

            REM PLOW ROUTINE TO GET NEXT PO HEADER THIS VENDOR  .
L11120:         call "PLOWNEXT" (#mc%, nextpo$, 9%, f1%(mc%))
                     if f1%(mc%) = 0 then exitplow1

L11181:         flag2% = 0               /* LINES PRINTED PER P.O.     */
                gosub L30000              /* LOAD UP HEADER             */
                gosub L12000              /* AND DO THE PRINT ON SCREEN */
                goto  L11120              /* AND GET NEXT ORDER.        */

            exitplow1: REM EXITS PLOW ROUTINE.
                REM IF THERE'S LINE ITEMS YET TO DISPLAY, DO SO...
                if displayline% = 0 then gosub L11340  /* SHOW NO PO'S  */
                   displayline% = 1000
                   gosub L20000           /* SCREEN DISPLAYER.          */
                   goto summarydsply

L11340:         REM ON EXIT, THIS SETS UP FOR NO ORDERS THIS VENDOR
                  init(" ") line$(), linekey$(), bk_lines$(), tr_lines$()
                    gosub L12800               /* FORMAT VENDOR NAME    */
                    tempmsg$ = "NO PURCHASE ORDERS OUTSTANDING"
                    str(line$(9),40-len(tempmsg$)/2)=tempmsg$
                    return

L12000: REM *************************************************************~
            *        S C R E E N   F O R M A T   A N   O R D E R        *~
            *                                                           *~
            * PRINTS BACKLOG LINE ITEMS IN SCREEN FORMAT FOR THE DESIRED*~
            * VENDOR                                                    *~
            *************************************************************

            prtponumber$ = ponumber$
            prtstore$ = "STORE:"
            str(prtstore$,  8) = store$
            readkey$ = nextpo$

            gosub L12800                       /* FORMAT VENDOR NAME    */

L12110:     REM PLOW EACH LINE
L12120:         call "PLOWNEXT" (#lc%, readkey$, 25%, f1%(lc%))
                     if f1% (lc%) = 0 then L12580
                gosub L30200                   /* LOAD A LINE           */
                if open$ <> "Y" then L12170
                if part$ = " " then L12120     /* DESCRIPTION ONLY LINE */
                if qtybkord = 0 then L12120    /* ENTIRELY SHIPPED      */
L12170:         REM PRINT FIRST LINE OF DOUBLET.
                    prtdate$ = datedue$
                       call "DATEFMT" (prtdate$)
                    temp = qtybkord * price        /* EXTENSION.       */
                    temp = round(temp-(temp*.01*disc), 2)
                    call "CONVERT" (temp, 2.4, prtext$)
                    call "CONVERT" (qtybkord, 0.2, prtqty$)
                    prtdescr$ = part$
                    gosub L20000                    /* PAGE CONTROL     */
                    put line$(displayline%), using L12630,                ~
                              prtponumber$, prtdescr$, prtdate$,         ~
                              str(prtqty$,2),  prtext$

            if curr$ = "N" then L12335       /* Multi-Currency not used */
                str(vbklnkey$) = readkey$
                tran_curr_code$, tran_curr_desc$ = " "
                call "READ100" (#cc%, vbklnkey$, f1%(cc%))
                if f1%(cc%) = 0% then L12333  /*    STATUTORY           */
                     get #cc% using L12326, tran_curr_code$, trext
L12326:                   FMT CH(04), POS(41), PD(14,4)
                call "CONVERT" (trext, 2.4, tr_ext$)
                put tr_lines$(displayline%), using L12630, prtponumber$,  ~
                    prtdescr$, prtdate$, str(prtqty$,2), tr_ext$
                goto L12335
L12333:         put tr_lines$(displayline%), using L12630, prtponumber$,  ~
                    prtdescr$, prtdate$, str(prtqty$,2), prtext$
L12335:         linekey$(displayline% + 3) = ponumber$
                init(" ") prtponumber$
                REM PRINT SECOND LINE OF DOUBLET.
                    prtdate$ = " "
                    if dateprmsd$ = datedue$ then L12400
                    if dateprmsd$ = " " or dateprmsd$ = blankdate$ then L12400
                       prtdate$ = dateprmsd$
                       call "DATEFMT" (prtdate$)
L12400:             gosub L20000                    /* PAGE CONTROL     */
                    prtsonumber$ = "       " & orderdate$
                    put line$(displayline%), using L12630,                ~
                        prtsonumber$, descr$, prtdate$, job$, " "

            if curr$ = "N" then L12417
                put tr_lines$(displayline%), using L12630, prtsonumber$,  ~
                    descr$, prtdate$, job$, " "
L12417:             linekey$(displayline% + 3) = ponumber$
                    prtsonumber$ = " "
*        Store / Currency Code & Description Line
                prtext$, tr_ext$ = " "
            if curr$ = "N" and prtstore$ = " "                           ~
                then L12540                    /* Line not generated */
            gosub L20000
            if curr$ = "N" then L12474         /* Store Code only    */
                currkey$ = str(tran_curr_code$)
                call "READ100" (#41, currkey$, f1%(41))
                if f1%(41) = 0% then L12455    /*   STATUTORY        */
                    get #41 using L12450, tran_curr_desc$
L12450:                 FMT POS(05), CH(30)
                str(prtdescr$) = str(tran_curr_code$) & "  " &           ~
                                 tran_curr_desc$
                goto L12465
L12455:         str(prtdescr$) = str(stat_curr_code$) & "  " &           ~
                                 stat_curr_desc$
L12465:         if prtstore$ = " " then                                  ~
                    put tr_lines$(displayline%), using L12660, " ",       ~
                                                 prtdescr$               ~
                else put tr_lines$(displayline%), using L12660, prtstore$,~
                                                 prtdescr$
L12474:         if prtstore$ = " " then                                  ~
                    put line$(displayline%), using L12660, " ", " "       ~
                else                                                     ~
                    put line$(displayline%), using L12660, prtstore$, " "
                linekey$(displayline% + 3) = ponumber$
                       prtstore$ = " "
L12540:     REM PRINT TRAILER LINE "+-----+--..." LINE
                    gosub L20000
                    put line$(displayline%), using L12690, " "
                if curr$ = "N" then L12570
                    put tr_lines$(displayline%), using L12690, " "
L12570:         goto L12110   /* PROCESS NEXT LINE */
L12580:  REM EXIT FROM THIS P.O.
             if displayline% <> 0 then put line$(displayline%),          ~
                                       using L12720, "---"
             if displayline% <> 0 then put tr_lines$(displayline%),      ~
                                       using L12720, "---"
        REM  INIT(" ") VENCODE$
             return

L12630: %################!################################!########!#####~
        ~####!##########

L12660: %  ##########    !################################!        !     ~
        ~    !

L12690: %  ##########    +--------------------------------+--------+-----~
        ~----+----------

L12720: %#---------------+--------------------------------+--------+-----~
        ~----+----------

L12800:     REM FORMAT VENDOR NAME
                cusdisplay$ = "Vendor:"
                str(cusdisplay$, 11) = cusname$
                str(cusdisplay$, len(cusdisplay$) + 2) = "("
                str(cusdisplay$, len(cusdisplay$) + 1) = vencode$
                str(cusdisplay$, len(cusdisplay$) + 1) = ")"
                return

        REM *************************************************************~
            *       C O M P U T E   B A C K L O G   S U M M A R Y       *~
            *                                                           *~
            * COMPUTES THE BACKLOG AGING FOR THE VENDOR   AND PRINT -   *~
            * FORMATS THE SUMMARY INFORMATION FOR US.                   *~
            * NOTE THAT THE ONLY FILE WE NEED TO LOOK AT IN THIS PART   *~
            * IS THE LINE ITEMS.                                        *~
            *************************************************************

        summarydsply
            readkey$ = vencode$
            REM SET UP BACKLOG AGING DATES.
                convert str(ccyymmdd$, 1%, 4%) to year%
                convert str(ccyymmdd$, 5%, 2%) to month%
                for temp% = 1 to 18

                    REM CONVERT DATE TO DISPLAY FORMAT (JAN  80, ETC...)
                        dspdate$(temp%) = str(months$,3*month%-2,3)
                        convert year% to str(dspdate$(temp%),5), pic(####)

                    REM NOW FORMAT DATE FOR COMPARISONS (COMPUTE AGINGS)
                        convert year%  to compdate$(temp%), pic(####)
                        convert month% to str(compdate$(temp%),5%),pic(00)

                    month% = month% + 1
                    if month% <= 12 then L13130
                       month% = 1
                       year% = year% + 1
L13130:          next temp%

                REM MAX DATE FOR > 18 MONTHS BACKLOG PARSE
                    convert year%    to str(maxdate$, 1%, 4%), pic(####)
                    convert month%-1 to str(maxdate$, 5%, 2%), pic(00)
                    futuredate$ = str(maxdate$, 5%, 2%) & ~
                                  "/" & str(maxdate$, 1%, 4%)
                    str(maxdate$, 7%, 2%) = "01"
                    call "DATECONV" (maxdate$)

                REM FORMAT VENDOR   CODE INFORMATION.
                    tempcus$ = vencode$
                    call "PUTPAREN" (tempcus$)

            REM PLOW THROUGH FILE GETTING ORDER DETAILS.
                mat amount = zer
                latetotal, wayouttotal, currenttotal = 0

L13225:         call "PLOWNEXT" (#lc%, readkey$, 9%, f1%(lc%))
                     if f1%(lc%) = 0 then L13500
                get #lc%, using L13247, qtybkord, price,  tempdate1$


L13247:     FMT XX(9),                   /* VENDOR CODE                */~
                XX(16),                  /* PURCHASE ORDER NUMBER      */~
                XX(3),                   /* SEQUENCE NUMBER            */~
                XX(3),                   /* ITEM NUMBER                */~
                XX(25),                  /* PART NUMBER                */~
                XX(32),                  /* DESCRIPTION                */~
                XX(4),                   /* CATEGORY CODE              */~
                XX(8),                   /* QUANTITY ORIGINALLY ORDERED*/~
                XX(8),                   /* QUANTITY RECEIVED          */~
                PD(14,4),                /* QUANTITY ON ORDER          */~
                PD(14,7),                /* UNIT PRICE                 */~
                XX(8),                   /* EXTENSION                  */~
                XX(9),                   /* PURCHASES ACCOUNT NUMBER   */~
                CH(6),                   /* DATE DUE INFORMATION       */~
                XX(6),                   /* DATE OF LAST RECEIPT       */~
                XX(6),                   /* DATE OF NEXT RECEIPT       */~
                XX(6),                   /* LOT NUMBER                 */~
                XX(8),                   /* JOB NUMBER                 */~
                XX(3),                   /* STORE NUMBER               */~
                XX(1),                   /* ON HAND POSTING OPTION     */~
                XX(50),                  /* FILLER                     */~
                XX(50),                  /*   DITTO                    */~
                XX(23)                   /*     AH SO...               */

                REM NOW DO AGING COMPUTATIONS ON THIS MESS.
                    REM COMPUTE EXTENSION.
                        temp = qtybkord * price
                        temp = round(temp,2)
                    REM FIGURE OUT WHICH CASE IT IS.
                        if tempdate1$ <  paydate$ then L13415 /*LATE */
                        if tempdate1$ >= maxdate$ then L13430
                           goto L13445    /* NORMAL ORDER.              */

L13415:             REM IF PAST DUE
                        latetotal = latetotal + temp
                        goto L13225
L13430:             REM IF OUT PAST HIGHEST AGING DATE
                        wayouttotal = wayouttotal + temp
                        goto L13225
L13445:             REM IF IN AGING PERIOD.
                        call "DATEFMT" (tempdate1$, nix%, tempdate2$)
                        search compdate$()=str(tempdate2$, 1%, 6%)  ~
                               to location$ step 6
                        if location$ = hex(0000) then /*GULP!*/ L13430
                           temp% = (val(location$,2)-1)/6+1
                           amount(temp%) = amount(temp%) + temp
                           goto L13225

L13500:     REM PRINT FORMAT LOTS OF BACKLOG TOTAL FIGURES.
                currenttotal = 0
                for temp% = 1 to 18
                    call "CONVERT" (amount(temp%), 2.4, amount$(temp%))
                    currenttotal = currenttotal + amount(temp%)
                    next temp%
                call "CONVERT" (latetotal, 2.4, pastdue2$)
                call "CONVERT" (wayouttotal, 2.4, futureamount$)
                     currenttotal = currenttotal + wayouttotal
                call "CONVERT" (currenttotal, 2.4, current$)
                call "CONVERT" (latetotal, 2.4, pastdue$)
                call "CONVERT" (currenttotal + latetotal, 2.4, total$)


            REM SHOW IT AND GO FOR WHATEVER.
L13585:         gosub L42000
                      if keyhit% = 1 then gosub startover
                      if keyhit% = 2 then       L11000
                      if keyhit% = 14 then L13620
                      if keyhit% = 16 then inputmode
                         goto L13585

L13620:     REM HANDLE PRINT MODE -- SPECIAL.
                bckline% = 1000
                pagenumber% = 0
                call "SHOSTAT" ("Printing Backlog For This Vendor")
                gosub L19000
                close printer
                goto inputmode

        REM *************************************************************~
            *     P R I N T   B A C K L O G   B Y   V E N D O R         *~
            *                                                           *~
            * CONTROL ROUTINE FOR PRINTING A RANGE OF VENDORS   BY      *~
            * WILDCARDING.  THEN ROUTINE PLOWS THROUGH VENDOR   FILE    *~
            * LOADING NEXT  VENDOR  AND CALLING THE SUBROUTINE THAT     *~
            * PRINTS THE BACKLOG BY VENDOR   FOR A GIVEN VENDOR         *~
            *************************************************************

        rangeprint
            firstcust$, fromdate$ = "ALL"
            lastcust$, todate$, errormsg$, blankline$ = " "
            backqty, backext = 0
            date_msg$ = "Date Range "
                runtime$ = " "
                call "TIME" (runtime$)
            select printer(134)

L18075:     gosub L43000
                  if keyhit%  =  1 then inputmode
                  if keyhit%  = 16 then inputmode
                  if keyhit% <> 0  then L18075
            gosub L51000
                  if errormsg$ <> " " then L18075

            REM INITIALIZE PAGE COUNTER TO FORCE PAGE FEED FIRST PAGE.
                pagenumber% = 0
                bckline% = 1000
                call "SETPRNT" ("VBK002", " ", 0%, 0%)
                call "SHOSTAT" ("Printing Backlog By Vendor Report")

            REM PLOW THROUGH THE FILE, GETTING VENDOR   CODES TO DO.
L18145:         call "PLOWNEXT" (#3, fromcust$, 0%, f1%(3))
                     if f1%(3) = 0 then L18200
                     if fromcust$ > tocust$ then L18200
                     vencode$ = fromcust$
                gosub L19040
                      backqty = backqty + qtytotal
                      backext = backext + extensiontotal
                      goto L18145

L18200:     REM PRINT TOTAL LINE FOR THE BACKLOG REPORT.
                   if pagenumber% = 0% then L18310
                   call "CONVERT" (backqty, 0.2, backqty$)
                   call "CONVERT" (backext, 2.4, backext$)
                   print using L18275, " ", backext$
                   bckline% = bckline% + 1

                   print using L19820
                   bckline% = bckline% + 1
                runtime$ = " " : call "TIME" (runtime$) : print skip(2)
                print "*****  END OF REPORT @ " & runtime$ & "  *****"
                close printer
                goto inputmode

L18275: %!**TOTAL**! T O T A L   A C T I V E   B A C K L O G   T H I S   ~
        ~R E P O R T                         !##########!##########!      ~
        ~  !

L18300:    u3% = 0%
L18310:    call "ASKUSER" (u3%, "***** NO REPORT *****", "There were " & ~
                          "NO RECORDS selected in the Requested Range",  ~
                          "Press RETURN to re-enter Range ",             ~
                          "or PF-16 to EXIT program")
           if u3% = 0% then L18075
           if u3% = 16% then L65000
           goto L18300

L19000: REM *************************************************************~
            *  P R I N T   B A C K L O G   F O R   A   V E N D O R      *~
            *                                                           *~
            * SUBROUTINE WHICH PRINTS THE BACKLOG FOR A SINGLE VENDOR   *~
            * THIS ROUTINE DOES NOT USE THE MULTI-COLUMN PRINT TECHNIQUE*~
            * BUT RATHER A FIXED-PRINT TECHNIQUE.  THE CODE IS HOPEFULLY*~
            * CLEAN ENOUGH SO THAT YOU CAN FIGURE IT OUT JUST BY LOOKING*~
            * AT IT.                                                    *~
            *************************************************************

                select printer (134)

                runtime$ = " "
                call "TIME" (runtime$)

L19040:         REM GET VENDOR   NAME
                    get #3, using L19048, cusname$
L19048:                           FMT XX(39), CH(30)
                REM SETUP THE PLOW ROUTINE FOR THIS VENDOR'S ORDERS
                    nextpo$ = vencode$
                    qtytotal, extensiontotal = 0
                    prtvencode$ = vencode$
                    prtcusname$ = cusname$
                    flag1% = 0   /* REM COUNTS LINES PER VENDOR   */

L19100:     REM PLOW ROUTINE TO FIND PURCHASE ORDERS FOR THAT VENDOR
L19110:         call "PLOWNEXT" (#mc%, nextpo$, 9%, f1%(mc%))
                     if f1%(mc%) = 0 then L19600
                gosub L30000       /* LOAD UP HEADER INFO */
                if fromdate$ = "ALL" then L19140
                    if odate% < fdate% or odate% > tdate% then L19110
L19140:         flag2% = 0        /* COUNTER OF LINES PRINTED ON P.O.  */
                flag3% = 0        /* STORE NUMBER PRINTED              */
                prtponumber$ = ponumber$
                prtsonumber$ = sonumber$
                prtstore$ = "STORE:"
                str(prtstore$, 8) = store$
                readkey$ = nextpo$

L19200:     REM PRINT LINE ITEMS, & VENDOR CODE, PO NUMBER, ETC.
L19210:         call "PLOWNEXT" (#lc%, readkey$, 25%, f1%(lc%))
                     if f1%(lc%) = 0 then L19100 /* GET NEXT P.O. */
                gosub L30200    /* LOAD UP LINE ITEM INFO */
                if open$ <> "Y" then L19260
                if part$ = " " then L19210   /* DESCRIPTION ONLY LINE */
                if qtybkord = 0 then L19210  /* LINE ENTIRELY SHIPPED */
L19260:         REM FORMAT DATA FOR LATER USE
                    prtdescr$ = part$
                    temp = qtybkord * price
                    extension = round(temp-(temp*.01*disc), 2)
                     prtjob$ = job$
                     call "CONVERT" (qtybkord,  0.2, prtqty$)
                     call "CONVERT" (price,     2.7, price$(1))
                     call "CONVERT" (extension, 2.2, prtext$)
                     prtdate1$ = datedue$
                     call "DATEFMT" (prtdate1$)
                     if datenext$ = datedue$ then L19500
                        prtdate2$ = datenext$
                        call "DATEFMT" (prtdate2$)

L19500:          REM NOW HANDLE PRINTING OF THE DATA.
                     gosub L19700
                     if bckline% <  20 then L19527
                        if flag1% = 0 then L19527
                        if flag2% <> 0 then print using L19932, " "       ~
                           else print using L19948
L19527:                 print using L19916,                               ~
                              prtvencode$, prtcusname$, prtponumber$,    ~
                              prtdescr$, prtdate1$, prtqty$, prtext$,    ~
                              prtjob$
                        init(" ") prtvencode$, prtcusname$, prtponumber$,~
                                  prtdescr$, prtdate2$, prtqty$, prtext$,~
                                  prtjob$
                        flag1% = flag1% + 1   /* LINES PER VENDOR   */
                        flag2% = flag2% + 1   /* LINES PER P.O.     */
                        prtdescr$ = descr$
                        prtsonumber$ = "       " & orderdate$
                        print using L19916, " "," ", prtsonumber$,        ~
                              prtdescr$, prtdate2$, price$(1)," "," "
                        init(" ") prtsonumber$, prtdate2$
                        if flag3% <> 0 then L19566: bckline% = bckline% + 1
                        print using L19941, prtstore$: flag3% = 1
L19566:             REM NOW ADD UP LINE ITEM AMOUNTS INTO ACCUMULATORS.
                        qtytotal = qtytotal + qtybkord
                        extensiontotal = extensiontotal + temp
                    goto L19200   /* GET NEXT LINE ITEM */

L19600:     REM PRINT TOTAL LINE, RETURN FROM ROUTINE.
                if flag1% = 0 then return   /* NOTHING PRINTED */
                print using L19948
                bckline% = bckline% + 2
                call "CONVERT" (qtytotal, 0.2, qtytotal$)
                call "CONVERT" (extensiontotal, 2.4, extensiontotal$)
                print using L19964, vencode$, " ", extensiontotal$
                bckline% = bckline% + 1
                print using L19884
                bckline% = bckline% + 1
                return

L19700:     REM PAGE CONTROL SUBROUTINE
                bckline% = bckline% + 3
                if bckline% < 66 then return
                   if bckline% < 1003 then print using L19820
                   print page
                   pagenumber% = pagenumber% + 1
                   print using L19800, today$, runtime$
                   print using L19809, hist_msg$, date_msg$, pagenumber%
                   if curr$ = "Y" then print using L19812
                   print using L19814
                   print using L19820
                   print using L19836
                   print using L19852
                   print using L19868, "#"
                   print using L19884
                   bckline% = 18
                   return

L19800: %RUN ########  ########                           B A C K L O G  ~
        ~ B Y   V E N D O R                                   VBKDSPLY:VBK~
        ~002
L19809: %  ####### Files Only                          ##################~
        ~####################                                   PAGE  ####
L19812: %                                                 Values are in S~
        ~TATUTORY currency
L19814: %                                                                ~


L19820: %+---------+------------------------------+----------------+-----~
        ~-----------------------------------------------------------------~
        ~--+

L19836: %!VENDOR   !                              !  P.O.  NUMBER  !     ~
        ~             L I N E   I T E M   B R E A K D O W N               ~
        ~  !

L19852: %!         !   V E N D O R   N A M E      !       & DATE   +-----~
        ~---------------------------+--------+----------+----------+------~
        ~--+

L19868: %!  CODE   !                              ! STORE NUMBER   !   PA~
        ~RT  NUMBER / DESCRIPTION   !DUE/NEXT!QTY/PRICE !EXTENSION ! JOB  ~
        ~# !

L19884: %+---------+------------------------------+----------------+-----~
        ~---------------------------+--------+----------+----------+------~
        ~--+

        %+---------+------------------------------+----------------+-----~
        ~---------------------------+--------+----------+----------+      ~
        ~  +

L19916: %!#########!##############################!################!#####~
        ~###########################!########!##########!##########!######~
        ~##!

L19932: %!         !                              !  ##########    +-----~
        ~---------------------------+--------+----------+----------+      ~
        ~  !
L19941: %!         !                              !  ##########    !     ~
        ~                           !        !          !          !      ~
        ~  !

L19948: %!         !                              +----------------+-----~
        ~---------------------------+--------+----------+----------+------~
        ~--!

L19964: %!         !                              ! *** TOTALS ***   FOR ~
        ~VENDOR   #########                  !##########!##########!      ~
        ~  !

        %!         !                              !                      ~

L20000: REM *************************************************************~
            *    D I S P L A Y   C O N T R O L L E R   R O U T I N E    *~
            *                                                           *~
            * WORKS LIKE A CROSS BETWEEN THE EDIT CONTROL ROUTINE IN A  *~
            * SOURCE DOCUMENT INPUT PROGRAM AND THE PAGE CONTROL ROUTINE*~
            * IN A PRINT PROGRAM.  IF WE'VE PRINTED A PAGE, THEN THIS   *~
            * ROUTINE DISPLAYS IT, AND MANAGES THE PFKEYS FOR IT, ELSE  *~
            * IT JUST INCREMENTS THE LINE COUNTER AND RETURNS.          *~
            *************************************************************

            displayline% = displayline% + 1
            if displayline% < 21 then return
L20120:        gosub'222                 /* AND JUST SHOW SCREEN AS IS */
                     if keyhit%  =  1 then gosub startover
                     if keyhit%  <> 2 then       L20150
                        ponum$ = " "
                        goto L20220
L20150:              if keyhit%  =  5 then       L20250
                     if keyhit% =   8 then       L20480
                     if keyhit% =  12 then       L20310
                     if keyhit%  = 14 then       L20400
                     if keyhit%  = 16 then       inputmode
                     goto L20120          /* FOR STARTOVER  */

L20220:        REM HANDLES CASE WHERE WE WANT TO GO TO FIRST P.O.
                   return clear
                   goto L11000            /* AND DO FIRST P.O.   .      */
L20250:        REM HANDLES CASE WHERE HE WANTS NEXT PAGE (17 LINES)
                   displayline% = 1      /* SET LINE COUNTER           */
                   copy str(line$(),1423) to str(line$(),1)
                   copy str(linekey$(),337) to str(linekey$(),49)
                   init(" ") str(line$(),238), str(linekey$(),97)
                   if curr$ = "N" then L20300
                       copy str(tr_lines$(),1502) to str(tr_lines$(),1)
                       copy str(linekey$(),353)   to str(linekey$(),33)
                       init(" ") str(tr_lines$(),159), str(linekey$(),81)
L20300:            return
L20310:        REM DISPLAY ENTIRE PO
                   REM GET CURSOR POSITION
                       close ws
                       call "SCREEN" addr("C",u3%,"I",i$(),cursor%())
                               u3% = u3%
                   if linekey$(cursor%(1)) = " " then L20120
                   gosub hold_bckdisplay_pointers
                   nextpokey$ = vencode$
                   str(nextpokey$, 10) = linekey$(cursor%(1))
                   gosub display_entire_po
                   gosub restore_bckdisplay_pointers
                   go to L20120
L20400:        REM HANDLES CASE WHERE USER WANTS PRINTED REPORT
                   bckline% = 1000
                   pagenumber% = 0
                   call "SHOSTAT" ("Printing Backlog For Vendor")
                   gosub L19000           /* PRTS ENTIRE LEDGER FOR VEN */
                   return clear
                   close printer
                   goto inputmode       /* GOES TO INPUT MODE         */

L20480:        REM BRANCH TO SUMMARY PAGE
                   return clear
                   goto summarydsply

        REM *************************************************************~
            *     H A R D C O P Y    P A G E    C O N T R O L L E R     *~
            *                                                           *~
            * HANDLES PAGING AND HEADINGS WHEN WE ARE PRINTING PO'S     *~
            *************************************************************

        control_print_entire_po:
                line% = line% + 1
                if line% < 64 then return
                print page
                print using L37855
                print skip (2)
                print using L37890, "VENDOR CODE             ", vencode$, ~
                                                               cusname$
                print using L37860, "PURCHASE ORDER NUMBER   ", ponumber$
                print skip (2)
                print using L37950
                print using L37995
                line% = 16
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
            *   R E A D   A   P . O .   H E A D E R S   &   L I N E S   *~
            *                                                           *~
            * SUBROUTINES FOR LOADING UP THE P.O. HEADER, AND THE LINE  *~
            * ITEMS, INCLUDING FORMAT STATEMENTS                        *~
            *************************************************************

            get #mc%, using L31000,  vencode$, ponumber$,                 ~
                    cusname$,  str(shipto$(),,60), str(shipto$(),61,30), ~
                    conname$, slsacct$,                                  ~
                    lastinvoice$,                                        ~
                    str(variable$(),1,200), orderdate$, canceldate$,     ~
                    jobnr$, datedue$, store$, origamt
            call "DATEOK" (orderdate$, odate%, errormsg$)
            call "GLFMT" (slsacct$)
            return

L30200:     get #lc%, using L31220,                                       ~
                    item$,          part$,          descr$,              ~
                    cat$,           qtyorig,                             ~
                    qtybkord,       price,                               ~
                                    acct$,          datedue$,            ~
                    daterecd$,      datenext$,      lot$,                ~
                    job$,           store$,         oh$

            if part$ = " " then return
            call "GLFMT" (acct$)
            return


L31000:     FMT CH(9),                   /* VENDOR CODE                */~
                CH(16),                  /* PURCHASE ORDER NUMBER      */~
                CH(30),                  /* VENDOR NAME                */~
                CH(60),                  /* ADDRESS LINES 1 & 2        */~
                XX(60),                  /*                            */~
                CH(30),                  /* 3rd ADDRESS LINE           */~
                CH(20),                  /* CONTACT'S NAME             */~
                CH(9),                   /* PAYABLES ACCOUNT DEFAULT   */~
                CH(16),                  /* LAST INVOICE NUMBER        */~
                CH(200),                 /* VARIABLE FIELDS (10 * 20)  */~
                CH(6),                   /* ORDER DATE                 */~
                CH(6),                   /* CANCELATION DATE           */~
                CH(8),                   /* DEFAULT JOB NUMBER         */~
                CH(6),                   /* DEFAULT DUE DATE           */~
                CH(3),                   /* DEFAULT STORE NUMBER       */~
                XX(131),                 /* REST OF RECORD             */~
                PD(14,4)                 /* ORIGINAL ORDER AMOUNT      */

L31220:     FMT XX(9),                   /* VENDOR CODE                */~
                XX(16),                  /* PURCHASE ORDER NUMBER      */~
                XX(3),                   /* SEQUENCE NUMBER            */~
                CH(3),                   /* ITEM NUMBER                */~
                CH(25),                  /* PART NUMBER                */~
                CH(32),                  /* DESCRIPTION                */~
                CH(4),                   /* CATEGORY CODE              */~
                PD(14,4),                /* QUANTITY ORIGINALLY ORDERED*/~
                XX(8),                   /* QUANTITY RECEIVED          */~
                PD(14,4),                /* QUANTITY ON ORDER          */~
                PD(14,7),                /* UNIT PRICE                 */~
                XX(8),                   /* EXTENSION                  */~
                CH(9),                   /* PURCHASES ACCOUNT NUMBER   */~
                CH(6),                   /* DATE DUE INFORMATION       */~
                CH(6),                   /* DATE OF LAST RECEIPT       */~
                CH(6),                   /* DATE OF NEXT RECEIPT       */~
                CH(6),                   /* LOT NUMBER                 */~
                CH(8),                   /* JOB NUMBER                 */~
                CH(3),                   /* STORE NUMBER               */~
                CH(1),                   /* ON HAND POSTING OPTION     */~
                CH(50),                  /* FILLER                     */~
                CH(50),                  /*   DITTO                    */~
                CH(23)                   /*     AH SO...               */

        REM *************************************************************~
            *    P O I N T E R   B U F F E R I N G   R O U T I N E      *~
            *                                                           *~
            * HOLDS THE MAJOR PLOW ROUTINE POINTERS WHILE WE GO DOWN TO *~
            * PO NUMBER DETAIL                                          *~
            *************************************************************

        hold_bckdisplay_pointers:
            bckline%                     = displayline%
            mat bckline$                 = line$
            bckvencode$                  = vencode$
            bckponumber$                 = ponumber$
            bckstore$                    = store$
            return

        restore_bckdisplay_pointers:
            displayline%                 = bckline%
            mat line$                    = bckline$
            vencode$                     = bckvencode$
            ponumber$                    = bckponumber$
            sonumber$                    = bcksonumber$
            store$                       = bckstore$
            return

        REM *************************************************************~
            *      D I S P L A Y   E N T I R E    P O                   *~
            *                                                           *~
            * READS THE ENTIRE PO FROM THE BACKLOG FILE SO WE CAN SHOW  *~
            * DETAIL.                                                   *~
            *************************************************************

        display_entire_po:

            call "READ100" (#mc%, str(nextpokey$,1,25), f1%(mc%))
                 if f1%(mc%) = 0 then return         /* NOT FOUND      */
                 get #mc%, using L34125,                                  ~
                      vencode$, ponumber$, cusname$,                     ~
                      str(shipto$(),1,60), str(shipto$(),61,30),conname$,~
                      slsacct$, lastinvnr$,                              ~
                      str(variable$(), 1), orderdate$,                   ~
                      canceldate$, job$, datedue$, store$, origdate$,    ~
                      origuserid$, lastdate$, lastuserid$, origamt, poamt

            if curr$ = "N" then L33945    /* Multi-Currency not used    */
                line_ext, potranet = 0%
                literal2$ = "---Statutory---  --Transaction--"
                vbklnkey$ = str(vencode$) & str(ponumber$)
                call "PLOWNEXT" (#cc%, vbklnkey$, 25%, f1%(cc%))
                if f1%(cc%) = 0% then L33776   /* Both are STATUTORY    */
L33770:              get #cc% using L33771, literal3$, line_ext
L33771:                   FMT CH(04), POS(41), PD(14,4)
                     potranet = (potranet + line_ext)
                     call "PLOWNEXT" (#cc%, vbklnkey$, 25%, f1%(cc%))
                     if f1%(cc%) = 0% then L33780
                         goto L33770
L33776:         call "CONVERT" (poamt, 2.2, po_tran_net$)
                    potranet = poamt
                    literal3$ = stat_curr_code$
                    goto L33945
L33780:         call "CONVERT" (potranet, 2.2, po_tran_net$)

L33945:     REM FORMAT ALL THE DATA IN THE HEADER
                call "DESCRIBE" (#2, slsacct$, slsacctdescr$,  1%, f1%(2))
                     call "GLFMT" (slsacct$)
                call "DESCRIBE" (#10, job$, jobdescr$, 1%, f1%(10))
                call "DESCRIBE" (#9, store$, storedescr$,      1%, f1%(9))
                call "DATEFMT" (orderdate$)
                call "DATEFMT" (datedue$)
                if canceldate$ = " " or canceldate$ = blankdate$ ~
                   then canceldate$ = "NONE"           ~
                   else call "DATEFMT" (canceldate$)
                call "DATEFMT" (origdate$)
                if lastdate$ = " " or lastdate$ = blankdate$ then ~
                   lastdate$ = "NONE"                        else ~
                   call "DATEFMT" (lastdate$)

L34125:     FMT CH(9),                   /* VENDOR CODE                */~
                CH(16),                  /* PURCHASE ORDER NUMBER      */~
                CH(30),                  /* VENDOR NAME                */~
                CH(60),                  /* ADDRESS 1 & 2              */~
                XX(60),                  /*                            */~
                CH(30),                  /* 3rd ADDRESS                */~
                CH(20),                  /* CONTACT'S NAME             */~
                CH(9),                   /* PAYABLES ACCOUNT DEFAULT   */~
                CH(16),                  /* LAST INVOICE NUMBER        */~
                CH(200),                 /* VARIABLE FIELDS (10 * 20)  */~
                CH(6),                   /* ORDER DATE                 */~
                CH(6),                   /* CANCELATION DATE           */~
                CH(8),                   /* DEFAULT JOB NUMBER         */~
                CH(6),                   /* DEFAULT DUE DATE           */~
                CH(3),                   /* DEFAULT STORE NUMBER       */~
                XX(113),                 /* REST OF RECORD             */~
                CH(6),                   /* ORIG DATE INPUT            */~
                CH(3),                   /* ORIG USERID                */~
                CH(6),                   /* LAST DATE MODIFIED         */~
                CH(3),                   /* LAST ID MODIFYING          */~
                PD(14,4),                /* ORIGINAL ORDER AMOUNT      */~
                PD(14,4)                 /* CURRENT ORDER AMOUNT       */~

L34440: REM DISPLAY FIRST PAGE...........................................
L34442:     init(" ") errormsg$
            gosub po_header_first_screen          /* 1ST PAGE    */
                  if keyhit%  =  0 then       L34575
                  if keyhit%  =  2 then       L34875
                  if keyhit%  =  5 then       L34575
                  if keyhit%  =  8 then gosub see_receipts
                  if keyhit%  = 13 then gosub print_entire_po
                  if keyhit%  = 16 then       return
                     goto L34442

L34575: REM DISPLAY SECOND PAGE...........................................
L34590:     gosub po_header_second_screen
                  if keyhit%  =  0 then       L34725
                  if keyhit%  =  2 then       L34875
                  if keyhit%  =  4 then       L34440
                  if keyhit%  =  5 then       L34725
                  if keyhit%  = 13 then gosub print_entire_po
                  if keyhit%  = 16 then       return
                     goto L34590

L34725: REM DISPLAY THIRD PAGE............................................
L34740:     gosub po_header_third_screen
                  if keyhit%  = 0  then       L34875
                  if keyhit%  = 2  then       L34875
                  if keyhit%  = 4  then       L34575
                  if keyhit%  = 5  then       L34875
                  if keyhit%  = 13 then gosub print_entire_po
                  if keyhit%  = 16 then       return
                     goto L34740

L34875: REM LINE ITEMS..................................................
            REM FIRST, SET KEY INFORMATION.
                str(readkey2$,  1) = vencode$
                str(readkey2$, 10) = ponumber$
                line%, t%, flag% = 0
                REM SET LINE ITEMS TO BLANKS
                    init(" ")                                            ~
                        item$(), part$(), descr$(),                      ~
                        qtyorig$(), qtyonord$(), qtyrecd$(), lit3$(),    ~
                        price$(), oh$()  , extension$(), daterecd$(),    ~
                        acct$(), lot$(), job$(), datenext$(),            ~
                        separator$(), datedue$(), tr_lit3$(), tr_unit$(),~
                        tr_price$(), disp_lit3$(), disp_price$()

L35070:     REM BASIC LOOP WHICH GETS SCREENFUL OF LINES STARTS HERE
                call "PLOWNEXT" (#lc%, readkey2$, 25%, f1%(lc%))
                     if f1%(lc%) = 0 then L35590
                flag% = 1      /* ANYTHING WRITTEN FOR THIS SCREEN?    */
                t% = t% + 1
                line% = line% + 1

                get #lc%, using L36210,                                   ~
                           item$(t%), part$(t%), descr$(t%),             ~
                           cat$(t%), qtyorig, qtyrecd, qtyonord, price,  ~
                           extension, acct$(t%), datedue$(t%),           ~
                           daterecd$(t%), datenext$(t%), lot$(t%),       ~
                           job$(t%), store$(t%), oh$(t%)


                if part$(t%) <> " " then L35400     /* DESCRIPTION ONLY?*/
                   init(" ")                                             ~
                       item$(t%), cat$(t%),             acct$(t%),       ~
                       lot$(t%), job$(t%), datedue$(t%), datenext$(t%),  ~
                       daterecd$(t%)
                   goto L35550

L35400:         if part$(t%) = "LABOR" then L35415
L35415:         call "CONVERT" (qtyorig,  0.2, qtyorig$(t%))
                call "CONVERT" (qtyrecd,  0.2, qtyrecd$(t%))
                call "CONVERT" (qtyonord, 0.2, qtyonord$(t%))
                call "CONVERT" (price,   2.7, price$(t%))
                call "CONVERT" (extension, 2.4, extension$(t%))
                call "GLFMT" (acct$(t%))
                call "DATEFMT" (datedue$(t%))
                call "DATEFMT" (daterecd$(t%))
                call "DATEFMT" (datenext$(t%))

            if curr$ = "N" then L35585     /* Multi-Currency not used   */
                vbklnkey$ = readkey2$
                call "READ100" (#cc%, vbklnkey$, f1%(cc%))
                if f1%(cc%) = 0% then L35550         /* STATUTORY PRICE  */
                     get #cc% using L35534, tr_lit3$(t%), trprice
L35534:                   FMT CH(04), POS(41), PD(14,4)
                     call "CONVERT" (trprice, 2.2, tr_price$(t%))
                     goto L35585
L35550:         tr_lit3$(t%)  = stat_curr_code$
                tr_price$(t%) = price$(t%)

L35585:         if t% < 5 then L35070               /* GET NEXT LINE    */
L35590:         if flag% = 0 then return           /* NO MORE LINES    */

            REM DISPLAY THIS PAGE OF LINE ITEMS
                call "SETSEP" (separator$(), line% - t%, t%)
L35625:         gosub po_line_item_screen
                      if keyhit%  =  0 then       L35745
                      if keyhit%  =  2 then       L34440
                      if keyhit%  =  5 then       L35745
                      if keyhit%  = 13 then gosub print_entire_po
                      if keyhit%  = 16 then       return
                         goto L35625

L35745:         REM BRANCH TO NEXT PAGE OF LINE ITEMS
                    item$      (1) = item$      (5)
                    part$      (1) = part$      (5)
                    descr$     (1) = descr$     (5)
                    cat$       (1) = cat$       (5)
                    acct$      (1) = acct$      (5)
                    qtyrecd$   (1) = qtyrecd$   (5)
                    qtyorig$   (1) = qtyorig$   (5)
                    qtyonord$  (1) = qtyonord$  (5)
                    price$     (1) = price$     (5)
                    lot$       (1) = lot$       (5)
                    job$       (1) = job$       (5)
                    datedue$   (1) = datedue$   (5)
                    datenext$  (1) = datenext$  (5)
                    daterecd$  (1) = daterecd$  (5)
                    extension$ (1) = extension$ (5)
                    separator$ (1) = separator$ (5)

                    for t% = 2 to 5
                        init(" ")                                        ~
                            item$(t%), part$(t%), descr$(t%), cat$(t%),  ~
                            qtyorig$(t%), qtyonord$(t%), qtyrecd$(t%),   ~
                            price$(t%),            extension$(t%),       ~
                                       acct$(t%), lot$(t%), job$(t%),    ~
                            separator$(t%), datedue$(t%), datenext$(t%), ~
                            daterecd$(t%)
                        next t%
                    t% = 1
                    flag% = 0
                    goto L35070

L36210:     FMT XX(9),                   /* VENDOR CODE                */~
                XX(16),                  /* PURCHASE ORDER NUMBER      */~
                XX(3),                   /* SEQUENCE NUMBER            */~
                CH(3),                   /* ITEM NUMBER                */~
                CH(25),                  /* PART NUMBER                */~
                CH(32),                  /* DESCRIPTION                */~
                CH(4),                   /* CATEGORY CODE              */~
                PD(14,4),                /* QUANTITY ORIGINALLY ORDERED*/~
                PD(14,4),                /* QUANTITY RECEIVED          */~
                PD(14,4),                /* QUANTITY ON ORDER          */~
                PD(14,7),                /* UNIT PRICE                 */~
                PD(14,4),                /* EXTENSION                  */~
                CH(9),                   /* PAYABLES ACCOUNT NUMBER    */~
                CH(6),                   /* DATE DUE INFORMATION       */~
                CH(6),                   /* DATE OF LAST RECEIPT       */~
                CH(6),                   /* DATE OF NEXT RECEIPT       */~
                CH(6),                   /* LOT NUMBER                 */~
                CH(8),                   /* JOB NUMBER                 */~
                CH(3),                   /* STORE NUMBER               */~
                CH(1)                    /* ON HAND POSTING OPTION     */
        print_entire_po:
            call "SETPRNT" ("VBK003", " ", 0%, 0%)
            call "SHOSTAT" ("Printing Purchase Order Information")
            select printer (134)         /* SET UP THE PAGE  */
            runtime$ = " "
            call "TIME" (runtime$)
            print page
            print using L37850, today$, runtime$
            print skip(3)
            print using L37860, "VENDOR CODE             ", vencode$;
            print "---------- AUDIT TRAIL DATES ---------"
            print using L37860, "PURCHASE ORDER NUMBER   ", ponumber$,    ~
                               "DATE OF PURCHASE ORDER  ", orderdate$
            print using L37860, "                        ", " ",          ~
                               "DATE ORIGINALLY INPUT   ", origdate$
            print using L37860, "                        ", " "      ,    ~
                               "ORIGINALLY INPUT BY     ", origuserid$
            print using L37860, "VENDOR NAME             ", cusname$,     ~
                               "DATE LAST MODIFIED      ", lastdate$
            print using L37860, "SHIP TO ADDRESS (1)     ", address$(1),  ~
                               "LAST MODIFIED BY        ", lastuserid$
            print using L37860, "SHIP TO ADDRESS (2)     ", address$(2),  ~
                               "                        ", " "
            print using L37860, "SHIP TO ADDRESS (3)     ", address$(3),  ~
                               "                        ", " "
            print using L37860, "VENDOR PHONE NUMBER     ", phone$
            print using L37890, "PURCHASE ACCOUNT        ", slsacct$,     ~
                                                           slsacctdescr$
            print using L37890, "DEFAULT DUE DATE        ", datedue$    , ~
                                                           " "
            print using L37890, "CANCELLATION DATE       ", canceldate$,  ~
                                                           " "
            print using L37890, "STORE NUMBER            ", store$,       ~
                                                           storedescr$
            print
            print using L37860, "VARIABLE FIELD (1)      ", variable$(1)
            print using L37860, "VARIABLE FIELD (2)      ", variable$(2)
            print using L37860, "VARIABLE FIELD (3)      ", variable$(3)
            print using L37860, "VARIABLE FIELD (4)      ", variable$(4)
            print using L37860, "VARIABLE FIELD (5)      ", variable$(5)
            print using L37860, "VARIABLE FIELD (6)      ", variable$(6)
            print using L37860, "VARIABLE FIELD (7)      ", variable$(7)
            print using L37860, "VARIABLE FIELD (8)      ", variable$(8)
            print using L37860, "VARIABLE FIELD (9)      ", variable$(9)
            print using L37860, "VARIABLE FIELD (10)     ", variable$(10)
            print
            if curr$ = "Y" then L37219
                print using L37920, "CURRENT P.O. AMOUNT", poamt, " ", " "
                goto L37230
L37219:     print tab(30), "---STATUTORY---     ---TRANSACTION---"
            print using L37920, "CURRENCT P.O. AMOUNT", poamt, potranet,  ~
                                literal3$
L37230:     REM LINE ITEMS...............................................
                REM FIRST, SET KEY INFORMATION.
                    str(readkey2$,  1) = vencode$
                    str(readkey2$, 10) = ponumber$
                    t% = 1
            print skip (2)
            if curr$ = "Y" then L37339
                print using L37950
                print using L37995
                goto L37350
L37339:     print using L37982
            print using L37995
L37350:     line% = 43

L37380:     REM BASIC LOOP WHICH GETS EACH LINE ITEM
                call "PLOWNEXT" (#lc%, readkey2$, 25%, f1%(lc%))
                     if f1%(lc%) = 1 then L37455      /* END OF PRINT   */
                close printer   :  return

L37455:         get #lc%, using L36210,                                   ~
                            item$(6), part$(6), descr$(6),               ~
                            cat$(6), qtyorig, qtyrecd, qtyonord, price,  ~
                            extension, acct$(6),                         ~
                            datedue$(6), daterecd$(6), datenext$(6),     ~
                            lot$(6), job$(6), store$(6), oh$(6)
                if part$(6) = " " then L37770       /* DESCRIPTION ONLY */
                if part$(6) = "LABOR" then L37575
L37575:         call "CONVERT" (qtyrecd,  0.2, qtyrecd$(6))
                call "CONVERT" (qtyorig,  0.2, qtyorig$(6))
                call "CONVERT" (qtyonord, 0.2, qtyonord$(6))
                call "CONVERT" (price,   2.7, price$(6))
                call "GLFMT" (acct$(6))
                call "DATEFMT" (datedue$(6))
                call "DATEFMT" (daterecd$(6))
                call "DATEFMT" (datenext$(6))

            if curr$ = "N" then L37695      /* Multi-currency not used  */
                vbklnkey$ = readkey2$
                call "READ100" (#cc%, vbklnkey$, f1%(cc%))
                if f1%(cc%) = 0% then L37679    /* Same as STATUTORY     */
                    get #cc% using L37675, tr_lit3$(6), trunit, trprice
L37675:                 FMT CH(04), POS(33), PD(14,7), PD(14,4)
                    call "CONVERT" (trprice, 2.2, tr_price$(6))
                    call "CONVERT" (trunit, 2.7, tr_unit$(6))
                    goto L37695
L37679:         tr_lit3$(6) = stat_curr_code$ : tr_price$(6) = price$(6)

L37695:         gosub control_print_entire_po
                if curr$ = "Y" then                                      ~
                print using L38072, part$(6), store$(6),qtyorig$(6),      ~
                      qtyonord$(6), price$(6), lot$(6), job$(6),         ~
                      daterecd$(6), datedue$(6), datenext$(6),           ~
                      stat_curr_code$, extension                         ~
                else                                                     ~
                print using L38040, part$(6), store$(6),qtyorig$(6),      ~
                      qtyonord$(6), price$(6), lot$(6), job$(6),         ~
                      daterecd$(6), datedue$(6), datenext$(6),           ~
                      extension
L37770:         gosub control_print_entire_po
                if curr$ <> "Y" then L37785
                    if tr_lit3$(6) = stat_curr_code$ then                ~
                        trprice = extension
                    print using L38086, descr$(6), tr_unit$(6),           ~
                                       tr_lit3$(6), trprice
                    goto L37800
L37785:         print using L38085, descr$(6)
L37800:         gosub control_print_entire_po
                print
                goto L37380

L37850: %RUN ########  ########         D E T A I L E D   P U R C H A S E~
        ~   O R D E R   P R I N T O U T                      VBKDSPLY:VBK0~
        ~03

L37855: %CONTINUATION OF LINE ITEMS:

L37860: %############################  ################################  ~
        ~                ############################  ########
L37890: %############################  ##############  ##################~
        ~##############  ############################  ########
L37920: %############################# -##########.##        -##########.~
        ~##  ####
L37950: %PART  NUMBER              STR   ORIGINAL    BO'D    UNIT PRICE  ~
        ~LOT    JOB  NO. RECEIVED    DUE     NEXT                   EXTENS~
        ~ION
L37982: %PART  NUMBER              STR   ORIGINAL    BO'D    UNIT PRICE  ~
        ~LOT    JOB  NO. RECEIVED    DUE     NEXT    CURRENCY       EXTENS~
        ~ION
L37995: %------------------------- --- ---------- ---------- ----------  ~
        ~------ -------- -------- -------- -------- ----------  ----------~
        ~---
L38040:    %######################### ### ########## ########## #########~
        ~# ######  ######## ######## ######## ########             -######~
        ~###.##
L38072:    %######################### ### ########## ########## #########~
        ~# ######  ######## ######## ######## ########    ####     -######~
        ~###.##
L38085: %     ################################
L38086:    %     ################################               #########~
        ~#                                                ####     -######~
        ~###.##
        see_receipts
            REM PONUMBER HISTORY OF RECEIPTS.............................
            call "POSTATUS" (vencode$, ponumber$, " ", 1%, #lc%, #8,     ~
                             #mc%, #11, #12, #4)
            return

        REM *************************************************************~
            *      G E T   V E N D O R       C O D E   S C R E E N      *~
            *                                                           *~
            * GETS VENDOR   CODE OFF SCREEN, ENABLES PF KEY TO PRINT.   *~
            *************************************************************

            deffn'200(fieldnr%)
                  init(hex(8c))linfac$()
                  on fieldnr% gosub  L40160         /* VENDOR   CODE    */
                  gosub L40220
                  return

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      linfac$(fieldnr%) = hex(80)
                      return
L40160:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      linfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT.
                      linfac$(fieldnr%) = hex(82)
                      return
L40220:     accept                                                       ~
               at (01,02),                                               ~
                  "Display/Print Vendor Backlog",                        ~
               at (01,67),                                               ~
                  "Date:",                                               ~
               at (01,73), fac(hex(8c)), today$                 , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Open Orders Only? (Y/N):",                            ~
               at (06,27), fac(linfac$(1)), open$               , ch(01),~
               at (07,02),                                               ~
                  "History Files? (Y/N):",                               ~
               at (07,27), fac(linfac$(1)), history$            , ch(01),~
               at (08,02),                                               ~
                  "Vendor Code To Display:",                             ~
               at (08,27), fac(linfac$(1)), vencode$            , ch(09),~
               at (09,02), "Purchase Order Number:",                     ~
               at (09,27), fac(linfac$(1)), ponum$              , ch(16),~
               at (11,02),                                               ~
                  "Enter 'open' option and Vendor Code then press RETURN ~
        ~to display orders.",                                             ~
               at (12,02),                                               ~
                  "Leave Vendor Code and PO Number Blank to Search for Ve~
        ~ndor.",                                                          ~
               at (13,02),                                               ~
                  "Enter '?' for PO Number to Search for PO Number.",    ~
               at (14,02),                                               ~
                  "Press PF(9) to print Backlog By Vendor Report",       ~
               at (21,02), fac(hex(a4)), blankline$             , ch(79),~
               at (23,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,02),                                               ~
                  "(9)Print Backlog By Vendor",                          ~
               at (24,65),                                               ~
                  "(16)EXIT PROGRAM",                                    ~
                                                                         ~
               keys(hex(0001090d0f10)),                                  ~
               key (keyhit%)

               if keyhit% <> 13 then L40540
                  call "MANUAL" ("VBKDSPLY")
                  goto L40220

L40540:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40220

        REM *************************************************************~
            *    D I S P L A Y   B A C K L O G   L I N E   I T E M S    *~
            *                                                           *~
            * DISPLAY THE LINE ITEMS IN THE PRE-FORMATTED LINE ITEM     *~
            * ARRAY THAT'S BUILT BY THE DISPLAY PRINT ROUTINE ABOVE.    *~
            *************************************************************

        deffn'222

            gosub setpf22

L41074:     mc_display$ = "N"
*        STATUTORY Display Setup
            title$ = " PO Number/Date !Part Number        / Description!D~
        ~ue/Next!Quan/Job#!Extension"
            mat bk_lines$ = line$
            goto L41120
L41082
*        TRANSACTION Display Setup
            title$ = " PO Number/Date !Part/Descr.         Curr./Descr.!D~
        ~ue/Next!Quan/Job#!Extension"
            mat bk_lines$ = tr_lines$

L41120:     accept                                                       ~
               at (01,02), fac(hex(8c)), pf$(1)                 , ch(79),~
               at (02,02), fac(hex(84)), cusdisplay$            , ch(79),~
               at (03,02), fac(hex(ac)), title$                 , ch(79),~
                                                                         ~
               at (04,02), fac(hex(8c)), bk_lines$( 1)          , ch(79),~
               at (05,02), fac(hex(8c)), bk_lines$( 2)          , ch(79),~
               at (06,02), fac(hex(8c)), bk_lines$( 3)          , ch(79),~
               at (07,02), fac(hex(8c)), bk_lines$( 4)          , ch(79),~
               at (08,02), fac(hex(8c)), bk_lines$( 5)          , ch(79),~
               at (09,02), fac(hex(8c)), bk_lines$( 6)          , ch(79),~
               at (10,02), fac(hex(8c)), bk_lines$( 7)          , ch(79),~
               at (11,02), fac(hex(8c)), bk_lines$( 8)          , ch(79),~
               at (12,02), fac(hex(8c)), bk_lines$( 9)          , ch(79),~
               at (13,02), fac(hex(8c)), bk_lines$(10)          , ch(79),~
               at (14,02), fac(hex(8c)), bk_lines$(11)          , ch(79),~
               at (15,02), fac(hex(8c)), bk_lines$(12)          , ch(79),~
               at (16,02), fac(hex(8c)), bk_lines$(13)          , ch(79),~
               at (17,02), fac(hex(8c)), bk_lines$(14)          , ch(79),~
               at (18,02), fac(hex(8c)), bk_lines$(15)          , ch(79),~
               at (19,02), fac(hex(8c)), bk_lines$(16)          , ch(79),~
               at (20,02), fac(hex(8c)), bk_lines$(17)          , ch(79),~
               at (21,02), fac(hex(8c)), bk_lines$(18)          , ch(79),~
               at (22,02), fac(hex(8c)), bk_lines$(19)          , ch(79),~
               at (23,02), fac(hex(ac)), bk_lines$(20)          , ch(79),~
                        keys(str(pfkeys$)), key(keyhit%)

               if curr$ = "N" then L41440

               if keyhit% <>  7 then L41440
                   if mc_display$ = "Y" then L41419
                       mc_display$ = "Y"
                       str(pf$(1),23,7) = "(7)Stat"
                       goto L41082
L41419:            str(pf$(1),23,7) = "(7)Tran"
                   goto L41074

L41440:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  return

            setpf22
               if curr$ = "N" then pf$(1) = "(1)Strt Over (2)1st (5)Nxt (~
        ~8)Sum (12)Detail (14)Prt Ven (15)Prt Scrn  (16)Next"             ~
               else pf$(1) = "(1)Over (2)1st (5)Nxt (7)Tran (8)Sum (12)Dt~
        ~l (14)Prt Ven (15)Prt Scrn (16)Next"
               pfkeys$ = hex(000102ffff05ff0708ffffff0cff0e0f10)
               if curr$ = "Y" then return
                   str(pfkeys$,8,1) = hex(ff)
               return

L42000: REM *************************************************************~
            *   D I S P L A Y   S U M M A R Y   I N F O R M A T I O N   *~
            *                                                           *~
            * DISPLAYS SUMMARY INFORMATION FOR THE VENDOR     BACKLOG,  *~
            * INCLUDING BACKLOG AGING DISPLAY.                          *~
            *************************************************************

            deffn'223

            if curr$ = "N" then literal1$ = " " else                     ~
                                literal1$ = "---Statutory---"

L42050:     accept                                                       ~
               at (01,03),                                               ~
                  "(1)Start Over (2)Orders (14)Print Vendor   (15)Print S~
        ~creen (16)Next Vendor",                                          ~
               at (03,03),                                               ~
                  "Vendor",                                              ~
               at (03,12), fac(hex(84)), tempcus$               , ch(11),~
               at (04,12), fac(hex(84)), cusname$               , ch(30),~
               at (05,12), fac(hex(84)), address$(1)            , ch(30),~
               at (06,12), fac(hex(84)), address$(2)            , ch(30),~
               at (07,12), fac(hex(84)), address$(3)            , ch(30),~
               at (12,31), fac(hex(8c)), literal1$              , ch(15),~
               at (24,19),                                               ~
                  "Payables Date:",                                      ~
               at (24,34), fac(hex(8c)), screendate$            , ch(08),~
               at (14,14),                                               ~
                  "Current Orders",                                      ~
               at (14,34), fac(hex(8c)), current$               , ch(12),~
               at (15,14),                                               ~
                  "Past Due Orders",                                     ~
               at (15,34), fac(hex(8c)), pastdue$               , ch(12),~
               at (16,14),                                               ~
                  "================================",                    ~
               at (17,14),                                               ~
                  "Total Orders",                                        ~
               at (17,34), fac(hex(8c)), total$                 , ch(12),~
                                                                         ~
               at (03,60),                                               ~
                  "18-Month Backlog",                                    ~
               at (04,55),                                               ~
                  "+-------------------------",                          ~
               at (05,55),                                               ~
                  "!   Past  Due",                                       ~
               at (05,71), fac(hex(8c)), pastdue2$              , ch(10),~
               at (06,55),                                               ~
                  "!",                                                   ~
               at (06,60), fac(hex(8c)), dspdate$( 1)           , ch(08),~
               at (06,71), fac(hex(8c)), amount$( 1)            , ch(10),~
               at (07,55),                                               ~
                  "!",                                                   ~
               at (07,60), fac(hex(8c)), dspdate$( 2)           , ch(08),~
               at (07,71), fac(hex(8c)), amount$( 2)            , ch(10),~
               at (08,55),                                               ~
                  "!",                                                   ~
               at (08,60), fac(hex(8c)), dspdate$( 3)           , ch(08),~
               at (08,71), fac(hex(8c)), amount$( 3)            , ch(10),~
               at (09,55),                                               ~
                  "!",                                                   ~
               at (09,60), fac(hex(8c)), dspdate$( 4)           , ch(08),~
               at (09,71), fac(hex(8c)), amount$( 4)            , ch(10),~
               at (10,55),                                               ~
                  "!",                                                   ~
               at (10,60), fac(hex(8c)), dspdate$( 5)           , ch(08),~
               at (10,71), fac(hex(8c)), amount$( 5)            , ch(10),~
               at (11,55),                                               ~
                  "!",                                                   ~
               at (11,60), fac(hex(8c)), dspdate$( 6)           , ch(08),~
               at (11,71), fac(hex(8c)), amount$( 6)            , ch(10),~
               at (12,55),                                               ~
                  "!",                                                   ~
               at (12,60), fac(hex(8c)), dspdate$( 7)           , ch(08),~
               at (12,71), fac(hex(8c)), amount$( 7)            , ch(10),~
               at (13,55),                                               ~
                  "!",                                                   ~
               at (13,60), fac(hex(8c)), dspdate$( 8)           , ch(08),~
               at (13,71), fac(hex(8c)), amount$( 8)            , ch(10),~
               at (14,55),                                               ~
                  "!",                                                   ~
               at (14,60), fac(hex(8c)), dspdate$( 9)           , ch(08),~
               at (14,71), fac(hex(8c)), amount$( 9)            , ch(10),~
               at (15,55),                                               ~
                  "!",                                                   ~
               at (15,60), fac(hex(8c)), dspdate$(10)           , ch(08),~
               at (15,71), fac(hex(8c)), amount$(10)            , ch(10),~
               at (16,55),                                               ~
                  "!",                                                   ~
               at (16,60), fac(hex(8c)), dspdate$(11)           , ch(08),~
               at (16,71), fac(hex(8c)), amount$(11)            , ch(10),~
               at (17,55),                                               ~
                  "!",                                                   ~
               at (17,60), fac(hex(8c)), dspdate$(12)           , ch(08),~
               at (17,71), fac(hex(8c)), amount$(12)            , ch(10),~
               at (18,55),                                               ~
                  "!",                                                   ~
               at (18,60), fac(hex(8c)), dspdate$(13)           , ch(08),~
               at (18,71), fac(hex(8c)), amount$(13)            , ch(10),~
               at (19,55),                                               ~
                  "!",                                                   ~
               at (19,60), fac(hex(8c)), dspdate$(14)           , ch(08),~
               at (19,71), fac(hex(8c)), amount$(14)            , ch(10),~
               at (20,55),                                               ~
                  "!",                                                   ~
               at (20,60), fac(hex(8c)), dspdate$(15)           , ch(08),~
               at (20,71), fac(hex(8c)), amount$(15)            , ch(10),~
               at (21,55),                                               ~
                  "!",                                                   ~
               at (21,60), fac(hex(8c)), dspdate$(16)           , ch(08),~
               at (21,71), fac(hex(8c)), amount$(16)            , ch(10),~
               at (22,55),                                               ~
                  "!",                                                   ~
               at (22,60), fac(hex(8c)), dspdate$(17)           , ch(08),~
               at (22,71), fac(hex(8c)), amount$(17)            , ch(10),~
               at (23,55),                                               ~
                  "!",                                                   ~
               at (23,60), fac(hex(8c)), dspdate$(18)           , ch(08),~
               at (23,71), fac(hex(8c)), amount$(18)            , ch(10),~
               at (24,55),                                               ~
                  "!Past",                                               ~
               at (24,61), fac(hex(8c)), futuredate$            , ch(07),~
               at (24,71), fac(hex(8c)), futureamount$          , ch(10),~
                                                                         ~
               keys(hex(0102050e0f10)),                                  ~
               key (keyhit%)

               if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L42050

L43000: REM *************************************************************~
            *       D I S P L A Y   F O R   P R I N T   R A N G E       *~
            *                                                           *~
            * GETS A VENDOR   CODE (W/WILDCARD SPECS) FOR PRINTING A    *~
            * BACKLOG BY CUSTOMR REPORT.                                *~
            *************************************************************
            blankline$ = "Enter Vendor and Date Ranges then Press RE"    ~
                       & "TURN to Print."

            accept                                                       ~
               at (01,02),                                               ~
                  "Print Backlog By Vendor",                             ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), today$                 , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Starting Vendor Code",                       ~
               at (06,24), fac(hex(81)), firstcust$             , ch(09),~
               at (06,37), "Ending Vendor Code",                         ~
               at (06,57), fac(hex(81)), lastcust$              , ch(09),~
                                                                         ~
               at (08,02), "Starting PO Date",                           ~
               at (08,24), fac(hex(81)), fromdate$              , ch(10),~
               at (08,37), "Ending PO Date",                             ~
               at (08,57), fac(hex(81)), todate$                , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)), blankline$             , ch(79),~
               at (23,02),                                               ~
                  "(1)Start Over",                                       ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)EXIT PROGRAM",                                    ~
                                                                         ~
               keys(hex(00010f10)),                                      ~
               key (keyhit%)

               if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  return

        REM *************************************************************~
            * D I S P L A Y   E N T I R E   P O    H E A D E R          *~
            *                                                           *~
            * DISPLAYS THE ENTIRE HEADER OF A PURCHASE ORDER            *~
            *************************************************************

        po_header_first_screen:
            init (hex(8c)) lfac$()
            pfkey$="(2)Line Items (5)Next Page (8)PO History (13)Print PO~
        ~ (15)Print Screen (16)EXIT"
L44085: accept                                                           ~
               at (01,02), fac(hex(8c)), pfkey$                 , ch(79),~
               at (02,75),                                               ~
                  "Page 1",                                              ~
               at (04,02), fac(hex(84)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Vendor Code",                                         ~
               at (06,30), fac(lfac$( 1)), vencode$             , ch(09),~
               at (07,02),                                               ~
                  "Purchase Order Number",                               ~
               at (07,30), fac(lfac$( 2)), ponumber$            , ch(16),~
               at (08,02),                                               ~
                  "Vendor Name",                                         ~
               at (08,30), fac(lfac$( 3)),  cusname$            , ch(30),~
               at (09,02),                                               ~
                  "Vendor Address: Line 1",                              ~
               at (09,30), fac(lfac$( 4)), address$(1)          , ch(30),~
               at (10,02),                                               ~
                  "                Line 2",                              ~
               at (10,30), fac(lfac$( 5)), address$(2)          , ch(30),~
               at (11,02),                                               ~
                  "                Line 3",                              ~
               at (11,30), fac(lfac$( 6)), address$(3)          , ch(30),~
               at (12,02),                                               ~
                  "Contact's Name",                                      ~
               at (12,30), fac(lfac$( 7)), conname$             , ch(20),~
               at (13,02),                                               ~
                  "Payables Account Default",                            ~
               at (13,30), fac(lfac$( 8)), slsacct$             , ch(16),~
               at (13,49), fac(hex(8c)),   slsacctdescr$        , ch(32),~
               at (14,02),                                               ~
                  "Last Invoice Number",                                 ~
               at (14,30), fac(lfac$( 9)), lastinvnr$           , ch(16),~
               at (15,02),                                               ~
                  "Order Date",                                          ~
               at (15,30), fac(lfac$(10)), orderdate$           , ch(08),~
               at (16,02),                                               ~
                  "Cancellation Date",                                   ~
               at (16,30), fac(lfac$(11)), canceldate$          , ch(08),~
               at (17,02),                                               ~
                  "Default Job Number",                                  ~
               at (17,30), fac(lfac$(12)), jobnr$               , ch(08),~
               at (17,49), fac(hex(8c)),   jobdescr$            , ch(32),~
               at (18,02),                                               ~
                  "Default Due Date",                                    ~
               at (18,30), fac(lfac$(13)), datedue$             , ch(08),~
               at (19,02),                                               ~
                  "Default Store Number",                                ~
               at (19,30), fac(lfac$(14)), store$               , ch(03),~
               at (19,49), fac(hex(8c)),   storedescr$          , ch(32),~
               keys (hex(000205080d0f10)),                               ~
               key  (keyhit%)

               if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L44085


        po_header_second_screen:

            if curr$ = "Y" then L44690
                literal2$, literal3$, po_tran_net$ = " "

L44690:     pfkey$="(2)Line Items  (4)First  (5)Next  (13)Print PO       ~
        ~(15)Print Screen  (16)EXIT"

L44710:     accept                                                       ~
               at (01,02), fac(hex(ac)), pfkey$                 , ch(79),~
               at (04,02),                                               ~
                  "Vendor Code",                                         ~
               at (04,30), fac(hex(84)), vencode$               , ch(09),~
               at (05,02),                                               ~
                  "Purchase Order Number",                               ~
               at (05,30), fac(hex(84)), ponumber$              , ch(16),~
                                                                         ~
               at (07,26), fac(hex(8c)), literal2$              , ch(32),~
               at (08,02),                                               ~
                  "Net Order Amount",                                    ~
               at (08,29), fac(hex(8c)), poamt       , pic(-########.##),~
               at (08,46), fac(hex(8c)), po_tran_net$           , ch(12),~
               at (08,60), fac(hex(8c)), literal3$              , ch(04),~
               at (11,02),                                               ~
                  "---------- AUDIT TRAIL DATES ----------",             ~
               at (12,02),                                               ~
                  "Date of Purchase Order",                              ~
               at (12,30), fac(hex(8c)), orderdate$             , ch(08),~
               at (13,02),                                               ~
                  "Date Originally Input",                               ~
               at (13,30), fac(hex(8c)), origdate$              , ch(08),~
               at (14,02),                                               ~
                  "Originally Input By",                                 ~
               at (14,30), fac(hex(8c)), origuserid$            , ch(03),~
               at (15,02),                                               ~
                  "Date Last Modified",                                  ~
               at (15,30), fac(hex(8c)), lastdate$              , ch(08),~
               at (16,02),                                               ~
                  "Last Modified By",                                    ~
               at (16,30), fac(hex(8c)), lastuserid$            , ch(03),~
                                                                         ~
               keys (hex(000204050d0f10)),                               ~
               key  (keyhit%)

               if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L44710

        po_header_third_screen:
            pfkey$="(2)Line Items  (4)Prev.  (5)Next  (13)Print PO       ~
        ~(15)Print Screen  (16)EXIT"
L45190:     accept                                                       ~
               at (01,02), fac(hex(ac)), pfkey$                 , ch(79),~
               at (04,02),                                               ~
                  "Vendor Code",                                         ~
               at (04,30), fac(hex(84)), vencode$               , ch(09),~
               at (05,02),                                               ~
                  "Purchase Order Number",                               ~
               at (05,30), fac(hex(84)), ponumber$              , ch(16),~
               at (08,02),                                               ~
                  "Variable Field 1",                                    ~
               at (08,30), fac(hex(8c)), variable$(1)           , ch(30),~
               at (09,02),                                               ~
                  "Variable Field 2",                                    ~
               at (09,30), fac(hex(8c)), variable$(2)           , ch(30),~
               at (10,02),                                               ~
                  "Variable Field 3",                                    ~
               at (10,30), fac(hex(8c)), variable$(3)           , ch(30),~
               at (11,02),                                               ~
                  "Variable Field 4",                                    ~
               at (11,30), fac(hex(8c)), variable$(4)           , ch(30),~
               at (12,02),                                               ~
                  "Variable Field 5",                                    ~
               at (12,30), fac(hex(8c)), variable$(5)           , ch(30),~
               at (13,02),                                               ~
                  "Variable Field 6",                                    ~
               at (13,30), fac(hex(8c)), variable$(6)           , ch(30),~
               at (14,02),                                               ~
                  "Variable Field 7",                                    ~
               at (14,30), fac(hex(8c)), variable$(7)           , ch(30),~
               at (15,02),                                               ~
                  "Variable Field 8",                                    ~
               at (15,30), fac(hex(8c)), variable$(8)           , ch(30),~
               at (16,02),                                               ~
                  "Variable Field 9",                                    ~
               at (16,30), fac(hex(8c)), variable$(9)           , ch(30),~
               at (17,02),                                               ~
                  "Variable Field 10",                                   ~
               at (17,30), fac(hex(8c)), variable$(10)          , ch(30),~
                                                                         ~
               keys (hex(000204050d0f10)),                               ~
               key  (keyhit%)

               if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L45190

        REM *************************************************************~
            *           D I S P L A Y    L I N E    I T E M S           *~
            *                                                           *~
            * DISPLAYS LINE ITEMS FOR A PURCHASE ORDER                  *~
            *************************************************************

        po_line_item_screen:

            gosub setpf33
L46063
*        STATUTORY - PO Line Item Setup.................................
            mc_display$ = "N"
            mat disp_price$ = price$
            mat disp_lit3$  = lit3$
            go to L46090
*        TRANSACTION - PO Line Item Setup...............................
L46084:     mat disp_price$ = tr_price$
            mat disp_lit3$  = tr_lit3$

L46090:     accept                                                       ~
               at (01,02), fac(hex(ac)), pf$(1)                 , ch(79),~
                                                                         ~
               at (02,02), "Purchase Order Number:",                     ~
               at (02,25), fac(hex(84)), ponumber$              , ch(16),~
               at (02,49), "Vendor:",                                    ~
               at (02,59), fac(hex(84)), vencode$               , ch(09),~
                                                                         ~
               at (05,02), fac(hex(84)), separator$(1)          , ch(79),~
                                                                         ~
               at (06,02), fac(hex(84)), item$(1)               , ch(03),~
               at (06,06), "P/N",                                        ~
               at (06,10), fac(hex(84)), part$(1)               , ch(25),~
               at (06,36), "(",                                          ~
               at (06,38), fac(hex(84)), descr$(1)              , ch(32),~
               at (06,71), ")",                                          ~
               at (06,73), "Str",                                        ~
               at (06,77), fac(hex(84)), store$(1)              , ch(03),~
                                                                         ~
               at (07,02), "Cat",                                        ~
               at (07,06), fac(hex(84)), cat$(1)                , ch(04),~
               at (07,11), "Orig",                                       ~
               at (07,16), fac(hex(84)), qtyorig$(1)            , ch(10),~
               at (07,27), "On-Ord",                                     ~
               at (07,34), fac(hex(84)), qtyonord$(1)           , ch(10),~
               at (07,45), "Recd",                                       ~
               at (07,50), fac(hex(84)), qtyrecd$(1)            , ch(10),~
               at (07,61), "Prce",                                       ~
               at (07,66), fac(hex(84)), disp_price$(1)         , ch(10),~
               at (07,77), fac(hex(84)), disp_lit3$(1)          , ch(04),~
                                                                         ~
               at (08,02), "Acc",                                        ~
               at (08,06), fac(hex(84)), acct$(1)               , ch(12),~
               at (08,19), "Due",                                        ~
               at (08,23), fac(hex(84)), datedue$(1)            , ch(08),~
               at (08,32), "Rec",                                        ~
               at (08,36), fac(hex(84)), daterecd$(1)           , ch(08),~
               at (08,45), "Nxt",                                        ~
               at (08,48), fac(hex(84)), datenext$(1)           , ch(05),~
               at (08,54), "LT",                                         ~
               at (08,57), fac(hex(84)), lot$(1)                , ch(06),~
               at (08,64), "JB",                                         ~
               at (08,67), fac(hex(84)), job$(1)                , ch(08),~
               at (08,76), "OH",                                         ~
               at (08,79), fac(hex(84)), oh$(1)                 , ch(01),~
                                                                         ~
               at (09,02), fac(hex(84)),    separator$(2)       , ch(79),~
                                                                         ~
               at (10,02), fac(hex(84)), item$(2)               , ch(03),~
               at (10,06), "P/N",                                        ~
               at (10,10), fac(hex(84)), part$(2)               , ch(25),~
               at (10,36), "(",                                          ~
               at (10,38), fac(hex(84)), descr$(2)              , ch(32),~
               at (10,71), ")",                                          ~
               at (10,73), "Str",                                        ~
               at (10,77), fac(hex(84)), store$(2)              , ch(03),~
                                                                         ~
               at (11,02), "Cat",                                        ~
               at (11,06), fac(hex(84)), cat$(2)                , ch(04),~
               at (11,11), "Orig",                                       ~
               at (11,16), fac(hex(84)), qtyorig$(2)            , ch(10),~
               at (11,27), "On-Ord",                                     ~
               at (11,34), fac(hex(84)), qtyonord$(2)           , ch(10),~
               at (11,45), "Recd",                                       ~
               at (11,50), fac(hex(84)), qtyrecd$(2)            , ch(10),~
               at (11,61), "Prce",                                       ~
               at (11,66), fac(hex(84)), disp_price$(2)         , ch(10),~
               at (11,77), fac(hex(84)), disp_lit3$(2)          , ch(04),~
                                                                         ~
               at (12,02), "Acc",                                        ~
               at (12,06), fac(hex(84)), acct$(2)               , ch(12),~
               at (12,19), "Due",                                        ~
               at (12,23), fac(hex(84)), datedue$(2)            , ch(08),~
               at (12,32), "Rec",                                        ~
               at (12,36), fac(hex(84)), daterecd$(2)           , ch(08),~
               at (12,45), "Nxt",                                        ~
               at (12,48), fac(hex(84)), datenext$(2)           , ch(05),~
               at (12,54), "LT",                                         ~
               at (12,57), fac(hex(84)), lot$(2)                , ch(06),~
               at (12,64), "JB",                                         ~
               at (12,67), fac(hex(84)), job$(2)                , ch(08),~
               at (12,75), "OH",                                         ~
               at (12,78), fac(hex(84)), oh$(2)                 , ch(01),~
                                                                         ~
               at (13,02), fac(hex(84)),    separator$(3)       , ch(79),~
                                                                         ~
               at (14,02), fac(hex(84)), item$(3)               , ch(03),~
               at (14,06), "P/N",                                        ~
               at (14,10), fac(hex(84)), part$(3)               , ch(25),~
               at (14,36), "(",                                          ~
               at (14,38), fac(hex(84)), descr$(3)              , ch(32),~
               at (14,71), ")",                                          ~
               at (14,73), "Str",                                        ~
               at (14,77), fac(hex(84)), store$(3)              , ch(03),~
                                                                         ~
               at (15,02), "Cat",                                        ~
               at (15,06), fac(hex(84)), cat$(3)                , ch(04),~
               at (15,11), "Orig",                                       ~
               at (15,16), fac(hex(84)), qtyorig$(3)            , ch(10),~
               at (15,27), "On-Ord",                                     ~
               at (15,34), fac(hex(84)), qtyonord$(3)           , ch(10),~
               at (15,45), "Recd",                                       ~
               at (15,50), fac(hex(84)), qtyrecd$(3)            , ch(10),~
               at (15,61), "Prce",                                       ~
               at (15,66), fac(hex(84)), disp_price$(3)         , ch(10),~
               at (15,77), fac(hex(84)), disp_lit3$(3)          , ch(04),~
                                                                         ~
               at (16,02), "Acc",                                        ~
               at (16,06), fac(hex(84)), acct$(3)               , ch(12),~
               at (16,19), "Due",                                        ~
               at (16,23), fac(hex(84)), datedue$(3)            , ch(08),~
               at (16,32), "Rec",                                        ~
               at (16,36), fac(hex(84)), daterecd$(3)           , ch(08),~
               at (16,45), "Nxt",                                        ~
               at (16,48), fac(hex(84)), datenext$(3)           , ch(05),~
               at (16,54), "LT",                                         ~
               at (16,57), fac(hex(84)), lot$(3)                , ch(06),~
               at (16,64), "JB",                                         ~
               at (16,67), fac(hex(84)), job$(3)                , ch(08),~
               at (16,75), "OH",                                         ~
               at (16,78), fac(hex(84)), oh$(3)                 , ch(01),~
                                                                         ~
               at (17,02), fac(hex(84)),    separator$(4)       , ch(79),~
                                                                         ~
               at (18,02), fac(hex(84)), item$(4)               , ch(03),~
               at (18,06), "P/N",                                        ~
               at (18,10), fac(hex(84)), part$(4)               , ch(25),~
               at (18,36), "(",                                          ~
               at (18,38), fac(hex(84)), descr$(4)              , ch(32),~
               at (18,71), ")",                                          ~
               at (18,73), "Str",                                        ~
               at (18,77), fac(hex(84)), store$(4)              , ch(03),~
                                                                         ~
               at (19,02), "Cat",                                        ~
               at (19,06), fac(hex(84)), cat$(4)                , ch(04),~
               at (19,11), "Orig",                                       ~
               at (19,16), fac(hex(84)), qtyorig$(4)            , ch(10),~
               at (19,27), "On-Ord",                                     ~
               at (19,34), fac(hex(84)), qtyonord$(4)           , ch(10),~
               at (19,45), "Recd",                                       ~
               at (19,50), fac(hex(84)), qtyrecd$(4)            , ch(10),~
               at (19,61), "Prce",                                       ~
               at (19,66), fac(hex(84)), disp_price$(4)         , ch(10),~
               at (19,77), fac(hex(84)), disp_lit3$(4)          , ch(04),~
                                                                         ~
               at (20,02), "Acc",                                        ~
               at (20,06), fac(hex(84)), acct$(4)               , ch(12),~
               at (20,19), "Due",                                        ~
               at (20,23), fac(hex(84)), datedue$(4)            , ch(08),~
               at (20,32), "Rec",                                        ~
               at (20,36), fac(hex(84)), daterecd$(4)           , ch(08),~
               at (20,45), "Nxt",                                        ~
               at (20,48), fac(hex(84)), datenext$(4)           , ch(05),~
               at (20,54), "LT",                                         ~
               at (20,57), fac(hex(84)), lot$(4)                , ch(06),~
               at (20,64), "JB",                                         ~
               at (20,67), fac(hex(84)), job$(4)                , ch(08),~
               at (20,75), "OH",                                         ~
               at (20,78), fac(hex(84)), oh$(4)                 , ch(01),~
                                                                         ~
               at (21,02), fac(hex(84)),    separator$(5)       , ch(79),~
                                                                         ~
               at (22,02), fac(hex(84)), item$(5)               , ch(03),~
               at (22,06), "P/N",                                        ~
               at (22,10), fac(hex(84)), part$(5)               , ch(25),~
               at (22,36), "(",                                          ~
               at (22,38), fac(hex(84)), descr$(5)              , ch(32),~
               at (22,71), ")",                                          ~
               at (22,73), "Str",                                        ~
               at (22,77), fac(hex(84)), store$(5)              , ch(03),~
                                                                         ~
               at (23,02), "Cat",                                        ~
               at (23,06), fac(hex(84)), cat$(5)                , ch(04),~
               at (23,11), "Orig",                                       ~
               at (23,16), fac(hex(84)), qtyorig$(5)            , ch(10),~
               at (23,27), "On-Ord",                                     ~
               at (23,34), fac(hex(84)), qtyonord$(5)           , ch(10),~
               at (23,45), "Recd",                                       ~
               at (23,50), fac(hex(84)), qtyrecd$(5)            , ch(10),~
               at (23,61), "Prce",                                       ~
               at (23,66), fac(hex(84)), disp_price$(5)         , ch(10),~
               at (23,77), fac(hex(84)), disp_lit3$(5)          , ch(04),~
                                                                         ~
               at (24,02), "Acc",                                        ~
               at (24,06), fac(hex(84)), acct$(5)               , ch(12),~
               at (24,19), "Due",                                        ~
               at (24,23), fac(hex(84)), datedue$(5)            , ch(08),~
               at (24,32), "Rec",                                        ~
               at (24,36), fac(hex(84)), daterecd$(5)           , ch(08),~
               at (24,45), "Nxt",                                        ~
               at (24,48), fac(hex(84)), datenext$(5)           , ch(05),~
               at (24,54), "LT",                                         ~
               at (24,57), fac(hex(84)), lot$(5)                , ch(06),~
               at (24,64), "JB",                                         ~
               at (24,67), fac(hex(84)), job$(5)                , ch(08),~
               at (24,75), "OH",                                         ~
               at (24,78), fac(hex(84)), oh$(5)                 , ch(01),~
                        keys(str(pfkeys$)), key(keyhit%)

               if curr$ = "N" then L48070

               if keyhit% <>  7 then L48070
                  if mc_display$ = "Y" then L48048
                      mc_display$ = "Y"
                      str(pf$(1),30,7) = "(7)Stat"
                      goto L46084
L48048:           str(pf$(1),30,7) = "(7)Tran"
                  goto L46063

L48070:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L46090

            setpf33
               if curr$ = "N" then                                       ~
               pf$(1)="(2)Header Page  (5)Next Page  (13)Print PO        ~
        ~(15)Print Screen  (16)EXIT    "                                  ~
               else                                                      ~
               pf$(1)="(2)Header Page  (5)Nxt Page  (7)Tran  (13)Print "&~
        "PO  (15)Print Screen   (16)EXIT  "
               pfkeys$ = hex(000002ffff05ff07ffffffffff0dff0f10)
               if curr$ = "Y" then return
                   str(pfkeys$,8,1) = hex(ff)
                   return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * BASICALLY, ALL WE DO IN THIS PROGRAM IS TEST TO MAKE SURE *~
            * THE VENDOR   CODE IS ON FILE AND THEN LOAD UP ALL SORTS OF*~
            * INFORMATION USED IN THE SUMMARY PAGE.                     *~
            *************************************************************

            deffn'150(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50120          /* VENDOR   CODE    */
                     return

L50120:           gosub test_for_history
                      if errormsg$ <> " " then return
                  REM TEST FOR EXISTING VENDOR PURCHASE ORDER
                  if vencode$ =  " " and ponum$  = " " then L50220
                  if vencode$ =  " " and ponum$ <> " " then L50265
                  if vencode$ <> " " and ponum$  = " " then L50170
                  if vencode$ <> " " and ponum$ <> " " then L50170
                  goto L10120

L50170:           call "READ100" (#3, vencode$, f1%(3))
                     if f1%(3) = 0% then L50220
                     if ponum$ <> " " then L50265
                     goto L50360

L50220:           call "GETCODE" (#3, vencode$, cusname$, 0%, 1.3, f1%(3))
                     if f1%(3) = 0% then L10120
                     if ponum$ <> " " then L50279
                     goto L10120

L50265:           call "REDALT0" (#mc%, ponum$, 1%, f1%(mc%))
                       if f1%(mc%) = 0% then L50279
                       get #mc%, using L50268, vencode$
L50268:                                FMT  CH(9)
                  str(plowkey$,,25) = str(vencode$) & ponum$
                  str(plowkey$,26) = all(hex(00))
                  call "READ104" (#lc%, plowkey$, f1%(lc%))
                       if f1%(lc%) = 0% then L50279
                       get #lc% using L50274, vq_out
L50274:                       FMT POS(109), PD(14,4)
                  if vq_out = 0 and open$ = "Y" then L50279
                  goto L50360

L50279:           if open$ = "Y" then L50315
                  descr$ = hex(06) & "Select PO Number to Display"
                  header$(2) = "  Vendor   PO Number          Vendor Name"
                  str(plowkey$,1,25) = str(vencode$) & hex(00)
                  incl_excl(1) = 0
                  incl_excl$(1) = " "
                  goto L50342
L50315:           descr$ = hex(06) & "Select Open Order to Display"
                  header$(2) = "  Vendor   PO Number          Vendor Name"
*                PLOWKEY$ = HEX(2021)
                  str(plowkey$,1,25) = str(vencode$) & hex(00)
                  incl_excl(1) = -109.08
                  incl_excl$(1) = hex(000000000000000f)
L50342:           call "PLOWCODE" (#lc%, plowkey$, descr$, -8025%,       ~
                                   -0.30, f1%(lc%), header$(), 0, 0,     ~
                                   incl_excl(), incl_excl$(), " ", " ",  ~
                                   #mc%)
                  if f1%(lc%) = 1% then L50354
                  errormsg$ = "PO Number Not Selected / On File."
                  return

L50354:           vencode$ = str(plowkey$,,9)
                  ponum$ = str(plowkey$,10)

L50360:           REM NOW GET VENDOR   NAME AND ADDRESS, AND PHONE
                      get #3, using L50400,  str(address$(),1,60),        ~
                                            str(address$(),61,30), temp$

L50400:                                 FMT XX(69),/* VENDOR / NAME    */~
                                            CH(60),/* ADDRESS (2*30)   */~
                                            XX(60),          /* SKIP   */~
                                            CH(30),/* ADDRESS          */~
                                            XX(20),          /* SKIP   */~
                                            CH(10) /* PHONE  NUMBER    */

                           if temp$ = " " then return
                           str(temp$, 11, 2) = "--"
                           phone$ = hex(0001020a0304050b06070809)
                           tran(phone$,temp$)
                                  return

        test_for_history
            if history$ = "Y" or history$ = "N" then L50570
                errormsg$ = "Display History Info 'Y'es or 'N'."
                return
L50570:     mc% = 5% : lc% = 6% : cc% = 40% /* Prime as Current Files */
            hist_msg$ = "Current"
            if history$ = "N" then return
                mc% = 15% : lc% = 16% : cc% = 42%    /* History Files */
                hist_msg$ = "History"
                return

L51000: REM *************************************************************~
            *     T E S T   D A T A   F O R   R A N G E   P R I N T     *~
            *                                                           *~
            * MAKES SURE THAT THE LAST IS NOT LESS THAN THE FIRST,      *~
            * NORMALIZES THE KEYS FOR PLOWING (IN THE CASE OF 'ALL'     *~
            * OR SINGLE VENDOR  ), ETC.                                 *~
            *************************************************************

            call "TESTRNGE" (firstcust$, lastcust$, fromcust$, tocust$,  ~
                             errormsg$)
            if errormsg$ <> " " then return

            if fromdate$ <> "ALL" then L51160
                str(date_msg$,13,) = fromdate$
                goto L51260
L51160:     if todate$ = " " then todate$ = fromdate$
            call "DATEOKC" (fromdate$, fdate%, errormsg$)
                if errormsg$ <> " " then return
            call "DATEOKC" (todate$, tdate%, errormsg$)
                if errormsg$ <> " " then return
            if tdate% = fdate% or tdate% > fdate% then L51250
                errormsg$ = "Ending Date MUST be EQUAL or GREATER than "&~
                            "the Starting Date"
                return
L51250:     str(date_msg$,13,) = fromdate$ & " thru " & todate$
L51260:     call "STRING" addr("CT", date_msg$, 35%)
            gosub test_for_history
                return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")
            call "SETPRNT" ("VBK002", " ", 0%, 1%)

            end
