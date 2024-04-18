        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS   H   H  PPPP   X   X   CCC   FFFFF  RRRR   M   M   *~
            *  S      H   H  P   P   X X   C   C  F      R   R  MM MM   *~
            *   SSS   HHHHH  PPPP     X    C      FFFF   RRRR   M M M   *~
            *      S  H   H  P       X X   C   C  F      R   R  M   M   *~
            *   SSS   H   H  P      X   X   CCC   F      R   R  M   M   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SHPXCFRM - Confirms shipment and sets up for update       *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/10/87 ! Original                                 ! MJB *~
            * 08/22/88 ! Corrected screen display errors & screen ! JDH *~
            *          !  scroll error. PRR 10110                 !     *~
            * 09/21/89 ! ASKUSER for MODE% only accepts 4 or 8 now! JDH *~
            * 10/02/89 ! Now won't display non-export orders for  ! JDH *~
            *          !  remove confirmation. Can reslct session.!     *~
            * 02/20/92 ! PRR 12058  Added call to SETPRNT.        ! JDH *~
            * 06/09/92 ! Added tests to ensure that lot tracked   ! JDH *~
            *          !  parts have lots assigned.               !     *~
            * 04/25/95 ! PRR - 13283 Additional Key to ARIBUFFR.  ! RJH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            bol$(100)3,                  /* BOL Array                  */~
            check$(100)1,                /* Check mark array           */~
            cursor%(2),                  /* Cursor location for edit   */~
            cuscode$(100)9,              /* Customer Code Array        */~
            custname$(100)30,            /* Customer Name Array        */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            export$1,                    /* Export flag                */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            invno$(100)8,                /* Invoice Number Array       */~
            invtypedescr$30,             /* Invoice type description   */~
            line1_header$60,             /* Screen header selection    */~
            line2$79,                    /* Screen Line #2             */~
            lot$6,                       /* Lot                        */~
            okfac$(13)1,                 /* Screen Facs                */~
            part$25,                     /* Part                       */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkey$32,                    /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            plowlin$99,                  /* Miscellaneous Read/Plow Key*/~
            postdate$8,                  /* Post Date (from Session)   */~
            readkey$99,                  /* Miscellaneous Read Key     */~
            session$6,                   /* Session Number             */~
            sono$(100)16,                /* S.O. Number array          */~
            sv_session$6,                /* Session Number             */~
            titles$79,                   /* Screen column titles       */~
            userid$3                     /* Current User Id            */~

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.02 11/13/95 Precious Metals                 "
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
            * # 1 ! ARIBUFFR ! Invoice Buffer- Headers                  *~
            * # 2 ! ARIINVRF ! A/R Invoice Report File                  *~
            * # 3 ! ARIBUF2  ! Invoice Buffer- Lines                    *~
            * # 4 ! SYSFILE2 ! Caelus Management System General Informa *~
            * # 5 ! HNYMASTR ! Inventory Master File                    *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 1, "ARIBUFFR",                                      ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  17,                     ~
                        alt key  1, keypos = 2001, keylen =  24 ,        ~
                            key  2, keypos =   34, keylen =   16, dup

            select # 2, "ARIINVRF",                                      ~
                        varc,     indexed,  recsize =   20,              ~
                        keypos =    1, keylen =  20                      ~

            select # 3, "ARIBUF2",                                       ~
                        varc,     indexed,  recsize =  750,              ~
                        keypos =   1,  keylen = 20

            select # 4, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select # 5, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (# 1, fs%( 1), f2%( 1), 0%, rslt$( 1))
            call "OPENCHCK" (# 2, fs%( 2), f2%( 2), 0%, rslt$( 2))
            call "OPENCHCK" (# 3, fs%( 3), f2%( 3), 0%, rslt$( 3))
            call "OPENCHCK" (# 4, fs%( 4), f2%( 4), 0%, rslt$( 4))
            call "OPENCHCK" (# 5, fs%( 5), f2%( 5), 0%, rslt$( 5))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62) = "SHPXCFRM: " & str(cms2v$,,8)

L09140
*        Now get which session to put the invoices in
            invtypedescr$ = "Export Shipment Confirmation"
            u3% = 1%
            call "UPDUSRLG" ("ARIUPDTE", "SHPXCFRM", invtypedescr$,      ~
                             "1", session$, u3%, postdate$, " ")
            if u3% <> 0% then exit_program

L09210:     mode% = 2%
            call "ASKUSER" (mode%, "*** ADD / DELETE OPTION ***",        ~
                 "Press PF4 to Confirm Shipments", "- OR -",             ~
                 "PF8 to remove prior Confirmations")
            if mode% <> 4 and mode% <> 8 then L09210

            titles$ = "*"
            str(titles$, 4) = "Cust #"
            str(titles$,15) = "Customer Name"
            str(titles$,47) = "Sales Order"
            str(titles$,65) = "BOL"
            str(titles$,70) = "Invoice"

            line1_header$ = "ADD Shipment Confirmations"
            if mode% = 8% then line1_header$ =                           ~
                               "REMOVE Prior Shipment Confirmations"

            str(line2$,35,20) = "Session #" & " " & session$

*        Is Lot Tracking Enforced?
            readkey$ = "SWITCHS.HNY         " : sys_lot$ = "N"
            call "READ100" (#4, readkey$, f1%(4))
            if f1%(4) = 0% then L10000
                get #4 using L09450, sys_lot$
L09450:             FMT POS(92), CH(1)
                if sys_lot$ <> "Y" then sys_lot$ = "N"

L10000: REM *************************************************************~
            *           T A B L E   L O A D   S E C T I O N             *~
            *-----------------------------------------------------------*~
            * Load array based on ADD / DELETE Select                   *~
            *************************************************************
            i% = 1%
            init (" ") check$(), cuscode$(), invno$(), sono$(), bol$(),  ~
                       custname$()
            init (hex(00)) plowkey$
            if mode% = 8% then plowkey$ = session$
        read_loop
            call "PLOWALTS" (#1, plowkey$, 1%, 6%, f1%(1))
                if f1%(1) = 0% then display_for_selection
            get #1 using L10114, export$
L10114:         FMT POS(891), CH(1)
            if export$ <> "X" then read_loop
            get #1 using L10140, cuscode$(i%), invno$(i%), sono$(i%),     ~
                                bol$(i%), custname$(i%)
L10140:         FMT CH(9), CH(8), POS(34), CH(16), CH(3), CH(30)
            readkey$ = "xxx" & str(cuscode$(i%)) & str(invno$(i%))
            call "READ100" (#2, readkey$, f1%(2))
                if f1%(2) = 0% then L10205
                cuscode$(i%), invno$(i%), sono$(i%), bol$(i%),           ~
                              custname$(i%) = " "
                goto read_loop
L10205:     gosub lot_checking
            i% = i% + 1%
            goto read_loop

        lot_checking
            if sys_lot$ = "N" or mode% = 8% then return
                plowlin$ = str(cuscode$(i%)) & str(invno$(i%)) &         ~
                           hex(000000)
L10280:         call "PLOWNEXT" (#3, plowlin$, 17%, f1%(3))
                     if f1%(3) = 0% then return
                get #3 using L10310, part$, ship_qty, lot$
L10310:              FMT POS(24), CH(25), POS(93), PD(14,4), POS(197),   ~
                         CH(6)
                if ship_qty = 0 or lot$ <> " " then return
                     call "LOTENABL" (part$, enabled%, 6%, #4, #5)
                     if enabled% <> 2% then return
                         check$(i%) = "l"
                         goto L10280

        REM *************************************************************~
            *     G O S U B   F O R   D I S P L A Y   S E C T I O N     *~
            *-----------------------------------------------------------*~
            * Logic for screen display and accept                       *~
            *************************************************************
        display_for_selection
            inpmessage$ = "Mark all Shipments to be confirmed, and"   &  ~
                          " Press RETURN to Process"
            if mode% = 8% then                                           ~
            inpmessage$ = "Mark Shipments from which to remove"   &      ~
                          " Confirmation, and press RETURN to Process"
            k% = 1%

             errormsg$ = " "
             if cuscode$(1) = " " and mode% = 4% then                    ~
                errormsg$ = "There are no EXPORT Invoices waiting "  &   ~
                            "for Shipment Confirmation"

             if cuscode$(1) = " " and mode% = 8% then                    ~
                errormsg$ = "There are no Confirmed EXPORT Shipments " & ~
                            "pending update"


L11230:     gosub L40000

              if keyhit% =  1 then gosub startover
              if keyhit% =  2 then k% = 1%
              if keyhit% =  3 then k% = max(1%, min(i%, i%-13%))
              if keyhit% =  4 then k% = max(1%, k%-13%)
              if keyhit% =  5 then k% = max(1%, min(i%, k%+13%, i%-13%))
              if keyhit% =  6 then k% = max(1%, k%-1%)
              if keyhit% =  7 then k% = max(1%, min(i%, k%+1%, i%-1%))
              if keyhit% =  9 then reselect_session
              if keyhit% = 16 then exit_program
              if keyhit% <> 0 then L11230

        REM *************************************************************~
            *        U P D A T E   S E C T I O N                        *~
            *-----------------------------------------------------------*~
            * Update as defined by user!                                *~
            *************************************************************
            if mode% = 8% then L12110
L12060:     ask% = 2%
            call "ASKUSER" (ask%, "*** INVOICE PRINT SELECT ***",        ~
                 "Press RETURN to print Invoices for all selections,",   ~
                 " - or -", "Press PF16 to skip printing")
            if ask% > 0% and ask% < 16% then L12060
L12110:     sv_session$ = all(hex(00))
            if mode% = 4% then sv_session$ = session$
            dup%, prt%, abend% = 0%

            if mode% = 4% and ask% = 0% then                             ~
                                  call "SETPRNT" ("ARI006", " ", 0%, 0%)

            for j% = 1% to i%
                if check$(j%) = " " or check$(j%) = "l" then L12240
                readkey$ = str(cuscode$(j%)) & str(invno$(j%))
                call "READ101" (#1, readkey$, f1%(1))
                put #1 using L12200, sv_session$
L12200:             FMT POS(2001), CH(6)
                rewrite #1
                if mode% = 4% and ask% = 0% then call "ARIXPRSB" (dup%,  ~
                      prt%, abend%, cuscode$(j%), invno$(j%), "ARI006")
L12240:     next j%

            if mode% = 4% and ask% = 0% then                             ~
                                  call "SETPRNT" ("ARI006", " ", 0%, 1%)
            goto L10000

        reselect_session
            u3% = 2%
            call "UPDUSRLG" ("ARIUPDTE", " ", " ", " ", session$, u3%,   ~
                             " ", " ")
            goto L09140

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the User the ability to START OVER when he wants.   *~
            *************************************************************
        startover
            u3% = 2%
L29914:     call "STARTOVR" (u3%)
            if u3%  = 1% then return
            if u3% <> 0% then L29914
                for l% = 1% to i%
                     if check$(l%) <> "l" then check$(l%) = " "
                next l%
                return clear all
                goto display_for_selection

L40000: REM *************************************************************~
            *        D I S P L A Y   F O R   S E L E C T I O N          *~
            *-----------------------------------------------------------*~
            * Display available for use selection                       *~
            *************************************************************
            gosub setpf
            init (hex(9c)) okfac$()
            for j% = 1% to 13%
                if cuscode$(k% + j% - 1%) = " " then L40100
                if check$(k% + j% - 1%) = "l" then L40095
                okfac$(j%) = hex(81) : goto L40100
L40095:         okfac$(j%) = hex(8c)
L40100:     next j%

L40120: accept                                                           ~
            at (01,02), fac(hex(8c)),  line1_header$            , ch(60),~
            at (01,66), "Today:",                                        ~
            at (01,73), fac(hex(8c)),  date$                    , ch(08),~
            at (02,02), fac(hex(ac)),  line2$                   , ch(79),~
            at (04,02), fac(hex(94)),  errormsg$                , ch(79),~
                                                                         ~
            at (06,02), fac(hex(ac)),  titles$                  , ch(79),~
                                                                         ~
            at (07,02), fac(okfac$(1%)), check$(k%)             , ch(01),~
            at (07,05), fac(hex(8c)),  cuscode$(k%)             , ch(09),~
            at (07,16), fac(hex(8c)),  custname$(k%)            , ch(30),~
            at (07,48), fac(hex(8c)),  sono$(k%)                , ch(16),~
            at (07,66), fac(hex(8c)),  bol$(k%)                 , ch(03),~
            at (07,71), fac(hex(8c)),  invno$(k%)               , ch(08),~
                                                                         ~
            at (08,02), fac(okfac$(2%)), check$(k%+1%)          , ch(01),~
            at (08,05), fac(hex(8c)),  cuscode$(k%+1%)          , ch(09),~
            at (08,16), fac(hex(8c)),  custname$(k%+1%)         , ch(30),~
            at (08,48), fac(hex(8c)),  sono$(k%+1%)             , ch(16),~
            at (08,66), fac(hex(8c)),  bol$(k%+1%)              , ch(03),~
            at (08,71), fac(hex(8c)),  invno$(k%+1%)            , ch(08),~
                                                                         ~
            at (09,02), fac(okfac$(3%)), check$(k%+2%)          , ch(01),~
            at (09,05), fac(hex(8c)),  cuscode$(k%+2%)          , ch(09),~
            at (09,16), fac(hex(8c)),  custname$(k%+2%)         , ch(30),~
            at (09,48), fac(hex(8c)),  sono$(k%+2%)             , ch(16),~
            at (09,66), fac(hex(8c)),  bol$(k%+2%)              , ch(03),~
            at (09,71), fac(hex(8c)),  invno$(k%+2%)            , ch(08),~
                                                                         ~
            at (10,02), fac(okfac$(4%)), check$(k%+3%)          , ch(01),~
            at (10,05), fac(hex(8c)),  cuscode$(k%+3%)          , ch(09),~
            at (10,16), fac(hex(8c)),  custname$(k%+3%)         , ch(30),~
            at (10,48), fac(hex(8c)),  sono$(k%+3%)             , ch(16),~
            at (10,66), fac(hex(8c)),  bol$(k%+3%)              , ch(03),~
            at (10,71), fac(hex(8c)),  invno$(k%+3%)            , ch(08),~
                                                                         ~
            at (11,02), fac(okfac$(5%)), check$(k%+4%)          , ch(01),~
            at (11,05), fac(hex(8c)),  cuscode$(k%+4%)          , ch(09),~
            at (11,16), fac(hex(8c)),  custname$(k%+4%)         , ch(30),~
            at (11,48), fac(hex(8c)),  sono$(k%+4%)             , ch(16),~
            at (11,66), fac(hex(8c)),  bol$(k%+4%)              , ch(03),~
            at (11,71), fac(hex(8c)),  invno$(k%+4%)            , ch(08),~
                                                                         ~
            at (12,02), fac(okfac$(6%)), check$(k%+5%)          , ch(01),~
            at (12,05), fac(hex(8c)),  cuscode$(k%+5%)          , ch(09),~
            at (12,16), fac(hex(8c)),  custname$(k%+5%)         , ch(30),~
            at (12,48), fac(hex(8c)),  sono$(k%+5%)             , ch(16),~
            at (12,66), fac(hex(8c)),  bol$(k%+5%)              , ch(03),~
            at (12,71), fac(hex(8c)),  invno$(k%+5%)            , ch(08),~
                                                                         ~
            at (13,02), fac(okfac$(7%)), check$(k%+6%)          , ch(01),~
            at (13,05), fac(hex(8c)),  cuscode$(k%+6%)          , ch(09),~
            at (13,16), fac(hex(8c)),  custname$(k%+6%)         , ch(30),~
            at (13,48), fac(hex(8c)),  sono$(k%+6%)             , ch(16),~
            at (13,66), fac(hex(8c)),  bol$(k%+6%)              , ch(03),~
            at (13,71), fac(hex(8c)),  invno$(k%+6%)            , ch(08),~
                                                                         ~
            at (14,02), fac(okfac$(8%)), check$(k%+7%)          , ch(01),~
            at (14,05), fac(hex(8c)),  cuscode$(k%+7%)          , ch(09),~
            at (14,16), fac(hex(8c)),  custname$(k%+7%)         , ch(30),~
            at (14,48), fac(hex(8c)),  sono$(k%+7%)             , ch(16),~
            at (14,66), fac(hex(8c)),  bol$(k%+7%)              , ch(03),~
            at (14,71), fac(hex(8c)),  invno$(k%+7%)            , ch(08),~
                                                                         ~
            at (15,02), fac(okfac$(9%)), check$(k%+8%)          , ch(01),~
            at (15,05), fac(hex(8c)),  cuscode$(k%+8%)          , ch(09),~
            at (15,16), fac(hex(8c)),  custname$(k%+8%)         , ch(30),~
            at (15,48), fac(hex(8c)),  sono$(k%+8%)             , ch(16),~
            at (15,66), fac(hex(8c)),  bol$(k%+8%)              , ch(03),~
            at (15,71), fac(hex(8c)),  invno$(k%+8%)            , ch(08),~
                                                                         ~
            at (16,02), fac(okfac$(10%)), check$(k%+9%)         , ch(01),~
            at (16,05), fac(hex(8c)),  cuscode$(k%+9%)          , ch(09),~
            at (16,16), fac(hex(8c)),  custname$(k%+9%)         , ch(30),~
            at (16,48), fac(hex(8c)),  sono$(k%+9%)             , ch(16),~
            at (16,66), fac(hex(8c)),  bol$(k%+9%)              , ch(03),~
            at (16,71), fac(hex(8c)),  invno$(k%+9%)            , ch(08),~
                                                                         ~
            at (17,02), fac(okfac$(11%)),  check$(k%+10%)       , ch(01),~
            at (17,05), fac(hex(8c)),  cuscode$(k%+10%)         , ch(09),~
            at (17,16), fac(hex(8c)),  custname$(k%+10%)        , ch(30),~
            at (17,48), fac(hex(8c)),  sono$(k%+10%)            , ch(16),~
            at (17,66), fac(hex(8c)),  bol$(k%+10%)             , ch(03),~
            at (17,71), fac(hex(8c)),  invno$(k%+10%)           , ch(08),~
                                                                         ~
            at (18,02), fac(okfac$(12%)),  check$(k%+11%)       , ch(01),~
            at (18,05), fac(hex(8c)),  cuscode$(k%+11%)         , ch(09),~
            at (18,16), fac(hex(8c)),  custname$(k%+11%)        , ch(30),~
            at (18,48), fac(hex(8c)),  sono$(k%+11%)            , ch(16),~
            at (18,66), fac(hex(8c)),  bol$(k%+11%)             , ch(03),~
            at (18,71), fac(hex(8c)),  invno$(k%+11%)           , ch(08),~
                                                                         ~
            at (19,02), fac(okfac$(13%)),  check$(k%+12%)       , ch(01),~
            at (19,05), fac(hex(8c)),  cuscode$(k%+12%)         , ch(09),~
            at (19,16), fac(hex(8c)),  custname$(k%+12%)        , ch(30),~
            at (19,48), fac(hex(8c)),  sono$(k%+12%)            , ch(16),~
            at (19,66), fac(hex(8c)),  bol$(k%+12%)             , ch(03),~
            at (19,71), fac(hex(8c)),  invno$(k%+12%)           , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(pfkey$),                                       ~
                     key (keyhit%)

               if keyhit% <> 13% then L41230
                     call "MANUAL" ("SHPXCFRM")
                     goto L40120

L41230:        if keyhit% <> 15% then L41270
                     call "PRNTSCRN"
                     goto L40120

L41270:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        setpf
           pf$(1) = "(1)Start Over  (2)First   (5)Next                 "&~
                    "             (13)Instructions"
           pf$(2) = "               (3)Last    (6)Down                 "&~
                    "             (15)Print Screen"
           pf$(3) = "               (4)Prev    (7)Up        (9)Reselect"&~
                    " Session     (16)Exit Program"
           pfkey$ = hex(01020304050607ff09ffffff0dff0f10ffffff00)
           if k% <> 1% then L41430
                str(pf$(1),16,8), str(pf$(3),16,8), str(pf$(2),27,7) = " "
                str(pfkey$, 2,1), str(pfkey$, 4,1), str(pfkey$, 6,1)     ~
                                                              = hex(ff)
L41430:    if k% + 13% < i% then L41470
                str(pf$(2),16,8), str(pf$(1),27,7), str(pf$(3),27,7) = " "
                str(pfkey$, 3,1), str(pfkey$, 5,1), str(pfkey$, 7,1)     ~
                                                              = hex(ff)
L41470:    return

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            call "SHOSTAT" ("One Moment Please")
            u3% = 2%
            call "UPDUSRLG" ("ARIUPDTE", " ", " ", " ", session$, u3%,   ~
                                                             " ", " ")
            end
