        REM CAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSINC~
            *                                                           *~
            *  H   H  N   N  Y   Y  PPPP   RRRR   DDDD    SSS   PPPP    *~
            *  H   H  NN  N  Y   Y  P   P  R   R  D   D  S      P   P   *~
            *  HHHHH  N N N   YYY   PPPP   RRRR   D   D   SSS   PPPP    *~
            *  H   H  N  NN    Y    P      R   R  D   D      S  P       *~
            *  H   H  N   N    Y    P      R   R  DDDD    SSS   P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYPRDSP - Subroutine to display part prices              *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and proprie- *~
            *  tary assets of CAELUS, Incorporated, Spokane, WA,        *~
            *  Embodying substantial creative efforts and confidential  *~
            *  information.  unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1988, an unpublished work by CAELUS,       *~
            *  Incorporated, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/27/88 ! Original                                 ! MJB *~
            * 10/27/88 ! Initialized Variables                    ! MJB *~
            * 04/10/89 ! CONV from HNYMASTR should be 7 decimals  ! GGO *~
            * 03/20/92 ! PRR 12037.  Fixed wandering ASKUSER.     ! JDH *~
            *          ! PRR 12010.  Honors future price effectvty!     *~
            * 11/12/92 ! PRR 12598 - Corrected Spelling Ln 2260.  ! RJH *~
            *          ! PRR 12599 - Added PF(11) toggle to view  !     *~
            *          !  Future Cost & effectivity date.         !     *~
            *          ! Misc Implied Integer corrections.        !     *~
            * 09/09/97 ! Changes for the year 2000.               ! DXL *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "HNYPRDSP" (part$, #1,   /* CPRPRICE File */                 ~
                               #2,   /* HNYMASTR File */                 ~
                               #3,   /* SYSFILE2 File */                 ~
                               #4)   /* GENCODES File */

        dim                                                              ~
            blankdate$8,                 /* Blank date for comparison  */~
            by$3,                        /* User who made last change  */~
            cby$3,                       /* User who made last change  */~
            fby$3,                       /* User who made last change  */~
            changed$8,                   /* Last Change Date           */~
            c_date$6,                    /* Last Change Date (Current) */~
            f_date$6,                    /* Last Change Date (Future)  */~
            date$8,                      /* Date for screen display    */~
            descr$32,                    /* Part Description           */~
            eff_date$8,                  /* Effectivity Date           */~
            hdr1$6, hdr2$6, hdr3$10,     /* Column Headings            */~
            inpmessage$79,               /* Informational Message      */~
            key$50,                      /* Record Key                 */~
            line2$79,                    /* Second screen line         */~
            markup(8), markup$(8)7,      /* Markup Percentages         */~
            cmarkup(8),                  /* Markup Percentages         */~
            fmarkup(8),                  /* Markup Percentages         */~
            margin(8), margin$(8)7,      /* Margin Percentages         */~
            cmargin(8),                  /* Margin Percentages         */~
            fmargin(8),                  /* Margin Percentages         */~
            future$1,                    /* Exists Future Prices Flag  */~
            part$25,                     /* Part Number                */~
            pc(16), pc$(16)10,           /* Price Code Unit Prices     */~
            cpc(16), cpc$(16)10,         /* Price Code Unit Prices     */~
            fpc(16), fpc$(16)10,         /* Price Code Unit Prices     */~
            price(8), price$(8)10,       /* Percent Price Displays     */~
            cprice(8),                   /* Percent Price Displays     */~
            fprice(8),                   /* Percent Price Displays     */~
            pf$(3)79,                    /* PF Key Prompts             */~
            pf11$18,                     /* PF Key Prompts             */~
            pfk$10,                      /* PF Keys Available          */~
            std$10,                      /* Current Std Cost           */~
            stkuom$4,                    /* Stocking UOM               */~
            uom$4, uomdescr$16,          /* Pricing Unit of Measure    */~
            userid$3                     /* Current User               */

        dim f2%(04),                     /* = 0 if the file is open    */~
            f1%(04)                      /* = 1 if READ was successful */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! CPRPRICE ! Customer Pricing File                    *~
            * #2  ! HNYMASTR ! Parts Master File                        *~
            * #3  ! SYSFILE2 ! CMS System Information File              *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date  :  call "DATEFMT" (date$)
            call "EXTRACT" addr("ID", userid$)
            str(line2$,62%) = "HNYPRDSP: " & cms2v$
            hdr1$ = "Markup"
            hdr2$ = "Margin"
            hdr3$ = "Unit Price"

        REM *************************************************************~
            *               M A I N   P R O G R A M                     *~
            * --------------------------------------------------------- *~
            * Main Load and Display Section                             *~
            *************************************************************

            init(" ") descr$, uom$, uomdescr$, changed$, std$, stkuom$,  ~
                      pc$(),  markup$(),  margin$(),  price$(),  by$,    ~
                      cpc$(), cby$, fpc$(), fby$,  str(line2$,37%, 8%),  ~
                      inpmessage$, c_date$, f_date$, eff_date$


            future% = 0%
            future$ = "N"

            gosub load_data
L01280:     gosub'111
                if keyhit% <> 11 then       L01330
                     if future% = 0%                                     ~
                          then gosub future_prices                       ~
                          else gosub current_prices
                     gosub format_numbers
L01330:         if keyhit% <> 16% then L01280
                end

        current_prices
            future%    = 0%
            changed$   = c_date$
            by$        = cby$
            mat pc     = cpc
            mat markup = cmarkup
            mat margin = cmargin
            mat price  = cprice
            pf11$      = "(11)Future Prices"
            call "DATEFMT" (changed$)
            str(line2$,,61%) = "Mode: Current Price Set structure"
            return

        future_prices
            if future$ = "N" then return
            future%    = 1%
            changed$   = f_date$
            by$        = fby$
            mat pc     = fpc
            mat markup = fmarkup
            mat margin = fmargin
            mat price  = fprice
            pf11$      = "(11)Current Prices"
            call "DATEFMT" (changed$)
            str(line2$,,61%) = "Mode: Future Price Set effective on " &  ~
                eff_date$
            return

        load_data
*        First load in part data.  Record already in buffer.
            get #2 using L02210 , stkuom$, uom$, conv
L02210:         FMT XX(73), 2*CH(4), PD(14,7)
            call "STCCOSTS" (part$, " ", #3, 1%, std)
            std = std * conv
            call "CONVERT" (std, -4.4, std$)

            uomdescr$ = "** Not on File **"
            call "READ100" (#4, "UOM      " & uom$, f1%(4%))
            if f1%(4%) = 0% then L02320
                get #4 using L02300 , uomdescr$
L02300:              FMT XX(24), CH(30)

L02320
*        Now load in the Pricing information
            mat cpc = con  :  mat cpc = (-1) * cpc
            mat fpc = con  :  mat fpc = (-1) * fpc
            for i% = 1% to 8%
                cmarkup(i%),cmargin(i%),fmarkup(i%),fmargin(i%) = -999.99
            next i%
            key$ = "C" & part$
            call "CPRUPDSB" (#1, 0%, "00", key$, 0%, f1%(1%))
            if f1%(1%) = 1% then L02400
L02391:         u3% = 0%
                call "ASKUSER" (u3%, "***** NO PRICES ON FILE ****",     ~
                     "There is NO pricing information for part number",  ~
                      part$, "Press RETURN to Continue")
                if u3% <> 0% then L02391
                end

L02400:     get #1 using L02404 , cby$, c_date$, cpc(), cmarkup(),         ~
                     eff_date$, fby$, f_date$, fpc(), fmarkup()
L02404:          FMT XX(47), CH(3), CH(6), 16*PD(14,4), 8*PD(14,4),      ~
                     POS(494), CH(6), CH(3), CH(6), 16*PD(14,4),         ~
                     8*PD(14,4)
            gosub calc_per_markup
            gosub current_prices
            gosub format_numbers
            if eff_date$ <> " " and eff_date$ <> blankdate$ ~
                  then future$ = "Y" else future$ = "N"
            call "DATEFMT" (eff_date$)
            return

        calc_per_markup
            for i% = 1% to 8%
                if cmarkup(i%) <>  -999.99 then L02550
                     cmargin(i%) = -999.99
                     goto L02600
L02550:         cprice (i%) = std + (std * cmarkup(i%) * .01)
                cmargin(i%) = 0
                if cprice(i%) = 0 then L02580
                     cmargin(i%) = round(100 * (1 - (std/cprice(i%))), 2)
L02580:         cprice (i%) = round(cprice(i%), 4)

L02600:         if fmarkup(i%) <>  -999.99 then L02630
                     fmargin(i%) = -999.99
                     goto L02670
L02630:         fprice (i%) = std + (std * fmarkup(i%) * .01)
                fmargin(i%) = 0
                if fprice(i%) = 0 then L02660
                     fmargin(i%) = round(100 * (1 - (std/fprice(i%))), 2)
L02660:         fprice (i%) = round(fprice(i%), 4)
L02670:     next i%
*          GOSUB FORMAT_NUMBERS
            return

        format_numbers
            init (" ") margin$(), markup$(), price$(), pc$()
            for i% = 1% to 8%
                if margin(i%) = -999.99 then L02850
                     call "CONVERT" (margin(i%), 1.2, margin$(i%))
                     call "CONVERT" (markup(i%), 1.2, markup$(i%))
                     call "CONVERT" (price (i%), 0.4, price$ (i%))
L02850:     next i%
            for i% = 1% to 16%
                if pc(i%) >= 0 then  call "CONVERT" (pc(i%), 4.4, pc$(i%))
            next i%
            return

        REM *************************************************************~
            *             S C R E E N   H A N D L I N G                 *~
            * --------------------------------------------------------- *~
            * Handles the only, the only, screen.                       *~
            *************************************************************

        deffn'111
            gosub pfkeys_part

L03380:   accept                                                         ~
            at (01,02), "Product Selling Prices",                        ~
            at (01,67), "Date:",                                         ~
            at (01,73), fac(hex(8c)), date$                     , ch(08),~
            at (02,02), fac(hex(ac)), line2$                    , ch(79),~
                                                                         ~
            at (05,02), "Part Code: ",                                   ~
            at (05,13), fac(hex(84)), part$                     , ch(25),~
            at (05,47), "UOMs: Pricing:",                                ~
            at (05,62), fac(hex(84)), uom$                      , ch(04),~
            at (05,67), "Stocking: ",                                    ~
            at (05,77), fac(hex(84)), stkuom$                   , ch(04),~
            at (06,13), fac(hex(8c)), descr$                    , ch(32),~
            at (06,47), "Last Change: ",                                 ~
            at (06,60), fac(hex(8c)), changed$                  , ch(08),~
            at (06,70), "by ",                                           ~
            at (06,73), fac(hex(8c)), by$                       , ch(03),~
                                                                         ~
            at (08,02), "** Unit Price Assignments **",                  ~
            at (09,06),"A", at(09,24),"B", at(09,42),"C", at(09,60), "D",~
            at (10,06),"E", at(10,24),"F", at(10,42),"G", at(10,60), "H",~
            at (11,06),"I", at(11,24),"J", at(11,42),"K", at(11,60), "L",~
            at (12,06),"M", at(12,24),"N", at(12,42),"O", at(12,60), "P",~
            at (09,08), fac(hex(84)),     pc$( 1%)              , ch(10),~
            at (09,26), fac(hex(84)),     pc$( 2%)              , ch(10),~
            at (09,44), fac(hex(84)),     pc$( 3%)              , ch(10),~
            at (09,63), fac(hex(84)),     pc$( 4%)              , ch(10),~
            at (10,08), fac(hex(84)),     pc$( 5%)              , ch(10),~
            at (10,26), fac(hex(84)),     pc$( 6%)              , ch(10),~
            at (10,44), fac(hex(84)),     pc$( 7%)              , ch(10),~
            at (10,63), fac(hex(84)),     pc$( 8%)              , ch(10),~
            at (11,08), fac(hex(84)),     pc$( 9%)              , ch(10),~
            at (11,26), fac(hex(84)),     pc$(10%)              , ch(10),~
            at (11,44), fac(hex(84)),     pc$(11%)              , ch(10),~
            at (11,63), fac(hex(84)),     pc$(12%)              , ch(10),~
            at (12,08), fac(hex(84)),     pc$(13%)              , ch(10),~
            at (12,26), fac(hex(84)),     pc$(14%)              , ch(10),~
            at (12,44), fac(hex(84)),     pc$(15%)              , ch(10),~
            at (12,63), fac(hex(84)),     pc$(16%)              , ch(10),~
                                                                         ~
            at (14,02), "** Percentage of Standard Cost Assignments **", ~
            at (14,50), "Current Standard =",                            ~
            at (14,69), fac(hex(8c)), std$                      , ch(10),~
            at (15,11), fac(hex(ac)), hdr1$,                             ~
            at (15,20), fac(hex(ac)), hdr2$,                             ~
            at (15,29), fac(hex(ac)), hdr3$,                             ~
            at (15,49), fac(hex(ac)), hdr1$,                             ~
            at (15,58), fac(hex(ac)), hdr2$,                             ~
            at (15,67), fac(hex(ac)), hdr3$,                             ~
            at (16, 6), "1", at (16, 43), "5",                           ~
            at (17, 6), "2", at (17, 43), "6",                           ~
            at (18, 6), "3", at (18, 43), "7",                           ~
            at (19, 6), "4", at (19, 43), "8",                           ~
            at (16,10), fac(hex(84)),     markup$( 1%)          , ch(07),~
            at (17,10), fac(hex(84)),     markup$( 2%)          , ch(07),~
            at (18,10), fac(hex(84)),     markup$( 3%)          , ch(07),~
            at (19,10), fac(hex(84)),     markup$( 4%)          , ch(07),~
            at (16,48), fac(hex(84)),     markup$( 5%)          , ch(07),~
            at (17,48), fac(hex(84)),     markup$( 6%)          , ch(07),~
            at (18,48), fac(hex(84)),     markup$( 7%)          , ch(07),~
            at (19,48), fac(hex(84)),     markup$( 8%)          , ch(07),~
                                                                         ~
            at (16,19), fac(hex(84)),     margin$( 1%)          , ch(07),~
            at (17,19), fac(hex(84)),     margin$( 2%)          , ch(07),~
            at (18,19), fac(hex(84)),     margin$( 3%)          , ch(07),~
            at (19,19), fac(hex(84)),     margin$( 4%)          , ch(07),~
            at (16,57), fac(hex(84)),     margin$( 5%)          , ch(07),~
            at (17,57), fac(hex(84)),     margin$( 6%)          , ch(07),~
            at (18,57), fac(hex(84)),     margin$( 7%)          , ch(07),~
            at (19,57), fac(hex(84)),     margin$( 8%)          , ch(07),~
                                                                         ~
            at (16,29), fac(hex(84))  ,   price$ ( 1%)          , ch(10),~
            at (17,29), fac(hex(84))  ,   price$ ( 2%)          , ch(10),~
            at (18,29), fac(hex(84))  ,   price$ ( 3%)          , ch(10),~
            at (19,29), fac(hex(84))  ,   price$ ( 4%)          , ch(10),~
            at (16,67), fac(hex(84))  ,   price$ ( 5%)          , ch(10),~
            at (17,67), fac(hex(84))  ,   price$ ( 6%)          , ch(10),~
            at (18,67), fac(hex(84))  ,   price$ ( 7%)          , ch(10),~
            at (19,67), fac(hex(84))  ,   price$ ( 8%)          , ch(10),~
                                                                         ~
            at (21,02), fac(hex(a4)),   inpmessage$             , ch(79),~
            at (22,02), fac(hex(8c)), pf$(1%)                    ,ch(79),~
            at (23,02), fac(hex(8c)), pf$(2%)                    ,ch(79),~
            at (24,02), fac(hex(8c)), pf$(3%)                    ,ch(79),~
                                                                         ~
                keys(pfk$),                                              ~
                key (keyhit%)

            if keyhit% <> 13% then L04310
                call "MANUAL" ("HNYPRDSP")
                goto L03380

L04310:     if keyhit% <> 15% then return
                call "PRNTSCRN"
                goto L03380

        pfkeys_part      /* Keys for Display Screen */

            pf$(1%) = "                                        " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                       (16)Exit Display"

            pfk$ = hex(ffffffff0d0f10ffffffff)
            if future$ = "N" then return
                str(pf$(1%), 41%, 17%) = pf11$
                pfk$ = hex(ffff0bff0d0f10ffffffff)

            return

