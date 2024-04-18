        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  BBBB    CCC   K   K  PPPP   RRRR    CCC    SSS   BBBB    *~
            *  B   B  C   C  K  K   P   P  R   R  C   C  S      B   B   *~
            *  BBBB   C      KKK    PPPP   RRRR   C       SSS   BBBB    *~
            *  B   B  C   C  K  K   P      R   R  C   C      S  B   B   *~
            *  BBBB    CCC   K   K  P      R   R   CCC    SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BCKPRCSB - This subroutine displays the costs and prices  *~
            *            for a part number passed to it. Substantial    *~
            *            logic and actual code were taken from CPRASSGN.*~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1988  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/22/89 ! Original (made currency-specific)        ! JIM *~
            * 10/06/89 ! Turned on Future Prices (CPRUPDSB)       ! JDH *~
            * 03/01/90 ! Changd PIC for exchange rate reverse date! JDH *~
            * 09/26/90 ! Allow display of negative discounts.     ! JDH *~
            * 04/23/96 ! PRR 12813, 13580. Fmt with variable decs.! JDH *~
            * 09/04/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        sub "BCKPRCSB" (customer$,       /* Customer code              */~
                       custtype$,        /* Customer type code         */~
                       partnbr$,         /* Part number                */~
                       category$,        /* Part category code         */~
                       pricecode$,       /* Customer's std price code  */~
                       currency$,        /* Currency Code              */~
                       currtype$,        /* Currency Table Type        */~
                       quantity,         /* Order Quantity             */~
                       #01,              /* SYSFILE2 UFB               */~
                       #02,              /* CUSTOMER UFB               */~
                       #03,              /* HNYMASTR UFB               */~
                       #10)              /* CURMASTR UFB               */

        dim                                                              ~
            alpha$(16)1,                 /* Alphabet thru 'P'          */~
            category$4,                  /* Part category code         */~
            codekey$51,                  /* 'C' code key               */~
            column2$79,                  /* Column header              */~
            curr$1,                      /* Currency in effect ?       */~
            currency$4, currdesc$32,     /* Currency Code & Descr      */~
            currtype$1,                  /* Currency Table Type        */~
            customer$9, custname$32,     /* Customer code & name       */~
            custtype$2,                  /* Customer type code         */~
            date1$8, date2$8,            /* Date for screen display    */~
            discs(10), discs$(10)6,      /* Special price discounts    */~
            errormsg$79,                                                 ~
            from$6, to$6, todate$8,      /* Specials date range        */~
            inpmessage$79,               /* Input message              */~
            line2$79,                    /* Screen Line #2             */~
            markup(8), mrkup$(8)6,       /* Regular markup percents    */~
            mfac$(8)1,                   /* Markup FACs                */~
            musell(8), msell$(8)10,      /* Sell prices from MUs       */~
            numer$(8)1,                  /* Numbers 1 thru 8           */~
            ovrpr$(10)1,                 /* Over-ride Price Codes      */~
            partnbr$25, partdesc$34,     /* Part number & description  */~
            plowkey$50,                  /* Miscellaneous Read/Plow Key*/~
            pricecode$1,                 /* Customer's std price code  */~
            readkey$50,                  /* Misc CPRPRICE file key     */~
            regular(16), reglr$(16)10,   /* Unit Prices (Regular)      */~
            rev_date$8,                  /* Reverse date for CURCONVR  */~
            rfac$(16)1,                  /* Regular price FACs         */~
            sfac$1, sfac$(10)1,          /* Special price FACs         */~
            special(10), specl$(10)10,   /* Unit Prices (Codes/Spcls)  */~
            speckey$51,                  /* 'S' code key               */~
            statutory$4,                 /* Statutory currency code    */~
            stdcost$10,                  /* Edited standard cost       */~
            thrudate$13,                 /* Specials thru date         */~
            to%(10), toqty$(10)8         /* To qty ranges              */

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
            * #01 ! SYSFILE2 ! CMS System Info   (Opened by Caller)     *~
            * #02 ! CUSTOMER ! Customer Master   (Opened by Caller)     *~
            * #03 ! HNYMASTR ! Parts Master File (Opened by Caller)     *~
            * #04 ! CPRPRICE ! Customer Pricing File                    *~
            * #05 ! CPRCURCY ! Currency-specific Customer Pricing File  *~
            * #06 ! CURCONVR ! Multi-Currency Conversion Tables         *~
            * #10 ! CURMASTR ! Multi-Currency Master (Opened by Caller) *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #04, "CPRPRICE",                                      ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos =    1, keylen =  47                      ~

            select #05,  "CPRCURCY",                                     ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos =  1, keylen = 51

            select #06, "CURCONVR",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  11

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

        REM One-time-only initializations
            if beenherebefore% <> 0% then goto L09240
                beenherebefore% = 1%
                call "OPENCHCK" (#04, fs%(04), f2%(04), 0%, rslt$(04))
                date1$, date2$ = date : call "DATEFMT" (date1$)
                str(line2$,62%) = "BCKPRCSB: " & str(cms2v$,,8%)
                str(alpha$()) = "ABCDEFGHIJKLMNOP"
                str(numer$()) = "12345678"
                column2$ = "  To Qty  Disc% P/C   U/Price  P/C   U/Pric"&~
                    "e P/C   U/Price P/C Mkup%   MU/Price"
                curr$ = "N" : statutory$ = " "
                call "READ100" (#01, "SWITCHS.CUR", f1%(1))
                if f1%(1) <> 0% then get #01 using L09190, curr$, statutory$
L09190:             FMT POS(21), CH(1), CH(4)
                if curr$ <> "Y" then goto L09240
                    call "OPENCHCK" (#05, 0%, f2%(5), 0%, " ")
                    call "OPENCHCK" (#06, 0%, f2%(6), 0%, " ")

L09240: REM Initialization continues
            init (" ") ovrpr$(), speckey$, discs$(), mrkup$(), reglr$(), ~
                specl$(), toqty$(), stdcost$, msell$(), currdesc$,       ~
                codekey$
            thrudate$ = "none"
            init (hex(8c)) sfac$, sfac$(), rfac$(), mfac$()
            mat to% = zer : mat special = zer : mat discs = zer
            mat regular = zer : mat markup = zer : mat musell = zer
            factor = 1          /* Default Currency conversion factor */

        REM Get total Standard Cost
            call "STCCOSTS" (partnbr$, " ", #01, 1%, stdcost)

        REM Get a little Customer Info
            call "DESCRIBE" (#02, customer$, custname$, 1%, f1%(2%))
                if f1%(2%) = 0% then custname$ = "(Customer not found)"
*        Sorry to have to do this here, but I noticed that the type can
*        sometimes be in error or blank & I couldn't stand to go back
*        into every driver to ensure the validity of this variable.
                if f1%(2%) = 1% then get #02 using L09430 , custtype$
L09430:                                  FMT POS(1023), CH(2)

        REM *************************************************************~
            *         M A I N   S U B R O U T I N E   L O G I C         *~
            * ... First, see if a currency-specific price can be found. *~
            *************************************************************

            if curr$ <> "Y" then goto L10710    /* No Multi-Currency */
            if currency$ = " " then goto L10710 /* No Currency code  */
            if currency$ = statutory$ then goto L10710 /* No Statutory */

*        SPECIALS- Find any Currency-specific OverRide PC or Price &/or
*                  discount
*             Search (1) Customer Number / Currency / Part Number
*                    (2) Customer Number / Currency / Part Category
*                    (3) Currency / Customer Type / Part Number
*                    (4) Currency / Customer Type / Part Category

*        SEARCH #1-  Customer Number / Currency / Part Number
            plowkey$ = all(hex(00))
            str(plowkey$, 2, 9) = customer$
            str(plowkey$,17,25) = partnbr$
            gosub currency_common_search_specials
            if speckey$ <> " " then goto currency_regular

*        SEARCH #2-  Customer Number / Currency / Part Category
            plowkey$ = all(hex(00))
            str(plowkey$, 2, 9) = customer$
            str(plowkey$,42, 4) = category$
            gosub currency_common_search_specials
            if speckey$ <> " " then goto currency_regular

*        SEARCH #3-  Currency / Customer Type / Part Number
            plowkey$ = all(hex(00))
            str(plowkey$,15, 2) = custtype$
            str(plowkey$,17,25) = partnbr$
            gosub currency_common_search_specials
            if speckey$ <> " " then goto currency_regular

*        SEARCH #4-  Currency / Customer Type / Part Category
            plowkey$ = all(hex(00))
            str(plowkey$,15, 2) = custtype$
            str(plowkey$,42, 4) = category$
            gosub currency_common_search_specials

        currency_regular   /* Regular Pricing, wrapup */
            readkey$ = "C" & str(currency$) & partnbr$
            call "READ100" (#05, readkey$, f1%(5))
            call "CPRUPDSB" (#05, 1%, "00", readkey$, 0%, f1%(5))
            if f1%(5) <> 0% then goto L10500
                if speckey$ = " " then goto L10570 /* non-currency */
                goto L10530
L10500:     codekey$ = readkey$
            get #05 using L10520, regular(), markup()
L10520:         FMT POS(61), 16*PD(14,4), 8*PD(14,4)
L10530:     call "DESCRIBE" (#10, currency$, currdesc$, 1%, f1%(10))
            str(inpmessage$,28) = "Currency: " & currency$ & " " &       ~
                currdesc$

L10570
*        Get the current Currency conversion factor
            call "DATREVRS" ( date1$, rev_date$, errormsg$ )
            if errormsg$ <> " " then goto L10660 /* Invalid date */
            readkey$ = str(currtype$) & str(currency$) & str(rev_date$,,6)
            call "PLOWNEXT" (#06, readkey$, 5%, f1%(6))
            if f1%(6) <> 0% then get #06 using L10650, factor
L10650:         FMT POS(26), PD(14,7)
L10660:     if speckey$ = " " and codekey$ = " "                         ~
                then str(inpmessage$,28) = "Prices converted to " &      ~
                     currency$ & " from Statutory Currency"              ~
                else goto L11190

L10710: REM *************************************************************~
            * ... Currency-specific prices have been searched for; fill *~
            * in the gaps with non-currency prices as necessary.        *~
            *************************************************************

*        SPECIALS  Find any Over-Ride PC or Price &/or Discount
*        SEARCH #1-  Customer Number / Part Number
            plowkey$ = all(hex(00))
            str(plowkey$, 2, 9) = customer$
            str(plowkey$,13,25) = partnbr$
            gosub non_curr_common_search_specials
            if speckey$ <> " " then goto non_curr_regular

*        SEARCH #2-  Customer Number / Part Category
            plowkey$ = all(hex(00))
            str(plowkey$, 2, 9) = customer$
            str(plowkey$,38, 4) = category$
            gosub non_curr_common_search_specials
            if speckey$ <> " " then goto non_curr_regular

*        SEARCH #3-  Customer Type   / Part Number
            plowkey$ = all(hex(00))
            str(plowkey$,11, 2) = custtype$
            str(plowkey$,13,25) = partnbr$
            gosub non_curr_common_search_specials
            if speckey$ <> " " then goto non_curr_regular

*        SEARCH #4-  Customer Type   / Part Category
            plowkey$ = all(hex(00))
            str(plowkey$,11, 2) = custtype$
            str(plowkey$,38, 4) = category$
            gosub non_curr_common_search_specials

        non_curr_regular
*        Find regular pricing, including markup percents */
            if codekey$ <> " " then goto L11190
            readkey$ = "C" & partnbr$
            call "READ100" (#04, readkey$, f1%(4))
            call "CPRUPDSB" (#04, 0%, "00", readkey$, 0%, f1%(4))
            if f1%(4) = 0% then goto L11190
            get #04 using L11120, regular(), markup()
L11120:         FMT POS(57), 16*PD(14,4), 8*PD(14,4)
            for n% = 1% to 16%
                if regular(n%) > 0 then regular(n%) = round(regular(n%)  ~
                     * factor, 4)
            next n%
            codekey$ = readkey$

L11190: REM *************************************************************~
            * Format all the data gathered from the pricing files.      *~
            *************************************************************

            call "DESCRIBE" (#03, partnbr$, partdesc$, 1%, f1%(3))
                if f1%(3) = 0% then partdesc$ = "(Part not found)"
            if codekey$ = " " then goto L11390 /*No Regular prices found*/
                for n% = 1% to 16% /* ... else edit Regular prices */
                     if regular(n%) > 0 then                             ~
                          call "CONVERT" (regular(n%), 2.4, reglr$(n%))
                next n%
                for n% = 1% to 8% /* ... and Markup percents */
                     if markup(n%) <= 0 then goto L11380
                          convert markup(n%) to mrkup$(n%), pic (####.#)
                          musell(n%) = stdcost * (1 + (markup(n%) / 100))
                          musell(n%) = round(musell(n%) * factor, 4)
                          call "CONVERT" (musell(n%), 2.4, msell$(n%))
L11380:         next n%
L11390:     if speckey$ = " " then goto L11480 /*No Special prices found*/
                for n% = 1% to 10% /* else edit Special prices */
                     if to%(n%) = 0 then goto L11470
                          convert to%(n%) to toqty$(n%), pic (########)
                          if special(n%) > 0 then                        ~
                             call "CONVERT" (special(n%), 2.4, specl$(n%))
                          if discs(n%) <> 0 then                         ~
                             call "CONVERT" (discs(n%), 0.1, discs$(n%))
L11470:         next n%
L11480:     convert round(stdcost * factor,4) to stdcost$, pic(#####.####)
            str(inpmessage$,,25) = "Standard cost: " & stdcost$
            if curr$ <> "Y" then goto L11540
            if currency$ <> " " and currency$ <> statutory$ then L11540
                str(inpmessage$,28) = "Currency is Statutory"

L11540: REM Set the FACs for display (hilite indicated price info).
            if speckey$ = " " then goto L11660
                sfac$ = hex(84)
                todate$ = to$
                call "DATEFMT" (todate$)
                thrudate$ = "thru " & todate$
                if quantity <= 0 then goto L11660
                     for s% = 1% to 10%
                          if quantity > to%(s%) then goto L11650
                               sfac$(s%) = hex(84)
                               goto L11660
L11650:              next s%
L11660:     if pricecode$ < "A" or pricecode$ > "P" then goto L11690
                rfac$(val(str(pricecode$,,1),1)-64%) = hex(84)
                goto L11720
L11690:     if pricecode$ < "1" or pricecode$ > "8" then goto L11720
                mfac$(val(str(pricecode$,,1),1)-48%) = hex(84)

L11720: REM *************************************************************~
            * And here's the display of the gathered data on the screen.*~
            *************************************************************

L11760:     accept                                                       ~
                at (01,02), "Part Costs and Prices",                     ~
                at (01,66), "Today:",                                    ~
                at (01,73), fac(hex(8c)), date1$                , ch(08),~
                at (02,02), fac(hex(ac)), line2$                , ch(79),~
                                                                         ~
                at (04,02), "Part:",                                     ~
                at (04,08), fac(hex(84)), partnbr$              , ch(25),~
                at (04,34), fac(hex(84)), partdesc$             , ch(34),~
                at (04,69), "Ctgy:",                                     ~
                at (04,75), fac(hex(84)), category$             , ch(04),~
                                                                         ~
                at (05,02), "Customer:",                                 ~
                at (05,12), fac(hex(84)), customer$             , ch(09),~
                at (05,22), fac(hex(84)), custname$             , ch(32),~
                at (05,56), "Type:",                                     ~
                at (05,62), fac(hex(84)), custtype$             , ch(02),~
                at (05,65), "Price code:",                               ~
                at (05,77), fac(hex(84)), pricecode$            , ch(01),~
                                                                         ~
                at (07,02), "Special Prices:",                           ~
                at (07,18), fac(sfac$),     thrudate$           , ch(13),~
                at (07,33), "*-------- R e g u l a r   P r i c e s ------~
        ~---*",                                                           ~
                at (08,02), fac(hex(ac)), column2$              , ch(79),~
                                                                         ~
                at (10,02), fac(sfac$( 1)), toqty$( 1)          , ch(08),~
                at (10,11), fac(sfac$( 1)), discs$( 1)          , ch(06),~
                at (10,19), fac(sfac$( 1)), ovrpr$( 1)          , ch(01),~
                at (10,21), fac(sfac$( 1)), specl$( 1)          , ch(10),~
                at (10,34), fac(rfac$( 1)), alpha$( 1)          , ch(01),~
                at (10,36), fac(rfac$( 1)), reglr$( 1)          , ch(10),~
                at (10,48), fac(rfac$( 9)), alpha$( 9)          , ch(01),~
                at (10,50), fac(rfac$( 9)), reglr$( 9)          , ch(10),~
                at (10,62), fac(mfac$( 1)), numer$( 1)          , ch(01),~
                at (10,64), fac(mfac$( 1)), mrkup$( 1)          , ch(06),~
                at (10,71), fac(mfac$( 1)), msell$( 1)          , ch(10),~
                                                                         ~
                at (11,02), fac(sfac$( 2)), toqty$( 2)          , ch(08),~
                at (11,11), fac(sfac$( 2)), discs$( 2)          , ch(06),~
                at (11,19), fac(sfac$( 2)), ovrpr$( 2)          , ch(01),~
                at (11,21), fac(sfac$( 2)), specl$( 2)          , ch(10),~
                at (11,34), fac(rfac$( 2)), alpha$( 2)          , ch(01),~
                at (11,36), fac(rfac$( 2)), reglr$( 2)          , ch(10),~
                at (11,48), fac(rfac$(10)), alpha$(10)          , ch(01),~
                at (11,50), fac(rfac$(10)), reglr$(10)          , ch(10),~
                at (11,62), fac(mfac$( 2)), numer$( 2)          , ch(01),~
                at (11,64), fac(mfac$( 2)), mrkup$( 2)          , ch(06),~
                at (11,71), fac(mfac$( 2)), msell$( 2)          , ch(10),~
                                                                         ~
                at (12,02), fac(sfac$( 3)), toqty$( 3)          , ch(08),~
                at (12,11), fac(sfac$( 3)), discs$( 3)          , ch(06),~
                at (12,19), fac(sfac$( 3)), ovrpr$( 3)          , ch(01),~
                at (12,21), fac(sfac$( 3)), specl$( 3)          , ch(10),~
                at (12,34), fac(rfac$( 3)), alpha$( 3)          , ch(01),~
                at (12,36), fac(rfac$( 3)), reglr$( 3)          , ch(10),~
                at (12,48), fac(rfac$(11)), alpha$(11)          , ch(01),~
                at (12,50), fac(rfac$(11)), reglr$(11)          , ch(10),~
                at (12,62), fac(mfac$( 3)), numer$( 3)          , ch(01),~
                at (12,64), fac(mfac$( 3)), mrkup$( 3)          , ch(06),~
                at (12,71), fac(mfac$( 3)), msell$( 3)          , ch(10),~
                                                                         ~
                at (13,02), fac(sfac$( 4)), toqty$( 4)          , ch(08),~
                at (13,11), fac(sfac$( 4)), discs$( 4)          , ch(06),~
                at (13,19), fac(sfac$( 4)), ovrpr$( 4)          , ch(01),~
                at (13,21), fac(sfac$( 4)), specl$( 4)          , ch(10),~
                at (13,34), fac(rfac$( 4)), alpha$( 4)          , ch(01),~
                at (13,36), fac(rfac$( 4)), reglr$( 4)          , ch(10),~
                at (13,48), fac(rfac$(12)), alpha$(12)          , ch(01),~
                at (13,50), fac(rfac$(12)), reglr$(12)          , ch(10),~
                at (13,62), fac(mfac$( 4)), numer$( 4)          , ch(01),~
                at (13,64), fac(mfac$( 4)), mrkup$( 4)          , ch(06),~
                at (13,71), fac(mfac$( 4)), msell$( 4)          , ch(10),~
                                                                         ~
                at (14,02), fac(sfac$( 5)), toqty$( 5)          , ch(08),~
                at (14,11), fac(sfac$( 5)), discs$( 5)          , ch(06),~
                at (14,19), fac(sfac$( 5)), ovrpr$( 5)          , ch(01),~
                at (14,21), fac(sfac$( 5)), specl$( 5)          , ch(10),~
                at (14,34), fac(rfac$( 5)), alpha$( 5)          , ch(01),~
                at (14,36), fac(rfac$( 5)), reglr$( 5)          , ch(10),~
                at (14,48), fac(rfac$(13)), alpha$(13)          , ch(01),~
                at (14,50), fac(rfac$(13)), reglr$(13)          , ch(10),~
                at (14,62), fac(mfac$( 5)), numer$( 5)          , ch(01),~
                at (14,64), fac(mfac$( 5)), mrkup$( 5)          , ch(06),~
                at (14,71), fac(mfac$( 5)), msell$( 5)          , ch(10),~
                                                                         ~
                at (15,02), fac(sfac$( 6)), toqty$( 6)          , ch(08),~
                at (15,11), fac(sfac$( 6)), discs$( 6)          , ch(06),~
                at (15,19), fac(sfac$( 6)), ovrpr$( 6)          , ch(01),~
                at (15,21), fac(sfac$( 6)), specl$( 6)          , ch(10),~
                at (15,34), fac(rfac$( 6)), alpha$( 6)          , ch(01),~
                at (15,36), fac(rfac$( 6)), reglr$( 6)          , ch(10),~
                at (15,48), fac(rfac$(14)), alpha$(14)          , ch(01),~
                at (15,50), fac(rfac$(14)), reglr$(14)          , ch(10),~
                at (15,62), fac(mfac$( 6)), numer$( 6)          , ch(01),~
                at (15,64), fac(mfac$( 6)), mrkup$( 6)          , ch(06),~
                at (15,71), fac(mfac$( 6)), msell$( 6)          , ch(10),~
                                                                         ~
                at (16,02), fac(sfac$( 7)), toqty$( 7)          , ch(08),~
                at (16,11), fac(sfac$( 7)), discs$( 7)          , ch(06),~
                at (16,19), fac(sfac$( 7)), ovrpr$( 7)          , ch(01),~
                at (16,21), fac(sfac$( 7)), specl$( 7)          , ch(10),~
                at (16,34), fac(rfac$( 7)), alpha$( 7)          , ch(01),~
                at (16,36), fac(rfac$( 7)), reglr$( 7)          , ch(10),~
                at (16,48), fac(rfac$(15)), alpha$(15)          , ch(01),~
                at (16,50), fac(rfac$(15)), reglr$(15)          , ch(10),~
                at (16,62), fac(mfac$( 7)), numer$( 7)          , ch(01),~
                at (16,64), fac(mfac$( 7)), mrkup$( 7)          , ch(06),~
                at (16,71), fac(mfac$( 7)), msell$( 7)          , ch(10),~
                                                                         ~
                at (17,02), fac(sfac$( 8)), toqty$( 8)          , ch(08),~
                at (17,11), fac(sfac$( 8)), discs$( 8)          , ch(06),~
                at (17,19), fac(sfac$( 8)), ovrpr$( 8)          , ch(01),~
                at (17,21), fac(sfac$( 8)), specl$( 8)          , ch(10),~
                at (17,34), fac(rfac$( 8)), alpha$( 8)          , ch(01),~
                at (17,36), fac(rfac$( 8)), reglr$( 8)          , ch(10),~
                at (17,48), fac(rfac$(16)), alpha$(16)          , ch(01),~
                at (17,50), fac(rfac$(16)), reglr$(16)          , ch(10),~
                at (17,62), fac(mfac$( 8)), numer$( 8)          , ch(01),~
                at (17,64), fac(mfac$( 8)), mrkup$( 8)          , ch(06),~
                at (17,71), fac(mfac$( 8)), msell$( 8)          , ch(10),~
                                                                         ~
                at (18,02), fac(sfac$( 9)), toqty$( 9)          , ch(08),~
                at (18,11), fac(sfac$( 9)), discs$( 9)          , ch(06),~
                at (18,19), fac(sfac$( 9)), ovrpr$( 9)          , ch(01),~
                at (18,21), fac(sfac$( 9)), specl$( 9)          , ch(10),~
                                                                         ~
                at (19,02), fac(sfac$(10)), toqty$(10)          , ch(08),~
                at (19,11), fac(sfac$(10)), discs$(10)          , ch(06),~
                at (19,19), fac(sfac$(10)), ovrpr$(10)          , ch(01),~
                at (19,21), fac(sfac$(10)), specl$(10)          , ch(10),~
                                                                         ~
                at (21,02), fac(hex(a4)),   inpmessage$         , ch(79),~
                at (22,65), "(13)Instructions",                          ~
                at (23,65), "(15)Print Screen",                          ~
                at (24,65), "(16)Exit Routine",                          ~
                                                                         ~
                keys(hex(0d0f10)), key (keyhit%)

            if keyhit% <> 13 then L13190
                call "MANUAL" ("BCKPRCSB")
                goto L11760

L13190:     if keyhit% <> 15 then L65000 /* Must be PF(16)Exit Routine */
                call "PRNTSCRN"
                goto L11760

        REM *************************************************************~
            *         C O M M O N   S U B R O U T I N E S               *~
            *************************************************************

        non_curr_common_search_specials
*        Find non-currency-specific Special prices, if any.
            str(plowkey$,,1) = "S" : speckey$ = " "
L16070:     call "PLOWNEXT" (#04, plowkey$, 41%, f1%(4))
                if f1%(4) = 0% then return
            get #04 using L16100, from$, to$
L16100:         FMT POS(42), 2*CH(6)
            if from$ > date2$ then goto L16070  /* Not yet Effectve */
            if to$   < date2$ then goto L16070  /* Already Expired  */
            speckey$ = plowkey$
            get #04 using L16150, to%(), ovrpr$(), special(), discs()
L16150:         FMT POS(103), 10*BI(4), 10*CH(1), 10*PD(14,4), 10*PD(14,4)
            for n% = 1% to 10%
                if special(n%) > 0 then special(n%) = round(special(n%)  ~
                     * factor, 4)
            next n%
            return

        currency_common_search_specials
*        Find currency-specific Special prices, if any.
            str(plowkey$,,1) = "S" : speckey$ = " "
            str(plowkey$,11,4) = str(currency$)
            call "PLOWNEXT" (#05, plowkey$, 45%, f1%(5))
                if f1%(5) = 0% then return
            get #05 using L16290, from$, to$
L16290:         FMT POS(46), 2*CH(6)
            if from$ > date2$ then goto L16070  /* Not yet Effectve */
            if to$   < date2$ then goto L16070  /* Already Expired  */
            speckey$ = plowkey$
            get #05 using L16340, to%(), ovrpr$(), special(), discs()
L16340:         FMT POS(107), 10*BI(4), 10*CH(1), 10*PD(14,4), 10*PD(14,4)
            return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1988  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

            end
