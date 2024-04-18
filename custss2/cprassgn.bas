        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   CCC   PPPP   RRRR    AAA    SSSS   SSSS   GGGG   N   N  *~
            *  C      P   P  R   R  A   A  S      S      G       NN  N  *~
            *  C      PPPP   RRRR   AAAAA   SSS    SSS   G  GG   N N N  *~
            *  C      P      R  R   A   A      S      A  G   G   N  NN  *~
            *   CCC   P      R   R  A   A  SSSS   SSSS    GGG    N   N  *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CPRASSGN - Retrieves Unit Price for Part & Price Code     *~
            *            specified.  Returns error message if no price  *~
            *            record found or price is blank.  Locates any   *~
            *            redirection of the price code, special pricing *~
            *            and discounts.                                 *~
            *                                                           *~
            *            Searches the currency-specific price file      *~
            *            first. Then, if no price was found, searches   *~
            *            the non-currency-specific price file.          *~
            *                                               (EWD) Begin *~
            * APC MODS - Modified for APC to handle (35) Price Codes    *~
            *            cannot have Markups or Margins. Also Multi-    *~
            *            Currency is Disabled. Special Pricing not      *~
            *            Affected.                                      *~
            *            ( A thru Z ) - Subscripts (01) thru (26)       *~
            *            ( 0 thru 9 ) - Subscripts (27) thru (36)(EWD)  *~
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
            * 06/11/86 ! ORIGINAL                                 ! ERN *~
            * 02/02/87 ! Specials Pricing Expansion               ! ERN *~
            * 05/06/87 ! Standard Costing Enhancements            ! ERN *~
            * 12/01/87 ! Multi-currency modifications.            ! JIM *~
            * 05/03/88 ! Format PRICE via CURRFMT per D. Line PRR.! JIM *~
            * 09/29/88 ! noFormat PRICE via CURRFMT & rnd to 4 dec! JDH *~
            * 02/02/89 ! Proj 7880714 new price set implementation! JIM *~
            * 05/03/89 ! Corrected FMT of CONV & price when PC = 0! JDH *~
            * 09/15/89 ! Now CPRCURCY is created if MC is on.     ! JDH *~
            *          !   CPRUPDSB needs it to be there.         !     *~
            * 03/01/90 ! Changd PIC for exchange rate reverse date! JDH *~
            * 04/10/90 ! No longer creates any files.  Changes to ! JDH *~
            *          !   CPRUPDSB allow this.  Also, skip reads !     *~
            *          !   if there are no special prices.        !     *~
            * 06/28/96 ! Add indate var for datein tests          ! DER *~
            *          ! add reverse date for key                 ! DER *~
            * 09/04/97 ! Primed plow keys with all(hex(00))       ! DXL *~
            * 03/04/98 ! APC MODS FOR 35 PRICE CODES.       (EWD) ! RHH *~
            *          ! NOTE- MULTI CURRENCY IS DISABLED - 2321  !     *~
            *          ! LINES - 1140, (12720 - 12930)            !     *~
            *          ! Mods to Check for Y2K Complient          !     *~                      !     *~
            *          !                                    (EWD) !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "CPRASSGN"   (cust$,         /* Customer to find price for */~
                          type$,         /* Customer Type Code         */~
                          part$,         /* Part to find price for.    */~
                          cat$,          /* Part Category Code         */~
                          pcin$,         /* Std Price code (in only)   */~
                          datein$,       /* Pricing Date (fmt/unfmt)   */~
                          currtype$,     /* Currency table             */~
                          currency$,     /* Currency code              */~
                          stdin,         /* Std Cost; -1 to use current*/~
                          qty,           /* Quantity to base price on  */~
                          #02,           /* Channel for SYSFILE2       */~
                          #03,           /* Channel for HNYMASTR       */~
                          price,         /* Gross Unit Pr (-1 if none) */~
                          disc,          /* Discount Percent           */~
                          err$)          /* ERR MESSAGE (BLANK--> OK)  */

*        This routine is used whenever a unit price is to be assigned-
*        both for stocked and non-stocked parts.
*        The first four calling arguments identify the item to be
*        priced.  The price code passed is the default entered or
*        from the customer master. The date passed in is used as the
*        pricing effective date; quantity is used to select from the
*        special's range table.
*
*        If STDIN is passed in with a value other than -1 it will
*          be used as the standard cost for the part (only used if
*          price is determined via mark-ups).
*
*        PRICE is returned as -1 is the operator is to enter the
*          price.  An appropriate error message is also returned.
*          If a unit price is located it is returned with a precision
*          of 4 decimals.
*        DISC (DISCOUNT) is from the special's table.  It is returned
*          as 0 if no discount applies.  Note that a discount may be
*          returned even though the operator needs to enter the price
*          for a non-stock part. 
*

        dim                                                              ~
            cat$4,                       /* Part Category Code         */~
            curr$1, currency$4, currtype$1, /* Currency fields         */~
            cust$9,                      /* Customer Code              */~
            date$8,                      /* Pricing effective date     */~
            datein$8,                    /* Pricing Date From Caller   */~
            discs(10%),                  /* Special Discounts          */~
            err$79,                      /* Err return Message         */~
            from$6,                      /* From effective date        */~
            indate$8,                    /* in date catch and check    */~
            ovrpc$(10%)1,                /* Over-ride Price Codes      */~
            part$25,                     /* Part Number                */~
            pc$1,                        /* Price Code (A - Z)         */~
            pcin$1,                      /* Incomming PC for Customer  */~
            plowkey$99,                  /* Plow Key                   */~
            prices(36%),                 /* Unit Prices (Codes/Spc(EWD)*/~
            readkey$99,                  /* Misc Purpose Read Key      */~
            rev_date$6,                  /* Reverse Date               */~
            statutory$4,                 /* Statutory currency code    */~
            to%(10%),                    /* To qty ranges              */~
            to$6,                        /* To effective date          */~
            type$2                       /* Customer Type Code         */

        dim f2%(10%), f1%(10%)

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 03/05/98 Year 2000 Compliancy (EWD)      "

            mat f2% = con
        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! CPRPRICE ! Customer Pricing File                    *~
            * #02 ! SYSFILE2 ! CMS System Info   (Opened by Caller)     *~
            * #03 ! HNYMASTR ! Parts Master File (Opened by Caller)     *~
            * #04 ! CPRCURCY ! Currency-specific Customer Pricing File  *~
            * #05 ! CURCONVR ! Multi-Currency Conversion Tables         *~
            *************************************************************

            select #01,  "CPRPRICE",                                     ~
                        varc, indexed,                                   ~
                        recsize =  700,                                  ~
                        keypos =  1, keylen = 47

            select #04,  "CPRCURCY",                                     ~
                        varc, indexed,                                   ~
                        recsize =  700,                                  ~
                        keypos =  1, keylen = 51

            select #05, "CURCONVR",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  11

            if been_here_before% <> 0% then goto L10000
                been_here_before% = 1%
                call "OPENCHCK" (#01, 0%, f2%(1), 0%, " ")
                curr$ = "N" : statutory$ = " "
                call "READ100" (#02, "SWITCHS.CUR", f1%(2))
                if f1%(2) <> 0% then get #02 using L02320, curr$, statutory$
L02320:             FMT POS(21), CH(1), CH(4)
                curr$ = "N"                      /* (EWD) MOD 03/05/98 */
                if curr$ <> "Y" then goto L02400
                    call "OPENCHCK" (#04, 0%, f2%(4), 0%, " ")
                    call "OPENCHCK" (#05, 0%, f2%(5), 0%, " ")

L02400: REM Any special prices on file?  If not, we can skip READs later.
            specials_exist% = 0%
            curr_specials_exist% = 0%
            plowkey$ = "S"
            call "PLOWNEXT" (#01, plowkey$, 1%, specials_exist%)
            if curr$ <> "Y" then L10000

            plowkey$ = "S"
            call "PLOWNEXT" (#04, plowkey$, 1%, curr_specials_exist%)

L10000: REM *************************************************************~
            *        S U B R O U T I N E   L O G I C                    *~
            * ... First, see if a currency-specific price can be found. *~
            *************************************************************

            if curr$ <> "Y" then goto L12000    /* No Multi-Currency */
            if currency$ = " " then goto L12000 /* No Currency code  */
            gosub L15000                   /* If OK, then initialize */

*        SPECIALS- Find any Currency-specific OverRide PC or Price &/or
*                  discount
*             Search (1) Customer Number / Currency / Part Number
*                    (2) Customer Number / Currency / Part Category
*                    (3) Currency / Customer Type / Part Number
*                    (4) Currency / Customer Type / Part Category

          if curr_specials_exist% = 0% then currency_regular

*        SEARCH #1-  Customer Number / Currency / Part Number
            init (" ") readkey$, plowkey$
            str(plowkey$, 2, 9) = cust$
            str(plowkey$,17,25) = part$
            gosub currency_common_search

*        SEARCH #2-  Customer Number / Currency / Part Category
            init (" ") readkey$, plowkey$
            str(plowkey$, 2, 9) = cust$
            str(plowkey$,42, 4) = cat$
            gosub currency_common_search

*        SEARCH #3-  Currency / Customer Type / Part Number
            init (" ") readkey$, plowkey$
            str(plowkey$,15, 2) = type$
            str(plowkey$,17,25) = part$
            gosub currency_common_search

*        SEARCH #4-  Currency / Customer Type / Part Category
            init (" ") readkey$, plowkey$
            str(plowkey$,15, 2) = type$
            str(plowkey$,42, 4) = cat$
            gosub currency_common_search
            goto currency_regular       /* No special pricing in effect */

        currency_common_search
            str(plowkey$,,1) = "S"
            str(plowkey$,11,4) = str(currency$)
            readkey$ = " "
            str(plowkey$,46%) = all(hex(00))
L10450:     call "PLOWNEXT" (#04, plowkey$, 45%, f1%(4))
            if f1%(4) = 0% then L10610
                get #04 using L10480, from$, to$
L10480:             FMT XX(45), 2*CH(6)
                if from$  > indate$ then goto L10610  /* Not yet Effectve */
                if to$    < indate$ then goto L10450  /* Already Expired  */
                    readkey$ = plowkey$            /* Might be the one */
                    get #04 using L10570, to%(), ovrpc$(),                ~
                        prices(1), prices(2), prices(3), prices(4),      ~
                        prices(5), prices(6), prices(7), prices(8),      ~
                        prices(9), prices(10), discs()

L10570:                 FMT XX(106), 10*BI(4), 10*CH(1), 10*PD(14,4),    ~
                            10*PD(14,4)
                goto L10450

L10610:     if readkey$ = " " then return          /* No Record found  */
                return clear   /* End the Search here because we've    */
                               /* found what we were looking for.      */
                i% = 0%
                for i1% = 10% to 1% step -1%     /* Find qty range     */
                    if to%(i1%)  = 0% then L10680
                        if abs(qty) <= to%(i1%) then i% = i1%
L10680:         next i1%
                if i% = 0% then goto currency_regular/* No range applies*/

                if ovrpc$(i%) <> " " then pc$ = ovrpc$(i%)
                disc  = discs (i%)
                price = prices(i%)

                if price > 0 then end   /* Got all we need  */

        currency_regular   /* Regular Pricing, wrapup */
            if pc$ >= "A" and pc$ <= "P" then currency_price_code
            if pc$ >= "0" and pc$ <= "8" then currency_markups
            goto L12000 /* Try non-currency-specific price file */

        currency_price_code  /* Standard Price Code. Derive unit price.*/
            pc% = val(str(pc$,,1), 1) - 64%
            readkey$ = "C" & str(currency$) & part$
            call "CPRUPDSB" (#04, 1%, "00", readkey$, 0%, f1%(4))
            if f1%(4) = 0% then goto L12000 /* Try non-currency-specific */
            get #04 using L10880, prices()
L10880:         FMT XX(60), 16*PD(14,4)

            price = prices(pc%)
            if price > 0 then end
            goto L12000 /* Try non-currency-specific price file */

        currency_markups /* Determine Unit price based on std cost */
            convert pc$ to pc%           /* 0 - 8  */
              if pc% = 0% then L11030
            readkey$ = "C" & str(currency$) & part$
            call "CPRUPDSB" (#04, 1%, "00", readkey$, 0%, f1%(4))
            if f1%(4) = 0% then goto L12000 /* Try non-currency-specific */
                get #04 using L11020, prices(1), prices(2), prices(3),    ~
                    prices(4), prices(5), prices(6), prices(7), prices(8)

L11020:             FMT XX(188), 8*PD(14,4)
L11030:     if pc% = 0% then markup = 0 else markup = prices(pc%)
            if markup = -999.99 then L12000 /* Try non-currency-specific*/
                if stdin = -1 then L11080
                     std = stdin    /* Use Standard passed from caller */
                     goto L11140
L11080:         call "READ100" (#03, part$, f1%(3))
                if f1%(3) = 0% then L12000    /* Unlikely */
                get #03 using L11110, conv
L11110:              FMT POS(82), PD(14,7)
                call "STCCOSTS" (part$, " ", #02, 1%, std)
                std   = conv * std
L11140:         price = round((std + (std * markup * .01)), 4)
                if price > 0 then end

L12000: REM *************************************************************~
            * ... No currency-specific price could be found, so we try  *~
            * the regular (original) price files.                       *~
            *************************************************************

*        SPECIALS  Find any Over-Ride PC or Price &/or Discount
*             Search (1) Customer Number / Part Number
*                    (2) Customer Number / Part Category
*                    (3) Customer Type   / Part Number
*                    (4) Customer Type   / Part Category

            gosub L15000 /* Re-initialize */

            if specials_exist% = 0% then regular

*        SEARCH #1-  Customer Number / Part Number
            init (" ") readkey$, plowkey$
            str(plowkey$, 2, 9) = cust$
            str(plowkey$,13,25) = part$
            gosub common_search

*        SEARCH #2-  Customer Number / Part Category
            init (" ") readkey$, plowkey$
            str(plowkey$, 2, 9) = cust$
            str(plowkey$,38, 4) = cat$
            gosub common_search

*        SEARCH #3-  Customer Type   / Part Number
            init (" ") readkey$, plowkey$
            str(plowkey$,11, 2) = type$
            str(plowkey$,13,25) = part$
            gosub common_search

*        SEARCH #4-  Customer Type   / Part Category
            init (" ") readkey$, plowkey$
            str(plowkey$,11, 2) = type$
            str(plowkey$,38, 4) = cat$
            gosub common_search
            goto regular       /* No special pricing in effect         */

        common_search
            str(plowkey$,,1) = "S"  :  readkey$ = " "
            str(plowkey$,42%) = all(hex(00))
L12400:     call "PLOWNEXT" (#01, plowkey$, 41%, f1%(1))
            if f1%(1) = 0% then L12560
                get #01 using L12430, from$, to$
L12430:             FMT XX(41), 2*CH(6)
                if from$  > indate$ then goto L12560  /* Not yet Effectve */
                if to$    < indate$ then goto L12400  /* Already Expired  */
                    readkey$ = plowkey$            /* Might be the one */
                    get #01 using L12520, to%(), ovrpc$(),                ~
                        prices(1), prices(2), prices(3), prices(4),      ~
                        prices(5), prices(6), prices(7), prices(8),      ~
                        prices(9), prices(10), discs()

L12520:                 FMT XX(102), 10*BI(4), 10*CH(1), 10*PD(14,4),    ~
                            10*PD(14,4)
                goto L12400

L12560:     if readkey$ = " " then return          /* No Record found  */
                return clear   /* End the Search here because we've    */
                               /* found what we were looking for.      */
                i% = 0%
                for i1% = 10% to 1% step -1%     /* Find qty range     */
                    if to%(i1%)  = 0% then L12630
                        if abs(qty) <= to%(i1%) then i% = i1%
L12630:         next i1%
                if i% = 0% then goto regular   /* No range applies */

                if ovrpc$(i%) <> " " then pc$ = ovrpc$(i%)
                disc  = discs (i%)
                price = prices(i%)

                if price <> -1 then get_out   /* Got all we need  */

        regular   /* Regular Pricing, wrapup */ /* (EWD) MOD 03/05/98 */
            if pc$ >= "A" and pc$ <= "P" then price_code
            if pc$ >= "R" and pc$ <= "Z" then price_code
        REM IF PC$ >= "0" AND PC$ <= "8" THEN MARKUPS
            if pc$ >= "0" and pc$ <= "9" then price_code_1
            if pc$  = "Q" then err$ = "Please Enter Unit Price"          ~
                          else err$ = "INVALID PRICE CODE."
            goto get_out
                                                  /* (EWD) 0 Mod Begin */
        price_code  /* Standard Price Code.  Derive unit price.        */
            pc% = val(str(pc$,,1), 1) - 64%       /*   A  THRU   Z     */
            goto L12810                           /* (01) THRU (26)    */
        price_code_1
            pc% = val(str(pc$,,1), 1) - 21%       /*   0  THRU   9     */
                                                  /* (27) THUR (36)    */
L12810:     readkey$ = "C" & part$
            call "CPRUPDSB" (#01, 0%, "00", readkey$, 0%, f1%(1))
            if f1%(1) = 1% then goto L12860
L12840:         err$ = "No prices on file for part: " & part$
                goto get_out
L12860:     get #01 using L12870, prices()
L12870:         FMT XX(56), 36*PD(14,4)           /* (EWD) - Mod End  */

            price = prices(pc%)
            if price <> -1 then get_out
L12910:         err$ = "No price assigned. Please enter price."
                goto get_out

        markups      /* Determine Unit price based on std cost         */
            convert pc$ to pc%           /* 0 - 8  */
              if pc% = 0% then L13030
            readkey$ = "C" & part$
            call "CPRUPDSB" (#01, 0%, "00", readkey$, 0%, f1%(1))
            if f1%(1) = 0% then goto L12840
                get #01 using L13020, prices(1), prices(2), prices(3),    ~
                    prices(4), prices(5), prices(6), prices(7), prices(8)

L13020:             FMT XX(184), 8*PD(14,4)
L13030:     if pc% = 0% then markup = 0 else markup = prices(pc%)
            if markup = -999.99 then L12910
                if stdin = -1 then L13080
                    std = stdin    /* Use Standard passed from caller */
                    goto L13140
L13080:         call "READ100" (#03, part$, f1%(3))
                if f1%(3) = 0% then L12910    /* Unlikely */
                get #03 using L13110, conv
L13110:             FMT POS(82), PD(14,7)
                call "STCCOSTS" (part$, " ", #02, 1%, std)
                std   = conv * std
L13140:         price = round((std + (std * markup * .01)), 4)

        get_out /* Routine factors price found to transaction price */
            if err$ <> " " then end
            if price = -1 then end
            if curr$ <> "Y" then end
            if currency$ = " " then end
            if currency$ = statutory$ then end
                call "DATREVRS" (date$, rev_date$, errormsg$)
                if errormsg$ <> " " then end
                readkey$ = str(currtype$) & str(currency$) &             ~
                    str(rev_date$,1,6)
                call "PLOWNEXT" (#05, readkey$, 5%, f1%(5))
                if f1%(5) = 0% then end
                    get #05 using L13260, convfact
L13260:                 FMT POS(26), PD(14,7)
                    price = round(price * convfact, 4)
                end

L15000: REM *************************************************************~
            *         I N I T I A L I Z A T I O N S                     *~
            *************************************************************

            price = -1
            disc  = 0
            pc$ = pcin$
            indate$ = datein$
            call "DATEOK" (indate$, d%, err$) /* Make sure its formatted */
            if d% = 0% then indate$ = date else call "DATUNFMT" (indate$)
            err$ = " "
            return

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN