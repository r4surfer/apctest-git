        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   AAA   RRRR   M   M   GGG   L       GGG   EEEEE  TTTTT   *~
            *  A   A  R   R  MM MM  G      L      G      E        T     *~
            *  AAAAA  RRRR   M M M  G GGG  L      G GGG  EEEE     T     *~
            *  A   A  R   R  M   M  G   G  L      G   G  E        T     *~
            *  A   A  R   R  M   M   GGG   LLLLL   GGG   EEEEE    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARMGLGET - Derives the account requested.  See calling    *~
            *            syntax for details.                            *~
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
            * 06/18/86 ! ORIGINAL                                 ! ERN *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "ARMGLGET"   (acct%,         /* Which account to derive    */~
                          customer$,     /* Customer Number            */~
                          part$,         /* Part Number                */~
                          cat$,          /* Part Category              */~
                          taxcode$,      /* Sales Tax Code             */~
                          store$,        /* Store Number               */~
                          open$,         /* For customizing            */~
                          #1,            /* SYSFILE2                   */~
                          #2,            /* CUSTOMER                   */~
                          #3,            /* HNYMASTR                   */~
                          #4,            /* CATEGORY                   */~
                          #5,            /* STXCODES                   */~
                          account$ )     /* Returned Account Number    */

*        ACCT% -   1: Sales Distribution
*                  2: Sales Discounts
*                  3: Shipped Not Billed Inventory
*                  4: Net Invoice (A/R Distribution)
*                  5: Cash-in-Bank           [For Invoicing]
*                  6: Sales Tax
*                  7: Freight
*                  8: Finance Charges
*                  9: Cash-in-Bank               [Cash Only]
*                 10: Cash Discounts- Earned     [Cash Only]
*                 11: Cash Discounts- Unearned   [Cash Only]
*
*        The Caller is responsible for opening all the files required
*        to derive the account(s) requested.  The first non-blank
*        account encountered is returned (after it is passed through
*        GLFMT).  It may or may not be valid!!!!


        dim                                                              ~
            acct$(11),                   /* Read array                 */~
            account$16,                  /* Account to derive          */~
            cat$4,                       /* Part Category Code         */~
            customer$9,                  /* Customer Number            */~
            open$20,                     /* For Customizations         */~
            part$25,                     /* Part Code                  */~
            readkey$30,                  /* Misc Read Key              */~
            store$3,                     /* Store Code                 */~
            taxcode$10                   /* Sales Tax Code             */

        dim f1%(64)                      /* = 1 if READ was successful */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "05.00.00 09/08/87 Standard cost to 12             "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! SYSFILE2 ! Caelus Management System General Informa *~
            * #2  ! CUSTOMER ! Customer Master File                     *~
            * #3  ! HNYMASTR ! Inventory Master File                    *~
            * #4  ! CATEGORY ! Inventory category codes file            *~
            * #5  ! STXCODES ! Sales Tax Codes                          *~
            *************************************************************~

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * Initializes information necessary for program.            *~
            *************************************************************

            account$ = " "

        REM *************************************************************~
            *                M A I N   L O G I C                        *~
            * --------------------------------------------------------- *~
            * Main program logic.                                       *~
            *************************************************************

            on acct% goto sales, sales_discounts, shipped_not_billed,    ~
                          ar, inv_cash_in_bank, sales_tax, freight,      ~
                          finance_charges, crc_cash_in_bank,             ~
                          allowed_discounts, unallowed_discounts
            goto L65000

        sales                /* Derive SALES DISTRIBUTION ACCOUNT (#1) */
        sales_discounts      /* Derive SALES DISCOUNT ACCOUNT     (#2) */
            if part$ <> " " then gosub read_part                         ~
                            else gosub read_customer
            if acct$(acct%) <> " " then exit_routine

            gosub read_category
            if acct$(acct%) <> " " then exit_routine

            gosub read_store
            if acct$(acct%) <> " " then exit_routine  else L65000


        shipped_not_billed   /* Derive UNBILLED SHIPMENTS ACCOUNT (#3) */
            gosub read_store
            if acct$(acct%) <> " " then exit_routine

            gosub read_system
            if acct$(acct%) <> " " then exit_routine  else L65000


        ar                   /* Derive NET INVOICE DISTR. ACCOUNT (#4) */
        inv_cash_in_bank     /* Derive INVCE CASH-IN-BANK ACCOUNT (#5) */
        sales_tax            /* Derive SALES TAX ACCOUNT          (#6) */
        freight              /* Derive FREIGHT ACCOUNT ACCOUNT    (#7) */
            gosub read_customer
            if acct$(acct%) <> " " then exit_routine

            gosub read_store
            if acct$(acct%) <> " " then exit_routine

            if acct% <> 6% then L10470
                gosub read_taxcode
                if acct$(acct%) <> " " then exit_routine  else L10480

L10470:     if acct% <> 7% then L65000    /* Freight          */
L10480:         gosub read_system        /* Freight & Tax    */
                if acct$(acct%) <> " " then exit_routine  else L65000


        finance_charges      /* Derive FINANCE CHARGE ACCOUNT     (#8) */
            gosub read_customer
            if acct$(acct%) <> " " then exit_routine

            gosub read_system
            if acct$(acct%) <> " " then exit_routine  else L65000


        crc_cash_in_bank     /* Derive CASH CASH_IN_BANK ACCOUNT (# 9) */
        allowed_discounts    /* Derive ALLOWED DISCOUNTS ACCOUNT (#10) */
        unallowed_discounts  /* Derive UNALLOWED DISCS.  ACCOUNT (#11) */
            gosub read_system
            if acct$(acct%) <> " " then exit_routine  else L65000



        exit_routine
            account$ = acct$(acct%)
            call "GLFMT" (account$)
            goto L65000


        REM *************************************************************~
            *             D I S K   I / O   R O U T I N E S             *~
            * --------------------------------------------------------- *~
            * Routines for retrieving data from files.                  *~
            *************************************************************
        read_store
            init(" ") acct$()
            if store$ = " " then return
                readkey$ = "DEFAULTS.STORE." & store$
                call "READ100" (#1, readkey$, f1%(1))
                if f1%(1) = 0% then return
                     get #1 using L30140, acct$( 4), acct$( 5), acct$( 7),~
                                         acct$( 2), acct$( 6), acct$( 3),~
                                         acct$( 1)
L30140:                   FMT XX(26), 7*CH(9)
                     return


        read_system
            init(" ") acct$()
            call "BCKSWTCH" ("AR ", "SHIPACCT", acct$( 3), v, r%)
                call "GLUNFMT" (acct$( 3))
            call "BCKSWTCH" ("AR ", "CASHACCT", acct$( 5), v, r%)
                call "GLUNFMT" (acct$( 5))
            call "BCKSWTCH" ("AR ", "TAXACCT ", acct$( 6), v, r%)
                call "GLUNFMT" (acct$( 6))
            call "BCKSWTCH" ("AR ", "FRTACCT ", acct$( 7), v, r%)
                call "GLUNFMT" (acct$( 7))
            call "BCKSWTCH" ("AR ", "FCACCT  ", acct$( 8), v, r%)
                call "GLUNFMT" (acct$( 8))
            call "BCKSWTCH" ("AR ", "DISCACCT", acct$(10), v, r%)
                call "GLUNFMT" (acct$(10))
            call "BCKSWTCH" ("AR ", "UNALDISC", acct$(11), v, r%)
                call "GLUNFMT" (acct$(11))
            acct$( 9) = acct$(5) /* Cash-in-Bank */
            return


        read_customer
            init(" ") acct$()
            if customer$ = " " then return
                call "READ100" (#2, customer$, f1%(2))
                if f1%(2) = 0% then return
                     get #2 using L30460, acct$( 1), acct$( 4), acct$( 5),~
                                         acct$( 7), acct$( 2), acct$( 6),~
                                         acct$( 8)
L30460:                   FMT XX(462), 6*CH(9), POS(1035), CH(9)
                     return

        read_part
            init(" ") acct$()
            if part$ = " " then return
                call "READ100" (#3, part$, f1%(3))
                if f1%(3) = 0% then return
                     get #3 using L30550, acct$(1), acct$(2)
L30550:                   FMT POS(371), CH(9), POS(497), CH(9)
                     return

        read_category
            init(" ") acct$()
            if cat$ = " " then return
                call "READ100" (#4, cat$, f1%(4))
                if f1%(4) = 0% then return
                     get #4 using L30640, acct$(1), acct$(2)
L30640:                   FMT XX(34), 2*CH(9)
                     return

        read_taxcode
            init(" ") acct$()
            if taxcode$ = " " then return
                call "READ100" (#5, taxcode$, f1%(5))
                if f1%(5) = 0% then return
                     get #5 using L30730, acct$(6)
L30730:                   FMT XX(48), CH(9)
                     return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            * --------------------------------------------------------- *~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            end
