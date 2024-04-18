        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  H   H  N   N  Y   Y   GGG   L       GGG   EEEEE  TTTTT   *~
            *  H   H  NN  N  Y   Y  G      L      G      E        T     *~
            *  HHHHH  N N N   YYY   G GGG  L      G GGG  EEEE     T     *~
            *  H   H  N  NN    Y    G   G  L      G   G  E        T     *~
            *  H   H  N   N    Y     GGG   LLLLL   GGG   EEEEE    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYGLGET - Gets indicated GL account from HNYQUAN, or if  *~
            *            HNYQUAN doesn't exist yet, uses the subroutine *~
            *            GLHNYQN to pass back what it should be, when   *~
            *            HNYPST1, HNYPST2, or HNYQIPUT create the record*~
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
            * 02/27/86 ! ORIGINAL                                 ! KEN *~
            * 01/19/87 ! HNYQUAN record layout changes.           ! LDJ *~
            * 04/09/87 ! Standard Cost Project Modifications      ! ERN *~
            * 04/23/87 ! Part/Store/blank Lot mod (whew!)         ! JIM *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            sub "HNYGLGET" (part$,       /* PART CODE                  */~
                            store$,      /* STORE/WAREHOUSE            */~
                            lotno$,      /* LOT                        */~
                            acct$,       /* ACCOUNT RECEIVER (unfmtd)  */~
                            acct%,       /* WHICH ONE (See below)      */~
                            #1,          /* HNYMASTR                   */~
                            #2)          /* HNYQUAN                    */~

*        ACCT% -   1 = Purchasing Source       !  Returned as 0 if
*                  2 = WIP Source              !  Accounts found in
*                  3 = Inventory Assets        !  HNYQUAN, -1 if found
*                  4 = Cost of Goods Sold      !  in HNYMASTR. 99 if
*                  5 = Sales                   !  invalid request, 98
*                  6 = Inventory Adjustments   !  if unable to find
*             7 - 18 = Variance Accounts       !  part.
*                 19 = Sales Discounts         !

        dim                                                              ~
            part$25,                     /* PART CODE                  */~
            store$3,                     /* STORE/WAREHOUSE            */~
            lot$16,                      /* LOT                        */~
            lotno$,                      /* LOT                        */~
            acct$9,                      /* ACCOUNT RECEIVER           */~
            readkey$50,                  /* THE EVER POPULAR READKEY   */~
            acct$(19)9,                  /* ACCOUNT ARRAY              */~
            cat$,                        /* CATEGORY CODE              */~
            costtype$1                   /* COSTING METHOD             */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto   L01102
            cms2v$ = "05.00.00 09/08/87 Standard cost to 12             "
L01102: REM *************************************************************

*        Initializations ***********************************************
            which% = acct% : acct$ = " " : lot$ = lotno$

            acct%  = 99% : if which% < 1% or which% > 19% then end

            acct% = 98%
            if open1% = 0% then call "OPENCHCK" (#1, open1%, 0%, 0%, " ")
            if open1% < 0% then end
            call "READ100" (#1, part$, f1%)
            if f1% = 0% then end

            if open2% = 0% then call "OPENCHCK" (#2, open2%, 0%, 0%, " ")
            if open2% < 0% then L10220


*        Get Accounts    ***********************************************
*        First try HNYQUAN for the accounts
            readkey$ = str(part$,,25) & str(store$,,3) & lot$
            call "READ100" (#2, readkey$, f1%)  /* 1st from HNYQUAN    */
            if f1% = 0% then L10120
                get #2 using L10060, acct$()
L10060:              FMT POS(241), 18*CH(9)
                get #1 using L10080, acct$(19)
L10080:              FMT POS(497), CH(9)
                acct% = 0%
                goto L10280

L10120: REM 04/23/87-  Part/Store/blank Lot mod (whew!)           JIM
            if lot$ = " " then goto L10220 /* Not if already blank */
*        Else try HNYQUAN for the accounts (with a blank Lot #)
            readkey$ = str(part$,,25) & str(store$,,3)
            call "READ100" (#2, readkey$, f1%)  /* 2nd from HNYQUAN    */
            if f1% = 0% then goto L10220
                get #2 using L10060, acct$()
                get #1 using L10080, acct$(19)
                acct% = 0%
                goto L10280

L10220
*        No HNYQUAN - get accounts from HNYMASTR
            get #1, using L10240, cat$, costtype$, acct$()
L10240:         FMT POS(90), CH(4), POS(307), CH(1), POS(335), 19*CH(9)
            acct% = -1%
            call "GLHNYQN" (part$, store$, lot$, cat$, costtype$, acct$())

L10280
*        Return to Caller
            acct$ = acct$(which%)
            end
