        REM *************************************************************~
            *                                                           *~
            *  IIIII  N   N  V   V   GGG   L       GGG   EEEEE  TTTTT   *~
            *    I    NN  N  V   V  G      L      G      E        T     *~
            *    I    N N N  V   V  G GGG  L      G GGG  EEEE     T     *~
            *    I    N  NN   V V   G   G  L      G   G  E        T     *~
            *  IIIII  N   N    V     GGG   LLLLL   GGG   EEEEE    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * INVGLGET - Gets indicated GL account from INVQUAN, or if  *~
            *            INVQUAN doesn't exist yet, uses the subroutine *~
            *            GLHNYQN to pass back what it should be, when   *~
            *            HNYPST1, HNYPST2, or HNYQIPUT create the record*~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/27/86 ! ORIGINAL                                 ! KEN *~
            * 01/19/87 ! HNYQUAN record layout changes.           ! LDJ *~
            * 04/09/87 ! Standard Cost Project Modifications      ! ERN *~
            * 04/23/87 ! Part/Store/blank Lot mod (whew!)         ! JIM *~
            * 01/30/06 ! (PAR000) CR347 New Part Number           ! RHH *~
            *************************************************************

            sub "INVGLGET" (part$,       /* PART CODE                  */~
                            store$,      /* STORE/WAREHOUSE            */~
                            lotno$,      /* LOT                        */~
                            acct$,       /* ACCOUNT RECEIVER (unfmtd)  */~
                            acct%,       /* WHICH ONE (See below)      */~
                            #1,          /* INVMASTR           (PAR000)*/~
                            #2)          /* INVQUAN            (PAR000)*/

*        ACCT% -   1 = Purchasing Source       !  Returned as 0 if
*                  2 = WIP Source              !  Accounts found in
*                  3 = Inventory Assets        !  INVQUAN, -1 if found
*                  4 = Cost of Goods Sold      !  in INVMASTR. 99 if
*                  5 = Sales                   !  invalid request, 98
*                  6 = Inventory Adjustments   !  if unable to find
*             7 - 18 = Variance Accounts       !  part.
*                 19 = Sales Discounts         !

        dim                                                              ~
            part$45,                     /* PART CODE          (PAR000)*/~
            store$3,                     /* STORE/WAREHOUSE            */~
            lot$16,                      /* LOT                        */~
            lotno$,                      /* LOT                        */~
            acct$9,                      /* ACCOUNT RECEIVER           */~
            readkey$70,                  /* THE EVER POPULAR READKEY   */~
            acct$(19%)9,                 /* ACCOUNT ARRAY              */~
            cat$,                        /* CATEGORY CODE              */~
            costtype$1                   /* COSTING METHOD             */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto   L01102
            cms2v$ = "REV:01.00 01/30/06 New Part Number Std cost to 12 "
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
*        First try INVQUAN for the accounts
            readkey$ = str(part$,,25) & str(store$,,3) & lot$
            call "READ100" (#2, readkey$, f1%)  /* 1st from INVQUAN    */
            if f1% = 0% then L10120
                                                /* (PAR000)            */ 
                get #2 using L10060, acct$()
L10060:              FMT POS(261), 18*CH(9)     /* (PAR000)            */
                get #1 using L10080, acct$(19)
L10080:              FMT POS(517), CH(9)        /* (PAR000)            */
                acct% = 0%
                goto L10280
                                                /* (PAR000)            */
L10120: REM 04/23/87-  Part/Store/blank Lot mod (whew!)           JIM
            if lot$ = " " then goto L10220 /* Not if already blank */
*        Else try INVQUAN for the accounts (with a blank Lot #)
                                                /* (PAR000)            */
            readkey$ = str(part$,,45) & str(store$,,3)
            call "READ100" (#2, readkey$, f1%)  /* 2nd from INVQUAN    */
            if f1% = 0% then goto L10220
                get #2 using L10060, acct$()
                get #1 using L10080, acct$(19)
                acct% = 0%
                goto L10280
                                                /* (PAR000)            */
L10220
*        No INVQUAN - get accounts from INVMASTR
            get #1, using L10240, cat$, costtype$, acct$()
L10240:         FMT POS(110), CH(4), POS(327), CH(1), POS(355), 19*CH(9)
            acct% = -1%
                                             /* (PAR000) Part not Used */ 
            call "GLHNYQN" (part$, store$, lot$, cat$, costtype$, acct$())

L10280
*        Return to Caller
            acct$ = acct$(which%)
            end
