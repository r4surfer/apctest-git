        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  H   H  N   N  Y   Y   AAA   V   V   AAA   IIIII  L       *~
            *  H   H  NN  N  Y   Y  A   A  V   V  A   A    I    L       *~
            *  HHHHH  N N N   YYY   AAAAA  V   V  AAAAA    I    L       *~
            *  H   H  N  NN    Y    A   A   V V   A   A    I    L       *~
            *  H   H  N   N    Y    A   A    V    A   A  IIIII  LLLLL   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYAVAIL - Validates Quantity against current Quantity    *~
            *            on Hand and Pending Withdrawals.  Returns      *~
            *            ERRORMSG for controlled parts, non-zero        *~
            *            return code for overdraw of others.            *~
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
            * 02/04/87 ! Original                                 ! KAB *~
            * 05/21/92 ! Zeroed pending quantity at start.        ! JDH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "HNYAVAIL" (#1, #2, part$, store$, lot$, errormsg$,          ~
                             withdraw, avail, return%)

        dim                                                              ~
            errormsg$79,                 /* Error Message              */~
            lot$16,                      /* Lot                        */~
            part$25,                     /* Part                       */~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            store$3                      /* Store                      */~

        dim f1%(2)                       /* = 1 if READ was successful */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.00 09/09/92 Cycle Counting & MPS Phase I    "
        REM *************************************************************

            mat f1% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! HNYMASTR ! Inventory Master File                    *~
            * # 2 ! HNYQUAN  ! Inventory Store/Lot Quantities           *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            errormsg$ = " "
            avail, pendqty = 0
            return% = 99%

        REM *************************************************************~
            *                M A I N   P R O G R A M                    *~
            *-----------------------------------------------------------*~
            * Handles the task.                                         *~
            *************************************************************

            str(readkey$,1,25) = part$
            str(readkey$,26)   = store$
            str(readkey$,29)   = lot$

            call "READ100" (#2, readkey$, f1%(2%))
               if f1%(2%) = 0% then L11000

            return% = 1%
            get #2, using L10150, onhand, pendqty
L10150:         FMT POS(69), PD(14,4), POS(109), PD(14,4)
            avail = onhand - pendqty
            if avail < withdraw then L11000

            return% = 0%
            goto L65000

L11000: REM *************************************************************~
            *                M A I N   P R O G R A M                    *~
            *-----------------------------------------------------------*~
            * Handles Error Condition.                                  *~
            *************************************************************

            call "READ100" (#1, str(readkey$,,25%), f1%(1%))
               if f1%(1%) = 0% then L65000

            get #1, using L11150, temp$
L11150:         FMT POS(132), CH(1)

            if temp$ <> "Y" then L65000

            errormsg$ = "Withdrawal Exceeds Maximum Quantity Available:"
            call "CONVERT" (avail, -0.2, str(errormsg$,                  ~
                               len(errormsg$)+2%, 10%))
            if pendqty <> 0 then                                         ~
               errormsg$ = errormsg$ & " (Pending Trans.)"

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
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

            end
