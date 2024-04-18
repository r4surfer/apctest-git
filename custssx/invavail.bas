        REM *************************************************************~
            *                                                           *~
            *  IIIII  N   N  V   V   AAA   V   V   AAA   IIIII  L       *~
            *    I    NN  N  V   V  A   A  V   V  A   A    I    L       *~
            *    I    N N N  V   V  AAAAA  V   V  AAAAA    I    L       *~
            *    I    N  NN   V V   A   A   V V   A   A    I    L       *~
            *  IIIII  N   N    V    A   A    V    A   A  IIIII  LLLLL   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * INVAVAIL - Validates Quantity against current Quantity    *~
            *            on Hand and Pending Withdrawals.  Returns      *~
            *            ERRORMSG for controlled parts, non-zero        *~
            *            return code for overdraw of others.            *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/04/87 ! Original                                 ! KAB *~
            * 05/21/92 ! Zeroed pending quantity at start.        ! JDH *~
            * 01/30/06 ! (PAR000) CR347 Mod for New Part Number   ! RHH *~
            **************************************************************

        sub "INVAVAIL" (#1, #2, part$, store$, lot$, errormsg$,          ~
                             withdraw, avail, return%)

        dim                                                              ~
            errormsg$79,                 /* Error Message              */~
            lot$16,                      /* Lot                        */~
            part$45,                     /* Part               (PAR000)*/~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            store$3                      /* Store                      */~

        dim f1%(2)                       /* = 1 if READ was successful */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "REV:01.00 01/30/06 Mo for New Part Number         "
        REM *************************************************************

            mat f1% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! INVMASTR ! Inventory Master File            (PAR000)*~
            * # 2 ! INVQUAN  ! Inventory Store/Lot Quantities   (PAR000)*~
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

            str(readkey$,1,45) = part$
            str(readkey$,46)   = store$
            str(readkey$,49)   = lot$

            call "READ100" (#2, readkey$, f1%(2%))
               if f1%(2%) = 0% then L11000

            return% = 1%
            get #2, using L10150, onhand, pendqty
L10150:         FMT POS(89), PD(14,4), POS(129), PD(14,4)
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
L11150:         FMT POS(152), CH(1)

            if temp$ <> "Y" then L65000

            errormsg$ = "Withdrawal Exceeds Maximum Quantity Available:"
            call "CONVERT" (avail, -0.2, str(errormsg$,                  ~
                               len(errormsg$)+2%, 10%))
            if pendqty <> 0 then                                         ~
               errormsg$ = errormsg$ & " (Pending Trans.)"

L65000:

            end
