        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  H   H  N   N  Y   Y  H   H   OOO   L      DDDD           *~
            *  H   H  NN  N  Y   Y  H   H  O   O  L      D   D          *~
            *  HHHHH  N N N   YYY   HHHHH  O   O  L      D   D          *~
            *  H   H  N  NN    Y    H   H  O   O  L      D   D          *~
            *  H   H  N   N    Y    H   H   OOO   LLLLL  DDDD           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYHOLD  - Short and to the point.  Updates Pending       *~
            *            Withdrawal Quantity if the record exists.      *~
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
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "HNYHOLD" (#1, part$, store$, lot$, quantity, return%)

        dim                                                              ~
            lot$16,                      /* Lot                        */~
            part$25,                     /* Part                       */~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            store$3                      /* Store                      */~

        dim f1%(1)                       /* = 1 if READ was successful */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "04.19.00 03/13/87 Serial number, Lot tracking     "
        REM *************************************************************

            mat f1% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! HNYQUAN  ! Inventory Quantity File                  *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            return% = 99%

        REM *************************************************************~
            *                M A I N   P R O G R A M                    *~
            *-----------------------------------------------------------*~
            * Handles the task.                                         *~
            *************************************************************

            str(readkey$,1,25) = part$
            str(readkey$,26)   = store$
            str(readkey$,29)   = lot$

            call "READ101" (#1, readkey$, f1%(1%))
               if f1%(1%) = 0% then L65000

            get #1, using L10140, pendqty
L10140:         FMT POS(109), PD(14,4)
            pendqty = max(0, pendqty + quantity)
            put #1, using L10140, pendqty

            rewrite #1
            return% = 0%

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
