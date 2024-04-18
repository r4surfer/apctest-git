        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  H   H  N   N  Y   Y  TTTTT   OOO   TTTTT   SSS   BBBB    *~
            *  H   H  NN  N  Y   Y    T    O   O    T    S      B   B   *~
            *  HHHHH  N N N   YYY     T    O   O    T     SSS   BBBB    *~
            *  H   H  N  NN    Y      T    O   O    T        S  B   B   *~
            *  H   H  N   N    Y      T     OOO     T     SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYTOTSB - Passes back to caller the HNYQUAN totals re-   *~
            *            quested. It works on the 'general purpose'     *~
            *            principle -- you may call it for 1 to 6 qty    *~
            *            totals for a part; you may call it for 1 to    *~
            *            6 quantities for a specific part/store/lot.    *~
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
            * 04/28/87 ! Original                                 ! JRH *~
            * 11/25/91 ! PRR 11991. Fixed internal documentation. ! JDH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "HNYTOTSB" (partnbr$,        /* Passed Part Number         */~
                                         /* -- MUST be non-blank       */~
                        store$,          /* Passed Store Number        */~
                                         /* -- may be blank            */~
                        lot$,            /* Passed Lot Number          */~
                                         /* -- may be blank            */~
                        quan(),          /* Returned quantity(ies)     */~
                                         /* -- MUST be an ARRAY at     */~
                                         /* -- least 1 element long.   */~
                                         /* -- Quantities passed back  */~
                                         /* -- depend on the 1st di-   */~
                                         /* -- mension of the array    */~
                                         /* -- 1- On Hand only         */~
                                         /* -- 2- On Hand thru Open SO */~
                                         /* -- 3- On Hand thru On Order*/~
                                         /* -- 4- On Hand thru Issued  */~
                                         /* -- 5- On Hand thru In QC   */~
                                         /* -- 6- On Hand thru Pending */~
                                         /* -- 7 & beyond are ignored  */~
                        sw%)             /* = 0- Use all stores        */~
                                         /* = 1- Numeric stores only   */

        dim                                                              ~
            lot$16,                      /* Lot #- may be blank        */~
            partnbr$25,                  /* Part #- required           */~
            plowkey$44,                  /* HNYQUAN PLOW key           */~
            qty(6),                      /* Quantity work area         */~
            quan(1),                     /* Passed quantity(ies)       */~
            rslt$20,                     /* OPENCHCK variable          */~
            store$3                      /* Store #- may be blank      */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.01.02 01/15/92 CMS Patch Release R6.01.02      "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! HNYQUAN  ! Inventory Costs & Quantity Master        *~
            *************************************************************

            select #1, "HNYQUAN", varc, indexed, recsize =  650,         ~
                        keypos =   17, keylen =  44,                     ~
             alt key 1, keypos =    1, keylen =  44

        REM *************************************************************~
            *     S U B R O U T I N E   I N I T I A L I Z A T I O N     *~
            *************************************************************

            if been_here_before% <> 0% then goto L10000
                been_here_before%, f2% = 1%
                call "OPENCHCK" (#1, fs%, f2%, 0%, rslt$)

L10000: REM *************************************************************~
            *       M A I N   P R O C E S S I N G   L O G I C           *~
            *************************************************************

            for i% = 1% to min(6%, dim(quan(), 1))
                quan(i%) = 0
            next i%
            if partnbr$ = " " then goto L65000 /*  Part # is required  */
            if fs% < 0% then goto L65000       /*  File not available  */

        REM Manufacture initial PLOWKEY *********************************
            plowkey$ = xor plowkey$
            str(plowkey$, 1, 25) = partnbr$
            klen% = 25%

            if store$ = " " then goto main_plow_loop
                str(plowkey$, 26, 3) = store$
                klen% = 28%

            if lot$ = " " then goto main_plow_loop
                str(plowkey$, 29, 16) = lot$
                klen% = 44%
                call "READ100" (#1, plowkey$, f1%)
                goto f1_tester

        main_plow_loop
            call "PLOWNEXT" (#1, plowkey$, klen%, f1%)
        f1_tester
            if f1% = 0% then goto L65000  /*  PLOW & Subroutine done  */
            if sw% <> 0% and pos("0123456789" = str(plowkey$,26,1)) = 0% ~
                then goto main_plow_loop
            get #1 using L10300, qty()
L10300:         FMT /* File #1- HNYQUAN */ POS(69), 6*PD(14, 4)
            for i% = 1% to min(6%, dim(quan(), 1))
                quan(i%) = quan(i%) + qty(i%)
            next i%
            goto main_plow_loop

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution.                                     *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            end
