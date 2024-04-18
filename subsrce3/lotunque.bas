        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  L       OOO   TTTTT  U   U  N   N   QQQ   U   U  EEEEE   *~
            *  L      O   O    T    U   U  NN  N  Q   Q  U   U  E       *~
            *  L      O   O    T    U   U  N N N  Q   Q  U   U  EEEE    *~
            *  L      O   O    T    U   U  N  NN  Q Q Q  U   U  E       *~
            *  LLLLL   OOO     T     UUU   N   N   QQQ    UUU   EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * LOTUNQUE - Tests that lot number entered is unique within *~
            *            arrays passed in. '1-Dimensional' version.     *~
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
            * 02/11/87 ! Original                                 ! ERN *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "LOTUNQUE" (part$(), lot$(), c%, #1, errormsg$)

*        PART$() is the list of part numbers (1 dimension).
*        LOT$ is the corresponding list of lots.
*
*        C% is the element that is to be checked.
*
*        Pass in ERROMSG$ as "IGNORE" to check regardless of lot
*          track flag settings.

        dim                                                              ~
            errormsg$79,                 /* Error Message              */~
            lot$(2)6,                    /* Lot Number Array           */~
            lotflag$1,                   /* Lot Track Flag             */~
            part$(2)25,                  /* Part Number Array          */~
            p%(100),                     /* SEARCH receiver            */~
            readkey$50,                  /* A Read Key                 */~
            unique$1                     /* Uniqueness Flag            */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "04.19.00 03/13/87 Serial number, Lot tracking     "
        REM *************************************************************

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            parts% = len(str(part$())) / dim(part$(),1)
            lots%  = len(str(lot$ ())) / dim(lot$ (),1)

            if readkey$ <> " " then L09230
                lotflag$, unique$ = "N"
                readkey$ = "SWITCHS.HNY"
                call "READ100" (#1, readkey$, f1%)
                if f1% = 0% then L09230
                     get #1 using L09200, lotflag$, unique$
L09200:                   FMT POS(92), 2*CH(1)
                     if lotflag$ = "N" then unique$ = "N"

L09230:     if errormsg$ = "IGNORE" or unique$ = "U" then L10000
                errormsg$ = " "
                goto exit_program

L10000: REM *************************************************************~
            *             M A I N   L O G I C                           *~
            * --------------------------------------------------------- *~
            * Check this lot out.                                       *~
            *************************************************************

            errormsg$ = " "
            mat p% = zer

            lot%  = (c% * lots%) - (lots% - 1%)
                if str(lot$(), lot%, 6%) = " " then exit_program
            part% = (c% * parts%) - (parts% - 1%)

            search str(lot$()) = str(lot$(),lot%,6) to p%() step lots%

            for h% = 1% to dim(p%(),1)
                if p%(h%) = 0% then exit_program
                base% = (p%(h%) + lots% - 1%) / lots%
                test% = (base% * parts%) - (parts% - 1%)
                if str(part$(), test%,25%) <>                            ~
                   str(part$(), part%,25%) then error_exit
            next h%
            goto exit_program

        error_exit
            errormsg$ = "Lot Number already exists for Part " &          ~
                                                str(part$(), test%, 25%)

        exit_program
            end

