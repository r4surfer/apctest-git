        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   SSS   EEEEE  RRRR   EEEEE  N   N   AAA   BBBB   L       *~
            *  S      E      R   R  E      NN  N  A   A  B   B  L       *~
            *   SSS   EEEE   RRRR   EEEE   N N N  AAAAA  BBBB   L       *~
            *      S  E      R  R   E      N  NN  A   A  B   B  L       *~
            *   SSS   EEEEE  R   R  EEEEE  N   N  A   A  BBBB   LLLLL   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SERENABL - Set Serial Number field Enable/Disable flag    *~
            *            for the passed in Part Number.  Reads the      *~
            *            HNYMASTR record for the passed in part to      *~
            *            determine if Serial Tracking is in effect for  *~
            *            the part.  If yes then ENABLED% is set to 1    *~
            *            else it's set to 0.  Also passes back the      *~
            *            maximum Serial # field length.                 *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/09/87 ! Original                                 ! LDJ *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "SERENABL" (part$,           /* Part Number to Check       */~
                        enabled%,        /* Enable Flag to Set         */~
                        length%,         /* Maximum S/N Field Length   */~
                        #1,              /* SYSFILE2 UFB               */~
                        #2)              /* HNYMASTR UFB               */

        dim                                                              ~
            format$20,                   /* Returned S/N Format        */~
            lastpart$25,                 /* Last Part Number Processed */~
            max$2,                       /* Maximum Lot Length         */~
            part$,                       /* Part Number for Lot        */~
            readkey$20,                  /* File Read Key Variable     */~
            sertrack$1                   /* Lot Tracking Flag          */~

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto   L09052
            cms2v$ = "04.19.00 03/13/87 Serial number, Lot tracking    "
L09052: REM *************************************************************
            if readkey$ > " " then L09230
                readkey$ = "SWITCHS.HNY"
                format$ = all("#")
                call "READ100" (#1, readkey$, f1%)
                if f1% = 0% then L09120
                get #1 using L09110, max$, format$
L09110:         FMT POS(42), CH(2), POS(46), CH(20)
L09120:         max% = 6% : convert max$ to max%, data goto L09230

L09230:     REM *** Retrieve Part Specific Flags From HNYMASTR ***
                if part$ = lastpart$ then L10000
                lastpart$ = part$
                sertrack$ = "N"          /* Default          */
                REM *** Check to See if we can save a read ***
                if fs(#2) <"10" and part$ <> " " and part$ = key(#2) then~
                   L09290
                call "READ100" (#2, lastpart$, f1%)
                if f1% = 0% then L10000
L09290:         get #2 using L09300, sertrack$
L09300:         FMT POS(131), CH(1)

L10000: REM *************************************************************~
            *          M A I N   R O U T I N E   B E G I N S            *~
            *-----------------------------------------------------------*~
            *   If S/N Track = NO then ENABLED% = 0% Else ENABLED% = 1%.*~
            *************************************************************

            if sertrack$ = "Y" then enabled% = 1% else enabled% = 0%
            length% = max%

        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            end
