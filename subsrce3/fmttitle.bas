        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  FFFFF  M   M  TTTTT  TTTTT  IIIII  TTTTT  L      EEEEE   *~
            *  F      MM MM    T      T      I      T    L      E       *~
            *  FFFF   M M M    T      T      I      T    L      EEEE    *~
            *  F      M   M    T      T      I      T    L      E       *~
            *  F      M   M    T      T    IIIII    T    LLLLL  EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * FMTTITLE - FORMATS TITLE STRING FOR PRINT/DISPLAY         *~
            *            SINGLE/DOUBLE SPACE                            *~
            *            RIGHT/LEFT/CENTER JUSTIFICATION                *~
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
            * 07/09/85 ! ORIGINAL                                 ! KAB *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "FMTTITLE" (base$, add$, function%)

            dim work$200

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "04.15.01 08/20/85 General Ledger & Purchasing    "
        REM *************************************************************
            l% = len(base$)
               if base$ = " " then l% = 0%
            t% = len(add$)
               if add$ = " " then t% = 0%
            s% = len(str(base$))

            if l% > 0% then L00650
               if t% = 0% then end
                  base$ = add$
                     t% = 0%
                     l% = len(base$)
L00650:     f1% = 0% : if int(function%/10%) > 0% then f1% = 1%
            f2% = mod(function%, 10%)

            if t% < 1% then L00750
            if l% + t% + 1% > s% then L00750
               base$ = str(base$,1,l%) & " " & add$
               l% = l% + t% + 1%

L00750:     init (" ") work$
            if f1% > 0% then L00800
L00770:        work$ = base$
               goto L00900

L00800:     if 2%*l% - 1% > s% then L00770
               for i% = 1% to l%
                   str(work$, 2%*i% - 1%, 1%) = str(base$,i%,1%)
               next i%
            l% = 2%*l% - 1%

L00900:     init (" ") base$
            if l% = s% then L00950

            on f2% goto L01000, L01050

L00950: REM LEFT JUSTIFY
            t% = 1%
            goto L01100

L01000: REM RIGHT JUSTIFY

            t% = s%-l%+1%
            goto L01100

L01050: REM CENTER JUSTIFY

            t% = int((s%-l%)/2%) + 1%

L01100: REM FINISH AND END

            str(base$,t%,l%) = str(work$,1%,l%)
            end
