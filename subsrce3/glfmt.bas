        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   GGG   L      FFFFF  M   M  TTTTT                        *~
            *  G      L      F      MM MM    T                          *~
            *  G GGG  L      FFFF   M M M    T                          *~
            *  G   G  L      F      M   M    T                          *~
            *   GGG   LLLLL  F      M   M    T                          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GLFMT    - FORMAT INTERNAL G/L ACCOUNT FOR DISPLAY/PRINT  *~
            *            SIMILAR TO DATEFMT.                            *~
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
            * 07/09/85 ! ORIGINAL                                 ! KEN *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            sub "GLFMT" (passing$)
*          DIM PASSING$12, WORK$12

            REM Example below unpacks first 3 numbers of compressed acct
            REM into first 6 characters of Formatted account. Then adds
            REM '/' as the seventh position of formated account, followed
            REM by pos 4 - 7 of comp acct concatenated onto the formatted
            REM account as a literal CH(4) in pos 8 - 11

*          IF PASSING$ = " " THEN END
*          WORK$ = " "
*          GET PASSING$, USING 10120, TEMP%, STR(WORK$,8)
*          FMT BI(3), CH(4)
*          CONVERT TEMP% TO STR(WORK$,,6), PIC (000000)
*          STR(WORK$,7,1) = "/"
*          PASSING$ = WORK$
        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "04.15.01 08/20/85 General Ledger & Purchasing    "
        REM *************************************************************
            end
