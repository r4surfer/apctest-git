        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   GGG   L      U   U  N   N  FFFFF  M   M  TTTTT          *~
            *  G      L      U   U  NN  N  F      MM MM    T            *~
            *  G GGG  L      U   U  N N N  FFFF   M M M    T            *~
            *  G   G  L      U   U  N  NN  F      M   M    T            *~
            *   GGG   LLLLL   UUU   N   N  F      M   M    T            *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GLUNFMT  - UNFORMAT EXTERNAL G/L ACCOUNT FOR STORAGE      *~
            *            VERY SIMILAR TO DATUNFMT                       *~
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

            sub "GLUNFMT" (passing$)
*          DIM PASSING$12, WORK$12

            REM Example below packs first 6 numbers of formated account
            REM into first three characters of compressed account. Then
            REM ignoring the seventh position of formated account, pos.
            REM eight - eleven is concatenated onto the compressed
            REM account as a literal CH(4)

*          IF PASSING$ = " " THEN END
*          WORK$ = " "
*          CONVERT STR(PASSING$,,6) TO TEMP%, DATA GOTO 10140
*          PUT WORK$, USING 10130, TEMP%, STR(PASSING$,8)
*          FMT BI(3), CH(4)
*          PASSING$ = WORK$
        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "04.15.01 08/20/85 General Ledger & Purchasing    "
        REM *************************************************************
            end
