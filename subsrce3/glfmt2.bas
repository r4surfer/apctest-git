        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   GGG   L      FFFFF  M   M  TTTTT   222                  *~
            *  G      L      F      MM MM    T    2   2                 *~
            *  G GGG  L      FFFF   M M M    T       2                  *~
            *  G   G  L      F      M   M    T      2                   *~
            *   GGG   LLLLL  F      M   M    T    22222                 *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GLFMT2   - FORMAT INTERNAL G/L ACCOUNT FOR DISPLAY/PRINT  *~
            *            SIMILAR TO DATEFMT. This is the 'Local Author- *~
            *            ity' version of it's parent GLFMT.             *~
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
            * 08/25/87 ! W.R. Grace accountancy (dual books) mods.! JIM *~
            *          !   (An absolute clone of GLFMT.)          !     *~
            * 04/20/89 ! REM out Caelus test format               ! GGO *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            sub "GLFMT2" (passing$)
*          DIM PASSING$12, WORK$12

            REM Example below is for Caelus internal testing.  If the
            REM account is in the format XX.X.XXXX it will be left alone.
            REM Otherwise it will be transformed from whatever it is
            REM (A12345678) to (A*12345678).  This will allow programmers
            REM to perform testing on their code.

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto  L10812
            cms2v$ = "R6.00.00 01/19/90 CMS2 / CMS-I Merge              "
L10812: REM *************************************************************
*          IF PASSING$ = " " THEN END
*          IF LEN(PASSING$) <> 9% THEN 11000
*             IF STR(PASSING$,3,1) <> "." THEN 11000
*             IF STR(PASSING$,5,1)  = "." THEN END
*          WORK$ = STR(PASSING$,1,1) & "*" & STR(PASSING$,2)
*          PASSING$ = WORK$
            end
