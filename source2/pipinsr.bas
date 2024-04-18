        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  PPPP   IIIII  PPPP   IIIII  N   N   SSS   RRRR           *~
            *  P   P    I    P   P    I    NN  N  S      R   R          *~
            *  PPPP     I    PPPP     I    N N N   SSS   RRRR           *~
            *  P        I    P        I    N  NN      S  R   R          *~
            *  P      IIIII  P      IIIII  N   N   SSS   R   R          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PIPINSR  - SCRATCHES XPIPIN IF IT EXISTS                  *~
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
            * 12/17/84 ! ORIGINAL                                 ! KEN *~
            * 11/11/93 ! Purchase Jobs - Added Scratch of XPOPIPXR! JBK *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        dim file$8, inlib$8, invol$6, rslt$20, axd$4

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "
        REM *************************************************************
        select #1, "XPIPIN", varc, indexed, recsize = 60,                ~
                   keypos = 30, keylen = 19,                             ~
                   alt key 1, keypos = 1, keylen = 48

        select #2, "XPOPIPXR", varc, indexed, recsize = 100,             ~
                   keypos = 1, keylen = 58

        f2% = 1% : call "OPENFILE" (#1, "INPUT", f2%, rslt$, axd$)

        if f2% <> 0% then L00700

        call "GETNAMES" addr(#1, file$, inlib$, invol$)

        close #1

        call "SCRATCH" addr("F", file$, inlib$, invol$, "B", " ", err%)
             if err% <> 0% then L00700

*        Now scratch the XPOPIPXR file
L00700: init (" ")  file$, inlib$, invol$, rslt$, axd$

        f2% = 1% : call "OPENFILE" (#2, "INPUT", f2%, rslt$, axd$)

        if f2% <> 0 then L65000

        call "GETNAMES" addr(#2, file$, inlib$, invol$)

        close #2

        call "SCRATCH" addr("F", file$, inlib$, invol$, "B", " ", err%)
             if err% <> 0% then L65000

L65000: REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            end
