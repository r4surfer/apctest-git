        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  PPPP   IIIII  PPPP   IIIII  N   N   OOO   PPPP           *~
            *  P   P    I    P   P    I    NN  N  O   O  P   P          *~
            *  PPPP     I    PPPP     I    N N N  O   O  PPPP           *~
            *  P        I    P        I    N  NN  O   O  P              *~
            *  P      IIIII  P      IIIII  N   N   OOO   P              *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PIPINOP  - RENAME EXISTING PIPIN FILE TO XPIPIN AND CREATE*~
            *            NEW PIPIN ON THE DEFAULT OUTVOL WITH SUFFICIENT*~
            *            NUMBER OF RECORDS ALLOCATED                    *~
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
            * 05/12/89 ! Removed output library from argument list! MJB *~
            *          !  of RENAME for OS720 compatibility       !     *~
            * 11/11/93 ! Purchase Jobs - Added re-naming of       ! JBK *~
            *          !  POPIPXRF to XPOPIPXR and creation of new!     *~
            *          !  POPIPXRF in same manner as PIPIN.       !     *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        dim file$8, xfile$8, inlib$8, invol$6, outvol$6, rslt$20, axd$4

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "
        REM *************************************************************
        select #1, "PIPIN", varc, indexed, recsize = 60,                 ~
                   keypos = 30, keylen = 19,                             ~
                   alt key 1, keypos = 1, keylen = 48

        select #2, "POPIPXRF", varc, indexed, recsize = 100,             ~
                   keypos = 1, keylen = 58

        f2% = 1% : call "OPENFILE" (#1, "INPUT", f2%, rslt$, axd$)

        if f2% <> 0 then L00950

        call "GETNAMES" addr(#1, file$, inlib$, invol$)

        call "EXTRACT" addr("OV", outvol$)

        close #1

        call "READFDR" addr(file$, inlib$, invol$, 0%, "RC", count%, err%)
           if err% <> 0 then L00950
        count% = max(count%, 100%)
        xfile$ = "X" & str(file$,1,7)

        /*  CALL "PUTPARM" ADDR("E",                                     ~
                              "FILE    ",         5%,                    ~
                              "OFILE   ", FILE$ , 8%,                    ~
                              "OLIBRARY", INLIB$, 8%,                    ~
                              "OVOLUME ", INVOL$, 6%,                    ~
                              "NFILE   ", XFILE$, 8%,                    ~
                              "NLIBRARY", INLIB$, 8%,                    ~
                              "@", "REN001  ", ERR%)

            CALL "PUTPARM" ADDR("E",                                     ~
                              "FILE    ",         0%,                    ~
                              "P", "REN002  ", ERR%)


            CALL "LINK"   ADDR("RENAME  ", ERR1%, ERR%)
               IF ERR1%<> 0 THEN 65000
               IF ERR% <> 0 THEN 65000
        */
            call "RENAME" addr("F", file$, inlib$, invol$, xfile$,       ~
                               "B", "L", " ", err%)
               if err% <> 0 then L00950

        open nogetparm #1, output, space = count%,                       ~
                    dpack = 100, ipack = 100,                            ~
                    file = file$, library = inlib$, volume = outvol$

        close #1

*        Now do the same thing for POPIPXRF
L00950: init (" ")  file$, xfile$, inlib$, invol$, outvol$, rslt$, axd$

        f2% = 1% : call "OPENFILE" (#2, "INPUT", f2%, rslt$, axd$)

        if f2% <> 0 then L65000

        call "GETNAMES" addr(#2, file$, inlib$, invol$)

        call "EXTRACT" addr("OV", outvol$)

        close #2

        call "READFDR" addr(file$, inlib$, invol$, 0%, "RC", count%, err%)
           if err% <> 0 then L65000
        count% = max(count%, 100%)
        xfile$ = "X" & str(file$,1,7)

            call "RENAME" addr("F", file$, inlib$, invol$, xfile$,       ~
                               "B", "L", " ", err%)
               if err% <> 0 then L65000

        open nogetparm #2, output, space = count%,                       ~
                    dpack = 100, ipack = 100,                            ~
                    file = file$, library = inlib$, volume = outvol$

        close #2

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
