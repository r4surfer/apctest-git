        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   OOO   PPPP   EEEEE  N   N   CCC   H   H   CCC   K   K   *~
            *  O   O  P   P  E      NN  N  C   C  H   H  C   C  K  K    *~
            *  O   O  PPPP   EEEE   N N N  C      HHHHH  C      KKK     *~
            *  O   O  P      E      N  NN  C   C  H   H  C   C  K  K    *~
            *   OOO   P      EEEEE  N   N   CCC   H   H   CCC   K   K   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * OPENCHCK - CHECK FILE STATUS DIRECTLY FROM UFB.  IT WILL  *~
            *            RETURN IF THE FILE IS OPEN, TRY TO OPEN IF     *~
            *            POSSIBLE, AND CREATE IF SO FLAGGED.  THIS IS   *~
            *            FOR INDEXED FILES TO BE OPENED IN SHARED MODE. *~
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
            * 11/27/85 ! ORIGINAL                                 ! KAB *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            sub "OPENCHCK" (#1,           /* UFB ADDRESS               */~
                            filestatus%,  /* NORMALLY 0% ON FIRST PASS */~
                                          /*   RETURNS 1% IF FILE OK   */~
                                          /*      -1% IF FILE NOT AVAIL*/~
                            f2%,          /* THE USUAL F2%             */~
                            create%,      /* CREATE FLAG -             */~
                                          /*      0% = DONT CREATE     */~
                                          /*  > 100% = NUMBER OF RECS  */~
                            userrslt$)    /* OPTIONAL - USER ACCESSABLE*/~
                                          /*        RESULT             */~

        dim axd$4, rslt$20, userrslt$20

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "04.15.04 12/03/85 Rescheduling & text mgmt.      "
        REM *************************************************************
            rslt$ = userrslt$
            call "GETUFBS1" addr(#1, filestatus%)
                 if filestatus% = 1% then open_exit

            f2% = 1%
            call "OPENFILE" (#1, "SHARE", f2%, rslt$, axd$)
               if f2% = 0% then open_exit

            if create% = 0% then error_exit

            rslt$ = userrslt$
            if create% < 101% then L03400

               rslt$ = "OUTPUT00000100050050"
               convert create% to str(rslt$,7,8), pic(00000000)

L03400:     call "OPENFILE" (#1, "OUTPT", f2%, rslt$, axd$)
               close #1
            call "OPENFILE" (#1, "SHARE", f2%, rslt$, axd$)

        open_exit
            f2% = 0%:filestatus% = 1%:goto L65000
        error_exit
            filestatus% = -1%:goto L65000

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

            userrslt$ = rslt$
            end
