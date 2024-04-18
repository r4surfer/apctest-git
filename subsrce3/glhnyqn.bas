        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   GGG   L      H   H  N   N  Y   Y   QQQ   N   N          *~
            *  G      L      H   H  NN  N  Y   Y  Q   Q  NN  N          *~
            *  G GGG  L      HHHHH  N N N   YYY   Q   Q  N N N          *~
            *  G   G  L      H   H  N  NN    Y    Q Q Q  N  NN          *~
            *   GGG   LLLLL  H   H  N   N    Y     QQQ   N   N          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GLHNYQN  - USER CONTROL OF GL ACCOUNTS IN HNYQUAN FILE.   *~
            *            USE THIS ROUTINE TO CONTROL HOW STORE NUMBER,  *~
            *            OR OTHER (???) MIGHT INFLUENCE GL ACCOUNTS     *~
            *            STUFFED INTO HNYQUAN, RATHER THAN JUST TAKING  *~
            *            HNYMASTR DEFAULTS.                             *~
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
            * 02/25/86 ! ORIGINAL                                 ! KAB *~
            * 04/09/87 ! Standard Costs Project Modifications     ! ERN *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            sub "GLHNYQN" (part$,        /* PART CODE                  */~
                           store$,       /* STORE NUMBER (CODE)        */~
                           lot$,         /* LOT OR BATCH               */~
                           cat$,         /* CATEGORY CODE              */~
                           costtype$,    /* COSTING METHOD             */~
                           acct$())      /* GL ACCOUNTS FROM HNYMASTR  */~

            dim                                                          ~
                acct$(19)9,              /* GL ACCOUNTS FROM HNYMASTR  */~
                cat$4,                   /* CATEGORY CODE              */~
                costtype$1,              /* COSTING METHOD             */~
                lot$6,                   /* LOT OR BATCH               */~
                part$25,                 /* PART CODE                  */~
                store$3                  /* STORE NUMBER (CODE)        */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto   L01082
            cms2v$ = "05.00.00 09/08/87 Standard cost to 12             "
L01082: REM *************************************************************
            goto L65000                   /* ALL TURNED OFF             */~

        REM HERE IS AN EXAMPLE OF WHAT MIGHT BE DONE
        /*
            FOR I% = 1% TO DIM(ACCT$(),1)
                IF ACCT$(I%) = " " THEN 10110
                   TEMP$ = ACCT$(I%)
                   CALL "GLFMT" (TEMP$)
                      TEMP$ = TEMP$ & "-" & STORE$
                   CALL "GLUNFMT" (TEMP$)
                   ACCT$(I%) = TEMP$
            NEXT I%

        REM BASED ON ????? WE COULD EVEN MODIFY COST METHOD HERE

            IF POS("RXASFLMPYT"=STR(LOT$,1,1)) = 0% THEN 65000
               COSTTYPE$ = STR(LOT$,1,1)
        */

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

