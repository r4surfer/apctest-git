        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  U   U  PPPP   DDDD   DDDD    OOO   N   N  EEEEE          *~
            *  U   U  P   P  D   D  D   D  O   O  NN  N  E              *~
            *  U   U  PPPP   D   D  D   D  O   O  N N N  EEEE           *~
            *  U   U  P      D   D  D   D  O   O  N  NN  E              *~
            *   UUU   P      DDDD   DDDD    OOO   N   N  EEEEE          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * UPDDONE  - REMOVES IN-PROCESS RECORD FROM 'UPDCTL'.       *~
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
            * 04/06/84 ! ORIGINAL                                 ! ERN *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        dim savenrs(5),                  /* RESTART SAVE NUMBERS       */~
            savestrs$(5)50,              /* RESTART SAVE STRINGS       */~
            userid$3                     /* USER ID                    */

        dim f2%(64)                      /* FILE STATUS FLAGS FOR      */

            mat f2% = con : cms2v$ = 'R41209         ' : cms2v$ = ' '


        REM *************************************************************~
            *   S E L E C T   S T A T E M E N T S                       *~
            *************************************************************

            select #1, "UPDCTL",                                         ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 500,                                    ~
                       keypos = 1,  keylen = 3


        REM *************************************************************~
            *        M A I N   P R O G R A M   L O G I C                *~
            *************************************************************

            call "UPDSUB" ("D", #1, f2%(1), " "      , "        ", " ",  ~
                           0%, 0%, " ", savenrs(), savestrs$(), userid$, ~
                           return%)
                 return% = return%


        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
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


            for u3% = 1 to 64
                if f2%(u3%) = 0 then close # u3%
                next u3%
            end
