        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  L       OOO   TTTTT  EEEEE  N   N   AAA   BBBB   L       *~
            *  L      O   O    T    E      NN  N  A   A  B   B  L       *~
            *  L      O   O    T    EEEE   N N N  AAAAA  BBBB   L       *~
            *  L      O   O    T    E      N  NN  A   A  B   B  L       *~
            *  LLLLL   OOO     T    EEEEE  N   N  A   A  BBBB   LLLLL   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * LOTENABL - Set Lot Number field Enable/Disable flag per   *~
            *            system and part record settings as follows--   *~
            *                                                           *~
            *            SYSTEM  PART  ENABLED   COMMENT                *~
            *            ------  ----  -------   ---------------------  *~
            *              N       N      0%     Field Disabled         *~
            *              Y       N      0%     Field Disabled         *~
            *              N       Y      1%     Enabled, Memo Entry    *~
            *              Y       Y      2%     Enabled, Validated     *~
            *                                                           *~
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

        sub "LOTENABL" (part$,           /* Part Number to Check       */~
                        enabled%,        /* Enable Flag (See above)    */~
                        length%,         /* Maximum Lot Field Length   */~
                        #1,              /* SYSFILE2 UFB               */~
                        #2)              /* HNYMASTR UFB               */


        dim                                                              ~
            lastpart$25,                 /* Last Part Number Processed */~
            lottrack$1,                  /* Lot Tracking Flag          */~
            max$2,                       /* Maximum Lot Length         */~
            part$,                       /* Part Number for Lot        */~
            readkey$20,                  /* File Read Key Variable     */~
            sys$1                        /* System Level enable flag   */


        REM *************************************************************~
            *           S U B R O U T I N E    L O G I C                *~
            *-----------------------------------------------------------*~
            * Does what requested to do.                                *~
            *************************************************************
        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto  L10052
            cms2v$ = "04.19.01 03/30/87 Patch release                   "
L10052: REM *************************************************************
        if readkey$ > " " then L10120
            readkey$ = "SWITCHS.HNY" : sys$ = "N" : max% = 6%
            call "READ100" (#1, readkey$, f1%)
            if f1% = 0% then L10120
                get #1 using L10100, max$, sys$
L10100:              FMT POS(21), CH(2), POS(92), CH(1)
                convert max$ to max%, data goto L10120
L10120:         length%  = max(1, max%)
                if sys$ <> "Y" then sys$ = "N"

*       ** Retrieve Part Specific Flag From HNYMASTR ***
            if part$ = lastpart$ then L10280
                lastpart$ = part$
                lottrack$ = "N"     /* Default */
                if fs(#2) < "10" and part$ <> " " and part$ = key(#2)    ~
                                                             then L10240
                     call "READ100" (#2, lastpart$, f1%)
                     if f1% = 0% then L10280
L10240:         get #2 using L10250, lottrack$
L10250:              FMT POS(130), CH(1)
                if lottrack$ <> "Y" then lottrack$ = "N"

L10280:     enabled% = 0% : if lottrack$ = "N" then end
                if sys$ = "N" then enabled% = 1% else enabled% = 2%
                end
