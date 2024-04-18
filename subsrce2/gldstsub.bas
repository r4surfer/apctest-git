        REM *************************************************************~
            *                                                           *~
            *   GGG   L      DDDD    SSS   TTTTT   SSS   U   U  BBBB    *~
            *  G      L      D   D  S        T    S      U   U  B   B   *~
            *  G GGG  L      D   D   SSS     T     SSS   U   U  BBBB    *~
            *  G   G  L      D   D      S    T        S  U   U  B   B   *~
            *   GGG   LLLLL  DDDD    SSS     T     SSS    UUU   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GLDSTSUB - Calculates and returns the accounts and amounts*~
            *            to be distibuted based on the G/L account      *~
            *            distribution table.  Stack is limited to       *~
            *            debits or credits only, call this routine      *~
            *            twice to get both. Note that data is added     *~
            *            to the end of the arrays, via MAXLINES.        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1985, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                 M O D I F I C A T I O N S                 *~
            *---WHEN---+-------------------WHAT-------------------+-WHO-*~
            * 02/02/87 ! ORIGINAL                                 ! HES *~
            *----------+------------------------------------------+-----*~

        sub "GLDSTSUB" (#1,              /* GLDTABLE File UFB          */~
                        #2,              /* GLDTABLE File UFB          */~
                        #3,              /* GLDTABLE File UFB          */~
                        code$,           /* Distribution Table         */~
                        temp,            /* Distribution Amount        */~
                        dcflag$,         /* D = Return Debits          */~
                                         /* C = Return Credits         */~
                        maxlines%,       /* In: Lines Used In Array    */~
                                         /* Out: New Lines Used In Arra*/~
                        acct$(),         /* Returned Accounts          */~
                        descr$(),        /* Returned Descriptions:     */~
                                         /* If less then 10 Elements,  */~
                                         /* Then Array is ignored      */~
                        amt$(),          /* Returned Amounts           */~
                        proj$(),         /* Returned Projects:         */~
                                         /* If less then 10 Elements,  */~
                                         /* Then Array is ignored      */~
                        hits%)           /* Returned Number Of Accounts*/~
                                         /* Added To Stacks. Geater    */~
                                         /* 999% if bad accts encountrd*/

            dim acct$12,                 /* G/L Account Number         */~
                acct$(100)12,            /* Returned Accounts Stack    */~
                amt$(100)10,             /* Returned Amounts Stack     */~
                code$6,                  /* Distribution  Table        */~
                dcflag$1,                /* Type Flag                  */~
                dc$1,                    /* Dsitribution Type          */~
                descr$30,                /* Description                */~
                descr$(100)30,           /* Returned Descriptions      */~
                proj$8,                  /* Project Code               */~
                proj$(100)8,             /* Returned Project Codes     */~
                readkey$16,              /* Work Variable              */~
                temp$10                  /* Work Variable              */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "04.19.00 03/13/87 Serial number, Lot tracking     "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !          D E S C R I P T I O N           *~
            *-----+----------+------------------------------------------*~
            * #1  ! GLDTABLE ! G/L Account Distribution Tables          *~
            * #2  ! GLMAIN   ! GENERAL LEDGER.  SALES ACCT VERIFICATION *~
            * #3  ! JOBMASTR ! WIP/JC MASTER FILE                       *~
            *************************************************************

        REM *************************************************************~
            *            G E N E R A T E   A R R A Y                    *~
            *                                                           *~
            * Based On Distibution table and amouunt to distribute,     *~
            * Create account array.                                     *~
            *************************************************************

            max_accts% = dim(acct$(),1)
            max_descr% = dim(descr$(),1)
            max_proj% = dim(proj$(),1)
            hits% = 0
            damt = temp

            REM First Pass Gets Flat Amounts...
            descr$ = "Auto Dist., Fixed Amount"
            readkey$ = code$
            str(readkey$,7) = all(hex(00))
L10170:     call "PLOWNEXT" (#1, readkey$, 6%, f1%)
                if f1% = 0 then L10310
            get #1, using L10200, acct$, dc$, amount, proj$
L10200:     FMT XX(6), CH(9), XX(30), CH(1), PD(14,4), XX(8), CH(8)
            if dcflag$ <> dc$ then L10170
            call "DESCRIBE" (#2, acct$, " ", 0%, f2%)
                if f2% = 0 then hits% = hits% + 1000%
                if f2% = 0 then L10170
            amount = min(amount, damt)
            gosub add_to_stack
            damt = damt - amount
            if damt <= 0 then end
            goto L10170

L10310:     REM Second Pass Gets Percentage Amounts...
            readkey$ = code$
            str(readkey$,7) = all(hex(00))
L10340:     call "PLOWNEXT" (#1, readkey$, 6%, f1%)
                if f1% = 0 then end
            get #1, using L10370, acct$, dc$, amount, proj$
L10370:     FMT XX(6), CH(9), XX(30), CH(1), XX(8), PD(14,4), CH(8)
            if dcflag$ <> dc$ then L10340
            call "DESCRIBE" (#2, acct$, " ", 0%, f2%)
                if f2% = 0 then hits% = hits% + 1000%
                if f2% = 0 then L10340
            descr$ = "Auto Dist, "
            call "CONVERT" (amount, -0.2, str(descr$,12,5))
            call "CONVERT" (damt, -2.2, temp$)
            descr$ = descr$ & "% of " & temp$
            amount = damt*amount*.01
            gosub add_to_stack
            goto L10340

        add_to_stack
            if amount = 0 then return

            REM Push New Item Onto Stack.
            if maxlines% >= max_accts% then end
            maxlines% = maxlines% + 1%
            hits% = hits% + 1%
            call "GLFMT" (acct$)
            acct$(maxlines%) = acct$
            call "CONVERT" (amount, 2.2, amt$(maxlines%))

            if max_descr% < 10 then L10630
            descr$(maxlines%) = descr$
L10630:     if max_proj% < 10 then return
            if proj$ = " " then return
            call "DESCRIBE" (#3, proj$, " ", 0%, f3%)
                if f3% = 0 then return
            proj$(maxlines%) = proj$
            return


