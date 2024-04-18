        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  JJJJJ  BBBB   M   M  TTTTT  PPPP    OOO    SSS   TTTTT   *~
            *    J    B   B  MM MM    T    P   P  O   O  S        T     *~
            *    J    BBBB   M M M    T    PPPP   O   O   SSS     T     *~
            *  J J    B   B  M   M    T    P      O   O      S    T     *~
            *   J     BBBB   M   M    T    P       OOO    SSS     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBMTPOST - Post material additions and withdrawals to job.*~
            *            Simple posting routine requiring that the user *~
            *            provide cost info for job updating purposes.   *~
            *            Usually this will be direct from HNYQUAN (Ave) *~
            *            & HNYPOST (Actual) but for scraped, etc, the   *~
            *            end user manualy defines the value.            *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/24/83 ! ORIGINAL                                 ! KEN *~
            * 11/01/85 ! ADDED USERID TO CALL                     ! HES *~
            * 01/08/86 ! Keeps MAT, LAB & OH Lined Up If Job Part ! HES *~
            * 06/09/87 ! Std Costing Changes (rewrite).           ! ERN *~
            * 05/15/93 ! Core Project, Original Store, Lot        ! KAB *~
            * 07/17/96 ! Changes for the year 2000.               ! DXL *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~

        REM *************************************************************~
            * ALLOWS ADDITION OR WITHDRAWAL FROM MATERIALS LEDGER ACC0RD-~
            * ING TO QUANTITY (+ OR -). ALL COSTS MUST BE PASSED IN BY   ~
            * CALLING PROGRAM AND SHOULD BE NON-NEGATIVE. THIS ROUTINE   ~
            * DIRECTLY EFFECTS MATERIAL COST IN THE JOB MASTER RECORD AS ~
            * WELL AS OUTPUT OF THE DETAIL RECORD.                       ~
            *************************************************************

        sub "JBMTPOST" (jobnr$,          /* Job Number to update       */~
                        partnr$,         /* Part Number to Post        */~
                        store$,          /* Store number               */~
                        lot$,            /* Lot number                 */~
                        quantity,        /* Quantity moved to job      */~
                        costs_in(),      /* Inventory Costs per Unit   */~
                        ostore$,         /* Original Store number      */~
                        olot$,           /* Original Lot number        */~
                        sysdate$,        /* Posting date passed        */~
                        userid$,         /* Who                        */~
                        coreflag$,       /* Core Flag                  */~
                        #1,              /* UFB Address of JBMASTR2    */~
                        #2,              /* UFB Address of JBMATER2    */~
                        #3,              /* UFB Address of SYSFILE2    */~
                        #4,              /* JBMASTRC Core Appendix     */~
                        returncode%)     /* Error Return Code          */~
                                         /*   0  = Record posted       */~
                                         /*   99 = Record *NOT* posted */~
                                         /*   98 = Job closed          */


        dim                                                              ~
            blankdate$8,                 /* Blank Date for Comparison  */~
            coreflag$4,                  /* Core Flag                  */~
            coremtl(12),                 /* Core Material Value        */~
            costs(12), costs$96,         /* Inventory Costs per QTY    */~
            costs_in(12),                /* Inventory Costs per unit   */~
            jbclosedate$6,               /* Job close date             */~
            jobmtl(12), jobmtl$96,       /* Job Material Costs         */~
            jobnr$8,                     /* Job number                 */~
            lot$6,                       /* Lot                        */~
            ostore$3, olot$6,            /* Original Store/Lot From    */~
            partnr$25,                   /* Part number                */~
            posttext$40,                 /* Posting text               */~
            stds(12), stds$96,           /* Standard Costs             */~
            store$3,                     /* Store                      */~
            sysdate$6,                   /* System date                */~
            systime$8,                   /* System time                */~
            userid$3,                    /* User id                    */~
            zeros$(3)200                 /* Zeroes                     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

        REM *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            returncode% = 99%            /* Set so good resets it.     */

            quantity = round(quantity, 2)
            if quantity = 0 then end       /* Nothing to do */

            call "OPENCHCK" (#1, fo%, fs%,   0%, " ")
            if fs% <> 0 then end      /* No JOB Master file         */
            call "OPENCHCK" (#2, fo%, fs%, 100%, " ") /* JBMATER2   */

            if userid$ = " " then call "EXTRACT" addr("ID", userid$)

            mat costs = costs_in

            init (hex(00)) zeros$()

        REM *************************************************************
*        First update Job Master record with actual material costs
            call "READ101" (#1, jobnr$, f1%)
            if f1% = 0% then end

            get #1 using L10060, jbclosedate$, jobttl, jobmtl()
L10060:         FMT POS(153), CH(6), POS(232), 13*PD(14,4)
            returncode% = 98%   /* Job exists but may be closed */
            if jbclosedate$ <> " " and jbclosedate$ <> blankdate$ ~
                 then end /* JOB CLOSED!!!!! */

            cost = 0
            for b% = 1% to 12%
                costs(b%)  = round(costs(b%) * quantity, 4)
                cost       = cost       + costs(b%)
                jobmtl(b%) = jobmtl(b%) + costs(b%)
            next b%
            jobttl = jobttl + cost
            if coreflag$ = " " then L10230
               goto L10300

L10230:     call "PACKZERO" (jobmtl(), jobmtl$)
            put #1 using L10250, jobttl, jobmtl$
L10250:         FMT POS(232), PD(14,4), CH(96)
            rewrite #1
            goto L10500

L10300:     if fs4% <> 0% then L10320
               call "OPENCHCK" (#4, fs4%, f1%, 100%, " ")
L10320:     call "READ101" (#4, jobnr$, f1%)
               if f1% <> 0% then L10360
                  put #4 using L10350, jobnr$, str(zeros$())
L10350:               FMT CH(8), CH(592)
L10360:        get #4 using L10370, coremtl, coremtl()
L10370:            FMT POS(9), PD(14,4), 12*PD(14,4)
               mat coremtl = coremtl + costs
               call "PACKZERO" (coremtl(), jobmtl$)
               coremtl = coremtl + cost
               put #4 using L10420, coremtl, jobmtl$
L10420:            FMT POS(9), PD(14,4), CH(96)
               if f1% = 0% then write #4 else rewrite #4

L10500
*        Now write materials to the Job Detail Record (JBMATER2)
            systime$ = time
            if quantity > 0 then posttext$="MATERIAL ISSUED TO JOB:"     ~
                            else posttext$="MATERIAL WITHDRAWN FROM JOB:"
            posttext$ = posttext$ & " " & jobnr$
            call "STCCOSTS" (partnr$, " ", #3, 2%, std, stds())
            call "PACKZERO" (stds() , stds$ )
            call "PACKZERO" (costs(), costs$)
            write #2 using L10660,                                        ~
                jobnr$, sysdate$, systime$, partnr$, store$, lot$,       ~
                sysdate$, systime$, quantity, cost, costs$, std, stds$,  ~
                posttext$, userid$, 0, coreflag$, ostore$, olot$, " "

            returncode% = 0%
            end

L10660:         FMT CH(8),               /* Job Number                 */~
                    CH(6),               /* System date                */~
                    CH(8),               /* System time                */~
                    CH(25),              /* Part number                */~
                    CH(3),               /* Store number               */~
                    CH(6),               /* Lot number                 */~
                    CH(6),               /* System date                */~
                    CH(8),               /* System time                */~
                    PD(14,4),            /* Quantity moved to/from job */~
                    PD(14,4),            /* Total Inventory Cost       */~
                    CH(96),              /* Inventory Cost Breakdown   */~
                    PD(14,4),            /* Total Standard Cost        */~
                    CH(96),              /* Standard Cost Breakdown    */~
                    CH(40),              /* Posting text               */~
                    CH(3),               /* User ID                    */~
                    PD(14,4),            /* Qty withdrawn              */~
                    CH(4),               /* Core Flag                  */~
                    CH(3),               /* Original Store             */~
                    CH(6),               /* Original Lot               */~
                    CH(50)               /* Filler                     */

*        Quantity withdrawn is a record of movement of this component
*        out of this Job.  It is honored in the materials movement
*        routines.

