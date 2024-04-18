        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  JJJJJ  BBBB    CCC   RRRR   PPPP    OOO    SSS   TTTTT   *~
            *    J    B   B  C   C  R   R  P   P  O   O  S        T     *~
            *    J    BBBB   C      RRRR   PPPP   O   O   SSS     T     *~
            *  J J    B   B  C   C  R   R  P      O   O      S    T     *~
            *   J     BBBB    CCC   R   R  P       OOO    SSS     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBCRPOST - Post Credits to Job for material completed     *~
            *            and removed from the job.                      *~
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
            * 06/09/87 ! Standard Costing Changes (rewrite).      ! ERN *~
            * 03/10/93 ! Ability to cost at Standard or Zero.     ! KAB *~
            *          !   Separate Costing Flags for BOM, TL.    !     *~
            * 05/15/93 ! Core Project                             ! KAB *~
            *          !   Flag Transaction if costs Negative.    !     *~
            *          !    Controlled by JBFLAGS option          !     *~
            * 08/20/93 ! Purchase Jobs - allow change to credit   ! KAB *~
            *          !   (JBCREDIT) to be + or -.               !     *~
            *          !                                          !     *~
            *          !   Subrtracting from existing credit re-  !     *~
            *          !   duces the effective quantity of the    !     *~
            *          !   credit & the unit value is returned    !     *~
            *          !   in COSTS().  The Job Credit Ledger is  !     *~
            *          !   reduced proportionally.                !     *~
            *          !                                          !     *~
            *          !   Adding to an existing credit will in-  !     *~
            *          !   crease the effective quantity (dec-    !     *~
            *          !   creasing or possibly zeroing prior     !     *~
            *          !   reversals in the process). As above,   !     *~
            *          !   the unit value is returned in COSTS()  !     *~
            *          !   and the Job Credit Ledge is increased  !     *~
            *          !   proportionally.                        !     *~
            *          !                                          !     *~
            *          ! * * * Warnings * * *                     !     *~
            *          ! A Credit Transaction Cannot have a neg-  !     *~
            *          ! tive effective quantity.                 !     *~
            *          ! You may adjust downward to zero, but no  !     *~
            *          ! lower. Attempts will fail (Bad Return).  !     *~
            *          ! If a Transation reaches zero, it has no  !     *~
            *          ! extended costs, so attempting to adjust  !     *~
            *          ! upward will be treated as a whole new    !     *~
            *          ! completion.  A new JBCREDIT key will be  !     *~
            *          ! returned.  Watch for it and hang on if   !     *~
            *          ! needed later.                            !     *~
            *          !                                          !     *~
            * 05/31/95 ! Added support for completion method 'B'. ! JDH *~
            *          !   Referenced on disk as 'M'.             !     *~
            * 07/16/96 ! Changes for the year 2000.               ! DXL *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~

*       ****************************************************************
*        Removes finished products from Job -or- returns finished pro- *
*        ducts for reversal of a previously sumbitted completion.      *
*        Value is calculated per the Cost Type indicated.  The routine *
*        affects the Credit Accumulators and the Quantity Completed in *
*        the Job Master (JBMASTR2).                                    *
*       ****************************************************************


        sub "JBCRPOST" (jobnr$,          /* Job to be updated          */~
                        costflg$,        /* P=Prorate, B=Back Flush    */~
                        comperrcst$,     /* Error Handler Flag         */~
                        part$,           /* Part number to post        */~
                        store$,          /* Store number               */~
                        lot$,            /* Lot number                 */~
                        quantity,        /* Quantity Completed         */~
                        revkey$,         /* JBCREDIT key for reversal  */~
                        costs(),         /* Inventory Costs per unit   */~
                        coreadj(),       /* Core Adjustment (Material) */~
                        corefg(),        /* Core in WIP                */~
                        wiptotal,        /* Total Posted to Job        */~
                        jobtonr$,        /* Transfer to Job Number     */~
                        postdate$,       /* Posting date CH(6)         */~
                        userid$,         /* Who                        */~
                        #1,              /* UFB address of JBMASTR2    */~
                        #2,              /* UFB address of JBCREDIT    */~
                        #3,              /* UFB address of SYSFILE2    */~
                        #4,              /* UFB address of JBCROSS2    */~
                        #5,              /* UFB address of JBMATER2    */~
                        #6,              /* UFB address of HNYMASTR    */~
                        returncode%)     /* Error Return Code          */~
                                         /*   0  = RECORD POSTED       */~
                                         /*   99 = RECORD *NOT* POSTED */~
                                         /*   98 = JOB CLOSED          */~
                                         /*   97 = QTY NOT CREDITED    */

*         A check is made to be sure removal or replacement of parts
*         does not exceed original job parameters.  This is to avoid
*         accidentally falling into a negative cost situation.

        dim                                                              ~
            blankdate$8,                 /* Blank Date for Comparison  */~
            closed$6,                    /* Job close date             */~
            costflag$2, costflg$2,       /* How to determine costs     */~
            costs(12),                   /* Inventory Costs            */~
            coreadj(12), corefg(12),     /* Core Adjustment            */~
            crbom(12), crbom$96,         /* JBCREDIT- Rollup- BOM      */~
            crtl(12), crtl$96,           /*                   This Lvl */~
            crfold(12), crfold$96,       /* JBCREDIT- Foldin           */~
            errormsg$79,                 /* Error Message              */~
            jbcrbom(12), jbcrbom$96,     /* JBMASTR2- Credits (Rollup) */~
            jbcrtl(12),  jbcrtl$96,      /*                            */~
            jbcrfold(12), jbcrfold$96,   /* JBMASTR2- Credits (Foldin) */~
            jbbom(12), jbtl(12),         /* JBMASTR2- Actual BOM and   */~
                       jbtl2(12),        /*           this Level Costs */~
            jobnr$8,                     /* Job Number                 */~
            jobtonr$8,                   /* To Job Number              */~
            lot$6,                       /* Lot                        */~
            part$25,                     /* Part Number                */~
            plstdate$6,                  /* Job Planned Start Date     */~
            postdate$6,                  /* Posting date               */~
            posttext$40,                 /* Posting text               */~
            revkey$22,                   /* JBCREDIT key to reverse    */~
            store$3,                     /* Store                      */~
            systime$8,                   /* System time                */~
            userid$3                     /* Who to Blame               */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

        REM *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            returncode% = 99%            /* SET SO GOOD RESETS IT.     */

            costflag$ = costflg$
            if pos("BPSZM" = str(costflag$,1%,1%)) = 0% then end
            if pos("BPSZ" = str(costflag$,2%,1%)) = 0% then              ~
                            str(costflag$,2%,1%) = str(costflag$,1%,1%)
            if str(costflag$,2%,1%) = "M" then end /* Shouldn't be */

            quantity = round(quantity, 2)
            if quantity = 0 then end

            call "OPENCHCK" (#1, 0%, f2%, 0%, " ")
            if f2% <> 0% then end        /* No JBMASTR2 */
            call "OPENCHCK" (#2, 0%, f2%, 100%, " ")  /* JBCREDIT */

            if userid$ = " " then call "EXTRACT" addr("ID", userid$)

        REM *************************************************************
            call "READ100" (#1, jobnr$, f1%)  /* JBMASTR2 */
            if f1% = 0% then end

            get #1 using L10070, jbqty, jbqtycomp, closed$, plstdate$,    ~
                                jbbom(), jbtl(), jbtl2(),                ~
                                jbcrtotal, jbcrbom(), jbcrtl(),jbcrfold()
L10070:         FMT  POS(83), 2*PD(14,4), POS(153), CH(6), POS(168),     ~
                     CH(6), POS(240), 36*PD(14,4), 37*PD(14,4)

            call "PIPINDEX" (#3, plstdate$, stdate%, returncode%)
            returncode% = 98%    /* Job exists but may be closed */
            if closed$ <> " " and closed$ <> blankdate$ ~
                              then end        /* JOB CLOSED!!!!! */

            mat jbtl  = jbtl + jbtl2
            mat jbtl2 = zer
            wiptotal, oldcrtotal = 0 : oldjbcrtotal = jbcrtotal
            jbcrbom_set% = 0% : jbcrtl_set% = 0% : jbcrfold_set% = 0%

            returncode% = 97%    /* Something may still go wrong */

*        Test QUANTITY.  New complete cannot be less than zero or
*        greater than quantity to build.
            qltb = jbqty - jbqtycomp             /* Qty left to build  */
            jbqtycomp = jbqtycomp + quantity
            if jbqtycomp > jbqty or jbqtycomp < 0 then end

            if revkey$ <> " " then L11000
            if quantity > 0 then regular_completion

L11000
*       ** QUANTITY < 0 (reversal).  Get costs from reversal record.
            call "READ101" (#2, revkey$, f1%)
            if f1% <> 0% then L11040
L11025:        if quantity > 0 then regular_completion
                  end

L11040:     get #2 using L11060, crqty, crtotal, crbom(), crtl(),         ~
                                crfold(), crrev
L11060:         FMT POS(71), 39*PD(14,4)
            netqty = max(0, crqty - crrev)
               if netqty = 0     then L11025
            newqty = crqty - crrev
            newqty = newqty + quantity
               if newqty < 0     then end
            crrev  = max(0, crrev - quantity)
               if crrev  > crqty then end
            crqty  = max(crqty, newqty)

            jbcrtotal = jbcrtotal - crtotal
            oldcrtotal = crtotal : crtotal = 0
            for b% = 1% to 12%
                jbcrbom (b%) = jbcrbom (b%) - crbom (b%)
                jbcrtl  (b%) = jbcrtl  (b%) - crtl  (b%)
                jbcrfold(b%) = jbcrfold(b%) - crfold(b%)

                unitbom   = crbom (b%) /  netqty
                unittl    = crtl  (b%) /  netqty
                unitfold  = crfold(b%) /  netqty
                costs(b%) = round(unitfold, 4)

                crbom (b%) = round(unitbom  * newqty, 4)
                crtl  (b%) = round(unittl   * newqty, 4)
                crfold(b%) = round(unitfold * newqty, 4)
                crtotal    = crtotal + crfold(b%)

                jbcrbom (b%) = jbcrbom (b%) + crbom (b%)
                jbcrtl  (b%) = jbcrtl  (b%) + crtl  (b%)
                jbcrfold(b%) = jbcrfold(b%) + crfold(b%)
            next b%
            jbcrtotal = jbcrtotal + crtotal

            call "PACKZERO" (crbom() , crbom$ )
            call "PACKZERO" (crtl()  , crtl$  )
            call "PACKZERO" (crfold(), crfold$)
            put #2 using L11370, crqty, crtotal, crbom$, crtl$, crfold$,  ~
                                crrev
L11370:         FMT POS(71), 2*PD(14,4), 3*CH(96), PD(14,4)
            rewrite #2
            goto update_jbmastr2

        regular_completion
*       ** Take care of QUANTITY > 0 (new completion).  Costs are based
*          on actuals recorded to JBMASTR2 record.
*
*        COST TYPE 'B' (Backflush).  Cost credited are equal to the
*        actuals that haven't been yet credited. [All Current Costs]
*        Entered as Valuation for Completion Method 'A' in JBCMPSUB.
*
*        COST TYPE 'P' (Prorate).  Cost credited are based on current
*        value of quantity remaining.
*          [Current unit cost = (Cost in - Cost out) / Left to build]
*
*        COST TYPE 'S' (Standard)  Current Standard Cost
*
*        COST TYPE 'Z' (Zero) Needs Explanation?
*
*        COST TYPE 'M' (Estimated BOM)  Based on BOM parts and kitted
*        material costs to estimate the assembly cost.
*        Uses JBBOMCST to calculate those costs.
*        Entered as Valuation for Completion Method 'A' in JBCMPSUB.
*
*        First the BOM Costs.  CRBOM = Total BOM Costs Credited
*
            if str(costflag$,1%,1%)  = "S" then L12300

            if str(costflag$,1%,1%) <> "Z" then L12230
               mat crbom = zer : goto L12300

L12230:     mat jbbom    = jbbom + coreadj  /* Adjust for core        */
            mat crbom    = jbbom - jbcrbom  /* Uncredited costs- BOM  */
            mat coreadj  = coreadj - corefg /* Not Yet Moved to WIP   */

            if abs(quantity - qltb) <  .01 then L12293
            if str(costflag$,1%,1%) <> "M" then L12260
                call "JBBOMCST" (part$, jobnr$, 2%, #4, #5, #6, crbom(), ~
                                           tot_cost, stdate%, errormsg$)
                  /* Any errors should have been caught in JBCMPSUB */
                tot_cost = tot_cost /* Compiler thing */
                mat crbom = (quantity) * crbom
                goto L12300
L12260:     if str(costflag$,1%,1%) <> "P" then L12293

            mat crbom    = (quantity) * crbom
            mat crbom    = (1 / qltb) * crbom

            mat coreadj  = (quantity) * coreadj
            mat coreadj  = (1 / qltb) * coreadj

            goto L12300

L12293:     mat jbcrbom = jbbom
            jbcrbom_set%  = 1%

L12300
*
*        Now the Value Added Costs.  CRTL = Total TL Costs Credited
*
            if str(costflag$,2%,1%)  = "S" then L12460

            if str(costflag$,2%,1%) <> "Z" then L12380
               mat crtl  = zer : goto L12460

L12380:     mat crtl     = jbtl  - jbcrtl   /* Uncredited costs- TL   */

            if str(costflag$,2%,1%) <> "P" then L12437
            if abs(quantity - qltb) <  .01 then L12437

            mat crtl     = (quantity) * crtl
            mat crtl     = (1 / qltb) * crtl
            goto L12460

L12437:     mat jbcrtl = jbtl
            jbcrtl_set%  = 1%
            if jbcrbom_set% = 0% then L12460

            mat jbtl2 = zer
            call "STCFOLDN" (part$, " ", #3, jbcrbom(), jbcrtl(),        ~
                             jbtl2(), jbcrtotal, jbcrfold())
            jbcrfold_set% = 1%
            goto L12760

*
L12460
*        Just in case one or the other was standard
*
            if str(costflag$,1%,1%)  = "S" then L12510   /* It is        */
            if str(costflag$,2%,1%) <> "S" then L12670   /* They Weren't */

L12510:     call "STCCOSTS" (part$, " ", #3, 3%, crtotal, crfold(),      ~
                                         jbbom(), jbtl(), jbtl2())

            if str(costflag$,1%,1%) <> "S" then L12560
               mat crbom = (quantity) * jbbom
L12560:     if str(costflag$,2%,1%)  <> "S" then L12760
               mat jbtl  = (quantity) * jbtl
               mat jbtl2 = (quantity) * jbtl2
               mat crtl  = jbtl + jbtl2

            if str(costflag$,1%,2%)  <> "SS" then L12760
               mat costs  = crfold             /* Returned for HNYPST2 */
               mat crfold = (quantity) * crfold
               goto L12890                            /* Good, all done */

*
L12670
*        How about both Zero?
*
            if str(costflag$,1%,2%)  <> "ZZ" then L12760
               mat costs  = zer                /* Returned for HNYPST2 */
               mat crfold = zer
               crtotal    = 0
               goto write_jbcredit             /* Don't even need Total*/

*
L12760
*        OK Lets fold it (Don't have to if we were 'SS' or 'ZZ')
*
            mat jbtl2 = zer
            call "STCFOLDN" (part$, " ", #3, crbom(), crtl(), jbtl2(),   ~
                             crtotal, crfold())

            returncode% = 0%    /* Block Negative Costs         */
            crtotal = 0
            for b% = 1% to 12%                 /* Returned for HNYPST2 */
                costs(b%) = round(crfold(b%) / quantity, 4)
                if crfold(b%) < 0 then returncode% = 96%
                crtotal      = crtotal      + crfold(b%)
            next b%
            if returncode%  =  0% then L12890 /* All Buckets >= 0      */
            if comperrcst$  = "N" then L12890 /* They don't Care       */
            if comperrcst$ <> "T" then L12870 /* They REALLY Care      */
               returncode% = 95%             /* Block on Total Only   */
               if crtotal >= 0 then L12890    /* Total OK              */
L12870:           end
*
L12890
*        Now for some totals and go home
*
            crtotal = 0
            for b% = 1% to 12%
                crfold(b%) = round(crfold(b%),4)
                crbom (b%) = round(crbom (b%),4)
                crtl  (b%) = round(crtl  (b%),4)
                crtotal    = crtotal + crfold(b%)

                if jbcrfold_set% <> 0% then L12970
                if jbcrbom_set%  <> 0% then L12941
                   jbcrbom (b%) = jbcrbom(b%)  + crbom (b%)
L12941:         if jbcrtl_set%   <> 0% then L12960
                   jbcrtl  (b%) = jbcrtl (b%)  + crtl  (b%)
L12960:            jbcrfold(b%) = jbcrfold(b%) + crfold(b%)
L12970:     next b%
            crtotal   = round(crtotal, 4)
            if jbcrfold_set% <> 0% then write_jbcredit
               jbcrtotal = round(jbcrtotal + crtotal, 4)

        write_jbcredit
            call "PACKZERO" (crbom() , crbom$)
            call "PACKZERO" (crtl()  , crtl$ )
            call "PACKZERO" (crfold(), crfold$)
            posttext$ = "MATERIAL CREDITED TO JOB: " & jobnr$

L14060:     systime$ = time
            write #2 using L14120, jobnr$, postdate$, systime$,           ~
                     part$, store$, lot$, postdate$, systime$,           ~
                     quantity, crtotal, crbom$, crtl$, crfold$, 0,       ~
                     jobtonr$, posttext$, userid$, str(costflag$,1%,1%), ~
                     date, str(costflag$,2%,1%), " ",                    ~
                     eod goto L14060
L14120:         FMT CH(8), CH(6), CH(8), CH(25), CH(3), CH(6), CH(6),    ~
                    CH(8), 2*PD(14,4), 3*CH(96), PD(14,4), CH(8), CH(40),~
                    CH(3), CH(1), CH(6), CH(1), CH(59)

            revkey$ = str(jobnr$,,8%) & str(postdate$,,6%) & systime$

        update_jbmastr2
            call "PACKZERO" (jbcrbom() , jbcrbom$ )
            call "PACKZERO" (jbcrtl()  , jbcrtl$  )
            call "PACKZERO" (jbcrfold(), jbcrfold$)
            call "READ101" (#1, jobnr$, f1%)
            get #1 using L15042, jbqty, estcomp
L15042:         FMT POS(83), PD(14,4), POS(1139), PD(14,4)
            if abs(jbqty - jbqtycomp) < .01 then estcomp = 100
            put #1 using L15070, jbqtycomp, jbcrtotal, jbcrbom$, jbcrtl$, ~
                                jbcrfold$, estcomp
L15070:         FMT POS(91), PD(14,4), POS(528), PD(14,4), 3*CH(96),     ~
                    POS(1139), PD(14,4)
            rewrite #1
            returncode% = 0%

            if jbcrfold_set% = 0% then wiptotal = crtotal - oldcrtotal   ~
                                  else wiptotal = jbcrtotal - oldjbcrtotal

        end
