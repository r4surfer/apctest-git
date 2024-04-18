        REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                                                           *~
            *   CCC    OOO   RRRR    CCC    SSS   TTTTT   SSS   BBBB    *~
            *  C   C  O   O  R   R  C   C  S        T    S      B   B   *~
            *  C      O   O  RRRR   C       SSS     T     SSS   BBBB    *~
            *  C   C  O   O  R   R  C   C      S    T        S  B   B   *~
            *   CCC    OOO   R   R   CCC    SSS     T     SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CORCSTSB - DETERMINES THE CORE PART STD COST AND          *~
            *            CALCULATES THE REMAN COST FROM THE COMBINED    *~
            *            REMAN-CORE COST AND THE CORE STD COST.         *~
            *            PRORATES THE CALCULATED REMAN PART COST OVER   *~
            *            THE COST BUCKETS.                              *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and proprie- *~
            * tary assets of CAELUS, INCORPORATED, Spokane, WA,    em-  *~
            * bodying substantial creative efforts  and confidential    *~
            * information.  Unauthorized use, copying, decompiling,     *~
            * translating, disclosure, or transfer of it is prohibited. *~
            * Copyright (c) 1983, an unpublished work by CAELUS,        *~
            * INCORPORATED, Spokane, wa.  All rights reserved.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/30/93 ! ORIGINAL                                 ! JBK *~
            CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL

        sub "CORCSTSB"                                                   ~
                     (remanpart$,        /* The Remanufactured Part #  */~
                      remanstore$,       /* The Reman's Store          */~
                      remanlot$,         /* The Reman's Lot            */~
                      combinedcost,      /* Reman's and Core's Cost    */~
                      combinedcosts(),   /* Reman & Core Cost Buckets  */~
                      corepart$,         /* The Core Part              */~
                      core_fg_acct$,     /* Core Finshed Goods Account */~
                      corestccost,       /* Core's Standard Cost       */~
                      remancost,         /* Reman Part Cost            */~
                      remancosts(),      /* Reman Part Cost Buckets    */~
                      pc_var_cost,       /* Price/Cost Variance Cost   */~
                      #1,                /* SYSFILE2 Channel           */~
                      #2,                /* HNYQUAN Channel            */~
                      #3,                /* HHYMASTR Channel           */~
                      #4,                /* Dummy Channel              */~
                      return%)           /* Return Code                */

        dim                                                              ~
            basecosts(12),               /* Base Cost Buckets          */~
            combinedcosts(12),           /* Combined Reman & Core Cost */~
            core_fg_acct$9,              /* Core Finished Goods Account*/~
            corepart$25,                 /* Core Part Number           */~
            remanpart$25,                /* Reman Part Number          */~
            remancosts(12),              /* Reman Part Cost Buckets    */~
            totalcost(1),                /* Total Inventory Cost       */~
            totalwork(1,12)              /* Work Array                 */


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.04 06/29/93 SFC & Cycle Count Enhancements  "
        REM *************************************************************

        REM *************************************************************~
            * Main Work Area, Verify Reman Part and Determine Costs     *~
            *************************************************************

*        Set-up default values for some variables.
            basecost, remancost, pc_var_cost = combinedcost
            mat remancosts = combinedcosts
            mat basecosts  = combinedcosts
            corestccost    = 0
            core_fg_acct$  = " "
            return%        = 99%

*        Check to see if this is a Reman part or not.  Call CORVALSB
*        with 'CK' Check option, get back core finished goods account
*        and the core standard costs as extras

            corepart$ = remanpart$
            call "CORVALSB" ("CK", corepart$, " ", " ", corestccost,     ~
                             " ", core_fg_acct$, " ", " ", " ", 0%, " ", ~
                             " ", #2, #1, #3, #4, corevalue%)
            if corevalue%  <> 0% then end
            if corestccost  = 0  then end

*        We now have a Reman part with an associated core part. Its
*        time to determine and ajust as necessary the Reman parts cost

            remancost = basecost - corestccost
            if remancost > 0 then prorate_reman_cost
                remancost = 0  :  mat remancosts = zer
                call "STCCOSTS" (remanpart$, " ", #1, 2%, basecost,      ~
                                                             basecosts())
                remancost = basecost  :  mat remancosts = basecosts
                if basecost = 0 then L10700


*        We have derived cost for the reman part, it is necessary to
*        prorate the cost among the cost buckets

        prorate_reman_cost
            hi% = 1%  :  mat remancosts = zer
            for i% = 1% to 12%
                if basecosts(i%) = 0 then L10500
                remancosts(i%) =                                         ~
                        round(remancost * basecosts(i%)/basecost,4%)
                if remancosts(i%) > remancosts(hi%) then hi% = i%
L10500:     next i%

*        Check for round-off error and fudge it
            mat totalwork = con
            mat totalcost = totalwork * remancosts
            remancost     = totalcost(1%)

*          IF FUDGE_FACTOR = 0 THEN 10700
*          IF FUDGE_FACTOR > 0 THEN REMANCOSTS(HI%) = REMANCOSTS(HI%) + ~
*                                                           FUDGE_FACTOR
*          IF FUDGE_FACTOR < 0 THEN REMANCOST = REMANCOST + FUDGE_FACTOR

L10700
*        Determine Price/Cost Variance Amount
            pc_var_cost = remancost + corestccost
            return% = 0%
            end


