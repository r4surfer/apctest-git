        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   SSS   TTTTT   CCC    SSS   PPPP   RRRR    OOO   L       *~
            *  S        T    C   C  S      P   P  R   R  O   O  L       *~
            *   SSS     T    C       SSS   PPPP   RRRR   O   O  L       *~
            *      S    T    C   C      S  P      R   R  O   O  L       *~
            *   SSS     T     CCC    SSS   P      R   R   OOO   LLLLL   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * STCSPROL - ROLL UP LOGIC FOR A SINGLE PART                *~
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
            * 08/18/87 ! ORIGINAL                                 ! KEN *~
            * 06/03/88 ! BOM per unit/fixed overage inversion     ! KAB *~
            * 04/03/89 ! Added secon Return Code to calling Args  ! MJB *~
            * 09/22/93 ! Added support for new BOM Markers 'NC' & ! JDH *~
            *          !   'NP', which do not add costs.          !     *~
            * 09/22/93 ! Added support for new BOM Marker 'BP',   ! JDH *~
            *          !   which subtracts costs.                 !     *~
            * 10/03/94 ! PRR 13198 - Now consider Overage Quantity! RJH *~
            *          !   for Phantom Parts.                     !     *~
            * 11/29/95 ! PRR 13407. Look to HNYACYXF for VEND step! JDH *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            sub "STCSPROL" (ppart$, pbom$, prte$, psrq, bom(), rte(),    ~
                            #5,          /* BOMMASTR                   */~
                            #10,         /* STCHNY                     */~
                            #12,         /* STCWCACT                   */~
                            #13,         /* STCLABR                    */~
                            #16,         /* RTEMASTR                   */~
                            #6,          /* WCMASTR                    */~
                            erte%,       /* Ret Code for RTE Error     */~
                            ebom%)       /* Ret Code for BOM Error     */

        dim                                                              ~
            act$(5)4,                    /* Activity Code              */~
            assybom$3,                   /* Assembly BOM               */~
            assyrte$3,                   /* Assembly Route             */~
            bom$3,                       /* Bill of Materials          */~
            bomcst1(12),                 /* Bill of Material Costs     */~
            bomcst2(12),                 /* Bill of Material Costs     */~
            bomcst3(12),                 /* Bill of Material Costs     */~
            bomcost(12),                 /* Bill of Material Costs     */~
            bucket%(6),                  /* Standard Cost Bucket       */~
            compplowkey$31,              /* BOM Plow Key               */~
            lclass$(4)4,                 /* Labor Classes Associated   */~
            lmult(4),                    /* Labor Class Multiplier     */~
            mkr$2,                       /* BOM Marker                 */~
            norm(5),                     /* Concurrent WC Factor       */~
            oldcompplowkey$(101)31,      /* BOM Plow Key Holder        */~
            palevel%(101),               /* Phantom Assy Level         */~
            paok$(101)1,                 /* Phantom Assy O.K.?         */~
            part$25,                     /* Part Code                  */~
            pbs%(1),                     /* Pick By Step Search Rec.   */~
            pbs$5,                       /* Pick By Step               */~
            pbs$(500)5,                  /* Pick By Steps              */~
            pbshold%(101),               /* Pick By Steps              */~
            phfact(101),                 /* Phantom Multiplier         */~
            plowkey$100,                 /* Miscellaneous Read/Plow Key*/~
            readkey$100,                 /* Miscellaneous Read/Plow Key*/~
            rate(6),                     /* Standard Cost Rates        */~
            rtecost(12),                 /* Route Costs                */~
            rtestep$(500)160,            /* Route Steps                */~
            wc$(5)4,                     /* Work Center                */~
            yield(500)                   /* Cumulative Net Yield       */~

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.03 08/12/96 Last Wang Release               "
        REM *************************************************************

            mat f1% = zer

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! HNYACTXF ! HNY, WC ACTIVITY CROSS REFERENCE         *~
            * # 5 ! BOMMASTR ! BOM relationship file                    *~
            * # 6 ! WCMASTR  ! Work Center Master file                  *~
            * #10 ! STCHNY   ! Inventory Master File                    *~
            * #12 ! STCWCACT ! Work Center Activity Costs               *~
            * #13 ! STCLABOR ! Labor Costs                              *~
            * #16 ! RTEMASTR ! Route Master File                        *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "HNYACTXF",                                      ~
                        varc,     indexed,  recsize =  512,              ~
                        keypos =  1,   keylen =  29,                     ~
                        alt key   1,   keypos =  26, keylen =   4, dup

            if been_here_before% = 1% then L10000
                been_here_before% = 1%
                call "OPENCHCK" (#01, fs%(01), f2%(01), 1%, rslt$(01))

L10000: REM *************************************************************~
            * COST PARTS, USE MISC. AS IS.  DERIVE RTE AND BOM COSTS.   *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************

            plowkey$ = ppart$
            assybom$ = pbom$
            assyrte$ = prte$
            srq      = psrq
            erte%, ebom% = 0%

            mat rtecost = zer:mat bomcost = zer
            rte% = 0%
            if assyrte$ <> " " then gosub route_costs
            if assybom$ <> " " then gosub bom_costs

               mat rtecost = (1/srq) * rtecost
               mat bomcost = (1/srq) * bomcost

            if erte% <> 0% or ebom% <> 0% then L65000
               mat bom = bomcost
               mat rte = rtecost
               goto L65000

        REM *************************************************************~
            * DERIVE ROUTE COSTS.                                       *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************

        route_costs

            call "STCRTE"                /*                            */~
                     (plowkey$,          /* PART NEEDED                */~
                      assyrte$,          /* THE WC ROUTE TO USE        */~
                      assybom$,          /* WHICH BOM TO USE           */~
                      rtestep$(),        /* THE DERIVED ROUTE          */~
                      pbs$(),            /* PICK-BY-STEPS              */~
                      rte%,              /* # OF ELEMENTS              */~
                      #5,                /* BOMMASTR                   */~
                      #16,               /* RTEMASTR                   */~
                      #10)               /* STCHNY                     */~

            if rte% > 0% then L12220
               assyrte$ = " " : erte% = erte% + 1% : return

L12220:     for i% = 1% to rte%

                get rtestep$(i%) using L12290, wc$(1), setup%, run,       ~
                                 wc$(2), norm(2), wc$(3), norm(3),       ~
                                 wc$(4), norm(4), yield(i%), act$(),     ~
                                 phfactor

L12290:             FMT CH(4), POS(40), BI(2), PD(14,4), POS(50), CH(4), ~
                        PD(7,4), CH(4), PD(7,4), CH(4), PD(7,4),         ~
                        POS(80), PD(14,7), POS(94), CH(4),               ~
                        POS(136), 4*CH(4), PD(14,4)

                if wc$(1%) <> "VEND" then L12340
                    gosub determine_vendor_costs
                    if determined% = 1% then L12520

L12340:         wcfactor = 24
                call "READ100" (#6, wc$(1), f1%(6))
                   if f1%(6) = 0% then L12400
                get #6 using L12380, wcfactor
L12380:             FMT XX(2019), BI(2)

L12400:         norm(1), norm(5) = 1 : wc$(5) = wc$(1)
                asrq = (100 * phfactor * srq)/yield(i%)
                run = (24 * run) / wcfactor

                for step% = 1% to 4%
                   gosub step_cost
                next step%

                if act$(5) = " " then L12520
                step% = 5% : asrq = 1 : run = (24 * setup%) / wcfactor
                gosub step_cost

L12520:     next i%

            return

        REM *************************************************************~
            * DERIVE COST OF A PARTICULAR STEP.                         *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************

        step_cost

            if wc$(step%) = " " and act$(step%) = " " then return

            readkey$ = str(wc$(step%),,4) & act$(step%)
            call "READ100" (#12, readkey$, f1%(12))
               if f1%(12) <> 0% then L14220
            str(readkey$,1,4) = " "
            if readkey$ = " " then return
            call "READ100" (#12, readkey$, f1%(12))
               if f1%(12) <> 0% then L14220
            readkey$ = wc$(step%)
            if readkey$ = " " then return
            call "READ100" (#12, readkey$, f1%(12))
               if f1%(12) = 0% then return

L14220:     get #12 using L14230, rate(), bucket%(), lclass$(), lmult()
L14230:         FMT POS(9), 6*PD(14,4), 6*BI(1), 4*CH(4), 4*PD(14,4)

            norm% = step% : gosub set_cost

            for labor% = 1% to 4%
                if lclass$(labor%) = " " then L14350
                   call "READ100" (#13, lclass$(labor%), f1%(13))
                      if f1%(13) = 0% then L14350
                   get #13 using L14320, rate(), bucket%()
L14320:                FMT POS(5), 6*PD(14,4), 6*BI(1)
                   norm% = 1% : norm(1%) = lmult(labor%)
                   gosub set_cost
L14350:     next labor%
            norm(1%) = 1
            return

        set_cost

            if bucket%(1%) = 0% then L14440
               rtecost(bucket%(1%)) = rtecost(bucket%(1%)) +             ~
                                      rate(1%) * norm(1%)
L14440:     if bucket%(2%) = 0% then L14470
               rtecost(bucket%(2%)) = rtecost(bucket%(2%)) +             ~
                                      rate(2%) * norm(1%)
L14470:     if bucket%(3%) = 0% then L14500
               rtecost(bucket%(3%)) = rtecost(bucket%(3%)) +             ~
                                      rate(3%) * asrq * norm(norm%)
L14500:     if bucket%(4%) = 0% then L14530
               rtecost(bucket%(4%)) = rtecost(bucket%(4%)) +             ~
                                      rate(4%) * asrq * norm(norm%)
L14530:     if bucket%(5%) = 0% then L14560
               rtecost(bucket%(5%)) = rtecost(bucket%(5%)) +             ~
                                      rate(5%) * asrq * run * norm(norm%)
L14560:     if bucket%(6%) = 0% then return
               rtecost(bucket%(6%)) = rtecost(bucket%(6%)) +             ~
                                      rate(6%) * asrq * run * norm(norm%)

            return

        determine_vendor_costs
            determined% = 0%
            readkey$ = str(ppart$,,25%) & str(act$(1%),,4%)
            call "READ100" (#01, readkey$, f1%(1%))
                if f1%(1%) = 0% then return
            get #01 using L14760, conv, vendor_cst, vendor_bucket%
L14760:         FMT POS(37), PD(14,7), PD(14,7), BI(4)
            vendor_cst = vendor_cst * conv * srq
            determined% = 1%
            rtecost(vendor_bucket%) = rtecost(vendor_bucket%) + vendor_cst
            return

        REM *************************************************************~
            * DERIVE COSTS INHERITED FROM LOWER LEVEL ASSEMBLIES.       *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************

        bom_costs

           ti%  = 1% : phfact(1%) = 1
           palevel%, palevel%(ti%) = 0%:paok$(ti%) = "Y"

           compplowkey$ = str(plowkey$,1,25) & str(assybom$,1,3) & "  0"

L16130:    call "PLOWNEXT" (#5, compplowkey$, 28%, f1%(5))
                if f1%(5%) <> 0% then L16200
                   assybom$ = " " : ebom% = ebom% + 1% : return

L16170:    call "PLOWNEXT" (#5, compplowkey$, 28%, f1%(5))
                if f1%(5%) =  0% then return

L16200:    get #5, using L16210, part$, qu, tu, addon, fu, mkr$, pbs$
L16210:       FMT CH(25), XX(31), 4*PD(14,4), CH(2), XX(8), CH(4)

           if mkr$ = "RE" or mkr$ = "TL" then L16170
           if mkr$ = "NC" or mkr$ = "NP" then L16170

           if ti% = 1% then L16300
           if paok$(ti%) = "Y" then L16300
              pbs% = pbshold%(ti% - 1%)
              goto L16380
L16300:    if rte% <= 0% then L16320
           if pbs$ <> " " then L16330
L16320:       pbs% = 1%:goto L16380
L16330:    put str(pbs$,5) using L16340, palevel%(ti%)
L16340:        FMT BI(1)
           search str(pbs$(),1%, 5%*rte%) = str(pbs$,,5) to pbs%() step 5
           pbs% = min(max(1%, int((pbs%(1) + 4%)/5%)), rte%)

L16380:    qtyu  = srq * phfact(ti%) * (qu*tu + fu)
           if rte% > 0% then qtyu  = (100 * qtyu) / yield(pbs%)
           qtyu  = qtyu + addon
           if mkr$ = "BP" and qtyu < 0 then L16460
           if qtyu <= 0 then L16170

           if mkr$ = "PH" then goto  L16600
           if mkr$ = "PA" then goto  L16600

L16460: REM ADD IN COSTS HERE  (or subtract for 'BP's)
           call "READ100"(#10, part$, f1%(10))
              if f1%(10) = 0% then L16170
L16490:    get #10, using L16500, bomcst1(), bomcst2(), bomcst3()
L16500:        FMT POS(60), 12*PD(14,4), 12*PD(14,4), 12*PD(14,4)
           mat bomcst1 = (qtyu) * bomcst1
           mat bomcost = bomcost + bomcst1
           mat bomcst2 = (qtyu) * bomcst2
           mat bomcost = bomcost + bomcst2
           mat bomcst3 = (qtyu) * bomcst3
           mat bomcost = bomcost + bomcst3

           goto L16170

L16600: REM * * * PHANTOM LOOP LOGIC * * *

             call "READ100"(#10, part$, f1%(10))
                if f1%(10) = 0% then L16170
             get #10, using L16650, bom$
L16650:         FMT POS(38), CH(3)
             if bom$ = " " then L16490
             if ti% > 99% then L16490
             oldcompplowkey$(ti%) = compplowkey$
             pbshold%(ti%) = pbs%
             ti% = ti% + 1%
             if mkr$ <> "PA" then L16750
             if paok$(ti% - 1%) <> "Y" then L16750
                palevel%, palevel%(ti%) = palevel% + 1%
                paok$(ti%) = "Y"
L16750
*           PHFACT(TI%) = PHFACT(TI%-1%) * QU * TU
             phfact(ti%) = qtyu / srq
             compplowkey$ = str(part$,1,25) & str(bom$,1,3) & "  0"
             gosub L16130
             paok$(ti%) = " "
             ti% = ti% - 1%
             compplowkey$ = oldcompplowkey$(ti%)
             goto L16170

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            end
