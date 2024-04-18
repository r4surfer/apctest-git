        REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                                                           *~
            *  JJJJJ  BBBB   BBBB    OOO   M   M   CCC    SSS   TTTTT   *~
            *    J    B   B  B   B  O   O  MM MM  C   C  S        T     *~
            *    J    BBBB   BBBB   O   O  M M M  C       SSS     T     *~
            *  J J    B   B  B   B  O   O  M   M  C   C      S    T     *~
            *   J     BBBB   BBBB    OOO   M   M   CCC    SSS     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBBOMCST - THIS FUNCTION WORKS IN TWO MODES.              *~
            *            1) ANALYZE THE JOB'S BOM AND MATERIAL LEDGER TO*~
            *               SEE IF COMPLETION VALUATION METHOD 'B' IS OK*~
            *            2) CALCULATE A PRORATED COST OF GOODS COMPLETED*~
            *               BASED ON THE JOB'S BOM.                     *~
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
            * 04/28/95 ! ORIGINAL                                 ! JDH *~
            CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL

        sub "JBBOMCST" (part$,           /* Job Part Number            */~
                        job$,            /* Job Number                 */~
                        mode%,           /* 1% - Analyze if 'B' is OK  */~
                                         /*      and return est. cost. */~
                                         /*      User interactive.     */~
                                         /* 2% - Calculate Cost Only.  */~
                                         /*      Auto ignore extras.   */~
                        #01,             /* JBCROSS2                   */~
                        #02,             /* JBMATER2                   */~
                        #04,             /* HNYMASTR                   */~
                        tot_cost(),      /* Calculated Cost  <Returned>*/~
                        tot_cost,        /* Total of Buckets <Returned>*/~
                        stdate%,         /* Start Date of Job          */~
                        errormsg$)       /* Error Message    <Returned>*/

        dim ask$(3)80,                   /* Ask User Text              */~
            bomid$3,                     /* Bom ID used for Job        */~
            bom$3,                       /* BOM Id.                    */~
            bom$(490)3,                  /* BOM List                   */~
            cost(13),                    /* Est. Cost of Components    */~
            errormsg$79,                 /* Error Message              */~
            job$8,                       /* Job Number                 */~
            kit_cost$(1601)104,          /* Cost of Mat'l Kitted to Job*/~
            kit_part$(1601)25,           /* Material Kitted to Job     */~
            kit_qty(1601),               /* Quantity Kitted to Job     */~
            mkr$2,                       /* BOM Marker                 */~
            part$25,                     /* Part Number                */~
            part$(1601)25,               /* BOM Part Number            */~
            phfact(101),                 /* Phantom Multiplier         */~
            plowkey$99,                  /* Misc Plowkey               */~
            pos%(1),                     /* Position of Search         */~
            quantity(1601),              /* Quantity                   */~
            readkey$(1601)99,            /* Misc Plowkey               */~
            readkey_ph$99,               /* Misc Plowkey for Phantom   */~
            readkey$99,                  /* Misc Plowkey               */~
            tot_cost(12)                 /* Total Est. Cost of Assy    */

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64)                      /* RECORD-ON-FILE FLAGS       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto   L01932
            cms2v$ = "R6.04.01 06/23/95 Patch Finalization of R6.04.01 "
L01932: REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! JBCROSS2 ! Job Cross-Reference file                 *~
            * #02 ! JBMATER2 ! Job Material's Ledger                    *~
            * #03 ! BOMMASTR ! Bill of Materials Relationship file      *~
            * #04 ! HNYMASTR ! Inventory Master File                    *~
            * #05 ! ENGMASTR ! Engineering Master file                  *~
            *************************************************************

            select #03, "BOMMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 150,                                  ~
                         keypos =  26, keylen = 31,                      ~
                         alt key  1, keypos = 1, keylen = 56

            select #05, "ENGMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2015,                                  ~
                        keypos = 1, keylen = 29

            if been_here_before% = 1% then L09000
                call "OPENCHCK" (#03, 0%, f2%(3%), 0%, " ")
                call "OPENCHCK" (#05, 0%, f2%(5%), 0%, " ")
                been_here_before% = 1%

L09000: REM *************************************************************~
            *                S T A R T   P R O C E S S                  *~
            *************************************************************

            init (" ") errormsg$, kit_part$(), kit_cost$(), part$()
            maxlines%, maxkit% = 0% : tot_cost = 0
            pok% = dim(part$(),1)
            mat cost = zer : mat tot_cost = zer : mat kit_qty = zer

*        First see if there is a JBCROSS2 record to get the BOM ID from
            plowkey$ = "JOB ORDER: " & str(job$)
            call "READ100" (#01, plowkey$, f1%(1%))
            if f1%(1%) = 0% then L09110
                get #01, using L09082, bomid$
L09082:              FMT POS(73), CH(3)
                if bomid$ = " " then L09110
                goto load_bom
L09110:     errormsg$ = "No BOM ID on file for this job.  Method 'B' " & ~
                        "Cannot be Used."
            severity% = 3%    /* 0% - No Error                        */
                              /* 1% - Not Used at this time           */
                              /* 2% - Warn, but Can Proceed           */
                              /* 3% - Fatal - Can't Proceed           */
            gosub error_exit

*        See if there's a BOM out there & Load it up
        load_bom
            init (hex(00)) readkey$
            phfact(1%), ph% = 1%
            readkey$ = str(part$,,25%) & str(bomid$,,3%) & "  0"
         bom_loop
            call "PLOWNEXT" (#03, readkey$, 28%, f1%(3%))
            if f1%(3%) <> 0% then L09560  /* Found a Component */
            if maxlines% > 0% then exit_bom_load
                errormsg$ = "BOM (" & bomid$ & ") is not on file.  " &   ~
                            "Method 'B' Cannot be Used."
                severity% = 3%
                gosub error_exit
L09560:     if maxlines% > pok%-2 then exit_bom_load
            maxlines% = maxlines% + 1%
            get #03, using L09600, part$(maxlines%), qu, tu, fx, ov,       ~
                                 mkr$, bom$
L09600:         FMT CH(25), XX(31), 4*PD(14,4), CH(2), XX(1), CH(3)
            if mkr$ = "SP" or mkr$ = "RE" then init_element/* skip these*/
            if part$(maxlines%) = part$ then init_element/* skip redund */
            call "READ100" (#04, part$(maxlines%), f1%(4))
              get #04, using L09660, parttype$
L09660:             FMT POS(180), CH(3)
                convert parttype$ to type%, data goto init_element
            if type% > 0% and type% < 200% then init_element
            ext = qu * tu + ov
            if type% > 199% and type% < 500% then purchased_or_not_phantom
            if str(mkr$,,1%) <> "P" then purchased_or_not_phantom
               phqty = round(ext * phfact(ph%) + fx, 2)
               goto phantom_processor
          purchased_or_not_phantom
            quantity(maxlines%) = round(ext * phfact(ph%) + fx, 2)
*        Now I've got to fool things up for non-costed item, this will
*         be done by setting the quantity to zero, hence no extened cost.
*        Items affected are NC (non-costed), NP (non-procured), and
*         standard byproducts (neg-qty ST).
            if str(mkr$,,1%) = "N" then L09795      /* A NC or NP         */
            if quantity(maxlines%) >= 0 then L09820 /* Not a Byproduct    */
            if mkr$ = "BP" then L09820              /* A Costed Byproduct */
L09795:         quantity(maxlines%) =  0          /* I said I'd do this */
L09820:     goto bom_loop

        phantom_processor
            if ph% > 100% then too_many_levels
            if bom$ <> " " then L09960
                if stdate% = 0 then no_bom_for_phantom
                readkey_ph$ = str(part$(maxlines%),,25%) & "1" & hex(00)
                call "PLOWNEXT" (#05, readkey_ph$, 26%, f1%(5%))
                    if f1%(5%) = 0 then no_bom_for_phantom
                       gosub get_bom_string

                    if bom$(stdate%) = " " then no_bom_for_phantom
                bom$ = bom$(stdate%)

L09960:     readkey$(ph%) = readkey$
            readkey$ = str(part$(maxlines%),,25) & str(bom$,,3) & "  0"
            ph% = ph% + 1%
            phfact(ph%) = phqty

            gosub init_element
            ph% = ph% - 1%
            readkey$ = readkey$(ph%)
            goto bom_loop

        init_element
          init (" ") part$(maxlines%)
          quantity(maxlines%) = 0%
          maxlines% = maxlines% - 1%
          goto bom_loop

            FMT CH(25),                  /* COMPONENT PART NUMBER      */~
                CH(25),                  /* ASSEMBLY PART NUMBER       */~
                CH(3),                   /* WHICH BOM STRUCTURE?       */~
                CH(3),                   /* SEQUENCE NUMBER            */~
                PD(14,4),                /* QUANTITY REQUIRED          */~
                PD(14,4),                /* TIMES USED (SIZE)          */~
                CH(2),                   /* BOM MARKER                 */~
                CH(21),                  /* DESCRIPTION                */~
                CH(3),                   /* WHICH RTE STRUCTURE?       */~
                BI(2),                   /* RTE STEP NUMBER            */~
                CH(3),                   /* COMPONENTS SPEC BOM        */~
                CH(47)                   /* FILLER                     */

        get_bom_string
            get #05, using L10460, bom$()
L10460:       FMT XX(29), 490 * CH(3)
            return

*       ** Unable to explode Phantom, too many levels
        too_many_levels
            ask$(2) = " Maximum Phantom Levels Exceeded."
            goto L10560

*       ** Unable to explode Phantom, No BOM
        no_bom_for_phantom
            ask$(2) = " No BOM found for explosion."

L10560:     ask$(1) = "Part:" & " " & part$(maxlines%)
            ask$(1) = ask$(1) & ask$(2)
            ask$(2) = "Press PF1 to Bypass Part."
            ask$(2) = ask$(2) & "  Press RETURN to bypass Explosion."
            ask$(3) = "Press PF12 to Terminate Process."
L10610:     ask% = 2%
            call "ASKUSER" (ask%, "** UNABLE TO EXPLODE PHANTOM **",     ~
                            ask$(1), ask$(2), ask$(3))
            if ask%  =  1% then init_element
            if ask%  =  0% then purchased_or_not_phantom
            if ask% <> 12% then L10610
               init (hex(ff)) readkey$
               goto init_element

        exit_bom_load

*        Now we have BOM parts & quantities, so let's figure kitted
*        parts and their costs
            init (hex(00)) readkey$ : maxkit% = 0%
            str(readkey$,,8%) = job$
L11030:     call "PLOWNEXT" (#02, readkey$, 8%, f1%(2%))
                if f1%(2%) = 0% then exit_kit
            maxkit% = maxkit% + 1%
            get #02 using L11070, kit_part$(maxkit%), kit_qty(maxkit%),   ~
                                                     kit_cost$(maxkit%)
L11070:         FMT POS(23), CH(25), POS(71), PD(14,4), CH(104)
            if kit_part$(maxkit%) = part$ then maxkit% = maxkit% - 1%
            goto L11030

        exit_kit

*        Got both sides of the equation, are they in sync?
*          Every BOM part has to have at least one of every component
*          kitted or no cost is possible.
*          If parts not on the BOM are kitted, we'll let them continue
*          but won't add those costs, since we don't know how to prorate
*          them.

*        1st FOR/NEXT is to see if each part has been kitted & get costs
            for i% = 1% to maxlines%
                second_half% = 0%
                search str(kit_part$(),,20000%) =                        ~
                                    str(part$(i%),,25%) to pos%() step 25
                if pos%(1%) > 0% then L11335 : second_half% = 20000%
                search str(kit_part$(),20001%,) =                        ~
                                    str(part$(i%),,25%) to pos%() step 25
                if pos%(1%) > 0% then L11335
                     errormsg$ = "Component: " & part$(i%) &             ~
                                 " Not Kitted. Method 'B' Can't be Used."
                     severity% = 3%
                     gosub error_exit
L11335:         pos%(1%) = (pos%(1%) + 24% + second_half%) / 25%
                get kit_cost$(pos%(1%)) using L11350, cost(1%),           ~
                                 cost(2%), cost(3%), cost(4%), cost(5%), ~
                                 cost(6%), cost(7%), cost(8%), cost(9%), ~
                                 cost(10%), cost(11%), cost(12%)
L11350:              FMT POS(9), 12*PD(14,4)
                cost(13%) = 0
                for j% = 1% to 12%
                     cost(j%) = round(cost(j%) / kit_qty(pos%(1%)) *     ~
                                quantity(i%), 4)   /* Est. Bucket Cost */
                     cost(13%) = cost(13%) + cost(j%) /* Tot Comp Cost */
                     tot_cost(j%) = tot_cost(j%) + cost(j%) /* Totaling */
                     next j%
                tot_cost = tot_cost + cost(13%)   /* Totaling */
                next i%

*        This FOR/NEXT is to see if any extras have been kitted
            for i% = 1% to maxkit%
                search str(part$(),,20000%) =                            ~
                                str(kit_part$(i%),,25%) to pos%() step 25
                if pos%(1%) > 0% then L11600
                search str(part$(),20001%,) =                            ~
                                str(kit_part$(i%),,25%) to pos%() step 25
                if pos%(1%) > 0% then L11600
                     get kit_cost$(i%) using L11480, cost(1%)
L11480:                   FMT PD(14,4)
                     cost(1%) = cost(1%) / kit_qty(i%)
                     call "CONVERT" (cost(1%), 2.2, temp$)
                     ask$(1%) = kit_part$(i%) & " kitted at cost of " &  ~
                                temp$ & ", but is not on BOM."
                     ask$(2%) = "Therefore, prorated costs are " &       ~
                                "indeterminate and will be ignored."
                     severity% = 2%
                     errormsg$ = "Non-BOM Part Kitted to Job, Method " & ~
                                 "'B' Cannot be Used."
                     gosub error_exit
L11600:         next i%

*        Well, if we got thru all that, I guess we can get out of Dodge
            goto L65000

        REM *************************************************************~
            * This program contains valuable trade secrets and proprie- *~
            * tary assets of CAELUS, INCORPORATED, Spokane, WA,    em-  *~
            * bodying substantial creative efforts  and confidential    *~
            * information.  Unauthorized use, copying, decompiling,     *~
            * translating, disclosure, or transfer of it is prohibited. *~
            * Copyright (c) 1983, an unpublished work by CAELUS,        *~
            * INCORPORATED, Spokane, wa.  All rights reserved.          *~
            *************************************************************

        REM *************************************************************~
            * Handle Errors here.  Might continue; might exit.          *~
            *************************************************************
        error_exit
            if severity% <> 3% then L60100
L60050:         return clear
                end

L60100:     if severity% = 0% then return
            if mode%     = 2% then L60560  /* Auto ignore extra material */
                                          /* kitted, but not on BOM.    */
            ask$(3%) = "Press RETURN to Continue; Press PF16 to Exit."
L60510:     keyhit% = 0%
            call "ASKUSER" (keyhit%, "Completion Value Method 'B'",      ~
                            ask$(1%), ask$(2%), ask$(3%))
            if keyhit% = 16% then L60050
            if keyhit% <> 0% then L60510
L60560:     errormsg$ = " "
            return

L65000: REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and proprie- *~
            * tary assets of CAELUS, INCORPORATED, Spokane, WA,    em-  *~
            * bodying substantial creative efforts  and confidential    *~
            * information.  Unauthorized use, copying, decompiling,     *~
            * translating, disclosure, or transfer of it is prohibited. *~
            * Copyright (c) 1983, an unpublished work by CAELUS,        *~
            * INCORPORATED, Spokane, wa.  All rights reserved.          *~
            CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL

            end
