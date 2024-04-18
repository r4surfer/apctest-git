        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  H   H  N   N  Y   Y  U   U   SSS   DDDD    SSS   PPPP    *~
            *  H   H  NN  N  Y   Y  U   U  S      D   D  S      P   P   *~
            *  HHHHH  N N N   YYY   U   U   SSS   D   D   SSS   PPPP    *~
            *  H   H  N  NN    Y    U   U      S  D   D      S  P       *~
            *  H   H  N   N    Y     UUU    SSS   DDDD    SSS   P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYUSDSP - Displays Usage information for a requested part*~
            *            from the Usage Summary files (HNYUASUM and     *~
            *            HNYURSUM).                                     *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1991  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/05/91 ! Original                                 ! JIM *~
            * 08/19/92 ! Misc mods for release.                   ! MLJ *~
            * 09/02/92 ! Misc mods for release.                   ! JDH *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        sub "HNYUSDSP"   (partnbr$,      /* Requested Part Number      */~
                          partdsc$)      /* Requested Part Description */

        dim                                                              ~
            aquant(60),  rquant(60),     /* Summary Quantity arrays    */~
            avalue(60),  rvalue(60),     /* Summary Value arrays       */~
            cal2$8,                      /* New Calendar Selection     */~
            calcode$8,                   /* Cal code for summaries     */~
            caldescr$30,                 /* Calendar Description       */~
            calyr$4,                     /* Calendar Year              */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            days$3,                      /* PFMCAL days in a period    */~
            dfltcal$8,                   /* Dflt Cal Code for non-MPS  */~
            dsply$(12)79,                /* Screen display lines       */~
            errormsg$79, e$1,            /* Error message & FAC        */~
            fcast(4,60), tfcst(4),       /* Forcast Usage Qty & Val    */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79, i$1,          /* Information Message & FAC  */~
            line1$64,                    /* Screen Line #1             */~
            line18$79,                   /* Screen Line #18- Underscore*/~
            line19$79,                   /* Screen Line #19- Totals    */~
            line2$79,                    /* Screen Line #2             */~
            line4$79,                    /* Screen Line #4             */~
            line5$79,                    /* Screen Line #5             */~
            mpsgrp$8,                    /* Part's MPS Group           */~
            mpsonly$1,                   /* MPS Parts only? (Y/N)      */~
            msg$79,                      /* PLOWCODE message           */~
            nbrpds$2,                    /* PFMCAL record # periods    */~
            nextcal$8,                   /* Next Linkage Calendar      */~
            nfcst(4,60), tnfct(4),       /* Non-F'cast Usage Qty & Val */~
            offset%(60),                 /* PFMCAL record date offsets */~
            other(4,60), tothr(4),       /* 'Other' Usage Qty & Val    */~
            part$25,                     /* For P/N testing            */~
            partds2$34,                  /* (Part Description)         */~
            partnbr$25, partdsc$32,      /* Incoming Part & Description*/~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$33,                   /* PF Key Hex Values          */~
            plowky1$99, plowky2$99,      /* Miscellaneous Read/Plow Key*/~
            prevcal$8,                   /* Previous Linkage Calendar  */~
            prodn(4,60), tprod(4),       /* Prod'n Usage Qty & Val     */~
            qty_val$30,                  /* A LINE1 description        */~
            req_act$30,                  /* Another LINE1 description  */~
            start$(60)10,                /* Period Start dates         */~
            store$3, stor2$3,            /* Summary Store Codes        */~
            total(12), ttotl(4),         /* Screen line totals         */~
            unkwn(4,60), tunkn(4),       /* Unknown Usage Qty & Val    */~
            usecal$8,                    /* Summary File Calendar ID   */~
            usedsnb$1,                   /* Detail/Summary/Neither/Both*/~
            userid$3,                    /* Current User Id            */~
            usestor$1,                   /* Post Store Codes?          */~
            usetype$5                    /* Usage Type Code            */

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20                  /* Text from file opening     */

        dim dat1$8, dat2$8, str1$3, str2$3      /* HNYDDISP parameters */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.00 09/09/92 Cycle Counting & MPS Phase I    "
        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! HNYUASUM ! Inventory Usage- Actual, Summary         *~
            * #02 ! HNYURSUM ! Inventory Usage- Requested, Summary      *~
            * #05 ! MPSITEMS ! MPS Items Master File                    *~
            * #06 ! STORNAME ! Store Master File                        *~
            * #08 ! SYSFILE2 ! Caelus Management System Information     *~
            * #09 ! PFMCAL   ! Forecast/Usage Calendar file             *~
            * #10 ! MPSGROUP ! MPS Groups Master file                   *~
            * #11 ! HNYMASTR ! Inventory Master File                    *~
            * #12 ! HNYDETAL ! Inventory Details                        *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "HNYUASUM",                                      ~
                        varc,     indexed,  recsize = 1100,              ~
                        keypos = 1,    keylen = 44,                      ~
                        alt key  1, keypos =   4, keylen =   41

            select #02, "HNYURSUM",                                      ~
                        varc,     indexed,  recsize = 1100,              ~
                        keypos = 1,    keylen = 44,                      ~
                        alt key  1, keypos =   4, keylen =   41

            select #05, "MPSITEMS",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =   8, dup,    ~
                            key  2, keypos =   34, keylen =   6, dup,    ~
                            key  3, keypos =   40, keylen =   2, dup,    ~
                            key  4, keypos =   42, keylen =  33

            select #06, "STORNAME",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos = 1, keylen = 3

            select #08, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #09, "PFMCAL",                                        ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   8,                     ~
                        alternate key 1, keypos = 39, keylen = 6, dup

            select #10, "MPSGROUP",                                      ~
                        varc,     indexed,  recsize = 200,               ~
                        keypos = 1,    keylen = 8,                       ~
                        alternate key 1, keypos = 39, keylen = 8, dup,   ~
                                  key 2, keypos = 47, keylen = 8, dup

            select #11, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup

            select #12, "HNYDETAL",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  42,                     ~
                        alt key  1, keypos =   43, keylen =   6, dup,    ~
                            key  2, keypos =   49, keylen =   2, dup

            if beenherebefore% <> 0% then goto L10000
*        One-time only initializations.
                beenherebefore% = 1%
                call "OPENCHCK" (#01, fs%(01), f2%(01), 0%, rslt$(01))
                call "OPENCHCK" (#02, fs%(02), f2%(02), 0%, rslt$(02))
                call "OPENCHCK" (#05, fs%(05), f2%(05), 0%, rslt$(05))
                call "OPENCHCK" (#06, fs%(06), f2%(06), 0%, rslt$(06))
                call "OPENCHCK" (#08, fs%(08), f2%(08), 0%, rslt$(08))
                call "OPENCHCK" (#09, fs%(09), f2%(09), 0%, rslt$(09))
                call "OPENCHCK" (#10, fs%(10), f2%(10), 0%, rslt$(10))
                call "OPENCHCK" (#11, fs%(11), f2%(11), 0%, rslt$(11))
                call "OPENCHCK" (#12, fs%(12), f2%(12), 0%, rslt$(12))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

*        One-time only initializations.
            call "EXTRACT" addr("ID", userid$)
            date$ = date : call "DATEFMT" (date$)
            str(line2$,62) = "HNYUSDSP: " & str(cms2v$,,8)
            line4$ = "Year:"
            line5$ = "Pd. Start     Forecast   Non-Fcst Production    U"&~
                "nknown      Other        Total"
            l% = 12%                      /* # display lines on screen */
*        Access system-level defaults and values.
            mpsonly$ = "Y"
            usestor$, usedsnb$ = "N"
            call "READ100" (#08, "SWITCHS.HNY", f1%(8))    /* SYSFILE2 */
                if f1%(8) = 0% then goto exit_program
            get #08 using L09190, mpsonly$, usestor$, usedsnb$, dfltcal$
L09190:         FMT POS(108), 3*CH(1), CH(8)

L10000: REM *************************************************************~
            *   H E R E ' S   W H E R E   I T   A L L   H A P P E N S   *~
            *************************************************************

*        First, perform some every-time initializations.

            gosub setup_for_actual_2  /* Force start with Actual Usage */
            init (" ") start$()
            gosub zero_the_arrays
            partds2$ = partdsc$
            if str(partds2$,1,1) <> "(" then call "PUTPAREN" (partds2$)
            str(line2$,,61) = "Part: " & partnbr$ & " " & partds2$

*        If Usage Method is MPS only, check for the Part in MPSITEMS.
            calcode$ = " " /* Initialize the Calendar Code for summary */
            call "READ100" (#05, partnbr$, f1%(5))    /* Yes- MPSITEMS */
               if f1%(5) = 0% then goto L10230          /* Not MPS Part */
            get #05 using L10180, mpsgrp$         /* Get MPS Group Code */
L10180:        FMT POS(26), CH(8)
            call "READ100" (#10, mpsgrp$, f1%(10))/* MPSGROUP */
               if f1%(10) <> 0% then get #10 using L10210, calcode$
L10210:              FMT POS(47), CH(8)

L10230
*        Determine the appropriate Calendar Code for summaries.
            if calcode$ = " " then calcode$ = dfltcal$

L10260
*        Get information from the appropriate calendar record.
            call "READ100" (#09, calcode$, f1%(9))           /* PFMCAL */
                if f1%(9) = 0% then goto exit_program         /* Uh oh */
            get #9 using L10300, calyr$, nbrpds$, days$, nextcal$, prevcal$
L10300:         FMT POS(165), CH(4), POS(175), CH(2), CH(3), 2*CH(8)
                call "MXFL2GT" addr(#9, 44%, offset%(), 60%)
            convert days$ to days%, data goto L10315
L10315:     if days$="MO" then days%=30%
            convert nbrpds$ to nbrpds%, data goto L10325
L10325:     str(line4$,7,4) = calyr$
            for x% = 1% to nbrpds%
                call "DATEOFF" ("IG", offset%(x%), start$(x%), u3%)
                call "DATEFMT" (start$(x%))
            next x%
            z% = max(1%, nbrpds% - l% + 1%)   /* Highest top of screen */
            allstr% = 1%  /* Start as aggragate of all stores */
            errormsg$, stor2$ = " " : blank% = 0%
            gosub all_store_summary
            p% = 1%       /* Start at top of screen           */
            goto display_the_data

        access_next_store
            gosub read_next_store
            p% = 1%              /* Begin at first period/totals, etc. */
        display_the_data
            gosub L40000
            if keyhit% = 16% then goto exit_program
*          IF BLANK%  =  0% THEN GOTO 10460
*              IF KEYHIT% = 8% THEN GOTO 10520 ELSE GOTO 10522
            if keyhit% =  2% then p% = 1%                /* First page */
            if keyhit% =  3% then p% = z%                 /* Last page */
            if keyhit% =  4% then p% = max(1%, p%-l%) /* Previous page */
            if keyhit% =  5% then p% = max(1%,min(z%,p%+l%)) /* Nxt pg */
            if keyhit% =  6% then p% = max(1%,p%-1%)           /* Down */
            if keyhit% =  7% then p% = max(1%,min(z%,p%+1%))     /* Up */
            if keyhit% =  8% then goto access_next_store  /* New store */
            if keyhit% <> 9% then goto L10530
               cal2$ = " "  :   caldescr$ = hex(06) & "Select Calendar"
               call "PLOWCODE" (#9, cal2$, caldescr$, 0%, .30, f1%(9%))
               if f1%(9%) = 0% then display_the_data  /* No Selection  */
               if blank% = 1% then L10528
               if calcode$ = cal2$ then display_the_data  /* No Change */
L10528:            calcode$ = cal2$  :  goto L10260    /* New Selection */
L10530:     if keyhit% = 10% then gosub toggle_request_actual
            if keyhit% = 11% then gosub toggle_quantity_value
            if keyhit% <>20% then L10546
                calcode$ = prevcal$ : goto L10260
L10546:     if keyhit% <>21% then L10550
                calcode$ = nextcal$ : goto L10260
L10550:     if keyhit% <>24% then goto L10590
                allstr% = 1%           /* Indicate 'All-Store Summary' */
                gosub all_store_summary
                goto display_the_data
L10590:     if keyhit% = 0% then gosub accum_display_detail
            goto display_the_data

        accum_display_detail /* Setup for and call HNYDDISP for detail */
            period% = cursor%(1) - 5%
            if period% < 1% or period% > 12% then return
            if period% + p% - 1% > nbrpds% then return
            dat1$, dat1$, str1$, str2$ = " "
            temp$ = start$(period%)                         /* MM/DD/YY */
            call "DATUNFMT" (str(temp$,,8))                   /* YYMMDD */
            dat1$ = str(temp$,1,6)                            /* YYMMDD */
            call "DATE" addr ("G+", dat1$, days%, dat2$, u3%)
            if allstr% <> 0%                     /* All-Store Summary? */~
                then str1$ = "ALL"                              /* Yes */~
                else str1$, str2$ = store$                       /* No */
            call "HNYDDISP" (partnbr$, "D", dat1$, dat2$, str1$, str2$,  ~
                "ALL", " ", "ALL", #11, #12)
            return

        REM *************************************************************~
            *     M I S C E L L A N E O U S   S U B R O U T I N E S     *~
            *************************************************************

        toggle_quantity_value
            on f% goto setup_for_value, setup_for_quantity,              ~
                setup_for_value, setup_for_quantity
            return

        setup_for_quantity
            f% = f% - 1%
            qty_val$ = "Quants."
            str(line4$,12,55) = "*--------- Usage Quantities by Type of"&~
                " Usage ---------*"
            goto exit_setup

        setup_for_value
            f% = f% + 1%
            qty_val$ = "Value"
            str(line4$,12,55) = "*----------- Usage Values by Type of U"&~
                "sage -----------*"
            goto exit_setup

        toggle_request_actual
            on f% goto setup_for_request, setup_for_request,             ~
                setup_for_actual, setup_for_actual
            return

        setup_for_request
            f% = f% + 2%
            req_act$ = "Display Requested Usage"
            str(line4$,71) = "Requested"
            request% = 1%      /* Indicate 'Displying Requested Usage' */
            goto exit_setup

        setup_for_actual
            f% = f% - 2%
            goto setup_for_actual_3
        setup_for_actual_2
            f% = 1%     /* Force Initialization to 'Actual Quantities' */
            qty_val$ = "Quantity"
            str(line4$,12,55) = "*--------- Usage Quantities by Type of"&~
                " Usage ---------*"
        setup_for_actual_3
            req_act$ = "Display Actual Usage"
            str(line4$,71) = "   Actual"
            request% = 0%  /* Indicate 'Not Displying Requested Usage' */

        exit_setup
            return

        read_next_store      /* User PLOWs & selects the store to view */
            allstr% = 0%           /* Indicate 'NOT All-Store Summary' */
            errormsg$, stor2$ = " " : blank% = 0%
            if usestor$ = "N" then goto L15700
                msg$ = hex(06) & "Select a Store for Part: " & partnbr$  ~
                     & " " & partds2$
                call "PLOWCODE" (#06, stor2$, msg$, 0%, .3, f1%(6))
                if f1%(6) <> 0% then goto L15640            /* STORNAME */
                     errormsg$ = "You must select a store for display. "&~
                          "Press PF(8) to view stores."
                     blank% = 1%            /* Indicate 'blank screen' */
                     e$ = hex(94) : i$ = hex(bc)
                     return
L15640:         get #06 using L15650, stor2$
L15650:              FMT CH(3)

        all_store_summary         /* PF(24) causes store to be ignored */
            gosub init_the_arrays

L15700
*        Read the appropriate Usage Summary records and array 'em.
*        (We read the alternate key to determine the 1st or next
*        Store Number). PF(24) overrides the store to 'all'.
        read_actual_summary                      /* Actual Usage First */
            if f1%(1) = 0%                                               ~
                then call "REDALT2" (#01, plowky1$, 1%, f1%(1))          ~
                else call "READNEXT" (#01, f1%(1))
            if f1%(1) = 0% then goto read_requested_summary
                get #1 using L35040, store$, part$, usetype$, usecal$
                    call "MXFLPGT" addr(#1, 50%, aquant(), 60%, 4%)
                    call "MXFLPGT" addr(#1, 530%, avalue(), 60%, 4%)
                if part$ <> partnbr$ then goto L15830
                if usecal$<>calcode$ then read_actual_summary
                if allstr% <> 0% then goto L15850
                if store$ = stor2$ then goto L15850
L15830:              f1%(1) = 0%
                     goto read_requested_summary
L15850:         act% = act% + 1%
                gosub populate_the_arrays
                goto read_actual_summary

        read_requested_summary
            if f1%(2) = 0%                                               ~
                then call "REDALT2" (#02, plowky2$, 1%, f1%(2))          ~
                else call "READNEXT" (#02, f1%(2))
            if f1%(2) = 0% then goto read_summary_exit
                get #2 using L35040, store$, part$, usetype$, usecal$
                    call "MXFLPGT" addr(#2, 50%, rquant(), 60%, 4%)
                    call "MXFLPGT" addr(#2, 530%, rvalue(), 60%, 4%)
                if part$ <> partnbr$ then goto read_summary_exit
                if usecal$<>calcode$ then read_requested_summary
                if allstr% <> 0% then goto L15990
                if store$ <> stor2$ then goto read_summary_exit
L15990:              req% = req% + 1%
                     gosub populate_the_arrays
                     goto read_requested_summary

        populate_the_arrays
*        'Forecast' Usage
            for x% = 1% to nbrpds%
                if usetype$ <> "FCST" then goto L16230
                if f1%(1) = 0% then goto L16160
                     fcast(1%, x%) = fcast(1%, x%) + round(aquant(x%),2)
                     tfcst(1%) = tfcst(1%) + round(aquant(x%),2)
                     fcast(2%, x%) = fcast(2%, x%) + round(avalue(x%),2)
                     tfcst(2%) = tfcst(2%) + round(avalue(x%),2)
                     goto L16930
L16160:         fcast(3%, x%) = fcast(3%, x%) + round(rquant(x%),2)
                tfcst(3%) = tfcst(3%) + round(rquant(x%),2)
                fcast(4%, x%) = fcast(4%, x%) + round(rvalue(x%),2)
                tfcst(4%) = tfcst(4%) + round(rvalue(x%),2)
                goto L16930

L16230
*        See if usage is 'Non-forecast'.
                if usetype$ <> "NFCT" then goto L16410
                if f1%(1) = 0% then goto L16340
                     nfcst(1%, x%) = nfcst(1%, x%) + round(aquant(x%),2)
                     tnfct(1%) = tnfct(1%) + round(aquant(x%),2)
                     nfcst(2%, x%) = nfcst(2%, x%) + round(avalue(x%),2)
                     tnfct(2%) = tnfct(2%) + round(avalue(x%),2)
                     goto L16930
L16340:         nfcst(3%, x%) = nfcst(3%, x%) + round(rquant(x%),2)
                tnfct(3%) = tnfct(3%) + round(rquant(x%),2)
                nfcst(4%, x%) = nfcst(4%, x%) + round(rvalue(x%),2)
                tnfct(4%) = tnfct(4%) + round(rvalue(x%),2)
                goto L16930

L16410
*        See if usage is 'Production'.
                if usetype$ <> "PROD" then goto L16590
                if f1%(1) = 0% then goto L16520
                     prodn(1%, x%) = prodn(1%, x%) + round(aquant(x%),2)
                     tprod(1%) = tprod(1%) + round(aquant(x%),2)
                     prodn(2%, x%) = prodn(2%, x%) + round(avalue(x%),2)
                     tprod(2%) = tprod(2%) + round(avalue(x%),2)
                     goto L16930
L16520:         prodn(3%, x%) = prodn(3%, x%) + round(rquant(x%),2)
                tprod(3%) = tprod(3%) + round(rquant(x%),2)
                prodn(4%, x%) = prodn(4%, x%) + round(rvalue(x%),2)
                tprod(4%) = tprod(4%) + round(rvalue(x%),2)
                goto L16930

L16590
*        See if usage is 'Unknown'.
                if usetype$ <> "UNKN" then goto L16770
                if f1%(1) = 0% then goto L16700
                     unkwn(1%, x%) = unkwn(1%, x%) + round(aquant(x%),2)
                     tunkn(1%) = tunkn(1%) + round(aquant(x%),2)
                     unkwn(2%, x%) = unkwn(2%, x%) + round(avalue(x%),2)
                     tunkn(2%) = tunkn(2%) + round(avalue(x%),2)
                     goto L16930
L16700:         unkwn(3%, x%) = unkwn(3%, x%) + round(rquant(x%),2)
                tunkn(3%) = tunkn(3%) + round(rquant(x%),2)
                unkwn(4%, x%) = unkwn(4%, x%) + round(rvalue(x%),2)
                tunkn(4%) = tunkn(4%) + round(rvalue(x%),2)
                goto L16930

L16770
*        None of the above. Ergo, it's 'Other'.
                if f1%(1) = 0% then goto L16870
                     other(1%, x%) = other(1%, x%) + round(aquant(x%),2)
                     tothr(1%) = tothr(1%) + round(aquant(x%),2)
                     other(2%, x%) = other(2%, x%) + round(avalue(x%),2)
                     tothr(2%) = tothr(2%) + round(avalue(x%),2)
                     goto L16930
L16870:         other(3%, x%) = other(3%, x%) + round(rquant(x%),2)
                tothr(3%) = tothr(3%) + round(rquant(x%),2)
                other(4%, x%) = other(4%, x%) + round(rvalue(x%),2)
                tothr(4%) = tothr(4%) + round(rvalue(x%),2)
                     FMT PD(14,4)
L16930:     next x%
            for x% = 1% to 4%
                ttotl(x%) = tfcst(x%) + tnfct(x%) + tprod(x%) + tunkn(x%)~
                     + tothr(x%)
            next x%
            return

        init_the_arrays
            gosub zero_the_arrays
            plowky1$ = xor plowky1$    /* Make Actual file PLOWKEY ... */
            if allstr% = 0%                      /* All-Store Summary? */~
                then str(plowky1$,,28) = str(partnbr$) & stor2$  /* No */~
                else str(plowky1$,,25) = str(partnbr$)          /* Yes */
            plowky2$ = plowky1$     /* ... Make Requested file PLOWKEY */
            if allstr% = 0% then ln% = 28% else ln% = 25% /* PLOW lgth */
            f1%(1), f1%(2), act%, req% = 0%
            e$ = hex(84) : i$ = hex(a4)
            return

        zero_the_arrays
            mat fcast = zer : mat tfcst = zer
            mat nfcst = zer : mat tnfct = zer
            mat prodn = zer : mat tprod = zer
            mat unkwn = zer : mat tunkn = zer
            mat other = zer : mat tothr = zer
            mat total = zer : mat ttotl = zer
            mat aquant = zer : mat rquant = zer
            mat avalue = zer : mat rvalue = zer
            return

        read_summary_exit       /* Test for any warning/error messages */
            if act% <> 0% or req% <> 0% then goto L17290
                errormsg$ = "There is NO usage for this part/store/" &   ~
                     "Calendar. Press PF(9) to re-select."
                e$ = hex(94) : i$ = hex(bc)
                blank% = 1% /* Indicate 'Blank screen' */
                return
L17290:     if act% <> 0% and req% = 0% then                             ~
                errormsg$ = "There is no Requested usage for this " &    ~
                     "part/store/Calendar."
            if act% = 0% and req% <> 0% then                             ~
                errormsg$ = "There is no Actual usage for this " &       ~
                     "part/store/Calendar."
*          IF ACT% = 0% THEN GOSUB TOGGLE_REQUEST_ACTUAL
            return

        REM *************************************************************~
            *            F O R M A T    S T A T E M E N T S             *~
            *************************************************************

L35040:     FMT  /* #01 and #02- HNYUASUM and HNYURSUM Usage Summaries */~
                CH(3),          /* Store Code                          */~
                CH(25),         /* Part Number                         */~
                XX(3),          /* Store Code again                    */~
                CH(5),          /* Usage Type Code                     */~
                CH(8),          /* Calendar Code                       */~
                XX(6),          /* Calendar Start Date YYMMDD          */~
                CH(480),        /* Usage Quantities                    */~
                CH(480),        /* Usage Values                        */~
                XX(8),          /* Overflow Qty Bucket                 */~
                XX(8),          /* Overflow Val Bucket                 */~
                XX(8),          /* Underflow Qty Bucket                */~
                XX(8),          /* Underflow Val Bucket                */~
                XX(6),          /* Date record last modified YYMMDD    */~
                XX(3),          /* User record last modified           */~
                XX(049)         /* Filler (Unused Space)               */

L40000: REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

            gosub set_pf1
            mat total = zer
            init (" ") dsply$()
            if blank% = 0% then goto L40140     /* Error- blank screen? */
                str(pf$(1),,63), str(pf$(2),,63), str(pf$(3),,63),       ~
                     line19$ = " "
                str(pf$(1),28,18) = "(9)Change Calendar"
                pfkeys$ = all(hex(ff)) : str(pfkeys$,,5) = hex(08090d0f10)
                if prevcal$ = " " then L40122
                     str(pf$(2),28,21) = "(20)Previous Calendar"
                     str(pfkeys$,6,1) = hex(14)
L40122:         if nextcal$ = " " then L40130
                     str(pf$(3),28,17) = "(21)Next Calendar"
                     str(pfkeys$,7,1) = hex(15)
L40130:         goto L40760
L40140:     for t% = 1% to min(l%, nbrpds%)
                total(t%) = fcast(f%, p%+t%-1%) + nfcst(f%, p%+t%-1%) +  ~
                     prodn(f%, p%+t%-1%) + unkwn(f%, p%+t%-1%) +         ~
                     other(f%, p%+t%-1%)
                dsply$(t%) = start$(p%+t%-1%)
                if fcast(f%, p%+t%-1%) >= 0                         then ~
                convert fcast(f%, p%+t%-1%) to str(dsply$(t%),12,11),    ~
                     pic(########.##)                               else ~
                convert fcast(f%, p%+t%-1%) to str(dsply$(t%),12,11),    ~
                     pic(-#######.##)
                if nfcst(f%, p%+t%-1%) >= 0                         then ~
                convert nfcst(f%, p%+t%-1%) to str(dsply$(t%),23,11),    ~
                     pic(########.##)                               else ~
                convert nfcst(f%, p%+t%-1%) to str(dsply$(t%),23,11),    ~
                     pic(-#######.##)
                if prodn(f%, p%+t%-1%) >= 0                         then ~
                convert prodn(f%, p%+t%-1%) to str(dsply$(t%),34,11),    ~
                     pic(########.##)                               else ~
                convert prodn(f%, p%+t%-1%) to str(dsply$(t%),34,11),    ~
                     pic(-#######.##)
                if unkwn(f%, p%+t%-1%) >= 0                         then ~
                convert unkwn(f%, p%+t%-1%) to str(dsply$(t%),45,11),    ~
                     pic(########.##)                               else ~
                convert unkwn(f%, p%+t%-1%) to str(dsply$(t%),45,11),    ~
                     pic(-#######.##)
                if other(f%, p%+t%-1%) >= 0                         then ~
                convert other(f%, p%+t%-1%) to str(dsply$(t%),56,11),    ~
                     pic(########.##)                               else ~
                convert other(f%, p%+t%-1%) to str(dsply$(t%),56,11),    ~
                     pic(-#######.##)
                if total(t%)           >= 0                         then ~
                convert total(t%)           to str(dsply$(t%),67,13),    ~
                     pic(##########.##)                             else ~
                convert total(t%)           to str(dsply$(t%),67,13),    ~
                     pic(-#########.##)
            next t%
            line19$ = "** Totals:"
            if tfcst(f%) >= 0                                       then ~
            convert tfcst(f%) to str(line19$,12,11), pic(########.##)    ~
                                                                    else ~
            convert tfcst(f%) to str(line19$,12,11), pic(-#######.##)
            if tnfct(f%) >= 0                                       then ~
            convert tnfct(f%) to str(line19$,23,11), pic(########.##)    ~
                                                                    else ~
            convert tnfct(f%) to str(line19$,23,11), pic(-#######.##)
            if tprod(f%) >= 0                                       then ~
            convert tprod(f%) to str(line19$,34,11), pic(########.##)    ~
                                                                    else ~
            convert tprod(f%) to str(line19$,34,11), pic(-#######.##)
            if tunkn(f%) >= 0                                       then ~
            convert tunkn(f%) to str(line19$,45,11), pic(########.##)    ~
                                                                    else ~
            convert tunkn(f%) to str(line19$,45,11), pic(-#######.##)
            if tothr(f%) >= 0                                       then ~
            convert tothr(f%) to str(line19$,56,11), pic(########.##)    ~
                                                                    else ~
            convert tothr(f%) to str(line19$,56,11), pic(-#######.##)
            if ttotl(f%) >= 0                                       then ~
            convert ttotl(f%) to str(line19$,67,13), pic(##########.##)  ~
                                                                    else ~
            convert ttotl(f%) to str(line19$,67,13), pic(-#########.##)

L40760
*        LINE1$ indicates the store (if any).
            line1$ = req_act$ & " " & qty_val$
            if allstr% = 0% then goto L40820      /* All-Store Summary? */
                line1$ = line1$ & "  Store" & hex(84) & "All" &          ~
                     hex(8c) & " Calendr: " &hex(84) &calcode$ &hex(8c)
                                         /* Yes- alter store indicator */
                goto L40860
L40820:     if store$<>" " and store$<>hex(000000) and usestor$<>"N"     ~
                then line1$ = line1$ & "  Store" & hex(84) & stor2$ &    ~
                     hex(8c) & " Calendr " & hex(84) & calcode$ & hex(8c)

L40860:     accept                                                       ~
                at (01,02), fac(hex(8c)), line1$                , ch(64),~
                at (01,66), "Today:",                                    ~
                at (01,73), fac(hex(8c)), date$                 , ch(08),~
                at (02,02), fac(hex(ac)), line2$                , ch(79),~
                at (03,02), fac(e$),      errormsg$             , ch(79),~
                at (04,02), fac(hex(8c)), line4$                , ch(79),~
                at (05,02), fac(hex(ac)), line5$                , ch(79),~
                                                                         ~
                at (06,02), fac(hex(8c)), dsply$( 1)            , ch(79),~
                at (07,02), fac(hex(8c)), dsply$( 2)            , ch(79),~
                at (08,02), fac(hex(8c)), dsply$( 3)            , ch(79),~
                at (09,02), fac(hex(8c)), dsply$( 4)            , ch(79),~
                at (10,02), fac(hex(8c)), dsply$( 5)            , ch(79),~
                at (11,02), fac(hex(8c)), dsply$( 6)            , ch(79),~
                at (12,02), fac(hex(8c)), dsply$( 7)            , ch(79),~
                at (13,02), fac(hex(8c)), dsply$( 8)            , ch(79),~
                at (14,02), fac(hex(8c)), dsply$( 9)            , ch(79),~
                at (15,02), fac(hex(8c)), dsply$(10)            , ch(79),~
                at (16,02), fac(hex(8c)), dsply$(11)            , ch(79),~
                at (17,02), fac(hex(8c)), dsply$(12)            , ch(79),~
                                                                         ~
                at (18,02), fac(hex(ac)), line18$               , ch(79),~
                at (19,02), fac(hex(8c)), line19$               , ch(79),~
                                                                         ~
                at (21,02), fac(i$),        inpmessage$         , ch(79),~
                at (22,02), fac(hex(8c)),   pf$(1)              , ch(79),~
                at (23,02), fac(hex(8c)),   pf$(2)              , ch(79),~
                at (24,02), fac(hex(8c)),   pf$(3)              , ch(79),~
                                                                         ~
                keys(pfkeys$), key(keyhit%)

            if keyhit% <> 13 then L41210
                call "MANUAL" ("HNYUSDSP") : goto L40860

L41210:     if keyhit% <> 15 then L41240
                call "PRNTSCRN" : goto L40860

L41240:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return

        set_pf1
            pf$(1) = "(2)1st Page  (5)Next Page   (9)Change Ca" &        ~
                     "l     (20)Prev Cal     (13)Instructions"
            pf$(2) = "(3)Last Page (6)Down (7)Up (10)Toggle Ac" &        ~
                     "t/Req (21)Next Cal     (15)Print Screen"
            pf$(3) = "(4)Prev Page (8)New Store  (11)Toggle Qt" &        ~
                     "y/Val (24)All-Str Sum  (16)Exit Display"
            pfkeys$ = hex(ff02030405060708090a0bff0dff0f10ffffff1415ffff1~
        8ffffffffffffffff00)
            if usestor$ <> "N" then goto L41400
                str(pf$(3),14,12) = " " : str(pfkeys$, 8,1) = hex(ff)
                str(pf$(3),47,15) = " " : str(pfkeys$,24,1) = hex(ff)
L41400:     if p% <> 1% then goto L41440
                str(pf$(1),,11), str(pf$(2),14,7), str(pf$(3),,12) = " "
                str(pfkeys$,2,1), str(pfkeys$,4,1), str(pfkeys$,6,1) =   ~
                     hex(ff)
            if prevcal$ <> " " then L41434
                str(pf$(1),47,12) = " " : str(pfkeys$,20,1) = hex(ff)
L41434:     if nextcal$ <> " " then L41440
                str(pf$(2),47,12) = " " : str(pfkeys$,21,1) = hex(ff)
L41440:     if p% <> z% then goto L41480
                str(pf$(2),,12), str(pf$(2),22,5), str(pf$(1),14,12) = " "
                str(pfkeys$,3,1), str(pfkeys$,5,1), str(pfkeys$,7,1) =   ~
                     hex(ff)
L41480:     inpmessage$ = "Select option -OR- position cursor and press"&~
                " (RETURN) to see movement details."
            str(pfkeys$,33,1) = hex(00)             /* Enable (RETURN) */
            if request% = 0% then goto L41540
                inpmessage$ = "Select a PF Key option below."
                str(pfkeys$,33,1) = hex(ff)        /* Disable (RETURN) */
L41540:     return

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1991  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            end
