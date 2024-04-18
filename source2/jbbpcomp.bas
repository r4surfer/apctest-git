        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  JJJJJ  BBBB   BBBB   PPPP    CCC    OOO   M   M  PPPP    *~
            *    J    B   B  B   B  P   P  C   C  O   O  MM MM  P   P   *~
            *    J    BBBB   BBBB   PPPP   C      O   O  M M M  PPPP    *~
            *  J J    B   B  B   B  P      C   C  O   O  M   M  P       *~
            *   J     BBBB   BBBB   P       CCC    OOO   M   M  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBBPCOMP - Allows the user to record by-products from     *~
            *            teardown jobs in terms of Scrapped, Good and   *~
            *            Future Yield. Posts "Good" by-products to      *~
            *            inventory. Extinguishes PIPOUTs as necessary.  *~
            *-----------------------------------------------------------*~
            *            Caveat- TURN SUBSCRIPT CHECKING OFF!           *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/26/92 ! Original                                 ! JIM *~
            * 07/20/93 ! PRR 12949 Correct 'Validate & Enable next! JIM *~
            *          !   ByProduct' scrolling logic.            !     *~
            * 07/20/93 ! PRR 12950 Tighten Lot Tracking.          ! JIM *~
            * 11/16/93 ! May now duplicate ByProduct part #s.     ! JIM *~
            * 11/16/93 ! Added PF2/Restart Line to PF11/Append.   ! JIM *~
            * 02/01/94 ! Added format check of default lot number ! MLJ *~
            *          !   and lines.                             !     *~
            * 08/12/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            avgcost(600),                /* Avg Cost per PIPOUT        */~
            avgcost$(600)96,             /* Avg Cost Breakdown         */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            buildprt$25, builddsc$34,    /* Building Part No. & Descr. */~
            byprodct$(600)25, byprodsc$34,/* By-Product Part No.       */~
            closedat$10,                 /* Job Close Date             */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            deflotnr$6,                  /* Default Lot No. (Job)      */~
            defstore$3,                  /* Default Store No. (Job)    */~
            edtmessage$79,               /* Edit screen message        */~
            end_date$10,                 /* Sch. Complete (Job)        */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            jbtagnbr$19,                 /* JBMASTR2/PIPOUT Tag #      */~
            jnlid$3,                     /* G/L Journal ID             */~
            joborder$8, jobdescr$32,     /* Job Order No. & Description*/~
            joblotnr$6,                  /* Default Lot No. (Job)      */~
            jobstore$3, storedsc$32,     /* Default Store No. (Job)    */~
            lfac$(20)1, afac$(20)1, bfac$(20)1, nfac$(20)1, xfac$(20)1,  ~
            line2$79,                    /* Screen Line #2             */~
            line10$79,                   /* Screen Line #10 (columns)  */~
            loc%(600),                   /* For the Lot # SEARCH       */~
            lotenabl%(600),              /* Lot # validation level     */~
            lotnumbr$(600)6,             /* By-Product Lot No.         */~
            lotunique$1,                 /* Lot uniqueness from SYS2   */~
            mode$1,                      /* Mode for HNYCDIST          */~
            modno$2,                     /* G/L Module ID              */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$33,                   /* PF Key Hex Values          */~
            pipoutdt$(600)10, pipoutdt%(600),/* PIPOUT Date- Future Qty*/~
            pipoutky$(600)56,            /* PIPOUT Primary Key         */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            postdate$8,                  /* Users inventory post date  */~
            qty_good$(600)9, qty_good(600),/* By-Product Good Qty      */~
            qtybuild$10,                 /* Qty to Build (Job)         */~
            qtyfutur$(600)9, qtyfutur(600,2),/* By-Product Future Yield*/~
            qtyrmain$10, qtyrdesc$32,    /* Qty Remaining to Build (Job*/~
            qtyscrap$(600)9, qtyscrap(600),/* By-Product Qty Scrapped  */~
            qtyxpect$(600)9, qtyxpect(600),/* By-Product Qty Expected  */~
            sfcdate$6,                   /* User's Inventory post date */~
            startdat$10,                 /* Job Start Date             */~
            storenbr$(600)3, strnrdsc$32,/* By-Product Store No.       */~
            summary$1,                   /* Summary indicator - G/L    */~
            temp$16,                     /* Temporary Variable         */~
            title$40,                    /* Journal title     - G/L    */~
            tpart$25,                    /* Temporary Variable         */~
            userid$3,                    /* User ID                    */~
            wrkcost(12)                  /* Avg Cost Breakdown         */

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
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
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
            * #01 ! JBMASTR2 ! Production job master file               *~
            * #02 ! HNYMASTR ! Inventory Master File                    *~
            * #03 ! HNYQUAN  ! Inventory Costs & Quantity Master (Summa *~
            * #09 ! STORNAME ! Store Master File                        *~
            * #10 ! PIPOUT   ! Planned inventory use detail rec         *~
            * #11 ! SYSFILE2 ! Caelus Management System Information     *~
            * #12 ! USERINFO ! User's Posting Dates                     *~
            * #13 ! HNYLOCNS ! Location Quantity Detail File            *~
            * #14 ! LOCATION ! Location Master File                     *~
            * #15 ! PIPMASTR ! Planned Inventory Position Master File   *~
            * #41 ! SFCUM2   ! Cumulative sales forecast file           *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "JBMASTR2",                                      ~
                        varc,     indexed,  recsize = 1300,              ~
                        keypos =    1, keylen =   8,                     ~
                        alt key  2, keypos =   58, keylen =  25, dup,    ~
                            key  1, keypos = 1120, keylen =  19, dup

            select #02, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  3, keypos =   26, keylen =  32, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  1, keypos =  102, keylen =   9, dup

            select #03, "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44

            select #09, "STORNAME",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 3

            select #10, "PIPOUT",                                        ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =   20, keylen =  37

            select #11, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #12, "USERINFO",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos = 1, keylen =  3

            select #13, "HNYLOCNS",                                      ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos = 1, keylen = 42,                         ~
                        alt key  1, keypos =  432, keylen =  42,         ~
                            key  2, keypos =  485, keylen =  42,         ~
                            key  3, keypos =  527, keylen =  42,         ~
                            key  4, keypos =  590, keylen =  42

            select #14, "LOCATION",                                      ~
                        varc,     indexed,  recsize =  400,              ~
                        keypos = 1, keylen = 11,                         ~
                        alt key  1, keypos =    4, keylen =  11

            select #15, "PIPMASTR",                                      ~
                        varc, indexed, recsize = 2024,                   ~
                        keypos = 2, keylen = 25,                         ~
                        alt key  1, keypos =  1, keylen =  26

            select #41, "SFCUM2",                                        ~
                        varc, indexed, recsize = 1985,                   ~
                        keypos = 1, keylen = 25

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01%), f2%(01%),   0%, rslt$(01%))
            call "OPENCHCK" (#02, fs%(02%), f2%(02%),   0%, rslt$(02%))
            call "OPENCHCK" (#03, fs%(03%), f2%(03%),   0%, rslt$(03%))
            call "OPENCHCK" (#09, fs%(09%), f2%(09%),   0%, rslt$(09%))
            call "OPENCHCK" (#10, fs%(10%), f2%(10%),   0%, rslt$(10%))
            call "OPENCHCK" (#11, fs%(11%), f2%(11%),   0%, rslt$(11%))
            call "OPENCHCK" (#12, fs%(12%), f2%(12%),   0%, rslt$(12%))
            call "OPENCHCK" (#15, fs%(15%), f2%(15%),   0%, rslt$(15%))
            call "OPENCHCK" (#41, fs%(41%), f2%(41%),   0%, rslt$(41%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$)
            date$ = date : call "DATEFMT" (date$)
            edtmessage$ = "To modify displayed values, position cursor "&~
                "to desired value & press (RETURN)."
            str(line2$,62%) = "JBBPCOMP: " & str(cms2v$,,8%)
            line10$ = "By-Product Part Code       Expected  Scrapped     ~
        ~ Good Str Lot    Future Yld"
            sum% = 10%                 /* Max lines per summary screen */
            mxl% = dim(byprodct$(), 1%)    /* Max line items permitted */

            call "READ100" (#12, userid$, f1%(12%))        /* USERINFO */
            if f1%(12%) <> 0% then goto L09230
                call "ASKUSER" (0%, "*** USER POSTING DATES MISSING ***",~
                     "Posting dates for User " & userid$ & " not found "&~
                     " in USERINFO.", " ", "Press (RETURN) to acknowled"&~
                     "ge and exit.")
                goto exit_program
L09230:     get #12 using L35240, sfcdate$, defstore$
            postdate$ = sfcdate$
            call "WHICHMON" (#11, postdate$, whichmonth%)
            call "DATEFMT" (postdate$)
            if whichmonth% > 0% then goto L09350
                call "ASKUSER" (0%, "*** INVALID POSTING DATE ***",      ~
                     "Your Shop Floor Posting Date- " & postdate$ & " i"&~
                     "s not in an Open Period.", "See your Administrato"&~
                     "r or run SYSDATES to correct the problem & try ag"&~
                     "ain.", "Press (RETURN) to acknowledge and exit.")
                goto exit_program

L09350:     call "PIPINDEX" (#11, " ", today%, u3%) /* Get System Date */
            if u3% = 0% then goto L09430
                u3% = 0%
                call "ASKUSER" (u3%, "*** PLANNING CALENDAR ERROR ***",  ~
                     "There is an error in your Planning Calendar.", " ",~
                     "Press any PF key to acknowledge and abort.")
                goto exit_program

L09430:     modno$ = "03"
            jnlid$ = "MPW"
            ll% = len(str(joblotnr$))

*        Determine status of Lot Tracking uniqueness.
            lotunique$ = "U"           /* Deny forced Lot # assignment */
            call "READ100" (#11, "SWITCHS.HNY", f1%(11%))  /* SYSFILE2 */
            if f1%(11%) <> 0% then get #11 using L09500, lotunique$
L09500:         FMT POS(93), CH(1)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 4%
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                     if enabled% = 0% then goto L10220
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                     if keyhit% =  1% then gosub startover
                     if keyhit% <> 4% then goto L10200
L10150:                   fieldnr% = max(1%, fieldnr% - 1%)
                          gosub'051(fieldnr%)
                          if enabled% = 1% then goto L10120
                          if fieldnr% = 1% then goto L10100
                          goto L10150
L10200:              if keyhit% = 16% and fieldnr% = 1% then exit_program
                     if keyhit% <> 0% then goto L10120
L10220:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                     if errormsg$ <> " " then goto L10120
            next fieldnr%
            if p% = 0% then goto editpg1 else goto display_pipout_summary

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'102(1%, 0%, 2%)         /* Display Screen - No Entry */
                if keyhit%  =  1% then gosub startover
                if keyhit% <>  2% then goto L11130
                     d% = max(1%, d%) /* Continue where lines left off */
                     goto display_pipout_summary_2
L11130:         if keyhit%  = 11% then goto append_by_products
                if keyhit%  = 16% then goto datasave
                if keyhit% <>  0% then goto editpg1
L11160:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 2% or fieldnr% > 4% then goto editpg1
            if fieldnr% = lastfieldnr% then goto editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                if enabled% =  0% then goto editpg1
L11210:     gosub'102(1%, fieldnr%, 2%)              /* Display/Accept */
                if keyhit%  =  1% then gosub startover
                if keyhit% <>  0% then goto L11210
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                if errormsg$ <> " " then goto L11210
                lastfieldnr% = fieldnr%
            goto L11160

        display_pipout_summary
            d% = 1%                    /* Begin at the first line item */
        display_pipout_summary_2
            lastfieldnr% = 0%
            gosub'102(2%, 0%, 2%)
            if keyhit% =  1% then gosub startover
            if keyhit% =  2% then d% = 1%                /* First page */
            if keyhit% =  3% then d% = zln%               /* Last page */
            if keyhit% =  4% then d% = max(1%, d% - sum%)   /* Prev pg */
            if keyhit% =  5% then d% = min(zln%, d% + sum%) /* Next pg */
            if keyhit% =  6% then d% = max(1%, d% - 1%)        /* Down */
            if keyhit% =  7% then d% = min(zln%, d% + 1%)        /* Up */
            if keyhit% =  9% then goto editpg1          /* Edit Header */
            if keyhit% = 11% then goto append_by_products
            if keyhit% = 16% then goto datasave
            if keyhit% = 10% then goto L11470       /* Enter/Edit Costs */
            if keyhit% = 24% then goto L11470/* Distribute to locations */
            if keyhit% <> 0% then goto display_pipout_summary_2
L11470
*        User wants to edit an existing line item, based on DLN%.
            gosub call_screen
            dx% = cursor%(1%) - 10%             /* Screen line to edit */
            if dx% < 1% or dx% > sum% then goto display_pipout_summary_2
            if dx% + d% - 1% > p% then goto display_pipout_summary_2
            if dx% = lastfieldnr% then goto display_pipout_summary_2
            dln% = dx% + d% - 1%        /* The array element # to edit */
            if keyhit% <> 10% then goto L11570
                gosub'211("E", dln%)                  /* Call HNYCDIST */
                goto display_pipout_summary_2
L11570:     if keyhit% <> 24% then goto L11600
                gosub'212(dln%)                       /* Call HNYLCSUB */
                goto display_pipout_summary_2
L11600:     gosub'052(dln%)
L11610:     gosub'102(2%, dx%, 1%)
            if keyhit% =  1% then gosub startover
            if keyhit% = 10% then gosub'211("E", dln%)/* Call HNYCDIST */
            if keyhit% = 24% then gosub'212(dln%)     /* Call HNYLCSUB */
            if keyhit% =  4% then goto L11680   /* Validate & Prev Line */
            if keyhit% =  5% then goto L11680   /* Validate & Next Line */
            if keyhit% <> 0% then goto L11610
L11680:     gosub'152(dln%)            /* Edit Fields for Valid Entry  */
            if errormsg$ <> " " then goto L11610
            lastfieldnr% = dx%
            if keyhit% <> 4% then goto L11760  /* Validate & Prev Line? */
                dln% = max(1%, dln% - 1%) /* Array element to edit/val */
                if dln% < d% then d% = max(1%, d% - 1%)/* Scroll down? */
                dx% = dln% - d% + 1% /* Screen line to enable for edit */
                goto L11600
L11760:     if keyhit% <> 5% then goto L11810  /* Validate & Next Line? */
                dln% = min(p%, dln% + 1%) /* Array element to edit/val */
                if dln% > sum%+d%-1% then d%=min(zln%,d%+1%)/* Scr up? */
                dx% = dln% - d% + 1% /* Screen line to enable for edit */
                goto L11600
L11810:     goto L11470

        append_by_products
            if p% >= mxl% then goto append_by_products_exit_2/* Max'd? */
            p% = p% + 1%   /* No- Get the array element # for the item */

        append_by_products_2                /* Restart Line comes here */
            gosub clear_by_product_element

            for fieldnr% = 1% to 6%
L11890:         gosub'053(fieldnr%)        /* Default / Enables */
                     if enabled% = 0% then goto L12040
L11910:         gosub'103(fieldnr%, 1%)    /* Display / Accept  */
                     if keyhit%  = 1% then gosub startover
                     if keyhit%  = 2% then goto append_by_products_2
                     if keyhit% <> 4% then goto L11990
L11940:                   fieldnr% = max(1%, fieldnr% - 1%)
                          gosub'053(fieldnr%)
                          if enabled% = 1% then goto L11910
                          if fieldnr% = 1% then goto L11890
                          goto L11940
L11990:              if keyhit% =  10% then gosub'211("E", p%)
                     if keyhit% =  24% then gosub'212(p%)
                     if keyhit% =  16% and fieldnr% = 1%                 ~
                          then goto append_by_products_exit
                     if keyhit% <>  0% then goto L11910
L12040:         gosub'153(fieldnr%)     /* Edit Field for Valid Entry */
                     if errormsg$ <> " " then goto L11910
            next fieldnr%
            gosub edit_by_products
            goto append_by_products

        append_by_products_exit
            gosub clear_by_product_element
            p% = p% - 1%
        append_by_products_exit_2
            gosub compute_zln
            errormsg$ = " "
            goto display_pipout_summary

        edit_by_products
            lastfieldnr% = 0%
            gosub'103(0%, 2%)           /* Display Screen - No Entry   */
                if keyhit%  =  1% then gosub startover
                if keyhit%  =  2% then goto append_by_products_2
                if keyhit%  = 10% then gosub'211("E", p%)  /* HNYCDIST */
                if keyhit%  = 24% then gosub'212(p%)       /* HNYLCSUB */
                if keyhit%  = 16% then return
                if keyhit% <>  0% then goto edit_by_products
L12260:     fieldnr% = cursor%(1%) - 10%
            if fieldnr% < 1% or fieldnr% > 6% then goto edit_by_products
            if fieldnr% = lastfieldnr% then goto edit_by_products
            gosub'053(fieldnr%)         /* Check Enables, Set Defaults */
                if enabled% =  0% then goto edit_by_products
L12310:     gosub'103(fieldnr%, 2%)     /* Display & Accept Screen     */
                if keyhit%  = 1% then gosub startover
                if keyhit% <> 0% then goto L12310
            gosub'153(fieldnr%)         /* Edit Field for Valid Entry  */
                if errormsg$ <> " " then goto L12310
                lastfieldnr% = fieldnr%
            goto L12260

        REM *************************************************************~
            *     M I S C E L L A N E O U S   S U B R O U T I N E S     *~
            *************************************************************

        clear_by_product_element          /* Clear 1 element of arrays */
            init(" ") errormsg$, inpmessage$, byprodct$(p%), byprodsc$,  ~
                lotnumbr$(p%), qtyfutur$(p%), pipoutdt$(p%), strnrdsc$,  ~
                qtyscrap$(p%), qtyxpect$(p%), qty_good$(p%),             ~
                storenbr$(p%), pipoutky$(p%)
            qtyfutur(p%,1%), qtyfutur(p%,2%), qtyscrap(p%), qtyxpect(p%),~
                qty_good(p%) = 0
            lotenabl%(p%), pipoutdt%(p%) = 0%
            return

        compute_zln
            zln% = max(1%, p% - sum% + 1%)   /* SUM% is computed above */
            return

        call_screen
            call "GETSCRN" ("C", " ", cursor%(), 0%)
            return

        deffn'200(temp%, t)        /* RJ Qtys for display; LJ for edit */
            call "CONVERT" (qtyscrap(temp%),    2.2*t, qtyscrap$(temp%))
            call "CONVERT" (qty_good(temp%),    2.2*t, qty_good$(temp%))
            call "CONVERT" (qtyfutur(temp%,1%), 2.2*t, qtyfutur$(temp%))
            return

        deffn'201(temp%)
*        Default Standard Cost into cost arrays.
            call "STCCOSTS" (byprodct$(temp%), " ", #11, 2%,             ~
                avgcost(temp%), wrkcost())
            put avgcost$(temp%) using L35250, wrkcost()
            return

        deffn'211(mode$, temp%) /* Call HNYCDIST for cost distribution */
            call "DESCRIBE" (#02, byprodct$(temp%), byprodsc$, 1%, 0,    ~
                 f1%(2%))                                  /* HNYMASTR */
            if f1%(2%) = 0% then byprodsc$ = "(Part Code not on file)"
            call "HNYCDIST" (mode$, byprodct$(temp%), byprodsc$,         ~
                str(line2$,,60%), #11, avgcost$(temp%), temp$,           ~
                avgcost(temp%))
            return

        deffn'212(temp%) /* Call HNYLCSUB for possible inventory moves */
            call "HNYLCSUB" (byprodct$(temp%), storenbr$(temp%),         ~
                lotnumbr$(temp%), qty_good(temp%), 3%, #11, #09, #12,    ~
                #02, #13, #03, #14, " ", " ")
            return

        deffn'213(temp%)  /* Determine if the By-Product P/N is a tool */
            tool% = 1%                 /* Assume tool -- don't want it */
            call "READ100" (#02, byprodct$(temp%), f1%(2%))/* HNYMASTR */
            if f1%(2%) = 0% then return/* Eliminate HNYMASTR not found */
            get #02 using L35120, temp$           /* HNYMASTR Part Type */
            convert temp$ to u3%, data goto L15590/* Eliminate bad data */
            if u3% > 489% and u3% < 500% then return    /* It's a tool */
            if u3% > 789% and u3% < 800% then return    /* It's a tool */
            tool% = 0%      /* Gor, blimey, it's NOT a tool -- keep it */
L15590:     return

        deffn'222(t)              /* Calls PIPFLAGS; Balances PIP, eh? */
            call "PIPFLAGS" (byprodct$(temp%), today%, pipoutdt%(temp%), ~
                t, #15, #41)
            return

        deffn'205(a%) /* See if there are duplicate Lots for diff P/Ns */
            errormsg$ = " "                                     /* JIC */
            if lotunique$ <> "U" then return        /* It don't matter */

            mat loc% = zer                                 /* Matlock? */
            search str(lotnumbr$())=str(lotnumbr$(a%)) to loc%() step ll%
            for b% = 1% to dim(loc%(), 1%)
                if loc%(b%) = 0% then return     /* Zero is A-OK, fine */
                loc%(b%) = ((loc%(b%)-1%)/ll%)+1%/* Posn now element # */
                if loc%(b%) = a% then goto L15820  /* Self not an error */
                if byprodct$(loc%(b%)) = byprodct$(a%)                   ~
                     then goto L15820/* Same ByProduct P/N not an error */
                     errormsg$ = "Lot " & lotnumbr$(a%) & " is duplicat"&~
                          "ed between non-identical ByProducts."
                     return
L15820:     next b%
            return                                       /* A-OK, fine */

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
*        First, get some journal stuff. Once.
            if journal% <> 0% then goto L19140
                journal% = 1%
                u3% = 0%
                call "JNLINFO" (modno$, jnlid$, pstseq%, summary$,       ~
                     title$, sfcdate$, #11, f1%(11%), u3%)

L19140
*        Next, see if we have to update the Job via the Qty Remaining.
            if qtyrmain = qtybuild then goto L19210     /* Not if equal */
                call "JB2TIF" ("J2", 0%, 0%, 10%, hex(15), joborder$,    ~
                     modno$, jnlid$, pstseq%, userid$, sfcdate$,         ~
                     buildprt$, " ", " ", qtybuild - qtyrmain, " ", " ", ~
                     " ")

L19210
*        Process the PIPOUTs as managed by the user. Scrap is ignored.
            if p% < 1% then goto L19620       /* Are there any PIPOUTs? */
            for temp% = 1% to p%                               /* Yup! */
                temp = (qtyfutur(temp%,2%) + qty_good(temp%))
                if pipoutky$(temp%) <> " " then goto L19350/* Appended? */
                if temp = 0 then goto L19470      /* Yup- Qty to write? */
*        The item IS appended and HAS Future + Good- Create a PIPOUT.
                     if pipoutdt%(temp%) = 0%                            ~
                          then pipoutdt%(temp%) = today%
                     write #10 using L35140, jbtagnbr$, byprodct$(temp%), ~
                          pipoutdt%(temp%), time, -temp, eod goto L19470
                     gosub'222(temp)               /* Balance PIPMASTR */
                     goto L19470  /* Continue processing the 'Good' Qty */

L19350
*        Read PIPOUT record and adjust quantity (or delete if complete).
                call "READ101" (#10, pipoutky$(temp%),       /* PIPOUT */~
                     f1%(10%))
                if f1%(10%) = 0% then goto L19470/* Very, very bad news */
*        Remember, the PIPOUT quantities are negative by definition.
                     get #10 using L35230, temp2      /* Get 'Expected' */
                     put #10 using L35230, -temp      /* Put 'Adjusted' */
                     if temp = 0/* Then determine what to do to PIPOUT */~
                          then delete #10     /* Extinguish the PIPOUT */~
                          else rewrite #10 /* Update/Adjust the PIPOUT */
                     gosub'222(temp + temp2)       /* Balance PIPMASTR */

L19470
*        Write to JBTIF if there is a 'Good' quantity (for inventory).
                if qty_good(temp%) = 0 then goto L19590     /* No JBTIF */
*        Move costs to workarea for compression.
                get avgcost$(temp%) using L35250, wrkcost()
*        'Compress' costs for disk compression.
                call "PACKZERO" (wrkcost(), avgcost$(temp%))
                call "JB2TIF" ("J2", 0%, 0%, 4%, hex(15), joborder$,     ~
                     modno$, jnlid$, pstseq%, userid$, sfcdate$,         ~
                     byprodct$(temp%), storenbr$(temp%),                 ~
                     lotnumbr$(temp%), qty_good(temp%), " ", " ",        ~
                     avgcost$(temp%))

L19590
*        And finally, bump to the next PIPOUT.
            next temp%

L19620
*        DATASAVE process continues.
            call "JB2TIF" ("J2", 2%, u3%)     /* Send Hullo to BG Task */
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20140,         /* Job Order No.          */~
                              L20170,         /* Qty Remaining to Build */~
                              L20220,         /* Default Store No.      */~
                              L20260          /* Default Lot No.        */
            return

L20140: REM Def/Enable Job Order No.               JOBORDER$
            return

L20170: REM Def/Enable Quantity Remaining to Build QTYRMAIN$
            if qtyrmain$ = " " then qtyrmain = qtybuild
            call "CONVERT" (qtyrmain, -2.2, qtyrmain$)
            return

L20220: REM Def/Enable Default Store No.           JOBSTORE$
            if jobstore$ = " " then jobstore$ = defstore$
            return

L20260: REM Def/Enable Default Lot No.             JOBLOTNR$
            if joblotnr$ = " " then joblotnr$ = deflotnr$
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  2  of Input. *~
            *************************************************************

        deffn'052(u3%)
            gosub'200(u3%, -1)
            if storenbr$(u3%) = " " then storenbr$(u3%) = jobstore$
            if lotnumbr$(u3%) = " " then lotnumbr$(u3%) = joblotnr$
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   3     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  3  of Input. *~
            *************************************************************

        deffn'053(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L24160,         /* By-Product Part No.    */~
                              L24190,         /* Qty Scrapped           */~
                              L24230,         /* Good Qty               */~
                              L24270,         /* Store No.              */~
                              L24350,         /* Lot No.                */~
                              L24430          /* Future Yield Qty & Date*/
            return

L24160: REM Def/Enable By-Product Part No.         BYPRODCT$()
            return

L24190: REM Def/Enable Qty Scrapped                QTYSCRAP$()
            call "CONVERT" (qtyscrap(p%), -2.2, qtyscrap$(p%))
            return

L24230: REM Def/Enable Good Qty                    QTY_GOOD$()
            call "CONVERT" (qty_good(p%), -2.2, qty_good$(p%))
            return

L24270: REM Def/Enable Store No.                   STORENBR$()
            if qty_good(p%) <> 0 then goto L24320
                storenbr$(p%) = " "
                enabled% = 0%
                return
L24320:     if storenbr$(p%) = " " then storenbr$(p%) = jobstore$
            return

L24350: REM Def/Enable Lot No.                     LOTNUMBR$()
            if qty_good(p%) <> 0 then goto L24391
L24370:         lotnumbr$(p%) = " "
                enabled% = 0%
                return
L24391:     if lotenabl%(p%) = 0% then goto L24370
            if lotnumbr$(p%) = " " then lotnumbr$(p%) = joblotnr$
            return

L24430: REM Def/Enable Future Yield Qty        QTYFUTUR$() & PIPOUTDT$()
            call "CONVERT" (qtyfutur(p%,1%), -2.2, qtyfutur$(p%))
            if pipoutdt$(p%) <> " " and pipoutdt$(p%) <> blankdate$ then return
                pipoutdt$(p%) = date
                call "DATEFMT" (pipoutdt$(p%))
                return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28130
                inpmessage$ = edtmessage$
                if scrnr% = 2% then inpmessage$ = "Place cursor at desi"&~
                     "red By-Product, then press (RETURN) to edit."
                return

L28130
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            if scrnr% = 2% then restore line = scrn2_msg, 1%
            if scrnr% = 3% then restore line = scrn3_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Job Order number, partial, blanks or '?' to see a list.",~
         "Enter Quantity Remaining to be Built (updates the Job Master fi~
        ~le).",                                                           ~
         "Enter Default Store number for Good By-Products, partial or '?'~
        ~ to see a list.",                                                ~
         "Enter Default Lot number for Good By-Products.               "

        scrn2_msg  :  data                                               ~
         "Edit By-Product data fields, then press (RETURN), PF(4) or PF(5~
        ~) to validate."

        scrn3_msg  :  data                                               ~
         "Enter By-Product Part number, partial or '?' to see a list.  ",~
         "Enter Quantity of By-Product Scrapped by this job.           ",~
         "Enter Quantity of Good By-Product from this job.             ",~
         "Enter Store number Good By-Product issued to, partial or '?' to~
        ~ see a list.",                                                   ~
         "Enter Lot Number Good By-Product issued to or '?' to see a list~
        ~.",                                                              ~
         "Enter Future Yield Quantity (if any) of By-Product -AND- Date O~
        ~ut."

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                buildprt$, byprodct$(), joblotnr$, jobstore$, end_date$, ~
                joborder$, lotnumbr$(), qtybuild$, qtyfutur$(),          ~
                qtyscrap$(), qtyxpect$(), qty_good$(), startdat$,        ~
                storenbr$(), pipoutky$(), storedsc$, builddsc$,          ~
                closedat$, jobdescr$, pipoutdt$(), qtyrmain$, byprodsc$, ~
                strnrdsc$, qtyrdesc$, deflotnr$
            init (hex(00)) avgcost$()
            mat avgcost  = zer
            mat qtyfutur = zer
            mat qtyscrap = zer
            mat qtyxpect = zer
            mat qty_good = zer
            mat pipoutdt% = zer
            mat lotenabl% = zer
            call "ALLFREE"
            call "JBINUSE" (" ", 1%)   /* Clears In-use Flag, if there */
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************

        dataload
*        Get appropriate data fields from JBMASTR2.
            get #01 using L35040, jbtagnbr$, buildprt$, qtybuild,         ~
                startdat$, closedat$, end_date$            /* JBMASTR2 */
            call "DATFMTC" (startdat$)
            call "DATFMTC" (end_date$)
            call "DESCRIBE" (#02, buildprt$, builddsc$, 1%,/* HNYMASTR */~
                f1%(2%))
            if f1%(2%) = 0% then builddsc$ = "(Part not on file)"
            call "CONVERT" (qtybuild, 2.2, qtybuild$)

*        Read appropriate PIPOUTs into arrays.
            p% = 0%        /* 'P%' contains the # of PIPOUTs in arrays */
            plowkey$ = xor plowkey$                            /* ZOT! */
            str(plowkey$,,19%) = str(jbtagnbr$)

L30220
*        This is the top of the READ loop for PIPOUT.
            p% = p% + 1%        /* Increment the array element counter */
            if p% > mxl% then goto dataload_exit  /* Arrays max'd out? */

L30260
*        Return here if the PIPOUT is not wanted for any reason.
            byprodct$(p%) = " " : qtyxpect(p%), pipoutdt%(p%) = 0
            call "PLOWNEXT" (#10, plowkey$, 19%, f1%(10%))   /* PIPOUT */
                if f1%(10%) = 0% then goto dataload_exit
            get #10 using L35210, byprodct$(p%), pipoutdt%(p%),           ~
                qtyxpect(p%)                                 /* PIPOUT */
            if qtyxpect(p%) >= 0 then goto L30260    /* Qty must be neg */
            gosub'213(p%)                        /* See if it's a tool */
            if tool% <> 0% then goto L30260         /* No tools allowed */

*        The PIPOUT is a keeper! Save data in arrays.
            call "LOTENABL" (byprodct$(p%), lotenabl%(p%), ll%, #11, #02)
            pipoutky$(p%) = plowkey$        /* Save PIPOUT Primary Key */
            qtyxpect(p%) = -qtyxpect(p%)/* Let's be positive on screen */
            qtyfutur(p%,2%) = qtyxpect(p%)    /* 'Bookkeeping' Fut Yld */
            call "CONVERT" (qtyxpect(p%), 2.2, qtyxpect$(p%))
            gosub'200(p%, 1)
            gosub'201(p%)         /* Get Standard Costs for By-Product */
            goto L30220              /* This PIPOUT stays in the arrays */

        dataload_exit
            p% = p% - 1%            /* Actual # of array elements used */
            gosub compute_zln       /* Compute 'highest top of screen' */
            return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *************************************************************

L35040:     FMT /* File #01- JBMASTR2                                  */~
                POS(39), CH(19),         /* Tag Number for PIPOUT      */~
                CH(25),                  /* Part Number to Build       */~
                PD(14,4),                /* Quantity to Build          */~
                POS(147), CH(6),         /* Date Actually Started      */~
                CH(6),                   /* Date Actually Ended (Closed*/~
                POS(174), CH(6)          /* Scheduled Completion Date  */

L35120:     FMT POS(180), CH(3)        /* File #02- HNYMASTR Part Type */

L35140:     FMT /* File #10- PIPOUT                                    */~
                CH(19),                  /* Tag Number                 */~
                CH(25),                  /* Part Number                */~
                BI(04),                  /* Date Out                   */~
                CH(08),                  /* System Time                */~
                PD(14,4)                 /* Quantity                   */

L35210:     FMT POS(20), CH(25), BI(4), POS(57),   /* File #10- PIPOUT */~
                PD(14,4)
L35230:     FMT POS(57), PD(14,4)                  /* File #10- PIPOUT */
L35240:     FMT POS(34), CH(6), POS(64), CH(3)   /* File #12- USERINFO */
L35250:     FMT 12*PD(14,4)                         /* 12 Cost buckets */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
            gosub'050(1%, fieldnr%)
            gosub set_pf1
            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            lfac$(1%) = hex(8c)
            on fieldnr% gosub L40190,         /* Job Order No.          */~
                              L40200,         /* Qty Remaining to Build */~
                              L40190,         /* Default Store No.      */~
                              L40190          /* Default Lot No.        */
            goto L40220

            lfac$(fieldnr%) = hex(80) : return  /* Up / Low   */
L40190:     lfac$(fieldnr%) = hex(81) : return  /* Upper Only */
L40200:     lfac$(fieldnr%) = hex(82) : return  /* Numeric    */

L40220:     accept                                                       ~
                at (01,02), "Report Completion of By-Products from Jobs",~
                at (01,66), "Today:",                                    ~
                at (01,73), fac(hex(8c)), date$                 , ch(08),~
                at (02,02), fac(hex(ac)), line2$                , ch(79),~
                at (03,02), fac(hex(94)), errormsg$             , ch(79),~
                                                                         ~
                at (04,02), "Job Order No.",                             ~
                at (04,20), fac(lfac$( 1%)), joborder$          , ch(08),~
                at (04,46), fac(hex(8c)),    jobdescr$          , ch(32),~
                                                                         ~
                at (05,02), "Building Part No.",                         ~
                at (05,20), fac(hex(8c)),    buildprt$          , ch(25),~
                at (05,46), fac(hex(8c)),    builddsc$          , ch(34),~
                                                                         ~
                at (06,02), "Quantity to Build",                         ~
                at (06,20), fac(hex(8c)),    qtybuild$          , ch(10),~
                at (06,46), "Start",                                     ~
                at (06,52), fac(hex(8c)),    startdat$          , ch(10),~
                at (06,63), "Compl",                                     ~
                at (06,69), fac(hex(8c)),    end_date$          , ch(10),~
                                                                         ~
                at (07,02), "Remaining Qty",                             ~
                at (07,20), fac(lfac$( 2%)), qtyrmain$          , ch(10),~
                at (07,46), fac(hex(8c)),    qtyrdesc$          , ch(32),~
                                                                         ~
                at (08,02), "Default Store No.",                         ~
                at (08,20), fac(lfac$( 3%)), jobstore$          , ch(03),~
                at (08,46), fac(hex(8c)),    storedsc$          , ch(32),~
                                                                         ~
                at (09,02), "Default Lot No.",                           ~
                at (09,20), fac(lfac$( 4%)), joblotnr$          , ch(06),~
                                                                         ~
                at (10,02), fac(hex(ac)), line10$               , ch(79),~
                                                                         ~
                at (21,02), fac(hex(a4)),   inpmessage$         , ch(79),~
                at (22,02), fac(hex(8c)),   pf$(1%)             , ch(79),~
                at (23,02), fac(hex(8c)),   pf$(2%)             , ch(79),~
                at (24,02), fac(hex(8c)),   pf$(3%)             , ch(79),~
                                                                         ~
                keys(pfkeys$), key(keyhit%)

            if keyhit% <> 13% then L40670
                call "MANUAL" ("JBBPCOMP") : goto L40220

L40670:     if keyhit% <> 15% then L40700
                call "PRNTSCRN" : goto L40220

L40700:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return

        set_pf1
            str(line2$,,61%) = "Job Order Selection & Maintenance"
        if edit% = 2% then L40900     /*  Input Mode             */
            pf$(1%) = "(1)Start Over (4)Prev Field             " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40860
                str(pf$(3%),64%)     = " " : str(pfkeys$,16%,1%) = hex(ff)
L40860:     if fieldnr% > 2% then L40880
                str(pf$(1%),15%,13%) = " " : str(pfkeys$, 4%,1%) = hex(ff)
L40880:     return

L40900: if fieldnr% > 0% then L41030  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "(2)Edit Line Items                      " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                            (11)Add By-P" &       ~
                      "roducts                (16)Save Data   "
            pfkeys$ = hex(0102ffffffffffffffff0bff0dff0f1000)
            if p% <> 0% then goto L41000
                str(pf$(2%),,18%) = " " : str(pfkeys$,2%,1%) = hex(ff)
L41000:     if p% < mxl% then goto L41020
                str(pf$(3%),29%,19%) = " " : str(pfkeys$,11%,1%) = hex(ff)
L41020:     return
L41030:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(mode%, fieldnr%, edit%)
            gosub'050(mode%, fieldnr%)
            on mode% gosub set_pf1, set_pf2
            init (hex(8c)) afac$(), bfac$(), nfac$(), xfac$()
            if mode% = 2% then goto L42180      /* What are we editing? */
*        We're editing the Header fields.
                if fieldnr% = 0% then init (hex(86)) xfac$()
                on fieldnr% gosub , L42170, L42160, L42160
                goto L42250
                     xfac$(fieldnr%) = hex(80) : return  /* Up / Low   */
L42160:              xfac$(fieldnr%) = hex(81) : return  /* Upper Only */
L42170:              xfac$(fieldnr%) = hex(82) : return  /* Numeric    */
L42180
*        We're editing the Line Items.
            if fieldnr% = 0% then goto L42230
                afac$(fieldnr%) = hex(81)
                if lotenabl%(d%+fieldnr%-1%) <> 0%                       ~
                     then bfac$(fieldnr%) = hex(81)
                nfac$(fieldnr%) = hex(82)
                goto L42250
L42230:     init (hex(86)) afac$(), bfac$(), nfac$()

L42250:     accept                                                       ~
                at (01,02), "Report Completion of By-Products from Jobs",~
                at (01,66), "Today:",                                    ~
                at (01,73), fac(hex(8c)), date$                 , ch(08),~
                at (02,02), fac(hex(ac)), line2$                , ch(79),~
                at (03,02), fac(hex(94)), errormsg$             , ch(79),~
                                                                         ~
                at (04,02), "Job Order No.",                             ~
                at (04,20), fac(hex(8c)),    joborder$          , ch(08),~
                at (04,46), fac(hex(8c)),    jobdescr$          , ch(32),~
                                                                         ~
                at (05,02), "Building Part No.",                         ~
                at (05,20), fac(hex(8c)),    buildprt$          , ch(25),~
                at (05,46), fac(hex(8c)),    builddsc$          , ch(34),~
                                                                         ~
                at (06,02), "Quantity to Build",                         ~
                at (06,20), fac(hex(8c)),    qtybuild$          , ch(10),~
                at (06,46), "Start",                                     ~
                at (06,52), fac(hex(8c)),    startdat$          , ch(10),~
                at (06,63), "Compl",                                     ~
                at (06,69), fac(hex(8c)),    end_date$          , ch(10),~
                                                                         ~
                at (07,02), "Remaining Qty",                             ~
                at (07,20), fac(xfac$( 2%)), qtyrmain$          , ch(10),~
                at (07,46), fac(hex(8c)),    qtyrdesc$          , ch(32),~
                                                                         ~
                at (08,02), "Default Store No.",                         ~
                at (08,20), fac(xfac$( 3%)), jobstore$          , ch(03),~
                at (08,46), fac(hex(8c)),    storedsc$          , ch(32),~
                                                                         ~
                at (09,02), "Default Lot No.",                           ~
                at (09,20), fac(xfac$( 4%)), joblotnr$          , ch(06),~
                                                                         ~
                at (10,02), fac(hex(ac)), line10$               , ch(79),~
                                                                         ~
                at (11,02), fac(hex(8c)),    byprodct$(d%)      , ch(25),~
                at (11,28), fac(hex(8c)),    qtyxpect$(d%)      , ch(09),~
                at (11,38), fac(nfac$( 1%)), qtyscrap$(d%)      , ch(09),~
                at (11,48), fac(nfac$( 1%)), qty_good$(d%)      , ch(09),~
                at (11,58), fac(afac$( 1%)), storenbr$(d%)      , ch(03),~
                at (11,62), fac(bfac$( 1%)), lotnumbr$(d%)      , ch(06),~
                at (11,70), fac(nfac$( 1%)), qtyfutur$(d%)      , ch(09),~
                                                                         ~
                at (12,02), fac(hex(8c)),    byprodct$(d%+1%)   , ch(25),~
                at (12,28), fac(hex(8c)),    qtyxpect$(d%+1%)   , ch(09),~
                at (12,38), fac(nfac$( 2%)), qtyscrap$(d%+1%)   , ch(09),~
                at (12,48), fac(nfac$( 2%)), qty_good$(d%+1%)   , ch(09),~
                at (12,58), fac(afac$( 2%)), storenbr$(d%+1%)   , ch(03),~
                at (12,62), fac(bfac$( 2%)), lotnumbr$(d%+1%)   , ch(06),~
                at (12,70), fac(nfac$( 2%)), qtyfutur$(d%+1%)   , ch(09),~
                                                                         ~
                at (13,02), fac(hex(8c)),    byprodct$(d%+2%)   , ch(25),~
                at (13,28), fac(hex(8c)),    qtyxpect$(d%+2%)   , ch(09),~
                at (13,38), fac(nfac$( 3%)), qtyscrap$(d%+2%)   , ch(09),~
                at (13,48), fac(nfac$( 3%)), qty_good$(d%+2%)   , ch(09),~
                at (13,58), fac(afac$( 3%)), storenbr$(d%+2%)   , ch(03),~
                at (13,62), fac(bfac$( 3%)), lotnumbr$(d%+2%)   , ch(06),~
                at (13,70), fac(nfac$( 3%)), qtyfutur$(d%+2%)   , ch(09),~
                                                                         ~
                at (14,02), fac(hex(8c)),    byprodct$(d%+3%)   , ch(25),~
                at (14,28), fac(hex(8c)),    qtyxpect$(d%+3%)   , ch(09),~
                at (14,38), fac(nfac$( 4%)), qtyscrap$(d%+3%)   , ch(09),~
                at (14,48), fac(nfac$( 4%)), qty_good$(d%+3%)   , ch(09),~
                at (14,58), fac(afac$( 4%)), storenbr$(d%+3%)   , ch(03),~
                at (14,62), fac(bfac$( 4%)), lotnumbr$(d%+3%)   , ch(06),~
                at (14,70), fac(nfac$( 4%)), qtyfutur$(d%+3%)   , ch(09),~
                                                                         ~
                at (15,02), fac(hex(8c)),    byprodct$(d%+4%)   , ch(25),~
                at (15,28), fac(hex(8c)),    qtyxpect$(d%+4%)   , ch(09),~
                at (15,38), fac(nfac$( 5%)), qtyscrap$(d%+4%)   , ch(09),~
                at (15,48), fac(nfac$( 5%)), qty_good$(d%+4%)   , ch(09),~
                at (15,58), fac(afac$( 5%)), storenbr$(d%+4%)   , ch(03),~
                at (15,62), fac(bfac$( 5%)), lotnumbr$(d%+4%)   , ch(06),~
                at (15,70), fac(nfac$( 5%)), qtyfutur$(d%+4%)   , ch(09),~
                                                                         ~
                at (16,02), fac(hex(8c)),    byprodct$(d%+5%)   , ch(25),~
                at (16,28), fac(hex(8c)),    qtyxpect$(d%+5%)   , ch(09),~
                at (16,38), fac(nfac$( 6%)), qtyscrap$(d%+5%)   , ch(09),~
                at (16,48), fac(nfac$( 6%)), qty_good$(d%+5%)   , ch(09),~
                at (16,58), fac(afac$( 6%)), storenbr$(d%+5%)   , ch(03),~
                at (16,62), fac(bfac$( 6%)), lotnumbr$(d%+5%)   , ch(06),~
                at (16,70), fac(nfac$( 6%)), qtyfutur$(d%+5%)   , ch(09),~
                                                                         ~
                at (17,02), fac(hex(8c)),    byprodct$(d%+6%)   , ch(25),~
                at (17,28), fac(hex(8c)),    qtyxpect$(d%+6%)   , ch(09),~
                at (17,38), fac(nfac$( 7%)), qtyscrap$(d%+6%)   , ch(09),~
                at (17,48), fac(nfac$( 7%)), qty_good$(d%+6%)   , ch(09),~
                at (17,58), fac(afac$( 7%)), storenbr$(d%+6%)   , ch(03),~
                at (17,62), fac(bfac$( 7%)), lotnumbr$(d%+6%)   , ch(06),~
                at (17,70), fac(nfac$( 7%)), qtyfutur$(d%+6%)   , ch(09),~
                                                                         ~
                at (18,02), fac(hex(8c)),    byprodct$(d%+7%)   , ch(25),~
                at (18,28), fac(hex(8c)),    qtyxpect$(d%+7%)   , ch(09),~
                at (18,38), fac(nfac$( 8%)), qtyscrap$(d%+7%)   , ch(09),~
                at (18,48), fac(nfac$( 8%)), qty_good$(d%+7%)   , ch(09),~
                at (18,58), fac(afac$( 8%)), storenbr$(d%+7%)   , ch(03),~
                at (18,62), fac(bfac$( 8%)), lotnumbr$(d%+7%)   , ch(06),~
                at (18,70), fac(nfac$( 8%)), qtyfutur$(d%+7%)   , ch(09),~
                                                                         ~
                at (19,02), fac(hex(8c)),    byprodct$(d%+8%)   , ch(25),~
                at (19,28), fac(hex(8c)),    qtyxpect$(d%+8%)   , ch(09),~
                at (19,38), fac(nfac$( 9%)), qtyscrap$(d%+8%)   , ch(09),~
                at (19,48), fac(nfac$( 9%)), qty_good$(d%+8%)   , ch(09),~
                at (19,58), fac(afac$( 9%)), storenbr$(d%+8%)   , ch(03),~
                at (19,62), fac(bfac$( 9%)), lotnumbr$(d%+8%)   , ch(06),~
                at (19,70), fac(nfac$( 9%)), qtyfutur$(d%+8%)   , ch(09),~
                                                                         ~
                at (20,02), fac(hex(8c)),    byprodct$(d%+9%)   , ch(25),~
                at (20,28), fac(hex(8c)),    qtyxpect$(d%+9%)   , ch(09),~
                at (20,38), fac(nfac$(10%)), qtyscrap$(d%+9%)   , ch(09),~
                at (20,48), fac(nfac$(10%)), qty_good$(d%+9%)   , ch(09),~
                at (20,58), fac(afac$(10%)), storenbr$(d%+9%)   , ch(03),~
                at (20,62), fac(bfac$(10%)), lotnumbr$(d%+9%)   , ch(06),~
                at (20,70), fac(nfac$(10%)), qtyfutur$(d%+9%)   , ch(09),~
                                                                         ~
                at (21,02), fac(hex(a4)),   inpmessage$         , ch(79),~
                at (22,02), fac(hex(8c)),   pf$(1%)             , ch(79),~
                at (23,02), fac(hex(8c)),   pf$(2%)             , ch(79),~
                at (24,02), fac(hex(8c)),   pf$(3%)             , ch(79),~
                                                                         ~
                keys(pfkeys$), key(keyhit%)

            if keyhit% <> 13% then L43500
                call "MANUAL" ("JBBPCOMP") : goto L42250

L43500:     if keyhit% <> 15% then L43530
                call "PRNTSCRN" : goto L42250

L43530:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return

        set_pf2
            str(line2$,,61%) = "By-Product Completion Reporting"
        if edit% = 2% then L43740
*        Edit Mode - Enabled.
            pf$(1%) = "(1)Start Over (4)Val/Prev               " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "              (5)Val/Next   (10)Enter/Ed" &       ~
                      "it Costs               (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "         (24)Locations                 "
            pfkeys$ = hex(01ffff0405ffffffff0affff0dff0fff1800)
            if dln% > 1% then goto L43700
                str(pf$(1%),15%,11%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L43700:     if dln% < p% then goto L43720
                str(pf$(2%),15%,11%) = " " : str(pfkeys$,5%,1%) = hex(ff)
L43720:     return

L43740
*        Edit Mode - Select Field.
            pf$(1%) = "(1)Start Over (4)Prev Page   (9)Edit Hea" &       ~
                      "der                    (13)Instructions"
            pf$(2%) = "(2)First Page (5)Next Page  (10)Enter/Ed" &       ~
                      "it Costs               (15)Print Screen"
            pf$(3%) = "(3)Last Page  (6)Down (7)Up (11)Add By-P" &       ~
                      "roducts  (24)Locations (16)Save Data   "
            pfkeys$ = hex(01020304050607ff090a0bff0dff0f101800)
*        If we're at the 'top' of the line items, DISable PF(2), 4 & 6.
            if d% <> 1% then goto L43880
                str(pf$(2%),,13%), str(pf$(1%),15%,12%),                 ~
                     str(pf$(3%),15%,7%) = " "
                str(pfkeys$,2%,1%), str(pfkeys$,4%,1%),                  ~
                     str(pfkeys$,6%,1%) = hex(ff)
L43880
*        If we're at the 'end' of the line items, DISable PF(3), 5 & 7.
            if d% <> zln% then goto L43940
                str(pf$(3%),,12%), str(pf$(2%),15%,12%),                 ~
                     str(pf$(3%),23%,5%) = " "
                str(pfkeys$,3%,1%), str(pfkeys$,5%,1%),                  ~
                     str(pfkeys$,7%,1%) = hex(ff)
L43940
*        No APPEND if we already have the maximum # of line items.
            if p% < mxl% then goto L43970
                str(pf$(3%),29%,19%) = " " : str(pfkeys$,11%,1%) = hex(ff)
L43970:     return

        REM *************************************************************~
            *               S C R E E N   P A G E   3                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'103(fieldnr%, edit%)
            gosub'050(3%, fieldnr%)
            gosub set_pf3
            str(line2$,,61%) = "Additional By-Product Input & Edit"
            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            on fieldnr% gosub L44210,           /* By-Product Part No.  */~
                              L44220,           /* Qty Scrapped         */~
                              L44220,           /* Good Qty             */~
                              L44210,           /* Store No.            */~
                              L44210,           /* Lot No.              */~
                              L44220            /* Future Yield Qty/Date*/
            goto L44240

            lfac$(fieldnr%) = hex(80) : return  /* Up / Low   */
L44210:     lfac$(fieldnr%) = hex(81) : return  /* Upper Only */
L44220:     lfac$(fieldnr%) = hex(82) : return  /* Numeric    */
L44230:
L44240:     accept                                                       ~
                at (01,02), "Report Completion of By-Products from Jobs",~
                at (01,66), "Today:",                                    ~
                at (01,73), fac(hex(8c)), date$                 , ch(08),~
                at (02,02), fac(hex(ac)), line2$                , ch(79),~
                at (03,02), fac(hex(94)), errormsg$             , ch(79),~
                                                                         ~
                at (04,02), "Job Order No.",                             ~
                at (04,20), fac(hex(8c)),    joborder$          , ch(08),~
                at (04,46), fac(hex(8c)),    jobdescr$          , ch(32),~
                                                                         ~
                at (05,02), "Building Part No.",                         ~
                at (05,20), fac(hex(8c)),    buildprt$          , ch(25),~
                at (05,46), fac(hex(8c)),    builddsc$          , ch(34),~
                                                                         ~
                at (06,02), "Quantity to Build",                         ~
                at (06,20), fac(hex(8c)),    qtybuild$          , ch(10),~
                at (06,46), "Start",                                     ~
                at (06,52), fac(hex(8c)),    startdat$          , ch(10),~
                at (06,63), "Compl",                                     ~
                at (06,69), fac(hex(8c)),    end_date$          , ch(10),~
                                                                         ~
                at (07,02), "Remaining Qty",                             ~
                at (07,20), fac(hex(8c)),    qtyrmain$          , ch(10),~
                at (07,46), fac(hex(8c)),    qtyrdesc$          , ch(32),~
                                                                         ~
                at (08,02), "Default Store No.",                         ~
                at (08,20), fac(hex(8c)),    jobstore$          , ch(03),~
                at (08,46), fac(hex(8c)),    storedsc$          , ch(32),~
                                                                         ~
                at (09,02), "Default Lot No.",                           ~
                at (09,20), fac(hex(8c)),    joblotnr$          , ch(06),~
                                                                         ~
                at (11,02), "By-Product Part #",                         ~
                at (11,20), fac(lfac$( 1%)), byprodct$(p%)      , ch(25),~
                at (11,46), fac(hex(8c)),    byprodsc$          , ch(34),~
                                                                         ~
                at (12,02), "Qty Scrapped",                              ~
                at (12,20), fac(lfac$( 2%)), qtyscrap$(p%)      , ch(09),~
                                                                         ~
                at (13,02), "Good Qty",                                  ~
                at (13,20), fac(lfac$( 3%)), qty_good$(p%)      , ch(09),~
                                                                         ~
                at (14,02), "Store No.",                                 ~
                at (14,20), fac(lfac$( 4%)), storenbr$(p%)      , ch(03),~
                at (14,46), fac(hex(8c)),    strnrdsc$          , ch(32),~
                                                                         ~
                at (15,02), "Lot No.",                                   ~
                at (15,20), fac(lfac$( 5%)), lotnumbr$(p%)      , ch(06),~
                                                                         ~
                at (16,02), "Future Yield",                              ~
                at (16,20), fac(lfac$( 6%)), qtyfutur$(p%)      , ch(09),~
                at (16,46), "Date Out",                                  ~
                at (16,55), fac(lfac$( 6%)), pipoutdt$(p%)      , ch(10),~
                                                                         ~
                at (21,02), fac(hex(a4)),   inpmessage$         , ch(79),~
                at (22,02), fac(hex(8c)),   pf$(1%)             , ch(79),~
                at (23,02), fac(hex(8c)),   pf$(2%)             , ch(79),~
                at (24,02), fac(hex(8c)),   pf$(3%)             , ch(79),~
                                                                         ~
                keys(pfkeys$), key(keyhit%)

            if keyhit% <> 13% then L44890
                call "MANUAL" ("JBBPCOMP") : goto L44230

L44890:     if keyhit% <> 15% then L44920
                call "PRNTSCRN" : goto L44230

L44920:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return

        set_pf3
        if edit% = 2% then L45130     /*  Input Mode             */
            pf$(1%) = "(1)Start Over (4)Prev Field             " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "(2)Restart Line             (10)Enter/Ed" &       ~
                      "it Costs               (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "         (24)Locations (16)Return      "
            pfkeys$ = hex(0102ff04ffffffffff0affff0dff0f101800)
            if fieldnr% = 1% then L45070
                str(pf$(3%),64%)     = " " : str(pfkeys$,16%,1%) = hex(ff)
L45070:     if fieldnr% > 1% then L45110
                str(pf$(1%),15%,13%) = " " : str(pfkeys$, 4%,1%) = hex(ff)
                str(pf$(2%), 1%,15%) = " " : str(pfkeys$, 2%,1%) = hex(ff)
                str(pf$(2%),29%,20%) = " " : str(pfkeys$,10%,1%) = hex(ff)
                str(pf$(3%),50%,13%) = " " : str(pfkeys$,17%,1%) = hex(ff)
L45110:     return

L45130: if fieldnr% > 0% then L45220  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "(2)Restart Line             (10)Enter/Ed" &       ~
                      "it Costs               (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "         (24)Locations (16)Next Add    "
            pfkeys$ = hex(0102ffffffffffffff0affff0dff0f101800)
            return
L45220:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50140,         /* Job Order No.          */~
                              L50410,         /* Qty Remaining to Build */~
                              L50510,         /* Default Store No.      */~
                              L50620          /* Default Lot No.        */
            return

L50140: REM Test for Job Order No.                JOBORDER$
            jobdescr$ = " "
            if joborder$ = "?" then joborder$ = " "
            call "GETCODE" (#01, joborder$, jobdescr$, 1%, /* JBMASTR2 */~
                0, f1%(1%))
            if f1%(1%) <> 0% then goto L50240
                errormsg$ = "You must select an existing Job."
L50210:         buildprt$, builddsc$, startdat$, end_date$, qtybuild$    ~
                     = " "
                return
L50240:     u3% = 2%       /* Check In-use & Write In-use Flag if free */
            call "JBINUSE" (joborder$, u3%)
            if u3% = 0% then L50290
                errormsg$ = hex(00)
                goto L50210
L50290:     gosub dataload
            if closedat$ = " " or closedat$ = blankdate$ then return
                call "DATEFMT" (closedat$)
L50320:         u3% = 2%
                call "ASKUSER" (u3%, "*** THAT JOB IS CLOSED ***",       ~
                     "The job you selected (" & joborder$ & ") was clos"&~
                     "ed on " & closedat$ & ".", " ", "Press (RETURN) t"&~
                     "o select another job.")
                if u3% <> 0% then goto L50320
                     joborder$ = "?"
                     goto L50140

L50410: REM Test for Quantity Remaining to Build  QTYRMAIN$
            qtyrdesc$ = " "
            if qtyrmain$ = " " then qtyrmain$ = "0"
            call "NUMTEST" (qtyrmain$, 0, 9e9, errormsg$, -2.2, qtyrmain)
            if errormsg$ <> " " then return
                if qtyrmain = qtybuild                                   ~
                     then qtyrdesc$ = "(Job Master will NOT be updated)" ~
                     else qtyrdesc$ = "(Job Master WILL be updated)"
                return

L50510: REM Test for Default Store No.            JOBSTORE$
            storedsc$ = " "
            if jobstore$ = "?" then jobstore$ = " "
            call "GETCODE" (#09, jobstore$, storedsc$, 1%, /* STORNAME */~
                0, f1%(9%))
            if f1%(9%) <> 0% then goto L50590
                errormsg$ = "You must select a Default Store Code."
                return
L50590:     defstore$ = jobstore$
            return

L50620: REM Test for Default Lot No.              JOBLOTNR$
            deflotnr$ = joblotnr$
            errormsg$ = "LOT-CHECK"
            tpart$ = " "
            call "LOTVALID" (tpart$, jobstore$, joblotnr$, #11, #02, #03,~
                             errormsg$)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 2.                      *~
            *************************************************************

        deffn'152(u3%)
            errormsg$ = " "

*        Validate Scrap Qty. It drives the 'Good' and Future Yield Qties.
            if qtyscrap$(u3%) = " " then qtyscrap$(u3%) = "0"
            call "NUMTEST" (qtyscrap$(u3%), 0, 9e9, errormsg$, -2.2,     ~
                qtyscrap(u3%))
            if errormsg$ = " " then goto L52170
                errormsg$ = "Qty Scrapped: " & errormsg$
                return

L52170
*        Validate OR compute 'Good' Quantity OR honor user's entry.
            if qty_good$(u3%) <> " " then goto L52230         /* Blank? */
                qty_good$(u3%) = "0"          /* Yup- force it to zero */
                call "NUMTEST" (qty_good$(u3%), 0, 9e9, errormsg$, -2.2, ~
                     qty_good(u3%))
                if errormsg$ = " " then goto L52320 else goto L52260
L52230:     call "NUMTEST" (qty_good$(u3%), 0, 9e9, errormsg$, -2.2,     ~
                qty_good(u3%))           /* Nope- validate/honor entry */
            if errormsg$ = " " then goto L52280
L52260:         errormsg$ = "Good Qty: " & errormsg$
                return
L52280:     if qty_good(u3%) <> 0 then goto L52320  /* Zero? Compute it */
                 qty_good(u3%) = max(0, qtyxpect(u3%) - qtyscrap(u3%))
                 call "CONVERT" (qty_good(u3%), 2.2, qty_good$(u3%))

L52320
*        Validate OR zap Store & Lot based on 'Good' value.
            if qty_good(u3%) <> 0 then goto L52360
                storenbr$(u3%), lotnumbr$(u3%) = " "           /* ZAP! */
                goto L52500             /* Skip Store & Lot validations */
L52360:     if storenbr$(u3%) <> " " then goto L52390
                storenbr$(u3%) = jobstore$
                lotnumbr$(u3%) = joblotnr$
L52390:     if storenbr$(u3%) = "?" then storenbr$(u3%) = " "
            call "GETCODE" (#09, storenbr$(u3%), " ", 0%,  /* STORNAME */~
                0, f1%(9%))
            if f1%(9%) <> 0% then goto L52460
                errormsg$ = "You must select a Store Code for the Qty g"&~
                     "oing to inventory."
                return
L52460:     gosub'216(u3%)                    /* Validate Lot Tracking */
            if errormsg$ <> " " then return

L52500
*        Validate OR compute Future Yield OR honor user's entry.
            if qtyfutur$(u3%) <> " " then goto L52560         /* Blank? */
                qtyfutur$(u3%) = "0"          /* Yup- force it to zero */
                call "NUMTEST" (qtyfutur$(u3%), 0, 9e9, errormsg$, -2.2, ~
                     qtyfutur(u3%,2%))
                if errormsg$ = " " then goto L52620 else goto L52590
L52560:     call "NUMTEST" (qtyfutur$(u3%), 0, 9e9, errormsg$, -2.2,     ~
                qtyfutur(u3%,2%))        /* Nope- validate/honor entry */
            if errormsg$ = " " then goto L52610
L52590:         errormsg$ = "Future Yield Qty: " & errormsg$
                return
L52610:     if qtyfutur(u3%,2%) = qtyfutur(u3%,1%) then goto L52640
L52620:         qtyfutur(u3%,1%) = qtyfutur(u3%,2%)/* Changed-honor it */
                return
L52640:     if pipoutky$(u3%) = " " then return /* No comp if Appended */
            qtyfutur(u3%,2%) = max(0, qtyxpect(u3%) - (qtyscrap(u3%) +   ~
                qty_good(u3%)))        /* Compute Fut Yld if UNchanged */
            qtyfutur(u3%,1%) = qtyfutur(u3%,2%)/* Honor the computation*/
            call "CONVERT" (qtyfutur(u3%,2%), 2.2, qtyfutur$(u3%))
            return

        deffn'216(temp%)                      /* Validate Lot Tracking */
*        Validate the Lot Number for possible editing, based on the
*        value returned in LOTENABL%(TEMP%). 0 = DISabled; 1 = Memo;
*        2 = Strict Lot Tracking.
            on lotenabl%(temp%) + 1% goto L52760, L53021, L52780

L52760:     lotnumbr$(temp%) = " " : return         /* Not Lot Tracked */

L52780:     if lotnumbr$(temp%) <> " " then goto L52832       /* Strict */
                errormsg$ = "Lot #s for Strict Lot-Tracked parts may no"&~
                     "t be blank."
                return
            errormsg$ = "LOT-CHECK"
L52832:     call "LOTVALID" (byprodct$(temp%), storenbr$(temp%),         ~
                lotnumbr$(temp%), #11, #02, #03, errormsg$)
            if errormsg$ <> " " then return
            call "LOTUNQUE" (byprodct$(), lotnumbr$(), temp%, #11,       ~
                errormsg$)
            if errormsg$ <> " " then return
            gosub'205(temp%)                /* Test for duplicate Lots */
            if errormsg$ <> " " then return
            readkey$ = str(byprodct$(temp%)) & str(storenbr$(temp%)) &   ~
                lotnumbr$(temp%)
            call "READ100" (#03, readkey$, f1%(3%))         /* HNYQUAN */
                if f1%(3%) <> 0% then return
L52940:     u2% = 2%                           /* Window at the bottom */
            call "ASKUSER" (u2%, "*** LOT NUMBER ADVISORY ***",          ~
                "The Lot Number you entered (" & lotnumbr$(temp%) & ") "&~
                "does not currently exist.", "Press PF(16) to continue.",~
                "Press PF(1) to re-enter the Lot Number.")
            if u2% <>  1% then goto L53020
                errormsg$ = "Please re-enter the Lot Number."
                return
L53020:     if u2% <> 16% then goto L52940
L53021:     return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 3.                      *~
            *************************************************************

        deffn'153(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L54160,         /* By-Product Part No.    */~
                              L54400,         /* Qty Scrapped           */~
                              L54460,         /* Qty Good               */~
                              L54520,         /* Store No.              */~
                              L54640,         /* Lot No.                */~
                              L54720          /* Future Yield Qty & Date*/
            return

L54160: REM Test for By-Product Part No.          BYPRODCT$()
            byprodsc$ = " "
            if byprodct$(p%) = "?" then byprodct$(p%) = " "
            call "GETCODE" (#02, byprodct$(p%), byprodsc$, /* HNYMASTR */~
                1%, 0, f1%(2%))
            if f1%(2%) <> 0% then goto L54240
                errormsg$ = "You must select a By-Product Part Code."
                return
L54240:     gosub'213(p%)                        /* See if it's a tool */
            if tool% = 0% then goto L54280          /* No tools allowed */
                errormsg$ = "Tools are not allowed as By-Products."
                return
L54280:     if byprodct$(p%) <> buildprt$ then goto L54370
                errormsg$ = "Job Part # may not also be a By-Product."
                return
L54370:     gosub'201(p%)   /* Get Standard Costs for added By-Product */
            call "LOTENABL" (byprodct$(p%), lotenabl%(p%), ll%, #11, #02)
            return

L54400: REM Test for Qty Scrapped                 QTYSCRAP$()
            if qtyscrap$(p%) = " " then qtyscrap$(p%) = "0"
            call "NUMTEST" (qtyscrap$(p%), 0, 9e9, errormsg$, -2.2,      ~
                qtyscrap(p%))
            return

L54460: REM Test for Qty Good                     QTY_GOOD$()
            if qty_good$(p%) = " " then qty_good$(p%) = "0"
            call "NUMTEST" (qty_good$(p%), 0, 9e9, errormsg$, -2.2,      ~
                qty_good(p%))
            return

L54520: REM Test for Store No.                    STORENBR$()
            if qty_good(p%) <> 0 then goto L54560
                storenbr$(p%) = " "
                return
L54560:     if storenbr$(p%) = " " then storenbr$(p%) = jobstore$
            if storenbr$(p%) = "?" then storenbr$(p%) = " "
            call "GETCODE" (#09, storenbr$(p%), strnrdsc$, /* STORNAME */~
                1%, 0, f1%(9%))
            if f1%(9%) = 0% then errormsg$ = "You must select a Store C"&~
                "ode for the Qty going to inventory."
            return

L54640: REM Test for Lot No.                      LOTNUMBR$()
            if qty_good(p%) <> 0 then goto L54671
L54660:         lotnumbr$(p%) = " "
                return
L54671:     if lotenabl%(p%) = 0% then goto L54660
            gosub'216(p%)                     /* Validate Lot Tracking */
            return

L54720: REM Test for Future Yield Qty         QTYFUTUR$() & PIPOUTDT$()
            if qtyfutur$(p%) = " " then qtyfutur$(p%) = "0"
            call "NUMTEST" (qtyfutur$(p%), 0, 9e9, errormsg$, -2.2,      ~
                qtyfutur(p%,2%))
            if errormsg$ <> " " then return
            qtyfutur(p%,1%) = qtyfutur(p%,2%)   /* Honor input Fut Yld */
            if qtyfutur(p%,2%) <> 0 then goto L54820
                pipoutdt$(p%) = " "
                pipoutdt%(p%) = today%
                return
L54820:     call "DATEOK" (pipoutdt$(p%), u3%, errormsg$)
                if errormsg$ <> " " then return
            temp$ = pipoutdt$(p%)
            call "DATUNFMT" ( temp$ )
            call "PIPINDEX" (#11, str(temp$,,6%), pipoutdt%(p%), u3%)
                if u3% = 0% then return            /* Date is OK, fine */
            on u3% goto L54890, L54900,, L54910,,,, L54910
            errormsg$ = "Invalid Date entered. Try again."        :return
L54890:     errormsg$ = "Can't find MONTH OPEN record in SYSFILE2":return
L54900:     errormsg$ = "Date is outside Planning Calendar."      :return
L54910:     errormsg$ = "Error processing Date. Try again."       :return

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
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "SHOSTAT" ("One Moment Please")
            call "JBJNLCLS" ("J2", userid$, modno$, jnlid$, pstseq%, u3%)
            end
