        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  JJJJJ  BBBB   M   M  DDDD   EEEEE  K   K  IIIII  TTTTT   *~
            *    J    B   B  MM MM  D   D  E      K  K     I      T     *~
            *    J    BBBB   M M M  D   D  EEEE   KKK      I      T     *~
            *  J J    B   B  M   M  D   D  E      K  K     I      T     *~
            *   J     BBBB   M   M  DDDD   EEEEE  K   K  IIIII    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBMDEKIT - Automatically or selectively return materials  *~
            *            previously kitted to a job back to inventory   *~
            *            or move them to another job.                   *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1993  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/19/93 ! Original (PRR 11709)                     ! JIM *~
            * 08/12/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            avgcosts(12),                /* Component Extended Costs   */~
            avgcosts$(1000)96,           /* Component Unit Costs       */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            buildprt$25, builddsc$34,    /* Building Part No. & Descr. */~
            closedat$10,                 /* Job Close Date             */~
            column$79, colp$79, cold$79, /* Screen Column Headers      */~
            componnt$(1000)25,           /* Kitted Component Part      */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            datekitd$(1000)8,            /* Date Kitted to Job         */~
            datetime$(10)8,              /* Toggle Date or Time Kitted */~
            dekitpct$6,                  /* Edited Qty DeKit Percent   */~
            descript$(1000)32,           /* Kitted Component Descriptn */~
            destnprt$25,                 /* P/N of the Dest'n Job      */~
            edtmessage$79,               /* Edit screen message        */~
            end_date$10,                 /* Sch. Complete (Job)        */~
            errormsg$79,                 /* Error message              */~
            forcestr$3, forcelot$6,      /* Force Comps to these values*/~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            jnlid$3,                     /* G/L Journal ID             */~
            jobdestn$8, destndsc$32,     /* Destination Job & Descr    */~
            joborder$8, jobdescr$32,     /* Job Order No. & Description*/~
            lfac$(20)1, afac$(20)1, bfac$(20)1, cfac$(20)1, nfac$(20)1,  ~
            xfac$(20)1, slfac$(2)1, dfac$(20)1,/*Dragnet joke goes here*/~
            line2$79,                    /* Screen Line #2             */~
            loc%(1000),                  /* For the Lot # SEARCH       */~
            lotenabl%(1000),             /* Lot # validation level     */~
            lotnumbr$(1000)6, lotnumbr$6,/* Component Lot No.          */~
            lotunique$1,                 /* Lot uniqueness from SYS2   */~
            modno$2,                     /* G/L Module ID              */~
            mtkey$(1000)22,              /* JBMATER2 Keys              */~
            nbrcomps$18,                 /* # of Components in list    */~
            partdesc$(10)32,             /* Toggle Comp or Description */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pf16msg$27,                  /* PF(16) Action message      */~
            pfkeys$33,                   /* PF Key Hex Values          */~
            plowkey$99, readkey$99,      /* Miscellaneous Read/Plow Key*/~
            postdate$8,                  /* User's inventory post date */~
            qtybuild$10,                 /* Job Quantity to Build      */~
            qtydekit$(1000)10,           /* Qty of Component to DeKit  */~
            qtydekit(1000),              /* Qty of Component to DeKit  */~
            qtydekit$10, qtyddesc$34,    /* Qty Part/Build to DeKit    */~
            qtykcost$(10)10,             /* Toggle Qty Kitted or T/Cost*/~
            qtykittd$(1000)10,           /* Qty Comp Kitted Remaining  */~
            qtykittd(1000),              /* Qty Comp Kitted Remaining  */~
            qtyrmain$10, qtyrdesc$34,    /* Qty Remaining to Build (Job*/~
            selector$(1000)1,            /* Comp Selected for DeKit?   */~
            serial$5,                    /* Edit # of serial-#'d Parts */~
            sfcdate$6,                   /* User's Inventory post date */~
            startdat$10,                 /* Job Start Date             */~
            storenbr$(1000)3, storenbr$3,/* Store Comp Kitted from     */~
            summary$1,                   /* Summary indicator - G/L    */~
            timekitd$(1000)8,            /* Time Kitted to Job         */~
            title$40,                    /* Journal title     - G/L    */~
            totlcost$(1000)10,           /* Component Total Cost       */~
            userid$3,                    /* Current User Id            */~
            work$56                      /* General Purpose ...        */

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
            * #01 ! USERINFO ! Users Default Information File           *~
            * #02 ! SYSFILE2 ! Caelus Management System Information     *~
            * #03 ! JBMASTR2 ! Production job master file               *~
            * #04 ! HNYMASTR ! Inventory Master File                    *~
            * #05 ! HNYQUAN  ! Inventory Costs & Quantity Master (Summa *~
            * #06 ! JBMATER2 ! Production job material used detail file *~
            * #07 ! STORNAME ! STORE INFORMATION FILE                   *~
            * #10 ! PIPOUT   ! Planned inventory use detail rec         *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "USERINFO",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =   3

            select #02, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #03, "JBMASTR2",                                      ~
                        varc,     indexed,  recsize = 1300,              ~
                        keypos =    1, keylen =   8,                     ~
                        alt key  2, keypos =   58, keylen =  25, dup,    ~
                            key  1, keypos = 1120, keylen =  19, dup

            select #04, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  3, keypos =   26, keylen =  32, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  1, keypos =  102, keylen =   9, dup

            select #05, "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44

            select #06, "JBMATER2",                                      ~
                        varc,     indexed,  recsize =  400,              ~
                        keypos =    1, keylen =  22,                     ~
                        alt key  1, keypos =   23, keylen =  48

            select #07, "STORNAME",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   3

            select #10, "PIPOUT",                                        ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =   20, keylen =  37

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01%), f2%(01%), 0%, rslt$(01%))
            call "OPENCHCK" (#02, fs%(02%), f2%(02%), 0%, rslt$(02%))
            call "OPENCHCK" (#03, fs%(03%), f2%(03%), 0%, rslt$(03%))
            call "OPENCHCK" (#04, fs%(04%), f2%(04%), 0%, rslt$(04%))
            call "OPENCHCK" (#05, fs%(05%), f2%(05%), 0%, rslt$(05%))
            call "OPENCHCK" (#06, fs%(06%), f2%(06%), 0%, rslt$(06%))
            call "OPENCHCK" (#07, fs%(07%), f2%(07%), 0%, rslt$(07%))
            call "OPENCHCK" (#10, fs%(10%), f2%(10%), 0%, rslt$(10%))

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
            str(line2$,62%) = "JBMDEKIT: " & str(cms2v$,,8%)
            colp$ = " X Part Code                          Date     Str L~
        ~ot    Net Kitted  Qty DeKit"
            cold$ = " X Part Description                   Time     Str L~
        ~ot     Unit Cost  Qty DeKit"
            column$ = colp$               /* Start with Part # toggled */
            sum% = dim(partdesc$(), 1%)  /* # Comps per summary screen */
            mxl% = dim(componnt$(), 1%)    /* Max Components permitted */
            call "READ100" (#01, userid$, f1%(1%))         /* USERINFO */
            if f1%(1%) <> 0% then goto L09250
                call "ASKUSER" (0%, "*** USER POSTING DATES MISSING ***",~
                     "Posting dates for User " & userid$ & " not found "&~
                     " in USERINFO.", " ", "Press (RETURN) to acknowled"&~
                     "ge and exit.")
                goto exit_program
L09250:     get #01 using L35290, sfcdate$                   /* USERINFO */
            postdate$ = sfcdate$
            call "WHICHMON" (#02, postdate$, whichmonth%)   /* SYSFILE2 */
            call "DATEFMT" (postdate$)
            if whichmonth% > 0% then goto L09350
                call "ASKUSER" (0%, "*** INVALID POSTING DATE ***",      ~
                     "Your Shop Floor Posting Date- " & postdate$ & " i"&~
                     "s not in an open period.", "See your Administrato"&~
                     "r or run SYSDATES to correct the problem & try ag"&~
                     "ain.", "Press (RETURN) to acknowledge and exit.")
                goto exit_program
L09350
            modno$ = "03"
            jnlid$ = "MPW"
            ll% = len(str(lotnumbr$))

*        Determine status of Lot Tracking uniqueness.
            lotunique$ = "U"           /* Deny forced Lot # assignment */
            call "READ100" (#02, "SWITCHS.HNY", f1%(2%))   /* SYSFILE2 */
            if f1%(2%) <> 0% then get #02 using L09440, lotunique$
L09440:         FMT POS(93), CH(1)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 3%
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                     if enabled% = 0% then goto L10220
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                     if keyhit%  =  1% then gosub startover
                     if keyhit% <>  4% then goto L10200
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

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Edit Component line items via the Summary screen.         *~
            *************************************************************

        display_component_summary   /* Maintain &/or delete Components */
            d% = 1%                    /* Begin at the first Component */
        display_component_summary_2
            lastfieldnr% = 0%
            gosub'102(0%, 2%)
            if keyhit% =  1% then gosub startover
            if keyhit% =  2% then d% = 1%                /* First page */
            if keyhit% =  3% then d% = zln%               /* Last page */
            if keyhit% =  4% then d% = max(1%, d% - sum%)   /* Prev pg */
            if keyhit% =  5% then d% = min(zln%, d% + sum%) /* Next pg */
            if keyhit% =  6% then d% = max(1%, d% - 1%)        /* Down */
            if keyhit% =  7% then d% = min(zln%, d% + 1%)        /* Up */
            if keyhit% = 10% then gosub component_toggle     /* Toggle */
            if keyhit% = 12% then goto L11240       /* Delete Component */
            if keyhit% = 27% then goto capture_force_store_lot
            if keyhit% = 16% then goto datasave          /* Mass DeKit */
            if keyhit% = 32% then goto datasave     /* Selective DeKit */
            if keyhit% <> 0% then goto display_component_summary_2
L11240
*        User wants to edit or delete a Component, based on DLN%.
            gosub call_screen
            dx% = cursor%(1%) - 10%      /* Screen line to edit/delete */
            if dx% < 1% or dx% > sum%  /* Selected line out of bounds? */~
                then goto display_component_summary_2
            if dx% + d% - 1% > p% then goto display_component_summary_2
            if dx% = lastfieldnr% then goto display_component_summary_2
            dln% = dx% + d% - 1% /* The array element # to edit/delete */
            if keyhit% = 12% then goto delete_comp /* Delete Component */
L11330:     gosub'052(dln%)     /* Set up to edit/validate a Component */
            gosub'102(dx%, 1%)
            if keyhit% =  1% then gosub startover
            if keyhit% =  4% then goto L11400   /* Validate & Prev Line */
            if keyhit% =  5% then goto L11400   /* Validate & Next Line */
            if keyhit% = 10% then gosub component_toggle     /* Toggle */
            if keyhit% <> 0% then goto L11330
L11400:     gosub'152(dln%)            /* Edit Fields for Valid Entry  */
            if errormsg$ <> " " then goto L11330
            lastfieldnr% = dx%
            if keyhit% <> 4% then goto L11480  /* Validate & Prev Line? */
                dln% = max(1%, dln% - 1%) /* Array element to edit/val */
                if dln% < d% then d% = max(1%, d% - 1%)/* Scroll down? */
                dx% = dln% - d% + 1% /* Screen line to enable for edit */
                goto L11330
L11480:     if keyhit% <> 5% then goto L11530  /* Validate & Next Line? */
                dln% = min(p%, dln% + 1%) /* Array element to edit/val */
                if dln% > sum%+d%-1% then d%=min(zln%,d%+1%)/* Scr up? */
                dx% = dln% - d% + 1% /* Screen line to enable for edit */
                goto L11330
L11530:     goto L11240

        capture_force_store_lot
            keyhit% = 27%                                  /* Trust me */
            gosub'102(0%, 2%)           /* Capture Store & (maybe) Lot */
            errormsg$ = " "
            if keyhit% =  1% then gosub startover
            if keyhit% <> 0% then goto capture_force_store_lot
            if forcestr$ = "?" then forcestr$ = " "
            call "GETCODE" (#07, forcestr$, " ", 0%, 0,    /* STORNAME */~
                f1%(7%))
            if f1%(7%) <> 0% then goto L11680
                errormsg$ = "You must select a valid Store Code."
                goto capture_force_store_lot
            if forcelot$ = " " then goto L11740
L11680:     if lotunique$ = "U" then goto L11740
                errormsg$ = "LOT-CHECK"
                call "LOTVALID" (" ", forcestr$, forcelot$, #02, #04,    ~
                     #05, errormsg$)
                if errormsg$ <> " " then goto capture_force_store_lot

L11740
*        Et, voila! Force Component Stores & Lots to the entered values.
            for u3% = 1% to p%
                storenbr$(u3%) = forcestr$
                if forcelot$ = " " then goto L11820
                if lotunique$ = "U" then goto L11820
                if lotenabl%(u3%) = 0%                                   ~
                     then lotnumbr$(u3%) = " "                           ~
                     else lotnumbr$(u3%) = forcelot$
L11820:     next u3%
            forcestr$, forcelot$ = " "
            goto display_component_summary_2

        delete_comp          /* Maybe delete a Component from the list */
            gosub'102(dx%, 3%)     /* DX% is the screen line Component */
            if keyhit% =  1% then display_component_summary_2 /* Abort */
            if keyhit% = 10% then gosub component_toggle     /* Toggle */
            if keyhit% <> 0% then goto delete_comp
*        DELETE Component element # DLN%.
                for m% = dln% to p%   /* DLN% is the deleted Component */
                     gosub'204(m%, m%+1%)/* Suck all elements down one */
                next m%
                gosub'203(p%)                /* Clear the last element */
                p% = p% - 1%             /* New actual # of Components */
                gosub compute_zln/* Compute new 'highest top of screen'*/
                goto display_component_summary_2

        REM *************************************************************~
            *     M I S C E L L A N E O U S   S U B R O U T I N E S     *~
            *************************************************************

        deffn'200(temp%, t)        /* RJ Qtys for display; LJ for edit */
            call "CONVERT" (qtydekit(temp%), 2.2*t, qtydekit$(temp%))
            return

        component_toggle/* Toggle the Part Number & Descriptions, etc. */
            init (" ") partdesc$(), datetime$(), qtykcost$()
            if toggle% = 0% then goto show_descriptions
                toggle% = 0%           /* Indicate 'Show Part Numbers' */
                column$ = colp$           /* Part Number Column Header */
                return
        show_descriptions
            toggle% = 1%          /* Indicate 'Show Part Descriptions' */
            column$ = cold$          /* Part Description Column Header */
            return

        deffn'202(u%, u3%)               /* Let JB2TIF do all the work */
            call "JB2TIF"                                                ~
                ("J2",                  /* Send transaction to JBPOST2 */~
                0%,                     /* Wake up task flag 0,1,2,9999*/~
                0%,                     /* Not used if Wake flag = 0   */~
                u%,                     /* Transaction Type passed in  */~
                hex(15),                /* Priority (within 'J2' only) */~
                joborder$,              /* From Job number affected    */~
                modno$,                 /* G/L module to post          */~
                jnlid$,                 /* G/L journal to post         */~
                pstseq%,                /* G/L posting sequence number */~
                userid$,                /* Who                         */~
                sfcdate$,               /* Posting Date                */~
                componnt$(u3%),         /* Inventory Part Code         */~
                storenbr$(u3%),         /* Inventory Store Code        */~
                lotnumbr$(u3%),         /* Inventory Lot Id.           */~
                qtydekit(u3%),          /* Quantity to process         */~
                work$,                  /* JBMATER2 Key, etc.          */~
                " ",                    /* Special Area                */~
                avgcosts$(u3%))         /* Average Cost Breakdown      */
            jbtif% = jbtif% + 1%
            return

        compute_zln
            zln% = max(1%, p% - sum% + 1%)  /* SUM% is computed @ 9xxx */
            nbrcomps$ = " "
            convert p% to nbrcomps$, pic (#,##0)
            call "STRING" addr ("LJ", nbrcomps$, len(nbrcomps$))
            if p% = 1%                                                   ~
                then nbrcomps$ = nbrcomps$ & " Component"                ~
                else nbrcomps$ = nbrcomps$ & " Components"
            call "PUTPAREN" (nbrcomps$)
            return

        call_screen
            call "GETSCRN" ("C", " ", cursor%(), 0%)
            return

        store_lot_test  /* Test Component Store/Lot combos at DATASAVE */
            strloterr% = 1%/* Assume worst- indicate 'Store/Lot error' */
            call "READ100" (#07, storenbr$(temp%), f1%(7%))/* STORNAME */
            if f1%(7%) = 0% then return                       /* Error */
            on lotenabl%(temp%) + 1% goto L15640, store_lot_test_ok,      ~
                L15660

L15640:     lotnumbr$(temp%) = " " : goto store_lot_test_ok

L15660:     if lotnumbr$(temp%) = " " then return  /* Blanks are Error */
            call "LOTVALID" (componnt$(temp%), storenbr$(temp%),         ~
                 lotnumbr$(temp%), #02, #04, #05, errormsg$)
            if errormsg$ <> " " then return                   /* Error */
            call "LOTUNQUE" (componnt$(), lotnumbr$(), u3%, #02,         ~
                 errormsg$)
            if errormsg$ <> " " then return                   /* Error */
            gosub'205(temp%)                /* Test for duplicate Lots */
            if errormsg$ <> " " then return                   /* Error */
        store_lot_test_ok       /* They're OK! They're OK! They're OK! */
            strloterr% = 0%           /* Indicate 'NO Store/Lot error' */
            return                                       /* A-OK, fine */

        test_for_short_list/* Were any Components NOT passed to JB2TIF?*/
            m% = 0%           /* The NEW # of Components in the arrays */
*        Suck all non-blank elements down to yield 'short list' arrays.
            for n% = 1% to p%   /* Rampage through the existing arrays */
                if componnt$(n%) = " " then goto L15890 /* A done deal? */
                     m% = m% + 1%   /* Nope- Count Comps in short list */
                     shortlist% = 1%/* Indicate we've got a short list */
                     if n% = m% then goto L15890 /* Not if it's hisself */
                          gosub'204(m%, n%)    /* Suck N% down over M% */
                          gosub'203(n%)    /* Clear the sucked element */
L15890:     next n%
            p% = m% /* P% = new actual # of elements in the short list */
            gosub compute_zln   /* Compute new 'highest top of screen' */
            if shortlist% <> 0% then errormsg$ = "The Components below we~
        ~re NOT processed due to invalid/duplicate Stores/Lots."
            return

        deffn'203(k%) /* Clear out one element of all Component arrays */
            init (hex(00)) avgcosts$(k%)
            init (" ") componnt$(k%), datekitd$(k%), descript$(k%),      ~
                lotnumbr$(k%), mtkey$(k%), qtydekit$(k%), qtykittd$(k%), ~
                selector$(k%), storenbr$(k%), timekitd$(k%), totlcost$(k%)
            lotenabl%(k%) = 0%
            qtydekit(k%), qtykittd(k%) = 0
            return

        deffn'204(m%, n%)               /* Move array element N% to M% */
            avgcosts$(m%) = avgcosts$(n%)
            componnt$(m%) = componnt$(n%)
            datekitd$(m%) = datekitd$(n%)
            descript$(m%) = descript$(n%)
            lotenabl%(m%) = lotenabl%(n%)
            lotnumbr$(m%) = lotnumbr$(n%)
            mtkey$   (m%) = mtkey$   (n%)
            qtydekit$(m%) = qtydekit$(n%)
            qtydekit (m%) = qtydekit (n%)
            qtykittd$(m%) = qtykittd$(n%)
            qtykittd (m%) = qtykittd (n%)
            selector$(m%) = selector$(n%)
            storenbr$(m%) = storenbr$(n%)
            timekitd$(m%) = timekitd$(n%)
            totlcost$(m%) = totlcost$(n%)
            return

        deffn'205(a%) /* See if there are duplicate Lots for diff P/Ns */
            errormsg$ = " "                                     /* JIC */
            if lotunique$ <> "U" then return        /* It don't matter */
            if jobdestn$ <> " " then return         /* It don't matter */

            mat loc% = zer                                 /* Matlock? */
            search str(lotnumbr$())=str(lotnumbr$(a%)) to loc%() step ll%
            for b% = 1% to dim(loc%(), 1%)
                if loc%(b%) = 0% then return     /* Zero is A-OK, fine */
                loc%(b%) = ((loc%(b%)-1%)/ll%)+1%/* Posn now element # */
                if loc%(b%) = a% then goto L16390  /* Self not an error */
                if componnt$(loc%(b%)) = componnt$(a%)                   ~
                     then goto L16390/* Same Component P/N not an error */
                     errormsg$ = "Lot " & lotnumbr$(a%) & " is duplicat"&~
                          "ed between non-identical Components."
                     return
L16390:     next b%
            return                                       /* A-OK, fine */

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            shortlist% = 0%  /* Indicate 'NO short list of Components' */
            jbtif% = 0%
            if keyhit% <> 16% then goto L18320
*        It's PF(16)- Mass DeKit. Alert the user & get confirmation.
L18110:         u3% = 0%                       /* Window in the middle */
                if jobdestn$ = " "                                       ~
                     then call "ASKUSER" (u3%, "*** MASS DE-KIT? ***",   ~
                          "You requested a Mass DeKit of ALL Components"&~
                          " back to Inventory.", "Press PF(16) to confi"&~
                          "rm the Mass DeKit.", "Press PF(1) to abort t"&~
                          "he Mass DeKit.")                              ~
                     else call "ASKUSER" (u3%, "*** MASS DE-KIT? ***",   ~
                          "You requested a Mass DeKit of ALL Components"&~
                          " to another Job.", "Press PF(16) to confirm "&~
                          "the Mass DeKit.", "Press PF(1) to abort the "&~
                          "Mass DeKit.")
                     if u3% =   1% then goto display_component_summary_2
                     if u3% <> 16% then goto L18110
                if jobdestn$ = " "                                       ~
                     then call "SHOSTAT" ("Mass DeKit from Job " &       ~
                          joborder$ & " to Inventory.")                  ~
                     else call "SHOSTAT" ("Move Components from Job " &  ~
                          joborder$ & " to Job " & jobdestn$ & ".")
                goto L18390

L18320
*        It's PF(32)- Selective DeKit. We don't get as excited as above.
            if jobdestn$ = " "                                           ~
                then call "SHOSTAT" ("Selective DeKit from Job " &       ~
                     joborder$ & " to Inventory.")                       ~
                else call "SHOSTAT" ("Selective DeKit from Job " &       ~
                     joborder$ & " to Job " & jobdestn$ & ".")

L18390
*        First, get some journal stuff. One time only.
            if journal% <> 0% then goto L18460
                journal% = 1%
                u3% = 0%
                call "JNLINFO" (modno$, jnlid$, pstseq%, summary$,       ~
                     title$, sfcdate$, #02, f1%(2%), u3%)

L18460
*        Determine the action to take based on JOBDESTN$ and KEYHIT%.
            if jobdestn$ = " " then goto L18620
*        We're moving Components Job-to-Job. Mass or Selective?
                for temp% = 1% to p%
*        Write to JBTIF if there is a quantity to move to another Job
*        and if the Component otherwise meets our KEYHIT% criteria.
                     if qtydekit(temp%)<= 0 then goto L18580/* No JBTIF */
                     if keyhit% = 16% then goto L18550          /* Mass */
                          if selector$(temp%) = " " then goto L18580
L18550:              put work$ using L35300, jobdestn$, mtkey$(temp%),    ~
                          0, 0, 0, 1%
                     gosub'202(2% /* Job-to-Job */, temp%)
L18580
*        Bump to the next Component.
                next temp%
                goto datasave_exit                         /* All done */

L18620
*        We're Returning Components to Inventory. Mass or Selective?
            for temp% = 1% to p%
*        Write to JBTIF if there is a quantity to post to Inventory and
*        if the Component otherwise meets our KEYHIT% criteria. Those
*        components that are 'done deals' are blanked in case we need
*        the rest (the 'NOT done deals') for the user's short list.
                if qtydekit(temp%) <= 0 then goto L18830    /* No JBTIF */
                if keyhit% = 16% then goto L18760               /* Mass */
                     if selector$(temp%) <> " " then goto L18730
                          gosub'203(temp%)            /* 'A done deal' */
                          goto L18830
L18730:              gosub store_lot_test      /* JB2TIF only if valid */
                     if strloterr% <> 0% then goto L18830 /* NOT a D.D. */
                     goto L18790
L18760
*        We'll only call JB2TIF if the store and lot are valid.
                gosub store_lot_test           /* JB2TIF only if valid */
                if strloterr% <> 0% then goto L18830      /* NOT a D.D. */
L18790
*        OK, it's gonna be passed to JB2TIF.
                put work$ using L35300, " ", mtkey$(temp%), 0, 0, 0, 1%
                gosub'202(4% /* Job-to-Inventory */, temp%)
                gosub'203(temp%)                      /* 'A done deal' */
L18830
*        Bump to the next Component.
            next temp%
            gosub test_for_short_list/* Done w/ Comps- is there a s/l? */

        datasave_exit             /* DATASAVE process continues; ends. */
            if jbtif% = 0% then L19110
            if jobdestn$ = " " then L19100
            str(work$,,8%) = joborder$
            call "JB2TIF"                                                ~
                ("J2",                  /* Send transaction to JBPOST2 */~
                0%,                     /* Wake up task flag 0,1,2,9999*/~
                0%,                     /* Not used if Wake flag = 0   */~
                12%,                    /* Transaction Type passed in  */~
                hex(15),                /* Priority (within 'J2' only) */~
                jobdestn$,              /* From Job number affected    */~
                modno$,                 /* G/L module to post          */~
                jnlid$,                 /* G/L journal to post         */~
                pstseq%,                /* G/L posting sequence number */~
                userid$,                /* Who                         */~
                sfcdate$,               /* Posting Date                */~
                buildprt$,              /* Inventory Part Code         */~
                " ",                    /* Inventory Store Code        */~
                " ",                    /* Inventory Lot Id.           */~
                0,                      /* Quantity to process         */~
                work$,                  /* JBMATER2 Key, etc.          */~
                " ",                    /* Special Area                */~
                " ")                    /* Average Cost Breakdown      */
L19100:     call "JB2TIF" ("J2", 2%, u3%)     /* Send Hullo to BG Task */
L19110:     if shortlist% <> 0%     /* Return point depends on whether */~
                then goto display_component_summary  /* or not there's */~
                else goto inputmode    /* a 'short list' of components */

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20130,         /* Job Order No.          */~
                              L20160,         /* Qty to DeKit from Job  */~
                              L20210          /* Destination Job #      */
            return

L20130: REM Def/Enable Job Order No.               JOBORDER$
            return

L20160: REM Def/Enable Qty to DeKit from Job       QTYDEKIT$
            if qtydekit$ = " " then qtydekit = qtyrmain
            call "CONVERT" (qtydekit, -2.2, qtydekit$)
            return

L20210: REM Def/Enable Destination Job #           JOBDESTN$
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  2  of Input. *~
            *************************************************************

        deffn'052(u3%)
            selector$(u3%) = "X"            /* Component is 'selected' */
            gosub'200(u3%, -1)             /* LJ the quantity for edit */
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
                     "red Component, then press (RETURN) to edit."
                return

L28130
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            if scrnr% = 2% then restore line = scrn2_msg, 1%
            if scrnr% = 3% then restore line = scrn3_msg, fieldnr%
            read inpmessage$                     /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Job Order number, partial, blanks or '?' to see a list.",~
         "Enter Parent Part's-worth of Components to DeKit from Job.   ",~
         "Enter Destination Job # (optional). Leave blank for DeKit back ~
        ~to inventory."

        scrn2_msg  :  data                                               ~
         "Edit Component data fields, then press (RETURN), PF(4) or PF(5)~
        ~ to validate."

        scrn3_msg  :  data                                               ~
         "Enter a Store and Lot (optional). ALL Components will be set to~
        ~ these values.",                                                 ~
         "Enter a Store Code. ALL Components will be set to this Store Co~
        ~de."

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables
            init(" ") errormsg$, inpmessage$, buildprt$, builddsc$,      ~
                componnt$(), datekitd$(), descript$(), joborder$,        ~
                lotnumbr$(), qtykittd$(), qtydekit$(), qtydekit$,        ~
                storenbr$(), selector$(), closedat$, startdat$,          ~
                qtyddesc$, joborder$, jobdescr$, qtybuild$, end_date$,   ~
                jobdestn$, destndsc$, dekitpct$, mtkey$(), partdesc$(),  ~
                selector$(), totlcost$(), timekitd$(), datetime$(),      ~
                qtykcost$(), qtyrmain$, qtyrdesc$, storenbr$, lotnumbr$, ~
                forcestr$, forcelot$, destnprt$
            init (hex(00)) avgcosts$()
            init (hex(8c)) slfac$()
            p%, shortlist% = 0%
            mat qtykittd = zer
            mat qtydekit = zer
            mat avgcosts = zer
            mat lotenabl%= zer
            toggle% = 1%
            gosub component_toggle      /* Show Part Numbers initially */
            call "ALLFREE"
            call "JBINUSE" (" ", 1%)   /* Clears In-use Flag, if there */
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1993  an unpublished work by CAELUS,       *~
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

        dataload_header /* Load/Edit header-related (Job Order #) data */
*        Get appropriate data fields from JBMASTR2.
            get #03 using L35040, buildprt$, qtybuild, temp, startdat$,   ~
                closedat$, end_date$                       /* JBMASTR2 */
            qtyrmain = qtybuild - temp  /* Quantity Remaining to Build */
            if qtyrmain > 0 then goto L30190
                call "ASKUSER" (2%, "*** NO REMAINING QTY ***",          ~
                     "There is no Quantity Remaining to Build in that J"&~
                     "ob.", " ", "Press any PF key to acknowledge.")
                buildprt$, startdat$, closedat$, end_date$, joborder$,   ~
                     jobdescr$ = " "
                errormsg$ = hex(00)
                return
L30190:     call "DATFMTC" (startdat$)
            call "DATFMTC" (end_date$)
            call "DESCRIBE" (#04, buildprt$, builddsc$, 1%,/* HNYMASTR */~
                f1%(4%))
            if f1%(4%) = 0% then builddsc$ = "(Part not on file)"
            call "CONVERT" (round(qtybuild, 2), 2.2, qtybuild$)
            call "CONVERT" (round(qtyrmain, 2), 2.2, qtyrmain$)
            qtyrdesc$ = "(The Qty available for DeKitting)"
            return                      /* Header data loading is done */

        dataload_components    /* Load/Edit Component (line item) data */
            call "SHOSTAT" ("Loading Component list ... please stand by")
            jobout = 0          /* Indicate 'no out-of-job quantities' */
            serial% = 0%         /* Indicate 'no Serial Numbers found' */
            destnprt% = 0% /* Indicate 'no Component P/N = Dest'n P/N' */
            p% = 0%  /* 'P%' contains the # of array elements occupied */

*        Read appropriate Materials Ledger into arrays.
            plowkey$ = xor plowkey$                            /* ZOT! */
            str(plowkey$,,8%) = str(joborder$)

L30400
*        This is the top of the READ loop for the Materials Ledger.
            p% = p% + 1%        /* Increment the array element counter */
            if p% > mxl% then goto dataload_exit  /* Arrays max'd out? */

L30440
*        Return here if the JBMATER2 record is not wanted for any reason.
            init (" ") datekitd$(p%), componnt$(p%), storenbr$(p%),      ~
                lotnumbr$(p%), mtkey$(p%), timekitd$(p%), totlcost$(p%), ~
                storenbr$, lotnumbr$
            qtykittd(p%) = 0
            call "PLOWNEXT" (#06, plowkey$, 8%, f1%(6%))   /* JBMATER2 */
                if f1%(6%) = 0% then goto dataload_exit
            get #06 using L35150, mtkey$(p%), componnt$(p%),              ~
                storenbr$(p%), lotnumbr$(p%), datekitd$(p%),             ~
                timekitd$(p%), temp1, temp2, avgcosts(), temp,           ~
                storenbr$, lotnumbr$                       /* JBMATER2 */
            jobout = jobout + temp  /* Accum any out-of-job quantities */
            if temp1 <= 0 then goto L30440       /* Throw out 0's, negs */
            qtykittd(p%) = temp1 - temp    /* Net Qty Remaining in Job */
            if qtykittd(p%) <= 0 then goto L30440/* Throw out 0's, negs */
            call "SERENABL" (componnt$(p%), enabled%, 0%, #02, #04)
            serial% = serial% + enabled%  /* Track if Serial #'d parts */
            if enabled% <> 0% then goto L30440  /* Throw out Serial #'d */
            if jobdestn$ = " " then goto L30660/* Toss Comps = Dest Job */
                if componnt$(p%) <> destnprt$ then goto L30660  /* Part */
                     destnprt% = 1%                 /* Number to Build */
                     goto L30440
L30660
*        This component is a keeper!
            call "LOTENABL" (componnt$(p%), lotenabl%(p%), ll%, #02, #04)
            if storenbr$ = " " then goto L30720
                if str(storenbr$(p%),,1%) <> "*" then goto L30720
                     storenbr$(p%) = storenbr$
                     lotnumbr$(p%) = lotnumbr$
L30720:     if lotenabl%(p%) = 0% then lotnumbr$(p%) = " "
            qtydekit(p%) = round(qtykittd(p%) * dekitpct, 2)
            call "DATEFMT" (datekitd$(p%))
            call "TIMEOK" (timekitd$(p%), temp, " ")
            call "DESCRIBE" (#04, componnt$(p%), descript$(p%), 0%,      ~
                f1%(4%))                                   /* HNYMASTR */
            if f1%(4%) = 0% then descript$(p%) = "Component not on file"
            call "CONVERT" (round(qtykittd(p%), 2), 2.2, qtykittd$(p%))
            call "CONVERT" (round(qtydekit(p%), 2), 2.2, qtydekit$(p%))
*        Compute Component Unit Costs from Extended Costs.
            call "CONVERT" (round(temp2 / temp1, 4), 4.4, totlcost$(p%))
            for u3% = 1% to 12%
                avgcosts(u3%) = round(avgcosts(u3%) / temp1, 4)
                put str(avgcosts$(p%),((u3%-1%)*8%)+1%,8%) using L30870,  ~
                     avgcosts(u3%)
L30870:              FMT PD(14,4)
            next u3%
            goto L30400                  /* This Component is a keeper! */

        dataload_exit
            p% = p% - 1%            /* Actual # of array elements used */
            if jobout = 0 then goto L31040
L30940:         u% = 0%                        /* Window in the middle */
                call "ASKUSER" (u%, "*** QUANTITY OUT OF JOB WARNING! *"&~
                     "**", "Quantities of some Components have been mov"&~
                     "ed out of this Job.", "The default Component Quan"&~
                     "tities may NOT be accurate.", "Press PF(1) to abo"&~
                     "rt. Press PF(16) to continue.")
                if u% <>  1% then goto L31030
                     return clear all
                     goto inputmode
L31030:         if u% <> 16% then goto L30940
L31040:     if destnprt% = 0% then goto L31110
                u% = 0%                        /* Window in the middle */
                call "ASKUSER" (u%, "*** COMPONENT PART # CONFLICT! ***",~
                     "A Component Part matches the Destination Job's Pa"&~
                     "rt to Build.", "This Component has been dropped f"&~
                     "rom the DeKit list.", "Press any PF key to acknow"&~
                     "ledge and continue.")
L31110:     if serial% = 0% then goto L31190
                convert serial% to serial$, pic (#,##0)
                call "STRING" addr ("LJ", serial$, len(str(serial$)))
                call "ASKUSER" (2%, "*** SERIAL #'D PARTS FOUND ***",    ~
                     serial$ & " Serial Numbered parts were found in th"&~
                     "e Materials Ledger.", "You will have to deal with"&~
                     " them manually.", "Press any PF key to acknowledg"&~
                     "e and continue.")
L31190:     if p% <> 0% then goto L31260
                call "ASKUSER" (2%, "*** NO COMPONENTS ***",             ~
                     "There were no Components found in the Materials L"&~
                     "edger.", " ", "Press any PF key to acknowledge an"&~
                     "d continue.")
                return clear all
                goto inputmode
L31260:     toggle% = 1%
            gosub component_toggle      /* Show Part Numbers initially */
            gosub compute_zln       /* Compute 'highest top of screen' */
            for x% = 1% to p%                  /* Compute Qty to DeKit */
                qtydekit(x%) = round(qtykittd(x%) * dekitpct, 2)
                call "CONVERT" (qtydekit(x%), 2.2, qtydekit$(x%))
            next x%
            return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *************************************************************

L35040:     FMT /* File #03- JBMASTR2 (The Job Master file)            */~
                POS(58), CH(25),         /* Part Number to Build       */~
                2*PD(14,4),              /* Quantity to Build/Complete */~
                POS(147), CH(6),         /* Date Actually Started      */~
                CH(6),                   /* Date Actually Ended (Closed*/~
                POS(174), CH(6)          /* Scheduled Completion Date  */

L35110:     FMT /* File #03- JBMASTR2 (The Job Master file)            */~
                POS(58), CH(25),         /* Part to Build              */~
                POS(83), 2*PD(14,4)      /* Quantity to Build/Complete */

L35150:     FMT /* File #06- JBMATER2 (The Job Materials Ledger)       */~
                CH(22),                  /* JBMATER2 Key               */~
                CH(25),                  /* Part (Component) code      */~
                CH(3),                   /* Store Number               */~
                CH(6),                   /* Lot Number                 */~
                CH(6),                   /* Date record written        */~
                CH(6),                   /* Time written (no 100ths)   */~
                POS(71), PD(14,4),       /* Quantity moved to job      */~
                POS(79), PD(14,4),       /* Total Inventory Cost       */~
                POS(87), 12*PD(14,4),    /* Component Extended Costs   */~
                POS(330), PD(14,4),      /* Quantity moved from job    */~
                POS(342), CH(3),         /* Store originally ktd frm   */~
                CH(6)                    /* Lot originally ktd frm     */~

L35290:     FMT POS(34), CH(6)           /* File #01-USERINFO SFC Date */
L35300:     FMT CH(8), CH(22), 3*PD(14,4), BI(2)     /* WORK$ Variable */
L35310:     FMT POS(153), CH(6)      /* File #03- JBMASTR2 Date Closed */

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
            on fieldnr% gosub L40180,              /* Job Order No.     */~
                              L40190,              /* Qty to DeKit      */~
                              L40180               /* Destination Job # */
            goto L40210

            lfac$(fieldnr%) = hex(80)  :  return         /* Up / Low   */
L40180:     lfac$(fieldnr%) = hex(81)  :  return         /* Upper Only */
L40190:     lfac$(fieldnr%) = hex(82)  :  return         /* Numeric    */

L40210:     accept                                                       ~
                at (01,02), "Mass DeKit Components from Job to Inventory ~
        ~OR another Job",                                                 ~
                at (01,66), "Today:",                                    ~
                at (01,73), fac(hex(8c)), date$                 , ch(08),~
                at (02,02), fac(hex(ac)), line2$                , ch(79),~
                at (03,02), fac(hex(94)), errormsg$             , ch(79),~
                                                                         ~
                at (04,02), "Job Order No.",                             ~
                at (04,20), fac(lfac$( 1%)), joborder$          , ch(08),~
                at (04,46), fac(hex(8c)),    jobdescr$          , ch(32),~
                                                                         ~
                at (05,02), "Build Parent Part",                         ~
                at (05,20), fac(hex(8c)),    buildprt$          , ch(25),~
                at (05,46), fac(hex(8c)),    builddsc$          , ch(34),~
                                                                         ~
                at (06,02), "Current Job Qty",                           ~
                at (06,20), fac(hex(8c)),    qtybuild$          , ch(10),~
                at (06,46), "Start",                                     ~
                at (06,52), fac(hex(8c)),    startdat$          , ch(10),~
                at (06,63), "Compl",                                     ~
                at (06,69), fac(hex(8c)),    end_date$          , ch(10),~
                                                                         ~
                at (07,02), "Remaining Qty",                             ~
                at (07,20), fac(hex(8c)),    qtyrmain$          , ch(10),~
                at (07,46), fac(hex(8c)),    qtyrdesc$          , ch(34),~
                                                                         ~
                at (08,02), "Quantity to DeKit",                         ~
                at (08,20), fac(lfac$( 2%)), qtydekit$          , ch(10),~
                at (08,46), fac(hex(8c)),    qtyddesc$          , ch(34),~
                                                                         ~
                at (09,02), "Destination Job #",                         ~
                at (09,20), fac(lfac$( 3%)), jobdestn$          , ch(08),~
                at (09,46), fac(hex(8c)),    destndsc$          , ch(32),~
                                                                         ~
                at (10,02), fac(hex(ac)),   column$             , ch(79),~
                                                                         ~
                at (21,02), fac(hex(a4)),   inpmessage$         , ch(79),~
                at (22,02), fac(hex(8c)),   pf$(1%)             , ch(79),~
                at (23,02), fac(hex(8c)),   pf$(2%)             , ch(79),~
                at (24,02), fac(hex(8c)),   pf$(3%)             , ch(79),~
                                                                         ~
                keys(pfkeys$), key(keyhit%)

            if keyhit% <> 13% then L40680
                call "MANUAL" ("JBMDEKIT") : goto L40210

L40680:     if keyhit% <> 15% then L40710
                call "PRNTSCRN" : goto L40210

L40710:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return

        set_pf1
            str(line2$,,61%) = "Job Order Selection & Maintenance"
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

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
            init (hex(8c)) slfac$(), dfac$()
            if edit% <> 3% then goto L42180                /* Deleting? */
*        Yes. Highlight a Component for possible deletion.
                init (hex(8c)) afac$(), bfac$(), cfac$(), dfac$(), nfac$()
                init (hex(86)) afac$(fieldnr%)/* A place for the cursor*/
                init (hex(94)) bfac$(fieldnr%), cfac$(fieldnr%),         ~
                     dfac$(fieldnr%), nfac$(fieldnr%)/* Blink, Protect */
                gosub set_pf2                  /* Remember, EDIT% = 3% */
                inpmessage$ = "Press (RETURN) to DELETE the highlighted"&~
                     " Component. PF(1) to abort delete."
                goto L42430
L42180:     if shortlist% = 0% or keyhit% <> 27% then goto L42290 /*S/L?*/
*        'Short List' of Components. Set up for PF(27) Store & Lot.
                if lotunique$ = "U"                                      ~
                     then gosub'050(3%, 2%)                              ~
                     else gosub'050(3%, 1%)
                gosub set_pf2
                init (hex(8c)) afac$(), bfac$(), cfac$(), nfac$(), xfac$()
                if lotunique$ <> "U"                                     ~
                     then init (hex(81)) slfac$()                        ~
                     else slfac$(1%) = hex(81)
                goto L42430
L42290
*        This is pretty much a normal display/edit mode of Components.
            gosub'050(2%, fieldnr%)
            gosub set_pf2
            init (hex(8c)) afac$(), bfac$(), cfac$(), nfac$(), xfac$()
            if fieldnr% = 0% then goto L42400
                afac$(fieldnr%) = hex(81)
                if jobdestn$ = " " then bfac$(fieldnr%) = hex(81)
                if jobdestn$ = " " and lotenabl%(d%+fieldnr%-1%) <> 0%   ~
                     then cfac$(fieldnr%) = hex(81)
                nfac$(fieldnr%) = hex(82)
                goto L42430
L42400:     init (hex(86)) afac$(), nfac$()
            if jobdestn$ = " " then init (hex(86)) bfac$(), cfac$()

L42430
*        Set up the Component columns according the TOGGLE% switch.
            init (" ") partdesc$(), datetime$(), qtykcost$()
            if toggle% <> 0% then goto L42530
                for t% = 1% to sum%
                     if componnt$(d%+t%-1%) = " " then goto L42600
                     partdesc$(t%) = componnt$(d%+t%-1%)  /* Component */
                     datetime$(t%) = datekitd$(d%+t%-1%)/* Date Kitted */
                     qtykcost$(t%) = qtykittd$(d%+t%-1%) /* Qty Kitted */
                next t%
                goto L42600
L42530:     for t% = 1% to sum%
                if componnt$(d%+t%-1%) = " " then goto L42600
                partdesc$(t%) = descript$(d%+t%-1%)     /* Description */
                datetime$(t%) = timekitd$(d%+t%-1%)     /* Time Kitted */
                qtykcost$(t%) = totlcost$(d%+t%-1%)      /* Total Cost */
            next t%

L42600:     accept                                                       ~
                at (01,02), "Mass DeKit Components from Job to Inventory ~
        ~OR another Job",                                                 ~
                at (01,66), "Today:",                                    ~
                at (01,73), fac(hex(8c)), date$                 , ch(08),~
                at (02,02), fac(hex(ac)), line2$                , ch(79),~
                at (03,02), fac(hex(94)), errormsg$             , ch(79),~
                                                                         ~
                at (04,02), "Job Order No.",                             ~
                at (04,20), fac(hex(8c)),    joborder$          , ch(08),~
                at (04,46), fac(hex(8c)),    jobdescr$          , ch(32),~
                                                                         ~
                at (05,02), "Build Parent Part",                         ~
                at (05,20), fac(hex(8c)),    buildprt$          , ch(25),~
                at (05,46), fac(hex(8c)),    builddsc$          , ch(34),~
                                                                         ~
                at (06,02), "Current Job Qty",                           ~
                at (06,20), fac(hex(8c)),    qtybuild$          , ch(10),~
                at (06,46), "Start",                                     ~
                at (06,52), fac(hex(8c)),    startdat$          , ch(10),~
                at (06,63), "Compl",                                     ~
                at (06,69), fac(hex(8c)),    end_date$          , ch(10),~
                                                                         ~
                at (07,02), "Remaining Qty",                             ~
                at (07,20), fac(hex(8c)),    qtyrmain$          , ch(10),~
                at (07,46), fac(hex(8c)),    qtyrdesc$          , ch(34),~
                                                                         ~
                at (08,02), "Quantity to DeKit",                         ~
                at (08,20), fac(xfac$( 2%)), qtydekit$          , ch(10),~
                at (08,46), fac(hex(8c)),    qtyddesc$          , ch(34),~
                                                                         ~
                at (09,02), "Destination Job #",                         ~
                at (09,20), fac(xfac$( 3%)), jobdestn$          , ch(08),~
                at (09,46), fac(hex(8c)),    destndsc$          , ch(32),~
                                                                         ~
                at (10,02), fac(hex(ac)),    column$            , ch(79),~
                                                                         ~
                at (11,03), fac(afac$( 1%)), selector$(d%)      , ch(01),~
                at (11,05), fac(dfac$( 1%)), partdesc$(1%)      , ch(32),~
                at (11,38), fac(dfac$( 1%)), datetime$(1%)      , ch(08),~
                at (11,49), fac(bfac$( 1%)), storenbr$(d%)      , ch(03),~
                at (11,53), fac(cfac$( 1%)), lotnumbr$(d%)      , ch(06),~
                at (11,60), fac(dfac$( 1%)), qtykcost$(1%)      , ch(10),~
                at (11,71), fac(nfac$( 1%)), qtydekit$(d%)      , ch(10),~
                                                                         ~
                at (12,03), fac(afac$( 2%)), selector$(d%+ 1%)  , ch(01),~
                at (12,05), fac(dfac$( 2%)), partdesc$(2%)      , ch(32),~
                at (12,38), fac(dfac$( 2%)), datetime$(2%)      , ch(08),~
                at (12,49), fac(bfac$( 2%)), storenbr$(d%+ 1%)  , ch(03),~
                at (12,53), fac(cfac$( 2%)), lotnumbr$(d%+ 1%)  , ch(06),~
                at (12,60), fac(dfac$( 2%)), qtykcost$(2%)      , ch(10),~
                at (12,71), fac(nfac$( 2%)), qtydekit$(d%+ 1%)  , ch(10),~
                                                                         ~
                at (13,03), fac(afac$( 3%)), selector$(d%+ 2%)  , ch(01),~
                at (13,05), fac(dfac$( 3%)), partdesc$(3%)      , ch(32),~
                at (13,38), fac(dfac$( 3%)), datetime$(3%)      , ch(08),~
                at (13,49), fac(bfac$( 3%)), storenbr$(d%+ 2%)  , ch(03),~
                at (13,53), fac(cfac$( 3%)), lotnumbr$(d%+ 2%)  , ch(06),~
                at (13,60), fac(dfac$( 3%)), qtykcost$(3%)      , ch(10),~
                at (13,71), fac(nfac$( 3%)), qtydekit$(d%+ 2%)  , ch(10),~
                                                                         ~
                at (14,03), fac(afac$( 4%)), selector$(d%+ 3%)  , ch(01),~
                at (14,05), fac(dfac$( 4%)), partdesc$(4%)      , ch(32),~
                at (14,38), fac(dfac$( 4%)), datetime$(4%)      , ch(08),~
                at (14,49), fac(bfac$( 4%)), storenbr$(d%+ 3%)  , ch(03),~
                at (14,53), fac(cfac$( 4%)), lotnumbr$(d%+ 3%)  , ch(06),~
                at (14,60), fac(dfac$( 4%)), qtykcost$(4%)      , ch(10),~
                at (14,71), fac(nfac$( 4%)), qtydekit$(d%+ 3%)  , ch(10),~
                                                                         ~
                at (15,03), fac(afac$( 5%)), selector$(d%+ 4%)  , ch(01),~
                at (15,05), fac(dfac$( 5%)), partdesc$(5%)      , ch(32),~
                at (15,38), fac(dfac$( 5%)), datetime$(5%)      , ch(08),~
                at (15,49), fac(bfac$( 5%)), storenbr$(d%+ 4%)  , ch(03),~
                at (15,53), fac(cfac$( 5%)), lotnumbr$(d%+ 4%)  , ch(06),~
                at (15,60), fac(dfac$( 5%)), qtykcost$(5%)      , ch(10),~
                at (15,71), fac(nfac$( 5%)), qtydekit$(d%+ 4%)  , ch(10),~
                                                                         ~
                at (16,03), fac(afac$( 6%)), selector$(d%+ 5%)  , ch(01),~
                at (16,05), fac(dfac$( 6%)), partdesc$(6%)      , ch(32),~
                at (16,38), fac(dfac$( 6%)), datetime$(6%)      , ch(08),~
                at (16,49), fac(bfac$( 6%)), storenbr$(d%+ 5%)  , ch(03),~
                at (16,53), fac(cfac$( 6%)), lotnumbr$(d%+ 5%)  , ch(06),~
                at (16,60), fac(dfac$( 6%)), qtykcost$(6%)      , ch(10),~
                at (16,71), fac(nfac$( 6%)), qtydekit$(d%+ 5%)  , ch(10),~
                                                                         ~
                at (17,03), fac(afac$( 7%)), selector$(d%+ 6%)  , ch(01),~
                at (17,05), fac(dfac$( 7%)), partdesc$(7%)      , ch(32),~
                at (17,38), fac(dfac$( 7%)), datetime$(7%)      , ch(08),~
                at (17,49), fac(bfac$( 7%)), storenbr$(d%+ 6%)  , ch(03),~
                at (17,53), fac(cfac$( 7%)), lotnumbr$(d%+ 6%)  , ch(06),~
                at (17,60), fac(dfac$( 7%)), qtykcost$(7%)      , ch(10),~
                at (17,71), fac(nfac$( 7%)), qtydekit$(d%+ 6%)  , ch(10),~
                                                                         ~
                at (18,03), fac(afac$( 8%)), selector$(d%+ 7%)  , ch(01),~
                at (18,05), fac(dfac$( 8%)), partdesc$(8%)      , ch(32),~
                at (18,38), fac(dfac$( 8%)), datetime$(8%)      , ch(08),~
                at (18,49), fac(bfac$( 8%)), storenbr$(d%+ 7%)  , ch(03),~
                at (18,53), fac(cfac$( 8%)), lotnumbr$(d%+ 7%)  , ch(06),~
                at (18,60), fac(dfac$( 8%)), qtykcost$(8%)      , ch(10),~
                at (18,71), fac(nfac$( 8%)), qtydekit$(d%+ 7%)  , ch(10),~
                                                                         ~
                at (19,03), fac(afac$( 9%)), selector$(d%+ 8%)  , ch(01),~
                at (19,05), fac(dfac$( 9%)), partdesc$(9%)      , ch(32),~
                at (19,38), fac(dfac$( 9%)), datetime$(9%)      , ch(08),~
                at (19,49), fac(bfac$( 9%)), storenbr$(d%+ 8%)  , ch(03),~
                at (19,53), fac(cfac$( 9%)), lotnumbr$(d%+ 8%)  , ch(06),~
                at (19,60), fac(dfac$( 9%)), qtykcost$(9%)      , ch(10),~
                at (19,71), fac(nfac$( 9%)), qtydekit$(d%+ 8%)  , ch(10),~
                                                                         ~
                at (20,03), fac(afac$(10%)), selector$(d%+ 9%)  , ch(01),~
                at (20,05), fac(dfac$(10%)), partdesc$(10%)     , ch(32),~
                at (20,38), fac(dfac$(10%)), datetime$(10%)     , ch(08),~
                at (20,49), fac(bfac$(10%)), storenbr$(d%+ 9%)  , ch(03),~
                at (20,53), fac(cfac$(10%)), lotnumbr$(d%+ 9%)  , ch(06),~
                at (20,60), fac(dfac$(10%)), qtykcost$(10%)     , ch(10),~
                at (20,71), fac(nfac$(10%)), qtydekit$(d%+ 9%)  , ch(10),~
                                                                         ~
                at (21,02), fac(hex(a4)),   inpmessage$         , ch(79),~
                at (22,02), fac(hex(8c)),   pf$(1%)             , ch(79),~
                at (23,02), fac(hex(8c)),   pf$(2%)             , ch(79),~
                at (23,53), fac(slfac$(1%)),forcestr$           , ch(03),~
                at (23,57), fac(slfac$(2%)),forcelot$           , ch(06),~
                at (24,02), fac(hex(8c)),   pf$(3%)             , ch(79),~
                                                                         ~
                keys(pfkeys$), key(keyhit%)

            if keyhit% <> 13% then L43890
                call "MANUAL" ("JBMDEKIT") : goto L42600

L43890:     if keyhit% <> 15% then L43920
                call "PRNTSCRN" : goto L42600

L43920:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return

        set_pf2
            str(line2$,,61%) = "Component Selection & Maintenance " &    ~
                nbrcomps$
        if edit% = 3% then goto L44670           /* Component deletion? */
        if edit% = 2% then goto L44150
*        Edit Mode - Enabled.
            pf$(1%) = "(1)Start Over (4)Val/Prev   (10)Toggle  " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "              (5)Val/Next               " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                                       "
            pfkeys$ = hex(01ffff0405ffffffff0affff0dff0fffff00)
            if dln% > 1% then goto L44110
                str(pf$(1%),15%,11%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L44110:     if dln% < p% then goto L44130
                str(pf$(2%),15%,11%) = " " : str(pfkeys$,5%,1%) = hex(ff)
L44130:     return

L44150
*        Edit Mode - Select Field.
            pf$(1%) = "(1)Start Over (4)Prev Page  (10)Toggle  " &       ~
                      "(12)Delete Comp        (13)Instructions"
            pf$(2%) = "(2)First Page (5)Next Page              " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "(3)Last Page  (6)Down (7)Up" & hex(84) &          ~
                      "(32)DeKit Selected only " & pf16msg$
            pfkeys$ = hex(01020304050607ffff0aff0c0dff0f100020ff)
            if shortlist% = 0% then goto L44440
                if lotunique$ = "U" then goto L44270
                     str(pf$(2%),29%,22%) = "(27)Set all Str/Lots ="
                     goto L44280
L44270:         str(pf$(2%),29%,20%) = "(27)Set all Stores ="
L44280:         str(pfkeys$,19%, 1%) = hex(1b)
                if keyhit% <> 27% then goto L44440
                     if lotunique$ <> "U"                                ~
                          then str(line2$,,61%) = "Enter Values for Com"&~
                               "ponent Stores & Lots"                    ~
                          else str(line2$,,61%) = "Enter Values for Com"&~
                               "ponent Stores"
                     gosub turn_off_evens
                     gosub turn_off_odds
                     str(pf$(1%),29%,10%) = " "
                     str(pfkeys$,10%,1%) = hex(ff)
                     str(pf$(3%),53%,27%) = " "
                     str(pfkeys$,16%,1%) = hex(ff)
                     str(pfkeys$,19%,1%) = hex(ff)   /* Disable PF(27) */
                     gosub turn_off_32
                     return
L44440:     if str(selector$()) = " " then gosub turn_off_32
*        If we're at the 'top' of the Components, DISable PF(2), 4 & 6.
            if d% = 1% then gosub turn_off_evens
*        If we're at the 'end' of the Components, DISable PF(3), 5 & 7.
            if d% = zln% then gosub turn_off_odds
            return

        turn_off_evens
            str(pf$(2%),,13%), str(pf$(1%),15%,12%), str(pf$(3%),15%,7%) ~
                = " "
            str(pfkeys$,2%,1%), str(pfkeys$,4%,1%), str(pfkeys$,6%,1%)   ~
                = hex(ff)
            return
        turn_off_odds
            str(pf$(3%),,12%), str(pf$(2%),15%,12%), str(pf$(3%),23%,5%) ~
                = " "
            str(pfkeys$,3%,1%), str(pfkeys$,5%,1%), str(pfkeys$,7%,1%)   ~
                = hex(ff)
            return
        turn_off_32
            str(pf$(3%),29%,23%) = " " : str(pfkeys$,18%,1%) = hex(ff)
            return

L44670
*        Delete mode. Maybe DELETE a Component.
            pf$(1%) = "(1)Abort Deletion           (10)Toggle  " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffffff0affff0dff0fffff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50130,         /* Job Order No.          */~
                              L50590,         /* Qty to DeKit from Job  */~
                              L50750          /* Destination Job #      */
            return

L50130: REM Test for Job Order No.                JOBORDER$
            jobdescr$ = " "
            if joborder$ = "?" then joborder$ = " "
            call "GETCODE" (#03, joborder$, jobdescr$, 1%, /* JBMASTR2 */~
                0, f1%(3%))
            if f1%(3%) <> 0% then goto L50230
                errormsg$ = "You must select an existing, open Job."
L50200:         buildprt$, builddsc$, startdat$, end_date$, qtybuild$    ~
                     = " "
                return
L50230
*        Check to see if job is kitted complete. Warn if not.
            readkey$ = xor readkey$
            str(readkey$,,19%) = "JOB ORDER: " & joborder$
            call "PLOWNEXT" (#10, readkey$, 19%, f1%(10%))   /* PIPOUT */
            if f1%(10%) = 0% then goto L50380          /* Nope, it's OK */
L50280:         u% = 0%                        /* Window in the middle */
                call "ASKUSER" (u%, "*** NOT KITTED COMPLETE WARNING! *"&~
                     "**", "PIPOUTs still exist for this incompletely-k"&~
                     "itted Job.", "The default Component Quantities ma"&~
                     "y NOT be accurate.", "Press PF(1) to abort. Press"&~
                     " PF(16) to continue.")
                if u% <>  1% then goto L50370
                     return clear all
                     goto inputmode
L50370:         if u% <> 16% then goto L50280
L50380:     u3% = 2%       /* Check In-use & Write In-use Flag if free */
            call "JBINUSE" (joborder$, u3%)
            if u3% = 0% then L50430
                errormsg$ = hex(00)
                goto L50200
L50430:     gosub dataload_header
            if errormsg$ = " " then L50470
               call "JBINUSE" (joborder$, 1%)
               return
L50470:     if closedat$ = " " or closedat$ = blankdate$ then return
                call "DATEFMT" (closedat$)
L50490:         u3% = 2%
                call "ASKUSER" (u3%, "*** THAT JOB IS CLOSED ***",       ~
                     "The Job you selected (" & joborder$ & ") was clos"&~
                     "ed on " & closedat$ & ".", " ", "Press (RETURN) t"&~
                     "o select another Job.")
                if u3% <> 0% then goto L50490
                     call "JBINUSE" (joborder$, 1%)
                     joborder$ = "?"
                     goto L50130

L50590: REM Test for Qty to DeKit                 QTYDEKIT$
            qtyddesc$ = " "
            if qtydekit$ = " " then qtydekit$ = "1"
            call "NUMTEST" (qtydekit$, 0, 9e9, errormsg$, -2.2, qtydekit)
            if errormsg$ <> " " then return
            if qtydekit <= qtyrmain then goto L50680
                errormsg$ = "Qty to DeKit may not exceed Quantity Remai"&~
                     "ning to Build."
                return
L50680:     dekitpct = round(qtydekit / qtybuild, 4)
            convert dekitpct * 100 to dekitpct$, pic (##0.00)
            call "STRING" addr ("LJ", dekitpct$, len(str(dekitpct$)))
            qtyddesc$ = "(" & dekitpct$ & "% DeKit from Job " &          ~
                joborder$ & ")"
            return

L50750: REM Test for Destination Job #            JOBDESTN$
            destnprt$, destndsc$ = " "
            pf16msg$ = "(16)Mass DeKit to Inventory"
            if jobdestn$ = " " then goto L51160        /* Blanks are OK */
            if jobdestn$ = "?" then jobdestn$ = " "
            call "GETCODE" (#03, jobdestn$, destndsc$, 1%, /* JBMASTR2 */~
                0, f1%(3%))
            if f1%(3%) <> 0% then goto L50860
                errormsg$ = "You must select an existing, open Job -OR-"&~
                     " enter blanks."
                return
L50860:     get #03 using L35110, destnprt$, temp, temp2
            if jobdestn$ <> joborder$ then goto L50910
                errormsg$ = "Destination Job may not be the same as the"&~
                     " Job Order No."
                return
L50910:     u3% = 2%       /* Check In-use & Write In-use Flag if free */
            call "JBINUSE" (jobdestn$, u3%)
            if u3% = 0% then L50960
                errormsg$ = hex(00)
                return
L50960:     get #03 using L35310, temp$     /* JBMASTR2 Date Job Closed */
            if str(temp$,,6%) = " " or temp$ = blankdate$ ~
               then goto L51080            /* Dest Job is open */
                call "DATEFMT" (str(temp$,,8%))  /* Dest Job is closed */
L50990:         u3% = 2%
                call "ASKUSER" (u3%, "*** THAT JOB IS CLOSED ***",       ~
                     "The Job you selected (" & jobdestn$ & ") was clos"&~
                     "ed on " & str(temp$,,8%) & ".", " ", "Press (RETU"&~
                     "RN) to select another job.")
                if u3% <> 0% then goto L50990
L51050:              call "JBINUSE" (jobdestn$, 1%)
                     jobdestn$ = "?"
                     goto L50750
L51080:     if temp - temp2 > 0 then goto L51150
L51090:         u3% = 2%
                call "ASKUSER" (u3%, "*** NOTHING LEFT TO BUILD ***",    ~
                     "The Job you selected (" & jobdestn$ & ") has no Q"&~
                     "uantity Remaining to Build.", " ", "Press (RETURN"&~
                     ") to select another job.")
                if u3% = 0% then goto L51050 else goto L51090
L51150:     pf16msg$ = "(16)Mass DeKit to Destn Job"
L51160
*        Load the Job Order #'s Component list.
            gosub dataload_components
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 2.                      *~
            *************************************************************

        deffn'152(u3%)
            errormsg$ = " "
            if selector$(u3%) <> " " then goto L52130   /* De-selected? */
                call "STRING" addr ("RJ", qtydekit$(u3%), /* Yes-reset */~
                     len(str(qtydekit$(u3%))))
                return

L52130
*        Not de-selected- validate the Store Number for possible editing.
*        Store and Lot are irrelevant if there is a Destination Job #.
            if jobdestn$ <> " " then goto L52550
                if storenbr$(u3%) = "?" then storenbr$(u3%) = " "
                call "GETCODE" (#07, storenbr$(u3%), " ",  /* STORNAME */~
                     0%, 0, f1%(7%))
                if f1%(7%) <> 0% then goto L52230
                     errormsg$ = "You must select a valid Store Code."
                     return

L52230
*        Validate the Lot Number for possible editing, based on the
*        value returned in LOTENABL%(U3%). 0=DISabled; 1=Memo; 2=Strict.
            on lotenabl%(u3%) + 1% goto L52270, L52550, L52290

L52270:     lotnumbr$(u3%) = " " : goto L52550       /* Not Lot Tracked */

L52290:     if lotnumbr$(u3%) <> " " then goto L52330
                errormsg$ = "Lot #s for Strict Lot-Tracked parts may no"&~
                     "t be blank."
                return
L52330:     call "LOTVALID" (componnt$(u3%), storenbr$(u3%),             ~
                lotnumbr$(u3%), #02, #04, #05, errormsg$)
            if errormsg$ <> " " then return
            call "LOTUNQUE" (componnt$(), lotnumbr$(), u3%, #02,         ~
                errormsg$)
            if errormsg$ <> " " then return
            gosub'205(u3%)                  /* Test for duplicate Lots */
            if errormsg$ <> " " then return
            readkey$ = str(componnt$(u3%)) & str(storenbr$(u3%)) &       ~
                lotnumbr$(u3%)
            call "READ100" (#05, readkey$, f1%(5%))         /* HNYQUAN */
                if f1%(5%) <> 0% then goto L52550
L52450:     u2% = 2%                           /* Window at the bottom */
            call "ASKUSER" (u2%, "*** LOT NUMBER ADVISORY ***",          ~
                "The Lot Number you entered (" & lotnumbr$(u3%) & ") do"&~
                "es not currently exist.", "Press PF(16) to continue.",  ~
                "Press PF(1) to re-enter the Lot Number.")
            if u2% <>  1% then goto L52530
                errormsg$ = "Please re-enter the Lot Number."
                return
L52530:     if u2% <> 16% then goto L52450

L52550
*        Validate the Quantity to DeKit from Job.
            if qtydekit$(u3%) = " " then qtydekit$(u3%) = "0"
            call "NUMTEST" (qtydekit$(u3%), 0, 9e9, errormsg$, -2.2,     ~
                qtydekit(u3%))
            if errormsg$ <> " " then return
            if qtydekit(u3%) <= qtykittd(u3%) then return
                errormsg$ = "Qty to DeKit may not exceed Net Quantity K"&~
                     "itted."
                return

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
            *  Copyright (c) 1993  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "SHOSTAT" ("One Moment Please")
            call "JBJNLCLS" ("J2", userid$, modno$, jnlid$, pstseq%, u3%)
            end
