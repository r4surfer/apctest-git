        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  JJJJJ  BBBB    QQQ   JJJJJ   OOO   BBBB                  *~
            *    J    B   B  Q   Q    J    O   O  B   B                 *~
            *    J    BBBB   Q   Q    J    O   O  BBBB                  *~
            *  J J    B   B  Q Q Q  J J    O   O  B   B                 *~
            *   J     BBBB    QQQ    J      OOO   BBBB                  *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBQJOB   - Displays information found in JBMASTR2 record. *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/02/87 ! Original                                 ! ERN *~
            * 11/10/87 ! No longer drops into TXTDSPLY uninvited  ! HES *~
            * 05/15/93 ! Core Project (Pass along Channels)       ! KAB *~
            *          ! PRR 12383 - after job is closed, try for !     *~
            *          !             correct cost set from Closing!     *~
            *          ! Include Closing Adjustments somehow.     !     *~
            * 07/17/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**


        sub "JBQJOB" (job$,                                              ~
                      #1, #2, #4,        /* JBMASTR2 SYSFILE2 HNYMASTR */~
                      #5, #6, #7,        /* PIPOUT   JBMATER2 JBVALUE2 */~
                      #8)                /* JBMASTRC                   */

        dim                                                              ~
            actl_bom(12), actl_rte(12),  /* Actual Job Costs           */~
                          actl_misc(12), /*                            */~
            adj_amt(12),                 /* Adjustments (Closing)      */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            buckets$(12)10,              /* Cost Bucket IDs            */~
            c(12), c1(12), c2(12),       /* Cost Display Work Areas    */~
                   cs(12), c$(13,2)10,   /*                            */~
            c1$(13,2)10,  c2$(13,2)10,   /* Cost Display Columns       */~
            c3$(13,2)10,  c4$(13,2)10,   /*                            */~
            c5$(13,2)10,  c6$(13,2)10,   /*                            */~
            cr_bom(12), cr_tl(12),       /* Credited Job Costs         */~
                        cr_fold(12),     /*                            */~
            cost_msg$(2)79,              /* Cost Summary Message       */~
            cor_db_matl(12),             /* Core Ledger                */~
            cor_db_misc(12),             /* Core Ledger                */~
            cor_cr_dist(12),             /* Core Ledger                */~
            cor_ad_dist(12),             /* Core Ledger                */~
            cor_mv_dist(12),             /* Core Ledger                */~
            cpart$25,                    /* Core Part                  */~
            cpartd$62,                   /* Core Part Descr            */~
            date$8,                      /* Date for screen display    */~
            date_as$8, date_ae$8,        /* Actual Start/End Dates     */~
            date_ps$8, date_pe$8,        /* Plannd Start/End Dates     */~
            disp_descr$79,               /* Display Part Description   */~
            hdr$(7)10,                   /* Column Headers             */~
            inpmessage$79,               /* Informational Message      */~
            job$8, job_descr$32,         /* Job Number                 */~
            kitd_msg$79,                 /* Kit Status Message         */~
            line2$79,                    /* 2nd Line of Screen Header  */~
            part$25, part_descr$62,      /* Part to Build              */~
            pf8$16,                      /* PF8 Prompt                 */~
            pf10$16,                     /* PF10 Prompt                */~
            pf_each$24,                  /* Totals/Per Unit Toggle     */~
            piptag$19,                   /* Planning Tag for Job       */~
            plowkey$99,                  /* Miscellaneous Plow Key     */~
            set$8, set_id$4,             /* Current Cost Set           */~
            jbset$8, jbset_id$4,         /* Cost Set at Closing        */~
            std_bom(12), std_rte(12),    /* Standard Job Costs (Each)  */~
                         std_misc(12),   /*                            */~
                         std_fold(12),   /*                            */~
            std_cbom(12), std_crte(12),  /* Standard Job Costs (Core)  */~
                          std_cmisc(12), /*                            */~
                          std_cfold(12), /*                            */~
            summsg$(9)50, sumcmsg$(9)50, /* Floating Summary Msgs      */~
            text$(196,1)70, textid$4     /* Job Text array and ID      */

        dim f2%(32),                     /* = 0 if the file is open    */~
            f1%(32),                     /* = 1 if READ was successful */~
            fs%(32),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(32)20                  /* Text from file opening     */

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
            * # 1 ! JBMASTR2 ! Production job master file               *~
            * # 2 ! SYSFILE2 ! Caelus Management System Information     *~
            * # 3 ! TXTFILE  ! System Text File                         *~
            * # 4 ! HNYMASTR ! Inventory Master File                    *~
            * # 5 ! PIPOUT   ! PIP Outs                                 *~
            * # 6 ! JBMATER2 ! Job Materials Ledger                     *~
            * # 7 ! JBVALUE2 ! Job Value Ledger                         *~
            * # 8 ! JBMASTRC ! Job Master Core Appendix                 *~
            * # 9 ! COREXREF ! Core Cross Reference File                *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 3, "TXTFILE",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  11

            select #9, "COREXREF",                                       ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =   26, keylen =  50,                     ~
                        alt key  1, keypos =    1, keylen =  50,         ~
                            key  2, keypos =   76, keylen =  25, dup

            call "OPENCHCK" (# 1, fs%( 1), f2%( 1), 0%, rslt$( 1))
            call "OPENCHCK" (# 2, fs%( 2), f2%( 2), 0%, rslt$( 2))
            call "OPENCHCK" (# 4, fs%( 4), f2%( 4), 0%, rslt$( 4))
            call "OPENCHCK" (# 5, fs%( 5), f2%( 5), 0%, rslt$( 5))
            call "OPENCHCK" (# 9, fs%( 9), f2%( 9), 0%, rslt$( 9))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes data and load info on Job.                    *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date  :  call "DATEFMT" (date$)

            if each% = 0% then each% = 1%

            set$ = " "
            call "STCSETID" (2%, #2, set$, set_id$, buckets$())
            hdr$(1) = "Bucket ID"

            call "READ100" (#1, job$, f1%(1))
            if f1%(1) = 0% then end

            gosub load_data

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        main_screen_control
            inpmessage$  = kitd_msg$
L10080:     gosub main_screen_display
                if keyhit%  =  6% then display_actuals
                if keyhit%  =  7% then display_credits
                if keyhit%  =  8% then display_difference
                if keyhit%  =  9% then display_standards
                if keyhit%  = 10% then main_screen_control_core
                if keyhit%  = 14% then gosub cost_detail_report
                if keyhit%  = 16% then exit_program
                if keyhit%  = 26% then gosub text_display
                goto L10080


        cost_screen_control
L10200:     gosub cost_screen_display
                if keyhit%  =  6% then display_actuals
                if keyhit%  =  7% then display_credits
                if keyhit%  =  8% then display_difference
                if keyhit%  =  9% then display_standards
                if keyhit% <> 10% then L10270
                     if each% = 1% then each% = 2% else each% = 1%
L10270:         if keyhit%  = 14% then gosub cost_detail_report
                if keyhit%  = 16% then main_screen_control
                if keyhit%  = 26% then gosub text_display
                goto L10200

        main_screen_control_core
            if core% = 0% then main_screen_control
            inpmessage$  = kitd_msg$
L10340:     gosub main_screen_display_core
                if keyhit%  =  6% then display_actuals_core
                if keyhit%  =  7% then display_credits_core
                if keyhit%  =  8% then display_difference_core
                if keyhit%  =  9% then display_standards_core
                if keyhit%  = 14% then gosub cost_detail_report
                if keyhit%  = 16% then main_screen_control
                if keyhit%  = 26% then gosub text_display
                goto L10340


        cost_screen_control_core
L10460:     gosub cost_screen_display
                if keyhit%  =  6% then display_actuals_core
                if keyhit%  =  7% then display_credits_core
                if keyhit%  =  8% then display_difference_core
                if keyhit%  =  9% then display_standards_core
                if keyhit% <> 10% then L10530
                     if each% = 1% then each% = 2% else each% = 1%
L10530:         if keyhit%  = 14% then gosub cost_detail_report
                if keyhit%  = 16% then main_screen_control_core
                if keyhit%  = 26% then gosub text_display
                goto L10460

        REM *************************************************************~
            *             S U P P O R T   R O U T I N E S               *~
            *-----------------------------------------------------------*~
            * Routines which aid and abet the program logic.            *~
            *************************************************************

        text_display
            call "TXTDSPLY" (#3, f2%(3), "021", line2$, textid$, text$())
            return

        cost_detail_report
            call "JBQCDTLS" (job$, #2, #1, #6, #7, #4, #8)
            return

        display_actuals
            inpmessage$  = kitd_msg$
            disp_descr$ = "Part to Build:" & hex(84) & part_descr$
            cost_msg$()  = " "
            cost_msg$(1) = "Total Actual Job Costs Display."
            call "CONVERT" (qty_make, -0.2, str(cost_msg$(2),,10))
            cost_msg$(2) = "Per Unit Actual Job Costs Display."  &       ~
                           "  Quantity to Build = " & cost_msg$(2) & "."
            hdr$(2) = "     B O M"
            hdr$(3) = "     Route"
            hdr$(4) = "     Misc."
            hdr$(5) = "    Rollup"
            hdr$(6) = "Std Rollup"
            hdr$(7) = "Actl - Std"
            base  = qty_make

            mat c  = actl_bom + cor_mv_dist
                                     : gosub do_column  :  c1$() = c$()
            mat c  = actl_rte        : gosub do_column  :  c2$() = c$()
            mat c  = actl_misc       : gosub do_column  :  c3$() = c$()
            mat c  = actl_bom + cor_mv_dist
            mat c  = c + actl_rte
            mat c  = c + actl_misc
            mat cs = c               : gosub do_column  :  c4$() = c$()
            mat c  = std_bom + std_rte
            mat c  = c       + std_misc
            mat c  = (qty_orig) * c  : gosub do_column  :  c5$() = c$()
            mat c  = cs - c          : gosub do_column  :  c6$() = c$()
            goto cost_screen_control

        display_credits
            inpmessage$  = kitd_msg$
            disp_descr$ = "Part to Build:" & hex(84) & part_descr$
            cost_msg$()  = " "
            cost_msg$(1) = "Total Job Credits Display."
            call "CONVERT" (qty_compl, -0.2, str(cost_msg$(2),,10))
            cost_msg$(2) = "Per Unit Job Credits Display.  Quantity" &   ~
                           " Reported Complete = " & cost_msg$(2) & "."
            hdr$(2) = "     B O M"
            hdr$(3) = "This Level"
            hdr$(4) = "    Rollup"
            hdr$(5) = "Std Rollup"
            hdr$(6) = "Comp - Std"
            hdr$(7) = "  Inv Cost"
            base  = qty_compl

            mat c  = cr_bom          : gosub do_column  :  c1$() = c$()
            mat c  = cr_tl           : gosub do_column  :  c2$() = c$()
            mat c  = cr_bom + cr_tl
            mat cs = c               : gosub do_column  :  c3$() = c$()
            mat c  = std_bom + std_rte
            mat c  = c       + std_misc
            mat c  = (qty_compl) * c : gosub do_column  :  c4$() = c$()
            mat c  = cs - c          : gosub do_column  :  c5$() = c$()
            mat c  = cr_fold         : gosub do_column  :  c6$() = c$()
            goto cost_screen_control

        display_difference
            inpmessage$  = kitd_msg$
            disp_descr$ = "Part to Build:" & hex(84) & part_descr$
            cost_msg$()  = " "
            if qty_left = 0 then L11752
            cost_msg$(1) = "Total Job Costs Difference Display."
            call "CONVERT" (qty_left, -0.2, str(cost_msg$(2),,10))
            cost_msg$(2) = "Per Unit Job Costs Difference Display."  &   ~
                           "  Quantity Left to Build = "             &   ~
                           cost_msg$(2) & "."
            goto L11760
L11752:     cost_msg$(1) = "Total Job Costs Variance Display."
            call "CONVERT" (qty_left, -0.2, str(cost_msg$(2),,10))
            cost_msg$(2) = "Per Unit Job Costs Variance Display."    &   ~
                           "  Quantity Left to Build = "             &   ~
                           cost_msg$(2) & "."

L11760:     hdr$(2) = "     B O M"
            hdr$(3) = "This Level"
            hdr$(4) = "    Rollup"
            hdr$(5) = "Std Rollup"
            hdr$(6) = "Left - Std"
            hdr$(7) = "  Inv Cost"
            base  = qty_left

            mat c  = actl_bom + cor_mv_dist
            mat c  = c - cr_bom        : gosub do_column  :  c1$() = c$()
            mat cs = c
            mat c  = actl_rte + actl_misc
            mat c  = c - cr_tl         : gosub do_column  :  c2$() = c$()
            mat c  = c + cs
            mat cs = c                 : gosub do_column  :  c3$() = c$()
            mat c  = std_bom + std_rte
            mat c  = c       + std_misc
            mat c  = (qty_make) * c    : gosub do_column  :  c4$() = c$()
            mat c  = cs - c            : gosub do_column  :  c5$() = c$()
            mat c1 = actl_bom + cor_mv_dist
            mat c1 = c1 - cr_bom
            mat c2 = actl_rte + actl_misc
            mat c2 = c2 - cr_tl
            mat cs = zer
            call "STCFOLDN" (part$, jbset$, #2, c1(), c2(), cs(), c, c())
                               c = c  :  gosub do_column  :  c6$() = c$()
            goto cost_screen_control

        display_standards
            cost_msg$(), inpmessage$ = " "
            disp_descr$ = "Part to Build:" & hex(84) & part_descr$
            call "CONVERT" (qty_make, -0.2, str(cost_msg$(1),,10))
            cost_msg$(1) = "Standard Cost Display for Job Quantity of " &~
                           cost_msg$(1) & "."
            cost_msg$(2) = "Per Unit Standard Job Costs Display."
            hdr$(2) = "     B O M"
            hdr$(3) = "     Route"
            hdr$(4) = "     Misc."
            hdr$(5) = "This Level"
            hdr$(6) = "   Roll-up"
            hdr$(7) = "   Fold-in"
            base  = qty_make

            mat c = (qty_make) * std_bom  : gosub do_column :c1$() = c$()
            mat c = (qty_make) * std_rte  : gosub do_column :c2$() = c$()
            mat c = (qty_make) * std_misc : gosub do_column :c3$() = c$()
            mat c = std_rte + std_misc
            mat c = (qty_make) * c        : gosub do_column :c4$() = c$()
            mat c = std_bom + std_rte
            mat c = c       + std_misc
            mat c = (qty_make) * c        : gosub do_column :c5$() = c$()
            mat c = (qty_make) * std_fold : gosub do_column :c6$() = c$()
            goto cost_screen_control

        do_column    /* Format, calc eaches, and total column          */
            c$() = " "  :  c1, c2 = 0
            for b% = 1% to 12%
                if c(b%) <> 0 then call "CONVERT" (c(b%), 4.4, c$(b%, 1))
                c1 = c1 + c(b%)

                each = 0 : if base <> 0 then each = c(b%) / base
                if each <> 0 then call "CONVERT" (each, 4.4, c$(b%, 2))
                c2 = c2 + each
            next b%
            call "CONVERT" (c1, 4.4, c$(13, 1))
            call "CONVERT" (c2, 4.4, c$(13, 2))
            return


        display_actuals_core
            inpmessage$  = kitd_msg$
            disp_descr$ = "Core Part:" & hex(84) & cpartd$
            cost_msg$()  = " "
            cost_msg$(1) = "Total Actual Job Costs Display - Core."
            call "CONVERT" (qty_make, -0.2, str(cost_msg$(2),,10))
            cost_msg$(2) = "Per Unit Actual Core Costs Display."  &      ~
                           "  Quantity to Build = " & cost_msg$(2) & "."
            hdr$(2) = "  Material"
            hdr$(3) = "  Indirect"
            hdr$(4) = "    Rollup"
            hdr$(5) = "Std Rollup"
            hdr$(6) = "Actl - Std"
            hdr$(7) = "          "
            base  = qty_make

            mat c  = cor_db_matl     : gosub do_column  :  c1$() = c$()
            mat c  = cor_db_misc     : gosub do_column  :  c2$() = c$()
            mat c  = cor_db_matl + cor_db_misc
            mat cs = c               : gosub do_column  :  c3$() = c$()
            mat c  = std_cbom + std_crte
            mat c  = c        + std_cmisc
            mat c  = (qty_orig) * c  : gosub do_column  :  c4$() = c$()
            mat c  = cs - c          : gosub do_column  :  c5$() = c$()
            c6$()  = " "
            goto cost_screen_control_core

        display_credits_core
            inpmessage$  = kitd_msg$
            disp_descr$ = "Core Part:" & hex(84) & cpartd$
            cost_msg$()  = " "
            cost_msg$(1) = "Job Credits Display - Core."
            call "CONVERT" (qty_compl, -0.2, str(cost_msg$(2),,10))
            cost_msg$(2) = "Per Unit Core Credits Display.  Quantity" &  ~
                           " Reported Complete = " & cost_msg$(2) & "."
            hdr$(2) = " Completed"
            hdr$(3) = "Mve to WIP"
            hdr$(4) = "    Rollup"
            hdr$(5) = "Std Rollup"
            hdr$(6) = "Comp - Std"
            hdr$(7) = "          "
            base  = qty_compl

            mat c  = cor_cr_dist     : gosub do_column  :  c1$() = c$()
            mat c  = cor_mv_dist     : gosub do_column  :  c2$() = c$()
            mat c  = cor_cr_dist + cor_mv_dist
            mat cs = c               : gosub do_column  :  c3$() = c$()
            mat c  = std_cbom + std_crte
            mat c  = c       + std_cmisc
            mat c  = (qty_compl) * c : gosub do_column  :  c4$() = c$()
            mat c  = cs - c          : gosub do_column  :  c5$() = c$()
            c6$()  = " "
            goto cost_screen_control_core

        display_difference_core
            inpmessage$  = kitd_msg$
            disp_descr$ = "Core Part:" & hex(84) & cpartd$
            cost_msg$()  = " "
            if qty_left = 0 then L13650
            cost_msg$(1) = "Total Job Costs Difference Display - Core."
            call "CONVERT" (qty_left, -0.2, str(cost_msg$(2),,10))
            cost_msg$(2) = "Per Unit Core Costs Difference Display."  &  ~
                           "  Quantity Left to Build = "             &   ~
                           cost_msg$(2) & "."
            goto L13710
L13650:     cost_msg$(1) = "Total Job Costs Variance Display - Core."
            call "CONVERT" (qty_left, -0.2, str(cost_msg$(2),,10))
            cost_msg$(2) = "Per Unit Core Costs Variance Display."    &  ~
                           "  Quantity Left to Build = "             &   ~
                           cost_msg$(2) & "."

L13710:     hdr$(2) = " Materials"
            hdr$(3) = "  Indirect"
            hdr$(4) = "  Total DB"
            hdr$(5) = "Completion"
            hdr$(6) = " Mve > WIP"
            hdr$(7) = " Remaining"
            base  = qty_left

            mat c  = cor_db_matl       : gosub do_column  :  c1$() = c$()
            mat c  = cor_db_misc       : gosub do_column  :  c2$() = c$()
            mat c  = cor_db_matl + cor_db_misc
            mat cs = c                 : gosub do_column  :  c3$() = c$()
            mat cs = cs - cor_cr_dist
            mat c  = cor_cr_dist       : gosub do_column  :  c4$() = c$()
            mat cs = cs - cor_mv_dist
            mat c  = cor_mv_dist       : gosub do_column  :  c5$() = c$()
            mat c  = cs                : gosub do_column  :  c6$() = c$()
            goto cost_screen_control_core

        display_standards_core
            cost_msg$(), inpmessage$ = " "
            disp_descr$ = "Core Part:" & hex(84) & cpartd$
            call "CONVERT" (qty_make, -0.2, str(cost_msg$(1),,10))
            cost_msg$(1) = "Standard Core Costs for Job Quantity of " &  ~
                           cost_msg$(1) & "."
            cost_msg$(2) = "Per Unit Standard Core Costs Display."
            hdr$(2) = "     B O M"
            hdr$(3) = "     Route"
            hdr$(4) = "     Misc."
            hdr$(5) = "This Level"
            hdr$(6) = "   Roll-up"
            hdr$(7) = "   Fold-in"
            base  = qty_make

            mat c = (qty_make) * std_cbom : gosub do_column :c1$() = c$()
            mat c = (qty_make) * std_crte : gosub do_column :c2$() = c$()
            mat c = (qty_make) * std_cmisc: gosub do_column :c3$() = c$()
            mat c = std_crte + std_cmisc
            mat c = (qty_make) * c        : gosub do_column :c4$() = c$()
            mat c = std_cbom + std_crte
            mat c = c        + std_cmisc
            mat c = (qty_make) * c        : gosub do_column :c5$() = c$()
            mat c = (qty_make) * std_cfold: gosub do_column :c6$() = c$()
            goto cost_screen_control_core

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        load_data
            get #1 using L35030, job_descr$, piptag$, part$, qty_make,    ~
                                qty_compl, date_as$,date_ae$,            ~
                                date_ps$, date_pe$, qty_orig, qty_scrap, ~
                                qty_rwk, textid$, actl_total, actl_bom(),~
                                actl_rte(), actl_misc(), cr_total,       ~
                                cr_bom(), cr_tl(), cr_fold(), std_each,  ~
                                std_bom(), std_rte(), std_misc(),        ~
                                adj_total, adj_amt(), jbset$, jbset_id$

            mat cor_db_matl = zer : mat cor_db_misc = zer
            mat cor_cr_dist = zer : mat cor_ad_dist = zer
            mat cor_ad_dist = zer
            cor_db_total, cor_cr_total, cor_ad_total, cor_mv_total = 0
            mat std_cbom = zer : mat std_crte = zer
                                 mat std_cmisc = zer
                                 mat std_cfold = zer
            std_ctotal, std_ceach = 0

            call "READ100" (#8, job$, core%)
               if core% = 0% then L30300
            get #8, using L30270,                                         ~
                           cor_db_total, cor_db_matl(), cor_db_misc(),   ~
                           cor_cr_total, cor_cr_dist(),                  ~
                           cor_ad_total, cor_ad_dist(),                  ~
                           cor_mv_total, cor_mv_dist()
L30270:     FMT POS(9), PD(14,4), 12*PD(14,4), 12*PD(14,4), PD(14,4),    ~
                12*PD(14,4), PD(14,4), 12*PD(14,4), PD(14,4), 12*PD(14,4)

L30300:     line2$ = "Job: " & job$ & " (" & job_descr$ & ")"
            str(line2$,64) = "JBQJOB: " & str(cms2v$,,8)

            call "READ100" (#4, part$, f1%(4))
            part_descr$ = "Not Found in Inventory Master"
            if f1%(4) = 1% then get #4 using L30360, part_descr$
L30360:         FMT XX(25), CH(32)
            part_descr$ = part$ & "  (" & part_descr$ & ")"

            call "DATEFMT" (date_as$)
            call "DATEFMT" (date_ae$)
            call "DATEFMT" (date_ps$)
            call "DATEFMT" (date_pe$)

*       ** Quantity Portion of Summary Screen
            qty_adj  = -(qty_orig - qty_scrap - qty_rwk - qty_make)
            qty_left = qty_make - qty_compl
            pf8$ = "(8)Difference"
            if qty_left = 0 then pf8$ = "(8)Variance"

*       ** Value Portion of Summary Screen
            actl_each, cr_each, rem_each, jbt_each = 0
            cor_mv_each, wip_each, cor_each, rem_ceach = 0
            cor_db_each, cor_cr_each = 0

            if qty_make <> 0 then                                        ~
                actl_each = round(actl_total / qty_make, 4)
            if qty_make <> 0 then                                        ~
                cor_db_each = round(cor_db_total / qty_make, 4)

            if qty_compl <> 0 then                                       ~
               cor_mv_each = cor_mv_total / qty_compl

            if qty_compl <> 0 then                                       ~
                cr_each = round(cr_total / qty_compl, 4)
            if qty_compl <> 0 then                                       ~
                cor_cr_each = round(cor_cr_total / qty_compl, 4)

            rem_total = actl_total + cor_mv_total - cr_total
            if qty_left <> 0 then                                        ~
                rem_each = round(rem_total / qty_left, 4)
            rem_ctotal = cor_db_total - (cor_cr_total + cor_mv_total)
            if qty_left <> 0 then                                        ~
                rem_ceach = round(rem_ctotal / qty_left, 4)

            /* No Calculation for Adjustment */

            wip_total = rem_total - adj_total
            if qty_left <> 0 then                                        ~
                wip_each = round(wip_total / qty_left, 4)

            cor_total = cor_db_total -                                   ~
                        (cor_cr_total + cor_ad_total + cor_mv_total)
            if qty_left <> 0 then                                        ~
               cor_each = cor_total / qty_left

            jbt_total = wip_total + cor_total
            if qty_left <> 0 then                                        ~
               jbt_each = jbt_total / qty_left

*       ** Standard Cost Portion of Summary Screen
            if date_ae$ = " " or date_ae$ = blankdate$ then jbset$ = " "
            if jbset$   = " " then jbset$ = set$
            if date_ae$ = blankdate$ or ~
               date_ae$ = " " then call "STCCOSTS" (part$, " ", #2, 3%,  ~
                                       std_each, std_fold(),             ~
                                       std_bom(), std_rte(), std_misc()) ~
                              else call "STCFOLDN" (part$, jbset$, #2,   ~
                                       std_bom(), std_rte(), std_misc(), ~
                                       std_each, std_fold())

            std_total = round(std_each * qty_orig, 4)

            plowkey$ = part$
            cpart$ = "No Core on file"
            cpartd$ = "No Core Cross Reference"
            call "PLOWALTS" (#9, plowkey$, 0%, 25%, f1%(9%))
               if f1%(9%) = 0% then L30898
            core% = 1%  /* Whether or not JBMASTRC exists yet */
            cpart$ = str(plowkey$,26%,25%)
            call "STCCOSTS" (cpart$, " ", #2, 3%, std_ceach, std_cfold(),~
                             std_cbom(), std_crte(), std_cmisc())
            std_ctotal = round(std_ceach * qty_orig, 4)

            call "READ100" (#4, cpart$, f1%(4))
            cpartd$ = "Not Found in Inventory Master"
            if f1%(4) = 1% then get #4 using L30896, cpartd$
L30896:         FMT XX(25), CH(32)

L30898:     cpartd$ = cpart$ & "  (" & cpartd$ & ")"

*       ** Kit Complete or Not Message
            kitd_msg$ = "All Planned Materials have been issued to" &    ~
                        " this Job."
            plowkey$ = all(hex(00))
            str(plowkey$,,19) = "JOB ORDER: " & str(job$)
            call "PLOWNEXT" (#5, plowkey$, 19%, f1%(5)) /* PIPOUT */
            if f1%(5) = 1% then                                          ~
                kitd_msg$ = "Notice: This Job is not completely" &       ~
                            " Kitted.  There are still Components Due."

            init (" ") summsg$(), pf10$, sumcmsg$()

            summsg$(1) = "Std Cost (* Orig)"
            convert std_each  to str(summsg$(1),19%,14%),                ~
                                 pic(-####,###.####)
            convert std_total to str(summsg$(1),34%,14%),                ~
                                 pic(-####,###.####)
            str(summsg$(1),18%,1%) = hex(8c)
            str(summsg$(1),33%,1%) = hex(8c)

            summsg$(2) = "Actual   (/ Curr)"
            convert actl_each  to str(summsg$(2),19%,14%),               ~
                                 pic(-####,###.####)
            convert actl_total to str(summsg$(2),34%,14%),               ~
                                 pic(-####,###.####)
            str(summsg$(2),18%,1%) = hex(84)
            str(summsg$(2),33%,1%) = hex(84)

            if core% <> 0% then goto summsg_core

            summsg$(3) = "Completed (/ Cmp)"
            convert  cr_each  to str(summsg$(3),19%,14%),                ~
                                 pic(-####,###.####)
            convert -cr_total to str(summsg$(3),34%,14%),                ~
                                 pic(-####,###.####)
            str(summsg$(3),18%,1%) = hex(84)
            str(summsg$(3),33%,1%) = hex(84)

            if qty_left = 0 then                                         ~
               summsg$(4) = "Variance  (/ Rem)"                          ~
                            else                                         ~
               summsg$(4) = "Difference(/ Rem)"
            convert rem_each  to str(summsg$(4),19%,14%),                ~
                                 pic(-####,###.####)
            convert rem_total to str(summsg$(4),34%,14%),                ~
                                 pic(-####,###.####)
            str(summsg$(4),18%,1%) = hex(84)
            str(summsg$(4),33%,1%) = hex(84)

            summsg$(5) = "Adjustments      "
            convert -adj_total to str(summsg$(5),34%,14%),               ~
                                 pic(-####,###.####)
            str(summsg$(5),33%,1%) = hex(84)

            summsg$(6) = "Job Total (/ Rem)"
            convert jbt_each  to str(summsg$(6),19%,14%),                ~
                                 pic(-####,###.####)
            convert jbt_total to str(summsg$(6),34%,14%),                ~
                                 pic(-####,###.####)
            str(summsg$(6),18%,1%) = hex(84)
            str(summsg$(6),33%,1%) = hex(84)

            return


        summsg_core

            pf10$ = "(10)Core Display"

            summsg$(3) = "Core WIP  (/ Cmp)"
            convert cor_mv_each  to str(summsg$(3),19%,14%),             ~
                                    pic(-####,###.####)
            convert cor_mv_total to str(summsg$(3),34%,14%),             ~
                                    pic(-####,###.####)
            str(summsg$(3),18%,1%) = hex(84)
            str(summsg$(3),33%,1%) = hex(84)

            summsg$(4) = "Completed (/ Cmp)"
            convert  cr_each  to str(summsg$(4),19%,14%),                ~
                                 pic(-####,###.####)
            convert -cr_total to str(summsg$(4),34%,14%),                ~
                                 pic(-####,###.####)
            str(summsg$(4),18%,1%) = hex(84)
            str(summsg$(4),33%,1%) = hex(84)

            if qty_left = 0 then                                         ~
               summsg$(5) = "Variance  (/ Rem)"                          ~
                            else                                         ~
               summsg$(5) = "Difference(/ Rem)"
            convert rem_each  to str(summsg$(5),19%,14%),                ~
                                 pic(-####,###.####)
            convert rem_total to str(summsg$(5),34%,14%),                ~
                                 pic(-####,###.####)
            str(summsg$(5),18%,1%) = hex(84)
            str(summsg$(5),33%,1%) = hex(84)

            summsg$(6) = "Adjustments      "
            convert -adj_total to str(summsg$(6),34%,14%),               ~
                                 pic(-####,###.####)
            str(summsg$(6),33%,1%) = hex(84)

            summsg$(7) = "WIP Total (/ Rem)"
            convert wip_each  to str(summsg$(7),19%,14%),                ~
                                 pic(-####,###.####)
            convert wip_total to str(summsg$(7),34%,14%),                ~
                                 pic(-####,###.####)
            str(summsg$(7),18%,1%) = hex(84)
            str(summsg$(7),33%,1%) = hex(84)

            summsg$(8) = "Core Total(/ Rem)"
            convert cor_each  to str(summsg$(8),19%,14%),                ~
                                 pic(-####,###.####)
            convert cor_total to str(summsg$(8),34%,14%),                ~
                                 pic(-####,###.####)
            str(summsg$(8),18%,1%) = hex(84)
            str(summsg$(8),33%,1%) = hex(84)

            summsg$(9) = "Job Total (/ Rem)"
            convert jbt_each  to str(summsg$(9),19%,14%),                ~
                                 pic(-####,###.####)
            convert jbt_total to str(summsg$(9),34%,14%),                ~
                                 pic(-####,###.####)
            str(summsg$(9),18%,1%) = hex(84)
            str(summsg$(9),33%,1%) = hex(84)

*       ** Now the Core Summary Messages

            sumcmsg$(1) = "Core STD (* Orig)"
            convert std_ceach  to str(sumcmsg$(1),19%,14%),              ~
                                 pic(-####,###.####)
            convert std_ctotal to str(sumcmsg$(1),34%,14%),              ~
                                 pic(-####,###.####)
            str(sumcmsg$(1),18%,1%) = hex(8c)
            str(sumcmsg$(1),33%,1%) = hex(8c)

            sumcmsg$(2) = "Core DB  (/ Curr)"
            convert cor_db_each  to str(sumcmsg$(2),19%,14%),            ~
                                 pic(-####,###.####)
            convert cor_db_total to str(sumcmsg$(2),34%,14%),            ~
                                 pic(-####,###.####)
            str(sumcmsg$(2),18%,1%) = hex(84)
            str(sumcmsg$(2),33%,1%) = hex(84)

            sumcmsg$(3) = "Core CR   (/ Cmp)"
            convert cor_cr_each  to str(sumcmsg$(3),19%,14%),            ~
                                    pic(-####,###.####)
            convert -cor_cr_total to str(sumcmsg$(3),34%,14%),           ~
                                    pic(-####,###.####)
            str(sumcmsg$(3),18%,1%) = hex(84)
            str(sumcmsg$(3),33%,1%) = hex(84)

            sumcmsg$(4) = "Core > WIP(/ Cmp)"
            convert cor_mv_each  to str(sumcmsg$(4),19%,14%),            ~
                                 pic(-####,###.####)
            convert -cor_mv_total to str(sumcmsg$(4),34%,14%),           ~
                                 pic(-####,###.####)
            str(sumcmsg$(4),18%,1%) = hex(84)
            str(sumcmsg$(4),33%,1%) = hex(84)

            if qty_left = 0 then                                         ~
               sumcmsg$(5) = "Variance  (/ Rem)"                         ~
                            else                                         ~
               sumcmsg$(5) = "Difference(/ Rem)"
            convert rem_ceach  to str(sumcmsg$(5),19%,14%),              ~
                                 pic(-####,###.####)
            convert rem_ctotal to str(sumcmsg$(5),34%,14%),              ~
                                 pic(-####,###.####)
            str(sumcmsg$(5),18%,1%) = hex(84)
            str(sumcmsg$(5),33%,1%) = hex(84)

            sumcmsg$(6) = "Adjustments      "
            convert -cor_ad_total to str(sumcmsg$(6),34%,14%),           ~
                                 pic(-####,###.####)
            str(sumcmsg$(6),33%,1%) = hex(84)

            sumcmsg$(7) = "Core WIP  (/ Rem)"
            convert cor_each  to str(sumcmsg$(7),19%,14%),               ~
                                 pic(-####,###.####)
            convert cor_total to str(sumcmsg$(7),34%,14%),               ~
                                 pic(-####,###.####)
            str(sumcmsg$(7),18%,1%) = hex(84)
            str(sumcmsg$(7),33%,1%) = hex(84)

            sumcmsg$(8) = "WIP Total (/ Rem)"
            convert wip_each  to str(sumcmsg$(8),19%,14%),               ~
                                 pic(-####,###.####)
            convert wip_total to str(sumcmsg$(8),34%,14%),               ~
                                 pic(-####,###.####)
            str(sumcmsg$(8),18%,1%) = hex(84)
            str(sumcmsg$(8),33%,1%) = hex(84)

            sumcmsg$(9) = "Job Total (/ Rem)"
            convert jbt_each  to str(sumcmsg$(9),19%,14%),               ~
                                 pic(-####,###.####)
            convert jbt_total to str(sumcmsg$(9),34%,14%),               ~
                                 pic(-####,###.####)
            str(sumcmsg$(9),18%,1%) = hex(84)
            str(sumcmsg$(9),33%,1%) = hex(84)

            return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35030: FMT                 /* FILE: JBMASTR2                          */~
            XX(8),          /* Production job code                     */~
            CH(30),         /* Description of production job           */~
            CH(19),         /* Tag number in level 2 planning          */~
            CH(25),         /* Part code                               */~
            PD(14,4),       /* Quantity to make                        */~
            PD(14,4),       /* Quantity completed to date              */~
            XX(48),         /* Filler (Internal, unused space)         */~
            CH(6),          /* Date production job actually started    */~
            CH(6),          /* Date production job actually ended      */~
            XX(9),          /* Work in process GL account code         */~
            CH(6),          /* Date production job planned to start    */~
            CH(6),          /* Date production job planned to be comple*/~
            PD(14,4),       /* Original quantity ordered               */~
            XX(24),         /* Filler (Internal, unused space)         */~
            PD(14,4),       /* The quantity of parts scrapped          */~
            PD(14,4),       /* The quantity of parts reworked          */~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            PD(14,4),       /* Actual (aka inventory) Costs            */~
            12*PD(14,4),    /* Actual (aka inventory) Costs            */~
            12*PD(14,4),    /* Actual (aka inventory) Costs            */~
            12*PD(14,4),    /* Actual (aka inventory) Costs            */~
            PD(14,4),       /* Total Credits from Job.                 */~
            12*PD(14,4),    /* Total Credits from Job.                 */~
            12*PD(14,4),    /* Total Credits from Job.                 */~
            12*PD(14,4),    /* Total Credits from Job.                 */~
            PD(14,4),       /* Cost associated with Standard Cost      */~
            12*PD(14,4),    /* Cost associated with Standard Cost      */~
            12*PD(14,4),    /* Cost associated with Standard Cost      */~
            12*PD(14,4),    /* Cost associated with Standard Cost      */~
            POS(1147),      /*                                         */~
            PD(14,4),       /* Closing adjustments                     */~
            12*PD(14,4),    /* Closing adjustments                     */~
            POS(1251),      /*                                         */~
            CH(8),          /* Cost Set at Closing                     */~
            CH(4)           /* Cost Set ID at Closing                  */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        main_screen_display

L40080:     accept                                                       ~
               at (01,02), "Job Status Summary",                         ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (04,02), "Part to Build: ",                            ~
               at (04,17), fac(hex(84))  , part_descr$,                  ~
                                                                         ~
               at (06,02), "Planned Start Date: MM/DD/YY    End: ",      ~
               at (06,22), fac(hex(84))  , date_ps$             , ch(08),~
               at (06,39), fac(hex(84))  , date_pe$             , ch(08),~
               at (06,50), "PIP Tag   ",                                 ~
               at (06,61), fac(hex(84))  , piptag$              , ch(19),~
               at (07,02), " Actual Start Date: MM/DD/YY    End: ",      ~
               at (07,22), fac(hex(84))  , date_as$             , ch(08),~
               at (07,39), fac(hex(84))  , date_ae$             , ch(08),~
                                                                         ~
               at (09,02), "------ QUANTITY SUMMARY ------",             ~
               at (10,02), "Orig to Build  ",                            ~
               at (10,18), fac(hex(84))  , qty_orig, pic(-##,###,###.##),~
               at (11,02), "Curr. to Build ",                            ~
               at (11,18), fac(hex(84))  , qty_make, pic(-##,###,###.##),~
               at (12,02), "Left to Build  ",                            ~
               at (12,18), fac(hex(84))  , qty_left, pic(-##,###,###.##),~
               at (13,02), "Completed      ",                            ~
               at (13,18), fac(hex(84))  , qty_compl,pic(-##,###,###.##),~
               at (14,02), "Scrapped       ",                            ~
               at (14,18), fac(hex(84))  , qty_scrap,pic(-##,###,###.##),~
               at (15,02), "Sent to Rework ",                            ~
               at (15,18), fac(hex(84))  , qty_rwk , pic(-##,###,###.##),~
               at (16,02), "Adjusted       ",                            ~
               at (16,18), fac(hex(84))  , qty_adj , pic(-##,###,###.##),~
                                                                         ~
               at (09,33),                                               ~
                     "---------------  COSTS SUMMARY  ---------------",  ~
               at (10,33),                                               ~
                     "                        Per Unit          Total",  ~
               at (11,33), fac(hex(8c)), summsg$(1)             , ch(48),~
               at (12,33), fac(hex(8c)), summsg$(2)             , ch(48),~
               at (13,33), fac(hex(8c)), summsg$(3)             , ch(48),~
               at (14,33), fac(hex(8c)), summsg$(4)             , ch(48),~
               at (15,33), fac(hex(8c)), summsg$(5)             , ch(48),~
               at (16,33), fac(hex(8c)), summsg$(6)             , ch(48),~
               at (17,33), fac(hex(8c)), summsg$(7)             , ch(48),~
               at (18,33), fac(hex(8c)), summsg$(8)             , ch(48),~
               at (19,33), fac(hex(8c)), summsg$(9)             , ch(48),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "Cost Breakdown Displays:",                   ~
               at (23,02), " (6)Actual  (8)Difference",                  ~
               at (23,14), fac(hex(8c)), pf8$                   , ch(16),~
               at (24,02), " (7)Credit  (9)Standards ",                  ~
               at (22,36), fac(hex(8c)), pf10$                  , ch(16),~
               at (23,36), "(14)Job Cost Details",                       ~
               at (24,36), "(26)Display Text",                           ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)RETURN",                                 ~
                     keys(hex(060708090a0d0e0f101a)), key(keyhit%)

            if keyhit% <> 13 then L40810
                call "MANUAL" ("JBQJOB  ")
                goto L40080

L40810:     if keyhit% <> 15 then return
                call "PRNTSCRN"
                goto L40080

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        cost_screen_display
            if each% = 1% then pf_each$ = "(10)Show value per Unit"      ~
                          else pf_each$ = "(10)Show value for Job "
            call "STRING" addr("CT", cost_msg$(each%), 79%)

L41110:     accept                                                       ~
               at (01,02), "Job Status Summary",                         ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(8c)), disp_descr$            , ch(79),~
               at (04,02), "Std. Cost Set: ",                            ~
               at (04,17), fac(hex(84))  , jbset$,                       ~
               at (05,02), fac(hex(84))  , cost_msg$(each%),             ~
                                                                         ~
               at (06,02), fac(hex(ac))  , hdr$(1)              , ch(10),~
               at (06,13), fac(hex(ac))  , hdr$(2)              , ch(10),~
               at (06,24), fac(hex(ac))  , hdr$(3)              , ch(10),~
               at (06,35), fac(hex(ac))  , hdr$(4)              , ch(10),~
               at (06,46), fac(hex(ac))  , hdr$(5)              , ch(10),~
               at (06,57), fac(hex(ac))  , hdr$(6)              , ch(10),~
               at (06,68), fac(hex(ac))  , hdr$(7)              , ch(10),~
                                                                         ~
               at (07,02), fac(hex(8c))  , buckets$( 1)         , ch(10),~
               at (08,02), fac(hex(8c))  , buckets$( 2)         , ch(10),~
               at (09,02), fac(hex(8c))  , buckets$( 3)         , ch(10),~
               at (10,02), fac(hex(8c))  , buckets$( 4)         , ch(10),~
               at (11,02), fac(hex(8c))  , buckets$( 5)         , ch(10),~
               at (12,02), fac(hex(8c))  , buckets$( 6)         , ch(10),~
               at (13,02), fac(hex(8c))  , buckets$( 7)         , ch(10),~
               at (14,02), fac(hex(8c))  , buckets$( 8)         , ch(10),~
               at (15,02), fac(hex(8c))  , buckets$( 9)         , ch(10),~
               at (16,02), fac(hex(8c))  , buckets$(10)         , ch(10),~
               at (17,02), fac(hex(8c))  , buckets$(11)         , ch(10),~
               at (18,02), fac(hex(8c))  , buckets$(12)         , ch(10),~
               at (19,02), "* TOTALS *",                                 ~
                                                                         ~
               at (07,13), fac(hex(84))  , c1$( 1, each%)       , ch(10),~
               at (08,13), fac(hex(84))  , c1$( 2, each%)       , ch(10),~
               at (09,13), fac(hex(84))  , c1$( 3, each%)       , ch(10),~
               at (10,13), fac(hex(84))  , c1$( 4, each%)       , ch(10),~
               at (11,13), fac(hex(84))  , c1$( 5, each%)       , ch(10),~
               at (12,13), fac(hex(84))  , c1$( 6, each%)       , ch(10),~
               at (13,13), fac(hex(84))  , c1$( 7, each%)       , ch(10),~
               at (14,13), fac(hex(84))  , c1$( 8, each%)       , ch(10),~
               at (15,13), fac(hex(84))  , c1$( 9, each%)       , ch(10),~
               at (16,13), fac(hex(84))  , c1$(10, each%)       , ch(10),~
               at (17,13), fac(hex(84))  , c1$(11, each%)       , ch(10),~
               at (18,13), fac(hex(a4))  , c1$(12, each%)       , ch(10),~
               at (19,13), fac(hex(84))  , c1$(13, each%)       , ch(10),~
                                                                         ~
               at (07,24), fac(hex(84))  , c2$( 1, each%)       , ch(10),~
               at (08,24), fac(hex(84))  , c2$( 2, each%)       , ch(10),~
               at (09,24), fac(hex(84))  , c2$( 3, each%)       , ch(10),~
               at (10,24), fac(hex(84))  , c2$( 4, each%)       , ch(10),~
               at (11,24), fac(hex(84))  , c2$( 5, each%)       , ch(10),~
               at (12,24), fac(hex(84))  , c2$( 6, each%)       , ch(10),~
               at (13,24), fac(hex(84))  , c2$( 7, each%)       , ch(10),~
               at (14,24), fac(hex(84))  , c2$( 8, each%)       , ch(10),~
               at (15,24), fac(hex(84))  , c2$( 9, each%)       , ch(10),~
               at (16,24), fac(hex(84))  , c2$(10, each%)       , ch(10),~
               at (17,24), fac(hex(84))  , c2$(11, each%)       , ch(10),~
               at (18,24), fac(hex(a4))  , c2$(12, each%)       , ch(10),~
               at (19,24), fac(hex(84))  , c2$(13, each%)       , ch(10),~
                                                                         ~
               at (07,35), fac(hex(84))  , c3$( 1, each%)       , ch(10),~
               at (08,35), fac(hex(84))  , c3$( 2, each%)       , ch(10),~
               at (09,35), fac(hex(84))  , c3$( 3, each%)       , ch(10),~
               at (10,35), fac(hex(84))  , c3$( 4, each%)       , ch(10),~
               at (11,35), fac(hex(84))  , c3$( 5, each%)       , ch(10),~
               at (12,35), fac(hex(84))  , c3$( 6, each%)       , ch(10),~
               at (13,35), fac(hex(84))  , c3$( 7, each%)       , ch(10),~
               at (14,35), fac(hex(84))  , c3$( 8, each%)       , ch(10),~
               at (15,35), fac(hex(84))  , c3$( 9, each%)       , ch(10),~
               at (16,35), fac(hex(84))  , c3$(10, each%)       , ch(10),~
               at (17,35), fac(hex(84))  , c3$(11, each%)       , ch(10),~
               at (18,35), fac(hex(a4))  , c3$(12, each%)       , ch(10),~
               at (19,35), fac(hex(84))  , c3$(13, each%)       , ch(10),~
                                                                         ~
               at (07,46), fac(hex(84))  , c4$( 1, each%)       , ch(10),~
               at (08,46), fac(hex(84))  , c4$( 2, each%)       , ch(10),~
               at (09,46), fac(hex(84))  , c4$( 3, each%)       , ch(10),~
               at (10,46), fac(hex(84))  , c4$( 4, each%)       , ch(10),~
               at (11,46), fac(hex(84))  , c4$( 5, each%)       , ch(10),~
               at (12,46), fac(hex(84))  , c4$( 6, each%)       , ch(10),~
               at (13,46), fac(hex(84))  , c4$( 7, each%)       , ch(10),~
               at (14,46), fac(hex(84))  , c4$( 8, each%)       , ch(10),~
               at (15,46), fac(hex(84))  , c4$( 9, each%)       , ch(10),~
               at (16,46), fac(hex(84))  , c4$(10, each%)       , ch(10),~
               at (17,46), fac(hex(84))  , c4$(11, each%)       , ch(10),~
               at (18,46), fac(hex(a4))  , c4$(12, each%)       , ch(10),~
               at (19,46), fac(hex(84))  , c4$(13, each%)       , ch(10),~
                                                                         ~
               at (07,57), fac(hex(84))  , c5$( 1, each%)       , ch(10),~
               at (08,57), fac(hex(84))  , c5$( 2, each%)       , ch(10),~
               at (09,57), fac(hex(84))  , c5$( 3, each%)       , ch(10),~
               at (10,57), fac(hex(84))  , c5$( 4, each%)       , ch(10),~
               at (11,57), fac(hex(84))  , c5$( 5, each%)       , ch(10),~
               at (12,57), fac(hex(84))  , c5$( 6, each%)       , ch(10),~
               at (13,57), fac(hex(84))  , c5$( 7, each%)       , ch(10),~
               at (14,57), fac(hex(84))  , c5$( 8, each%)       , ch(10),~
               at (15,57), fac(hex(84))  , c5$( 9, each%)       , ch(10),~
               at (16,57), fac(hex(84))  , c5$(10, each%)       , ch(10),~
               at (17,57), fac(hex(84))  , c5$(11, each%)       , ch(10),~
               at (18,57), fac(hex(a4))  , c5$(12, each%)       , ch(10),~
               at (19,57), fac(hex(84))  , c5$(13, each%)       , ch(10),~
                                                                         ~
               at (07,68), fac(hex(84))  , c6$( 1, each%)       , ch(10),~
               at (08,68), fac(hex(84))  , c6$( 2, each%)       , ch(10),~
               at (09,68), fac(hex(84))  , c6$( 3, each%)       , ch(10),~
               at (10,68), fac(hex(84))  , c6$( 4, each%)       , ch(10),~
               at (11,68), fac(hex(84))  , c6$( 5, each%)       , ch(10),~
               at (12,68), fac(hex(84))  , c6$( 6, each%)       , ch(10),~
               at (13,68), fac(hex(84))  , c6$( 7, each%)       , ch(10),~
               at (14,68), fac(hex(84))  , c6$( 8, each%)       , ch(10),~
               at (15,68), fac(hex(84))  , c6$( 9, each%)       , ch(10),~
               at (16,68), fac(hex(84))  , c6$(10, each%)       , ch(10),~
               at (17,68), fac(hex(84))  , c6$(11, each%)       , ch(10),~
               at (18,68), fac(hex(a4))  , c6$(12, each%)       , ch(10),~
               at (19,68), fac(hex(84))  , c6$(13, each%)       , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "Cost Breakdown Displays:",                   ~
               at (23,02), " (6)Actual  (8)Difference",                  ~
               at (23,14), fac(hex(8c)), pf8$                   , ch(16),~
               at (24,02), " (7)Credit  (9)Standards ",                  ~
               at (22,36), fac(hex(8c)), pf_each$,   /* PF 10  */        ~
               at (23,36), "(14)Job Cost Details",                       ~
               at (24,36), "(26)Display Text",                           ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Summary Scrn",                           ~
                     keys(hex(00060708090a0d0e0f101a)), key(keyhit%)

               if keyhit% <> 13 then L42440
                  call "MANUAL" ("JBQJOB  ")
                  goto L41110

L42440:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L41110

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        main_screen_display_core

L44080:     accept                                                       ~
               at (01,02), "Job Status Summary - Core",                  ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (04,02), "Core Part: ",                                ~
               at (04,13), fac(hex(84))  , cpartd$,                      ~
                                                                         ~
               at (06,02), "Planned Start Date: MM/DD/YY    End: ",      ~
               at (06,22), fac(hex(84))  , date_ps$             , ch(08),~
               at (06,39), fac(hex(84))  , date_pe$             , ch(08),~
               at (06,50), "PIP Tag   ",                                 ~
               at (06,61), fac(hex(84))  , piptag$              , ch(19),~
               at (07,02), " Actual Start Date: MM/DD/YY    End: ",      ~
               at (07,22), fac(hex(84))  , date_as$             , ch(08),~
               at (07,39), fac(hex(84))  , date_ae$             , ch(08),~
                                                                         ~
               at (09,02), "------ QUANTITY SUMMARY ------",             ~
               at (10,02), "Orig to Build  ",                            ~
               at (10,18), fac(hex(84))  , qty_orig, pic(-##,###,###.##),~
               at (11,02), "Curr. to Build ",                            ~
               at (11,18), fac(hex(84))  , qty_make, pic(-##,###,###.##),~
               at (12,02), "Left to Build  ",                            ~
               at (12,18), fac(hex(84))  , qty_left, pic(-##,###,###.##),~
               at (13,02), "Completed      ",                            ~
               at (13,18), fac(hex(84))  , qty_compl,pic(-##,###,###.##),~
               at (14,02), "Scrapped       ",                            ~
               at (14,18), fac(hex(84))  , qty_scrap,pic(-##,###,###.##),~
               at (15,02), "Sent to Rework ",                            ~
               at (15,18), fac(hex(84))  , qty_rwk , pic(-##,###,###.##),~
               at (16,02), "Adjusted       ",                            ~
               at (16,18), fac(hex(84))  , qty_adj , pic(-##,###,###.##),~
                                                                         ~
               at (09,33),                                               ~
                     "---------------  COSTS SUMMARY  ---------------",  ~
               at (10,33),                                               ~
                     "                        Per Unit          Total",  ~
               at (11,33), fac(hex(8c)), sumcmsg$(1)            , ch(48),~
               at (12,33), fac(hex(8c)), sumcmsg$(2)            , ch(48),~
               at (13,33), fac(hex(8c)), sumcmsg$(3)            , ch(48),~
               at (14,33), fac(hex(8c)), sumcmsg$(4)            , ch(48),~
               at (15,33), fac(hex(8c)), sumcmsg$(5)            , ch(48),~
               at (16,33), fac(hex(8c)), sumcmsg$(6)            , ch(48),~
               at (17,33), fac(hex(8c)), sumcmsg$(7)            , ch(48),~
               at (18,33), fac(hex(8c)), sumcmsg$(8)            , ch(48),~
               at (19,33), fac(hex(8c)), sumcmsg$(9)            , ch(48),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "Cost Breakdown Displays:",                   ~
               at (23,02), " (6)Actual  (8)Difference",                  ~
               at (23,14), fac(hex(8c)), pf8$                   , ch(16),~
               at (24,02), " (7)Credit  (9)Standards ",                  ~
               at (23,36), "(14)Job Cost Details",                       ~
               at (24,36), "(26)Display Text",                           ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Job Summary",                            ~
                     keys(hex(060708090d0e0f101a)), key(keyhit%)

            if keyhit% <> 13 then L44730
                call "MANUAL" ("JBQJOB  ")
                goto L44080

L44730:     if keyhit% <> 15 then return
                call "PRNTSCRN"
                goto L44080

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
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

        exit_program
            end
