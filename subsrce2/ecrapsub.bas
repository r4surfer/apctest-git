        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  EEEEE   CCC   RRRR    AAA   PPPP    SSS   U   U  BBBB    *~
            *  E      C   C  R   R  A   A  P   P  S      U   U  B   B   *~
            *  EEEE   C      RRRR   AAAAA  PPPP    SSS   U   U  BBBB    *~
            *  E      C   C  R   R  A   A  P          S  U   U  B   B   *~
            *  EEEEE   CCC   R   R  A   A  P       SSS    UUU   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ECRAPSUB - This subroutine is used to initially define the*~
            *            approvals which apply to a specific ECR,       *~
            *            maintain that list of approval types for the   *~
            *            ECR, record Actual approvals/denials, and      *~
            *            finally allow view/display only access to the  *~
            *            approvals for a specific ECR.                  *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/31/94 ! Original                                 ! LDJ *~
            * 07/15/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        sub "ECRAPSUB" (operation$,      /* "N" for NEW ECR - select   */~
                                         /*     applicable Approval    */~
                                         /*     Types                  */~
                                         /* "E" for Enter Approval /   */~
                                         /*     Denial info this ECR   */~
                                         /* "M" for Maintain Applicable*/~
                                         /*     Approval Types this ECR*/~
                                         /* "V" for View Approvals Only*/~
                                         /* "D" for Delete all Approval*/~
                                         /*     information for thisECR*/~
                                         /* "S" Save current Data      */~
                                         /* "L" Load Data for this ECR#*/~
                        ecr_number$,     /* ECR Number     CH(16)      */~
                        title$,          /* Title of ECR   CH(40)      */~
                        reason$,         /* Reason for ECR CH(15)      */~
                        reasondescr$,    /* Reason for ECR CH(32) Descr*/~
                        status$,         /* Current Status CH(15)      */~
                        statusdescr$,    /* Current Status CH(32) Descr*/~
                        #01,             /* USERLCMS                   */~
                        #02,             /* GENCODES                   */~
                        #03,             /* ECRAUTHU                   */~
                        #04,             /* ECRMASTR                   */~
                        #05,             /* ECRAPPRV                   */~
                        #06,             /* SYSFILE2                   */~
                        #07,             /* HNYMASTR                   */~
                        #08,             /* BOMMASTR                   */~
                        #09,             /* RTEMASTR                   */~
                        #10,             /* ENGMASTR                   */~
                        #11,             /* CALMASTR                   */~
                        #12,             /* TXTFILE                    */~
                        #13,             /* STCBOMXF                   */~
                        return_code%)    /* Return Code to Caller      */

        dim                                                              ~
            bfac$(100)1,                 /* Field Attribute Characters */~
            blankdate$8,                 /* Blank date for comparison  */~
            bom$3,                       /* Temp BOM ID variable       */~
            columnhdr$(2)80,             /* PLOWCODE Column Headers    */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            descr_map(12),               /* PLOWCODE Description Map   */~
            dfac$(100)1,                 /* Field Attribute Characters */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            ffac$(100)1,                 /* Field Attribute Characters */~
            i$(24)80,                    /* Screen Image               */~
            incl_excl(1), incl_excl$(1)3,/* PLOWCODE Include/Exclude   */~
            inpmessage$79,               /* Informational Message      */~
            last_ecr$,                   /* Last ECR Processed         */~
            lfac$(100)1,                 /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            line9$79,                    /* Screen Line #9 Column Hdg  */~
            p%(2),                       /* Search Position Receiver   */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pf16$24,                     /* PF16 Link Prompt Text      */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            run_from$16,                 /* Run From Program/Proc/Menu */~
            runlib$8,                    /* Override Run Library       */~
            runvol$6,                    /* Override Run Volume        */~
            texta$(196,1)70,             /* Text Matrix for TXTINSUB   */~
            textmsg$79,                  /* Message to TXTINSUB        */~
            userid$3                     /* Current User Id            */~

        dim                              /* ECR SPECIFIC VARIABLES     */~
            approval_date$(100)10,       /* Date Granted or Denied     */~
            approval_granted$(100)1,     /* Approval Granted? (Y/N)    */~
            approval_required$(100)1,    /* Approval Required? (Y/N)   */~
            approval_type$(100)15,       /* Approval Types             */~
            approval_typedescr$(100)30,  /* Approval Types             */~
            approved_by$(100)3,          /* Approved By User           */~
            approved_bydescr$(100)30,    /* Approved By User           */~
            approved_for$(100)30,        /* Approved For               */~
            bom_id$3,                    /* Based on BOM Revision ID   */~
            bom_iddescr$32,              /* Based on BOM Revision ID   */~
            changed_by$3,                /*             ECR Changed By */~
            changed_bydescr$32,          /*             ECR Changed By */~
            control$8,                   /* Control Number             */~
            coord_by$30,                 /* ECR Coordinated By         */~
            created_by$3,                /*             ECR Entered By */~
            created_bydescr$32,          /*             ECR Entered By */~
            date_changed$10,             /* System Date ECR Changed    */~
            date_created$10,             /* System Date ECR Entered    */~
            date_implemented$10,         /* Date Enacted / Implemented */~
            disp_descr$(5)40,            /* Description of Disposition */~
            drawing$16,                  /* Drawing Number / Reference */~
            ecr_descr$(5)40,             /* Description of ECR         */~
            ecr_number$16,               /* ECR Number                 */~
            filler$64,                   /* Unused record area         */~
            fillra$33,                   /* Unused Record Area         */~
            implemented_by$30,           /* Enacted / Implemented By   */~
            lot$16,                      /* First Lot Number           */~
            net_cost_chg$15,             /* Expected Net Chg in Cost   */~
            new_bomid$3,                 /* Implemented in BOM Rev ID  */~
            new_bomiddescr$32,           /* Implemented in BOM Rev ID  */~
            new_route_id$3,              /* Implemented in Route ID    */~
            new_route_iddescr$32,        /* Implemented in Route ID    */~
            orig_by$30,                  /* ECR Originated By          */~
            orig_date$10,                /* Date Originated            */~
            part$25,                     /* Part Assembly Code         */~
            partdescr$34,                /* Part Assembly Code         */~
            reason$15,                   /* Reason for ECR             */~
            reasondescr$32,              /* Reason for ECR             */~
            rte_id$3,                    /* Based on Route ID          */~
            save_control$8,              /* Control Number temp storage*/~
            sched_date$10,               /* Scheduled Implementation   */~
            serial_nbr$20,               /* First Serial Number        */~
            severity$15,                 /* Severity / Priority Code   */~
            severitydescr$32,            /* Severity / Priority Code   */~
            status$15,                   /* Current Status             */~
            statusdescr$32,              /* Current Status             */~
            textid$4,                    /* X-ref to ECR General Text  */~
            textid2$4,                   /* X-ref to ECR Approval Text */~
            textid$(100)4,               /* Point To Text in TXTFILE   */~
            time_changed$8,              /*        Time ECR Changed    */~
            time_created$8,              /*        Time ECR Entered    */~
            title$40,                    /* Title of ECR               */~
            vfs$200                      /* Variable Fields            */

        dim f1%(14)                      /* = 1 if READ was successful */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! USERLCMS ! CMS Program Access Control User Info fil *~
            * #02 ! GENCODES ! System General Codes file.               *~
            * #03 ! ECRAUTHU ! ECT Users who may Approve/Deny ECR Appro *~
            * #04 ! ECRMASTR ! ECT Engineering Change Request Master Fi *~
            * #05 ! ECRAPPRV ! ECT module ECR Approvals Tracking file   *~
            * #10 ! ENGMASTR ! Engineering Calendar Master              *~
            * #11 ! CALMASTR ! Manufacturing Calendar Master File       *~
            * #12 ! TXTFILE  ! System Text File                         *~
            * #13 ! STCBOMXF ! BOM and Std Cost Cross Reference File    *~
            *************************************************************~

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            return_code% = 0%
            if pos("NMEVDSL" = operation$) > 0% then L09120
                call "ASKUSER" (keyhit%, "INVALID CALLING SYNTAX",       ~
                     "OPERATION Argument contains invalid value",        ~
                     "Terminating Function ECTAPSUB!",                   ~
                     "(" & operation$ & ")")
                return_code% = 99%
                goto exit_program
L09120:     call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            operation% = pos("NMEVDSL" = operation$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62) = "ECRAPSUB: " & str(cms2v$,,8)

            REM *** Determine if User can Change Approvals for this ECR***
            get #04 using L09220, created_by$
L09220:     FMT POS(943), CH(3)
            if created_by$ = userid$ then maint_enabled% = 1%            ~
                                     else maint_enabled% = 0%

        REM *************************************************************~
            *                   M A I N   P R O G R A M                 *~
            *-----------------------------------------------------------*~
            * MAIN Processing / Control Section                         *~
            *************************************************************

            on operation% gosub select_approval_types, /* New ECR      */~
                                maintain_approv_types, /* Add/Delete   */~
                                enter_approval_data,   /* Enter Approva*/~
                                view_approvals_info,   /* View/Display */~
                                delete_approvals_data, /* Delete Apprvs*/~
                                save_approvals_data,   /* Save to File */~
                                load_approvals_data    /* Load from Fil*/
            goto exit_program

        select_approval_types
*       ****************************************************************
*          Routine used when saving a new ECR.  Called by ECRINPUT.    *
*          First Load all Approval Types from GENCODES into Array      *
*          Next display screen and ask user to select (Y/N) which      *
*          Approval Types apply to current ECR.  If User elects to     *
*          Exit without saving return to caller with Return Code set.  *
*          Else after user finishes selection write to ECRAPPRV file   *
*          and then Exit.                                              *
*       ****************************************************************
            if max_lines% > 0% and ecr_number$ = last_ecr$ then L10670
            gosub initialize_variables
            plowkey$ = ecr_number$
            call "DELETE" (#05, plowkey$, 16%) /* Delete Any Partials  */
            plowkey$ = "ECRAPPROV"
L10300:     call "PLOWNEXT" (#02, plowkey$, 9%, f1%(2%))
            if f1%(2%) = 0% then L10380
                c% = min(dim(approval_type$(),1), c% + 1%)
                get #02 using L10350, approval_type$(c%),                 ~
                                     approval_typedescr$(c%)
L10350:         FMT POS(10), CH(15), CH(30)
                approval_required$(c%) = "Y" /* Approval Type Required?*/
                goto L10300
L10380:     max_lines% = c%
            if max_lines% > 0% then L10670
L10400:         call "ASKUSER" (keyhit%, "SETUP ERROR!",                 ~
                     "You have not defined the ECT Approval Types Yet!", ~
                     "Press RETURN to define the approval Types NOW, or",~
                     "Press PF16 to CANCEL Entry of this ECR.")
                if keyhit% <> 0% and keyhit% <> 16% then L10400
                if keyhit% <> 16% then L10480
                     return_code% = 32%
                     return
L10480:         REM *** Run GENCODES Setup Proc for Approval Types ***
                close ws
                call "CMSLINK" addr(#1,           /* USERLCMS File     */~
                           userid$,      /* Retrieve Run Info Using ID */~
                           "R",          /* 'R' = Run or Link to the   */~
                           "PROCGC53",   /* Program or Proc to Run     */~
                           runlib$,      /* Out: Library Run From      */~
                           runvol$,      /* Out: Volume  Run From      */~
                           " ",          /* ' ' = Default.             */~
                           "Y",          /* 'Y' = Message User if Not  */~
                                         /*       able to run program. */~
                           pf16$,        /* Optional Text for Command  */~
                           run_from$,    /* Optional - Name of Menu /  */~
                                         /* Program / Proc being run   */~
                           "N",          /* Transparent Link? (Y/N)    */~
                           comp%,        /* LINK Completion Return Code*/~
                           return_code%) /* Return Code passed back    */
                goto select_approval_types

L10670:     str(line2$,,50%) = "Select Relevant Approvals for New ECR"
            inpmessage$ = "Select the Relevant Approvals for this ECR " &~
                          "by entering 'Y' or Deselect with 'N'"
            gosub display_approval_types
            if keyhit% = 1% then select_approval_types
            if keyhit% <> 32% then L10760
               gosub initialize_variables
               return_code% = 32%
               return
L10760:     REM *** Return & Wait for Save! **
            return_code% = 0%
            last_ecr$ = ecr_number$
            return

        maintain_approv_types  /* Add/Delete   */
            c% = max_lines%
            plowkey$ = "ECRAPPROV"
L10840:     call "PLOWNEXT" (#02, plowkey$, 9%, f1%(2%))
            if f1%(2%) = 0% then L10950
                search approval_type$() = str(plowkey$,10%,15%) to p%()  ~
                                         step 15%
                if p%(1%) > 0% then L10840          /* Already here     */
                c% = min(dim(approval_type$(),1), c% + 1%)
                get #02 using L10920, approval_type$(c%),                 ~
                                     approval_typedescr$(c%)
L10920:         FMT POS(10), CH(15), CH(30)
                approval_required$(c%) = "N" /* Approval Type Required?*/
                goto L10840
L10950:     max_lines% = c%
            str(line2$,,50%) = "Edit the Relevant Approvals for this ECR"
            inpmessage$ = "ReSelect the Relevant Approvals for this ECR "~
                        & "by entering 'Y' or 'N'"
            gosub display_approval_types
            if keyhit% = 1% then gosub load_approvals_data
            if keyhit% = 1% then maintain_approv_types
            if keyhit% <> 32% then L11060
               gosub load_approvals_data
               return_code% = 32%
               return
L11060:     REM *** Return & Wait for Save! **
            for x% = 1% to max_lines%
                if approval_required$(x%) <> " " then L11110
                     x% = dim(approval_required$(), 1)
                     goto L11290
L11110:         if approval_required$(x%) <> "N" then L11290
                if textid$(c%) < hex(ffffffff) then L11290
                for y% = x% to max_lines%
                     approval_type$    (y%) = approval_type$    (y%+1%)
                     approved_by$      (y%) = approved_by$      (y%+1%)
                     approval_required$(y%) = approval_required$(y%+1%)
                     approval_granted$ (y%) = approval_granted$ (y%+1%)
                     approval_date$    (y%) = approval_date$    (y%+1%)
                     textid$           (y%) = textid$           (y%+1%)
                next y%
                approval_type$    (y%) = " "
                approved_by$      (y%) = " "
                approval_required$(y%) = " "
                approval_granted$ (y%) = " "
                approval_date$    (y%) = " "
                textid$           (y%) = all(hex(ff))
                max_lines% = max_lines% - 1%
                x% = x% - 1%
L11290:     next x%
            return_code% = 0%
            return

        display_approval_types
            o% = 0%                      /* Offset into Arrays         */
L11350:     gosub'101(1%)               /* Display Screen - W/ Entry   */
                  if keyhit%  =  1% then       return
                  if keyhit%  =  2% then o% = 0%
                  if keyhit%  =  3% then o% = max(0%, max_lines% - 10%)
                  if keyhit%  =  4% then o% = max(0%, o% - 10%)
                  if keyhit%  =  5% then o% =                            ~
                                 max(0%, min(max_lines% - 10%, o% + 10%))
                  if keyhit%  =  6% then o% = max(0%, o% - 1%)
                  if keyhit%  =  7% then o% =                            ~
                                 max(0%, min(max_lines% - 1%, o% + 1%))
                  if keyhit%  = 32% then       return
                  if keyhit% <> 0% and keyhit% <> 16% then L11350
            gosub'151(1%)               /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11350
                  if keyhit%  = 16% then       return
                  goto L11350

        view_approvals_info    /* View/Display */
            gosub initialize_variables
            call "READ100" (#4, ecr_number$, f1%(4%))
            if f1%(4%) = 1% then gosub load_ecr_data
            inpmessage$ = " "
            edit%, return_code%, o% = 0%
L11580:     gosub'103(edit%)            /* Display Screen              */
                  errormsg$ = " "
                  if keyhit% <>  1% then       L11610
                     call "PLOWNEXT" (#4, ecr_number$, 0%, f1%(4%))
                     goto view_approvals_info
L11610:           if f1%(4%)  =  0% then L11700
                  if keyhit%  =  2% then o% = 0%
                  if keyhit%  =  3% then o% = max(0%, max_lines% - 9%)
                  if keyhit%  =  4% then o% = max(0%, o% - 9%)
                  if keyhit%  =  5% then o% =                            ~
                                 max(0%, min(max_lines% - 9%, o% + 9%))
                  if keyhit% <>  9% then L11670
                                         gosub'105(0%) /* ECR Info     */
                                         goto L11580
L11670:           if keyhit%  = 10% then gosub display_bom_rte_xref
                  if keyhit%  = 11% then gosub display_bom_effectivity
                  if keyhit%  = 12% then gosub display_bom_xplosion
L11700:           if keyhit%  = 16% then       return
                  if keyhit%  = 32% then       return
                  if keyhit%  = 25% then gosub display_ecr_text
                  if keyhit%  = 26% then gosub display_disp_text
                  if keyhit%  = 27% then gosub display_text
                  if keyhit% <> 0% and keyhit% <> 1% then L11580
            gosub'153(1%)               /* Edit Field for Valid Entry  */
                  goto L11580

        enter_approval_data    /* Enter Approva*/
            str(line2$,,50%) = "Change Required Approvals Status for " & ~
                               "this ECR"
            inpmessage$ = "Provide the Approval/Denial data for the " &  ~
                          "Approval Types you have authority for"
            edit% = 1%
            gosub display_approval_data
            if keyhit% <> 1% then L11890
                gosub load_approvals_data
                goto enter_approval_data
L11890:     if keyhit% <> 32% then L11930
                gosub load_approvals_data
                return_code% = 32%
                return
L11930:     return_code% = 0%
            return

        display_approval_data
L11970:     gosub'102(edit%)            /* Display Screen              */
                  if keyhit%  =  1% then       return
                  if keyhit%  =  2% then o% = 0%
                  if keyhit%  =  3% then o% = max(0%, max_lines% - 10%)
                  if keyhit%  =  4% then o% = max(0%, o% - 10%)
                  if keyhit%  =  5% then o% =                            ~
                                 max(0%, min(max_lines% - 10%, o% + 10%))
                  if keyhit%  =  6% then o% = max(0%, o% - 1%)
                  if keyhit%  =  7% then o% =                            ~
                                 max(0%, min(max_lines% - 1%, o% + 1%))
                  if keyhit% <>  8% then       L12100
                                         gosub maintain_approv_types
                                         goto  display_approval_data
L12100:           if keyhit%  = 32% then       return
                  if keyhit%  = 25% then gosub manage_text
                  if keyhit%  = 26% then gosub display_text
                  if keyhit% <> 0% and keyhit% <> 16% then L11970
            gosub'151(2%)               /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11970
                  if keyhit%  = 16% then       return
                  goto L11970

        manage_text
            if cursor%(1%) < 10% or cursor%(1%) > 19% then return
            row% = cursor%(1%) - 9%
            c% = o% + row%
            if c% > max_lines% then return
            if lfac$(c%) = hex(84) then return /* Not allowed to Update*/
            textmsg$ = "ECR " & ecr_number$ & ", Text for Approval Type:"~
                     & " " & approval_type$(c%)
            call "TXTINSUB" (#12,   u3%   , "036", textmsg$, textid$(c%),~
                                                             texta$())
            return

        display_text
            if operation$ = "V" then max_row% = 8% else max_row% = 10%
            if operation$ = "V" then row% = cursor%(1%) - 12%            ~
                                else row% = cursor%(1%) - 9%
            if row% < 1% or row% > max_row% then return
            c% = o% + row%
            if c% > max_lines% then return
            textmsg$ = "ECR " & ecr_number$ & ", Text for Approval Type:"~
                     & " " & approval_type$(c%)
            call "TXTDSPLY" (#12,   u3%   , "036", textmsg$, textid$(c%),~
                                                             texta$())
            return

        display_ecr_text
            textmsg$ = "ECR " & ecr_number$ & "  " & title$
            call "TXTDSPLY" (#12,   u3%   , "034", textmsg$, textid$,    ~
                                                             texta$())
            return

        display_disp_text
            textmsg$ = "Disposition Text for ECR " & ecr_number$
            call "TXTDSPLY" (#12,   u3%   , "035", textmsg$, textid2$,   ~
                                                             texta$())
            return

        delete_approvals_data  /* Delete Apprvs*/
            gosub initialize_variables
            plowkey$ = ecr_number$
L12590:     call "PLOWNXT1" (#05, plowkey$, 16%, f1%(5%))
            if f1%(5%) = 0% then L12660
            c% = c% + 1%
            gosub dataload
            delete #5
            call "TXTFUTIL" (#12,   u3%   , "DELE", textid$(c%))
            goto L12590
L12660:     return_code% = 0%
            return

        save_approvals_data    /* Save to File */
            plowkey$ = ecr_number$
            call "DELETE" (#05, plowkey$, 16%)
            REM *** Write Out Selected Approvals **
            for c% = 1% to max_lines%
                if approval_required$(c%) = "Y" then gosub dataput
            next c%
            return_code% = 0%
            return

        load_approvals_data    /* Load From File */
            gosub initialize_variables
        load_approvals_data_2  /* Second Entry Point */
            plowkey$ = ecr_number$
L12830:     call "PLOWNEXT" (#05, plowkey$, 16%, f1%(5%))
            if f1%(5%) = 0% then L12880
            c% = min(dim(approval_type$(),1), c% + 1%)
            gosub dataload
            goto L12830
L12880:     max_lines% = c%
            return_code% = 0%
            return

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(hex(ff)) textid$()
            c% = 0%       /* Approval Types Line Counter               */
            max_lines% = 0%  /* Number of Approval Types in Arrays     */
            o% = 0%       /* Current Offset Into Arrays                */
            init(" ") errormsg$, inpmessage$, fillra$,                   ~
                      approval_date$(), approval_granted$(),             ~
                      approval_required$(), approval_type$(),            ~
                      approval_typedescr$(), approved_by$(),             ~
                      approved_bydescr$(), approved_for$()

            if operation$ <> "V" then return  /* if not View ECR stuff */
            init(" ")                                                    ~
                      bom_id$, bom_iddescr$, changed_by$,                ~
                      changed_bydescr$, control$, coord_by$,             ~
                      created_by$, created_bydescr$,                     ~
                      date_created$, date_implemented$, disp_descr$(),   ~
                      drawing$, ecr_descr$(),                            ~
                      implemented_by$, lot$, net_cost_chg$, new_bomid$,  ~
                      new_bomiddescr$, new_route_id$,                    ~
                      new_route_iddescr$, orig_by$, orig_date$, part$,   ~
                      partdescr$, reason$, reasondescr$, rte_id$,        ~
                                    sched_date$, serial_nbr$,            ~
                      severity$, severitydescr$, status$,                ~
                      statusdescr$,                time_created$,        ~
                      title$, vfs$, date_changed$, time_changed$
            textid$, textid2$ = all(hex(ff))
            call "TXTFUTIL" (#12, u3%,      "INTL", textid$)
            sn_length% = 20%             /* Maximum S/N Field Length   */
            lot_length% = 6%             /* Maximum LOT Field Length   */
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *************************************************************

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload
            get #05 using L35030, /* FILE: ECRAPPRV                     */~
            ecr_number$,    /* ECR Number                              */~
            approval_type$(c%),    /* ECR Approval Type Code           */~
            approved_by$(c%),      /* ECR Approval Granted/Denied by Us*/~
            approval_required$(c%),/* Is this Approval Type required fo*/~
            approval_granted$(c%), /* Approval to this ECR granted for */~
            approval_date$(c%),    /* Date Approval granted or denied t*/~
            approved_for$(c%),     /* Name of Individual/Mgr User enter*/~
            textid$(c%),           /* ECR Approved/denied text pointer */~
            date_changed$,         /* Date Record Was Last Changed     */~
            time_changed$,         /* Time record was last modified    */~
            changed_by$,           /* User ID that last modified the re*/~
            fillra$                /* Unused Space                     */

            if approval_date$(c%) <> " " then                            ~
                call "DATFMTC" (approval_date$(c%))

            call "TXTFUTIL" (#12,   u3%   , "LOAD", textid$(c%))
            return

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from ECRMASTR Record  into Program Variables.  *~
            *************************************************************
        load_ecr_data

        get #04 using L35180,/* FILE: ECRMASTR                          */~
            control$,       /* ECR Control Number                      */~
            ecr_number$,    /* ECR Number                              */~
            title$,         /* Title of ECR                            */~
            orig_by$,       /* Originator of Engineering Change Request*/~
            coord_by$,      /* Coordinator of Engineering Change Reques*/~
            part$,          /* Assembly Part Code                      */~
            bom_id$,        /* BOM ID version                          */~
            rte_id$,        /* Route ID version                        */~
            orig_date$,     /* ECR Origination Date                    */~
            sched_date$,    /* Date ECR is due/scheduled to be implemen*/~
            reason$,        /* ECR Reason Code                         */~
            severity$,      /* ECR Severity Code                       */~
            net_cost_chg,   /* Net change in Product per unit cost due */~
            ecr_descr$(),   /* Short description of ECR                */~
            status$,        /* ECR Status Code                         */~
            date_implemented$, /* ECR Change Enacted Date              */~
            implemented_by$,/* ECR Change Enacted By Name              */~
            disp_descr$(),  /* Short Description of ECR disposition    */~
            new_bomid$,     /* New BOM ID containing implemented ECR ch*/~
            new_route_id$,  /* New Route ID implementing ECR change.   */~
            drawing$,       /* Drawing Reference                       */~
            serial_nbr$,    /* Serial Number                           */~
            lot$,           /* Lot Number                              */~
            vfs$,           /* Variable Fields Data Area               */~
            textid$,        /* General ECR Text pointer                */~
            textid2$,       /* ECR Approved/denied text pointer        */~
            date_created$,  /* The system date a file or record was cre*/~
            time_created$,  /* The system time a file or record was cre*/~
            created_by$,    /* Record created by User ID               */~
            date_changed$,  /* Date Record Was Last Changed            */~
            time_changed$,  /* Time record was last modified           */~
            changed_by$,    /* User ID that last modified the record   */~
            filler$         /* Unused Space                            */

            plowkey$  = "ECRREASON" & reason$
            call "DESCRIBE" (#02, plowkey$, reasondescr$, 1%, f1%(2%))
            call "DESCRIBE" (#07, part$, partdescr$, 1%, f1%(7%))
            plowkey$ = str(part$) & str(bom_id$) & "  0"
            call "DESCRIBE" (#08, plowkey$, bom_iddescr$, 1%, f1%(8%))
            plowkey$  = "ECRSEVERI" & severity$
            call "DESCRIBE" (#02, plowkey$, severitydescr$, 1%, f1%(2%))
            plowkey$  = "ECRSTATUS" & status$
            call "DESCRIBE" (#02, plowkey$, statusdescr$, 1%, f1%(2%))
            plowkey$ = str(part$) & str(new_bomid$) & "  0"
            call "DESCRIBE" (#08, plowkey$, new_bomiddescr$, 1%, f1%(8%))
            call "DESCRIBE" (#01, created_by$,created_bydescr$,1%,f1%(1%))
            call "DESCRIBE" (#01, changed_by$,changed_bydescr$,1%,f1%(1%))
            call "DATFMTC" (orig_date$)
            call "DATFMTC" (sched_date$)
            call "DATFMTC" (date_implemented$)
            call "DATFMTC" (date_created$)
            call "DATFMTC" (date_changed$)
            call "TIMEOK"  (time_changed$, time_created, error$)
            call "TIMEOK"  (time_created$, time_created, error$)
            call "CONVERT" (net_cost_chg, 2.4, net_cost_chg$)
            call "TXTFUTIL" (#12, u3%,      "LOAD", textid$) /*Load Text*/
            call "TXTFUTIL" (#12, u3%,      "LOAD",textid2$) /*Load Text*/
*          Load Approvals Information In                              *
            gosub load_approvals_data_2
            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            date_changed$ = date
            time_changed$ = time
            changed_by$   = userid$

            call "DATUFMTC" (approval_date$(c%))

            put #05 using L35030, /* FILE: ECRAPPRV                     */~
            ecr_number$,           /* ECR Number                       */~
            approval_type$(c%),    /* ECR Approval Type Code           */~
            approved_by$(c%),      /* ECR Approval Granted/Denied by Us*/~
            approval_required$(c%),/* Is this Approval Type required fo*/~
            approval_granted$(c%), /* Approval to this ECR granted for */~
            approval_date$(c%),    /* Date Approval granted or denied t*/~
            approved_for$(c%),     /* Name of Individual/Mgr User enter*/~
            textid$(c%),           /* ECR Approved/denied text pointer */~
            date_changed$,         /* Date Record Was Last Changed     */~
            time_changed$,         /* Time record was last modified    */~
            changed_by$,           /* User ID that last modified the re*/~
            fillra$                /* Unused Space                     */

            write #05
            call "TXTFUTIL" (#12,   u3%   , "TOS2", textid$(c%))
            return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35030: FMT                 /* FILE: ECRAPPRV                          */~
            CH(16),         /* ECR Number                              */~
            CH(15),         /* ECR Approval Type Code                  */~
            CH(3),          /* ECR Approval Granted/Denied by User ID  */~
            CH(1),          /* Is this Approval Type required for this */~
            CH(1),          /* Approval to this ECR granted for this Ap*/~
            CH(8),          /* Date Approval granted or denied to this */~
            CH(30),         /* Name of Individual/Mgr User enters appro*/~
            CH(4),          /* ECR Approved/denied text pointer        */~
            CH(8),          /* Date Record Was Last Changed            */~
            CH(6),          /* Time record was last modified           */~
            CH(3),          /* User ID that last modified the record   */~
            CH(33)          /* Unused Space                            */

L35180: FMT                 /* FILE: ECRMASTR                          */~
            CH(8),          /* ECR Control Number                      */~
            CH(16),         /* ECR Number                              */~
            CH(40),         /* Title of ECR                            */~
            CH(30),         /* Originator of Engineering Change Request*/~
            CH(30),         /* Coordinator of Engineering Change Reques*/~
            CH(25),         /* Assembly Part Code                      */~
            CH(3),          /* BOM ID version                          */~
            CH(3),          /* Route ID version                        */~
            CH(8),          /* ECR Origination Date                    */~
            CH(8),          /* Date ECR is due/scheduled to be implemen*/~
            CH(15),         /* ECR Reason Code                         */~
            CH(15),         /* ECR Severity Code                       */~
            PD(15,4),       /* Net change in Product per unit cost due */~
            5*CH(40),       /* Short description of ECR                */~
            CH(15),         /* ECR Status Code                         */~
            CH(8),          /* ECR Change Enacted Date                 */~
            CH(30),         /* ECR Change Enacted By Name              */~
            5*CH(40),       /* Short Description of ECR disposition    */~
            CH(3),          /* New BOM ID containing implemented ECR ch*/~
            CH(3),          /* New Route ID implementing ECR change.   */~
            CH(16),         /* Drawing Reference                       */~
            CH(20),         /* Serial Number                           */~
            CH(16),         /* Lot Number                              */~
            CH(200),        /* Variable Fields Data Area               */~
            CH(4),          /* General ECR Text pointer                */~
            CH(4),          /* ECR Approved/denied text pointer        */~
            CH(8),          /* The system date a file or record was cre*/~
            CH(6),          /* The system time a file or record was cre*/~
            CH(3),          /* Record created by User ID               */~
            CH(8),          /* Date Record Was Last Changed            */~
            CH(6),          /* Time record was last modified           */~
            CH(3),          /* User ID that last modified the record   */~
            CH(62)          /* Unused Space                            */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Initial Entry & Maintenance of Applicable Approvals.      *~
            *************************************************************

        deffn'101(fieldnr%)
              line9$ = "Required?    Approval Type       Description"
              gosub set_pf1
              init(hex(86)) lfac$()
              if max_lines% = 0% or fieldnr% = 0% then L40060
              for x% = 1% to max_lines%
                  if approval_granted$(x%) = " " and                     ~
                     textid$(x%) = hex(ffffffff) then lfac$(x%) = hex(81)
              next x%
L40060:       if errormsg$ = " " or c% < 1% then L40135
                 lfac$(c%) = hex(b1)
                 if c% - o% > 0% and c% - o% < 11% then L40135
                     o% = max(0%, min(max_lines% - 10%, c% - 1%))

L40135:     accept                                                       ~
               at (01,02),                                               ~
                  "Record Approvals/Denials for ECR's",                  ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "For ECR:",                                   ~
               at (04,18), fac(hex(84)),   ecr_number$          , ch(16),~
                                                                         ~
               at (04,40), fac(hex(84)),   title$               , ch(40),~
                                                                         ~
               at (05,02), "Current Status:",                            ~
               at (05,18), fac(hex(84)),   status$              , ch(15),~
               at (05,40), fac(hex(84)),   statusdescr$         , ch(32),~
                                                                         ~
               at (06,02), "Reason for ECR:",                            ~
               at (06,18), fac(hex(84)),   reason$              , ch(15),~
               at (06,40), fac(hex(84)),   reasondescr$         , ch(32),~
                                                                         ~
               at (08,02), "Approval",                                   ~
               at (09,02), fac(hex(ac)), line9$                 , ch(79),~
               at (10,02), fac(lfac$(o%+01%)),approval_required$(o%+01%),~
               at (11,02), fac(lfac$(o%+02%)),approval_required$(o%+02%),~
               at (12,02), fac(lfac$(o%+03%)),approval_required$(o%+03%),~
               at (13,02), fac(lfac$(o%+04%)),approval_required$(o%+04%),~
               at (14,02), fac(lfac$(o%+05%)),approval_required$(o%+05%),~
               at (15,02), fac(lfac$(o%+06%)),approval_required$(o%+06%),~
               at (16,02), fac(lfac$(o%+07%)),approval_required$(o%+07%),~
               at (17,02), fac(lfac$(o%+08%)),approval_required$(o%+08%),~
               at (18,02), fac(lfac$(o%+09%)),approval_required$(o%+09%),~
               at (19,02), fac(lfac$(o%+10%)),approval_required$(o%+10%),~
                                                                         ~
               at (10,15), fac(hex(8c)),      approval_type$    (o%+01%),~
               at (11,15), fac(hex(8c)),      approval_type$    (o%+02%),~
               at (12,15), fac(hex(8c)),      approval_type$    (o%+03%),~
               at (13,15), fac(hex(8c)),      approval_type$    (o%+04%),~
               at (14,15), fac(hex(8c)),      approval_type$    (o%+05%),~
               at (15,15), fac(hex(8c)),      approval_type$    (o%+06%),~
               at (16,15), fac(hex(8c)),      approval_type$    (o%+07%),~
               at (17,15), fac(hex(8c)),      approval_type$    (o%+08%),~
               at (18,15), fac(hex(8c)),      approval_type$    (o%+09%),~
               at (19,15), fac(hex(8c)),      approval_type$    (o%+10%),~
                                                                         ~
               at (10,35), fac(hex(8c)),     approval_typedescr$(o%+01%),~
               at (11,35), fac(hex(8c)),     approval_typedescr$(o%+02%),~
               at (12,35), fac(hex(8c)),     approval_typedescr$(o%+03%),~
               at (13,35), fac(hex(8c)),     approval_typedescr$(o%+04%),~
               at (14,35), fac(hex(8c)),     approval_typedescr$(o%+05%),~
               at (15,35), fac(hex(8c)),     approval_typedescr$(o%+06%),~
               at (16,35), fac(hex(8c)),     approval_typedescr$(o%+07%),~
               at (17,35), fac(hex(8c)),     approval_typedescr$(o%+08%),~
               at (18,35), fac(hex(8c)),     approval_typedescr$(o%+09%),~
               at (19,35), fac(hex(8c)),     approval_typedescr$(o%+10%),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L40400
                  if operation$ = "N" then                               ~
                     call "MANUAL" ("ECRINPUT")                          ~
                  else                                                   ~
                     call "MANUAL" ("ECRAPSUB")
                  goto L40135

L40400:        if keyhit% <> 15% then L40425
                  call "PRNTSCRN" : goto L40135

L40425:        return

        set_pf1
            pf$(1) = "(1)Reload Entries                       " &        ~
                     "                       (13)Instructions"
            pf$(2) = "(2)First (4)Prev (6)Up1                 " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "(3)Last  (5)Next (7)Down1               " &        ~
                     "(32)Exit-No Save       (16)Edit&Return "
            pfkeys$ = hex(01020304050607ffffffff200dff0f1000)
            if o% + 10% < max_lines% then L40490
                str(pf$(3%),,25%) = " "  :  str(pfkeys$,3%,1%)= hex(ff)
                                            str(pfkeys$,5%,1%)= hex(ff)
                                            str(pfkeys$,7%,1%)= hex(ff)
L40490:     if o% > 0% then L40500
                str(pf$(2%),,25%) = " "  :  str(pfkeys$,2%,1%)= hex(ff)
                                            str(pfkeys$,4%,1%)= hex(ff)
                                            str(pfkeys$,6%,1%)= hex(ff)
L40500:     return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Enter / edit approvals & denials information              *~
            *************************************************************

        deffn'102(fieldnr%)
              line9$ = "Approval Type   Granted?  Grant Date  By   For"
              gosub set_pf2
              init(hex(84)) lfac$(), bfac$(), dfac$(), dfac$(), ffac$()
              if max_lines% = 0% or fieldnr% = 0% then L41160
              for x% = o% + 1% to o% + 10%
                  if x% > max_lines% then L41109
                  readkey$ = str(approval_type$(x%)) & userid$
                  call "READ100" (#03, readkey$, f1%(3%))
                  if f1%(3%) = 0% then L41109
                  init(hex(81)) lfac$(x%), bfac$(x%), dfac$(x%)
                  init(hex(80)) ffac$(x%)
L41109:       next x%
L41160:       if errormsg$ = " " or c% < 1% then L41390
                 if c% - o% > 0% and c% - o% < 11% then L41200
                     o% = max(0%, min(max_lines% - 10%, c% - 1%))
L41200:          on error% gosub L41260,            /* Approved/Denied */ ~
                                 L41290,            /* Approved Date   */ ~
                                 L41320,            /* Approved By     */ ~
                                 L41350             /* Approved For    */
                 goto L41390

L41260:          REM *** Set Approved/Denied Error Fac       ***
                     lfac$(c%) = hex(b1)           /* Blink, UC        */
                     return
L41290:          REM *** Set Approved Date   Error Fac       ***
                     dfac$(c%) = hex(b1)           /* Blink, UC        */
                     return
L41320:          REM *** Set Approved By     Error Fac       ***
                     bfac$(c%) = hex(b1)           /* Blink, UC        */
                     return
L41350:          REM *** Set Approved For    Error Fac       ***
                     lfac$(c%) = hex(b0)           /* Blink, LC        */
                     return

L41390:     accept                                                       ~
               at (01,02),                                               ~
                  "Record Approvals/Denials for ECR's",                  ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "For ECR:",                                   ~
               at (04,18), fac(hex(84)),   ecr_number$          , ch(16),~
                                                                         ~
               at (04,40), fac(hex(84)),   title$               , ch(40),~
                                                                         ~
               at (05,02), "Current Status:",                            ~
               at (05,18), fac(hex(84)),   status$              , ch(15),~
               at (05,40), fac(hex(84)),   statusdescr$         , ch(32),~
                                                                         ~
               at (06,02), "Reason for ECR:",                            ~
               at (06,18), fac(hex(84)),   reason$              , ch(15),~
               at (06,40), fac(hex(84)),   reasondescr$         , ch(32),~
                                                                         ~
               at (09,02), fac(hex(ac)), line9$                 , ch(79),~
                                                                         ~
               at (10,02), fac(hex(8c)),      approval_type$    (o%+01%),~
               at (11,02), fac(hex(8c)),      approval_type$    (o%+02%),~
               at (12,02), fac(hex(8c)),      approval_type$    (o%+03%),~
               at (13,02), fac(hex(8c)),      approval_type$    (o%+04%),~
               at (14,02), fac(hex(8c)),      approval_type$    (o%+05%),~
               at (15,02), fac(hex(8c)),      approval_type$    (o%+06%),~
               at (16,02), fac(hex(8c)),      approval_type$    (o%+07%),~
               at (17,02), fac(hex(8c)),      approval_type$    (o%+08%),~
               at (18,02), fac(hex(8c)),      approval_type$    (o%+09%),~
               at (19,02), fac(hex(8c)),      approval_type$    (o%+10%),~
                                                                         ~
               at (10,20), fac(lfac$(o%+01%)),approval_granted$ (o%+01%),~
               at (11,20), fac(lfac$(o%+02%)),approval_granted$ (o%+02%),~
               at (12,20), fac(lfac$(o%+03%)),approval_granted$ (o%+03%),~
               at (13,20), fac(lfac$(o%+04%)),approval_granted$ (o%+04%),~
               at (14,20), fac(lfac$(o%+05%)),approval_granted$ (o%+05%),~
               at (15,20), fac(lfac$(o%+06%)),approval_granted$ (o%+06%),~
               at (16,20), fac(lfac$(o%+07%)),approval_granted$ (o%+07%),~
               at (17,20), fac(lfac$(o%+08%)),approval_granted$ (o%+08%),~
               at (18,20), fac(lfac$(o%+09%)),approval_granted$ (o%+09%),~
               at (19,20), fac(lfac$(o%+10%)),approval_granted$ (o%+10%),~
                                                                         ~
               at (10,28), fac(dfac$(o%+01%)),approval_date$    (o%+01%),~
               at (11,28), fac(dfac$(o%+02%)),approval_date$    (o%+02%),~
               at (12,28), fac(dfac$(o%+03%)),approval_date$    (o%+03%),~
               at (13,28), fac(dfac$(o%+04%)),approval_date$    (o%+04%),~
               at (14,28), fac(dfac$(o%+05%)),approval_date$    (o%+05%),~
               at (15,28), fac(dfac$(o%+06%)),approval_date$    (o%+06%),~
               at (16,28), fac(dfac$(o%+07%)),approval_date$    (o%+07%),~
               at (17,28), fac(dfac$(o%+08%)),approval_date$    (o%+08%),~
               at (18,28), fac(dfac$(o%+09%)),approval_date$    (o%+09%),~
               at (19,28), fac(dfac$(o%+10%)),approval_date$    (o%+10%),~
                                                                         ~
               at (10,40), fac(bfac$(o%+01%)),approved_by$      (o%+01%),~
               at (11,40), fac(bfac$(o%+02%)),approved_by$      (o%+02%),~
               at (12,40), fac(bfac$(o%+03%)),approved_by$      (o%+03%),~
               at (13,40), fac(bfac$(o%+04%)),approved_by$      (o%+04%),~
               at (14,40), fac(bfac$(o%+05%)),approved_by$      (o%+05%),~
               at (15,40), fac(bfac$(o%+06%)),approved_by$      (o%+06%),~
               at (16,40), fac(bfac$(o%+07%)),approved_by$      (o%+07%),~
               at (17,40), fac(bfac$(o%+08%)),approved_by$      (o%+08%),~
               at (18,40), fac(bfac$(o%+09%)),approved_by$      (o%+09%),~
               at (19,40), fac(bfac$(o%+10%)),approved_by$      (o%+10%),~
                                                                         ~
               at (10,45), fac(ffac$(o%+01%)),approved_for$     (o%+01%),~
               at (11,45), fac(ffac$(o%+02%)),approved_for$     (o%+02%),~
               at (12,45), fac(ffac$(o%+03%)),approved_for$     (o%+03%),~
               at (13,45), fac(ffac$(o%+04%)),approved_for$     (o%+04%),~
               at (14,45), fac(ffac$(o%+05%)),approved_for$     (o%+05%),~
               at (15,45), fac(ffac$(o%+06%)),approved_for$     (o%+06%),~
               at (16,45), fac(ffac$(o%+07%)),approved_for$     (o%+07%),~
               at (17,45), fac(ffac$(o%+08%)),approved_for$     (o%+08%),~
               at (18,45), fac(ffac$(o%+09%)),approved_for$     (o%+09%),~
               at (19,45), fac(ffac$(o%+10%)),approved_for$     (o%+10%),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L42270
                  call "MANUAL" ("ECRAPSUB") : goto L41390

L42270:        if keyhit% <> 15 then L42300
                  call "PRNTSCRN" : goto L41390

L42300:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
            pf$(1) = "(1)Reload Entries (8)Reselect Approvals " &        ~
                     "(25)Enter Comments     (13)Instructions"
            pf$(2) = "(2)First (4)Prev (6)Up1                 " &        ~
                     "(26)View  Comments     (15)Print Screen"
            pf$(3) = "(3)Last  (5)Next (7)Down1               " &        ~
                     "(32)Exit (No Save)     (16)Edit&Return "
            pfkeys$ = hex(0102030405060708191aff200dff0f1000)
            if o% + 10% < max_lines% then L42440
                str(pf$(3%),,25%) = " "  :  str(pfkeys$,3%,1%)= hex(ff)
                                            str(pfkeys$,5%,1%)= hex(ff)
                                            str(pfkeys$,7%,1%)= hex(ff)
L42440:     if o% > 0% then L42480
                str(pf$(2%),,25%) = " "  :  str(pfkeys$,2%,1%)= hex(ff)
                                            str(pfkeys$,4%,1%)= hex(ff)
                                            str(pfkeys$,6%,1%)= hex(ff)
L42480:     if edit% = 0% then str(pf$(1%),,20%) = " "  /* View Only */
            if edit% = 0% then              str(pfkeys$,1%,1%)= hex(ff)
            if edit% = 0% then              str(pfkeys$,9%,1%)= hex(ff)
            if edit% = 0% then str(pf$(1%),40%,18%)=" " /* View Only */
            if edit% = 0% then str(pf$(3%),41%,20%)=" " /* View Only */
            if edit% = 0% then str(pf$(3%),64%,16%) = "(16)Return"
            if maint_enabled% = 1% then return
                str(pf$(1%),19%,21%) = " ": str(pfkeys$,8%,1%)= hex(ff)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   3                   *~
            *-----------------------------------------------------------*~
            * General ECR Inquiry & Display Screen (Full Inquiry Mode)  *~
            *************************************************************

        deffn'103(fieldnr%)
              str(line2$,,60%) = "Change Requests Summary & Approvals "  ~
                               & "Tracking Display Screen"
              line9$ = "Approval Type   Granted?  Grant Date  By   For"
              gosub set_pf3
              init(hex(84)) lfac$(), bfac$(), dfac$(), dfac$(), ffac$()

L43410:     accept                                                       ~
               at (01,02),                                               ~
                  "ECT Engineering Change Requests Display & Inquiry",   ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Control Number:",                            ~
               at (04,18), fac(hex(81)),   control$             , ch(08),~
                                                                         ~
               at (04,28), "ECR Number:",                                ~
               at (04,40), fac(hex(81)),   ecr_number$          , ch(16),~
                                                                         ~
               at (05,02), "Title of ECR  :",                            ~
               at (05,18), fac(hex(84)),   title$               , ch(40),~
                                                                         ~
               at (06,02), "Affected Part :",                            ~
               at (06,18), fac(hex(84)),   part$                , ch(25),~
               at (06,45), fac(hex(8c)),   partdescr$           ,        ~
                                                                         ~
               at (07,02), "Based on BOM  :",                            ~
               at (07,18), fac(hex(84)),   bom_id$              , ch(03),~
               at (07,23), "Route:",                                     ~
               at (07,30), fac(hex(84)),   rte_id$              , ch(03),~
               at (07,45), fac(hex(8c)),   bom_iddescr$         , ch(32),~
                                                                         ~
               at (08,02), "Implementd BOM:",                            ~
               at (08,18), fac(hex(84)),   new_bomid$           , ch(03),~
               at (08,23), "Route:",                                     ~
               at (08,30), fac(hex(84)),   new_route_id$        , ch(03),~
               at (08,45), fac(hex(8c)),   new_bomiddescr$      , ch(32),~
                                                                         ~
               at (09,02), "Current Status:",                            ~
               at (09,18), fac(hex(84)),   status$              , ch(15),~
               at (09,45), fac(hex(8c)),   statusdescr$         , ch(32),~
                                                                         ~
               at (10,02), "Reason for ECR:",                            ~
               at (10,18), fac(hex(84)),   reason$              , ch(15),~
               at (10,45), fac(hex(8c)),   reasondescr$         , ch(32),~
                                                                         ~
               at (12,02), fac(hex(ac)), line9$                 , ch(79),~
                                                                         ~
               at (13,02), fac(hex(84)),      approval_type$    (o%+01%),~
               at (14,02), fac(hex(84)),      approval_type$    (o%+02%),~
               at (15,02), fac(hex(84)),      approval_type$    (o%+03%),~
               at (16,02), fac(hex(84)),      approval_type$    (o%+04%),~
               at (17,02), fac(hex(84)),      approval_type$    (o%+05%),~
               at (18,02), fac(hex(84)),      approval_type$    (o%+06%),~
               at (19,02), fac(hex(84)),      approval_type$    (o%+07%),~
               at (20,02), fac(hex(84)),      approval_type$    (o%+08%),~
                                                                         ~
               at (13,20), fac(hex(84)),      approval_granted$ (o%+01%),~
               at (14,20), fac(hex(84)),      approval_granted$ (o%+02%),~
               at (15,20), fac(hex(84)),      approval_granted$ (o%+03%),~
               at (16,20), fac(hex(84)),      approval_granted$ (o%+04%),~
               at (17,20), fac(hex(84)),      approval_granted$ (o%+05%),~
               at (18,20), fac(hex(84)),      approval_granted$ (o%+06%),~
               at (19,20), fac(hex(84)),      approval_granted$ (o%+07%),~
               at (20,20), fac(hex(84)),      approval_granted$ (o%+08%),~
                                                                         ~
               at (13,28), fac(hex(84)),      approval_date$    (o%+01%),~
               at (14,28), fac(hex(84)),      approval_date$    (o%+02%),~
               at (15,28), fac(hex(84)),      approval_date$    (o%+03%),~
               at (16,28), fac(hex(84)),      approval_date$    (o%+04%),~
               at (17,28), fac(hex(84)),      approval_date$    (o%+05%),~
               at (18,28), fac(hex(84)),      approval_date$    (o%+06%),~
               at (19,28), fac(hex(84)),      approval_date$    (o%+07%),~
               at (20,28), fac(hex(84)),      approval_date$    (o%+08%),~
                                                                         ~
               at (13,40), fac(hex(84)),      approved_by$      (o%+01%),~
               at (14,40), fac(hex(84)),      approved_by$      (o%+02%),~
               at (15,40), fac(hex(84)),      approved_by$      (o%+03%),~
               at (16,40), fac(hex(84)),      approved_by$      (o%+04%),~
               at (17,40), fac(hex(84)),      approved_by$      (o%+05%),~
               at (18,40), fac(hex(84)),      approved_by$      (o%+06%),~
               at (19,40), fac(hex(84)),      approved_by$      (o%+07%),~
               at (20,40), fac(hex(84)),      approved_by$      (o%+08%),~
                                                                         ~
               at (13,45), fac(hex(84)),      approved_for$     (o%+01%),~
               at (14,45), fac(hex(84)),      approved_for$     (o%+02%),~
               at (15,45), fac(hex(84)),      approved_for$     (o%+03%),~
               at (16,45), fac(hex(84)),      approved_for$     (o%+04%),~
               at (17,45), fac(hex(84)),      approved_for$     (o%+05%),~
               at (18,45), fac(hex(84)),      approved_for$     (o%+06%),~
               at (19,45), fac(hex(84)),      approved_for$     (o%+07%),~
               at (20,45), fac(hex(84)),      approved_for$     (o%+08%),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L44290
                  call "MANUAL" ("ECRAPSUB") : goto L43410

L44290:        if keyhit% <> 15 then L44320
                  call "PRNTSCRN" : goto L43410

L44320:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf3
            pf$(1) = "(1)Next ECR  (9)Detail (10)BOM/Rte Info " &        ~
                     "(25)ECR Text           (13)Instructions"
            pf$(2) = "(2)First (4)Prev       (11)BOM Effect.  " &        ~
                     "(26)Disposition Text   (15)Print Screen"
            pf$(3) = "(3)Last  (5)Next       (12)BOM Xplosion " &        ~
                     "(27)Approval Text      (16)Return      "
            pfkeys$ = hex(0102030405ffffff090a0b0c0dff0f10191a1b00)
            if o% + 9% < max_lines% then L44480
                str(pf$(3%),,16%) = " "  :  str(pfkeys$,3%,1%)= hex(ff)
                                            str(pfkeys$,5%,1%)= hex(ff)
L44480:     if o% > 0% then L44520
                str(pf$(2%),,16%) = " "  :  str(pfkeys$,2%,1%)= hex(ff)
                                            str(pfkeys$,4%,1%)= hex(ff)
L44520:     if textid$ <> " " and textid$ <> hex(ffffffff) then L44540
                str(pf$(1%),40%,21%)=" " :  str(pfkeys$,17%,1%)= hex(ff)
L44540:     if textid2$ <> " " and textid2$ <> hex(ffffffff) then L44600
                str(pf$(2%),40%,21%)=" " :  str(pfkeys$,18%,1%)= hex(ff)
L44600:     return


        REM *************************************************************~
            *               S C R E E N   P A G E   5                   *~
            *-----------------------------------------------------------*~
            * ECR Identification Information Screen.                    *~
            *************************************************************

        deffn'105(edit%)
              str(line2$,,60%) = "ECR Identification Information Screen"
              gosub set_pf5
              init(hex(84)) lfac$()

L45320:     accept                                                       ~
               at (01,02),                                               ~
                  "ECT Engineering Change Requests Display & Inquiry",   ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Control Number:",                            ~
               at (06,30), fac(lfac$( 1)), control$             , ch(08),~
                                                                         ~
               at (07,02), "ECR Number:",                                ~
               at (07,30), fac(lfac$( 2)), ecr_number$          , ch(16),~
                                                                         ~
               at (08,02), "Title of ECR:",                              ~
               at (08,30), fac(lfac$( 3)), title$               , ch(40),~
                                                                         ~
               at (09,02), "Part Assembly Code:",                        ~
               at (09,30), fac(lfac$( 4)), part$                , ch(25),~
               at (09,56), fac(hex(8c)),   partdescr$           , ch(25),~
                                                                         ~
               at (10,02), "Based on BOM Revision ID:",                  ~
               at (10,30), fac(lfac$( 5)), bom_id$              , ch(03),~
               at (10,49), fac(hex(8c)),   bom_iddescr$         , ch(32),~
                                                                         ~
               at (11,02), "Based on Route ID:",                         ~
               at (11,30), fac(lfac$( 6)), rte_id$              , ch(03),~
                                                                         ~
               at (12,02), "ECR Originated By:",                         ~
               at (12,30), fac(lfac$( 7)), orig_by$             , ch(30),~
                                                                         ~
               at (13,02), "ECR Coordinated By:",                        ~
               at (13,30), fac(lfac$( 8)), coord_by$            , ch(30),~
                                                                         ~
               at (14,02), "Date Originated:",                           ~
               at (14,30), fac(lfac$( 9)), orig_date$           , ch(10),~
                                                                         ~
               at (15,02), "Scheduled Implementation:",                  ~
               at (15,30), fac(lfac$(10)), sched_date$          , ch(10),~
                                                                         ~
               at (16,02), "Reason for ECR:",                            ~
               at (16,30), fac(lfac$(11)), reason$              , ch(15),~
               at (16,49), fac(hex(8c)),   reasondescr$         , ch(32),~
                                                                         ~
               at (17,02), "Severity / Priority Code:",                  ~
               at (17,30), fac(lfac$(12)), severity$            , ch(15),~
               at (17,49), fac(hex(8c)),   severitydescr$       , ch(32),~
                                                                         ~
               at (18,02), "Expected Net Chg in Cost:",                  ~
               at (18,30), fac(lfac$(13)), net_cost_chg$        , ch(15),~
                                                                         ~
               at (19,02), "Current Status:",                            ~
               at (19,30), fac(lfac$(14)), status$              , ch(15),~
               at (19,49), fac(hex(8c)),   statusdescr$         , ch(32),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L45980
                  call "MANUAL" ("ECRINPUT") : goto L45320

L45980:        if keyhit% <> 15 then L46005
                  call "PRNTSCRN" : goto L45320

L46005:        if keyhit% <> 5% then L46040
                  gosub'106(0%)
L46040:        return

        set_pf5
            pf$(1) = "                                        " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (5)Next Screen         " &        ~
                     "                       (16)Return      "
            pfkeys$ = hex(ffffffff05ffffffffffffff0dff0f10ff)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   6                   *~
            *-----------------------------------------------------------*~
            * ECR Descriptive & Disposition Text Screen.                *~
            *************************************************************

        deffn'106(edit%)
              gosub set_pf6
              str(line2$,,60%) = "ECR Descriptive Information Screen"
              init(hex(84)) lfac$()

L46650:     accept                                                       ~
               at (01,02),                                               ~
                  "ECT Engineering Change Requests Display & Inquiry",   ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Description of ECR:",                        ~
               at (06,30), fac(lfac$( 1)), ecr_descr$(1)        , ch(40),~
               at (07,30), fac(lfac$( 1)), ecr_descr$(2)        , ch(40),~
               at (08,30), fac(lfac$( 1)), ecr_descr$(3)        , ch(40),~
               at (09,30), fac(lfac$( 1)), ecr_descr$(4)        , ch(40),~
               at (10,30), fac(lfac$( 1)), ecr_descr$(5)        , ch(40),~
                                                                         ~
               at (12,02), "Description of Disposition:",                ~
               at (12,30), fac(lfac$( 2)), disp_descr$(1)       , ch(40),~
               at (13,30), fac(lfac$( 2)), disp_descr$(2)       , ch(40),~
               at (14,30), fac(lfac$( 2)), disp_descr$(3)       , ch(40),~
               at (15,30), fac(lfac$( 2)), disp_descr$(4)       , ch(40),~
               at (16,30), fac(lfac$( 2)), disp_descr$(5)       , ch(40),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L46970
                  call "MANUAL" ("ECRINPUT") : goto L46650

L46970:        if keyhit% <> 15 then L46995
                  call "PRNTSCRN" : goto L46650

L46995:        if keyhit% =  4% then gosub'105(0%)
               if keyhit% =  5% then gosub'107(0%)
               return

        set_pf6
            pf$(1) = "                                        " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Screen     " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (5)Next Screen         " &        ~
                     "                       (16)Return      "
            pfkeys$ = hex(ffffff0405ffffffffffffff0dff0f10ff)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   7                   *~
            *-----------------------------------------------------------*~
            * Document Display Screen.                                  *~
            *************************************************************

        deffn'107(edit%)
              gosub set_pf7
              str(line2$,,60%) = "ECR Disposition Information Screen"
              init(hex(84)) lfac$()

L47660:     accept                                                       ~
               at (01,02),                                               ~
                  "ECT Engineering Change Requests Display & Inquiry",   ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "ECR Status:",                                ~
               at (06,30), fac(lfac$( 1)), status$              , ch(15),~
               at (06,49), fac(hex(8c)),   statusdescr$         , ch(32),~
                                                                         ~
               at (07,02), "Date Implemented/Cancelled:",                ~
               at (07,30), fac(lfac$( 2)), date_implemented$    , ch(10),~
                                                                         ~
               at (08,02), "Enacted / Implemented By:",                  ~
               at (08,30), fac(lfac$( 3)), implemented_by$      , ch(30),~
                                                                         ~
               at (09,02), "Implemented in BOM Rev ID:",                 ~
               at (09,30), fac(lfac$( 4)), new_bomid$           , ch(03),~
               at (09,49), fac(hex(8c)),   new_bomiddescr$      , ch(32),~
                                                                         ~
               at (10,02), "Implemented in Route ID:",                   ~
               at (10,30), fac(lfac$( 5)), new_route_id$        , ch(03),~
                                                                         ~
               at (11,02), "Drawing Number / Reference:",                ~
               at (11,30), fac(lfac$( 6)), drawing$             , ch(16),~
                                                                         ~
               at (12,02), "First Serial Number:",                       ~
               at (12,30), fac(lfac$( 7)), str(serial_nbr$,,sn_length%), ~
                                                                         ~
               at (13,02), "First Lot Number:",                          ~
               at (13,30), fac(lfac$( 8)), str(lot$,,lot_length%),       ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L48100
                  call "MANUAL" ("ECRINPUT") : goto L47660

L48100:        if keyhit% <> 15 then L48125
                  call "PRNTSCRN" : goto L47660

L48125:        if keyhit% = 4% then gosub'106(0%)
               if keyhit% <> 5% then L48160
                   call "VFINPSUB" ("ECRMASTR", "D",                     ~
                  "ECT Engineering Change Requests Display & Inquiry",   ~
                  "Display User Defined Fields for ECR: " & ecr_number$, ~
                  "YY", vfs$, keyhit%)
                  if keyhit% = 4% then L48160
                  if keyhit% = 5% then gosub'109(0%)
L48160:        return

        set_pf7
            pf$(1) = "                                        " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Screen     " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (5)Next Screen         " &        ~
                     "                       (16)Return      "
            pfkeys$ = hex(ffffff0405ffffffffffffff0dff0f10ff)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   9                   *~
            *-----------------------------------------------------------*~
            * Document Audit Information Screen.                        *~
            *************************************************************

        deffn'109(edit%)
              gosub set_pf9
              str(line2$,,60%) = "ECR Audit Information Screen"
              init(hex(84)) lfac$()

L48770:     accept                                                       ~
               at (01,02),                                               ~
                  "ECT Engineering Change Requests Display & Inquiry",   ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "System Date ECR Entered:",                   ~
               at (06,30), fac(lfac$( 1)), date_created$        , ch(10),~
                                                                         ~
               at (07,02), "       Time ECR Entered:",                   ~
               at (07,30), fac(lfac$( 2)), time_created$        , ch(08),~
                                                                         ~
               at (08,02), "            ECR Entered By:",                ~
               at (08,30), fac(lfac$( 3)), created_by$          , ch(03),~
               at (08,49), fac(hex(8c)),   created_bydescr$     , ch(32),~
                                                                         ~
               at (09,02), "System Date ECR Changed:",                   ~
               at (09,30), fac(lfac$( 4)), date_changed$        , ch(10),~
                                                                         ~
               at (10,02), "       Time ECR Changed:",                   ~
               at (10,30), fac(lfac$( 5)), time_changed$        , ch(08),~
                                                                         ~
               at (11,02), "            ECR Changed By:",                ~
               at (11,30), fac(lfac$( 6)), changed_by$          , ch(03),~
               at (11,49), fac(hex(8c)),   changed_bydescr$     , ch(32),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L49150
                  call "MANUAL" ("ECRINPUT") : goto L48770

L49150:        if keyhit% <> 15 then L49175
                  call "PRNTSCRN" : goto L48770

L49175:        if keyhit% = 4% then gosub'107(0%)
               return

        set_pf9
            pf$(1) = "                                        " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Screen     " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Return      "
            pfkeys$ = hex(ffffff04ffffffffffffffff0dff0f10ff)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            error% = 0%
            on fieldnr% gosub L50100,         /* Select Approval Types  */~
                              L50150          /* Approval Data          */
            return

L50100: REM Test for Select / Deselect Approval Types  APPROVAL_TYPE$()
            for x% = 1% to max_lines%
                if approval_required$(x%) = "Y" or                       ~
                   approval_required$(x%) = "N" then L50131
                     errormsg$ = "Please enter Y or N to indicate " &    ~
                    "whether this Approval Type applies to this ECR!"
                     c% = x%  /* line in error */
                     x% = max_lines%  /* so we exit loop */
L50131:     next x%
            return

L50150: REM Test for Approval Granted / Denied    APPROVAL_GRANTED$()
            for x% = 1% to max_lines%
                if approval_granted$(x%) = " " and                       ~
                   (approval_date$(x%) = " "  or                         ~
                    approval_date$(x%) = blankdate$) and                 ~
                     approved_by$(x%) = "   " and                        ~
                     approved_for$(x%) = " " then nextx
                if approval_granted$(x%) = "Y" or                        ~
                   approval_granted$(x%) = "N" then L50200
                     errormsg$ = "Please enter Y or N to indicate " &    ~
                    "whether the Approval is GRANTED (Y) or DENIED "
                     error% = 1%  /* Indicate which field on line */
                     goto L50346

L50200: REM Test for Approval Granted Date        APPROVAL_DATE$()
                call "DATEOKC" (approval_date$(x%), comp%, errormsg$)
                if errormsg$ = " " then L50250
                     error% = 2%  /* Indicate which field on line */
                     goto L50346

L50250: REM Test for Approved By UID              APPROVED_BY$()
                if approved_by$(x%) <> "  " then L50300
                     errormsg$ = "You must enter a someone's initials!"
                     error% = 3%  /* Indicate which field on line */
                     goto L50346
*       *****************************************************************
*              This code dead until someone figures out its wanted again*
                if approved_by$(x%) = "?" then L50284
L50260:         call "READ100" (#01, approved_by$(x%), f1%(1%))
                if f1%(1%) = 1% then L50300
                     errormsg$ = "Must be a Valid CMS User!  Enter ? " & ~
                                 "to Select from Name List."
                     error% = 3%  /* Indicate which field on line */
                     goto L50346
L50284:         if c% < o% + 11% and c% > o% then                        ~
                        f1%(1%) = -(c% - o% + 9%)  /* Current Row      */~
                else    f1%(1%) = 0%               /* Not on Screen Now*/
                call "PLOWCODE" (#01, approved_by$(x%),                  ~
                                 approved_bydescr$(x%), 0%, .3, f1%(1%))
                goto L50260
*       *****************************************************************

L50300: REM Test for Approved For Name            APPROVED_FOR$()
            goto nextx
L50346:              c% = x%  /* line in error */
                     x% = max_lines%  /* so we exit loop */
        nextx: next x%
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on the Inquiry Screen (3)         *~
            *************************************************************

        deffn'153(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L51120          /* Select ECR #           */

            return

L51120: REM * ECR or Control Number Selection
            if ecr_number$ > " " or control$ = " " then                  ~
                                   gosub find_by_ecr_nbr                 ~
                              else gosub find_by_control_nbr
            save_control$ = control$
            gosub initialize_variables
            control$ = save_control$
            if f1%(4%) = 0% then errormsg$ = "ECR Not Found or Selected!"~
                            else gosub load_ecr_data
            return

        find_by_control_nbr
            columnhdr$(1%) = "  ECR Number       Part Assembly Code" &   ~
                             "          Current Status"
            columnhdr$(2%) = "Control #    ECR Number       Part Assembly~
        ~ Code          Current Status"
            mat descr_map = zer
            descr_map(01%) = 009.16  : descr_map(02%) = 001 /* ECR Nbr  */
            descr_map(03%) = 025.40  : descr_map(04%) =1001 /* Title    */
            descr_map(05%) = 125.25  : descr_map(06%) = 018 /* Part Nbr */
            descr_map(07%) =-026.32  : descr_map(08%) =1042 /* Part Desc*/
            descr_map(09%) = 410.15  : descr_map(10%) = 046 /* Status   */
            mat incl_excl = zer
            plowkey$ = control$
            call "PLOWCODE" (#04, plowkey$, title$, 9008%, -1.4, f1%(4%),~
                             columnhdr$(), 0, -125, incl_excl(),         ~
                             incl_excl$(), "D", " ", #07, descr_map())
            if f1%(4%) = 0% then return
                ecr_number$ = str(plowkey$,9%)
                return

        find_by_ecr_nbr
            columnhdr$(1%) = "Control #  ECR Number       Part Assembly C~
        ~ode          Current Status"
            mat descr_map = zer
            descr_map(01%) = 001.08  : descr_map(02%) = 001 /* Control  */
            descr_map(03%) = 009.16  : descr_map(04%) = 010 /* ECR Nbr  */
            descr_map(05%) = 025.40  : descr_map(06%) =1001 /* Title    */
            descr_map(07%) = 125.25  : descr_map(08%) = 027 /* Part Nbr */
            descr_map(09%) =-026.32  : descr_map(10%) =1045 /* Part Desc*/
            descr_map(11%) = 410.15  : descr_map(12%) = 055 /* Status   */
            mat incl_excl = zer
            call "PLOWCODE" (#04, ecr_number$, title$, 9000%,.4, f1%(4%),~
                             columnhdr$(), 0, -125, incl_excl(),         ~
                             incl_excl$(), "D", " ", #07, descr_map())
            return

        display_bom_rte_xref
            call "BOMSRTES" (part$, #7, #8, #9, #13)
            return

        display_bom_effectivity
            call "BOMEFDSP" (part$, #10, #08, #09, #07, #11, #13)
            return

        display_bom_xplosion
            if cursor%(1%) = 8% then bom$ = new_bomid$                   ~
                                else bom$ = bom_id$

            call "BOMBRWSB" (#8,       /* BOMMASTR                     */~
                             #10,      /* ENGMASTR                     */~
                             #6,       /* SYSFILE2                     */~
                             #7,       /* HNYMASTR                     */~
                             part$,    /* Parent part passed in        */~
                             bom$,     /* BOM (use effective if blank) */~
                             1%,       /* 1 = display,2 =display/select*/~
                             " ",      /* part selected, passed back   */~
                             errormsg$)/* blank means all OK           */

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
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program

            end
