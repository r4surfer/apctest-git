        REM THISPROGRAMWASGENERATEDUSINGTHEGENMENUPROGRAMWHICHISAPROPRIET~
            *                                                           *~
            *   CCCC  M   M   SSS   L      IIIII  N   N  K  K           *~
            *  C      MM MM  S      L        I    NN  N  K K            *~
            *  C      M M M   SSS   L        I    N N N  KK             *~
            *  C      M   M      S  L        I    N  NN  K K            *~
            *   CCCC  M   M   SSS   LLLLL  IIIII  N   N  K  K           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CMSLINK -  CAELUS System Routine for invoking/running     *~
            *            programs/procs within the CMS Tools Environment*~
            *            Note;  Be SURE to CLOSE the workstation before *~
            *                   calling.                                *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1988, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/01/88 ! Original                                 ! LDJ *~
            * 09/03/88 ! Corrected bug - blowup if USERLCMS not   ! LDJ *~
            *          ! present on system.  Now defaults to      !     *~
            *          ! current proglib & vol if file not present!     *~
            *          ! or user not defined in file.             !     *~
            *          ! Corrected bug - now FINDs Procedure      !     *~
            *          ! files - before rejected as not found.    !     *~
            *          ! Also now no longer closes USERLCMS if    !     *~
            *          ! already opened by the caller.            !     *~
            * 10/21/88 ! Now passes Log File Library to CMSLOGSB. ! LDJ *~
            * 07/12/93 ! Unix - Never show error if run = CMSCONTR! KAB *~
            * 04/03/96 ! Added messages to VB swatch.exe routine  ! LDJ *~
            *          ! letting him know that a screen clear     !     *~
            *          ! occurred (result of RUN statement).      !     *~
            * 03/05/97 ! Added GUICHENV.  Now should be only place! LDJ *~
            *          ! GUICHENV is called from.                 !     *~
            * 04/17/97 ! Changed messages sent to Swatch for NT   !     *~
            *          ! before and after a link.  Requires SWATCH!     *~
            *          ! version 2.6 or higher.                   !     *~
            * 05/01/97 ! Attempt to avoid startup probs in        ! LDJ *~
            *          ! CMSCONTR and CMSGUI.                     !     *~
            ARYPRODUCTOFCAELUSASSOCIATESSPOKANEWAALLRIGHTSRESERVEDGENMENU

        sub "CMSLINK" addr(#1,           /* USERLCMS File              */~
                           user$,        /* Retrieve Run Info Using ID */~
                           run_or_find$, /* 'R' = Run or Link to the   */~
                                         /*       Passed Program Name. */~
                                         /* 'F' = Find The Library &   */~
                                         /*       Volume To Run From.  */~
                           run$,         /* Program or Proc to Run     */~
                           runlib$,      /* In:  Optional Override Lib */~
                                         /* Out: Library Run From      */~
                           runvol$,      /* In:  Optional Override Vol */~
                                         /* Out: Volume  Run From      */~
                           run_flag$,    /* 'P' = Use Override Lib/Vol */~
                                         /*       Only!                */~
                                         /* 'S' = Use System (@SYSTEM@)*/~
                                         /*       Library & Volume Only*/~
                                         /* ' ' = Default.             */~
                                         /*       Try Override Lib 1st,*/~
                                         /*       Then try System Lib, */~
                                         /*       Then User's Default  */~
                                         /*       Run Lists set in     */~
                                         /*       CMSUSRIN.            */~
                           signal_error$,/* 'Y' = Message User if Not  */~
                                         /*       able to run program. */~
                                         /* 'N' = Caller will handle.  */~
                           pf16$,        /* Optional Text for Command  */~
                                         /* processor PF16 prompt.     */~
                                         /* (Optional means may be left*/~
                                         /* blank but arg is required!)*/~
                           run_from$,    /* Optional - Name of Menu /  */~
                                         /* Program / Proc being run   */~
                                         /* from.  Default is calling  */~
                                         /* programs name.             */~
                           transparent$, /* Transparent Link? (Y/N)    */~
                           comp%,        /* LINK Completion Return Code*/~
                                         /* value - returned to caller.*/~
                           return%)      /* Return Code passed back    */~
                                         /* from Program to be RUN.    */

        dim                                                              ~
            askhdr$40,                   /* ASKUSER String             */~
            askpf1$80,                   /* ASKUSER String             */~
            askmid$80,                   /* ASKUSER String             */~
            askpf2$80,                   /* ASKUSER String             */~
            caller$8,                    /* Name of Current Program    */~
            last_userid$3,               /* UserID when last invoked   */~
            linkmsg$25,                  /* PF16 Help Prompt Text      */~
            log_flag$1,                  /* Log User Activity Flag     */~
            log_library$8,               /* Log Activity in Library ...*/~
            mode$1,                      /* Link 'run mode'            */~
            option$1,                    /* LINKCMS Link Bits Flag     */~
            pf16$24,                     /* PF16 Help Prompt Text      */~
            rlib$8,                      /* Run library for link call  */~
            run$8,                       /* Program to link to         */~
            run_flag$1,                  /* What Libs to Use           */~
            run_from$16,                 /* Run From Program/Proc/Menu */~
            runlib$8,                    /* Override Run Library       */~
            runlib$(6)8,                 /* Additional run libs to srch*/~
            run_or_find$1,               /* Run it or Find It (R or F) */~
            runvol$6,                    /* Override Run Volume        */~
            runvol$(6)6,                 /* Volumes for run libs       */~
            rvol$6,                      /* Run volume for link call   */~
            signal_error$1,              /* Signal User if Error?      */~
            show_bbar$1,                 /* Show Button Bar?           */~
            show_tbar$1,                 /* Show Toolbar?              */~
            swatch_on$1,                 /* Does user have Swatch going*/~
            syslib$8,                    /* System Library Name        */~
            sysvol$6,                    /* System Volume Name         */~
            transparent$1,               /* Transparent Link? Y or N   */~
            user$3,                      /* User ID Passed In          */~
            userid$3,                    /* User running this program  */~
            uw$1                         /* CoStar Magic Character     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

        REM *************************************************************~
            *              I N I T I A L I Z A T I O N                  *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            if run$ = " " then normal_exit
            if run_or_find$ = " " then run_or_find$ = "R"
            if run$ = "CMSCONTR" then signal_error$ = "N"
            userid$ = user$ : file_open% = 0%
            if userid$ = " " then call "EXTRACT" addr("ID", userid$)
            if userid$ = last_userid$ then get_ready
            last_userid$ = userid$
            call "CHECKGUI" addr(gui%) : uw$ = hex(7f)
            call "EXTRACT" addr("XL",syslib$,"XV",sysvol$,"CF",caller$,  ~
                                "OL",log_library$)
            call "GETUFBS1" addr(#1, file_open%)
            save_open_flag% = file_open%
            if file_open% = 0% then gosub open_userlcms
            if file_open% = 0% then get_ready
            REM Get pertinent info from users control record...
            f1% = 1%
            if fs(#1) < "10" and key(#1) = userid$ then load_user_info
            call "READ100" (#1, userid$, f1%)
            if f1% = 0% then L09210
        load_user_info
            get #1 using L09190, runlib$(), runvol$(),log_flag$,swatch_on$,~
                    log_library$, show_tbar$, show_bbar$
L09190:     FMT POS(96), 6*CH(8), 6*CH(6), XX(2), CH(1), POS(246),CH(1),POS(263), CH(8),~
                POS(288),2*CH(1)
            REM  Later may be desirable to add encryption & security code.
L09210:     rem if save_open_flag% = 0% then close #1


        REM *************************************************************~
            *                    M A I N   R O U T I N E                *~
            *-----------------------------------------------------------*~
            * Get ready, Get Set, Run it!                               *~
            *************************************************************
        get_ready
            if (show_tbar$ <> "Y" and show_tbar$ <> "N") or pf16$ = "STARTUP" then show_tbar$ = "N"
            if (show_bbar$ <> "Y" and show_bbar$ <> "N") or pf16$ = "STARTUP" then show_bbar$ = "N"

            if (file_open% = 0% or f1% = 0%) then                        ~
               call "EXTRACT" addr("RL", runlib$(1%), "RV", runvol$(1%))
            mode$ = "P" : rlib$ = runlib$ : rvol$ = runvol$ : try% = 0%
            if rlib$ > " " and rvol$ > " " then get_set
            REM *** No Override Library / Volume ***
            rlib$ = runlib$(1%) : rvol$ = runvol$(1%) : try% = 1%

        get_set
            option$ = hex(48)
            if transparent$ = "Y" then option$ = hex(4a)
            if pf16$ <> " " then linkmsg$ = pf16$                        ~
                            else linkmsg$ = "Return to"
            if run_from$ > " " then linkmsg$ = linkmsg$ & " " & run_from$~
                               else linkmsg$ = linkmsg$ & " " & caller$
            linkmsg$ = hex(18) & linkmsg$
            if run_flag$ <> "S" then log_it
               mode$ = "S" : rlib$ = syslib$ : rvol$ = sysvol$

        log_it
            if run_or_find$ = "F" then run_it
REM *** Send Message to Swatch that Clear Screen  & Run Program is coming ***
            if gui% = 1% and swatch_on$ = "Y" then                       ~
               call "SENDCMD" (uw$ & "UWVBXSWATCH,7732,Link" & uw$)
            call "GUICHENV" (run$,1%,show_tbar$,show_bbar$)
            if log_flag$ <> "Y" then run_it
            call "CMSLOGSB" (run$,   /* Link(ed) to Program            */~
                             rlib$,  /* Link(ed) to Program Library    */~
                             rvol$,  /* Link(ed) to Program Volume     */~
                             mode$,  /* Link Type in LINK subroutin    */~
                             "IN ",  /* Logging IN or logging OUT      */~
                             -1%,    /* Link Completion Code           */~
                             -1%,    /* Return Code from program       */~
                             log_library$) /* Log file Library Name    */

        run_it
            if run_or_find$ <> "F" then L11300
               gosub find_it
               goto L11340

L11300:     call "LINKCMS" addr(option$, run$, rlib$, rvol$,             ~
                                linkmsg$, return%)
            if return% >= 0% then comp% = 0%    /* Ran OKEY DOKEY  */    ~
                             else comp% = 8%    /* Not found because ...*/
            if return% = -156% then comp% = 16% /* Program Canceled */
L11340:     REM *** What Happened? ***
            if comp% <> 8% then finish_log
            if run_flag$ <> " " then finish_log
            REM *** Look Through Users Run Lib List... ***
L11380:     try% = try% + 1% : if try% > 6% then L11420
            if runlib$(try%)=" " or runvol$(try%)=" " then L11380
            rlib$ = runlib$(try%) : rvol$ = runvol$(try%)
            goto run_it
L11420:     if try% > 7% then finish_log
            rlib$ = syslib$ : rvol$ = sysvol$
            goto run_it

        find_it                  /* Find Program instead of running it */
            call "READFDR" addr(run$, rlib$, rvol$, 0%, "FT", file_type$,~
                                "RS", rec_size%, return%)
            comp% = 8%
            if return% <> 0% then return
            if file_type$ = "C" and rec_size% <> 80% then return
            if file_type$ <> "O" and file_type$ <> "C" then return
            comp% = 0%
            return

        finish_log
            if run_or_find$ = "F" then L11440
            REM *** Send Message to Swatch that Clear Screen & Back from Program happened ***
            if gui% = 1% and swatch_on$ = "Y" then                       ~
               call "SENDCMD" (uw$ & "UWVBXSWATCH,7734,UnLink" & uw$)
            call "GUICHENV" (caller$,1%,show_tbar$,show_bbar$)
            REM Linked OK, Couldn't Link, or Crashed;
            if log_flag$ = "Y" then                                      ~
            call "CMSLOGSB" (run$,   /* Link(ed) to Program            */~
                             rlib$,  /* Link(ed) to Program Library    */~
                             rvol$,  /* Link(ed) to Program Volume     */~
                             mode$,  /* Link Type in LINK subroutin    */~
                             "OUT",  /* Logging IN or logging OUT      */~
                             comp%,  /* Link Completion Code           */~
                             return%, /* Return Code from program      */~
                             log_library$) /* Log file Library Name    */
L11440:
            if signal_error$ = "N" or comp% = 0% then normal_exit
            keyhit% = 2%
            on comp%/8% gosub no_link, program_cancelled
            call "ASKUSER" (keyhit%, askhdr$,askpf1$,askmid$,askpf2$)
            goto normal_exit

        no_link                  /* Couldn't run it for some reason... */
            askhdr$ = "*** UNABLE TO RUN " & run$ & " ***"
            askpf1$ = "Sorry " & userid$ & ", but I was unable to run " &~
                      run$ & " because of either:"
            askmid$ = "1) " & run$ & " was Not Found in the specified " &~
                      "Run Library(s) / Volume(s);"
            askpf2$ = "2) You do not have Access to run " &              ~
                      run$ & ".  Press RETURN to Acknowlege"
            return

        program_cancelled                  /* User Canceled Program */
            askhdr$ = "*** " & run$ & " WAS CANCELLED! ***"
            askpf1$ = run$ & " was Cancelled due to System, Operator, or"~
                    & " User Action."
            askmid$ = " "
            askpf2$ = "Press RETURN to Acknowlege & Continue " &         ~
                      " (PF15 will Print this Screen)."
            return

        open_userlcms
            call "READFDR" addr("USERLCMS", syslib$, sysvol$, 0%, f2%)
            if f2% <> 0% then return           /* Can't Find File */
            call "PUTNAMES" addr(#1, "USERLCMS", syslib$, sysvol$)
            call "WORKOPN2" (#1, "SHARE", 0%, f2%)
            file_open% = 1%
            return

        normal_exit
            if file_open% = 1% and f1% = 1% then ~
                get #1 using T_B_barFmt, show_tbar$, show_bbar$
        T_B_barFmt: fmt pos(288), 2*ch(1)
            if save_open_flag% = 0% and file_open% = 1% then close #1
            if comp% <> 0% and comp% <> 16% then L65220
            runlib$ = rlib$ : runvol$ = rvol$
L65220:     end

