        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  JJJJJ  BBBB   V   V   AAA   RRRR   BBBB   K   K   TTTTT  *~
            *    J    B   B  V   V  A   A  R   R  B   B  K  K      T    *~
            *    J    BBBB   V   V  AAAAA  RRRR   BBBB   KKKK      T    *~
            *  J J    B   B   V V   A   A  R  R   B   B  K   K     T    *~
            *   J     BBBB     V    A   A  R   R  BBBB   K    K    T    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBVARBKT - Print actual to std variances for jobs by cost *~
            *            bucket.  This report will search for jobs      *~
            *            having variances beyond a threshhold and only  *~
            *            print them. Provides summary and detail fmt.   *~
            *            Prints in background or foreground and gives   *~
            *            user control over threshhold and range select. *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1993  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/16/93 ! Original                                 ! WPH *~
            * 06/28/94 ! Qty extension and initialize totals      ! WPH *~
            * 08/14/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            back$1,                      /* BACKGROUND FLAG            */~
            bdescr$(12)20,               /* Cost bucket description    */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            bn$(12)2,                    /* Cost bucket number 1 - 12  */~
            bktid$(12)10,                /* Cost bucket ID             */~
            company$60,                  /* COMPANY / DIVISION NAME    */~
            cost$10,                     /*                            */~
            closed_or_all$1,             /* Include closed or all jobs */~
            control$19,                  /* Master Job control number  */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            date_ae$8,                   /* DATE JOB ACTUALLY ENDED    */~
            datefrom$10,                 /* Date From Range            */~
            d_title$6,                   /* Literal 'Descr' for report */~
            dateto$10,                   /* Date to Range              */~
            show_detail$1,               /* Bkt totals or all costs    */~
            est_per$3,                   /* Estimate Percent Complete  */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            favorable_pn$1,              /* Favorable as positive/neg  */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            jbact$10,                    /* Total actual cost JBMASTR2 */~
            jbstd$10,                    /* Std cost at close,JBMASTR2 */~
            stccost$10,                  /* Current total std cost     */~
            jbcode$8,                    /* JOB CODE                   */~
            jobdescr$30,                 /* JOB Description            */~
            jobfrom$(2%)8,               /* Job from Range             */~
            jobto$(2%)8,                 /* Job to range               */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            message$78,                  /* INFORMATION MESSAGE        */~
            na$6,                        /* Label to indicate N/A      */~
            part$25,                     /* PART CODE                  */~
            partdesc$30,                 /* Job Part desc              */~
            partfrom$(2%)25,             /* Part From Range            */~
            partto$(2%)25,               /* Part to Range              */~
            partqty$10,                  /* Job quantity completed     */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            print_title$60,              /* REPORT TITLE               */~
            rework$10,                   /* Job rework quantity        */~
            rpt_time$8,                  /* REPORT TIME                */~
            set$8,                       /* Cost Set                   */~
            setdescr$30,                 /* Cost Set Description       */~
            setid$4,                     /* Cost Set Internal ID       */~
            set_title$60,                /* Cost Set in Title of Report*/~
            scrap$10,                    /* Job scrap  quantity        */~
            syskey$20,                   /*                            */~
            tdate$8,                     /* Temporary Date Variable    */~
            to_build$10,                 /* Job quantity to build      */~
            tolerance_tot$3,             /* Tolerance of total variance*/~
            tolerance_bkt$3,             /* Tolerance of a bucket var. */~
            title$(2)30,                 /* SCREEN COLUMN HEADINGS     */~
            userid$3                     /* Current User Id            */

        dim b_act$(12)10,                /* BOM costs - Actual / bucket*/~
            b_act(12),                   /* BOM costs - Actual / bucket*/~
            b_std$(12)10,                /* BOM costs - Standard / bkt */~
            b_std(12),                   /* BOM costs - Standard / bkt */~
            b_var$(12)10,                /* BOM costs - Variance / bkt */~
            b_var(12),                   /* BOM costs - Variance / bkt */~
            b_per$(12)6,                 /* BOM costs - Var. % /  bkt  */~
            b_per(12),                   /* BOM costs - Var. % /  bkt  */~
                                                                         ~
            b_tact$10,                   /* BOM costs - total actual   */~
            b_tstd$10,                   /* BOM costs - total standard */~
            b_tvar$10,                   /* BOM costs - total variance */~
            b_tper$6,                    /* BOM costs - total percent  */~
                                                                         ~
            r_act$(12)10,                /* RTE costs - Actual / bucket*/~
            r_act(12),                   /* RTE costs - Actual / bucket*/~
            r_std$(12)10,                /* RTE costs - Standard / bkt */~
            r_std(12),                   /* RTE costs - Standard / bkt */~
            r_var$(12)10,                /* RTE costs - Variance / bkt */~
            r_var(12),                   /* RTE costs - Variance / bkt */~
            r_per$(12)6,                 /* RTE costs - Var. % /  bkt  */~
            r_per(12),                   /* RTE costs - Var. % /  bkt  */~
                                                                         ~
            r_tact$10,                   /* RTE costs - total actual   */~
            r_tstd$10,                   /* RTE costs - total standard */~
            r_tvar$10,                   /* RTE costs - total variance */~
            r_tper$6,                    /* RTE costs - total percent  */~
                                                                         ~
            m_act$(12)10,                /* MISC costs- Actual / bucket*/~
            m_act(12),                   /* MISC costs- Actual / bucket*/~
            m_std$(12)10,                /* MISC costs- Standard / bkt */~
            m_std(12),                   /* MISC costs- Standard / bkt */~
            m_var$(12)10,                /* MISC costs- Variance / bkt */~
            m_var(12),                   /* MISC costs- Variance / bkt */~
            m_per$(12)6,                 /* MISC costs- Var. % /  bkt  */~
            m_per(12),                   /* MISC costs- Var. % /  bkt  */~
                                                                         ~
            m_tact$10,                   /* MISC costs- total actual   */~
            m_tstd$10,                   /* MISC costs- total standard */~
            m_tvar$10,                   /* MISC costs- total variance */~
            m_tper$6,                    /* MISC costs- total percent  */~
                                                                         ~
            t_act$(12)10,                /* TOT costs - Actual / bucket*/~
            t_act(12),                   /* TOT costs - Actual / bucket*/~
            t_std$(12)10,                /* TOT costs - Standard / bkt */~
            t_std(12),                   /* TOT costs - Standard / bkt */~
            t_var$(12)10,                /* TOT costs - Variance / bkt */~
            t_var(12),                   /* TOT costs - Variance / bkt */~
            t_per$(12)6,                 /* TOT costs - Var. % /  bkt  */~
            t_per(12),                   /* TOT costs - Var. % /  bkt  */~
                                                                         ~
            t_tact$10,                   /* TOT costs - total actual   */~
            t_tstd$10,                   /* TOT costs - total standard */~
            t_tvar$10,                   /* TOT costs - total variance */~
            t_tper$6,                    /* TOT costs - total percent  */~
                                                                         ~
            s_fld(12)                    /* Folded costs from STCCOSTS */

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
            * # 1 ! JBMASTR2 ! Production job master file               *~
            * # 5 ! SYSFILE2 ! SYSTEM INFO                              *~
            * # 6 ! WORK1    ! WORK JOB/PART INFO                       *~
            * #10 ! HNYMASTR ! Inventory Parts Master File              *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 1, "JBMASTR2",                                      ~
                        varc,     indexed,  recsize = 1300,              ~
                        keypos =    1, keylen =   8                      ~

            select # 5, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize = 500,               ~
                        keypos =    1, keylen =  20

*          SELECT # 6, "WORK1",                                         ~
*                      VARC,     INDEXED,  RECSIZE =   105,             ~
*                      KEYPOS =    1, KEYLEN =  25

            select #10, "HNYMASTR",                                      ~
                         varc, indexed, recsize = 900,                   ~
                         keypos = 1, keylen = 25,                        ~
                         alternate key 1, keypos = 102, keylen = 9, dup, ~
                                   key 2, keypos = 90, keylen = 4, dup,  ~
                                   key 3, keypos = 26, keylen = 32, dup

            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (# 1, fs%( 1), f2%( 1), 0%, rslt$( 1))
            call "OPENCHCK" (# 5, fs%( 5), f2%( 5), 0%, rslt$( 5))
            call "OPENCHCK" (#10, fs%(10), f2%(10), 0%, rslt$(10))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$, "TT", back$)
            date$ = date
            call "DATEFMT" (date$)
            call "COMPNAME" (12%, company$, 0%)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
            na$ = "   N/A"
            str(line2$,62) = "JBVARBKT: " & str(cms2v$,,8)
            if back$ = "B" then printing_in_background
            title$(1) = "From" : title$(2) = "To  "

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 10%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10210
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10210:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10130
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            inpmessage$ = edtmessage$
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       set_up_the_report
                  if keyhit% <>  0% then       editpg1
L11140:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 10% then editpg1

            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11190:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11190
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11190
                  lastfieldnr% = fieldnr%
            goto L11140

        REM *************************************************************~
            *         S E T  U P  T H E   R E P O R T                   *~
            *-----------------------------------------------------------*~
            *  Get everything ready, deal with background, etc.         *~
            *************************************************************

        set_up_the_report
*          Setup for background processing
            if back$ <> "Y" then L19260
                call "READ101" (#5, "zJBVARBKT." & userid$, f1%(5%))
                put #5, using L19130, "zJBVARBKT." & userid$, jobfrom$(), ~
                     jobto$(), partfrom$(), partto$(), lowdate%,         ~
                     highdate%, set$, closed_or_all$, show_detail$,      ~
                     favorable_pn$, tolerance_tot$, tolerance_bkt$, back$
L19130:         FMT CH(20), 4*CH(8), 4*CH(25), 2*BI(4),                  ~
                     CH(08), 3*CH(01), 2*CH(3), CH(1)
                if f1%(5%) = 0% then write #5 else rewrite #5
                call "TASKUP" ("ME", 0%)
                goto L65000

        printing_in_background
            message$ = "rptReport JBVARBKT in background: Aborted."
            call "READ101" (#5, "zJBVARBKT." & userid$, f1%(5))
                 if f1%(5) = 0% then tell_user
            get #5, using L19130, syskey$, jobfrom$(), jobto$(),          ~
                 partfrom$(), partto$(), lowdate%, highdate%, set$,      ~
                 closed_or_all$, show_detail$, favorable_pn$,            ~
                 tolerance_tot$, tolerance_bkt$, back$
            delete #5

L19260:     if show_detail$ = "D" then L19455

            if show_detail$ <> "F" then L19400
               call "SHOSTAT" ("Generating Summary Fold-In Report")
               print_title$ = "Job Cost Variance Summary by Cost " &     ~
                                   "Bucket Report via FOLD-IN"
               goto L19458

L19400:        call "SHOSTAT" ("Generating Summary Roll-Up Report")
               print_title$ = "Job Cost Variance Summary by Cost " &     ~
                                   "Bucket Report via ROLL-UP"
               goto L19458

L19455:     call "SHOSTAT" ("Generating Detail Report")
            print_title$ = "Job Cost Variance Detail by Bucket Report"

L19458:     call "FMTTITLE" (print_title$, " ", 12%)

*        Find the number of records in JBMASTR2 & call SETPRNT
            recs% = val(str(rslt$(1),17,4), 4)
            recs% = recs%/2
            call "SETPRNT" ("JB0014", "        ", recs%, 0%)

*        Prepare tolerances for use in later testing
            tol_tot = tol_tot/100
            tol_bkt = tol_bkt/100

*        Get misc. stuff set up
            rpt_time$ = " "
            call "TIME" (rpt_time$)  /*  GET REPORT TIME    */
            select printer(134)
            linecnt% = 0
            pagecnt% = -1%

*        Do the deed
            gosub print_parameters
            gosub print_report

            gosub print_end_report
            close printer
            call "SETPRNT" ("JB0014", "        ", recs%, 1%)

            if pagecnt% > 0% then L19790

                message$ = "rptReport JBVARBKT in background: No data" & ~
                            " met selection criteria."
                if back$ = "Y" then tell_user
                keyhit% = 2%
                call "ASKUSER" (keyhit%, "****",                         ~
                    "SORRY, no jobs eligible for printing were found",   ~
                    "using the requested selection criteria.",           ~
                    "Press RETURN to change your selection or to exit.")
                goto editpg1

L19790:     if back$ <> "Y" then inputmode

            message$ = "rptReport JBVARBKT in background: completed."
            call "TIME" (str(message$,44,8))

        tell_user
            call "MESSAGE"addr("XM",str(userid$)&hex(20),message$,78%,0%)
            goto L65000

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20130,         /* Job Number Range       */~
                              L20190,         /* Part Number Range      */~
                              L20240,         /* Date Range             */~
                              L20290,         /* Cost Set               */~
                              L20325,         /* Show closed or all jobs*/~
                              L20340,         /* Show favorable as +/-  */~
                              L20364,         /* Summary or Detail      */~
                              L20370,         /* Total Tolerance        */~
                              L20380,         /* Bucket Tolerance       */~
                              L20390          /* Run in Background?     */

            return
L20130: REM Def/Enable Job Number Range            JOBFROM$ JOBTO$
            inpmessage$ = "Enter the Job Number Range to be Included " & ~
                          "or 'ALL' for All Jobs."
            if jobfrom$(1%) = " " then jobfrom$(1%) = "ALL"
            return

L20190: REM Def/Enable Part Number Range           PARTFROM$ PARTTO$
            inpmessage$ = "Enter the Part Number Range to be Included " &~
                          "or 'ALL' for All Parts."
            if partfrom$(1) = " " then partfrom$(1) = "ALL"
            return

L20240: REM Def/Enable Date Range                  DATEFROM$ DATETO$
            inpmessage$ = "Enter the Date Range to be Included " &       ~
                          "or 'ALL' for All Dates."
            if datefrom$ = " " or datefrom$ = blankdate$ then datefrom$ = "ALL"
            return

L20290: REM Def/Enable Cost Set                    SET$
            inpmessage$ = "Enter the Cost Set or Leave blank to use " &  ~
                          "the current implemented set."

            return

L20325: REM Def/Enable Report only closed jobs or all     CLOSED_OR_ALL$
            inpmessage$ = "Enter 'C' to Include only closed jobs or " &  ~
                          "Enter 'A' to report all jobs."
            if closed_or_all$ = " " then closed_or_all$ = "A"
            return

L20340: REM DEF/ENABLE DETAIL OR SUMMARY           SHOW_DETAIL$
            inpmessage$ = "Enter 'D' for Detail format, 'R' for Roll-Up "~
                       &  "Summary, or 'F' Fold-In Summary"
            if show_detail$ = " " then show_detail$ = "R"
            return

L20364: REM DEF/ENABLE SHOW FAVORABLE AS +/-   FAVORABLE_PN$
            inpmessage$ = "Enter a 'P' to show Favorable Variance as "  &~
                          "Positive or 'N' to show as Negative"
            if favorable_pn$  = " " then favorable_pn$ = "P"
            return

L20370: REM DEF/ENABLE TOTAL TOLERANCE         TOLERANCE_TOT$
            inpmessage$ = "Show only jobs with total variances outs"    &~
                          "ide this percent. Enter 0 to see all"
            if tolerance_tot$ = " " then tolerance_tot$ = "1"
            return

L20380: REM DEF/ENABLE BUCKET TOLERANCE         TOLERANCE_BKT$
            inpmessage$ = "Show only jobs with a bucket variance ou"    &~
                          "tside this percent. Enter 0 to see all"
            if tolerance_bkt$ = " " then tolerance_bkt$ = "1"
            return

L20390: REM DEF/ENABLE FOR RUN IN BACKGROUND
            inpmessage$ = "Enter a 'Y' to Print this report in "        &~
                          "Background."
            if back$ = " " then back$ = "N"
            return

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, jobfrom$(), jobto$(),      ~
                      partfrom$(), partto$(), dateto$, datefrom$,        ~
                      set$, closed_or_all$, show_detail$, back$,         ~
                      favorable_pn$, tolerance_tot$, tolerance_bkt$,     ~
                      set_title$, jbcode$
            call "ALLFREE"
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1988  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
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
            *       G E N E R A T E   T H E   R E P O R T               *~
            *-----------------------------------------------------------*~
            * Starting with the lowest job number in the range, plow    *~
            * through the rest of JBMASTR2 file, honor the part, job, & *~
            * date ranges.  When we get a hit, load the costs and       *~
            * calculate the variances.  Check the bucket and total      *~
            * variances against the tolerance filters.  If the variance *~
            * is out of tolerance, print the job & cost info in the     *~
            * format specified by the user.  The summary format         *~
            * prints basic job data and the costs and variances by      *~
            * bucket, and the totals across all buckets.  The detail    *~
            * format shows costs by type by bucket.                     *~
            *************************************************************

        print_report
            total_read% = 0%   /* Total number of jobs examined         */
            in_range%   = 0%   /* Number of jobs within ranges. (job    */
                               /*   number, part, date, closed, etc.)   */
            in_tolerance% = 0% /* Number of jobs inside tolerance filter*/
            got_printed%  = 0% /* Made it to the paper                  */

            init(hex(00)) plowkey$
            str(plowkey$,1,8) = str(jobfrom$(2),,)

*        Plow first time through, read thereafter
            call "PLOWALTS" (#1, plowkey$, 0%, 0%, f1%(1%))
            goto L30370

        clear_and_read
            mat t_act = zer
            mat t_std = zer
            mat t_var = zer
            t_tact, t_tstd, t_tvar = 0
            b_tact, b_tstd, b_tvar = 0
            r_tact, r_tstd, r_tvar = 0
            m_tact, m_tstd, m_tvar = 0

        read_job
            call "READNEXT" (#1, f1%(1%))
L30370:         if f1%(1%) = 0% then return
            jbcode$ = key(#1)

            total_read% = total_read% + 1%

*       *Check if Job is within the range

            if jbcode$ > jobto$(2) then read_job

            get #1, using L33270, jbcode$, jobdescr$, part$,              ~
                        to_build,  partqty, date_st$, date_ae$, scrap,   ~
                        rework,   jbact,   b_act(), r_act(), m_act(),    ~
                        jbstd , b_std(), r_std(), m_std(), control$,     ~
                        est_percent

*       *Check if part code is within the range

            if part$ < partfrom$(2) or                                   ~
               part$ > partto$(2) then read_job

            partdesc$ = "Part Not on File"
            call "DESCRIBE"(#10, part$, partdesc$, 0%, f1%(1))

*       *Check if actual job end date is within the range

            if date_ae$ <> " " and date_ae$ <> blankdate$ then L30670
                if closed_or_all$ = "C" then read_job
                goto L30730          /* Skip end date testing because   */
                                    /* we are dealing with an open job.*/

L30670:     tdate$ = date_ae$
            call "DATEOK" (tdate$, date_ae%, " ")
            if date_ae% = 0% then read_job
            if date_ae% < lowdate% or                                    ~
               date_ae% > highdate% then read_job

*       *Overlay costs from specified or current cost set

L30730:     call "STCCOSTS" (part$,   set$, #5 , 3%, stccost, s_fld(),   ~
                                   b_std(), r_std(), m_std())

            in_range% = in_range% + 1%

*       *Determine FOLD-IN cost totals if appropriate

            if show_detail$ <> "F" then L30780
              call "STCFOLDN"(part$, set$, #5, b_act(), r_act(),         ~
                                    m_act(), cost, t_act())
            mat t_std = s_fld
            goto L30860

L30780
*       *Tally the bucket detail arrays to produce ROLL-UP bucket totals

            mat t_act = b_act + r_act
            mat t_act = t_act + m_act

            mat t_std = b_std + r_std
            mat t_std = t_std  + m_std

L30860
*       *Extend the bucket standards by the job quantity
            for i% = 1% to 12%
              t_std(i%) = t_std(i%) * to_build
              b_std(i%) = b_std(i%) * to_build
              r_std(i%) = r_std(i%) * to_build
              m_std(i%) = m_std(i%) * to_build
            next i%

*       *Tally the bucket totals to produce total actual and total std
            for i% = 1% to 12%
               t_tact = t_tact + t_act(i%)
               t_tstd = t_tstd + t_std(i%)
            next i%

*       *Calculate variances by bucket
            if favorable_pn$ <> "P" then L30960
               mat t_var = t_std - t_act  /* favorable is positive */
               mat b_var = b_std - b_act
               mat r_var = r_std - r_act
               mat m_var = m_std - m_act
               goto L30990
L30960:     mat t_var = t_act - t_std     /* favorable is negative */
            mat b_var = b_act - b_std
            mat r_var = r_act - r_std
            mat m_var = m_act - m_std

*       *Calculate the total variance
L30990:     if favorable_pn$ <> "P" then L31020
               t_tvar = t_tstd - t_tact
               goto L31050
L31020:     t_tvar = t_tact - t_tstd

*       *Proceed to print if total variance exceeds tolerance
L31050
*       *Note - this and the next test are logical OR

            if tol_bkt = 0  and tol_tot = 0 then L31210  /* give em all */
               if t_tstd = 0 then L31130
               if abs(t_tvar/t_tstd) >=  tol_tot then L31210

*       *Proceed to print if any bucket exceeds tolerance

L31130:     for t% = 1% to 12%
               if t_std(t%) = 0 then L31160
               if abs(t_var(t%)/t_std(t%))  >=  tol_bkt then L31210
L31160:     next t%
            in_tolerance% = in_tolerance% + 1%   /* don't print; read  */
            goto clear_and_read       /* next job if within tolerance */

*        Calculate variance percents
L31210:     for i% = 1% to 12%
               if show_detail$ <> "D" then L31360

               if b_std(i%) = 0% then L31280
                  b_per(i%) = b_var(i%)/b_std(i%)
                  b_per(i%) = b_per(i%)*100

L31280:        if r_std(i%) = 0% then L31320
                  r_per(i%) = r_var(i%)/r_std(i%)
                  r_per(i%) = r_per(i%)*100

L31320:        if m_std(i%) = 0% then L31360
                  m_per(i%) = m_var(i%)/m_std(i%)
                  m_per(i%) = m_per(i%)*100

L31360:        if t_std(i%) = 0% then L31390
                  t_per(i%) = t_var(i%)/t_std(i%)
                  t_per(i%) = t_per(i%)*100
L31390:     next i%

            if t_tstd = 0% then L31460
               t_tper = t_tvar/t_tstd
               t_tper = t_tper*100

*       *Tally detail buckets to produce column totals
L31460:     if show_detail$ <> "D" then L31670

            for i% = 1% to 12%
              b_tact = b_tact + b_act(i%)
              b_tstd = b_tstd + b_std(i%)
              b_tvar = b_tvar + b_var(i%)

              r_tact = r_tact + r_act(i%)
              r_tstd = r_tstd + r_std(i%)
              r_tvar = r_tvar + r_var(i%)

              m_tact = m_tact + m_act(i%)
              m_tstd = m_tstd + m_std(i%)
              m_tvar = m_tvar + m_var(i%)

            next i%

            if b_tstd = 0 then L31640
            b_tper = b_tvar/b_tstd
            b_tper = b_tper * 100

L31640:     if r_tstd = 0 then L31650
            r_tper = r_tvar/r_tstd
            r_tper = r_tper * 100

L31650:     if m_tstd = 0 then L31670
            m_tper = m_tvar/m_tstd
            m_tper = m_tper * 100

*       *Convert values needed for summary format report
L31670:     for j% = 1% to 12%
               call "CONVERT" (t_act(j%), 2.2, t_act$(j%) )
               call "CONVERT" (t_std(j%), 2.2, t_std$(j%) )
               call "CONVERT" (t_var(j%), 2.2, t_var$(j%) )
               if t_std(j%) <> 0 then L31750
                   t_per$(j%) =  na$
                   goto L31760

L31750:        call "CONVERT" (t_per(j%), 1.1, t_per$(j%) )
L31760:        convert j% to bn$(j%), pic(##)
            next j%

            call "CONVERT" (t_tact, 2.2, t_tact$ )
            call "CONVERT" (t_tstd, 2.2, t_tstd$ )
            call "CONVERT" (t_tvar, 2.2, t_tvar$ )
               if t_tstd <> 0 then L31850
                   t_tper$ = na$
                   goto L31880
L31850:     call "CONVERT" (t_tper, 1.1, t_tper$ )

*       *Convert values needed for detail format report
L31880:     if show_detail$ <> "D" then L32200
               for i% = 1% to 12%
                 call "CONVERT" (b_act(i%), 2.2, b_act$(i%))
                 call "CONVERT" (r_act(i%), 2.2, r_act$(i%))
                 call "CONVERT" (m_act(i%), 2.2, m_act$(i%))
                 call "CONVERT" (b_std(i%), 2.2, b_std$(i%))
                 call "CONVERT" (r_std(i%), 2.2, r_std$(i%))
                 call "CONVERT" (m_std(i%), 2.2, m_std$(i%))
                 call "CONVERT" (b_var(i%), 2.2, b_var$(i%))
                 call "CONVERT" (r_var(i%), 2.2, r_var$(i%))
                 call "CONVERT" (m_var(i%), 2.2, m_var$(i%))
                      if b_std(i%) <> 0 then L31990
                      b_per$(i%) = na$
                      goto L31991
L31990:          call "CONVERT" (b_per(i%), 1.1, b_per$(i%))
L31991:               if r_std(i%) <> 0 then L32000
                      r_per$(i%) = na$
                      goto L32001
L32000:          call "CONVERT" (r_per(i%), 1.1, r_per$(i%))
L32001:               if m_std(i%) <> 0 then L32010
                      m_per$(i%) = na$
                      goto L32020
L32010:          call "CONVERT" (m_per(i%), 1.1, m_per$(i%))
L32020:        next i%

            call "CONVERT" (b_tact, 2.2, b_tact$)
            call "CONVERT" (b_tstd, 2.2, b_tstd$)
            call "CONVERT" (b_tvar, 2.2, b_tvar$)
                      if b_tstd <> 0 then L32070
                      b_tper$ =  na$
                      goto L32090
L32070:     call "CONVERT" (b_tper, 1.1, b_tper$)

L32090:     call "CONVERT" (r_tact, 2.2, r_tact$)
            call "CONVERT" (r_tstd, 2.2, r_tstd$)
            call "CONVERT" (r_tvar, 2.2, r_tvar$)
                      if r_tstd <> 0 then L32120
                      r_tper$ = na$
                      goto L32140
L32120:     call "CONVERT" (r_tper, 1.1, r_tper$)

L32140:     call "CONVERT" (m_tact, 2.2, m_tact$)
            call "CONVERT" (m_tstd, 2.2, m_tstd$)
            call "CONVERT" (m_tvar, 2.2, m_tvar$)
                      if m_tstd <> 0 then L32170
                      m_tper$ = na$
                      goto L32200
L32170:     call "CONVERT" (m_tper, 1.1, m_tper$)

*       *Load the bucket descriptions
L32200:     call "STCSETID" (3%, #5, set$, setid$, bktid$(), bdescr$(),  ~
                                          setdescr$)

*       *Clean up some last minute details
           d_title$ = " Desc:"
           if jobdescr$ <> partdesc$ then L32280
              jobdescr$ = " "
              d_title$ = " "
L32280:    got_printed% = got_printed% + 1%
           call "CONVERT" (to_build, 2.2, to_build$)
           call "CONVERT" (scrap, 2.2, scrap$)
           call "CONVERT" (rework, 2.2, rework$)
           call "CONVERT" (est_percent, 0.0, est_per$)
           call "CONVERT" (partqty, 2.2, partqty$)
           call "CONVERT" (jbact, 2.2, jbact$)
           call "CONVERT" (jbstd, 2.2, jbstd$)
           call "CONVERT" (stccost, 2.2, stccost$) /* this total unused*/
           call "CONVERT" (cost, 2.2, cost$)       /* this total unused*/
           call "DATEFMT" (date_st$)
           call "DATEFMT" (date_ae$)
           if date_ae$ = " " or date_ae$ = blankdate$ then date_ae$ = " *Open* "

           if set_title$ <> " " then L32460
              set_title$ = "Cost Set: " & set$ & " - " & setdescr$
              call "FMTTITLE" (set_title$, " ", 2%)

*       *Print the summary report
L32460:     if show_detail$  = "D" then print_details

            if linecnt% > 50% then gosub print_page_head
            print using L64280 , jbcode$, d_title$, jobdescr$
            print using L64300 , part$, date_st$
            print using L64310 , partdesc$, date_ae$, bn$(1%),bdescr$(1%),~
                            t_act$(1%), t_std$(1%), t_var$(1%), t_per$(1%)
            print using L64320 , control$, est_per$,  bn$(2%),bdescr$(2%),~
                            t_act$(2%), t_std$(2%), t_var$(2%), t_per$(2%)
            print using L64330 , to_build$,           bn$(3%),bdescr$(3%),~
                            t_act$(3%), t_std$(3%), t_var$(3%), t_per$(3%)
            print using L64340 , partqty$,            bn$(4%),bdescr$(4%),~
                            t_act$(4%), t_std$(4%), t_var$(4%), t_per$(4%)
            print using L64350 , scrap$ ,             bn$(5%),bdescr$(5%),~
                            t_act$(5%), t_std$(5%), t_var$(5%), t_per$(5%)
            print using L64360 , rework$ ,            bn$(6%),bdescr$(6%),~
                            t_act$(6%), t_std$(6%), t_var$(6%), t_per$(6%)
            linecnt% = linecnt% + 8%

*       *We printed six, now determine how many more lines to print
            maxprint% = 6%
            for i% = 7% to 12%
               if bdescr$(i%) <> "  " then L32638
               if t_act(i%) <> 0 then   L32638
               if t_std(i%) <> 0 then   L32638
               goto L32640
L32638:     maxprint% = i%
L32640:     next i%
            if maxprint% = 6% then summary_tot

*       *Print the additional lines because active buckets found

            for i% = 7% to maxprint%
            print using L64365,                       bn$(i%),bdescr$(i%),~
                            t_act$(i%), t_std$(i%), t_var$(i%), t_per$(i%)
            next i%
            linecnt% = linecnt% + (maxprint% - 6%)

        summary_tot
            print using L64370 , t_tact$, t_tstd$ , t_tvar$, t_tper$
            print
            linecnt% = linecnt% + 2%
            goto   clear_and_read


        print_details
            if linecnt% > 50% then gosub print_page_head
            print
            print using L64410 , jbcode$, d_title$, jobdescr$, to_build$, ~
                                scrap$, date_st$
            print using L64420 , part$, partqty$,  rework$,    date_ae$


            print using L64430 , partdesc$, control$, est_per$
            print
            print
            print using L64450   /* Banner */
            print using L64470   /* Column Headers */
            print using L64480   /* Underlines     */

            for i% = 1% to 12%   /* BOM and RTE costs */
               print using L64490, bn$(i%), bdescr$(i%),  b_act$(i%),     ~
                                  b_std$(i%), b_var$(i%), b_per$(i%),    ~
                                  r_act$(i%), r_std$(i%), r_var$(i%),    ~
                                  r_per$(i%)
            next i%

            print using L64500 , b_tact$, b_tstd$, b_tvar$, b_tper$,      ~
                                r_tact$, r_tstd$, r_tvar$, r_tper$
            print
            print
            print using L64460   /* Banner */
            print using L64470   /* Column Headers */
            print using L64480   /* Underlines     */

            for i% = 1% to 12%   /* MISC and TOTAL costs */
               print using L64490, bn$(i%), bdescr$(i%),  m_act$(i%),     ~
                                  m_std$(i%), m_var$(i%), m_per$(i%),    ~
                                  t_act$(i%), t_std$(i%), t_var$(i%),    ~
                                  t_per$(i%)
            next i%

            print using L64500 , m_tact$, m_tstd$, m_tvar$, m_tper$,      ~
                                t_tact$, t_tstd$, t_tvar$, t_tper$
            linecnt% = 99%
            goto   clear_and_read

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L33270: FMT                 /* FILE: JBMASTR2                          */~
            CH(8),          /* Production job code                     */~
            CH(30),         /* Job Description                         */~
            POS(58),                                                     ~
            CH(25),         /* Part code                               */~
            POS(83),                                                     ~
            PD(14,4),       /* Quantity to make                        */~
            PD(14,4),       /* Quantity completed to date              */~
            POS(147),                                                    ~
            CH(6),          /* Date production job actually started    */~
            CH(6),          /* Date production job actually ended      */~
            POS(212),                                                    ~
            PD(14,4),       /* Scrap quantity                          */~
            PD(14,4),       /* Rework Quantity                         */~
            POS(232),                                                    ~
            PD(14,4),       /* Actual (aka inventory) Costs            */~
            12*PD(14,4),    /* Actual BOM Costs                        */~
            12*PD(14,4),    /* Actual RTE/LABOR Costs                  */~
            12*PD(14,4),    /* Actual MISC Costs                       */~
            POS(824),                                                    ~
            PD(14,4),       /* Cost associated with Standard Cost      */~
            12*PD(14,4),    /* Standard BOM Costs                      */~
            12*PD(14,4),    /* Standard RTE/LABOR Costs                */~
            12*PD(14,4),    /* Standard MISC Costs                     */~
            CH(19),         /* Control Number                          */~
            PD(14,4)        /* Percent Complete                        */



        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub set_pf1
*            STR(LINE2$,,50) = "Report Selection Criteria"
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40240,         /* Job From Range    */   ~
                                L40240,         /* Job to Range      */   ~
                                L40240,         /* Part From Range   */   ~
                                L40240,         /* Part to Range     */   ~
                                L40240,         /* Date From Range   */   ~
                                L40240,         /* Date to Range     */   ~
                                L40240,         /* Job Cut-off Range */   ~
                                L40240,         /* Zero Include flag */   ~
                                L40240,         /* JOB SUMMARY FLAG  */   ~
                                L40240          /* Background Flag   */
              goto L40270

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40240:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40270:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Job Costs and Variance by Cost Bucket",         ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,27), fac(hex(ac)), title$(1)              , ch(25),~
               at (05,55), fac(hex(ac)), title$(2)              , ch(25),~
                                                                         ~
               at (06,02), "Job Number Range",                           ~
               at (06,27), fac(lfac$( 1)), jobfrom$(1%)         , ch(08),~
               at (06,55), fac(lfac$( 1)), jobto$(1%)           , ch(08),~
                                                                         ~
               at (07,02), "Part Number Range",                          ~
               at (07,27), fac(lfac$( 2)), partfrom$(1%)        , ch(25),~
               at (07,55), fac(lfac$( 2)), partto$(1%)          , ch(25),~
                                                                         ~
               at (08,02), "Date Range",                                 ~
               at (08,27), fac(lfac$( 3)), datefrom$            , ch(10),~
               at (08,55), fac(lfac$( 3)), dateto$              , ch(10),~
                                                                         ~
               at (09,02), "Cost Set",                                   ~
               at (09,27), fac(lfac$( 4)), set$                 , ch(08),~
                                                                         ~
               at (10,02), "Show Closed Jobs or All?",                   ~
               at (10,27), fac(lfac$( 5)), closed_or_all$       , ch(01),~
                                                                         ~
               at (11,02), "Show Summary or Detail?",                    ~
               at (11,27), fac(lfac$( 6)), show_detail$         , ch(01),~
                                                                         ~
               at (12,02), "How to Show Favorable?",                     ~
               at (12,27), fac(lfac$( 7)), favorable_pn$        , ch(01),~
                                                                         ~
               at (13,02), "Tolerance %, Total",                         ~
               at (13,27), fac(lfac$( 8)), tolerance_tot$       , ch(04),~
                                                                         ~
               at (14,02), "Tolerance %, Bucket",                        ~
               at (14,27), fac(lfac$( 9)), tolerance_bkt$       , ch(04),~
                                                                         ~
               at (15,02), "Run in Background?",                         ~
               at (15,27), fac(lfac$(10)), back$                , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40710
                  call "MANUAL" ("JBVARBKT") : goto L40270

L40710:        if keyhit% <> 15 then L40740
                  call "PRNTSCRN" : goto L40270

L40740:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40930     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40890
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40890:     if fieldnr% > 1% then L40910
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40910:     return

L40930: if fieldnr% > 0% then L41050  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Print Report"
            if back$ = "Y" then pf$(3) =                                 ~
                     "                                        " &        ~
                     "                       (16)Submit Task "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L41050:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
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
            on fieldnr% gosub L50160,         /* Job From Range         */~
                              L50210,         /* Part From Range        */~
                              L50260,         /* Date From Range        */~
                              L50420,         /* Cost Set               */~
                              L50460,         /* Closed or All          */~
                              L50520,         /* Detail or Summary      */~
                              L50560,         /* How show favorable?    */~
                              L50570,         /* Total Tolerance        */~
                              L50585,         /* Bucket Tolerance       */~
                              L50600          /* Background Flag        */
            return
L50160: REM Test for Job From Range               JOBFROM$
            call "TESTRNGE" (jobfrom$(1%), jobto$(1%),                   ~
                             jobfrom$(2%), jobto$(2%), errormsg$)
            return

L50210: REM Test for Part From Range              PARTFROM$
            call "TESTRNGE"(partfrom$(1%), partto$(1%),                  ~
                            partfrom$(2%), partto$(2%), errormsg$)
            return

L50260: REM Test for Date From Range              DATEFROM$

            lowdate% = 0% :  highdate% = 99999999%
            if datefrom$ = " " or datefrom$ = blankdate$ then datefrom$ = "ALL"
            if datefrom$ = "ALL" then L50380
                call "DATEOKC" (datefrom$, lowdate%, errormsg$)
                if errormsg$ <> " " then return
                if dateto$ <> " " and dateto$ <> blankdate$ then L50360
                     dateto$ = datefrom$ : highdate% = lowdate%
                     goto L50380
L50360:     call "DATEOKC" (dateto$, highdate%, errormsg$)
            if errormsg$ <> " " then return
L50380:     if lowdate% > highdate% then errormsg$ =                     ~
                "FROM Date must be LESS than or EQUAL to the TO Date"
                return

L50420: REM Cost Set                              SET$
            if set$ = " " then return
            plowkey$     = "STC.HDR." & set$
            setdescr$ = hex(06) & "Select Cost Set"
                                   call "PLOWCODE" (#5, plowkey$,        ~
                                                       setdescr$,        ~
                                                    8%, 0.30, onfile%)

            if onfile% = 1% then L50444
                errormsg$ = "Cost Set Not on File.  Select or leave blank"
                setdescr$ = " "
                return
L50444:     set$ =  str(plowkey$,9)
                return

L50460: REM Test for show closed or all jobs      CLOSED_OR_ALL$
            if closed_or_all$ = "C" or closed_or_all$ = "A" then return
            errormsg$ = "Please enter 'C' or 'A' to include 'C'lose" &   ~
                        "d or 'A'll jobs"
            return

L50520: REM Test for Detail or Summary    SHOW_DETAIL$
            test% = pos("DFR" = show_detail$)
            if test% <> 0% then return
            errormsg$ = "Please enter 'D' , 'R', or 'F' to indicate th" &~
                        "e type and format."
            return

L50560: REM Test for How to show favorable   FAVORABLE_PN$
            if favorable_pn$ = "P" or favorable_pn$ = "N" then return
            errormsg$ = "Please enter 'P' or 'N' to show favorable  " &  ~
                        "variances as 'P'ositive or 'N'egative"
            return

L50570: REM Test for total tolerance     TOLERANCE_TOT$
            call "NUMTEST"(tolerance_tot$, 0, 999,errormsg$, 0.0, tol_tot)
            return

L50585: REM Test for bucket tolerance     TOLERANCE_BKT$
            call "NUMTEST"(tolerance_bkt$, 0, 999,errormsg$, 0.0, tol_bkt)
            return

L50600: REM Test for run in background    BACK$
            if back$ <> "Y" then back$ = "N"
            return


        REM *************************************************************~
            *     P R I N T   R O U T I N E S                           *~
            *                                                           *~
            *                                                           *~
            *                                                           *~
            *************************************************************

        print_page_head
            print page
            pagecnt% = pagecnt% + 1%
            print using L64060, date$, rpt_time$, company$
            print using L64100, print_title$, pagecnt%
            print using L64130, set_title$
            print
            print
            linecnt% = 5%
            return

        print_parameters
            if back$ <> "Y" then L61100
               if lowdate% = 0% and highdate% = 99999999% then datefrom$   ~
                  = "ALL"
            if datefrom$ = "ALL" then L61100

            convert lowdate% to datefrom$, pic(########)
            call "DATEFMT"(datefrom$)
            convert highdate% to dateto$, pic(########)
            call "DATEFMT"(dateto$)
L61100:     gosub print_page_head
            print skip(3)
            print tab(37);
            print "--------------------- Report Selection Parameters ----~
        ~---------"
            print
            print using L64220, "Job Range", jobfrom$(1), jobto$(1)
            print using L64220, "Part Range", partfrom$(1), partto$(1)
            print using L64220, "Date Range", datefrom$, dateto$
            print using L64220, "Cost Set  ", set$
            print using L64220, "Closed or All", closed_or_all$
            print using L64220, "Detail or Summary", show_detail$
            print using L64220, "Show Favorable P/N", favorable_pn$
            print using L64220, "Total Tolerance", tolerance_tot$
            print using L64220, "Bucket Tolerance", tolerance_bkt$
            print using L64220, "Print in Background", back$
            print tab(37);
            print "------------------------------------------------------~
        ~---------"
            linecnt% = 999%  /* trigger new page later */
            return


        print_end_report
            gosub print_page_head
            print using L64510
            print
            print using L64520 , total_read%
            print using L64530 , in_range%
            print using L64540 , in_tolerance%
            print using L64550 , got_printed%

            print
            print using L64840
            return

        REM *************************************************************~
            *                  R E P O R T   F O R M A T S              *~
            *-----------------------------------------------------------*~
            *  REPORT FORMAT LINES USED BY PRINT STATEMENTS             *~
            *************************************************************

*       ** Page Header Images

L64060: % ######## ########                  ############################~
        ~################################                    JBVARBKT:JB00~
        ~14

L64100: %                                    ############################~
        ~################################                        PAGE:####

L64130: %                                    ############################~
        ~################################

*       ** Page 0 Images

        %                                     ----------------- REPORT SE~
        ~LECTION PARAMETERS -----------------

        %                                                           FROM ~
        ~                     TO

L64220: %                                      #################### #####~
        ~#################### #########################

        %                                     ---------------------------~
        ~------------------------------------


*       ** Summary Format Images

L64280: %Job:  ########  ###### ##############################    Bkt Cos~
        ~t Bucket Description            Actual   Standard        Var$   V~
        ~ar%

L64300: %P/N:  #########################   Start Date: ########## --- ---~
        ~--------------------------- ---------- ----------  ---------- ---~
        ~---

L64310: %Desc: ############################# End Date: ########## ##  ###~
        ~########################### ########## ##########  ########## ###~
        ~###

L64320: %Control No: ###################  Est. % Complete: ###    ##  ###~
        ~########################### ########## ##########  ########## ###~
        ~###

L64330: %Job Quantity: ##########                                 ##  ###~
        ~########################### ########## ##########  ########## ###~
        ~###


L64340: %Qty Complete: ##########                                 ##  ###~
        ~########################### ########## ##########  ########## ###~
        ~###

L64350: %Qty Scrapped: ##########                                 ##  ###~
        ~########################### ########## ##########  ########## ###~
        ~###

L64360: %Qty Reworked: ##########                                 ##  ###~
        ~########################### ########## ##########  ########## ###~
        ~###

L64365: %                                                         ##  ###~
        ~########################### ########## ##########  ########## ###~
        ~###

L64370: %                                                                ~
        ~                  Totals:   ########## ##########  ########## ###~
        ~###

*       ** Detail Format Images

L64410: %Job: ########  ###### ##############################  Job Quanti~
        ~ty: ##########  Qty Scrapped: ##########  Actual Start: ########

L64420: %Part: #########################                       Qty Comple~
        ~te: ##########  Qty Reworked: ##########  Actual End  : ########

L64430: %Desc: ################################  Control No.: ###########~
        ~########  Est. % Complete: ###

L64450: %                                    **************** BOM COSTS *~
        ~**************         ************** ROUTE COSTS **************

L64460: %                                    *************** MISC. COSTS ~
        ~**************         ************** TOTAL COSTS **************

L64470: %Bkt      Bucket Description             Actual    Standard  Vari~
        ~ance $   Var %            Actual    Standard   Variance $  Var %

L64480: %--  ------------------------------  ----------  ----------  ----~
        ~------  ------        ----------  ----------  ----------  ------

L64490: %##  ##############################  ##########  ##########  ####~
        ~######  ######        ##########  ##########  ##########  ######


L64500: %                        Totals:     ##########  ##########  ####~
        ~######  ######        ##########  ##########  ##########  ######

L64510: %                                      * * * *  R E P O R T   S U~
        ~ M M A R Y  * * * *

L64520: %                                           Total Jobs Examined: ~
        ~#######
L64530: %                                           Jobs in Range:       ~
        ~#######
L64540: %                                           Jobs in Tolerance:   ~
        ~#######
L64550: %                                           Jobs Printed:        ~
        ~#######

L64840: %                                      * * * *  E N D   O F   R E~
        ~ P O R T  * * * *

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
            * COPYRIGHT (C) 1988  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
