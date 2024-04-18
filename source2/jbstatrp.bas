        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  JJJJJ  BBBB    SSS   TTTTT   AAA   TTTTT  RRRR   PPPP    *~
            *    J    B   B  S        T    A   A    T    R   R  P   P   *~
            *    J    BBBB    SSS     T    AAAAA    T    RRRR   PPPP    *~
            *  J J    B   B      S    T    A   A    T    R  R   P       *~
            *   J     BBBB    SSS     T    A   A    T    R   R  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBSTATRP - THIS PROGRAM WILL PRODUCE THE JOB              *~
            *            STATISTICS ANALYSIS REPORT.                    *~
            *            THIS PROGRAM PRODUCES JOB SUMMARY STATISTIC    *~
            *            INFORMATION FOR PART, COMPONENTS, AND WORK     *~
            *            CENTERS.                                       *~
            *            A STANDARD DEVIATION CALCULATION IS DONE       *~
            *            FOR SEVERAL FIELDS ON THE REPORT. THE STD      *~
            *            DEVIATION FORMULA IS EXPLAINED IN THE          *~
            *            DATALOAD SECTION OF THE PROGRAM.               *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1988  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/18/88 ! Original                                 ! BPN *~
            * 08/13/90 ! Added option to include jobs that have no! MJB *~
            *          !   completed quantity, defaults to NO.    !     *~
            *          ! Corrected part description to be part    !     *~
            *          !   description, not job description.      !     *~
            *          ! Changed quantity print format to 2 dec.  !     *~
            * 08/13/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            activity$4,                  /* ACTIVITY CODE              */~
            back$1,                      /* BACKGROUND FLAG            */~
            comp$25,                     /* COMPONENT CODE             */~
            company$60,                  /* COMPANY / DIVISION NAME    */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            date_ae$8,                   /* DATE JOB ACTUALLY ENDED    */~
            date_as$8,                   /* DATE JOB ACTUALLY STARTED  */~
            date_pe$8,                   /* DATE JOB PLANNED END       */~
            date_ps$8,                   /* DATE JOB PLANNED START     */~
            datefrom$10,                 /* Date From Range            */~
            dateto$10,                   /* Date to Range              */~
            dtewk$(2%)6,                 /* DATE WORK AREAS            */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            jbcode$8,                    /* JOB CODE                   */~
            jobdescr$30,                 /* JOB Description            */~
            jobsum$1,                    /* JOB SUMMARY FLAG           */~
            jobcount$4,                  /* Job Cut-off Range          */~
            jobfrom$(2%)8,               /* Job from Range             */~
            jobto$(2%)8,                 /* Job to range               */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            message$78,                  /* INFORMATION MESSAGE        */~
            part$25,                     /* PART CODE                  */~
            partdesc$30,                 /* Job Part desc              */~
            partfrom$(2%)25,             /* Part From Range            */~
            partto$(2%)25,               /* Part to Range              */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            print$(12)10,                /* WORK VARIABLE              */~
            print4$(6)4,                 /* WORK PRINT VARIABLE        */~
            print6$(4)6,                 /* WORK PRINT VARIABLE        */~
            print8$(2)9,                 /* WORK PRINT VARIABLE        */~
            print_title$60,              /* REPORT TITLE               */~
            readkey$99,                  /* READ KEY                   */~
            rpt_time$8,                  /* REPORT TIME                */~
            step$7,                      /* STEP CODE                  */~
            syskey$20,                   /*                            */~
            tdate$10,                    /* Temporary Date Variable    */~
            title$(2)30,                 /* SCREEN COLUMN HEADINGS     */~
            userid$3,                    /* Current User Id            */~
            wk_comp$25,                  /* WORK COMPONENT CODE        */~
            wk_partdate$6,               /* WORK PART DATE             */~
            zflag$1,                     /* 0 complete jobs include    */~
            wrkctr$4                     /* WORK CENTER                */

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
            * # 2 ! JBMATER2 ! Production job material used detail file *~
            * # 3 ! JBSTATUS ! Production job actual structure (RTE) ac *~
            * # 4 ! WCMASTR  ! Workcenter Master File                   *~
            * # 5 ! SYSFILE2 ! SYSTEM INFO                              *~
            * # 6 ! WORK1    ! WORK JOB/PART INFO                       *~
            * # 7 ! WORK2    ! WORK PART/COMPONENT INFO                 *~
            * # 8 ! WORK3    ! WORK PART/WK CENTER INFO                 *~
            * # 9 ! WORK4    ! WORK JOBS PROCESSED                      *~
            * #10 ! HNYMASTR ! Inventory Parts Master File              *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 1, "JBMASTR2",                                      ~
                        varc,     indexed,  recsize = 1300,              ~
                        keypos =    1, keylen =   8                      ~

            select # 2, "JBMATER2",                                      ~
                        varc,     indexed,  recsize =  400,              ~
                        keypos =    1, keylen =  22,                     ~
                        alt key  1, keypos =   23, keylen =  48          ~

            select # 3, "JBSTATUS",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    1, keylen =  12,                     ~
                        alt key  1, keypos =   21, keylen =  44,         ~
                            key  2, keypos =   29, keylen =  36          ~

            select # 4, "WCMASTR",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    2, keylen =   5,                     ~
                        alt key  1, keypos =    1, keylen =   6          ~

            select # 5, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize = 500,               ~
                        keypos =    1, keylen =  20

            select # 6, "WORK1",                                         ~
                        varc,     indexed,  recsize =   105,             ~
                        keypos =    1, keylen =  25

            select # 7, "WORK2",                                         ~
                        varc,     indexed,  recsize =   276,             ~
                        keypos =    1, keylen =  50

            select # 8, "WORK3",                                         ~
                        varc,     indexed,  recsize =   116,             ~
                        keypos =    1, keylen =  37

            select # 9, "WORK4",                                         ~
                        varc,     indexed,  recsize =    33,             ~
                        keypos =    1, keylen =  33

            select #10, "HNYMASTR",                                      ~
                         varc, indexed, recsize = 900,                   ~
                         keypos = 1, keylen = 25,                        ~
                         alternate key 1, keypos = 102, keylen = 9, dup, ~
                                   key 2, keypos = 90, keylen = 4, dup,  ~
                                   key 3, keypos = 26, keylen = 32, dup

            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (# 1, fs%( 1), f2%( 1), 0%, rslt$( 1))
            call "OPENCHCK" (# 2, fs%( 2), f2%( 2), 0%, rslt$( 2))
            call "OPENCHCK" (# 3, fs%( 3), f2%( 3), 0%, rslt$( 3))
            call "OPENCHCK" (# 4, fs%( 4), f2%( 4), 0%, rslt$( 4))
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

            str(line2$,62) = "JBSTATRP: " & str(cms2v$,,8)
            if back$ = "B" then printing_in_background
            title$(1) = "From" : title$(2) = "To  "

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  7%
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
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       editpg1
L11140:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% >  8% then editpg1
            if fieldnr% > 6% then fieldnr% = fieldnr% - 1%
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
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
        REM SETUP FOR BACKGROUND PROCESSING
            if back$ <> "Y" then L19270
                call "READ101" (#5, "zJBSTATRP." & userid$, f1%(5))
                put #5, using L19130, "zJBSTATRP." & userid$, jobfrom$(), ~
                     jobto$(), partfrom$(), partto$(), lowdate%,         ~
                     highdate%, datefrom$, dateto$, jobcount$, jobsum$,  ~
                     back$
L19130:         FMT CH(20), 4*CH(8), 4*CH(25), 2*BI(4), 2*CH(08),        ~
                     CH(04), 2*CH(01)
                if f1%(5) = 0 then write #5 else rewrite #5
                call "TASKUP" ("ME", 0%)
                goto L65000

        printing_in_background
            message$ = "rptReport JBSTATRP in background: Aborted."
            call "READ101" (#5, "zJBSTATRP." & userid$, f1%(5))
                 if f1%(5) = 0 then tell_user
            get #5, using L19130, syskey$, jobfrom$(), jobto$(),          ~
                 partfrom$(), partto$(), lowdate%, highdate%, datefrom$, ~
                 dateto$, jobcount$, jobsum$, back$
            delete #5

L19270:     call "SHOSTAT" ("Creating Work Files")
            records% = max(500%, val(str(rslt$(1), 17%, 4%),4))
            printsz% = printsz% + records%
            call "WORKOPEN" (# 6, "IO", records%, f1%(6))
            records% = max(500%, val(str(rslt$(2), 17%, 4%),4))
            printsz% = printsz% + records%
            call "WORKOPEN" (# 7, "IO", records%, f1%(7))
            records% = max(500%, val(str(rslt$(3), 17%, 4%),4))
            printsz% = printsz% + records%
            call "WORKOPEN" (# 8, "IO", records%, f1%(8))

            if jobsum$ <> "Y" then L19440

            records% = max(500%, val(str(rslt$(1), 17%, 4%),4))
            printsz% = printsz% + records%
            call "WORKOPEN" (# 9, "IO", records%, f1%(9))

L19440:     gosub dataload
            call "SHOSTAT" ("Gathering Summary Information")
            gosub variance_load
            call "SHOSTAT" ("Report Generation in Progress")
            printsz% = printsz%/2
            call "SETPRNT" ("JB0007", "        ", printsz%, 0%)
            rpt_time$ = " "
            call "TIME" (rpt_time$)  /*  GET REPORT TIME    */
            select printer(134)
            linecnt% = 0
            pagecnt% = -1%
            gosub print_parameters
            gosub print_report

            gosub print_end_report
            close printer
            call "SETPRNT" ("JB0007", "        ", printsz%, 1%)
            printsz% = 0
            call "FILEBGON" (#6)
            call "FILEBGON" (#7)
            call "FILEBGON" (#8)
            if jobsum$ = "Y" then call "FILEBGON" (#9)

            if pagecnt% > 0 then L19790

                message$ = "rptReport JBSTATRP in background: No data met~
        ~ selection criteria."
                if back$ = "Y" then tell_user
                keyhit% = 2%
                call "ASKUSER" (keyhit%, "****",                         ~
                    "SORRY, no jobs eligible for printing were found",   ~
                    "using the requested selection criteria.",           ~
                    "Press RETURN to change your selection or to exit.")
                goto editpg1

L19790:     if back$ <> "Y" then inputmode

            message$ = "rptReport JBSTATRP in background: completed."
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
                              L20290,         /* Job Cut-off Range      */~
                              L20325,         /* Include 0 complete     */~
                              L20340,         /* JOB SUMMARY FLAG       */~
                              L20390          /* BACKGROUND FLAG        */

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

L20290: REM Def/Enable Part Cut-off Range          JOBCOUNT$
            inpmessage$ = "Enter the Number of Occurrences for Each " &  ~
                          "Part to be Included on the Report."
            if jobcount$ = " " then jobcount$ = "9999"
            return

L20325: REM Def/Enable Include jobs with zero complete     ZFLAG$
            inpmessage$ = "Enter 'Y' to Include Jobs that have zero " &  ~
                          "Quantity Complete"
            if zflag$ = " " then zflag$ = "N"
            return

L20340: REM DEF/ENABLE FOR JOB SUMMARY             JOBSUM$
            inpmessage$ = "Enter a 'Y' to Print a Job Summary for "     &~
                          "Parts Reported."
            if jobsum$ = " " then jobsum$ = "N"
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
                      jobsum$, print$(), jobcount$, back$, zflag$
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
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            *  BUILD WORK FILES TO PRODUCE THE REPORT.                  *~
            *  THE DATALOAD SECTION SUMMARIZES ALL PART, COMPONENT,     *~
            *  AND WORK CENTER INFORMATION BY JOB AND WRITES IT TO THE  *~
            *  APPROPRIATE WORK FILE. WORK1 = PART, WORK2 = COMPONENT,  *~
            *  WORK3 = WORKCENTER.                                      *~
            *  A STANDARD DEVIATION IS CALCULATED FOR SEVERAL FIELDS.   *~
            *  THE SUMMARY INFORMATION IS USED IN THE VARIANCE_LOAD     *~
            *  SECTION FOR THE FIRST PART OF THE CALCULATION.           *~
            *  THE STANDARD DEVIATION CALCULATION IS,                   *~
            *      STD DEV = SQR((X - X1)^2)/N - 1                      *~
            *       WHERE N = TOTAL JOBS THAT BUILT THIS PART           *~
            *             X1 = AVG  (TOTAL PART, COMP OR WORK CENTER)/N *~
            *             X  = EACH RECORD (PART, COMP, OR WORK CENTER) *~
            *                  TOTAL                                    *~
            *             (X - X1)^2 = SUMMARY TOTAL FOR EACH RECORD    *~
            *                          (PART, COMP, WORK CENTER)        *~
            *                     CALCULATED IN THE VARIANCE_LOAD       *~
            *                     SECTION                               *~
            *   THE ACTUAL STD DEVIATION IS CALCULATED IN THE           *~
            *   PRINT_REPORT SECTION, USING THE SUMMARY TOTAL           *~
            *   CALCULATED IN THE VARIANCE_LOAD SECTION.                *~
            *************************************************************
        dataload
            convert jobcount$ to jobcount
            plowkey$ = jobfrom$(2)
        REM ** READ TO PRIME THE FILE **
            call "PLOWALTS" (#1, plowkey$, 0%, 0%, f1%(1))
            goto L30340

        read_job
            call "READNEXT" (#1, f1%(1))
L30340:         if f1%(1) = 0% then return
            jbcode$ = key(#1)

        REM ** CHECK IF JOB IS WITHIN PARAMETER RANGE **
            if jbcode$ < jobfrom$(2) or                                  ~
               jbcode$ > jobto$(2) then read_job

            get #1, using L35060, jbcode$, jobdescr$, part$, partqty,     ~
                        date_ae$, partcost_act, partcrdt, partcost_std

        REM ** CHECK IF PART IS WITHIN PARAMETER RANGE **
            if part$ < partfrom$(2) or                                   ~
               part$ > partto$(2) then read_job

            partdesc$ = "Part Not on File"
            call "DESCRIBE"(#10, part$, partdesc$, 0%, f1%(1))

        REM ** CHECK IF ACTUAL END DATE IS WITHIN PARAMETER RANGE **
            if date_ae$ = " " or date_ae$ = blankdate$ then read_job
            tdate$ = date_ae$
            call "DATEOK" (tdate$, date_ae%, " ")
            if date_ae% = 0% then read_job
            if date_ae% < lowdate% or                                    ~
               date_ae% > highdate% then read_job

        REM ** Check If Qty Complete = 0 should be included **
            if partqty = 0 and zflag$ = "N" then read_job

        REM ** SAVE PART INFORMATION **
            readkey$ = part$
            call "READ100" (#6, readkey$, f1%(6))
                if f1%(6) = 0% then L30680
            get #6, using L35710, part$, partdesc$, partcnt, wk_partqty,  ~
                         wk_partdate$, wk_partcost_act, wk_partcrdt,     ~
                         wk_partstd_cost

        REM ** CHECK IF PART COUNT TOTAL HAS BEEN REACHED **
            if partcnt = jobcount then goto read_job

L30680:     gosub build_comp_info
            gosub build_wc_info
            readkey$ = part$
            call "READ101" (#6, readkey$, f1%(6))
                if f1%(6) = 0% then L30850
            get #6, using L35710, part$, partdesc$, partcnt, wk_partqty,  ~
                         wk_partdate$, wk_partcost_act, wk_partcrdt,     ~
                         wk_partstd_cost, partqty_var

            partqty = partqty + wk_partqty
            partcost_act = partcost_act + wk_partcost_act
            partcrdt = partcrdt + wk_partcrdt
            partcost_std = partcost_std + wk_partstd_cost
            partcnt = partcnt + 1
            goto put_part_info

        REM ** FIRST TIME ADD **
L30850:     partcnt = 1

        put_part_info
            put #6, using L35710, part$, partdesc$, partcnt, partqty,     ~
                         date_ae$, partcost_act, partcrdt, partcost_std, ~
                         partqty_var

            if f1%(6) = 1% then rewrite #6 else write #6
            goto check_job_summary

        build_comp_info
            plowkey$ = jbcode$
            str(plowkey$,9%) = all(hex(00))

        save_comp_info
            call "PLOWNEXT" (#2, plowkey$, 8%, f1%(2))
                if f1%(2) = 0% then return
            get #2, using L35220, jbcode$, comp$, compqty, compact_cost,  ~
                         compstd_cost

            readkey$ = str(part$) & str(comp$)
            call "READ101" (#7, readkey$, f1%(7))
                if f1%(7) = 0% then L31160
            get #7, using L35840, part$, wk_comp$, wk_compqty,            ~
                          wk_compact_cost, wk_compstd_cost, wk_comp_cnt

            compqty = compqty + wk_compqty
            wk_comp_cnt = wk_comp_cnt + 1
            compact_cost = compact_cost + wk_compact_cost
            compstd_cost = compstd_cost + wk_compstd_cost
            goto put_comp_info

L31160: REM * FIRST TIME ADD *
            wk_comp_cnt = 1

        put_comp_info
            put #7, using L35840, part$, comp$, compqty, compact_cost,    ~
                         compstd_cost, wk_comp_cnt
            if f1%(7) = 1% then rewrite #7 else write #7
            goto save_comp_info

        build_wc_info
            plowkey$ = str(jbcode$) & hex(00000000)

        save_wc_info
            call "PLOWNEXT" (#3, plowkey$, 8%, f1%(3))
                if f1%(3) = 0% then return
            get #3, using L35350, jbcode$, wc_seq%, wrkctr$,              ~
                         activity$, step$, setup%, runtime%, wc_moved,   ~
                         wc_scrap, wc_rework

            readkey$ = str(part$) & bin(wc_seq%,4) & str(step$)          ~
                       & str(wrkctr$) & str(activity$)
            call "READ101" (#8, readkey$, f1%(8))
                if f1%(8) = 0% then L31430
            get #8, using L35950, part$, wc_seq%, step$, wrkctr$,         ~
                          activity$, wk_setup, wk_runtime,               ~
                          wk_wc_moved, wk_wc_scrap, wk_wc_rework, wc_cnt

L31430:     setup = setup%
            runtime = runtime%

        REM ** READ WC MASTER FOR UNITS PER DAY FOR CALCULATIONS **
            call "READ100" (#4, wrkctr$, f1%(4))
                if f1%(4) <> 0% then L31530
            hoursperunit = 0
            unitsperday = 24
            goto L31560

L31530:     get #4 using L31540, unitsperday%
L31540:          FMT POS(2020), BI(2)
            unitsperday = unitsperday%
L31560:     hoursperunit = unitsperday / 24
            if f1%(8) = 0% then L31700

            setup = setup * hoursperunit
            setup = setup + wk_setup
            runtime = runtime * hoursperunit
            runtime = runtime + wk_runtime
            wc_moved = wc_moved + wk_wc_moved
            wc_scrap = wc_scrap + wk_wc_scrap
            wc_rework = wc_rework + wk_wc_rework
            wc_cnt = wc_cnt + 1
            goto put_wc_info

        REM * FIRST TIME ADDS *
L31700:     wc_cnt = 1
            setup = setup * hoursperunit
            runtime = runtime * hoursperunit

        put_wc_info
            put #8, using L35950, part$, wc_seq%, step$, wrkctr$,         ~
                         activity$, setup, runtime, wc_moved,            ~
                         wc_scrap, wc_rework, wc_cnt, setup_var,         ~
                         runtime_var, hoursperunit
            if f1%(8) = 1% then rewrite #8 else write #8
            goto save_wc_info

        check_job_summary
            if jobsum$ = "N" then goto read_job
            readkey$ = str(part$) & str(jbcode$)
            call "READ101" (#9, readkey$, f1%(9))
                if f1%(9) = 1% then L31900
            put #9, using L31880, part$, jbcode$
L31880:         FMT CH(25), CH(08)
            write #9
L31900:     goto read_job

*       ****************************************************************~
*         LOAD VARIANCES FOR STANDARD DEVIATION CALCULATIONS           *~
*         THIS SECTION WILL USE THE SUMMARY TOTALS CALCULATED IN       *~
*         THE DATALOAD SECTION TO CALCULATE THE FIRST PART NEEDED      *~
*         FOR THE STD DEVIATION CALCULATION. THE TOTALS ARE ADDED TO   *~
*         THE APPROPRIATE WORK FILE.                                   *~
*       ****************************************************************
        variance_load
        REM ** READ TO PRIME THE FILE **
            plowkey$ = jobfrom$(2)
            call "PLOWALTS" (#1, plowkey$, 0%, 0%, f1%(1))
            goto L32075

        read_job_var
            call "READNEXT" (#1, f1%(1))
L32075:         if f1%(1) = 0% then return
            jbcode$ = key(#1)

        REM ** CHECK IF JOB IS WITHIN PARAMETER RANGE **
            if jbcode$ < jobfrom$(2) or                                  ~
               jbcode$ > jobto$(2) then read_job_var

            get #1, using L35060, jbcode$, partdesc$, part$, partqty,     ~
                        date_ae$, partcost_act, partcrdt, partcost_std

        REM ** CHECK IF PART IS WITHIN PARAMETER RANGE **
            if part$ < partfrom$(2) or                                   ~
               part$ > partto$(2) then read_job_var

        REM ** CHECK IF ACTUAL END DATE IS WITHIN PARAMETER RANGE **
            if date_ae$ = " " or date_ae$ = blankdate$ then read_job_var
            tdate$ = date_ae$
            call "DATEOK" (tdate$, date_ae%, " ")
            if date_ae% = 0% then read_job_var
            if date_ae% < lowdate% or                                    ~
               date_ae% > highdate% then read_job_var

        REM ** Check If Qty Complete = 0 should be included **
            if partqty = 0 and zflag$ = "N" then read_job_var

        REM ** CALCULATE FIRST PART OF PART STD DEVIATION **
            readkey$ = part$
            call "READ101" (#6, readkey$, f1%(6))
                if f1%(6) = 0% then read_job_var
            get #6, using L35710, part$, partdesc$, partcnt, wk_partqty,  ~
                         wk_partdate$, wk_partcost_act, wk_partcrdt,     ~
                         wk_partstd_cost, partqty_var

            if partqty = 0 then L32235
            part_avg = wk_partqty/partcnt
            partqty_var = partqty_var + (partqty - part_avg)^2

L32235:     put #6, using L35710, part$, partdesc$, partcnt, wk_partqty,  ~
                         wk_partdate$,wk_partcost_act, wk_partcrdt,      ~
                         wk_partstd_cost, partqty_var

            if f1%(6) = 1% then rewrite #6 else write #6
            partqty_var = 0

        REM ** CALCULATE FIRST PART OF COMPONENT STD DEVIATION **
            plowkey$ = jbcode$
            str(plowkey$,9%) = all(hex(00))

        save_comp_var
            call "PLOWNEXT" (#2, plowkey$, 8%, f1%(2))
                if f1%(2) = 0% then build_wc_var
            get #2, using L35220, jbcode$, comp$, compqty, compact_cost,  ~
                         compstd_cost

            readkey$ = str(part$) & str(comp$)
            call "READ101" (#7, readkey$, f1%(7))
                if f1%(7) = 0% then goto save_comp_var
            get #7, using L35840, part$, wk_comp$, wk_compqty,            ~
                          wk_compact_cost, wk_compstd_cost, wk_comp_cnt, ~
                          compqty_var, compact_var, compstd_var

            if compqty = 0 then L32365
            compqty_avg = wk_compqty/partcnt
            compqty_var = compqty_var + (compqty - compqty_avg)^2
L32365:     if compact_cost = 0 then L32380
            compact_avg = wk_compact_cost/partcnt
            compact_var = compact_var + (compact_cost - compact_avg)^2
L32380:     if compstd_cost = 0 then put_comp_var
            compstd_avg = wk_compstd_cost/partcnt
            compstd_var = compstd_var + (compstd_cost - compstd_avg)^2

        put_comp_var
            put #7, using L35840, part$, wk_comp$, wk_compqty,            ~
                         wk_compact_cost, wk_compstd_cost, wk_comp_cnt,  ~
                         compqty_var, compact_var, compstd_var

            if f1%(7) = 1% then rewrite #7 else write #7
            compqty_var, compact_var, compstd_var = 0
            goto save_comp_var

        build_wc_var
        REM ** CALCULATE FIRST PART OF WORK CENTER STD DEVIATION **
            plowkey$ = str(jbcode$) & hex (00000000)

        save_wc_var
            call "PLOWNEXT" (#3, plowkey$, 8%, f1%(3))
                if f1%(3) = 0% then read_job_var
            get #3, using L35350, jbcode$, wc_seq%, wrkctr$,              ~
                         activity$, step$, setup%, runtime%, wc_moved,   ~
                         wc_scrap, wc_rework

            readkey$ = str(part$) & bin(wc_seq%,4) & str(step$)          ~
                       & str(wrkctr$) & str(activity$)
            call "READ101" (#8, readkey$, f1%(8))
                if f1%(8) = 0% then goto save_wc_var
            get #8, using L35950, part$, wc_seq%, step$, wrkctr$,         ~
                          activity$, wk_setup, wk_runtime,               ~
                          wk_wc_moved, wk_wc_scrap, wk_wc_rework,        ~
                          wc_cnt, setup_var, runtime_var, hoursperunit

            setup = setup%
            runtime = runtime%

            if setup = 0 then L32635
            setup_avg = wk_setup/partcnt
            setup = setup * hoursperunit
            setup_var = setup_var + (setup - setup_avg)^2

L32635:     if runtime = 0 then put_wc_var
            runtime_avg = wk_runtime/partcnt
            runtime = runtime * hoursperunit
            runtime_var = runtime_var + (runtime - runtime_avg)^2

        put_wc_var
            put #8, using L35950, part$, wc_seq%, step$, wrkctr$,         ~
                         activity$, wk_setup, wk_runtime,                ~
                         wk_wc_moved, wk_wc_scrap, wk_wc_rework,         ~
                         wc_cnt, setup_var, runtime_var, hoursperunit
            if f1%(8) = 1% then rewrite #8 else write #8
            setup_var, runtime_var = 0
            goto save_wc_var

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35060: FMT                 /* FILE: JBMASTR2                          */~
            CH(8),          /* Production job code                     */~
            CH(30),         /* PRODUCTION JOB DESC                     */~
            POS(58),                                                     ~
            CH(25),         /* Part code                               */~
            POS(91),                                                     ~
            PD(14,4),       /* Quantity completed to date              */~
            POS(153),                                                    ~
            CH(6),          /* Date production job actually ended      */~
            POS(232),                                                    ~
            PD(14,4),       /* Actual (aka inventory) Costs            */~
            POS(528),                                                    ~
            PD(14,4),       /* Total Credits from Job.                 */~
            POS(824),                                                    ~
            PD(14,4)        /* Cost associated with Standard Cost      */

L35220: FMT                 /* FILE: JBMATER2                          */~
            CH(8),          /* Production job code                     */~
            POS(23),                                                     ~
            CH(25),         /* Part code                               */~
            POS(71),                                                     ~
            PD(14,4),       /* quantity moved to job                   */~
            PD(14,4),       /* TOTAL Inventory Costs                   */~
            POS(183),                                                    ~
            PD(14,4),       /* TOTAL Cost assoc with Standard Cost     */~
            POS(327),                                                    ~
            CH(3),          /* user-id of specific user                */~
            PD(14,4)        /* Quantity of a subcomponent withdrawn fro*/

L35350: FMT                 /* FILE: JBSTATUS                          */~
            CH(8),          /* Production job code                     */~
            BI(4),          /* Binary Test                             */~
            CH(4),          /* Where stuff was moved                   */~
            CH(4),          /* Where stuff was moved                   */~
            POS(72),                                                     ~
            CH(7),          /* Where stuff was moved                   */~
            POS(91),                                                     ~
            BI(4),          /* Set Up time in WC units                 */~
            BI(4),          /* Run Time in Work Center Units           */~
            PD(14,4),       /* The quantity of parts Moved             */~
            PD(14,4),       /* The quantity of parts scrapped          */~
            PD(14,4)        /* The quantity of parts reworked          */

L35710: FMT                 /* FILE: WORK1                             */~
            CH(25),         /* Part code                               */~
            CH(30),         /* PART DESCRIPTION                        */~
            BI(4),          /* NUMBER OF TIMES THIS PART BUILT         */~
            PD(14,4),       /* Quantity completed to date              */~
            CH(6),          /* Date production job actually ended      */~
            PD(14,4),       /* Actual (aka inventory) Costs            */~
            PD(14,4),       /* Total Credits from Job.                 */~
            PD(14,4),       /* Cost associated with Standard Cost      */~
            PD(14,4)        /* QUANTITY VARIANCE                       */

L35840: FMT                 /* FILE: WORK2                             */~
            CH(25),         /* Part code                               */~
            CH(25),         /* COMPONENT CODE                          */~
            PD(14,4),       /* quantity moved to job                   */~
            PD(14,4),       /* Inventory Costs                         */~
            PD(14,4),       /* Cost associated with Standard Cost      */~
            PD(14,4),       /* COUNT OF NUMBER OF OCCURRENCES          */~
            PD(14,4),       /* COMPONENT QUANTITY VARIANCE             */~
            PD(14,4),       /* COMPONENT ACTUAL COST VARIANCE          */~
            PD(14,4)        /* COMPONENT STANDARD COST VARIANCE        */

L35950: FMT                 /* FILE: WORK3                             */~
            CH(25),         /* Part code                               */~
            BI(4),          /* Binary Test SEQ #                       */~
            CH(7),          /* Where stuff was MOVED - STEP            */~
            CH(4),          /* Where stuff was moved - WORK CENTER     */~
            CH(4),          /* Where stuff was moved - ACTIVITY CODE   */~
            PD(14,4),       /* Set Up time in WC units                 */~
            PD(14,4),       /* Run Time in Work Center Units           */~
            PD(14,4),       /* The quantity of parts Moved             */~
            PD(14,4),       /* The quantity of parts scrapped          */~
            PD(14,4),       /* The quantity of parts reworked          */~
            PD(14,4),       /* COUNT NUMBER OF TIMES USED              */~
            PD(14,4),       /* Set Up VARIANCE                         */~
            PD(14,4),       /* Run Time VARIANCE                       */~
            PD(14,4)        /* HOURSPERUNIT                            */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub set_pf1
              str(line2$,,50) = "Report Selection Criteria"
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40230,         /* Job From Range    */   ~
                                L40230,         /* Job to Range      */   ~
                                L40230,         /* Part From Range   */   ~
                                L40230,         /* Part to Range     */   ~
                                L40230,         /* Date From Range   */   ~
                                L40230,         /* Date to Range     */   ~
                                L40230,         /* Job Cut-off Range */   ~
                                L40230,         /* Zero Include flag */   ~
                                L40230,         /* JOB SUMMARY FLAG  */   ~
                                L40230          /* Background Flag   */
              goto L40260

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40230:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40260:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Job Statistics Analysis Report",                ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,21), fac(hex(ac)), title$(1)              , ch(25),~
               at (05,47), fac(hex(ac)), title$(2)              , ch(25),~
                                                                         ~
               at (06,02), "Job Number Range",                           ~
               at (06,21), fac(lfac$( 1)), jobfrom$(1%)         , ch(08),~
               at (06,47), fac(lfac$( 1)), jobto$(1%)           , ch(08),~
                                                                         ~
               at (07,02), "Part Number Range",                          ~
               at (07,21), fac(lfac$( 2)), partfrom$(1%)        , ch(25),~
               at (07,47), fac(lfac$( 2)), partto$(1%)          , ch(25),~
                                                                         ~
               at (08,02), "Date Range",                                 ~
               at (08,21), fac(lfac$( 3)), datefrom$            , ch(10),~
               at (08,47), fac(lfac$( 3)), dateto$              , ch(10),~
                                                                         ~
               at (09,02), "Occurrences",                                ~
               at (09,21), fac(lfac$( 4)), jobcount$            , ch(04),~
                                                                         ~
               at (10,02), "Include 0 Complete?",                        ~
               at (10,21), fac(lfac$( 5)), zflag$               , ch(01),~
                                                                         ~
               at (12,02), "Print Job Summary?",                         ~
               at (12,21), fac(lfac$( 6)), jobsum$              , ch(01),~
                                                                         ~
               at (13,02), "Run in Background?",                         ~
               at (13,21), fac(lfac$( 7)), back$                , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40670
                  call "MANUAL" ("JBSTATRP") : goto L40260

L40670:        if keyhit% <> 15 then L40700
                  call "PRNTSCRN" : goto L40260

L40700:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40890     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40850
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40850:     if fieldnr% > 1% then L40870
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40870:     return

L40890: if fieldnr% > 0% then L41010  /*  Edit Mode - Select Fld */
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
L41010:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50300,         /* Job From Range         */~
                              L50400,         /* Part From Range        */~
                              L50500,         /* Date From Range        */~
                              L50820,         /* Job Cut-off Range      */~
                              L50870,         /* Zero complete Flag     */~
                              L50900,         /* JOB SUMMARY REPORT     */~
                              L51000          /* BACKGROUND FLAG        */
            return
L50300: REM Test for Job From Range               JOBFROM$
            call "TESTRNGE" (jobfrom$(1%), jobto$(1%),                   ~
                             jobfrom$(2%), jobto$(2%), errormsg$)
            return

L50400: REM Test for Part From Range              PARTFROM$
            call "TESTRNGE"(partfrom$(1%), partto$(1%),                  ~
                            partfrom$(2%), partto$(2%), errormsg$)
            return

L50500: REM Test for Date From Range              DATEFROM$

            lowdate% = 0% :  highdate% = 99999999%
            if datefrom$ = " " or datefrom$ = blankdate$ then datefrom$ = "ALL"
            if datefrom$ = "ALL" then L50740
                call "DATEOKC" (datefrom$, lowdate%, errormsg$)
                if errormsg$ <> " " then return
                if dateto$ <> " " and dateto$ <> blankdate$ then L50700
                     dateto$ = datefrom$ : highdate% = lowdate%
                     goto L50740
L50700:     call "DATEOKC" (dateto$, highdate%, errormsg$)
            if errormsg$ <> " " then return
L50740:     if lowdate% > highdate% then errormsg$ =                     ~
                "FROM Date must be LESS than or EQUAL to the TO Date"
                return

L50820: REM Test for Part Cut-off Range           JOBCOUNT$
            call "NUMTEST" (jobcount$, 1, 9999, errormsg$, 0.0, 0)
            return

L50870: REM Test for Include Zero Complete        ZFLAG$
            if zflag$ = "Y" or zflag$ = "N" then return
            errormsg$ = "Please enter 'Y' or 'N' to include zero " &     ~
                        "complete jobs"
            return

L50900: REM TEST FOR JOB SUMMARY
            if jobsum$ <> "Y" and jobsum$ <> "N" then errormsg$ =        ~
                "Job Summary Indicator must be Equal to 'Y' or 'N'"
            return

L51000: REM TEST FOR RUN IN BACKGROUND
            if back$ <> "Y" then back$ = "N"
            return

        REM *************************************************************~
            *                 R E P O R T   R O U T I N E S             *~
            *-----------------------------------------------------------*~
            *  READ INFORMATION,  AND PRINT REPORT                      *~
            *************************************************************~

        print_report
            plowkey$ = partfrom$(2)
            call "PLOWALTS" (#6, plowkey$, 0%, 0%, f1%(6))
            goto L57070

        print_part
            call "READNEXT" (#6, f1%(6))
L57070:         if f1%(6) = 0% then return
            get #6, using L35710, part$, partdesc$, partcnt, partqty,     ~
                         date_ae$, partcost_act, partcrdt, partcost_std, ~
                         partqty_var

            call "CONVERT" (partqty, 0.2, print$(1))
            avg_qty = partqty/partcnt
            call "CONVERT" (avg_qty, 0.2, print$(2))
            if partcnt > 1 and partqty_var <> 0 then L57135
                partqty_dev = 0
                goto L57136
L57135:     partqty_dev = sqr(partqty_var)/(partcnt - 1)
L57136:     call "CONVERT" (partqty_dev, 0.4, print$(3))
            call "CONVERT" (partcnt, 0.2, print4$(1))

            gosub print_part_heading
            print using L64310, print$(1), print$(2), print$(3),          ~
                       print4$(1)
            linecnt% = linecnt% - 1%
            print$(), print4$() = " "
            goto print_comp

        print_part_heading
            gosub print_headings
            print
            print using L64280, part$, partdesc$ : print
            linecnt% = linecnt% - 3%
            return

        print_comp
            gosub print_comp_headings
            plowkey$ = part$

        build_comp_ln
            call "PLOWNEXT" (#7, plowkey$, 25%, f1%(7))
                if f1%(7) = 0% then print_comp_totals
            get #7, using L35840, part$, comp$, compqty, compact_cost,    ~
                         compstd_cost, wk_comp_cnt, compqty_var,         ~
                         compact_var, compstd_var

            call "CONVERT" (wk_comp_cnt, 0.2, print4$(1))
            call "CONVERT" (compqty, 2.2, print8$(1))
            avg_qty = compqty/partcnt
            call "CONVERT" (avg_qty, 2.2, print8$(2))
            if partcnt > 1 and compqty_var <> 0 then L58150
                compqty_dev = 0
                goto L58160
L58150:     compqty_dev = sqr(compqty_var)/(partcnt - 1)
L58160:     call "CONVERT" (compqty_dev, 0.4, print$(3))
            call "CONVERT" (compact_cost, 2.2, print$(4))
            avg_act = compact_cost/partcnt
            call "CONVERT" (avg_act, 2.2, print$(5))
            if partcnt > 1 and compact_var <> 0 then L58194
                compact_dev = 0
                goto L58195
L58194:     compact_dev = sqr(compact_var)/(partcnt - 1)
L58195:     call "CONVERT" (compact_dev, 2.2, print$(6))
            call "CONVERT" (compstd_cost, 2.2, print$(7))
            avg_std = compstd_cost/partcnt
            call "CONVERT" (avg_std, 2.2, print$(8))
            if partcnt > 1 and compstd_var <> 0 then L58250
                compstd_dev = 0
                goto L58260
L58250:     compstd_dev = sqr(compstd_var)/(partcnt - 1)
L58260:     call "CONVERT" (compstd_dev, 2.2, print$(9))

            comp_totcnt = comp_totcnt + wk_comp_cnt
            comp_totstd = comp_totstd + compstd_cost
            comp_totact = comp_totact + compact_cost

            if linecnt% > 0% then L58360
                gosub print_part_heading
                gosub print_comp_headings

L58360:     print using L64450, comp$, print4$(1), print8$(1),            ~
                       print8$(2), print$(3), print$(4), print$(5),      ~
                       print$(6), print$(7), print$(8), print$(9)
            linecnt% = linecnt% - 1
            print$(), print4$(), print8$() = " "
            goto build_comp_ln

        print_comp_headings
            print : print using L64340
            print using L64360
            print using L64390
            print using L64420
            linecnt% = linecnt% - 5%
            return

        print_comp_totals
            if comp_totcnt <> 0 then L58580
                print using L64450, "NO COMPONENTS AVAILABLE"
                print using L64420
                linecnt% = linecnt% - 2
                goto print_wc

L58580:     call "CONVERT" (comp_totact, 2.2, print$(1))
            call "CONVERT" (comp_totstd, 2.2, print$(2))
            print using L64420
            print using L64490, print$(1), print$(2)
            linecnt% = linecnt% - 2
            comp_totact, comp_totstd, comp_totcnt = 0
            print$() = " "

        print_wc
            gosub print_wc_headings
            plowkey$ = str(part$) & hex(00000000)

        build_wc_line
            call "PLOWNEXT" (#8, plowkey$, 25%, f1%(8))
                if f1%(8) = 0% then print_wc_seperator
            get #8, using L35950, part$, wc_seq%, step$, wrkctr$,         ~
                         activity$, setup, runtime, wc_moved,            ~
                         wc_scrap, wc_rework, wc_cnt, setup_var,         ~
                         runtime_var

            wc_totcnt = wc_totcnt + wc_cnt
            convert wc_seq% to print8$(1), pic (####)
            call "CONVERT" (wc_cnt, 0.0, print4$(2))
            call "CONVERT" (wc_moved, 0.0, print$(2))
            call "CONVERT" (wc_rework, 0.0, print$(3))
            call "CONVERT" (wc_scrap, 0.0, print$(4))
            call "CONVERT" (setup, 0.2, print6$(1))
            avg_setup = setup/partcnt
            call "CONVERT" (avg_setup, 0.2, print6$(2))
            if partcnt > 1 and setup_var <> 0 then L59180
                setup_dev = 0
                goto L59185
L59180:     setup_dev = sqr(setup_var)/(partcnt - 1)
L59185:     call "CONVERT" (setup_dev, 0.4, print$(5))
            call "CONVERT" (runtime, 0.2, print6$(3))
            avg_runtime = runtime/partcnt
            call "CONVERT" (avg_runtime, 0.2, print6$(4))
            if partcnt > 1 and runtime_var <> 0 then L59220
                runtime_dev = 0
                goto L59225
L59220:     runtime_dev = sqr(runtime_var)/(partcnt - 1)
L59225:     call "CONVERT" (runtime_dev, 0.4, print$(6))

            if linecnt% > 0% then L59255
                gosub print_part_heading
                gosub print_wc_headings

L59255:     print using L64630, step$, wrkctr$, activity$, print8$(1),    ~
                       print4$(2), print$(2), print$(3), print$(4),      ~
                       print6$(1), print6$(2), print$(5), print6$(3),    ~
                       print6$(4), print$(6)
            linecnt% = linecnt% - 1%
            print$(), print4$(), print6$(), print8$() = " "
            goto build_wc_line

        print_wc_headings
            print : print using L64520
            print using L64540
            print using L64570
            print using L64600
            linecnt% = linecnt% - 5
            return

        print_wc_seperator
            if wc_totcnt <> 0 then L59365
                print using L64450, "NO WORK CENTERS AVAILABLE"
                print using L64600
                linecnt% = linecnt% - 2
                goto print_job
L59365:     print using L64600
            linecnt% = linecnt% - 1
            wc_totcnt = 0

        REM ** PRINT JOB SUMMARY FOR THE PART **
        print_job
            if jobsum$ = "N" then goto print_part
            gosub print_job_headings
            plowkey$ = part$

        build_job_print
            call "PLOWNEXT" (#9, plowkey$, 25%, f1%(9))
                if f1%(9) = 0% then goto print_job_totals
            get #9, using L59435, part$, jbcode$
L59435:         FMT CH(25), CH(8)

            readkey$ = jbcode$
            call "READ100" (#1, readkey$, f1%(1))
                if f1%(1) = 0% then goto print_part
            get #1, using L59470, qty_made, date_as$, date_ae$, date_ps$, ~
                         date_pe$, orig_qty, jbqty_scr, jbqty_rew
L59470:     FMT POS(91), PD(14,4), POS(147), CH(6), CH(6), POS(168),     ~
                CH(6), CH(6), POS(180), PD(14,4), POS(212), PD(14,4),    ~
                PD(14,4)

            dtewk$(1) = date_ps$
            dtewk$(2) = date_pe$
            call "DATE" addr("G-", dtewk$(1), dtewk$(2), daydiff%, err%)
            convert daydiff% + 1 to print4$(1), pic (####)
            daydiff% = 0
            dtewk$(1) = date_as$
            dtewk$(2) = date_ae$
            call "DATE" addr("G-", dtewk$(1), dtewk$(2), daydiff%, err%)
            convert daydiff% + 1 to print4$(2), pic (####)
            daydiff% = 0
            call "DATEFMT" (date_ps$)
            call "DATEFMT" (date_pe$)
            call "DATEFMT" (date_as$)
            call "DATEFMT" (date_ae$)
            call "CONVERT" (orig_qty, 0.2, print$(3))
            call "CONVERT" (qty_made, 0.2, print$(4))
            qty_diff = orig_qty - qty_made
            call "CONVERT" (qty_diff, 0.2, print$(5))
            call "CONVERT" (jbqty_rew, 0.2, print$(6))
            call "CONVERT" (jbqty_scr, 0.2, print$(7))

            jobtot_orgqty = jobtot_orgqty + orig_qty
            jobtot_actqty = jobtot_actqty + qty_made
            jobtot_rew = jobtot_rew + jbqty_rew
            jobtot_scr = jobtot_scr + jbqty_scr

            if linecnt% > 0% then L59635
                gosub print_job_headings

L59635:     print using L64780, jbcode$, date_ps$, date_pe$, print4$(1),  ~
                       date_as$, date_ae$, print4$(2), print$(3),        ~
                       print$(4), print$(5), print$(6), print$(7)
            linecnt% = linecnt% - 1%
            print$(), print4$() = " "
            goto build_job_print

        print_job_headings
            gosub print_part_heading : print
            print using L64690
            print using L64720
            print using L64750
            linecnt% = linecnt% - 4%
            return

        print_job_totals
            call "CONVERT" (jobtot_orgqty, 0.2, print$(1))
            call "CONVERT" (jobtot_actqty, 0.2, print$(2))
            qty_diff = jobtot_orgqty - jobtot_actqty
            call "CONVERT" (qty_diff, 0.2, print$(3))
            call "CONVERT" (jobtot_rew, 0.2, print$(4))
            call "CONVERT" (jobtot_scr, 0.2, print$(5))

            print using L64750
            print using L64810, print$(1), print$(2), print$(3),          ~
                       print$(4), print$(5)
            print$() = " "
            jobtot_orgqty, jobtot_actqty, jobtot_rew, jobtot_scr = 0
            goto print_part

        print_end_report
            print : print using L64840
            return

        print_headings
            print page
            pagecnt% = pagecnt% + 1%
            print using L64060, date$, rpt_time$, company$
            print_title$ = "JOB STATISTICS ANALYSIS REPORT"
            call "FMTTITLE" (print_title$, " ", 12%)
            print using L64100, print_title$, pagecnt%
            linecnt% = 55%
            return

        print_parameters
            gosub print_headings
            print skip(3)
            print using L64160 : print
            print using L64190
            print using L64220, "Job Range", jobfrom$(1), jobto$(1)
            print using L64220, "Part Range", partfrom$(1), partto$(1)
            print using L64220, "Date Range", datefrom$, dateto$
            print using L64220, "Occurrences", jobcount$
            print using L64220, "Include 0's", zflag$
            print : print using L64220, "JOB SUMMARY?", jobsum$
            print using L64220, "RUN IN BACKGROUND?", back$
            print : print using L64250
            return

        REM *************************************************************~
            *                  R E P O R T   F O R M A T S              *~
            *-----------------------------------------------------------*~
            *  REPORT FORMAT LINES USED BY PRINT STATEMENTS             *~
            *************************************************************

L64060: % ######## ########                  ############################~
        ~################################                    JBSTATRP:JB00~
        ~07

L64100: %                                    ############################~
        ~################################                        PAGE:####

        %                                    ############################~
        ~################################

L64160: %                                     ----------------- REPORT SE~
        ~LECTION PARAMETERS -----------------

L64190: %                                                           FROM ~
        ~                     TO

L64220: %                                      #################### #####~
        ~#################### #########################

L64250: %                                     ---------------------------~
        ~------------------------------------

L64280: %      PART TO BUILD: #########################    DESC: ########~
        ~######################

L64310: %      TOTAL QTY PRODUCED: ##########    AVG RUN QTY: ########## ~
        ~   STD DEVIATION: ##########    OCCURRENCES: ####

L64340: %     COMPONENT STATISTICS (SUMMARY)

L64360: %                           TIMES    TOTAL   AVERAGE    STD DEV  ~
        ~    ACTUAL   ACT COSTS  ACT COSTS    STANDARD  STD COSTS  STD COS~
        ~TS
L64390: % COMPONENT PARTS            USED  QUANTITY  QUANTITY   QUANTITY ~
        ~     COSTS    AVERAGE    STD DEV       COSTS    AVERAGE    STD DE~
        ~V
L64420: % -------------------------  ----  --------  -------- ---------- ~
        ~ ---------- ---------- ----------  ---------- ---------- --------~
        ~--

L64450:  % #########################  #### ######### ######### ##########~
        ~  ########## ########## ##########  ########## ########## #######~
        ~###

L64490: % TOTALS:                                                        ~
        ~ ##########                        ##########

L64520: %     WORK CENTER STATISTICS (SUMMARY)

L64540: %          WORK                 TIMES      MOVED     REWORKED    ~
        ~SCRAPPED   --------  SETUP  --------   -------- RUNTIME --------

L64570: %   STEP   CNTR  ACTV     SEQ    USED    QUANTITY    QUANTITY    ~
        ~QUANTITY   HOURS    AVG     STD DEV    HOURS    AVG     STD DEV

L64600: % -------  ----  ----    ----    ----  ----------  ----------  --~
        ~--------   ------ ------  ----------   ------ ------  ----------

L64630: % #######  ####  ####    ####    ####  ##########  ##########  ##~
        ~########   ###### ######  ##########   ###### ######  ##########

L64690: %            ---  PLANNED  ---          ---   ACTUAL  ---        ~
        ~  ORIGINAL     ACTUAL                  TOTAL       TOTAL

L64720: % JOB NBR      START    END     DAYS      START    END     DAYS  ~
        ~  QUANTITY    QUANTITY     DIFF       REWORKED    SCRAPPED

L64750: % --------   -------- --------  ----    -------- --------  ----  ~
        ~----------  ----------  ----------  ----------  ----------

L64780: % ########   ######## ########  ####    ######## ########  ####  ~
        ~##########  ##########  ##########  ##########  ##########

L64810: % TOTALS:                                                        ~
        ~##########  ##########  ##########  ##########  ##########

L64840: %                               * * * *  E N D   O F   R E P O R ~
        ~T  * * * *

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
