        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  JJJJJ  BBBB   DDDD    SSS   PPPP   RRRR   PPPP   TTTTT   *~
            *    J    B   B  D   D  S      P   P  R   R  P   P    T     *~
            *    J    BBBB   D   D   SSS   PPPP   RRRR   PPPP     T     *~
            *  J J    B   B  D   D      S  P      R   R  P        T     *~
            *   J     BBBB   DDDD    SSS   P      R   R  P        T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBDSPRPT - Print a prioritized list of jobs scheduled to  *~
            *            be in and/or reported to be in each workcenter *~
            *            of a specified range of workcenters for a      *~
            *            specified range of dates. For a given work-    *~
            *            center, the report shows 3 lists of jobs -     *~
            *            those in the workcenter that have partial      *~
            *            completions in jbstatus, those that have been  *~
            *            reported into the workcenter, and finally,     *~
            *            those that are still upstream.  The lists are  *~
            *            sorted by critical ratio with most critical    *~
            *            first.  The method used to calculate C/R is    *~
            *            selected by the user.  The more critical the   *~
            *            job, the smaller the C/R value,ie., 1.5 is more*~
            *            critical than 3.7, etc. Generally, a negative  *~
            *            C/R means the scheduled completion date for    *~
            *            the job is in the past, by that many days.     *~
            *            The critical ratio calculation formulas are not*~
            *            traditional 'APICS' because this is a finite   *~
            *            capacity system.  See line 34000 for details.  *~
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
            * 09/27/87 ! Original                                 ! WPH *~
            * 05/05/88 ! Added option to print part & job descrtns! HES *~
            * 01/31/89 ! Corrected divide by 0 at ln 35086        ! MJB *~
            *          ! Corrected W/C code on 'All Reported'     !     *~
            *          !   print line                             !     *~
            *          ! Fixed Range print on page 0              !     *~
            * 04/04/89 ! Fixed rounding errors (PRR 10148)        ! RJM *~
            * 05/16/90 ! PRR 10633  Added  multiple methods for   ! WPH *~
            *          ! prioritization.  A = pure PIPIN due date !     *~
            *          ! order, B = Weighted Critical ratio as    !     *~
            *          ! described in the remarks in this program,!     *~
            *          ! C = same as B except calculates total    !     *~
            *          ! available time from today until job end  !     *~
            *          ! instead of from job start until job end. !     *~
            *          ! Also added option to print demand info.  !     *~
            * 05/05/93 ! Added new section to show active jobs in ! WPH *~
            *          ! the workcenter (active means some qty    !     *~
            *          ! reported out) and sort/select by group.  !     *~
            * 07/07/93 ! Rewrite of workfile building section to  ! WPH *~
            *          ! include unplanned jobs (via JBSTATUS)    !     *~
            *          ! if prioritizing by due date.  Also, now  !     *~
            *          ! does not show jobs for a given workcenter!     *~
            *          ! if step is flagged as complete in that   !     *~
            *          ! workcenter in JBSTATUS (via JBACTSUB).   !     *~
            * 07/20/95 ! Chanl #3 now open with OPENCHCK arguments! RJH *~
            * 08/15/95 ! PRR 13489. SCFLAG$ no longer overridden  ! JDH *~
            *          !  with 'N' prior to write to workfile.    !     *~
            * 08/12/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            activity$4,                  /* Activity/Operation Code    */~
            ajbflag$1,                   /* Show Advices/Jobs or Both? */~
            avail%(490),                 /* Capacity Available         */~
            begwc$4,                     /* WORK BEG WORKCENTER        */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            carryover$1,                 /* Flag set if Job carries ovr*/~
            calcmethod$1,                /* C/R Calculation methods    */~
            company$60,                  /* Company name for report    */~
            cursor%(2),                  /* Cursor location for edit   */~
            currentwccode$4,             /*                            */~
            critratio$10,                /*                            */~
            currentstep$7,               /*                            */~
            currenthead$1,               /* Sub-Header printed last    */~
            currentwcstep$11,            /*                            */~
            cuscode$9,                   /* Customer Code              */~
            date$8,                      /* Date for screen display    */~
            dateout$6,                   /* Date of WCOUT              */~
            dem$19,                      /* Demand code/line           */~
            demtype$1,                   /* Demand type                */~
            dempriority$1,               /* Demand priority            */~
            descrprt$1,                  /* Print Descriptions Flag    */~
            edtmessage$79,               /* Edit screen message        */~
            endwc$4,                     /* WORK ENDING WC             */~
            errormsg$79,                 /* Error message              */~
            firstwc$4,                   /* Range of Workcenters       */~
            firstgp$3,                   /* Range of Workcenter Groups */~
            firstwcout$8,                /* Date of 1st WCOUT for Step */~
            flag$1,                      /* Work variable              */~
            group$(2)3,                  /* Workcenter group           */~
            header1$8,                   /* Header line for FROM       */~
            header2$8,                   /* Header line for TO         */~
            i$(24)80,                    /* Screen Image               */~
            includemq$1,                 /* Include M/Q in Crit. Ratio?*/~
            inact$4,                     /* In Activity                */~
            inpmessage$79,               /* Informational Message      */~
            inwc$4,                      /* In Workcenter              */~
            instep$7,                    /*                            */~
            job$8,                       /* Job number                 */~
            jobdescr$30,                 /* Job Description            */~
            jobnumber$8,                 /* Job number                 */~
            ldate$8,                     /* End of Dates Range         */~
            lastdate$8,                  /* End of Dates Range         */~
            lastwcout$8,                 /* Date of Last WCOUT for Step*/~
            lastwc$4,                    /* End of  Workcenter Range   */~
            lastgp$3,                    /* End of WC Group range      */~
            late$1,                      /* Flag set if Job is Late    */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            nextwc$4,                    /* Next Workcenter            */~
            part$25,                     /* Part Number of part built  */~
            partdescr$32,                /* Part Number Description    */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            planend$8,                   /* Job planned end date       */~
            pldate$8,                    /* Planning base date         */~
            planstart$8,                 /* Job planned start date     */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            plowkey2$99,                 /* Miscellaneous Read/Plow Key*/~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            reportedin$1,                /* Flag if Job reported to WC */~
            scflag$1,                    /* Step Complete flag (Y/N)   */~
            secondline$132,              /* Shows desription & dem info*/~
            sort_by_gp$1,                /* Sort by WC group flag (Y/N)*/~
            start$6,                     /* Start Date in YYMMDD       */~
            step$7,                      /* Job Step Number            */~
            today$6,                     /* Todays date in YYMMDD      */~
            towc$4,                      /* Destination Workcenter     */~
            tag$19,                      /* Tag Number                 */~
            tagstep$26,                  /* Tag Nbr. Step concatination*/~
            uh$1,                        /* Units or Hours Flag        */~
            units$5,                     /* Screen Litteral Units/Hours*/~
            used%(490),                  /* Capacity used              */~
            userid$3,                    /* Current User Id            */~
            wc$4,                        /* Workcenter Code            */~
            wcstep$11,                   /* Workcenter Code & Step no. */~
            wccode$4,                    /* Workcenter Code            */~
            wcdescr$30                   /* Workcenter Description     */

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
            * # 1 ! WCOUT    ! Planned work center use detail           *~
            * # 2 ! WCMASTR  ! Workcenter Master File                   *~
            * # 3 ! JBSTATUS ! Production job actual structure (RTE) ac *~
            * # 4 ! JBMASTR2 ! Production job master file               *~
            * # 5 ! WORK1    ! Temporary System Workfile                *~
            * # 6 ! SYSFILE2 ! System Misc. File                        *~
            * # 7 ! PIPIN    ! Planned Inventory Additions File         *~
            * # 8 ! WORK2    ! Temporary System Workfile                *~
            * # 9 ! HNYMASTR ! Inventory Master File                    *~
            * #10 ! PIPCROSS ! Pegging File                             *~
            * #11 ! DEMMASTR ! Demand Master File                       *~
            * #12 ! GENCODES ! General Purpose Codes Validation File    *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 1, "WCOUT",                                         ~
                        varc,     indexed,  recsize =   68,              ~
                        keypos =    9, keylen =  23,                     ~
                        alt key  1, keypos =    1, keylen =  27          ~

            select # 2, "WCMASTR",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    2, keylen =   5,                     ~
                        alt key  1, keypos =    1, keylen =   6          ~

            select # 3, "JBSTATUS",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    1, keylen =  12,                     ~
                        alt key  1, keypos =   21, keylen =  44,         ~
                            key  2, keypos =   29, keylen =  36          ~

            select # 4, "JBMASTR2",                                      ~
                        varc,     indexed,  recsize =  1300,             ~
                        keypos =    1, keylen =   8                      ~

            select # 5, "WORK1",                                         ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos = 25,   keylen =   30,                    ~
                        alt key  1, keypos =   1, keylen =  54

            select # 6, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize = 500,               ~
                        keypos =    1, keylen =  20                      ~

            select # 7, "PIPIN",                                         ~
                        varc,     indexed,  recsize = 60,                ~
                        keypos = 30,   keylen =  19,                     ~
                        alt key  1, keypos =   1 , keylen =  48          ~

            select # 8, "WORK2",                                         ~
                        varc,     indexed,  recsize =   50,              ~
                        keypos = 1 ,   keylen =   23

            select # 9, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =  1,   keylen =  25

            select #10, "PIPCROSS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos =    1, keylen =  71,                     ~
                        alt key  1, keypos =   20, keylen =  52,         ~
                            key  2, keypos =   39, keylen =  33          ~


            select #11, "DEMMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  123,                                  ~
                        keypos =    2, keylen =  27,                     ~
                        alt key  1, keypos =   10, keylen =  19,         ~
                            key  2, keypos =    1, keylen =  28,         ~
                            key  3, keypos =   29, keylen =  25, dup

            select #12,  "GENCODES",                                     ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (# 1, fs%( 1), f2%( 1), 0%, rslt$( 1))
            call "OPENCHCK" (# 2, fs%( 2), f2%( 2), 0%, rslt$( 2))
            call "OPENCHCK" (# 3, fs%( 3%), f2%( 3%), 0%, rslt$( 3%))
            call "OPENCHCK" (# 4, fs%( 4), f2%( 4), 0%, rslt$( 4))
            /* we open the workfile when needed */
            call "OPENCHCK" (# 6, fs%( 6), f2%( 6), 0%, rslt$( 6))
            call "OPENCHCK" (# 7, fs%( 7), f2%( 7), 0%, rslt$( 7))
            /* we open the workfile when needed */
            call "OPENCHCK" (# 9, fs%( 9), f2%( 9), 0%, rslt$( 9))
            call "OPENCHCK" (#10, fs%(10), f2%(10), 0%, rslt$(10))
            call "OPENCHCK" (#11, fs%(11), f2%(11), 0%, rslt$(11))
            call "OPENCHCK" (#12, fs%(12), f2%(12), 0%, rslt$(12))


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            mat used% = zer
            mat avail% = zer

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            today$ = date$
            call "DATEFMT" (date$)

            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62) = "JBDSPRPT: " & str(cms2v$,,8)
            str(header1$) = "From    "
            str(header2$) = "To      "


        REM  Get the plan base date

            call "READ100" (#6, "MONTHS OPEN", f1%(6))
            if f1%(6) = 0 then L51036
            get #6, using L09270 , pldate$
L09270:        FMT XX(32), CH(6)
            call "DATE" addr("G-", pldate$, today$, today%, err%)
            if err% <> 0 then L51036
            today% = today%+1%

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub L28000

            for fieldnr% = 1% to  11%
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0 then L10220
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10200
L10150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10120
                         if fieldnr% = 1% then L10100
                         goto L10150
L10200:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <>  0% then       L10120
L10220:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10120
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       editpg1
            if cursor%(1%) < 6% or cursor%(1%) > 15% then editpg1
            if cursor%(1%) = 8% then editpg1
            if cursor%(1%) <> 6% then  L11143
            if cursor%(2%) < 40% then fieldnr% = 1% else fieldnr% = 2%
                  goto L11190
L11143:     if cursor%(1%) <> 7% then  L11150
            if cursor%(2%) < 40% then fieldnr% = 3% else fieldnr% = 4%
                  goto L11190
L11150:     fieldnr% = cursor%(1%) - 4%

L11190:     if fieldnr% = lastfieldnr% then editpg1
L11200:     gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       editpg1
L11220:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11220
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11220
                  if fieldnr% <> 1% then L11264  /* force edit of the    */
                     fieldnr% = 2%              /* 'from' field too.    */
                     goto L11200
L11264:           if fieldnr% <> 3% then L11270  /* ditto                */
                     fieldnr% = 4%
                     goto L11200
L11270:           lastfieldnr% = fieldnr%
            goto L11190

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub build_the_files
            gosub calc_critical_ratio
            gosub print_the_report

            if f2%(5%) <> 0% then L19160
            call"FILEBGON" (#5)
            call"FILEBGON" (#8)
              f2%(5%) =  1%
              f2%(8%) =  1%
L19160:     goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20160,         /* First Group        */    ~
                              L20170,         /* Last  Group        */    ~
                              L20180,         /* First Workcenter   */    ~
                              L20230,         /* Last  Workcenter   */    ~
                              L20290,         /* Last  Date         */    ~
                              L20340,         /* Include M/Q (Y/N)  */    ~
                              L20380,         /* Advices/Jobs/Both  */    ~
                              L20420,         /* Units or Hours     */    ~
                              L20460,         /* Print Descriptions */    ~
                              L20500,         /* Calculation Method */    ~
                              L20540          /* Sort by Group Y/N  */
            return


L20160: REM Def/Enable First Workcenter Group   FIRSTGP$
            if firstgp$ <> " " then return
            firstgp$ = "ALL"
            lastgp$ = " "
            return

L20170: REM Def/Enable Last Workcenter Group   LASTGP$
            if firstgp$ <> "ALL"  then  L20174
            lastgp$ = " "
            enabled% = 0% : return
L20174:     if lastgp$ = " " and firstgp$ <> "ALL" then lastgp$ = firstgp$
            return

L20180: REM Def/Enable First Workcenter         FIRSTWC$
            if firstwc$ <> " " then return
            firstwc$ = "ALL"
            lastwc$ = " "
            return

L20230: REM Def/Enable Last Workcenters        LASTWC$
            if firstwc$ <> "ALL"  then  L20260
            lastwc$ = " "
            enabled% = 0% : return
L20260:     if lastwc$ = " " and firstwc$ <> "ALL" then lastwc$ = firstwc$
            return

L20290: REM Def/Enable Last Date of Range         LASTDATE$
            if lastdate$ <> " " and lastdate$ <> blankdate$ then L20320
            call "DATE" addr("G+", today$, 7%, lastdate$, err%)
            call "DATEFMT" (lastdate$)
L20320:     return

L20340: REM Def/Enable Include M/Q in Crit. Ratio? INCLUDEMQ$
            includemq$ = "Y"
            return

L20380: REM Def/Enable Show Advices/Jobs or Both?  AJBFLAG$
            ajbflag$ = "B"
            return

L20420: REM Def/Enable Production Units or Hours  UH$
            uh$ = "H"
            return

L20460: REM Def/Enable Print Job And Part Descr   DESCRPRT$
            descrprt$ = "Y"
            return

L20500: REM Critical Ratio Calculation Method     CALCMETHOD$
            calcmethod$ = "A"
            return

L20540: REM Sort by Workcenter Group              SORT_BY_GP$
            sort_by_gp$ = "N"
            if firstgp$ <> "ALL" then sort_by_gp$ = "Y"
            return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L27110
                inpmessage$ = edtmessage$
                return

L27110
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter the First Workcenter Group to Report or 'ALL'          ",~
         "Enter the Last Workcenter Group to Report or 'ALL'           ",~
         "Enter the First Workcenter to Report or 'ALL'                ",~
         "Enter the Last Workcenter to include or '?' to see the list  ",~
         "The report will include scheduled activity up until this date",~
         "Should Move/Queue time be included in C/R calculation? Y or N",~
         "Enter A to include Advices, J to include Jobs, or B for both ",~
         "Enter U to show capacity Units or H to show capacity in Hours",~
         "Enter Y to show Job & Part Descr, D to show Demand Info, B = Bo~
        ~th, N = Neither",                                                ~
         "Prioritize by A = Due Date, B = C/R (Job Life), C = C/R (Today ~
        ~to Job End)",                                                    ~
         "Enter Y to sort the report by workcenters within workcenter gro~
        ~ups."


L28000: REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

            init(" ") errormsg$, inpmessage$, ajbflag$, firstwc$,        ~
                      includemq$, lastdate$, lastwc$, uh$, descrprt$,    ~
                      calcmethod$, firstgp$, lastgp$, sort_by_gp$


            return

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
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************~


        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all

            call"FILEBGON" (#5)
            call"FILEBGON" (#8)
            goto inputmode

        REM *************************************************************~
            *         B U I L D   T H E   W O R K F I L E S             *~
            *-----------------------------------------------------------*~
            * We build the workfiles in two steps, first pass the       *~
            * JBSTATUS file and gather information in the workfile      *~
            * for all jobs already reported into the selected           *~
            * workcenters.                                              *~
            *                                                           *~
            * Secondly, we plow the WCOUT file to get info on all those *~
            * jobs scheduled to be in the workcenters.  The primary     *~
            * workfile is #5 and contains one summary record for each   *~
            * Job/Step in the workcenter during the selected date range.*~
            *                                                           *~
            * If a job hasn't been reported into the workcenter yet,    *~
            * the workfile record doesn't get created until the WCOUT   *~
            * phase.                                                    *~
            *                                                           *~
            * If the user has chosen to calculate the critical ratio    *~
            * based on work remaining (and not just by job due date) we *~
            * calculate a look-ahead factor for use in the C/R          *~
            * calculation which is stored in the second work file (#8). *~
            * When both files are built, we then re-process this data   *~
            * to calculate the C/R and then finally read the records    *~
            * from the main workfile according to the C/R key to print  *~
            * the report.  See further discussion of C/R below.         *~
            *************************************************************

        build_the_files

            call "SHOSTAT" ("Building the Workfiles")

            init(" ") late$, jobnumber$, step$, activity$, part$, job$,  ~
                      carryover$, reportedin$, firstwcout$, lastwcout$,  ~
                      wccode$ , critratio$, group$(), scflag$, closed$,  ~
                      dateout$

            jobqty, critratio = 0
            hits%, wcouts%, totmq%, totsetup%, totrun%, firsthit%  = 0%

            ldate$ = lastdate$
            call "DATUNFMT" (ldate$)
            begwc$ = firstwc$
            endwc$ = lastwc$
            if firstwc$ <> "ALL" then L30240
                init(hex(00)) firstwc$
                init(hex(ff)) lastwc$

        REM *************************************************************~
            *       P R O C E S S  T H E  J B S T A T U S  F I L E      *~
            *-----------------------------------------------------------*~
            * We pass the entire JBSTATUS File.  Each time we find      *~
            * activity reported for a workcenter in the selected range  *~
            * we record it in the primary workfile.  Note that a given  *~
            * JBSTATUS record can yield info about a quantity moved into*~
            * one workcenter as well as info about time spent in the    *~
            * previous workcenter so we can potentially update two      *~
            * workfile records for each JBSTATUS record.                *~
            *************************************************************

*        If reporting on just advices, there won't be JBSTATUS records
L30240:     if ajbflag$ = "A" then process_wcout_file

*        Pass the JBSTATUS only if prioritizing based on due date
            if calcmethod$ <> "A" then process_wcout_file

            init (hex(00)) plowkey$
            call "PLOWALTS" (#3, plowkey$, 0%, 0%, f1%(3))
            goto L30295

        read_jbstatus
            call "READNEXT" (#3, f1%(3))
L30295:        if f1%(3) = 0% then process_wcout_file

            if str(key(#3),1%,8%) <> job$ then L30325
                  if closed$ = " " then get_jbstatus_record
                  goto read_jbstatus

L30325:     call "READ100" (#4, key(#3), f1%(4%))
                if f1%(4%) = 0% then read_jbstatus
            get #4, using L30340, closed$
L30340:         FMT  POS(153), CH(6)

            if closed$ <> " " and closed$ <> blankdate$ then read_jbstatus

        get_jbstatus_record

            get #3 using L30390, job$, towc$, toact$, fromwc$, fromact$,  ~
                                fromstep$, tostep$, setup%, run%,        ~
                                toqty, scrap, rwk, scflag$

L30390:     FMT         CH(8), POS(13), CH(4), CH(4), POS(29), CH(4),    ~
                        POS(47), CH(4), POS(65), CH(7), CH(7), POS(91),  ~
                        BI(4), BI(4), PD(14,4), PD(14,4), PD(14,4),      ~
                        POS(158), CH(1)

            if scflag$ <> "Y" then scflag$ = "N"
            init(" ") tag$
            str(tag$,1%,11%) = "JOB ORDER: "
            str(tag$,12%,8%) = str(job$,,)

        REM *************************************************************~
            *   Process the tag/step for the 'to' workcenter.  Create   *~
            *   the workfile record if it doesn't exist.  If it does    *~
            *   exist, then update the quantity moved into that         *~
            *   workcenter.   Since all 'from' workcenters start out as *~
            *   'to' workcenters, this is where we create workfile      *~
            *   records, which later get updated in 'from' processing   *~
            *************************************************************
            if towc$ = " " then process_for_from_wc
            wccode$ = towc$
            gosub get_group    /* find what group this WC belongs to */
               if f1%(2%) = 0% then read_jbstatus
            gosub range_test
               if inrange% = 0% then process_for_from_wc
            step$ = tostep$
            activity$ = toact$
            nextwc$ = " "
            if f2%(5%) <> 0% then goto write_new_record /* first time */

            gosub read_workfile
               if f1%(5%) = 0% then write_new_record
               gosub update_to_record

        REM *************************************************************~
            *  Here we process the source workcenters by tallying       *~
            *  the quantity moved out of them and the time spent doing  *~
            *  work in them.                                            *~
            *************************************************************~

        process_for_from_wc    /* process the tagstep for 'from' wc */
            if fromwc$ = " " then read_jbstatus
            wccode$ = fromwc$
            gosub get_group    /* find what group this WC belongs to */
               if f1%(2%) = 0% then read_jbstatus
            gosub range_test
               if inrange% = 0% then read_jbstatus
            step$ = fromstep$
            activity$ = fromact$
            gosub read_workfile
               if f1%(5%) = 0% then read_jbstatus  /* shouldn't happen */
               gosub update_from_record
            goto read_jbstatus

        range_test         /* exits with inrange% = 1% if in range */
            inrange% = 0%
            if wccode$ < firstwc$ then return
            if wccode$ > lastwc$ then return
            if firstgp$ <> "ALL" then L30635
               inrange% = 1%
               return

L30635:     if group$(2%) < firstgp$ then return
            if group$(2%) > lastgp$ then return
               inrange% = 1%
               return

        read_workfile
            init(hex(00)) readkey$
            str(readkey$, 1, 4) = str(wccode$, 1,4)
            str(readkey$, 5,19) = str(tag$, 1,19)
            str(readkey$, 24, 7) = str(step$, 1, 7)

            call "READ101" (#5, readkey$,  f1%(5))
            return

        write_new_record
            gosub load_job_data
            toutsetup%  =  0%
            toutrun%    =  0%
            toutqty     =  0
            toutscrap   =  0
            toutrework  =  0
            reportedin$ = "Y"
            firstwcout$ = " "
            lastwcout$  = " "
            setup%  = 0%
            run%    = 0%
            wcouts% = 0%
            inwc$   = " "
            instep$ = " "
            inact$  = " "
            carryover$ = "N"

            gosub write_wf1
            goto process_for_from_wc

        update_to_record

            get #5, using L30815, qty
L30815:        FMT   POS(182), PD(14,4)
            qty = qty + toqty

            put #5, using L30815, qty
            rewrite #5
            return

        update_from_record

            get #5, using L30875, reportedin$, flag$,toutsetup%, toutrun%,~
                    toutqty, toutscrap, toutrework, nextwc$

L30875:        FMT POS(8), CH(1), POS(145), CH(1), BI(4),BI(4),          ~
                                   3*PD(14,4), POS(178), CH(4)

            if flag$ = "N" and scflag$ = "Y" then flag$ = "Y"
            nextwc$ = towc$
            toutsetup%  =  toutsetup%  + setup%
            toutrun%    =  toutrun%    + run%
            toutqty     =  toutqty     + toqty
            toutscrap   =  toutscrap   + scrap
            toutrework  =  toutrework  + rwk
            reportedin$ = "X"

            put #5, using L30875, reportedin$, flag$,toutsetup%, toutrun%,~
                    toutqty, toutscrap, toutrework, nextwc$
            rewrite #5
            return

        REM *************************************************************~
            *         P R O C E S S  T H E  W C O U T  F I L E          *~
            *-----------------------------------------------------------*~
            * This is the second phase of workfile building where we    *~
            * search the wcout file and update the workfile records     *~
            * for jobs planned to be in the selected workcenters.       *~
            *************************************************************

        process_wcout_file
            ret% = 0%
            call "PIPINDEX" (#6, ldate$, ldate%, ret%)
            init(hex(00)) plowkey$
            str(plowkey$, 1, 4) = str(firstwc$,1,4)
            goto next_wcout

        next_workcenter
            init(hex(ff)) str(plowkey$, 5) /* set plow to catch next wc */

        next_wcout

            mq% = 0%
            tagstep$ = " "
            call "PLOWALTS" (#1, plowkey$, 1%, 0%, f1%(1))
              if f1%(1) = 0% then return
            get  #1, using L31180,  wccode$, dateout%, tag$,              ~
                                   setup%, run%, step$, activity$

L31180:         FMT  CH(4), BI(2), XX(2), CH(19), XX(4), BI(4), BI(4),   ~
                                   CH(4), XX(4), CH(4)

            if  wccode$ > lastwc$ then return

            gosub get_group
               if f1%(2%) = 0% then next_workcenter

            if firstgp$ = "ALL" then L31320
            if group$(2%) < firstgp$ then next_workcenter
            if group$(2%) > lastgp$ then next_workcenter

L31320:     call "DATE" addr("G+", pldate$, dateout% - 1%, dateout$, err%)
            if dateout$ >  ldate$ then next_workcenter

            if str(tag$, 1, 2) <> "JO" then L31380
              if ajbflag$ = "A"  then  next_wcout
              goto  got_a_qualified_wcout
L31380:     if ajbflag$ = "J"  then  next_wcout


        REM *************************************************************~
            * We get to here with a qualified WCOUT record.  We check   *~
            * to see if we have encountered this tag/step before in     *~
            * our pass of JBSTATUS or as a WCOUT.  We update the        *~
            * workfile accordingly.                                     *~
            *************************************************************

        got_a_qualified_wcout

            hits% = hits% + 1%

            if f2%(5%) <> 0% then new_tagstep /* branch for first time */

            init(hex(00)) readkey$
            str(readkey$, 1, 4) = str(wccode$, 1,4)
            str(readkey$, 5,19) = str(tag$, 1,19)
            str(readkey$, 24, 7) = str(step$, 1, 7)

            call "READ101" (#5, readkey$,  f1%(5))
                 if f1%(5) =  0% then new_tagstep

        REM *************************************************************~
            * This tagstep has more than one WCOUT record so we update   ~
            * the record.                                                ~
            *************************************************************


            get #5, using L31850, firstwcout$, totmq%, totsetup%, totrun%,~
                                 lastwcout$, wcouts%

L31850:         FMT  POS(55), CH(6), POS(98), BI(4), BI(4), BI(4),       ~
                               POS(111), CH(6), BI(4)

            if wcouts% = 0% then firstwcout$ = dateout$
            wcouts% = wcouts% + 1%

            if setup% + run% <> 0% then L31930 /* 0 = a move/queue day */
                totmq% =  totmq% + 1%
                goto L31950

L31930:     totsetup% =  totsetup% + setup%
            totrun% =    totrun% +   run%
L31950:     lastwcout$ = dateout$

            put #5, using L31850, firstwcout$, totmq%, totsetup%, totrun%,~
                                  lastwcout$, wcouts%

            rewrite #5

            goto next_wcout

        new_tagstep

*       Get job or PIPIN data since it's first hit on a step/WC
            part$, planend$, inwc$, instep$  = " "
            wcouts% = 1%
            firsthit% = 1%
            firstwcout$ = dateout$
            reportedin$ = "Z"
            if str(tag$,,2) <> "WO" then L32210
               gosub load_pipin_data
               goto L32370

L32210:     gosub load_job_data
              if f1%(4) = 0% then next_wcout  /* what?, no job master? */
              if jobqty <= 0 then next_wcout  /* Shouldn't happen */

*       We create 2nd workfile record since its our first hit Job/WC
L32370:     if calcmethod$ = "A" then determine_step_status
            if f2%(8%) = 0% then goto L32400
            call "WORKOPEN" (#8, "IO", 3000%, f2%(8))
L32400:     str(readkey$) = all(hex(00))
            str(readkey$,1,19) = str(tag$)
            str(readkey$,20,4) = str(wccode$)
            call "READ100" (#8, readkey$, f1%(8%))
                if f1%(8%) <> 0% then determine_step_status
                wc$ = wccode$

            gosub find_and_write_the_factor

        determine_step_status  /* used when processing WCOUT file */
            if str(tag$,,2) = "WO" then write_workfile

            REM Let's see if the job has been reported into the WC yet...
            init(hex(00)) readkey$
            str(readkey$, 1, 8) = str(jobnumber$)

L32610:     call "PLOWNEXT" (#3, readkey$, 8%, f1%(3))
              if f1%(3) = 0% then write_workfile /* not reported in yet */
            get #3, using L32640, inwc$, inact$, instep$
L32640:       FMT  XX(12), CH(4), CH(4), XX(51), CH(7)

            if inwc$ <> wccode$ then L32610   /* wrong record, try again */
            if instep$ <> step$ then L32610   /* wrong record, try again */

            reportedin$ = "Y"       /* We got a hit so it has  */

*        Now lets see if it's active, ie., qty reported from step
            toutsetup%, toutrun% = 0%
            toutqty, toutscrap, toutrework = 0
            init(hex(00)) readkey$
            str(readkey$, 1%,8%) = str(jobnumber$,,)
            str(readkey$, 9%,4%) = str(wccode$,,)
L32770:     call "PLOWALTS" (#3, readkey$, 1%, 12%, f1%(3%))
              if f1%(3%) = 0% then write_workfile /* no 'from' record */
            get #3, using L32810,  towc$, instep$, outsetup%, outrun%,    ~
                               outqty, outscrap, outrework, scflag$
L32810:       FMT  POS(13), CH(4), POS(65), CH(7), POS(91), BI(4), BI(4),~
                           PD(14,4), PD(14,4), PD(14,4), POS(158), CH(1)

            if scflag$ <> "Y" then scflag$ = "N"

            if instep$ <> step$ then write_workfile /* instep$ is 'from'*/
            reportedin$ = "X"       /* We got a hit so it's active */

            REM Now plow through JBSTATUS and tally the capacity and qtys
            toutsetup% = toutsetup% + outsetup%
            toutrun% = toutrun% + outrun%

            toutqty = toutqty + outqty
            toutscrap = toutscrap + outscrap
            toutrework = toutrework + outrework
            goto L32770

        write_workfile  /*  Write the main workfile  */
            totsetup%, totrun%, totmq%  = 0%
            wcouts% = 1%
            if setup% + run% = 0% then mq% = 1%
            firstwcout$ = dateout$
            lastwcout$ = dateout$
            carryover$ = "N"
            nextwc$ = towc$
            firsthit% = 2%  /* until reset to 1 we will be doing mults */
            gosub write_wf1
            goto next_wcout

         find_and_write_the_factor
            start$ = today$
            if planstart$ > today$ then start$ = planstart$
            call "PIPINDEX" (#6, start$, jobstart%, ret%)
            call "PIPINDEX" (#6, planend$, jobend%, ret%)
            totused% , totavail%, gtavail% = 0%
            call "READ100" (#2, wc$, f1%(2))
                if f1%(2) = 0% then return
                get #2, using  L33400, avail%(), used%(), unitsperday%
L33400:             FMT POS(60), 490 * BI(2), 490 * BI(2), BI(2)

            unitsperday = unitsperday%
            hoursperunit = 24/unitsperday

            for i%  = jobstart% to jobend%
                totavail% = totavail% + avail%(i%)
                totused% = totused% + used%(i%)
            next i%

*        Now if they want grand total available from today...
            if calcmethod$ <> "C" then L33580
                for i%  = today% to jobend%
                    gtavail% = gtavail% + avail%(i%)
               next i%
            gtavail = gtavail%
            gtavail = gtavail * hoursperunit

L33580:     totavail = totavail%
            totused  = totused%
            totused = totused * hoursperunit
            totavail = totavail * hoursperunit
            if totavail = 0  then L33670
            factor = totused/totavail
            if calcmethod$ = "C" then totavail = gtavail
            write #8, using L33660, tag$, wc$, factor,totavail, totused
L33660:       FMT CH(19), CH(4), PD(14,4), PD(14,4), PD(14,4)
L33670:     return

        REM *************************************************************~
            *        C O M M O N   S U B R O U T I N E S                *~
            *-----------------------------------------------------------*~
            *   Subroutines used by both the Workfile building sections *~
            *************************************************************

        write_wf1    /* Set-up, then write a primary workfile record */
            if f2%(5%) = 0% then goto L33685
            call "WORKOPEN" (#5, "IO", 4000%, f2%(5)) /* set back IO*/

L33685:     call "DATE" addr("G-",today$, planend$, daysout%, err%)
            critratio = daysout%
            if critratio < 0 then critratio = critratio - 1%
            critratio = critratio + 1000000
            call "CONVERT" (critratio, 2.2, critratio$)
            group$(1%) = hex(00)
            if sort_by_gp$ = "Y" then group$(1%) = group$(2%)

            write  #5, using L33706 , group$(1%), wccode$, reportedin$,   ~
                     critratio$, planstart$, wccode$, tag$, step$,       ~
                     firstwcout$, activity$, part$, jobqty,   mq%,       ~
                     setup%, run%, carryover$, lastwcout$, wcouts%,      ~
                     planend$, inwc$, instep$, inact$, group$(2%),       ~
                     "N", toutsetup%, toutrun%, toutqty, toutscrap,      ~
                     toutrework, nextwc$, toqty

L33706:      FMT CH(3), CH(4), CH(1), CH(10), CH(6),CH(4), CH(19), CH(7),~
               CH(6),CH(4), CH(25), PD(14,4), BI(4), BI(4), BI(4), CH(1),~
               CH(6), BI(4), CH(6), CH(4), CH(7), CH(4), CH(3),          ~
               CH(1), BI(4), BI(4), PD(14,4), PD(14,4), PD(14,4), CH(4), ~
               PD(14,4)

            return

        load_pipin_data   /* For advices, read PIPIN file for basic info*/
            call "READ100" (#7, tag$, f1%(7))
              if f1%(7) = 0% then next_wcout /* what?, no PIPIN? */
            get #7, using L33770,  part$, datein%, jobqty, start%
L33770:       FMT  CH(25), BI(4), XX(19), PD(14,4), BI(4)
            call "DATE" addr("G+",pldate$,datein%,planend$,err%)
            call "DATE" addr("G+",pldate$,start%,planstart$,err%)
            return

        load_job_data       /* Read job data out of the JBMASTR2 File */
            jobnumber$ =  str(tag$, 12, 8)
            call "READ100" (#4, jobnumber$, f1%(4))
              if f1%(4) = 0% then return
            get #4, using L33870,  part$, jobqty, planstart$, planend$
L33870:     FMT  XX(57), CH(25), PD(14,4), XX(77), CH(6), CH(6)
            return

        get_group    /* Read the WCMASTR file and get the WC Group Code */
            call "READ100" (#2, wccode$, f1%(2%))
               if f1%(2%) = 0% then return
               get #2 using L33940, group$(2%)
L33940:        FMT POS(2022), CH(3)
            return

        REM *************************************************************~
            *   C A L C U L A T E   T H E   C R I T I C A L  R A T I O  *~
            *-----------------------------------------------------------*~
            * The critical ratio in the workfile records coming in here *~
            * is the number of days from today to the end date of the   *~
            * job.  If it is negative, it means the job was to finish   *~
            * before today.  If the user selected to have the C/R be    *~
            * pure PIPIN due date order (method 'A') we're pretty much  *~
            * done.  If they selected one of the other methods, we get  *~
            * fancy.  The rest of this discussion attempts to explain   *~
            * what fancy means.  If C/R is method 'B' or 'C' and it is  *~
            * negative coming in here, we leave it alone.               *~
            * For all those other wf records having a positive C/R,     *~
            * meaning the job planned end date is still in the future,  *~
            * we will calculate a proper C/R to prioritize them.        *~
            * The formula used for calculation of the Critical ratio is *~
            * non-traditional (ie. not APICS) because CMS is a finite   *~
            * capacity system. By this we mean that we look ahead and   *~
            * adjust the C/R based on how busy the downstream work-     *~
            * centers are.      Also, since the calculation takes time  *~
            * we will only do it once for each job.  We'll stash it in  *~
            * a special job-specific record in workfile #8 keyed by     *~
            * tag number and HEX(00).                                   *~
            * The problem with the traditional C/R formula of time      *~
            * remaining divided by work remaining is that a) in a finite*~
            * capacity situation, the amount of time remaining is a     *~
            * function of the available capacity in each of the work-   *~
            * centers the job is scheduled to visit, and b) time in     *~
            * certain workcenters may be in very short supply.  Jobs    *~
            * that must pass through workcenters that will be very busy *~
            * in the coming days/weeks are given higher priority.       *~
            * In summary, we calculate Critical Ratio by first finding  *~
            * the total hours remaining as the sum of the orignal       *~
            * available hours in each of the workcenters the job is     *~
            * scheduled to visit for the range of days from the planned *~
            * start date of the job or today (whichever is later) until *~
            * the planned end date of the job.This total amount of time *~
            * is then divided by the sum of the work remaining to be    *~
            * done in each work center.  However, before we summarize   *~
            * the work in various workcenters and do the division, we   *~
            * multiply the work remaining in each workcenter by a       *~
            * factor calculated for that workcenter.  The factor is     *~
            * an adjustment for the criticality of the workcenter,      *~
            * based on how heavily it is loaded during the life span    *~
            * of the job. This adjustment expresses the variation       *~
            * in criticality for the different workcenters involved     *~
            * by considering the scheduled load in them for job duration*~
            * and the amount of involvement the particular job has with *~
            * each of them. In other words, it brings bottle-necks into *~
            * the calculation.  The factors for each job/wc combination *~
            * were calculated and stored in workfile #8 back up in the  *~
            * BUILD_THE_FILES section.  The workfile #8 records also    *~
            * contain the total hours originally available and used for *~
            * each workcenter within the date range for the job.        *~
            *                                                           *~
            *   FACTOR =  SUM OF CAPACTITY BOOKED FOR LIFE OF JOB/      *~
            *               SUM OF CAPACITY AVAILABLE FOR LIFE OF JOB   *~
            *                                                           *~
            *   ADJUSTED WORK FOR WC = CAPACITY REQUIRED IN WC *        *~
            *                            THE FACTOR FOR THE JOB/WC      *~
            *                                                           *~
            *  C/R =  SUM OF TIME AVAILABLE IN EACH OF THE WORKCENTERS/ *~
            *           SUM OF THE ADJUSTED WORK FOR THE WORKCENTERS    *~
            *                                                           *~
            *************************************************************

        calc_critical_ratio

            call "SHOSTAT" ("Calculating Critical Ratios")
            currentstep$ = " "
            init(hex(00)) plowkey$

        get_next_record

            call "PLOWNXT1" (#5, plowkey$, 0%, f1%(5))
              if f1%(5) = 0% then return

            get #5, using L34470, wccode$, critratio$, planstart$, tag$,  ~
              step$, quantity, stepsetup%, steprun%, planend$, scflag$

L34470:      FMT  POS(4), CH(4), POS(9), CH(10), CH(6), POS(29), CH(19), ~
                  CH(7), POS(90), PD(14,4), POS(102), BI(4), BI(4),      ~
                  POS(121), CH(6), POS(145), CH(1)

             if scflag$ = "Y" then get_next_record

         REM We take a little detour to set the carryover flag if the    ~
             step has activity in the workcenter beyond the date range
             init(hex(00)) plowkey2$
             str(plowkey2$, 1, 19) = str(tag$)
             str(plowkey2$, 20,2) = bin(ldate%, 2)
             str(plowkey2$, 22,2) = hex(ffff)
             call "PLOWNEXT" (#1, plowkey2$, 19%, f1%(1))
               if f1%(1) = 0% then L34520

             carryover$ = "Y"
             put #5, using L34506, carryover$
L34506:        FMT  POS(110), CH(1)
             rewrite #5

L34520:      cr = 0:convert critratio$ to cr, data goto L34530
L34530:      test = cr - 1000000
             if test <= 0 then get_next_record
             if calcmethod$ = "A" then get_next_record

         REM  Now we've found a job that is not late already, so ...     ~
              next issue is to see if job C/R already calc'ed

            str(readkey$,1,19) = str(tag$)
            str(readkey$,20,4) = hex(00000000)

            call "READ100" (#8, readkey$, f1%(8))
            if f1%(8) = 0% then  L34660
              get #8, using L34641, critratio
L34641:       FMT XX(23), PD(14,4)
              goto update_cr

L34660:     gosub work_remaining
            gosub time_remaining
            gosub calculation
            goto  update_cr

        REM Figure out the dividend of the C/R calculation
        time_remaining
           totaltime = 0
            str(readkey$,1,19) = str(tag$)
            str(readkey$,20,4) = hex(00000000)
L34691:    call "PLOWNEXT" (#8, readkey$, 19%, f1%(8))
              if f1%(8) = 0 then return   /* got em all */
           get #8, using L34695, hours
L34695:      FMT XX(31), PD(14,4)
            totaltime =  totaltime + hours
            goto L34691

        REM Figure out the divisor for the calculation
        work_remaining
            factor = 1
            wcstep$ = " " : currentwcstep$ = " "
            totalwork, work, adjusted, mq = 0
            init(hex(00)) plowkey2$
            str(plowkey2$, 1,19) = str(tag$)

L34970:     call "PLOWNEXT" (#1, plowkey2$, 19%, f1%(1))
              if f1%(1) = 0% then  L35278
            get #1, using L34992,  wc$, dateout%, setup%, run%, step$
L34992:       FMT  CH(4), BI(2), XX(25), BI(4), BI(4), CH(4)
            wcstep$ = str(wc$) & str(step$)
            if wcstep$ = currentwcstep$ then L35180
            str(readkey$)  = all(hex(00))
            str(readkey$,1,19) = str(tag$)
            str(readkey$,20,4) = str(wc$)
            call "READ100" (#8, readkey$, f1%(8))
               if f1%(8) <> 0 then L35073
               gosub find_and_write_the_factor
               goto L35082

L35073:     get #8, using L35074, factor, totavail, totused
L35074:        FMT XX(23), PD(14,4), PD(14,4), PD(14,4)

        REM If the factor is 0 then the only wcouts are before today ... ~
            so we recalc the factor using the step as the usage
L35082:     if factor > 0 then L35090
              steptotal% = stepsetup% + steprun%
              steptotal = steptotal%
              totused = totused + steptotal
              if totavail = 0 then totavail = 1
              factor = totused / totavail

L35090:     call "READ100" (#2, wc$, f1%(2))
            get #2, using L35110, unitsperday%
L35110:       FMT  XX(2019), BI(2)
            unitsperday = unitsperday%
            hoursperunit = 24 / unitsperday
            currentwcstep$ = wcstep$

L35180:     if setup% + run% <> 0% then goto L35220
                mq% = mq% + 1%
            goto L34970

L35220:     setup =  setup%
            run = run%
            setup = setup * hoursperunit
            run = run * hoursperunit
            work = setup + run
            adjusted = work * factor
            totalwork = totalwork + adjusted
            goto L34970

L35278:     if includemq$ = "N" then return
              mq = mq% * 24
            totalwork = totalwork + mq
            return


        REM Perform the C/R calculation and save it for the job

        calculation
            if totalwork <> 0 then L35480
              critratio = 0
              goto  L35481

L35480:     critratio = totaltime/totalwork
L35481:     critratio = critratio + 1000000
            write #8, using L35493, tag$, hex(00000000), critratio
L35493:      FMT  CH(19), CH(4), PD(14,4)
           return

        update_cr
            call "CONVERT" (critratio, 2.2, critratio$)
            call "READ101"  (#5, plowkey$, f1%(5))
              if f1%(5%) = 0% then  return  /* something is fishy */
            put #5, using L35560, critratio$
L35560:       FMT POS(9), CH(10)
            rewrite #5
            goto   get_next_record

        REM *************************************************************~
            *   P R I N T   T H E   R E P O R T                         *~
            *-----------------------------------------------------------*~
            * Read the workfile for each of the workcenters & print.    *~
            * This process is complicated because the column headers    *~
            * change depending on whether the job has been reported in  *~
            * to the workcenter yet.  Welcome to my nightmare.          *~
            * The records are read from the workfile by group, then     *~
            * workcenter then by the reported in flag (X, then Y, then  *~
            * Z) and then finally by critical ratio.  The tricky part   *~
            * is making sure the proper column headers (sub header) is  *~
            * set to appear above the record you just read.             *~
            * YOU BREAK IT, YOU OWN IT.                                 *~
            *************************************************************

        print_the_report

        REM First we set everything up ...
            init(" ") wccode$, jobnumber$, tag$, step$, activity$, part$,~
                      carryover$, reportedin$, firstwcout$, lastwcout$,  ~
                      critratio$, inwc$, instep$, scflag$
            critratio = 0
            units$ = "UNITS"
            currentwccode$ = " "
            if uh$ = "H" then units$ = "HOURS"
            call "SHOSTAT" ("Printing Dispatch List")
            runtime$ = " "
            call "TIME" (runtime$)
            call "COMPNAME" (12%, company$, u3%)
            page% = -1%
            line% = 999%
            select printer(134)
            call "SETPRNT" ("JB0006", " ", 0%, 0%)

        REM and we set up the counters to control the sub-headings
            currenthead$ = " "   /* last sub-head printed indicator  */

            active% = 0%         /* number of records in & active    */
            in% = 0%             /* number of records reported in WC */
            notin% = 0%          /* number of records not reported in*/

            count%   = 0%        /* total report lines counter       */
            printed% = 0%        /* printed something if = 1%        */

        REM Print the Report Selection Criteria
            gosub print_parameters

        REM Then we find the first record & then fall into the loop...
            init(hex(00)) plowkey$
            call "PLOWALTS" (#5, plowkey$, 1%, 0%, f1%(5%))
            goto L36310

        read_record
            call "READNEXT" (#5, f1%(5%))
L36310:     if f1%(5%) = 0% then end_report

            get #5, using L36380, wccode$, reportedin$, critratio$,       ~
                planstart$, tag$,  step$, firstwcout$, activity$,        ~
                        part$,  quantity, mq%, setup%, run%, carryover$, ~
                          lastwcout$, wcouts%, planend$, inwc$, instep$, ~
                          inact$, group$(2%), scflag$, toutsetup%,       ~
                          toutrun%, toutqty, toutscrap, toutrework, towc$

L36380:      FMT    POS(4), CH(4), CH(1), CH(10), CH(6), XX(04), CH(19), ~
                    CH(7), CH(6), CH(4), CH(25), PD(14,4), BI(4), BI(4), ~
                    BI(4),  CH(1), CH(6), BI(4), CH(6), CH(4), CH(7),    ~
                    CH(4), CH(3), CH(1),  BI(4), BI(4), 3*PD(14,4), CH(4)

            if scflag$ = "Y" then read_record

            critratio = 0
            convert critratio$ to critratio, data goto L36440
L36440:     critratio = critratio - 1000000

            call "DATEFMT" (firstwcout$)
            call "DATEFMT" (lastwcout$)
            call "DATEFMT" (planend$)
            call "DATEFMT" (planstart$)
            if carryover$ = "Y" then lastwcout$ = "CARRYOVR"

            if reportedin$ <> "X" then print_control
               setup%   = max(0%, setup% - toutsetup%)
               run%     = max(0%, run% - toutrun%)
               quantity = max(0%, quantity-toutqty-toutscrap-toutrework)
               if towc$ = " " then towc$ = "DONE"

         print_control
            if printed% <> 0% then L36570
                gosub load_new_wc     /* first time through */
                                     /* loads data and prints page head */

L36570:     if wccode$ = currentwccode$ then L36650

            gosub print_message              /* change in WC  */
               if currenthead$ = "Z" then L36620
               if currenthead$ = "Y" then L36602
                  gosub print_subhead2        /* probably need a check */
                  gosub print_message         /* line count here abouts*/
L36602:        gosub print_subhead3
                  gosub print_message

L36620:     gosub load_new_wc

L36650:     if currenthead$ <> reportedin$ then L36660
            gosub print_line             /* normal - record fits header*/
            goto read_record

L36660:     if currenthead$  = "Y" then L36700
            gosub print_message
            gosub print_subhead2
            if reportedin$ <> "Y" then L36700
            gosub print_line
            goto read_record

L36700:     gosub print_message
            gosub print_subhead3
            gosub print_line
            goto read_record

        print_message
            REM we get here if the new record doesn't fit current subhead
            if currenthead$ <> "X" then goto  L36790
            if active% > 0% then goto  L36900
            print
            print using L38530, currentwccode$ /* none reported in */
            return

L36790:     if currenthead$ <> "Y" then goto  L36850
            if in% > 0% then goto  L36900
            print
            print using L38540, currentwccode$ /* none reported in */
            return

L36850:     if notin% > 0% then goto  L36900
            print
            print using L38550, currentwccode$ /* all reported in */
            return

L36900:     print
            print using L38560   /* end of list message */
            line% = line% + 2%
            return

        print_line
            printed% = 1%
            if line% >  50% then gosub print_page_heading
            run = run%
            setup = setup%
            if  uh$  = "U" then L37019
                run   =  run * hoursperunit
                setup =  setup * hoursperunit

L37019:     if reportedin$ <> "X" then L37080
            print using L38630, round(critratio,1), tag$, step$,          ~
                               activity$, part$, round(quantity,0),      ~
                               round(setup,2), round(run,2), firstwcout$,~
                               lastwcout$, towc$
            active% = active% + 1%
            goto L37210

L37080:     if reportedin$ <> "Y" then L37160
            print using L38640, round(critratio,1), tag$, step$,          ~
                               activity$, part$, round(quantity,0),      ~
                               round(setup,2), round(run,2), firstwcout$,~
                               lastwcout$, planstart$, planend$
            in% = in% + 1%
            goto L37210

L37160:     print using L38710, round(critratio,1), tag$, step$,          ~
                               activity$, part$, round(quantity,0),      ~
                               round(setup,2), round(run,2), firstwcout$,~
                               lastwcout$, inwc$, instep$
            notin% = notin% + 1%
L37210:     line% = line% + 1%
            printed% = 1%
            count% = count% + 1%

        REM Print the second line if requested

            if descrprt$ =  "N" then return
                secondline$ = " "
            if descrprt$ =  "D" then L37330
                jobdescr$, partdescr$ = "** Not On File **"
                call "DESCRIBE" (#4, str(tag$,12), jobdescr$, 0%, f1%(4))
                call "DESCRIBE" (#9, part$, partdescr$, 0%, f1%(9))
                if jobdescr$ = partdescr$ then jobdescr$ = " "
                str(secondline$,11,30) = jobdescr$
                str(secondline$,44,32) = partdescr$

L37330:     if descrprt$ =  "Y" then L37490
                str(secondline$,78,50) = "DEM                     CUS    ~
        ~       T=  P=     "
                demtype$, dempriority$, cuscode$ = " "
                dem$ = "*No Pegging Found*"
                init(hex(00)) readkey$
                str(readkey$,1,19) = str(tag$)
                call "PLOWALTS" (#10, readkey$, 1%, 19%, f1%(10))
                   if f1%(10) = 0% then L37490
                   get #10, using L37400, dem$
L37400:            FMT  CH(19)
                call "REDALT0" (#11, dem$, 1%, f1%(11))
                   get #11, using L37430, demtype$, dempriority$, cuscode$
L37430:            FMT POS(2), CH(1), CH(1), POS(89), CH(9)
                str(secondline$,82,19) = dem$
                str(secondline$,107,9)  = cuscode$
                str(secondline$,118,1) = demtype$
                str(secondline$,122,1) = dempriority$
                str(secondline$,124,8) = planend$

L37490:         print using L38740, secondline$
                line% = line% + 1%
                return

        end_report
            if count% > 0% then L37640
                call "ASKUSER" (0%, "NOTHING TO PRINT",                  ~
                                "There is no activity to print.",  " ",  ~
                                "Press RETURN to Continue...")
                goto L37685

L37640:     if currenthead$ <> "X" then L37645
                gosub print_message
                gosub print_subhead2

L37645:     if currenthead$ <> "Y" then L37670
                gosub print_message
                gosub print_subhead3

L37670:     gosub print_message
            print
            print using L38262     /* End of Report message */

L37685:     call "SETPRNT" ("JB0006", " ", 0%, 1%)
            close printer
            return

        load_new_wc
            totused% , totavail% = 0%
            call "READ100" (#2, wccode$, f1%(2))
              if f1%(2) = 0% then goto print_page_heading
              get #2, using  L37735, wcdescr$ , avail%(), used%(),        ~
                                    unitsperday%
L37735:       FMT XX(6), CH(30), XX(23), 490 * BI(2), 490 * BI(2), BI(2)

            call "PIPINDEX" (#6, today$, fdate%, ret%)
            for i% = fdate% to ldate%
              totavail% = totavail% + avail%(i%)
              totused% = totused% + used%(i%)
            next i%

            currentwccode$ = wccode$
            currenthead$ = "X"
            active% = 0%
            in% = 0%
            notin% = 0%
            unitsperday = unitsperday%
            hoursperunit = 24 / unitsperday

            totused = totused%
            totavail = totavail%

            if uh$ = "U" then L37835
              totused = totused * hoursperunit
              totavail = totavail * hoursperunit

L37835:     if totavail   > 0  then L37860
              percent = 0
              remaining  = 0
              goto L37880

L37860:     percent =  totused  / totavail
            remaining  = totavail  - totused
            if remaining  <= 0  then remaining  = 0

L37880:     percent = percent * 100

        print_page_heading
            page% = page% + 1%  :  line% = 7%
            print page
            print using L38240, date$, runtime$, company$
            print using L38270, page%
            print using L38300, lastdate$

            if page% = 0% then return
            print using L38392, group$(2%)
            print using L38400, wccode$, round(unitsperday,0),            ~
                               round(hoursperunit,2), units$,            ~
                               round(totavail,1), units$,                ~
                               round(remaining,1)

            print using L38430, wcdescr$, units$, round(totused,1),       ~
                               round(percent,1)

            if currenthead$ = "X" then gosub print_subhead1
            if currenthead$ = "Y" then gosub print_subhead2
            if currenthead$ = "Z" then gosub print_subhead3
            return

        print_subhead1
            print
            print using L38471, currentwccode$ /* reported in & ACTIVE */
            print using L38480  /* short underline */
            print using L38570, units$, units$ /* column header   */
            print using L38620  /* column underline*/
            currenthead$ = "X"
            line% = line% + 3%
            return

        print_subhead2
            print
            print using L38470, currentwccode$ /*     reported in */
            print using L38500  /* short underline */
            print using L38574, units$, units$ /* column header   */
            print using L38600  /* column underline*/
            currenthead$ = "Y"
            line% = line% + 3%
            return

        print_subhead3
            print
            print using L38460, currentwccode$ /* not reported in */
            print using L38510  /* short underline */
            print using L38660, units$, units$ /* column header   */
            print using L38680  /* column underline*/
            currenthead$ = "Z"
            line% = line% + 3%
            return

        print_parameters
            gosub print_page_heading
            print skip(4)
            print using L38320 : print
            print using L38340
            print using L38360, "RANGE OF WORKCENTER GROUPS", firstgp$,   ~
                                lastgp$
            print using L38360, "RANGE OF WORKCENTERS", begwc$, endwc$
            print using L38360, "CUTOFF DATE FOR ACTIVITY", lastdate$
            print using L38360, "INCLUDE M/Q IN CRIT. RATIO?", includemq$
            print using L38360, "SHOW ADVICES/JOBS OR BOTH?", ajbflag$
            print using L38360, "SHOW CAPACITY UNITS OR HOURS?", uh$
            print using L38360, "SHOW DESCRIP AND/OR DEMANDS?", descrprt$
            print using L38360, "CALCULATION METHOD FOR C/R?", calcmethod$
            print using L38360, "SORT BY WORKCENTER GROUP?", sort_by_gp$

            print : print using L38380
            line% = 999%
            return

        REM Behold, the images

L38240: %RUN DATE: ######## ########             ########################~
        ~####################################                 JBDSPRPT:JB0~
        ~006
L38262: %                                                * * * * * * *  E~
        ~ND OF REPORT  * * * * * * *
L38270: %                                                          WORKCE~
        ~NTER DISPATCH LIST                                         PAGE: ~
        ~###
L38300: %                                                  SHOWING SCHEDU~
        ~LED ACTIVITY THRU ########
L38320: %                                 ----------- REPORT SELECTION PA~
        ~RAMETERS -----------
L38340: %                                                                ~
        ~  FROM      TO
L38360: %                                 ############################## ~
        ~  ########  ########
L38380: %                                 -------------------------------~
        ~--------------------

L38392: %GROUP: ###

L38400: %WORKCENTER: ########   UNITS/24 HRS: ###### (1 = ##.## HRS)  ###~
        ~## AVAILABLE TODAY TIL CUT-OFF: #######.#  ##### REMAINING: #####~
        ~#.#
L38430: %DESCRIPTION: ##############################                  ###~
        ~## BOOKED (ADVISED & RELEASED): #######.#  CAP UTILIZATION %:  ##~
        ~#.#
L38460: %SCHEDULED JOBS NOT YET REPORTED INTO WC ####
L38470: %SCHEDULED JOBS ALREADY REPORTED INTO WC ####
L38471: %SCHEDULED JOBS CURRENTLY ACTIVE IN WC ####
L38480: %============================================                    ~
        ~    ----------REMAINING--------- --PLANNED STEP---            NEXT

L38500: %============================================                    ~
        ~      JOB     SET-UP      RUN    --PLANNED STEP--- ---JOB PLANNED~
        ~---
L38510: %============================================                    ~
        ~      JOB     SET-UP      RUN    --PLANNED STEP--- --NOW IN---
L38530: % * * * NO SCHEDULED JOBS IN WC #### ARE REPORTED ACTIVE * * *
L38540: % * * * NO SCHEDULED JOBS HAVE BEEN REPORTED INTO WC #### * * *
L38550: % * * * ALL SCHEDULED JOBS HAVE BEEN REPORTED INTO WC #### * * *
L38560: %                                                * * * * * * * * ~
        ~END OF LIST  * * * * * * * *

L38570: %  C/R     JOB OR TAG NUMBER   STEP   ACT      PART TO BUILD     ~
        ~    QUANTITY S/U ##### RUN #####  START     END             WORKC~
        ~NTR

L38574: %  C/R     JOB OR TAG NUMBER   STEP   ACT      PART TO BUILD     ~
        ~    QUANTITY  #####      #####    START     END    ST DATE  DUE D~
        ~ATE
L38600: %-------- ------------------- ------- ---- ----------------------~
        ~--- -------- --------- --------- -------- -------- -------- -----~
        ~---

L38620: %-------- ------------------- ------- ---- ----------------------~
        ~--- -------- --------- --------- -------- --------          -----~
        ~---
L38630: %-#####.# ################### ####### #### ######################~
        ~### ######## ######.## ######.## ######## ########            ####


L38640: %-#####.# ################### ####### #### ######################~
        ~### ######## ######.## ######.## ######## ######## ######## #####~
        ~###
L38660: %   C/R    JOB OR TAG NUMBER   STEP   ACT      PART TO BUILD     ~
        ~    QUANTITY  #####      #####    START     END     WC   STEP
L38680: %-------- ------------------- ------- ---- ----------------------~
        ~--- -------- --------- --------- -------- -------- ---- ------

L38710: %-#####.# ################### ####### #### ######################~
        ~### ######## ######.## ######.## ######## ######## #### ######

L38740: %################################################################~
        ~#################################################################~
        ~###

        %PRINTED #### JOB STEP(S) IN SELECTED WORKCENTER(S)


        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

        REM                 /* FILE  WCOUT                             */~
            CH(4),    1     /* Work Center code                        */~
            BI(2),    1     /* Date out of PIP in date subscript form f*/~
            BI(2),    1     /* General purpose sequence number         */~
            CH(19), P 1     /* Tag number in level 2 planning          */~
            BI(2),  P       /* Date out of PIP in date subscript form f*/~
            BI(2),  P       /* General purpose sequence number         */~
            BI(4),          /* Set Up time in WC units                 */~
            BI(4),          /* Run time in hours per part              */~
            CH(4),          /* RTE Step To Identify A Route Line       */~
            BI(1),          /* Phantom counter for route steps         */~
            BI(2),          /* alternate part seq. no.                 */~
            BI(1),          /* Concurrent Workcenter Seq. relative to p*/~
            CH(04),         /* Activity to be performed                */~
            CH(17)          /* Filler (Internal, unused space)         */

        REM                 /* FILE  JBSTATUS                          */~
            CH(8),  P       /* Production job code                     */~
            BI(4),  P       /* General purpose sequence number         */~
            CH(4),          /* Workcenter to which it was moved        */~
            CH(4),          /* Activity to which it was moved          */~
            CH(8),    1     /* Job Number                              */~
            CH(4),    1  2  /* Workcenter from which it was moved      */~
            CH(6),    1  2  /* system date when record written         */~
            CH(8),    1  2  /* Time Transaction occurred               */~
            CH(4),    1  2  /* Activity   from which is was moved      */~
            CH(6),    1  2  /* system (clock) date from the computer   */~
            CH(8),    1  2  /* Time from the system clock              */~
            CH(7),          /* Step moved from                         */~
            CH(7),          /* Step moved to                           */~
            CH(12),         /* employee code                           */~
            BI(4),          /* The reported set up hours in a work cent*/~
            BI(4),          /* The reported run hours in a given work c*/~
            PD(14,4),       /* The quantity of parts Moved             */~
            PD(14,4),       /* The quantity of parts scrapped          */~
            PD(14,4),       /* The quantity of parts reworked          */~
            CH(30),         /* Any free text information               */~
            CH(4),          /* user-id of specific user                */~
            CH(1),          /* Step Complete Flag                      */~
            CH(43)          /* Filler (Internal, unused space)         */

        REM         keys  pos  /* FILE  WORK1  The main workfile #5    */~
            CH(3),     1   1   /* Workcenter group                     */~
            CH(4),     1   4   /* Workcenter code                      */~
            CH(1),     1   8   /* Reported In Flag                     */~
            CH(10),    1   9   /* Critical Ratio                       */~
            CH(6),     1  19   /* Job Planned start date               */~
            CH(4),   P 1  25   /* Workcenter code                      */~
            CH(19),  P 1  29   /* Tag Number                           */~
            CH(7),   P 1  48   /* Step Number                          */~
            CH(6),        55   /* Earliest WCOUT date for step         */~
            CH(4),        61   /* Activity code/operation number       */~
            CH(25),       65   /* Part Number                          */~
            PD(14,4),     90   /* Job Quantity to Build                */~
            BI(4),        98   /* Total Move/Queue Units               */~
            BI(4),       102   /* Total Set-up Units                   */~
            BI(4),       106   /* Total Run Units                      */~
            CH(1),       110   /* Carry-over Flag                      */~
            CH(6),       111   /* Latest WCOUT date for step           */~
            BI(4),       117   /* Number of WCOUTs for step            */~
            CH(6),       121   /* Job planned end date                 */~
            CH(4),       127   /* Last WC into which job reported      */~
            CH(7),       131   /* Last Step into which job reported    */~
            CH(4),       138   /* Last Activity to which job reported  */~
            CH(3),       142   /* Workcenter group                     */~
            CH(1)        145   /* Step Complete Flag                   */~
            BI(4),       146   /* Total actual setup reported          */~
            BI(4),       150   /* Total actual runtime reported        */~
            PD(14,4),    154   /* Total good quantity reported out     */~
            PD(14,4),    162   /* Total scrap quantity reported out    */~
            PD(14,4),    170   /* Total rework quantity reported out   */~
            CH(04),      178   /* Destination/next workcenter          */~
            PD(14,4),    182   /* Quantity reported into this step     */~
            CH(10)       190   /* Filler, unused space                 */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L42200,         /* First Group       */   ~
                                L42200,         /* Last Group        */   ~
                                L42200,         /* First Workcenter  */   ~
                                L42200,         /* Last  Workcenter  */   ~
                                L42200,         /* Ending Date       */   ~
                                L42200,         /* Include M/Q (Y/N) */   ~
                                L42200,         /* Advices/Jobs/Both */   ~
                                L42200,         /* Units or Hours    */   ~
                                L42200,         /* Print Descrptns   */   ~
                                L42200,         /* Calc Method       */   ~
                                L42200          /* Sort by Group     */

              goto L42500

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L42200:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L42500:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Workcenter Dispatch List",                      ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,35), fac(hex(ac)), header1$               , ch(08),~
               at (05,45), fac(hex(ac)), header2$               , ch(08),~
                                                                         ~
               at (06,02), "Range of Workcenter Groups"         ,        ~
               at (06,35), fac(lfac$( 1)), firstgp$             , ch(03),~
               at (06,45), fac(lfac$( 2)), lastgp$              , ch(03),~
                                                                         ~
               at (07,02), "Range of Workcenters"               ,        ~
               at (07,35), fac(lfac$( 3)), firstwc$             , ch(04),~
               at (07,45), fac(lfac$( 4)), lastwc$              , ch(04),~
                                                                         ~
               at (09,02), "Cut-Off Date for Activity"          ,        ~
               at (09,35), fac(lfac$( 5)), lastdate$            , ch(08),~
                                                                         ~
               at (10,02), "Include M/Q in Crit. Ratio?"        ,        ~
               at (10,35), fac(lfac$( 6)), includemq$           , ch(01),~
                                                                         ~
               at (11,02), "Show Advices/Jobs or Both?"         ,        ~
               at (11,35), fac(lfac$( 7)), ajbflag$             , ch(01),~
                                                                         ~
               at (12,02), "Show Capacity Units or Hours?"      ,        ~
               at (12,35), fac(lfac$( 8)), uh$                  , ch(01),~
                                                                         ~
               at (13,02), "Show Descrips and/or Demands?"      ,        ~
               at (13,35), fac(lfac$( 9)), descrprt$            , ch(01),~
                                                                         ~
               at (14,02), "Calculation Method for C/R?"        ,        ~
               at (14,35), fac(lfac$(10)), calcmethod$          , ch(01),~
                                                                         ~
               at (15,02), "Sort by Workcenter Group?"          ,        ~
               at (15,35), fac(lfac$(11)), sort_by_gp$          , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L46400
                  call "MANUAL" ("JBDSPRPT") : goto L42500

L46400:        if keyhit% <> 15 then L46700
                  call "PRNTSCRN" : goto L42500

L46700:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L48600     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L48200
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L48200:     if fieldnr% > 1% then L48400
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L48400:     return

L48600: if fieldnr% > 0% then L49500  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Print Report"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L49500:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50146,         /* First WC Group    */     ~
                              L50216,         /* Last  WC Group    */     ~
                              L50336,         /* First Workcenter  */     ~
                              L50406,         /* Last Workcenter   */     ~
                              L50526,         /* Last Date         */     ~
                              L50606,         /* Include M/Q (Y/N) */     ~
                              L50666,         /* Advices/Jobs/Both */     ~
                              L50746,         /* Units or Hours    */     ~
                              L50806,         /* Print Descriptions*/     ~
                              L50886,         /* Calculation Method*/     ~
                              L50956          /* Sort by WC Group? */

            return

L50146: REM Test for First Workcenter Group of Range   FIRSTGP$
            if firstgp$ = "ALL" then return
            groupdescr$ = hex(06) & "Select Workcenter Group Code"
            readkey$ = "WCGROUPS " & firstgp$
            call "PLOWCODE" (#12, readkey$, groupdescr$,9%, 0.3,f1%(12%))
               if f1%(12%) = 1% then L50210
               errormsg$ = "Invalid Workcenter Group Code."
               return
L50210:     firstgp$ = str(readkey$,10)
               return


L50216: REM Test for Last Workcenter Group of Range     LASTGP$
            if firstgp$ <> "ALL" then L50230
               lastgp$ = " "
               return
L50230:     if lastgp$ <> " " then L50236
               lastgp$ = firstgp$
               return
L50236:     if lastgp$ = "?" then lastgp$ = " "
            readkey$ = "WCGROUPS " & lastgp$
            call "PLOWCODE" (#12, readkey$, groupdescr$,9%, 0.3,f1%(12%))
               if f1%(12%) = 1% then L50250
               errormsg$ = "Invalid Workcenter Group Code."
               return
L50250:     lastgp$ = str(readkey$,10)
            if firstgp$ > lastgp$ then L50306
               return

L50306:     errormsg$ ="'TO' Workcenter Group must be greater than 'FROM'"
               return

L50336: REM Test for First Workcenter of range    FIRSTWC$
            if firstwc$ = "ALL" then return
            call "GETCODE" (#2, firstwc$, " ", 0%, 0, f1%(2))
            if f1%(2) = 1% then return
            errormsg$ = "Enter or select a valid Workcenter or 'ALL'"
                  return

L50406: REM Test for Last Workcenter of Range     LASTWC$
            if firstwc$ = "ALL" then return
            call "GETCODE" (#2, lastwc$, " ", 0%, 0, f1%(2))
            if f1%(2) = 0% then L50476
            if firstwc$ > lastwc$ then L50496
            return

L50476:     errormsg$ = "Enter or select a valid Workcenter"
                  return
L50496:     errormsg$ = "'TO' Workcenter must be greater than 'FROM'"
                  return

L50526: REM Test for Ending Date                  LASTDATE$
            call "DATEOK" (lastdate$, l%, errormsg$)
            if errormsg$ <> " " then return
            call "DATEOK" (date, d%, "  ")  /* tricky way to get d% */
            if l% >= d%     then return
            errormsg$ = "Cut-Off Date must be on or after today"
            return

L50606: REM Test for Include M/Q in Crit. Ratio?  INCLUDEMQ$
            if includemq$ = "Y" then  return
            if includemq$ = "N" then  return
            errormsg$ = "Enter a  Y or an N"
            return

L50666: REM Test for Show Advices/Jobs or Both?   AJBFLAG$
            if ajbflag$ = "A" then  return
            if ajbflag$ = "J" then  return
            if ajbflag$ = "B" then  return
            errormsg$ = "Enter an A to report advised jobs, J to report r~
        ~eleased jobs or B for both"
            return

L50746: REM Test for Show Hours or Units          UH$
            if uh$ = "U" then  return
            if uh$ = "H" then  return
            errormsg$ = "Enter either a U or an H"
            return

L50806: REM Test for Print Job And Part Descr     DESCRPRT$
            if descrprt$ = "Y" then  return
            if descrprt$ = "D" then  return
            if descrprt$ = "B" then  return
            if descrprt$ = "N" then  return
            errormsg$ = "Enter either a Y, D, B, or an N"
            return

L50886: REM Test for Calculation Method           CALCMETHOD$
            if calcmethod$ = "A" then  return
            if calcmethod$ = "B" then  return
            if calcmethod$ = "C" then  return
            errormsg$ = "Enter either an A, B, or C"
            return

L50956: REM Test for Sort by WC Group             SORT_BY_GP$
            if sort_by_gp$ = "Y" then  return
            if sort_by_gp$ = "N" then  return
            errormsg$ = "Enter either a 'Y' or 'N'"
            return

L51036: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
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
            call "SHOSTAT" ("One Moment Please")



            end
