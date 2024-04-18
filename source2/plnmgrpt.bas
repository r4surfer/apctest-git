        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  PPPP   L      N   N  M   M   GGG   RRRR   PPPP   TTTTT   *~
            *  P   P  L      NN  N  MM MM  G      R   R  P   P    T     *~
            *  PPPP   L      N N N  M M M  G GGG  RRRR   PPPP     T     *~
            *  P      L      N  NN  M   M  G   G  R   R  P        T     *~
            *  P      LLLLL  N   N  M   M   GGG   R   R  P        T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PLNMGRPT - Scan for and display PIP management situations.*~
            *            Critical Shortages, Safety Stock Intrusions, or*~
            *            Surplus Conditions.  Display start day of cond-*~
            *            ition, if possible, last day of condition and  *~
            *            optionally print detail of PIP activity between*~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/10/86 ! Original                                 ! KAB *~
            * 05/14/87 ! Standard Costing Changes                 ! ERN *~
            * 07/14/87 ! ADDED SFTY STK, LEADTIME, ABCCODE, O/HAND! JER *~
            * 07/14/87 ! ADDED 4TH CONDITION-LATE ACTIVITY, ETC.  ! KAB *~
            * 05/23/88 ! 1) Changed PIP problem condition calcs   ! RJM *~
            *          !    to use Shelf instead of PIP.          !     *~
            *          ! 2) Added Shelf to report.                !     *~
            *          ! 3) Added more selection criteria         !     *~
            *          ! 4) Added Minimum Days to be a problem by !     *~
            *          !    condition, if problem doesn't last    !     *~
            *          !    Min days, it's not reported.          !     *~
            * 06/13/88 ! Now reports both Critical Shortages and  ! RJM *~
            *          !    Forecast Shortages.                   !     *~
            * 06/24/88 ! Fixed Range Selection Tests @ 50000      ! RJM *~
            * 09/14/89 ! Attempted to fix 'NEXT w/o FOR' error.   ! JDH *~
            * 08/13/90 ! Fixed FMT statement @ 30210.             ! MJB *~
            * 04/24/91 !(PRR 11692) Expanded Type Selection option! RJB *~
            *          !     to allow for the printing of all PIP !     *~
            *          !     Also added call to ALLFREE new stand.!     *~
            * 06/19/91 ! QC-FIX Moved call to ALLFREE to the right! RJB *~
            *          !    place, added display of new option to !     *~
            *          !    the Header Page.                      !     *~
            *          !(PRR 11907) Corrected the branching in the!     *~
            *          !     'PRINT_MAIN' Section to goto the post!     *~
            *          !     FOR-NEXT validation code instead of  !     *~
            *          !     to the NEXT statment.                !     *~
            * 07/30/91 !(NO PRR) Added code to the Date range test! RJB *~
            *          !     to allow the 'ALL' option to work.   !     *~
            * 01/06/94 !Added UOM & Userid to report. Thnxs Walter! JDH *~
            *          !PRR 12597,12795 INITed Action quantity.   !     *~
            *          !PRR 11911 Added selections for input.     !     *~
            *          !PRR 12715,12786 Fixed multiple breaks on  !     *~
            *          !  same part & end date range test.        !     *~
            * 08/23/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            blankdate$8,                 /* Blank Date for Comparison  */~
            bpl$3,                       /* Buyer/Scheduler            */~
            bpldescr$32,                 /* Buyer/Scheduler Descr.     */~
            abccode$1,                   /* ABC CODE FROM HNYMASTR     */~
            cat$(2)4,                    /* Part Category Range        */~
            class$(2)4,                  /* PART CLASS RANGE           */~
            company$60,                  /* Company Name               */~
            cumf%(490),                  /* CUMULATIVE SALES FORECAST  */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            date$(2)10,                  /* Date Range                 */~
            date1%(2),                   /* DATE RANGE                 */~
            detail$1,                    /* Detail?                    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            from$30,                     /* From screen prompt         */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lastpart$25,                 /* Part Code                  */~
            leadtime$10,                 /* HNYMASTR LEAD TIME         */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            mdays$(3)3,                  /* MIN DAYS TO BE A PROBLEM   */~
            mindays%(5),                 /* MIN DAYS TO BE A PROBLEM   */~
            part$(2)25,                  /* Part Code                  */~
            part$25,                     /* Part Code                  */~
            partdescr$32,                /* Part Description           */~
            pcat$4,                      /* Part Category              */~
            pclass$4,                    /* Part Class                 */~
            pip%(490),                   /* Planned Inventory Position */~
            pip(4),                      /* On Hand, SS, MOQ, Pansize  */~
            pip$8,                       /* PIP on Detail day          */~
            pipact$8,                    /* PIP Action Quantity        */~
            pipday1$8,                   /* Start of something bad     */~
            pipday2$8,                   /* End of something bad       */~
            pipohqty$10,                 /* PIPMASTR ON-HAND QTY       */~
            pipmsg$25,                   /* What is Something bad      */~
            pipin$19,                    /*   Pip                      */~
            pipout$19,                   /*       Detail               */~
            pipinday$8,                  /*             Print          */~
            pipoutday$8,                 /*                  Variables */~
            pipinqty$10,                 /*                            */~
            pipoutqty$10,                /*                            */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            pf4$18,                      /* PF 4 Screen Literal        */~
            pland1$8,                    /* Planning Day 1             */~
            plandl$8,                    /* Planning Day 490           */~
            plbase$6,                    /* Planning Day 1             */~
            plowdescr$99,                /* PLOWCODE Decription        */~
            plowkeym$100,                /* Miscellaneous Read/Plow Key*/~
            plowkeyi$100,                /* Miscellaneous Read/Plow Key*/~
            plowkeyo$100,                /* Miscellaneous Read/Plow Key*/~
            readkey$100,                 /* Miscellaneous Read/Plow Key*/~
            rpttime$8,                   /* Report Time                */~
            rpttitle$60,                 /* Report Title               */~
            safetystk$10,                /* HNYMASTR SAFETY STOCK      */~
            sch$3,                       /* Buyer/Scheduler            */~
            schdescr$32,                 /* Buyer/Scheduler Descr.     */~
            shelf$8,                     /* Shelf Quantity             */~
            temp$10,                                                     ~
            title$8,                                                     ~
            to$30,                       /* To screen prompt           */~
            to_from$21,                  /* PLOWCODE prompt            */~
            type$(2)3,                   /* Part Type                  */~
            uom$4,                       /* Unit of Measure            */~
            userid$3,                    /* Current User Id            */~
            x$(5)1                       /* Select Type                */~

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
            * #1  ! PIPMASTR ! Planned Inventory Position Master File   *~
            * #2  ! HNYMASTR ! Inventory Master File                    *~
            * #3  ! PIPIN    ! Planned inventory additions detail       *~
            * #4  ! PIPOUT   ! Planned inventory use detail rec         *~
            * #5  ! CATEGORY ! Inventory Category File                  *~
            * #7  ! SYSFILE2 ! System Control File                      *~
            * #8  ! GENCODES ! General Codes File                       *~
            * #41 ! SFCUM2   ! Cumulative Sales Forecast File           *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "PIPMASTR",                                      ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    2, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  26          ~

            select #2,  "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            select #3,  "PIPIN",                                         ~
                        varc,     indexed,  recsize =   60,              ~
                        keypos =   30, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  48          ~

            select #4,  "PIPOUT",                                        ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =   20, keylen =  37

            select #5,  "CATEGORY",                                      ~
                        varc,     indexed,  recsize = 500,               ~
                        keypos = 1, keylen = 4

            select #7,  "SYSFILE2",                                      ~
                        varc,     indexed,  recsize = 500,               ~
                        keypos =    1, keylen =  20

            select #8,  "GENCODES",                                      ~
                        varc, indexed, recsize = 128,                    ~
                        keypos =    1,  keylen = 24

            select #41, "SFCUM2",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1985,                                  ~
                        keypos =    1, keylen =  25

            call "SHOSTAT" ("Opening Files, One Moment Please")

               rslt$(1), rslt$(7), rslt$(41) = "REQUIRED"
            call "OPENCHCK" (#1,  fs%(1%), f2%(1%), 0%, rslt$(1%))
               if fs%(1) < 0% then L65000
            call "OPENCHCK" (#7,  fs%(7%), f2%(7%), 0%, rslt$(7%))
               if fs%(7) < 0% then L65000
            call "OPENCHCK" (# 2,  fs%(2%), f2%(2%), 0%, rslt$(2%))
            call "OPENCHCK" (# 3,  fs%(3%), f2%(3%), 0%, rslt$(3%))
            call "OPENCHCK" (# 4,  fs%(4%), f2%(4%), 0%, rslt$(4%))
            call "OPENCHCK" (# 5,  fs%(5%), f2%(5%), 0%, rslt$(5%))
            call "OPENCHCK" (# 8,  fs%(8%), f2%(8%), 0%, rslt$(8%))
            call "OPENCHCK" (#41,  fs%(41), f2%(41), 0%, rslt$(41))
               if fs%(41) < 0% then L65000

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            from$ = "From"
            to$   = "To"

            call "PIPINDEX" (#7, " ", today%, err%)
                if err% <> 0% then L65000

            call "DATE" addr("G+", date, 1% - today%, pland1$, err%)
                if err% <> 0% then L65000
            plbase$ = pland1$
            call "DATEFMT" (pland1$)

            call "DATE" addr("G+", date, 490% - today%, plandl$, err%)
                if err% <> 0% then L65000
            call "DATEFMT" (plandl$)

            call "COMPNAME" (12%, company$, err%)

            rpttitle$ = "PLANNED INVENTORY POSITION MANAGEMENT ANALYSIS R~
        ~EPORT"
            call "FMTTITLE" (rpttitle$, " ", 12%)

*        See if there are Part Types defined to Check against
            readkey$ = "PARTTYPE "
            call "PLOWNEXT" (#08, readkey$, 9%, types_on_file%)

*        See if there are Part Classes defined to Check against
            readkey$ = "PARTCLASS"
            call "PLOWNEXT" (#08, readkey$, 9%, class_on_file%)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            call "ALLFREE"
            pf4$="(4)Previous Field" : pf16$="(16)Exit Program"
            init(" ") errormsg$, inpmessage$,                            ~
                      part$()                     ,/* Part Code        */~
                      date$()                     ,/* Date Range       */~
                      class$()                    ,/* Part Class Range */~
                      type$()                     ,/* Part Type        */~
                      cat$()                      ,/* Part Category    */~
                      mdays$()                    ,/* Min Days for Prob*/~
                      detail$                     ,/* Detail?          */~
                      x$()                        ,/* Select Type:     */~
                      bpl$                        ,/* Buyer/Scheduler  */~
                      bpldescr$                   ,/* BPL Description  */~
                      sch$                        ,/* Buyer/Scheduler  */~
                      schdescr$                    /* SCH Description  */~

            for fieldnr% = 1% to  10%
L10220:         gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0% then L10400
L10240:         if fieldnr% > 1 then pf4$="(4)Previous Field"            ~
                                     else pf4$=" "
L10260:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10350
                      if fieldnr% =  1% then L10350
L10300:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10240
                         if fieldnr% = 1% then L10220
                         goto L10300
L10350:               if keyhit%  = 16% then       exit_program
                      if keyhit%  =  0% then       L10400
                print at (02,02), bell
                goto L10240

L10400:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10260
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            pf4$  = " "
            pf16$ = "(16)Print Report"
            inpmessage$ = edtmessage$
            lastfieldnr% = 0%
            gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       datasave
                  if keyhit%  =  0% then       L11250
            print at(02,02), bell
            goto editpg1

L11250:     fieldnr% = cursor%(1) - 5%
            if fieldnr% < 1% then editpg1
            if fieldnr% < 9% then goto L11500
            if fieldnr% < 10% or fieldnr% > 14% then editpg1
            if cursor%(2) > 4% then fieldnr% = 10% else fieldnr% = 9%

L11500:     if fieldnr% = lastfieldnr% then editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then editpg1
                  pf4$, pf16$ = " "
                  lastfieldnr% = fieldnr%
L11530:     gosub'101(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  0% then L11590
            print at(02,02), bell
            goto L11530

L11590:     gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11530
            goto L11250

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            call "SHOSTAT" ("Report Generation in Progress.")
            select printer (134)
            call "SETPRNT" ("PLN004", " ", 0%, 0%)
            init (hex(00)) plowkeym$, lastpart$
            if part$(1) = "ALL" then L19120
               str(plowkeym$,,25) = part$(1)
               plowkeym$ = plowkeym$ addc all(hex(ff))

L19120:     line% = 999%
            page% = 0%
            rpttime$ = " "
            call "TIME" (rpttime$)

            gosub print_main

            if line% < 999% then L19170
               goto L19190

L19170:     print skip(2)
            print "                                         ********** E ~
        ~N D   O F   R E P O R T ********** "

L19190:     close printer
            call "SETPRNT" (" ", " ", 0%, 1%)


            if line% < 999% then inputmode

            errormsg$ = "No Management Conditions Detected for these Sele~
        ~ctions."
            goto editpg1

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20100,         /* Part Code        */~
                                    L20200,         /* Date Range       */~
                                    L20300,         /* Part Type        */~
                                    L20850,         /* Part Category    */~
                                    L20800,         /* Part Class       */~
                                    L20400,         /* Buyer/Scheduler  */~
                                    L20500,         /* Buyer/Scheduler  */~
                                    L20600,         /* Detail?          */~
                                    L20700,         /* Select Type      */~
                                    L20900          /* Min Days for Prob*/
                     return
L20100:     REM Part Code                             PART$(1)
                if part$(1) <> " " then L20150
                if part$(2) <> " " then L20150
                   part$(1) = "ALL"
                   part$(2) = " "
L20150:         inpmessage$ = "Enter Part Code Range, 'ALL', or '?'."
                return
L20200:     REM Date Range                            DATE$(1)
                if date$(1%) <> " " and date$(1%) <> blankdate$ then L20250
                if date$(2%) <> " " and date$(2%) <> blankdate$ then L20250
                   date$(1%) = date
                   call "DATFMTC" (date$(1%))
                   date$(2%) = plandl$
                   call "DATUFMTC" (date$(2%))
                   call "DATFMTC" (date$(2%))
L20250:         inpmessage$ = "Enter Date Range or 'ALL'."
                return
L20300:     REM Part Type                             TYPE$(1)
                if type$(1) <> " " then L20350
                if type$(2) <> " " then L20350
                   type$(1) = "ALL"
                   type$(2) = " "
L20350:         inpmessage$ = "Enter Part Type Range, 'ALL', or '?'."
                return
L20400:     REM Buyer/Scheduler                       BPL$
                inpmessage$ = "Indicate a Specific Buyer Part Class or le~
        ~ave BLANK for 'ALL'."
                return
L20500:     REM Buyer/Scheduler                       SCH$
                inpmessage$ = "Indicate a Specific Scheduler Part Class o~
        ~r leave BLANK for 'ALL'."
                return
L20600:     REM Detail?                               DETAIL$
                if detail$ = " " then detail$ = "Y"
                inpmessage$ = "'Y' = Full Detail, 'N' = Summary Only"
                return
L20700:     REM Select Type                           X$
                if str(x$()) = " " then init ("X") str(x$())
                x$(5) = " "
                inpmessage$ = "Non-blank characters indicate which condit~
        ~ions WILL BE reported."
                return
L20800:     REM Part Class                            CLASS$(1)
                if class$(1) <> " " or class$(2) <> " " then L20825
                     class$(1) = "ALL"
                     class$(2) = " "
L20825:         inpmessage$ = "Enter Part Class Range, 'ALL', or '?'."
                return
L20850:     REM Part Category                         CAT$(1)
                if cat$(1) <> " " or cat$(2) <> " " then L20870
                     cat$(1) = "ALL"
                     cat$(2) = " "
L20870:         inpmessage$ = "Enter Part Category Range, 'ALL', or '?'."
                return
L20900:     REM Minimum Days to be a problem          MDAYS$()
                if str(mdays$()) <> " " then L20920
                mdays$(1), mdays$(2), mdays$(3) = "1"
L20920:         inpmessage$ = "Enter Minimum number of Days each conditio~
        ~n must exist to be Printed"
                return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
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
            * PRINT MAIN REPORT                                         *~
            *-----------------------------------------------------------*~
            * Loads data from PIPMASTR record and Prints if necessary.  *~
            *************************************************************

        print_main

L30080:     call "PLOWNEXT" (#1, plowkeym$, 0%, f1%(1))
               if f1%(1) = 0% then return
            get #1, using L30110, part$, pip%(), pip(), piptype%
L30110:         FMT XX(1), CH(25), 490*BI(4), 4*PD(14,4), BI(2)

            if part$(1) = "ALL" then L30150
               if part$ > part$(2) then return
L30150:     if piptype% < type1% then L30080
            if piptype% > type2% then L30080

            call "DESCRIBE" (#2, part$, partdescr$, 0%, f1%(2))
                  if f1%(2) = 0% then L30080
            get #2, using L30210, uom$, pcat$, abccode$, pclass$, leadtime$
L30210:         FMT XX(73), CH(4), XX(12), CH(4), XX(17), CH(1), XX(21), ~
                    CH(4), XX(33), CH(3)

            if class$(1) = "ALL" then L30270
            if pclass$ < class$(1) or pclass$ > class$(2) then L30080

L30270:     if cat$(1) = "ALL" then L30300
            if pcat$ < cat$(1) or pcat$ > cat$(2) then L30080

L30300:     mat cumf% = zer
            call "READ100" (#41, part$, f1%(41))
                if f1%(41) = 0% then L30360
            get #41, using L30340, cumf%()
L30340:         FMT XX(25), 490*BI(4)

L30360:
            call "CONVERT" (pip(1), 2.2, pipohqty$)
            call "CONVERT" (pip(2), 2.2, safetystk$)

            if bpl$ = " " then L30450
               get #2, using L30420, temp$
L30420:            FMT POS(200), CH(3)
               if temp$ <> bpl$ then L30080

L30450:     if sch$ = " " then L30500
               get #2, using L30470, temp$
L30470:            FMT POS(309), CH(3)
               if temp$ <> sch$ then L30080

L30500:     if x$(4) = " " then L30790
            holdpipday1%, pipday1% = 999%
            part% = 0%

            init (hex(00)) plowkeyi$, plowkeyo$
            str(plowkeyi$,,25) = part$
            str(plowkeyo$,,25) = part$

            call "PLOWALTS" (#3, plowkeyi$, 1%, 25%, f1%(3))
               if f1%(3) = 0% then L30630
            get str(plowkeyi$,26,4) using L30610, holdpipday1%
L30610:         FMT BI(4)

L30630:     call "PLOWALTS" (#4, plowkeyo$, 1%, 25%, f1%(4))
               if f1%(4) = 0% then L30670
            get str(plowkeyo$,26,4) using L30610, pipday1%

L30670:     pipday1% = min(pipday1%, holdpipday1%)
            if pipday1% >= date1%(1) then L30790
               holdpipday1% = pipday1%
               pipday2%     = date1%(1) - 1%
               pipstatus%   = 6%
               gosub print_part
               pipday1% = holdpipday1%
*             IF DETAIL$ <> "N" THEN GOSUB PRINT_DETAILS
               goto L30800        /* TOGGLE THIS AND LINE ABOVE TO PRINT */
                                 /* LATE & MANAGEMENT DETAIL AS ONE OR  */
                                 /* TWO BLOCKS ON REPORT.               */

L30790:     holdpipday1%, part% = 0%:lastpart$ = " "
L30800:     pipday1%, pipday2%, pipstatus% = 0%

*          NOTE: The following loop to look for problem conditions will
*                look beyond the ending date of scan range only if the
*                starting date of a condition has been found and the end
*                of that condition is beyond the ending date.

            for i% = date1%(1) to 490%
                on pipstatus% goto L31200, L31240, L31290, L31340, L31346

L30900:            if i% > date1%(2) then L31140  /* Beyond End Date ? */

*          First Find a Condition
                   if x$(1) = " " then L30980
                      if pip%(i%) >= 0% then L30980
                         pipstatus% = 1%  /* CRITICAL SHORTAGE */
                         goto set_start_of_condition

L30980:            if x$(2) = " " then L31030
                      if pip%(i%) - max(0,cumf%(i%)) >= pip(2) then L31030
                         pipstatus% = 2%  /* SAFETY STCK INTRUSION */
                         goto set_start_of_condition

L31030:            if x$(3) = " " then L31090
                      if pip%(i%) - max(0,cumf%(i%)) <= pip(2) + pip(3)  ~
                                  then L31090
                         pipstatus% = 3%  /* SURPLUS CONDITION */
                         goto set_start_of_condition

L31090:            if x$(1) = " " then L31112
                      if pip%(i%) - max(0,cumf%(i%)) >= 0% then L31112
                         pipstatus% = 4%  /* FORECAST SHORTAGE */
                         goto set_start_of_condition
L31112:            if x$(5) = " " then goto upper_next_i
                         pipstatus% = 5%  /* PRINT ALL PARTS/PIP */
                   set_start_of_condition : pipday1%   = i%
            upper_next_i : next i%
L31140:     if pipstatus% <> 0% then L31370
L31150:     if part% = 0% then L30080
               pipday2% = date1%(1) - 1%
               goto L31430

*        Found Critical Shortage, When (If Ever) does It Clear.
L31200:               if pip%(i%) >= 0% then set_change_of_condition
                         goto L31346

*        Found Safety Stock Intrusion, When (If Ever) does It Clear.
L31240:               if pip%(i%) - max(0,cumf%(i%)) < 0% then           ~
                                                  set_change_of_condition
                      if pip%(i%) - max(0,cumf%(i%)) >= pip(2) then      ~
                                                  set_change_of_condition
                         goto L31346

*        Found Surplus, When (If Ever) does It Clear.
L31290:               if pip%(i%) - max(0,cumf%(i%)) <= pip(2) + pip(3)  ~
                                then set_change_of_condition
                         goto L31346

*        Found Forecast Shortage, When (If Ever) does It Clear.
L31340:               if pip%(i%) - max(0,cumf%(i%)) >= 0% then          ~
                                                  set_change_of_condition
                          goto L31346

*        No Problems Found (SELECT TYPE for ALL)
L31346:               if pip%(i%) = 0% then L31350
                          if i% = 1% then set_change_of_condition
                          if pip%(i%) <> pip%(i%-1%) then                ~
                                                  set_change_of_condition
L31350:     next i%

L31370:     if mindays%(pipstatus%) <= i% - pipday1% +1% then L31400
                pipday1%, pipday2%, pipstatus%, holdpipday1% = 0%
                goto L31150
L31400:     pipday2% = i%
            if holdpipday1% = 0% then holdpipday1% = pipday1%
            gosub print_part
L31430:     pipday1% = holdpipday1%
            if detail$ <> "N" then gosub print_details
            goto L30080

          set_change_of_condition
            if mindays%(pipstatus%) <= i% - pipday1% + 1% then L31500
                pipday1%, pipday2%, pipstatus%, holdpipday1% = 0%
                goto upper_next_i
L31500:     pipday2% = i%
            if holdpipday1% = 0% then holdpipday1% = pipday1%
            gosub print_part
            if detail$ = "N" then L31870

            newpipstatus% = 0%
            if i% > date1%(2) then L31850    /* Beyond ending date ? */
*          Still Bad News?
                   if x$(1) = " " then L31630
                      if pip%(i%) >= 0% then L31630
                         newpipstatus% = 1%  /* Critical Shortage */
                         goto set_start_of_condition_2

L31630:            if x$(2) = " " then L31680
                     if pip%(i%) - max(0%,cumf%(i%)) >= pip(2) then L31680
                         newpipstatus% = 2%  /* Safety Stck Intrusion */
                         goto set_start_of_condition_2

L31680:            if x$(3) = " " then L31740
                      if pip%(i%) - max(0%,cumf%(i%)) <= pip(2) + pip(3) ~
                                then L31740
                         newpipstatus% = 3%  /* Surplus Condition */
                         goto set_start_of_condition_2

L31740:            if x$(1) = " " then L31810
                      if pip%(i%) - max(0%,cumf%(i%)) >= 0% then L31781
                         newpipstatus% = 4%  /* Forecast Shortage */
                         goto set_start_of_condition_2

L31781:            if x$(5) = " " or pip%(i%) = 0% then                  ~
                                                 set_start_of_condition_2
                         if i% = 1% then L31788
                         if pip%(i%) = pip%(i%-1%) then                  ~
                                                 set_start_of_condition_2
L31788:                  newpipstatus% = 5%  /* Print All Parts/PIP */
                   set_start_of_condition_2 : pipday1%   = i%

L31810:     if newpipstatus% = 0% then L31850
            pipstatus% = newpipstatus%
            goto upper_next_i

L31850:     pipday1% = holdpipday1%
            gosub print_details
L31870:     holdpipday1%, part%, pipday1%, pipday2%, pipstatus% = 0%
            goto L30900  /* Almost to the top & check the end date */

        REM *************************************************************~
            * PRINT MAIN REPORT                                         *~
            *-----------------------------------------------------------*~
            * Prints what we just found, in whatever format.            *~
            *************************************************************

        print_part

            maxpip% = -999999999% : minpip% = 999999999%
            maxshelf% = -999999999% : minshelf% = 999999999%
            for j% = pipday1% to pipday2%
                maxpip% = max(maxpip%, pip%(j%))
                minpip% = min(minpip%, pip%(j%))
                shelf% = pip%(j%) - max(0%,cumf%(j%))
                maxshelf% = max(maxshelf%, shelf%)
                minshelf% = min(minshelf%, shelf%)
            next j%
            pipday1$, pipday2$ = " "
            call "DATE" addr("G+", plbase$, pipday1% - 1%, pipday1$, err%)
            call "DATE" addr("G+", plbase$, pipday2% - 1%, pipday2$, err%)
            call "DATEFMT" (pipday1$)
            call "DATEFMT" (pipday2$)
            on pipstatus% goto L32230, L32250, L32270, L32290, L32312, L32310
L32230:        pipmsg$ = "  CRITICAL SHORTAGE"
                  goto L32330
L32250:        pipmsg$ = "SAFETY STOCK INTRUSION"
                  goto L32330
L32270:        pipmsg$ = "  SURPLUS CONDITION"
                  goto L32330
L32290:        pipmsg$ = "  FORECAST SHORTAGE"
                  goto L32330
L32310:        pipmsg$ = " PRIOR/LATE ACTIVITY"
                  goto L32330
L32312:        pipmsg$ = " No Problems"

L32330:     if detail$ <> "N" then part_for_detail

            if line% > 58% then gosub print_header

            on pipstatus% goto L32380, L32400, L32420, L32435, L32442, L32440
L32380:        pipmsg$ = "  C/S"
                  goto L32460
L32400:        pipmsg$ = "S/S I"
                  goto L32460
L32420:        pipmsg$ = " SURP"
                  goto L32460
L32435:        pipmsg$ = "  F/S"
                  goto L32460
L32440:        pipmsg$ = " LATE"
                  goto L32460
L32442:        pipmsg$ = "   OK"

L32460:     if part$ = lastpart$ then L32550
               print
               line% = line% + 1%

            print using L36180, part$, partdescr$, pipmsg$, pipday1$,     ~
                               pipday2$, minpip%, maxpip%, minshelf%,    ~
                               maxshelf%
            goto L32580

L32550:     print using L36180, " ", " ", pipmsg$, pipday1$,              ~
                               pipday2$, minpip%, maxpip%, minshelf%,    ~
                               maxshelf%
L32580:     line% = line% + 1%
            lastpart$ = part$
            return

        part_for_detail

L32640:     if part% > 0% then L32710
            if line% > 56% then gosub print_header

            print using L36210
            print using L36240
            line% = line% + 2%

L32710:     if line% < 59% then L32770
               print using L36210
               part% = 0%
               line% = 99%
               goto L32640

L32770:     if part$ <> lastpart$ then L32830
            print using L36270, " ", " ", pipmsg$, pipday1$,              ~
                               pipday2$, minpip%, maxpip%

            goto L32860

L32830:     print using L36270, part$, partdescr$, pipmsg$, pipday1$,     ~
                               pipday2$, minpip%, maxpip%

L32860:     lastpart$ = part$
            line% = line% + 1%
            part% = part% + 1%

            return

        REM *************************************************************~
            * PRINT DETAILS                                             *~
            *-----------------------------------------------------------*~
            * Appends detail information if found and requested.        *~
            *************************************************************
        print_details

            init (hex(00)) plowkeyi$, plowkeyo$
            oldpipactday%, pipout%, pipin%, detail%, f1%(3), f1%(4) = 0%
            put plowkeyi$ using L34050, part$, pipday1%
L34050:         FMT CH(25), BI(4)
            put plowkeyo$ using L34050, part$, pipday1%

L34065:     if f1%(3) < 0% then L34120
            if pipin% > 0% then L34120
            call "PLOWALTS" (#3, plowkeyi$, 1%, 25%, f1%(3))
                if f1%(3) = 0 then L34100
            get #3, using L34090, pipin%
L34090:         FMT XX(25), BI(4)
            if pipin% <= pipday2% then L34120
L34100:        init (" ") pipinday$, pipin$, pipinqty$
               f1%(3) = -99%
               pipin% = 999%

L34120:     if f1%(4) < 0 then L34175
            if pipout% > 0% then L34175
            call "PLOWALTS" (#4, plowkeyo$, 1%, 25%, f1%(4))
                if f1%(4) = 0 then L34155
            get #4, using L34145, pipout%
L34145:         FMT  XX(44), BI(4)
            if pipout% <= pipday2% then L34175
L34155:        init (" ") pipoutday$, pipout$, pipoutqty$
               f1%(4) = -99%
               pipout% = 999%

L34175:     if f1%(3) < 0% and f1%(4) < 0% then L34505

            init (" ") pipin$, pipout$, pipinqty$, pipoutqty$,           ~
                       pipinday$, pipoutday$, pip$, pipact$, shelf$

            if pipout% < pipin% then L34245

            get #3, using L34215, pipin$, pipinqty
L34215:         FMT XX(29), CH(19), PD(14,4)
            call "CONVERT" (pipinqty, 2.2, pipinqty$)
            pipinday$ = " "
            call "DATE" addr("G+", plbase$, pipin% - 1%, pipinday$, err%)
            call "DATEFMT" (pipinday$)
            pipactday% = pipin%

L34245:     if pipin% < pipout% then L34290

            get #4, using L34260, pipout$, pipoutqty
L34260:         FMT CH(19), XX(37), PD(14,4)
            call "CONVERT" (pipoutqty, 2.2, pipoutqty$)
            pipoutday$ = " "
            call "DATE" addr("G+", plbase$, pipout% - 1%, pipoutday$,err%)
            call "DATEFMT" (pipoutday$)
            pipactday% = pipout%

L34290:     convert pip%(pipactday%) to pip$, pic(-#######)
          shelf% = max(0%, pip%(pipactday%) - max(0%,cumf%(pipactday%)))
            convert shelf% to shelf$, pic(-#######)

            pipact% = 0%
           if pip%(pipactday%) - max(0%,cumf%(pipactday%)) < pip(2%) then~
         pipact% = pip(2%) -(pip%(pipactday%) -max(0%,cumf%(pipactday%)))

        if pip%(pipactday%) -max(0%,cumf%(pipactday%)) > pip(2%) +pip(3%)~
               then pipact% = pip(2%) + pip(3%) - ( pip%(pipactday%)     ~
                                            - max(0%,cumf%(pipactday%)) )

            convert pipact% to pipact$, pic(-#######)

            if line% < 57% then L34390
               if detail% > 0% then print using L36440                    ~
                                    else print using L36210
               line% = line% + 1%
               oldpipactday%, part%, detail% = 0%
               gosub print_header
               gosub part_for_detail

L34390:     if detail% > 0% then L34430
            print using L36293, safetystk$, pipohqty$, uom$, abccode$,    ~
                               leadtime$, minshelf%, maxshelf%
               print using L36310
               print using L36350
               print using L36380
               line% = line% + 4%

L34430:     if oldpipactday% = pipactday% then                           ~
                                        init (" ") pip$, pipact$, shelf$

            print using L36410, pipin$, pipinday$, pipinqty$,             ~
                               pipout$, pipoutday$, pipoutqty$,          ~
                               pip$, shelf$, pipact$

            line% = line% + 1%
            detail% = detail% + 1%
            oldpipactday% = pipactday%
            if pipin$ <> " " then pipin% = 0%
            if pipout$ <> " " then pipout% = 0%

            goto L34065

L34505:     if detail% = 0% then L34535
               print using L36440
               line% = line% + 1%
               goto L34555


L34535:     print using L36293, safetystk$, pipohqty$, uom$, abccode$,    ~
                               leadtime$, minshelf%, maxshelf%
            print using L36210
            line% = line% + 2%
L34555:     print skip(1)
            line% = line% + 1%

            return

        REM *************************************************************~
            * PRINT HEADINGS CONTROL                                    *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************

        print_header

            if page% > 0% then L35120
               gosub co_head
               gosub print_selections

L35120:     page% = page% + 1%
            gosub co_head

            lastpart$ = " "

            if detail$ <> "N" then return

            print using L36120
            print using L36150
            line% = line% + 2%
            return

        co_head
            print page
            print using L36060, date$, rpttime$, userid$, company$
            print using L36085, rpttitle$, page%
            print
            line% = 4%
            return

        print_selections
            print skip(3)
            print using L36510:print
            print using L36530
            print using L36550, "Part Code Range", part$(1), part$(2)
            print using L36550, "Date Range     ", date$(1), date$(2)
            print using L36550, "Part Type Range", type$(1), type$(2)
            print using L36550, "PART Category RANGE", cat$(1), cat$(2)
            print using L36550, "Part Class Range", class$(1), class$(2)
            print
            temp$ = bpl$
            if temp$ = " " then temp$ = "ALL"
            print using L36570, "Buyer Part Class Code", temp$, bpldescr$
            temp$ = sch$
            if temp$ = " " then temp$ = "ALL"
            print using L36570, "Prod. Scheduler Class", temp$, schdescr$
            print using L36550, "Detail Selected?", detail$, " "
            print
            print using L36550, "Conditions Selected:", " ", " "
            if x$(1) <> " " then                                         ~
              print using L36570, "   Critical Shortages", mdays$(1),     ~
                                 "Days Minimum"
            if x$(2) <> " " then                                         ~
              print using L36570, "   Safety Stock Intrusions", mdays$(2),~
                                 "Days Minimum"
            if x$(3) <> " " then                                         ~
              print using L36570, "   Surplus Conditions", mdays$(3),     ~
                                 "Days Minimum"
            if x$(4) <> " " then                                         ~
              print using L36570, "   Prior/Late Activity", " ", " "
            if x$(5) <> " " then                                         ~
              print using L36570, "ALL with or without Problems", " ", " "
            print
            print using L36590

            return

        REM *************************************************************~
            * IMAGE STATEMENTS                                          *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************

L36060: %Run: ########  ########  By: ###    ############################~
        ~################################                     PLNMGRPT:PLN~
        ~004

L36085: %                                    ############################~
        ~################################                         Page:  #~
        ~###

L36120: %Part Code                 Part Description                  Cond~
        ~     From    Through     Min PIP     Max PIP   Min Shelf   Max Sh~
        ~elf

L36150: %------------------------- --------------------------------  ----~
        ~-  -------- --------  ----------  ----------  ----------  -------~
        ~---

L36180: %######################### ################################  ####~
        ~#  ######## ########  -#########  -#########  -#########  -######~
        ~###

L36210: %+---------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~--+

L36240: %!Part Code:                Part Description:                Mana~
        ~gement Condition:   From:     Through:       Minimum:   Maximum: ~
        ~  !

L36270: %!######################### ################################ ####~
        ~##################  ########  ########  PIP: -#########  -#######~
        ~##!

L36293: %!*Saftey Stock: ##########  Qty On Hand: ##########  UOM: ####  ~
        ~ABC Code: #   Lead Time (Days): ###   Shelf: -#########  -#######~
        ~##!

L36310: %+---+---------------------+----------+------------++------------~
        ~---------+----------+------------++----------+----------+--------~
        ~--+

L36350: %    ! Procurement Tag No. ! Date In  !   Quantity !! Withdrawal ~
        ~Tag No.  ! Date Out !   Quantity !! P. I .P. !   Shelf  !   Actio~
        ~n !

L36380: %    !---------------------+----------+------------++------------~
        ~---------+----------+------------++----------+----------!--------~
        ~--!

L36410: %    ! ################### ! ######## ! ########## !! ###########~
        ~######## ! ######## ! ########## !! ######## ! ######## ! #######~
        ~# !

L36440: %    +---------------------+----------+------------++------------~
        ~---------+----------+------------++----------+----------+--------~
        ~--+

*       ****************** SELECTION PARAMETERS ************************

L36510: %                          -------------------- Report Selection ~
        ~ Parameters ------------------------------
L36530: %                                                       From     ~
        ~                 To
L36550: %                          ############################ #########~
        ~################ #########################
L36570: %                          ############################ ###    ##~
        ~##############################
L36590: %                          --------------------------------------~
        ~------------------------------------------

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  str(line2$,61%) = "PLNMGRPT: " & str(cms2v$,,8%)
                  title$ = "Min Days"
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                  else                  init(hex(86)) lfac$()
                  on fieldnr% gosub L40250,         /* Part Code        */~
                                    L40250,         /* Date Range       */~
                                    L40250,         /* Part Type        */~
                                    L40250,         /* Part Category    */~
                                    L40250,         /* PART CLASS       */~
                                    L40250,         /* Buyer/Scheduler  */~
                                    L40250,         /* Buyer/Scheduler  */~
                                    L40250,         /* Detail?          */~
                                    L40250,         /*    Critical Short*/~
                                    L40280          /* MIN DAYS, PROB   */

                  if errormsg$ > " " and fieldnr% > 0% then              ~
                     lfac$(fieldnr%) = or hex(10)
                  goto L40320

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L40250:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
L40280:           REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return

L40320:     accept                                                       ~
               at (01,02),                                               ~
                  "Planned Inventory Position Management Report",        ~
               at (01,66), "Date:",                                      ~
               at (01,72), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,28), fac(hex(ac)), from$                  , ch(25),~
               at (05,55), fac(hex(ac)), to$                    , ch(25),~
                                                                         ~
               at (06,02), "Part Code Range",                            ~
               at (06,28), fac(lfac$( 1)), part$(1)             , ch(25),~
               at (06,55), fac(lfac$( 1)), part$(2)             , ch(25),~
                                                                         ~
               at (07,02), "Date Range",                                 ~
               at (07,28), fac(lfac$( 2)), date$(1)             , ch(10),~
               at (07,55), fac(lfac$( 2)), date$(2)             , ch(10),~
                                                                         ~
               at (08,02), "Part Type Range",                            ~
               at (08,28), fac(lfac$( 3)), type$(1)             , ch(03),~
               at (08,55), fac(lfac$( 3)), type$(2)             , ch(03),~
                                                                         ~
               at (09,02), "Part Category Range",                        ~
               at (09,28), fac(lfac$( 4)), cat$(1)              , ch(04),~
               at (09,55), fac(lfac$( 4)), cat$(2)              , ch(04),~
                                                                         ~
               at (10,02), "Part Class Range",                           ~
               at (10,28), fac(lfac$( 5)), class$(1)            , ch(04),~
               at (10,55), fac(lfac$( 5)), class$(2)            , ch(04),~
                                                                         ~
               at (11,02), "Buyer Part Class Code",                      ~
               at (11,28), fac(lfac$( 6)), bpl$                 , ch(03),~
               at (11,35), fac(hex(8c))  , bpldescr$            , ch(32),~
               at (12,02), "Prod. Scheduler Class",                      ~
               at (12,28), fac(lfac$( 7)), sch$                 , ch(03),~
               at (12,35), fac(hex(8c))  , schdescr$            , ch(32),~
                                                                         ~
               at (13,02), "Detail?",                                    ~
               at (13,28), fac(lfac$( 8)), detail$              , ch(01),~
                                                                         ~
               at (14,02), "Select Type:",                               ~
                                                                         ~
               at (14,30), fac(hex(ac)), title$                 , ch(08),~
                                                                         ~
               at (15,02), fac(lfac$( 9)), x$(1)                , ch(01),~
               at (15,05), "Critical Shortage",                          ~
               at (15,30), fac(lfac$(10)), mdays$(1)            , ch(03),~
                                                                         ~
               at (16,02), fac(lfac$( 9)), x$(2)                , ch(01),~
               at (16,05), "Safety Stock Intrusion",                     ~
               at (16,30), fac(lfac$(10)), mdays$(2)            , ch(03),~
                                                                         ~
               at (17,02), fac(lfac$( 9)), x$(3)                , ch(01),~
               at (17,05), "Surplus Condition",                          ~
               at (17,30), fac(lfac$(10)), mdays$(3)            , ch(03),~
                                                                         ~
               at (18,02), fac(lfac$( 9)), x$(4)                , ch(01),~
               at (18,05), "Prior/Late Activity",                        ~
                                                                         ~
               at (19,02), fac(lfac$( 9)), x$(5)                , ch(01),~
               at (19,05), "ALL (with or without problems)",             ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,20), fac(hex(8c)), pf4$                           ,~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65), fac(hex(84)), pf16$                          ,~
                                                                         ~
               keys(hex(0001040d0f10)),                                  ~
               key (keyhit%)

               if keyhit% <> 13 then L40970
                  call "MANUAL" ("PLNMGRPT")
                  goto L40320

L40970:        if keyhit% <> 15 then L41010
                  call "PRNTSCRN"
                  goto L40320

L41010: REM    IF FIELDNR% > 0% THEN RETURN
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100,         /* Part Code        */~
                                    L50200,         /* Date Range       */~
                                    L50300,         /* Part Type        */~
                                    L50880,         /* Part Category    */~
                                    L50800,         /* Part Class Range */~
                                    L50400,         /* Buyer/Scheduler  */~
                                    L50500,         /* Buyer/Scheduler  */~
                                    L50600,         /* Detail?          */~
                                    L50700,         /* Select Type:     */~
                                    L50900          /* Min Days for Prob*/

                  return
L50100:     REM Part Code                             PART$(1)
              call "TESTRNGE" (part$(1), part$(2), " ", " ", errormsg$,#2)
                   return

L50200:     REM Date Range                            DATE$()
            if date$(1) <> "ALL" then L50205
                date$(1) = pland1$
                date$(2) = plandl$
                return
L50205:     if date$(2%) = " " or date$(2%) = blankdate$ then ~
                                  date$(2%) = date$(1%)
            for i% = 1% to 2%
               call "DATEOKC" (date$(i%), err%, errormsg$)
                   if errormsg$ <> " "  then return
               temp$ = date$(i%)
               call "DATUFMTC" (temp$)
               call "DATE" addr("G-", plbase$, temp$, date1%(i%), err%)
                   if err% = 0% then L50255
L50245:            errormsg$="Date not valid for planning"
                   return
L50255:        date1%(i%) = date1%(i%) + 1%
               if date1%(i%) < 1% then L50245
               if date1%(i%) > 490% then L50245
            next i%
            if date1%(1) > date1%(2) then                                ~
                     errormsg$ = "Date Range is Not Valid"
         return

L50300: REM Test for Part Type Range              TYPE$()
            type1% = 0%:type2% = 999%
            if type$(1%) <> "ALL" then L50308
                type$(2%) = " "
                goto L50336
L50308:     for i% = 1% to 2%
                if i% = 1% then to_from$ = "Part Type From Range"        ~
                           else to_from$ = "Part Type To Range"
                if type$(i%) = "?" and types_on_file% = 1% then L50324
                convert type$(i%) to type%, data goto L50344
                convert type% to type$(i%), pic(000)
                if type% < 0% or type% > 999% then L50344
                if types_on_file% <> 1% then L50332
L50324:              gosub check_gencode_for_part_type
                     if errormsg$ <> " " then return
                     if i% = 1% and type$(2%) = " " then type$(2%) =     ~
                                                                type$(1%)
L50332:     next i%

L50336:     call "TESTRNGE" (type$(1%), type$(2%), " ", " ", errormsg$)
            convert type$(1%) to type1%, data goto L50339
L50339:     convert type$(2%) to type2%, data goto L50340
L50340:     return

L50344:         errormsg$ = "Please enter " & to_from$ &                 ~
                            " as numeric, 000 to 999"
                return

        check_gencode_for_part_type
            if type$(i%) = "?" then type$(i%) = all(hex(20))
            readkey$ = "PARTTYPE " &  type$(i%)
            plowdescr$   = hex(06) & "Select " & to_from$
            call "PLOWCODE" (#08, readkey$, plowdescr$, 9%, .3, f1%(8%))
            if f1%(8%) = 1% then L50368
                errormsg$ = to_from$ & " not on File"
                return
L50368:     type$(i%) = str(readkey$, 10%, 3%)
            return

L50400:     REM Buyer/Scheduler                       BPL$
                if bpl$ <> " " then L50410
                   bpldescr$ = " "
                   return
L50410:         if bpl$ = "?" then bpl$ = " "
                readkey$ = "BYCLASSES" & bpl$
                bpldescr$ = hex(06) & "Select Buyer Part Class"
                call "PLOWCODE" (#8, readkey$, bpldescr$, 9%, 0.3, f1%(8))
                  if f1%(8) = 0% then L50445
                     bpl$ = str(readkey$,10)
                     call "PUTPAREN" (bpldescr$)
                       return
L50445:         if bpl$ = " " then return
                   errormsg$ = "Invalid Buyer Part Class: " & bpl$
                   return
L50500:     REM Buyer/Scheduler                       SCH$
                if sch$ <> " " then L50510
                   schdescr$ = " "
                   return
L50510:         if sch$ = "?" then sch$ = " "
                readkey$ = "PSCLASSES" & sch$
                schdescr$ = hex(06) & "Select Scheduler Part Class"
                call "PLOWCODE" (#8, readkey$, schdescr$, 9%, 0.3, f1%(8))
                   if f1%(8) = 0% then L50565
                      sch$ = str(readkey$,10)
                      call "PUTPAREN" (schdescr$)
                      return
L50565:         if sch$ = " " then return
                   errormsg$ = "Invalid Scheduler Part Class: " & sch$
                   return
L50600:     REM Detail?                               DETAIL$
                if detail$ <> "Y" then detail$ = "N"
                return
L50700:     REM Select Type                           X$
                if str(x$()) = " " then L50720
                     if x$(5) = " " then return
                          init("X") str(x$()) : return
L50720:         errormsg$ = "At Least One Selection Must Be Made."
                return
L50800:     REM Part Class Range                      CLASS$()
            if class$(1%) <> "ALL" then L50808
                class$(2%) = " "
                goto L50836
L50808:     for i% = 1% to 2%
                if i% = 1% then to_from$ = "Part Class From Range"       ~
                           else to_from$ = "Part Class To Range"
                if class$(i%) = "?" and class_on_file% = 1% then L50824
                if class_on_file% <> 1% then L50832
L50824:              gosub check_gencode_for_part_class
                     if errormsg$ <> " " then return
                     if i% = 1% and class$(2%) = " " then class$(2%) =   ~
                                                               class$(1%)
L50832:     next i%

L50836:     call "TESTRNGE" (class$(1%), class$(2%), " ", " ", errormsg$)
            return

                errormsg$ = "Invalid entry: " & to_from$
                return

        check_gencode_for_part_class
            if class$(i%) = "?" then class$(i%) = all(hex(20))
            readkey$ = "PARTCLASS" &  class$(i%)
            plowdescr$   = hex(06) & "Select " & to_from$
            call "PLOWCODE" (#08, readkey$, plowdescr$, 9%, .3, f1%(8%))
            if f1%(8%) = 1% then L50866
                errormsg$ = to_from$ & " not on File"
                return
L50866:     class$(i%) = str(readkey$, 10%, 4%)
            return

L50880:     REM Part Category Range                   CAT$()
              call "TESTRNGE" (cat$(1), cat$(2), " ", " ", errormsg$, #5)
                return
L50900:     REM Minimum Days to be a problem          MDAYS$()
                mat mindays% = zer
                for i% = 1% to 3%
                     if x$(i%) = " " then L50940
                     convert mdays$(i%) to mindays%(i%), data goto L50960
                     if mindays%(i%) < 1% then L50960
L50940:         next i%
                mindays%(4) = mindays%(1)    /* Forecast Shortage Days */
                return
L50960:         errormsg$ = "An Invalid number of Days was entered."
                return

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
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
