        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  H   H  N   N  Y   Y   SSS   L       OOO   W   W          *~
            *  H   H  NN  N   Y Y   S      L      O   O  W   W          *~
            *  HHHHH  N N N    Y     SSS   L      O   O  W W W          *~
            *  H   H  N  NN    Y        S  L      O   O  WW WW          *~
            *  R   H  N   N    Y     SSS   LLLLL   OOO   W   W          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYSLOW  - List slow moving inventory parts, based on     *~
            *            averages of past usage data.                   *~
            *----------------------------------------------------------Q*~
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
            * 02/06/87 ! Original                                 ! HES *~
            * 05/13/87 ! Std Cost Changes (HNYMASTR/QUAN files).  ! ERN *~
            * 06/07/89 ! Fixed number of months history accessed  ! MLJ *~
            *          !   for prior year, added report total for !     *~
            *          !   Value of On Hand.                      !     *~
            * 04/08/93 ! PRR 12463 ASKUSER now refers to HNYMASTR ! JIM *~
            *          !   and CLOSEs PRINTER file with totals.   ! JIM *~
            * 04/08/93 ! Standardized page 0 FAC removal method.  ! JIM *~
            * 04/08/93 ! Got rid of implied integer conversions.  ! JIM *~
            * 04/08/93 ! Added Time of Day stamp to End of Report.! JIM *~
            * 04/08/93 ! PRRs 12059&12836 Add Fiscal/Calendar opt.! JIM *~
            * 04/08/93 ! Moved page 0 FROM/TO to look like screen.! JIM *~
            * 04/08/93 ! Clear SUPPLY$ @ INPUTMODE & default to 1.! JIM *~
            * 08/24/93 ! QC/Rework- all screen & print references ! JIM *~
            *          !   (only) to 'month' are now 'period'.    !      ~
            * 09/22/94 ! Fixed crash in background.               ! JDH *~
            * 06/27/96 ! add date var ccyymmdd                    ! DER *~
            *          ! HNYHSTRY year file handling CH(2) BI(2)  !     *~
            *          ! HNYHSTRF year file handling CH(4) BI(4)  !     *~
            * 04/03/00 ! Mod to allow a start month other that Jan! CMG *~
            *          !   & to uses actual months from start     !     *~
            *          !   not just months with usage. (EWD0001)  !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            back$1,                      /* Background flag            */~
            beg_catg$(2)4,               /* Beginning Category Code    */~
            beg_part$(2)25,              /* Beginning Part Number      */~
            beg_store$(2)3,              /* Beginning Store Number     */~
            cat$4,                       /* Part Category Code         */~
            ccyymmdd$8,                  /* Date var for num year/month*/~
            company$60,                  /* Company / Division Name    */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            dt_temp$10,                  /* temporary date             */~
            end_catg$(2)4,               /* Ending Category Code       */~
            end_part$(2)25,              /* Ending Part Number         */~
            end_store$(2)3,              /* Ending Store Number        */~
            errormsg$79,                 /* Error message              */~
            file$8,                      /* Work File Name             */~
            firstmon$6,                  /* First Month For Usage Calc */~
            fis_cal$1,                   /* Calendar or Fiscal file?   */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            last_cat$4,                  /* Catg code For Print Cntrl  */~
            last_part$25,                /* Part code For Print Cntrl  */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            lib$8,                       /* Work File Library          */~
            line2$79,                    /* Second Line of Screen Headr*/~
            message$78,                  /* Informational Message      */~
            part$25,                     /* Part code                  */~
            partd$32,                    /* Part code description      */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            pf4$18,                      /* PF 4 Screen Literal        */~
            pf5$16,                      /* PF 5 Screen Literal        */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            print_title$60,              /* Report Sorted By Title     */~
            print$(4)10,                 /* Work Variable              */~
            print1$11,                   /* Work Variable              */~
            print_total$11,              /* Rpt totla - Value On Hand  */~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            rpt_time$8,                  /* Report Time                */~
            store$3,                     /* Store Number               */~
            sortby$1,                    /* Sorting Sequence, V or D   */~
            supply$2,                    /* Months Supply To Print     */~
            title$(2)30,                 /* Screen Column Headings     */~
            userid$3,                    /* Current User Id            */~
            vol$6,                       /* Work File Volume           */~
            wdsu(13),                    /* Withdrawls- Units          */~
            savkey$32                    /* Save Key for HNYHSTRF      */

        dim f1%(32)                      /* = 1 if READ was successful */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! HNYHSTRY ! Inventory Usage History                  *~
            * #2  ! HNYMASTR ! Inventory Master File                    *~
            * #3  ! HNYQUAN  ! Inventory Quantity File                  *~
            * #4  ! SYSFILE2 ! System Info (Default Pay Dates)          *~
            * #5  ! HNYHSTRF ! Inventory History on Fiscal Calendar     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "HNYHSTRY",                                      ~
                        varc,     indexed,  recsize =  450,              ~
                        keypos =    1, keylen =  30                      ~

            select #2,  "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            select #3,  "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44          ~

            select #4,  "SYSFILE2",                                      ~
                        varc, indexed, recsize = 500,                    ~
                        keypos = 1, keylen = 20

            select #5,  "HNYHSTRF",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos =    1, keylen =  32                      ~

        call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1,  0%, 0%, 0%, " ")
            call "OPENCHCK" (#2,  0%, 0%, 0%, " ")
            call "OPENCHCK" (#3,  0%, 0%, 0%, " ")
            call "OPENCHCK" (#4, fs%, 0%, 0%, " ")
            call "OPENCHCK" (#5,  0%, 0%, 0%, " ")
                if fs% <> 1% then exit_program

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$, "TT", back$)
            date$ = date
            call "DATEFMT" (date$, scratch%, ccyymmdd$)
            call "COMPNAME" (12%, company$, ret%)
            call "GETNAMES" addr(#2, file$, lib$, vol$)
            call "READFDR" addr(file$,lib$,vol$,0%,"RC",records%,ret%)

            REM Don't include current month in average usage calculation
            convert str(ccyymmdd$,1%,4%) to thisyear%
            convert str(ccyymmdd$,5%,2%) to thismon%
            thismon% = thismon% - 1%
            if thismon% = 0% then thisyear% = thisyear% - 1%
            if thismon% = 0% then thismon%  = 12%
            rpt_total = 0

            if back$ = "B" then printing_in_background
            title$(1%) = "From"  :  title$(2%) = "To  "

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            pf4$="(4)Previous Field" : pf5$=" ": pf16$="(16)EXIT PROGRAM"
            errormsg$, inpmessage$, beg_store$(), beg_catg$(), sortby$,  ~
            end_store$(), end_catg$(), end$, beg_part$(), end_part$(),   ~
            back$, fis_cal$, supply$ = " "
            lines_printed% = 0%

            for fieldnr% = 1% to 8%
L10140:         gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0% then L10260
L10160:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  = 1% then gosub startover
                      if keyhit% <> 4% then       L10240
L10190:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10160
                         if fieldnr% = 1% then L10140
                         goto L10190
L10240:               if keyhit%  = 16% then       exit_program
                      if keyhit% <>  0% then       L10160
L10260:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10160
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        edtpg1
            inpmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
            pf4$  = " "
            pf5$  = " "
            pf16$ = "(16)PRINT REPORT"
            if back$ = "Y" then pf16$ = "(16)SUBMIT TASK"
            gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       edtpg1
            oldfield% = 0%
L11180:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 11% then edtpg1
            if fieldnr% = 3% or fieldnr% =  4% or fieldnr% = 8%          ~
                then goto edtpg1
            if fieldnr% > 8% then fieldnr% = fieldnr% - 1%
            if fieldnr% > 3% then fieldnr% = fieldnr% - 2%
            if fieldnr% = oldfield% then edtpg1

            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       edtpg1
                  pf4$, pf5$, pf16$ = " "
L11280:     gosub'101(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11280
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11280
                  oldfield% = fieldnr%
                  goto L11180

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            if back$ <> "Y" then L19260
                call "READ101" (#4, "zHNYSLOW." & userid$, f1%(4%))
                put #4, using L19130, "zHNYSLOW." & userid$, firstmon$,   ~
                  supply$, beg_part$(), end_part$(), beg_store$(),       ~
                  end_store$(), beg_catg$(), end_catg$(), sortby$, back$,~
                  fis_cal$
                if f1%(4%) = 0% then write #4 else rewrite #4
L19130:         FMT CH(20), 19*CH(25)
                call "TASKUP" ("ME", 0%)
                goto L65000 /* We're outa here */

                printing_in_background
                message$ = "rptReport HNYSLOW in background: Aborted."
                call "READ101" (#4, "zHNYSLOW." & userid$, f1%(4%))
                     if f1%(4%) = 0% then tell_user
                get #4, using L19130, errormsg$, firstmon$, supply$,      ~
                  beg_part$(), end_part$(), beg_store$(), end_store$(),  ~
                  beg_catg$(), end_catg$(), sortby$, back$, fis_cal$
                if fis_cal$ = "C" then file% = 1% else file% = 5%
                delete #4

L19260:     REM Prepare for task at hand...
            convert str(firstmon$,1%,4%) to startyear%
            convert str(firstmon$,5%,2%) to startmon%
            convert supply$ to monsupply
            testmonths = 0                 /*  (EWD0001)  - Begin  */
            testyear%  = 0%
            testyear%  = startyear%

L19270:     a1% = 1% : a2% = 12% : if file% = 5% then a2% = 12%
            if testyear% = startyear% then a1% = startmon%
            if testyear% = thisyear% then a2% = thismon%
            for i% = a1% to a2%
                 testmonths = testmonths + 1
            next i%
            if thisyear% = testyear% then L19280
               testyear% = testyear% + 1%
            goto L19270                   /*  (EWD0001)  - End  */

L19280:     call "SHOSTAT" ("Report Generation In Progress")
            call "SETPRNT" ("HNY036", "        ", records%, 0%)
            rpt_time$ = " "             /* Get System Time            */
            call "TIME" (rpt_time$)
            select printer (134)
            l% = 0%
            page% = -1%                 /* Page Counter                */
            gosub print_params
            if sortby$ = "P" then gosub print_by_part                    ~
                             else gosub print_by_category
            if page% > 0% then L19520
               message$ = "rptReport HNYSLOW in background: No data met s~
        ~election criteria."
               if back$ = "Y" then tell_user
               call "ASKUSER" (2%, "*** NULL SET SELECTED ***",          ~
                 "Sorry, there are no parts in the HNYMASTR file that m"&~
                 "eet your criteria.", " ", "Press any PF key to acknow"&~
                 "ledge and continue.")

L19520:     gosub report_totals
            rpt_total = 0
            close printer
            call "SETPRNT" ("HNY036","        ",records%, 1%)
            if back$ <> "Y" then inputmode

            message$ = "rptReport HNYSLOW in background: Completed"
            call "TIME" (str(message$,44%,8%))

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
                  on fieldnr% gosub L20160,         /* Months Back      */~
                                    L20210,         /* #Months Supply   */~
                                    L20250,         /* Category Code    */~
                                    L20300,         /* Store no Range   */~
                                    L20350,         /* Part Range       */~
                                    L20520,         /* Fiscal/Calendar? */~
                                    L20400,         /* Sorting Sequence */~
                                    L20460          /* Print In Bckgrnd?*/
                     return
L20160:     REM Default/Enable for Months Back
                inpmessage$ = "Enter the oldest period of history to " & ~
                              "be included in average usage calculation."
                if firstmon$ = " " then firstmon$ = "190101"
                return
L20210:     REM Default/Enable for #Months Supply
                if supply$ = " " then supply$ = "01"
                inpmessage$ = "Enter the # of Periods Supply part must "&~
                              "have or exceed for inclusion on report."
                return
L20250:     REM Default/Enable for Category Code Range
                inpmessage$ = "Enter the Category Code Range To be " &   ~
                              "Included or 'ALL' for all Categories."
                if beg_catg$(1%) = " " then beg_catg$(1%) = "ALL"
                return
L20300:     REM Default/Enable for Store Number Range
                inpmessage$ = "Enter the Store Number Range To be " &    ~
                              "Included or 'ALL' for all Stores."
                if beg_store$(1%) = " " then beg_store$(1%) = "ALL"
                return
L20350:     REM Default/Enable for Part Range
                inpmessage$ = "Enter the Part Number Range To be " &     ~
                              "Included or 'ALL' for all Parts."
                if beg_part$(1%) = " " then beg_part$(1%) = "ALL"
                return
L20400:     REM Default/Enable for Sorting Sequence
                if sortby$ = " " then sortby$ = "C"
                inpmessage$ = "Enter 'P' to print by Part Number "  &    ~
                              "or 'C' to print by Category Code."
                return

L20460:     REM Default/Enable for Print In Background?
                if back$ = " " then back$ = "N"
                inpmessage$ = "Enter 'Y' to print by this report in"  &  ~
                              " background."
                return

L20520:     REM Default/Enable for Calendar or Fiscal?     FIS_CAL$
                if fis_cal$ = " " then fis_cal$ = "C"
                inpmessage$ = "Enter 'C' to report on Calendar values -"&~
                     "OR- 'F' to report on Fiscal values."
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
            u3% = 0%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  str(line2$,62%) = " HNYSLOW: " & str(cms2v$,,8%)
                  str(line2$,,50%) = "Report Selection Criteria"
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                  else                  init(hex(86)) lfac$()
                  on fieldnr% gosub L40220,         /* Months Back      */~
                                    L40220,         /* #Months Supply   */~
                                    L40220,         /* Part Range       */~
                                    L40220,         /* Store no Range   */~
                                    L40220,         /* Category Code    */~
                                    L40220,         /* Fiscal/Calendar? */~
                                    L40220,         /* Sorting Sequence */~
                                    L40220          /* Print In Bckgrnd?*/
                  goto L40280

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L40220:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return
L40280:
L40290:     accept                                                       ~
               at (01,02), "Print Slow Moving Inventory Report",         ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Oldest History (YYYYPP)",                    ~
               at (06,30), fac(lfac$(1%)), firstmon$            , ch(06),~
                                                                         ~
               at (07,02), "Periods Supply",                             ~
               at (07,30), fac(lfac$(2%)), supply$              , ch(02),~
                                                                         ~
               at (09,22), fac(hex(ac)), title$(1%)             , ch(25),~
               at (09,48), fac(hex(ac)), title$(2%)             , ch(25),~
                                                                         ~
               at (10,02), "Category Code",                              ~
               at (10,22), fac(lfac$(3%)), beg_catg$(1%)        , ch(04),~
               at (10,48), fac(lfac$(3%)), end_catg$(1%)        , ch(04),~
                                                                         ~
               at (11,02), "Store Number",                               ~
               at (11,22), fac(lfac$(4%)), beg_store$(1%)       , ch(03),~
               at (11,48), fac(lfac$(4%)), end_store$(1%)       , ch(03),~
                                                                         ~
               at (12,02), "Part Number",                                ~
               at (12,22), fac(lfac$(5%)), beg_part$(1%)        , ch(25),~
               at (12,48), fac(lfac$(5%)), end_part$(1%)        , ch(25),~
                                                                         ~
               at (14,02), "Calendar or Fiscal?",                        ~
               at (14,23), fac(lfac$(6%)), fis_cal$             , ch(01),~
                                                                         ~
               at (15,02), "Sorting Sequence?",                          ~
               at (15,23), fac(lfac$(7%)), sortby$              , ch(01),~
                                                                         ~
               at (16,02), "Print In Background?",                       ~
               at (16,23), fac(lfac$(8%)), back$                , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,02),                                               ~
                  "(1)Start Over",                                       ~
               at (23,20), fac(hex(8c)), pf4$                           ,~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65), fac(hex(84)), pf16$                          ,~
                                                                         ~
               keys(hex(000104050d0f10)),                                ~
               key (keyhit%)

               if keyhit% <> 13% then L40830
                  call "MANUAL" ("HNYSLOW")
                  goto L40290

L40830:        if keyhit% <> 15% then L40870
                  call "PRNTSCRN"
                  goto L40290

L40870: REM    IF FIELDNR% > 0% THEN RETURN
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
                  on fieldnr% gosub L50160,         /* Months Back      */~
                                    L50300,         /* #Months Supply   */~
                                    L50330,         /* Category Code    */~
                                    L50380,         /* Store no Range   */~
                                    L50430,         /* Part Range       */~
                                    L50560,         /* Fiscal/Calendar? */~
                                    L50480,         /* Sorting Sequence */~
                                    L50520          /* Print In Bckgrnd?*/
                  return
L50160:     REM Test Data for Months Back
                dt_temp$ = str(firstmon$,1%,6%) & "01"
                call "DATEOKC" (dt_temp$, 0%, errormsg$)
                     if errormsg$ = " " then L50220
                     errormsg$ = "Enter Year And Period, as YYYYPP"
                     return
L50220:         if str(firstmon$,1%,6%) < str(ccyymmdd$,1%,6%) then return
                     errormsg$ = "Must Be Before Current Year And Period"
                     return

L50300:     REM Test Data for #Months Supply
                call "NUMTEST" (supply$, 1, 99, errormsg$, 0.0, 0)
                return
L50330:     REM Test Data for Category Code Range
                call "TESTRNGE"   (beg_catg$(1%), end_catg$(1%),         ~
                                   beg_catg$(2%), end_catg$(2%),         ~
                                   errormsg$)
                return
L50380:     REM Test Data for Store Number Range
                call "TESTRNGE"   (beg_store$(1%), end_store$(1%),       ~
                                   beg_store$(2%), end_store$(2%),       ~
                                   errormsg$)
                return
L50430:     REM Test Data for Part Range
                call "TESTRNGE"   (beg_part$(1%), end_part$(1%),         ~
                                   beg_part$(2%), end_part$(2%),         ~
                                   errormsg$)
                return
L50480:     REM Test Data for Sorting Sequence
                if sortby$ = "C" or sortby$ = "P" then return
                errormsg$ = "Must be 'C' or 'P'"
                return
L50520:     REM Test Data for Background?
                if back$ <> "Y" then back$ = "N"
                return

L50560:     REM Test Data for Calendar or Fiscal?        FIS_CAL$
                if fis_cal$ <> "C" and fis_cal$ <> "F" then goto L50580
                     if fis_cal$ = "C" then file% = 1% else file% = 5%
                     return
L50580:         errormsg$ = "Must be 'C' or 'F'"
                return

        REM *************************************************************~
            *               R E P O R T   R O U T I N E S               *~
            *-----------------------------------------------------------*~
            * Report printing logic.                                    *~
            *************************************************************

        print_by_category  /* Primer */
            plowkey$ = beg_catg$(2%)
            call "PLOWALTS" (#2, plowkey$, 2%, 0%, f1%(2%))
            goto L55190

        print_by_part      /* Primer */
            plowkey$ = beg_part$(2%)
            call "PLOWALTS" (#2, plowkey$, 0%, 0%, f1%(2%))
            goto L55190

        main_read_loop
            call "READNEXT" (#2, f1%(2%))
L55190:         if f1%(2%) = 0% then return
            part$ = key(#2,0%)
            if part$ > beg_part$(2%) and part$ <= end_part$(2%) then L55240
                if sortby$ = "P" then return
                goto main_read_loop
L55240:     cat$ = key(#2,2%)
            if cat$ > beg_catg$(2%) and cat$ <= end_catg$(2%) then L55280
                if sortby$ = "C" then return
                goto main_read_loop
L55280:     plowkey$ = str(part$) & str(beg_store$(2%))
            get #2, using L55300, partd$
L55300:     FMT XX(25), CH(32)

        load_next_store
            store_on_hand, on_hand_value = 0
            call "PLOWALTS" (#3, plowkey$, 0%, 25%, f1%(3%))
                if f1%(3%) = 0% then main_read_loop
            store$ = str(plowkey$,26%,3%)
            if store$ > end_store$(2%) then main_read_loop
            gosub load_history
            if months > 0 then L55450  /* Any History Found? */
                str(readkey$,29%) = all(hex(ff))
                goto load_next_store

L55430:     call "PLOWALTS" (#3, plowkey$, 0%, 28%, f1%(3%))
                if f1%(3%) = 0% then L55540
L55450:     get #3, using L55460, this_qty, pending, cost
L55460:     FMT POS(69), PD(14,4), POS(109), PD(14,4), PD(14,4)
            quantity = this_qty - pending
            store_on_hand = store_on_hand + quantity
            on_hand_value = on_hand_value + quantity * cost
            goto L55430

L55540:     if store_on_hand <= 0 then load_next_store
            if amu > 0 then L55580
                supply = 9999999999
                goto L55590
L55580:     supply = round(store_on_hand/amu, 0)
L55590:     if supply >= monsupply then gosub print_report_line
            goto load_next_store

        load_history
            total, months, amu = 0
            convert firstmon$ to firstmon%
            if file% = 1% then               /* get Year from Date */ ~
               firstmon% = (firstmon% - mod(firstmon%,100))/100%
            readkey$ = str(part$) & str(store$)
            if file% = 1%                                                ~
                then readkey$ = readkey$ & bin(firstmon%,2%)             ~
                else readkey$ = readkey$ & bin(firstmon%,4%)
            if file% = 5% then gosub set_read       /*  (EWD0001)  */
            call "READ100" (#file%, readkey$, f1%(file%))
                if f1%(file%) <> 0% then L56061
L56050:     call "PLOWALTS" (#file%, readkey$, 0%, 28%, f1%(file%))
                if f1%(file%) = 0% then L56200
L56061:     if file% = 1%                                                ~
                then get #1 using L56080, year%, wdsu()                   ~
                else get #5 using L56081, year%, wdsu()
L56080:     FMT POS(29), BI(2), POS(127), 12*PD(14,4)
L56081:     FMT POS(29), BI(4), POS(137), 13*PD(14,4)

            if file% = 1% then goto L56100
				REM Get rid of month in BI(4) data
				year% = int( (year%/100) )
L56100:     REM Add Months To Totals...
            a1% = 1% : a2% = 12% : if file% = 5% then a2% = 12%
            if year% = startyear% then a1% = startmon%
            if year% = thisyear% then a2% = thismon%
            for i% = a1% to a2%
                total = total + wdsu(i%)
                months = months + 1
            next i%
            if year% < thisyear% then L56050

L56200:     REM Calculate Average Monthly Usage...
            if months = 0 then return
            if testmonths = 0 then return
            amu = total/testmonths
            return

        set_read                          /*  (EWD0001)  - Begin  */
            init(" ") readkey$, cmg$, savkey$
            testyear% = 0%
            testyear% = firstmon%
            testyear% = int((testyear%/100))
            convert testyear% to cmg$, pic(0000)
            cmg$ = str(cmg$,1%,4%) & "01"
            convert cmg$ to testyear%
            readkey$ = str(part$) & str(store$)
            readkey$ = readkey$ & bin(testyear%,4%)
         return                         /*  (EWD0001)  - End  */


        print_report_line
            if l% < 1% then gosub print_heading
            call "CONVERT" (store_on_hand, 0.2, print$(1%))
            call "CONVERT" (testmonths, 0.0, print$(2%))
            call "CONVERT" (amu, 0.2, print$(3%))
            call "CONVERT" (supply, 0.0, print$(4%))
            if supply = 9999999999 then print$(4%) = "   UNKNOWN"
            on_hand_value = round(on_hand_value,2)
            rpt_total = rpt_total + on_hand_value
            call "CONVERT" (on_hand_value, 2.2, print1$)

            if last_cat$ <> cat$ then L57200
            if last_part$ <> part$ then L57150
               REM Nothing Changed...
               print using L64290, " ", " ", " ", store$, print$(1),      ~
                                 print$(2), print$(3), print$(4), print1$
               goto L57240

L57150:     REM Part Changed...
               print using L64290, " ", part$, partd$, store$, print$(1), ~
                                 print$(2), print$(3), print$(4), print1$
               goto L57240

L57200:     REM Category Changed...
               print using L64290, cat$, part$, partd$, store$, print$(1),~
                                 print$(2), print$(3), print$(4), print1$

L57240:     l% = l% - 1%
            last_cat$ = cat$
            last_part$ = part$
            lines_printed% = lines_printed% + 1%
            return

        report_totals
            if l% < 6% then gosub print_heading
            print
            call "CONVERT" (rpt_total, 2.2, print_total$)
            print using L64320, lines_printed%, print_total$
            print
            rpt_time$ = " "             /* Get System Time            */
            call "TIME" (rpt_time$)
            print using L64440, rpt_time$
            return

        print_heading
            print page
            page% = page% + 1%
            print using L64060, date$, rpt_time$, company$
            print_title$ = "Slow Moving Inventory Report"
            call "FMTTITLE" (print_title$, " ", 12%)
            print using L64100, print_title$, page%
            if fis_cal$ = "C"                                            ~
                then print_title$ = "CALENDAR DATA"                      ~
                else print_title$ = "FISCAL DATA"
            if sortby$ = "P"                                             ~
                then print_title$ = print_title$ & " BY PART"            ~
                else print_title$ = print_title$ & " BY CATEGORY"
            call "FMTTITLE" (print_title$, " ", 12%)
            print using L64130, print_title$
            if end$ = "Y" then L62170
            print
            print using L64180, "#"
            print using L64210
            print using L64250
            l% = 48%
L62170:     last_cat$, last_part$ = all(hex(ff))
            return

        print_params
            end$ = "Y"
            gosub print_heading
L63022:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then L63040
                str(i$(), i%, 1%) = hex(20)
                goto L63022
L63040:     print skip(3%)
            print using L63210 : print

            print using L63250, "Oldest History", firstmon$
            print using L63250, "Periods Supply", supply$
            print : print using L63230
            print using L63250, "Category Code",beg_catg$(1%),            ~
                                                             end_catg$(1%)
            print using L63250, "Store Number", beg_store$(1%),           ~
                                                            end_store$(1%)
            print using L63250, "Part Number", beg_part$(1%),             ~
                                                            end_part$(1%)
            print
            print using L63250, "Calendar or Fiscal", fis_cal$, " "
            print using L63250, "Sorting Sequence", sortby$, " "
            print using L63250, "Print In Background", back$

            print : print using L63270
            end$ = " "
            return
     

L63210: %                                     ----------------- Report Se~
        ~lection Parameters -----------------
L63230: %                                                           FROM ~
        ~                     TO
L63250: %                                      #################### #####~
        ~#################### #########################
L63270: %                                     ---------------------------~
        ~------------------------------------

        REM *************************************************************~
            *                R E P O R T   F O R M A T S                *~
            *-----------------------------------------------------------*~
            *  Report format lines used by print statements.            *~
            *************************************************************

L64060: % ######## ########                  ############################~
        ~################################                     HNYSLOW:HNY0~
        ~36

L64100: %                                    ############################~
        ~################################                        PAGE:####

L64130: %                                    ############################~
        ~################################

L64180: %                                                                ~
        ~           Current   # Periods      Period     Periods     Value ~
        ~Of
L64210: %Cat.  Part Number               Part Description                ~
        ~ Store     On Hand  Hstry Data   Avg Usage      Supply      On Ha~
        ~nd

L64250: %====  ========================= ================================~
        ~  ===   ==========  ==========  ==========  ==========  =========~
        ~==
L64290: %####  ######################### ################################~
        ~  ###   ##########  ##########  ##########  ##########  #########~
        ~##
L64320: %Lines Printed: #########                                        ~
        ~                                  Total On Hand Value:  #########~
        ~##
L64440: %                                  * * * * *   E N D   O F   R E ~
        ~P O R T   @   ########   * * * * *

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
