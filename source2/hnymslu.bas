        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  H   H  N   N  Y   Y  M   M   SSS   L      U   U          *~
            *  H   H  NN  N   Y Y   MM MM  S      L      U   U          *~
            *  HHHHH  N N N    Y    M M M   SSS   L      U   U          *~
            *  H   H  N  NN    Y    M   M      S  L      U   U          *~
            *  R   H  N   N    Y    M   M   SSS   LLLLL   UUU           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYMSLU  - Prints last month used by type of usage.       *~
            *            Months Since Last Used, aka MSLU.              *~
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
            * 02/19/91 ! Added option to drive against HNYHSTRF.  ! LDJ *~
            * 03/01/91 ! Corrected misc. bugs and calculation of  ! GJM *~
            *          !   of periods since last used.            !     *~
            *          ! Resolves PRRs 11598, 11649, & 11856.     !     *~
            * 04/02/92 ! Minor mods for DEC Compatability.        ! JDH *~
            * 03/31/93 ! PRR 12843. Fixed problem with usage in   ! JDH *~
            *          !  future compared with current period open!     *~
            * 06/28/96 ! Add ccyymmdd date for extraction elements! DER *~
            *          ! Add temp holding date                    !     *~
            *          ! modified year%/period% to handle BI      !     *~
            * 09/02/97 !  and add all(hex(00)) to plowkey         ! RJH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            back$1,                      /* Background flag            */~
            begdates$(17)8,              /* Current Fiscal Calendar    */~
            begmm$2,                     /* Month of 1st fiscal period */~
            begyy$2,                     /* Year of 1st fiscal period  */~
            beg_catg$(2)4,               /* Beginning Category Code    */~
            beg_part$(2)25,              /* Beginning Part Number      */~
            beg_store$(2)3,              /* Beginning Store Number     */~
            box$(20)1,                   /* For Actvty Type Selections */~
            cat$4,                       /* Part Category Code         */~
            ccyymmdd$8,                  /* Date extraction            */~
            company$60,                  /* Company / Division Name    */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            end_catg$(2)4,               /* Ending Category Code       */~
            end_part$(2)25,              /* Ending Part Number         */~
            end_store$(2)3,              /* Ending Store Number        */~
            errormsg$79,                 /* Error message              */~
            file$8,                      /* Work File Name             */~
            fiscal_or_greg$1,            /* Fiscal or Gregorian History*/~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            last_cat$4,                  /* Catg code For Print Cntrl  */~
            last_part$25,                /* Part code For Print Cntrl  */~
            last_per_used$10,            /* Last Period Used           */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            lib$8,                       /* Work File Library          */~
            line2$79,                    /* Second Line of Screen Headr*/~
            message$78,                  /* Informational Message      */~
            minmons$3,                   /* Min inactive months for prn*/~
            mm$6,                        /* Month work variable        */~
            part$25,                     /* Part code                  */~
            partd$32,                    /* Part code description      */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            pf4$18,                      /* PF 4 Screen Literal        */~
            pf5$16,                      /* PF 5 Screen Literal        */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            print_title$60,              /* Report Sorted By Title     */~
            print$(4)10,                 /* Work Variable              */~
            print1$11,                   /* Work Variable              */~
            print3$11,                   /* Work Variable              */~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            rpt_time$8,                  /* Report Time                */~
            store$3,                     /* Store Number               */~
            sortby$1,                    /* Sorting Sequence, V or D   */~
            subt$(3)22,                  /* Screen Titles              */~
            ohstat$1,                    /* Types Of Onhand To Include */~
            tmp_date$10,                 /* temp formated date         */~
            title$(2)30,                 /* Screen Column Headings     */~
            userid$3,                    /* Current User Id            */~
            vol$6,                       /* Work File Volume           */~
            wdsu(20),                    /* Usage Data                 */~
            yy$4                         /* Year work variable         */

        dim f1%(05)                      /* = 1 if READ was successful */

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
            * #1  ! HNYHSTRY ! Inventory Usage History - Gregorian Cal. *~
            * #2  ! HNYMASTR ! Inventory Master File                    *~
            * #3  ! HNYQUAN  ! Inventory Quantity File                  *~
            * #4  ! SYSFILE2 ! SYSTEM INFO (DEFAULT PAY DATES)          *~
            * #5  ! HNYHSTRF ! Inventory Usage History - Fiscal Calendar*~
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
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select #5,  "HNYHSTRF",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  32

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1,  0%, 0%, 0%, " ")
            call "OPENCHCK" (#2,  0%, 0%, 0%, " ")
            call "OPENCHCK" (#3,  0%, 0%, 0%, " ")
            call "OPENCHCK" (#4,  0%, 0%, 0%, "REQUIRED")
            call "OPENCHCK" (#5,  0%, 0%, 0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$, "TT", back$)
            date$ = date
            call "DATEFMT" (date$, scratch%, ccyymmdd$)
            convert str(ccyymmdd$,5%,2%) to curmons%
            convert str(ccyymmdd$,1%,4%) to temp
            curmons% = temp * 12 + curmons%
            call "COMPNAME" (12%, company$, ret%)
            call "GETNAMES" addr(#2, file$, lib$, vol$)
            call "READFDR" addr(file$,lib$,vol$,0%,"RC",records%,ret%)
            REM Load Fiscal Year Data...
            call "READ100" (#4, "FISCAL DATES       ", f1%(4%))
            if f1%(4%) = 0% then exit_program
            get #4 using L09145, nperiods%, begdates$(), cur_per%
L09145:     FMT POS(21), BI(2), 17*CH(8), BI(2)
            tmp_date$ = begdates$(7%)
            call "DATFMTC" (tmp_date$, scratch%, ccyymmdd$)
            convert str(ccyymmdd$,1%,4%) to cur_yr%
            tmp_date$ = begdates$(1%)
            call "DATFMTC" (tmp_date$, scratch%, ccyymmdd$)
            begyy$ = str(ccyymmdd$,3%,2%)
            begmm$ = str(ccyymmdd$,5%,2%)

            if back$ = "B" then printing_in_background
            back$ = " "
            title$(1) = "From"  :  title$(2) = "To  "
            subt$(1) = "      Additions"
            subt$(2) = "     Withdrawals"
            subt$(3) = "      Adjustments"

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            pf4$="(4)Previous Field" : pf5$=" ": pf16$="(16)Exit Program"
            errormsg$ = " "
            call "ALLFREE"
            lines_printed% = 0

            for fieldnr% = 1 to 9
L10140:         gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10260
L10160:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10240
L10190:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10160
                         if fieldnr% = 1% then L10140
                         goto L10190
L10240:               if keyhit%  = 16 then       exit_program
                      if keyhit% <>  0 then       L10160
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
            pf16$ = "(16)Print Report"
            if back$ = "Y" then pf16$ = "(16)Submit Task"
            gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       edtpg1
            oldfield% = 0%
L11180:     fieldnr% = cursor%(1) - 4
            if fieldnr% < 1 or fieldnr% > 16 then edtpg1
            if fieldnr% = 3 or fieldnr% =  4 then edtpg1
            if fieldnr% = 9 or fieldnr% = 10 then edtpg1
            if fieldnr% = 8 and cursor%(2) > 25 then fieldnr% = 9%
            if cursor%(1)= 12 and cursor%(2) > 50 then fieldnr% = 10%
            if fieldnr% > 10 then fieldnr% = 11%
            if fieldnr% > 3 then fieldnr% = fieldnr% - 2
            if fieldnr% = oldfield% then edtpg1

            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       edtpg1
                  pf4$, pf5$, pf16$ = " "
L11300:     gosub'101(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11300
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11300
                  oldfield% = fieldnr%
                  goto L11180

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            if back$ <> "Y" then L19280
                call "READ101" (#4, "zHNYMSLU." & userid$, f1%(4))
                put #4, using L19140, "zHNYMSLU." & userid$, minmons$,    ~
                  ohstat$, beg_part$(), end_part$(), beg_store$(),       ~
                  end_store$(), beg_catg$(), end_catg$(), sortby$, back$,~
                  fiscal_or_greg$, str(box$())
                if f1%(4) = 0 then write #4  else rewrite #4
L19140:         FMT CH(20), 19*CH(25)
                call "TASKUP" ("ME", 0%)
                goto L65000 /* We're outa here */

                printing_in_background
                message$ = "rptReport HNYMSLU in background: Aborted."
                call "READ101" (#4, "zHNYMSLU." & userid$, f1%(4))
                     if f1%(4) = 0 then tell_user
                get #4, using L19140, errormsg$, minmons$, ohstat$,       ~
                  beg_part$(), end_part$(), beg_store$(), end_store$(),  ~
                  beg_catg$(), end_catg$(), sortby$, back$,              ~
                  fiscal_or_greg$, str(box$())
                delete #4

L19280:     REM Prepare for task at hand...
            convert minmons$ to minmons%
            call "SHOSTAT" ("Report Generation In Progress")
            call "SETPRNT" ("HNY039", "        ", records%, 0%)
            rpt_time$ = " "             /* Get System Time            */
            call "TIME" (rpt_time$)
            select printer (134)
            l% = 0
            page% = -1%                 /* Page Counter                */
            gosub print_params
            if sortby$ = "C" then gosub print_by_category                ~
                             else gosub print_by_part
            if page% > 0% then L19540
               message$ = "rptReport HNYMSLU in background: No data met s~
        ~election criteria."
               if back$ = "Y" then tell_user
               call "ASKUSER" (2%, "CHECK SELECTION PARAMETERS",         ~
                 "Sorry, no Parts eligible for printing were found",     ~
                 "using these selection parameters.",                    ~
                 "Press RETURN to change the parameters or exit.")
               goto L10000

L19540:     gosub report_totals
            close printer
            call "SETPRNT" ("HNY039","        ",records%, 1%)
            if back$ <> "Y" then inputmode

            message$ = "rptReport HNYMSLU in background: Completed"
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
                  on fieldnr% gosub L20170,         /* Min inactive mths*/~
                                    L20220,         /* On Hand Status   */~
                                    L20270,         /* Category Code    */~
                                    L20320,         /* Store no Range   */~
                                    L20370,         /* Part Range       */~
                                    L20420,         /* Sorting Sequence */~
                                    L20480,         /* Print In Bckgrnd?*/~
                                    L20531,         /* Fiscal or Gregori*/~
                                    L20540          /* Activity Types   */
                     return
L20170:     REM Default/Enable for Min inactive months for print
                inpmessage$ = "Enter period to limit in-activity " &     ~
                          "search.  Usage Outside this window is ignored"
                if minmons$ = " " then minmons$ = "0"
                return
L20220:     REM Default/Enable for On Hand Status
                inpmessage$ = "Use 'A' for All, 'Z' for parts with Ze" & ~
                              "ro or less onhand, 'P' for Positive only."
                if ohstat$ = " " then ohstat$ = "A"
                return
L20270:     REM Default/Enable for Category Code Range
                inpmessage$ = "Enter the Category Code Range To be " &   ~
                              "Included or 'ALL' for all Categories."
                if beg_catg$(1) = " " then beg_catg$(1) = "ALL"
                return
L20320:     REM Default/Enable for Store Number Range
                inpmessage$ = "Enter the Store Number Range To be " &    ~
                              "Included or 'ALL' for all Stores."
                if beg_store$(1) = " " then beg_store$(1) = "ALL"
                return
L20370:     REM Default/Enable for Part Range
                inpmessage$ = "Enter the Part Number Range To be " &     ~
                              "Included or 'ALL' for all Parts."
                if beg_part$(1) = " " then beg_part$(1) = "ALL"
                return
L20420:     REM Default/Enable for Sorting Sequence
                if sortby$ = " " then sortby$ = "C"
                inpmessage$ = "Enter 'P' to print by Part Number "  &    ~
                              "or 'C' to print by Category Code."
                return

L20480:     REM Default/Enable for Print In Background?
                if back$ = " " then back$ = "N"
                inpmessage$ = "Enter 'Y' to print by this report in"  &  ~
                              " background."
                return

L20531:     REM Default/Enable for Gregorian or Fiscal History?
                if fiscal_or_greg$ = " " then fiscal_or_greg$ = "G"
                inpmessage$ = "Enter 'G' to determine History from "  &  ~
                              "Gregorian Calendar or 'F' for Fiscal."
                return

L20540:     REM Default/Enable for Activity Types
                inpmessage$ = "Select Activity Types To Search For "  &  ~
                              " By 'X'ing The Appropriate Boxes"
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
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  line2$ = "Report Selection Criteria"
                  str(line2$,63%) = "HNYMSLU: " & str(cms2v$,,8%)
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                  else                  init(hex(86)) lfac$()
                  on fieldnr% gosub L40240,         /* Time Window      */~
                                    L40240,         /* On Hand Status   */~
                                    L40240,         /* Part Range       */~
                                    L40240,         /* Store no Range   */~
                                    L40240,         /* Category Code    */~
                                    L40240,         /* Sorting Sequence */~
                                    L40240,         /* Print In Bckgrnd?*/~
                                    L40240,         /* Fiscal or Greg.? */~
                                    L40240          /* Activity Types   */
                  goto L40300

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L40240:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return
L40300:
L40310:     accept                                                       ~
               at (01,02), "Print Months Since Last Activity Report",    ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Only Print Parts With More Than xxx Months Of~
        ~ Inactivity.",                                                   ~
               at (05,34), fac(lfac$( 1)), minmons$             , ch(03),~
                                                                         ~
               at (06,02), "Include Parts With Current On Hand Of:",     ~
               at (06,41), fac(lfac$( 2)), ohstat$              , ch(01),~
                                                                         ~
               at (08,22), fac(hex(ac)), title$(1)              , ch(25),~
               at (08,48), fac(hex(ac)), title$(2)              , ch(25),~
                                                                         ~
               at (09,02), "Category Code",                              ~
               at (09,22), fac(lfac$( 3)), beg_catg$(1)         , ch(04),~
               at (09,48), fac(lfac$( 3)), end_catg$(1)         , ch(04),~
                                                                         ~
               at (10,02), "Store Number",                               ~
               at (10,22), fac(lfac$( 4)), beg_store$(1)        , ch(03),~
               at (10,48), fac(lfac$( 4)), end_store$(1)        , ch(03),~
                                                                         ~
               at (11,02), "Part Number",                                ~
               at (11,22), fac(lfac$( 5)), beg_part$(1)         , ch(25),~
               at (11,48), fac(lfac$( 5)), end_part$(1)         , ch(25),~
                                                                         ~
               at (12,02), "Sorting Sequence?",                          ~
               at (12,22), fac(lfac$( 6)), sortby$              , ch(01),~
                                                                         ~
               at (12,26), "Print In Background?",                       ~
               at (12,47), fac(lfac$( 7)), back$                , ch(01),~
                                                                         ~
               at (12,50), "Fiscal or Gregorian History?",               ~
               at (12,79), fac(lfac$( 8)), fiscal_or_greg$      , ch(01),~
                                                                         ~
               at (14,03), "Look For:",                                  ~
               at (14,13), fac(hex(ac)), subt$(1)               , ch(21),~
                                                                         ~
               at (15,13), fac(lfac$( 9)), box$(1)              , ch(01),~
               at (15,15), "Direct Additions",                           ~
                                                                         ~
               at (16,13), fac(lfac$( 9)), box$(11)             , ch(01),~
               at (16,15), "PO receipt",                                 ~
                                                                         ~
               at (17,13), fac(lfac$( 9)), box$(7)              , ch(01),~
               at (17,15), "Job Completion",                             ~
                                                                         ~
               at (18,13), fac(lfac$( 9)), box$(6)              , ch(01),~
               at (18,15), "Job Byproduct",                              ~
                                                                         ~
               at (19,13), fac(lfac$( 9)), box$(14)             , ch(01),~
               at (19,15), "A/P transaction",                            ~
                                                                         ~
               at (20,13), fac(lfac$( 9)), box$(12)             , ch(01),~
               at (20,15), "QC to On-Hand",                              ~
                                                                         ~
               at (14,35), fac(hex(ac)), subt$(2)               , ch(22),~
               at (15,35), fac(lfac$( 9)), box$(5)              , ch(01),~
               at (15,37), "Direct Withdrawal",                          ~
                                                                         ~
               at (16,35), fac(lfac$( 9)), box$(8)              , ch(01),~
               at (16,37), "Kitted To Job",                              ~
                                                                         ~
               at (17,35), fac(lfac$( 9)), box$(10)             , ch(01),~
               at (17,37), "PO/QC to Rework",                            ~
                                                                         ~
               at (18,35), fac(lfac$( 9)), box$(13)             , ch(01),~
               at (18,37), "A/R transaction",                            ~
                                                                         ~
               at (19,35), fac(lfac$( 9)), box$(15)             , ch(01),~
               at (19,37), "Moved to Project",                           ~
                                                                         ~
               at (14,58), fac(hex(ac)), subt$(3)               , ch(22),~
               at (15,58), fac(lfac$( 9)), box$(4)              , ch(01),~
               at (15,60), "Physical adjustment",                        ~
                                                                         ~
               at (16,58), fac(lfac$( 9)), box$(2)              , ch(01),~
               at (16,60), "Inter-store movement",                       ~
                                                                         ~
               at (17,58), fac(lfac$( 9)), box$(9)              , ch(01),~
               at (17,60), "Job Scrap",                                  ~
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

               if keyhit% <> 13 then L41280
                  call "MANUAL" ("HNYMSLU")
                  goto L40310

L41280:        if keyhit% <> 15 then L41320
                  call "PRNTSCRN"
                  goto L40310

L41320: REM    IF FIELDNR% > 0% THEN RETURN
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
                  on fieldnr% gosub L50170,         /* Min inactive mths*/~
                                    L50390,         /* On Hand Status   */~
                                    L50440,         /* Category Code    */~
                                    L50500,         /* Store no Range   */~
                                    L50550,         /* Part Range       */~
                                    L50610,         /* Sorting Sequence */~
                                    L50660,         /* Print In Bckgrnd?*/~
                                    L50692,         /* Fiscal or Greg?  */~
                                    L50700          /* Activity Types   */
                  return
L50170:     REM Test Data for Min inactive months for print
                call "NUMTEST" (minmons$, 0, 999, errormsg$, 0.0, 0)
                return

L50390:     REM Test Data for On Hand Status...
                if pos("APZ" = ohstat$) <> 0 then return
                     errormsg$ = "Please Enter 'A', 'Z', or 'P'"
                     return

L50440:     REM Test Data for Category Code Range
                call "TESTRNGE"   (beg_catg$(1), end_catg$(1),           ~
                                   beg_catg$(2), end_catg$(2),           ~
                                   errormsg$)
                return

L50500:     REM Test Data for Store Number Range
                call "TESTRNGE"   (beg_store$(1), end_store$(1),         ~
                                   beg_store$(2), end_store$(2),         ~
                                   errormsg$)
                return
L50550:     REM Test Data for Part Range
                call "TESTRNGE"   (beg_part$(1), end_part$(1),           ~
                                   beg_part$(2), end_part$(2),           ~
                                   errormsg$)
                return

L50610:     REM Test Data for Sorting Sequence
                if sortby$ = "C" or sortby$ = "P" then return
                errormsg$ = "Must be 'C' or 'P'"
                return

L50660:     REM Test Data for Background?
                if back$ <> "Y" then back$ = "N"
                return

L50692:     REM Test Data for Fiscal or Gregorian History?
                if fiscal_or_greg$ <> "F" and fiscal_or_greg$ <> "G" then~
                   errormsg$ = "Must be 'G' or 'F'"
                return

L50700:     REM Test Data for Activity Types...
                if str(box$(),,14) <> " " then L50740
                     errormsg$ = "Please Select At Least One Type."
                     return
L50740:         for i% = 1 to 20
                     if box$(i%) <> " " then box$(i%) = "X"
                next i%
                return

        REM *************************************************************~
            *               R E P O R T   R O U T I N E S               *~
            *-----------------------------------------------------------*~
            * Report printing logic.                                    *~
            *************************************************************

        print_by_category  /* Primer */
            plowkey$ = beg_catg$(2)
            call "PLOWALTS" (#2, plowkey$, 2%, 0%, f1%(2))
            goto L55190

        print_by_part      /* Primer */
            plowkey$ = beg_part$(2)
            call "PLOWALTS" (#2, plowkey$, 0%, 0%, f1%(2))
            goto L55190

        main_read_loop  /* Read Next Part From HNYMASTR */
            call "READNEXT" (#2, f1%(2))
L55190:         if f1%(2) = 0% then return
            part$ = key(#2,0)
            if part$ > beg_part$(2) and part$ <= end_part$(2) then L55240
                if sortby$ = "P" then return
                goto main_read_loop
L55240:     cat$ = key(#2,2)
            if cat$ > beg_catg$(2) and cat$ <= end_catg$(2) then L55280
                if sortby$ = "C" then return
                goto main_read_loop
L55280:     plowkey$ = str(part$) & str(beg_store$(2))
            get #2, using L55300, partd$
L55300:     FMT XX(25), CH(32)

        load_next_store    /* Read in HNYQAUN data for this Part */
            store_on_hand, on_hand_value = 0
            call "PLOWALTS" (#3, plowkey$, 0%, 25%, f1%(3))
                if f1%(3) = 0% then main_read_loop
            store$ = str(plowkey$,26,3)
            if store$ > end_store$(2) then main_read_loop
            gosub load_history
            if mslu < 0 or mslu >= minmons% then L55460  /* In Range? */
                str(readkey$,29%) = all(hex(ff))
                goto load_next_store

            REM Read all lots for this store...
L55440:     call "PLOWALTS" (#3, plowkey$, 0%, 28%, f1%(3))
                if f1%(3) = 0% then L55540
L55460:     get #3, using L55470, this_qty, pending, cost
L55470:     FMT POS(69), PD(14,4), POS(109), PD(14,4), PD(14,4)
            quantity = this_qty - pending
            store_on_hand = store_on_hand + quantity
            on_hand_value = on_hand_value + quantity * cost
            goto L55440

L55540:     if ohstat$ = "A" then L55580
                if store_on_hand <= 0 and ohstat$ = "Z" then L55580
                if store_on_hand  > 0 and ohstat$ = "P" then L55580
                goto load_next_store
L55580:     if store_on_hand < 0 then on_hand_value = 0
            gosub print_report_line
            goto load_next_store

        load_history
            last_per_used$ = " "
            if fiscal_or_greg$ = "F" then load_fiscal_history
            oldest = 0 : mslu = -1
            readkey$ = str(part$) & str(store$) & all(hex(00))
L56040:     call "PLOWALTS" (#1, readkey$, 0%, 28%, f1%(1))
                if f1%(1) = 0% then L56180
            get #1 using L56070, year%, wdsu()
L56070:     FMT XX(28), BI(2), POS(415), 20*BI(1)
            if oldest = 0 then oldest = year%

            REM See if used in this year...
            for i% = 1 to 20
                if box$(i%) = " " then L56160  /* Not To Be Check For */
                if wdsu(i%) < 1 or wdsu(i%) > 12 then L56160
                mslu = max(year% * 12 + wdsu(i%), mslu)
L56160:     next i%
            goto L56040

L56180:     print3$ = " " : if mslu < 0 then return
            temp = mod(mslu,12)
            temp1 = mod(int(mslu/12),100%)
            if temp > 0 then L56185
                temp = 12 : temp1 = temp1 - 1
L56185:     call "CONVERT" (temp, 0.0, str(print3$,6,2))
            call "CONVERT" (temp1, 0.0, str(print3$,9,2))
            if str(print3$,9,1) = " " then str(print3$,9,1) = "0"
            str(print3$,8,1) = "/"
            if mslu >= 0 then mslu = curmons% - mslu
            return

        load_fiscal_history
            oldest = 0 : mslu = -1
            readkey$ = str(part$) & str(store$) & all(hex(00))
L56250:     call "PLOWALTS" (#5, readkey$, 0%, 28%, f1%(5))
                if f1%(5) = 0% then L56520
            get #5 using L56280, period%, wdsu()
L56280:     FMT XX(28), BI(4), POS(449), 20*BI(1)
            if oldest = 0 then oldest = period%
            year% = int( period%/100 )
            convert year%   to yy$, PIC(0000)
            convert period% to mm$, PIC(000000)
            yy$ = str(yy$, 3%, 2%)
            mm$ = str(mm$, 5%, 2%)

            REM See if used in this year...
            for i% = 1% to 20%
              if box$(i%) = " " then L56490  /* Not To Be Check For */
              if wdsu(i%) < 1 or wdsu(i%) > nperiods% then L56490
              save_mslu = mslu

              if yy$ <> begyy$ or mm$ <> begmm$ then L56380
                   work = max(0, cur_per% - wdsu(i%))
                   if mslu < 0 then mslu = work                          ~
                               else mslu = min(mslu, work)
                   goto L56450

L56380:       if yy$ <> begyy$ or mm$ = begmm$ or                        ~
                 cur_per% >  nperiods% then L56402
                   work = cur_per% - wdsu(i%) + nperiods%
                   if mslu < 0 then mslu = work                          ~
                               else mslu = min(mslu, work)
                   goto L56450

L56402:       if yy$ <> begyy$ or mm$ = begmm$ or                        ~
                 cur_per% <= nperiods% then L56410
                   work = cur_per% - wdsu(i%) + nperiods% - 13%
                   if mslu < 0 then mslu = work                          ~
                               else mslu = min(mslu, work)
                   goto L56450

L56410:       if yy$ = begyy$ then L56450
                   work = (cur_yr% - year%) * nperiods%                  ~
                                            + cur_per% - wdsu(i%)
                   if mslu < 0 then mslu = work                          ~
                               else mslu = min(mslu, work)

L56450:       if save_mslu = mslu then L56490
*       -*-*   IF NPERIODS% < 13%                                       ~
*       -*-*   THEN CONVERT WDSU(I%) - 1 TO LAST_PER_USED$, PIC(##)     ~
*       -*-*   ELSE CONVERT WDSU(I%) TO LAST_PER_USED$, PIC(##)
                convert wdsu(i%) to last_per_used$, pic(##)
                last_per_used$ = last_per_used$ & " / " & yy$ & mm$

L56490:     next i%

            goto L56250

L56520:     return

        print_report_line
            if l% < 1% then gosub print_heading
            print$() = " "
            call "CONVERT" (store_on_hand, 0.2, print$(1))
            print$(2) = "   19"
            if fiscal_or_greg$ = "F"                                     ~
                then call "CONVERT" (oldest, 0.0, str(print$(2),4,4))    ~
                else call "CONVERT" (oldest, 0.0, str(print$(2),4,4))
            if oldest = 0 then print$(2) = "   NONE"
            if mslu >= 0 then L57100
                print$(3) = "  No Usage"
                goto L57190
L57100:     call "CONVERT" (mslu, -0.001, print$(3))
            str(print$(3),4%) = str(print3$,4%)
            if fiscal_or_greg$ = "F" then                                ~
                 call "CONVERT" (mslu,  0.0, print$(3))
            print$(4) = last_per_used$
L57190:     call "CONVERT" (on_hand_value, 2.2, print1$)

            if last_cat$ <> cat$ then L57330
            if last_part$ <> part$ then L57280
               REM Nothing Changed...
               print using L64260, " ", " ", " ", store$, print$(1),      ~
                                 print$(2), print$(3), print$(4), print1$
               goto L57370

L57280:     REM Part Changed...
               print using L64260, " ", part$, partd$, store$, print$(1), ~
                                 print$(2), print$(3), print$(4), print1$
               goto L57370

L57330:     REM Category Changed...
               print using L64260, cat$, part$, partd$, store$, print$(1),~
                                 print$(2), print$(3), print$(4), print1$

L57370:     l% = l% - 1%
            last_cat$ = cat$
            last_part$ = part$
            lines_printed% = lines_printed% + 1
            return

        report_totals
            if l% < 4% then gosub print_heading
            rpt_time$ = " "             /* Get System Time            */
            call "TIME" (rpt_time$)
            print
            print using L64290, rpt_time$
            print
            print using L64310, lines_printed%
            return

        print_heading
            print page
            page% = page% + 1%
            print using L64060, date$, rpt_time$, company$
            print_title$ = "Months Since Last Used Report"
            if fiscal_or_greg$ = "F" then                                ~
               print_title$ = "Periods Since Last Used Report"
            call "FMTTITLE" (print_title$, " ", 12%)
            print using L64100, print_title$, page%
            if sortby$ = "P" then print_title$ = "BY PART"               ~
                             else print_title$ = "BY CATEGORY"
            call "FMTTITLE" (print_title$, " ", 12%)
            print using L64130, print_title$
            if end$ = "Y" then L62170
            print
            if fiscal_or_greg$ = "G" then L62130
               print using L64182
               print using L64212
               print using L64252
               goto L62160
L62130:     print using L64160
            print using L64190
            print using L64230
L62160:     l% = 50%
L62170:     last_cat$, last_part$ = all(hex(ff))
            return

        print_params
            end$ = "Y"
            gosub print_heading
            tran(i$(), hex(208c2084208620ac))replacing
            print skip(3)
            print using L63280 : print

            print using L63300
            print using L63320, "Min Months For Print", minmons$
            print using L63320, "Onhand Status", ohstat$
            print using L63320, "Category Code",beg_catg$(1), end_catg$(1)
            print using L63320, "Store Number", beg_store$(1),            ~
                                                            end_store$(1)
            print using L63320, "Part Number", beg_part$(1), end_part$(1)
            print using L63320, "Sorting Sequence", sortby$, " "
            print using L63320, "Print In Background", back$
            print using L63320, "Gregorian or Fiscal", fiscal_or_greg$
            print
            print using L63340, box$(1),  box$(5),  box$(4)
            print using L63360, box$(11), box$(8),  box$(2)
            print using L63380, box$(7),  box$(10), box$(9)
            print using L63400, box$(6),  box$(13)
            print using L63420, box$(14), box$(15)
            print using L63440, box$(12)

            print : print using L63460
            end$ = " "
            return

L63280: %                                     ----------------- Report Se~
        ~lection Parameters ----------------------------------------------
L63300: %                                                           FROM ~
        ~                     TO
L63320: %                                      #################### #####~
        ~#################### #########################
L63340: %                                                 Look For: # Dir~
        ~ect Additions     # Direct Withdrawal      # Physical adjustment
L63360: %                                                           # PO ~
        ~receipt           # Kitted To Job          # Inter-store movement
L63380: %                                                           # Job~
        ~ Completion       # PO/QC to Rework        # Job Scrap
L63400: %                                                           # Job~
        ~ Byproduct        # A/R transaction
L63420: %                                                           # A/P~
        ~ transaction      # Moved to Project
L63440: %                                                           # QC ~
        ~to On-Hand
L63460: %                                     ---------------------------~
        ~-----------------------------------------------------------------

        REM *************************************************************~
            *                R E P O R T   F O R M A T S                *~
            *-----------------------------------------------------------*~
            *  Report format lines used by print statements.            *~
            *************************************************************

L64060: % ######## ########                  ############################~
        ~################################                     HNYMSLU:HNY0~
        ~39

L64100: %                                    ############################~
        ~################################                        PAGE:####

L64130: %                                    ############################~
        ~################################

L64160: %                                                                ~
        ~           Current    Oldest    Mnths Since                Value ~
        ~Of
L64182: %                                                                ~
        ~           Current    Oldest  Periods Since    Last        Value ~
        ~Of
L64190: %Cat.  Part Number               Part Description                ~
        ~ Store     On Hand  Hstry Data   Last Used                  On Ha~
        ~nd
L64212: %Cat.  Part Number               Part Description                ~
        ~ Store     On Hand  Hstry Data   Last Used  Period Used     On Ha~
        ~nd

L64230: %====  ========================= ================================~
        ~  ===   ==========  ==========  ==========              =========~
        ~==
L64252: %====  ========================= ================================~
        ~  ===   ==========  ==========  ========== ============ =========~
        ~==
L64260: %####  ######################### ################################~
        ~  ###   ##########  ##########  ##########  ##########  #########~
        ~##
L64290: %                                  * * * * *   E N D   O F   R E ~
        ~P O R T   (@ ########)  * * * * *
L64310: %Lines Printed: #########

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
