        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  PPPP   IIIII  PPPP   BBBB   Y   Y  W   W  EEEEE  K  K    *~
            *  P   P    I    P   P  B   B   Y Y   W   W  E      K K     *~
            *  PPPP     I    PPPP   BBBB     Y    W W W  EEEE   KK      *~
            *  P        I    P      B   B    Y    WW WW  E      K K     *~
            *  P      IIIII  P      BBBB     Y    W   W  EEEEE  K  K    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PIPBYWEK - List summary PIP information for selected      *~
            *            parts, broke out into 8 weekly buckets, then   *~
            *            two 28 day buckets, a prior, and future bucket.*~
            *            Show net inventory position for each period,   *~
            *            and optionally the aproximate inventory values.*~
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
            * 10/06/89 ! Original                                 ! HES *~
            * 08/21/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            back$1,                      /* Background flag            */~
            btotal(11),                  /* For Totaling up da datas   */~
            btype$(25)2,                 /* For segragating da datas   */~
            bucket(25,11),               /* For rounding up da datas   */~
            bucket$(11)8,                /* For printing da datas      */~
            condition$1,                 /* Additional selection criter*/~
            conddescr$42,                /* Description of condition   */~
            company$60,                  /* Company / Division Name    */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            dates$(11)8,                 /* Date for report title      */~
            dates%(11),                  /* Date for indexing          */~
            dow$9,                       /* Day of week                */~
            end$1,                       /* Work Variable              */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            message$78,                  /* Informational Message      */~
            part$(2,2)25,                /* Part Code Range            */~
            part$29,                     /* Part code                  */~
            pcat$4,                      /* Part Category              */~
            pcat$(2,2)4,                 /* Part Category Range        */~
            pdescr$32,                   /* Part Description           */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            pf4$18,                      /* PF 4 Screen Literal        */~
            pf5$16,                      /* PF 5 Screen Literal        */~
            print_title$60,              /* Report Sorted By Title     */~
            prtcost$1,                   /* Print approximate Inv value*/~
            piptype$2,                   /* PIP in/out tpye identifer  */~
            pven$(2,2)9,                 /* Primary Vendor Range       */~
            pven$9,                      /* Primary Part Supplier Code */~
            qtyoh$7,                     /* Quantity On Hand           */~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            rpt_time$8,                  /* Report Time                */~
            ss$6,                        /* Safety Stock Level         */~
            title$(2)30,                 /* Screen Column Headings     */~
            total(11),                   /* For Totaling up da datas   */~
            type$3,                      /* Part Type                  */~
            type$(2,2)3,                 /* Part Type Range            */~
            unitcost$12,                 /* Formated Unit Cost         */~
            userid$3                     /* Current User Id            */

        dim f1%(64)                      /* = 1 if READ was successful */

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
            * #1  ! PIPIN    ! Planned Inventory Additions Detail File  *~
            * #2  ! HNYMASTR ! Inventory Master File                    *~
            * #3  ! WORKFILE ! Work file for screen greasing            *~
            * #4  ! SYSFILE2 ! System Info (Default Pay Dates)          *~
            * #5  ! CATEGORY ! Part Category File                       *~
            * #6  ! VENDOR   ! Vendor Master File                       *~
            * #8  ! HNYQUAN  ! Inventory Quantities File                *~
            * #9  ! PIPOUT   ! Planned inventory use detail record      *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "PIPIN",                                         ~
                        varc,     indexed,  recsize =   60,              ~
                        keypos =   30, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  48

            select #2,  "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup

            select #3,  "WORKFILE",                                      ~
                        varc,     indexed,  recsize =   50,              ~
                        keypos = 1, keylen = 2

            select #4,  "SYSFILE2",                                      ~
                        varc, indexed, recsize = 500,                    ~
                        keypos = 1, keylen = 20

            select #5,  "CATEGORY",                                      ~
                        varc,     indexed,  recsize =   200,             ~
                        keypos  =   1, keylen = 4

            select #6,  "VENDOR",                                        ~
                        varc,     indexed,  recsize =   600,             ~
                        keypos = 1, keylen =  9,                         ~
                        alt key  1, keypos = 10, keylen = 30, dup

            select #8,  "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =   650,             ~
                        keypos =   17, keylen =  44

            select #9,  "PIPOUT",                                        ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =   20, keylen =  37

            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1,  0%, 0%, 0%, " ")
            call "OPENCHCK" (#2,  0%, 0%, 0%, " ")
            call "WORKOPEN" (#3, "IO", 10%, 1%)
            call "OPENCHCK" (#4,  0%, 0%, 0%, " ")
            call "OPENCHCK" (#5,  0%, 0%, 0%, " ")
            call "OPENCHCK" (#6,  0%, 0%, 0%, " ")
            call "OPENCHCK" (#8,  0%, 0%, 0%, " ")
            call "OPENCHCK" (#9,  0%, 0%, 0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$, "TT", back$)
            date$ = date
            call "DATEFMT" (date$)
            call "COMPNAME" (12%, company$, 0%)
            call "READ100" (#4, "MONTHS OPEN", f1%(4))
                if f1%(4) <> 0% then L09190
                call "ASKUSER" (0%, "Sorry",                             ~
                            "Can't Find Months Open record in SYSFILE2.",~
                            "Press (RETURN) To Exit.", " ")
                goto L65000
L09190:     if back$ = "B" then printing_in_background
            title$(1) = "From"  :  title$(2) = "To  "

            REM Available Sort Options...
            write #3 using L09280,"1","All Parts In Range"
            write #3 using L09280,"2","All Parts With Activity"
            write #3 using L09280,"3","Unresolved Safety Stock Intrusions"
            write #3 using L09280,"4","Planned ADDS With No Planned USES"
            write #3 using L09280,"5","Negative PIP In At Lease One Period"
            write #3 using L09280,"6",                                     ~
                               "Inactive Parts (OH with no Planned Uses)"
L09280:     FMT CH(2), CH(40)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            errormsg$, inpmessage$, part$(), pcat$(), type$(), pven$(),  ~
            back$, condition$, conddescr$, prtcost$ = " "
            lines_printed% = 0%

            for fieldnr% = 1 to 7
L10120:         gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10260
                pf4$="(4)Previous Field" : pf16$="(16)EXIT PROGRAM"
                if fieldnr% = 1% then pf4$ = " " else pf16$ = " "
L10160:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10240
L10190:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10160
                         if fieldnr% = 1% then L10120
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
            pf16$ = "(16)PRINT REPORT"
            if back$ = "Y" then pf16$ = "(16)SUBMIT TASK"
            gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       edtpg1
            oldfield% = 0%
L11180:     fieldnr% = cursor%(1) - 5%
            if fieldnr% < 1 or fieldnr% > 8% then edtpg1
            if fieldnr% = 5 then edtpg1
            if fieldnr% > 5 then fieldnr% = fieldnr% - 1%
            if fieldnr% = oldfield% then edtpg1

            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       edtpg1
                  pf4$, pf5$, pf16$ = " "
L11270:     gosub'101(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11270
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11270
                  oldfield% = fieldnr%
                  goto L11180

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            if back$ <> "Y" then L19260
                call "READ101" (#4, "zPIPBYWEK." & userid$, f1%(4))
                put #4, using L19130, "zPIPBYWEK." & userid$, part$(),    ~
                              str(pcat$()), str(type$()), pven$(),       ~
                              condition$, prtcost$, back$, conddescr$,   ~
                              str(conddescr$,26)
                if f1%(4) = 0 then write #4  else rewrite #4
L19130:         FMT CH(20), 19*CH(25)
                call "TASKUP" ("ME", 0%)
                goto inputmode  /*That all there is to do this instance*/

                printing_in_background
                message$ = "rptReport PIPBYWEK in background: Aborted."
                call "READ101" (#4, "zPIPBYWEK." & userid$, f1%(4))
                     if f1%(4) = 0 then tell_user
                get #4, using L19130, errormsg$, part$(), str(pcat$()),   ~
                      str(type$()), pven$(), condition$, prtcost$, back$,~
                      conddescr$, str(conddescr$,26)
                delete #4

L19260:     REM Prepare for task at hand...
            call "DATE" addr("G+", date, -1%, str(dates$(1%),,6), u3%)
            dates$(2%) = date : dates$(11%) = "20991231"
            call "DATECONV" (dates$(11%))
            for i% = 0% to 6%
                call "DATE" addr("G+", date, i%, str(dates$(2%),,6), u3%)
                call "DATE" addr("GD", str(dates$(2%),,6), dow$, u3%)
                if dow$ = "SUNDAY" then L19340
            next i%
L19340:     for i% = 3% to 8%
                call "DATE" addr("G+", str(dates$(i%-1%),,6), 7%,        ~
                                       str(dates$(i%),,6), u3%)
            next i%
            for i% = 9% to 10%
                call "DATE" addr("G+", str(dates$(i%-1%),,6), 28%,       ~
                                       str(dates$(i%),,6), u3%)
            next i%
            for i% = 1% to 11%
                call "PIPINDEX" (#4, dates$(i%), dates%(i%), 0%)
                call "DATEFMT" (dates$(i%))
            next i%
            call "SHOSTAT" ("Report Generation In Progress")
            call "SETPRNT" ("PIP004", "        ", 0%, 0%)
            rpt_time$ = " "             /* Get System Time            */
            call "TIME" (rpt_time$)
            select printer (134)
            l% = 0
            page% = -1%                 /* Page Counter               */
            gosub print_params
            gosub print_heading
            gosub print_datas
            if page% > 0% then L19670
               message$ = "rptReport PIPBYWEK in background: No data met ~
        ~selection criteria."
               if back$ = "Y" then tell_user
               k% = 2%
               call "ASKUSER" (k%, "INVALID SELECTION PARAMETERS",       ~
                 "Sorry, no Parts eligible for Printing",                ~
                 "were found using the given selection parameters.",     ~
                 "Press RETURN to change the parameters or exit.")
               goto edtpg1

L19670:     gosub report_totals
            close printer
            call "SETPRNT" ("PIP004", " ", 0%, 1%)
            if back$ <> "Y" then inputmode

            message$ = "rptReport PIPBYWEK in background: Completed"
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
                  on fieldnr% gosub L20160,         /* Part Number Range*/~
                                    L20220,         /* Category Range   */~
                                    L20280,         /* Type Range       */~
                                    L20350,         /* Vendor Range     */~
                                    L20410,         /* Condition        */~
                                    L20460,         /* Print approx cost*/~
                                    L20510          /* Print In Bckgrnd?*/
                     return
L20160:     REM Default/Enable for Part Number Range
                inpmessage$ = "Enter the Part Number range to be " &     ~
                              "included or 'ALL' for all parts."
                if part$(1,1)=" "and part$(1,2)=" " then part$(1,1)="ALL"
                return

L20220:     REM Default/Enable for Part Category Range
                inpmessage$ = "Enter the Part Category range to be " &   ~
                              "included or 'ALL' for all categories."
                if pcat$(1,1)=" "and pcat$(1,2)=" " then pcat$(1,1)="ALL"
                return

L20280:     REM Default/Enable for Part Type Range
                inpmessage$ = "Enter the Part Type range to be " &       ~
                              "included or 'ALL' for all types."
                if type$(1,1) <> " " or type$(1,2) <> " " then return
                   type$(1,1) = "001" : type$(1,2) = "499"
                   return

L20350:     REM Default/Enable for Vendor Code Range
                inpmessage$ = "Enter the Primary Vendor range to be " &  ~
                              "included or 'ALL' for any vendors."
                if pven$(1,1)=" "and pven$(1,2)=" " then pven$(1,1)="ALL"
                return

L20410:     REM Default/Enable for Condition Flag
                inpmessage$ = "Enter Condition Flag.  Blank & RET" &     ~
                              "URN to see list of choices."
                return

L20460:     REM Default/Enable for Print approx cost
                inpmessage$ = "Enter 'Y' to print the approximate " &    ~
                              "value along with the inventory levels."
                return

L20510:     REM Default/Enable for Print In Background?
                if back$ = " " then back$ = "N"
                inpmessage$ = "Enter 'Y' to print by this report in"  &  ~
                              " background."
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
                  str(line2$,62%) = "PIPBYWEK: " & str(cms2v$,,8%)
                  str(line2$,,50) = "Report Selection Criteria"
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                  else                  init(hex(86)) lfac$()
                  on fieldnr% gosub L40230,         /* Part Number Range*/~
                                    L40230,         /* Category Range   */~
                                    L40230,         /* Type Range       */~
                                    L40230,         /* Vendor Range     */~
                                    L40230,         /* Condition        */~
                                    L40230,         /* Print approx cost*/~
                                    L40230          /* Print In Bckgrnd?*/
                  goto L40290

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L40230:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return
L40290:
L40300:     accept                                                       ~
               at (01,02), "Inventory Summary Status Report",            ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,22), fac(hex(ac)), title$(1)              , ch(25),~
               at (05,48), fac(hex(ac)), title$(2)              , ch(25),~
                                                                         ~
               at (06,02), "Part Number",                                ~
               at (06,22), fac(lfac$( 1)), part$(1,1)           , ch(25),~
               at (06,48), fac(lfac$( 1)), part$(1,2)           , ch(25),~
                                                                         ~
               at (07,02), "Part Category",                              ~
               at (07,22), fac(lfac$( 2)), pcat$(1,1)           , ch(04),~
               at (07,48), fac(lfac$( 2)), pcat$(1,2)           , ch(04),~
                                                                         ~
               at (08,02), "Part Type",                                  ~
               at (08,22), fac(lfac$( 3)), type$(1,1)           , ch(03),~
               at (08,48), fac(lfac$( 3)), type$(1,2)           , ch(03),~
                                                                         ~
               at (09,02), "Primary Vendor",                             ~
               at (09,22), fac(lfac$( 4)), pven$(1,1)           , ch(09),~
               at (09,48), fac(lfac$( 4)), pven$(1,2)           , ch(09),~
                                                                         ~
               at (11,02), "Condition Flag",                             ~
               at (11,26), fac(lfac$( 5)), condition$           , ch(01),~
               at (11,30), fac(hex(8c))  , conddescr$           , ch(42),~
                                                                         ~
               at (12,02), "Print Approx. Costs?",                       ~
               at (12,26), fac(lfac$( 6)), prtcost$             , ch(01),~
                                                                         ~
               at (13,02), "Print In Background?",                       ~
               at (13,26), fac(lfac$( 7)), back$                , ch(01),~
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

               if keyhit% <> 13 then L40830
                  call "MANUAL" ("PIPBYWEK")
                  goto L40300

L40830:        if keyhit% <> 15 then L40870
                  call "PRNTSCRN"
                  goto L40300

L40870:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50170,         /* Part Number Range*/~
                                    L50220,         /* Category Range   */~
                                    L50270,         /* Type Range       */~
                                    L50320,         /* Vendor Range     */~
                                    L50370,         /* Condition        */~
                                    L50430,         /* Print approx cost*/~
                                    L50480          /* Print In Bckgrnd?*/
                  return

L50170:     REM Test Data for Part Number Range...
                call "TESTRNGE" (part$(1,1), part$(1,2), part$(2,1),     ~
                                 part$(2,2), errormsg$, #2)
                return

L50220:     REM Test Data for Part Category Range...
                call "TESTRNGE" (pcat$(1,1), pcat$(1,2), pcat$(2,1),     ~
                                 pcat$(2,2), errormsg$, #5)
                return

L50270:     REM Test Data for Part Type Range...
                call "TESTRNGE" (type$(1,1), type$(1,2), type$(2,1),     ~
                                 type$(2,2), errormsg$)
                return

L50320:     REM Test Data for Primary Vendor Range...
                call "TESTRNGE" (pven$(1,1), pven$(1,2), pven$(2,1),     ~
                                 pven$(2,2), errormsg$, #6)
                return

L50370:     REM Test Data for Special conditions...
                call "GETCODE" (#3, condition$,conddescr$,1%,0.48,f1%(3))
                     if f1%(3) <> 0 then return
                errormsg$ = "Invalid Condition Flag.  Please Re-select."
                return

L50430:     REM Test Data for Print approx cost
                if pos("YN" = prtcost$) > 0% then return
                   errormsg$ = "Please Enter 'Y' or 'N'"
                   return

L50480:     REM Test Data for Background?
                if back$ <> "Y" then back$ = "N"
                return

        REM *************************************************************~
            *               R E P O R T   R O U T I N E S               *~
            *-----------------------------------------------------------*~
            * Report printing logic.                                    *~
            *************************************************************

        print_datas
            hits% = 0% : mat total = zer
            call "PLOWALTS" (#2, part$(2,1), 0%, 0%, f1%(2))
               goto L55130

        main_read_loop
            call "READNEXT" (#2, f1%(2))
L55130:         if f1%(2) = 0% then return
            get #2, using L55150, part$, pdescr$, pcat$, pven$, type$, ss
L55150:     FMT CH(25), CH(32), POS(90), CH(4), POS(102), CH(9),         ~
                POS(180), CH(3), POS(318), PD(14,4)
            if part$ >  part$(2,2) then return
            if type$ >  type$(2,2) then main_read_loop
            if type$ <= type$(2,1) then main_read_loop
            if pcat$ >  pcat$(2,2) then main_read_loop
            if pcat$ <= pcat$(2,1) then main_read_loop
            if pven$ >  pven$(2,2) then main_read_loop
            if pven$ <= pven$(2,1) then main_read_loop
            if condition$ = "3" and ss = 0 then main_read_loop
            mat btotal = zer  : mat bucket = zer
            gosub get_onhand  : call "CONVERT" (qtyoh, -0.01, qtyoh$)
            gosub scan_for_ins
            gosub scan_for_outs
            btotal(1%) = btotal(1%) + qtyoh
            for i% = 1% to 11%
                if i% > 1% then btotal(i%) = btotal(i%) + btotal(i%-1%)
            next i%
            if condition$ <> "1" then L55380
               REM All Parts In Range
               if ins% + outs% > 0% then L55610
               if qtyoh <> 0 then L55610
               goto main_read_loop
L55380:     if condition$ <> "2" then L55420
               REM All Parts With Activity
               if ins% + outs% > 0% then L55610
               goto main_read_loop
L55420:     if condition$ <> "3" then L55460
               REM Safety Stock Intrusions Only
               if btotal(11) < ss then L55610
               goto main_read_loop
L55460:     if condition$ <> "4" then L55500
               REM Planned Adds With No Planned Uses
               if ins% > 0  and outs% = 0% then L55610
               goto main_read_loop
L55500:     if condition$ <> "5" then L55560
               REM Negative PIP In At Lease One Week
               for i% = 1% to 11%
                 if btotal(i%) < 0 then L55610
               next i%
               goto main_read_loop
L55560:     if condition$ <> "6" then L55610
               REM Inactive parts only
               if outs% = 0% and qtyoh > 0 then L55610
               goto main_read_loop

L55610:     REM Got a keeper, lets print it...
            call "CONVERT" (ss, -0.01, ss$)
            gosub format_and_print
            hits% = hits% + 1%
            gosub part_totals
            goto main_read_loop

        get_onhand
            REM Determnine current On Hand...
            readkey$ = part$ : qtyoh, unitcost = 0 : qtyoh$ = " "
L60030:     call "PLOWNEXT" (#8, readkey$, 25%, f1%(8%))
                if f1%(8%) = 0% then return
            if str(readkey$,26,3) > "999" then return
            get #8, using L60070, quan, work
L60070:     FMT POS(69), PD(14,4), POS(117), PD(14,4)
            qtyoh = qtyoh + quan
            if work > unitcost then unitcost = work
            goto L60030

        scan_for_ins
            btype$() = " " : ins% = 0% : nextavail% = 0%
            REM Accumulate quantities planned in...
            readkey$ = all(hex(00))
            str(readkey$,,25) = part$
L60170:     call "PLOWALTS" (#1, readkey$, 1%, 25%, f1%(1))
                 if f1%(1) = 0% then return
            piptype$ = str(readkey$,30,2)
            call "BCKPREFX" (piptype$, errormsg$)
            if errormsg$ = " " then piptype$ = "SO"
            search btype$() = str(piptype$) to cursor%() step 2
            ptr% = (cursor%(1)+1)/2
            if ptr% <> 0% then L60280
               nextavail% = min(25%, nextavail% + 1%)
               ptr% = nextavail%
               btype$(ptr%) = piptype$
L60280:     get #1, using L60290, date%, quan
L60290:     FMT POS(26), BI(4), XX(19), PD(14,4)
            gosub age_data
            ins% = ins% + 1%
            goto L60170

        scan_for_outs
            outs% = 0% : f% = (nextavail% * 2%) + 1%
            REM Accumulate quantities planned out...
            readkey$ = all(hex(00))
            str(readkey$,,25) = part$
L60390:     call "PLOWALTS" (#9, readkey$, 1%, 25%, f1%(9%))
                 if f1%(9%) = 0% then return
            get #9, using L60420, date%, quan
L60420:     FMT POS(45), BI(4), POS(57), PD(14,4)
*          IF QUAN < 0 THEN 60350   /* Skip Tool Returns */
            piptype$ = str(key(#9,0),,2)
            call "BCKPREFX" (piptype$, errormsg$)
            if errormsg$ = " " then piptype$ = "SO"
            search str(btype$(),f%)=str(piptype$) to cursor%() step 2
            ptr% = (cursor%(1)+1)/2
            if ptr% <> 0% then L60540
               nextavail% = min(25%, nextavail% + 1%)
               ptr% = nextavail%
               btype$(ptr%) = piptype$
               goto L60550
L60540:     ptr% = ptr% + (f%-1%)/2%
L60550:     quan = quan * (-1)
            gosub age_data
            outs% = outs% + 1%
            goto L60390

        age_data
            for i% = 1% to 10%
                if date% <= dates%(i%) then L60650
            next i%
            i% = 11%
L60650:     bucket(ptr%, i%) = bucket(ptr%, i%) + quan
            btotal(i%) = btotal(i%) + quan
            return

        format_and_print
            ptr% = 0%
            if lines_printed% + 1% > l% then gosub print_heading
            for j% = 1% to 25%
                bucket$() = " " : all0% = 0%
                for i% = 1% to 11%
                    call "CONVERT" (bucket(j%, i%), 0.0, bucket$(i%))
                    if bucket(j%, i%) <> 0 then all0% = 1%
                next i%
                if j% = 24% and ptr% = 0% then L60800  /* Print zeroes */
                if j% = 25% and ptr% < 2% then L60800  /* Print zeroes */
                if all0% = 0% then L60910
L60800:            if lines_printed% > l% then gosub print_heading
                   if ptr% > 0% then part$ = " "
                   if ptr% = 1% then                                     ~
                      put part$, using L60920, type$, qtyoh$, ss$
                   if all0% = 0% then btype$(j%) = " "
                   print using L64240, part$, btype$(j%), bucket$(1),     ~
                         bucket$(2), bucket$(3), bucket$(4), bucket$(5), ~
                         bucket$(6), bucket$(7), bucket$(8), bucket$(9), ~
                         bucket$(10), bucket$(11)
                   lines_printed% = lines_printed% + 1%
                   ptr% = ptr% + 1%
L60910:     next j%
L60920:     %Type:### OH:####### SS:######
            return

        part_totals
            for i% = 1% to 11%
                call "CONVERT" (btotal(i%), 0.0, bucket$(i%))
            next i%
            print using L64210
            print using L64270, pdescr$, bucket$(1),                      ~
                  bucket$(2), bucket$(3), bucket$(4), bucket$(5),        ~
                  bucket$(6), bucket$(7), bucket$(8), bucket$(9),        ~
                  bucket$(10), bucket$(11)
            lines_printed% = lines_printed% + 2%
            if prtcost$ <> "Y" then L61190
               unitcost$ = "("
               call "CONVERT" (unitcost, -2.4, str(unitcost$,2))
               unitcost$ = unitcost$ & "ea)"
               for i% = 1% to 11%
                   extcost = btotal(i%) * unitcost
                   call "CONVERT" (extcost, 0.0, bucket$(i%))
                   total(i%) = total(i%) + extcost
               next i%
               print using L64270, "Approximate Value " & unitcost$,      ~
                     bucket$(1), bucket$(2), bucket$(3), bucket$(4),     ~
                     bucket$(5), bucket$(6), bucket$(7), bucket$(8),     ~
                     bucket$(9), bucket$(10), bucket$(11)
                     lines_printed% = lines_printed% + 1%
L61190:     if lines_printed% > l% then return
               print : print : lines_printed% = lines_printed% + 2%
               return

        print_heading
            print page
            page% = page% + 1%
            print using L64060, date$, rpt_time$, company$
            print_title$ = "Inventory Status Summary"
            call "FMTTITLE" (print_title$, " ", 12%)
            print using L64100, print_title$, page%
            if end$ = "Y" then return
            print
            print using L64130
            print using L64150,dates$(1), dates$(2), dates$(3), dates$(4),~
                  dates$(5), dates$(6), dates$(7), dates$(8), dates$(9), ~
                  dates$(10), dates$(11)
            print using L64180
            l% = 50% : lines_printed% = 0%
            if prtcost$ = "Y" then l% = 49% /* Fudge Factor */
            return

        report_totals
            if prtcost$ <> "Y" then L63280
               gosub print_heading
               for i% = 1% to 11%
                   call "CONVERT" (total(i%), 0.0, bucket$(i%))
               next i%
               print using L64270, "Total Approximate Value", bucket$(1), ~
                     bucket$(2), bucket$(3), bucket$(4), bucket$(5),     ~
                     bucket$(6), bucket$(7), bucket$(8), bucket$(9),     ~
                     bucket$(10), bucket$(11)
L63280:     if l% - lines_printed% < 2% then gosub print_heading
            print
            print using L64300
            print using L64320, hits%
            return

        print_params
            end$ = "Y"
            gosub print_heading
            print skip(3)
            print using L63530 : print

            print using L63550
            print using L63570, "Part Number",    part$(1,1), part$(1,2)
            print using L63570, "Part Category",  pcat$(1,1), pcat$(1,2)
            print using L63570, "Part type",      type$(1,1), type$(1,2)
            print using L63570, "Primary Vendor", pven$(1,1), pven$(1,2)
            print using L63570, "Condition Flag", condition$, conddescr$
            print using L63570, "Print Approx. costs?", prtcost$, " "
            print using L63570, "Print In Background", back$, " "

            print : print using L63590
            end$ = " "
            return

L63530: %                                     ----------------- Report Se~
        ~lection Parameters -----------------
L63550: %                                                           FROM ~
        ~                     TO
L63570: %                                      #################### #####~
        ~#################### ##########################################
L63590: %                                     ---------------------------~
        ~------------------------------------

        REM *************************************************************~
            *                R E P O R T   F O R M A T S                *~
            *-----------------------------------------------------------*~
            *  Report format lines used by print statements.            *~
            *************************************************************

L64060: %######## ########                   ############################~
        ~################################                    PIPBYWEK:PIP0~
        ~04

L64100: %                                    ############################~
        ~################################                        PAGE:####

L64130: %Part Number               Actvty   LATE     THRU     THRU     TH~
        ~RU     THRU     THRU     THRU     THRU     THRU     THRU     THRU
L64150: %Description                 Type ######## ######## ######## ####~
        ~#### ######## ######## ######## ######## ######## ######## ######~
        ~##
L64180: %============================= == ======== ======== ======== ====~
        ~==== ======== ======== ======== ======== ======== ======== ======~
        ~==
L64210: %----------------------------- -- -------- -------- -------- ----~
        ~---- -------- -------- -------- -------- -------- -------- ------~
        ~--
L64240: %############################# ## ######## ######## ######## ####~
        ~#### ######## ######## ######## ######## ######## ######## ######~
        ~##
L64270: %################################ ######## ######## ######## ####~
        ~#### ######## ######## ######## ######## ######## ######## ######~
        ~##
L64300: %                                        * * * * *   E N D   O F ~
        ~  R E P O R T   * * * * *
L64320: %Parts Printed: #########

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
