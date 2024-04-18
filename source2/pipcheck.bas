        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  PPPP   IIIII  PPPP    CCC   H   H  EEEEE   CCC   K  K    *~
            *  P   P    I    P   P  C      H   H  E      C      K K     *~
            *  PPPP     I    PPPP   C      HHHHH  EEE    C      KK      *~
            *  P        I    P      C      H   H  E      C      K  K    *~
            *  P      IIIII  P       CCC   H   H  EEEEE   CCC   K   K   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PIPCHECK - COMPARE PIPMASTR TO PIP DETAILS AND ONHAND TO  *~
            *            POINT OUT ANY DISCREPENCIES.                   *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/01/86 ! ORIGINAL                                 ! HES *~
            * 02/04/87 ! Changed HNYQUAN Format                   ! KAB *~
            * 05/13/87 ! Std Cost Changes (HNYMASTR/QUAN files).  ! ERN *~
            * 02/17/88 ! Fixed Range logic, Problem w/TODAY%      ! HES *~
            * 11/04/92 ! PRR 12520 - End or Report print msg in   ! RJH *~
            *          !  seperate print file.                    !     *~
            *          ! FMT statement, line 19150 corrected      !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            back$1,                      /* Background flag            */~
            beg_cat$(2)4,                /* Beginning Part Category    */~
            beg_part$(2)25,              /* Beginning Part Number      */~
            beg_type$(2)3,               /* Beginning Part Type        */~
            cat$4,                       /* Part Category              */~
            company$60,                  /* Company / Division Name    */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            end_cat$(2)4,                /* Ending Part Category       */~
            end_part$(2)25,              /* Ending Part Number         */~
            end_type$(2)3,               /* Ending Part Type           */~
            errormsg$79,                 /* Screen Error Message       */~
            header$79,                   /* Screen Header Line         */~
            hnypart$50,                  /* Part Number                */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            message$78,                  /* Informational Message      */~
            moq$10,                      /*                            */~
            oldpip%(490),                /* Current PIP Array          */~
            part$25,                     /* Part Number                */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            pf4$18,                      /* PF 4 Screen Literal        */~
            pip%(490),                   /* 'Should Be' PIP Array      */~
            pip(490),                    /* PIP Array                  */~
            pipinpart$25,                /* Part Number                */~
            pipoutpart$25,               /* Part Number                */~
            print_title$60,              /* Report Sorted By Title     */~
            readkey$100,                 /* Work Variable              */~
            rpt_time$8,                  /* Report Time                */~
            title$(2)30,                 /* Screen Column Headings     */~
            type$10,                     /* Part Type                  */~
            userid$3,                    /* Current User Id            */~
            yymmdd$6                     /* Planning Calendar Start Dt */

        dim f1%(64)                      /* = 1 If read was successful */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.03 02/16/93 Customer Credit & Core Trackng  "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! SYSFILE2 ! Caelus Management System Information     *~
            * # 2 ! HNYMASTR ! Inventory Master File                    *~
            * # 3 ! HNYQUAN  ! Inventory Store Quantity File            *~
            * # 4 ! PIPMASTR ! Planned Inventory Position Master File   *~
            * # 5 ! PIPIN    ! Planned inventory additions detail       *~
            * # 6 ! PIPOUT   ! Planned inventory use detail rec         *~
            * # 7 ! SFCUM2   ! Cumulative sales forecast file           *~
            * # 8 ! SFMASTR2 ! Sales forecast master file               *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select # 1, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos =    1, keylen =  20                      ~

            select # 2, "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  900,                                  ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            select # 3, "HNYQUAN",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  650,                                  ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =   44

            select # 4, "PIPMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =    2, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  26          ~

            select # 5, "PIPIN",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   60,                                  ~
                        keypos =   30, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  48          ~

            select # 6, "PIPOUT",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   64,                                  ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =   20, keylen =  37          ~

            select # 7, "SFCUM2",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1985,                                  ~
                        keypos =    1, keylen =  25                      ~

            select # 8, "SFMASTR2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =    1, keylen =  25                      ~


            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#2, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#3, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#4, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#5, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#6, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#7, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#8, 0%, 0%, 0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * Initializes information necessary for program.            *~
            *************************************************************

            date$ = date
            call "EXTRACT" addr("ID", userid$, "TT", back$)
            title$(1) = "From"  :  title$(2) = "To  "
            call "COMPNAME" (12%, company$, ret%)

            call "READ100" (#1, "MONTHS OPEN", f1%(1))
               if f1%(1) <> 1% then L65000
            get #1, using L09140, yymmdd$
L09140:         FMT XX(32), CH(6)

            call "DATE" addr("G-", yymmdd$, date$, today%, ret%)
              if ret% <> 0% then goto L65000
              if today% = 0% then goto L65000

            call "DATEFMT" (date$)
            if back$ = "B" then printing_in_background

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            pf4$="(4)Previous Field" : pf16$="(16)EXIT PROGRAM"
            errormsg$, inpmessage$, beg_part$(), end_part$(), beg_cat$(),~
            end_cat$(), beg_type$(), beg_type$(), back$ = " "

            for fieldnr% = 1 to 4
L10160:         gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10280
L10180:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10260
L10210:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10180
                         if fieldnr% = 1% then L10160
                         goto L10210
L10260:               if keyhit%  = 16 then       L65000
                      if keyhit% <>  0 then       L10180
L10280:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10180
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editmode
            inpmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
            pf4$  = " "
            pf16$ = "(16)PRINT REPORT"
            if back$ = "Y" then pf16$ = "(16)SUBMIT TASK"
            gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       editmode
            oldfield% = 0%
L11170:     fieldnr% = cursor%(1) - 6
            if fieldnr% < 1 or fieldnr% > 4 then editmode
            if fieldnr% = oldfield% then editmode

            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then editmode
                  pf4$, pf16$ = " "
L11240:     gosub'101(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11240
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11240
                  oldfield% = fieldnr%
                  goto L11170

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after input/editing.                   *~
            *************************************************************

        datasave
            if back$ <> "Y" then L19270
                call "READ101" (#1, "zPIPCHECK." & userid$, f1%(1))
                put #1, using L19100, " ", " "
L19100:         FMT 2*CH(250)
                put #1, using L19150, "zPIPCHECK." & userid$,             ~
                  beg_part$(), end_part$(), beg_type$(), end_type$(),    ~
                  beg_cat$(), end_cat$()
                if f1%(1) = 0 then write #1  else rewrite #1
L19150:         FMT CH(20), 4*CH(25), 4*CH( 3), 4*CH( 4)
                call "TASKUP" ("ME", 0%)
                goto L65000

        printing_in_background
                call "READ101" (#1, "zPIPCHECK." & userid$, f1%(1))
                     if f1%(1) = 0 then error_exit
                get #1, using L19150, errormsg$,                          ~
                  beg_part$(), end_part$(), beg_type$(), end_type$(),    ~
                  beg_cat$(), end_cat$()
                delete #1

L19270:     init (hex(00)) pipinpart$, pipoutpart$, hnypart$
            f1%(5), f1%(6), f1%(3) = -1%
            call "SHOSTAT" ("Scanning PIP Files For Imbalances")
            rpt_time$ = " "             /* Get System Time            */
            call "TIME" (rpt_time$)
            call "SETPRNT" ("PIP001","        ", 0%, 0%)
            select printer (134)
            l%, page% = 0               /* Number of Lines Left on Page*/

        REM Processing Begins Here...
            call "PLOWNEXT" (#2, beg_part$(2), 0%, f1%(2))
                  goto L19400
L19390:     call "READNEXT" (#2, f1%(2))
L19400:         if f1%(2) = 0 then L19610
            get #2, using L19420, part$, cat$, lead$, type$, moq$, q2, q4
L19420:         FMT CH(25), XX(64), CH(3), XX(77), 3*CH(10), XX(118),    ~
                    2*PD(14,4)
            if part$ > end_part$(2) then L19610
            if type$ < beg_type$(2) or type$ > end_type$(2) then L19390
            if cat$ < beg_cat$(2) or cat$ > end_cat$(2) then L19390

            if q2 > 99999999 then q2 = 0
            if q4 > 99999999 then q4 = 0
            q3=0:convert moq$ to q3, data goto L19510
L19510:     convert type$ to type%, data goto L19580
            lead% = 0:convert lead$ to lead%, data goto L19530
L19530:     if type% > 0% and type% < 200% then goto L19580

            gosub bal_pips
            goto L19390

L19580:     gosub del_pips
            goto L19390

L19610: REM FINAL CLEANUP ON PIPS
            call "SHOSTAT" ("Scanning For Orphan PIPMASTR Records")

            call "PLOWNEXT" (#4, beg_part$(2), 0%, f1%(4))
                goto L19680
L19670:     call "READNEXT" (#4, f1%(4))
L19680:         if f1%(4) = 0 then good_exit
            get #4, using L19700, part$
L19700:         FMT XX(1), CH(25)
            if part$ > end_part$(2) then good_exit
            call "READ100" (#2, part$, f1%(2))
            if f1%(2) <> 0 then L19670
            message$ = "PIP data on file, but part is not in HNYMASTR"
            gosub print_report_line
            goto L19670

        del_pips
            init (hex(00)) readkey$
            str(readkey$,,25) = str(part$,1,25)
            call "PLOWNEXT"  (#4, readkey$, 25%, f1%(4)) /* PIPMASTR */
              message$ = "PIPMASTR exists for non-planned part"
              if f1%(4) <> 0 then gosub print_report_line
            call "PLOWNEXT"  (#7, readkey$, 25%, f1%(7)) /* SFCUM2   */
              message$ = "SFCUM2 record exists for non-planned part"
              if f1%(7) <> 0 then gosub print_report_line
            call "PLOWNEXT"  (#8, readkey$, 25%, f1%(8)) /* SFMASTR2 */
              message$ = "SFMASTR2 record exists for non-planned part"
              if f1%(8) <> 0 then gosub print_report_line

            init (hex(00)) readkey$
            str(readkey$,1,25) = str(part$,1,25)
            call "PLOWALTS" (#5, readkey$, 1%, 25%, f1%(5)) /* PIPIN */
                if f1%(5) = 0 then L19980
            message$ = "PIPIN exists for non-planned part"
            gosub print_report_line

L19980:     init (hex(00)) readkey$
            str(readkey$,1,25) = str(part$,1,25)
            call "PLOWALTS" (#6, readkey$, 1%, 25%, f1%(6)) /* PIPOUT */
                if f1%(6) = 0 then L20060
            message$ = "PIPOUT exists for non-planned part"
            gosub print_report_line

L20060:     f1%(5), f1%(6) = -1%
            return

        bal_pips
            mat pip = zer
            q1=0
            if f1%(3) < 0% then L20160
            if str(hnypart$,1,1) = hex(ff) then L20330
            goto L20200
L20160:     init (hex(00)) readkey$
            str(readkey$,,26) = str(part$,,25) & hex(2f)
            call "PLOWNEXT" (#3, readkey$, 0%, f1%(3))
                if f1%(3) = 0 then L20310
L20200:     get #3, using L20210, hnypart$
L20210:         FMT POS(17), CH(26)
                if str(hnypart$,,25) < str(part$,,25) then L20290
                if str(hnypart$,,25) > str(part$,,25) then L20330
                if str(hnypart$,26,1) < hex(30) then L20290
                if str(hnypart$,26,1) > hex(39) then L20330
            get #3, using L20270, q
L20270:         FMT POS(69), PD(14,4)
            q1=round(q1+q,2)
L20290:     read #3, eod goto L20310
            goto L20200
L20310:     init (hex(ff)) hnypart$

L20330:     if f1%(5) < 0% then L20360
            if str(pipinpart$,,1) = hex(ff) then L20500
            goto L20400
L20360:     init (hex(00)) readkey$
            str(readkey$,,25) = str(part$,,25)
            call "PLOWALTS" (#5, readkey$, 1%, 0%, f1%(5))
                if f1%(5) = 0 then L20480
L20400:     get #5, using L20410, pipinpart$, p%, q
L20410:         FMT CH(25), BI(4), XX(19), PD(14,4)
            if pipinpart$ > part$ then L20500
            if pipinpart$ < part$ then L20460
            p% = min(490%, max(1%,p%))
            pip(p%) = round(pip(p%) + q,2)
L20460:     read #5, eod goto L20480
            goto L20400
L20480:     init (hex(ff)) pipinpart$

L20500:     if f1%(6) < 0% then L20530
            if str(pipoutpart$,1,1) = hex(ff) then L20670
            goto L20570
L20530:     init (hex(00)) readkey$
            str(readkey$,1,25) = str(part$,1,25)
            call "PLOWALTS" (#6, readkey$, 1%, 0%, f1%(6))
                if f1%(6) = 0 then L20650
L20570:     get #6, using L20580, pipoutpart$, p%, q
L20580:         FMT XX(19), CH(25), BI(4), XX(8), PD(14,4)
            if pipoutpart$ > part$ then L20670
            if pipoutpart$ < part$ then L20630
            p% = min(490%, max(1%,p%))
            pip(p%) = round(pip(p%) - q,2)
L20630:     read #6, eod goto L20650
            goto L20570
L20650:     init (hex(ff)) pipoutpart$

L20670:     pip(today%) = round(pip(today%) + q1,2)

            for i% = 489% to 1% step -1%
            if abs(pip(i%)) < .0001 then L20740
               for j% = i%+1% to 490%
                 pip(j%) = round(pip(j%) + pip(i%),2)
               next j%
L20740:     next i%
            mat pip% = pip

            message$ = "PIP master not found for planned part"
            call "READ100" (#4, part$, f1%(4))
                if f1%(4) = 0 then L21120
            get #4 using L20820, oldpip%(), oldq1,  oldq2, oldq3, oldq4,  ~
                                oldtype%, oldlead%
L20820:     FMT XX(26), 490*BI(4), 4*PD(14,4), 2*BI(2)

            message$ = "PIP current quantity on hand field is incorrect"
            if abs(oldq1 - q1) > .009 then gosub L21120

            message$ = "PIP safety stock quantity field is incorrect"
            if abs(oldq2 - q2) > .009 then gosub L21120

            message$ = "PIP minimum order quantity field is incorrect"
            if abs(oldq3 - q3) > .009 then gosub L21120

            message$ = "PIP pan size quantity field is incorrect"
            if abs(oldq4 - q4) > .009 then gosub L21120

            message$ = "PIP part type field doesn't match HNYMASTR"
            if oldtype% <> type% then gosub L21120

            message$ = "PIP part lead time field doesn't match HNYMASTR"
            if oldlead% <> lead% then gosub L21120

            message$ = "PIP is out of balance By:"
            for i% = today%+1% to 490
                if pip%(i%) <> oldpip%(i%) then L21080
            next i%
            return

L21080:     temp = pip%(i%) - oldpip%(i%)
            call "CONVERT" (temp, -0.2, temp$)
            message$ = message$ & " " & temp$

L21120:     gosub print_report_line
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L25240,         /* Part Range       */~
                                    L25280,         /* Part Type Range  */~
                                    L25320,         /* Part Cat. Range  */~
                                    L25430          /* Print In Bckgrnd?*/
                     return
L25240:     REM Default/Enable for Part Range
                inpmessage$ = "Enter the Part Number Range To be " &     ~
                              "Included or 'ALL' for all Parts."
                if beg_part$(1) = " " then beg_part$(1) = "ALL"
                return
L25280:     REM Default/Enable for Part Type Range
                inpmessage$ = "Enter the Part Type Range To be " &       ~
                              "Included or 'ALL' for all Part Types."
                if beg_type$(1) = " " then beg_type$(1) = "ALL"
                return
L25320:     REM Default/Enable for Part Category Range
                inpmessage$ = "Enter the Part Category Range To be " &   ~
                              "Included or 'ALL' for all Part Categories"
                if beg_cat$(1) = " " then beg_cat$(1) = "ALL"
                if end_date$ = " " then end_date$="12/31/99"
                return
L25430:     REM Default/Enable for Print In Background?
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
            *               R E P O R T   R O U T I N E S               *~
            *-----------------------------------------------------------*~
            * Report printing logic.                                    *~
            *************************************************************

        print_report_line
            if l% < 1% then gosub print_heading
            print using L30810, part$, message$
            l% = l% - 1%
            return

        print_heading
            if page% = 0% then gosub print_params
            page% = page% + 1%
            gosub print_title
            print
            print using L30760
            print using L30780
            l% = 54%
            return

        print_params
            gosub print_title
            print skip(3)
            print using L30510 : print

            print using L30530
            print using L30550, "Part Number", beg_part$(1), end_part$(1)
            print using L30550, "Part Type", beg_type$(1), end_type$(1)
            print using L30550, "Part Category", beg_cat$(1), end_cat$(1)
            temp$ = back$ : if back$ = "B" then temp$ = "Y"
            print using L30550, "Print In Background", temp$

            print : print using L30570
            return

        print_title
            print page
            print using L30660, date$, rpt_time$, company$
            print_title$ = "IMBALANCED PIP REPORT"
            call "FMTTITLE" (print_title$, " ", 12%)
            print using L30700, userid$, print_title$, page%
        return

L30510: %                                     ----------------- Report Se~
        ~lection Parameters -----------------
L30530: %                                                           FROM ~
        ~                     TO
L30550: %                                      #################### #####~
        ~#################### #########################
L30570: %                                     ---------------------------~
        ~------------------------------------

        REM *************************************************************~
            *                R E P O R T   F O R M A T S                *~
            *-----------------------------------------------------------*~
            *  Report format lines used by print statements.            *~
            *************************************************************

L30660: % ######## ########                  ############################~
        ~################################                    PIPCHECK:PIP0~
        ~01

L30700: % USER:###                           ############################~
        ~################################                        PAGE:####

        %                                    ############################~
        ~################################

L30760: %PART NUMBER                 PROBLEM

L30780: %=========================   ====================================~
        ~=================================================================~
        ~==
L30810: %#########################   ####################################~
        ~##########################################

L30840: %                                * * * * *   E N D   O F   R E P ~
        ~O R T   @  ##########    * * * * *

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  header$ = "Report Selection Criteria"
                  str(header$,62) = "PIPCHECK: " & str(cms2v$,,8)
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                  else                  init(hex(86)) lfac$()
                  on fieldnr% gosub L40200,         /* Part Range       */~
                                    L40200,         /* Part Type Range  */~
                                    L40200,         /* Part Cat. Range  */~
                                    L40200          /* Print In Bckgrnd?*/
                  goto L40260

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L40200:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return
L40260:
L40270:     accept                                                       ~
               at (01,02), "List Out Of Balance PIP Conditions",         ~
               at (01,60), "Today's Date:",                              ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), header$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,22), fac(hex(ac)), title$(1)              , ch(25),~
               at (06,48), fac(hex(ac)), title$(2)              , ch(25),~
                                                                         ~
               at (07,02), "Part Number",                                ~
               at (07,22), fac(lfac$( 1)), beg_part$(1)         , ch(25),~
               at (07,48), fac(lfac$( 1)), end_part$(1)         , ch(25),~
                                                                         ~
               at (08,02), "Part Type",                                  ~
               at (08,22), fac(lfac$( 2)), beg_type$(1)         , ch(03),~
               at (08,48), fac(lfac$( 2)), end_type$(1)         , ch(03),~
                                                                         ~
               at (09,02), "Part Category",                              ~
               at (09,22), fac(lfac$( 3)), beg_cat$(1)          , ch(04),~
               at (09,48), fac(lfac$( 3)), end_cat$(1)          , ch(04),~
                                                                         ~
               at (10,02), "Print In Background?",                       ~
               at (10,23), fac(lfac$( 4)), back$                , ch(01),~
                                                                         ~
               at (13,02), "This is a maintenance routine to ensure that ~
        ~Planned Inventory Positions shown ",                             ~
               at (14,02), "in the PIPMASTR file agree with current quant~
        ~ity on hand (via HNYQUAN) and the ",                             ~
               at (15,02), "Planned Inventory Position details reflected ~
        ~in the PIPIN and PIPOUT files.",                                 ~
               at (17,02), "Also, by responding to the current part type ~
        ~ in HNYMASTR, it will detect any",                               ~
               at (18,02), "missing or unnecessary PIPMASTR records.",   ~
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

               if keyhit% <> 13 then L40790
                  call "MANUAL" ("PIPCHECK")
                  goto L40270

L40790:        if keyhit% <> 15 then L40830
                  call "PRNTSCRN"
                  goto L40270

L40830:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50130,         /* Part Range       */~
                                    L50180,         /* Part Type Range  */~
                                    L50230,         /* Part Cat. Range  */~
                                    L50280          /* Print In Bckgrnd?*/
                  return
L50130:     REM Test Data for Part Range
                call "TESTRNGE"   (beg_part$(1), end_part$(1),           ~
                                   beg_part$(2), end_part$(2),           ~
                                   errormsg$)
                return
L50180:     REM Test Data for Part Type Range
                call "TESTRNGE"   (beg_type$(1), end_type$(1),           ~
                                   beg_type$(2), end_type$(2),           ~
                                   errormsg$)
                return
L50230:     REM Test Data for Part Category Range
                call "TESTRNGE"   (beg_cat$(1), end_cat$(1),             ~
                                   beg_cat$(2), end_cat$(2),             ~
                                   errormsg$)
                return
L50280:     REM Test Data for Background?
                if back$ <> "Y" then back$ = "N"
                return

            REM Exits for background mode...
        good_exit

            rpt_time$ = " "  :   call "TIME" (rpt_time$)
            if page% = 0% then L59020
            print
            print using L30840 , rpt_time$

L59020:     call "SETPRNT" ("PIP001","        ", 0%, 1%)
            if page% <> 0% then L59170
            if back$ <> "B" then L59090
            message$ = "rptReport PIPCHECK in background: No data met sel~
        ~ection criteria."
            goto L59240

L59090:        call "ASKUSER" (2%, "INVALID SELECTION PARAMETERS",       ~
                 "Sorry, no data eligible for printing was",             ~
                 "found using the given selection parameters.",          ~
                 "Press RETURN to change the parameters or exit.")
               goto L10000

L59170:     if back$ <> "B" then inputmode
            message$ = "rptReport PIPCHECK in background: Completed"
            call "TIME" (str(message$,45,8))
            goto L59240
        error_exit
            call "SETPRNT" ("PIP001","        ", 0%, 1%)
            message$ = "rptReport PIPCHECK in background: Canceled."
L59240:     call "MESSAGE"addr("XM",str(userid$)&hex(20),message$,78%,0%)

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            call "SHOSTAT" ("One Moment Please")
            end
