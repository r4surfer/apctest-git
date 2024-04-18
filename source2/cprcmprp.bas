        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   CCC   PPPP   RRRR    CCC   M   M  PPPP   RRRR   PPPP    *~
            *  C      P   P  R   R  C      MM MM  P   P  R   R  P   P   *~
            *  C      PPPP   RRRR   C      M M M  PPPP   RRRR   PPPP    *~
            *  C      P      R  R   C      M   M  P      R  R   P       *~
            *   CCC   P      R   R   CCC   M   M  P      R   R  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CPRCMPRP - Customer Pricing- Codes Report. Comparison     *~
            *            of current to future price set WITHOUT updating*~
            *            to the latter.                                 *~
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
            * 02/02/89 ! ORIGINAL (Proj 7880714)                  ! JIM *~
            * 09/15/89 ! Pricing to stocking UOM Conv. to 7 dec.  ! JDH *~
            * 12/15/93 ! PRR 13068.  Minor Mods of UNIX printers. ! JDH *~
            * 08/02/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            blankdate$8,                 /* Blank Date for Comparison  */~
            code$1,                      /* Price Code                 */~
            comment$50,                  /* Report Comment             */~
            company$60,                  /* Company Name               */~
            cursor%(2),                  /* Cursor Location            */~
            date$8,                      /* Date for screen display    */~
            desc$32,                     /* Part Description           */~
            errormsg$79,                 /* Error message              */~
            from$25, rfrom$26,           /* From range parameters      */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Screen FACs                */~
            line2$79,                    /* Second screen line         */~
            cmargin$9, fmargin$9,        /* Margin to print            */~
            cmarkup$9, fmarkup$9,        /* Markups to Print           */~
            cmarkup(8), fmarkup(8),      /* Markups                    */~
            part$25,                     /* Part Number                */~
            plowkey$99,                  /* A Plow Key                 */~
            cprice(16), fprice(16),      /* Percent Price Displays     */~
            cprice$10, fprice$10,        /* Unit Price                 */~
            pf16$16,                     /* PF 16 Prompt               */~
            std$10,                      /* Standard Cost              */~
            time$8,                      /* Run Time                   */~
            to$25, rto$26,               /* To range parameters        */~
            uom$4,                       /* Pricing Unit of Measure    */~
            var_amt$10, var_pct$9        /* Variance figures           */

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64)                      /* = 1 if READ was successful */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! CPRPRICE ! Customer Pricing File                    *~
            * #2  ! HNYMASTR ! Parts Master File                        *~
            * #4  ! SYSFILE2 ! CMS System Information File              *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "CPRPRICE",                                      ~
                        varc, indexed,                                   ~
                        recsize =  700,                                  ~
                        keypos =     1, keylen =  47

            select  #2, "HNYMASTR",                                      ~
                         varc, indexed,                                  ~
                         recsize = 900,                                  ~
                         keypos = 1, keylen = 25,                        ~
                         alternate key 1, keypos = 102, keylen = 9, dup, ~
                                   key 2, keypos =  90, keylen = 4, dup

            select  #4, "SYSFILE2",                                      ~
                         varc, indexed, recsize = 500,                   ~
                         keypos = 1, keylen = 20

            call "SHOSTAT" ("Opening files. One moment please")
            call "OPENCHCK" (#1, 0%, f2%(1), 100%, " ")
            call "OPENCHCK" (#2, 0%, f2%(2),   0%, " ")
            call "OPENCHCK" (#4, 0%, f2%(4),   0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" ( blankdate$ )

            date$ = date  :  call "DATEFMT" (date$)
            line2$ = "(with no update to Future Prices)"
            str(line2$,62) = "CPRCMPRP: " & cms2v$
            call "COMPNAME" (12%, company$, u%)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            * --------------------------------------------------------- *~
            * Get initial values for selection criteria.                *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, from$, to$, comment$

            for fieldnr% = 1% to 2%
                gosub'051(fieldnr%)
L10110:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 then       L65000
                      if keyhit% <>  0 then       L10110
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10110
                next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            * --------------------------------------------------------- *~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editmode
L11070:     gosub'111(0%)
                if keyhit%  =  1 then gosub startover
                if keyhit%  = 16 then       print_report
                if keyhit% <>  0 then goto editmode

                l% = cursor%(1) : fieldnr% = 0%
                if l% >=  6% and l% <=  7% then fieldnr% = 1%
                if l%  =  9%               then fieldnr% = 2%
                if fieldnr% = 0% then goto editmode

            gosub'051(fieldnr%)
L11180:     gosub'111(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11180
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11180
                  goto L11070

        REM *************************************************************~
            *             P R I N T    R E P O R T                      *~
            * --------------------------------------------------------- *~
            * Print the report with the codes requested.                *~
            *************************************************************

        print_report
            call "SHOSTAT" ("Printing Price Set Comparison Report")
            time$ = " " : call "TIME" (time$)
            line% = 857%
            plowkey$ = rfrom$
            call "PLOWNEXT" (#1, plowkey$, 1%, f1%(1))
            if f1%(1) = 1% then L12170
                call "ASKUSER" (2%, "NO RECORDS",                        ~
                   "No records were found within the report parameters", ~
                   "Press any key to continue...", " ")
                goto inputmode
L12170:     select printer(134)
            call "SETPRNT" ("CPR003", " ", 0%, 0%)

*        Print out Selection Criteria (Page zero)
            page% = -1% : gosub print_heading
L12212:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then L12220
                str(i$(), i%, 1%) = hex(20)
                goto L12212
L12220:     print skip(3), tab(20), "Report Selection Criteria:"
            print skip(2)
            for i% = 6% to 19%
                print tab(25), i$(i%)
            next i%
            line% = 857%
            goto L12330

        report_loop
            call "PLOWNEXT" (#1, plowkey$, 1%, f1%(1))
                if f1%(1) = 0% then goto end_of_report
L12330:     get #1 using L12350, part$, cprice(), cmarkup(),/* Current */ ~
                eff_date$, fprice(), fmarkup() /* Future */
L12350:         FMT XX(1), CH(25), POS(57), 16*PD(14,4), 8*PD(14,4),     ~
                     POS(494), CH(6), XX(9), 16*PD(14,4), 8*PD(14,4)
            if part$ > str(rto$,2) then end_of_report
            if eff_date$ = " " or eff_date$ = blankdate$ then goto report_loop
            call "DATEFMT" (eff_date$)

*        Describe the part
            call "READ100" (#2, part$, f1%(2))
            if f1%(2) = 1% then L12480
                desc$ = "** NOT ON FILE **"
                uom$   = "****"
                std    = 0
                conv   = 1
                goto L12520
L12480:     get #2 using L12490, desc$, uom$, conv
L12490:         FMT XX(25), CH(32), POS(78), CH(4), PD(14,7)
            call "STCCOSTS" (part$, " ", #4, 1%, std)
            std = std * conv
L12520:     call "CONVERT" (std, 4.4, std$)
            linesw% = 0%

        REM *************************************************************~
            *    H E R E ' S   T H E   A C T U A L   P R I N T I N G    *~
            *************************************************************

*        Print Price Set data (Prices, then Markups)
            for p% = 1% to 16%
                init (" ") code$, cprice$, fprice$, cmarkup$, fmarkup$,  ~
                     cmargin$, fmargin$, var_amt$, var_pct$
                if cprice(p%) = -1 then goto L12690
                     call "CONVERT" (cprice(p%), 4.4, cprice$)
                     if std <> 0 then call "CONVERT" (100 * ((cprice(p%) ~
                          / std) - 1), 2.2, cmarkup$)
                     if cprice(p%) <> 0 then call "CONVERT" (100 * (1 -  ~
                          (std / cprice(p%))), 2.2, cmargin$)
L12690:         if fprice(p%) = -1 then goto L12750
                     call "CONVERT" (fprice(p%), 4.4, fprice$)
                     if std <> 0 then call "CONVERT" (100 * ((fprice(p%) ~
                          / std) - 1), 2.2, fmarkup$)
                     if fprice(p%) <> 0 then call "CONVERT" (100 * (1 -  ~
                          (std / fprice(p%))), 2.2, fmargin$)
L12750:         if cprice$ = " " and fprice$ = " " then goto L12810
                if cprice(p%) = -1 then cprice(p%) = 0
                if fprice(p%) = -1 then fprice(p%) = 0
                code$ = bin(p% + 64%, 1)
                gosub'201 (cprice(p%), fprice(p%))
                gosub print_line
L12810:     next p%

*        Print Markups
            for p% = 1% to 8%
                init (" ") code$, cprice$, fprice$, cmarkup$, fmarkup$,  ~
                     cmargin$, fmargin$, var_amt$, var_pct$
                cprice, fprice = 0
                if cmarkup(p%) = -999.99 then L12940
                     cprice = std + (std * cmarkup(p%) * .01)
                     call "CONVERT" (cprice, 4.4, cprice$)
                     call "CONVERT" (cmarkup(p%), 2.2, cmarkup$)
                     if cprice <> 0 then call "CONVERT" (100 * (1 -      ~
                          (std / cprice)), 2.2, cmargin$)
L12940:         if fmarkup(p%) = -999.99 then L13000
                     fprice = std + (std * fmarkup(p%) * .01)
                     call "CONVERT" (fprice, 4.4, fprice$)
                     call "CONVERT" (fmarkup(p%), 2.2, fmarkup$)
                     if fprice <> 0 then call "CONVERT" (100 * (1 -      ~
                          (std / fprice)), 2.2, fmargin$)
L13000:         if cmarkup$ = " " and fmarkup$ = " " then goto L13040
                convert p% to code$, pic(0)
                gosub'201 (cprice, fprice)
                gosub print_line
L13040:     next p%

            if linesw% < 2% then gosub print_line /* Force at least...*/
            if linesw% < 2% then gosub print_line    /* two lines */
            print " " : line% = line% + 1%
            goto report_loop

        deffn'201(curr, futr) /* Computes variance figures */
            call "CONVERT" (futr - curr, 4.4, var_amt$)
            if curr = 0                                                  ~
                 then var_pct$ = "#########"                             ~
                 else call "CONVERT" (((futr - curr) / curr) * 100, 2.2, ~
                    var_pct$)
            return

        print_line
            if line% > 56% then gosub print_heading
            if linesw% <> 0% then goto L13260
                print using L60200, part$, std$, code$, cprice$, cmarkup$,~
                    cmargin$, eff_date$, fprice$, fmarkup$, fmargin$,    ~
                    var_amt$, var_pct$
                goto L13340
L13260:     if linesw% <> 1% then goto L13310
                print using L60230, desc$, uom$, code$, cprice$, cmarkup$,~
                    cmargin$, " ", fprice$, fmarkup$, fmargin$, var_amt$,~
                    var_pct$
                goto L13340
L13310:     print using L60200, " ", " ", code$, cprice$, cmarkup$,       ~
                cmargin$, " ", fprice$, fmarkup$, fmargin$, var_amt$,    ~
                var_pct$
L13340:     line% = line% + 1%
            linesw% = linesw% + 1%
            return

        print_heading
            page% = page% + 1%
            line% = 7%
            print page
            print using L60040, date$, time$, company$
            print using L60070, page%
            if page% = 0% then return
            if comment$ <> " " then print using L60100, comment$
            if comment$ <> " " then line% = line% + 1%
            print
            print using L60110
            print using L60140
            print using L60170
            return

        end_of_report
            print
            time$ = " " : call "TIME" (time$)
            print using L60270, time$
            close printer
            call "SETPRNT" ("CPR003", " ", 0%, 1%)
            goto inputmode

        REM *************************************************************~
            *             D E F A U L T   /  E N A B L E S              *~
            * --------------------------------------------------------- *~
            * Actually justs sets INPMESSAGE$.                          *~
            *************************************************************

        deffn'051(fieldnr%)
            inpmessage$ = " "
            on fieldnr% gosub            L20200,    /* Part Range       */~
                                         L20350     /* Rpt Comment      */
            return

L20200
*        Set Message for PART NUMBER RANGE
            inpmessage$ = "Enter Part Number Range, 'ALL', 'FIRST'," &   ~
                          " 'LAST'."
            return

L20350
*        Set Message for REPORT COMMENT
            inpmessage$ = "Enter comment to appear on report"
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            * --------------------------------------------------------- *~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

            keyhit1% = 2%  /* PUT MSG AREA AT BOTTOM OF SCREEN  */
            call "STARTOVR" (keyhit1%)
                if keyhit1% = 1% then return

                return clear all
                goto inputmode

        REM *************************************************************~
            *             S C R E E N   H A N D L I N G                 *~
            * --------------------------------------------------------- *~
            * Handles the only, the only, screen.                       *~
            *************************************************************

        deffn'101(fieldnr%)              /* Input Mode                 */
            init (hex(8c)) lfac$()
            pf16$ = "(16)Exit Program"
            goto L40210

        deffn'111(fieldnr%)              /* Edit Mode                  */
            if fieldnr% = 0% then init (hex(84)) lfac$()  else           ~
                                  init (hex(8c)) lfac$()
            if fieldnr% = 0% then pf16$ = "(16)Print Report" else        ~
                                  pf16$ = " "
            if fieldnr% = 0% then                                        ~
                inpmessage$ = "Locate Cursor and press (RETURN) to" &    ~
                              " modify displayed fields."


L40210:     on fieldnr% gosub            L40260,    /* Part Range       */~
                                         L40250     /* Report Comment   */
            goto L40300

L40250:         lfac$(fieldnr%) = hex(80) : return
L40260:         lfac$(fieldnr%) = hex(81) : return
                lfac$(fieldnr%) = hex(82) : return


L40300:   accept                                                         ~
            at (01,02), "Current-to-Future Price Comparison Report",     ~
            at (01,67), "Date:",                                         ~
            at (01,73), fac(hex(8c)), date$                     , ch(08),~
            at (02,02), fac(hex(ac)), line2$                    , ch(79),~
            at (04,02), fac(hex(94)), errormsg$                 , ch(79),~
                                                                         ~
            at (06,02), "Part Number Range- From",                       ~
            at (06,30), fac(lfac$( 1)), from$                   , ch(25),~
            at (07,02), "                   To  ",                       ~
            at (07,30), fac(lfac$( 1)),   to$                   , ch(25),~
                                                                         ~
            at (09,02), "Report Comment",                                ~
            at (09,18), fac(lfac$( 2)),   comment$              , ch(50),~
                                                                         ~
            at (21,02), fac(hex(a4)),   inpmessage$             , ch(79),~
            at (22,02), "(1)Start Over",                                 ~
            at (22,65), "(13)Instructions",                              ~
            at (23,65), "(15)Print Screen",                              ~
            at (24,65), fac(hex(84)), pf16$                      ,ch(16),~
                keys(hex(00010d0f10)),                                   ~
                key (keyhit%)

            if keyhit% <> 13 then L40570
                call "MANUAL" ("CPRCMPRP")
                goto L40300

L40570:     if keyhit% <> 15 then L40610
                call "PRNTSCRN"
                goto L40300

L40610:     close ws
            call "SCREEN" addr("C", u%, "I", i$(), cursor%())
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * Test data for the items on Page 1.                        *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub            L50120,    /* Part Range       */~
                                         L50180     /* Comment          */
            return

L50120
*        Test PART NUMBER RANGE
            call "TESTRNGE" (from$, to$, str(rfrom$,2), str(rto$,2),     ~
                                                              errormsg$)
            str(rfrom$,,1), str(rto$,,1) = "C"
            return

L50180
*        Test COMMENT
            return

        REM *************************************************************~
            *             I M A G E   S T A T E M E N T S               *~
            *************************************************************

L60040: %RUN DATE: ######## @ ########      #############################~
        ~###############################                      CPRCMPRP-CPR~
        ~003
L60070: %                                              CURRENT-TO-FUTURE ~
        ~PRICE SET COMPARISON                                       PAGE #~
        ~###
L60100: %##################################################
L60110: %PART NUMBER                 STD COST  PR *------ CURRENT PRICES ~
        ~------* *----------- FUTURE PRICES -----------*     AMOUNT   PERC~
        ~ENT
L60140: %PART DESCRIPTION                 UOM  CD UNIT PRICE  % MARKUP  %~
        ~ MARGIN EFF.DATE UNIT PRICE  % MARKUP  % MARGIN   VARIANCE  VARIA~
        ~NCE
L60170: %---------------------------------------- ---------- --------- --~
        ~------- -------- ---------- --------- --------- ---------- ------~
        ~---
L60200: %######################### ##########  #: ########## ######### ##~
        ~####### ######## ########## ######### ######### ########## ######~
        ~###
L60230: %################################ #### #: ########## ######### ##~
        ~####### ######## ########## ######### ######### ########## ######~
        ~###

L60270: % END OF REPORT (@ ########)

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            * --------------------------------------------------------- *~
            * Closes all the files currently open, and also displays    *~
            * a message (ONLY if in foreground) while linking to the    *~
            * next program.                                             *~
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
