        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   AAA   RRRR   M   M  BBBB   Y   Y   AAA    CCC   TTTTT   *~
            *  A   A  R   R  MM MM  B   B  Y   Y  A   A  C   C    T     *~
            *  AAAAA  RRRR   M M M  BBBB    YYY   AAAAA  C        T     *~
            *  A   A  R   R  M   M  B   B    Y    A   A  C   C    T     *~
            *  A   A  R   R  M   M  BBBB     Y    A   A   CCC     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARMBYACT - Prints A/R Summary by Account report.          *~
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
            * 12/24/86 ! Original                                 ! ERN *~
            * 07/26/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            acct$12,                     /* Accounts Number            */~
            accts$(1000)13, accts(1000), /* Accounts and Balances      */~
            asof$8, asofu$8,             /* As of Date                 */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            compname$30,                 /* Company Name               */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            descr$30,                    /* Account Description        */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            last_stl$8, last_stlu$6,     /* Date Last Settled          */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            p%(1),                       /* Stupid Receiver for SEARCH */~
            pf16$16, pf4$18, pf5$16,     /* PF Key Literals            */~
            postdate$6,                  /* Transaction Post Date      */~
            readkey$50,                  /* Read Key                   */~
            stlmnt$12,                   /* Settlement Number          */~
            time$8                       /* Report Run Time            */

        dim f2%(32),                     /* = 0 if the file is open    */~
            f1%(32),                     /* = 1 if READ was successful */~
            fs%(32),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(32)20                  /* Text from file opening     */

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
            * # 1 ! ARMTRIAL ! Accounts Receivable Trial Balance        *~
            * # 2 ! SYSFILE2 ! Caelus Management System Information     *~
            * # 4 ! GLMAIN   ! General Ledger CHart Of Accounts File.   *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 1, "ARMTRIAL",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  21                      ~

            select # 2, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select # 4, "GLMAIN",                                        ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   9                      ~

        call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (# 1, fs%( 1), f2%( 1), 0%, rslt$( 1))
            call "OPENCHCK" (# 2, fs%( 2), f2%( 2), 0%, rslt$( 2))
            call "OPENCHCK" (# 4, fs%( 4), f2%( 4), 0%, rslt$( 4))

            if min(fs%()) < 0% then exit_program

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" ( blankdate$ )

            date$ = date : call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            readkey$  = "ARM.LAST.SETTLING"
            last_stl$ = blankdate$
            call "READ100" (#2, readkey$, f1%(2))
            if f1%(2) = 1% then get #2 using L09160, last_stl$
L09160:         FMT XX(20), CH(6)
            last_stlu$ = last_stl$  :  call "DATEFMT" (last_stl$)


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            pf4$ = " " : pf5$ = " " : pf16$ = "(16)Exit Program"
            init(" ") errormsg$, inpmessage$, asof$, asofu$

            for fieldnr% = 1 to  1
                if fieldnr% > 1 then pf4$ = "(4)Previous Field"
L10320:         gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10440
L10340:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10420
L10370:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10340
                         if fieldnr% = 1% then L10320
                         goto L10370
L10420:               if keyhit% = 16 and fieldnr% = 1 then exit_program
                      if keyhit% <>  0 then       L10340
L10440:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10340
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            pf4$, pf5$ = " "  :  pf16$ = "(16)Print Report"
            inpmessage$ = edtmessage$
            lastfieldnr% = 0%
            gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       print_report
                  if keyhit% <>  0 then       editpg1
L11140:     fieldnr% = max(1, min(1, cursor%(1) - 5))
            if fieldnr% = lastfieldnr% then editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       editpg1
                  pf4$, pf5$, pf16$ = " "
L11190:     gosub'101(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11190
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11190
                  lastfieldnr% = fieldnr%
            goto L11140

        REM *************************************************************~
            *             P R I N T    R E P O R T                      *~
            *-----------------------------------------------------------*~
            * Summarize Trial Balance by Accounts and Print Report.     *~
            *************************************************************
        print_report

*        First Summarize Trial Balance by Account.
            call "SHOSTAT" ("Summarizing Trial Balance by Account")
L12100:     call "READNEXT" (#1, f1%(1))
            if f1%(1) = 0% then L12280
                get #1 using L12130, stlmnt$, amt, acct$, postdate$
L12130:              FMT XX(9), CH(12), POS(68), PD(14,4), CH(9),        ~
                         POS(97), CH(6)
                if postdate$ > asofu$ then L12100
                if str(stlmnt$,11,2) <> "00" then L12250
                     if idx% > 0% then L12180 : idx%, i% = 1% : goto L12230
L12180:              search str(accts$(),,13%*idx%) = str(acct$,,9)      ~
                                                         to p%() step 13
                     if p%(1) = 0% then idx% = idx% + 1%
                     if p%(1) = 0% then i% = idx% else                   ~
                                        i% = (p%(1) + 12%) / 13%
L12230:              accts$(i%) = acct$
                     convert i% to str(accts$(i%),10,4), pic(0000)
L12250:         accts(i%) = accts(i%) + amt  :  total = total + amt
                goto L12100

L12280
*        Now print the report
            call "SHOSTAT" ("Printing A/R SUMMARY BY ACCOUNT")
            if idx% > 1% then call "SORT" addr(str(accts$()), idx%, 13%)
            call "TIME" (time$)
            call "COMPNAME" (12%, compname$, u3%)
            select printer (134)
            call "SETPRNT"  ("ARM013", " ", 0%, 0%)
            gosub page_heading
            if i% = 0% then L12480

            for a% = 1% to idx%
                acct$ = str(accts$(a%),,9)
                call "DESCRIBE" (#4, acct$, descr$, 0%, f1%(4))
                call "GLFMT" (acct$)
                convert str(accts$(a%),10,4) to i%
                if line% > 55% then gosub page_heading
                print using L12730, acct$, descr$, accts(i%)
                line% = line% + 1%
            next a%

            print using L12750
L12480:     print using L12770, total
            print "** END OF REPORT **"
            goto exit_program


            page_heading
                page% = page% + 1%  :  line% = 6%
                print page
                print using L12650, date$, time$, compname$
                print using L12670, asof$, page%
                print
                print using L12690
                print using L12710
                return



L12650: %RUN DATE: ######## ########             ########################~
        ~######             ARMBYACT-ARM013
L12670: %   AS OF: ########                 A/R TRIAL BALANCE SUMMARY BY ~
        ~G/L ACCOUNT            PAGE: ###
L12690: %                         G/L ACCOUNT  ACCOUNT DESCRIPTION       ~
        ~         ACCT BALANCE
L12710: %                        ------------  --------------------------~
        ~----   --------------
L12730: %                        ############  ##########################~
        ~####  -###,###,###.##
L12750: %                                                                ~
        ~       --------------
L12770: %                                                    ** REPORT TO~
        ~TALS  -###,###,###.##


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            inpmessage$ = " "
            enabled% = 1%
            on fieldnr%       gosub L20100          /* As Of Date       */
            return

L20100
*        As OF Date                            ASOF$
            inpmessage$ = "Enter report As Of Date."
            if asof$ = " " or asof$ = blankdate$ then asof$ = date$
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
              str(line2$,62%) = "ARMBYACT: " & str(cms2v$,,8%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40140          /* Date Range        */
              goto L40170

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40140:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40170:     accept                                                       ~
               at (01,02),                                               ~
                  "A/R Summary by Account Report",                       ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "As Of",                                      ~
               at (06,30), fac(lfac$( 1)), asof$                , ch(08),~
               at (06,49), "Last Settled",                               ~
               at (06,62), fac(hex(8c))  , last_stl$            , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,20), fac(hex(8c)), pf4$,                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(8c)), pf16$,                          ~
                     keys(hex(0001040d0f10)),  key(keyhit%)

               if keyhit% <> 13 then L40460
                  call "MANUAL" ("ARMBYACT")
                  goto L40170

L40460:        if keyhit% <> 15 then L40500
                  call "PRNTSCRN"
                  goto L40170

L40500:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100          /* As Of Date       */
                  return

L50100
*        As Of Date                            ASOF$
            call "DATEOK" (asof$, u3%, errormsg$)
            if errormsg$ <> " " then return
                asofu$ = asof$  :  call "DATUNFMT" (asofu$)
                if asofu$ > last_stlu$ then return
                     errormsg$ = "As Of must be after Last Settled date."
                     return


        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
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
            call "SETPRNT" ("ARM013", " ", 0%, 1%)
            end
