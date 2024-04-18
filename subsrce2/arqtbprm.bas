        REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                                                           *~
            *   AAA   RRRR    QQQ   TTTTT  BBBB   PPPP   RRRR   M   M   *~
            *  A   A  R   R  Q   Q    T    B   B  P   P  R   R  MM MM   *~
            *  AAAAA  RRRR   Q   Q    T    BBBB   PPPP   RRRR   M M M   *~
            *  A   A  R   R  Q   Q    T    B   B  P      R   R  M   M   *~
            *  A   A  R   R   QQ Q    T    BBBB   P      R   R  M   M   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARQTBPRM - Pass through to ARQTBSUB, Picking Up Parameters*~
            *            on the way.                                    *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and proprie- *~
            * tary assets of CAELUS, INCORPORATED, Spokane, WA,    em-  *~
            * bodying substantial creative efforts  and confidential    *~
            * information.  Unauthorized use, copying, decompiling,     *~
            * translating, disclosure, or transfer of it is prohibited. *~
            * Copyright (c) 1983, an unpublished work by CAELUS,        *~
            * INCORPORATED, Spokane, wa.  All rights reserved.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/28/92 ! ORIGINAL                                 ! KB2 *~
            * 07/11/96 ! Changes for the year 2000.               ! DXL *~
            CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL

        sub "ARQTBPRM" (billto$, curr$, statutory$, #1, #2, #3, #4)

        dim                                                              ~
            acct$(2)12, acctie$1,        /* Selected A/R Account       */~
            ageper$1,                    /* Aging Per- Disc,Net,Invce  */~
            aging%(10), aging$(7)20,     /* Aging Parameters           */~
            asof$10, asofu$6,            /* As Of Date                 */~
            blankdate$8,                 /* Blank unfmt date           */~
            billto$9, oldbillto$9,       /* Bill to (Passed)           */~
            curr$1, statutory$4,         /* M/C Paramteres (Passed)    */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            last_stl$10,                 /* Last Settling Date         */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            pf16$16,                     /* PF 16 Screen Literal       */~
            pf4$18,                      /* PF 4 Screen Literal        */~
            pf8$26,                      /* PF 5 Screen Literal        */~
            readkey$50,                  /* Read Key                   */~
            whentoask$(3)1               /* How often to show screen   */

        dim f1%(32)                      /* File Read Status Flag      */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! ARMTRIAL ! Accounts Rec. Trial Balance              *~
            * #2  ! CUSTOMER ! Customer Master File                     *~
            * #3  ! SYSFILE2 ! System File                              *~
            * #4  ! GLMAIN   ! G/L Account Master File                  *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            if str(whentoask$()) <> " " then L09500
            blankdate$ = " "
            call "DATUNFMT" (blankdate$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            readkey$ = "ARM.LAST.SETTLING"
            last_stl$ = "19010101"
            call "DATECONV" (last_stl$)
            call "READ100" (#3, readkey$, f1%(3))
            if f1%(3) = 1% then get #3 using L09180, last_stl$
L09180:         FMT XX(20), CH(6)
            call "DATFMTC" (last_stl$)

L09500:     if str(whentoask$()) = " " then L10000
               if whentoask$(1) <> " " then edtpg1
               if whentoask$(3) <> " " then datasave
                  if billto$ = oldbillto$ then datasave
                     goto edtpg1

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            pf4$="(4)Previous Field" : pf8$=" ": pf16$="(16)Return"
            init(" ") errormsg$, inpmessage$, asof$, ageper$, aging$(),  ~
                      asofu$, acct$(), acctie$, whentoask$()

            mat aging% = zer
                aging%(1) =  30% : aging%( 2) =  60% : aging%(3) =  90%
                aging%(4) = 120% : aging%(10) =   5%
                aging%(8) = -99999%  :  aging%(5), aging%(9) = 99999%

            for fieldnr% = 1% to 5%
                if fieldnr% <> 4% then L10170
                   if first% = 0% then age% = 99% else age% = 0%
                   gosub aging_parameters
                   goto L10310
L10170:         gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10290
L10190:         gosub'101(fieldnr%, 1%) /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10270
L10220:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10190
                         if fieldnr% = 1% then L10170
                         goto L10220
L10270:               if keyhit%  = 16 then       exit_program
                      if keyhit% <>  0 then       L10190
L10290:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10190
L10310:     next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        edtpg1
            pf4$  = " " : pf8$ = "(8)Modify Aging Parameters"
            pf16$ = "(16)Proceed"
            lastfieldnr% = 0% : age% = 0% : first% = 1%
            inpmessage$ = edtmessage$
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  8 then gosub aging_parameters
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       edtpg1
L11160:     fieldnr% = min(max(1%, cursor%(1) - 5%), 19%)
                if fieldnr% > 3% and fieldnr% < 11% then fieldnr% = 4%
                if fieldnr% > 10% then fieldnr% = 5%
                if fieldnr% < 1% or   fieldnr% > 5% then edtpg1
                if fieldnr% = lastfieldnr% then edtpg1
                if fieldnr% <> 4% then L11230
                     gosub aging_parameters  :  goto edtpg1

L11230:     gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then edtpg1
                  pf4$, pf8$, pf16$ = " "
L11260:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11260
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11260
                     lastfieldnr% = fieldnr%
                     goto L11160


        aging_parameters
            call "ARMDATES" (asofu$, "A/R TRIAL BALANCE PARAMETERS ",    ~
                             aging%(), 1%, 1%, aging$(), age%)
            if age% <> 1% then return
                return clear all
                goto   inputmode

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Calls print subroutine.                                   *~
            *************************************************************

        datasave
            oldbillto$ = billto$
            call "ARQTBSUB" (billto$, aging%(), asofu$, ageper$,         ~
                             #1, #2, "N", 0%, " ", " ", " ", curr$,      ~
                             statutory$, acctie$, acct$(1), acct$(2))
            goto exit_program

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
            enabled% = 1%
                  on fieldnr% gosub L20180,         /* As Of Date       */~
                                    L20590,         /* Age Per Date     */~
                                    L20650,,        /* Selected Account */~
                                    L20750          /* When to Ask      */
                     return

L20180
*        As Of Date                            ASOF$
            inpmessage$ = "Transactions posted after this date are"  &   ~
                          " ignored by the report."
            if asof$ = " " or asof$ = blankdate$ then asof$ = date$
            call "DATFMTC" (asof$)
            if first% = 0% then enabled% = 0%
            return

L20590
*        Age Per Date                          AGEPER$
            inpmessage$ = "Enter 'D' to Age by Cash Disc Date, 'N' by" & ~
                          " Net Due Date, or 'I' by Invoice Date."
            if ageper$  = " " then ageper$ = "D"
            if first% = 0% then enabled% = 0%
            return

L20650
*        Selected Account
            inpmessage$ = "Enter G/L Account Range or 'ALL' & Include" & ~
                          "/Exclude selection (via I or E)."
            if first% = 0% then enabled% = 0%
            if acctie$  = " " then acctie$  = "I"
            if acct$(1) <> " " then return
               acct$(1) = "ALL"
               acct$(2) = " "
            return

L20750
*        When to Ask
            inpmessage$ = "Indicate How often to prompt for these"     & ~
                          " Parameters"
            if str(whentoask$()) = " " then whentoask$(3) = "X"
            if first% = 0% then enabled% = 0%
            return

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

            deffn'101(fieldnr%, edit%)
                  str(line2$,62%) = "ARQTBPRM: " & str(cms2v$,,8%)
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                                   else init(hex(86)) lfac$()
                  if edit% = 1% and fieldnr% < 4% then lfac$(4) = hex(9c)
                  on fieldnr% gosub L40190,         /* As Of Date       */~
                                    L40190,         /* Age per date     */~
                                    L40190,,        /* Selected Account */~
                                    L40190          /* When to Ask      */
                  goto L40300

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80) : return
L40190:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81) : return
                  REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82) : return
L40300:     accept                                                       ~
               at (01,02),                                               ~
                  "A/R TRIAL BALANCE PARAMETERS",                        ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "As Of Date",                                 ~
               at (06,30), fac(lfac$( 1)), asof$                , ch(10),~
               at (06,49), "Last Settled on ",                           ~
               at (06,65), fac(hex(8c)), last_stl$              , ch(10),~
                                                                         ~
               at (07,02), "Age Per Date (Disc/Net/Inv)",                ~
               at (07,30), fac(lfac$( 2)), ageper$              , ch(01),~
                                                                         ~
               at (08,02), "Include/Exclude G/L Accts:",                 ~
               at (08,30), fac(lfac$( 3)), acct$(1)             , ch(12),~
               at (08,43), "to",                                         ~
               at (08,46), fac(lfac$( 3)), acct$(2)             , ch(12),~
               at (08,60), "Include/Exclude",                            ~
               at (08,76), fac(lfac$( 3)), acctie$              , ch(01),~
                                                                         ~
               at (10,02), "Aging Parameters:",                          ~
               at (10,20), "1)", at(11,20), "2)", at(12,20), "3)",       ~
               at (13,20), "4)", at(14,20), "5)", at(10,30), "Days",     ~
               at (10,23), fac(lfac$( 4)), aging%( 1)       ,pic(-#####),~
               at (11,23), fac(lfac$( 4)), aging%( 2)       ,pic(-#####),~
               at (12,23), fac(lfac$( 4)), aging%( 3)       ,pic(-#####),~
               at (13,23), fac(lfac$( 4)), aging%( 4)       ,pic(-#####),~
               at (14,23), fac(lfac$( 4)), aging%( 5)       ,pic(-#####),~
               at (10,37), fac(lfac$( 4)), aging$( 1)           , ch(20),~
               at (11,37), fac(lfac$( 4)), aging$( 2)           , ch(20),~
               at (12,37), fac(lfac$( 4)), aging$( 3)           , ch(20),~
               at (13,37), fac(lfac$( 4)), aging$( 4)           , ch(20),~
               at (14,37), fac(lfac$( 4)), aging$( 5)           , ch(20),~
                                                                         ~
               at (16,02),                                               ~
                  "How often should these parameters be reviewed:",      ~
                                                                         ~
               at (17,02), fac(lfac$( 5)), whentoask$(1)        , ch(01),~
               at (17,04),                                               ~
                  "Each time a Trial Balance Inquiry is requested",      ~
               at (18,02), fac(lfac$( 5)), whentoask$(2)        , ch(01),~
               at (18,04),                                               ~
                  "Each time the Bill-To Customer Changes",              ~
               at (19,02), fac(lfac$( 5)), whentoask$(3)        , ch(01),~
               at (19,04),                                               ~
                  "No further prompts for this session",                 ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,20), fac(hex(8c)), pf8$,                           ~
               at (22,65), "(13)Instructions",                           ~
               at (23,20), fac(hex(8c)), pf4$,                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(8c)), pf16$,                          ~
                     keys(hex(000104080d0f10)), key (keyhit%)

               if keyhit% <> 13 then L40865
                  call "MANUAL" ("ARQTBPRM")
                  goto L40300

L40865:        if keyhit% <> 15 then L40885
                  call "PRNTSCRN"
                  goto L40300

L40885:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50200,         /* As Of Date       */~
                                    L50350,         /* Age Per Date     */~
                                    L50400,,        /* Selected Account */~
                                    L50600          /* When to Ask      */
                  return

L50200
*        As Of Date                            ASOF$
            call "DATEOKC" (asof$, u3%, errormsg$)
            if errormsg$ <> " " then return
                call "DATUFMTC" (asof$)
                call "DATUFMTC" (last_stl$)
                if asof$ <  last_stl$ then errormsg$ =                   ~
        "As Of Date cannot be before last settling date."
                asofu$ = asof$
                call "DATFMTC" (asof$)
                call "DATFMTC" (last_stl$)
                if edit% = 2% then                                       ~
                     call "ARMDATES" (asofu$, " ", aging%(), 1%, 1%,     ~
                                                           aging$(), 99%)
                return

L50350
*        Age Per                               AGEPER$
            if pos("DNI" = ageper$) > 0% then return
                errormsg$ = "Enter 'D' or 'N'."
                return

L50400
*        Select G/L Account
            if acct$(1) = "ALL" then L50540
            if (pos("IE" = acctie$) = 0%) then acctie$ = "I"
                call "GLVALID" (acct$(1), temp$, errormsg$)
                    if errormsg$ <> " " then return
                temp$ = hex(06) & "From Account.."
                call "GETCODE" (#4, acct$(1), temp$, 0%, 0, f1%(4))
                if acct$(2) = " " then acct$(2) = acct$(1)
                call "GLVALID" (acct$(2), temp$, errormsg$)
                    if errormsg$ <> " " then return
                temp$ = hex(06) & "To Account...."
                call "GETCODE" (#4, acct$(2), temp$, 0%, 0, f1%(4))
                if acct$(2) < acct$(1) then errormsg$ = "Invalid Range."
                return
L50540:     acct$(2) = " "
            acctie$ = "I"
            return

L50600
*        When to ask
            p% = pos(str(whentoask$()) <> " ")
            if p% <> 0% then L50650
               p% = cursor%(1%) - 16%
               if p% < 1% or p% > 3% then L50690
L50650:     str(whentoask$()) = " "
            str(whentoask$(),p%,1%) = "X"
            return

L50690:     errormsg$ = "Indicate how often to review these Parameters"
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

            end
