        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *   GGG   L      FFFFF  L       AAA    GGG    SSS           *~
            *  G      L      F      L      A   A  G      S              *~
            *  G GGG  L      FFFF   L      AAAAA  G GGG   SSS           *~
            *  G   G  L      F      L      A   A  G   G      S          *~
            *   GGG   LLLLL  F      LLLLL  A   A   GGG    SSS           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GLFLAGS  - Maintains User defined options for general     *~
            *            ledger export file.                            *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1990  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07-12-90 ! Original                                 ! RAC *~
            * 11/05/90 ! Added Management Values Flags & Accounts.! JDH *~
            *          !   Port Id added, also. Your welcome Rich.!     *~
            * 04/26/91 ! Removed Smoke & Mirrors Port ID.  Added  ! JDH *~
            *          !   Full Screen Edit.                      !     *~
            * 05/21/91 ! If creating SWITCHS.GL, Dual Books OFF.  ! JDH *~
            * 09/17/91 ! Added warning if no posting to happen.   ! JDH *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            accounts$(4)12,              /* Management Value Accounts  */~
            active$1,                    /* Export Active              */~
            backgnd$8,                   /* Allow Background Processing*/~
            cdate$8,                     /* Record Creation Date       */~
            cursor%(2),                  /* Cursor location for edit   */~
            cuser$3,                     /* Record created by          */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            idle$3,                      /* Idle time until task ends i*/~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            mgmnt_on$1,                  /* Management Value Posting?  */~
            nfac$(20)1,                  /* Field Attribute Characters */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            post$1,                      /* Post CMS general ledger    */~
            submit$1,                    /* Submittal class            */~
            temp$50,                     /* Misc variable              */~
            timeout$3,                   /* Time-out for Background Tas*/~
            userid$3                     /* Current User Id            */~

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
            cms2v$ = "R6.01.00 10/07/91 CMS General Release             "
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
            * #01 ! SYSFILE2 ! Caelus Management System Information     *~
            * #02 ! GLMAIN   ! General Ledger Main File                 *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select #02, "GLMAIN",                                        ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   9

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01), 0%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02), 0%, rslt$(02))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62) = "GLFLAGS : " & str(cms2v$,,8)

        check_system
            plowkey$ = "SWITCHS.GL"
            call "READ100" (#1, plowkey$, f1%(1))
                if f1%(1) = 0% then inputmode
            gosub dataload
            goto editpg1

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 1%
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

L11000: REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  9  then       mod_admin
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% > 1% and fieldnr% < 14% then fieldnr% = 1%
            if fieldnr% < 1% or  fieldnr% >  1% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
            goto L11120

        mod_admin    /* Allow maintenance of GL Module Administrators  */
            call "CMSMAINP" ("GL", "General Ledger")
            goto L11000

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub dataput
            goto L65000

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100          /* All Default/Enables    */
            return
L20100: REM Def/Enable Export Active               ACTIVE$

        REM Def/Enable Allow Background Processing BACKGND$

        REM Def/Enable Time-out for Background Tas TIMEOUT$

        REM Def/Enable Idle time until task ends i IDLE$

        REM Def/Enable Submittal class             SUBMIT$

        REM Def/Enable Post CMS general ledger     POST$

        REM Def/Enable Post Management Values?     MGMNT_ON$

        REM Def/Enable Management Values Accounts  ACCOUNTS$()
            return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28055
                inpmessage$ = edtmessage$
                return

L28055
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Export and Management Values parameters.               "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, submit$,                   ~
                      active$, backgnd$, idle$, post$, timeout$,         ~
                      mgmnt_on$, accounts$()
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1990  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
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
            goto check_system

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload

            get #1 using  L30140, dual$, active$, backgnd$, timeout%,     ~
                                idle%, submit$, post$, cuser$, cdate$,   ~
                                mgmnt_on$, accounts$()
L30140:     FMT XX(20), 2*CH(1), CH(8), 2*BI(2), 2*CH(1), CH(3), CH(8),  ~
                XX(11), CH(1), 4*CH(9)

            convert timeout% to timeout$, pic(##0)
            convert idle% to idle$,       pic(##0)

            for i% = 1% to 4%
                call "GLFMT" (accounts$(i%))
            next i%

            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            call "READ101" (#1, plowkey$, f1%(1))
            if f1%(1) = 1% then L31092
            cuser$ = userid$
            cdate$ = date

L31092:     for i% = 1% to 4%
                call "GLUNFMT" (accounts$(i%))
            next i%

            convert timeout$ to timeout%, data goto L31120
L31120:     convert idle$    to idle%,    data goto L31130
            if dual$ = " " then dual$ = "N"
L31130:     put #1 using L31160, plowkey$, dual$, active$, backgnd$,      ~
                                timeout%, idle%, submit$, post$, cuser$, ~
                                cdate$, userid$, date, mgmnt_on$,        ~
                                accounts$(), " ", " "
L31160:     FMT CH(20), 2*CH(1), CH(8), 2*BI(2), 2*CH(1), CH(3), CH(8),  ~
                CH(3), CH(8), CH(1), 4*CH(9), CH(149), CH(256)
            if f1%(1) = 0% then write #1 else rewrite #1

            return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              gosub set_pf1

              init(hex(86)) lfac$(), nfac$()
              if fieldnr% = 0% then L40290
                  init(hex(81)) lfac$()
                  init(hex(82)) nfac$()
                  goto L40290

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
                  lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40290:     accept                                                       ~
               at (01,02),                                               ~
                  "Manage General Ledger Export User Defined Options",   ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Export Active",                              ~
               at (06,66), fac(lfac$( 1)), active$              , ch(01),~
                                                                         ~
               at (07,04), "Background Processing Program Name",         ~
               at (07,66), fac(lfac$( 1)), backgnd$             , ch(08),~
                                                                         ~
               at (08,04),                                               ~
               "Time-out for Background Task submit (seconds)",          ~
               at (08,66), fac(nfac$( 1)), timeout$             , ch(03),~
                                                                         ~
               at (09,04), "Idle time until task ends itself (minutes)", ~
               at (09,66), fac(nfac$( 1)), idle$                , ch(03),~
                                                                         ~
               at (10,04), "Background Procedure Submittal Class (A-Z)", ~
               at (10,66), fac(lfac$( 1)), submit$              , ch(01),~
                                                                         ~
               at (12,02), "Update CMS general ledger",                  ~
               at (12,66), fac(lfac$( 1)), post$                , ch(01),~
                                                                         ~
               at (14,02), "Management Reporting Values Active?",        ~
               at (14,66), fac(lfac$( 1)), mgmnt_on$            , ch(01),~
                                                                         ~
               at (15,04), "Intra-Divisional Sales Account Code",        ~
               at (15,66), fac(lfac$( 1)), accounts$(1)         , ch(12),~
                                                                         ~
               at (16,04),                                               ~
               "Alternate Owners Customer Sales Account Code",           ~
               at (16,66), fac(lfac$( 1)), accounts$(2)         , ch(12),~
                                                                         ~
               at (17,04),                                               ~
               "Intra-Divisional Cost of Goods Sold Account Code",       ~
               at (17,66), fac(lfac$( 1)), accounts$(3)         , ch(12),~
                                                                         ~
               at (18,04),                                               ~
               "Alternate Owners Customer Cost of Goods Sold Account",   ~
               at (18,66), fac(lfac$( 1)), accounts$(4)         , ch(12),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40750
                  call "MANUAL" ("GLFLAGS ") : goto L40290

L40750:        if keyhit% <> 15 then L40765
                  call "PRNTSCRN" : goto L40290

L40765:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40875     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40855
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40855:     if fieldnr% > 2% then L40865
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40865:     return

L40875: if fieldnr% > 0% then L40920  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "(9)Maintain G/L Module Administrators   " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01ffffffffffffff09ffffff0dff0f1000)
            return
L40920:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50100          /* All Testing            */
            return

L50100: REM Test for Export Active                ACTIVE$
            if active$ = "Y" or active$ = "N" then L50130
                errormsg$ = "Enter 'Y'es or 'N'o for Export Active."
                return
L50130:     if active$ = "N" and mgmnt_on$ = "Y" then mgt_conflict_error

        REM Test for Allow Background Processing  BACKGND$

        REM Test for Time-out for Background Task TIMEOUT$
            if timeout$ = " " then timeout$ = "  0"
            if backgnd$ = " " then L50400
            convert timeout$ to n%, data goto L50330
            goto L50345
L50330:       errormsg$ = "Allowable range: 20 - 480 seconds for Time-out"
              return
L50345:     if n% < 20% or n% > 480% then L50330
            convert n% to timeout$, pic(##0)

L50400: REM Test for Idle time until task ends    IDLE$
            if idle$ = " " then idle$ = "  0"
            if backgnd$ = " " then L50480
            convert idle$ to n%, data goto L50430
            goto L50450
L50430:       errormsg$ = "Allowable range: 10 - 999 minutes for Idle."
              return
L50450:     if n% < 10% or n% > 999% then L50430
            convert n% to idle$, pic(##0)

L50480: REM Test for Submittal class              SUBMIT$
            if backgnd$ = " " then L50500
            if submit$ < "A"  or submit$ > "Z" then errormsg$ =          ~
            submit$ & " not a valid submittal class."
            if errormsg$ <> " " then return

L50500: REM Test for Post CMS general ledger      POST$
            if post$ = "Y" or post$ = "N" then L50540
                errormsg$ = "Enter 'Y'es, or 'N'o for Post CMS."
                return
L50540:     if active$ = "Y" or post$ = "Y" then L50700
                keyhit1% = 2%
                call "ASKUSER" (keyhit1%, "*** WARNING ***",             ~
                     "Posting to both Export and CMS have been turned " &~
                     "OFF!!", "Are you SURE this is desired?", "If not,"&~
                     " change entries prior to Data Save.  Hit" &        ~
                     " RETURN to acknowledge.")

L50700: REM Test for Posting Management Values    MGMNT_ON$
            if mgmnt_on$ = "Y" or mgmnt_on$ = "N" then L50735
                errormsg$ = "Enter 'Y'es, or 'N'o for Management Values"
                return
L50735:     if active$ = "N" and mgmnt_on$ = "Y" then mgt_conflict_error

        REM Test Intra-Div Sales Account          ACCOUNTS$(1)
            temp$ = hex(06) & "Intra-Divisional Sales Account"
            acct$ = accounts$(1) : gosub test_acct
            if errormsg$ = " " then L50820
                errormsg$ = "Intra-Divisional Sales " & errormsg$
                return
L50820:     accounts$(1) = acct$

        REM Test Alt Owner Sales Account          ACCOUNTS$(2)
            temp$ = hex(06) & "Alternate Owner Sales Account"
            acct$ = accounts$(2) : gosub test_acct
            if errormsg$ = " " then L50850
                errormsg$ = "Alternate Owner Sales " & errormsg$
                return
L50850:     accounts$(2) = acct$

        REM Test Intra-Div CoGS Account           ACCOUNTS$(3)
            temp$ = hex(06) & "Intra-Divisional CoGS Account"
            acct$ = accounts$(3) : gosub test_acct
            if errormsg$ = " " then L50880
                errormsg$ = "Intra-Divisional CoGS " & errormsg$
                return
L50880:     accounts$(3) = acct$

        REM Test Alt Owner CoGS Account           ACCOUNTS$(4)
            temp$ = hex(06) & "Alternate Owner CoGS Account"
            acct$ = accounts$(4) : gosub test_acct
            if errormsg$ = " " then L50910
                errormsg$ = "Alternate Owners CoGS " & errormsg$
                return
L50910:     accounts$(4) = acct$
            return

        test_acct         /* Test account number          */
            if acct$ = " " and mgmnt_on$ = "N" then return
            call "GETCODE" (#02, acct$, temp$, 0%, 0.30, f1%(2))
            if f1%(2) = 1% then L50990
                errormsg$ = "Account not on file!"
                return
L50990:     call "GLFMT" (acct$)
            return

        mgt_conflict_error
            errormsg$ = "Can't have Export OFF and Management Values ON."
            return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1990  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
