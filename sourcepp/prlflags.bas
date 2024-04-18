        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  PPPP   RRRR   L      FFFFF  L       AAA    GGG    SSS    *~
            *  P   P  R   R  L      F      L      A   A  G      S       *~
            *  PPPP   RRRR   L      FFFF   L      AAAAA  G GGG   SSS    *~
            *  P      R   R  L      F      L      A   A  G   G      S   *~
            *  P      R   R  LLLLL  F      LLLLL  A   A   GGG    SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLFLAGS - Set switches for Payroll - Personnel Module    *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/08/92 ! Original                                 ! JBK *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            first_time$(6)51,            /* Scrn Message - 1st Time In */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            ntoy$(6)51,                  /* 'N' to 'Y' Scrn Message    */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            prl_active$1,                /* Is CMS Payroll Active?     */~
            readkey$20,                  /* File Read Key              */~
            scrn_msg_hdr$50,             /* Scrn Message Header        */~
            scrn_msg$(6)51,              /* Scrn Message Body          */~
            yton$(6)51,                  /* 'Y' to 'N' Scrn Message    */~
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
            cms2v$ = "R6.02.01 11/05/92 Payroll Switch & Other          "
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
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01), 0%, rslt$(01))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

*        See if this User is a Data Base or Module Administrator
            call "CMSMACHK" ("PRL", lfac$(1), lfac$(2))
            if lfac$(1) = "Y" or lfac$(2) = "Y" then L09200
                call "ASKUSER" (0%, "*** SECURITY CHECK ***",  " ",      ~
                    "You must be a Data Base Administrator to run this" &~
                    " program.", " ")
                goto L65000

L09200:     call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62) = "PRLFLAGS: " & str(cms2v$,,8)

*        Define screen warning messages
            scrn_msg_hdr$ = "------  W A R N I N G  ------"
            call "STRING" addr("CT", scrn_msg_hdr$, 50%)

            first_time$(1%) = "Setting this flag to 'Y' will enable all" ~
                            & " the CMS  "
            first_time$(2%) = "Payroll/Personnel programs and disable t" ~
                            & "he SFC    "
            first_time$(3%) = "employee programs.  Setting this flag to" ~
                            & " 'N' will "
            first_time$(4%) = "have the reverse effect.  Unless Payroll" ~
                            & "/Personnel"
            first_time$(5%) = "is already installed or is to be, the su" ~
                            & "ggested   "
            first_time$(6%) = "setting for this flag is 'N'.           "

                  ntoy$(1%) = "Changing this flag from 'N' to 'Y' will " ~
                            & "enable the"
                  ntoy$(2%) = "CMS Payroll/Personnel programs to functi" ~
                            & "on.  If   "
                  ntoy$(3%) = "the SFC employee functions are currently" ~
                            & " being    "
                  ntoy$(4%) = "used, they will be disabled and any empl" ~
                            & "oyee data "
                  ntoy$(5%) = "they use will have to be re-built throug" ~
                            & "h the CMS "
                  ntoy$(6%) = "Payroll/Personnel functions.  Proceed wi" ~
                            & "th caution!"


                  yton$(1%) = "Changing this flag from 'Y' to 'N' will " ~
                            & "result in "
                  yton$(2%) = "the immediate and complete loss of acces" ~
                            & "s to any  "
                  yton$(3%) = "Payroll/Personnel data accumulated in th" ~
                            & "e database"
                  yton$(4%) = "and eventually to loss of the data itsel" ~
                            & "f.  Proceed"
                  yton$(5%) = "with caution!                           " ~

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables
            gosub dataload /*Load then Format stuff all nice and purty */
                if f1%(1%) = 1% then editpg1

            for fieldnr% = 1% to  1%
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
L11120:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% >  1% then editpg1
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
            on fieldnr% gosub L20100          /* Payroll Active         */
            return
L20100: REM Def/Enable Is CMS Payroll Active?      PRL_ACTIVE$
            if prl_active$ = " " then prl_active$ = "N"
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
         "Enter 'Y' if the CMS Payroll Module is Active, Else Enter 'N'"

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      prl_active$
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
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
            goto inputmode

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload
            init(" ")  scrn_msg$()
            call "READ100" (#1, "SWITCHS.PRL", f1%(1))
            if f1%(1) = 1% then L30130

*        No record on file
            prl_active$ = "N"
            mat scrn_msg$ = first_time$
            return

L30130:     get #1 using L30140, readkey$, prl_active$
L30140:         FMT CH(20), CH(1)
            mat scrn_msg$ = yton$
            if prl_active$ <> "Y" then mat scrn_msg$ = ntoy$
            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            call "READ101" (#1, "SWITCHS.PRL", f1%(1))
            put #1 using L31080, "SWITCHS.PRL", prl_active$, " ", " "
L31080:         FMT CH(20), CH(1), CH(239), CH(240)

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
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40075          /* Payroll Active    */
              goto L40090

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40075:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40090:     accept                                                       ~
               at (01,02),                                               ~
                  "Payroll - Personnel Behavior Switches",               ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Is CMS Payroll Active?",                     ~
               at (06,30), fac(lfac$( 1)), prl_active$          , ch(01),~
                                                                         ~
               at (13,15), fac(hex(84)), scrn_msg_hdr$          , ch(60),~
               at (14,15), fac(hex(8c)), scrn_msg$(1%)          , ch(60),~
               at (15,15), fac(hex(8c)), scrn_msg$(2%)          , ch(60),~
               at (16,15), fac(hex(8c)), scrn_msg$(3%)          , ch(60),~
               at (17,15), fac(hex(8c)), scrn_msg$(4%)          , ch(60),~
               at (18,15), fac(hex(8c)), scrn_msg$(5%)          , ch(60),~
               at (19,15), fac(hex(8c)), scrn_msg$(6%)          , ch(60),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40200
                  call "MANUAL" ("PRLFLAGS") : goto L40090

L40200:        if keyhit% <> 15 then L40215
                  call "PRNTSCRN" : goto L40090

L40215:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40310     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40290
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40290:     if fieldnr% > 2% then L40300
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40300:     return

L40310: if fieldnr% > 0% then L40355  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40355:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50100          /* Payroll Active         */
            return
L50100: REM Test for Is CMS Payroll Active?       PRL_ACTIVE$
            if pos("YN" = prl_active$) > 0% then return
                errormsg$ = "Valid Values are 'Y' or 'N'."
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
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
