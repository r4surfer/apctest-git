        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  PPPP    AAA   Y   Y   AAA    CCC    CCC   TTTTT   SSS    *~
            *  P   P  A   A  Y   Y  A   A  C   C  C   C    T    S       *~
            *  PPPP   AAAAA   YYY   AAAAA  C      C        T     SSS    *~
            *  P      A   A    Y    A   A  C   C  C   C    T        S   *~
            *  P      A   A    Y    A   A   CCC    CCC     T     SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PAYACCTS - This Program Maintains the Classifications of  *~
            *            certain Liability Accounts for Accounts Payable*~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1993  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/09/93 ! Rewrite of Original with Additions       ! JBK *~
            * 01/16/95 ! PRR 13343.  Add more NON-AP Accts.       ! JDH *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            accounttype$1,               /* Account Type               */~
            apaccount$(28)16,            /* Valid AP Liability Accounts*/~
            apnumacct$2,                 /* Number of AP Accounts      */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(51)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            nonapaccount$(50)12,         /* Valid Non-AP Liability Acct*/~
            nonapnumacct$2,              /* Number of Non-AP Accounts  */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            pgmmsg$(7)74,                /* Screen Messages            */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
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
            cms2v$ = "R6.04.00 02/24/95 CMS General Release R6.04.00    "
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
            * #1  ! SYSFILE2 ! Caelus Management System Information     *~
            * #2  ! GLMAIN   ! General Ledger CHart Of Accounts File.   *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select #2,  "GLMAIN",                                        ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   9                      ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(1%), f2%(1%), 0%, rslt$(1%))
            call "OPENCHCK" (#02, fs%(2%), f2%(2%), 0%, rslt$(2%))

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

            str(line2$,62) = "PAYACCTS: " & str(cms2v$,,8)

            pgmmsg$(1%) = "This classification of G/L LIABILITY accou"&  ~
                          "nts includes accounts   "
            pgmmsg$(2%) = "that are to be used for Trade Accounts pay"&  ~
                          "able (ie. an invoice    "
            pgmmsg$(3%) = "will be received and a check will be issue"&  ~
                          "d in payment).          "
            pgmmsg$(4%) = " "
            pgmmsg$(5%) = "This classification of G/L LIABILITY accou"&  ~
                          "nts includes accounts that are  "
            pgmmsg$(6%) = "expected to be used to accrue liabilities "&  ~
                          "which will be satisfied with the"
            pgmmsg$(7%) = "issuance of a payment, but for which no in"&  ~
                          "voice is expected to be received"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

*        See if the records already exist
            plowkey$ = "APACCOUNTS"
            call "READ100" (#1, plowkey$, ap%)
                if ap% = 0% then L10160
            gosub dataload1

L10160:     plowkey$ = "APACCOUNTS-NON"
            call "READ100" (#1, plowkey$, nonap%)
                if nonap% = 0% then L10210
            gosub dataload2

L10210:     if ap% = 1% and nonap% = 1% then editpg1

            for fieldnr% = 1% to  2%
L10510:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10630
L10530:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10610
L10560:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10530
                         if fieldnr% = 1% then L10510
                         goto L10560
L10610:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10530
L10630:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10530
            next fieldnr%

            for fieldnr% = 1% to  2%
L10680:         gosub'052(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10800
L10700:         gosub'102(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10780
L10730:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'052(fieldnr%)
                         if enabled% = 1% then L10700
                         if fieldnr% = 1% then L10680
                         goto L10730
L10780:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10700
L10800:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10700
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
                  if keyhit%  =  5% then       editpg2
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       editpg1
L11130:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% = 1% then L11150
            if fieldnr% < 8% or fieldnr% > 13% then editpg1
                fieldnr% = 2%
L11150:     if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11180:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11180
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11180
                  lastfieldnr% = fieldnr%
            goto L11130

        editpg2
            lastfieldnr% = 0%
            gosub'102(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  4% then       editpg1
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       editpg2
L11330:     fieldnr% = cursor%(1%) - 3%
            if fieldnr% = 1% then L11350
            if fieldnr% < 8% or fieldnr% > 17% then editpg2
                fieldnr% = 2%
L11350:     if fieldnr% = lastfieldnr% then    editpg2
            gosub'052(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg2
L11380:     gosub'102(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11380
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11380
                  lastfieldnr% = fieldnr%
            goto L11330

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub dataput
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* APNUMACCT$             */~
                              L20200          /* APACCOUNT$()           */
            return
L20100: REM Def/Enable Number of AP Accounts       APNUMACCT$
            if apnumacct$ = " " then apnumacct$ = " 1"
            return

L20200: REM Def/Enable Valid AP Liability Accounts APACCOUNT$(1)
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  2  of Input. *~
            *************************************************************

        deffn'052(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L21100,         /* NONAPNUMACCT$          */~
                              L21200          /* NONAPACCOUNT$()        */
            return
L21100: REM Def/Enable Number of Non-AP Accounts   NONAPNUMACCT$
            if nonapnumacct$ = " " then nonapnumacct$ = " 1"
            return

L21200: REM Def/Enable Valid Non-AP Liability Acct NONAPACCOUNT$(1)
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
            if scrnr% = 2% then restore line = scrn2_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Number of AP Accounts                                  ",~
         "Enter Valid AP Liability Accounts                            "

        scrn2_msg  :  data                                               ~
         "Enter Number of Non-AP Accounts                              ",~
         "Enter Valid Non-AP Liability Acct                            "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      apaccount$(), apnumacct$, nonapaccount$(),         ~
                      nonapnumacct$, accounttype$
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1993  an unpublished work by CAELUS,       *~
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
        dataload1
            get #1 using L30070, acctnum%, apaccount$()
L30070:         FMT POS(21), BI(2), 28*CH(16)
            convert acctnum% to apnumacct$, pic(##)
            for i% = 1% to 28%
                if apaccount$(i%) = " " then L30120
                call "GLFMT" (apaccount$(i%))
L30120:     next i%
            call "LINSMASH" (apaccount$())
            return

        dataload2
            get #1 using L30220, nonacctnum%, nonapaccount$()
L30220:         FMT POS(21), BI(2), 50*CH(09)
            convert nonacctnum% to nonapnumacct$, pic(##)
            for i% = 1% to 50%
                if nonapaccount$(i%) = " " then L30270
                call "GLFMT" (nonapaccount$(i%))
L30270:     next i%
            call "LINSMASH" (nonapaccount$())
            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            plowkey$ =    "APACCOUNTS"
            call "READ101" (#1, plowkey$, f1%(1%))
                if f1%(1%) = 0% then L31100
            delete #1
L31100:     if str(apaccount$()) = " " then L31180
            for i% = 1% to 28%
                if apaccount$(i%) = " " then L31140
                call "GLUNFMT" (apaccount$(i%))
L31140:     next i%
            write #1 using L31160, plowkey$, acctnum%, apaccount$(), " "
L31160:         FMT CH(20), BI(2), 28*CH(16), CH(30)

L31180:     plowkey$ =    "APACCOUNTS-NON"
            call "READ101" (#1, plowkey$, f1%(1%))
                if f1%(1%) = 0% then L31220
            delete #1
L31220:     if str(nonapaccount$()) = " " then L31300
            for i% = 1% to 50%
                if nonapaccount$(i%) = " " then L31260
                call "GLUNFMT" (nonapaccount$(i%))
L31260:     next i%
            write #1 using L31290, plowkey$, nonacctnum%, nonapaccount$(),~
                                  " "
L31290:         FMT CH(20), BI(2), 50*CH(09), CH(28)
L31300:     goto L65000

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              gosub set_pf1
              init (" ")  str(line2$,,60%)
              str(line2$,1%,60%) = "Valid A/P Liability Accounts"
              if fieldnr% > 0% then L40065
                  init (hex(86))  lfac$()  :  goto L40095
L40065:       if fieldnr% > 1% then L40085
                  init (hex(8c))  lfac$()
                  lfac$(1%) = hex(82)
                  if errormsg$ <> " " then L40105 else goto L40095
L40085:       if errormsg$ <> " " then L40095
                  init (hex(81))  str(lfac$(),2%)  :  lfac$(1%) = hex(8c)
L40095:       init (hex(9c))  str(lfac$(),1%+acctnum%+1%)

L40105:     accept                                                       ~
               at (01,02),                                               ~
                  "A/P - Liability Account Classification",              ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Number of A/P Accounts",                     ~
               at (06,30), fac(lfac$( 1)), apnumacct$           , ch(02),~
               at (08,10), fac(hex(8c)), pgmmsg$(1%)            , ch(66),~
               at (09,10), fac(hex(8c)), pgmmsg$(2%)            , ch(66),~
               at (10,10), fac(hex(8c)), pgmmsg$(3%)            , ch(66),~
                                                                         ~
               at (12,02), "Valid A/P Liability Accounts",               ~
               at (13,02), fac(lfac$( 2%)), apaccount$( 1%)     , ch(12),~
               at (13,17), fac(lfac$( 3%)), apaccount$( 2%)     , ch(12),~
               at (13,32), fac(lfac$( 4%)), apaccount$( 3%)     , ch(12),~
               at (13,47), fac(lfac$( 5%)), apaccount$( 4%)     , ch(12),~
               at (13,62), fac(lfac$( 6%)), apaccount$( 5%)     , ch(12),~
               at (14,02), fac(lfac$( 7%)), apaccount$( 6%)     , ch(12),~
               at (14,17), fac(lfac$( 8%)), apaccount$( 7%)     , ch(12),~
               at (14,32), fac(lfac$( 9%)), apaccount$( 8%)     , ch(12),~
               at (14,47), fac(lfac$(10%)), apaccount$( 9%)     , ch(12),~
               at (14,62), fac(lfac$(11%)), apaccount$(10%)     , ch(12),~
               at (15,02), fac(lfac$(12%)), apaccount$(11%)     , ch(12),~
               at (15,17), fac(lfac$(13%)), apaccount$(12%)     , ch(12),~
               at (15,32), fac(lfac$(14%)), apaccount$(13%)     , ch(12),~
               at (15,47), fac(lfac$(15%)), apaccount$(14%)     , ch(12),~
               at (15,62), fac(lfac$(16%)), apaccount$(15%)     , ch(12),~
               at (16,02), fac(lfac$(17%)), apaccount$(16%)     , ch(12),~
               at (16,17), fac(lfac$(18%)), apaccount$(17%)     , ch(12),~
               at (16,32), fac(lfac$(19%)), apaccount$(18%)     , ch(12),~
               at (16,47), fac(lfac$(20%)), apaccount$(19%)     , ch(12),~
               at (16,62), fac(lfac$(21%)), apaccount$(20%)     , ch(12),~
               at (17,02), fac(lfac$(22%)), apaccount$(21%)     , ch(12),~
               at (17,17), fac(lfac$(23%)), apaccount$(22%)     , ch(12),~
               at (17,32), fac(lfac$(24%)), apaccount$(23%)     , ch(12),~
               at (17,47), fac(lfac$(25%)), apaccount$(24%)     , ch(12),~
               at (17,62), fac(lfac$(26%)), apaccount$(25%)     , ch(12),~
               at (18,02), fac(lfac$(27%)), apaccount$(26%)     , ch(12),~
               at (18,17), fac(lfac$(28%)), apaccount$(27%)     , ch(12),~
               at (18,32), fac(lfac$(29%)), apaccount$(28%)     , ch(12),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40390
                  call "MANUAL" ("PAYACCTS") : goto L40105

L40390:        if keyhit% <> 15 then L40405
                  call "PRNTSCRN" : goto L40105

L40405:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40500     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40480
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40480:     if fieldnr% > 2% then L40490
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40490:     return

L40500: if fieldnr% > 0% then L40545  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (5)Next Screen         " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01ffffff05ffffffffffffff0dff0f1000)
            return
L40545:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
              gosub'050(2%, fieldnr%)
              gosub set_pf2
              str(line2$,1%,60%) = "Valid Non-A/P Liability Accounts"
              if fieldnr% > 0% then L41060
                  init (hex(86))  lfac$()  :  goto L41090
L41060:       if fieldnr% > 1% then L41080
                  init (hex(8c))  lfac$()
                  lfac$(1%) = hex(82)
                  if errormsg$ <> " " then L41100 else goto L41090
L41080:       if errormsg$ <> " " then L41090
                  init (hex(81))  str(lfac$(),2%)  :  lfac$(1%) = hex(8c)
L41090:       init (hex(9c))  str(lfac$(),1%+nonacctnum%+1%)

L41100:     accept                                                       ~
               at (01,02),                                               ~
                  "A/P - Liability Account Classification",              ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Number of Non-A/P Accounts",                 ~
               at (04,30), fac(lfac$( 1)), nonapnumacct$        , ch(02),~
               at (06,05), fac(hex(8c)), pgmmsg$(5%)            , ch(74),~
               at (07,05), fac(hex(8c)), pgmmsg$(6%)            , ch(74),~
               at (08,05), fac(hex(8c)), pgmmsg$(7%)            , ch(74),~
               at (10,02), "Valid Non-A/P Liability Acct",               ~
               at (11,02), fac(lfac$( 2%)), nonapaccount$( 1%)  , ch(12),~
               at (11,17), fac(lfac$( 3%)), nonapaccount$( 2%)  , ch(12),~
               at (11,32), fac(lfac$( 4%)), nonapaccount$( 3%)  , ch(12),~
               at (11,47), fac(lfac$( 5%)), nonapaccount$( 4%)  , ch(12),~
               at (11,62), fac(lfac$( 6%)), nonapaccount$( 5%)  , ch(12),~
               at (12,02), fac(lfac$( 7%)), nonapaccount$( 6%)  , ch(12),~
               at (12,17), fac(lfac$( 8%)), nonapaccount$( 7%)  , ch(12),~
               at (12,32), fac(lfac$( 9%)), nonapaccount$( 8%)  , ch(12),~
               at (12,47), fac(lfac$(10%)), nonapaccount$( 9%)  , ch(12),~
               at (12,62), fac(lfac$(11%)), nonapaccount$(10%)  , ch(12),~
               at (13,02), fac(lfac$(12%)), nonapaccount$(11%)  , ch(12),~
               at (13,17), fac(lfac$(13%)), nonapaccount$(12%)  , ch(12),~
               at (13,32), fac(lfac$(14%)), nonapaccount$(13%)  , ch(12),~
               at (13,47), fac(lfac$(15%)), nonapaccount$(14%)  , ch(12),~
               at (13,62), fac(lfac$(16%)), nonapaccount$(15%)  , ch(12),~
               at (14,02), fac(lfac$(17%)), nonapaccount$(16%)  , ch(12),~
               at (14,17), fac(lfac$(18%)), nonapaccount$(17%)  , ch(12),~
               at (14,32), fac(lfac$(19%)), nonapaccount$(18%)  , ch(12),~
               at (14,47), fac(lfac$(20%)), nonapaccount$(19%)  , ch(12),~
               at (14,62), fac(lfac$(21%)), nonapaccount$(20%)  , ch(12),~
               at (15,02), fac(lfac$(22%)), nonapaccount$(21%)  , ch(12),~
               at (15,17), fac(lfac$(23%)), nonapaccount$(22%)  , ch(12),~
               at (15,32), fac(lfac$(24%)), nonapaccount$(23%)  , ch(12),~
               at (15,47), fac(lfac$(25%)), nonapaccount$(24%)  , ch(12),~
               at (15,62), fac(lfac$(26%)), nonapaccount$(25%)  , ch(12),~
               at (16,02), fac(lfac$(27%)), nonapaccount$(26%)  , ch(12),~
               at (16,17), fac(lfac$(28%)), nonapaccount$(27%)  , ch(12),~
               at (16,32), fac(lfac$(29%)), nonapaccount$(28%)  , ch(12),~
               at (16,47), fac(lfac$(30%)), nonapaccount$(29%)  , ch(12),~
               at (16,62), fac(lfac$(31%)), nonapaccount$(30%)  , ch(12),~
               at (17,02), fac(lfac$(32%)), nonapaccount$(31%)  , ch(12),~
               at (17,17), fac(lfac$(33%)), nonapaccount$(32%)  , ch(12),~
               at (17,32), fac(lfac$(34%)), nonapaccount$(33%)  , ch(12),~
               at (17,47), fac(lfac$(35%)), nonapaccount$(34%)  , ch(12),~
               at (17,62), fac(lfac$(36%)), nonapaccount$(35%)  , ch(12),~
               at (18,02), fac(lfac$(37%)), nonapaccount$(36%)  , ch(12),~
               at (18,17), fac(lfac$(38%)), nonapaccount$(37%)  , ch(12),~
               at (18,32), fac(lfac$(39%)), nonapaccount$(38%)  , ch(12),~
               at (18,47), fac(lfac$(40%)), nonapaccount$(39%)  , ch(12),~
               at (18,62), fac(lfac$(41%)), nonapaccount$(40%)  , ch(12),~
               at (19,02), fac(lfac$(42%)), nonapaccount$(41%)  , ch(12),~
               at (19,17), fac(lfac$(43%)), nonapaccount$(42%)  , ch(12),~
               at (19,32), fac(lfac$(44%)), nonapaccount$(43%)  , ch(12),~
               at (19,47), fac(lfac$(45%)), nonapaccount$(44%)  , ch(12),~
               at (19,62), fac(lfac$(46%)), nonapaccount$(45%)  , ch(12),~
               at (20,02), fac(lfac$(47%)), nonapaccount$(46%)  , ch(12),~
               at (20,17), fac(lfac$(48%)), nonapaccount$(47%)  , ch(12),~
               at (20,32), fac(lfac$(49%)), nonapaccount$(48%)  , ch(12),~
               at (20,47), fac(lfac$(50%)), nonapaccount$(49%)  , ch(12),~
               at (20,62), fac(lfac$(51%)), nonapaccount$(50%)  , ch(12),~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L41370
                  call "MANUAL" ("PAYACCTS") : goto L41100

L41370:        if keyhit% <> 15 then L41385
                  call "PRNTSCRN" : goto L41100

L41385:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
        if edit% = 2% then L41480     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L41460
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L41460:     if fieldnr% > 2% then L41470
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L41470:     return

L41480: if fieldnr% > 0% then L41525  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Screen     " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            return
L41525:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50100,         /* APNUMACCT$             */~
                              L50200          /* APACCOUNT$()           */
            return
L50100: REM Test for Number of AP Accounts        APNUMACCT$
                if apnumacct$ = " " then  L50160
                convert apnumacct$ to acctnum%, data goto L50160
                if acctnum% <  1% or acctnum% > 28% then  L50160
                str(apaccount$(), acctnum% * 16% + 1%) = " "
                goto L50190
L50160:         errormsg$ = "Number of AP ACCOUNTS Must be between 1 " & ~
                            "and 28."
L50190:     return

L50200: REM Test for Valid AP Liability Accounts  APACCOUNT$(1)
            for i% = 1% to 28%
            if apaccount$(i%) = " " then L50390
            call "GETCODE" (#2, apaccount$(i%), " ", 0%, 0, f1%(2%))
                if f1%(2%) <> 0% then L50300
                     if errormsg$ = " " then errormsg$ = "Account Numb" &~
                                  "er Must be a Valid General Ledger "  &~
                                  "or Blank."
                     lfac$(i%+1%) = hex(91)
                     goto L50410
L50300:     get #2 using L50310, accounttype$
L50310:         FMT POS(40), CH(1)
            if accounttype$ = "L" then L50390
                if errormsg$ = " " then errormsg$ = "G/L Account Must " &~
                                  "be a Liability Account (Type 'L"     &~
                                  "')."
                lfac$(i%+1%) = hex(91)
                goto L50410

L50390:         lfac$(i%+1%) = hex(8c)
                goto L50410
L50410:    next i%
           if errormsg$ = " " then call "LINSMASH" (apaccount$())
           return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 2.                      *~
            *************************************************************

        deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L51100,         /* NONAPNUMACCT$          */~
                              L51200          /* NONAPACCOUNT$()        */
            return
L51100: REM Test for Number of Non-AP Accounts    NONAPNUMACCT$
                if nonapnumacct$ = " " then  L51160
                convert nonapnumacct$ to nonacctnum%, data goto L51160
                if nonacctnum% <  1% or nonacctnum% > 50% then  L51160
                str(nonapaccount$(), nonacctnum% * 12% + 1%) = " "
                goto L50190
L51160:         errormsg$ = "Number of Non-AP ACCOUNTS Must be betwee" & ~
                            "n 1 and 50."
            return

L51200: REM Test for Valid Non-AP Liability Acct  NONAPACCOUNT$(1)
            for i% = 1% to 50%
            if  nonapaccount$(i%) = " " then L51390
            call "GETCODE" (#2, nonapaccount$(i%), " ", 0%, 0, f1%(2%))
                if f1%(2%) <> 0% then L51300
                     if errormsg$ = " " then errormsg$ = "Account Numb" &~
                                  "er Must be a Valid General Ledger "  &~
                                  "or Blank."
                     lfac$(i%+1%) = hex(91)
                     goto L51410
L51300:     get #2 using L51310, accounttype$
L51310:         FMT POS(40), CH(1)
            if accounttype$ = "L" then L51390
                if errormsg$ = " " then errormsg$ = "G/L Account Must " &~
                                  "be a Liability Account (Type 'L')."

                lfac$(i%+1%) = hex(91)
                goto L51410

L51390:         lfac$(i%+1%) = hex(8c)
                goto L51410
L51410:     next i%
            if errormsg$ = " " then call "LINSMASH" (nonapaccount$())
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
            *  Copyright (c) 1993  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
