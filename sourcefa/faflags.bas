        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  FFFFF   AAA   FFFFF  L       AAA    GGG    SSS           *~
            *  F      A   A  F      L      A   A  G      S              *~
            *  FFFF   AAAAA  FFFF   L      AAAAA  G GGG   SSS           *~
            *  F      A   A  F      L      A   A  G   G      S          *~
            *  F      A   A  F      LLLLL  A   A   GGG    SSS           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * FAFLAGS  - Set switches for Fixed Asset Module.           *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1988  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/22/88 ! Original                                 ! TLJ *~
            * 08/06/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            fiscbeg$10,                  /* Fiscal Year Beginning      */~
            fiscend$10,                  /* Fiscal Year Ending         */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            proration$1,                 /* Proration Convention       */~
            prodescr$20,                 /* Proration Convention DESCR */~
            userid$3                     /* Current User Id            */

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
            * #02 ! SYSFILE2 ! Caelus Management System Information     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #02, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#02, fs%(02), f2%(02), 0%, rslt$(02))
                if f2%(02) <> 0% then L64000

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)

*        See if this User is a Data Base or Module Administrator
            call "CMSMACHK" ("FA ", lfac$(1), lfac$(2))
            if lfac$(1) = "Y" or lfac$(2) = "Y" then L09130
            pf$(1) = "You must be a Data Base or FA Module Administrator"~
                     & " to run this program."
            call "ASKUSER" (0%, "SECURITY CHECK", " ", pf$(1), " ")
            goto exit_program

L09130:     date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62) = " FAFLAGS: " & str(cms2v$,,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables
            gosub dataload

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  9% then       mod_admin
                  if keyhit%  = 16% then       datasave
                  if keyhit%  = 32% then       exit_program
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% >  2% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%)       /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
            goto L11120

        REM *************************************************************~
            *           M O D U L E   A D M I N I S T R T O R S         *~
            *-----------------------------------------------------------*~
            * Allow setting of FA Module Administrators                 *~
            *************************************************************
        mod_admin:
            call "CMSMAINP" ("FA ", "Fixed Assets")
            goto editpg1

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            call "SHOSTAT" ("Saving Data, One Moment Please")
            gosub dataput
            goto  exit_program

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Fiscal Year            */~
                              L20200          /* Proration              */
            return
L20100: REM Def/Enable Fiscal Year                 FISCAL$
            return

L20200: REM Def/Enable Proration Convention        PRORATION$
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
        "Enter Fiscal Year                                             ",~
        "Proration Conventions: (1)Half-Year, (2)Mid-Quarter, (3)Mid-Mont~
        ~h"
        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      fiscbeg$, fiscend$, proration$
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1988  an unpublished work by CAELUS,       *~
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
            call "READ100" (#02, "SWITCHS.FA ", f1%(2))
            if f1%(2) = 1% then L30100
              a$ = " "
              call "DATFMTC" (date, 0%, str(a$, 1%, 4%))
              fiscbeg$ = a$ & "0101"
              call "DATFMTC" (fiscbeg$)
              fiscend$ = a$ & "1231"
              call "DATFMTC" (fiscend$)
              proration$ = "1"
              goto L30120
L30100:     get #02 using L30110, fiscbeg$, fiscend$, proration$
L30110:             FMT POS(21), CH(8), CH(8), CH(1)
            call "DATFMTC" (fiscbeg$)
            call "DATFMTC" (fiscend$)
L30120:     gosub get_proration_descr
            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            call "READ101" (#02, "SWITCHS.FA ", f1%(2))
            call "DATUFMTC" (fiscbeg$)
            call "DATUFMTC" (fiscend$)
            put #02 using L31080,"SWITCHS.FA ",fiscbeg$,fiscend$,proration$
L31080:         FMT CH(20), CH(8), CH(8), CH(1), XX(463)
            if f1%(2) = 0% then write #02 else rewrite #02
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%)
              gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40080,         /* Fiscal Year       */   ~
                                L40085          /* Proration         */
              goto L40095

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40080:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40085:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40095:     accept                                                       ~
               at (01,02),                                               ~
                  "Manage Fixed Assets Module Switches",                 ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Fiscal Year",                                ~
               at (06,35), fac(lfac$( 1)), fiscbeg$             , ch(10),~
               at (06,47), "TO",                                         ~
               at (06,51), fac(lfac$( 1)), fiscend$             , ch(10),~
                                                                         ~
               at (07,02), "Default Proration Convention",               ~
               at (07,35), fac(lfac$( 2)), proration$           , ch(01),~
               at (07,40), fac(hex(8c)),   prodescr$            , ch(20),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40220
                  call "MANUAL" ("FAFLAGS ") : goto L40095

L40220:        if keyhit% <> 15 then L40235
                  call "PRNTSCRN" : goto L40095

L40235:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1:
        if fieldnr% > 0% then L40375  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (9)Module Administrator" &        ~
                     "s                      (16/32)Save/Exit"
            pfkeys$ = hex(01ffffffffffffff09ffffff0dff0f102000ffffffff)
            return
L40375:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50100,         /* Fiscal Year            */~
                              L50200          /* Proration              */
            return

L50100: REM Test for Fiscal Year                  FISCAL$
            call "DATEOKC" (fiscbeg$, u3%, errormsg$)
                 if errormsg$ <> " " then return
            call "DATEOKC" (fiscend$, u4%, errormsg$)
                 if errormsg$ <> " " then return
            if u3% >= u4% then errormsg$ = "Fiscal Begining Date MUST be ~
        ~before the Fiscal Ending Date!"
            return

L50200: REM Test for Proration Convention         PRORATION$
            if proration$ < "1" or proration$ > "3" then L50260
              gosub get_proration_descr
              goto L50290
L50260:     errormsg$ = "Enter 1, 2, or 3."
            prodescr$ = " "
L50290:     return

        get_proration_descr:
            if proration$ = "1" then prodescr$ = "( Mid-Year )"
            if proration$ = "2" then prodescr$ = "( Mid-Quarter )"
            if proration$ = "3" then prodescr$ = "( Mid-Month )"
            return

L64000: REM SYSFILE2 OPEN ERROR
            u3% = 0%
            call "ASKUSER" (u3%,  "***** SYSFILE2 ERROR *****",          ~
                 "SYSFILE2 is either NOT OPEN or MISSING", " ",          ~
                 "Press RETURN to EXIT Program and Resolve the Problem")

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1988  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
