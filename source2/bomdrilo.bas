        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  BBBB    OOO   M   M  DDDD   RRRR   IIIII  L      L       *~
            *  B   B  O   O  MM MM  D   D  R   R    I    L      L       *~
            *  BBBB   O   O  M M M  D   D  RRRR     I    L      L       *~
            *  B   B  O   O  M   M  D   D  R   R    I    L      L       *~
            *  BBBB    OOO   M   M  DDDD   R   R  IIIII  LLLLL  LLLLL   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BOMDRILO - Simple driver for BOMBRWOP.  Prompts for       *~
            *            parent part and BOM version, then calls the    *~
            *            sub.  The sub handles all the drill-down       *~
            *            features.                                      *~
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
            * 01/08/93 ! Original                                 ! WPH *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            bom$(490)3,                  /* Component BOM array        */~
            cursor%(2),                  /* Cursor position coords     */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            err$79,                      /* Error message returned     */~
            hdr$(2)79,                   /* Header for Plowcode        */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            parent$25,                   /* Parent Part Number         */~
            parentbom$3,                 /* Bill Version               */~
            parentdescr$32,              /* Parent Part Description    */~
            parenttype$3,                /* Parent Part Type           */~
            pldate$6,                    /* Planning Base Date         */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            readkey$99,                  /* Miscellaneous Read         */~
            today$6,                     /* Todays date                */~
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
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "
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
            * #01 ! BOMMASTR ! BOM relationship File                    *~
            * #02 ! ENGMASTR ! Engineering Master File                  *~
            * #03 ! SYSFILE2 ! Caelus Management System Information     *~
            * #04 ! HNYMASTR ! Inventory Master File                    *~
            * #05 ! HNYOPTNS ! Inventory Master File                    *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "BOMMASTR",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =   26, keylen =  31,                     ~
                        alt key  1, keypos =    1, keylen =  56          ~

            select #02, "ENGMASTR",                                      ~
                        varc,     indexed,  recsize = 2015,              ~
                        keypos =    1, keylen =  29                      ~

            select #03, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select #04, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  3, keypos =   26, keylen =  32, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  1, keypos =  102, keylen =   9, dup

            select #05, "HNYOPTNS",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    1, keylen =  54

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01), 0%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02), 0%, rslt$(02))
            call "OPENCHCK" (#03, fs%(03), f2%(03), 0%, rslt$(03))
            call "OPENCHCK" (#04, fs%(04), f2%(04), 0%, rslt$(04))
            call "OPENCHCK" (#05, fs%(05), f2%(05), 0%, rslt$(05))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            today$, date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62%) = "BOMDRILO: " & str(cms2v$,,8%)

            call "READ100" (#3, "MONTHS OPEN", f1%(3%))
                if f1%(3%) = 0% then L65000
            get #3, using L09170, pldate$
L09170:         FMT XX(32), CH(6)

            call "PIPINDEX" (#3, today$, todayindex%, ret%)
                if ret% <> 0% then exit_program

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            parent$ = " "
        inputmode2
            gosub initialize_variables

            for fieldnr% = 1% to  2%
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

            goto call_the_sub




        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Parent Part            */~
                              L20200          /* Bill Version           */
            return
L20100: REM Def/Enable Parent Part Number          PARENT$
            return

L20200: REM Def/Enable Bill Version                PARENTBOM$
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
         "Enter Parent Part Number or partial or blank to see list.    ",~
         "Enter Bill Version, or '?' to see list, or blank for effective"

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                               parentbom$
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
            *                  P R O C E S S I N G                      *~
            *-----------------------------------------------------------*~
            * Just call the subroutine.                                 *~
            *************************************************************

        call_the_sub

            call "BOMBRWOP" (#1, #2, #3, #4, #5, parent$, parentbom$,    ~
                             err$)

            if err$ = " " then goto inputmode2

            k% = 2%
            call "ASKUSER" (k%, "* * * *  NOTE  * * * *",                ~
                   err$,                                                 ~
                   "Part Number: " & parent$,                            ~
                   "Press any key to acknowledge.")

            goto inputmode

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
              on fieldnr% gosub L40080,         /* Parent Part       */   ~
                                L40080          /* Bill Version      */
              goto L40095

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40080:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40095:     accept                                                       ~
               at (01,02),                                               ~
                  "Option Bill of Materials Review",                     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Parent Part Number",                         ~
               at (06,30), fac(lfac$( 1%)), parent$             , ch(25),~
                                                                         ~
               at (07,02), "Bill Version",                               ~
               at (07,30), fac(lfac$( 2%)), parentbom$          , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L40220
                  call "MANUAL" ("BOMBRWOP") : goto L40095

L40220:        if keyhit% <> 15% then L40235
                  call "PRNTSCRN" : goto L40095

L40235:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40330     /*  Input Mode             */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2%)= "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40310
                str(pf$(3%),64%)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40310:     if fieldnr% > 2% then L40320
                str(pf$(2%),18%,26%) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40320:     return

L40330: if fieldnr% > 0% then L40375  /*  Edit Mode - Select Fld */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                 (5)Next Screen         " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01ffffff05ffffffffffffff0dff0f1000)
            return
L40375:                              /*  Edit Mode - Enabled    */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
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
            on fieldnr% gosub L50100,         /* Parent Part            */~
                              L50200          /* Bill Version           */
            return
L50100: REM Test for Parent Part Number           PARENT$
                if parent$ = "?" then parent$ = " "
                call "GETCODE"(#4, parent$,  parentdescr$,0%,0,f1%(4))
                     if f1%(4%) <> 0% then L50160
                     errormsg$ = "Assembly Part Number Not On File: "    ~
                                  & parent$
                     return
L50160:         get #4, using L50170, parenttype$
L50170:         FMT XX(179), CH(3)
                if parenttype$ <> "000" then L50185
                return

L50185:         errormsg$ = "Assembly Part Number Not Generic."
                return

L50200: REM Test for Bill Version                 PARENTBOM$
            if parentbom$ = " " then L50340
            if parentbom$ = "?" then parentbom$ = "   "
L50220:     readkey$ = str(parent$,,25%) & str(parentbom$,,3%)
            hdr$()="  Listed Below Are The Existing BOMs For Part: " &   ~
                                                                parent$
            errormsg$ = hex(06) & "Select Bill Of Materials"
            call "PLOWCODE" (#1, readkey$, errormsg$, 2025%, .30,        ~
                                                     f1%(1%),  hdr$(), 3)
            errormsg$ = " "
                if f1%(1%) <> 0% then L50310
                errormsg$ = "BOM not on file" : return
L50310:     parentbom$ = str(readkey$,26%,3%)
            return

L50340
*        Get the effective BOM
            init(hex(00)) readkey$
            str(readkey$,1%,29%) = str(parent$,1%,25%) & "1" & "001"
            call "READ100" (#2, readkey$, f1%(2%))   /* engmastr */
                 if f1%(2%) <> 1% then L50220
                   get #2, using L50380,  bom$()
L50380:               FMT POS(30), 490 * CH(3)
            parentbom$  = bom$(todayindex%)

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
