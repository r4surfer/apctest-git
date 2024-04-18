        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  PPPP   RRRR   L      TTTTT   CCC   L      BBBB   L       *~
            *  P   P  R   R  L        T    C   C  L      B   B  L       *~
            *  PPPP   RRRR   L        T    C      L      BBBB   L       *~
            *  P      R   R  L        T    C   C  L      B   B  L       *~
            *  P      R   R  LLLLL    T     CCC   LLLLL  BBBB   LLLLL   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLTCLBL - Prints Time Card labels based on user input.   *~
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
            * 08/21/90 ! Original (Custom for Conklin Instrument) ! JIM *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            autopay$1,                   /* Auto Pay code              */~
            curdept$4,                   /* Employee's department      */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            empcode$12,                  /* Employee Code              */~
            empcode$(4)12,               /* Employee Code Range        */~
            errormsg$79,                 /* Error message              */~
            frstnme$10,                  /* Employee 1st name          */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lastnme$15,                  /* Employee last name         */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            midlnme$1,                   /* Employee middle initial    */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            pline1$(4)31,                /* Label line 1               */~
            pline2$(4)31,                /* Label line 2               */~
            pline3$(4)31,                /* Label line 3               */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            rptid$6,                     /* Report ID                  */~
            userid$3,                    /* Current User Id            */~
            weekend$8                    /* Week-Ending Date           */

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
            * #01 ! PERMASTR ! Personnel master file-ties to EMPMASTR i *~
            * #02 ! EMPMASTR ! Employee master file                     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "PERMASTR",                                      ~
                        varc,     indexed,  recsize =  950,              ~
                        keypos =   39, keylen =  12,                     ~
                        alt key  1, keypos =   28, keylen =  23,         ~
                            key  2, keypos =    2, keylen =  49,         ~
                            key  3, keypos =    1, keylen =  50

            select #02, "EMPMASTR",                                      ~
                        varc,     indexed,  recsize =  136,              ~
                        keypos =    1, keylen =  12,                     ~
                        alt key  1, keypos =   70, keylen =   1, dup

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01), 0%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02), 0%, rslt$(02))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$)
            date$ = date : call "DATEFMT" (date$)
            edtmessage$ = "To modify displayed values, position cursor "&~
                "to desired value & press (RETURN)."
            str(line2$,62) = "PRLTCLBL: " & str(cms2v$,,8)
            rptid$ = "PRL999"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  2%
                gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10160
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit% =  1% then gosub startover
                      if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10120
L10160:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10120
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
                  if keyhit%  = 16% then       print_the_labels
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% >  2% then editpg1
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
            * P R I N T   P A Y R O L L   T I M E C A R D   L A B E L S *~
            *************************************************************

        print_the_labels
            call "SHOSTAT" ("Printing Payroll Time Card Labels ... plea"&~
                "se stand by.")
            gosub clear_print
            select printer (126)
            call "SETPRNT" (rptid$, " ", 0%, 0%)
            plowkey$ = xor plowkey$
            str(plowkey$,,12) = empcode$(3)
            call "READ102" (#01, plowkey$, f1%(1)) /* 'Prime' PERMASTR */
            goto L19170

L19150
*        Read the records ... print the labels.
            call "READNEXT" (#01, f1%(1))
L19170:         if f1%(1) = 0% then goto end_of_labels
            get #01 using L19200, lastnme$, frstnme$, midlnme$, empcode$, ~
                curdept$
L19200:         FMT POS(2), CH(15), CH(10), CH(1), POS(39), CH(12),      ~
                     POS(369), CH(4)
            if empcode$ > empcode$(4) then goto end_of_labels
            autopay$ = " "
            call "READ100" (#02, empcode$, f1%(2))
                if f1%(2) <> 0% then get #02 using L19260, autopay$
L19260:         FMT POS(57), CH(1)
            if autopay$ <> "N" then goto L19150

*        This employee will have a label printed.
            if printsw% >= 4% then gosub print_labels

*        Store current employee in a set of print array elements.
            printsw% = printsw% + 1%
            str(pline1$(printsw%), 2, 3) = "Emp"
            str(pline1$(printsw%), 6,12) = empcode$
            str(pline1$(printsw%),21, 4) = "Dept"
            str(pline1$(printsw%),26, 4) = curdept$
            str(pline2$(printsw%), 2,28) = frstnme$ & " " & midlnme$ &   ~
                " " & lastnme$
            call "SPCESMSH" (str(pline2$(printsw%)), 1%)
            str(pline3$(printsw%), 2,13) = "Period Ending"
            str(pline3$(printsw%),16, 8) = weekend$
            goto L19150

        end_of_labels
            if printsw% > 0% then gosub print_labels
            close printer
            call "SETPRNT" (rptid$, " ", 0%, 1%)
            goto inputmode

        print_labels
            print skip
            print using L19570, str(pline1$())
            print using L19570, str(pline2$())
            print using L19570, str(pline3$())

L19570: %################################################################~
        ~############################################################

            print skip (2)
        clear_print
            init (" ") pline1$(), pline2$(), pline3$()
            printsw% = 0%
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20120,         /* Week-Ending Date       */~
                              L20150          /* Employee Code Range    */
            return

L20120: REM Def/Enable Week-Ending Date            WEEKEND$
            return

L20150: REM Def/Enable Employee Code Range         EMPCODE$()
            if empcode$(1) <> " " then return
                empcode$(1) = "ALL"
                empcode$(2) = " "
                return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28110
                inpmessage$ = edtmessage$
                return

L28110
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Week-Ending Date.                                      ",~
         "Enter Employee Code Range, 'FIRST', 'LAST' or 'ALL'.         "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables
            init(" ") errormsg$, inpmessage$, empcode$(), weekend$
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
              on fieldnr% gosub L40160,         /* Week-Ending Date   */  ~
                                L40160          /* Employee Code Range*/
              goto L40190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40160:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40190:     accept                                                       ~
               at (01,02), "Payroll System Time Card Label Print",       ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Week-Ending Date",                           ~
               at (06,30), fac(lfac$( 1)), weekend$             , ch(08),~
                                                                         ~
               at (07,02), "Employee Code Range",                        ~
               at (07,30), fac(lfac$( 2)), empcode$(1)          , ch(12),~
               at (07,43), "thru",                                       ~
               at (07,48), fac(lfac$( 2)), empcode$(2)          , ch(12),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40440
                  call "MANUAL" ("PRLTCLBL") : goto L40190

L40440:        if keyhit% <> 15 then L40470
                  call "PRNTSCRN" : goto L40190

L40470:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40640     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40620
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40620:     return

L40640: if fieldnr% > 0% then L40730  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Print Labels"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40730:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50120,         /* Week-Ending Date       */~
                              L50160          /* Employee Code Range    */
            return

L50120: REM Test for Week-Ending Date             WEEKEND$
            call "DATEOK" (weekend$, u3%, errormsg$)
            return

L50160: REM Test for Employee Code Range          EMPCODE$()
            call "TESTRNGE" (empcode$(1), empcode$(2), empcode$(3),      ~
                empcode$(4), errormsg$)
            return

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
            *  Copyright (c) 1990  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "SHOSTAT" ("One Moment Please")
            end
