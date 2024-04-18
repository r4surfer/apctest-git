        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   SSS   H   H  PPPP   L       AAA   BBBB   EEEEE  L       *~
            *  S      H   H  P   P  L      A   A  B   B  E      L       *~
            *   SSS   HHHHH  PPPP   L      AAAAA  BBBB   EEE    L       *~
            *      S  H   H  P      L      A   A  B   B  E      L       *~
            *   SSS   H   H  P      LLLLL  A   A  BBBB   EEEEE  LLLLL   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SHPLABEL - This program generates shipping labels.  It    *~
            * prompts the user for the Sales Order range and the        *~
            * number of labels to print.                                *~
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
            * 07/09/90 ! Original (Robbins Scientific)            ! JIM *~
            * 02/22/91 ! Close each group of labels at DATASAVE.  ! JIM *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        dim                                                              ~
            cursor%(2),                  /* Cursor location for edit   */~
            cuscode$9,                   /* Customer code              */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            line2$79,                    /* Screen Line #2             */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            numlabels$2,                 /* Number of Labels to Print  */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            shiptoaddr$(6)30,            /* Ship to Address to Print   */~
            so_no$16,                    /* Sales Order Number         */~
            soout$(4)16,                 /* Sales order range to print */~
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
            cms2v$ = "R6.01.00 10/07/91 CMS General Release             "

            mat f2% = con
                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! BCKLINES ! Back Log Line Item File                  *~
            * #02 ! BCKMASTR ! Sales order master file                  *~
            *************************************************************~

            select #01, "BCKLINES", varc,     indexed,  recsize = 300,   ~
                        keypos =  10,  keylen = 19

            select #02, "BCKMASTR", varc, indexed, recsize = 1000,       ~
                        keypos =    1, keylen =  25

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
            edtmessage$  = "To modify displayed values, position cursor"&~
                " to desired value & press (RETURN)."
            str(line2$,62) = "SHPLABEL: " & str(cms2v$,,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub L29000                    /* Initialize variables */

            for fieldnr% = 1 to  2
                gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0 then L10160
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit% =  1 then gosub startover
                      if keyhit% = 16 and fieldnr% = 1 then exit_program
                      if keyhit% <> 0 then       L10120
L10160:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10120
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************~

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave  /* Print Labels */
                  if keyhit% <>  0 then       editpg1
L11120:     fieldnr% = cursor%(1) - 5   /* Input line is on line 6  */
            if fieldnr% < 1 or fieldnr% >  2 then editpg1
            if fieldnr% = lastfieldnr% then editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
            goto L11120

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave     /* Actually Prints the Labels Here */
            select printer(134)
            call "SETPRNT" ("SHP009", " ", 0%, 0%)
            gosub L30000  /* Print labels */
            close printer
            call "SETPRNT" ("SHP009", " ", 0%, 1%)
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20120,         /* Sales Order        */    ~
                              L20160          /* Number of Labels   */
            return

L20120: REM Def/Enable Sales Order Range           SOOUT$
            soout$(1) = "ALL"
            return

L20160: REM Def/Enable Number of Labels            NUMLABELS$
            numlabels$ = "1 "
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
         "Enter Sales Order Range to RePrint labels for.               ",~
         "Enter The Number of Shipping Labels Desired.                 "

L29000: REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

            init(" ") errormsg$, inpmessage$, soout$(), numlabels$
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
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

L30000: REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************

            call "SHOSTAT" ("Printing Shipping Labels ... stand by.")
            plowkey$ = soout$(3)

        main_loop
*        Sales Order numbers are accessed via reading BCKLINES.
            str(plowkey$,17,1) = hex(ff)
            call "READ102" (#01, plowkey$, f1%(1))
                if f1%(1) = 0% then return
            get #01 using L30150, cuscode$, so_no$
L30150:         FMT CH(9), CH(16)
            if so_no$ > soout$(4) then return
            plowkey$ = so_no$

*        Go after print information from BCKMASTR.
            call "READ100" (#02, str(cuscode$) & so_no$, f1%(2))
                if f1%(2) = 0% then goto main_loop
            get #02, using L30230, so_no$, po_no$, shiptoaddr$()
L30230:         FMT POS(10), CH(16), CH(16), 6*CH(30)
            call "LINSMASH" (shiptoaddr$())

            for labels% = 1% to cartons
                print skip(8)
                print using L60040, shiptoaddr$(1)
                print using L60040, shiptoaddr$(2)
                print using L60040, shiptoaddr$(3)
                print using L60040, shiptoaddr$(4)
                print using L60040, shiptoaddr$(5)
                print using L60040, shiptoaddr$(6)
                print skip(3)
                print using L60040, po_no$
            next labels%
            goto main_loop

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
              on fieldnr% gosub L40150,         /* Sales Order       */   ~
                                L40150          /* Number of Labels  */
              goto L40170

L40150:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */

L40170:     accept                                                       ~
               at (01,02), "Print Shipping Labels Module",               ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,05), "Enter Sales Order Range",                    ~
               at (06,35), fac(lfac$( 1)), soout$(1)            , ch(16),~
               at (06,55), fac(lfac$( 1)), soout$(2)            , ch(16),~
               at (07,05), "Enter Number of Labels",                     ~
               at (07,35), fac(lfac$( 2)), numlabels$           , ch(02),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40390
                  call "MANUAL" ("SHPLABEL") : goto L40170

L40390:        if keyhit% <> 15 then L40420
                  call "PRNTSCRN" : goto L40170

L40420:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40590     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40570
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40570:     return

L40590: if fieldnr% > 0% then L40680  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Print Labels"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40680:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50120,         /* Sales Order       */     ~
                              L50170          /* Number of Labels  */
            return

L50120: REM Test for Sales order numbers          SOOUT$
            call "TESTRNGE" (soout$(1), soout$(2), soout$(3), soout$(4), ~
                errormsg$)
            return

L50170: REM Test for Number of Labels
            call "NUMTEST" (numlabels$, 1, 9e7, errormsg$, 0, cartons)
            return

        REM *************************************************************~
            *  P R I N T   L I N E   F O R M A T S                      *~
            *************************************************************

L60040: %     ##############################

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
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            call "SHOSTAT" ("One Moment Please")
            end
