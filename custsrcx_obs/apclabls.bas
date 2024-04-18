        REM *************************************************************~
            *                                                           *~
            *   AAA   PPPP    CCC   L       AAA   BBBB   L       SSSS   *~
            *  A   A  P   P  C   C  L      A   A  B   B  L      S       *~
            *  AAAAA  PPPP   C      L      AAAAA  BBBB   L       SSS    *~
            *  A   A  P      C   C  L      A   A  B   B  L          S   *~
            *  A   A  P       CCC   LLLLL  A   A  BBBB   LLLLL  SSSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * APCLABLS - Print Generic Labels ( 2 Line Print )          *~
            *                                                           *~
            *            (45) CHARTERS BY (2) LINES                     *~
            *                                                           *~
            *     <------------------ (45) -------------------->        *~
            *     <------------------ (45) -------------------->        *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/06/92 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 11/02/92 ! Mod Add PF(  ) for Printing Picture      ! RHH *~
            *          ! Window Labels. Subroutine (APCLABTT)     ! RHH *~
            *          !                                          !     *~
            * 11/11/97 ! Revision Update For 60403                ! DJD *~
            *************************************************************

        dim                                                              ~
            quantity$4,                  /* Quantity of Labels to Print*/~
            descr$(2)45,                 /* Label Text                 */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim f2%(5),                      /* = 0 if the file is open    */~
            f1%(5)                       /* = 1 if READ was successful */
                                         /*   doesn't exist, or 0 if   */
                                         /*   not yet checked (OPENCHCK*/
        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "06.04.03 11/11/97 Pre-Release Version            "
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
            * #01 ! HNYMASTR ! Part Master File                         *~
            *     !          !                                          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            call "SHOSTAT" ("Opening Files, One Moment Please")

            f1%(1), f1%(2), f1%(3), f1%(4), f1%(5) = 0%

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

            select% = 0%                           /* PRINTER NOT OPEN */

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to   3%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10215
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10215:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% = 10% then gosub picture_windows
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
                  if keyhit%  =  9% then gosub print_test
                  if keyhit%  = 10% then gosub picture_windows
                  if keyhit%  = 14% then gosub print_labels
                  if keyhit%  = 16% then goto exit_program
                  if keyhit% <>  0% then       editpg1
L11150:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 3% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11200:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11200
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11200
                  lastfieldnr% = fieldnr%
            goto L11150

        REM *************************************************************~
            *             P R I N T   L A B E L S                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        print_labels
           call "SHOSTAT" ("Printing Generic Labels")
           gosub select_printer
           qty_max% = quantity%
           for rhh% = 1% to qty_max%
               print using L55060, descr$(1%)
               print using L55060, descr$(2%)
               print
           next rhh%

        return clear all
        goto inputmode

        print_test
           gosub select_printer
           call "SHOSTAT" ("Test Pattern")
           for rhh% = 1% to 8%
               print using L55040
               print using L55040
               print
           next rhh%

        return clear all
        goto editpg1

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20140,         /* Quantity of Labels    */ ~
                              L20180,         /* First Text Line       */ ~
                              L20220          /* Second Text Line      */

         return

L20140: REM Quantity                               QUANTITY$
        REM QUANTITY$ = " "
         return

L20180: REM First Text Line                        DESCR$(1%)
        REM DESCR$(1%) = " "
         return

L20220: REM Second Text Line                       DESCR$(2%)
        REM DESCR$(2%) = " "
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
         "Enter Number of Labels to Print ( 0 to 9999 ).               ",~
         "Enter The Valid Text for Line One. ( 45 Characters )         ",~
         "Enter the Valid Text for Line Two. ( 45 Characters )         "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, quantity$, descr$()


        return

        REM *************************************************************~
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
        REM DATALOAD

        REM RETURN

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        REM DATAPUT

        REM RETURN CLEAR ALL
        REM GOTO INPUTMODE


        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

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
              on fieldnr% gosub L40190,         /* Qty of Labels     */   ~
                                L40170,         /* Line One (1) Text */   ~
                                L40170          /* Line Two (2) Text */

              goto L40210

L40170:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
                  lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40190:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40210:     accept                                                       ~
               at (01,02),                                               ~
                  "Generic Label Printing ",                             ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Number of Labels :",                         ~
               at (06,25), fac(lfac$( 1)), quantity$            , ch(04),~
                                                                         ~
               at (07,02), "Text for Line (1):",                         ~
               at (07,25), fac(lfac$( 2)), descr$(1%)           , ch(45),~
               at (08,02), "Text for Line (2):",                         ~
               at (08,25), fac(lfac$( 3)), descr$(2%)           , ch(45),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L40470
                  call "PRNTSCRN"
                  goto L40210

L40470:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40660     /*  Input Mode             */
            pf$(1) = "(1)Start Over    (4)Previous Field      " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (10)Picture Wd Labels  " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffff0affffffff0f1000)
            if fieldnr% = 1% then L40620
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40620:     if fieldnr% > 1% then L40640
                str(pf$(1),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40640:     return

L40660: if fieldnr% > 0% then L40750  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Print Labels"
            pf$(2) = "                 (9)Print Test          " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (10)Picture WD Labels  " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffff090affffff0e0f1000)
            return
L40750:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50140,         /* Number of Labels      */ ~
                              L50270,         /* Text for Line (1)     */ ~
                              L50310          /* Text for Line (2)     */

            return

L50140: REM Number of Labels                      QUANTITY$
            quantity% = 0%
            if quantity$ <> " " then goto L50190
               goto L50230

L50190:     convert quantity$ to quantity%, data goto L50230

            convert quantity% to quantity$, pic(0000)
        return
L50230:     errormsg$ = "Invalid Label Quantity."
            quantity$ = " "
         return

L50270: REM Line (1) Text                         DESCR$(1%)

        return

L50310: REM Line (2) Text                         DESCR$(2%)
           if len(descr$(1%)) < 2 and len(descr$(2%)) < 2 then           ~
                                      goto L50350
        return
L50350:    errormsg$ = "(Error) At least One (1) Line Must have Text."
           init(" ") descr$()
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************
                                                       /* TEST PATTERN */
L55040: %XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
                                                       /* LINE TEXT    */
L55060: %#############################################

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        select_printer
            if select% <> 0% then return
            call "SETPRNT" ("APCLAB", " ", 0%, 0%)
            select printer (134)
            select% = 1%
        return

        close_printer
            if select% = 0% then return
            call "SETPRNT" ("APCLAB", " ", 0%, 1%)
        return

        picture_windows
            call "APCLABTT" (quantity$)
        return clear all
        goto inputmode

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            gosub close_printer
            call "SHOSTAT" ("One Moment Please")

            end
