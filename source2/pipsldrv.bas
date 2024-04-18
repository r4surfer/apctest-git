        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  PPPP   IIIII  PPPP    SSS   L      DDDD   RRRR   V   V   *~
            *  P   P    I    P   P  S      L      D   D  R   R  V   V   *~
            *  PPPP     I    PPPP    SSS   L      D   D  RRRR   V   V   *~
            *  P        I    P          S  L      D   D  R   R   V V    *~
            *  P      IIIII  P       SSS   LLLLL  DDDD   R   R    V     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PIPSLDRV - Simple driver                                  *~
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
            * 05/18/90 ! Original                                 ! WPH *~
            * 08/05/91 !(QC-FIXES) Added call to "ALLFREE", Fixed ! RJB *~
            *          !     PLOWCODE changed 0% to 0 (KEY-DESCR) !     *~
            * 08/09/91 !Added READ100 to test for valid job       ! WPH *~
            * 10/19/93 ! Slide Purchase Jobs, Too                 ! KAB *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            descr_m(6),                  /* Descr Map For PlowCode     */~
            descr$30,                    /* Descr Map For PlowCode     */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            header$(3)79,                /* Dear PLOWCODE, ...         */~
            ie(3),                       /* Plowcode Include/Exclude   */~
            ie$(3)2,                     /* Plowcode Include/Exclude   */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            tag$19,                      /* Tag Number                 */~
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
            * #60 ! SYSFILE2 ! Caelus Management System Information     *~
            * #41 ! SFCUM2   ! Cumulative sales forecast file           *~
            * #09 ! JBMASTR2 ! Production job master file               *~
            * #11 ! WCMASTR  ! Workcenter Master File                   *~
            * #08 ! JBCROSS2 ! Cross reference of RTE & BOM planned for *~
            * #02 ! PIPMASTR ! Planned Inventory Position Master File   *~
            * #23 ! WCOUT    ! Planned work center use detail file      *~
            * #33 ! PIPIN    ! Planned inventory additions detail       *~
            * #34 ! PIPOUT   ! Planned inventory use detail rec         *~
            * #35 ! PIPCROSS ! hard peg cross reference                 *~
            * #36 ! JBPIPXRF ! option part harder peg                   *~
            * #04 ! HNYMASTR ! Part Master File                         *~
            * #07 ! RTEMASTR ! Route Master File                        *~
            * #15 ! BOMMASTR ! Bill of Materials File                   *~
            * #24 ! ENGMASTR ! BOM/RTE Effectivity file                 *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #60, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select #41, "SFCUM2",                                        ~
                        varc,     indexed,  recsize = 1985,              ~
                        keypos =    1, keylen =  25                      ~

            select #09, "JBMASTR2",                                      ~
                        varc,     indexed,  recsize = 1300,              ~
                        keypos =    1, keylen =   8                      ~

            select #11, "WCMASTR",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    2, keylen =   5,                     ~
                        alt key  1, keypos =    1, keylen =   6          ~

            select #08, "JBCROSS2",                                      ~
                        varc,     indexed,  recsize =   94,              ~
                        keypos =   29, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  47,         ~
                            key  2, keypos =   48, keylen =  47          ~

            select #02, "PIPMASTR",                                      ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    2, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  26          ~

            select #23, "WCOUT",                                         ~
                        varc,     indexed,  recsize =   68,              ~
                        keypos =    9, keylen =  23,                     ~
                        alt key  1, keypos =    1, keylen =  27          ~

            select #33, "PIPIN",                                         ~
                        varc,     indexed,  recsize =   60,              ~
                        keypos =   30, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  48          ~

            select #34, "PIPOUT",                                        ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =   20, keylen =  37          ~

            select #35, "PIPCROSS",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  71,                     ~
                        alt key  1, keypos =   20, keylen =  52,         ~
                            key  2, keypos =   39, keylen =  33          ~

            select #36, "JBPIPXRF",                                      ~
                        varc,     indexed,  recsize =   63,              ~
                        keypos =    1, keylen =  63,                     ~
                        alt key  1, keypos =   45, keylen =  19          ~

            select #4,  "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  900,                                  ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            select #7,  "RTEMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  400,                                  ~
                        keypos =    5, keylen =  31,                     ~
                        alt key  1, keypos =    1, keylen =  35          ~

            select #15, "BOMMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos =   26, keylen =  31,                     ~
                        alt key  1, keypos =    1, keylen =  56          ~

            select #24, "ENGMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2015,                                  ~
                        keypos =    1, keylen =  29                      ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#60, fs%(60), f2%(60), 0%, rslt$(60))
            call "OPENCHCK" (#41, fs%(41), f2%(41), 0%, rslt$(41))
            call "OPENCHCK" (#09, fs%(09), f2%(09), 0%, rslt$(09))
            call "OPENCHCK" (#11, fs%(11), f2%(11), 0%, rslt$(11))
            call "OPENCHCK" (#08, fs%(08), f2%(08), 0%, rslt$(08))
            call "OPENCHCK" (#02, fs%(02), f2%(02), 0%, rslt$(02))
            call "OPENCHCK" (#23, fs%(23), f2%(23), 0%, rslt$(23))
            call "OPENCHCK" (#33, fs%(33), f2%(33), 0%, rslt$(33))
            call "OPENCHCK" (#34, fs%(34), f2%(34), 0%, rslt$(34))
            call "OPENCHCK" (#35, fs%(35), f2%(35), 0%, rslt$(35))
            call "OPENCHCK" (#36, fs%(36), f2%(36), 0%, rslt$(36))
            call "OPENCHCK" (#04, fs%(04), f2%(04), 0%, rslt$(04))
            call "OPENCHCK" (#07, fs%(07), f2%(07), 0%, rslt$(07))
            call "OPENCHCK" (#15, fs%(15), f2%(15), 0%, rslt$(15))
            call "OPENCHCK" (#24, fs%(24), f2%(24), 0%, rslt$(24))

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

            str(line2$,62) = "PIPSLDRV: " & str(cms2v$,,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

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
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100          /* Tag Number             */
            return
L20100: REM Def/Enable Tag Number                  TAG$
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
         "Enter a Tag Number or leave the field Blank and Press RETURN to~
        ~ Select."

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            call "ALLFREE"
            init(" ") errormsg$, inpmessage$,                            ~
                      tag$
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
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************



        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
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
              on fieldnr% gosub L40075          /* Tag Number        */
              goto L40090

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40075:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40090:     accept                                                       ~
               at (01,02),                                               ~
                  "Slide WO Advices or Released Jobs",                   ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Tag Number to Slide:",                       ~
               at (06,30), fac(lfac$( 1)), tag$                 , ch(19),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40200
                  call "MANUAL" ("PIPSLDRV") : goto L40090

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
            on fieldnr% gosub L50100          /* Tag Number             */
            return

L50100: REM Test for Tag Number                   TAG$
            ie(1) = 30.02 : ie$(1) = "JO"
            ie(2) = 30.02 : ie$(2) = "WO"
            ie(3) = 30.02 : ie$(3) = "BW"
            descr_m(01) =    30.19 : descr_m(02) = 0001.0
            descr_m(03) =     1.25 : descr_m(04) = 0022.0
            descr_m(05) =    49.08 : descr_m(06) = 0050.104
            header$(3) = hex(84) & "Select a Tag Number"
            header$(1) = "Job or Tag Number      Part"
            str(header$(1),54) = "Quantity"
            init (hex(00)) plowkey$
            if tag$ = "?" then tag$ = " "
            if tag$ = " " then L50130
            gosub L50160   /* check to see if they entered a valid job */
            str(plowkey$, 1, 19)  = tag$
L50130:     call "PLOWCODE" (#33, plowkey$, descr$, 9000%, 0.80, f1%(33),~
                                  header$(), 0, 0, ie(), ie$(),          ~
                                  "D", "Y", #33, descr_m())
               if f1%(33) <> 0% then L50200
            errormsg$ = "Select or enter a Tag Number"
            return

L50160:     str(plowkey$, 1, 19)  = "JOB ORDER: " &  str(tag$,,)
            call "READ100" (#33, plowkey$, f1%(33))
               if f1%(33) =  0% then return

L50200:     tag$ = str(plowkey$,1,19)
            call "PIPSLIDE" (tag$, #60, #41, #9, #11, #8, #2, #23, #33,  ~
                                   #34, #35, #36, #4, #7, #15, #24)

            return clear all
            goto inputmode

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
