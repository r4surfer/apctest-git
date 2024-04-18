        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  BBBB    CCC   K   K  PPPP   M   M   CCC    AAA   L       *~
            *  B   B  C   C  K  K   P   P  MM MM  C   C  A   A  L       *~
            *  BBBB   C      KKK    PPPP   M M M  C      AAAAA  L       *~
            *  B   B  C   C  K  K   P      M   M  C   C  A   A  L       *~
            *  BBBB    CCC   K   K  P      M   M   CCC   A   A  LLLLL   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BCKPMCAL - This program calculates the precious metal     *~
            *            surcharge for a CMS Part                       *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/13/94 ! Original                                 ! RJH *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            calc_date$8,                 /* Date to base calcs on      */~
            cursor%(2),                  /* Cursor location for edit   */~
            cuscode$9,                   /* Customer Code              */~
            cusdescr$30,                 /* Customer Descr             */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            mode$1,                      /* Mode (C, D, P)             */~
            part$25,                     /* Part Number                */~
            partdescr$32,                /* Part Number Description    */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Plowkey                    */~
            pm_code$(100)10,             /* Precious Metal             */~
            pm_descr$(100)30,            /* PM Description             */~
            pm_ext$(100)10,              /* Line Amount                */~
            pm_factor$(100)10,           /* Factor                     */~
            pm_price$(100)10,            /* Price                      */~
            pm_qty$(100)10,              /* PM Quantity                */~
            pm_uom$(100)4,               /* Unit of Measure            */~
            qty$10,                      /* Quantity                   */~
            s_charge$10,                 /* Surcharge                  */~
            s_total$35,                  /* Surcharge Display Totals   */~
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
            cms2v$ = "R6.04.02 11/13/95 Precious Metals                 "
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
            * #01 ! HNYPMTBL ! Precious metal code table                *~
            * #02 ! HNYPMPRC ! Precious Metal price table               *~
            * #03 ! HNYMASTR ! Part Master File                         *~
            * #04 ! PMCODES  ! Precious Metal Codes File                *~
            * #05 ! CUSTOMER ! Customer Master File                     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "HNYPMTBL",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    1, keylen =  35,                     ~
                        alt key  1, keypos =   26, keylen =  10, dup

            select #02, "HNYPMPRC",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    1, keylen =  25

            select  #3, "HNYMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 900,                                  ~
                         keypos = 1, keylen = 25,                        ~
                         alternate key 1, keypos = 102, keylen = 9, dup, ~
                                   key 2, keypos = 90, keylen = 4, dup,  ~
                                   key 3, keypos = 26, keylen = 32, dup

            select #04, "PMCODES",                                       ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos = 1,    keylen =  10
            select #05,  "CUSTOMER",                                     ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup


            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#01, fs%(01%), f2%(01%), 0%, rslt$(01%))
            call "OPENCHCK" (#02, fs%(02%), f2%(02%), 0%, rslt$(02%))
            call "OPENCHCK" (#03, fs%(03%), f2%(03%), 0%, rslt$(03%))
            call "OPENCHCK" (#04, fs%(04%), f2%(04%), 0%, rslt$(04%))
            call "OPENCHCK" (#05, fs%(05%), f2%(05%), 0%, rslt$(05%))

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

            str(line2$,62) = "BCKPMCAL: " & str(cms2v$,,8)


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  5%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10205
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10205:               if keyhit%  = 8% then gosub select_pm_part
                      if keyhit% = 16% and fieldnr% = 1% then exit_program
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
                  if keyhit%  = 16% then       dataload
                  if keyhit% <>  0% then       editpg1
L11130:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% >  5% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11180:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11180
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11180
                  lastfieldnr% = fieldnr%
            goto L11130

        select_pm_part
            if part$ = "?" then part$ = " "
            plowkey$ = str(part$) & hex(00)
            call "PLOWCODE" (#01, plowkey$, " ", -25%, -0.01, f1%(1%))
            if f1%(1%) = 1% then part$ = str(plowkey$,, 25%)
            return

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        dataload
               qty = 0
               convert qty$ to qty, data goto L19075
               price = 0
L19075:        call "PMCALSUB"( cuscode$, part$, qty, price, 1, mode$,   ~
                                calc_date$, s_charge, "S", " ", " ", " ",~
                                                                 result%)
               call "CONVERT" (s_charge, 2.2, s_charge$)
               if result% =  1% then L19190   /* All OK */
               if result% = -1% then L19150   /* No PM Price */
                  /* Fall thru - Must be no PM's */
               errormsg$ =                                               ~
                      "No Precious Metal Items associated with this Part."
               goto editpg1

L19150:        errormsg$ =                                               ~
                 "WARNING: At least one PM Item has No Associated Value"
               goto editpg1

L19190:        if mode$ = "C" then goto editpg1
               goto inputmode


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20500,         /* Customer               */~
                              L20100,         /* Part Number            */~
                              L20200,         /* Quantity               */~
                              L20300,         /* Mode (C, D, P)         */~
                              L20400          /* Applied Date           */
            return
L20100: REM Def/Enable Part Number                 PART$
            return

L20200: REM Def/Enable Quantity                    QTY$
            return

L20300: REM Def/Enable Mode (C, D, P)              MODE$
            return

L20400: REM Def/Enable Applied date                CALC_DATE$
            if calc_date$ = " " then calc_date$ = date$
            return

L20500: REM Def/Enable Customer                    CUSCODE$
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
         "Enter Customer Name                                          ",~
         "Enter Part Number                                            ",~
         "Enter Quantity                                               ",~
         "Enter Mode ('C'alculate, 'D'isplay, or 'P'rint)              ",~
         "Enter Applied Date                                           "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, calc_date$,                ~
                     mode$, part$, qty$, s_charge$, cuscode$,            ~
                     pm_code$(), pm_descr$(), pm_ext$(), pm_qty$(),      ~
                     pm_factor$(), pm_price$(), pm_uom$()

            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
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
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

        FMT                 /* FILE: HNYPMTBL                          */~
            CH(25),         /* Part Number (Product Code)              */~
            CH(10),         /* Precious Metal Item Code                */~
            PD(14,7),       /* Precious metal quantity                 */~
            PD(14,7),       /* Multiplier to set PM Price per xxx Parts*/~
            CH(4),          /* Unit of Measure                         */~
            CH(3),          /* Definition of Type USER (user ids)      */~
            CH(6),          /* Date record last changed                */~
            CH(36)          /* Unused Space                            */~

        FMT                 /* FILE: HNYPMPRC                          */~
            CH(9),          /* Customer Code                           */~
            CH(10),         /* Precious Metal Item Code                */~
            CH(6),          /* Reversed date (100000 - yymmdd)         */~
            CH(6),          /* effective date                          */~
            PD(14,7),       /* Precious Metal Price at SO Time         */~
            CH(4),          /* Unit of Measure                         */~
            CH(3),          /* Definition of Type USER (user ids)      */~
            CH(6),          /* Date record last changed                */~
            CH(48)          /* Unused Space                            */~

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
              on fieldnr% gosub L40090,         /* Customer          */   ~
                                L40090,         /* Part Number       */   ~
                                L40095,         /* Quantity          */   ~
                                L40090,         /* Mode (C, D, P)    */   ~
                                L40090          /* Applied Date      */
              goto L40100

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40090:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40095:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40100:     if s_charge$ = " " then s_total$ = " " else                  ~
                 s_total$ = "Surcharge total: " & s_charge$

L40105:     accept                                                       ~
               at (01,02),                                               ~
                  "Precious Metal Surcharge Calculation",                ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Customer Code",                              ~
               at (06,30), fac(lfac$( 1)), cuscode$             , ch(09),~
                                                                         ~
               at (07,02), "Part Number",                                ~
               at (07,30), fac(lfac$( 2)), part$                , ch(25),~
                                                                         ~
               at (08,02), "Quantity",                                   ~
               at (08,30), fac(lfac$( 3)), qty$                 , ch(10),~
               at (08,45), fac(hex(84)), s_total$               , ch(34),~
                                                                         ~
               at (09,02), "Mode (C, D, P)",                             ~
               at (09,30), fac(lfac$( 4)), mode$                , ch(01),~
                                                                         ~
               at (10,02), "Applied Date",                               ~
               at (10,30), fac(lfac$( 5)), calc_date$           , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40260
                  call "MANUAL" ("BCKPMCAL") : goto L40105

L40260:        if keyhit% <> 15 then L40275
                  call "PRNTSCRN" : goto L40105

L40275:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40370     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40350
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40350:     if fieldnr% > 2% then L40360
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
            if fieldnr% <> 2% then L40360
                str(pf$(3),18,26) = "(8)Select PM Part"
                str(pfkeys$, 8,1) = hex(08)
L40360:     return

L40370: if fieldnr% > 0% then L40435  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)PM Details  "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            if mode$ = "D" then return
                str(pf$(3%),64%,14%) = "(16)Process   "
            if mode$ = "C" then return
                str(pf$(3%),64%,14%) = "(16)Report    "
            return
L40435:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50500,         /* Customer               */~
                              L50100,         /* Part Number            */~
                              L50200,         /* Quantity               */~
                              L50300,         /* Mode (C, D, P)         */~
                              L50400          /* Applied Date           */
            return

L50100: REM Test for Part Number                  PART$
            if part$ = "?" then  part$ = " "

            partdescr$ = hex(06) & "Select Part for Precious Metals"
            call "GETCODE" (#3, part$, partdescr$, 0%, 0.32, f1%(3%))
            if f1%(3%) = 1% then return
                errormsg$ = "Sorry, Part Not on File.  Please Re-enter."
                return
            return

L50200: REM Test for Quantity                     QTY$
            errormsg$ = " "
            call "NUMTEST" (qty$, 0, 9999999, errormsg$, -2.2, qty)
                       qty = qty
            return

L50300: REM Test for Mode (C, D, P)               MODE$
            if mode$ = "C" or mode$ = "D" or mode$ = "P" then return
                errormsg$ = "Enter 'C', 'D', or 'P'"
            return

L50400: REM Test for Applied Date                 CALC_DATE$
            errormsg$ = " "
            call "DATEOK" (calc_date$, date%, errormsg$)
                date% = date%
            return

L50500: REM Test for Customer Code                CUSCODE$
                if cuscode$ = "?" then cuscode$ = " "
                cusdescr$ = hex(06) & "Select Customer Code"
                call "GETCODE" (#05, cuscode$, cusdescr$, 0%, 1, f1%(5))
                if f1%(5) = 1% then L50510
                     errormsg$ = "Customer not on file." : return
L50510:     return



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
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
