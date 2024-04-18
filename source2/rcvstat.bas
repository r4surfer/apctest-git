        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  RRRR    CCC   V   V   SSS   TTTTT   AAA   TTTTT          *~
            *  R   R  C   C  V   V  S        T    A   A    T            *~
            *  RRRR   C      V   V   SSS     T    AAAAA    T            *~
            *  R   R  C   C   V V       S    T    A   A    T            *~
            *  R   R   CCC     V     SSS     T    A   A    T            *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * RCVSTAT  - Driver program for RCVSTATS.  Here you just    *~
            *            enter the receiver and pass it on.             *~
            *            You can find the receiver by part or vendor.   *~
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
            * 08/13/93 ! Original                                 ! JDH *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            date$8,                      /* Date for screen display    */~
            descr_m(12),                 /* Descr Map For PlowCode     */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            hdr$(3)79,                   /* For PLOWCODE Call          */~
            inc(2),                      /* Plowcode Arguments of some */~
            inc$(2)16,                   /*   sort known only to LDJ   */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            rcvr$16,                     /* Receiver Number            */~
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
            * #01 ! RCVMASTR ! Receiver Master File                     *~
            * #02 ! RCVLINES ! Receiver Line Items File  (Purchasing)   *~
            * #20 ! TXTFILE  ! System Text File                         *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "RCVMASTR",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos= 1, keylen = 16                           ~

            select #02, "RCVLINES",                                      ~
                        varc,     indexed,  recsize =  800,              ~
                        keypos =   26, keylen =  52,                     ~
                        alt key  3, keypos =  128, keylen =  24,         ~
                            key  2, keypos =   42, keylen =  36,         ~
                            key  1, keypos =    1, keylen =  69

            select #20, "TXTFILE",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  11                      ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01%), f2%(01%), 0%, rslt$(01%))
            call "OPENCHCK" (#02, fs%(02%), f2%(02%), 0%, rslt$(02%))
            call "OPENCHCK" (#20, fs%(20%), f2%(20%), 0%, rslt$(20%))

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

            str(line2$,62) = " RCVSTAT: " & str(cms2v$,,8)

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
                      if keyhit% =  8% then gosub find_by_part
                      if keyhit% =  9% then gosub find_by_vendor
                      if keyhit% <> 0% then       L10130
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
            next fieldnr%
            goto L11000

        find_by_part
            mat inc = zer : mat descr_m = zer
            init (" ") inc$(), hdr$()
            init (hex(00)) plowkey$
            hdr$(3%) = hex(84) & "Select a Receiver Number"
            hdr$(1%) = "  Part Number               Receiver Number  Vend~
        ~or    Purchase Order  Line"
             descr_m(01) =  1.25            : descr_m(02) = 0001.0
             descr_m(03) = 26.16            : descr_m(04) = 0027.0
             descr_m(05) = 42.09            : descr_m(06) = 0044.0
             descr_m(07) = 51.16            : descr_m(08) = 0054.0
             descr_m(09) = 67.03            : descr_m(10) = 0071.0
             call "PLOWCODE" (#02, plowkey$, " ", 9000%, 1.30, f1%(2%),  ~
                              hdr$(), 0, 78, inc(), inc$(), "D", "Y",    ~
                              #02, descr_m())

            if f1%(2%) = 0% then return
                rcvr$ = str(plowkey$, 26%, 16%)
                return

        find_by_vendor
            mat inc = zer : mat descr_m = zer
            init (" ") inc$(), hdr$()
            init (hex(00)) plowkey$
            hdr$(3%) = hex(84) & "Select a Receiver Number"
            hdr$(1%) = "  Vendor    Purchase Order  Line Receiver Number ~
        ~ Part"
             descr_m(01) = 42.09            : descr_m(02) = 0001.0
             descr_m(03) = 51.16            : descr_m(04) = 0011.0
             descr_m(05) = 67.03            : descr_m(06) = 0028.0
             descr_m(07) = 26.16            : descr_m(08) = 0032.0
             descr_m(09) =  1.25            : descr_m(10) = 0049.0
             call "PLOWCODE" (#02, plowkey$, " ", 9000%, 2.30, f1%(2%),  ~
                              hdr$(), 0, 78, inc(), inc$(), "D", "Y",    ~
                              #02, descr_m())

            if f1%(2%) = 0% then return
                get #02 using L10680, rcvr$
L10680:             FMT POS(26), CH(16)
                return

L11000: REM *************************************************************~
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
L11120:     fieldnr% = 1%
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
            call "RCVSTATS" (rcvr$, " ", " ", " ", #01, #02, #20)
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100          /* Receiver Number        */
            return
L20100: REM Def/Enable Receiver Number             RCVR$
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
         "Enter Receiver Number                                        "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      rcvr$
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
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40075          /* Receiver Number   */
              goto L40090

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40075:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40090:     accept                                                       ~
               at (01,02),                                               ~
                  "Receiver Inquiry",                                    ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Receiver Number",                            ~
               at (06,30), fac(lfac$(1%)), rcvr$                , ch(16),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L40200
                  call "MANUAL" ("RCVSTAT ") : goto L40090

L40200:        if keyhit% <> 15% then L40215
                  call "PRNTSCRN" : goto L40090

L40215:        return

        set_pf1
        if edit% = 2% then L40310     /*  Input Mode             */
            pf$(1%)= "(1)Start Over                       (8)F" &        ~
                     "ind by Part            (13)Instructions"
            pf$(2%)= "                 (4)Previous Field  (9)F" &        ~
                     "ind by Vendor          (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffff0809ffffff0dff0f1000)
            if fieldnr% = 1% then L40290
                str(pf$(3%),64%)  = " "    : str(pfkeys$,16%,1%) = hex(ff)
L40290:     if fieldnr% > 2% then L40300
                str(pf$(2%),18%,17%) = " " : str(pfkeys$, 4%,1%) = hex(ff)
L40300:     return

L40310: if fieldnr% > 0% then L40355  /*  Edit Mode - Select Fld */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Rcvr Status "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40355:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50100          /* Receiver Number        */
            return
L50100: REM Test for Receiver Number              RCVR$
            init(" ") hdr$()
            init(hex(00)) plowkey$
            str(plowkey$,,16%) = rcvr$
            if rcvr$ = "?" then str(plowkey$,,16%) = " "
            hdr$(3%) = hex(0684) & "Select a Receiver Number"
            call "GETCODE" (#01, plowkey$, hdr$(3%), 0%, 0.001, f1%(1%))
            if f1%(1%) = 0% then L50185
                rcvr$ = str(plowkey$,,16%)
                return
L50185:     errormsg$ = "Receiver not on file."
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
            *  Copyright (c) 1993  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
