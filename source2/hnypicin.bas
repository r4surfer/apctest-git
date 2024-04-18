        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  H   H  N   N  Y   Y  PPPP   IIIII   CCC   IIIII  N   N   *~
            *  H   H  NN  N  Y   Y  P   P    I    C   C    I    NN  N   *~
            *  HHHHH  N N N   YYY   PPPP     I    C        I    N N N   *~
            *  H   H  N  NN    Y    P        I    C   C    I    N  NN   *~
            *  H   H  N   N    Y    P      IIIII   CCC   IIIII  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYPICIN - Modify the book quantity in the cost picture   *~
            *            for specific session/part/store/lot records.   *~
            *            Variance reason codes are required for all     *~
            *            modifications to cost picture quantities.      *~
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
            * 05/05/93 ! Original                                 ! MLJ *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            active_session$1,            /* Cycle Count Active Session */~
            coname$60,                   /* Company Name               */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            descr$32,                    /* PLOWCODE Description       */~
            descr_map(8),                /* Description Map For PLOW   */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            hdr$(3)79,                   /* PLOWCODE Headers           */~
            incl$(1)1,                   /* PLOWCODE Include/Exclude   */~
            incl(1),                     /* PLOWCODE Include/Exclude   */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            lot$16,                      /* Lot Number                 */~
            new_book$12,                 /* New Book Quantity          */~
            old_book$12,                 /* Current Book Quantity      */~
            part$25,                     /* Part Number                */~
            pdescr$32,                   /* Part Number Description    */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            reason$6,                    /* Variance Reason Code       */~
            rdescr$30,                   /* Variance Reason Description*/~
            runtime$8,                   /* Program Run Time           */~
            sdescr$30,                   /* Store Code Description     */~
            sesdescr$30,                 /* Session Description        */~
            session$2,                   /* Count Session Number       */~
            session_name$12,             /* Cycle Count Session Name   */~
            store$3,                     /* Store/Warehouse Code       */~
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
            cms2v$ = "R6.02.04 06/29/93 SFC & Cycle Count Enhancements  "
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
            * #02 ! GENCODES ! System General Codes file.               *~
            * #03 ! HNYPICST ! Physical Inventory Captured Costs/Qtys F *~
            * #04 ! HNYCCSYS ! Cycle Count System Control File          *~
            * #05 ! HNYCCDTL ! Cycle Count Session Detail File          *~
            * #06 ! HNYMASTR ! Inventory Master File                    *~
            * #07 ! STORNAME ! Store Names And Addresses                *~
            * #08 ! HNYPISYS ! Physical Inventory System Session Contro *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select #02, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24                      ~

            select #03, "HNYPICST",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  46

            select #04, "HNYCCSYS",                                      ~
                        varc,     indexed,  recsize =  249,              ~
                        keypos =    1, keylen =  12,                     ~
                    alt key 1, keypos = 43,  keylen = 13, dup,           ~
                        key 2, keypos = 56,  keylen = 10

            select #05, "HNYCCDTL",                                      ~
                        varc,     indexed,  recsize =  448,              ~
                        keypos =    1, keylen =  57,                     ~
                    alt key 1, keypos = 13,  keylen = 45, dup,           ~
                        key 2, keypos = 14,  keylen = 44, dup

            select  #6, "HNYMASTR",                                      ~
                         varc,    indexed,  recsize =  900,              ~
                         keypos = 1, keylen = 25,                        ~
                    alt key 1, keypos = 102, keylen = 9, dup,            ~
                        key 2, keypos = 90, keylen = 4, dup,             ~
                        key 3, keypos = 26, keylen = 32, dup

            select #7,  "STORNAME",                                      ~
                         varc,    indexed,  recsize =  300,              ~
                         keypos = 1, keylen = 3

            select #8,  "HNYPISYS",                                      ~
                         varc,    indexed,  recsize =  512,              ~
                         keypos =    7, keylen =   2,                    ~
                     alt key  1, keypos =  1, keylen = 8

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(1%), f2%(1%), 0%, rslt$(1%))
            call "OPENCHCK" (#02, fs%(2%), f2%(2%), 0%, rslt$(2%))
            call "OPENCHCK" (#03, fs%(3%), f2%(3%), 0%, rslt$(3%))
            call "OPENCHCK" (#04, fs%(4%), f2%(4%), 0%, rslt$(4%))
            call "OPENCHCK" (#05, fs%(5%), f2%(5%), 0%, rslt$(5%))
            call "OPENCHCK" (#06, fs%(6%), f2%(6%), 0%, rslt$(6%))
            call "OPENCHCK" (#07, fs%(7%), f2%(7%), 0%, rslt$(7%))
            call "OPENCHCK" (#08, fs%(8%), f2%(8%), 0%, rslt$(8%))

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

        REM Is user a Data Base or Module Administrator...
            call "CMSMACHK" ("HNY", lfac$(1%), lfac$(2%))
                if lfac$(1%) = "Y" or lfac$(2%) = "Y" then L09190
            errormsg$ ="You MUST be a Data Base or HNY Module Administr"&~
                       "ator to run this program"
            call "ASKUSER" (0%, "SECURITY CHECK", " ", errormsg$, " ")
            goto exit_program

L09190:     str(line2$,62) = "HNYPICIN: " & str(cms2v$,,8)
            ret%, page% = 0%  :  line% = 60%
            call "COMPNAME" (12%, coname$, ret%)
            call "SETPRNT" ("HNY058", " ", 0%, 0%)
            select printer
            runtime$ = " "
            call "TIME" (runtime$)

        REM Plowcode Screen Mapping...
            hdr$(1%)              = "  PART"
            str(hdr$(1%),30%,5%)  = "STORE"
            str(hdr$(1%),37%,3%)  = "LOT"
                descr_map(1%) =  3.250  :  descr_map(2%) =  1.0
                descr_map(3%) = 28.030  :  descr_map(4%) = 28.0
                descr_map(5%) = 31.160  :  descr_map(6%) = 35.0

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  2%
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10220
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10200
L10150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10120
                         if fieldnr% = 1% then L10100
                         goto L10150
L10200:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10120
L10220:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10120
            next fieldnr%

        input2
            fieldnr% = 6%
            for fieldnr% = 6% to  7%
L10290:         gosub'052(fieldnr%)
                      if enabled% = 0% then L10410
L10310:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10390
L10340:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10310
                         if fieldnr% = 1% then L10290
                         goto L10340
L10390:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10310
L10410:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10310
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 5%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 7%
            if fieldnr% < 6% or fieldnr% >  7% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'052(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11170
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
            goto L11120

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            plowkey$ = str(session$) & str(part$) & str(store$) &        ~
                       str(lot$)
            convert new_book$ to new_book, data goto L19100
L19100:     call "READ101" (#3, plowkey$, f1%(3%))
            put #3 using L19120, new_book, reason$
L19120:         FMT POS(47), PD(14,4), POS(159), CH(6)
            rewrite #3
                chg% = 1%
                call "STRING" addr("RJ", old_book$, 12%)
                call "STRING" addr("RJ", new_book$, 12%)
            gosub cycle_count
            gosub print_report
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields 1 and 2 of.              *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20120,         /* Count Session          */~
                              L20170          /* Part Number            */
            return

L20120: REM Def/Enable Count Session Number        SESSION$
            inpmessage$ = "Enter Inventory Session Number Or Leave Blan"&~
                          "k To Search."
            return

L20170: REM Def/Enable Part Number                 PART$
            inpmessage$ = "Enter Part Number For This Session Or Leave "&~
                          "Blank To Search."
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields 1 and 2 of.              *~
            *************************************************************

        deffn'052(fieldnr%)
            enabled% = 1%
            if fieldnr% = 6% then gosub L20610 else gosub L20650
                return

L20610: REM Def/Enable New Book Quantity           NEW_BOOK$
            inpmessage$ = "Enter New Book Quantity."
            return

L20650: REM Def/Enable Variance Reason Code        REASON$
            inpmessage$ = "Enter Variance Reason Code For Change In Qua"&~
                          "ntity."
            return

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      lot$, new_book$, old_book$, part$, reason$,        ~
                      session$, store$, pdescr$, sdescr$, rdescr$,       ~
                      sesdescr$
            chg% = 0%

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
        dataload
            plowkey$ = str(session$) & str(part$) & str(store$) &        ~
                       str(lot$)
            call "READ100" (#3, plowkey$, f1%(3%))
            get #3 using L30100, part$, store$, lot$, old_book
L30100:         FMT POS(3), CH(25), CH(3), CH(16), PD(14,4)
            call "CONVERT" (old_book, -2.2, old_book$)
                call "DESCRIBE" (#6, str(part$), pdescr$, 0%, f1%(6%))
                call "DESCRIBE" (#7, str(store$), sdescr$, 0%, f1%(7%))
            return


        REM *************************************************************~
            *                        R E P O R T                        *~
            *************************************************************

        print_report
            if line% > 55% then gosub page_heading
            print using L32160, cc$, session$, part$, store$, lot$,       ~
                  old_book$, new_book$, reason$, userid$
            line% = line% + 1%
            return

        page_heading
            print page
            page% = page% + 1%
            print using L32040, date$, runtime$, coname$
            print using L32070, "Cost Picture Modification Log", page%
            print skip (2)
            print using L32100
            print using L32130
            print
            line% = 10%
            return

        REM *************************************************************~
            *              Printer Image Statements                     *~
            *************************************************************

L32040: %RUN DATE: ######## @ ########        ###########################~
        ~#################################              HNYPICIN:HNY058

L32070: %                                                     ###########~
        ~##################                                 PAGE: ###

L32100: %SESSION  PART                       STORE   LOT                O~
        ~LD BOOK QTY   NEW BOOK QTY   VAR REASON   MODIFIED BY

L32130: %-------  -------------------------  -----   ----------------   -~
        ~-----------   ------------   ----------   -----------

L32160: % # ##    #########################   ###    ################   #~
        ~###########   ############   ######       ###

        REM *************************************************************~
            *                C Y C L E    C O U N T                     *~
            *-----------------------------------------------------------*~
            * If this modifiecation is to a cycle count session, then   *~
            * HNYCCDTL adjustments must also be made.                   *~
            *************************************************************

        cycle_count
        REM Is this a Cycle Count Session...
            cc$ = " "
            plowkey$ = str(session$) & hex(00000000000000000000)
            call "PLOWALTS" (#4, plowkey$, 2%, 2%, f1%(4%))
                if f1%(4%) = 0% then return         /* Not a CC Session */

        REM Get info from HNYCCSYS and memory to read HNYCCDTL...
            get #4 using L33160, session_name$, active_session$
L33160:         FMT CH(12), POS(43), CH(1)
            plowkey$ = str(session_name$) &  str(active_session$) &      ~
                       str(part$) & str(store$) & str(lot$)

        REM Read HNYCCDTL, re-calculate the Unit Variance & Cost Variance
            call "READ101" (#5, plowkey$, f1%(5%))
                if f1%(5%) = 0% then return        /* Shouldn't Happen */
            get #5 using L33240, qty, unitcost
L33240:         FMT POS(68), 2*PD(14,4)
            unitvar = qty - new_book
            costvar = unitvar * unitcost

        REM Output new calculations to HNYCCDTL
            put #5 using L33300, unitvar, costvar, reason$
L33300:        FMT POS(84), 2*PD(14,4), POS(131), CH(6)
            rewrite #5
            cc$ = "*"
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              if fieldnr% = 0% then L40080 else L40100
L40080:           inpmessage$ = "To Modify, Position Cursor And Press R"&~
                                "ETURN."
L40100:       gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              lfac$(fieldnr%) = hex(81)
              if edit% = 2% then init(hex(8c)) lfac$(1%), lfac$(2%)

L40160:     accept                                                       ~
               at (01,02),                                               ~
                  "Cost Picture Quantity Modification",                  ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Count Session Number",                       ~
               at (06,25), fac(lfac$( 1)), session$             , ch(02),~
               at (06,50), fac(hex(8c)),   sesdescr$            , ch(30),~
                                                                         ~
               at (07,02), "Part Number",                                ~
               at (07,25), fac(lfac$( 2)), part$                , ch(25),~
               at (07,50), fac(hex(8c)),   pdescr$              , ch(30),~
                                                                         ~
               at (08,02), "Store/Warehouse Code",                       ~
               at (08,25), fac(hex(8c)),   store$               , ch(03),~
               at (08,50), fac(hex(8c)),   sdescr$              , ch(30),~
                                                                         ~
               at (09,02), "Lot Number",                                 ~
               at (09,25), fac(hex(8c)),   lot$                 , ch(16),~
                                                                         ~
               at (11,02), "Current Book Quantity",                      ~
               at (11,25), fac(hex(8c)),   old_book$            , ch(12),~
                                                                         ~
               at (13,02), "New Book Quantity",                          ~
               at (13,25), fac(lfac$( 6)), new_book$            , ch(12),~
                                                                         ~
               at (14,02), "Variance Reason Code",                       ~
               at (14,25), fac(lfac$( 7)), reason$              , ch(06),~
               at (14,50), fac(hex(8c)),   rdescr$              , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L40590
                  call "MANUAL" ("HNYPICIN") : goto L40160

L40590:        if keyhit% <> 15% then L40620
                  call "PRNTSCRN" : goto L40160

L40620:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40810     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40770
                str(pf$(3%),64%)    = " " : str(pfkeys$,16%,1%) = hex(ff)
L40770:     if fieldnr% <> 1% and fieldnr% <> 6% then L40790
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40790:     return

L40810: if fieldnr% > 0% then L40900  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40900:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50120,         /* Count Session          */~
                              L50300          /* Part Number            */
            return

L50120: REM Test for Count Session Number         SESSION$
            if session$ = "?" then session$ = " "
            plowkey$ = str(session$) & str(part$) & str(store$) &        ~
                       str(lot$) & hex(00)
            descr$ = hex(06) & " Select Inventory Session"
            call "PLOWCODE" (#3, plowkey$, descr$, -2%, -.001, f1%(3%))
            if f1%(3%) = 1% then L50210
                errormsg$ = "Inventory Session NOT On File"
                return
L50210:     session$ = str(plowkey$,1%,2%)
                call "DESCRIBE" (#8, session$, sesdescr$, 0%, f1%(8%))
                if f1%(8%) <> 0% then return            /*  PI SESSION  */
            plowkey$ = str(session$) & hex(000000000000000000)
            call "REDALT4" (#4, plowkey$, 2%, f1%(4%))
            get #4 using L50270, sesdescr$               /*  CC SESSION  */
L50270:         FMT POS(13), CH(30)
            return

L50300: REM Test for Part Number                  PART$
            if part$ = "?" then part$ = " "
            plowkey$ = str(session$) & str(part$) & str(store$) &        ~
                       str(lot$) & hex(00)
            descr$ = hex(06) & "Select Part/Store/Lot To Use"
            call "PLOWCODE" (#3, plowkey$, descr$,  9002%, .44, f1%(3%), ~
                 hdr$(), 0, 3.00030, incl(), incl$(), "D", " ", #3,      ~
                 descr_map())
            if f1%(3%) = 1% then L50410
                errormsg$ = "Part Is NOT Valid For This Session"
                return
L50410:     part$  = str(plowkey$,3%,25%)
            store$ = str(plowkey$,28%,3%)
            lot$   = str(plowkey$,31%,16%)
            gosub dataload
            goto input2


        deffn'152(fieldnr%)
            errormsg$ = " "
            if fieldnr% = 6% then gosub L50530 else gosub L50600
            return

L50530: REM Test for New Book Quantity            NEW_BOOK$
            if new_book$ <> " " then L50570
                errormsg$ = "New Book Quantity CANNOT Be Blank"
                return
L50570:     call "NUMTEST"  (new_book$, 0, 9e7, errormsg$, 2.2, new_book)
            return

L50600: REM Test for Variance Reason Code         REASON$
            if reason$ = "?" then reason$  =  " "
            plowkey$ = "VARREASON" &  reason$
            descr$   = hex(06) & "Select Variance Reason Code"
            call "PLOWCODE" (#2, plowkey$, descr$, 9%, .3, f1%(2%))
                 if f1%(2) = 1% then L50680
                     errormsg$ = "Variance Reason Code NOT On File"
                     return
L50680:     call "DESCRIBE" (#2, plowkey$, rdescr$, 0%, f1%(2%))
            reason$ = str(plowkey$,10%,6%)
            goto editpg1

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
            if page% = 0% then L65210
            runtime$ = " "
            call "TIME" (runtime$)
            print
            print "*****   END OF REPORT @ " & runtime$ & " *****"
L65210:     call "SETPRNT" ("HNY058", " ", 0%, 1%)
            end
