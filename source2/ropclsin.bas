        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  RRRR    OOO   PPPP    CCC   L       SSS   IIIII  N   N   *~
            *  R   R  O   O  P   P  C   C  L      S        I    NN  N   *~
            *  RRRR   O   O  PPPP   C      L       SSS     I    N N N   *~
            *  R   R  O   O  P      C   C  L          S    I    N  NN   *~
            *  R   R   OOO   P       CCC   LLLLL   SSS   IIIII  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ROPCLSIN - Maintain ROP Part Class Codes                  *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1989  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/27/89 ! Original                                 ! MJB *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            descr_map(10),               /* Description Map for PLOW   */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            fill$13,                     /* Record Filler              */~
            hdr$(3)132,                  /* for PLOWCODE               */~
            incl(1),                     /* for PLOWCODE               */~
            incl$(1)1,                   /* for PLOWCODE               */~
            inpmessage$79,               /* Informational Message      */~
            inpmsg$(5,15)79,             /* Input Message Array        */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$25,                  /* for PLOWCODE               */~
            ropcflag$1,                  /* ROP Calculation Flaf       */~
            ropcldescr$30,               /* ROP Class Description      */~
            ropcode$4,                   /* ROP Class Code             */~
            ropformula$1,                /* ROP Calc Formula           */~
            ropxflag$1,                  /* ROP Excess Test Flag       */~
            rptdescr$35,                 /* for PLOWCODE               */~
            userid$3                     /* Current User Id            */~

        dim f2%(08),                     /* = 0 if the file is open    */~
            f1%(08),                     /* = 1 if READ was successful */~
            fs%(08),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(08)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.00.00 01/19/90 CMS2 / CMS-I Merge              "
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
            * #01 ! ROPCLASS ! ROP Part Class Codes                     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "ROPCLASS",                                      ~
                        varc,     indexed,  recsize =  50,               ~
                        keypos = 1,    keylen =  4

            select #02, "ROPHNY",                                        ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos = 1,    keylen =  25,                     ~
                        alt key  1, keypos =  104, keylen = 4, dup


            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01), 50%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02),  0%, rslt$(02))


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date  :  fill$ = " "
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62) = "ROPCLSIN: " & str(cms2v$,,8)
            gosub load_messages

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
L10205:               if keyhit% = 14% then gosub print_report
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
                  gosub call_screen
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  8% then       delete_code
                  if keyhit%  = 14% then gosub print_report
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       editpg1
L11130:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 2% or fieldnr% >  5% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11180:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  gosub call_screen
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11180
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11180
                  lastfieldnr% = fieldnr%
            goto L11130

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            call "READ101" (#1, ropcode$, f1%(1))
            put #1 using L35030, ropcode$, ropcldescr$, ropxflag$,        ~
                                ropcflag$, ropformula$, fill$
            if f1%(1) = 1% then rewrite #1 else write #1
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* ROP Class Code         */~
                              L20200,         /* ROP Class Desc         */~
                              L20300,         /* ROP Excess Flag        */~
                              L20400,         /* ROP Calc Flag          */~
                              L20500          /* ROP Calc Formula       */
            return
L20100: REM Def/Enable ROP Class Code              ROPCODE$
            return

L20200: REM Def/Enable ROP Class Description       ROPCLDESCR$
            return

L20300: REM Def/Enable ROP Excess Test Flag        ROPXFLAG$
            if ropxflag$ = " " then ropxflag$ = "N"
            return

L20400: REM Def/Enable ROP Calculation Flaf        ROPCFLAG$
            if ropcflag$ = " " then ropcflag$ = "Y"
            return

L20500: REM Def/Enable ROP Calc Formula            ROPFORMULA$
            if ropformula$ = " " then ropformula$ = "1"
            return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        load_messages
            init (" ") inpmsg$()
            inpmsg$(1%,  1%) = "Enter ROP Class Code                "
            inpmsg$(1%,  2%) = "Enter ROP Class Description         "
            inpmsg$(1%,  3%) = "Enter 'Y' or 'N' to test the Deviat " &  ~
                               "ion Ratio for this Class"
            inpmsg$(1%,  4%) = "Enter 'Y' or 'N' to Calculate ROP "   &  ~
                               "for this Class"
            inpmsg$(1%,  5%) = "Enter 1, 2, or 3 to designate the "   &  ~
                               "Formula for Excess Inventory Calculation"
            return

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      ropcflag$, ropcldescr$, ropcode$, ropformula$,     ~
                      ropxflag$
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1989  an unpublished work by CAELUS,       *~
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
            call "READ100" (#1, ropcode$, f1%(1))
            if f1%(1) = 0% then return
            get #1 using L35030, ropcode$, ropcldescr$, ropxflag$,        ~
                                ropcflag$, ropformula$, fill$
            return

        REM *************************************************************~
            *           P R I N T   C O D E S   R E P O R T             *~
            *-----------------------------------------------------------*~
            * Call PLOWCODE to Print ROP Class Codes Report.            *~
            *************************************************************
        print_report
            plowkey$ = " "
            rptdescr$ = hex(06) & "REORDER POINT CLASS CODES LISTING"
            incl(1) = 0  :  incl$(1) = " "
            hdr$(1) = "ROP Class Code  Description                   " & ~
                      "Test Std Dev  Calculate ROP   Formula         "

                     /*     xxxx       xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx */
                     /*     x              x            x             */

            descr_map(1) =  1.04  : descr_map( 2) =  6.0
            descr_map(3) =  5.30  : descr_map( 4) = 17.0
            descr_map(5) = 35.01  : descr_map( 6) = 52.0
            descr_map(7) = 36.01  : descr_map( 8) = 67.0
            descr_map(9) = 37.01  : descr_map(10) = 80.0

            call "PLOWCODE" (#1, plowkey$, rptdescr$, -9000%, 0.3,       ~
                             f1%(1), hdr$(), 0, 0, incl(), incl$(),      ~
                             "r", " ", #2, descr_map())
            return

        REM *************************************************************~
            *           D E L E T E   C L A S S   C O D E               *~
            *-----------------------------------------------------------*~
            * Attempt to Delete an ROP Class Code                       *~
            *************************************************************
        delete_code
            call "REDALT0" (#2, ropcode$, 1%, f1%(2))
                if f1%(2) <> 0% then L32210
L32090:     ask% = 2%
            call "ASKUSER" (ask%, "***** DELETE CONFIRMATION *****",     ~
                            "Code May Be DELETED", "Press PF-24 to " &   ~
                            "DELETE", "or Press PF-1 to RETURN to E" &   ~
                            "dit Mode")
            if ask% = 1% then editpg1
            if ask% <> 24% then L32090
            call "READ101" (#1, ropcode$, f1%(1))
            delete #1
            goto inputmode

L32210:     ask% = 2%
            call "ASKUSER" (ask%, "***** DELETE DENIAL *****",           ~
                           "ROP Code '" & ropcode$ & "' is currently " & ~
                           "Assigned to one or more Parts", "It CANNOT" &~
                           " be DELETED at this time", "Press PF-1 to" & ~
                           " RETURN to Edit Mode")
            if ask% = 1% then editpg1
            goto L32210

        REM *************************************************************~
            *     M I S C E L L A N E O U S   S U B R O U T I N E S     *~
            *-----------------------------------------------------------*~
            * Internal Subroutines for GETSCRN, MANUAL and PRNTSCRN.    *~
            *************************************************************

            call_screen
                call "GETSCRN" ("C", " ", cursor%(), 0%)
                return

            pf1315
                if keyhit% <> 13% then L34650
                    call "MANUAL" ("ROPCLSIN")
                    keyhit% = 15%
                    return
L34650:         if keyhit% <> 15% then return
                    call "PRNTSCRN"
                    return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35030: FMT                 /* FILE: ROPCLASS                          */~
            CH(4),          /* ROP Part Class Code                     */~
            CH(30),         /* Description                             */~
            CH(1),          /* General Purpose Flag or Switch Indicator*/~
            CH(1),          /* General Purpose Flag or Switch Indicator*/~
            CH(1),          /* General Purpose Flag or Switch Indicator*/~
            CH(13)          /* Unused Space                            */~

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              if fieldnr% = 0% then inpmessage$ = edtmessage$            ~
                               else inpmessage$ = inpmsg$(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40100,         /* ROP Class Code    */   ~
                                L40095,         /* ROP Class Desc    */   ~
                                L40100,         /* ROP Excess Flag   */   ~
                                L40100,         /* ROP Calc Flag     */   ~
                                L40100          /* ROP Calc Formula  */
              goto L40115

L40095:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40100:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40115:     accept                                                       ~
               at (01,02),                                               ~
                  "ROP Part Class Code Maintenance",                     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "ROP Class Code",                             ~
               at (06,35), fac(lfac$( 1)), ropcode$             , ch(04),~
                                                                         ~
               at (07,02), "ROP Class Description",                      ~
               at (07,35), fac(lfac$( 2)), ropcldescr$          , ch(30),~
                                                                         ~
               at (08,02), "Deviation Ratio Test Flag",                  ~
               at (08,35), fac(lfac$( 3)), ropxflag$            , ch(01),~
                                                                         ~
               at (09,02), "ROP Calculation Flag",                       ~
               at (09,35), fac(lfac$( 4)), ropcflag$            , ch(01),~
                                                                         ~
               at (10,02), "ROP Calculation Formula Code",               ~
               at (10,35), fac(lfac$( 5)), ropformula$          , ch(01),~
               at (10,40), "Formula '1' is OH - ROP - EOQ",              ~
               at (11,40), "        '2' is ATC",                         ~
               at (12,40), "        '3' is OH ",                         ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               gosub pf1315
               if keyhit% = 15% then L40115
               return

        set_pf1
        if edit% = 2% then L40365     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                      (1" &        ~
                     "4)Print Listing        (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0d0e0f1000)
            if fieldnr% = 1% then L40345
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
                str(pf$(3),39,18) = " "  :  str(pfkeys$,14,1) = hex(ff)
L40345:     if fieldnr% > 2% then L40355
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40355:     return

L40365: if fieldnr% > 0% then L40410  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                          (" &        ~
                     "8)Delete Code          (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                      (1" &        ~
                     "4)Print Listing        (16)Save Data   "
            pfkeys$ = hex(01fffffffffff708ffffffff0d0e0f1000)
            return
L40410:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50100,         /* ROP Class Code         */~
                              L50200,         /* ROP Class Desc         */~
                              L50300,         /* ROP Excess Flag        */~
                              L50400,         /* ROP Calc Flag          */~
                              L50500          /* ROP Calc Formula       */
            return
L50100: REM Test for ROP Class Code               ROPCODE$
            if ropcode$ <> " " then L50160
            call "GETCODE" (#1, ropcode$, " ", 0%, 0.30, f1%(1))
                if f1%(1) <> 0% then L50160
                     errormsg$ = hex(00)
                     return
L50160:     gosub dataload
            if f1%(1) = 0% then return
                return clear all
                goto editpg1

L50200: REM Test for ROP Class Description        ROPCLDESCR$
            if ropcldescr$ <> " " then return
            errormsg$ = "ROP Class Code Description Cannot be Blank"
            return

L50300: REM Test for ROP Excess Test Flag         ROPXFLAG$
            if ropxflag$ = "Y" or ropxflag$ = "N" then return
                errormsg$ = "Must Enter 'Y' or 'N'"
                return

L50400: REM Test for ROP Calculation Flag         ROPCFLAG$
            if ropcflag$ = "Y" or ropcflag$ = "N" then return
                errormsg$ = "Must Enter 'Y' or 'N'"
                return

L50500: REM Test for ROP Calc Formula             ROPFORMULA$
            if ropformula$ = "1" or ropformula$ = "2" or                 ~
                                    ropformula$ = "3" then return
                errormsg$ = "Must Enter 1, 2, or 3"
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
            *  Copyright (c) 1989  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
