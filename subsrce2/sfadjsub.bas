        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS   FFFFF   AAA   DDDD   JJJJJ   SSS   U   U  BBBB    *~
            *  S      F      A   A  D   D    J    S      U   U  B   B   *~
            *   SSS   FFFF   AAAAA  D   D    J     SSS   U   U  BBBB    *~
            *      S  F      A   A  D   D  J J        S  U   U  B   B   *~
            *   SSS   F      A   A  DDDD    J      SSS    UUU   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SFADJSUB - This SUB allows the user to move any quantity  *~
            *            on any date from the cumulative forecast back  *~
            *            to Shelf.                                      *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1988  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/19/88 ! Original                                 ! RJM *~
            * 06/13/88 ! Changed name for SFAJSTSB to SFADJSUB    ! RJM *~
            * 08/08/89 ! Fixed SFCUM2 File Status 22, also changed! MLJ *~
            *          !   one remaining SFAJSTSB to SFADJSUB.    !     *~
            * 09/22/89 ! Fixed SFMASTR2 file Status 22, and other ! JEF *~
            *          !   write and update problems.             !     *~
            * 10/02/89 ! Adjusted Shelf calc & move qty logic.    ! JDH *~
            * 07/22/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "SFADJSUB" (inpart$, #1, #2, #3, #4, #5)

        dim                                                              ~
            blankdate$8,                 /* Blank Date for Comparison  */~
            cumfcst$10,                  /* Cumulative Forecast Qty    */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            duedate$8,                   /* Date                       */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpart$25,                   /* Part Number passed in      */~
            inpmessage$79,               /* Informational Message      */~
            hdr$(2)8,                    /* Current & Adjusted hdrs    */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            moveqty$8,                   /* Quantity to Move to Shelf  */~
            new_cumf$10,                 /* New Cumulative Forecast Qty*/~
            new_shelf$10,                /* New Shelf Quantity         */~
            part$25,                     /* Part Number                */~
            partdescr$34,                /* Part Description           */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            pip$10,                      /* PIP                        */~
            pldate$6,                    /* Start Date of Plan Calendar*/~
            shelf$10,                    /* Shelf Quantity             */~
            temp$8,                      /* Temporary Date variable    */~
            userid$3                     /* Current User Id            */~

        dim                                                              ~
            cumf%(490),                  /* Cumulative Sales Forecast  */~
            work%(490)                   /* Total Sales Forcast        */~

        dim f1%(64)                      /* = 1 if READ was successful */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! SFCUM2   ! Cumulative sales forecast file           *~
            * # 2 ! SFMASTR2 ! Sales forecast master file               *~
            * # 3 ! PIPMASTR ! Planned Inventory Position Master        *~
            * # 4 ! HNYMASTR ! Inventory Master File                    *~
            * # 5 ! SYSFILE2 ! Caelus Management System Information     *~
            *************************************************************~

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$)
            date$ = date

            edtmessage$  = "To Modify Move Quantity, Press (RETURN)."

            str(line2$,62) = "SFADJSUB: " & str(cms2v$,,8)
            hdr$(1) = " Current" : hdr$(2) = "Adjusted"

            part$ = inpart$

            call "READ100" (#5, "MONTHS OPEN", f1%(5))
            if f1%(5)=0% then L65000
            get #5, using L09190, pldate$
L09190:         FMT XX(32), CH(6)
            call "DATE" addr("G-", pldate$, date$, today%, err%)
            if err%<>0% then L65000
            today%=today%+1%
            if today% < 1% or today% > 490% then L65000

            call "DATEFMT" (date$)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  3%
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
L11120:     fieldnr% = 3%
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
            on fieldnr% gosub L20100,         /* Part Number            */~
                              L20200,         /* Date                   */~
                              L20500          /* Quantity to Move       */
            return
L20100: REM Def/Enable Part Number                 PART$
            return

L20200: REM Def/Enable Date                        DUEDATE$
            if duedate$ = " " or duedate$ = blankdate$ then duedate$ = date$
            return

L20500: REM Def/Enable Quantity to Move to Shelf   MOVEQTY$
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
         "Enter Part Number                                            ",~
         "Enter Date to adjust Shelf Quantity.                         ",~
         "Enter Quantity to Move to Shelf                              "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, partdescr$, pip$,          ~
                      cumfcst$, duedate$, moveqty$, shelf$, new_cumf$,   ~
                      new_shelf$
            mat cumf% = zer  :  mat work% = zer
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1988  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
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
            call "STRTRLSE" (#1)
            call "STRTRLSE" (#2)
            return clear all
            goto inputmode

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload
            if new_cum_flag% = 0% then L30060
                mat cumf% = zer
                goto L30100
L30060:     get #1, using L35030, cumf%()

L30100:     get #3, using L35130, work%()

            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            call "SHOSTAT" ("Saving Changes . . .")

            for kkkk% = revert_date% to 490%
                 cumf%(kkkk%) = cumf%(kkkk%) - moveqty%
            next kkkk%

            if new_cum_flag% = 0% then L31120
                gosub new_sfcum
                goto L31145

L31120:     put #1, using L35030, cumf%()
            rewrite #1

L31145:     if new_sf_flag% = 0% then L31150
                gosub new_sfmastr
                goto L31250

L31150:     call "READ101" (#2, part$, f1%(2%))
                if f1%(2%) <> 0% then L31190
                     gosub locked_record
                     goto L31150
L31190:     get #2, using L35065, work%()
            work%(revert_date%) = work%(revert_date%) - moveqty%
            temp$ = date$
            call "DATUNFMT" (temp$)

            put #2, using L31225, work%(), userid$, temp$
L31225:         FMT POS(26), 490*BI(4), CH(3), CH(6)
            rewrite #2

L31250:     return


        locked_record
            kh% = 2%
            call "ASKUSER" (kh%, "RECORD IS LOCKED", " ",                ~
                                 "PRESS RETURN TO TRY AGAIN, or",        ~
                                 "PRESS PF 1 TO ABORT")
            if kh% = 0% then return
            return clear all
            goto inputmode
        new_sfcum

            put #1, using L32030, part$, cumf%()
L32030:         FMT CH(25), 490*BI(4)

            write #1
            return

        new_sfmastr

            temp$ = date$
            call "DATUNFMT" (temp$)
            mat work% = zer
            work%(revert_date%) = - moveqty%
            put #2, using L32170, part$, work%(), userid$, temp$, " "
L32170:         FMT CH(25), 490*BI(4), CH(3), CH(6), CH(30)

            write #2
            return


        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35030: FMT                 /* FILE: SFCUM2                            */~
            POS(26),                                                     ~
            490*BI(4)       /* cumulative sales forecast integer array */~

L35065: FMT                 /* FILE: SFMASTR2                          */~
            POS(26),                                                     ~
            490*BI(4)       /* integer array of sales forecasts        */

L35130: FMT                 /* FILE: PIPMASTR                          */~
            POS(27),                                                     ~
            490*BI(4)       /* PLANNED INVENTORY POSITION ARRAY        */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(84)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40095,         /* Part Number       */   ~
                                L40095,         /* Date              */   ~
                                L40100          /* Quantity to Move  */
              goto L40110

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40095:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40100:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40110:     accept                                                       ~
               at (01,02),                                               ~
                  "ADJUST FORECAST QUANTITIES",                          ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Part Number",                                ~
               at (06,30), fac(lfac$( 1)), part$                , ch(25),~
               at (07,30), fac(hex(84)), partdescr$             , ch(34),~
                                                                         ~
               at (08,02), "Date",                                       ~
               at (08,30), fac(lfac$( 2)), duedate$             , ch(08),~
                                                                         ~
               at (09,30), fac(hex(ac)), hdr$(1)                , ch(08),~
               at (09,42), fac(hex(ac)), hdr$(2)                , ch(08),~
                                                                         ~
               at (10,02), "PIP Quantity",                               ~
               at (10,28), fac(hex(8c)), pip$                   , ch(10),~
                                                                         ~
               at (11,02), "Cumulative Forecast Qty",                    ~
               at (11,28), fac(hex(8c)), cumfcst$               , ch(10),~
               at (11,40), fac(hex(84)), new_cumf$              , ch(10),~
                                                                         ~
               at (12,02), "Shelf Quantity",                             ~
               at (12,28), fac(hex(8c)), shelf$                 , ch(10),~
               at (12,40), fac(hex(84)), new_shelf$             , ch(10),~
                                                                         ~
               at (14,02), "Quantity to Move to Shelf",                  ~
               at (14,30), fac(lfac$( 3)), moveqty$             , ch(08),~
                                                                         ~
               at (18,02),"NOTE: Be careful if you UNPLAN the forecast de~
        ~mand if adjustments are made.",                                  ~
               at (19,02),"      Unplanning could result in a negative Fo~
        ~recast amount.",                                                 ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40280
                  call "MANUAL" ("SFRVRTSB") : goto L40110

L40280:        if keyhit% <> 15 then L40295
                  call "PRNTSCRN" : goto L40110

L40295:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40390     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40370
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40370:     if fieldnr% > 2% then L40380
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40380:     return

L40390: if fieldnr% > 0% then L40435  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
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
            on fieldnr% gosub L50100,         /* Part Number            */~
                              L50200,         /* Date                   */~
                              L50500          /* Quantity to Move       */
            return
L50100: REM Test for Part Number                  PART$
            new_cum_flag%, new_sf_flag% = 0%
            call "GETCODE" (#4, part$, partdescr$, 1%, 0.32, f1%(4%))
                if f1%(4%) <> 0% then L50125
                     errormsg$ = "A Part Must be Selected!"
                     return
L50125:     call "READ101" (#1, part$, f1%(1%))
                if f1%(1%) = 1% then L50140
                     new_cum_flag% = 1%
L50140:     call "READ100" (#2, part$, f1%(2%))
                if f1%(2%) = 1% then L50160
                     new_sf_flag% = 1%

L50160:     call "READ100" (#3, part$, f1%(3%))
                if f1%(3%) <> 0% then L50180
                     errormsg$ = "Part Not Found in PIP!"
                     return
L50180:     gosub dataload
            return

L50200: REM Test for Date                         DUEDATE$
            call "DATEOK" (duedate$, err%, errormsg$)
                if errormsg$ <> " " then return
            temp$ = duedate$
            call "DATUNFMT" (temp$)
            call "DATE" addr ("G-", pldate$, temp$, revert_date%, err%)
                if err% = 0% then L50250
L50240:         errormsg$ = "Date NOT Valid for Planning."
                return
L50250:     revert_date% = revert_date% + 1%
            if revert_date% < 1% or revert_date% > 490% then L50240
            convert cumf%(revert_date%) to cumfcst$, pic(-#########)
            shlf% = work%(revert_date%) - max(0%, cumf%(revert_date%))
            convert shlf% to shelf$, pic(-#########)
*          IF CUMF%(REVERT_DATE%) <= 0% THEN 51000
            convert work%(revert_date%) to pip$, pic(-#########)
            return

L50500: REM Test for Quantity to Move to Shelf    MOVEQTY$
            convert moveqty$ to moveqty%, data goto L50850
            new_shelf% = shlf%
            if moveqty% = 0% then L50800     /* No Change */
            if moveqty% < 0% then L50700

*        Positive movements to move Shelf upward
            if cumf%(revert_date%) < 0 then L50640         /* Neg CUMF */
            if moveqty% <= cumf%(revert_date%) then L50800 /* Pos CUMF */
                errormsg$ = "Quantity too large, must not be more than th~
        ~e Cumulative Forecast Quantity."
                return
L50640:         errormsg$ = "Can't move to Shelf from a Negative Forecast~
        ~ Position."
                return

*        Negative Movements to adjust CUMF upward
L50700:     if shlf% < 0 then L50750                       /* Neg Shelf */
            if cumf%(revert_date%) < 0 then L50730         /* Neg CUMF */
                if abs(moveqty%) <= shlf% then L50800
                     errormsg$ = "Can't move to Forecast more than you ha~
        ~ve in Shelf."
                     return
L50730:     if abs(moveqty%) <= shlf% + abs(cumf%(revert_date%))         ~
                then L50790
                errormsg$ = "Can't move to Forecast more than Shelf plus ~
        ~amount of Neg. Forecast."
                return
L50750:     if cumf%(revert_date%) < 0 then L50760         /* Neg CUMF */
                errormsg$ = "Can't move to Forecast from a Negative Shelf~
        ~ Position."
                return
L50760:     if moveqty% >= cumf%(revert_date%) then L50810
                errormsg$ = "Can't force Forecast to a Positive Position ~
        ~with Neg. Shelf."
                return

L50790:     new_shelf% = shlf% - max(0, cumf%(revert_date%) - moveqty%)
                goto L50810
L50800:     new_shelf% = shlf% + moveqty%
L50810:     new_cumf% = cumf%(revert_date%) - moveqty%
            convert new_cumf% to new_cumf$, pic(-#########)
            convert new_shelf% to new_shelf$, pic(-#########)
            return
L50850:     errormsg$ = "Quantity Invalid !"
            return

        REM *************************************************************~
            *   ERROR MESSAGE FOR WHEN THERE IS NOTHING IN CUM FORECAST *~
            *************************************************************

            kh% = 2%
            call "ASKUSER" (kh%, "CAN NOT CONTINUE", "There is nothing le~
        ~ft in the Forecast", "to be moved back to Shelf.", "Press any key~
        ~ to Continue . . .")
            return clear all
            goto inputmode

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1988  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
