        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  JJJJJ  BBBB   TTTTT   CCC   PPPP   RRRR    GGG   EEEEE   *~
            *    J    B   B    T    C   C  P   P  R   R  G      E       *~
            *    J    BBBB     T    C      PPPP   RRRR   G GGG  EEEE    *~
            *  J J    B   B    T    C   C  P      R   R  G   G  E       *~
            *   J     BBBB     T     CCC   P      R   R   GGG   EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBTCPRGE - Purges time cards from the files JBTCMSTR and  *~
            *            JBTCLINE.                                      *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1988  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/17/88 ! Original                                 ! RN2 *~
            * 07/19/93 ! PRR 12316. Added option to purge JBTCTASK! JDH *~
            *          ! PRR 12866. Added Processing Message.     !     *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            emp$12,                      /* Employee Code              */~
            emp_range$(4)12,             /* Employee Code Range        */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Misc Plow Key              */~
            purge_datef$8, purge_date$8, /* Purge through date         */~
            purge_task$1,                /* Purge JBTCTASK file? (Y/N) */~
            readkey$50,                  /* A Read Key                 */~
            tc_date$6                    /* Time Card Date             */


        dim f2%(64),                     /* = 0 if the file is open    */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20, axd$4           /* Text from file opening     */

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
            * #05 ! JBTCMSTR ! Time Card Master File                    *~
            * #06 ! JBTCLINE ! Time Card Details File                   *~
            * #07 ! JBTCTASK ! Summary by Task Code File                *~
            * #09 ! JBTCBUFF ! Time Card Master File (Buffer)           *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #05, "JBTCMSTR",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    8, keylen =  18,                     ~
                        alt key  1, keypos =    2, keylen =  18,         ~
                            key  2, keypos =    1, keylen =  19          ~

            select #06, "JBTCLINE",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    1, keylen =  25                      ~

            select #07, "JBTCTASK",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    1, keylen =  34,                     ~
                        alt key  1, keypos =   11, keylen =  6, dup

            select #09, "JBTCBUFF",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    8, keylen =  18,                     ~
                        alt key  1, keypos =    2, keylen =  18,         ~
                            key  2, keypos =    1, keylen =  19          ~

        call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENFILE" (#5, "IO   ", f2%(5%), rslt$(5%), axd$)
            call "OPENFILE" (#6, "IO   ", f2%(6%), rslt$(6%), axd$)
            if f2%(5%) <> 0% or f2%(6%) <> 0% then exit_program

            call "OPENCHCK" (#7, fs%(7%), f2%(7%), 0%, rslt$(7%))
            call "OPENCHCK" (#9, fs%(9%), f2%(9%), 0%, rslt$(9%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62) = "JBTCPRGE: " & str(cms2v$,,8)

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
                         errormsg$ = " "
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
                  if keyhit%  = 16% then       purge_data
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% >  3% then editpg1
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
            *                  P U R G E   D A T A                      *~
            *-----------------------------------------------------------*~
            * Purge time cards per the selection criteria.  Also, check *~
            * that the card is not in the buffer file before zapping.   *~
            *************************************************************

        purge_data
            call "SHOSTAT" ("Purging Data...")
            if purge_task$ = "O" then task_purge

            buffer% = 0%
            plowkey$ = emp_range$(3)
            call "READ103" (#5, plowkey$, f1%)
            goto L12140

        purge_loop
            call "READNXT1" (#5, f1%)
L12140:     if f1% = 0% then task_purge

            get #5 using L12170, tc_date$, readkey$, emp$
L12170:         FMT POS(2), CH(6), CH(18), POS(8), CH(12)
            if emp$     > emp_range$(4) then task_purge
            if tc_date$ > purge_date$   then purge_loop

            call "READ100" (#9, readkey$, f1%)
            if f1% = 0% then L12260
                buffer% = buffer% + 1%
                goto purge_loop

L12260:     delete #5
            call "DELETE" (#6, readkey$, 18%)
            goto purge_loop


        end_purge
            if buffer% = 0% then exit_program

            if buffer% = 1% then                                         ~
                errormsg$ = "A Time Card was skipped because it was"     ~
                            else                                         ~
                errormsg$ = "Time Cards were skipped because they were"
            u3% = 2%
            call "ASKUSER" (u3%, "TIME CARD(S) BEING PROCESSED",         ~
                     errormsg$,                                          ~
                     "found to also be in the buffer files.",            ~
                     "Press RETURN to continue...")
            goto exit_program

        task_purge   /* Purge the JBTCTASK file, if so ordered */
            if purge_task$ = "N" then end_purge
                plowkey$ = all(hex(00))
                call "READ103" (#07, plowkey$, f7%)
                goto L12520
        task_purge_loop
            call "READNXT1" (#07, f7%)
L12520:     if f7% = 0% then end_purge
                get #07 using L12540, tc_date$, emp$
L12540:              FMT CH(6), POS(17), CH(12)
                if tc_date$ > purge_date$ then end_purge
                if emp_range$(1%) = "ALL" then L12600
                if emp$ < emp_range$(1%) or emp$ > emp_range$(2%)        ~
                                                     then task_purge_loop

L12600:         delete #07
                goto task_purge_loop

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Purge thru date        */~
                              L20200,         /* Employee Range         */~
                              L20300          /* Purge JBTCTASK?        */
            return

L20100: REM Def/Enable Purge through date          PURGE_DATEF$
            if purge_datef$ <> " " then return
                call "DATE" addr("G+", date, -30%, purge_datef$, u3%)
                call "DATEFMT" (purge_datef$)
                return

L20200: REM Def/Enable Employee Code Range         EMP_RANGE$(4)
            if emp_range$(1) = " " and emp_range$(2) = " " then          ~
                                                    emp_range$(1) = "ALL"
            return

L20300: REM Def/Enable Purge JBTCTASK Option       PURGE_TASK$
            if purge_task$ = " " then purge_task$ = "N"
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
         "Enter most recent Time Card Date to purge.                   ",~
         "Enter Employee Code Range; enter 'ALL' for all employees.    ",~
         "Enter 'Y' to also Purge the 'Summary Hours by Task Code' file; ~
        ~ 'O' for Only."

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, emp_range$(), purge_datef$,~
                      purge_task$
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1988  an unpublished work by CAELUS,       *~
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
              on fieldnr% gosub L40080,         /* Purge thru date   */   ~
                                L40080,         /* Employee Range    */   ~
                                L40080          /* Purge JBTCTASK?   */
              goto L40095

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40080:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40095:     accept                                                       ~
               at (01,02),                                               ~
                  "Purge Time Card Files",                               ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Purge Through Date",                         ~
               at (06,30), fac(lfac$(1%)), purge_datef$         , ch(08),~
                                                                         ~
               at (07,02), "Employee Code Range",                        ~
               at (07,30), fac(lfac$(2%)), emp_range$(1)        , ch(12),~
               at (07,43), "to",                                         ~
               at (07,46), fac(lfac$(2%)), emp_range$(2)        , ch(12),~
                                                                         ~
               at (08,02), "Purge Task File? (Y/N/O)",                   ~
               at (08,30), fac(lfac$(3%)), purge_task$          , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L40220
                  call "MANUAL" ("JBTCPRGE") : goto L40095

L40220:        if keyhit% <> 15% then L40235
                  call "PRNTSCRN" : goto L40095

L40235:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40330     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40310
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40310:     if fieldnr% > 1% then L40320
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40320:     return

L40330: if fieldnr% > 0% then L40375  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)PURGE Data  "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40375:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50100,         /* Purge thru date        */~
                              L50200,         /* Employee Range         */~
                              L50300          /* Purge JBTCTASK Option  */
            return

L50100: REM Test for Purge through date           PURGE_DATEF$
            call "DATEOK" (purge_datef$, u3%, errormsg$)
            if errormsg$ <> " " then return
                purge_date$ = purge_datef$
                call "DATUNFMT" (purge_date$)
                if purge_date$ >= date then                              ~
                     errormsg$ = "Purge through date must be before" &   ~
                                 " today."
                return

L50200: REM Test for Employee Code Range          EMP_RANGE$(4)
            call "TESTRNGE" (emp_range$(1), emp_range$(2),               ~
                             emp_range$(3), emp_range$(4), errormsg$)
            return

L50300: REM Test for Purge JBTCTASK Option        PURGE_TASK$
            if purge_task$ = "Y" or purge_task$ = "N" or                 ~
               purge_task$ = "O" then return
                errormsg$ = "Invalid entry.  Please enter 'Y', 'N', or" &~
                            " 'O'."
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
            *  Copyright (c) 1988  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
