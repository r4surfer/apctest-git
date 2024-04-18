        REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                                                           *~
            *  H   H  N   N  Y   Y   CCC    CCC    SSS   IIIII  PPPP    *~
            *  H   H  NN  N  Y   Y  C   C  C   C  S        I    P   P   *~
            *  HHHHH  N N N   YYY   C      C       SSS     I    PPPP    *~
            *  H   H  N  NN    Y    C   C  C   C      S    I    P       *~
            *  H   H  N   N    Y     CCC    CCC    SSS   IIIII  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYCCSIP - This sub-routine will allow the user to        *~
            *            Create/Delete a Session.                       *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/17/92 ! Original                                 ! SID *~
            * 05/13/93 ! Modified to return all entries to caller.! MLJ *~
            * 07/16/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

            sub "HNYCCSIP" (session$, sessiondescr$, plancntdate$,       ~
                            #01, #02)
        /*  SESSION$ - Session Name                                      ~
            #01 -  HNYCCSYS - Cycle Count System Control File            ~
            #02 -  HNYCCDTL - Cycle Count Session Detail File      */

        /*  Active Session Flag                                          ~
            'P' - Pre Active, 'A' - Active, 'C' - Closed           */

        dim                                                              ~
            activeflag$1,                /* Active Session Flag        */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            datetime$8,                  /* System Date/Time Stamp     */~
            descr$42,                    /* Descr For PlowCode         */~
            descr_m(6),                  /* Descr Map For PlowCode     */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            header$(3)79,                /* For PLOWCODE Call          */~
            inc(2),                      /* Plowcode Arguments         */~
            inc$(2)16,                   /* Plowcode Arguments         */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plancntdate$8,               /* Planned Count Date         */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            session$12,                  /* Session Name               */~
            sessiondescr$30,             /* Session Description        */~
            userid$3                     /* Current User Id            */~

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64)                      /* = 1 if READ was successful */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
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
            * #55 ! DUMMY    ! For PLOWCODE Extended Arguments          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #55, "DUMMY",                                         ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  42

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62) = "HNYCCSIP: " & str(cms2v$,,8)

            if plancntdate$ = " " or ~
               plancntdate$ = blankdate$ then L10000
                call "DATEOK" (plancntdate$, u3%, errormsg$)

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 3%
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
                  if keyhit%  = 12% then       delete_session
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 3% then editpg1
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
            goto exit_program

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Session Name           */~
                              L20200,         /* Session Descripti      */~
                              L20350          /* Planned Count Date     */
            return

L20100: REM Def/Enable Session Name                SESSION$
            return

L20200: REM Def/Enable Session Description         SESSIONDESCR$
            return

L20350: REM Def/Enable Planned Count Date          PLANCNTDATE$
            if plancntdate$ = " " or ~
               plancntdate$ = blankdate$ then plancntdate$ = date$
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
         "Enter Session Name                                           ",~
         "Enter Session Description                                    ",~
         "Enter Planned Count Date                                     "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,  activeflag$
            exist% = 0%
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
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
            sessiondescr$, plancntdate$ = " "
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto exit_program

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload
            call "SHOSTAT" ("Loading Session Informations....")
            get #01 using L30080, session$, sessiondescr$, activeflag$,   ~
                                 plancntdate$
L30080:                   FMT    CH(12), CH(30), CH(1), POS(66), CH(6)
            call "DATEFMT" (plancntdate$)
            exist% = 1%
            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            call "SHOSTAT" ("Saving Information to the File")
            if activeflag$ = " " then activeflag$ = "P"
            readkey$ = str(session$)
            call "DATUNFMT" (plancntdate$)
            call "READ101" (#01, readkey$, f1%(01))
L31065:     call "GETDTTM" addr(datetime$)
            put #01 using L31090, session$, sessiondescr$,                ~
                    activeflag$, session$, " ", datetime$,               ~
                    plancntdate$, " "
L31090:         FMT CH(12), CH(30), CH(1), CH(12), CH(2), CH(8),         ~
                    CH(6), CH(178)
            if f1%(01) = 0% then write #01, eod goto L31065               ~
                            else rewrite #01
            call "DATEFMT" (plancntdate$)
            return

        delete_session
            keyhit% = 0%
            call "ASKUSER" (keyhit%, "CONFIRMATION",                     ~
                   hex(8c) & "Session:" & hex(84) & session$ & hex(8c) & ~
                   "Is about to Be Deleted" & hex(84),                   ~
                   hex(8c) & "To Delete, Press PF(16)." & hex(84),       ~
                   "Hit any other PF key To abort delete")
            if keyhit% = 16% then L33096
            goto editpg1

L33096:     readkey$ = str(session$)
            call "READ101" (#02, readkey$, f1%(02))
              if f1%(02) = 0% then L33190
              keyhit% = 2%
              call "ASKUSER" (keyhit%, "***** ABORTED *****",            ~
                   hex(8c) & "Session:" & hex(84) & session$ & hex(8c) & ~
                   "Is still active and can't be deleted" & hex(84),     ~
                   "Press any PF key to continue")
            goto editpg1

L33190:     call "DELETE" (#01, readkey$, 12%)
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
              on fieldnr% gosub L40090,         /* Session Name      */   ~
                                L40085,         /* Session Descripti */   ~
                                L40085          /* Planned Cnt Date  */
              goto L40105

L40085:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40090:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40105:     accept                                                       ~
               at (01,02), "Generate Cycle Counting Session"    ,        ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Session Name",                               ~
               at (06,30), fac(lfac$( 1)), session$             , ch(12),~
                                                                         ~
               at (07,02), "Session Description",                        ~
               at (07,30), fac(lfac$( 2)), sessiondescr$        , ch(30),~
                                                                         ~
               at (08,02), "Planned Count Date",                         ~
               at (08,30), fac(lfac$( 3)), plancntdate$         , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40260
                  call "MANUAL" ("HNYCCSIP") : goto L40105

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
                     "                       (16)Return      "
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40347
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
                return
L40347:         str(pf$(2),18,17) = " "  :  str(pfkeys$, 4,1) = hex(ff)
            return

L40370: if fieldnr% > 0% then L40415  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "(12)Delete Session     (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01ffffffffffffffffffff0c0dff0f1000)
            if exist% = 1% then L40410
               str(pf$(2),41,18) = " " : str(pfkeys$,12,1) = hex(ff)
L40410:     return
L40415:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50100,         /* Session Name           */~
                              L50200,         /* Session Description    */~
                              L50308          /* Planned Count date     */
            return

L50100: REM Test for Session Name                 SESSION$
            if session$ = " " or session$ = "?" then L50120
            call "READ100" (#01, session$, f1%(01))
                if f1%(01) = 1% then L50180
                return
L50120:     init(hex(00)) str(plowkey$,,99)
            mat descr_m = zer : mat inc = zer : init(" ") inc$()
            header$(3) = hex(80) & "Select Session Name"
            header$(1) = "  Session Name     Description                "~
                       & "      Planned Count Date"
            descr_m(01) =     1.12  : descr_m(02) = 0001.0
            descr_m(03) =    13.30  : descr_m(04) = 0016.0
            descr_m(05) =    66.061 : descr_m(06) = 0051.0

            call "PLOWCODE" (#01, plowkey$, descr$, 9000%, 0.42, f1%(01),~
                             header$(), 0, 0, inc(), inc$(), "D", " ",   ~
                             #55, descr_m())


            if f1%(01) = 1% then L50180
               errormsg$ = "Session Name Not on File" : return
L50180:     gosub dataload
            fieldnr% = 3%   /* Skip down to edit mode */
            return

L50200: REM Test for Session Description          SESSIONDESCR$
            return

L50308: REM Test for Planned Count Date           PLANCNTDATE$
            call "DATEOK" (plancntdate$, 0%, errormsg$)
            if errormsg$ <> " " then return
            call "DATUNFMT" (plancntdate$)
            call "DATUNFMT" (date$)
            if date$ > plancntdate$ then errormsg$ =                     ~
                  "Planned Count Date Must Be Equal or Greater Than "    ~
                & "Today's Date."
            call "DATEFMT" (plancntdate$)
            call "DATEFMT" (date$)
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
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            end
