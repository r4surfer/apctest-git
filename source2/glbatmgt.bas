        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *   GGG   L      BBBB    AAA   TTTTT  M   M   GGG   TTTTT   *~
            *  G   G  L      B   B  A   A    T    MM MM  G        T     *~
            *  G      L      BBBB   AAAAA    T    M M M  G GGG    T     *~
            *  G  GG  L      B   B  A   A    T    M   M  G   G    T     *~
            *   GGG   LLLLL  BBBB   A   A    T    M   M   GGG     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GLBATMGT - List, manage or print General Ledger batch     *~
            *            control records.                               *~
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
            * 09-20-90 ! Original                                 ! RAC *~
            * 12/12/90 ! Added Management Status                  ! JDH *~
            * 06/25/91 ! Conditioned Interim Export File on       ! JBK *~
            *          !   presence of background task.           !     *~
            *          ! Added CALL to ALLFREE.                   !     *~
	    * 06/14/96 ! Changes for the year 2000.               ! DXL *~
            *PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            backgnd$8,                   /* Background task for export */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            interim_file$8,              /* Interim export file name   */~
            interim_file_on$1,           /* Interim export file used   */~
            interim_file_prompt$24,      /* Screen prompt for Int. file*/~
            jnlid$3,                     /* Journal ID                 */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line$(15)79,                 /* 1                          */~
            line2$79,                    /* Screen Line #2             */~
            line3$79,                    /* Screen Line #3             */~
            line4$79,                    /* Screen Line #4             */~
            mgt_status$1,                /* Management Status          */~
            modno$2,                     /* MODULE ID                  */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pf4$(500)13,                 /* Previous Srceen Plowkey    */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            postdate$10,                 /* Posting Date               */~
            pstseq$10,                   /* Posting Sequence           */~
            savekey$99,                  /* Save update record key     */~
            status$1,                    /* Batch Status               */~
            userid$3                     /* USER ID                    */~

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
            * #01 ! GLBATCH  ! General Ledger Batch File                *~
            * #02 ! SYSFILE2 ! System file                              *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "GLBATCH",                                       ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    5, keylen =   9,                     ~
                        alt key  1, keypos =    1, keylen =  13,         ~
                            key  2, keypos =   22, keylen =   8, dup     ~

            select #02, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01), 0%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02), 0%, rslt$(02))

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

            str(line2$,62) = "GLBATMGT: " & str(cms2v$,,8)
            line3$ = "   Batch   User  Module  Journal    Post" &        ~
                     "ing     Date                           "
            line4$ = "   Status   ID     ID      ID       Sequ" &        ~
                     "ence   Posted                          "
            n% = 3%
            interim_file_on$ = "N"
            call "READ100" (#2, "SWITCHS.GL", f1%(2))
               if f1%(2) = 0% then L10000
            get #2 using L09210, backgnd$
L09210:     FMT XX(22), CH(8)
            if backgnd$ = " " then L10000
            interim_file_on$ = "Y"
            interim_file_prompt$ = "Interim Export File Name"
            n% = 4%
            str(line3$,61,7)  = "Interim"
            str(line4$,59,11) = "Export File"

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables
            gosub first_screen
        main_screen
L10100:     gosub'050(1%, 1%)
L10130:         gosub'101                  /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  =  4% then prev_screen
                      if keyhit%  =  5% then next_screen
                      if keyhit% =  14% then call "GLBATPRT" (#1,        ~
                                                        interim_file_on$)
                      if keyhit% =  16% then exit_program
                      if keyhit% <> 0% then L10130
            fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > l% then L10130
            gosub dataload
            fieldsv% = fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg2
            lastfieldnr% = 0%
            gosub'102(0%)
                  if keyhit%  =  1% then L10100
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       editpg2
L11330:     fieldnr% = cursor%(1%) - 10%
            if fieldnr% < 1% or fieldnr% >  n% then editpg2
            if fieldnr% = lastfieldnr% then    editpg2
            gosub'052(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg2
L11380:     gosub'102(fieldnr%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11380
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11380
                  lastfieldnr% = fieldnr%
            goto L11330

        REM *************************************************************~
            *             S C R E E N  H A N D L I N G                  *~
            *-----------------------------------------------------------*~
            * Screen handling routines                                  *~
            *************************************************************

        first_screen

            init (" ") plowkey$, pf4$() : pf4% = 0%
            gosub load_screen
            goto  main_screen

        next_screen:
            if line$(1) = " " or str(line$(1),,6) = "** END" then L12170
                pf4%       = pf4% + 1%
                pf4$(pf4%) = key$
                pf4$(pf4%) = addc all(hex(ff))
L12170:     gosub load_screen
            goto  main_screen

        prev_screen
            if pf4% = 0% then main_screen
                plowkey$ = pf4$(pf4%)
                pf4%     = pf4% - 1%
                gosub load_screen
                goto  main_screen


        load_screen:
            line$(), key$ = " "
            l% = 0%

L12320:     call "PLOWALTS" (#1, plowkey$, 1%, 0%, f1%(1))
            if f1%(1) = 1% then L12360
                line$(l%+1%) = "** END OF DATA **"
                return
L12360:         l% = l% + 1%
                if l% = 1% then key$ = str(plowkey$,1,13)
            get #1 using L12400, str(line$(l%),5,1), str(line$(l%),12, 3),~
                str(line$(l%),20, 2), str(line$(l%),28, 3), temp%,       ~
                str(line$(l%),47, 8), str(line$(l%),59, 8),              ~
                str(line$(l%), 7, 1), str(line$(l%),68, 8)
L12400:     FMT CH(1), CH(3), CH(2), CH(3), BI(4), POS(16), CH(6), CH(8),~
		POS(62), CH(1), CH(8)

            str(line$(l%), 6, 1) = ":"

            call "DATFMTC" (str(line$(l%),47,10))
            convert temp% to str(line$(l%),35,10), pic(##########)
            str(line$(l%),1,3) = hex(860b8c)
            if l% < 15% then L12320
            return

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub dataput
            goto main_screen

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  2  of Input. *~
            *************************************************************

        deffn'052(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L21500,         /* Batch Status           */~
                              L21800,         /* Mgmnt Status           */~
                              L21600,         /* Posting Date           */~
                              L21700          /* Interim Export File    */
            return
L21500: REM Def/Enable Batch Status                STATUS$(1)
            return

L21600: REM Def/Enable Posting Date                POSTDATE$(1)
            return

L21700: REM Def/Enable Interim Export file Name    INTERIM_FILE$
            if interim_file_on$ <> "Y" then enabled% = 0%
            return

L21800: REM Def/Enable Mgmnt Status                MGT_STATUS$
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
            if scrnr% = 2% then restore line = scrn2_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "To modify a Batch Record, Position Cursor and press <RETURN> "

        scrn2_msg  :  data                                               ~
         "Enter Batch Status                                           ",~
         "Enter Management Status (blank or '1')                       ",~
         "Enter Posting Date                                           ",~
         "Enter Interim Export File Name                               "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      interim_file$, jnlid$, line$(), modno$, postdate$, ~
                      pstseq$, status$, userid$
            l% = 0%
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
        dataload
            convert str(line$(fieldnr%),35,10) to pstseq%, data goto L30140
            put plowkey$ using L30054, str(line$(fieldnr%),20, 2),        ~
                str(line$(fieldnr%),28, 3), pstseq%
L30054:     FMT CH(2), CH(3), BI(4)
            call "READ100" (#1, plowkey$, f1%(1))
                if f1%(1) = 0% then L30140
            get #1 using L30059, status$, userid$, postdate$,             ~
                   interim_file$, mgt_status$
L30059:     FMT CH(1), CH(3), XX(11), CH(6), CH(8), POS(62), CH(1)
            savekey$  = str(plowkey$,1,13)
            modno$    = str(line$(fieldnr%),20, 2)
            jnlid$    = str(line$(fieldnr%),28, 3)
            pstseq$   = str(line$(fieldnr%),35,10)
            call "DATFMTC" (postdate$)
            return

L30140:     return clear all
            goto main_screen

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            str(line$(fieldsv%),47,10) = postdate$
            call "DATUFMTC" (postdate$)
            call "READ101" (#1, savekey$, f1%(1))
                if f1%(1) = 0% then L30140
            put #1 using L31120, status$, postdate$, interim_file$,       ~
                                mgt_status$
L31120:     FMT CH(1), POS(16), CH(6), CH(8), POS(62), CH(1)
            rewrite #1

            call "ALLFREE"

            str(line$(fieldsv%), 5, 1) = status$
            str(line$(fieldsv%),12, 3) = userid$
            str(line$(fieldsv%),20, 2) = modno$
            str(line$(fieldsv%),28, 3) = jnlid$
            str(line$(fieldsv%),35,10) = pstseq$
            str(line$(fieldsv%),59, 8) = interim_file$
            str(line$(fieldsv%), 7, 1) = mgt_status$
            init (" ")  savekey$
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101
              gosub set_pf1
L40090:     accept                                                       ~
               at (01,02),                                               ~
                  "Display CMS posting Batch Control Record",            ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(8c)), line3$                 , ch(79),~
               at (04,02), fac(hex(8c)), line4$                 , ch(79),~
               at (06,02), fac(hex(8c)), line$(1)               , ch(79),~
               at (07,02), fac(hex(8c)), line$(2)               , ch(79),~
               at (08,02), fac(hex(8c)), line$(3)               , ch(79),~
               at (09,02), fac(hex(8c)), line$(4)               , ch(79),~
               at (10,02), fac(hex(8c)), line$(5)               , ch(79),~
               at (11,02), fac(hex(8c)), line$(6)               , ch(79),~
               at (12,02), fac(hex(8c)), line$(7)               , ch(79),~
               at (13,02), fac(hex(8c)), line$(8)               , ch(79),~
               at (14,02), fac(hex(8c)), line$(9)               , ch(79),~
               at (15,02), fac(hex(8c)), line$(10)              , ch(79),~
               at (16,02), fac(hex(8c)), line$(11)              , ch(79),~
               at (17,02), fac(hex(8c)), line$(12)              , ch(79),~
               at (18,02), fac(hex(8c)), line$(13)              , ch(79),~
               at (19,02), fac(hex(8c)), line$(14)              , ch(79),~
               at (20,02), fac(hex(8c)), line$(15)              , ch(79),~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40200
                  call "MANUAL" ("GLBATMGT") : goto L40090

L40200:        if keyhit% <> 15 then L40215
                  call "PRNTSCRN" : goto L40090

L40215:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous            " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (5)Next                " &        ~
                     "(14)Print Report       (16)Exit Program"
            pfkeys$ = hex(01ffff0405ffffffffffffff0d0e0f1000)
            if pf4% > 0%     then L40290
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40290:     if f1%(1) = 1% then L40300
                str(pf$(3),18,23) = " "  :  str(pfkeys$, 5,1) = hex(ff)
L40300:     return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%)
              gosub'050(2%, fieldnr%)
              gosub set_pf2
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
                     init (hex(8c)) str(lfac$(),n% + 1%)
              on fieldnr% gosub L41110,         /* Batch Status         */~
                                L41110,         /* Mgmnt Status         */~
                                L41110,         /* Posting Date         */~
                                L41105          /* Interim Export File  */
              goto L41120

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L41105:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L41110:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L41120:     accept                                                       ~
               at (01,02),                                               ~
                  "Display CMS posting Batch Control Record",            ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "User ID",                                    ~
               at (06,30), fac(hex(8c)), userid$                , ch(03),~
                                                                         ~
               at (07,02), "Module ID",                                  ~
               at (07,30), fac(hex(8c)), modno$                 , ch(02),~
                                                                         ~
               at (08,02), "Journal ID",                                 ~
               at (08,30), fac(hex(8c)), jnlid$                 , ch(03),~
                                                                         ~
               at (09,02), "Posting Sequence",                           ~
               at (09,30), fac(hex(8c)), pstseq$                , ch(10),~
                                                                         ~
               at (11,02), "Batch Status",                               ~
               at (11,30), fac(lfac$( 1)), status$              , ch(01),~
                                                                         ~
               at (12,02), "Management Status",                          ~
               at (12,30), fac(lfac$( 2)), mgt_status$          , ch(01),~
                                                                         ~
               at (13,02), "Posting Date",                               ~
               at (13,30), fac(lfac$( 3)), postdate$            , ch(10),~
                                                                         ~
               at (14,02), fac(hex(8c)), interim_file_prompt$   , ch(24),~
               at (14,30), fac(lfac$( 4)), interim_file$        , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L41320
                  call "MANUAL" ("GLBATMGT") : goto L41120

L41320:        if keyhit% <> 15 then L41335
                  call "PRNTSCRN" : goto L41120

L41335:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
        if fieldnr% > 0% then L41475  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L41475:                              /*  Edit Mode - Enabled    */
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

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 2.                      *~
            *************************************************************

        deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L51500,         /* Batch Status           */~
                              L51800,         /* Mgmnt Status           */~
                              L51600,         /* Posting Date           */~
                              L51700          /* Interim Export File    */
            return

L51500: REM Test for Batch Status                 STATUS$(1)
            convert status$ to status%, data goto L51530
            if status% >= 0% and status% <= 7% then return
L51530:     errormsg$ = "Batch status must be between 0 and 7"
            return

L51600: REM Test for Posting Date                 POSTDATE$(1)
            call "DATEOKC" (postdate$, 0%, errormsg$)
            return

L51700: REM Test for Interim Export File Name     INTERIM_FILE$
            return

L51800: REM Test for Management Status            MGT_STATUS$
            if mgt_status$ = " " or mgt_status$ = "1" then return
            errormsg$ = "Management status must be blank or '1'."
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
            *  Copyright (c) 1990  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
