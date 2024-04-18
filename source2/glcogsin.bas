        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *   GGG   L       CCC    OOO    GGG    SSS   IIIII  N   N   *~
            *  G      L      C   C  O   O  G      S        I    NN  N   *~
            *  G GGG  L      C      O   O  G GGG   SSS     I    N N N   *~
            *  G   G  L      C   C  O   O  G   G      S    I    N  NN   *~
            *   GGG   LLLLL   CCC    OOO    GGG    SSS   IIIII  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GLCOGSIN - Assigns COGS accounts to each cost bucket for  *~
            *            a primary COGS account.                        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1995  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/02/95 ! Original                                 ! RJ1 *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            account$(12)12,              /* Assigned Account Number    */~
            acct_descr$(12)60,           /* Assigned Account No. Descr */~
            bucket_descr$(12)20,         /* Bucket Description         */~
            bucket_id$(12)10,            /* Bucket ID                  */~
            cogs_acct$12,                /* Primary COGS Account       */~
            cogs_descr$32,               /* primary COGS Account Descr */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            header$(2)79,                /* Table Header               */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            bfac$(20)1,                  /* Field Attribute Characters */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            setid$4,                     /* Cost Set Id                */~
            setid_descr$30,              /* Cost Set Id Description    */~
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
            * #01 ! SYSFILE2 ! Caelus Management System Information     *~
            * #02 ! GLCOGSXR ! GL COGS Accounts via Cost buckets        *~
            * #03 ! GLMAIN   ! General Ledger CHart Of Accounts File.   *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select #02, "GLCOGSXR",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  9

            select #03, "GLMAIN",                                        ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   9                      ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01%), f2%(01%),   0%, rslt$(01%))
            call "OPENCHCK" (#02, fs%(02%), f2%(02%), 100%, rslt$(02%))
            call "OPENCHCK" (#03, fs%(03%), f2%(03%),   0%, rslt$(03%))

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

            str(line2$,62) = "GLCOGSIN: " & str(cms2v$,,8)


            str(header$(2),  1) = hex(ac) & "##" & hex(8c)
            str(header$(2),  5) = hex(ac) & "Bucket ID" & hex(8c)
            str(header$(2), 18) = hex(ac) & "Bucket Description  "       ~
                                                                 & hex(8c)
            str(header$(2), 40) = hex(ac) & "Account No. " & hex(8c)
            str(header$(2), 54) = hex(ac) & "Account Description "       ~
                                                                 & hex(8c)
*        Describe Current Cost Set
            rqst% = 3%
            call "STCSETID" (rqst%, #1, "        ", setid$, bucket_id$(),~
                             bucket_descr$(), setid_descr$)
            if rqst%  <> 0% then goto inputmode
                ask% = 2%
                call "ASKUSER" (ask%, "** COST SET BUCKET PROBLEM **",   ~
                              "Unable to determine the cost set Buckets",~
                              "Press Any PF Key to Acknowledge and Exit",~
                              " ")
                goto exit_program

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  2%
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
                      if fieldnr% = 1% and data_loaded% = 1% then        ~
                             fieldnr% = 3%  /* Exit this Loop */

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
            fieldnr% = 0%
L11130:     if cursor%(1%) < 5%                     then editpg1
            if cursor%(1%) = 5% then fieldnr% = 1%
            if cursor%(1%) = 6% or cursor%(1%) = 7% then editpg1
            if cursor%(1%) > 19%                    then editpg1
            fieldnr% = 2%
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11210:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11210
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11210
                  lastfieldnr% = fieldnr%
            goto L11130

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
            on fieldnr% gosub L20100,         /* COGS Account           */~
                              L20200          /* Account No             */
            return
L20100: REM Def/Enable Original COGS Account       COGS_ACCT$
            return

L20200: REM Def/Enable Assigned Account Number     ACCOUNT$
            init (hex(8c)) bfac$()
            for i% = 1% to 12%
                if bucket_id$(i%) <> " " then bfac$(i%) = hex(80)
            next i%
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
         "Enter Primary COGS Account                                   ",~
         "Assign Account Number to Cost Buckets                        "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, cogs_descr$,               ~
                      account$(), acct_descr$(), cogs_acct$
            init (hex(8c)) bfac$()
            data_loaded% = 0%

            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1995  an unpublished work by CAELUS,       *~
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
            init (" ") account$(), acct_descr$()
            data_loaded% = 0%
            readkey$ = cogs_acct$
            call "GLUNFMT" ( readkey$)
            call "READ100" (#02, readkey$, f1%(2%))
                if f1%(2%) = 0% then return
            get #02 using L30110, account$()
L30110:         FMT POS(10), 12*CH(9)

            for i% = 1% to 12%
                if bucket_id$(i%) = " " then L30170    /* Next I% */
                    call "DESCRIBE" (#3, account$(i%), acct_descr$(i%),  ~
                                      0%, f1%(3%))
                    call "GLFMT" (account$(i%))
L30170:     next i%

            data_loaded% = 1%

            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            for i% = 1% to 12%
                 call "GLUNFMT"(account$(i%))
            next i%

            call "GLUNFMT" (cogs_acct$)
            readkey$ = cogs_acct$
            call "READ101" (#02, readkey$, f1%(2%))
            put #02 using L31100, cogs_acct$,account$(), userid$, date, " "
L31100:         FMT CH(9), 12*CH(9), CH(3), CH(6), CH(22)
            if f1%(2%) = 0% then write #2 else rewrite #2

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
              on fieldnr% gosub L40130,         /* COGS Account      */   ~
                                L40130          /* Account No        */
              goto L40160

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40130:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40160:     accept                                                       ~
               at (01,02),                                               ~
                  "Set Cost of Good Sold Per Cost Buckets",              ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Primary COGS Account",                       ~
               at (05,25), fac(lfac$( 1)), cogs_acct$           , ch(12),~
               at (05,40), fac(hex(8c)),   cogs_descr$          , ch(30),~
                                                                         ~
               at (07,02), fac(hex(ac)), header$(2%)            , ch(79),~
                                                                         ~
               at (08,03), " 1",                                         ~
               at (09,03), " 2",                                         ~
               at (10,03), " 3",                                         ~
               at (11,03), " 4",                                         ~
               at (12,03), " 5",                                         ~
               at (13,03), " 6",                                         ~
               at (14,03), " 7",                                         ~
               at (15,03), " 8",                                         ~
               at (16,03), " 9",                                         ~
               at (17,03), "10",                                         ~
               at (18,03), "11",                                         ~
               at (19,03), "12",                                         ~
                                                                         ~
               at (08,07), fac(hex(8c)), bucket_id$( 1%)        , ch(10),~
               at (09,07), fac(hex(8c)), bucket_id$( 2%)        , ch(10),~
               at (10,07), fac(hex(8c)), bucket_id$( 3%)        , ch(10),~
               at (11,07), fac(hex(8c)), bucket_id$( 4%)        , ch(10),~
               at (12,07), fac(hex(8c)), bucket_id$( 5%)        , ch(10),~
               at (13,07), fac(hex(8c)), bucket_id$( 6%)        , ch(10),~
               at (14,07), fac(hex(8c)), bucket_id$( 7%)        , ch(10),~
               at (15,07), fac(hex(8c)), bucket_id$( 8%)        , ch(10),~
               at (16,07), fac(hex(8c)), bucket_id$( 9%)        , ch(10),~
               at (17,07), fac(hex(8c)), bucket_id$(10%)        , ch(10),~
               at (18,07), fac(hex(8c)), bucket_id$(11%)        , ch(10),~
               at (19,07), fac(hex(8c)), bucket_id$(12%)        , ch(10),~
                                                                         ~
               at (08,20), fac(hex(8c)), bucket_descr$(01%)     , ch(20),~
               at (09,20), fac(hex(8c)), bucket_descr$(02%)     , ch(20),~
               at (10,20), fac(hex(8c)), bucket_descr$(03%)     , ch(20),~
               at (11,20), fac(hex(8c)), bucket_descr$(04%)     , ch(20),~
               at (12,20), fac(hex(8c)), bucket_descr$(05%)     , ch(20),~
               at (13,20), fac(hex(8c)), bucket_descr$(06%)     , ch(20),~
               at (14,20), fac(hex(8c)), bucket_descr$(07%)     , ch(20),~
               at (15,20), fac(hex(8c)), bucket_descr$(08%)     , ch(20),~
               at (16,20), fac(hex(8c)), bucket_descr$(09%)     , ch(20),~
               at (17,20), fac(hex(8c)), bucket_descr$(10%)     , ch(20),~
               at (18,20), fac(hex(8c)), bucket_descr$(11%)     , ch(20),~
               at (19,20), fac(hex(8c)), bucket_descr$(12%)     , ch(20),~
                                                                         ~
               at (08,42), fac(bfac$( 1%)), account$( 1%)       , ch(12),~
               at (09,42), fac(bfac$( 2%)), account$( 2%)       , ch(12),~
               at (10,42), fac(bfac$( 3%)), account$( 3%)       , ch(12),~
               at (11,42), fac(bfac$( 4%)), account$( 4%)       , ch(12),~
               at (12,42), fac(bfac$( 5%)), account$( 5%)       , ch(12),~
               at (13,42), fac(bfac$( 6%)), account$( 6%)       , ch(12),~
               at (14,42), fac(bfac$( 7%)), account$( 7%)       , ch(12),~
               at (15,42), fac(bfac$( 8%)), account$( 8%)       , ch(12),~
               at (16,42), fac(bfac$( 9%)), account$( 9%)       , ch(12),~
               at (17,42), fac(bfac$(10%)), account$(10%)       , ch(12),~
               at (18,42), fac(bfac$(11%)), account$(11%)       , ch(12),~
               at (19,42), fac(bfac$(12%)), account$(12%)       , ch(12),~
                                                                         ~
               at (08,56), fac(hex(8c)), acct_descr$(01%)       , ch(25),~
               at (09,56), fac(hex(8c)), acct_descr$(02%)       , ch(25),~
               at (10,56), fac(hex(8c)), acct_descr$(03%)       , ch(25),~
               at (11,56), fac(hex(8c)), acct_descr$(04%)       , ch(25),~
               at (12,56), fac(hex(8c)), acct_descr$(05%)       , ch(25),~
               at (13,56), fac(hex(8c)), acct_descr$(06%)       , ch(25),~
               at (14,56), fac(hex(8c)), acct_descr$(07%)       , ch(25),~
               at (15,56), fac(hex(8c)), acct_descr$(08%)       , ch(25),~
               at (16,56), fac(hex(8c)), acct_descr$(09%)       , ch(25),~
               at (17,56), fac(hex(8c)), acct_descr$(10%)       , ch(25),~
               at (18,56), fac(hex(8c)), acct_descr$(11%)       , ch(25),~
               at (19,56), fac(hex(8c)), acct_descr$(12%)       , ch(25),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L41050
                  call "MANUAL" ("GLCOGSIN") : goto L40160

L41050:        if keyhit% <> 15 then L41080
                  call "PRNTSCRN" : goto L40160

L41080:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L41270     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L41230
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L41230:     if fieldnr% > 2% then L41250
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L41250:     return

L41270: if fieldnr% > 0% then L41360  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L41360:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50100,         /* COGS Account           */~
                              L50200          /* Account No             */
            return
L50100: REM Test for Original COGS Account        COGS_ACCT$
            if cogs_acct$ = "?" then cogs_acct$ = " "
            cogs_descr$ = hex(06) & "Select a COGS Account"
            call "PLOWCODE" (#3, cogs_acct$ ,cogs_descr$,0%,0.3,f1%(3%))
            if f1%(3%) <> 0% then L50170
                errormsg$ = "Account not on file: " & cogs_acct$
                return

L50170:     cogs_descr$ = "(" & cogs_descr$ & ")"
            gosub dataload
            return

L50200: REM Test for Assigned Account Number      ACCOUNT$
            init (hex(8c)) bfac$()
            for i% = 1% to 12%
                if bucket_id$(i%) = " " then L50300    /* Next I% */
                    acct_descr$(i%) = hex(06)&"Select an Account to " &  ~
                            "Assign to " & bucket_id$(i%) & " Bucket "
                    call "PLOWCODE" (#3, account$(i%), acct_descr$(i%),  ~
                                  0%,0.3,f1%(3%))
                    if f1%(3%) <> 0% then L50300    /* Next I% */
                        errormsg$ = "Account not on file: " & account$(i%)
                        bfac$(i%) = hex(80)
                        i% = 13%
                        goto L50300    /* Next I% */
L50300:     next i%
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
            *  Copyright (c) 1995  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
