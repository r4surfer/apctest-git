        REM *************************************************************~
            *                                                           *~
            *   AAA   PPPP    CCC   RRRR   PPPP   TTTTT    1    55555   *~
            *  A   A  P   P  C   C  R   R  P   P    T     11    5       *~
            *  AAAAA  PPPP   C      RRRRR  PPPP     T      1    5555    *~
            *  A   A  P      C   C  R  R   P        T      1        5   *~
            *  A   A  P       CCC   R   R  P        T    11111  5555    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * APCRPT15 - Summary Totals for Specified Product Line      *~
            *            or Report for (ALL) Product Lines              *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/29/91 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 06/16/93 ! Mod to Create a Report for All Models    ! RHH *~
            * 01/19/97 ! Mods for (New) Planning System           ! RHH *~
            *          !                                          !     *~
            * 11/06/97 ! Revision Update For 60403                ! DJD *~
            *          !                                          !     *~
            *************************************************************

        dim                                                              ~
            dt_key1$57, dt_rec$256,      /* Alt Key (1) for APCPLNDT   */~
            part$25,                     /* Part Number                */~
            model$(50%)3,                /* Model Codes                */~
            color$(50%)2,                /* Color Codes                */~
            mod_val$(50%)5,              /* Model Production Count     */~
            mod_val%(50%),               /* Model Production Counter   */~
            r_mod$(1000%)3,              /* REPORT MODELS              */~
            r_val%(1000%,7%),            /* REPORT COLOR VALUES        */~
            descr$32,                    /* Color Code Description     */~
            dt_dept$3,                   /* Department Code            */~
            dt_dept_d$30,                /* Department Description     */~
            b_dte$10,                    /* Formatted Due Date         */~
            b_dte1$10,                   /* Beginning Date             */~
            e_dte$10,                    /* Formatted Due Date         */~
            e_dte1$10,                   /* ENDING    Date             */~
            readkey$24,                  /* Gencodes Key               */~
            company$60,                  /* For Report Company Name    */~
            print_title$60,              /* For Report Title           */~
            rpt_time$8,                  /* For Report Time            */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$10,                     /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim f2%(10%),                    /* = 0 if the file is open    */~
            f1%(10%),                    /* = 1 if READ was successful */~
            fs%(10%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(10%)20                 /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "06.04.03 11/06/97 Product Count for Departments  "
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
            * #1  ! APCPLNDT ! Load Piece Detail Items - Makes and Pull *~
            * #4  ! GENCODES ! SYSTEM MASTER CODE TABLE FILES           *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************


            select #1,   "APCPLNDT",                                     ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =   24, keylen =   23,                    ~
                        alt key  1, keypos =   47, keylen =  57,         ~
                            key  2, keypos =   53, keylen =  51,         ~
                            key  3, keypos =    1, keylen =  23, dup,    ~
                            key  4, keypos =   96, keylen =   8, dup

            select #4,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24


            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),  0%, rslt$(1%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%),  0%, rslt$(4%))

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATFMTC" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to   2%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10215
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10215:               if keyhit% = 16% and fieldnr% = 1% then exit_program
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
                  if keyhit%  = 14% then gosub print_report
                  if keyhit%  = 16% then goto exit_program
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 2% then editpg1
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
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

            print_report
               gosub generate_report
            return clear all
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
         return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28110
                inpmessage$ = edtmessage$
                return

L28110
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter a Valid Starting and Ending Production Date.           ",~
         "Enter a Valid Department or (ALL) for All Departments?       "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, b_dte$, b_dte1$, e_dte$,   ~
                      e_dte1$, dt_dept$, dt_dept_d$, dt_rec$
            msg$ = "Mod Cl Qty  "
        clear_data
            init(" ") model$(), mod_val$(), color$(), r_mod$()
            mat mod_val% = zer
            mat r_val%   = zer
            for i% = 1% to 50%
              convert mod_val%(i%) to mod_val$(i%), pic(#####)
            next i%
        return

        REM *************************************************************~
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
        REM DATALOAD

        REM RETURN

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        REM DATAPUT

        REM RETURN CLEAR ALL
        REM GOTO INPUTMODE


        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************
                                         /* APCWORK1 - Work File       */
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
              on fieldnr% gosub L40160,         /* Production Dates  */   ~
                                L40170          /* Department Code   */

              goto L40200

L40160:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40170:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40200:     accept                                                       ~
               at (01,02),"New Planning",                                ~
               at (01,21),                                               ~
                  "***************************************",             ~
               at (02,02),"(APCRPT15)",                                  ~
               at (02,21),                                               ~
                  "***  Production Summary for Models  ***",             ~
               at (03,21),                                               ~
                  "***************************************",             ~
               at (01,64), "Today:",                                     ~
               at (01,71), fac(hex(8c)), date$                  , ch(10),~
               at (02,64), "Time :",                                     ~
               at (02,71), fac(hex(8c)), rpt_time$              , ch(08),~
               at (04,14),                                               ~
                 "Print Screen for Hard Copy - No Data From Alpha Loads",~
               at (05,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Starting Production Date:",                  ~
               at (06,28), fac(lfac$(1%)), b_dte$               , ch(10),~
               at (06,40), "Ending Production Date  :",                  ~
               at (06,65), fac(lfac$(1%)), e_dte$               , ch(10),~
                                                                         ~
               at (07,02), "For Department          :",                  ~
               at (07,28), fac(lfac$(2%)), dt_dept$             , ch(03),~
               at (07,40), fac(hex(84)),   dt_dept_d$           , ch(30),~
                                                                         ~
               at (09,02), fac(hex(a4)), msg$                   , ch(12),~
               at (10,02), fac(hex(a4)), model$(1%)             , ch(03),~
               at (10,06), fac(hex(a4)), color$(1%)             , ch(02),~
               at (10,09), fac(hex(a4)), mod_val$(1%)           , ch(05),~
                                                                         ~
               at (11,02), fac(hex(a4)), model$(2%)             , ch(03),~
               at (11,06), fac(hex(a4)), color$(2%)             , ch(02),~
               at (11,09), fac(hex(a4)), mod_val$(2%)           , ch(05),~
                                                                         ~
               at (12,02), fac(hex(a4)), model$(3%)             , ch(03),~
               at (12,06), fac(hex(a4)), color$(3%)             , ch(02),~
               at (12,09), fac(hex(a4)), mod_val$(3%)           , ch(05),~
                                                                         ~
               at (13,02), fac(hex(a4)), model$(4%)             , ch(03),~
               at (13,06), fac(hex(a4)), color$(4%)             , ch(02),~
               at (13,09), fac(hex(a4)), mod_val$(4%)           , ch(05),~
                                                                         ~
               at (14,02), fac(hex(a4)), model$(5%)             , ch(03),~
               at (14,06), fac(hex(a4)), color$(5%)             , ch(02),~
               at (14,09), fac(hex(a4)), mod_val$(5%)           , ch(05),~
                                                                         ~
               at (15,02), fac(hex(a4)), model$(6%)             , ch(03),~
               at (15,06), fac(hex(a4)), color$(6%)             , ch(02),~
               at (15,09), fac(hex(a4)), mod_val$(6%)           , ch(05),~
                                                                         ~
               at (16,02), fac(hex(a4)), model$(7%)             , ch(03),~
               at (16,06), fac(hex(a4)), color$(7%)             , ch(02),~
               at (16,09), fac(hex(a4)), mod_val$(7%)           , ch(05),~
                                                                         ~
               at (17,02), fac(hex(a4)), model$(8%)             , ch(03),~
               at (17,06), fac(hex(a4)), color$(8%)             , ch(02),~
               at (17,09), fac(hex(a4)), mod_val$(8%)           , ch(05),~
                                                                         ~
               at (18,02), fac(hex(a4)), model$(9%)             , ch(03),~
               at (18,06), fac(hex(a4)), color$(9%)             , ch(02),~
               at (18,09), fac(hex(a4)), mod_val$(9%)           , ch(05),~
                                                                         ~
               at (19,02), fac(hex(a4)), model$(10%)            , ch(03),~
               at (19,06), fac(hex(a4)), color$(10%)            , ch(02),~
               at (19,09), fac(hex(a4)), mod_val$(10%)          , ch(05),~
                                                                         ~
               at (09,22), fac(hex(a4)), msg$                   , ch(12),~
               at (10,22), fac(hex(a4)), model$(11%)            , ch(03),~
               at (10,26), fac(hex(a4)), color$(11%)            , ch(02),~
               at (10,29), fac(hex(a4)), mod_val$(11%)          , ch(05),~
                                                                         ~
               at (11,22), fac(hex(a4)), model$(12%)            , ch(03),~
               at (11,26), fac(hex(a4)), color$(12%)            , ch(02),~
               at (11,29), fac(hex(a4)), mod_val$(12%)          , ch(05),~
                                                                         ~
               at (12,22), fac(hex(a4)), model$(13%)            , ch(03),~
               at (12,26), fac(hex(a4)), color$(13%)            , ch(02),~
               at (12,29), fac(hex(a4)), mod_val$(13%)          , ch(05),~
                                                                         ~
               at (13,22), fac(hex(a4)), model$(14%)            , ch(03),~
               at (13,26), fac(hex(a4)), color$(14%)            , ch(02),~
               at (13,29), fac(hex(a4)), mod_val$(14%)          , ch(05),~
                                                                         ~
               at (14,22), fac(hex(a4)), model$(15%)            , ch(03),~
               at (14,26), fac(hex(a4)), color$(15%)            , ch(02),~
               at (14,29), fac(hex(a4)), mod_val$(15%)          , ch(05),~
                                                                         ~
               at (15,22), fac(hex(a4)), model$(16%)            , ch(03),~
               at (15,26), fac(hex(a4)), color$(16%)            , ch(02),~
               at (15,29), fac(hex(a4)), mod_val$(16%)          , ch(05),~
                                                                         ~
               at (16,22), fac(hex(a4)), model$(17%)            , ch(03),~
               at (16,26), fac(hex(a4)), color$(17%)            , ch(02),~
               at (16,29), fac(hex(a4)), mod_val$(17%)          , ch(05),~
                                                                         ~
               at (17,22), fac(hex(a4)), model$(18%)            , ch(03),~
               at (17,26), fac(hex(a4)), color$(18%)            , ch(02),~
               at (17,29), fac(hex(a4)), mod_val$(18%)          , ch(05),~
                                                                         ~
               at (18,22), fac(hex(a4)), model$(19%)            , ch(03),~
               at (18,26), fac(hex(a4)), color$(19%)            , ch(02),~
               at (18,29), fac(hex(a4)), mod_val$(19%)          , ch(05),~
                                                                         ~
               at (19,22), fac(hex(a4)), model$(20%)            , ch(03),~
               at (19,26), fac(hex(a4)), color$(20%)            , ch(02),~
               at (19,29), fac(hex(a4)), mod_val$(20%)          , ch(05),~
                                                                         ~
               at (09,40), fac(hex(a4)), msg$                   , ch(12),~
               at (10,40), fac(hex(a4)), model$(21%)            , ch(03),~
               at (10,44), fac(hex(a4)), color$(21%)            , ch(02),~
               at (10,47), fac(hex(a4)), mod_val$(21%)          , ch(05),~
                                                                         ~
               at (11,40), fac(hex(a4)), model$(22%)            , ch(03),~
               at (11,44), fac(hex(a4)), color$(22%)            , ch(02),~
               at (11,47), fac(hex(a4)), mod_val$(22%)          , ch(05),~
                                                                         ~
               at (12,40), fac(hex(a4)), model$(23%)            , ch(03),~
               at (12,44), fac(hex(a4)), color$(23%)            , ch(02),~
               at (12,47), fac(hex(a4)), mod_val$(23%)          , ch(05),~
                                                                         ~
               at (13,40), fac(hex(a4)), model$(24%)            , ch(03),~
               at (13,44), fac(hex(a4)), color$(24%)            , ch(02),~
               at (13,47), fac(hex(a4)), mod_val$(24%)          , ch(05),~
                                                                         ~
               at (14,40), fac(hex(a4)), model$(25%)            , ch(03),~
               at (14,44), fac(hex(a4)), color$(25%)            , ch(02),~
               at (14,47), fac(hex(a4)), mod_val$(25%)          , ch(05),~
                                                                         ~
               at (15,40), fac(hex(a4)), model$(26%)            , ch(03),~
               at (15,44), fac(hex(a4)), color$(26%)            , ch(02),~
               at (15,47), fac(hex(a4)), mod_val$(26%)          , ch(05),~
                                                                         ~
               at (16,40), fac(hex(a4)), model$(27%)            , ch(03),~
               at (16,44), fac(hex(a4)), color$(27%)            , ch(02),~
               at (16,47), fac(hex(a4)), mod_val$(27%)          , ch(05),~
                                                                         ~
               at (17,40), fac(hex(a4)), model$(28%)            , ch(03),~
               at (17,44), fac(hex(a4)), color$(28%)            , ch(02),~
               at (17,47), fac(hex(a4)), mod_val$(28%)          , ch(05),~
                                                                         ~
               at (18,40), fac(hex(a4)), model$(29%)            , ch(03),~
               at (18,44), fac(hex(a4)), color$(29%)            , ch(02),~
               at (18,47), fac(hex(a4)), mod_val$(29%)          , ch(05),~
                                                                         ~
               at (19,40), fac(hex(a4)), model$(30%)            , ch(03),~
               at (19,44), fac(hex(a4)), color$(30%)            , ch(02),~
               at (19,47), fac(hex(a4)), mod_val$(30%)          , ch(05),~
                                                                         ~
               at (09,60), fac(hex(a4)), msg$                   , ch(12),~
               at (10,60), fac(hex(a4)), model$(31%)            , ch(03),~
               at (10,64), fac(hex(a4)), color$(31%)            , ch(02),~
               at (10,67), fac(hex(a4)), mod_val$(31%)          , ch(05),~
                                                                         ~
               at (11,60), fac(hex(a4)), model$(32%)            , ch(03),~
               at (11,64), fac(hex(a4)), color$(32%)            , ch(02),~
               at (11,67), fac(hex(a4)), mod_val$(32%)          , ch(05),~
                                                                         ~
               at (12,60), fac(hex(a4)), model$(33%)            , ch(03),~
               at (12,64), fac(hex(a4)), color$(33%)            , ch(02),~
               at (12,67), fac(hex(a4)), mod_val$(33%)          , ch(05),~
                                                                         ~
               at (13,60), fac(hex(a4)), model$(34%)            , ch(03),~
               at (13,64), fac(hex(a4)), color$(34%)            , ch(02),~
               at (13,67), fac(hex(a4)), mod_val$(34%)          , ch(05),~
                                                                         ~
               at (14,60), fac(hex(a4)), model$(35%)            , ch(03),~
               at (14,64), fac(hex(a4)), color$(35%)            , ch(02),~
               at (14,67), fac(hex(a4)), mod_val$(35%)          , ch(05),~
                                                                         ~
               at (15,60), fac(hex(a4)), model$(36%)            , ch(03),~
               at (15,64), fac(hex(a4)), color$(36%)            , ch(02),~
               at (15,67), fac(hex(a4)), mod_val$(36%)          , ch(05),~
                                                                         ~
               at (16,60), fac(hex(a4)), model$(37%)            , ch(03),~
               at (16,64), fac(hex(a4)), color$(37%)            , ch(02),~
               at (16,67), fac(hex(a4)), mod_val$(37%)          , ch(05),~
                                                                         ~
               at (17,60), fac(hex(a4)), model$(38%)            , ch(03),~
               at (17,64), fac(hex(a4)), color$(38%)            , ch(02),~
               at (17,67), fac(hex(a4)), mod_val$(38%)          , ch(05),~
                                                                         ~
               at (18,60), fac(hex(a4)), model$(39%)            , ch(03),~
               at (18,64), fac(hex(a4)), color$(39%)            , ch(02),~
               at (18,67), fac(hex(a4)), mod_val$(39%)          , ch(05),~
                                                                         ~
               at (19,60), fac(hex(a4)), model$(40%)            , ch(03),~
               at (19,64), fac(hex(a4)), color$(40%)            , ch(02),~
               at (19,67), fac(hex(a4)), mod_val$(40%)          , ch(05),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 10 then goto L42260
                  if dt_dept$ <> "ALL" then goto L42220
                     keyhit% = 14%
                     goto L42260

L42220:           rpt% = 0%
                  gosub scan_production
                  goto L40200

L42260:        if keyhit% <> 15 then goto L42300
                  call "PRNTSCRN"
                  goto L40200

L42300:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        init(" ") rpt_time$
        call "TIME" (rpt_time$)

        if edit% = 2% then L42520     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L42480
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L42480:     if fieldnr% > 1% then L42500
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L42500:     return

L42520: if fieldnr% > 0% then L42610  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2) = "                         (10)Select Data" &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffff0affffff0e0f1000)
            return
L42610:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50130,         /* Production Dates      */ ~
                              L50320          /* Department Selection  */

            return

L50130: REM Production Dates                      B_DTE$ E_DTE$
            b_dte1$, e_dte1$ = " " : date% = 0%
            if b_dte$ <> " " then goto L50170
               goto L50220
L50170:     call "DATEOKC" (b_dte$, date%, errormsg$ )
            if errormsg$ <> " " then return
               b_dte1$ = b_dte$
               call "DATUFMTC" (b_dte1$)
               goto L50240
L50220:     errormsg$ = "Must Enter a Valid Starting Production Date"
         return
L50240:     if e_dte$ <> " " then goto L50260
               e_dte$ = b_dte$
L50260:     call "DATEOKC" (e_dte$, date%, errormsg$ )
            if errormsg$ <> " " then return
               e_dte1$ = e_dte$
               call "DATUFMTC" (e_dte1$)
        return

L50320: REM Department Selection                  DT_DEPT$, DT_DEPT_D$
            init(" ") dt_dept_d$, readkey$
            if dt_dept$ <> " " then goto L50390
L50350:        dt_dept$ = "ALL"
               dt_dept_d$ = "(All) Departments"
               return

L50390:     if str(dt_dept$,1%,1%) = "A" then goto L50350
               str(readkey$,1%,9%)   = "PLAN DEPT"
               str(readkey$,10%,15%) = dt_dept$
               read #4,key = readkey$, using L50440, dt_dept_d$,          ~
                                                eod goto L50460
L50440:           FMT POS(25), CH(30)
        return
L50460:     errormsg$ = "(Error) - Invalid Department Code Selected?"
            init(" ") dt_dept$, dt_dept_d$
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

                                                   /* Report Header */
L55050: %!########## @ ########          APC Building Products           ~
        ~     APCRPT15 !

L55080: %!USER ID: ###            (New)Production Summary Report         ~
        ~    PAGE: ### !

L55110: %!Beg Date: ##########                                     End Da~
        ~te: ##########!

L55140: %! Model !   ML   !   WH   !   BZ   !   BL   !   CO   !   BG   ! ~
        ~    Total     !
L55160: %!  ###  ! #####- ! #####- ! #####- ! #####- ! #####- ! #####- ! ~
        ~    ######-   !
L55180: %!-------!--------!--------!--------!--------!--------!--------!-~
        ~--------------!
L55200: %+---------------------------------------------------------------~
        ~--------------+
L55220: %!---------------------------------------------------------------~
        ~--------------!
        %!                                                               ~
        ~              !

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        print_header
            page_no% = page_no% + 1%
            if lcnt% <> 99% then print using L55200
            print page
            print using L55200
            print using L55050, date$, rpt_time$
            print using L55080, userid$, page_no%
            print using L55110, b_dte$, e_dte$
            print using L55220
            print using L55140
            lcnt% = 6%
        return

        print_detail
            if lcnt% > 55% then gosub print_header
            print using L55180
            print using L55160, r_mod$(i%), r_val%(i%,1%), r_val%(i%,2%), ~
                           r_val%(i%,3%), r_val%(i%,4%), r_val%(i%,5%),  ~
                           r_val%(i%,6%), r_val%(i%,7%)
            lcnt% = lcnt% + 2%
        return

        select_printer
            page_no% = 0%
            lcnt%    = 99%
            rpt_time$ = " "
            print_title$ = "Production Summary for Models"
            call "FMTTITLE" (print_title$, " ", 12%)
            date$ = date  :  call "DATFMTC" (date$)
            call "TIME" (rpt_time$)
            call "COMPNAME" (12%, company$, f1%(5))
            call "SETPRNT" ("APCLDS", " ", 0%, 0%)
            select printer (134)
        return

        close_printer
            call "SETPRNT" ("APCLDS", " ", 0%, 1%)
        return

        generate_report
            gosub select_printer
            rpt% = 1%
            gosub scan_production

            for i% = 1% to mod%
                if r_val%(i%,7%) < 1% then goto L60510
                gosub print_detail
L60510:     next i%

            print using L55200
            gosub close_printer
        return

        scan_production
            gosub clear_data
            mod% = 0%
            if rpt% = 1% then gosub load_models
            call "SHOSTAT" ("Scanning Production Data")
            dt_key1$ = all(hex(00))
            str(dt_key1$,1%,6%) = str(b_dte1$,1%,6%)
            read #1,key 1% > dt_key1$, using L60660, dt_rec$,             ~
                                             eod goto scan_prod_done
L60660:        FMT CH(256)
            goto L60710
        scan_prod_next
            read #1, using L60660, dt_rec$, eod goto scan_prod_done

L60710:     if str(dt_rec$,47%,6%) > str(e_dte1$,1%,6%) then             ~
                                                 goto scan_prod_done
                                                   /* Skip Alpha Loads */
            if str(dt_rec$,1%,1%) = "A" then goto scan_prod_next
            if dt_dept$ <> "ALL" then goto L60790
               gosub check_support
               if supp% = 1% then goto scan_prod_next
               goto L60800
L60790:     if dt_dept$ <> str(dt_rec$,42%,3%) then goto scan_prod_next
L60800:        part$ = str(dt_rec$,189%,25%)
                                                   /* Skip Parts       */
               if str(dt_rec$,215%,1%) = "Y" then goto scan_prod_next
                                                   /* Skip Sashs       */
               if str(dt_rec$,214%,1%) <> "0" then goto scan_prod_next

               if rpt% = 0% then gosub screen_data                       ~
                            else gosub report_data
            goto scan_prod_next
        scan_prod_done
            if rpt% = 1% then return
            for i% = 1% to mod%
              readkey$ = all(hex(00))
              readkey$ = "COLOR    " & color$(i%)
              call "DESCRIBE" (#4, readkey$, descr$, 0%, f1%(4%) )
              color$(i%) = str(descr$,1%,2%)
              if f1%(4%) = 0% then color$(i%) = "XX"
              convert mod_val%(i%) to mod_val$(i%), pic(#####)

            next i%
        return

        screen_data
            if mod% = 0% then goto L61080
            for i% = 1% to mod%
              if str(part$,1%,3%) = model$(i%) and                       ~
                 str(part$,4%,1%) = color$(i%) then goto L61120
            next i%
L61080:        mod% = mod% + 1%
               i% = mod%
               model$(i%) = str(part$,1%,3%)
               color$(i%) = str(part$,4%,1%)
L61120:     mod_val%(i%) = mod_val%(i%) + 1%
        return

        report_data
            x% = 0%                                   /* Convert Color */
            convert str(part$,4%,1%) to x%, data goto L61180
L61180:
            if x% = 0% then return
            convert str(part$,1%,3%) to i%, data goto L61240

            r_val%(i%,x%) = r_val%(i%,x%) + 1%
            r_val%(i%,7%) = r_val%(i%,7%) + 1%
L61240: return

        load_models
           call "SHOSTAT" ("Loading Models for Report")
           mod% = 1000%
           readkey$ = all(hex(00))
           str(readkey$,1%,9%) = "MODEL    "
        load_mod_next
           read #4,key > readkey$, using L61330, readkey$, eod goto L61390
L61330:        FMT CH(24)
           if str(readkey$,1%,5%) <> "MODEL" then goto L61390
           convert str(readkey$,10%,3%) to i%, data goto L61360
L61360:
           r_mod$(i%) = str(readkey$,10%,3%)
           goto load_mod_next
L61390: return

        check_support
            supp% = 0%                         /* Override Pull Stock */
            if str(dt_rec$,42%,3%) = "102" or str(dt_rec$,42%,3%) = "104"~
                                                         then goto L61500
            init(" ") readkey$
            str(readkey$,1%,9%) = "PLAN SUPP"
            str(readkey$,10%,15%) = str(dt_rec$,42%,3%)
            read #4,key = readkey$, eod goto L61500
        supp% = 1%
L61500: return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
