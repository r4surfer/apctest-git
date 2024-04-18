        REM *************************************************************~
            *                                                           *~
            *   AAA   PPPP    CCC    SSS   TTTTT   OOO    CCC   K   K   *~
            *  A   A  P   P  C   C  S        T    O   O  C   C  K  K    *~
            *  AAAAA  PPPP   C       SSS     T    O   O  C      KKK     *~
            *  A   A  P      C   C      S    T    O   O  C   C  K  K    *~
            *  A   A  P       CCC    SSS     T     OOO    CCC   K   K   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * APCSTOCK - BIN Location for all Standard Products         *~
            *    - Part Number                                          *~
            *    - BIN Location                                         *~
            *    - Level                                                *~
            *    - BIN Description                                      *~
            *    - Filler                                               *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/13/90 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 01/18/93 ! Modification Add Store No. as a Report   ! RHH *~
            *          !   Selection Criteria.                    !     *~
            *          !                                          !     *~
            * 11/06/97 ! Change revision number to 60403          ! DJD *~
            * 02/02/00 ! (EWD001) Mods to add Security            ! RHH *~
            *************************************************************

        dim                                                              ~
            stk_key$32,                  /* Stock Key                  */~
            stk_part$25,                 /* Part Number                */~
            stk_bin$5,                   /* BIN Location               */~
            stk_level$2,                 /* Level within BIN           */~
            stk_descr$30,                /* BIN Description            */~
            stk_filler$1,                /* Filler area                */~
            sav_dept$3,                  /* Save Department Code       */~
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
            dept$3,                      /* Department Code            */~
            dept_desc$30,                /* Department Description     */~
            model$3,                     /* Model Code                 */~
            model_desc$30,               /* Model Description          */~
            selection$1,                 /* Report Selection           */~
            sel_desc$30,                 /* Selection Description      */~
            store$(10%)3,                /* Store Numbers with Qty     */~
            store$3,                     /* Store Numbers or (ALL)     */~
            store_desc$30,               /* Store Number DESCRIPTION   */~
            on_hand$(10)6,               /* On Hand Quantities         */~
            part_desc$32,                /* Part Description           */~
            apc_scr$120,                 /* Screen Description         */~
            apc_prt$60,                  /* Printer Description        */~
            apc_sze$20,                  /* Size Long Form             */~
            readkey$50,                  /* DUMMY KEY                  */~
            company$60,                  /* For Report Company Name    */~
            print_title$60,              /* For Report Title           */~
            rpt_time$8,                  /* For Report Time            */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            users$(10%)3,                /* Use Id's that can Chg(EWD001)*/~
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
            cms2v$ = "06.04.03 11/06/97 APC New Program                "
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
            * #1  ! HNYMASTR ! Part Master File                         *~
            * #2  ! APCSTOCK ! Rack Location for Standard Products      *~
            * #3  ! AMTBOMIF ! BOM Generator Master Validity File       *~
            * #4  ! HNYQUAN  ! Part Quantity File                       *~
            * #5  ! GENCODES ! MASTER TABLES                            *~
            * #6  ! STORNAME ! Store Name Master                        *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  3, keypos =   26, keylen =  32, dup

            select #2,  "APCSTOCK",                                      ~
                        varc,     indexed,  recsize =  70,               ~
                        keypos =    1, keylen =  32,                     ~
                        alt key  1, keypos =   8, keylen = 32

            select #3,  "AMTBOMIF",                                      ~
                        varc,     indexed,  recsize =  120,              ~
                        keypos =    1, keylen =  32                      ~


            select #4,  "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =   650,             ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44


            select #5,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #6,  "STORNAME",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos = 1, keylen = 3

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),  0%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),500%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),500%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%),  0%, rslt$(4%))
            call "OPENCHCK" (#5, fs%(5%), f2%(5%),  0%, rslt$(5%))
            call "OPENCHCK" (#6, fs%(6%), f2%(6%),  0%, rslt$(6%))

            mat f1% = zer

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

                                                      /* (EWD001)      */
            users$( 1%) = "MET"
            users$( 2%) = "KDB"
            users$( 3%) = "CAV"
            users$( 4%) = "LS1"
            users$( 5%) = "JKG"
            users$( 6%) = "CEP"

            max_users% = 6%
                                                      /* (EWD001)      */
        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  4%
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10200
L10150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10120
                         if fieldnr% = 1% then L10100
                         goto L10150
L10200:               if keyhit% = 14% then goto inputmode_a
                      if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10120
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10120
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
                  if keyhit%  = 12% then gosub delete_part
                  if keyhit%  = 14% then goto inputmode_a
                  if keyhit%  = 16% then gosub dataput
                  if keyhit% <>  0% then       editpg1
L11140:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 4% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11190:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11190
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11190
                  lastfieldnr% = fieldnr%
            goto L11140

        inputmode_a

            for fieldnr% = 1% to  4%
L11300:         gosub'061(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L11430
L11320:         gosub'102(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L11410
L11350:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'061(fieldnr%)
                         if enabled% = 1% then L11320
                         if fieldnr% = 1% then L11300
                         goto L11350
L11410:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L11320
L11430:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L11320
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   P A G E   T W O                *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg2
            lastfieldnr% = 0%
            gosub'102(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then gosub print_report
                  if keyhit% <>  0% then       editpg2
L11610:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 4% then editpg2
            if fieldnr% = lastfieldnr% then    editpg2
            gosub'061(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg2
L11660:     gosub'102(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11660
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11660
                  lastfieldnr% = fieldnr%
            goto L11610

        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        print_report
            gosub select_printer

            on sel% gosub rpt_1, rpt_2, rpt_3

            gosub close_printer
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

        deffn'061(fieldnr%)
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
         "Enter a Valid BIN Location.                                  ",~
         "Enter a Valid Level within BIN Location.                     ",~
         "Enter a Valid MFG OR Raw Material Part Number.               ",~
         "Enter BIN Description.                                       "

        deffn'060(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28280
                inpmessage$ = edtmessage$
                return

L28280
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn2_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn2_msg  :  data                                               ~
         "Enter a Valid Report Selection ( 1 thru 3).                  ",~
         "Enter a Valid Department Code or 'ALL' (When Applicable).    ",~
         "Enter a Valid Model Code or 'ALL' (When Applicable).         ",~
         "Enter a Valid Store Number or 'ALL'.                         "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, stk_part$, stk_bin$,       ~
                      stk_level$, stk_descr$, stk_key$, stk_filler$,     ~
                      part_desc$, apc_scr$, apc_prt$, apc_sze$,          ~
                      selection$, dept$, model$, sel_desc$, dept_desc$,  ~
                      model_desc$
        return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF ALUMINUM PRODUCTS CORP., INC., RURAL HALL, *~
            * N.C. AND IS CONFIDENTIAL INFORMATION. UNATHORIZED USE,    *~
            * COPYING, DECOMPILING, TRANSLATING, DISCLOSURE, OR TRANSFER*~
            * OF IT IS PROHIBITED. COPYRIGHT (C) 1991, AN UNPUBLISHED   *~
            * WORK BY ALUMINUM PRODUCTS CORP., INC., RURAL HALL, N.C.   *~
            * ALL RIGHTS RESERVED.                                      *~
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
          rec% = 0%
          str(stk_key$,1%,5%)   = stk_bin$
          str(stk_key$,6%,2%)   = stk_level$
          str(stk_key$,8%,25%)  = stk_part$
          read #2,key = stk_key$,using L35040, stk_bin$, stk_level$,      ~
                        stk_part$, stk_bin$, stk_level$, stk_descr$,     ~
                                             stk_filler$, eod goto L30130
          rec% = 1%                                         /* ON FILE */
L30130: return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Update Store Data and Part Data                           *~
            *************************************************************

        dataput
          gosub check_security                         /* (EWD001)     */
        
          call "SHOSTAT" ("Updating ("&stk_part$&")")
          str(stk_key$,1%,5%)   = stk_bin$
          str(stk_key$,6%,2%)   = stk_level$
          str(stk_key$,8%,25%)  = stk_part$
          read #2,hold,key = stk_key$, eod goto L31120
            delete #2
L31120:   write #2, using L35040, stk_bin$, stk_level$, stk_part$,        ~
                            stk_bin$, stk_level$, stk_descr$, stk_filler$

        return clear all
        goto inputmode

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************
                                         /* APCSTOCK - RACK LOCATION   */
L35040:     FMT CH(05),                  /* BIN Location               */~
                CH(02),                  /* Level within BIN           */~
                CH(25),                  /* Part Number                */~
                CH(05),                  /* BIN Location               */~
                CH(02),                  /* Level within BIN           */~
                CH(30),                  /* BIN Description            */~
                CH(01)                   /* Filler Area                */

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
              on fieldnr% gosub L40190,         /* BIN Location      */   ~
                                L40200,         /* Level within BIN  */   ~
                                L40190,         /* Part Number       */   ~
                                L40180          /* BIN Description   */

              goto L40220

L40180:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40190:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40200:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40220:     accept                                                       ~
               at (01,02),                                               ~
                  "BIN Location for Standard Products",                  ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "BIN Location:",                              ~
               at (06,20), fac(lfac$( 1)), stk_bin$             , ch(05),~
               at (07,02), "Level       :",                              ~
               at (07,20), fac(lfac$( 2)), stk_level$           , ch(02),~
               at (08,02), "Part Number :",                              ~
               at (08,20), fac(lfac$( 3)), stk_part$            , ch(25),~
               at (08,47), fac(hex(84)), part_desc$             , ch(32),~
               at (09,02), "Description :",                              ~
               at (09,20), fac(lfac$( 4)), stk_descr$           , ch(32),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L40500
                  call "PRNTSCRN"
                  goto L40220

L40500:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40700     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffff0e0f1000)
            if fieldnr% = 1% then L40660
                str(pf$(1),64%)   = " "  :  str(pfkeys$,14%,1%) = hex(ff)
                str(pf$(3),64%)   = " "  :  str(pfkeys$,16%,1%) = hex(ff)
L40660:     if fieldnr% > 1% then L40680
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40680:     return

L40700: if fieldnr% > 0% then L40810  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                 (12)Delete Part        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01ffffffffffffffffffff0cffff0f1000)
            if rec% = 1% then return
               str(pf$(2),18%,30%) = " " : str(pfkeys$,12%,1%)=hex(ff)
            return
L40810:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return


        REM *************************************************************~
            *               R E P O R T   S E L E C T I O N             *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
              gosub'060(1%, fieldnr%)
              gosub set_pf2
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L42190,         /* REPORT SELECTION  */   ~
                                L42180,         /* DEPARTMENT CODE   */   ~
                                L42180,         /* MODEL             */   ~
                                L42180          /* STORE NUMBER, ALL */
              goto L42210

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L42180:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L42190:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L42210:     accept                                                       ~
               at (01,02),                                               ~
                  "Report Selections for Standard Stock Inventory",      ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Report Selection: ",                         ~
               at (06,25), fac(lfac$( 1)), selection$           , ch(01),~
               at (06,40), fac(hex(84)),   sel_desc$            , ch(30),~
               at (07,02), "Department Code : ",                         ~
               at (07,25), fac(lfac$( 2)), dept$                , ch(03),~
               at (07,40), fac(hex(84)),   dept_desc$           , ch(30),~
               at (08,02), "Model Code      : ",                         ~
               at (08,25), fac(lfac$( 3)), model$               , ch(03),~
               at (08,40), fac(hex(84)),   model_desc$          , ch(30),~
               at (09,02), "Store No. or ALL: ",                         ~
               at (09,25), fac(lfac$( 4)), store$               , ch(03),~
               at (09,40), fac(hex(84)),   store_desc$          , ch(30),~
                                                                         ~
               at (10,20), "****************************************",   ~
               at (11,20), "*          Report Selections           *",   ~
               at (12,20), "*                                      *",   ~
               at (13,20), "* (1) - Standard Stock by BIN Location *",   ~
               at (14,20), "*                                      *",   ~
               at (15,20), "* (2) - Standard Stock by Department   *",   ~
               at (16,20), "*                                      *",   ~
               at (17,20), "* (3) - Standard Stock by Model        *",   ~
               at (18,20), "*                                      *",   ~
               at (19,20), "****************************************",   ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L42600
                  call "PRNTSCRN"
                  goto L42210

L42600:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
        if edit% = 2% then L42790     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L42750
                str(pf$(3),64%)   = " "  :  str(pfkeys$,16%,1%) = hex(ff)
L42750:     if fieldnr% > 1% then L42770
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L42770:     return

L42790: if fieldnr% > 0% then L42880  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Print Report"
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)
            return
L42880:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50150,         /* BIN Location          */ ~
                              L50320,         /* Level within BIN      */ ~
                              L50420,         /* Part Number           */ ~
                              L50620          /* BIN Description       */

            return

L50150: REM BIN Location                          STK_BIN$
           if stk_bin$ <> " " then goto L50200
L50170:       errormsg$ = "Invalid BIN Location"
              stk_bin$ = " "
              return
L50200:    convert stk_bin$ to stk_bin%, data goto L50240

           convert stk_bin% to stk_bin$, pic(00000)
        return
L50240:    x% = len(stk_bin$)
           x$ = str(stk_bin$,x%,1%)
           convert str(stk_bin$,1%,x%-1%) to stk_bin%, data goto L50170

           convert stk_bin% to str(stk_bin$,1%,4%), pic(0000)
           str(stk_bin$,5%,1%) = x$
        return

L50320: REM Level within BIN                      STK_LEVEL$
           if stk_level$ <> " " then goto L50360
L50340:       stk_level$ = "00"

L50360:    convert stk_level$ to stk_level%, data goto L50340

           convert stk_level% to stk_level$, pic(00)

        return

L50420: REM Part Number                           STK_PART$
           if stk_part$ <> " " then goto L50480
              part_desc$ = hex(06) & "Select MFG or Raw Mat'l "
              call "PLOWCODE" (#1, stk_part$, part_desc$,0%,.30,f1%(1))
              part_desc$ = " "
              if f1%(1) = 0 then goto L50570
L50480:    read #1,key = stk_part$, eod goto L50570
           get #1, using L50500, part_desc$
L50500:       FMT POS(26), CH(32)

           gosub dataload
           if rec% = 1% then fieldnr% = 4%

        return
L50570:     errormsg$ = "(Error)-Invalid MFG or Raw Material Part No."
            init(" ") stk_part$, part_desc$

        return

L50620: REM BIN Description                       STK_DESCR$

        return

        deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50740,         /* Report Selection      */ ~
                              L50880,         /* Department Code       */ ~
                              L51050,         /* Model Code            */ ~
                              L51210          /* Store Number          */
            return

L50740: REM Report Selection                      SELECTION$
           sel% = 1%
           if selection$ = " " then selection$ = "1"
           convert selection$ to sel%, data goto L50840

           if sel% < 1% or sel% > 3% then goto L50840
           if sel% = 1% then sel_desc$ = "Report by BIN Location."
           if sel% = 2% then sel_desc$ = "Report by Department.  "
           if sel% = 3% then sel_desc$ = "Report by Model Code.  "
        return
L50840:    errormsg$ = "Invalid Report Selection. ( 1 thru 3 )"
           selection$, sel_desc$ = " "
        return

L50880: REM Department Code                       DEPT$
           if dept$ <> " " then goto L50910
              dept$ = "ALL"
L50910:    if dept$ <> "ALL" then goto L50950
              dept_desc$ = "'ALL' Departments."
              return
        lookup_dept
L50950:     dept_desc$ = " "
            readkey$ = all(hex(00))
            readkey$ = "DEPTCODE " & str(dept$,1%,1%)
            call "DESCRIBE" (#5, readkey$, dept_desc$, 0%, f1%(5%) )
            if f1%(5%) = 0% then goto L51010
        return
L51010:     errormsg$ = "Invalid Department Code. "
            dept$, dept_desc$ = " "
        return

L51050: REM Model Code                            MODEL$
           if model$ <> " " then goto L51080
              model$ = "ALL"
L51080:    if model$ <> "ALL" then goto L51110
              model_desc$ = "'ALL' Models. "
              return
L51110:     model_desc$ = " "
            readkey$ = all(hex(00))
            readkey$ = "MODEL    " & model$
            call "DESCRIBE" (#5, readkey$, model_desc$, 0%, f1%(5%) )
            if f1%(5%) = 0% then goto L51170
        return
L51170:     errormsg$ = "Invalid Model Code. "
            model$, model_desc$ = " "
        return

L51210: REM Store Number                          STORE$
           if store$ <> " " then goto L51240
              store$ = "ALL"
L51240:    if store$ <> "ALL" then goto L51270
              store_desc$ = "'ALL' Stores. "
              return
L51270:     store_desc$ = " "
            readkey$ = store$
            read #6,key = readkey$, using L51310, store_desc$,            ~
                                                 eod goto L51330
L51310:        FMT XX(3), CH(30)
        return
L51330:     errormsg$ = "Invalid Store Number."
            store$, store_desc$ = " "
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

                                                   /* REPORT HEADER */
L55050: %######## ########                   ############################~
        ~################################                        APCSTOCK:

L55080: %USER ID: ###                        ############################~
        ~################################                      PAGE: #####

L55093: %                                                   #############~
        ~#################
                                                   /* COLUMN HEADER   */
L55110: % BIN NO.  LEVEL  <----- BIN DESCRIPTION ------>  <----- PART NUM~
        ~BER ----->  <------ PRINTER DESCRIPTION ------>  <------ SIZE ---~
        ~-->
L55140: % -------  -----  ------------------------------  ---------------~
        ~----------  -----------------------------------  ----------------~
        ~---
                                                   /* DETAIL        */
L55180: %  #####    ##    ##############################  ###############~
        ~##########  ###################################  ################~
        ~###

L55215: %!---------------------------!--------------------------------!--~
        ~-----!----!-----!---------!-----------!-------------------------!

L55240: %+---------------------------------------------------------------~
        ~----------------------------------------------------------------+
L55260: %! PART NUMBER               ! DESCRIPTION                    ! B~
        ~IN   ! LV ! STR ! ON HAND !  ACTUAL   ! COMMENTS                !
L55280: %! ######################### ! ############################## ! #~
        ~#### ! ## ! ### !  ###### ! _________ ! _______________________ !

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

                                                    /* (EWD001)       */
        Check_security
           for ii% = 1% to max_users%
             if userid$ = users$(ii%) then return
           next ii%

           comp% = 2%
           hdr$     = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = "You do not have access to Add/Change/Delete"
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return clear all
        goto inputmode
                                                    /* (EWD001)       */
        select_printer
            if sel% <> 1% then goto L60100
               call "SHOSTAT" ("Creating BIN Location Report")
               print_title$ = "BIN Location for Standard Parts"
               goto L60120

L60100:        call "SHOSTAT" ("Creating Stock Status Report")
               print_title$ = "Manufactured Stock Inventory Report"
L60120:     page_no% = 0%
            lcnt%    = 99%
            date$ = date  :  call "DATEFMT" (date$)
            call "TIME" (rpt_time$)
            call "COMPNAME" (12%, company$, f1%(5))
            call "SETPRNT" ("APCSTK", " ", 0%, 0%)
            call "FMTTITLE" (print_title$, " ", 12%)
            select printer (134)
        return

        print_header
          page_no% = page_no% + 1%
          print page
          print using L55050, date$, rpt_time$, company$
          print using L55080, userid$, print_title$, page_no%
          print
          print using L55110
          print using L55140
          lcnt% = 5%
        return

        print_header_a
          page_no% = page_no% + 1%
          if page_no% > 1% then print using L55240
          print page
          gosub lookup_dept
          call "FMTTITLE" (dept_desc$, " ", 12%)
          print using L55050, date$, rpt_time$, company$
          print using L55080, userid$, print_title$, page_no%
          print using L55093, dept_desc$
          print
          print using L55240
          print using L55260
          lcnt% = 6%
        return

        print_detail
          if lcnt% > 60% then gosub print_header
           print using L55180, stk_bin$, stk_level$, stk_descr$,          ~
                              stk_part$, apc_prt$, apc_sze$
          lcnt% = lcnt% + 1%
        return

        print_detail_a
          if lcnt% > 60% then gosub print_header_a
           print using L55215
           print using L55280, stk_part$, stk_descr$, stk_bin$,stk_level$,~
                              store$(i%), on_hand$(i%)
          lcnt% = lcnt% + 2%
        return

        close_printer
            call "SETPRNT" ("APCSTK", " ", 0%, 1%)
        return

        delete_part
          gosub check_security                        /* (EWD001)     */

          call "SHOSTAT" ("Deleting ("&stk_part$&")")
          stk_key$ = all(hex(00))
          str(stk_key$,1%,5%)  = stk_bin$
          str(stk_key$,6%,2%)  = stk_level$
          str(stk_key$,8%,25%) = stk_part$
          read #2,hold,key = stk_key$, eod goto L60750
             delete #2
L60750:   rec% = 0%
          return clear all
          goto inputmode

        rpt_1
            stk_key$ = all(hex(00))
            err% = 0%
        rpt_1_next
            read #2,key > stk_key$,using L35040,stk_bin$, stk_level$,     ~
               stk_part$, stk_bin$, stk_level$,stk_descr$,               ~
                                                      eod goto rpt_1_done
            str(stk_key$,1%,5%)   = stk_bin$
            str(stk_key$,6%,2%)   = stk_level$
            str(stk_key$,8%,25%)  = stk_part$
            call "APCDESCR" (stk_part$, apc_scr$, apc_prt$, apc_sze$, #3,~
                                                           err% )
            gosub print_detail
            goto rpt_1_next
        rpt_1_done
        return

        rpt_2
        rpt_3
            sav_dept$ = dept$
            dept$ = " "
            stk_key$ = all(hex(00))
            if sav_dept$ = "ALL" then goto L61030
               str(stk_key$,1%,1%) = sav_dept$
L61030:     if model$ = "ALL" then goto L61060
               str(stk_key$,1%,3%) = model$

L61060: rpt_2_next
            read #2,key 1% > stk_key$,using L35040,stk_bin$, stk_level$,  ~
               stk_part$, stk_bin$, stk_level$,stk_descr$,               ~
                                                      eod goto rpt_2_done
            str(stk_key$,1%,25%)  = stk_part$
            str(stk_key$,26%,5%)  = stk_bin$
            str(stk_key$,31%,2%)  = stk_level$
            if sav_dept$ = "ALL" then goto L61150
               if sav_dept$ <> str(stk_part$,1%,1%) then goto rpt_2_done
L61150:     if model$ = "ALL" then goto L61170
               if model$ <> str(stk_part$,1%,3%) then goto rpt_2_done
L61170:     if dept$ <> " " then goto L61190
               goto L61200
L61190:     if str(dept$,1%,1%) = str(stk_part$,1%,1%) then goto L61230
L61200:        str(dept$,1%,1%) = str(stk_part$,1%,1%)
               gosub print_header_a

L61230:     gosub get_hnyquan_data
            for i% = 1% to inc%
                if store$(i%) = " " then goto L61290
                if store$ = "ALL" then goto L61280
                   if store$ <> store$(i%) then goto L61290
L61280:         gosub print_detail_a
L61290:     next i%
            goto rpt_2_next
        rpt_2_done
            print using L55240
        return

        get_hnyquan_data
          init(" ") store$(), on_hand$()
          readkey$ = all(hex(00)) : inc% = 1%
          on_hand$(1) = "    0 " : store$(1) = "300"
          str(readkey$,1%,25%) = stk_part$
L61400:   read #4,hold,key > readkey$, eod goto L61510
            get #4, using L61420, readkey$, on_hand
L61420:        FMT POS(17), CH(44), POS(69), PD(14,4)
          if str(readkey$,26%,3%) <> "000" then goto L61460
             delete #4
             goto L61400
L61460:   if str(readkey$,1%,25%) <> stk_part$ then goto L61510
             store$(inc%) = str(readkey$,26%,3%)
             convert on_hand to on_hand$(inc%), pic(#####-)
             inc% = inc% + 1%
             goto L61400
L61510: return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
