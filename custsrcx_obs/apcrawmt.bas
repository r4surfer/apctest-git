        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCRAWMT                             *~
            *  Creation Date     - 07/30/97                             *~
            *  Last Modified Date- 11/06/97                             *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - This utility is used to Set-Up       *~
            *                      BIN and Level Location information   *~
            *                      for Raw Materials and Print Rack     *~
            *                      Labels.                              *~
            *                                                           *~
            *  Code Tables Used  - (MODEL    ) - Model Codes            *~
            *                                                           *~
            *  Special Comments  - All Tables Excep (MODEL) can have    *~
            *                      Alphnumeric values.                  *~
            *                                                           *~
            *                      Rack Bin Location = (4) 0000 - 9999  *~
            *                      Rack Level Loc.   = (1) A - Z        *~
            *                      Raw Material Part = (15)             *~
            *                      Raw Material Desc = (30)             *~
            *                      Raw Material Qty  = (8)              *~
            *                      Filler            = (7)              *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/30/97 ! New Program for (APC) - Last Mod Date    ! RHH *~
            *          !                                          !     *~
            * 11/06/97 ! Revision Update For 60403                ! DJD *~
            * 05/31/02 ! (EWD001) Mod for Name Change.            ! TLM *~
            *          !                                          !     *~
            *************************************************************

        dim                                                              ~
            raw_key$20,                  /* Stock Key                  */~
            stk_part$25, raw_part$15,    /* Part Number                */~
            raw_bin$4,                   /* BIN Location               */~
            raw_level$1,                 /* Level within BIN           */~
            raw_descr$30,                /* BIN Description            */~
            raw_filler$7,                /* Filler area                */~
            beg_bin$4, end_bin$4,        /* Beg/End BIN Location       */~
            beg_level$1, end_level$1,    /* Beg/End Level Location     */~
            part_desc$32,                /* Part Description           */~
            company$30,                  /* For Report Company Name    */~
            print_title$32,              /* For Report Title           */~
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
            dim apc$40, pname$21
            apc$   = " Receiving Raw Material's Bin Locations "
            pname$ = "APCRAWMT - Rev: 06.04"

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
            * #2  ! APCRAWMT ! Rack Location for Raw Materials          *~
            * #3  ! AMTBOMIF ! BOM Generator Master Validity File       *~
            * #4  ! HNYQUAN  ! Part Quantity File                       *~
            * #5  ! GENCODES ! MASTER TABLES                            *~
            * #6  ! GENCODES ! Store Name Master                        *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  3, keypos =   26, keylen =  32, dup

            select #2,  "APCRAWMT",                                      ~
                        varc,     indexed,  recsize =  64,               ~
                        keypos =    1, keylen =  20,                     ~
                        alt key  1, keypos =   6, keylen = 15, dup

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
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),  0%, rslt$(3%))
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

            for fieldnr% = 1% to  2%
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
            if fieldnr% < 1% or fieldnr% > 2% then editpg2
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
            gosub rpt_1
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
         "Enter a Valid BIN Location (0000 thru 9999)?                 ",~
         "Enter a Valid Level within BIN Location (A thru Z)?          ",~
         "Enter a Valid Raw Material Part Number (Max Size = 15)?      ",~
         "Enter BIN Description for Raw Material?                      "

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
         "Enter a Valid Beginning/Ending BIN LocatioN, or ALL=All Bins?",~
         "Enter a Valid Beginning/Ending Level Location, or 0 = ALL?   "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, stk_part$, raw_bin$,       ~
                      raw_level$, raw_descr$, raw_key$, raw_filler$,     ~
                      part_desc$, raw_part$

        return

        REM *************************************************************~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
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
          str(raw_key$,1%,4%)   = raw_bin$
          str(raw_key$,5%,1%)   = raw_level$
          str(raw_key$,6%,15%)  = raw_part$
          read #2,key = raw_key$,using L35040, raw_bin$, raw_level$,      ~
                        raw_part$, raw_descr$, raw_qty, eod goto L30130
          rec% = 1%                                         /* ON FILE */
L30130: return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Update Store Data and Part Data                           *~
            *************************************************************

        delete_part
        dataput
          str(raw_key$,1%,4%)   = raw_bin$
          str(raw_key$,5%,1%)   = raw_level$
          str(raw_key$,6%,15%)  = raw_part$
          read #2,hold,key = raw_key$, eod goto L31150
            delete #2
            if keyhit% = 12% then goto L31180

L31150:   write #2, using L35040, raw_bin$, raw_level$, raw_part$,        ~
                                 raw_descr$, raw_qty, raw_filler$

L31180: return clear all
        goto inputmode

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************
                                         /* APCRAWMT - RACK LOCATION   */
L35040:     FMT CH(04),                  /* BIN Location               */~
                CH(01),                  /* Level within BIN           */~
                CH(15),                  /* Part Number                */~
                CH(30),                  /* BIN Description            */~
                PD(14,4),                /* Raw Material Quantity      */~
                CH(06)                   /* Filler Area                */

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
              on fieldnr% gosub L40200,         /* BIN Location      */   ~
                                L40190,         /* Level within BIN  */   ~
                                L40190,         /* Part Number       */   ~
                                L40180          /* BIN Description   */

              goto L40220

L40180:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40190:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40200:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40220:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,21), fac(hex(a4)), apc$                   , ch(40),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "BIN Location:",                              ~
               at (06,20), fac(lfac$(1%)), raw_bin$             , ch(04),~
               at (07,02), "Level       :",                              ~
               at (07,20), fac(lfac$(2%)), raw_level$           , ch(01),~
               at (08,02), "Part Number :",                              ~
               at (08,20), fac(lfac$(3%)), raw_part$            , ch(15),~
               at (08,47), fac(hex(84)), part_desc$             , ch(32),~
               at (09,02), "Description :",                              ~
               at (09,20), fac(lfac$(4%)), raw_descr$           , ch(32),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then goto L40500
                  call "PRNTSCRN"
                  goto L40220

L40500:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40700     /*  Input Mode             */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2%)= "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffff0e0f1000)
            if fieldnr% = 1% then L40660
                str(pf$(1%),64%)  = " "  :  str(pfkeys$,14%,1%) = hex(ff)
                str(pf$(3%),64%)  = " "  :  str(pfkeys$,16%,1%) = hex(ff)
L40660:     if fieldnr% > 1% then L40680
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40680:     return

L40700: if fieldnr% > 0% then L40810  /*  Edit Mode - Select Fld */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                 (12)Delete Location    " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01ffffffffffffffffffff0cffff0f1000)
            if rec% = 1% then return
               str(pf$(2%),18%,30%) = " " : str(pfkeys$,12%,1%)=hex(ff)
            return
L40810:                              /*  Edit Mode - Enabled    */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                                        " &        ~
                     "                                       "
            pf$(3%)= "                                        " &        ~
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
              on fieldnr% gosub L42160,         /* Begin/End BIN Loc    */~
                                L42160          /* Begin/End Level Locat*/
              goto L42190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L42160:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L42190:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,21), fac(hex(a4)), apc$                   , ch(40),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Beg/End BIN Location  :",                    ~
               at (06,30), fac(lfac$(1%)), beg_bin$             , ch(04),~
               at (06,40), fac(lfac$(1%)), end_bin$             , ch(04),~
                                                                         ~
               at (07,02), "Beg/End Level Location:",                    ~
               at (07,30), fac(lfac$(2%)), beg_level$           , ch(01),~
               at (07,40), fac(lfac$(2%)), beg_level$           , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L42450
                  call "PRNTSCRN"
                  goto L42190

L42450:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
        if edit% = 2% then L42640     /*  Input Mode             */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L42600
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L42600:     if fieldnr% > 1% then L42620
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L42620:     return

L42640: if fieldnr% > 0% then L42730  /*  Edit Mode - Select Fld */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Print Report"
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)
            return
L42730:                              /*  Edit Mode - Enabled    */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                                        " &        ~
                     "                                       "
            pf$(3%)= "                                        " &        ~
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
                              L50280,         /* Level within BIN      */ ~
                              L50390,         /* Part Number           */ ~
                              L50600          /* BIN Description       */

            return

L50150: REM BIN Location                          RAW_BIN$
           if raw_bin$ <> " " then goto L50200
              errormsg$ = "Invalid BIN Location"
              raw_bin$ = " "
              return
L50200:    convert raw_bin$ to raw_bin%, data goto L50240

           convert raw_bin% to raw_bin$, pic(0000)
        return
L50240:    errormsg$ = "(Error) Invalid Bin Location?"
           init(" ") raw_bin$
        return

L50280: REM Level within BIN                      RAW_LEVEL$
           if raw_level$ <> " " then goto L50320
              raw_level$ = "A"

L50320:    pp% = pos("ABCDEFGHIJKLMNOPQRSTUVWXYZ" = raw_level$)
           if pp% = 0% then goto L50350
        return
L50350:    errormsg$ = "(Error) - Invalid Level Location?"
           init(" ") raw_level$
        return

L50390: REM Part Number                           RAW_PART$
           init(" ") stk_part$, part_desc$
           stk_part$ = raw_part$
           if stk_part$ <> " " then goto L50470
              part_desc$ = hex(06) & "Select Manufactured Part"
              call "PLOWCODE" (#1, stk_part$, part_desc$,0%,.30,f1%(1))
              part_desc$ = " "
              if f1%(1) = 0 then goto L50560
L50470:    read #1,key = stk_part$, using L50490,part_desc$,eod goto L50560
L50490:       FMT POS(26), CH(32)
           if len(stk_part$) > 18% then goto L50560
              raw_part$ = str(stk_part$,1%,15%)
           gosub dataload
           if rec% = 1% then fieldnr% = 4%

        return
L50560:     errormsg$ = "(Error) - Invalid Raw Material Part No.?"
            init(" ") raw_part$, stk_part$, part_desc$
        return

L50600: REM BIN Description                       RAW_DESCR$

        return

        deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50700,         /* Beg/End Bin Location  */ ~
                              L50860          /* Beg/End Level         */
            return

L50700: REM Beg/End Bin Location             BEG_BIN$, END_BIN$
           if beg_bin$ <> " " then goto L50750
L50720:       beg_bin$ = "ALL "
              end_bin$ = "ALL "
              return
L50750:    if str(beg_bin$,1%,3%) = "ALL" then goto L50720
              convert beg_bin$ to beg_bin%, data goto L50820

              convert end_bin$ to end_bin%, data goto L50820

              if beg_bin% > end_bin% then goto L50820
        return
L50820:    errormsg$ = "(Error)-Invalid BIN Location Specified?"
           init(" ") beg_bin$, end_bin$
        return

L50860: REM Beginning/Ending Level                BEG_LEVEL$,END_LEVEL$
           if beg_level$ <> " " then goto L50910
L50880:       beg_level$ = "0"
              end_level$ = "0"
              return
L50910:    if str(beg_level$,1%,1%) = "0" then goto L50880
              pp% = pos("ABCDEFGHIJKLMNOPQURSTUVWXYZ" = beg_level$)
              if pp% = 0% then goto L51000
              p% = pp%
              pp% = pos("ABCDEFGHIJKLMNOPQURSTUVWXYZ" = end_level$)
              if pp% = 0% then goto L51000

              if p% > pp% then goto L51000
        return
L51000:    errormsg$ = "(Error)-Invalid Level Location Specified?"
           init(" ") beg_level$, end_level$
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

                                                   /* REPORT HEADER */
L55050: %!########@########                            ##################~
        ~############                                           APCRAWMT !

L55080: %!User Id: ###                                ###################~
        ~#############                                       Page: ####  !

L55110: %!BIN No.!Level!<----- BIN Description ------>!<-Raw Material->!<~
        ~------ Printer Description ----> !<------ Comments ------------>!

L55140: %!-------!-----!------------------------------!----------------!-~
        ~---------------------------------!------------------------------!

                                                   /* DETAIL        */
L55180: %! ####  !  #  !##############################! ###############! ~
        ~################################ !                              !


L55215: %!---------------------------------------------------------------~
        ~----------------------------------------------------------------!

L55240: %+---------------------------------------------------------------~
        ~----------------------------------------------------------------+

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        select_printer
            init(" ") company$, print_title$, date$, rpt_time$
            call "SHOSTAT" ("Creating BIN Location Report")
            call "COMPNAME" (12%, company$, ret%)            /* (EWD001) */
REM            company$     = "   Ellisons Window and Doors  "
            print_title$ = "BIN Locations for Raw Material's"
            page_no% = 0%
            lcnt%    = 99%
            date$ = date  :  call "DATEFMT" (date$)
            call "TIME" (rpt_time$)
            call "SETPRNT" ("APCSTK", " ", 0%, 0%)
            select printer (134)
        return

        print_header
          if lcnt% <> 99% then print using L55240
          page_no% = page_no% + 1%
          print page
          print using L55240
          print using L55050, date$, rpt_time$, company$
          print using L55080, userid$, print_title$, page_no%
          print using L55215
          print using L55110
          lcnt% = 5%
        return

        print_detail
          if lcnt% > 57% then gosub print_header
          print using L55140
          print using L55180, raw_bin$, raw_level$, raw_descr$, raw_part$,~
                             part_desc$
          lcnt% = lcnt% + 2%
        return

        close_printer
            call "SETPRNT" ("APCSTK", " ", 0%, 1%)
        return

        rpt_1
            init(" ") raw_key$
            if str(beg_bin$,1%,3%) = "ALL" then goto rpt_1_next
               str(raw_key$,1%,4%) = beg_bin$
               if beg_level$ = "0" then goto rpt_1_next
                  str(raw_key$,5%,1%) = beg_level$

        rpt_1_next
            read #2,key > raw_key$,using L35040,raw_bin$, raw_level$,     ~
               raw_part$, raw_descr$, raw_qty, raw_filler$,              ~
                                                      eod goto rpt_1_done
            if str(beg_bin$,1%,3%) = "ALL" then goto L60540
               if raw_bin$ > end_bin$ then goto rpt_1_done
L60540:     if beg_level$ = "0" then goto L60570
               if raw_level$ > end_level$ then goto rpt_1_next

L60570:     str(raw_key$,1%,4%)   = raw_bin$
            str(raw_key$,5%,1%)   = raw_level$
            str(raw_key$,6%,15%)  = raw_part$
            read #1,key = raw_part$, using L60620, part_desc$,            ~
                                                  eod goto rpt_1_next
L60620:       FMT POS(26), CH(32)
            gosub print_detail
            goto rpt_1_next
        rpt_1_done
            print using L55240
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
