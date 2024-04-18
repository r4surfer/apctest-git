        REM *************************************************************~
            *                                                           *~
            *   AAA   PPPP    CCC    CCC    SSSS  TTTTT  H   H  PPPP    *~
            *  A   A  P   P  C   C  C   C  S        T    H   H  P   P   *~
            *  AAAAA  PPPP   C      C        S      T    HHHHH  PPPP    *~
            *  A   A  P      C   C  C   C      S    T    H   H  P       *~
            *  A   A  P       CCC    CCC   SSSS     T    H   H  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * APCCSTHP - Utility Program for Entering in the Hardware   *~
            *            and Packaging Costs for a Product              *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/01/93 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 01/30/95 ! Mod - Use the Inventory Master to Calc.  ! RHH *~
            *          !   Unit Cost. New Routine to Auto Update. !     *~
            * 09/15/95 ! Mod Switch to New Cost Routine (APCCST9b)! RHH *~
            *          !   Add Cost Types 3 and 4 for use by      !     *~
            *          !   New Costmate Utility.                  !     *~
            * 11/12/97 ! Mod - for Upgrade to Release R6.04.03    ! RHH *~
            * 06/05/98 ! (EWD001) Fix Color Entry Problem         ! RHH *~
            *          !                                          !     *~
            *************************************************************

        dim                                                              ~
            raw_unit$20,                 /* HNYMASTR Conversion Value  */~
            rpt_type$1, rpt_type_d$30,   /* REPORT TYPE SELECTION      */~
            beg_model$3,  beg_model_d$30,/* REPORT MODEL SELECTION     */~
            end_model$3,  end_model_d$30,/* REPORT MODEL SELECTION     */~
            cst_key$20,                  /* Primary Key                */~
            cst_type$1,                  /* (0)=Hardware,(1)=Packaging */~
            cst_type_d$10,               /*                            */~
            cst_model$4,                 /* APC Model Code             */~
            cst_model_d$25, desc$30,     /* Model Description          */~
            cst_cl$1,                    /* APC Model Color            */~
            cst_cl_d$6,                  /* Color Description          */~
            cst_code$1,                  /* COST TYPE CODE             */~
            cst_code_d$10,               /* COST TYPE CODE DESC.       */~
            cst_raw$25,                  /* Raw Material Part No./Id.  */~
            cst_desc$25,                 /* Description                */~
            cst_qty$10,                  /* UNIT QUANTITY              */~
            cst_cost$10, raw_cost$10,    /* Cost                       */~
            cst_extnd$10, raw_desc$32,   /* Extended Cost              */~
            cst_unit$1, new_cost$10,     /* Cost Unit Value            */~
            cst_unit_d$30,               /*                            */~
            cst_stat$1, cst_stat_d$3,    /* Status Code of Cost        */~
            cst_fil$1,                   /* Filler Area                */~
            units$(10%)30,               /*                            */~
            date$8,                      /* SCREEN DATE                */~
            title$40,                    /* REPORT TITLE               */~
            runtime$8,                   /* REPORT RUN TIME            */~
            readkey$50,                  /* Generic Key                */~
            cursor%(2%),                 /* Cursor location for edit   */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim f2%(5%),                     /* = 0 if the file is open    */~
            f1%(5%),                     /* = 1 if READ was successful */~
            fs%(5%),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5%)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(New) Utility for Hardware/Packaging    "
            pname$ = "APCCSTHP - Rev: R6.04"

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
            * #1  ! APCCSTHP ! Hardware and Packaging Components        *~
            * #2  ! GENCODES ! Master Code Table File                   *~
            * #3  ! HNYMASTR ! Inventory Master File                    *~
            * #4  ! HNYQUAN  ! MASTER QUANTITIES FILE                   *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,   "APCCSTHP",                                     ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    1, keylen =   20

            select #2,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #3,  "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =  1,   keylen =  25,                     ~
                        alt key  1, keypos  =   102, keylen =  9, dup,   ~
                            key  2, keypos  =    90, keylen =  4, dup,   ~
                            key  3, keypos  =    26, keylen = 32, dup

            select #4,  "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44          ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1), f2%(1),500%, rslt$(1))
            call "OPENCHCK" (#2, fs%(2), f2%(2),  0%, rslt$(2))
            call "OPENCHCK" (#3, fs%(3), f2%(3),  0%, rslt$(3))
            call "OPENCHCK" (#4, fs%(4), f2%(4),  0%, rslt$(4))

            f1%(1), f1%(2), f1%(3), f1%(4), f1%(5) = 0%

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            call "EXTRACT" addr("ID", userid$)
            date$ = date : u3% = 0%
            call "DATEFMT" (date$)

            gosub load_units

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 8%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  = 14% then goto inputmode_a
                      if keyhit% <>  4% then       L10215
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10215:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% =  9% then gosub rebuild_costs
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
                  if keyhit%  =  9% then gosub rebuild_costs
                  if keyhit%  = 12% then gosub delete_it
                  if keyhit%  = 14% then goto inputmode_a
                  if keyhit%  = 16% then gosub dataput
                  if keyhit% <>  0% then       editpg1
L11140:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 8% then editpg1
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

        REM *************************************************************~
            *       I N P U T   M O D E   F O R   R E P O R T           *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode_a
            gosub initialize_variables

            for fieldnr% = 1% to 3%
L12100:         gosub'052(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L12220
L12120:         gosub'102(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L12200
L12150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'052(fieldnr%)
                         if enabled% = 1% then L12120
                         if fieldnr% = 1% then L12100
                         goto L12150
L12200:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L12120
L12220:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L12120
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   F O R   R E P O R T            *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg2
            lastfieldnr% = 0%
            gosub'102(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 14% then gosub print_report
                  if keyhit%  = 16% then gosub exit_program
                  if keyhit% <>  0% then       editpg2
L12390:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 3% then editpg2
            if fieldnr% = lastfieldnr% then    editpg2
            gosub'052(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg2
L12440:     gosub'102(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L12440
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L12440
                  lastfieldnr% = fieldnr%
            goto L12390

        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        print_report
            gosub select_printer
            gosub generate_report
            close printer
        return clear all
        goto inputmode

        select_printer
            call "SHOSTAT" ("Printing Report")
            runtime$ = " "
            call "TIME" (runtime$)
            select printer (134)
            pageno% = 0%
            lcntr% = 99%
            title$ = " Costing Hardware and Packaging Report  "
            rpt% = 1%
        return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
        return

        deffn'052(fieldnr%)
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
         "Enter a Valid Type? (0) = Hardware, (1) = Packaging          ",~
         "Enter a Valid Model Code and Color Code?                     ",~
         "Enter a Cost Type Code, 0=Std, 1=TSO, 2=BSO, 3 or 4=Costmate?",~
         "Enter a Valid Raw Material Part No. or (A) with Identifier?  ",~
         "Enter a Valid Applicabel Description?                        ",~
         "Enter a Valid Cost Unit Value ( 1 thru 9 )?                  ",~
         "Enter a Valid Cost for Part Specified?                       ",~
         "Enter a Valid Quantity Associated with Raw Material?         "

        deffn'060(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28310
                inpmessage$ = edtmessage$
                return

L28310
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn2_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn2_msg  :  data                                               ~
         "Enter a Valid Cost Type Code (0) = Hardware, (1) = Packaging?",~
         "Enter a Valid Beginning Model Code or (ALL)?                 ",~
         "Enter a Valid Ending Model Code?                             "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, cst_key$, cst_model$,      ~
                      cst_model_d$, cst_raw$, cst_desc$, cst_cost$,      ~
                      cst_unit$, cst_unit_d$, cst_fil$, cst_type_d$,     ~
                      rpt_type$, rpt_type_d$, beg_model$, beg_model_d$,  ~
                      end_model$, end_model_d$, cst_type$, cst_qty$,     ~
                      cst_extnd$, cst_code$, cst_code_d$, raw_unit$,     ~
                      cst_cl$, cst_cl_d$, cst_stat$, cst_stat_d$,        ~
                      raw_cost$, raw_desc$, new_cost$
            rpt% = 0%
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
            if rpt% = 1% then goto L30150
               cst_key$ = all(hex(00))
               str(cst_key$,1%,1%)  = cst_type$
               str(cst_key$,2%,4%)  = cst_model$
               str(cst_key$,6%,1%)  = cst_code$
               str(cst_key$,7%,14%) = str(cst_raw$,1%,14%)
            read #1,key = cst_key$, eod goto L30500

L30150:     get #1, using L35040, cst_type$, cst_model$, cst_code$,       ~
                                 cst_raw$, cst_desc$, cst_unit$,         ~
                                 cst_cost, cst_qty, cst_stat$, cst_fil$

            init(" ") readkey$, cst_type_d$, cst_model_d$, cst_unit_d$,  ~
                      cst_code_d$, cst_cl$, cst_cl_d$
            str(readkey$,1%,9%)   = "MODEL    "
            str(readkey$,10%,15%) = str(cst_model$,1%,3%)
            read #2,key = readkey$, using L30250, desc$, eod goto L30260
L30250:         FMT POS(25), CH(30)
            cst_model_d$ = str(desc$,1%,25%)
L30260:     cst_cl$ = str(cst_model$,4%,1%)
            str(readkey$,1%,9%)   = "COLOR    "
            str(readkey$,10%,15%) = cst_cl$
            read #2,key = readkey$, using L30305, cst_cl_d$,              ~
                                                           eod goto L30306
L30305:         FMT POS(30), CH(6)
L30306:     str(cst_model_d$,19%,7%) = " " & cst_cl_d$
            if cst_type$ = "0" then cst_type_d$ = "HARDWARE "
            if cst_type$ = "1" then cst_type_d$ = "PACKAGING"

            if cst_code$ = "0" then cst_code_d$ = "(ALL)    "
            if cst_code$ = "1" then cst_code_d$ = "TSO-ONLY "
            if cst_code$ = "2" then cst_type_d$ = "BSO-ONLY "
            if cst_code$ = "3" then cst_type_d$ = "ETC.     "
            cst_unit% = 1%
            convert cst_unit$ to cst_unit%, data goto L30400
L30400:
            cst_unit_d$ = units$(cst_unit%)
            convert cst_cost to cst_cost$, pic(###.#####-)

            convert cst_qty to cst_qty$, pic(#####.###-)

            x = round(cst_qty * cst_cost, 5)
            convert x to cst_extnd$, pic(###.#####-)

            rec% = 1%
            gosub calc_material_cost
L30500: return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        delete_it
            call "SHOSTAT" ("Deleting Reference")
        dataput
            if cst_stat% < 3% then goto L31100
               goto L31120
L31100:     cst_cost = new_cost

L31120:     rec% = 0%
            cst_key$ = all(hex(00))
            str(cst_key$,1%,1%)  = cst_type$
            str(cst_key$,2%,4%)  = cst_model$
            str(cst_key$,6%,1%)  = cst_code$
            str(cst_key$,7%,14%) = str(cst_raw$,1%,14%)
            read #1,hold,key = cst_key$, eod goto L31220
               delete #1
               if keyhit% = 12% then goto L31270

L31220:     put #1, using L35040, cst_type$, cst_model$, cst_code$,       ~
                                 cst_raw$, cst_desc$, cst_unit$,         ~
                                 cst_cost, cst_qty, cst_stat$, cst_stat$,~
                                 cst_fil$
            write #1
L31270: return clear all
        goto inputmode

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************
                                         /* File = (APCCSTHP)          */
L35040: FMT CH(01),                      /* Type of Entry (0) or (1)   */~
            CH(04),                      /* Product Model Code         */~
            CH(01),                      /* Costing Type Code          */~
            CH(14),                      /* Assoc. Raw Mat. Part No.   */~
            CH(25),                      /* Part No. Desc              */~
            CH(01),                      /* Cost Unit Value (0 to 8)   */~
            PD(14,4),                    /* Cost                       */~
            PD(14,4),                    /* Type Unit Quantity         */~
            CH(01),                      /* Status Code                */~
            CH(01)                       /* Filler Area                */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub set_pf1

              gosub'050(1%, fieldnr%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40240,         /* Type of Entry        */~
                                L40230,         /* Product Model Code   */~
                                L40230,         /* Type Code            */~
                                L40240,         /* Part Raw Material No.*/~
                                L40220,         /* Part Desc            */~
                                L40240,         /* Cost Unit Value      */~
                                L40240,         /* Part Cost            */~
                                L40240          /* Material Quantity    */
              goto L40260

L40220:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40230:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40240:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40260:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "Type of Entry (0),(1):",                     ~
               at (03,30), fac(lfac$( 1)), cst_type$            , ch(01),~
               at (03,40), fac(hex(84)), cst_type_d$            , ch(10),~
                                                                         ~
               at (04,02), "Product Model Code   :",                     ~
               at (04,30), fac(lfac$( 2)), cst_model$           , ch(04),~
               at (04,40), fac(hex(84)), cst_model_d$           , ch(25),~
                                                                         ~
               at (05,02), "Product Type Code    :",                     ~
               at (05,30), fac(lfac$( 3)), cst_code$            , ch(01),~
               at (05,40), fac(hex(84)), cst_code_d$            , ch(10),~
                                                                         ~
               at (06,02), "Raw Mat. or Unq. Id  :",                     ~
               at (06,30), fac(lfac$( 4)), cst_raw$             , ch(14),~
                                                                         ~
               at (07,02), "Part Description     :",                     ~
               at (07,30), fac(lfac$( 5)), cst_desc$            , ch(25),~
                                                                         ~
               at (08,02), "Cost Unit Value (1-9):",                     ~
               at (08,30), fac(lfac$( 6)), cst_unit$            , ch(01),~
               at (08,40), fac(hex(84)), cst_unit_d$            , ch(30),~
                                                                         ~
               at (09,02), "Cost Assoc. with Part:",                     ~
               at (09,30), fac(lfac$( 7)), cst_cost$            , ch(10),~
                                                                         ~
               at (10,02), "Quantity of Raw Mat. :",                     ~
               at (10,30), fac(lfac$( 8)), cst_qty$             , ch(10),~
                                                                         ~
               at (10,50), "Extended Cost: ",                            ~
               at (10,65), fac(hex(84)), cst_extnd$             , ch(10),~
                                                                         ~
               at (12,02), "Calculated Cost Inv.  :",                    ~
               at (12,30), fac(hex(84)), new_cost$              , ch(10),~
                                                                         ~
               at (12,50), "Status:",                                    ~
               at (12,60), fac(hex(84)), cst_stat_d$            , ch(03),~
                                                                         ~
               at (15,02), fac(hex(84)), units$(1)              , ch(30),~
               at (16,02), fac(hex(84)), units$(2)              , ch(30),~
               at (17,02), fac(hex(84)), units$(3)              , ch(30),~
               at (18,02), fac(hex(84)), units$(4)              , ch(30),~
               at (19,02), fac(hex(84)), units$(5)              , ch(30),~
                                                                         ~
               at (15,40), fac(hex(84)), units$(6)              , ch(30),~
               at (16,40), fac(hex(84)), units$(7)              , ch(30),~
               at (17,40), fac(hex(84)), units$(8)              , ch(30),~
               at (18,40), fac(hex(84)), units$(9)              , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L40920
                  call "PRNTSCRN"
                  goto L40260

L40920:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
            rpt% = 0%
        if edit% = 2% then L41160     /*  Input Mode             */
            pf$(1) = "(1)Start Over    (4)Previous Field      " &        ~
                     "                       (14)print report"
            pf$(2) = "                 (9)Re_build Costs      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (12)Delete             " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffff09ffff0cff0e0f1000)
            if fieldnr% = 1% then L41100
                str(pf$(1),64)    = " "  :  str(pfkeys$,14,1) = hex(ff)
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 9,1) = hex(ff)
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L41100:     if fieldnr% > 1% then L41120
                str(pf$(1),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L41120:     if rec% = 1% then goto L41140
                str(pf$(3),18,26) = " "  :  str(pfkeys$,12,1) = hex(ff)
L41140:     return

L41160: if fieldnr% > 0% then L41280  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (12)Delete             " &        ~
                     "                       (16)Update Data "
            pfkeys$ = hex(01ffffffffffffffffffff0cffff0f1000)
            if rec% = 1% then goto L41260
                str(pf$(3),18,26) = " "  :  str(pfkeys$,12,1) = hex(ff)
L41260:     return

L41280:
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

        REM *************************************************************~
            *               R E P O R T   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
              gosub set_pf2

              gosub'060(1%, fieldnr%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L41560,         /* COST TYPE CODE       */~
                                L41560,         /* BEG MODEL CODE       */~
                                L41560          /* END MODEL CODE       */
              goto L41590

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L41560:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L41590:     accept                                                       ~
               at (01,02),                                               ~
                  "Costing Hardware and Packaging Report",               ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "Cost Type Code (0),(1):",                    ~
               at (03,30), fac(lfac$( 1)), rpt_type$            , ch(01),~
               at (03,40), fac(hex(84)), rpt_type_d$            , ch(30),~
                                                                         ~
               at (04,02), "Beginning Model Code :",                     ~
               at (04,30), fac(lfac$( 2)), beg_model$           , ch(03),~
               at (04,40), fac(hex(84)), beg_model_d$           , ch(30),~
                                                                         ~
               at (05,02), "Ending Model Code    :",                     ~
               at (05,30), fac(lfac$( 3)), end_model$           , ch(03),~
               at (05,40), fac(hex(84)), end_model_d$           , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L41890
                  call "PRNTSCRN"
                  goto L41590

L41890:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
        if edit% = 2% then L42080     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L42040
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L42040:     if fieldnr% > 1% then L42060
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L42060:     return

L42080: if fieldnr% > 0% then L42180  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return

L42180:
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
            on fieldnr% gosub L50180,         /* Type of Entry (0),(1) */ ~
                              L50340,         /* Assoc. Model Code     */ ~
                              L50530,         /* Costing Type Code     */ ~
                              L50670,         /* Inv. Raw Material No. */ ~
                              L50940,         /* Raw Mat. Description  */ ~
                              L50970,         /* Cost Unit Value       */ ~
                              L51110,         /* Cost Value Assoc.     */ ~
                              L51220          /* Raw Material Quantity */
            return

L50180: REM Type of Entry                         CST_TYPE$
            if cst_type$ <> " " then goto L50220
               cst_type$ = "0"

L50220:     cst_type% = 0%
            convert cst_type$ to cst_type%, data goto L50240
L50240:
            if cst_type% <> 0% and cst_type% <> 1% then goto L50300

            if cst_type$ = "0" then cst_type_d$ = "Hardware "            ~
                               else cst_type_d$ = "Packaging"
        return
L50300:     errormsg$ = "(Error) - Invalid Costing Type, (0) or (1)?"
            init(" ") cst_type$, cst_type_d$
        return

L50340: REM Product Model Code                    CST_MODEL$
            readkey$ = all(hex(00))
            str(readkey$,1%,9%) = "MODEL    "
            str(readkey$,10%,15%) = str(cst_model$,1%,3%)
            read #2,key = readkey$, using L50390, desc$, eod goto L50490
L50390:        FMT POS(25), CH(30)
            cst_model_d$ = str(desc$,1%,25%)
            cst_cl$ = str(cst_model$,4%,1%)
            str(readkey$,1%,9%)   = "COLOR    "
            str(readkey$,10%,15%) = cst_cl$
            read #2,key = readkey$, using L50460, cst_cl_d$,              ~
                                                           eod goto L50490
L50460:         FMT POS(30), CH(6)
            str(cst_model_d$,19%,7%) = " " & cst_cl_d$
        return
L50490:    errormsg$ = "(Error) - Invalid Product Model/Color Code?"
           init(" ") cst_model$, cst_model_d$, cst_cl$, cst_cl_d$
        return

L50530: REM Product Type Code
           if cst_code$ <> " " then goto L50560
              cst_code$ = "0"
L50560:    cst_code_d$ = " "
           if cst_code$ = "0" then cst_code_d$ = "Standard "
           if cst_code$ = "1" then cst_code_d$ = "TSO-ONLY "
           if cst_code$ = "2" then cst_code_d$ = "BSO_ONLY "
           if cst_code$ = "3" then cst_code_d$ = "Costmate1"
           if cst_code$ = "4" then cst_code_d$ = "Costmate2"
           if cst_code_d$ <> " " then return
           errormsg$ = "(Error) - Invalid Costing Type Code?"
           cst_code$, cst_code_d$ = " "
        return


L50670: REM Product Raw Material
           if str(cst_raw$,1%,1%) = "A" then goto L50770
           if cst_raw$ <> " " then goto L50720
              cst_desc$ = hex(06) & "Select a Raw Mat. Part"
              call "GETCODE" (#3, cst_raw$, cst_desc$, 0%, 1.25, f1%(3))
L50720:       zz% = len(cst_raw$)
              if zz% < 2% or zz% > 14% then goto L50870
           read #3,key = cst_raw$, using L50760, cst_desc$, raw_unit$,    ~
                                                           eod goto L50770
L50760:       FMT XX(25), CH(25), POS(686), CH(20)
L50770:    gosub dataload
           if rec% = 1% then goto L50850
              if str(cst_raw$,1%,1%) = "A" then return
                 gosub check_costing
                 if check_cst% = 0% then goto L50900
                    fieldnr% = 7%
                    return

L50850:    fieldnr% = 8%                     /* RECORD FOUND ON FILE */
        return
L50870:    errormsg$ = "(Error) - Invalid Product Raw Material No. ?"
           init(" ") cst_raw$, cst_desc$
        return
L50900:    errormsg$ = "(Error) - No Costing Units Defined for Mat.?"
           init(" ") cst_raw$, cst_desc$
        return

L50940: REM Raw Material Description                   CST_DESC$
           return

L50970: REM Cost Unit Value                            CST_UNIT$
            cst_unit% = 0%
            if cst_unit$ <> " " then goto L51020
               cst_unit$ = "1"

L51020:     convert cst_unit$ to cst_unit%, data goto L51070

            if cst_unit% < 1% or cst_unit% > 9% then goto L51070
            cst_unit_d$ = units$(cst_unit%)
        return
L51070:     errormsg$ = "(Error) - Invalid Cost Unit Factor?"
            init(" ") cst_unit$
        return

L51110: REM Cost Factor Associated with type           CST_COST$
            cst_cost = 0.0
            convert cst_cost$ to cst_cost, data goto L51180

            convert cst_cost to cst_cost$, pic(###.#####-)

        return
L51180:     init(" ") cst_cost$
            errormsg$ = "(Error) - Invalid Cost Value? "
        return

L51220: REM Raw Material Quantity                      CST_QTY$
           cst_qty = 0.0
           if cst_qty$ <> " " then goto L51260
              cst_qty$ = "1.0"
L51260:    convert cst_qty$ to cst_qty, data goto L51360

           convert cst_qty to cst_qty$, pic(#####.###-)

           if cst_qty = 0 then goto L51360
           x = round(cst_qty * cst_cost, 5)
           convert x to cst_extnd$, pic(###.#####-)

           gosub calc_material_cost
        return
L51360:    errormsg$ = "(Error) - Invalid Raw Material Quantity?"
           cst_qty$ = "1.0" : cst_qty = 1.0
           cst_extnd$ = " "
        return

        REM *************************************************************~
            *                 R E P O R T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on REPORT 1.                      *~
            *************************************************************

        deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L51550,         /* Type of Cost Entry    */ ~
                              L51710,         /* Beginning Model       */ ~
                              L51900          /* Ending Model          */

            return

L51550: REM Type of Cost Entry                    RPT_TYPE$
            if rpt_type$ <> " " then goto L51590
               rpt_type$ = "0"

L51590:     rpt_type% = 0%
            convert rpt_type$ to rpt_type%, data goto L51610
L51610:
            if rpt_type% <> 0% and rpt_type% <> 1% then goto L51670

            if rpt_type$ = "0" then rpt_type_d$ = "Hardware "            ~
                               else rpt_type_d$ = "Packaging"
        return
L51670:     errormsg$ = "(Error) - Invalid Costing Type, (0) or (1)?"
            init(" ") rpt_type$, rpt_type_d$
        return

L51710: REM Beginning Model                       BEG_MODEL$
            if beg_model$ <> " " then goto L51780
L51730:        beg_model$ = "ALL"
               beg_model_d$ = "(ALL) Models"
               end_model$, end_model_d$ = " "
               fieldnr% = 3%
               return
L51780:     if str(beg_model$,1%,1%) = "A" then goto L51730
            readkey$ = all(hex(00))
            str(readkey$,1%,9%) = "MODEL    "
            str(readkey$,10%,15%) = beg_model$
            read #2,key = readkey$, using L51840, beg_model_d$,           ~
                                                 eod goto L51860
L51840:        FMT POS(25), CH(30)
        return
L51860:    errormsg$ = "(Error) - Invalid Product Model Code? ?"
           init(" ") beg_model$, beg_model_d$
        return

L51900: REM Ending Model                            END_MODEL$
            if end_model$ <> " " then goto L51950
               end_model$ = beg_model$
               end_model_d$ = beg_model_d$
               return
L51950:     readkey$ = all(hex(00))
            str(readkey$,1%,9%) = "MODEL    "
            str(readkey$,10%,15%) = end_model$
            read #2,key = readkey$, using L52000, end_model_d$,           ~
                                                 eod goto L52020
L52000:        FMT POS(25), CH(30)
        return
L52020:    errormsg$ = "(Error) - Invalid Product Model Code? ?"
           init(" ") end_model$, end_model_d$
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

L55040: %+---------------------------------------------------------------~
        ~----------------------------------------------------------------+
L55060: %!---------------------------------------------------------------~
        ~----------------------------------------------------------------!
L55080: %! ######## @ ########                        ###################~
        ~#####################                                Page: #### !

        %!                                                               ~
        ~                                                                !

L55140: %! Costing Type: #  ##########                                   ~
        ~                                                                !

L55170: %!Stat!Mod !<------ Description ---->!Code! Raw Material !<---- D~
        ~escription ------>!U!<--- Description --->!Extnd Cost! Quanitiy !

L55200: %!### !####!#########################!####!##############!#######~
        ~##################!#!#####################!##########!##########!

L55230: %!----!----!-------------------------!----!--------------!-------~
        ~------------------!-!---------------------!----------!----------!

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        print_header
           if lcntr% <> 99% then print using L55040
           print page
           pageno% = pageno% + 1%
           print using L55040
           print using L55080, date$, runtime$, title$, pageno%
           print using L55140, rpt_type$, rpt_type_d$
           print using L55060
           print using L55170
           lcntr% = 5%
        return

        print_detail
           if lcntr% > 57% then gosub print_header
           if cst_code$ = "0" then cst_code_e$ = "ALL "
           if cst_code$ = "1" then cst_code_e$ = "TSO "
           if cst_code$ = "2" then cst_code_e$ = "BSO "
           if cst_code$ = "3" then cst_code_e$ = "ECT."

           print using L55230
           print using L55200, cst_stat_d$, cst_model$,cst_model_d$,      ~
                              cst_code_e$, cst_raw$, cst_desc$,cst_unit$,~
                              cst_unit_d$, cst_extnd$, cst_qty$
           lcntr% = lcntr% + 2%
        return

        generate_report
            rpt% = 1%
            cst_key$ = all(hex(00))
            str(cst_key$,1%,1%) = rpt_type$
            if beg_model$ = "ALL" then goto generate_next
               str(cst_key$,2%,3%) = beg_model$

        generate_next
            read #1,key > cst_key$, using L60400, cst_key$,               ~
                                                   eod goto generate_done
L60400:        FMT CH(20)
            if str(cst_key$,1%,1%) <> rpt_type$ then generate_done
            if beg_model$ = "ALL" then goto L60460
               if str(cst_key$,2%,3%) < beg_model$ or                    ~
                  str(cst_key$,2%,3%) > end_model$ then                  ~
                                                       goto generate_next
L60460:     gosub dataload
            gosub print_detail
            goto generate_next

        generate_done
            print using L55040
            rpt% = 0%
        return

        load_units
            i% = 0%
            readkey$ = all(hex(00))
            str(readkey$,1%,9%) = "COST UNIT"
        load_next
            i% = i% + 1%
            if i% > 9% then goto L60660
            convert i% to str(readkey$,10%,15%), pic(#)
            read #2,key = readkey$, using L60640,units$(i%),eod goto L60660
L60640:        FMT POS(25), CH(30)
            goto load_next
L60660: return

        check_costing
            check_cst% = 0%
            readkey$ = all(hex(00))
            str(readkey$,1%,25%) = cst_raw$
L60720:     cost = 0.0
            read #4,key > readkey$, using L60750, readkey$, cost,         ~
                                                 eod goto L60980
L60750:        FMT POS(17), CH(44), POS(117), PD(14,4)
            if str(readkey$,1%,25%) <> cst_raw$ then goto L60800
            if str(readkey$,26%,3%) = "100" then goto L60800
               goto L60720

L60800:     p% = pos(raw_unit$ = "/")
            raw_size = 1.0 : cst_unit% = 1%
            convert str(raw_unit$,1%,p%-1%) to raw_size, data goto L60980

            convert str(raw_unit$,p%+1%,1%) to cst_unit%, data goto L60980

            cst_cost = round( (cost / raw_size), 5)
            convert cst_cost to cst_cost$, pic(###.#####-)
            cst_unit$ = str(raw_unit$,p%+1%,1%)
            cst_unit_d$ = units$(cst_unit%)
            cst_qty = 1.0
            convert cst_qty to cst_qty$, pic(#####.###-)

            x = round(cst_qty * cst_cost, 5)
            convert x to cst_extnd$, pic(###.#####-)

        check_cst% = 1%
        return
L60980:    init(" ") cst_unit$, cst_unit_d$, raw_unit$, cst_cost$,       ~
                      cst_extnd$, cst_qty$
           cst_cost = 0.0
           cst_qty  = 0.0
        return

        calc_material_cost
           if str(cst_raw$,1%,1%) = "A" then goto L61460
           if rpt% = 1% then goto L61500

           new_cost = 0.0
           raw_unit, raw_frt, raw_vinyl = 0.0
           call "APCCST9B" ( cst_raw$,      /* Raw Material Part No.   */~
                             cst_qty,       /* Raw Material Quantity   */~
                             cst_unit%,     /* Raw Material Units Code */~
                             raw_cost,      /* Calc of Total Mat'l Cost*/~
                             raw_desc$,     /* Descript Material       */~
                             raw_unit,      /* Raw Material Unit Cost  */~
                             raw_frt,       /* Raw Calc Freight Cost   */~
                             raw_vinyl,     /* Raw Calc Vinyl Disc Cost*/~
                             #4,            /* (HNYQUAN) - File        */~
                             #3,            /* (HNYMASTR) - File       */~
                             raw_err% )     /* 0% = ok                 */

           if cst_qty = 0 then goto L61270
              new_cost = round( raw_cost / cst_qty, 5)
              new_cost = round( new_cost + raw_frt, 5)
              new_cost = round( new_cost - raw_vinyl, 5)

L61270:    convert new_cost to new_cost$, pic(###.#####-)

        REM IF REC% = 1% AND CST_STAT$ <> " " THEN GOTO 61390
              cst_stat$ = "0"
              if raw_err% <> 0% then goto L61400
              if cst_qty = 0 then goto L61420
              if new_cost = 0 then goto L61440
              if new_cost <> cst_cost then goto L61380
                 cst_stat$ = "0"                 /* They are Equal     */
                 goto L61500

L61380: cst_stat$ = "1" : goto L61500             /* They are Different */

L61400: cst_stat$ = "2" : goto L61500             /* Error in Calc      */

L61420: cst_stat$ = "3" : goto L61500             /* Quantity Zero      */

L61440: cst_stat$ = "4" : goto L61500             /* Cost Calc Zero     */

L61460: cst_stat$ = "5"                          /* Special Product    */
        new_cost = cst_cost
        convert new_cost to new_cost$, pic(###.#####-)

L61500: cst_stat_d$ = "OK "
        if cst_stat$ = "1" then cst_stat_d$ = "MOD"
        if cst_stat$ = "2" then cst_stat_d$ = "ERR"
        if cst_stat$ = "3" then cst_stat_d$ = "Q00"
        if cst_stat$ = "4" then cst_stat_d$ = "C00"
        if cst_stat$ = "5" then cst_stat_d$ = "SPC"
        cst_stat% = 0%
        convert cst_stat$ to cst_stat%, data goto L61590

L61590: return

        rebuild_costs
           call "SHOSTAT" ("Re_Build Material Cost")
           cst_key$ = " "
           rec% = 0%
        rebuild_next
           read #1,hold,key > cst_key$, using L61680, cst_key$,           ~
                                              eod goto rebuild_done
L61680:        FMT CH(20)
            get #1, using L35040, cst_type$, cst_model$, cst_code$,       ~
                                 cst_raw$, cst_desc$, cst_unit$,         ~
                                 cst_cost, cst_qty, cst_stat$, cst_fil$
            cst_unit% = 0%
            convert cst_unit$ to cst_unit%, data goto L61740
L61740:
            delete #1
            gosub calc_material_cost

            put #1, using L35040, cst_type$, cst_model$, cst_code$,       ~
                                 cst_raw$, cst_desc$, cst_unit$,         ~
                                 new_cost, cst_qty, cst_stat$, cst_fil$
            write #1, eod goto L61860
            goto rebuild_next
        rebuild_done
        return clear all
        goto inputmode
L61860:     stop "(Error) - Writing record -->  " & cst_key$
            goto rebuild_next

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
