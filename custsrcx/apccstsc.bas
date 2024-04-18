        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCCSTSC                             *~
            *  Creation Date     - 10/28/93                             *~
            *  Last Modified Date- 11/07/97                             *~
            *  Written By        - Roy H. Hoffman                       *~
            *  Description       - Display screen for Showing the Cost  *~
            *                      of a Manufactured Product.           *~
            *                                                           *~
            *  Special Comments  - Use PF(6) to turn on Debug Display   *~
            *                                                           *~
            *                                                           *~
            *   Subroutines - (APCCST5B) - Sub to Calculate the Direct  *~
            *                              and Indirect Labor Cost      *~
            *                              Assoc. with a MFG Part.      *~
            *                     Tables - 'COST MHPU', 'COST 01LB'     *~
            *                              'COST 02LB', 'COST 03LB'     *~
            *                              'COST 04LB', 'COST 05LB'     *~
            *                                                           *~
            *                 (APCCST1B) - Sub to Calculate the Primary,*~
            *                              Misc., and Grid/Liting       *~
            *                              Material Cost Associated     *~
            *                              with a MFG Part. Scrap Cost  *~
            *                              is also Calculated.          *~
            *                              (APCCSTSB - Used )           *~
            *                     Tables - 'COST 03LT', 'COST 01SP'     *~
            *                              'COST 01PD' = Liting Pads    *~
            *                                                           *~
            *                 (APCCST9B) - Calc the Unit Cost Assoc.    *~
            *                              with a Raw Mat'l Part No.    *~
            *                                                           *~
            *                 (APCCST2B) - Calc the Glass and Screen    *~
            *                              Cost of Mat'ls for a MFG     *~
            *                              Part. Scrap Cost also Calc.  *~
            *                     Tables - 'COST 01GM', 'COST 01GS'     *~
            *                              'COST 02SC', 'COST 01SP'     *~
            *                                                           *~
            *                 (APCCST3B) - Calc the Cost of Locks and   *~
            *                              Keeper Rail for MFG Part     *~
            *                     Tables - 'COST 06LK'                  *~
            *                               Note - Lock and Keeper Raw  *~
            *                                      Material             *~
            *                            - 'COST 07LK'                  *~
            *                               Note - Lock and Keeper      *~
            *                                      Screws               *~
            *                                                           *~
            *                 (APCCST4B) - Calc. the Cost of Hardware   *~
            *                              and Packaging. When doing    *~
            *                              Hardware also Calc the Cost  *~
            *                              of Balcance Tubes.           *~
            *                     Tables - 'COST 05BL'                  *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/28/93 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 10/02/95 ! Mod to put in the New Subs and the Cost  ! RHH *~
            *          !   of Freight and Vinyl Discount          !     *~
            * 10/02/95 ! Mod to Calculate Wood Surround and Fact  ! RHH *~
            *          !   Mull Products. (W_F_COST$)             !     *~
            * 05/31/96 ! Mod to add the Display of the Debug      ! RHH *~
            *          !   Screen. To Activate use PF(6) to turn  !     *~
            *          !   on. Only Valid for Userid (RHH)        !     *~
            * 11/07/97 ! Mods for Upgrade to R6.04.03             ! RHH *~
            *02/13/2019! CR-1894  Increase EMP DEPT size to 3     ! DES *~
            *************************************************************

        dim                                                              ~
            apc_scr$120,                 /* Screen Description         */~
            apc_prt$60,                  /* Print Description          */~
            apc_sze$20,                  /* Size Description           */~
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

        dim part$25,                     /* MFG PART NUMBER            */~
            part_desc$32,                /* PART DESCRIPTION           */~
            lb_typ$1,                    /* A=Actual, S=Standard       */~
            lb_typ_d$30,                 /* Labor Type Description     */~
            readkey$50,                  /* GENERIC KEY                */~
            vinyl_disc$10,               /* Vinyl Discount Amount      */~
            total_mat_disc$10,           /* Matrial                    */~
            total_mfg_disc$10,           /* MFG                        */~
            sub_pcnt$7,                  /* Screen Pcnt's              */~
            ovr_pcnt$7,                  /* Screen Pcnt's              */~
            labor_pcnt$7,                /* Screen Pcnt's              */~
            vinyl_pcnt$7,                /* Screen Pcnt's              */~
            glass_pcnt$7,                /* Screen Pcnt's              */~
            screen_pcnt$7,               /* Screen Pcnt's              */~
            hardware_pcnt$7,             /* Screen Pcnt's              */~
            packaging_pcnt$7,            /* Screen Pcnt's              */~
            misc_pcnt$7,                 /* Screen Pcnt's              */~
            material_pcnt$7,             /* Screen Pcnt's              */~
            frt_pcnt$7,                  /* Screen Pcnt's              */~
            total_mfg$10,                /*                            */~
            total_vinyl$10,              /*                            */~
            total_glass$10,              /*                            */~
            total_screen$10,             /*                            */~
            total_hardware$10,           /*                            */~
            total_packaging$10,          /*                            */~
            total_misc$10,               /*                            */~
            total_mat$10,                /*                            */~
            avg_pay(15%),                /* AVERAGE PAY PER DEPT       */~
            uph(15%),                    /* UNIT PER MANHOUR BY DEPT   */~
            frt_cost$10,                 /* Calculated Freight Cost    */~
            mfg_labor$10,                /* Direct Labor Product Line  */~
            gls_labor$10,                /* Indirect Glass Labor       */~
            scr_labor$10,                /* Indirect Screen Labor      */~
            mat_labor$10,                /* Indirect Material Labor    */~
            stg_labor$10,                /* Indirect Staging Labor     */~
            lod_labor$10,                /* Indirect Loading Labor     */~
            tot_labor$10,                /* Total DIRECT/INDIRECT      */~
            ovr_labor$10,                /* Overhead = (1/2) of Labor  */~
            w_f_cost$10, wood_code$3,    /* Wood Surround Fac Mull     */~
            value$10,                    /* W/F                        */~
            total$10,                    /* TOTAL OVERALL LABOR        */~
            disp_txt$30,                 /* Screen Header Text         */~
            disp_msg$79                  /* Screen Prompt Text         */

        dim sel$2, apc_err%(20%),        /* Debug Selection Code       */~
            cuscode$9, lab(10%),         /* Debug Customer Code        */~
            tc(25%),                     /* Debug Assoc. Material Costs*/~
            tt(25%),                     /* Debug Total Cost Buckets   */~
            rm_mat(20%),                 /* Debug Mat'l Costs No Scrap */~
            rm_mats(20%),                /* Debug Mat'l Costs Scrap Onl*/~
            rm_part$(100%)10,            /* Inv Raw Mat'l Part No.     */~
            rm_raw$(100%)14,             /* HARDWARE/PACKAGING   .     */~
            rm_desc$(100%)32,            /* Raw Mat'l Description      */~
          rm_cuts(100%),rm_cuts$(100%)10,/* Total Inches In Decimal    */~
          rm_cost(100%),rm_cost$(100%)10,/* Total Cost Raw Mat'l       */~
            rm_cuts_s(100%),             /* Total Inch In Decimal SCRAP*/~
            rm_cost_s(100%),             /* Total Cost Raw Mat'l SCRAP */~
            rm_cuts_s$(100%)8,           /* Total Inch In Decimal SCRAP*/~
            rm_cost_s$(100%)8,           /* Total Cost Raw Mat'l SCRAP */~
            rm_unit(100%),               /* Raw Material Unit Cost     */~
            rm_eq$(100%)3,               /* Save Cal Typ and Eq.No     */~
            rm_ph$(100%)5,               /* Save Phantom No.           */~
            rm_total$10,                 /* Total All Raw Mat'ls       */~
            cst_type$1, area$1,          /* HARDWARE/PACKAGING         */~
            hdr$(9%)32, txt$(10%)30      /* Headers , Text Lines       */

        dim gs_raw$(9%)25,               /* Inv Raw Mat'l Part No.     */~
            gs_desc$(9%)32,              /* Raw Mat'l Description      */~
            gs_qty(9%), gs_qty$(9%)10,   /* Total Material (1-8)       */~
            gs_qty_s(9%),gs_qty_s$(9%)10,/* Total Material (1-8) Scrap */~
            gs_cost(9%), gs_cost$(10%)10,/* Total Cost Raw Mat'l (1-8) */~
            gs_cost_s(9%), gs_cost_s$(10%)10,/* Scrap Total Cost (1-8) */~
            gs_units%(100%),gs_units$(100%)1, /* Total Cost Raw Mat'l  */~
            gs_total$10, gs_total_s$10   /* Total All Raw Mat'ls       */


        dim f2%(15%),                    /* = 0 if the file is open    */~
            f1%(15%),                    /* = 1 if READ was successful */~
            fs%(15%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(15%)20                 /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "Cost Explosion Screen Display Utility   "
            pname$ = "APCCSTSC - Rev: R6.04"

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
            * #01 ! APCCUTEQ ! Saw Optimization Cross-Reference File    *~
            * #02 ! HNYMASTR ! Part Master File                         *~
            * #03 ! HNYQUAN  ! INVENTORY QUANTITIES MASTER              *~
            * #04 ! GENCODES ! Master Code Table File                   *~
            * #05 ! AMTBOMCD ! Master Equation File                     *~
            * #06 ! AMTBOMIF ! Master Part Validity File                *~
            * #07 ! APCEMPLY ! EMPLOYEE MASTER FILE                     *~
            * #08 ! APCEQUAT ! EQUATION AN PARTS CROSS REFERENCE FILE   *~
            * #09 ! APCCSTHP ! HARDWARE AND PACKAGING COSTING COMPONENTS*~
            * #10 ! APCCSTLR !DEPARTMENTS AVERAGE HOURLY RATES          *~
            * #11 ! APCCSTEX ! APC COSTMATE EXCEPTION FILE              *~
            * #12 ! APCSTOCK ! APC STOCK MASTER FILE                    *~
            * #13 ! APCPLNDP ! Planning Master Department File          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "APCCUTEQ",                                      ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    2, keylen =   7,                     ~
                        alt key  1, keypos  =     1, keylen =  8

            select #2,  "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =  1,   keylen =  25,                     ~
                        alt key  1, keypos  =   102, keylen =  9, dup,   ~
                            key  2, keypos  =    90, keylen =  4, dup,   ~
                            key  3, keypos  =    26, keylen = 32, dup

            select #3,  "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =   650,             ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44

            select #4,  "GENCODES",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =    1, keylen =  24

            select #5,  "AMTBOMCD",                                      ~
                        varc,     indexed,  recsize = 250,               ~
                        keypos = 1,    keylen = 42

            select #6,  "AMTBOMIF",                                      ~
                        varc,     indexed,  recsize =  120,              ~
                        keypos =    1, keylen =  32                      ~

            select #7,  "APCEMPLY",                                      ~
                        varc,     indexed,  recsize =  1024,             ~
                        keypos =    7, keylen =   5,                     ~
                        alt key  1, keypos =    1, keylen =  11, dup,    ~
                            key  2, keypos  =  12, keylen =  26, dup

            select #8,  "APCEQUAT",                                      ~
                        varc,     indexed,  recsize =   16,              ~
                        keypos =    1, keylen =   8

            select #9,  "APCCSTHP",                                      ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    1, keylen =  20

            select #10, "APCCSTLR",                                      ~
                        varc,     indexed,  recsize =  102,              ~
                        keypos =    1, keylen =  3

            select #11,  "APCCSTEX",                                     ~
                        varc,     indexed,  recsize = 1100,              ~
                        keypos =    1, keylen =    9

            select #12, "APCSTOCK",                                      ~
                        varc,     indexed,  recsize =  70,               ~
                        keypos =    1, keylen =  32,                     ~
                        alt key  1, keypos =   8, keylen = 32

            select #13, "APCPLNDP",                                      ~
                        varc,     indexed,  recsize =    32,             ~
                        keypos =   11, keylen =  12,                     ~
                        alt key  1, keypos  =   9, keylen =  14,         ~
                            key  2, keypos  =   4, keylen =  12,         ~
                            key  3, keypos  =   1, keylen =  15

                           

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),100%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),  0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),  0%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%),  0%, rslt$(4%))
            call "OPENCHCK" (#5, fs%(5%), f2%(5%),  0%, rslt$(5%))
            call "OPENCHCK" (#6, fs%(6%), f2%(6%),  0%, rslt$(6%))
            call "OPENCHCK" (#7, fs%(7%), f2%(7%),  0%, rslt$(7%))
            call "OPENCHCK" (#8, fs%(8%), f2%(8%),  0%, rslt$(8%))
            call "OPENCHCK" (#9, fs%(9%), f2%(9%),  0%, rslt$(9%))
            call "OPENCHCK" (#10, fs%(10%), f2%(10%),100%, rslt$(10%))
            call "OPENCHCK" (#11, fs%(11%), f2%(11%),100%, rslt$(11%))
            call "OPENCHCK" (#12, fs%(12%), f2%(12%),100%, rslt$(12%))
            call "OPENCHCK" (#13, fs%(13%), f2%(13%),100%, rslt$(13%))

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

            for fieldnr% = 1% to  1%
L03070:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L03190
L03090:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L03170
L03120:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L03090
                         if fieldnr% = 1% then L03070
                         goto L03120
L03170:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L03090
L03190:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L03090
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
                  if keyhit%  = 16% then gosub exit_program
                  if keyhit% <>  0% then       editpg1
L03350:     fieldnr% = cursor%(1%) - 3%
            if fieldnr% < 1% or fieldnr% > 1% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L03400:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L03400
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L03400
                  lastfieldnr% = fieldnr%
            goto L03350

        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

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
            if fieldnr% <> 0% then L03750
                inpmessage$ = edtmessage$
                return

L03750
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter a Valid MFG Part No., and (A)ctual or (S)tandard Labor."

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, part$, part_desc$,         ~
                      apc_scr$, apc_prt$, apc_sze$, readkey$, lb_typ$,   ~
                      lb_typ_d$, frt_cost$
            mat avg_pay = zer
            mat uph     = zer
            debug% = 0%
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
            * Update Store Data and Part Data                           *~
            *************************************************************
        REM DATAPUT
        REM RETURN

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
L04420:       gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L04510           /* Part Number       */

              goto L04540

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L04510:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L04540:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,15), fac(hex(84)), apc_prt$               , ch(60),~
               at (04,02), "Part Number:",                               ~
               at (04,15), fac(lfac$(1%)), part$                , ch(25),~
               at (04,45), fac(hex(84)), part_desc$             , ch(30),~
                                                                         ~
               at (05,02), "(A)ct,(S)td:",                               ~
               at (05,15), fac(lfac$(1%)), lb_typ$              , ch(01),~
               at (05,45), fac(hex(84)), lb_typ_d$              , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)
               display_flag% = 1%
               if keyhit% <> 2% then goto L04810
                  gosub calc_labor
                  goto L04420

L04810:        if keyhit% <> 3% then goto L04850
                  gosub calc_material
                  goto L04420

L04850:        if keyhit% <> 4% then goto L04900
                  typ% = 0%
                  gosub calc_glass
                  goto L04420

L04900:        if keyhit% <> 5% then goto L04950
                  typ% = 1%
                  gosub calc_screen
                  goto L04420

L04950:        if keyhit% <> 6% then goto L04980
                  debug% = 1%

L04980:        if keyhit% <> 7% then goto L05020
                  gosub calc_locks
                  goto L04420

L05020:        if keyhit% <> 8% then goto L05070
                  cst_type$ = "0"
                  gosub calc_hardware
                  goto L04420

L05070:        if keyhit% <> 9% then goto L05120
                  cst_type$ = "1"
                  gosub calc_hardware
                  goto L04420

L05120:        if keyhit% <> 14% then goto L05160
                  gosub calc_total_cost
                  goto L04420

L05160:        if keyhit% <> 15 then goto L05200
                  call "PRNTSCRN"
                  goto L04540

L05200:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L05350      /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return

L05350: if fieldnr% > 0% then L05460   /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over      (4)Calc Glass        " &        ~
                     "(7)Calc Locks          (14)Total Cost  "
            pf$(2) = "(2)Calc Labor      (5)Calc Screen       " &        ~
                     "(8)Calc Hardware       (15)Print Screen"
            pf$(3) = "(3)Calc Material   (6)Debug Screen      " &        ~
                     "(9)Calc Packaging      (16)Exit Program"
            pfkeys$ = hex(010203040506070809ffffffff0e0f1000)
            if userid$ = "RHH" then return
               str(pf$(3),20%,16%) = " " : str(pfkeys$,6%,1%) = hex(ff)
            return
L05460:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L05680           /* Part Number           */

            return

L05680: REM MFG Part Number                       PART$
          if lb_typ$ <> " " then goto L05710
             lb_typ$ = "A"
L05710:   if lb_typ$ = "A" then lb_typ_d$ = "Calculate Actual Labor  "
          if lb_typ$ = "S" then lb_typ_d$ = "Use Standard Labor Rates"
          if lb_typ$ <> "A" and lb_typ$ <> "S" then goto L05880

          if part$ <> " " then goto L05810
             part_desc$ = " "
             part_desc$ = hex(06) & "Select a MFG Part Number"
             call "GETCODE" (#2, part$, part_desc$, 0%, 1.32,f1%(2))
             if f1%(2) = 0% then goto L05850

L05810:      gosub lookup_description
        REM  IF ERR% <> 0% THEN GOTO 5560
                part_desc$ = apc_sze$
        return
L05850:     errormsg$ = "(Error) - Invalid Part Number."
            init(" ") part$, part_desc$, lb_typ$, lb_typ_d$
        return
L05880:     errormsg$ = "(Error) - Invalid Labor Type Code "
            init(" ") lb_typ$, lb_typ_d$
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        lookup_description
           err% = 0%
           call "APCDESCR" (part$, apc_scr$, apc_prt$, apc_sze$, #6,     ~
                                                                  err% )
        return

        calc_labor
           if display_flag% = 0% then goto L60056
              call "SHOSTAT" ("Calculatng Direct/Indirect Labor")

L60056:    mfg_labor, gls_labor, scr_labor, mat_labor = 0.0
           stg_labor, lod_labor, tot_labor, ovr_labor = 0.0
           lab_err% = 0%
           init(" ") mfg_labor$, gls_labor$, scr_labor$, mat_labor$,     ~
                     stg_labor$, lod_labor$, tot_labor$, ovr_labor$,     ~
                     total$

           call "APCCST5B" (part$,            /* MFG PART NUMBER       */~
                            lb_typ$,          /* A=Actual,S=Standard   */~
                            mfg_labor,        /* Direct Labor Prod Line*/~
                            gls_labor,        /* Direct   Glass Labor  */~
                            scr_labor,        /* Direct   Screen Labor */~
                            mat_labor,        /* Indirect Material Lab */~
                            stg_labor,        /* Indirect Staging Labor*/~
                            lod_labor,        /* Indirect Loading Labor*/~
                            ind_labor,        /* Standard Indirec Labor*/~
                            sub_labor,        /* Total of Direct+Indire*/~
                            ovr_labor,        /* Labor Overhead        */~
                            tot_labor,        /* Total Labor           */~
                            avg_pay(),        /* Average Pay Per Dept  */~
                            uph(),            /* Unit Per Manhour Dept */~
                                   #7,        /* (APCEMPLY) - FILE     */~
                                   #4,        /* (GENCODES) - FILE     */~
                                  #10,        /* (APCCSTLR) - FILE     */~
                                  #13,        /* (APCPLNDP) - FILE     */~
                            lab_err% )        /* 0% = OK               */
           if debug% = 0% then goto L60200
              mat lab = zer
              lab(1%) = mfg_labor  : lab(2%)  = gls_labor
              lab(3%) = scr_labor  : lab(4%)  = mat_labor
              lab(5%) = stg_labor  : lab(6%)  = lod_labor
              lab(7%) = ind_labor  : lab(8%)  = sub_labor
              lab(9%) = ovr_labor  : lab(10%) = tot_labor
              sel$ = "06"
              gosub display_screen

L60200:    if display_flag% = 0% then return
           convert mfg_labor to mfg_labor$, pic(####.####-)
           convert gls_labor to gls_labor$, pic(####.####-)
           convert scr_labor to scr_labor$, pic(####.####-)
           if lb_typ$ = "S" then goto L60236
              convert mat_labor to mat_labor$, pic(####.####-)
              convert stg_labor to stg_labor$, pic(####.####-)
              convert lod_labor to lod_labor$, pic(####.####-)
              goto L60244
L60236:    convert ind_labor to mat_labor$, pic(####.####-)

L60244:    convert sub_labor to sub_labor$, pic(####.####-)
           convert ovr_labor to ovr_labor$, pic(####.####-)
           convert tot_labor to tot_labor$, pic($###.####-)

           init(" ") txt$()
           if lb_typ$ = "S" then goto L60292
              txt$(1%) = "Indirect Material Labor  :"
              txt$(2%) = "Indirect Staging Labor   :"
              txt$(3%) = "Indirect Loading Labor   :"
              txt$(4%) = "                          "
              txt$(5%) = "                          "
              goto L60300
L60292:       txt$(1%) = "Total Indirect Labor     :"

L60300: REM - Labor Display Screen
            disp_txt$ = "MFG  Direct and Indirect Labor"
            gosub set_display

L60316:     accept                                                       ~
               at (01,02),                                               ~
                  "Exploded Cost's for Specified Manufactured Part",     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,15), fac(hex(84)), apc_prt$               , ch(60),~
               at (04,02), "Part Number:",                               ~
               at (04,15), fac(hex(84)), part$                  , ch(25),~
               at (04,45), fac(hex(84)), part_desc$             , ch(30),~
                                                                         ~
               at (05,02), "(A)ct,(S)td:",                               ~
               at (05,15), fac(hex(84)), lb_typ$                , ch(01),~
               at (05,45), fac(hex(84)), lb_typ_d$              , ch(30),~
                                                                         ~
               at (06,24), fac(hex(94)), disp_txt$              , ch(30),~
                                                                         ~
               at (08,02), "MFG Direct Labor         :",                 ~
               at (08,30), fac(hex(84)), mfg_labor$             , ch(10),~
               at (09,02), "Glass Direct Labor       :",                 ~
               at (09,30), fac(hex(84)), gls_labor$             , ch(10),~
               at (10,02), "Screen Direct Labor      :",                 ~
               at (10,30), fac(hex(84)), scr_labor$             , ch(10),~
                                                                         ~
               at (12,02), fac(hex(84)), txt$(1%)               , ch(26),~
               at (12,30), fac(hex(84)), mat_labor$             , ch(10),~
               at (13,02), fac(hex(84)), txt$(2%)               , ch(26),~
               at (13,30), fac(hex(84)), stg_labor$             , ch(10),~
               at (14,02), fac(hex(84)), txt$(3%)               , ch(26),~
               at (14,30), fac(hex(84)), lod_labor$             , ch(10),~
                                                                         ~
               at (16,02), "Total (Direct & Indirect):",                 ~
               at (16,30), fac(hex(84)), sub_labor$             , ch(10),~
                                                                         ~
               at (18,02), "Total Labor ( Overhead ) :",                 ~
               at (18,30), fac(hex(84)), ovr_labor$             , ch(10),~
                                                                         ~
               at (20,02), "Total Labor              :",                 ~
               at (20,30), fac(hex(84)), tot_labor$             , ch(10),~
                                                                         ~
               at (23,02), fac(hex(a4)),   disp_msg$            , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L60512
                  call "PRNTSCRN"
                  goto L60316

L60512:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_display
            disp_msg$ = "Press <Return> To Continue?, or PF(15) To Print ~
        ~the Screen?"

            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return

        calc_material
           if display_flag% = 0% then goto L60568
              call "SHOSTAT" ("Calculating Primary Raw Material Cost")
L60568:    tot_cost_mat = 0.0
           tot_frt_cost = 0.0
           tot_vinyl_disc = 0.0
           rm_cnt%, rm_err% = 0%
           init(" ") rm_part$(), rm_cuts$(), rm_cost$(), rm_desc$(),     ~
                     rm_total$, hdr$(), rm_cuts_s$(), rm_cost_s$(),      ~
                     rm_total_s$, rm_vinyl$, rm_vinyl_s$, rm_misc$,      ~
                     rm_misc_s$

           call "APCCST1B" (part$,            /* MFG PART NUMBER       */~
                            rm_part$(),       /* Inv Raw Mat'l Part No.*/~
                            rm_cuts(),        /* Total Inches in Decima*/~
                            rm_cost(),        /* Total Cost Raw Mat'l  */~
                            rm_desc$(),       /* Raw Mat'l Description */~
                            gs_units%(),      /* UNITS OF MEASURE      */~
                            rm_unit(),        /* Raw Material Unit Cost*/~
                            rm_vinyl,         /* Vinyl Material Cost   */~
                            rm_misc,          /* Misc Material Cost    */~
                            rm_total,         /* Total Material Cost   */~
                            rm_cnt%,          /* Raw Material Count    */~
                            rm_cuts_s(),      /* Total Inch Decima SCRA*/~
                            rm_cost_s(),      /* Total Cst Raw Mat SCRA*/~
                            rm_vinyl_s,       /* Vinyl Mat'l Cost Scrap*/~
                            rm_misc_s,        /* Misc Mat'l Cost Scrap */~
                            rm_total_s,       /* Raw Mat Tot Cost SCRAP*/~
                            rm_eq$(),         /* Save Cal Typ and Eq.No*/~
                            rm_ph$(),         /* Save Phantom No.      */~
                            rm_frt,           /* Save Freight Cost     */~
                            rm_vinyl_d,       /* Save Vinyl Disc Amt.  */~
                                   #1,        /* (APCCUTEQ) - FILE     */~
                                   #4,        /* (GENCODES) - FILE     */~
                                   #2,        /* (HNYMASTR) - FILE     */~
                                   #3,        /* (HNYQUAN ) - FILE     */~
                                   #5,        /* (AMTBOMCD) - FILE     */~
                                   #12,       /* (APCSTOCK) - FILE     */~
                            rm_err%   )       /* 0% = OK               */
           area$ = "1"
           gosub calc_costmate

           tot_cost_vny = rm_vinyl + rm_vinyl_s
           tot_cost_msc = rm_misc  + rm_misc_s
           tot_cost_mat = rm_total + rm_total_s
           tot_frt_cost   = tot_frt_cost   + rm_frt
           tot_vinyl_disc = tot_vinyl_disc + rm_vinyl_d
           if debug% = 0% then goto L60780
              mat lab = zer
              lab(1%) = rm_vinyl   : lab(2%)  = rm_misc
              lab(3%) = rm_total   : lab(4%)  = rm_vinyl_s
              lab(5%) = rm_misc_s  : lab(6%)  = rm_total_s
              lab(7%) = rm_frt     : lab(8%)  = rm_vinyl_d
              sel$ = "07"
              gosub display_screen

L60780:    if display_flag% = 0% then return
           for i% = 1% to rm_cnt%
               convert rm_cuts_s(i%) to rm_cuts_s$(i%), pic(##.####-)
               convert rm_cost_s(i%) to rm_cost_s$(i%), pic(##.####-)
               convert rm_cuts(i%) to rm_cuts$(i%), pic(####.####-)
               x = round( rm_cost(i%) + rm_cost_s(i%), 4)
               convert x to rm_cost$(i%), pic(####.####-)
               convert gs_units%(i%) to gs_units$(i%), pic(#)
           next i%
           convert rm_vinyl to rm_vinyl$, pic($###.####-)
           convert rm_misc  to rm_misc$ , pic($###.####-)
           convert rm_total to rm_total$, pic($###.####-)

           convert rm_vinyl_s to rm_vinyl_s$, pic($###.####-)
           convert rm_misc_s  to rm_misc_s$ , pic($###.####-)
           convert rm_total_s to rm_total_s$,pic($###.####-)

           convert tot_cost_vny to tot_cost_vny$,pic($###.####-)
           convert tot_cost_msc to tot_cost_msc$,pic($###.####-)
           convert tot_cost_mat to tot_cost_mat$,pic($###.####-)

        REM - Raw Material Cost Display
            disp_txt$ = "MFG  Primary Raw Material Cost"
            gosub set_display
            i% = 1%
            hdr$(1) = "Raw Mat'l "                             /* (10) */
            hdr$(2) = "< Raw Mat'l Description >"              /* (25) */
            hdr$(3) = "Mat'l Inch"                             /* (10) */
            hdr$(4) = "Srp Inch"                               /* (08) */
            hdr$(5) = "Srp Cost"                               /* (08) */
            hdr$(6) = "Total Cost"                             /* (10) */
            hdr$(7) = "U"                                      /* (01) */

L60912:     more$ = "(More-Data)"
            if (i% + 10%) >= rm_cnt% then more$ = "           "
L60920:     accept                                                       ~
               at (01,02),                                               ~
                  "Exploded Cost's for Specified Manufactured Part",     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,15), fac(hex(84)), apc_prt$               , ch(60),~
               at (04,02), "Part Number:",                               ~
               at (04,15), fac(hex(84)), part$                  , ch(25),~
               at (04,45), fac(hex(84)), part_desc$             , ch(30),~
                                                                         ~
               at (05,02), "(A)ct,(S)td:",                               ~
               at (05,15), fac(hex(84)), lb_typ$                , ch(01),~
               at (05,45), fac(hex(84)), lb_typ_d$              , ch(30),~
                                                                         ~
               at (06,24), fac(hex(94)), disp_txt$              , ch(30),~
               at (06,66), fac(hex(94)), more$                  , ch(11),~
                                                                         ~
               at (08,02), fac(hex(a4)), hdr$(1)                , ch(10),~
               at (08,13), fac(hex(a4)), hdr$(2)                , ch(25),~
               at (08,39), fac(hex(a4)), hdr$(3)                , ch(10),~
               at (08,50), fac(hex(a4)), hdr$(4)                , ch(08),~
               at (08,59), fac(hex(a4)), hdr$(5)                , ch(08),~
               at (08,68), fac(hex(a4)), hdr$(6)                , ch(10),~
               at (08,79), fac(hex(a4)), hdr$(7)                , ch(01),~
                                                                         ~
               at (09,02), fac(hex(84)), rm_part$(i%)           , ch(10),~
               at (09,13), fac(hex(84)), rm_desc$(i%)           , ch(25),~
               at (09,39), fac(hex(84)), rm_cuts$(i%)           , ch(10),~
               at (09,50), fac(hex(84)), rm_cuts_s$(i%)         , ch(08),~
               at (09,59), fac(hex(84)), rm_cost_s$(i%)         , ch(08),~
               at (09,68), fac(hex(84)), rm_cost$(i%)           , ch(10),~
               at (09,79), fac(hex(84)), gs_units$(i%)          , ch(01),~
                                                                         ~
               at (10,02), fac(hex(84)), rm_part$(i%+1%)        , ch(10),~
               at (10,13), fac(hex(84)), rm_desc$(i%+1%)        , ch(25),~
               at (10,39), fac(hex(84)), rm_cuts$(i%+1%)        , ch(10),~
               at (10,50), fac(hex(84)), rm_cuts_s$(i%+1%)      , ch(08),~
               at (10,59), fac(hex(84)), rm_cost_s$(i%+1%)      , ch(08),~
               at (10,68), fac(hex(84)), rm_cost$(i%+1%)        , ch(10),~
               at (10,79), fac(hex(84)), gs_units$(i%+1%)       , ch(01),~
                                                                         ~
               at (11,02), fac(hex(84)), rm_part$(i%+2%)        , ch(10),~
               at (11,13), fac(hex(84)), rm_desc$(i%+2%)        , ch(25),~
               at (11,39), fac(hex(84)), rm_cuts$(i%+2%)        , ch(10),~
               at (11,50), fac(hex(84)), rm_cuts_s$(i%+2%)      , ch(08),~
               at (11,59), fac(hex(84)), rm_cost_s$(i%+2%)      , ch(08),~
               at (11,68), fac(hex(84)), rm_cost$(i%+2%)        , ch(10),~
               at (11,79), fac(hex(84)), gs_units$(i%+2%)       , ch(01),~
                                                                         ~
               at (12,02), fac(hex(84)), rm_part$(i%+3%)        , ch(10),~
               at (12,13), fac(hex(84)), rm_desc$(i%+3%)        , ch(25),~
               at (12,39), fac(hex(84)), rm_cuts$(i%+3%)        , ch(10),~
               at (12,50), fac(hex(84)), rm_cuts_s$(i%+3%)      , ch(08),~
               at (12,59), fac(hex(84)), rm_cost_s$(i%+3%)      , ch(08),~
               at (12,68), fac(hex(84)), rm_cost$(i%+3%)        , ch(10),~
               at (12,79), fac(hex(84)), gs_units$(i%+3%)       , ch(01),~
                                                                         ~
               at (13,02), fac(hex(84)), rm_part$(i%+4%)        , ch(10),~
               at (13,13), fac(hex(84)), rm_desc$(i%+4%)        , ch(25),~
               at (13,39), fac(hex(84)), rm_cuts$(i%+4%)        , ch(10),~
               at (13,50), fac(hex(84)), rm_cuts_s$(i%+4%)      , ch(08),~
               at (13,59), fac(hex(84)), rm_cost_s$(i%+4%)      , ch(08),~
               at (13,68), fac(hex(84)), rm_cost$(i%+4%)        , ch(10),~
               at (13,79), fac(hex(84)), gs_units$(i%+4%)       , ch(01),~
                                                                         ~
               at (14,02), fac(hex(84)), rm_part$(i%+5%)        , ch(10),~
               at (14,13), fac(hex(84)), rm_desc$(i%+5%)        , ch(25),~
               at (14,39), fac(hex(84)), rm_cuts$(i%+5%)        , ch(10),~
               at (14,50), fac(hex(84)), rm_cuts_s$(i%+5%)      , ch(08),~
               at (14,59), fac(hex(84)), rm_cost_s$(i%+5%)      , ch(08),~
               at (14,68), fac(hex(84)), rm_cost$(i%+5%)        , ch(10),~
               at (14,79), fac(hex(84)), gs_units$(i%+5%)       , ch(01),~
                                                                         ~
               at (15,02), fac(hex(84)), rm_part$(i%+6%)        , ch(10),~
               at (15,13), fac(hex(84)), rm_desc$(i%+6%)        , ch(25),~
               at (15,39), fac(hex(84)), rm_cuts$(i%+6%)        , ch(10),~
               at (15,50), fac(hex(84)), rm_cuts_s$(i%+6%)      , ch(08),~
               at (15,59), fac(hex(84)), rm_cost_s$(i%+6%)      , ch(08),~
               at (15,68), fac(hex(84)), rm_cost$(i%+6%)        , ch(10),~
               at (15,79), fac(hex(84)), gs_units$(i%+6%)       , ch(01),~
                                                                         ~
               at (16,02), fac(hex(84)), rm_part$(i%+7%)        , ch(10),~
               at (16,13), fac(hex(84)), rm_desc$(i%+7%)        , ch(25),~
               at (16,39), fac(hex(84)), rm_cuts$(i%+7%)        , ch(10),~
               at (16,50), fac(hex(84)), rm_cuts_s$(i%+7%)      , ch(08),~
               at (16,59), fac(hex(84)), rm_cost_s$(i%+7%)      , ch(08),~
               at (16,68), fac(hex(84)), rm_cost$(i%+7%)        , ch(10),~
               at (16,79), fac(hex(84)), gs_units$(i%+7%)       , ch(01),~
                                                                         ~
               at (17,02), fac(hex(84)), rm_part$(i%+8%)        , ch(10),~
               at (17,13), fac(hex(84)), rm_desc$(i%+8%)        , ch(25),~
               at (17,39), fac(hex(84)), rm_cuts$(i%+8%)        , ch(10),~
               at (17,50), fac(hex(84)), rm_cuts_s$(i%+8%)      , ch(08),~
               at (17,59), fac(hex(84)), rm_cost_s$(i%+8%)      , ch(08),~
               at (17,68), fac(hex(84)), rm_cost$(i%+8%)        , ch(10),~
               at (17,79), fac(hex(84)), gs_units$(i%+8%)       , ch(01),~
                                                                         ~
               at (18,02), fac(hex(84)), rm_part$(i%+9%)        , ch(10),~
               at (18,13), fac(hex(84)), rm_desc$(i%+9%)        , ch(25),~
               at (18,39), fac(hex(84)), rm_cuts$(i%+9%)        , ch(10),~
               at (18,50), fac(hex(84)), rm_cuts_s$(i%+9%)      , ch(08),~
               at (18,59), fac(hex(84)), rm_cost_s$(i%+9%)      , ch(08),~
               at (18,68), fac(hex(84)), rm_cost$(i%+9%)        , ch(10),~
               at (18,79), fac(hex(84)), gs_units$(i%+9%)       , ch(01),~
                                                                         ~
               at (20,02), "   Vinyl Cost :",                            ~
               at (20,18), fac(hex(84)), rm_vinyl$              , ch(10),~
               at (20,30), "Vinyl Scrap:",                               ~
               at (20,42), fac(hex(84)), rm_vinyl_s$            , ch(10),~
               at (20,57), "Total Vinyl",                                ~
               at (20,69), fac(hex(84)), tot_cost_vny$          , ch(10),~
                                                                         ~
               at (21,02), "   Misc. Cost :",                            ~
               at (21,18), fac(hex(a4)), rm_misc$               , ch(10),~
               at (21,30), "Misc. Scrap:",                               ~
               at (21,42), fac(hex(a4)), rm_misc_s$             , ch(10),~
               at (21,57), "Total Misc:",                                ~
               at (21,69), fac(hex(a4)), tot_cost_msc$          , ch(10),~
                                                                         ~
               at (22,02), "Material Cost :",                            ~
               at (22,18), fac(hex(84)), rm_total$              , ch(10),~
               at (22,30), "Scrap Cost :",                               ~
               at (22,42), fac(hex(84)), rm_total_s$            , ch(10),~
               at (22,57), "Total Cost:",                                ~
               at (22,69), fac(hex(84)), tot_cost_mat$          , ch(10),~
                                                                         ~
               at (23,02), fac(hex(a4)),   disp_msg$            , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L61464
                  call "PRNTSCRN"
                  goto L60920

L61464:        if (i% + 10%) > rm_cnt% then goto L61480
                  i% = i% + 10%
                  goto L60912

L61480:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        calc_glass
        calc_screen
         if display_flag% = 0% then goto L61520
           if typ% = 0% then call "SHOSTAT" ("Calculatng Cost of Glass")
           if typ% = 1% then call "SHOSTAT" ("Calculatng Cost of Screen")

L61520:    init(" ") gs_qty$(), gs_raw$(), gs_desc$(), gs_cost$(),       ~
                     gs_total$, gs_units$(), gs_qty_s$(), gs_cost_s$(),  ~
                     gs_total_s$
           gs_err% = 0%
           if typ% = 0% then tot_cost_gls = 0.0
           if typ% = 1% then tot_cost_scr = 0.0
           call "APCCST2B" (part$,            /* MFG PART NUMBER       */~
                            typ%,             /* 0% = Glass,1% = Screen*/~
                            gs_qty(),         /* G/S Mat'l Units (1-8) */~
                            gs_raw$(),        /* Raw Mat'l Parts (1-8) */~
                            gs_desc$(),       /* Raw Mat'l Descr. (1-8)*/~
                            gs_cost(),        /* Raw Mat'l Cost (1-8)  */~
                            gs_units%(),      /* RAW Mat'l U. M. (1-8) */~
                            rm_unit(),        /* Raw Material Unit Cost*/~
                            gs_vinyl,         /* Vinyl Raw Mat'l Cost  */~
                            gs_misc,          /* Misc. Raw Mat'l Cost  */~
                            gs_total,         /* Total Raw Mat'l Cost  */~
                            gs_cnt%,          /* G/S No. Raw Mat'l Item*/~
                            gs_qty_s(),       /* G/S Units Mat'l Scrap */~
                            gs_cost_s(),      /* G/S Scrap Cost Mat'l  */~
                            gs_vinyl_s,       /* Vinyl Raw Mat'l Scrap */~
                            gs_misc_s,        /* Misc. Raw Mat'l Scrap */~
                            gs_total_s,       /* G/S Total Cost Scrap  */~
                            rm_eq$(),         /* Save Cal Typ and Eq.No*/~
                            rm_ph$(),         /* Save Phantom No.      */~
                            rm_frt,           /* Save Freight Cost     */~
                            rm_vinyl_d,       /* Save Vinyl Disc Amt.  */~
                                   #8,        /* (APCEQUAT) - File     */~
                                   #5,        /* (AMTBOMCD) - File     */~
                                   #4,        /* (GENCODES) - File     */~
                                   #3,        /* (HNYQUAN ) - File     */~
                                   #2,        /* (HNYMASTR) - File     */~
                            gs_err% )         /* 0% = Ok, Non 0% = Err */

           if typ% <> 0% then goto L61688
              gs_vinyl_gls = gs_vinyl + gs_vinyl_s
              gs_misc_gls  = gs_misc  + gs_misc_s
              tot_cost_gls = gs_total + gs_total_s
              tot_frt_cost   = tot_frt_cost   + rm_frt
              tot_vinyl_disc = tot_vinyl_disc + rm_vinyl_d
              convert tot_cost_gls to tot_cost_gls$, pic($###.####-)
              goto L61716
L61688:    tot_cost_scr = gs_total + gs_total_s
           gs_vinyl_scr = gs_vinyl + gs_vinyl_s
           gs_misc_scr  = gs_misc  + gs_misc_s
           tot_frt_cost   = tot_frt_cost   + rm_frt
           tot_vinyl_disc = tot_vinyl_disc + rm_vinyl_d
           convert tot_cost_scr to tot_cost_gls$, pic($###.####-)

L61716:    if debug% = 0% then goto L61756
              mat lab = zer
              lab(1%) = gs_vinyl   : lab(2%)  = gs_misc
              lab(3%) = gs_total   : lab(4%)  = gs_vinyl_s
              lab(5%) = gs_misc_s  : lab(6%)  = gs_total_s
              lab(7%) = rm_frt     : lab(8%)  = rm_vinyl_d
              sel$ = "08"
              if typ% = 1% then sel$ = "09"
              gosub display_screen

L61756:    if display_flag% = 0% then return

           for i% = 1% to gs_cnt%
               convert gs_qty(i%) to gs_qty$(i%), pic(####.####-)
               convert gs_qty_s(i%) to gs_qty_s$(i%), pic(###.###-)
               convert gs_cost_s(i%) to gs_cost_s$(i%), pic(##.####-)
               x = round( gs_cost(i%) + gs_cost_s(i%), 4)
               convert x to gs_cost$(i%), pic(####.####-)
               convert gs_units%(i%) to gs_units$(i%), pic(#)
           next i%

           convert gs_vinyl to gs_vinyl$, pic($###.####-)
           convert gs_vinyl_s to gs_vinyl_s$, pic($###.####-)
           gs_vinyl_tot = gs_vinyl + gs_vinyl_s
           convert gs_vinyl_tot to gs_vinyl_tot$, pic($###.####-)

           convert gs_misc to gs_misc$, pic($###.####-)
           convert gs_misc_s to gs_misc_s$, pic($###.####-)
           gs_misc_tot = gs_misc + gs_misc_s
           convert gs_misc_tot to gs_misc_tot$, pic($###.####-)

           convert gs_total to gs_total$, pic($###.####-)
           convert gs_total_s to gs_total_s$, pic($###.####-)

        REM - Glass Material Cost Display
            if typ% = 0% then disp_txt$="MFG Secondary Glass Mat'l Cost"
            if typ% = 1% then disp_txt$="MFG Second.  Screen Mat'l Cost"
            gosub set_display
            hdr$(1) = "  Raw Mat'l   "                         /* (14) */
            hdr$(2) = "Raw Mat'l Description"                  /* (21) */
            hdr$(3) = "Mat'l Inch"                             /* (10) */
            hdr$(4) = "Srp Inch"                               /* (08) */
            hdr$(5) = "Srp Cost"                               /* (08) */
            hdr$(6) = "Total Cost"                             /* (10) */
            hdr$(7) = "U"                                      /* (01) */

L61900:     accept                                                       ~
               at (01,02),                                               ~
                  "Exploded Cost's for Specified Manufactured Part",     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,15), fac(hex(84)), apc_prt$               , ch(60),~
               at (04,02), "Part Number:",                               ~
               at (04,15), fac(hex(84)), part$                  , ch(25),~
               at (04,45), fac(hex(84)), part_desc$             , ch(30),~
                                                                         ~
               at (05,02), "(A)ct,(S)td:",                               ~
               at (05,15), fac(hex(84)), lb_typ$                , ch(01),~
               at (05,45), fac(hex(84)), lb_typ_d$              , ch(30),~
                                                                         ~
               at (06,24), fac(hex(94)), disp_txt$              , ch(30),~
                                                                         ~
               at (08,02), fac(hex(a4)), hdr$(1)                , ch(14),~
               at (08,17), fac(hex(a4)), hdr$(2)                , ch(21),~
               at (08,39), fac(hex(a4)), hdr$(3)                , ch(10),~
               at (08,50), fac(hex(a4)), hdr$(4)                , ch(08),~
               at (08,59), fac(hex(a4)), hdr$(5)                , ch(08),~
               at (08,68), fac(hex(a4)), hdr$(6)                , ch(10),~
               at (08,79), fac(hex(a4)), hdr$(7)                , ch(01),~
                                                                         ~
               at (09,02), fac(hex(84)), gs_raw$(1%)            , ch(14),~
               at (09,17), fac(hex(84)), gs_desc$(1%)           , ch(21),~
               at (09,39), fac(hex(84)), gs_qty$(1%)            , ch(10),~
               at (09,50), fac(hex(84)), gs_qty_s$(1%)          , ch(08),~
               at (09,59), fac(hex(84)), gs_cost_s$(1%)         , ch(08),~
               at (09,68), fac(hex(84)), gs_cost$(1%)           , ch(10),~
               at (09,79), fac(hex(84)), gs_units$(1%)          , ch(01),~
                                                                         ~
               at (10,02), fac(hex(84)), gs_raw$(2%)            , ch(14),~
               at (10,17), fac(hex(84)), gs_desc$(2%)           , ch(21),~
               at (10,39), fac(hex(84)), gs_qty$(2%)            , ch(10),~
               at (10,50), fac(hex(84)), gs_qty_s$(2%)          , ch(08),~
               at (10,59), fac(hex(84)), gs_cost_s$(2%)         , ch(08),~
               at (10,68), fac(hex(84)), gs_cost$(2%)           , ch(10),~
               at (10,79), fac(hex(84)), gs_units$(2%)          , ch(01),~
                                                                         ~
               at (11,02), fac(hex(84)), gs_raw$(3%)            , ch(14),~
               at (11,17), fac(hex(84)), gs_desc$(3%)           , ch(21),~
               at (11,39), fac(hex(84)), gs_qty$(3%)            , ch(10),~
               at (11,50), fac(hex(84)), gs_qty_s$(3%)          , ch(08),~
               at (11,59), fac(hex(84)), gs_cost_s$(3%)         , ch(08),~
               at (11,68), fac(hex(84)), gs_cost$(3%)           , ch(10),~
               at (11,79), fac(hex(84)), gs_units$(3%)          , ch(01),~
                                                                         ~
               at (12,02), fac(hex(84)), gs_raw$(4%)            , ch(14),~
               at (12,17), fac(hex(84)), gs_desc$(4%)           , ch(21),~
               at (12,39), fac(hex(84)), gs_qty$(4%)            , ch(10),~
               at (12,50), fac(hex(84)), gs_qty_s$(4%)          , ch(08),~
               at (12,59), fac(hex(84)), gs_cost_s$(4%)         , ch(08),~
               at (12,68), fac(hex(84)), gs_cost$(4%)           , ch(10),~
               at (12,79), fac(hex(84)), gs_units$(4%)          , ch(01),~
                                                                         ~
               at (13,02), fac(hex(84)), gs_raw$(5%)            , ch(14),~
               at (13,17), fac(hex(84)), gs_desc$(5%)           , ch(21),~
               at (13,39), fac(hex(84)), gs_qty$(5%)            , ch(10),~
               at (13,50), fac(hex(84)), gs_qty_s$(5%)          , ch(08),~
               at (13,59), fac(hex(84)), gs_cost_s$(5%)         , ch(08),~
               at (13,68), fac(hex(84)), gs_cost$(5%)           , ch(10),~
               at (13,79), fac(hex(84)), gs_units$(5%)          , ch(01),~
                                                                         ~
               at (14,02), fac(hex(84)), gs_raw$(6%)            , ch(14),~
               at (14,17), fac(hex(84)), gs_desc$(6%)           , ch(21),~
               at (14,39), fac(hex(84)), gs_qty$(6%)            , ch(10),~
               at (14,50), fac(hex(84)), gs_qty_s$(6%)          , ch(08),~
               at (14,59), fac(hex(84)), gs_cost_s$(6%)         , ch(08),~
               at (14,68), fac(hex(84)), gs_cost$(6%)           , ch(10),~
               at (14,79), fac(hex(84)), gs_units$(6%)          , ch(01),~
                                                                         ~
               at (15,02), fac(hex(84)), gs_raw$(7%)            , ch(14),~
               at (15,17), fac(hex(84)), gs_desc$(7%)           , ch(21),~
               at (15,39), fac(hex(84)), gs_qty$(7%)            , ch(10),~
               at (15,50), fac(hex(84)), gs_qty_s$(7%)          , ch(08),~
               at (15,59), fac(hex(84)), gs_cost_s$(7%)         , ch(08),~
               at (15,68), fac(hex(84)), gs_cost$(7%)           , ch(10),~
               at (15,79), fac(hex(84)), gs_units$(7%)          , ch(01),~
                                                                         ~
               at (16,02), fac(hex(84)), gs_raw$(8%)            , ch(14),~
               at (16,17), fac(hex(84)), gs_desc$(8%)           , ch(21),~
               at (16,39), fac(hex(84)), gs_qty$(8%)            , ch(10),~
               at (16,50), fac(hex(84)), gs_qty_s$(8%)          , ch(08),~
               at (16,59), fac(hex(84)), gs_cost_s$(8%)         , ch(08),~
               at (16,68), fac(hex(84)), gs_cost$(8%)           , ch(10),~
               at (16,79), fac(hex(84)), gs_units$(8%)          , ch(01),~
                                                                         ~
               at (17,02), fac(hex(84)), gs_raw$(9%)            , ch(14),~
               at (17,17), fac(hex(84)), gs_desc$(9%)           , ch(21),~
               at (17,39), fac(hex(84)), gs_qty$(9%)            , ch(10),~
               at (17,50), fac(hex(84)), gs_qty_s$(9%)          , ch(08),~
               at (17,59), fac(hex(84)), gs_cost_s$(9%)         , ch(08),~
               at (17,68), fac(hex(84)), gs_cost$(9%)           , ch(10),~
               at (17,79), fac(hex(84)), gs_units$(9%)          , ch(01),~
                                                                         ~
               at (20,02), "   Vinyl Cost :",                            ~
               at (20,18), fac(hex(84)), gs_vinyl$              , ch(10),~
               at (20,30), "Vinyl Scrap:",                               ~
               at (20,42), fac(hex(84)), gs_vinyl_s$            , ch(10),~
               at (20,57), "Total Vinyl",                                ~
               at (20,69), fac(hex(84)), gs_vinyl_tot$          , ch(10),~
                                                                         ~
               at (21,02), "   Misc. Cost :",                            ~
               at (21,18), fac(hex(a4)), gs_misc$               , ch(10),~
               at (21,30), "Misc. Scrap:",                               ~
               at (21,42), fac(hex(a4)), gs_misc_s$             , ch(10),~
               at (21,57), "Total Misc:",                                ~
               at (21,69), fac(hex(a4)), gs_misc_tot$           , ch(10),~
                                                                         ~
               at (22,02), "Material Cost :",                            ~
               at (22,18), fac(hex(84)), gs_total$              , ch(10),~
               at (22,30), "Scrap Cost :",                               ~
               at (22,42), fac(hex(84)), gs_total_s$            , ch(10),~
               at (22,57), "Total Cost:",                                ~
               at (22,69), fac(hex(84)), tot_cost_gls$          , ch(10),~
                                                                         ~
               at (23,02), fac(hex(a4)),   disp_msg$            , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L62408
                  call "PRNTSCRN"
                  goto L61900

L62408:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        calc_locks
          if display_flag% = 0% then goto L62440
           call "SHOSTAT" ("Calculating Locks Cost")

L62440:    tot_cost_locks = 0.0
           rm_cnt%, rm_err% = 0%
           init(" ") rm_part$(), rm_cuts$(), rm_cost$(), rm_desc$(),     ~
                     rm_total$, hdr$()

           call "APCCST3B" (part$,            /* MFG PART NUMBER       */~
                            rm_part$(),       /* Inv Raw Mat'l Part No.*/~
                            rm_cuts(),        /* Total Inches in Decima*/~
                            rm_cost(),        /* Total Cost Raw Mat'l  */~
                            rm_desc$(),       /* Raw Mat'l Description */~
                            gs_units%(),      /* Units of Measure      */~
                            rm_unit(),        /* Raw Material Unit Cost*/~
                            rm_vinyl,         /* Raw Mat'l Cost Vinyl  */~
                            rm_misc,          /* Raw Mat'l Cost Vinyl  */~
                            rm_total,         /* Raw Material Tot Cost */~
                            rm_cnt%,          /* Raw Material Count    */~
                            rm_frt,           /* Save Freight Cost     */~
                            rm_vinyl_d,       /* Save Vinyl Disc Amt.  */~
                                   #4,        /* (GENCODES) - FILE     */~
                                   #2,        /* (HNYMASTR) - FILE     */~
                                   #3,        /* (HNYQUAN ) - FILE     */~
                            rm_err%   )       /* 0% = OK               */

           for i% = 1% to rm_cnt%
               convert rm_cuts(i%) to rm_cuts$(i%), pic(####.####-)
               convert rm_cost(i%) to rm_cost$(i%), pic(####.####-)
               convert gs_units%(i%) to gs_units$(i%), pic(#)
           next i%
           lk_vinyl = rm_vinyl
           lk_misc  = rm_misc
           tot_cost_locks = rm_total
           convert lk_vinyl to lk_vinyl$, pic($###.####-)
           convert lk_misc  to lk_misc$,  pic($###.####-)
           convert tot_cost_locks to tot_cost_locks$, pic($###.####-)
           tot_frt_cost   = tot_frt_cost   + rm_frt
           tot_vinyl_disc = tot_vinyl_disc + rm_vinyl_d
           if display_flag% = 0% then return

        REM - Raw Material Cost Display
            disp_txt$ = "MFG  Locks Material Cost"
            gosub set_display
            i% = 1%
            hdr$(1) = "Raw Mat'l "                             /* (10) */
            hdr$(2) = "<--- Raw Mat'l Description ---->"       /* (32) */
            hdr$(3) = "Mat'l Inch"                             /* (10) */
            hdr$(4) = "Total Cost"                             /* (10) */
            hdr$(5) = "U"                                      /* (01) */

            more$ = "(More-Data)"
            if (i% + 10%) >= rm_cnt% then more$ = "           "
L62640:     accept                                                       ~
               at (01,02),                                               ~
                  "Exploded Cost's for Specified Manufactured Part",     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,15), fac(hex(84)), apc_prt$               , ch(60),~
               at (04,02), "Part Number:",                               ~
               at (04,15), fac(hex(84)), part$                  , ch(25),~
               at (04,45), fac(hex(84)), part_desc$             , ch(30),~
                                                                         ~
               at (05,02), "(A)ct,(S)td:",                               ~
               at (05,15), fac(hex(84)), lb_typ$                , ch(01),~
               at (05,45), fac(hex(84)), lb_typ_d$              , ch(30),~
                                                                         ~
               at (06,24), fac(hex(94)), disp_txt$              , ch(30),~
               at (06,66), fac(hex(94)), more$                  , ch(11),~
                                                                         ~
               at (08,02), fac(hex(a4)), hdr$(1)                , ch(10),~
               at (08,15), fac(hex(a4)), hdr$(2)                , ch(32),~
               at (08,50), fac(hex(a4)), hdr$(3)                , ch(10),~
               at (08,63), fac(hex(a4)), hdr$(4)                , ch(10),~
               at (08,79), fac(hex(a4)), hdr$(5)                , ch(01),~
                                                                         ~
               at (09,02), fac(hex(84)), rm_part$(1%)           , ch(10),~
               at (09,15), fac(hex(84)), rm_desc$(1%)           , ch(32),~
               at (09,50), fac(hex(84)), rm_cuts$(1%)           , ch(10),~
               at (09,63), fac(hex(84)), rm_cost$(1%)           , ch(10),~
               at (09,79), fac(hex(84)), gs_units$(1%)          , ch(01),~
                                                                         ~
               at (10,02), fac(hex(84)), rm_part$(2%)           , ch(10),~
               at (10,15), fac(hex(84)), rm_desc$(2%)           , ch(32),~
               at (10,50), fac(hex(84)), rm_cuts$(2%)           , ch(10),~
               at (10,63), fac(hex(84)), rm_cost$(2%)           , ch(10),~
               at (10,79), fac(hex(84)), gs_units$(2%)          , ch(01),~
                                                                         ~
               at (11,02), fac(hex(84)), rm_part$(3%)           , ch(10),~
               at (11,15), fac(hex(84)), rm_desc$(3%)           , ch(32),~
               at (11,50), fac(hex(84)), rm_cuts$(3%)           , ch(10),~
               at (11,63), fac(hex(84)), rm_cost$(3%)           , ch(10),~
               at (11,79), fac(hex(84)), gs_units$(3%)          , ch(01),~
                                                                         ~
               at (12,02), fac(hex(84)), rm_part$(4%)           , ch(10),~
               at (12,15), fac(hex(84)), rm_desc$(4%)           , ch(32),~
               at (12,50), fac(hex(84)), rm_cuts$(4%)           , ch(10),~
               at (12,63), fac(hex(84)), rm_cost$(4%)           , ch(10),~
               at (12,79), fac(hex(84)), gs_units$(4%)          , ch(01),~
                                                                         ~
               at (20,02), "   Vinyl Cost :",                            ~
               at (20,18), fac(hex(84)), lk_vinyl$              , ch(10),~
                                                                         ~
               at (21,02), "   Misc. Cost :",                            ~
               at (21,18), fac(hex(a4)), lk_misc$               , ch(10),~
                                                                         ~
               at (22,02), "Material Cost :",                            ~
               at (22,18), fac(hex(84)), tot_cost_locks$        , ch(10),~
                                                                         ~
               at (23,02), fac(hex(a4)),   disp_msg$            , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L62904
                  call "PRNTSCRN"
                  goto L62640

L62904:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        calc_hardware
          if display_flag% = 0% then goto L62948
           if cst_type$ = "0" then                                       ~
              call "SHOSTAT" ("Calculating Secondary Hardware Cost")
           if cst_type$ = "1" then                                       ~
              call "SHOSTAT" ("Calculating Secondary Packaging Cost")

L62948:    if cst_type$ = "0" then tot_cost_hdr = 0.0
           if cst_type$ = "1" then tot_cost_pck = 0.0

           init(" ") rm_cuts$(), rm_cost$(), rm_total$, hdr$(),          ~
                     gs_units$()

        call "APCCST4B" (part$,          /* MFG Part Number           */ ~
                        cst_type$,       /* '0'=Hardware,'1'=Packaging*/ ~
                        rm_raw$(),       /* Inv. Raw Material Part    */ ~
                        rm_cuts(),       /* Total Inches in Decimal   */ ~
                        rm_cost(),       /* Total Cost Raw Mat. Part  */ ~
                        rm_desc$(),      /* Raw Material Description  */ ~
                        gs_units%(),     /* RAW MAT'L UNITS OF MEASURE*/ ~
                        rm_unit(),       /* Raw Material Unit Cost    */ ~
                        rm_vinyl,        /* Raw Mat'l Vinyl Cost      */ ~
                        rm_misc,         /* Raw Mat'l Misc. Cost      */ ~
                        rm_total,        /* Raw Material Total Cost   */ ~
                        rm_cnt%,         /* Raw Material Count        */ ~
                        rm_cuts_s(),     /* Total Inch Decima SCRA     */~
                        rm_cost_s(),     /* Total Cst Raw Mat SCRA     */~
                        rm_vinyl_s,      /* Vinyl Mat'l Cost Scrap     */~
                        rm_misc_s,       /* Misc Mat'l Cost Scrap      */~
                        rm_total_s,      /* Raw Mat Tot Cost SCRAP     */~
                        rm_eq$(),        /* Save Cal Typ and Eq.No     */~
                        rm_ph$(),        /* Save Phantom No.           */~
                        rm_frt,          /* Save Freight Cost          */~
                        rm_vinyl_d,      /* Save Vinyl Disc Amt.       */~
                              #9,        /* (APCCSTHP) - File         */ ~
                              #4,        /* (GENCODES) - File         */ ~
                              #3,        /* (HNYQUAN ) - File         */ ~
                              #2,        /* (HNYMASTR) - File         */ ~
                        rm_err% )        /* 0% = Ok, Non 0% = Error   */
           if cst_type$ = "0" then area$ = "5"                           ~
                              else area$ = "6"
           gosub calc_costmate

           hh_vinyl = rm_vinyl
           hh_misc  = rm_misc
           if cst_type$ <> "0" then goto L63128
              tot_cost_hdr = rm_total
              hh_vinyl_hdr = hh_vinyl
              hh_misc_hdr  = hh_misc
              tot_frt_cost   = tot_frt_cost   + rm_frt
              tot_vinyl_disc = tot_vinyl_disc + rm_vinyl_d
              goto L63152
L63128:    tot_cost_pck = rm_total
           hh_vinyl_pck = hh_vinyl
           hh_misc_pck  = hh_misc
           tot_frt_cost   = tot_frt_cost   + rm_frt
           tot_vinyl_disc = tot_vinyl_disc + rm_vinyl_d

L63152:    convert hh_vinyl to hh_vinyl$, pic($###.####-)
           convert hh_misc  to hh_misc$,  pic($###.####-)
           convert rm_total to rm_total$, pic($###.####-)

           if debug% = 0% then goto L63208
              mat lab = zer
              lab(1%) = rm_vinyl   : lab(2%)  = rm_misc
              lab(3%) = rm_total   : lab(4%)  = rm_vinyl_s
              lab(5%) = rm_misc_s  : lab(6%)  = rm_total_s
              lab(7%) = rm_frt     : lab(8%)  = rm_vinyl_d
              sel$ = "10"
              if cst_type$ = "1" then sel$ = "11"
              gosub display_screen

L63208:    if display_flag% = 0% then return

           for i% = 1% to rm_cnt%
               convert gs_units%(i%) to gs_units$(i%), pic(#)
               convert rm_cuts(i%) to rm_cuts$(i%), pic(####.####-)
               convert rm_cost(i%) to rm_cost$(i%), pic(####.####-)
           next i%

        REM - Raw Material Cost Display
           if cst_type$ = "0" then                                       ~
              disp_txt$ = "MFG  Secondary Hardware Cost  "
           if cst_type$ = "1" then                                       ~
              disp_txt$ = "MFG  Secondary Packaging Cost "
            gosub set_display
            i% = 1%
            hdr$(1) = "  Raw Mat'l   "
            hdr$(2) = "<--- Raw Mat'l Description ---->"
            hdr$(3) = "Total Inch"
            hdr$(4) = "Total Cost"
            hdr$(5) = "U"

L63292:     more$ = "(More-Data)"
            if (i% + 10%) >= rm_cnt% then more$ = "           "
L63300:     accept                                                       ~
               at (01,02),                                               ~
                  "Exploded Cost's for Specified Manufactured Part",     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,15), fac(hex(84)), apc_prt$               , ch(60),~
               at (04,02), "Part Number:",                               ~
               at (04,15), fac(hex(84)), part$                  , ch(25),~
               at (04,45), fac(hex(84)), part_desc$             , ch(30),~
                                                                         ~
               at (05,02), "(A)ct,(S)td:",                               ~
               at (05,15), fac(hex(84)), lb_typ$                , ch(01),~
               at (05,45), fac(hex(84)), lb_typ_d$              , ch(30),~
                                                                         ~
               at (06,24), fac(hex(94)), disp_txt$              , ch(30),~
               at (06,66), fac(hex(94)), more$                  , ch(11),~
                                                                         ~
               at (08,02), fac(hex(a4)), hdr$(1)                , ch(14),~
               at (08,19), fac(hex(a4)), hdr$(2)                , ch(32),~
               at (08,54), fac(hex(a4)), hdr$(3)                , ch(10),~
               at (08,67), fac(hex(a4)), hdr$(4)                , ch(10),~
               at (08,79), fac(hex(a4)), hdr$(5)                , ch(01),~
                                                                         ~
               at (09,02), fac(hex(84)), rm_raw$(i%)            , ch(14),~
               at (09,19), fac(hex(84)), rm_desc$(i%)           , ch(32),~
               at (09,54), fac(hex(84)), rm_cuts$(i%)           , ch(10),~
               at (09,67), fac(hex(84)), rm_cost$(i%)           , ch(10),~
               at (09,79), fac(hex(84)), gs_units$(i%)          , ch(01),~
                                                                         ~
               at (10,02), fac(hex(84)), rm_raw$(i%+1%)         , ch(14),~
               at (10,19), fac(hex(84)), rm_desc$(i%+1%)        , ch(32),~
               at (10,54), fac(hex(84)), rm_cuts$(i%+1%)        , ch(10),~
               at (10,67), fac(hex(84)), rm_cost$(i%+1%)        , ch(10),~
               at (10,79), fac(hex(84)), gs_units$(i%+1%)       , ch(01),~
                                                                         ~
               at (11,02), fac(hex(84)), rm_raw$(i%+2%)         , ch(14),~
               at (11,19), fac(hex(84)), rm_desc$(i%+2%)        , ch(32),~
               at (11,54), fac(hex(84)), rm_cuts$(i%+2%)        , ch(10),~
               at (11,67), fac(hex(84)), rm_cost$(i%+2%)        , ch(10),~
               at (11,79), fac(hex(84)), gs_units$(i%+2%)       , ch(01),~
                                                                         ~
               at (12,02), fac(hex(84)), rm_raw$(i%+3%)         , ch(14),~
               at (12,19), fac(hex(84)), rm_desc$(i%+3%)        , ch(32),~
               at (12,54), fac(hex(84)), rm_cuts$(i%+3%)        , ch(10),~
               at (12,67), fac(hex(84)), rm_cost$(i%+3%)        , ch(10),~
               at (12,79), fac(hex(84)), gs_units$(i%+3%)       , ch(01),~
                                                                         ~
               at (13,02), fac(hex(84)), rm_raw$(i%+4%)         , ch(14),~
               at (13,19), fac(hex(84)), rm_desc$(i%+4%)        , ch(32),~
               at (13,54), fac(hex(84)), rm_cuts$(i%+4%)        , ch(10),~
               at (13,67), fac(hex(84)), rm_cost$(i%+4%)        , ch(10),~
               at (13,79), fac(hex(84)), gs_units$(i%+4%)       , ch(01),~
                                                                         ~
               at (14,02), fac(hex(84)), rm_raw$(i%+5%)         , ch(14),~
               at (14,19), fac(hex(84)), rm_desc$(i%+5%)        , ch(32),~
               at (14,54), fac(hex(84)), rm_cuts$(i%+5%)        , ch(10),~
               at (14,67), fac(hex(84)), rm_cost$(i%+5%)        , ch(10),~
               at (14,79), fac(hex(84)), gs_units$(i%+5%)       , ch(01),~
                                                                         ~
               at (15,02), fac(hex(84)), rm_raw$(i%+6%)         , ch(14),~
               at (15,19), fac(hex(84)), rm_desc$(i%+6%)        , ch(32),~
               at (15,54), fac(hex(84)), rm_cuts$(i%+6%)        , ch(10),~
               at (15,67), fac(hex(84)), rm_cost$(i%+6%)        , ch(10),~
               at (15,79), fac(hex(84)), gs_units$(i%+6%)       , ch(01),~
                                                                         ~
               at (16,02), fac(hex(84)), rm_raw$(i%+7%)         , ch(14),~
               at (16,19), fac(hex(84)), rm_desc$(i%+7%)        , ch(32),~
               at (16,54), fac(hex(84)), rm_cuts$(i%+7%)        , ch(10),~
               at (16,67), fac(hex(84)), rm_cost$(i%+7%)        , ch(10),~
               at (16,79), fac(hex(84)), gs_units$(i%+7%)       , ch(01),~
                                                                         ~
               at (17,02), fac(hex(84)), rm_raw$(i%+8%)         , ch(14),~
               at (17,19), fac(hex(84)), rm_desc$(i%+8%)        , ch(32),~
               at (17,54), fac(hex(84)), rm_cuts$(i%+8%)        , ch(10),~
               at (17,67), fac(hex(84)), rm_cost$(i%+8%)        , ch(10),~
               at (17,79), fac(hex(84)), gs_units$(i%+8%)       , ch(01),~
                                                                         ~
               at (18,02), fac(hex(84)), rm_raw$(i%+9%)         , ch(14),~
               at (18,19), fac(hex(84)), rm_desc$(i%+9%)        , ch(32),~
               at (18,54), fac(hex(84)), rm_cuts$(i%+9%)        , ch(10),~
               at (18,67), fac(hex(84)), rm_cost$(i%+9%)        , ch(10),~
               at (18,79), fac(hex(84)), gs_units$(i%+9%)       , ch(01),~
                                                                         ~
               at (20,02), "   Vinyl Cost :",                            ~
               at (20,18), fac(hex(84)), hh_vinyl$              , ch(10),~
                                                                         ~
               at (21,02), "   Misc. Cost :",                            ~
               at (21,18), fac(hex(a4)), hh_misc$               , ch(10),~
                                                                         ~
               at (22,02), "Material Cost :",                            ~
               at (22,18), fac(hex(84)), rm_total$              , ch(10),~
                                                                         ~
               at (23,02), fac(hex(a4)),   disp_msg$            , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L63708
                  call "PRNTSCRN"
                  goto L63300

L63708:        if (i% + 10%) > rm_cnt% then goto L63724
                  i% = i% + 10%
                  goto L63292

L63724:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        calc_total_cost
            call "SHOSTAT" ( "Calculating Total MFG Cost" )
            display_flag% = 0%
            gosub calc_labor              /* Direct and Indirect Labor */
            gosub calc_material           /* Primary Materials lineal  */
            typ% = 0%
            gosub calc_glass              /* Calculate Glass/Swiggle   */
            typ% = 1%
            gosub calc_screen             /* Calc Screen, Mat'l, Strip */
            gosub calc_locks              /* Calculate Locks           */
            cst_type$ = "0"
            gosub calc_hardware           /* Calculate Misc. Hardware  */
            cst_type$ = "1"
            gosub calc_hardware           /* Calculate Misc. Packaging */
            display_flag% = 1%

           if display_flag% = 0% then return
           gosub calc_wood_surround       /* Calc Wood Surround/Fact   */
           gosub format_screen

        REM - Total Cost Display Screen
            disp_txt$ = "   MFG  T o t a l   C o s t   "
            gosub set_display

L63836:     accept                                                       ~
               at (01,02),                                               ~
                  "Exploded Cost's for Specified Manufactured Part",     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,15), fac(hex(84)), apc_prt$               , ch(60),~
               at (04,02), "Part Number:",                               ~
               at (04,15), fac(hex(84)), part$                  , ch(25),~
               at (04,45), fac(hex(84)), part_desc$             , ch(30),~
                                                                         ~
               at (05,02), "(A)ct,(S)td:",                               ~
               at (05,15), fac(hex(84)), lb_typ$                , ch(01),~
               at (05,45), fac(hex(84)), lb_typ_d$              , ch(30),~
                                                                         ~
               at (06,24), fac(hex(94)), disp_txt$              , ch(30),~
                                                                         ~
               at (08,02), "Labor    ( Direct/Indirect ):",              ~
               at (08,32), fac(hex(84)), sub_labor$             , ch(10),~
               at (08,50), fac(hex(84)), sub_pcnt$              , ch(07),~
               at (08,58), "%",                                          ~
               at (08,65), "Terms Disc",                                 ~
               at (09,02), "         ( O v e r h e a d ):",              ~
               at (09,32), fac(hex(a4)), ovr_labor$             , ch(10),~
               at (09,50), fac(hex(a4)), ovr_pcnt$              , ch(07),~
               at (09,58), "%",                                          ~
               at (10,02), "         (    T o t a l    ):",              ~
               at (10,32), fac(hex(84)), tot_labor$             , ch(10),~
               at (10,50), fac(hex(84)), labor_pcnt$            , ch(07),~
               at (10,58), "%",                                          ~
               at (12,02), "Material (    V i n y l    ):",              ~
               at (12,32), fac(hex(84)), total_vinyl$           , ch(10),~
               at (12,50), fac(hex(84)), vinyl_pcnt$            , ch(07),~
               at (12,58), "%",                                          ~
               at (12,65), fac(hex(84)), vinyl_disc$            , ch(10),~
               at (13,02), "         (    G l a s s    ):",              ~
               at (13,32), fac(hex(84)), total_glass$           , ch(10),~
               at (13,50), fac(hex(84)), glass_pcnt$            , ch(07),~
               at (13,58), "%",                                          ~
               at (14,02), "         (   S c r e e n   ):",              ~
               at (14,32), fac(hex(84)), total_screen$          , ch(10),~
               at (14,50), fac(hex(84)), screen_pcnt$           , ch(07),~
               at (14,58), "%",                                          ~
               at (15,02), "         ( H a r d w a r e ):",              ~
               at (15,32), fac(hex(84)), total_hardware$        , ch(10),~
               at (15,50), fac(hex(84)), hardware_pcnt$         , ch(07),~
               at (15,58), "%",                                          ~
               at (16,02), "         (P a c k a g i n g):",              ~
               at (16,32), fac(hex(84)), total_packaging$       , ch(10),~
               at (16,50), fac(hex(84)), packaging_pcnt$        , ch(07),~
               at (16,58), "%",                                          ~
               at (17,02), "         (    M i s c .    ):",              ~
               at (17,32), fac(hex(a4)), total_misc$            , ch(10),~
               at (17,50), fac(hex(a4)), misc_pcnt$             , ch(07),~
               at (17,58), "%",                                          ~
               at (18,02), "         (    T o t a l    ):",              ~
               at (18,32), fac(hex(84)), total_mat$             , ch(10),~
               at (18,50), fac(hex(84)), material_pcnt$         , ch(07),~
               at (18,58), "%",                                          ~
               at (18,65), fac(hex(84)), total_mat_disc$        , ch(10),~
                                                                         ~
               at (20,02), "         (  F r e i g h t  ):",              ~
               at (20,32), fac(hex(84)), frt_cost$              , ch(10),~
               at (20,50), fac(hex(84)), frt_pcnt$              , ch(07),~
               at (20,58), "%",                                          ~
                                                                         ~
               at (21,02), "Total    ( M F G   C o s t ):",              ~
               at (21,32), fac(hex(84)), total_mfg$             , ch(10),~
                                                                         ~
               at (21,50), "(Disc. Cost):",                              ~
               at (21,65), fac(hex(84)), total_mfg_disc$        , ch(10),~
                                                                         ~
               at (23,02), fac(hex(a4)),   disp_msg$            , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L64160
                  call "PRNTSCRN"
                  goto L63836

L64160:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        format_screen
            vinyl_disc, total_mat_disc, total_mfg_disc = 0.0
            frt_cost  = round( tot_frt_cost, 4)
            w_f_cost  = round( w_f_cost,     4)
            total_mfg = round( tot_labor + tot_cost_mat + tot_cost_gls + ~
                                         tot_cost_scr + tot_cost_locks + ~
                                         tot_cost_hdr + tot_cost_pck   + ~
                                         frt_cost + w_f_cost, 4)
            total_vinyl = round( tot_cost_vny + gs_vinyl_gls +           ~
                                 gs_vinyl_scr + lk_vinyl + hh_vinyl_hdr +~
                                 hh_vinyl_pck, 4)
            total_glass     = round( gs_misc_gls, 4)
            total_screen    = round( gs_misc_scr, 4)
            total_hardware  = round( lk_misc + hh_misc_hdr, 4)
            total_packaging = round( hh_misc_pck, 4 )
            total_misc      = round( tot_cost_msc, 4)
            total_mat = round(total_vinyl + total_glass + total_screen + ~
                              total_hardware + total_packaging +         ~
                              total_misc, 4)
          if total_mfg = 0 then total_mfg = .0000001
          sub_pcnt       = round((sub_labor / total_mfg) * 100.0, 2)
          ovr_pcnt       = round((ovr_labor / total_mfg) * 100.0, 2)
          labor_pcnt     = round((tot_labor / total_mfg) * 100.0, 2)
          vinyl_pcnt     = round((total_vinyl / total_mfg) * 100.0, 2)
          glass_pcnt     = round((total_glass / total_mfg) * 100.0, 2)
          screen_pcnt    = round((total_screen / total_mfg) * 100.0, 2)
          hardware_pcnt  = round((total_hardware / total_mfg) * 100.0, 2)
          packaging_pcnt = round((total_packaging/total_mfg) * 100.0, 2)
          misc_pcnt      = round((total_misc / total_mfg) * 100.0, 2)
          material_pcnt  = round((total_mat / total_mfg) * 100.0, 2)
          frt_pcnt       = round((frt_cost / total_mfg) * 100.0, 2)

          convert sub_labor to sub_labor$, pic($###.####-)
          convert ovr_labor to ovr_labor$, pic($###.####-)
          convert tot_labor to tot_labor$, pic($###.####-)

          convert total_mfg to total_mfg$,             pic($###.####-)
          convert total_vinyl to total_vinyl$,         pic($###.####-)
          convert total_glass to total_glass$,         pic($###.####-)
          convert total_screen to total_screen$,       pic($###.####-)
          convert total_hardware to total_hardware$,   pic($###.####-)
          convert total_packaging to total_packaging$, pic($###.####-)
          convert total_misc to total_misc$,           pic($###.####-)
          convert total_mat to total_mat$,             pic($###.####-)
          convert frt_cost to frt_cost$,               pic($###.####-)
          convert w_f_cost to w_f_cost$,               pic($###.####-)
                                         /* SPECIAL FOR TERMS DISCOUNT */

          vinyl_disc     = round(tot_vinyl_disc, 4)
          total_mat_disc = round(total_mat - vinyl_disc, 4)
          total_mfg_disc = round(total_mfg - vinyl_disc, 4)

          convert vinyl_disc to vinyl_disc$,           pic($###.####-)
          convert total_mat_disc to total_mat_disc$,   pic($###.####-)
          convert total_mfg_disc to total_mfg_disc$,   pic($###.####-)

          convert sub_pcnt to sub_pcnt$,     pic(###.##-)
          convert ovr_pcnt to ovr_pcnt$,     pic(###.##-)
          convert labor_pcnt to labor_pcnt$, pic(###.##-)

          convert vinyl_pcnt to vinyl_pcnt$,         pic(###.##-)
          convert glass_pcnt to glass_pcnt$,         pic(###.##-)
          convert screen_pcnt to screen_pcnt$,       pic(###.##-)
          convert hardware_pcnt to hardware_pcnt$,   pic(###.##-)
          convert packaging_pcnt to packaging_pcnt$, pic(###.##-)
          convert misc_pcnt to misc_pcnt$,           pic(###.##-)
          convert material_pcnt to material_pcnt$,   pic(###.##-)
          convert frt_pcnt to frt_pcnt$,             pic(###.##-)

        return

        calc_costmate                       /* Exception Utility       */
           x_err% = 0%
           call "APCCST6B" ( area$,         /* Area To Assign Def      */~
                             part$,         /* MFG Part Number         */~
                            rm_raw$(),      /* Inv Raw Material Part No*/~
                            rm_cuts(),      /* Total Inchec in Decimal */~
                            rm_cost(),      /* Total Cost Rsw Mat'l Par*/~
                            rm_desc$(),     /* Raw Material Description*/~
                            gs_units%(),    /* Raw Mat'l Units of Meas */~
                            rm_unit(),      /* Raw Mat'l Unit Cost     */~
                            rm_vinyl,       /* Raw Mat'l Vinyl Cost    */~
                            rm_misc,        /* Raw Mat'l Misc Cost     */~
                            rm_total,       /* Raw Mat'l Total Cost    */~
                            rm_cnt%,        /* Raw Mat'l Count (Pieces)*/~
                            rm_cuts_s(),    /* Tot Inch Decimal Scrap  */~
                            rm_cost_s(),    /* Tot Cost Raw Mat'l Scrap*/~
                            rm_vinyl_s,     /* Raw Mat'l Vinyl Scrap Co*/~
                            rm_misc_s,      /* Raw Mat'l Misc Scrap Cos*/~
                            rm_total_s,     /* Raw Mat'l Tot Cost Scrap*/~
                            rm_eq$(),       /* Save Type and Eq. No.   */~
                            rm_ph$(),       /* Save Eq. Phantom Code   */~
                            rm_frt,         /* Save Freight Cost       */~
                            rm_vinyl_d,     /* Save Vinyl Discount Amt */~
                             #1,            /*   (APCCUTEQ)            */~
                             #2,            /*   (HNYMASTR)            */~
                             #3,            /*   (HNYQUAN)             */~
                             #4,            /*   (GENCODES)            */~
                             #5,            /*   (AMTBOMCD)            */~
                             #7,            /*   (APCEMPLY)            */~
                             #8,            /*   (APCEQUAT)            */~
                             #9,            /*   (APCCSTHP)            */~
                             #10,           /*   (APCCSTLR)            */~
                             #11,           /*   (APCCSTEX)            */~
                             #12,           /*   (APCSTOCK)            */~
                             x_err% )       /* 0% = Ok, 1% = Error     */
        return

        calc_wood_surround
            w_f_cost = 0.0 : value$ = "0.0"
            if len(part$) < 22 then goto L64664
            if str(part$,1%,1%) = "9" then goto L64664
            if len(part$) = 22 then wood_code$ = str(part$,20%,3%)       ~
                               else wood_code$ = str(part$,23%,3%)
            if wood_code$ = "000" then goto L64664
               readkey$ = " " : value$ = "0.0"
               str(readkey$,1%,9%) = "COST WOOD"
               str(readkey$,10%,3%) = wood_code$
               read #4,key = readkey$, using L64652,value$, eod goto L64656
L64652:           FMT POS(25), CH(10)
L64656:        convert value$ to w_f_cost, data goto L64660
L64660:                                    /* Fixed Cost Wood Surround */
L64664: return

        display_screen
           call "APCPLNDD" ( sel$,       /* Debug Screen Selection     */~
                             cuscode$,   /* Customer Code for Pricing  */~
                             part$,      /* MFG Part Number            */~
                             lab(),      /* Labor Costs (1 thru 10)    */~
                             avg_pay(),  /* Avg Pay per Dept           */~
                             uph(),      /* Avg Units Per Manhour Dept */~
                             tc(),       /* Material Costs (1 thru 20) */~
                             tt(),       /* Total Cost Buckets         */~
                             rm_mat(),   /* Material Costs (1 thru 10) */~
                             rm_mats(),  /* Mat'l Scrap Costs(1 thru 9)*/~
                             apc_err%()) /* 0% = Ok, Non Zero Error    */

        return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
