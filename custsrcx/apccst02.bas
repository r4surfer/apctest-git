        REM *************************************************************~
            *                                                           *~
            *   AAA   PPPP    CCC    CCC    SSSSS  TTTTT   000    222   *~
            *  A   A  P   P  C   C  C   C  S         T    0   0  2   2  *~
            *  AAAAA  PPPP   C      C        S       T    0   0    2    *~
            *  A   A  P      C   C  C   C       S    T    0   0   2     *~
            *  A   A  P       CCC    CCC   SSSSS     T     000   22222  *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * APCCST02 - Costing Report Utility Program.                *~
            *                                                           *~
            *      Routines - (APCCST5B) - Sub to Calculate the Direct  *~
            *                              and Indirect Labor Cost      *~
            *                              Assoc. with a MFG Part.      *~
            *                     Tables - 'CAPACITY ', 'COST 01LB'     *~
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
            * 08/08/94 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 10/02/95 ! Mod to Use Calculated Freight and Vinyl  ! RHH *~
            *          !   Discount Amount.                       !     *~
            * 11/11/97 ! Mod for New Release Upgrade to R6.04.03  ! RHH *~
            *          !                                          !     *~
            * 03/12/98 ! y2k conversion                           ! djd *~
            * 10/31/05 ! (AWD001) CR347 Mod for Sub Part          ! CMG *~
            *************************************************************

        dim                              /* Report Variables           */~
            lb_typ$1, lb_typ_d$30,       /* LAB TYPE (S)tandard (A)ctua*/~
            bg_mod$3, bg_mod_d$30,       /* Beginning Model Code all   */~
            ed_mod$3, ed_mod_d$30,       /* Ending MOdel Code          */~
            adj_mat$4,                   /* Material Adjustment        */~
            cst_mat$7,                   /* Material Cost              */~
            cst_lab$7,                   /* Labor Cost - Direct/Labor  */~
            cst_ovr$7,                   /* Labor Overhead             */~
            cst_tot$7,                   /* Total of all Pieces        */~
            ind_pct$7,                   /* Independent Discount Pcnt  */~
            ind_prc$7,                   /* INDEPENDENT Price          */~
            ind_gpft$7,                  /* Independent Gross Profit   */~
            ind_gpcnt$2,                 /* Independent Percent        */~
            x_pct$7,                     /* 1st Store Advertising Pcnt */~
            x_prc$7,                     /* 1st Store Price            */~
            x_gpft$7,                    /* 1st Store Gross Profit     */~
            x_gpcnt$2,                   /* 1st Store GP Pcnt          */~
            y_pct$7,                     /* 2nd Store Advertising Pcnt */~
            y_prc$7,                     /* 2nd Store Price            */~
            y_gpft$7,                    /* 2nd Store Gross Profit     */~
            y_gpcnt$2,                   /* 2nd Store GP Pcnt          */~
            store1$9, store1_d$30,       /* 1st Store Code             */~
            store2$9, store2_d$30,       /* 2nd Store Code             */~
            apc_key$25, cd$1,            /* Gencodes Primary Key       */~
            apc_desc$30                  /* Gencodes Description       */

        dim                              /* (Program) - Variables      */~
            part$25, part_desc$32,       /*                            */~
            partno1$20,                  /* Sub Part1        (AWD001)  */~
            mod$3,                       /*                            */~
            apc_scr$120,                 /*                            */~
            apc_prt$60,                  /*                            */~
            apc_sze$20,                  /*                            */~
            title$40, date$8,            /* REPORT TITLE               */~
            runtime$8, code$3,           /* REPORT RUN TIME            */~
            readkey$50, desc$30,         /* Generic Key                */~
            cursor%(2%),                 /* Cursor location for edit   */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim                              /* Costing Variables          */~
            area$1,                      /* COST MATE                  */~
            w_f_cost$10, value$10,       /* WOOD SURROUND / FAC MULL   */~
            wood_code$3,                 /* WOOD SURROUND CODE         */~
            avg_pay(15%),                /* Avg Dept Pay Rates         */~
            uph(15%),                    /* Unit Per Manhour by Dept   */~
            rm_raw$(100%)14,             /*INV. RAW MAT'L PART         */~
            rm_part$(100%)10,            /* Inventory Raw Mat No.      */~
            rm_cuts(100%),               /* Total Inches in Decimal    */~
            rm_cost(100%),               /* Total Cost Raw Material    */~
            rm_desc$(100%)32,            /* Raw Material Description   */~
            rm_cuts_s(100%),             /* Total Inch Desc. Scrap     */~
            rm_cost_s(100%),             /* Total Cost Raw Mat Scrap   */~
            rm_unit(100%),               /* Raw Mat'l Unit Cost        */~
            rm_eq$(100%)3,               /* Calc Eq. No.               */~
            rm_ph$(100%)5,               /* Save Phantom Number        */~
            gs_qty(9%),                  /* G/S Material Units         */~
            gs_raw$(9%)25,               /* G/S Material Parts         */~
            gs_desc$(9%)32,              /* G/S Material Description   */~
            gs_cost(9%),                 /* Raw Materal Cost           */~
            gs_units%(100%),             /* Raw Material Unit Measure  */~
            gs_unit(100%),               /* RAW MAT'L UNIT COST        */~
            gs_qty_s(9%),                /* G/S Material Scrap         */~
            gs_cost_s(9%),               /* G/S Material Cost Scrap    */~
            cst_typ$1                    /* 0 = Hardware, 1 = Package  */

        dim                              /* Pricing Variables          */~
            size$1,                      /* (O)pening, (E)xact         */~
            pc(36%),                     /* Calc Dealer Price Catalog  */~
            upd$1,                       /* Update Prices Y or N       */~
            cuscode$9,                   /* Customer Code              */~
            ref$(15%)2,                  /* Ref Type Codes Catalog     */~
            ref1$(15%)2,                 /* Ref Type Codes Spec. Cat.  */~
            ref_p(15%),                  /* Ref Prices APC CAtalog     */~
            ref_p1(15%)                  /* Ref Prices Spec. Cat.      */

        dim f2%(20%),                    /* = 0 if the file is open    */~
            f1%(20%),                    /* = 1 if READ was successful */~
            fs%(20%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(20%)20                 /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = " Costing Master Utility Edit and Report "
            pname$ = "APCCST02 - Rev: R6.04"

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
            * #03 ! HNYQUAN  ! Inventory Quantities Master File         *~
            * #04 ! GENCODES ! Master Code Table File                   *~
            * #05 ! AMTBOMCD ! Master Equation File                     *~
            * #06 ! AMTBOMIF ! Master Part Validity File                *~
            * #07 ! APCEMPLY ! Employee Master File                     *~
            * #08 ! APCEQUAT ! Equation an Parts Cross Reference File   *~
            * #09 ! APCCSTHP ! Hardware and Packaging Costing Components*~
            * #10 ! APCCSTLR ! Departments Average Hourly Rates         *~
            * #11 ! CPRPRICE ! Master System Price File                 *~
            * #12 ! CUSTOMER ! Master Customer File                     *~
            * #13 ! APCPCMST ! Pricing Definition file                  *~
            * #14 ! APCSKUNO ! Home Center's Skuno File                 *~
            * #15 ! APCPCMSK ! Pricing Key Definition File              *~
            * #16 ! APCPCMSD ! Pricing Master Calc Definition File      *~
            * #17 ! APCCSTEX ! APC COSTING EXCEPTION FILE DEF'S         *~
            * #18 ! APCSTOCK ! APC MASTER STOCK FILE                    *~
            * #20 ! APCCSTWK ! Costing Report Work File                 *~
            * #21 ! APCPLNDP !                                          *~
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

            select #11, "CPRPRICE"                                       ~
                        varc,     indexed,  recsize = 700,               ~
                        keypos = 1,    keylen =  47

            select #12, "CUSTOMER",                                      ~
                        varc,     indexed,  recsize =  1200,             ~
                        keypos =  1,   keylen =  9,                      ~
                        alt key  1, keypos  =    10, keylen = 30, dup,   ~
                            key  2, keypos  =   424, keylen =  9, dup,   ~
                            key  3, keypos  =   771, keylen =  9, dup,   ~
                            key  4, keypos  =   780, keylen =  9, dup

            select #13, "APCPCMST"                                       ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos = 9,    keylen =  53,                     ~
                        alt key  1, keypos  =     1, keylen = 8

            select #14, "APCSKUNO"                                       ~
                        varc,     indexed,  recsize =  73,               ~
                        keypos = 1,    keylen =  28,                     ~
                        alt key  1, keypos  =    29, keylen = 28, dup

            select #15, "APCPCMSK"                                       ~
                        varc,     indexed,  recsize =  64,               ~
                        keypos = 1,    keylen =   5

            select #16, "APCPCMSD"                                       ~
/*AWD001*/              varc,     indexed,  recsize =  768,              ~
                        keypos = 1,    keylen =   9

            select #17,  "APCCSTEX",                                     ~
                        varc,     indexed,  recsize = 1100,              ~
                        keypos =    1, keylen =    9

            select #18, "APCSTOCK",                                      ~
                        varc,     indexed,  recsize =  70,               ~
                        keypos =    1, keylen =  32,                     ~
                        alt key  1, keypos =   8, keylen = 32

            select #20, "APCCSTWK",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =    1, keylen =  25

            select #21, "APCPLNDP",                                      ~
                        varc,     indexed,  recsize =  32,               ~
                        keypos =   11, keylen =  12,                     ~
                        alt key  1, keypos =   9, keylen = 14,           ~
                            key  2, keypos =   4, keylen = 12,           ~
                            key  3, keypos =   1, keylen = 15

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),  0%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),  0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),  0%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%),  0%, rslt$(4%))
            call "OPENCHCK" (#5, fs%(5%), f2%(5%),  0%, rslt$(5%))
            call "OPENCHCK" (#6, fs%(6%), f2%(6%),  0%, rslt$(6%))
            call "OPENCHCK" (#7, fs%(7%), f2%(7%),  0%, rslt$(7%))
            call "OPENCHCK" (#8, fs%(8%), f2%(8%),  0%, rslt$(8%))
            call "OPENCHCK" (#9, fs%(9%), f2%(9%),  0%, rslt$(9%))
            call "OPENCHCK" (#10, fs%(10%), f2%(10%),  0%, rslt$(10%))
            call "OPENCHCK" (#11, fs%(11%), f2%(11%),  0%, rslt$(11%))
            call "OPENCHCK" (#12, fs%(12%), f2%(12%),  0%, rslt$(12%))
            call "OPENCHCK" (#13, fs%(13%), f2%(13%),  0%, rslt$(13%))
            call "OPENCHCK" (#14, fs%(14%), f2%(14%),  0%, rslt$(14%))
            call "OPENCHCK" (#15, fs%(15%), f2%(15%),  0%, rslt$(15%))
            call "OPENCHCK" (#16, fs%(16%), f2%(16%),  0%, rslt$(16%))
            call "OPENCHCK" (#17, fs%(17%), f2%(17%), 10%, rslt$(17%))
            call "OPENCHCK" (#18, fs%(18%), f2%(18%), 10%, rslt$(18%))
            call "OPENCHCK" (#21, fs%(21%), f2%(21%), 10%, rslt$(21%))

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 9%
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
                  if keyhit%  = 16% then gosub exit_program
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 3%
            if fieldnr% < 1% or fieldnr% > 9% then editpg1
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
            mode% = 1% : gosub open_work
            mode% = 3% : gosub open_work
            gosub select_printer
            gosub generate_report
            close printer
            gosub delete_work
        return clear all
        goto inputmode

        select_printer
            runtime$ = " "
            call "TIME" (runtime$)
            select printer (134)
            pageno% = 0%
            lcntr% = 99%
        return

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
         "Enter an (S)tandard or (A)ctual for the Costimg Method to Use",~
         "Enter a Beginning Model Product Code or (ALL)?               ",~
         "Enter a Ending Model Product Code?                           ",~
         "Enter a Valid Independent Price Discount Percent in Decimal? ",~
         "Enter a Valid Store One Customer Code?                       ",~
         "Enter a Valid Store One Advertising Discount Percent in Dec.?",~
         "Enter a Valid Store Two Customer Code?                       ",~
         "Enter a Valid Store Two Advertising Discount Percent in Dec.?",~
         "Enter a Valid Terms Vinyl Discount Percent in Decimal?       "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, readkey$, lb_typ$, desc$,  ~
                      lb_typ_d$, bg_mod$, bg_mod_d$, ed_mod$, ed_mod_d$, ~
                      cst_mat$, cst_lab$, cst_ovr$, cst_tot$, ind_pct$,  ~
                      ind_prc$, ind_prc$, ind_gpft$, ind_gpcnt$,         ~
                      x_pct$, x_prc$, x_gpft$, x_gpcnt$, y_pct$,         ~
                      y_prc$, y_gpft$, y_gpcnt$, apc_key$, apc_desc$,    ~
                      title$, runtime$, store1$, store2$, store1_d$,     ~
                      store2_d$, partno1$
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
        REM DATALOAD

        REM RETURN

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
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
              gosub set_pf1

              gosub'050(1%, fieldnr%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40230,         /* (S)tandard,(A)ctual  */~
                                L40230,         /* Beginning Model Code */~
                                L40230,         /* Ending Model Code    */~
                                L40230,         /* Independant Discount */~
                                L40230,         /* 1st Store Customer Cd*/~
                                L40230,         /* 1st Store Advertise %*/~
                                L40230,         /* 2nd Store Customer Cd*/~
                                L40230          /* 2nd Store Advertise %*/
              goto L40260

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40230:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40260:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,21), fac(hex(a4)), apc$                   , ch(40),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "(S)tandard, (A)ctual Method:",               ~
               at (04,31), fac(lfac$(1%)), lb_typ$              , ch(01),~
               at (04,40), fac(hex(84)), lb_typ_d$              , ch(30),~
                                                                         ~
               at (05,02), "Beginning Model Code (ALL) :",               ~
               at (05,31), fac(lfac$(2%)), bg_mod$              , ch(03),~
               at (05,40), fac(hex(84)), bg_mod_d$              , ch(30),~
                                                                         ~
               at (06,02), "Ending Model Code          :",               ~
               at (06,31), fac(lfac$(3%)), ed_mod$              , ch(03),~
               at (06,40), fac(hex(84)), ed_mod_d$              , ch(30),~
                                                                         ~
               at (07,02), "Independent Price Percent  :",               ~
               at (07,31), fac(lfac$(4%)), ind_pct$             , ch(07),~
                                                                         ~
               at (08,02), "1st Store Customer Code    :",               ~
               at (08,31), fac(lfac$(5%)), store1$              , ch(09),~
               at (08,42), fac(hex(84)), store1_d$              , ch(30),~
                                                                         ~
               at (09,02), "1st Store Advertising Pcnt :",               ~
               at (09,31), fac(lfac$(6%)), x_pct$               , ch(07),~
                                                                         ~
               at (10,02), "2nd Store Customer Code    :",               ~
               at (10,31), fac(lfac$(7%)), store2$              , ch(09),~
               at (10,42), fac(hex(84)), store2_d$              , ch(30),~
                                                                         ~
               at (11,02), "2nd Store Advertising Pcnt :",               ~
               at (11,31), fac(lfac$(8%)), y_pct$               , ch(07),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L40730
                  call "PRNTSCRN"
                  goto L40260

L40730:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40920     /*  Input Mode             */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                   (4)Previous Field    " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffff0e0f1000)
            if fieldnr% = 1% then L40880
               str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L40880:     if fieldnr% > 1% then L40900
               str(pf$(2%),20%,18%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40900: return

L40920: if fieldnr% > 0% then L41020  /*  Edit Mode - Select Fld */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
        return

L41020:     pf$(1%)= "(1)Start Over                           " &        ~
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
            on fieldnr% gosub L50190,         /* (S)TANDARD,(A)CTUAL   */ ~
                              L50310,         /* Beg Model Code        */ ~
                              L50460,         /* End Model Code        */ ~
                              L50610,         /* Indpendent Price Pcnt */ ~
                              L50730,         /* Store One Customer Cod*/ ~
                              L50890,         /* Store One Advertise % */ ~
                              L51010,         /* Store Two Customer Cod*/ ~
                              L51160          /* Store Two Advertise % */
            return

L50190: REM Standard / Actual Costing Method      LB_TYP$, LB_TYP_D$
            if lb_typ$ <> " " then goto L50230
               lb_typ$ = "A"

L50230:     if lb_typ$ <> "S" and lb_typ$ <> "A" then goto L50270
               if lb_typ$ = "S" then lb_typ_d$ ="Standard Costing Method"
               if lb_typ$ = "A" then lb_typ_d$ ="Actual Costing Method"
        return
L50270:     errormsg$ = "(Error) - Invalid Costing Method Selected??"
            lb_typ$, lb_typ_d$ = " "
        return

L50310: REM Beginning Mode Code              BG_MOD$, BG_MOD_D$
            if bg_mod$ <> " " then goto L50360
L50330:        bg_mod$ = "ALL"
               bg_mod_d$  = "(ALL) Model Codes in Table"
               return
L50360:     if str(bg_mod$,1%,1%) = "A" then goto L50330
            code$ = bg_mod$
            gosub check_model
            if code% = 0% then goto L50420
            bg_mod_d$ = desc$
        return
L50420:     errormsg$ = "(Error) - Invalid Model Code "
            bg_mod$, bg_mod_d$ = " "
        return

L50460: REM Ending                           ED_MOD$, Ed_MOD_D$
            if ed_mod$ <> " " then goto L50510
L50480:        ed_mod$ = "ALL"
               bg_mod_d$  = "(ALL) Model Codes in Table"
               return
L50510:     if str(bg_mod$,1%,1%) = "A" then goto L50480
            code$ = ed_mod$
            gosub check_model
            if code% = 0% then goto L50570
            ed_mod_d$ = desc$
        return
L50570:     errormsg$ = "(Error) - Invalid Model Code "
            bg_mod$, bg_mod_d$ = " "
        return

L50610: REM Independent Price Discount        IND_PCT$,
            convert ind_pct$ to ind_pct, data goto L50690

            convert ind_pct to ind_pct$, pic(###.##-)

            if ind_pct < .00 or ind_pct > .99 then goto L50690

        return
L50690:   errormsg$="(Error) - Invalid Price Discount Percent?"
          ind_pct$ = " "
        return

L50730: REM 1st Customer Code                STORE1$, STORE1_D$
            cuscode$ = " " : srce1% = 0%
            str(cuscode$,1%,9%) = store1$
            read #12,key = cuscode$, eod goto L50850
                get #12, using L50780, store1_d$, cd$
L50780:       FMT POS(10), CH(30), POS(525), CH(1)


            if cd$ >= "A" and cd$ <= "Z" then                            ~
               srce1% = val(cd$) - 64%          /* (01) Thru (26) */     ~
               else  srce1% = val(cd$) - 21%    /* (27) Thru (36) */
        return
L50850:     errormsg$ = "(Error) - Invalid Store (1) Customer Code?"
            store1$, store1_d$ = " "
        return

L50890: REM 1st Store Advertising             X_PCT$,
            convert x_pct$ to x_pct, data goto L50970

            convert x_pct to x_pct$, pic(###.##-)

            if x_pct < .00 or x_pct > .99 then goto L50970

        return
L50970:   errormsg$="(Error) - Invalid Advertising Percentage?"
          x_pct$ = " "
        return

L51010: REM 2nd Customer Code                STORE2$, STORE2_D$
            cuscode$ = " " : srce2% = 0%
            str(cuscode$,1%,9%) = store2$
            read #12,key = cuscode$, eod goto L51120
                get #12, using L51060, store2_d$, cd$
L51060:       FMT POS(10), CH(30), POS(525), CH(1)

            if cd$ >= "A" and cd$ <= "Z" then                            ~
               srce2% = val(cd$) - 64%          /* (01) Thru (26) */     ~
               else  srce2% = val(cd$) - 21%    /* (27) Thru (36) */
        return
L51120:     errormsg$ = "(Error) - Invalid Store (2) Customer Code?"
            store2$, store2_d$ = " "
        return

L51160: REM Store (2) Advertising Pcnt        Y_PCT$,
            convert y_pct$ to y_pct, data goto L51240

            convert y_pct to y_pct$, pic(###.##-)

            if y_pct < .00 or y_pct > .99 then goto L51240

        return
L51240:   errormsg$="(Error) - Invalid Advertising Percentage?"
          y_pct$ = " "
        return

        check_model
           code% = 0%
           readkey$ = " "
           str(readkey$,1%,9%)    = "MODEL    "
           str(readkey$,10%,15%)  = code$
           read #4,key = readkey$, using L51460, desc$, eod goto L51480
L51460:        FMT POS(25), CH(30)
           code% = 1%
L51480: return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

L55040: %+---------------------------------------------------------------~
        ~----------------------------------------------------------------+
L55060: %!---------------------------------------------------------------~
        ~----------------------------------------------------------------!
L55080: %! ######## @ ########                        ###################~
        ~#####################                                Page: #### !

L55110: %! Costing Method: ##############################                ~
        ~                                        Ind Multiplier: ####### !
        %!                                                               ~

L55150: %!<------ Description ----------->!Mater'l! Labor !Over Hd!Tot Cs~
        ~t! Price !GProfit! %% !1st Prc!1st Gpt! %% !2nd Prc!2nd Gpt! %% !

L55180: %!** ######################### ***!       !       !       !      ~
        ~ !       !       !    !       !       !    !       !       !    !

L55210: %!################################!#######!#######!#######!######~
        ~#!#######!#######! ##%!#######!#######! ##%!#######!#######! ##%!

L55240: %!--------------------------------!-------!-------!-------!------~
        ~-!-------!-------!----!-------!-------!----!-------!-------!----!
        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        print_header
           if lcntr% <> 99% then print using L55040
           print page
           pageno% = pageno% + 1%
           print using L55040
           print using L55080, date$, runtime$, title$, pageno%
           print using L55110, lb_typ_d$, ind_pct$
           print using L55060
           print using L55150
           lcntr% = 5%
        return

        print_detail
           if lcntr% > 57% then gosub print_header
              print using L55240
              print using L55180, part$
              print using  L55210, part_desc$, cst_mat$, cst_lab$,        ~
                                cst_ovr$, cst_tot$, ind_prc$, ind_gpft$, ~
                                  ind_gpcnt$, x_prc$, x_gpft$, x_gpcnt$, ~
                                  y_prc$, y_gpft$, y_gpcnt$

              lcntr% = lcntr% + 3%
        return

        generate_report
           call "SHOSTAT" ("Sorting Products")
           gosub sort_product
           title$ = "***** APC Costing Analysis Report ******"
           call "SHOSTAT" ("Calculating Product Costs")
           apc_key$ = " "
           read #20,key > apc_key$, using L60200, part$, part_desc$,      ~
                                         adj_mat$, eod goto generate_done
           goto L60205
        generate_nxt
           read #20, using L60200, part$, part_desc$, adj_mat$,           ~
                                                   eod goto generate_done
L60200:       FMT POS(26), CH(25), CH(32), CH(4)
L60205:    gosub calc_cost
           str(cuscode$,1%,9%) = store1$     /* Set Store (1)         */
           gosub calc_price
                                             /* Catalog / Independent */
                                             /* Independent - Price   */
                                             /* Ind Gross Profit      */
                                             /* Ind Gross Profit Pcnt */
           ind_prc   = round( pc(1%) * ind_pct, 2)
           ind_gpft  = round(ind_prc - cst_tot, 2)
           if ind_prc > 0 then goto L60270
              ind_prc = 1.0
              part_desc$ = "***** APC PRICE (ERROR) ******"

L60270:    ind_gpcnt = round( (ind_gpft / ind_prc) * 100.0, 2)
           convert ind_prc   to ind_prc$,   pic(###.##-)
           convert ind_gpft  to ind_gpft$,  pic(###.##-)
           x% = int( ind_gpcnt + .5)
           convert x% to ind_gpcnt$, pic(##)
                                             /* 1st Store             */
                                             /* 1st Store Price       */
                                             /* 1st Store Gross Profit*/
                                             /* 1st Gross Profit Pcnt */
           price = pc(srce1%)             /* Cat. Price with Cust Disc*/
           if sp% > 0% and p1 > 0.1 then price = p1  /* Special Price */
           x = price * x_pct

           x_prc   = round( price - x , 2)
           x_gpft  = round(x_prc - cst_tot, 2)
           if x_prc > 0 then goto L60365
              x_prc = 1.0
              part_desc$ = "***** STORE 0NE (ERROR) ******"

L60365:    x_gpcnt = round( (x_gpft / x_prc) * 100.0, 2)
           convert x_prc   to x_prc$,   pic(###.##-)
           convert x_gpft  to x_gpft$,  pic(###.##-)
           x% = int( x_gpcnt + .5)
           convert x% to x_gpcnt$, pic(##)
           str(cuscode$,1%,9%) = store2$     /* Set Store (2)         */
           gosub calc_price
                                             /* 2nd Store             */
                                             /* 2nd Store Price       */
                                             /* 2nd Store Gross Profit*/
                                             /* 2nd Gross Profit Pcnt */
           price = pc(srce2%)             /* Cat. Price with Cust Disc*/
           if sp% > 0% and p1 > 0.1 then price = p1  /* Special Price */
           x = price * y_pct

           y_prc   = round( price - x , 2)
           y_gpft  = round(y_prc - cst_tot, 2)
           if y_prc > 0 then goto L60470
              y_prc = 1.0
              part_desc$ = "***** STORE TWO (ERROR) ******"

L60470:    y_gpcnt = round( (y_gpft / y_prc) * 100.0, 2)
           convert y_prc   to y_prc$,   pic(###.##-)
           convert y_gpft  to y_gpft$,  pic(###.##-)
           x% = int( y_gpcnt + .5)
           convert y_gpcnt to y_gpcnt$, pic(##)

              gosub print_detail
           goto generate_nxt
        generate_done
           print using L55040
        return

        sort_product
           call "SHOSTAT" ("Sorting Products")
           readkey$ = " "
           str(readkey$,1%,9%) = "COST 0000"
           read #4,key > readkey$, using L60575, readkey$, desc$,         ~
                                                   eod goto sort_done
           goto L60580
        sort_nxt
           read #4, using L60575, readkey$, desc$, eod goto sort_done
L60575:        FMT CH(24), CH(30)
L60580:    if str(readkey$,1%,9%) <> "COST 0000" then goto sort_done
           mod$ = str(desc$,1%,3%)
           if bg_mod$ = "ALL" then goto L60605
              if mod$ < bg_mod$ then goto sort_nxt
              if mod$ > ed_mod$ then goto sort_nxt
L60605:          part$ = str(desc$,1%,25%)
                 adj_mat$ = str(desc$,27%,4%)
                 call "APCDESCR" (part$, apc_scr$, apc_prt$, apc_sze$,   ~
                                                               #6, err% )
             str(part_desc$,1%,16%) = str(apc_prt$,1%,16%)
             str(part_desc$,17%,16%)= str(apc_sze$,1%,16%)
             apc_key$ = " "
             str(apc_key$,1%,4%)  = str(part$,1%,4%)
             str(apc_key$,5%,7%)  = str(part$,13%,7%)
             str(apc_key$,12%,2%) = str(part$,7%,2%)
             str(apc_key$,14%,2%) = str(part$,5%,2%)
             str(apc_key$,16%,4%) = str(part$,9%,4%)

             write #20, using L60680, apc_key$, part$, part_desc$,        ~
                            adj_mat$, "              ", eod goto sort_nxt
L60680:           FMT CH(25), CH(25), CH(32), CH(4), CH(14)
             goto sort_nxt
        sort_done
        return


        calc_cost
           apc_err% = 0%
           gosub calc_labor                 /* SUB_LABOR$ - DIRECT/IND */
                                            /* OVR_LABOR$ -  OVERHEAD  */
           gosub calc_material              /* (1) - (TOT_COST_VNY)    */
                                            /* (2) - (TOT_COST_MSC)    */
                                            /* (3) - (TOT_COST_MAT)    */
           typ% = 0%
           gosub calc_glass                 /* (1) - (GS_VINYL_GLS)    */
                                            /* (2) - (GS_MISC_GLS)     */
                                            /* (3) - (TOT_COST_GLS)    */

           typ% = 1%
           gosub calc_screen                /* (1) - (TOT_COST_SCR)    */
                                            /* (2) - (GS_VINYL_SCR)    */
                                            /* (3) - (GS_MISC_SCR)     */

           gosub calc_locks                 /* (1) - (LK_VINYL      )  */
                                            /* (2) - (LK_MISC       )  */
                                            /* (3) - (TOT_COST_LOCKS)  */
           cst_typ$ = "0"
           gosub calc_hardware              /* (1) - (TOT_COST_HDR)    */
                                            /* (2) - (HH_VINYL_HDR)    */
                                            /* (3) - (HH_MISC_HDR)     */
           cst_typ$ = "1"
           gosub calc_hardware              /* (1) - (TOT_COST_PCK)    */
                                            /* (2) - (HH_VINYL_PCK)    */
                                            /* (3) - (HH_MISC_PCK)     */
           gosub calc_wood_surround         /* WOOD SURROUND/FACT MULL */

            total_vinyl, total_glass, total_screen = 0.0
            total_hardware, total_packaging, total_misc = 0.0
            vinyl_adj = 0.0
            total_vinyl = round( tot_cost_vny + gs_vinyl_gls +           ~
                                 gs_vinyl_scr + lk_vinyl + hh_vinyl_hdr +~
                                 hh_vinyl_pck, 4)
            vinyl_adj = round( tot_frt + tot_vinyl, 4)
            total_vinyl = round(total_vinyl - vinyl_adj, 4)

            total_glass     = round( gs_misc_gls, 4)
            total_screen    = round( gs_misc_scr, 4)
            total_hardware  = round( lk_misc + hh_misc_hdr, 4)
            total_packaging = round( hh_misc_pck, 4 )
            total_misc      = round( tot_cost_msc, 4)
            w_f_cost        = round( w_f_cost, 4)

            total_mat = round(total_vinyl + total_glass + total_screen + ~
                              total_hardware + total_packaging +         ~
                              total_misc + w_f_cost, 4)
            tot_mat_cst = round(total_mat, 2)

           adj_mat = 0.0
           convert adj_mat$ to adj_mat, data goto L60975
L60975:
           sub_labor = round(sub_labor,2)
           ovr_labor = round(ovr_labor,2)
           if str(part$,1%,3%) = "312" then                              ~
                                       adj_mat = round(adj_mat/10.0, 2)  ~
                             else adj_mat = round(adj_mat / 100.0 , 2)

           cst_mat = round(tot_mat_cst + adj_mat, 2)
           cst_tot = round(cst_mat + sub_labor + ovr_labor, 2)

           convert cst_mat   to cst_mat$, pic(###.##-)  /* Total Mat'l */
           convert sub_labor to cst_lab$, pic(###.##-)  /* Tot D/I Lab */
           convert ovr_labor to cst_ovr$, pic(###.##-)  /* Tot Overhead*/
           convert cst_tot   to cst_tot$, pic(###.##-)  /* Total Cost  */
        return

        calc_labor
           mfg_labor, gls_labor, scr_labor, mat_labor = 0.0
           stg_labor, lod_labor, tot_labor, ovr_labor = 0.0
           ind_labor, sub_labor = 0.0
           err% = 0%
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
                                  #21,        /* (APCPLNDP) - FILE     */~
                                err% )        /* 0% = OK               */
           if err% <> 0% then apc_err% = 1%   /* Labor Error           */
        return

        calc_material
           err% = 0%
           rm_vinyl_s, rm_misc_s, rm_total_s = 0.0
           tot_cost_mat, tot_cost_vny, tot_cost_msc = 0.0
           tot_frt, tot_vinyl = 0.0
           call "APCCST1B" (part$,            /* MFG PART NUMBER       */~
                            rm_part$(),       /* Inv Raw Mat'l Part No.*/~
                            rm_cuts(),        /* Total Inches in Decima*/~
                            rm_cost(),        /* Total Cost Raw Mat'l  */~
                            rm_desc$(),       /* Raw Mat'l Description */~
                            gs_units%(),      /* UNITS OF MEASURE      */~
                            rm_unit(),        /* Raw Mat'l Unit Cost   */~
                            rm_vinyl,         /* Vinyl Material Cost   */~
                            rm_misc,          /* Misc Material Cost    */~
                            rm_total,         /* Total Material Cost   */~
                            rm_cnt%,          /* Raw Material Count    */~
                            rm_cuts_s(),      /* Total Inch Decima SCRA*/~
                            rm_cost_s(),      /* Total Cst Raw Mat SCRA*/~
                            rm_vinyl_s,       /* Vinyl Mat'l Cost Scrap*/~
                            rm_misc_s,        /* Misc Mat'l Cost Scrap */~
                            rm_total_s,       /* Raw Mat Tot Cost SCRAP*/~
                            rm_eq$(),         /* Save Calc Type Eq No. */~
                            rm_ph$(),         /* Save Phantom No.      */~
                            rm_frt,           /* Save Freight Amount   */~
                            rm_vinyl_d,       /* Save Vinyl Disc Amt   */~
                                   #1,        /* (APCCUTEQ) - FILE     */~
                                   #4,        /* (GENCODES) - FILE     */~
                                   #2,        /* (HNYMASTR) - FILE     */~
                                   #3,        /* (HNYQUAN ) - FILE     */~
                                   #5,        /* (AMTBOMCD) - FILE     */~
                                   #18,       /* (APCSTOCK) - FILE     */~
                               err%   )       /* 0% = OK               */
           area$ = "1"
           gosub calc_costmate

           tot_cost_vny = rm_vinyl + rm_vinyl_s
           tot_cost_msc = rm_misc  + rm_misc_s
           tot_cost_mat = rm_total + rm_total_s
           tot_frt   = tot_frt + rm_frt
           tot_vinyl = tot_vinyl + rm_vinyl_d
           if err% <> 0% then apc_err% = 2%   /* Material Error        */
        return

        calc_glass
        calc_screen
           err% = 0%
           gs_vinyl_s, gs_misc_s, gs_total_s = 0.0
           gs_vinyl, gs_misc, gs_total = 0.0
           gs_cnt% = 0%
           if typ% = 0% then tot_cost_gls = 0.0
           if typ% = 1% then tot_cost_scr = 0.0
           call "APCCST2B" (part$,            /* MFG PART NUMBER       */~
                            typ%,             /* 0% = Glass,1% = Screen*/~
                            gs_qty(),         /* G/S Mat'l Units (1-8) */~
                            gs_raw$(),        /* Raw Mat'l Parts (1-8) */~
                            gs_desc$(),       /* Raw Mat'l Descr. (1-8)*/~
                            gs_cost(),        /* Raw Mat'l Cost (1-8)  */~
                            gs_units%(),      /* RAW Mat'l U. M. (1-8) */~
                            gs_unit(),        /* G/S Unit Cost of Mat'l*/~
                            gs_vinyl,         /* Vinyl Raw Mat'l Cost  */~
                            gs_misc,          /* Misc. Raw Mat'l Cost  */~
                            gs_total,         /* Total Raw Mat'l Cost  */~
                            gs_cnt%,          /* G/S No. Raw Mat'l Item*/~
                            gs_qty_s(),       /* G/S Units Mat'l Scrap */~
                            gs_cost_s(),      /* G/S Scrap Cost Mat'l  */~
                            gs_vinyl_s,       /* Vinyl Raw Mat'l Scrap */~
                            gs_misc_s,        /* Misc. Raw Mat'l Scrap */~
                            gs_total_s,       /* G/S Total Cost Scrap  */~
                            rm_eq$(),         /* Save Calc Type Eq No. */~
                            rm_ph$(),         /* Save Phantom No.      */~
                            rm_frt,           /* Save Freight Amount   */~
                            rm_vinyl_d,       /* Save Vinyl Disc Amt   */~
                                   #8,        /* (APCEQUAT) - File     */~
                                   #5,        /* (AMTBOMCD) - File     */~
                                   #4,        /* (GENCODES) - File     */~
                                   #3,        /* (HNYQUAN ) - File     */~
                                   #2,        /* (HNYMASTR) - File     */~
                               err% )         /* 0% = Ok, Non 0% = Err */

           if typ% <> 0% then goto L61620
              gs_vinyl_gls = gs_vinyl + gs_vinyl_s
              gs_misc_gls  = gs_misc  + gs_misc_s
              tot_cost_gls = gs_total + gs_total_s
              tot_frt      = tot_frt + rm_frt
              tot_vinyl    = tot_vinyl + rm_vinyl_d
              if err% <> 0% then apc_err% = 3%    /* Glass Error       */
              return
L61620:    tot_cost_scr = gs_total + gs_total_s
           gs_vinyl_scr = gs_vinyl + gs_vinyl_s
           gs_misc_scr  = gs_misc  + gs_misc_s
           tot_frt      = tot_frt + rm_frt
           tot_vinyl    = tot_vinyl + rm_vinyl_d
           if err% <> 0% then apc_err% = 4%   /* Screen Error          */
        return

        calc_locks
           err% = 0%
           tot_cost_locks = 0.0
           call "APCCST3B" (part$,            /* MFG PART NUMBER       */~
                            rm_part$(),       /* Inv Raw Mat'l Part No.*/~
                            rm_cuts(),        /* Total Inches in Decima*/~
                            rm_cost(),        /* Total Cost Raw Mat'l  */~
                            rm_desc$(),       /* Raw Mat'l Description */~
                            gs_units%(),      /* Units of Measure      */~
                            rm_unit(),        /* Raw Mat'l Unit Cost   */~
                            rm_vinyl,         /* Raw Mat'l Cost Vinyl  */~
                            rm_misc,          /* Raw Mat'l Cost Vinyl  */~
                            rm_total,         /* Raw Material Tot Cost */~
                            rm_cnt%,          /* Raw Material Count    */~
                            rm_frt,           /* Save Freight Amount   */~
                            rm_vinyl_d,       /* Save Vinyl Disc Amt   */~
                                   #4,        /* (GENCODES) - FILE     */~
                                   #2,        /* (HNYMASTR) - FILE     */~
                                   #3,        /* (HNYQUAN ) - FILE     */~
                               err%   )       /* 0% = OK               */

           lk_vinyl = rm_vinyl
           lk_misc  = rm_misc
           tot_cost_locks = rm_total
           tot_frt   = tot_frt + rm_frt
           tot_vinyl = tot_vinyl + rm_vinyl_d
           if err% <> 0% then apc_err% = 5%   /* Locks Error           */
        return

        calc_hardware
           err% = 0%
           if cst_typ$ = "0" then tot_cost_hdr = 0.0
           if cst_typ$ = "1" then tot_cost_pck = 0.0

        call "APCCST4B" (part$,          /* MFG Part Number           */ ~
                        cst_typ$,        /* '0'=Hardware,'1'=Packaging*/ ~
                        rm_raw$(),       /* Inv. Raw Material Part    */ ~
                        rm_cuts(),       /* Total Inches in Decimal   */ ~
                        rm_cost(),       /* Total Cost Raw Mat. Part  */ ~
                        rm_desc$(),      /* Raw Material Description  */ ~
                        gs_units%(),     /* RAW MAT'L UNITS OF MEASURE*/ ~
                        rm_unit(),       /* Raw Mat'l Unit Cost       */ ~
                        rm_vinyl,        /* Raw Mat'l Vinyl Cost      */ ~
                        rm_misc,         /* Raw Mat'l Misc. Cost      */ ~
                        rm_total,        /* Raw Material Total Cost   */ ~
                        rm_cnt%,         /* Raw Material Count        */ ~
                        rm_cuts_s(),     /* Total Inch Decima SCRA     */~
                        rm_cost_s(),     /* Total Cst Raw Mat SCRA     */~
                        rm_vinyl_s,      /* Vinyl Mat'l Cost Scrap     */~
                        rm_misc_s,       /* Misc Mat'l Cost Scrap      */~
                        rm_total_s,      /* Raw Mat Tot Cost SCRAP     */~
                        rm_eq$(),        /* Save Calc Type Eq No.      */~
                        rm_ph$(),        /* Save Phantom No.           */~
                        rm_frt,          /* Save Freight Amount        */~
                        rm_vinyl_d,      /* Save Vinyl Disc Amt        */~
                              #9,        /* (APCCSTHP) - File         */ ~
                              #4,        /* (GENCODES) - File         */ ~
                              #3,        /* (HNYQUAN ) - File         */ ~
                              #2,        /* (HNYMASTR) - File         */ ~
                           err% )        /* 0% = Ok, Non 0% = Error   */

           if cst_typ$ = "0" then area$ = "5"                            ~
                             else area$ = "6"
              gosub calc_costmate

           if cst_typ$ <> "0" then goto L62025
              tot_cost_hdr = rm_total
              hh_vinyl_hdr = rm_vinyl
              hh_misc_hdr  = rm_misc
              tot_frt      = tot_frt + rm_frt
              tot_vinyl    = tot_vinyl + rm_vinyl_d
              if err% <> 0% then apc_err% = 6%   /* Hardware Error     */
              return
L62025:    tot_cost_pck = rm_total
           hh_vinyl_pck = rm_vinyl
           hh_misc_pck  = rm_misc
           tot_frt      = tot_frt + rm_frt
           tot_vinyl    = tot_vinyl + rm_vinyl_d
           if err% <> 0% then apc_err% = 7%   /* Packaging Error       */
        return

        calc_price                  /* Get Correct Size and Price      */
          mat pc     = zer
          mat ref_p  = zer
          mat ref_p1 = zer
          p1    = 0.0      /* Spc Customer Price, Dealer Catalog Always*/
          sp%   = 0%       /* (0%) Dealer Price Catalog (Only)         */
                           /* (1%) Spc Customer Catalog Price in (P1)  */
                           /* (2%) Spc Customer EDI Price in     (P1)  */
                           /* Note - Information from 'CUS PRICE' Table*/
          size$ = "E"      /* Always Exact Size                        */
          upd$ = "N"       /* Do Not Update Price Sheets               */
          err%  = 0%       /* Clear Error Flag                         */
          call "APCPRSUB" ( part$,       /* Part Number                */~
                            partno1$,    /* Part Number 1       (AWD001)*/~
                            size$,       /* (O)pen,(E)xact,(F)No Deduct*/~
                            pc(),        /* Calc Dealer Price Catalog  */~
                            p1,          /* Special Customer Price     */~
                            sp%,         /* Special Price Code 0,1,2   */~
                            upd$,        /* Update Price Sheet Y or N  */~
                            cuscode$,    /* Customer Code              */~
                            err%,        /* Error Return Codes         */~
                            ref$(),      /* Ref. Type Codes Catalog    */~
                            ref1$(),     /* Ref. Type Codes Spec Cat.  */~
                            ref_p(),     /* Ref Prices APC Catalog     */~
                            ref_p1(),    /* Ref Prices Special Cat.    */~
                            #13,         /* Channel of (APCPCMST) File */~
                            #15,         /* Channel of (APCPCMSK) File */~
                            #16,         /* Channel of (APCPCMSD) File */~
                            #4,          /* Channel of (GENCODES) File */~
                            #11,         /* Channel of (CPRPRICE) File */~
                            #12,         /* Channel of (CUSTOMER) File */~
                            #14 )        /* Channel of (APCSKUNO) File */
                                         /* After Call PART$ Always    */
                                         /* has Exact Size             */
           if err% <> 0% then apc_err% = 8%   /* Pricing Error         */
        return

        open_work
            if mode% = 1% then mode$ = "OUTPT"
            if mode% = 2% then mode$ = "INPUT"
            if mode% = 3% then mode$ = "SHARE"

            call "WORKOPN2" (#20,mode$, 500%, f2%)
            if f2% <> 0% then goto L62285
        return
L62285:     call "SHOSTAT" ("ERROR - CANNOT OPEN (APCGLS4X)") : stop
        return
        delete_work
            call "FILEBGON" (#20)
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
                             #17,           /*   (APCCSTEX)            */~
                             #18,           /*   (APCSTOCK)            */~
                             x_err% )       /* 0% = Ok, 1% = Error     */
        return

        calc_wood_surround
            w_f_cost = 0.0 : value$ = "0.0"
            if len(part$) < 22 then goto L62580
            if str(part$,1%,1%) = "9" then goto L62580
            if len(part$) = 22 then wood_code$ = str(part$,20%,3%)       ~
                               else wood_code$ = str(part$,23%,3%)
            if wood_code$ = "000" then goto L62580
               readkey$ = " " : value$ = "0.0"
               str(readkey$,1%,9%) = "COST WOOD"
               str(readkey$,10%,3%) = wood_code$
               read #4,key = readkey$, using L62555,value$, eod goto L62560
L62555:           FMT POS(25), CH(10)
L62560:        convert value$ to w_f_cost, data goto L62565
L62565:                                    /* Fixed Cost Wood Surround */
               convert w_f_cost to w_f_cost$,pic($###.####-)

L62580: return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")
            call "FILEBGON" addr(#20)
            end
