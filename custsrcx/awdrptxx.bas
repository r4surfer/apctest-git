        REM *************************************************************~
            *            ( No Longer Used )                             *~
            *   AAA   W   W  DDDD   RRRR   PPPP   TTTTT   000   3333    *~
            *  A   A  W   W  D   D  R   R  P   P    T    0   0     3    *~
            *  AAAAA  W w W  D   D  RRRRR  PPPP     T    0   0   33     *~
            *  A   A  Ww wW  D   D  R  R   P        T    0   0     3    *~
            *  A   A  W   W  DDDD   R   R  P        T     000   3333    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * AWDRPT03 - AWD Custom Glass Analysis Report               *~
            *                                                           *~
            *    Notes - Key Tables                                     *~
            *                                                           *~
            *            (CSWARRANT) Used by Selection to Calc Warranty *~
            *                        Dollar Value.                      *~
            *                        Key SSSSSSSS-Sales order           *~
            *                            LL- Line Item No.              *~
            *                            desc$,1%,2%) = so_reason$      *~
            *                            desc$,6%,2%) = so_grid$        *~
            *                                                           *~
            *            (CUSTOMWRR) Custom Warranty Code Description   *~
            *                        so_reason$                         *~ 
            *                                                           *~
            *            Note for custom Glass Warranty Product. A code *~
            *                 will be in 'BCKLINES' at Position         *~
            *                 str(so_rec$(),287%,2%) if exists then     *~
            *                 product is Custom Glass Warranty item.    *~
            *                                                           *~              
            *          - Key Subroutines                                *~
            *                                                           *~
            *            print_report - Main Routine                    *~
            *               open_work                                   *~
            *               generate_report                             *~
            *                                                           *~
            *            selecting_data    - Used for Selection (1)     *~
            *            selecting _data_b - Used for Selection (2)     *~ 
            *            lookup_so - Get S.O. Order Infor and Ord Date  *~
            *            calculate_price                                *~
            *               lookup_shape - Verify Special Shape         *~
            *               lookup_cross - Find Specific Shape          *~
            *                  lookup_specials - Circles, Octagons      *~
            *                  convert_shape   - Get Dim from Part No.  *~
            *                  call 'EWDCALSS' Calc Glass Sizes         *~
            *                  convert_fraction - convert Decimals Value*~
            *                     convert_sixteen - Nearest 16th Inch   *~  
            *               check_grid_size - 1", 3/4", 5/8"            *~
            *               check_for_temp  - Tempered Glass            *~
            *               call "AWDCALYY" - Calc Custom Glass/Grid Prc*~
            *                                 (AWD001)                  *~
            *            build_sort_key - Based on Selection            *~
            *            update_work    - Create Sorted work File       *~ 
            *            lookup_reason  - Warranty Code Description     *~
            *            get_shape_abbrev - Convert No. to Code         *~
            *            print_detail     - Generate report             *~
            *            close_printer    - And delete work file        *~                
            *                                                           *~          
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/15/04 ! New Program for (AWD) - Last Mod Date    ! RHH *~
            * 08/02/05 ! (AWD001) Modes for New Custom Pricing    !     *~
            *          !                                          !     *~
            *************************************************************

        dim                                                              ~
            mode$5,                      /* Use for File Open          */~
            readkey$25, desc$32,         /* Gencodes lookup            */~
            sel_key$24, sel_desc$128,    /* Gencodes Data Lookup       */~ 
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
            dte1$10, beg_dte$10,         /* Beg     Sales Order Date   */~
            dte2$10, end_dte$10,         /* End     Sales Order Date   */~
            sort_code$1, sort_code_d$25, /* Sort Code Selection/Descr  */~
            shape_code$2, shape_code_d$25,/* shape Code Selection      */~
            shape_sel$1, shape_sel_d$29, /* Report Selection 1 or 2    */~  
            sh$(30%)8,                   /* Shape Abbreviations        */~
            s$(20%)60, ss$(20%)60,       /* Special Shape Selections   */~
            ct_shp$3, ct_shp1$3,         /* Shape Abbrev.              */~
            so_reason$2, so_reason_d$25, /* Warranty Reson Code        */~
            so_grid$2,                   /* Grid Code Override         */~
            so_rec$(3%)100,              /* Bcklines Record            */~              
            so_key$19,                   /* Primary key BCKLINES       */~
            bck_key$25,                  /* Primary key BCKMASTR       */~
            so_cust$9,                   /* Customer Code              */~
            so_no$8,                     /* Sales Order Number         */~
            so_ln$3,                     /* Sales Order Line Item      */~
            so_qty$8,                    /* Line Item Quantity         */~
            so_part$25,                  /* MFG Part Number            */~
            so_ord_date$6,               /* SO Order Date              */~
            order_date$10,               /* SO Order Date Formatted    */~
            count$6,                     /* Screen Counter             */~  
            ct_rec$256,                  /* Custom Detail Record       */~
            ct_part$25,                  /* MFG Part Number            */~
            ct_wood$1,                   /* Wood Surround Prod Y/N     */~
            ct_wood_d$3,                 /* Wood Surround Code         */~
            ct_width$9,                  /* Calculated Glass Width     */~
            ct_height$9,                 /* Calculated Glass Height    */~
            sh_model$3,                  /* Model Code                 */~
            shape_cross$2,               /* Special Shape Cross Ref    */~
            sh_config$3, err$2,          /* Special Shape Number       */~
            sh_config_seq$2,             /* Shape Sequence Code        */~
            sh_codes$7,                  /* Dimension Codes            */~
            sh_grid$2,                   /* Glass Grid Code            */~
            sh_glass$2,                  /* Glass Code                 */~
            sh_type$1,                   /* Grid Type 1", 3/4", 5/8"   */~
            sh(10%),                     /* Data Entry Values shd()    */~
            shc(10%), shc$(10%%)10,      /* Calculated Values          */~
            shd(10%),                    /* Part Number Values sh()    */~
            sh_fields$7,                 /* Print Fields for sh_codes$ */~
            sh_position$7,               /* Loc of Print Fields        */~
            sh_entry$7,                  /* Dat Fields entered sh_codes*/~
            w$1, h$1,                    /* Use for Glass width/Height */~
            dt_bar$18,                   /* Barcode use S.O. - Line    */~  
            dt_txt$4,                    /* Text Code                  */~
            text$(2%)70, txt$40,         /* Text Record and text       */~
            cl$1,                        /* Color Code                 */~
            lk$1,                        /* Lock Code                  */~
            lks$40,                      /* 1" Lock Codes              */~
            scr_sel$1,                   /* Used for Price Calc '9'    */~
            ct_price$10,                 /* Calculated Custom Price    */~
            cnt$10, rhh$10,              /* Counter                    */~
            ct_cnt$8, ct_prc_1$10, ct_prc_2$10,       /* Total Buckets */~ 
            ct_clear_t$10, ct_obs_t$10, ct_lowe_t$10, /* Total Buckets */~
            ct_inv_grid$8,               /* Total of Invlid Grid Codes */~
            wrk_key$44,                  /* Work File Key              */~
            sz$100, wd1$9,               /* Fractions nearest 16th     */~
            company$40,                  /* For Report Company Name    */~
            print_title$40,              /* For Report Title           */~
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
            dim apc$35, pname$21                                   
            apc$   = "Custom Glass Pricing Analysis Rpt  "
            pname$ = "AWDRPTXX - 08/02/2005"

        REM *************************************************************

            mat f2% = con

            mat f1% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! AWDGLSCT ! Custom Glass Detail Data                 *~
            * #02 ! GENCODES ! System Master Code Table Use for Sel=1   *~
            * #03 ! GENCODES ! System Master Code Table Files           *~
            * #05 ! BCKMASTR ! Backlog master file                      *~
            * #06 ! BCKLINES ! Back Log Line Item File                  *~
            * #09 ! TXTFILE  ! Master Text File                         *~
            * #10 ! AWDGLSWK ! Report Work File                         *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************


            select #01, "AWDGLSCT",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =    1, keylen =   23,                    ~
                        alt key  1, keypos =   12, keylen =  12,         ~
                            key  2, keypos  = 154, keylen =  29

            select #02,  "GENCODES",                                     ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24


            select #03,  "GENCODES",                                     ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #05, "BCKMASTR",                                      ~
                        varc,     indexed,  recsize =  1000,             ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #06, "BCKLINES",                                      ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =  10,  keylen = 19

            select #09, "TXTFILE",                                       ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =    1, keylen =  11

                                          /* Sort = 44, Data Rec = 256 */
            select #10, "AWDGLSWK",                                      ~
                        varc,     indexed,  recsize =   300,             ~
                        keypos =    1, keylen =  44

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#02, fs%(2%), f2%(2%),  0%, rslt$(2%))
            call "OPENCHCK" (#03, fs%(3%), f2%(3%),  0%, rslt$(3%))
            call "OPENCHCK" (#05, fs%(5%), f2%(5%),  0%, rslt$(5%))
            call "OPENCHCK" (#06, fs%(6%), f2%(6%),  0%, rslt$(6%))
            call "OPENCHCK" (#09, fs%(9%), f2%(9%),  0%, rslt$(9%))
 
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

            sh$( 1%) = "00 - PW "     :     sh$(11%) = "51 - CIR"
            sh$( 2%) = "01 - RPT"     :     sh$(12%) = "60 - EYE"
            sh$( 3%) = "02 - LPT"     :     sh$(13%) = "63 - CA "
            sh$( 4%) = "03 - RTP"     :     sh$(14%) = "64 - HR "
            sh$( 5%) = "04 - LTP"     :     sh$(15%) = "64 - ELP" 
            sh$( 6%) = "05 - RTG"     :     sh$(16%) = "66 - RQR"
            sh$( 7%) = "06 - LTG"     :     sh$(17%) = "67 - LQR"
            sh$( 8%) = "07 - TRI"     :     sh$(18%) = "70 - REB"
            sh$( 9%) = "15 - DPT"     :     sh$(19%) = "71 - LEB"
            sh$(10%) = "25 - OCT"     :     sh$(20%) = "73 - RCA" 
                                            sh$(21%) = "74 - LCA"
                                            sh$(22%) = "99 - ***"

            sh_max% = 22% 
                                                                           
            ss$( 1%) = "00 - Picture Window     51 - Circle                 "
            ss$( 2%) = "01 - Irreg Right Pent   60 - Eyebrow                "
            ss$( 3%) = "02 - Irreg Left Pent    63 - Colonial Arch          "
            ss$( 4%) = "03 - Trapezoid Right    64 - Half Rnd/Elliptical    "
            ss$( 5%) = "04 - Trapezoid Left     66 - Right Quarter Round    " 
            ss$( 6%) = "05 - Right Triangle     67 - Left Quarter Round     "
            ss$( 7%) = "06 - Left Triangle      70 - 1/2 Right Eyebrow      "
            ss$( 8%) = "07 - ISOC Triangle      71 - 1/2 Left Eyebrow       "
            ss$( 9%) = "15 - Doghouse Pent      73 - 1/2 Right Colonial Arch"
            ss$(10%) = "25 - Octagon            74 - 1/2 Left Colonial Arch " 

            lks$ = "ABCDEFGIKLMNOQRSTUZ"             /* 1 Inch Grid     */     
                                    /* (AWD001)                         */    
            scr_sel$ = "9"          /* Special Selection for (AWDCALYY) */
                                    /* Do not Want to Update Custom     */
                                    /* Pricing in the Database.         */
                                    /* (AWD001)                         */
        REM - NEAREST 16TH OF AN INCH
           sz$ = " 1/16 1/8  3/16 1/4  5/16 3/8  7/16 1/2  9/16 5/8 11/16~
        ~ 3/4 13/16 7/8 15/16     "

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 4%
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
            if fieldnr% < 1% or fieldnr% > 4% then editpg1
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

        REM    gosub open_work_file

            gosub generate_report
        return clear all
        REM GOTO INPUTMODE
        goto exit_program

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

        scrn1_msg  :  data                                                ~
         "Enter Selection (1)=Warr (Old),(2) All Shape Glass?,(3) Warranty Tracking?",~
         "Enter a Beginning and Ending Production Date for Custom Glass?",~
         "Enter Sort Code, 1=Sales Order,2=Date/S.O./Line, 3=Shape Code?",~
         "Enter Special Shape Selection, 'AL' or Special Shapes Code?   "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, dte1$, dte2$, beg_dte$,    ~
                      end_dte$, sort_code$, sort_code_d$, shape_code$,   ~
                      shape_code_d$, shape_sel$, shape_sel_d$, s$(), rhh$


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
        REM RETURN CLEAR ALL
        REM GOTO INPUTMODE

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
              on fieldnr% gosub L40180,      /* Selection Code 1 or 2 */ ~
                                L40180,      /* Beg/End Sales Order DT*/ ~
                                L40180,      /* Sort Code Selection   */ ~
                                L40180,      /* Shape Code Selection  */ ~
                                
              goto L40210

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40180:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40210:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,63), "Today:",                                     ~
               at (01,70), fac(hex(8c)), date$                  , ch(10),~
               at (01,24), fac(hex(a4)), apc$                   , ch(35),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Report Selection(1,2,3):",                  ~
               at (06,30), fac(lfac$(1%)), shape_sel$           ,  ch(1),~
               at (06,50), fac(hex(84)), shape_sel_d$           , ch(29),~
                                                                         ~
               at (07,02), "Beg/End Order Date      :",                  ~
               at (07,30), fac(lfac$(2%)), dte1$                , ch(10),~
               at (07,50), fac(lfac$(2%)), dte2$                , ch(10),~
                                                                         ~
               at (08,02), "Sort Code (1,2, or 3)   :",                  ~
               at (08,30), fac(lfac$(3%)), sort_code$           , ch(01),~
               at (08,50), fac(hex(84)), sort_code_d$           , ch(25),~
                                                                         ~
               at (09,02), "Shape Sel 'AL' or Code  :",                  ~
               at (09,30), fac(lfac$(4%)), shape_code$          , ch(02),~
               at (09,50), fac(hex(84)), shape_code_d$          , ch(25),~
                                                                         ~
               at (11,02), fac(hex(84)), s$(1%)                 , ch(60),~
               at (12,02), fac(hex(84)), s$(2%)                 , ch(60),~
               at (13,02), fac(hex(84)), s$(3%)                 , ch(60),~
               at (14,02), fac(hex(84)), s$(4%)                 , ch(60),~
               at (15,02), fac(hex(84)), s$(5%)                 , ch(60),~
               at (16,02), fac(hex(84)), s$(6%)                 , ch(60),~
               at (17,02), fac(hex(84)), s$(7%)                 , ch(60),~
               at (18,02), fac(hex(84)), s$(8%)                 , ch(60),~
               at (19,02), fac(hex(84)), s$(9%)                 , ch(60),~
               at (20,02), fac(hex(84)), s$(10%)                , ch(60),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then goto L40560
                  call "PRNTSCRN"
                  goto L40210

L40560:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40750     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                                       "
            pf$(2%) = "                 (4)Previous Field      " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L40710
                str(pf$(3%),64%)    = " "  :  str(pfkeys$,16%,1%) = hex(ff)
L40710:     if fieldnr% > 1% then L40730
                str(pf$(2%),18%,26%) = " "  :  str(pfkeys$,4%,1%) = hex(ff)
L40730:     return

L40750: if fieldnr% > 0% then L40840  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (14)Print Report"
            pf$(2%) = "                                        " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return
L40840:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                                       "
            pf$(2%) = "                                        " &       ~
                     "                                       "
            pf$(3%) = "                                        " &       ~
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
            on fieldnr% gosub L50400,        /* Processs Selection     */~
                              L50150,        /* Beg/End Order Date     */~
                              L50200,        /* Sort Selection 1,2,3   */~
                              L50300,        /* Special Shape Selection*/

            return


L50150: REM Beginning and Ending Order Date Date             DTE1$,DTE2$
            if dte1$ <> " " then goto L50160
               dte1$ = date

L50160:        date% = 0%
               call "DATEOKC" (dte1$, date%, errormsg$)
               if errormsg$ <> " " then goto L50185
               if dte2$ <> " " then goto L50180
                  dte2$ = dte1$

L50180:           call "DATEOKC" (dte2$, date%, errormsg$)
                  if errormsg$ <> " " then L50185
               beg_dte$ = dte1$
               end_dte$ = dte2$
               call "DATUFMTC" (beg_dte$)
               call "DATUFMTC" (end_dte$)
               if beg_dte$ > end_dte$ then goto L50195

        return
L50185:        errormsg$ = "(Error) Sales Order Date."
               init(" ") dte1$, dte2$, beg_dte$, end_dte$
               gosub error_prompt
        return
L50195:        errormsg$ = "(Error) Invalid beginning Sales Order Date?"
               init(" ") dte1$, dte2$, beg_dte$, end_dte$
               gosub error_prompt
        return

L50200: REM Valid Sort Selection                      sort_code$, sort_code_d$

           init(" ") sort_code_d$

           if sort_code$ = " " then sort_code$ = "1"

           if sort_code$ < "1" or sort_code$ > "3" then goto L50250

           sort_code% = 0%
           convert sort_code$ to sort_code%, data goto L50250

           if sort_code$ = "1" then sort_code_d$ = "Sort By S.O./Line/Date   "
           if sort_code$ = "2" then sort_code_d$ = "Sort By Date/S.O./Line   "
           if sort_code$ = "3" then sort_code_d$ = "Sort By Shp/Date/S.O./Ln "  

           copy ss$() to s$()
            
        return
L50250:   errormsg$ = "(Error) Invalid Sort Selection (1,2, or 3)? "
          gosub error_prompt
          init(" ") sort_code$, sort_code_d$   
        return           

L50300: REM Special Shapes Selection Code          shape_code$, shape_code_d$
           init(" ") readkey$, shape_code_d$
           if shape_sel$ = "1" then shape_code$ = "AL"
           if shape_code$ <> " " then goto L50305
              shape_code$ = "AL"
L50305: 
           if shape_code$ <> "AL" then goto L50310
              shape_code_d$ = "(ALL) Special Shape Codes "
              init(" ") s$()

              return
L50310:
           str(readkey$,1%,9%)   = "PLNCONFIG"
           str(readkey$,10%,15%) = shape_code$
           read #3,key = readkey$, using L50320, shape_code_d$,eod goto L50350
L50320:        FMT POS(25), CH(32)

           init(" ") s$()

        return
L50350:   errormsg$ = "(Error) Invalid Shape code Selection? "
          gosub error_prompt
          init(" ") shape_code$, shape_code_d$   
        return           
 
L50400: REM Seletion Process                                shape_sel$
           if shape_sel$ <> "  " then goto L50410
              shape_sel$ = "1"
L50410: 
           if shape_sel$ <> "1" and shape_sel$ <> "2" and shape_sel$ <> "3" ~
                                                    then goto L50420
       
              if shape_sel$ = "1" then                                ~
                            shape_sel_d$ = "Calc Price for Warranty (Old)"

              if shape_sel$ = "2" then                                ~
                            shape_sel_d$ = "Calc Price for All Shapes    "

              if shape_sel$ = "3" then                                ~
                            shape_sel_d$ = "Calc Price for Warr. Tracking"

        return
L50420:   errormsg$ = "(Error) Invalid Special Shape Process Selection (1,2)?"
          gosub error_prompt
          init(" ") shape_sel$, shape_sel_d$   
        return           
 
        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

                                                   /* REPORT HEADER */
L55050: %!########## ########                        ####################~
        ~####################                                    AWDRPTXX ~
        ~!

L55090: %!Order DTE Beg: ##########     End: ##########  ################~
        ~########################                             Page: ##### ~
        ~!

L55130: %!Sort Code: #  #########################        Special Shape Co~
        ~de: ## #########################            Report Selection: #  ~
        ~!

                                                   /* Detail Header      */
L55210: %!Number!Order DATE!Customer !Sales  Ord! Line!Model!Shape!Grid!G~
        ~lass!Quantity!Custom Price!Total  Price!  Width  !  Height !Type ~
        ~!

L55250: %!------!----------!---------!----------!-----!-----!-----!----!-~
        ~----!--------!------------!------------!---------!---------!-----~
        ~!

L55290: %!######!##########!#########! ######## ! ### ! ### ! ### ! ## ! ~
        ~##  !########! ########## ! ########## !#########!#########!#####~
        ~!

L55320: %+---------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~+

L55340: %!  Clear: ##########  OBS: ##########  LowE: ##########         ~
        ~    !########! ########## ! ########## !Grid(?Z)No Price:########~
        ~!

L55360: %!---------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~!


        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        select_printer
            call "SETPRNT" ("AWDRPT", " ",2500%, 0%)
            select printer (134)

            page_no% = 0%
            lcnt%    = 99%
                                                                
            print_title$ = "Custom Glass Pricing Analysis Report "
            call "FMTTITLE" (print_title$, " ", 12%)
            date$ = date  :  call "DATFMTC" (date$)
            call "TIME" (rpt_time$)
            company$ = "ATRIUM Windows and Doors"
            call "FMTTITLE" (company$, " ", 12%)
        return

        close_printer
            call "SETPRNT" ("AWDRPT", " ",0%, 1%)
        return

        generate_report
            ct_inv_grid = 0.0            /* Total of Inv Grid Code    */
                                         /* Check Custom Detail File  */ 
            if shape_sel$ = "1" then gosub selecting_data              ~
                                else gosub selecting_data_b

                                         /* Print sorted report       */

            call "SHOSTAT" ("PRINTING REPORT")
            CALL "PAUSE" ADDR(50%)
            
            ct_cnt   = 0.0               /* Total Number of Panels    */
            ct_prc_1 = 0.0               /* Total unit price          */  
            ct_prc_2 = 0.0               /* Total Extended price      */
         
            ct_clear_t = 0.0
            ct_obs_t   = 0.0
            ct_lowe    = 0.0 

            count%          = 0%
            cnt%            = 0%

            gosub select_printer

            wrk_key$ = all(hex(00))

        generate_report_next
            init(" ") cnt$, so_ord_date$, so_cust$, so_no$, so_ln$, sh_model$,~
                      sh_config$, sh_grid$, sh_glass$, so_qty$, ct_price$, ~
                      ct_prc$, so_reason$, so_reason_d$, ct_rec$, order_date$, ~
                      ct_width$, ct_height$

            read #10,key > wrk_key$, using GEN_1, wrk_key$, ct_rec$,   ~
                                                  eod goto generate_done
GEN_1:         FMT CH(44), CH(256)

            cnt% = cnt% + 1%                   /* Line Counter        */
            convert cnt% to cnt$, pic(######)

                                               /* Order Date          */ 
            order_date$ = str(ct_rec$,1%,6%)
            call "DATFMTC" (order_date$)
                                               /* S.O. Customer       */ 
            so_cust$     = str(ct_rec$,70%,9%)
                                               /* Sales Order         */
            so_no$       = str(ct_rec$,24%,8%)
                                               /* Line Item           */
            so_ln$       = str(ct_rec$,21%,3%)
                                               /* Model Code          */
            sh_model$    = str(ct_rec$,32%,3%)
                                               /* Shape Code          */
            sh_config$   = str(ct_rec$,41%,3%)
                                               /* Get Grid Code       */
            sh_grid$     = str(ct_rec$,47%,2%)
                                               /* Get Glass Code      */
            sh_glass$    = str(ct_rec$,49%,2%)
                                               /* Line Item Qty       */
            so_qty$      = str(ct_rec$,80%,8%)
 
            if len(so_no$) > 5% then goto SS_1
               goto generate_report_next       /* Invalid S.O.        */
SS_1

                                               /* Custom_price        */
            ct_price = 0.0
            get str(ct_rec$,127%,8%), using SO_2, ct_price 

                                               /* Clear Glass Only    */
            get str(ct_rec$,220%,8%), using SO_2, ct_clear 
                                               /* OBS Glass Only      */
            get str(ct_rec$,228%,8%), using SO_2, ct_obs 
                                               /* LoW E Glass Only    */
            get str(ct_rec$,236%,8%), using SO_2, ct_lowe
     
            ct_clear_t = ct_clear_t + ct_clear /* Clear Glass Only Tot*/ 
            ct_obs_t   = ct_obs_t   + ct_obs   /* OBS Glass Only Tot  */
            ct_lowe_t  = ct_lowe_t  + ct_lowe  /* Lowe Glass Only Tot */

            convert ct_price to ct_price$, pic(######.##-)

            so_qty = 1.0                       /* Extented price      */
            convert so_qty$ to so_qty, data goto GEN_2
GEN_2:  
            ct_cnt = ct_cnt + so_qty           /* Total Panels        */

            ct_prc = 0.0
            ct_prc = so_qty * ct_price
            convert ct_prc to ct_prc$, pic(######.##-)

            ct_prc_1 = ct_prc_1 + ct_price     /* Total Price         */

            ct_prc_2 = ct_prc_2 + ct_prc       /* Total Extended Price*/

                                               /* Glass Width         */
            ct_width$  = str(ct_rec$,51%,9%)
                                               /* Glass Height        */
            ct_height$ = str(ct_rec$,60%,9%)
            
                                               /* Warranty Reason     */
            so_reason$ = str(ct_rec$,136%,2%)
            gosub lookup_reason                /* so_reason_d$        */ 

            gosub get_shape_abbrev             /* ct_shp$             */

            gosub print_detail

            goto generate_report_next
        generate_done
            init(" ") ct_cnt$, ct_prc_1$, ct_prc_2$

            convert ct_cnt to ct_cnt$,     pic(#####.##)

            convert ct_prc_1 to ct_prc_1$, pic(#######.##)

            convert ct_prc_2 to ct_prc_2$, pic(#######.##)

            convert ct_clear_t to ct_clear_t$, pic(#######.##)

            convert ct_obs_t   to ct_obs_t$,   pic(#######.##)

            convert ct_lowe_t  to ct_lowe_t$,  pic(#######.##)
                                           /* Invalid Grid Total  */
            convert ct_inv_grid to ct_inv_grid$, pic(#####.##)



            print using L55360
            print using L55340, ct_clear_t$, ct_obs_t$, ct_lowe_t$, ~
                                ct_cnt$, ct_prc_1$, ct_prc_2$,      ~
                                ct_inv_grid$
            print using L55320

            gosub close_printer

            gosub delete_work

        REM     call "FILEBGON" addr(#10)
        return

        print_header                        /* Page Header       */
          page_no% = page_no% + 1%
          if lcnt% <> 99% then print using L55320
          lcnt% = 0%
          print page
          print using L55320
          print using L55050, date$, rpt_time$, company$
          print using L55090, dte1$, dte2$, print_title$, page_no%
          print using L55130, sort_code$, sort_code_d$, shape_code$,~
                              shape_code_d$, shape_sel$
          print using L55360
          print using L55210
          lcnt% = lcnt% + 6%
        return

        print_detail                        /* Line Item Detail  */
          count% = count% + 1%

          if lcnt% > 54% then gosub print_header
                                            /* Print Columns     */
          print using L55250
                                            /* print Detail      */
          gosub print_a
 
        return

        print_a                             /* Detail Data Line   */
          print using L55290, cnt$, order_date$, so_cust$, so_no$,   ~
                              so_ln$, sh_model$, ct_shp$, sh_grid$,  ~
                              sh_glass$, so_qty$, ct_price$, ct_prc$,~
                              ct_width$, ct_height$, str(so_reason_d$,1%,5%)

          lcnt% = lcnt% + 2%
        return

        selecting_data                     /* Select Warranty Data */
                                           /* from Table.          */
                                           /* (CSWARRANT)          */
             been_here% = 0%
             call "SHOSTAT" ("Selecting Custom Warranty Data")
             init(" ") sel_key$, sel_desc$

             count% = 0%

             str(sel_key$,1%,9%) = "CSWARRANT"
            
SEL_1:
             read #2,key > sel_key$, using SEL_2, sel_desc$,            ~
                                          eod goto selecting_data_done
SEL_2:          FMT CH(128)
                                         /* Check Table               */
             if str(sel_desc$,1%,9%) <> "CSWARRANT" then               ~
                                         goto selecting_data_done
             if str(sel_desc$,10%,3%) = "SSS"       then               ~
                                         goto selecting_data_done
 
                                         /* Set Primary key           */
             sel_key$ = str(sel_desc$,1%,24%)    

             if mod(count%,10%) <> 0 then goto SEL_3
                convert count% to count$, pic(######)
                call "SHOSTAT" ("Warranty S.O. Selected ("&count$&")")

SEL_3:
             init(" ") so_no$, so_ln$, so_reason$, so_grid$
 
                                         /* Sales Order From Table    */
             so_no$ = str(sel_desc$,10%,8%) 
                                         /* Line Item From Table      */ 
             so_ln$ = str(sel_desc$,18%,2%)
             convert so_ln$ to so_ln%, data goto SEL_4
SEL_4:
             convert so_ln% to so_ln$, pic(###)

                                         /* Warranty Reason Code      */
             so_reason$ = str(sel_desc$,25%,2%)
                                         /* Override Grid Code        */
             so_grid$   = str(sel_desc$,30%,2%)
                                         /* 1st Lookup Sales Order    */
                                         /* Get Customer, Part No.,   */
                                         /* Quantity and Order Date   */

                                         /* RHHTEST */
        REM     call "SHOSTAT" ("SO- " & so_no$ & "  " & so_ln$ & "  " & so_reason$ & "  " & so_grid$)
        REM     stop

             gosub lookup_so 
             if check% = 0% then goto SEL_7

             gosub calculate_price

             if err% = 1% then goto SEL_1   /* Model Not a Special Shape  */

        REM     if err% = 2% then goto SEL_1/* Not Valid Shape Cross Ref. */

             count% = count% + 1%
                                            /* Create Sort Key           */
             gosub build_sort_key
 
             gosub update_work
             goto SEL_1

        selecting_data_done
              if count% = 0% then goto SEL_6

        return
SEL_6:
            errormsg$ = "No Data Found"
            gosub error_prompt
        return 
SEl_7:
            errormsg$ = "(Error) Sales Order--> " & so_no$ & "  " & so_ln$ & " Not Found"
            gosub error_prompt
        goto SEL_1


        selecting_data_b                   /* Select Sales Order  */
                                           /* Data From BCKLINES  */
             been_here% = 0%
             call "SHOSTAT" ("Selecting Sales Order Data B")
             init(" ") so_key$, bck_key$, dt_txt$
             count% = 0%
             rhh%   = 0%
             bck_key$ = all(Hex(00))
SEL_1B:
             read #05,key 0% > bck_key$, using SEL_2B, bck_key$,       ~
                             so_ord_date$,  eod goto selecting_data_done_b
SEL_2B:         FMT CH(25), POS(806), CH(06)
 
             rhh% = rhh% + 1%
             if mod(rhh%,500%) <> 0 then goto SEL_2BA
                convert rhh% to rhh$, pic(######)
                call "SHOSTAT" ("S.O. " & rhh$ & " " & bck_key$)
SEL_2BA:


             if so_ord_date$ < str(beg_dte$,1%,6%) or                 ~
                so_ord_date$ > str(end_dte$,1%,6%)  then goto SEL_1B

             so_key$ = all(hex(00))
             str(so_key$,1%,8%) = str(bck_key$,10%,8%)
SEL_3B:
             read #06,key 0% > so_key$, using SO_1, so_rec$(),        ~
                                                       eod goto SEL_1B

             if str(so_rec$(),10%,8%) <> str(bck_key$,10%,8%) then    ~  
                                                           goto SEL_1B
             so_key$ = str(so_rec$(),10%,19%)  

             init(" ") so_cust$, so_part$, so_ln$, so_qty$, so_reason$

             so_part$ = str(so_rec$(),32%,25%)

             sh_model$ = str(so_part$,1%,3%)
                                               /* Check for MFG Part No.  */
             if len(so_part$) < 19 then goto SEL_3B

                                               /* Check Model for a Valid */
             gosub lookup_shape                /* Shape in (PLAN SHAP)    */
             if check% = 0% then goto SEL_3B   /* Not Special Shape       */

                                               /* Check for Warranty Order*/
                                               /* for Selection '3'       */
             so_reason$ = str(so_rec$(),287%,2%)
             gosub lookup_reason
             if shape_sel$ = "3" and check% = 0% then goto SEL_3B
                                               /* Not for Warranty Tracking*/

             so_cust$ = str(so_rec$(),1%,9%)
                                        /* Sales Order                */
             so_no$   = str(so_rec$(),10%,8%) 
                                         /* Line Item From Table      */ 
             so_ln$   = str(so_rec$(),26%,3%)

             convert so_ln$ to so_ln%, data goto SEL_4B
SEL_4B:
             convert so_ln% to so_ln$, pic(###)

             so_part$ = str(so_rec$(),32%,25%)

             get str(so_rec$(),93%,8%), using SO_2, so_qty

             convert so_qty to so_qty$, pic(####.##-)
                                         /* Save Line Item Text ID    */
             dt_txt$ = str(so_rec$(),242%,4%)
 
             if shape_sel$ <> "3" then so_reason$ = "00" /* Not Valid */

             so_grid$   = "  "

             gosub calculate_price       /* Calc Price for Seleted Shape */

                                         /* Check for Shape Selection */
             if shape_code$ = "AL" then goto SEL_5BA

                if str(sh_config$,2%,2%) <> shape_code$ then goto SEL_3B
SEL_5BA:

             if mod(count%,50%) <> 0 then goto SEL_5B
                convert count% to count$, pic(######)
                call "SHOSTAT" ("S.S. Orders Selected ("&count$&")")

SEL_5B:
             count% = count% + 1%
                                            /* Create Sort Key           */
             gosub build_sort_key
 
             gosub update_work
             goto SEL_3B

        selecting_data_done_b
              if count% = 0% then goto SEL_6B

        return
SEL_6B:
            errormsg$ = "No Data Found"
            gosub error_prompt
        return clear all
        goto inputmode 

        update_work

            write #10, using UPD_1, wrk_key$, ct_rec$, eod goto UPD_2

UPD_1:         FMT CH(44), CH(256)
        return
UPD_2:
            errormsg$ = "(Error) Updating S.O. - " & so_no$ & " Line - " & so_ln$
            gosub error_prompt
        return

        REM open_work_file
        REM     call "OPENCHCK" (#10, fs%(10%), f2%(10%), 500%, rslt$(10%))
        REM return

        error_prompt
           comp% = 2%
           hdr$     = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        build_sort_key
           wrk_key$ = all(hex(00))

           convert count% to count$, pic(######)

           if sort_code$ <> "1" then goto CHK_1
                                              /* Sort Selection (1)  */
                                              /* S.O./Ln/Date        */
              str(wrk_key$,1%,8%) = so_no$    /* Sales Order Number  */
              str(wrk_key$,9%,3%) = so_ln$    /* S.O. Line Item      */
                                              /* Order Date          */
              str(wrk_key$,12%,6%)= so_ord_date$
              str(wrk_key$,18%,6%)= count$

              goto CHK_3
 
CHK_1:
           if sort_code$ <> "2" then goto CHK_2
                                              /* Sort Selection (2)  */
                                              /* Date/S.O./Ln        */
              str(wrk_key$,1%,6%)  = so_ord_date$
              str(wrk_key$,7%,8%)  = so_no$
              str(wrk_key$,15%,3%) = so_ln$
              str(wrk_key$,18%,6%) = count$
              goto CHK_3

CHK_2:
                                              /* Sort Selection (3)   */
                                              /* Shape,Date,S.O./Ln   */
              str(wrk_key$,1%,3%)  = sh_config$
              str(wrk_key$,4%,6%)  = so_ord_date$
              str(wrk_key$,10%,8%) = so_no$
              str(wrk_key$,18%,3%) = so_ln$
              str(wrk_key$,21%,6%) = count$
CHK_3:

        return

        get_shape_abbrev
            init(" ") ct_shp$, ct_config$, ct_shp1$

  
            ct_config$ = str(ct_rec$,41%,3%)
 
            ct_shp1% = 0%
            convert ct_config$ to ct_shp1%, data goto CT_SHP1
CT_SHP1:
            convert ct_shp1% to ct_shp1$, pic(00)

            for jj% = 1% to sh_max%

                if ct_shp1$ = str(sh$(jj%),1%,2%) then goto CT_SHP2

            next jj%
        return
CT_SHP2:
        ct_shp$ = str(sh$(jj%),6%,3%)

        if str(sh_grid$,1%,1%) = "C" then ct_shp$ = "ELP"
                                     /* Count Invalid Grid Codes */
        if ct_shp1$ = "99" then ct_inv_grid = ct_inv_grid + 1.0

        return
  
        lookup_so
            check% = 0% 
            init(" ") so_key$, bck_key$, so_cust$, so_part$, so_qty$, ~
                      so_ord_date$, dt_txt$ 
                                             /* (BCKLINES)          */
            str(so_key$,1%,8%)  = so_no$
            str(so_key$,17%,3%) = so_ln$

            read #06,key 0% = so_key$, using SO_1, so_rec$(),         ~
                                               eod goto lookup_so_end
SO_1:          FMT 3*CH(100)

            so_cust$ = str(so_rec$(),1%,9%)

            so_part$ = str(so_rec$(),32%,25%)

            if str(so_grid$,1%,1%) <> " " then str(so_part$,7%,2%) = so_grid$

            get str(so_rec$(),93%,8%), using SO_2, so_qty
SO_2:          FMT PD(15,4)
   
            convert so_qty to so_qty$, pic(####.##-)

            dt_txt$ = str(so_rec$(),242%,4%) /* Save Text ID        */
                                             /* (BCKMASTR)          */
            str(bck_key$,1%,9%)   = so_cust$
            str(bck_key$,10%,16%) = so_no$

            read #05, key 0% = bck_key$, using SO_3, so_ord_date$,    ~
                                              eod goto lookup_so_end
SO_3:           FMT POS(806), CH(06)

            check% = 1%
                                              /* RHHTEST */ 
                                              /* Everything Went Well */ 
        REM    call "SHOSTAT" ("SO= " & so_no$ & " Ln= " & so_ln$ & " Qty= " & so_qty$)
        REM    stop
          
            if shape_sel$ <> "1" then return

            return                            /* Do Not Update - Testing */

            read #06,hold,key 0% = so_key$, using SO_1, so_rec$(),    ~
                                               eod goto lookup_so_end
               delete #06

           str(so_rec$(),287%,2%) = so_reason$

           write #06, using SO_1, so_rec$()

        return
      
        lookup_so_end

        return

        calculate_price
           init(" ") ct_rec$, ct_price$, shape_cross$
           err% = 0%                         /* Use Order Date instead of  */
           init(" ") ct_rec$, ct_price$      /* Production Date.           */
           str(ct_rec$,1%,6%) = so_ord_date$
           ct_part$              = so_part$  /* MFG Part Number            */
           str(ct_rec$,195%,25%) = ct_part$
           sh_model$             = str(ct_part$,1%,3%)                

           str(shape_cross$,1%,1%) = str(ct_part$,7%,1%) /* 1st Digit Liting*/
           str(shape_cross$,2%,1%) = str(ct_part$,10%,1%)/* 2nd Digit Hinge*/


           gosub lookup_cross                /* Find Special Shape Number  */
           if check% = 0% then err% = 2%     /* Grid Code Not in(SHAPCROSS)*/

                                         /* Check for Shape Selection */
           if shape_code$ = "AL" then goto PP_1A

                if str(sh_config$,2%,2%) <> shape_code$ then return
PP_1A:

           str(ct_rec$,32%,3%)  = sh_model$  /* Save the Model Code        */
           str(ct_rec$,41%,3%)  = sh_config$ /* Special Shape Number       */ 

                                            /* Calculated Glass Width      */
           p% = pos(sh_fields$ = "W")
           w$ = "1"
           if p% <> 0% then w$ = str(sh_position$,p%,1%)
           convert w$ to ww%, data goto PP_1
PP_1:
           str(ct_rec$,51%,9%) = str(shc$(ww%),1%,9%)

                                            /* Calculated Glass Height    */
           p% = pos(sh_fields$ = "H")
           h$ = "1"
           if p% <> 0% then h$ = str(sh_position$,p%,1%)
           convert h$ to hh%, data goto PP_2
PP_2:
           str(ct_rec$,60%,9%) = str(shc$(hh%),1%,9%)

                                            /* Shape Radius Not Used      */
           str(ct_rec$,69%,1%) = "N"        /* Not Applicable             */
           str(ct_rec$,70%,9%) = "         "

                                            /* Leg1 Not Used              */
           str(ct_rec$,79%,1%) = "N"        /* Not Applicable             */
           str(ct_rec$,80%,9%) = "         "
                         
                                            /* Grid Code                  */
                                            /* So_grid$ Override          */
           sh_grid$ = str(ct_part$,7%,2%)
                                            /* Skip Operable Shapes       */  
           if sh_grid$ = "R2" then goto ERR_1

           if str(so_grid$,1%,1%) <> " " then sh_grid$ = so_grid$

           str(ct_rec$,47%,2%) = sh_grid$

                                            /* Glass Code                 */
           sh_glass$ = str(ct_part$,5%,2%)
           str(ct_rec$,49%,2%) = sh_glass$
                                            
                                            /* Grid Sizes                 */
           gosub check_grid_size
                                            /* Tempered Y/N               */
           gosub check_for_temp
                                            /* Report Items               */ 
                                            /* Sales Order Number         */
           str(ct_rec$,24%,8%) = so_no$ 
                                            /* Sales Order Line Item      */
                                            /* Save in Remake No. Location*/
           str(ct_rec$,21%,3%) = so_ln$
                                            /* Sales Order Qty            */
                                            /* Save in Leg1 Location      */
                                            /* Formatted ####.##-         */
           if err% = 2% then so_qty$ = "   0.00 "    /* Invalid Grid Code */

           so_qty = 0.0
           str(ct_rec$,80%,8%) = so_qty$
           convert so_qty$ to so_qty, data goto CAL_A0
CAL_A0:
                                            /* Remake or Warranty Reason  */
           if err% = 2% then so_reason$ = "25"    /* Invalid Grid Code    */
      
           str(ct_rec$,136%,2%)= so_reason$

                                            /* S.O. Customer              */
           str(ct_rec$,70%,9%) = so_cust$
                                            /* Wood Surround              */
           ct_wood$   = "N"
           ct_wood_d$ = "000"
           ct_wood%   = 0%

           if len(ct_part$) > 23 then ct_wood_d$ = str(ct_part$,23%,3%)
           convert ct_wood_d$ to ct_wood%, data goto CAL_0
             
           ct_wood$ = "Y"
CAL_0:
           str(ct_rec$,184%,1%) = ct_wood$
                                            /* Set Wood Surround Code      */
 
           ct_price = 0.0                   /* Panel Price                 */
           ct_clear = 0.0                   /* Glass Only Clear            */
           ct_obs   = 0.0                   /* Glass Only Obs              */
           ct_lowe  = 0.0                   /* Glass Only Clear            */

                                            /* Invalid Grid Code           */ 
           if err% = 2% then goto CAL_1

                                            /* New Custom Pricing Tables   */
           err% = 0%                        /* (AWD001)                    */ 
           call "AWDCALYY"   (been_here%,   /* Initialize Arrays           */~
                              scr_sel$,     /* Screen Selection            */~
                              ct_rec$,      /* Custom Data Record          */~
                              ct_price$,    /* Custom Glass Price          */~  
                              #3,           /* GENCODES File               */~
                              #1,           /* AWDGLSCT (Not Used) Dummy   */~
                              #1,           /* APCPLNDT (Not Used) Dummy   */~
                              err% )        /* Error Code 0 = Ok, 0 <> err */
                                            /* (AWD001)                    */ 
           if err% <> 0% then goto ERR_3

           convert ct_price$ to ct_price, data goto CAL_1

CAL_1:
                                            /* RHH-Special               */
        REM   if str(sh_grid$,2%,1%) = "Z" then ct_price = 0.0
        REM   if str(sh_grid$,2%,1%) = "Z" then                              ~
        REM                   ct_inv_grid = ct_inv_grid + so_qty

                                            /* RHH-Special               */

           convert ct_price to ct_price$, pic(######.##-)

                                              /* (RHHTEST)               */
        REM   call "SHOSTAT" ("Special Shape Price = " & ct_price$)
        REM   stop

           put str(ct_rec$,127%,8%), using SO_2, ct_price 
                                              /* Invalid Grid Code        */ 
           if err% = 2% then put str(ct_rec$,220%,8%), using SO_2, ct_clear

           if err% = 2% then put str(ct_rec$,228%,8%), using SO_2, ct_obs

           if err% = 2% then put str(ct_rec$,236%,8%), using SO_2, ct_lowe

        return
ERR_1:
           err% = 1%                          /* Model not a Special Shape */
        return
ERR_3:
           err% = 3%                          /* Special Shape Pricing Error */ 
        return

        lookup_shape                             /* Verify Shape Model Code */
           init(" ") readkey$
           check% = 0% 
           str(readkey$,1%,9%)   = "PLAN SHAP"
           str(readkey$,10%,15%) = sh_model$
           read #3,key = readkey$, eod goto lookup_shape_done
              check% = 1%
        lookup_shape_done
        return 

        lookup_cross 
           init(" ") readkey$, desc$, sh_config$, sh_config_seq$, sh_codes$, ~
                     sh_entry$
           shape% = 99%                          /* Init to Invalid Shape Code  */
           sh_config$ = "99" 
           check% = 0% 
                                                 /* For Elipticles      */
           if str(shape_cross$,1%,1%) = "C" then str(shape_cross$,2%,1%) = "0"
                                                 /* For Cirlces         */
           if str(shape_cross$,1%,1%) = "E" then str(shape_cross$,2%,1%) = "0"
                                                 /* For Octagons        */
           if str(shape_cross$,1%,1%) = "I" then str(shape_cross$,2%,1%) = "0"

           gosub lookup_specials                 /* Check for Octagon   */
                                                 /* and Cirles Until WW */
                                                 /* Fixed (Left Out)    */
           str(readkey$, 1%,9%)  = "SHAPCROSS"
           str(readkey$,10%,15%) = shape_cross$
           read #3,key = readkey$, using CAL_2, desc$,               ~
                                       eod goto lookup_cross_done
           sh_config% = 0%
           sh_config$     = str(desc$,1%,2%)     /* Shape Code          */
           sh_config_seq$ = str(desc$,3%,2%)     /* Sequence Code       */
           convert sh_config$ to sh_config%, data goto lookup_cross_done

           convert sh_config% to sh_config$, pic(000)  
                                                 /* Unpack Codes        */ 
           str(sh_codes$,1%,1%) = str(desc$,6%,1%)
           str(sh_codes$,2%,1%) = str(desc$,8%,1%)
           str(sh_codes$,3%,1%) = str(desc$,10%,1%)
           str(sh_codes$,4%,1%) = str(desc$,12%,1%)

           sh_entry$ = sh_codes$

           shape%    = sh_config%

           dt_bar$   = so_no$ & so_ln$
 
           check% = 1%   
        lookup_cross_done

           if check% = 0% then return        /* Invalid Grid Code          */

           gosub convert_shape

           call "EWDCALSS"   (shape%,        /* Shape Code                 */~
                              sh_model$,     /* Model Code                 */~
                              sh(),          /* Data Entry Values          */~
                              shc(),         /* Calculated Values          */~ 
                              sh_fields$,    /* Label Field Flags          */~
                              sh_position$,  /* Location of Field Data     */~
                              sh_entry$,     /* Name of Data Field Entered */~
                              dt_bar$,       /* arcode for Debug           */~   
                              shape_cross$,  /* 'SHAPCROSS' Code           */~
                              #3,            /* GENCODES File              */~
                              err% )         /* Error Code 0 = Ok, 0 <> err*/

           for k% = 1% to 7%
               gosub convert_fraction
 
           next k%

           if err% <> 0% then goto lookup_cross_done_er

        return
lookup_cross_done_er
                                               /* Turn Off for Now   */
           err% = 0%

        return

           convert err% to err$, pic(00)

           convert shape% to err1$, pic(00)

           errormsg$ = "(Error) Calc Glass (" & err$ & ") Shape = " & err1$ 
           gosub error_prompt
           
       REM    errormsg$ = "Fields --> " & sh_fields$ & "  Positions --> " & sh_position$
           errormsg$ = "Part Number-" & ct_part$
           gosub error_prompt

           err% = 0%

        return

        convert_shape
           nbr_lines% = 0%
           init(" ") txt$, text$()
           call "APCPLTXT" (#9, dt_txt$, text$(), nbr_lines%)
           txt$   = str(text$(2%), 1%,40%)       /* Obtain Glass Text        */
                                                 /* From Sales Order         */
                                                 /* Need sh() and sh$()      */
                                                 /* Convert the Dimensions   */
                                                 /* from the Part Number     */
                                                 /* Created by Window Wizard */  
           a1, a2, shd(1%), shd(2%), shd(3%), shd(4%) = 0.0
           sh(1%), sh(2%), sh(3%), sh(4%), sh(5%), sh(6%), sh(7%) = 0.0
           if str(sh_codes$,1%,1%) = "?" then goto CS2
           convert str(ct_part$,13%,3%) to a1, data goto CS1
CS1:
           convert str(ct_part$,16%,1%) to a2, data goto CS2
CS2:
           sh(1%),shd(1%) = a1 + (a2/8.0)        /* Decimal Width            */

           a1 = 0.0 : a2 = 0.0
           if str(sh_codes$,2%,1%) = "?" then goto CS4
           convert str(ct_part$,17%,2%) to a1, data goto CS3
CS3:
           convert str(ct_part$,19%,1%) to a2, data goto CS4
CS4:
           sh(2%), shd(2%) = a1 + (a2/8.0)       /* Decimal Height           */
     
           a1 = 0.0 : a2 = 0.0
           if str(sh_codes$,3%,1%) = "?" then goto CS6
           convert str(ct_part$,20%,2%) to a1, data goto CS5
CS5:
           convert str(ct_part$,22%,1%) to a2, data goto CS6
CS6:
           sh(3%),shd(3%) = a1 + (a2/8.0)        /* Decimal Leg Height       */

                                                 /* Fourth Dimension is in   */
                                                 /* 1st three digits of the  */
                                                 /* Glass Text               */ 
           a1 = 0.0 : a2 = 0.0
           if str(sh_codes$,4%,1%) = "?" then goto CS8
           convert str(txt$,1%,2%) to a1, data goto CS7
CS7:
           convert str(txt$,3%,1%) to a2, data goto CS8
CS8:
           sh(4%),shd(4%) = a1 + (a2/8.0)        /* Decimal Leg2 Height      */
     
        return 
       
        lookup_specials                         /* Octagon/Circles     */
           init(" ") readkey$, desc$
           str(readkey$,1%,9%)   = "SHAPEXTRA"
           str(readkey$,10%,15%) = sh_model$
           read #3,key = readkey$, using CAL_2, desc$,               ~
                                    eod goto lookup_specials_done
                shape_cross$ = str(desc$,1%,2%)

        lookup_specials_done

        return
        
        check_grid_size
           init(" ") sh_type$, contour$, lk$, cl$

           sh_type$ = "3"               /* 5/8 Inch Grid   */ 
           cl$ = str(ct_part$,4%,1%)
           lk$ = str(ct_part$,12%,1%)
           if cl$ = "H" or cl$ = "I" or cl$ = "G" or cl$ = "J" then ~
              sh_type$ = "2"            /* 3/4 Inch Grid   */

           p% = pos(lks$ = lk$)
           if p% <> 0% then sh_type$ = "1"
                                       /* 1 Inch Grid      */

                                       /* Contour Grid Y/N */
           contour$ = "N"
           if sh_type$ = "1" then contour$ = "Y"
                                       /* Grid Size        */
           if sh_type$ = "1" then str(ct_rec$,44%,3%) = "1  "
           if sh_type$ = "2" then str(ct_rec$,44%,3%) = "3/4"
           if sh_type$ = "3" then str(ct_rec$,44%,3%) = "5/8"
 
                                       /* Grid Thickness   */
           if sh_type$ = "1" then str(ct_rec$,90%,6%) = ".75000"  /* 1"   */
           if sh_type$ = "2" then str(ct_rec$,90%,6%) = ".81250"  /* 3/4" */
           if sh_type$ = "3" then str(ct_rec$,90%,6%) = ".62500"  /* 5/8" */
 
           str(ct_rec$,183%,1%) = contour$

                                               /* RHHTEST */
        REM   call "SHOSTAT" ("Grid Size ---- > " & sh_type$ )
        REM   stop

        return

        check_for_temp
           str(ct_rec$,125%,1%) = "N"
           str(ct_rec$,126%,1%) = "N" 
           init(" ") readkey$, desc$
           str(readkey$, 1%,9%)  = "PLAN TEMP"
           str(readkey$,10%,15%) = sh_glass$           /* Glass Code */
           read #3,key = readkey$, using CAL_2, desc$,                  ~
                                       eod goto check_for_temp_done
CAL_2:          FMT POS(25), CH(30)
           str(ct_rec$,125%,1%) = "Y"                  /* Tempered   */
           str(ct_rec$,126%,1%) = "Y"                  /* Grid Only  */                                  
        check_for_temp_done
                                               /* RHHTEST */
        REM   call "SHOSTAT" ("Tempered Flags - " & str(ct_rec$,125%,2%) )
        REM   stop

        return        

        lookup_reason                         /* Get Reason Descrip */
           init(" ") readkey$, desc$, so_reason_d$
           check% = 0%
           str(readkey$,1%,9%)   = "CUSTOMWRR"
           str(readkey$,10%,15%) = so_reason$
           read #3,key = readkey$, using CAL_2, desc$,               ~
                                    eod goto lookup_reason_done

        REM   if shape_sel$ <> "1" then str(ct_rec$,185%,10%) = str(desc$,1%,10%)  
           if shape_sel$  = "1" then so_reason_d$ = str(desc$,1%,24%)     ~
                                else so_reason_d$ = str(ct_rec$,185%,10%)
           check% = 1%
        lookup_reason_done

        return
        
        convert_fraction

           wd1$ = "         "
           if shc(k%) < 1.0 then goto convert_fraction_done
 
           calc = shc(k%)                      /* Convert size   (3) */
           gosub convert_sixteen
           convert a% to str(wd1$,1%,3%), pic(###)

           if b% = 0% then goto convert_fraction_done
                                                 /* Check For Fraction */
              str(wd1$,5%,5%) = str(sz$,(b%*5%) - 4%, 5%)
        convert_fraction_done
           shc$(k%) = wd1$                     /* Calc with Fraction   */
        return

        convert_sixteen
            calc = round( calc, 4 )
            a% = int(calc)
            b% = int((calc - a%) * 10000)
            if b% = 0% then goto convert_sixteen_done     /****************/
               d% = int(b%/625)                           /* Conversion of*/
               if mod(b%,625) <> 0 then d% = d% + 1%      /* Decimals to  */
                  b% = d%                                 /*  Sixteen's   */
                                                          /****************/
                  if b% <> 16% then goto convert_sixteen_done
                     a% = a% + 1% : b% = 0%         /* A% = WHOLE PART */
convert_sixteen_done:
        return


       open_work
            if mode% = 1% then mode$ = "OUTPT"
            if mode% = 2% then mode$ = "INPUT"
            if mode% = 3% then mode$ = "SHARE"

            call "WORKOPN2" (#10,mode$, 500%, f2%)
            if f2% <> 0% then goto WORK_1
        return
WORK_1:     call "SHOSTAT" ("Error - Cannot Open (AWDGLSWK)") : stop
        return
        delete_work
            call "FILEBGON" (#10)
        return



        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program

            end
