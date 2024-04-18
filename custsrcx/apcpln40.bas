        REM *************************************************************~
            *              ( Replaces - Old (APCPLN40) )                *~
            *  Program Name      - APCPLN40                             *~
            *  Creation Date     - 01/06/97                             *~
            *  Last Modified Date- 09/17/02                             *~
            *  Written By        - Royal H. Hoffman                     *~
            *  Modification By   - Christie M Gregory                   *~
            *                                                           *~
            *  Description       - Production/Stock Reports for Items   *~
            *                      "Not Scanned" or "Scanned" for a     *~
            *                      Specific Production Period.          *~
            *                                                           *~
            *  Code Tables Used  - (PLAN DEPT) - New Departments Loaded *~
            *                                    from this Master Table.*~
            *                                                           *~
            *  Subroutine Used   - (APCPLN1B) - Used to Display         *~
            *                                   Planning Codes          *~
            *                                                           *~
            *                    - (APCPLC40) - Screen Utility for      *~
            *                                   Scanned Units.(APCPLN2B)*~
            *  Special Comments  - (Note) Subroutine (APCPLB40) is the  *~
            *                      Exact Same Program as (APCPLN40)     *~
            *                                                           *~
            *                    - '099' Invalid Product Codes are put  *~
            *                            into this Department.          *~
            *                                                           *~
            *                    - Parts = Less than 19 Digits          *~
            *                                                           *~
            *                    - Sashs = Screen Codes 4=TSO, 5=BSO,   *~
            *                              and 6=FGO                    *~
            *                                                           *~
            *                    - PF(12) = Used to Purge Old Data.     *~
            *                               Only Turned on when on      *~
            *                               Field (2). Only RHH, JBF    *~
            *                                                           *~
            *                    - DT_SC% = Toggle Flag used to Switch  *~
            *                               between (1%) Production Date*~
            *                               (2%) Scanned Date.          *~
            *                                                           *~
            *           Screens/Reports   = Mod to Report Data Based on *~
            *                               a Production Day between    *~
            *                               7 AM to Next Day at 6 59 AM *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/06/97 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 01/06/97 ! Contains all Mods Associated with Old    ! RHH *~
            *          ! Scanning (APCPLN40) and Scan Program     !     *~
            *          !   (APCSCAN4).                            !     *~
            * 01/31/97 ! Mods to Correct Problems                 ! RHH *~
            * 02/15/97 ! Mods to Count Sashs and Parts when Charge! RHH *~
            * 08/21/97 ! Mod - Note SC_PROC$ the process code is  ! RHH *~
            *          !   no longer used. Now the option for     ! RHH *~
            *          !   Scanned and Un-Scanned Load Data       !     *~
            *          !   SC_LOAD$ is Beginning and ED_load$ End !     *~
            * 09/23/97 ! Mod to clean-Up Purge Subroutine. Also   ! RHH *~
            *          !   purge (APCPLNAD), (APCPLNSD),(APCPLNDT)!     *~
            * 11/14/97 ! Mod for Upgrade to Release R6.04.03      ! RHH *~
            * 03/21/98 ! Y2K                                      ! LDJ *~
            * 05/27/98 ! (EWD001) - Mod for setting Beg/End Dates ! RHH *~
            *          !   using the Beginning and Ending Load No.!     *~
            * 06/17/99 ! (EWD002) - Mod For correcting Begin/End  ! RHH *~
            *          !   to conform to load data. Use Scheduled !     *~
            *          !   production date instead of planned.    !     *~
            * 08/18/99 ! (EWD003) - Mod to replace 'OPENCHEK'     ! RHH *~
            *          !            with new 'EWDOPEN' Subroutine !     *~
            *          !            to improve HP speed.          !     *~
            * 02/28/00 ! (EWD004) - Mod for change in parts with  ! CMG *~
            *          !            code in CLMR or WALLWIDTH     !     *~
            * 06/19/00 ! (EWD005) - Mod to exclude dept 001 in    ! CMG *~
            *          !            total when selection is '5'.  !     *~
            * 07/05/01 ! (EWD006) Mod to add '@' on screen for    ! CMG *~
            *          !           loads with a parent load of 'A'!     *~
            *          !           for backorders.                !     *~
            * 09/17/02 ! (EWD007) Fix for Special Shapes Grid Code! CMG *~
            * 05/13/03 ! (EWD008) Mod for Scanning Orders.        ! CMG *~
            * 06/18/03 ! (EWD009) Mod to put Parent Load on Screen! CMG *~
            * 09/11/03 ! (EWD010) Mod to put Resort Screen Data.  ! CMG *~
            * 10/28/03 ! (EWD011) Mod for ending date when load   ! CMG *~
            *          !       range specified                    !     *~
            * 06/04/04 ! (EWD012) Quick fix for bad scan and prd  ! CMG *~
            *          !       date ranges when using a load range!     *~
            * 04/05/05 ! (AWD013) Mod to the reclen of APCPLNSD   ! CMG *~
            * 03/06/07 ! (AWD014) Add not scan dept 32 w/ all RGA ! DES *~
            * 10/03/07 ! (AWD015) is status 15, only show as rga  ! DES *~
            *01/03/2008! (AWD016) change display to not blink     ! DES *~
            *02/15/2008! (AWD017) Add scanned for RGA 032         ! DES *~
            *06/12/2015! (SR65848) Fix Dept #032 to show on option! PWW *~
            *          !           #3. Show sts 15 on option #1.  !     *~
            *06/17/2015! (SR66095) Add HowShip Code 20.           ! PWW *~
            *07/31/2015! (SR67518) Don't show sts 15 on option #1 ! PWW *~
            *08/05/2015! (SR67684) Add Option #8 for product      ! PWW *~
            *          !  Scanned but NOT Staged.                 !     *~
            *10/12/2016! (CR456) DC Center status 15 RGA          ! CMG *~
            *03/02/2020! (CR2443) New dept 108 reporting          ! RDB *~
            *04/14/2020! (CR2502) New dept 106 reporting          ! RDB *~
            *************************************************************

        dim                              /* (APCPLNDT) - FILE          */~
            dt_key$57, db$100,           /* Alt Key (1)                */~
            dt_load$5, stock$1,          /* Production Load Number     */~
            dt_bar$18, sav_so$8,         /* Planned Barcode            */~
            dt_dept$3,                   /* Production Department Code */~
            dt_proc$2,                   /* Production Process Code    */~
            dt_date$6, dt_date1$8,       /* Scheduled/Scanned Date     */~
            dt_st$2, dt_st_d$10,         /* Prod. Status Code-PLAN STAT*/~
            dt_shft$2,                   /* Production Shift Code      */~
            dt_seq$5,                    /* Production Sequence No.    */~
            dt_ref$8,                    /* Production Reference Number*/~
            dt_dte$6, dt_dte1$8,         /* Production Date            */~
            dt_dte1x$8, tempdte$10, logmsg$255,                          ~
            dt_time$8,                   /* Scheduled/Scanned Time     */~
            dt_cust$9,                   /* S.O. Customer Code         */~
            dt_fil$17, cnt$53,           /* Filler Area                */~
            dt_part$25, cst(6%),         /* Part Number - Report Only  */~
            xx_dte$6, dt_sash$1,         /*                            */~
            dt_prt$1, dt_samp$1,         /*                            */~
            dt_wood$3, dt_rec$256,       /* WOOD SURROUND '000' = N/A  */~
/*          SR$(7%)30,                      SCREEN CODE                */~
/*SR67684*/ sr$(8%)30,                   /* SCREEN CODE                */~
            bg_prompt$26, rpt_desc$10,   /* Beginning Date Prompt      */~
            ed_prompt$23,                /* Ending Date Prompt         */~
            model$3,                     /* Model Code  - Report Only  */~
            wrk_date$6,                  /* Work File Date (EWD010)    */~
            wrk_date1$10,                /* Work File Date (EWD010)    */~
            wrk_key$54                   /* Work File Key  (EWD010)    */
            
        /* cr2443 */ dim dsp_ss$5

        dim                              /* (Program) - Variables      */~
            filename$8,                  /* Used for EWDOPEN   EWD003) */~
/*          H1$(7%)50, H2$(7%)50,           SCREEN/REPORT HEADERS      */~
/*SR67684*/ h1$(8%)50, h2$(8%)50,        /* Screen/Report Headers      */~
/*          H3$(7%)40, H4$(7%)40,           SCREEN/REPORT HEADERS      */~
/*SR67684*/ h3$(8%)40, h4$(8%)40,        /* Screen/Report Headers      */~
/*          H5$79, H6$(7%)40,               SCREEN/REPORT HEADERS      */~
/*SR67684*/ h5$79, h6$(8%)40,            /* Screen/Report Headers      */~
            tab$(5%)9,                   /* Lookup Table Names         */~
            cc$(100%)3%,                 /* Store Department Codes     */~
            supp$(100%)3%,               /* Store Department Codes     */~
            dd$(100%)30,                 /* Store Department Descripts */~
            ss%(100%,3%),                /* Store Scanned Dept Totals  */~
            nn%(100%,3%),                /* Store Not Scanned Dept Tot */~
            ss_tot%(3%),                 /* Store Scanned Totals       */~
            nn_tot%(3%),                 /* Store Not Scanned Totals   */~
            hdr$40, msg$(3%)79,          /* Askuser - Var's            */~
            sc_date$10, sc_dte$6,        /* Beginning Production Date  */~
            ed_date$10, ed_dte$6,        /* Ending Production Date     */~
            sc_sel$1, sc_sel_d$30,       /* Report Selection           */~
            sc_id$3, sc_id_d$30,         /* Scanning Id.               */~
            sc_dept$3,sc_dept_d$30,      /* Department Selection       */~
            sc_load$5,sc_load_d$30,      /* Prod. Production Load or Al*/~
            ed_load$5,                   /* Ending Production Load     */~
            sav_dte$6, sav_date$10,      /* Save Production Date Totals*/~
            txt$(100%)36,                /* Display Screen Text        */~
            title$40, date$8,            /* REPORT TITLE               */~
            runtime$8,                   /* REPORT RUN TIME            */~
            readkey$50, desc$30,         /* Generic Key                */~
            p_shft$2,                    /* Shift Code for Calc        */~
            p_mod$(306%)3,               /* Model Codes                */~
            p_unt%(306%,3%),             /* Product Unit Values        */~
            p_unts%(306%,3%),            /* Product Unit Values Samples*/~
            p_untss%(306%,3%),           /* Charged Sash Unit Values   */~
            p_untpp%(306%,3%),           /* Charged Part Unit Values   */~
            p_val(306%,3%), p_mrp(6%,3%),/* Product Dollar Values      */~
            cursor%(2%),                 /* Cursor location for edit   */~
            edtmessage$79,               /* Edit screen message        */~
            message$256,                 /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            x$10, datex$10,              /* Temporary Date variables   */~
            userid$3                     /* Current User Id            */

        dim f2%(15%),                    /* = 0 if the file is open    */~
            f1%(15%),                    /* = 1 if READ was successful */~
            fs%(15%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(6%)20                  /* Text from file opening     */

        dim c1$(6000%)5, count$26,       /* Product Sequence No.       */~
            c2$(6000%)25,                /* Product Part Number        */~
            c3$(6000%)18,                /* Product Barcode            */~
            c4$(6000%)6,                 /* Customer Code              */~
            c5$(6000%)5,                 /* Load Number                */~
            c6$(6000%)8,                 /* Production Date (EWD006)   */~
            c7$(6000%)5,                 /* Parent Load     (EWD009)   */~
            bo$(6000%)1,                 /* BackOrder Code (EWD006)    */~
            tt_dte$6, bb_dte$6, ee_dte$6,/* Calc Scanned Dates         */~
            ap$2, jdate1$7, jdate2$7     /* 'AM/PM' - Julian Dates     */

        dim des_key$33,tmp1$18, tmp2$18  /* AWD017 */
        dim des_tbl$(1000%)18            /* AWD017 */
        dim des_st$2, des_bar$18

        dim ad_key$33, ad_rec$64,        /* Audit Key and Record       */~
            ad_time$8, sd_key$23,        /* Scanned Time               */~
            ad_dte$6,                    /* Scanned Date               */~
            ad_shft$2,                   /* Scanned Shift              */~
            ad_st$2                      /* Scanned Status             */


        dim                              /* FILE = APCPLNSC            */~
            sc_key$10,                   /* Primary Key   EWD006       */~
            sc_sav$10,                   /* Save readkey  EWD006       */~
            pload$5                      /* Parent Load   EWD006       */


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(EWD) Production Scanning Reports       "
            pname$ = "APCPLN40 - Rev: R6.04"

        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                     S E L E C T                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! APCPLNAD ! (NEW) Planning Audit File                *~
            * #2  ! APCPLNDT ! (NEW) Planning Detail File (Old APCPLNTK)*~
            * #3  ! GENCODES ! Master Code Tables File                  *~
            * #4  ! APCPLNDP ! Master Department File                   *~
            * #5  ! APCPLNLD ! Planning/Scheduling Load Master          *~
            * #6  ! APCPLNSD ! Planning Schedule Detail                 *~
            * #7  ! APCPLNSC ! Planning Master Schedule-Old APCLINES EWD006*~
            * #8  ! APCPLNOR ! New Planninf S. O. Header                *~
            * #10 ! WORKFILE ! Workfile to sort screen data     (EWD010)*~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "APCPLNAD",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos = 19,   keylen = 31,                      ~
                        alt key 1, keypos =  1, keylen = 31

            select #2,  "APCPLNDT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos = 24,   keylen = 23,                      ~
                        alt key 1, keypos = 47, keylen = 57,             ~
                            key 2, keypos = 53, keylen = 51,             ~
                            key 3, keypos =  1, keylen = 23, dup,        ~
                            key 4, keypos = 96, keylen =  8, dup

            select #3,  "GENCODES",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos = 1,    keylen = 24

            select #4,  "APCPLNDP",                                      ~
                        varc,     indexed,  recsize =   32,              ~
                        keypos = 11,   keylen = 12,                      ~
                        alt key 1, keypos =  9, keylen = 14,             ~
                            key 2, keypos =  4, keylen = 12,             ~
                            key 3, keypos =  1, keylen = 15

            select #5,  "APCPLNLD",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 11,   keylen =  5,                      ~
                        alt key 1, keypos =  3, keylen = 13,             ~
                            key 2, keypos =  1, keylen = 15
/* (AWD013) - Mod to key and reclen */
            select #6,  "APCPLNSD",                                      ~
                        varc,     indexed,  recsize =  64,               ~
                        keypos = 1,    keylen = 23

                                                     /*  (EWD006)       */
            select #7, "APCPLNSC",                                       ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =   24, keylen =   10,                    ~
                        alt key  1, keypos =    7, keylen =  27,         ~
                            key  2, keypos =    1, keylen =  33
/*SR66095 + */
            select #8,  "APCPLNOR",                                      ~
                        varc,     indexed,  recsize =  170,              ~
                        keypos = 1,    keylen = 51,                      ~
                        alt key 1, keypos = 27, keylen = 25,             ~
                            key 2, keypos = 70, keylen =  8, dup,        ~
                            key 3, keypos = 78, keylen =  8, dup,        ~
                            key 4, keypos = 52, keylen =  8,             ~
                            key 5, keypos = 36, keylen = 16, dup
/*SR66095 - */
            select #10, "WORKFILE",                                      ~
                        varc, indexed, recsize =  120,                   ~
                        keypos = 1,  keylen = 54

            call "SHOSTAT" ("Initialization")
                                                        /* (EWD003)    */
            filename$ = "APCPLNAD" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNDT" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GENCODES" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNDP" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNLD" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNSD" : call "EWDOPEN" (#6, filename$, err%)
            if err% <> 0% then gosub open_error
/*SR66095*/ filename$ = "APCPLNOR" : call "EWDOPEN" (#8, filename$, err%)
            if err% <> 0% then gosub open_error
                                                        /* (EWD003)    */
                                                    /*  (EWD006)       */
            filename$ = "APCPLNSC" : call "EWDOPEN" (#7, filename$, err%)
            if err% <> 0% then gosub open_error

            mat f1% = zer
            mat fs% = zer
            init(" ") rslt$()
            des_cnt% = 0%

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            init(" ") h1$(), h2$(), h3$(), h4$(), h5$, h6$()
        mat ss% = zer : mat ss_tot% = zer
        mat nn% = zer : mat nn_tot% = zer
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)

          sr$(1%) = "(1) - Production Not Scanned  "
          sr$(2%) = "(2) - Stock Not Scanned       "
          sr$(3%) = "(3) - Production Scanned      "
          sr$(4%) = "(4) - Stock Production Scanned"
          sr$(5%) = "(5) - Sales Orders Not Scanned"
          sr$(6%) = "(6) - Product Received Scanned"
          sr$(7%) = "(7) - Product Not Received    "
          sr$(8%) = "(8) - Product Not Staged      "    /*SR67684 */

          tab$(1%) = "PLAN DEPT" : tab$(4%) = "PLAN SHFT"
          tab$(2%) = "PLAN PROC" : tab$(3%) = "PLANINLIN"

          h1$(1%) = "New-Production Detail Screen'Not Scanned'-APCPLN40"
          h1$(2%) = "New-Stock Detail Screen 'Not Scanned'    -APCPLN40"
          h1$(3%) = "New-Production Detail Screen 'Scanned'   -APCPLN40"
          h1$(4%) = "New-Stock Detail Screen   'Scanned'      -APCPLN40"
          h1$(5%) = "New-Prod Detail (No Stock) 'Not Scanned' -APCPLN40"
          h1$(6%) = "New-Prod Detail Received   'Scanned'     -APCPLN40"
          h1$(7%) = "New-Prod Detail Received   'Not Scanned' -APCPLN40"
          h1$(8%) = "New-Prod Detail Not Staged 'Not Staged'  -APCPLN40"

          h2$(1%) = "New-Production Summary Screen'Not Scanned-APCPLN40"
          h2$(2%) = "New-Stock Summary Screen  'Not Scanned'  -APCPLN40"
          h2$(3%) = "New-Production Summary Screen  'Scanned' -APCPLN40"
          h2$(4%) = "New-Stock Summary Screen    'Scanned'    -APCPLN40"
          h2$(5%) = "New-Prod Summary (No Stock) 'Not Scanned'-APCPLN40"
          h2$(6%) = "New-Prod Summary Received  'Scanned'     -APCPLN40"
          h2$(7%) = "New-Prod Summary Received  'Not Scanned' -APCPLN40"
          h2$(8%) = "New-Prod Summary Not Staged 'Not Staged' -APCPLN40"

          h3$(1%) = "Build Screen for 'Not Scanned' Product"
          h3$(2%) = "Build Screen for 'Not Scanned'  Stock "
          h3$(3%) = "Build Screen for   'Scanned'   Product"
          h3$(4%) = "Build Screen for   'Scanned'    Stock "
          h3$(5%) = "Build Screen for(No Stock)-Not Scanned"
          h3$(6%) = "Build Screen for Received  'Scanned'  "
          h3$(7%) = "Build Screen for Recevied'Not Scanned'"
          h3$(8%) = "Build Screen for Not Stag 'Not Staged'"

          h4$(1%) = "Printing Production'Not Scanned'Report"
          h4$(2%) = "Printing Stock 'Not Scanned' Report"
          h4$(3%) = "Printing Production  'Scanned'  Report"
          h4$(4%) = "Printing Stock   'Scanned'   Report"
          h4$(5%) = "Printing Prod (No Stock) 'Not Scanned'"
          h4$(6%) = "Printing Production 'Scanned'         "
          h4$(7%) = "Printing Production 'Not Scanned'     "
          h4$(8%) = "Printing Production 'Not Staged'      "

          h6$(1%) = "Scanning Report of Products*Not Scanned*"
          h6$(2%) = "Scanning Report of Stock   *Not Scanned*"
          h6$(3%) = "Scanning Report of Products ***Scanned**"
          h6$(4%) = "Scanning Report of Stock    ***Scanned**"
          h6$(5%) = "Scanning Rpt (No Stock) Prod*Not Scanned"
          h6$(6%) = "Scanning Report Received   'Scanned'    "
          h6$(7%) = "Scanning Report Received   'Not Scanned'"
          h6$(8%) = "Scanning Report Not Staged  'Not Staged'"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 5%
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
L11120:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 5% then editpg1
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
            if rpt% = 1% then goto L19100              /* Report Format */
               call "SHOSTAT" ( h3$(sc_sel%) )
               goto L19110
L19100:     gosub select_printer
L19110:

           gosub select_report_type

            if rpt% = 1% then goto L19240              /* Report Format */
               if sc_dept$ = "ALL" then goto L19200
                  i% = 1%
L19160:           gosub'102(sc_sel$)           /* Screen Detail Format */
                  if keyhit% = 1% then gosub startover
                  if keyhit% = 16% then goto L19250
                     goto L19160
L19200:        gosub'103(sc_sel$)    /* Screen Department Total Format */
               if keyhit% = 1% then gosub startover
               if keyhit% = 16% then goto L19250
                  goto L19200
L19240: close printer
L19250: return clear all
        goto inputmode

        select_report_type

REM           if str(bg_prompt$,11%,4%) = "Scan" and sc_sel% <> 3%      ~
                    then goto generate_all
           if str(bg_prompt$,11%,4%) <> "Scan" then goto prd_dte
              if sc_sel% <> 1% and sc_sel% < 6% then goto generate_all
/*AWD014   IF SC_SEL$ = "1" AND SC_DEPT$ = "032"                        ~
                    THEN GOTO GENERATE_ALL  (CR456) */
prd_dte:
           if sc_sel% < 6% and sc_dept$ = "ALL"                         ~
                 and str(sc_load$,1%,3%) = "ALL" then goto generate_all
               gosub generate_report
            return
        generate_all
            gosub generate_report_all
        return

        select_printer
            call "SHOSTAT" ( h4$(sc_sel%) )
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
REM  "SELECT,1=NOT SCAN,2=NOT STOCK,3=SCAN,4=STK,5=NOT SO,6=REC,7=NOT REC"
        scrn1_msg  :  data                                               ~
         "Enter a Valid Beg/End Scan or Production Date?               ",~
/*"SELECT,1=NOT SCAN,2=NOT STOCK,3=SCAN,4=STK,5=NOT SO,6=REC,7=NOT REC,*/~
    "Select,1=Not Scan,2=Not Stock,3=Scan,4=StK,5=Not SO,6=Rec,7=Not Rec,~
         ~8=Not Staged",                                                 ~
         "Enter a Valid Shift Code, or (AL) = 'ALL' ?                  ",~
         "Enter a Valid Department Code Selection, or ALL ?            ",~
         "Enter a Valid Production Load or (ALL)?                      "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, readkey$, dt_load$,dt_bar$,~
                      dt_dept$, dt_proc$, dt_date$, dt_st$, dt_shft$,    ~
                      dt_seq$, dt_ref$, dt_dte$, dt_time$,dt_cust$,      ~
                      dt_fil$, sc_date$, sc_dte$, sc_sel$, sc_sel_d$,    ~
                      sc_id$, sc_id_d$, sc_dept$, sc_dept_d$, sc_load$,  ~
                      sc_load_d$, dt_key$, dt_part$, db$, ed_load$,      ~
                      model$, dt_dte1$, ed_date$,dt_dte1x$,              ~
                      ed_dte$, c1$(), c2$(), c3$(), c4$(), c5$(), c6$(), ~
                      bo$(), c7$() /* (EWD009) */

                                                           /* (EWD006) */
             rpt% = 1%                /* Set Default to Report Print   */
             t_max% = 0%              /* Init Screen Max Counter       */
             mat cst = zer            /* Init all Price/Costing Values */
             dt_sc% = 2%              /* Set Date Index to Scanned     */
             gosub load_departments   /* Load for Screen and Totals    */
             gosub load_supp_departments   /*Load for Screen and Totals*/
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
REM            call "SHOSTAT" (" DATALOAD " )  stop
            err% = 0%
            get #2, using L35040, dt_load$, dt_bar$, dt_dept$, dt_proc$, ~
                                 dt_date$, dt_dte$, dt_st$, dt_ref$,     ~
                                 dt_shft$, dt_seq$, dt_time$, dt_cust$,  ~
                                 cst(), dt_part$, dt_sash$, dt_prt$,     ~
                                 dt_samp$, dt_wood$

REM            IF DT_BAR$ = "075706200400040006" THEN CALL "SHOSTAT" ("BAR")
REM            IF DT_BAR$ = "075706200400040006" THEN STOP

            if all% <> 1% then goto not_ad
            dt_shft$ = ad_shft$                    /* Override with    */
            dt_dte$  = ad_dte$                     /* Audit Data       */
            dt_time$ = ad_time$
            dt_st$   = ad_st$
            des_bar$ = dt_bar$

not_ad:
            dt_date1$ = dt_date$                   /* Production Date  */
            call "DATEFMT" (dt_date1$)             /* (EWD006)         */
            dt_dte1$ = dt_dte$
            call "DATEFMT" (dt_dte1$)              /* Scan Date  EWD006*/
            dt_dte1x$ = dt_dte$
            call "DATEFMT" (dt_dte1x$)
            model$ = str(dt_part$,1%,3%)
            for i% = 1% to dept_max%                     /* Find Dept. */
                if dt_dept$ = cc$(i%) then goto L30260   /* Subscript  */
            next i%
L30260:     xx% = i%                              /* 1st Set Dept Value*/
            tt% = 1%                              /* TT% = 1% Window   */
            if dt_sash$ <> "0" then tt% = 2%      /* TT% = 2% Sashs    */
            if dt_prt$  = "Y" then tt% = 3%       /* TT% = 3% Parts    */
            gosub check_samples                   /*  (EWD004)         */

/* <AWD014> */
/*(CR456)*/ REM IF SC_SEL$ = "1" AND DT_ST$ = "15" THEN GOTO L30270
/*(CR456)*/ REM IF SC_SEL$ = "1" AND SC_DEPT$ = "032" THEN GOTO L30275
/*SR65848   IF SC_SEL$ = "3" AND SC_DEPT$ = "032" THEN GOTO L30342 */
            goto L30280
L30270:
/* <AWD015> */
/*SR65848   IF SC_DEPT$ <> "ALL" AND SC_DEPT$ <> "032"  THEN RETURN */
/* </AWD015> */
L30275:
            for i% = 1% to supp_max%                      /* Find Dept. */
            if rga_dept% = 0 then goto L30340
                if dt_dept$ = supp$(i%) then goto L30280  /* Subscript  */
            next i%
REM         nn%(rga_dept%,tt%) = nn%(rga_dept%,tt%) + 1%

REM         goto L30340
L30280:
/* (CR456)  IF SC_SEL$ = "1" AND DT_ST$ = "15" THEN GOTO L30335     */
/*SR65848   IF SC_SEL$ = "3" AND SC_DEPT$ = "032" THEN GOTO L30342  */
/* (CR456)  IF SC_SEL$ <> "1" OR SC_DEPT$ <> "032" THEN GOTO L30340 */

            goto L30340             /* (CR456) */
L30335:     for i% = 1% to supp_max%                       /* Find Dept. */
                if dt_dept$ = supp$(i%) then goto L30360   /* Subscript  */
            next i%
REM         IF DT_ST$ = "15" THEN RETURN
/*(CR456)   IF (DT_ST$ = "15") THEN GOTO L30347 */
/*SR65848   IF (DT_DEPT$ = "032" AND DT_ST$ < "12") THEN GOTO L30347 */
/*(CR456)  GOTO L30360      */                   /* Sashs, Parts By Dept*/

L30340:
/* </AWD014> */
            if sc_sel% = 6% and dt_st$ <> "13" then goto L30360
            if sc_sel% = 7% and dt_st$ > "12" then goto L30360

/*<AWD017>IF SC_SEL% <> 3% OR DT_ST$ <> "15" THEN GOTO L30343  (CR456) */
/*<AWD017>*/
            if sc_sel% <> 3% then goto L30343
L30342:
/*(CR456)   DES_BAR$ = DT_BAR$ */
/*(CR456)   IF DT_ST$ = "15" THEN GOTO L30360 */
/*(CR456)   GOSUB CHECK_SCANNED_RGA */
/*(CR456)   IF DES_SW > 0.0 THEN GOTO L30360 */
/*(CR456)   RETURN */
L30343:
/* IF SC_SEL% <> 6% AND SC_SEL% <> 7% AND DT_ST$ > "11" THEN GOTO L30360*/
/*SR67684 */
           if sc_sel% <> 8% and sc_sel% <> 6% and sc_sel% <> 7% and      ~
              dt_st$ > "11" then goto l30360

L30345:     if str(sc_dept$,1%,3%) = "ALL" and dt_dept$ = "001"        ~
                      then goto L30350          /* (EWD005)            */
L30347:

REM MESSAGE$ = "1 DEPT = " & DT_DEPT$ & ", ST = " & DT_ST$
REM CALL "LOGFILE" (MESSAGE$)

               nn_tot%(tt%) = nn_tot%(tt%) + 1% /* Not Scanned Total   */
L30350:        if str(sc_dept$,1%,3%) = "ALL" and dt_dept$ = "001"     ~
                       then goto L30355         /* (EWD005)            */
/*(CR456)      IF (DT_ST$ = "15" AND RGA_DEPT% > 0) THEN              ~
                  NN%(RGA_DEPT%,TT%) = NN%(RGA_DEPT%,TT%) + 1%        ~
               ELSE NN%(XX%,TT%) = NN%(XX%,TT%) + 1%  */

/*(CR456) add back or uncomment */
               nn%(xx%,tt%) = nn%(xx%,tt%) + 1%   /* Not Scanned Windows*/

L30355:        dt_st_d$ = "*Sched*"
               goto L30490                        /* Sashs, Parts By Dept*/
                                                  /* Scanned Values      */
L30360:
/*SR65848   IF SC_SEL% = 3% AND SC_DEPT$ = "032" THEN XX% = 1%     */
REM MESSAGE$ = "3 DEPT = " & DT_DEPT$ & ", ST = " & DT_ST$
REM CALL "LOGFILE" (MESSAGE$)
            ss_tot%(tt%) = ss_tot%(tt%) + 1%    /* Scanned Total       */
            ss%(xx%,tt%) = ss%(xx%,tt%) + 1%    /* Scanned Windows,    */
                                                /* Sashs, Parts By Dept*/
            dt_st_d$ = "WW Comp"
            if tt% <> 2% then goto L30440
               dt_st_d$ = "SS Comp"
               if cst(1%) <> 0 then dt_st_d$ = "SS Ccnt"
               goto L30480
L30440:     if tt% <> 3% then goto L30480
               dt_st_d$ = "PP Comp"
               if cst(1%) <> 0 then dt_st_d$ = "PP Ccnt"

L30480:     if dt_samp$ <> "0" then dt_st_d$ = "Samples"
L30490:     if sc_dept$ = "ALL" then return
                                            /* Totals Only     */
                                            /* Screen and Total Arrays */
                                            /* For Screen Utility Load,*/
            gosub check_backorder           /*    (EWD006)             */
            if rpt% = 0% then gosub load_screen
        return

        check_scanned_rga              /*  (AWD014)         */
           des_st$ = "  "
           init(hex(00)) des_key$
        des_sw = 0.0
           des_sw2 = 1.0
           gosub check_rga_table
           if des_sw2 < 1.0 then goto L60441
           str(des_key$,1%,18%) = des_bar$
L60440:    /* read apcplnad looking for status 15 records */
           read #1, key 1% > des_key$, eod goto L60441
             get #1, using L60445, des_key$

           des_st$ = str(des_key$,32,2)
           tmp1$ = str(des_key$,1,18)
           tmp2$ = str(dt_rec$,24,18)
           if str(des_key$,1%,18%) <> des_bar$ then goto L60441
           if str(des_key$,19%,6%) < sc_dte$ then goto L60440
           if str(des_key$,19%,6%) > ed_dte$ then goto L60440
           if des_st$ <> "15" and str(des_key$,25,3) <> "032"        ~
                                                   then goto L60440
           des_sw = 1.0
L60441:    return

        check_rga_table
           des_sw2 = 1.0
           if des_cnt% > 1000% then return
           for l% = 1% to des_cnt% + 1%
              if str(des_tbl$(l%),1,1) = " " then goto L60442
              if des_bar$ = des_tbl$(l%) then goto L60443
              goto L60444

L60442:       des_cnt% = des_cnt% + 1%  /* insert */
          des_tbl$(l%) = des_bar$
              l% = des_cnt% + 1%
          goto L60444

L60443:       des_sw2 = 0.0  /* already printed */
              l% = des_cnt% + 1%

L60444:    next l%

L60445:    FMT CH(33)
        return

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
                                         /* File = (APCPLNDT)          */
L35040: FMT CH(05),                      /* Production Load Number     */~
            XX(05),                      /* Drop Seq No.               */~
            XX(02),                      /* Customer Drop No.          */~
            XX(05),                      /* Prod/Customer Sort Code    */~
            XX(02),                      /* Prod S.O. Line Item No.    */~
            XX(04),                      /* Prod Item No. Fld #3       */~
            CH(18),                      /* Planned Barcode            */~
            CH(03),                      /* Prod. Dept Code - PLAN DEPT*/~
            CH(02),                      /* Prod. Proc Code - PLAN PROC*/~
            CH(06),                      /* Production Date as Planned */~
            CH(06),                      /* Production Scanned Date    */~
            XX(03),                      /* Prod. Dept Code            */~
            XX(02),                      /* Prod. Proc Code   PLAN PROC*/~
            CH(02),                      /* Prod. Status Code-PLAN STAT*/~
            XX(30),                      /* User Defined Sort Index    */~
            CH(08),                      /* Warranty/Remake Number     */~
            CH(02),                      /* Scheduled/Scanned Shift Cd */~
            XX(05),                      /* Prod Sort Seq.(APCPLNDP)   */~
            CH(05),                      /* Prod. Seq. Number          */~
            CH(08),                      /* Prod Scan Time HH MM ?m    */~
            CH(09),                      /* Customer Assoc. with S.O.  */~
            6*PD(14,4),                  /* S.O Price for Unit Net     */~
                                         /* Product Material Cost      */~
                                         /* Product Labor Cost         */~
                                         /* Product Overhead Cost      */~
                                         /* Product Freight Cost       */~
                                         /* Product Vinyl Discount     */~
            XX(08),                      /* Dept Unit Per Manhour Used */~
            CH(25),                      /* MFG Part Number            */~
            CH(01),                      /* Sash 0=No,1=Top,2=Bot,3=Fix*/~
            CH(01),                      /* MFG Part (Y)es or (N)o     */~
            CH(01),                      /* Sample 0=No,1=Sample,2=Disp*/~
            CH(03)                       /* Wood Surround Code         */


        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
L40070:       gosub set_pf1

              gosub'050(1%, fieldnr%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40200,         /* Scanning Date        */~
                                L40210,         /* Report Selection     */~
                                L40200,         /* Shift Code or AL     */~
                                L40200,         /* Department Selection */~
                                L40200          /* Prod Shift or All    */
              goto L40230

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40200:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40210:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40230:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(84)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), fac(hex(84)), bg_prompt$             , ch(26),~
               at (03,30), fac(lfac$(1%)), sc_date$             , ch(10),~
                                                                         ~
               at (03,42), fac(hex(84)), ed_prompt$             , ch(23),~
               at (03,67), fac(lfac$(1%)), ed_date$             , ch(10),~
                                                                         ~
               at (04,02), "Selection Codes (1 thru 7):",                ~
               at (04,30), fac(lfac$(2%)), sc_sel$              , ch(01),~
               at (04,42), fac(hex(84)), sc_sel_d$              , ch(30),~
                                                                         ~
               at (05,02), "Shift Code or (AL) = All  :",                ~
               at (05,30), fac(lfac$(3%)), sc_id$               , ch(02),~
               at (05,42), fac(hex(84)), sc_id_d$               , ch(30),~
                                                                         ~
               at (06,02), "Prod. Dept. Code or (ALL) :",                ~
               at (06,30), fac(lfac$(4%)), sc_dept$             , ch(03),~
               at (06,42), fac(hex(84)), sc_dept_d$             , ch(30),~
                                                                         ~
               at (07,02), "Beg. Prod. Load or (ALL)  :",                ~
               at (07,30), fac(lfac$(5%)), sc_load$             , ch(05),~
               at (07,42), "End Load:",                                  ~
               at (07,52), fac(lfac$(5%)), ed_load$             , ch(05),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 5% then goto L40630
                  gosub calc_scanned
                  goto L40070

L40630:        if keyhit% <> 6% then goto L40680
                  table% = 1%                      /* Department Codes */
                  gosub display_codes
                  goto L40070

L40680:        if keyhit% <> 7% then goto L40730
                  table% = 2%                      /* Process Codes    */
                  gosub display_codes
                  goto L40070

L40730:        if keyhit% <> 9% then goto L40780    /* Shift Codes      */
                  table% = 4%
                  gosub display_codes
                  goto L40070

L40780:        if keyhit% <> 10% then goto L40830 /* Screen Utility     */
                  rpt% = 0%                       /* Change Flag for    */
                  gosub print_report              /* Screen Only Display*/
                  goto L40070

L40830:        if keyhit% <> 11% then goto L40870
REM                 if mod(dt_sc%,2%) = 0% then dt_sc% = 1%             ~
                                           else dt_sc% = 2%
                    if dt_sc% = 1% then dt_sc% = 2%
                    if dt_sc% = 2% then dt_sc% = 1%
                  goto L40070

L40870:        if keyhit% <> 12% then goto L40910
                  gosub purge_data
                  goto L40070

L40910:        if keyhit% <> 15 then goto L40950
                  call "PRNTSCRN"
                  goto L40070

L40950:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
           if dt_sc% <> 1% then goto L41050
              bg_prompt$ = "Beginning Production Date:"
              ed_prompt$ = "Ending Production Date:"
              rpt_desc$  = "Production"
              goto L41090
L41050:    bg_prompt$ = "Beginning Scanning Date  :"
           ed_prompt$ = "Ending Scanning Date  :"
           rpt_desc$  = " Scanning "

L41090: if edit% = 2% then L41260     /*  Input Mode             */
            pf$(1%) = "(1)Start Over    (4)Previous Field      " &       ~
                      "(9)Shift Codes         (12)Purge Data  "
            pf$(2%) = "                 (6)Department Codes    " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                 (7)Process Codes       " &       ~
                      "(11)Toggle Prod/Scan   (16)Exit Program"
            pfkeys$ = hex(01ffff04ff0607ff09ff0b0cffff0f1000)
            if fieldnr% = 1% then L41190
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L41190:     if fieldnr% > 1% then L41210
                str(pf$(1%),18%,18%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L41210:     if userid$ <> "RHH" and userid$ <> "JBF" then goto L41230
            if fieldnr% = 2% then goto L41240
L41230:         str(pf$(1%),64%)    = " " : str(pfkeys$,12%,1%) = hex(ff)
L41240:     return

L41260: if fieldnr% > 0% then L41370  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over    (5)Calculate Units     " &       ~
                      "(9)Shift Codes         (14)Print Report"
            pf$(2%) = "                 (6)Department Codes    " &       ~
                      "(10)Screen Utility     (15)Print Screen"
            pf$(3%) = "                 (7)Process Codes       " &       ~
                      "(11)Toggle Prod/Scan   (16)Exit Program"
            pfkeys$ = hex(01ffffff050607ff090a0bffff0e0f1000)
            if str(sc_dept$,1%,1%) <> "A" then return
                str(pf$(1%),18%,22%)= " " : str(pfkeys$, 5%,1%) = hex(ff)
            return
L41370:
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                                       "
            pf$(3%) = "                                        " &       ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

        REM *************************************************************~
            *         S T A T U S   D I S P L A Y   S C R E E N         *~
            *************************************************************

        deffn'102(sc_sel$)
L42050:     gosub set_pf2
            accept                                                       ~
               at (01,02), fac(hex(84)), h1$(sc_sel%)           , ch(50),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
                                                                         ~
               at (03,02), fac(hex(84)), bg_prompt$             , ch(26),~
               at (03,30), fac(hex(84)),   sc_date$             , ch(10),~
                                                                         ~
               at (03,42), fac(hex(84)), ed_prompt$             , ch(23),~
               at (03,67), fac(hex(84)),   ed_date$             , ch(10),~
                                                                         ~
               at (04,02), "Screen Selection Code     :",                ~
               at (04,30), fac(hex(84)),   sc_sel$              , ch(01),~
               at (04,42), fac(hex(84)), sc_sel_d$              , ch(30),~
                                                                         ~
               at (05,02), "Screen Selected Shift Code:",                ~
               at (05,30), fac(hex(84)),   sc_id$               , ch(02),~
               at (05,42), fac(hex(84)), sc_id_d$               , ch(30),~
                                                                         ~
               at (06,02), "Production Department Code:",                ~
               at (06,30), fac(hex(84)),   sc_dept$             , ch(03),~
               at (06,42), fac(hex(84)), sc_dept_d$             , ch(30),~
                                                                         ~
               at (07,02), "Beg. Prod. Load or (ALL)  :",                ~
               at (07,30), fac(hex(84)),   sc_load$             , ch(05),~
               at (07,42), "End Load:",                                  ~
               at (07,52), fac(hex(84)), ed_load$               , ch(05),~
                                                                         ~
               at (08,03),                                               ~
                  " Seq. <----- Part Number -----> <--- Barcode ---->",  ~
               at (09,02),                                               ~
                  " ----- ------------------------- ------------------", ~
               at (08,54),                                               ~
                  "Cust.  Load  Prd Dte  PLoad",                         ~
               at (09,54),                                               ~
                  "------ ----- -------- -----",                         ~
/*(EWD006)*/   at (10,01), fac(hex(84)), bo$(i%  )              , ch(01),~
               at (10,04), fac(hex(84)), c1$(i%  )              , ch(05),~
               at (10,10), fac(hex(84)), c2$(i%  )              , ch(25),~
               at (10,35), fac(hex(84)), c3$(i%  )              , ch(18),~
               at (10,54), fac(hex(84)), c4$(i%  )              , ch(06),~
               at (10,61), fac(hex(84)), c5$(i%  )              , ch(05),~
               at (10,67), fac(hex(84)), c6$(i%  )              , ch(08),~
/*(EWD009)*/   at (10,76), fac(hex(84)), c7$(i%  )              , ch(05),~
                                                                         ~
/*(EWD006)*/   at (11,01), fac(hex(84)), bo$(i%+1%)             , ch(01),~
               at (11,04), fac(hex(84)), c1$(i%+1%)             , ch(05),~
               at (11,10), fac(hex(84)), c2$(i%+1%)             , ch(25),~
               at (11,35), fac(hex(84)), c3$(i%+1%)             , ch(18),~
               at (11,54), fac(hex(84)), c4$(i%+1%)             , ch(06),~
               at (11,61), fac(hex(84)), c5$(i%+1%)             , ch(05),~
               at (11,67), fac(hex(84)), c6$(i%+1%)             , ch(08),~
/*(EWD009)*/   at (11,76), fac(hex(84)), c7$(i%+1%)             , ch(05),~
                                                                         ~
/*(EWD006)*/   at (12,01), fac(hex(84)), bo$(i%+2%)             , ch(01),~
               at (12,04), fac(hex(84)), c1$(i%+2%)             , ch(05),~
               at (12,10), fac(hex(84)), c2$(i%+2%)             , ch(25),~
               at (12,35), fac(hex(84)), c3$(i%+2%)             , ch(18),~
               at (12,54), fac(hex(84)), c4$(i%+2%)             , ch(06),~
               at (12,61), fac(hex(84)), c5$(i%+2%)             , ch(05),~
               at (12,67), fac(hex(84)), c6$(i%+2%)             , ch(08),~
/*(EWD009)*/   at (12,76), fac(hex(84)), c7$(i%+2%)             , ch(05),~
                                                                         ~
/*(EWD006)*/   at (13,01), fac(hex(84)), bo$(i%+3%)             , ch(01),~
               at (13,04), fac(hex(84)), c1$(i%+3%)             , ch(05),~
               at (13,10), fac(hex(84)), c2$(i%+3%)             , ch(25),~
               at (13,35), fac(hex(84)), c3$(i%+3%)             , ch(18),~
               at (13,54), fac(hex(84)), c4$(i%+3%)             , ch(06),~
               at (13,61), fac(hex(84)), c5$(i%+3%)             , ch(05),~
               at (13,67), fac(hex(84)), c6$(i%+3%)             , ch(08),~
/*(EWD009)*/   at (13,76), fac(hex(84)), c7$(i%+3%)             , ch(05),~
                                                                         ~
/*(EWD006)*/   at (14,01), fac(hex(84)), bo$(i%+4%)             , ch(01),~
               at (14,04), fac(hex(84)), c1$(i%+4%)             , ch(05),~
               at (14,10), fac(hex(84)), c2$(i%+4%)             , ch(25),~
               at (14,35), fac(hex(84)), c3$(i%+4%)             , ch(18),~
               at (14,54), fac(hex(84)), c4$(i%+4%)             , ch(06),~
               at (14,61), fac(hex(84)), c5$(i%+4%)             , ch(05),~
               at (14,67), fac(hex(84)), c6$(i%+4%)             , ch(08),~
/*(EWD009)*/   at (14,76), fac(hex(84)), c7$(i%+4%)             , ch(05),~
                                                                         ~
/*(EWD006)*/   at (15,01), fac(hex(84)), bo$(i%+5%)             , ch(01),~
               at (15,04), fac(hex(84)), c1$(i%+5%)             , ch(05),~
               at (15,10), fac(hex(84)), c2$(i%+5%)             , ch(25),~
               at (15,35), fac(hex(84)), c3$(i%+5%)             , ch(18),~
               at (15,54), fac(hex(84)), c4$(i%+5%)             , ch(06),~
               at (15,61), fac(hex(84)), c5$(i%+5%)             , ch(05),~
               at (15,67), fac(hex(84)), c6$(i%+5%)             , ch(08),~
/*(EWD009)*/   at (15,76), fac(hex(84)), c7$(i%+5%)             , ch(05),~
                                                                         ~
/*(EWD006)*/   at (16,01), fac(hex(84)), bo$(i%+6%)             , ch(01),~
               at (16,04), fac(hex(84)), c1$(i%+6%)             , ch(05),~
               at (16,10), fac(hex(84)), c2$(i%+6%)             , ch(25),~
               at (16,35), fac(hex(84)), c3$(i%+6%)             , ch(18),~
               at (16,54), fac(hex(84)), c4$(i%+6%)             , ch(06),~
               at (16,61), fac(hex(84)), c5$(i%+6%)             , ch(05),~
               at (16,67), fac(hex(84)), c6$(i%+6%)             , ch(08),~
/*(EWD009)*/   at (16,76), fac(hex(84)), c7$(i%+6%)             , ch(05),~
                                                                         ~
/*(EWD006)*/   at (17,01), fac(hex(84)), bo$(i%+7%)             , ch(01),~
               at (17,04), fac(hex(84)), c1$(i%+7%)             , ch(05),~
               at (17,10), fac(hex(84)), c2$(i%+7%)             , ch(25),~
               at (17,35), fac(hex(84)), c3$(i%+7%)             , ch(18),~
               at (17,54), fac(hex(84)), c4$(i%+7%)             , ch(06),~
               at (17,61), fac(hex(84)), c5$(i%+7%)             , ch(05),~
               at (17,67), fac(hex(84)), c6$(i%+7%)             , ch(08),~
/*(EWD009)*/   at (17,76), fac(hex(84)), c7$(i%+7%)             , ch(05),~
                                                                         ~
/*(EWD006)*/   at (18,01), fac(hex(84)), bo$(i%+8%)             , ch(01),~
               at (18,04), fac(hex(84)), c1$(i%+8%)             , ch(05),~
               at (18,10), fac(hex(84)), c2$(i%+8%)             , ch(25),~
               at (18,35), fac(hex(84)), c3$(i%+8%)             , ch(18),~
               at (18,54), fac(hex(84)), c4$(i%+8%)             , ch(06),~
               at (18,61), fac(hex(84)), c5$(i%+8%)             , ch(05),~
               at (18,67), fac(hex(84)), c6$(i%+8%)             , ch(08),~
/*(EWD009)*/   at (18,76), fac(hex(84)), c7$(i%+8%)             , ch(05),~
                                                                         ~
/*(EWD006)*/   at (19,01), fac(hex(84)), bo$(i%+9%)             , ch(01),~
               at (19,04), fac(hex(84)), c1$(i%+9%)             , ch(05),~
               at (19,10), fac(hex(84)), c2$(i%+9%)             , ch(25),~
               at (19,35), fac(hex(84)), c3$(i%+9%)             , ch(18),~
               at (19,54), fac(hex(84)), c4$(i%+9%)             , ch(06),~
               at (19,61), fac(hex(84)), c5$(i%+9%)             , ch(05),~
               at (19,67), fac(hex(84)), c6$(i%+9%)             , ch(08),~
/*(EWD009)*/   at (19,76), fac(hex(84)), c7$(i%+9%)             , ch(05),~
                                                                         ~
                                                                         ~
               at (22,02), fac(hex(84)),   h5$                  , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 2% then goto L43220
L43190:           i% = 1%
                  goto L42050

L43220:        if keyhit% <> 3% then goto L43290
L43230:           x% = int(t_max% / 10% )
                  i% = (x% * 10%)
                  if mod(t_max%,10%) <> 0% then i% = i% + 1%
                  if i% = 0% then i% = 1%
                  goto L42050

L43290:        if keyhit% <> 4% then goto L43340
                  if i% < 11% then goto L43190
                  i% = i% - 10%
                  goto L42050

L43340:        if keyhit% <> 5% then goto L43390
                  i% = i% + 10%
REM               if i% < t_max% then goto L42050
                  if i% <= t_max% then goto L42050
                  goto L43230

L43390:        if keyhit% <> 15 then goto L43430
                  call "PRNTSCRN"
                  goto L42050

L43430:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
            for kk% = 1% to dept_max%                     /* Find Dept. */
                if sc_dept$ = cc$(kk%) then goto L43520   /* Subscript  */
            next kk%

L43520:     xx% = kk%                              /* 1st Set Dept Value*/
            init(" ") h5$
/*AWD014    IF SC_SEL% = 3% AND SC_DEPT$ = "032" THEN XX% = 1%       */
/*SR65848   IF SC_SEL% = 3% AND SC_DEPT$ = "032" THEN XX% = 1%       */
            if sc_sel% > 2% and sc_sel% < 5% then goto L43700
               x1% = nn%(xx%,1%)                             /* Windows*/
               x2% = nn%(xx%,2%)                             /* Sashs  */
               x3% = nn%(xx%,3%)                             /* Parts  */
/*(CR456)IF SC_SEL$ <> "1" OR SC_DEPT$ <> "032" THEN GOTO L43550 */
              if sc_sel$ <> "1" then goto L43550

               x1% = nn_tot%(1)
               x2% = nn_tot%(2)
               x3% = nn_tot%(3)

L43550:
            if sc_sel% = 1% then                                         ~
               h5$="Windows NOT Scan= XXXXX Sash = XXXX Parts = XXXX"
            if sc_sel% = 2% then                                         ~
               h5$=" Stock NOT Scan = XXXXX Sash = XXXX Parts = XXXX"
            if sc_sel% = 3% then                                         ~
               h5$="Windows Scanned = XXXXX Sash = XXXX Parts = XXXX"
            if sc_sel% = 4% then                                         ~
               h5$=" Stock  Scanned = XXXXX Sash = XXXX Parts = XXXX"
            if sc_sel% = 5% then                                         ~
               h5$="SO Not Scanned  = XXXXX Sash = XXXX Parts = XXXX"
            if sc_sel% = 6% then                                         ~
               h5$="Windows Received= XXXXX Sash = XXXX Parts = XXXX"
            if sc_sel% = 7% then                                         ~
               h5$="Windows NOT Rece= XXXXX Sash = XXXX Parts = XXXX"
/*SR67684*/ if sc_sel% = 8% then                                         ~
               h5$="Scanned NOT Stag= XXXXX Sash = XXXX Parts = XXXX"

            convert x1% to str(h5$,19%,5%),pic(#####)

            convert x2% to str(h5$,32%,4%),pic(####)

            convert x3% to str(h5$,45%,4%),pic(####)
               goto L43840

L43700:        x1% = ss%(xx%,1%)                             /* Windows*/
               x2% = ss%(xx%,2%)                             /* Sashs  */
               x3% = ss%(xx%,3%)                             /* Parts  */

            if sc_sel% = 1% then                                         ~
               h5$="Windows NOT Scan= XXXXX Sash = XXXX Parts = XXXX"
            if sc_sel% = 2% then                                         ~
               h5$=" Stock NOT Scan = XXXXX Sash = XXXX Parts = XXXX"
            if sc_sel% = 3% then                                         ~
               h5$="Windows Scanned = XXXXX Sash = XXXX Parts = XXXX"
            if sc_sel% = 4% then                                         ~
               h5$=" Stock  Scanned = XXXXX Sash = XXXX Parts = XXXX"
            if sc_sel% = 5% then                                         ~
               h5$="SO Not Scanned  = XXXXX Sash = XXXX Parts = XXXX"
            if sc_sel% = 6% then                                         ~
               h5$="Windows Received= XXXXX Sash = XXXX Parts = XXXX"
            if sc_sel% = 7% then                                         ~
               h5$="Windows NOT Rece= XXXXX Sash = XXXX Parts = XXXX"
/*SR67684*/ if sc_sel% = 8% then                                         ~
               h5$="Scanned NOT Stag= XXXXX Sash = XXXX Parts = XXXX"


            convert x1% to str(h5$,19%,5%),pic(#####)

            convert x2% to str(h5$,32%,4%),pic(####)

            convert x3% to str(h5$,45%,4%),pic(####)

L43840:     pf$(1%) = "(1)Start Over    (3)Last          (5)Nex" &       ~
                      "t                      (15)Print Screen"
            pf$(2%) = "(2)First         (4)Previous            " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(0102030405ffffffffffffffffff0f1000)
            if i% <> 1% then goto L43920
               str(pf$(2%),1%,10%)  = " " : str(pfkeys$,2%,1%) = hex(ff)
               str(pf$(2%),18%,15%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L43920:     if (i% + 10%) <= t_max% then goto L43950
               str(pf$(1%),18%,10%) = " " : str(pfkeys$,3%,1%) = hex(ff)
               str(pf$(1%),35%,10%) = " " : str(pfkeys$,5%,1%) = hex(ff)
L43950: return

        REM *************************************************************~
            *                S U M M A R Y   S C R E E N                *~
            *************************************************************

        deffn'103(sc_sel$)
L44050:     gosub set_pf3
            accept                                                       ~
               at (01,02), fac(hex(84)), h2$(sc_sel%)           , ch(50),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
                                                                         ~
               at (03,02), fac(hex(84)), bg_prompt$             , ch(26),~
               at (03,30), fac(hex(84)),   sc_date$             , ch(10),~
                                                                         ~
               at (03,42), fac(hex(84)), ed_prompt$             , ch(23),~
               at (03,67), fac(hex(84)),   ed_date$             , ch(10),~
                                                                         ~
               at (04,02), "Screen Selection Code     :",                ~
               at (04,30), fac(hex(84)),   sc_sel$              , ch(01),~
               at (04,42), fac(hex(84)), sc_sel_d$              , ch(30),~
                                                                         ~
               at (05,02), "Screen Selected Shift Code:",                ~
               at (05,30), fac(hex(84)),   sc_id$               , ch(02),~
               at (05,42), fac(hex(84)), sc_id_d$               , ch(30),~
                                                                         ~
               at (06,02), "Production Department Code:",                ~
               at (06,30), fac(hex(84)),   sc_dept$             , ch(03),~
               at (06,42), fac(hex(84)), sc_dept_d$             , ch(30),~
                                                                         ~
               at (07,02), "Beg. Prod. Load or (ALL)  :",                ~
               at (07,30), fac(hex(84)),   sc_load$             , ch(05),~
               at (07,42), "End Load:",                                  ~
               at (07,52), fac(hex(84)), ed_load$               , ch(05),~
                                                                         ~
                                                                         ~
               at (08,02),                                               ~
                  "<Dept's Description> Wind. Sash Part",                ~
               at (09,02),                                               ~
                  "-------------------- ----- ---- ----",                ~
               at (08,40),                                               ~
                  "<Dept's Description> Wind. Sash Part",                ~
               at (09,40),                                               ~
                  "-------------------- ----- ---- ----",                ~
               at (10,02), fac(hex(84)), txt$(1% + dp%)         , ch(36),~
               at (10,40), fac(hex(84)), txt$(13% + dp%)        , ch(36),~
                                                                         ~
               at (11,02), fac(hex(84)), txt$(2% + dp%)         , ch(36),~
               at (11,40), fac(hex(84)), txt$(14% + dp%)        , ch(36),~
                                                                         ~
               at (12,02), fac(hex(84)), txt$(3% + dp%)         , ch(36),~
               at (12,40), fac(hex(84)), txt$(15% + dp%)        , ch(36),~
                                                                         ~
               at (13,02), fac(hex(84)), txt$(4% + dp%)         , ch(36),~
               at (13,40), fac(hex(84)), txt$(16% + dp%)        , ch(36),~
                                                                         ~
               at (14,02), fac(hex(84)), txt$(5% + dp%)         , ch(36),~
               at (14,40), fac(hex(84)), txt$(17% + dp%)        , ch(36),~
                                                                         ~
               at (15,02), fac(hex(84)), txt$(6% + dp%)         , ch(36),~
               at (15,40), fac(hex(84)), txt$(18% + dp%)        , ch(36),~
                                                                         ~
               at (16,02), fac(hex(84)), txt$(7% + dp%)         , ch(36),~
               at (16,40), fac(hex(84)), txt$(19% + dp%)        , ch(36),~
                                                                         ~
               at (17,02), fac(hex(84)), txt$(8% + dp%)         , ch(36),~
               at (17,40), fac(hex(84)), txt$(20% + dp%)        , ch(36),~
                                                                         ~
               at (18,02), fac(hex(84)), txt$(9% + dp%)         , ch(36),~
               at (18,40), fac(hex(84)), txt$(21% + dp%)        , ch(36),~
                                                                         ~
               at (19,02), fac(hex(84)), txt$(10% + dp%)        , ch(36),~
               at (19,40), fac(hex(84)), txt$(22% + dp%)        , ch(36),~
                                                                         ~
               at (20,02), fac(hex(84)), txt$(11% + dp%)        , ch(36),~
               at (20,40), fac(hex(84)), txt$(23% + dp%)        , ch(36),~
                                                                         ~
               at (21,02), fac(hex(84)), txt$(12% + dp%)        , ch(36),~
               at (21,40), fac(hex(84)), txt$(24% + dp%)        , ch(36),~
                                                                         ~
               at (22,02), fac(hex(84)),   inpmessage$          , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 2% then goto L44110
L44100:           dp% = 0%
                  goto L44050

L44110:        if keyhit% <> 3% then goto L44130
L44120:           x% = int(dept_max% / 24%)
                  dp% = (x% * 24%)
                  goto L44050

L44130:        if keyhit% <> 4% then goto L44140
                  if dp% < 25% then goto L44100
                  dp% = dp% - 24%
                  if dp% <= 1% then goto L44100
                  goto L44050

L44140:        if keyhit% <> 5% then goto L44150
                  dp% = dp% + 24%
                  if dp% < dept_max% then goto L44050
                  goto L44120

L44150:        if keyhit% <> 15 then goto L44880
                  call "PRNTSCRN"
                  goto L44050

L44880:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf3
            init(" ") txt$(), inpmessage$
/*SR65848   IF SC_SEL% = 3% AND SC_DEPT$ = "032" THEN GOTO L44888 */
            if sc_sel% > 2% and sc_sel% < 5% then goto L45100
L44888:        for i% = 1% to dept_max%
                   txt$(i%) = str(dd$(i%),1%,20%)
                   convert nn%(i%,1%) to str(txt$(i%),22%,5%), pic(#####)
                   convert nn%(i%,2%) to str(txt$(i%),28%,4%), pic(####)
                   convert nn%(i%,3%) to str(txt$(i%),33%,4%), pic(####)
               next i%

            if sc_sel% = 1% then                                         ~
           inpmessage$ = " P r o d u c t   N o t   S c a n n e d - XXXXX"
            if sc_sel% = 2% then                                         ~
           inpmessage$ = " S t o c k  N o t   S c a n n e d      - XXXXX"
            if sc_sel% = 3% then                                         ~
           inpmessage$ = " P r o d u c t   S c a n n e d         - XXXXX"
            if sc_sel% = 4% then                                         ~
           inpmessage$ = "   S t o c k    S c a n n e d          - XXXXX"
            if sc_sel% = 5% then                                         ~
           inpmessage$ = " S a l e s  O r d e r s  N o t  S c a n- XXXXX"
            if sc_sel% = 6% then                                         ~
           inpmessage$ = " W i n d o w s    R e c e i v e d      - XXXXX"
            if sc_sel% = 7% then                                         ~
           inpmessage$ = " W i n d o w s  N O T  R e c e i v e d - XXXXX"
/*SR67684*/ if sc_sel% = 8% then                                         ~
           inpmessage$ = " S c a n n e d  N O T  S t a g e d     - XXXXX"

               xx% = nn_tot%(1%) + nn_tot%(2%) + nn_tot%(3%)
               convert xx% to str(inpmessage$,42%,5%), pic(#####)
               goto L45240

L45100:        for i% = 1% to dept_max%
                   txt$(i%) = str(dd$(i%), 1%,20%)
                   convert ss%(i%,1%) to str(txt$(i%),22%,5%), pic(#####)
                   convert ss%(i%,2%) to str(txt$(i%),28%,4%), pic(####)
                   convert ss%(i%,3%) to str(txt$(i%),33%,4%), pic(####)
               next i%

            if sc_sel% = 1% then                                         ~
           inpmessage$ = " P r o d u c t   N o t   S c a n n e d - XXXXX"
            if sc_sel% = 2% then                                         ~
           inpmessage$ = " S t o c k  N o t   S c a n n e d      - XXXXX"
            if sc_sel% = 3% then                                         ~
           inpmessage$ = " P r o d u c t   S c a n n e d         - XXXXX"
            if sc_sel% = 4% then                                         ~
           inpmessage$ = "   S t o c k    S c a n n e d          - XXXXX"
            if sc_sel% = 5% then                                         ~
           inpmessage$ = " S a l e s  O r d e r s  N o t  S c a n- XXXXX"
            if sc_sel% = 6% then                                         ~
           inpmessage$ = " W i n d o w s    R e c e i v e d      - XXXXX"
            if sc_sel% = 7% then                                         ~
           inpmessage$ = " W i n d o w s  N O T  R e c e i v e d - XXXXX"
/*SR67684*/ if sc_sel% = 8% then                                         ~
           inpmessage$ = " S c a n n e d  N O T  S t a g e d     - XXXXX"

               xx% = ss_tot%(1%) + ss_tot%(2%) + ss_tot%(3%)
               convert xx% to str(inpmessage$,42%,5%), pic(#####)

L45240:     pf$(1%) = "(1)Start Over       (3)Last             " &       ~
                      " (5)Next               (15)Print Screen"
            pf$(2%) = "(2)First            (4)Previous         " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(0102030405ffffffffffffffffff0f1000)
            gosub check_screen
        return

        check_screen
            if dept_max% > 24% then goto L46000
               gosub no_first
               gosub no_next
               gosub no_last
               gosub no_prev
               return
L46000:      if dp% >= 24% then goto L46010
                gosub no_first
                gosub no_prev
L46010:      if (dp% + 24%) <= dept_max% then goto L46020
                gosub no_last
L46020:      if dp% <= (dept_max% - 24%) then goto L46030
                gosub no_next
L46030: return

        no_first
            str(pf$(2%),1%, 9%)  = " " : str(pfkeys$,2%,1%) = hex(ff)
        return
        no_next
            str(pf$(1%),41%, 9%) = " " : str(pfkeys$,5%,1%) = hex(ff)
        return
        no_last
            str(pf$(1%),20%,9%)  = " " : str(pfkeys$,3%,1%) = hex(ff)
        return
        no_prev
            str(pf$(2%),20%,12%) = " " : str(pfkeys$,4%,1%) = hex(ff)
        return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50150,         /* Beg/End Production Dat*/ ~
                              L50410,         /* Report Selection      */ ~
                              L50550,         /* Scan Id/Shft or AL=ALL*/ ~
                              L50760,         /* Prod. Dept Code or ALL*/ ~
                              L50970          /* Prod. Load No. or All */
            return

L50150: REM Production Date                       SC_DATE$, SC_DTE$
            if sc_date$ <> " " then goto L50190
               sc_date$ = date

L50190:     date% = 0%
            call "DATEOKC" (sc_date$, date%, errormsg$)
            if date% = 0% then return
            x$ = sc_date$
            call "DATUFMTC" (x$)
            bb_dte$ = x$
            sc_dte$ = str(x$,1%,6%)
        REM Ending Production Date                ED_DATE$, ED_DTE$
            if ed_date$ <> " " then goto L50290
               ed_date$ = sc_date$

L50290:     date% = 0%
            call "DATEOKC" (ed_date$, date%, errormsg$)
            if date% = 0% then return
            x$ = ed_date$
            call "DATUFMTC" (x$)
            ee_dte$ = x$
            ed_dte$ = str(x$,1%,6%)
            if sc_dte$ > ed_dte$ then goto L50370
        return
L50370:     errormsg$ = "(Error) - Invalid Date Range?"
            init(" ") sc_date$, sc_dte$, ed_date$, ed_dte$
        return

L50410: REM Report Selection                      SC_SEL$, SC_SEL_D$
            if sc_sel$ <> " " then goto L50450
               sc_sel$ = "1"

L50450:     sc_sel% = 1%
            convert sc_sel$ to sc_sel%, data goto L50510

/*          if sc_sel% < 1% or sc_sel% > 7% then goto L50510 */
/*SR67684*/ if sc_sel% < 1% or sc_sel% > 8% then goto L50510
               sc_sel_d$ = sr$(sc_sel%)
        return
L50510:  errormsg$="(Error) - Invalid Report Selection? (1 thru 6)"
          init(" ") sc_sel$, sc_sel_d$
        return

L50550: REM Scanning ID.                          SC_ID$, SC_ID_D$
            if sc_id$ <> " " then goto L50610
L50570:        sc_id$   = "AL"
               sc_id_d$ = "(ALL) - Shift Codes      "
               return

L50610:     if sc_id$ = "AL" then goto L50570
            convert sc_id$ to xx%, data goto L50660

            convert xx% to sc_id$, pic(00)

L50660:     table% = 4%
            code$ = sc_id$
            gosub check_code
            if code% = 0% then goto L50720
               sc_id_d$ = desc$
        return
L50720:   errormsg$="(Error) - Invalid Shift Code Selection?"
          init(" ") sc_id$, sc_id_d$
        return

L50760: REM Department Select                     SC_DEPT$, SC_DEPT_D$
            if sc_dept$ <> " " then goto L50820
L50780:        sc_dept$ = "ALL"
               sc_dept_d$ = "(ALL) - Departments  "
               return

L50820:     if sc_dept$ = "ALL" then goto L50780
            convert sc_dept$ to xx%, data goto L50870

            convert xx% to sc_dept$, pic(000)

L50870:     table% = 1%
            code$ = sc_dept$
            gosub check_code
            if code% = 0% then goto L50930
               sc_dept_d$ = desc$
        return
L50930:   errormsg$="(Error) - Invalid Production Department Code?"
          init(" ") sc_dept$, sc_dept_d$
        return

L50970: REM Production Load or ALL                SC_LOAD$, ED_LOAD$
            if sc_load$ <> " " then goto L51030
L50990:        sc_load$   = "ALL  "
               ed_load$   = "ALL  "
               return

L51030:     if str(sc_load$,1%,3%) = "ALL" then goto L50990
               convert sc_load$ to sc_load%, data goto L51090

               convert sc_load% to sc_load$, pic(00000)
               goto L51130
                                               /* Alpha or Stock Loads */
L51090:        convert str(sc_load$,2%,4%) to sc_load%, data goto L51310

               convert sc_load% to str(sc_load$,2%,4%), pic(0000)

L51130:        read #5,key = sc_load$, eod goto L51310
                  init(" ") x$
                  get #5, using L51135, x$           /* (EWD001) - Mod */
L51135:             FMT POS(46), CH(8)               /* (EWD002) - Mod */
                  call "DATUFMTC" (x$)

                  day% = 9%                         /*  (EWD012)  - BEG */
                  call "DAY" addr(x$, day%)
                  if day% = 2% then goto first_day
                  day% = day% - 2%                  /* 2% for Monday */
                  day% = -(day%)

                  call "DATE" addr("G+", x$, day%, x$, err%)
                                                    /*  (EWD012) - END */
first_day:
                  sc_dte$ = str(x$,1%,6%)
                  sc_date$ = x$
                  call "DATFMTC" (SC_DATE$)

                                                      /* (EWD001) - Mod */
        REM Ending Load No.
               if ed_load$ <> " " then goto L51180
                  ed_load$ = sc_load$

L51180:        convert ed_load$ to ed_load%, data goto L51230

               convert ed_load% to ed_load$, pic(00000)
               goto L51270
                                               /* Alpha or Stock Loads */
L51230:        convert str(ed_load$,2%,4%) to ed_load%, data goto L51340

               convert ed_load% to str(ed_load$,2%,4%), pic(0000)

L51270:

                                                     /*  (EWD011) - Beg  */
REM read #5,key = ed_load$, eod goto L51340
REM                  init(" ") x$                      /* (EWD001) - Mod */
REM                  get #5, using L51280, x$
REM L51280             FMT POS(86), CH(8)             /* (EWD002) - Leave*/

                  ret% = 0%
                  call "DATE" addr("G+", date, 1%, x$, ret%)
                                                     /*  (EWD011) - END  */
                  call "DATUFMTC" (x$)                /* as Planned Load */
                  ed_dte$ = str(x$,1%,6%)             /* Date            */
                  ed_date$ = x$
                  call "DATFMTC" (ed_date$)
                                                      /* (EWD001) - Mod */
           if ed_load$ < sc_load$ then goto L51370
        return
L51310:   errormsg$="(Error) - Invalid Beginning Production Load?"
          init(" ") sc_load$, ed_load$
        return
L51340:   errormsg$="(Error) - Invalid Ending Production Load?"
          init(" ") sc_load$, ed_load$
        return
L51370:   errormsg$="(Error) - Invalid Ending Production Load?"
          init(" ") sc_load$, ed_load$
        return

        display_codes
            call "APCPLN1B" (table%, #3)
        return

        check_code
           code% = 0%
           readkey$ = " "
           str(readkey$,1%,9%)    = tab$(table%)
           str(readkey$,10%,15%)  = code$
           read #3,key = readkey$, using L51520, desc$,                   ~
                                                           eod goto L51540
L51520:        FMT POS(25), CH(30)
           code% = 1%
L51540: return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

L55040: %+---------------------------------------------------------------~
        ~---------------+
L55060: %!---------------------------------------------------------------~
        ~---------------!
L55080: %! ######## @ ########    #######################################~
        ~#   Page: #### !

        %!                                                               ~
        ~               !

L55140: %! Department  : ##############################  ########## Beg ~
       ~Date: ##########!

L55170: %! Shift Code .: ##############################  Load:##### End ~
       ~Date: ##########!

L55200: %!Seq. !<------ Part Number ---->!<--- Bar Code --->!Load !Custom~
        ~!Dept! Status  !

L55230: %!#####!#########################!##################!#####!######~
        ~!### ! ####### !

L55260: %!-----!-------------------------!------------------!-----!------~
        ~!----!---------!

L55290: %!Seq. !<--- Bar Code --->!Load !Custom!Dep!Scan DTE!T i m e !Shf~
        ~t!Model!Status !

L55320: %!#####!##################!#####!######!###!########!########! ##~
        ~ ! ### !#######!

L55350: %!-----!------------------!-----!------!---!--------!--------!---~
        ~-!-----!-------!

                                                  /* Totals Header and */
                                                  /* Detail Formats    */

L55400: %! Department  : ##############################      Prod. Total ~
        ~Date:##########!

L55430: %! Shift Code  : ##############################      Prod. Load N~
        ~o.  :    ##### !

L55460: %!############################## Windows: #####-  Sashs: ####-  P~
        ~arts: ####-    !

L55490: %! Total for all Departments     Windows: #####-  Sashs: ####-  P~
        ~arts: ####-    !

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        load_screen
            if workfile% = 1% then goto write_work_file    /*  (EWD010)  */
            if t_max% > 5999% then t_max% = 5999%   /* MAX SCREEN SIZE */
            t_max% = t_max% + 1%
            c1$(t_max%) = dt_seq$
            c2$(t_max%) = dt_part$
            c3$(t_max%) = dt_bar$ 
            c4$(t_max%) = dt_cust$
            c5$(t_max%) = dt_load$
            c6$(t_max%) = dt_date1$
            c7$(t_max%) = pload$                               /*EWD009*/
            if sc_sel% > 2% and sc_sel% < 5% then c6$(t_max%) = dt_dte1$
*       RHH
*       RHH   CONVERT CST(1%) TO C6$(T_MAX%), PIC(####.##-)
            if str(pload$,1%,1%) = "A" then bo$(t_max%) = "@"  /*EWD006*/
/*SR66095 + */
            or_key$ = str(dt_bar$,1%, 8%)
            or_hows$ = "  "
            read #8, key 4% = or_key$, using or_fmt, or_hows$,          ~
                                                      eod goto skip_or
or_fmt:              FMT POS(92), CH(02)
            if or_hows$ = "90" then bo$(t_max%) = "E"
        skip_or
/*SR66095 - */
        return
        write_work_file                                    /*  (EWD010)  */
              init(" ") wrk_date$
              wrk_date$ = dt_date$
REM              if sc_sel% > 2% and sc_sel% < 5% then wrk_date$ = dt_dte1$

              init(" ") wrk_key$
              str(wrk_key$,1%,6%)   = wrk_date$
              str(wrk_key$,7%,5%)   = dt_seq$
              str(wrk_key$,12%,25%) = dt_part$
              str(wrk_key$,37%,18%) = dt_bar$
              read #10, hold, key = wrk_key$, eod goto L60420
                   delete #10 
L60420:
              put #10, using L60400, wrk_date$,                           ~
                                     dt_seq$,                             ~
                                     dt_part$,                            ~
                                     dt_bar$,                             ~
                                     dt_cust$,                            ~
                                     dt_load$,                            ~
                                     pload$

L60400:            FMT CH(6), CH(5), CH(25), CH(18), CH(9), CH(5), CH(5)

              write #10
        return
        load_screen_wrk
            if workfile% <> 1% then return
            init(" ") dt_seq$, dt_part$, dt_bar$, dt_cust$,              ~
                      dt_load$, wrk_date$, wrk_date1$
            wrk_key$ = all(hex(00))
            read #10, key > wrk_key$, eod goto screen_wrk_done
                  goto screen_wrk_first
        load_screen_wrk_nxt
            read #10, eod goto screen_wrk_done

screen_wrk_first

                get #10, using L60400, wrk_date$,                         ~
                                     dt_seq$,                             ~
                                     dt_part$,                            ~
                                     dt_bar$,                             ~
                                     dt_cust$,                            ~
                                     dt_load$,                            ~
                                     pload$
                delete #10

            wrk_date1$ = wrk_date$
            call "DATEFMT" (wrk_date1$)
            if t_max% > 5999% then t_max% = 5999%   /* MAX SCREEN SIZE */
            t_max% = t_max% + 1%
            c1$(t_max%) = dt_seq$
            c2$(t_max%) = dt_part$
            c3$(t_max%) = dt_bar$
            c4$(t_max%) = dt_cust$
            c5$(t_max%) = dt_load$
            c6$(t_max%) = wrk_date1$
            c7$(t_max%) = pload$

            if str(pload$,1%,1%) = "A" then bo$(t_max%) = "@"
/*SR66095 + */
            or_key$ = str(dt_bar$,1%, 8%)
            or_hows$ = "  "
            read #8, key 4% = or_key$, using or_fmt, or_hows$,         ~
                                                        eod goto skip_or2
            if or_hows$ = "90" then bo$(t_max%) = "E"
        skip_or2
/*SR66095 - */
                 goto load_screen_wrk_nxt
        screen_wrk_done
        return                                             /*  (EWD010)  */

        check_backorder                  /*  (EWD006) */
           init(" ") sc_key$, sc_sav$, pload$
REM           str(sc_key$,1%,8%) = str(dt_key$,1%,8%)
REM           str(sc_key$,9%,2%) = str(dt_key$,9%,2%)
           str(sc_key$,1%,10%) = str(dt_bar$,1%,10%)

           read #7, key = sc_key$, eod goto backorder_done

             get #7, using L60500, pload$
L60500:         FMT POS(105), CH(05)
        str(sc_sav$,1%,10%) = str(sc_key$,1%,10%)

        backorder_done
        return

        print_hdr_tot
           print page
           pageno% = pageno% + 1%
           print using L55040
           print using L55080, date$, runtime$, title$, pageno%
           print using L55400, sc_dept_d$, sav_date$
           print using L55430, sc_id_d$, sc_load$
           lcntr% = 4%
        return                                                           ~

        print_header
           if lcntr% <> 99% then print using L55040
           print page
           pageno% = pageno% + 1%
           print using L55040
           print using L55080, date$, runtime$, title$, pageno%
           print using L55140, sc_dept_d$, rpt_desc$, sc_date$
           print using L55170, sc_id_d$, sc_load$, ed_date$
           print using L55060
           if sc_sel% < 3% or sc_sel% > 4% then print using L55200        ~
                                           else print using L55290
           lcntr% = 6%
        return

        print_detail
           if rpt% = 0% then return                  /* Screen Display */
           if workfile% = 1% then goto write_work_print   /*  (EWD010)  */

        print_wrk_detail
           if rpt% = 0% then return                  /* Screen Display */
           if lcntr% > 57% then gosub print_header
           if sc_sel% > 2% and sc_sel% < 5% then goto L60520
              print using L55260
              print using L55230, dt_seq$, dt_part$, dt_bar$, dt_load$,   ~
                                 dt_cust$, dt_dept$, dt_st_d$
              goto L60560

L60520:       print using L55350
              print using L55320, dt_seq$, dt_bar$, dt_load$, dt_cust$,   ~
                               dt_dept$, dt_dte1x$, dt_time$, dt_shft$,   ~
                                 model$, dt_st_d$
L60560:       lcntr% = lcntr% + 2%
        return
        write_work_print                                   /*  (EWD010)  */

              init(" ") wrk_key$
              str(wrk_key$,1%,6%)   = wrk_date$
              str(wrk_key$,7%,5%)   = dt_seq$
              str(wrk_key$,12%,25%) = dt_part$
              str(wrk_key$,37%,18%) = dt_bar$
              read #10, hold, key = wrk_key$, eod goto L60420
                   delete #10  
REM              if sc_sel% > 2% and sc_sel% < 5% then wrk_date$ = dt_dte1$

              put #10, using L60450, wrk_date$,                           ~
                                     dt_seq$,                             ~
                                     dt_part$,                            ~
                                     dt_bar$,                             ~
                                     dt_cust$,                            ~
                                     dt_load$,                            ~
                                     pload$,                              ~
                                     dt_dept$,                            ~
                                     dt_st_d$,                            ~
                                     dt_time$,                            ~
                                     dt_shft$,                            ~
                                     model$,                              ~
                                     dt_dte$,                             ~
                                     dt_date$

L60450:            FMT CH(6), CH(5), CH(25), CH(18), CH(9), CH(5), CH(5), ~
                       CH(3), CH(10), CH(8), CH(2), CH(3), CH(6), CH(6)

              write #10
        return
        print_screen_wrk
            if workfile% <> 1% then return
            init(" ") dt_seq$, dt_part$, dt_bar$, dt_cust$,              ~
                      dt_load$, wrk_date$, wrk_date1$, dt_load$, pload$, ~
                      dt_dept$, dt_st_d$, dt_time$, dt_shft$, model$,    ~
                      dt_dte$, dt_date$

            wrk_key$ = all(hex(00))
            read #10, key > wrk_key$, eod goto print_wrk_done
                  goto print_wrk_first
        print_screen_wrk_nxt
            read #10, eod goto screen_wrk_done

print_wrk_first

                get #10, using L60450, wrk_date$,                         ~
                                     dt_seq$,                             ~
                                     dt_part$,                            ~
                                     dt_bar$,                             ~
                                     dt_cust$,                            ~
                                     dt_load$,                            ~
                                     pload$,                              ~
                                     dt_dept$,                            ~
                                     dt_st_d$,                            ~
                                     dt_time$,                            ~
                                     dt_shft$,                            ~
                                     model$,                              ~
                                     dt_dte$,                             ~
                                     dt_date$
                delete #10

            dt_date1$ = dt_date$
            call "DATEFMT" (dt_date1$)
            dt_dte1$ = dt_dte$
            call "DATEFMT" (dt_dte1$)
            dt_dte1x$ = dt_dte$
            call "DATEFMT" (dt_dte1x$)

                 gosub print_wrk_detail

                 goto print_screen_wrk_nxt
        print_wrk_done
        return                                           /*  (EWD010)    */


        print_totals
           if rpt% = 0% then return                    /* Screen Display */
           sav_date$ = sav_dte$
           call "DATFMTC" (sav_date$)
           if lcntr% <> 99% then print using L55040
           gosub print_hdr_tot
           for i% = 1% to dept_max%
             print using L55060
             if sc_sel% < 3% or sc_sel% > 4% then                        ~
               print using L55460,dd$(i%),nn%(i%,1%),nn%(i%,2%),nn%(i%,3%)~
                                                  else                   ~
               print using L55460,dd$(i%),ss%(i%,1%),ss%(i%,2%),ss%(i%,3%)
           next i%

           print using L55060
           if sc_sel% < 3% or sc_sel% > 4% then                           ~
              print using L55490, nn_tot%(1%), nn_tot%(2%), nn_tot%(3%)   ~
                                                else                      ~
              print using L55490, ss_tot%(1%), ss_tot%(2%), ss_tot%(3%)
           print using L55040
           mat ss% = zer : mat ss_tot% = zer
           mat nn% = zer : mat nn_tot% = zer
           lcntr%   = 99%
        return


        generate_report
           all% = 0%
           init(" ") title$, sav_dte$
           count$ = "Checked Units [ XXXXXXXX ]"
           count% = 0%
           title$ = h6$(sc_sel%)                /* Assign Report Title */
           sav_dte$ = sc_dte$                   /* Set Beginning Date  */

           tt_dte$ = sav_dte$
           gosub calc_scanned_units
REM         IF SAV_DTE$ > ED_DTE$ THEN GOTO GENERATE_DONE_ALL
REM         GOTO GENERATE_NXT_ALL


        return



        generate_report_all
           all% = 1%
           init(" ") title$, sav_dte$
           count$ = "Checked Units [ XXXXXXXX ]"
           count% = 0%
           title$ = h6$(sc_sel%)                /* Assign Report Title */
           sav_dte$ = sc_dte$                   /* Set Beginning Date  */
        generate_nxt_all
           tt_dte$ = sav_dte$
           gosub calc_scanned_all
         if sav_dte$ > ed_dte$ then goto generate_done_all
         goto generate_nxt_all
        generate_done_all

        return

        purge_data
            gosub ok_purge
            if comp% <> 0% then return
            if userid$ <> "RHH" and userid$ <> "DJD" then return

               cnt% = 0% : aud% = 0% : sched% = 0%
               call "SHOSTAT" ("Purging Old Scanning Data")
          cnt$ = "Del (DT)=[ XXXXXXX ] (AD)=[ XXXXXXX ] (SD)=[ XXXXXX ]"
               init(" ") dt_key$, sav_so$
        purge_next
               read #2,hold,key > dt_key$, using L61120, dt_key$, pg_dte$,~
                                                           eod goto L61320
L61120:           FMT POS(24), CH(23), CH(6)
               if mod(cnt%,100%) <> 0 then goto L61210
                  convert cnt% to str(cnt$,12%,7%), pic(#######)

                  convert aud% to str(cnt$,29%,7%), pic(#######)

                  convert sched% to str(cnt$,46%,6%), pic(######)

                  print at(02,14);hex(84); cnt$;
L61210:        if sav_so$ = str(dt_key$,1%,8%) then goto L61240
                  if pg_dte$ > ed_dte$ then goto L61320
                                         /* Purge All or None for S.O. */
L61240:        delete #2
               cnt% = cnt% + 1%
               if sav_so$ = str(dt_key$,1%,8%) then goto purge_next
                  sav_so$ = str(dt_key$,1%,8%)    /* Purge all Data    */
                  gosub purge_audit               /* for Sales Order   */
                  gosub purge_sched
                  goto purge_next

L61320: return clear all
        goto inputmode

        ok_purge
            init(" ") hdr$, msg$()
            comp% = 2%
            hdr$ = "*** Purge Utility Program ***"
            msg$(1) = " All Data Prior-To and Including the Production "
            msg$(2) = "******** Date Entered will be Purged??  ********"
            msg$(3) = "Press <RETURN> To Continue, Any PF() Key To Exit"
            call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
        return

        load_departments
            init(" ") readkey$, cc$(), dd$()
            kk% = 0%
            rga_dept% = 0%
            r% = 0%
            mat ss% = zer : mat ss_tot% = zer    /* Scanned Totals     */
            mat nn% = zer : mat nn_tot% = zer    /* Not Scanned Totals */
            str(readkey$,1%,9%) = "PLAN DEPT"
        load_dept_nxt
            read #3,key > readkey$, using L61540, readkey$, desc$,       ~
                                                 eod goto load_dept_done
L61540:        FMT CH(24), CH(30)
            if str(readkey$,1%,9%) <> "PLAN DEPT" then                   ~
                                                  goto load_dept_done
REM         if str(readkey$,10%,1%) <> "0" then goto load_dept_done
            if str(readkey$,10%,1%) > "1" then goto load_dept_done
               kk% = kk% + 1%
               convert kk% to kk$, pic (##)
               cc$(kk%) = str(readkey$,10%,3%) /* Set Dept Code Value  */
               dd$(kk%) = desc$                /* Set Dept Description */
               if str(readkey$,10%,3%) = "108" then r% = kk%  /* CR2443 */ 
               if str(readkey$,10%,3%) = "106" then u% = kk%  /* CR2508 */                 
               if str(desc$,1,3) = "RGA" then           ~
                    rga_dept% = kk%
               goto load_dept_nxt
        load_dept_done
           dept_max% = kk%
        return

        load_supp_departments
            init(" ") readkey$, supp$()
            kk% = 0%
            str(readkey$,1%,9%) = "PLAN SUPP"
        load_supp_nxt
            read #3,key > readkey$, using L61540, readkey$, desc$,       ~
                                                 eod goto load_supp_done
            if str(readkey$,1%,9%) <> "PLAN SUPP" then                   ~
                                                  goto load_supp_done
               if str(readkey$,10%,3%) = "102" then goto load_supp_nxt
               kk% = kk% + 1%
               supp$(kk%) = str(readkey$,10%,3%) /* Set Dept Code Value  */
               goto load_supp_nxt
        load_supp_done
           supp_max% = kk%
        return

        calc_scanned
           p_screen% = 0%                    /* Display Screen         */
           p_scan% = 0%                      /* Production Not Scanned */
                                             /* Pruction Scanned       */
           if sc_sel% > 2% and sc_sel% < 5% then p_scan% = 1%
           p_flg% = 0% : p_max% = 0%         /* Load Products from     */
                                             /* (APCPLNDP) Data File   */
           p_shft$ = sc_id$                  /* Shift Code 'AL' = All  */
           call "APCPLC40" ( sc_dept$,   /* Specified Department Code  */~
                             sc_dte$,    /* Specified Production Date  */~
                                         /* for a Production Day-Begin */~
                             p_shft$,    /* Shift Code or (AL) = All   */~
                             sc_load$,   /* Prod. Load or (ALL)= All   */~
                             ed_load$,   /* Ending Prod. Load          */~
                             p_mod$(),   /* Department Products        */~
                             p_unt%(),   /* Product Units + Samples    */~
                             p_unts%(),  /* Samples Only               */~
                             p_untss%(), /* Charged Sashes             */~
                             p_untpp%(), /* Charged Parts              */~
                             p_val(),    /* Product Dollar Value       */~
                             p_mrp(),    /* (5) Costing Buckets        */~
                             p_max%,     /* Max Number of Products     */~
                             p_flg%,     /* 0% = Load, 1% = No Load    */~
                             p_scan%,    /* 0%=Not Scanned, 1%=Scanned */~
                             p_screen%,  /* 0% = Yes, 1% = No - Display*/~
                             #4,         /* (APCPLNDP) Master Dept.    */~
                             #2,         /* (APCPLNDT) Prod. Tracking  */~
                             #1,         /* (APCPLNAD) Audit Tracking  */~
                             #3 )        /* (GENCDSIN) Master Code Tab */

        return

        calc_scanned_units
           gosub open_work_file                         /*  (EWD010)    */
REM           if sc_sel% < 6% and sc_id$ = "AL" and sc_dept$ = "ALL"      ~
                 and str(sc_load$,1%,3%) = "ALL" then goto calc_scanned_all
REM           if sc_sel% < 6% and sc_dept$ = "ALL"                        ~
                 and str(sc_load$,1%,3%) = "ALL" then goto calc_scanned_all

                                          /* 1st Check Specified Prod. */
            p_scan% = 0%                     /* Production Not Scanned */
            if sc_sel% > 2% and sc_sel% < 5% then p_scan% = 1%
            if sc_sel% = 6%                  then p_scan% = 2%
            if sc_sel% = 7%                  then p_scan% = 3%
                                          /* For Screen Selection 3 & 4 */
                                          /* P_SCAN = 1 else P_SCAN = 0 */


            if str(bg_prompt$,1%,20%) = "Beginning Production" and ~
               str(sc_load$,1%,3%) = "ALL:" then dt_sc% = 1%
            if str(bg_prompt$,1%,20%) = "Beginning Scanning  " and ~
               str(sc_load$,1%,3%) = "ALL:" then dt_sc% = 2%
            if str(sc_load$,1%,3%) <> "ALL" then dt_sc% = 3%

REM            if dt_sc% = 2% then goto calc_scanned_all

           dt_key$, dt_rec$ = all(hex(00))
           if dt_sc% <> 1% and dt_sc% <> 2% then goto set_3
           str(dt_key$,1%,6%) = bb_dte$

set_3:     if dt_sc% <> 3% then goto read_all
              str(dt_key$,1%,5%) = sc_load$
read_all:
           read #2,key dt_sc% > dt_key$, using L62870, dt_rec$,           ~
                                                eod goto calc_scanned_done
L62870:       FMT CH(256)                       /* Check Load Selection*/
               goto calc_scanned_first
calc_scanned_nxt
           read #2, using L62870, dt_rec$, eod goto calc_scanned_done

calc_scanned_first
              count% = count% + 1%
              if mod(count%,100%) <> 0% then goto L62540
                 convert count% to str(count$,17%,8%), pic(########)

                 print at(02,28);hex(84);count$;
L62540:

REM        IF STR(DT_REC$,24%, 18%) <> "07570620040004000601" THEN  ~
REM                   CALL "SHOSTAT" ("BAR READ")
REM        IF STR(DT_REC$,24%, 18%) <> "07570620040004000601" THEN  ~
REM                   STOP

        pww_cont2
           if dt_sc% <> 1% then goto not_1
              str(dt_key$,1%,57%) = str(dt_rec$,47%,57%)
               if str(dt_key$,1%,6%) > ee_dte$ then goto calc_scanned_done
not_1:

           if dt_sc% <> 2% then goto not_2
              str(dt_key$,1%,57%) = str(dt_rec$,53%,51%)
               if str(dt_key$,1%,6%) > ee_dte$ then goto calc_scanned_done
not_2:

           if dt_sc% <> 3% then goto not_3
              str(dt_key$,1%,57%) = str(dt_rec$,1%,23%)
                 if str(dt_key$,1%,5%) > ed_load$ then                  ~
                                               goto calc_scanned_done
not_3:

/*(CR456)  IF SC_SEL$ = "1" AND SC_DEPT$ = "032" THEN GOTO L62910 */
/*SR65848  IF SC_SEL$ = "3" AND SC_DEPT$ = "032" THEN GOTO L62910 */
           if sc_dept$ = "ALL" then goto L62910
              if str(dt_rec$,42%,3%) <> sc_dept$ then goto calc_scanned_nxt

L62910:    if str(sc_load$,1%,3%) = "ALL" then goto L62920
              if str(dt_rec$,1%,5%) < sc_load$ or                        ~
                 str(dt_rec$,1%,5%) > ed_load$ then goto calc_scanned_nxt

L62920:    REM  call "SHOSTAT" ( " I am here at status check " )
          convert p_scan% to p_scan$, pic (##)
           if sc_sel$ <> "8" then goto notopt8           /*SR67684 */
           if str(dt_rec$,64%,2%) > "11" and             /*SR67684 */     ~
              str(dt_rec$,64%,2%) < "14" then goto L63100
           goto calc_scanned_nxt
       notopt8
/*(CR456)  IF SC_SEL$ = "1" AND SC_DEPT$ = "032" THEN GOTO L62930 */
/*SR65848  IF SC_SEL$ = "3" AND SC_DEPT$ = "032" THEN GOTO L62930 */
/* (CR456) add status 32 check */
           if p_scan% = 0% and ( (str(dt_rec$,64%,2%) < "12") or               ~
             (str(dt_rec$,42%,3%) = "032" and str(dt_rec$,64%,2%) = "32") )    ~
                                                               then goto L62940

REM       IF P_SCAN% = 1% AND (STR(DT_REC$,64%,2%) > "11" AND         ~
                         STR(DT_REC$,64%,2%) < "14") THEN GOTO L62940
/* (CR456) + */
REM       IF P_SCAN% = 1% AND STR(DT_REC$,64%,2%) > "11" THEN GOTO L62940
REM       IF P_SCAN% = 2% AND STR(DT_REC$,64%,2%) = "13" THEN GOTO L62940
REM       IF P_SCAN% = 3% AND STR(DT_REC$,64%,2%) < "13"                 ~
REM              THEN GOSUB CHECK_INLINE
REM       IF P_SCAN% = 3% AND STR(DT_REC$,64%,2%) < "13" AND             ~
REM              INLINE% = 0% THEN GOTO L62940

           if p_scan% = 1% and str(dt_rec$,64%,2%) > "11" then goto L62940
           if p_scan% = 2% and str(dt_rec$,64%,2%) = "13" then goto L62940
           if p_scan% = 3% and str(dt_rec$,64%,2%) < "13" then gosub check_inline
           if p_scan% = 3% and str(dt_rec$,64%,2%) < "13" and             ~
                                                inline% = 0% then goto L62940
/* (CR456) - */
          /* <AWD014> */
L62930:    /* && st = "15"? */
/*SR65848 IF SC_SEL$ = "1" AND STR(DT_REC$,64%,2%) = "15"            ~
                                                    THEN GOTO L62932 */
/*SR67518 IF SC_SEL$ = "1" AND STR(DT_REC$,64%,2%) = "15"            ~
                                                    THEN GOTO L62935 */
/*(CR456)  IF SC_SEL$ = "1" AND STR(DT_REC$,64%,2%) = "15"           ~
                                                    THEN GOTO L62932 */
/*(CR456)  IF SC_SEL$ = "1" AND SC_DEPT$ = "032" THEN GOTO L62935 */
/*(CR456)  IF SC_SEL$ = "3" AND STR(DT_REC$,64%,2%) = "15"          ~
                                                    THEN GOTO L62932 */
/*SR65848  if sc_sel$ = "3" and sc_dept$ = "032" then goto L62935 */
           goto calc_scanned_nxt

/* <AWD015> */
L62932:
/*(CR456) IF SC_DEPT$ <> "ALL" AND SC_DEPT$ <> "032" THEN           ~
                                               GOTO CALC_SCANNED_NXT*/
           if sc_dept$ <> "ALL" then goto calc_scanned_nxt
/* </AWD015> */
L62935:
/*(CR456) IF STR(DT_REC$,42,3) = "032" AND STR(DT_REC$,64,2) < "12" ~
                                                    THEN GOTO L63100 */
/*(CR456) IF STR(DT_REC$,64,2) = "15" THEN GOTO L63100 */

          /* </AWD014> */
              goto calc_scanned_nxt
L62940:
REM            call "SHOSTAT" ( " I am here passed status check " )  stop

                                           /* 1st Check Specified Prod. */
/*@@@*/     if sc_sel% = 1% then goto L63100     /* Include Stock Prod  */
            if sc_sel% = 3% then goto L63100     /* Include Stock Prod  */
            if sc_sel% = 6% then goto L63100     /* Include Stock Prod  */
            if sc_sel% = 7% then goto L63100     /* Include Stock Prod  */
                                                 /* Check for Stock Load*/
               stock$ = str(dt_rec$,1%, 1%)      /* Skip Stock Prod.    */
               if sc_sel% = 5% and stock$ = "S" then /* Skip Stock Prod*/~
                                                 goto calc_scanned_nxt
               if sc_sel% = 5% then goto L63100  /* NO STOCK INCLUDED   */
               if stock$ = "S" then goto L63100  /* Selection 2 and 4   */
                  goto calc_scanned_nxt          /* Stock Only          */

L63100:     if str(sc_load$,1%,3%) <> "ALL" or sc_id$ = "AL"           ~
                                                       then goto L63260
            init(" ") ad_time$, ad_dte$
            xx_dte$  = str(dt_rec$,53%,6%)
            ad_time$ = str(dt_rec$,116%,8%)
            hr% = 0%                            /* Scanned Product     */
            convert str(ad_time$,1%,2%) to hr%, data goto L63120
L63120:
            ap$ = str(ad_time$,7%,2%)           /* Scanned AM or PM    */
            if xx_dte$ = ee_dte$ then goto L63230
                                                /*Check Specified First*/
                                                /* All of 1st and 2nd  */
                                                /* part of 3rd shift   */
               if ap$ = "PM" then goto L63260
               if hr% < 7% or hr% = 12% then goto calc_scanned_nxt
                  goto L63260                    /*Check Next Day Second*/
                                                 /*Only last part of 3rd*/
                                                 /*shift.Midnight to 7 AM*/
L63230:     if ap$ = "PM" then goto calc_scanned_nxt
            if hr% < 7% or hr% = 12% then goto L63260    /* Midnight to */
                  goto calc_scanned_nxt                  /* 6 59AM      */
L63260: REM PARTS AND SASH'S ARE IN SEPERATE BUCKETS
            gosub dataload
            if err% = 0% then gosub print_detail
            goto calc_scanned_nxt

        calc_scanned_done
            if rpt% = 0% then gosub load_screen_wrk                       ~
               else           gosub print_screen_wrk
            gosub print_totals
            sav_dte$ = ee_dte$
        return

        check_inline

            inline% = 0%
            if str(dt_rec$,217%,3%) <= "099" then goto not_inline

            table%  = 3%
            code$   = str(dt_rec$,189%,3%) & str(dt_rec$,217%,3%)
REM            CALL "SHOSTAT" ("CHECKING CODE") STOP
            gosub check_code
            if code% = 0% then return
        not_inline
             inline% = 1%
        return


        calc_scanned_all
           gosub open_work_file                         /*  (EWD010)    */
           yr% = 0%
           datex$ = tt_dte$
           call "DATFMTC" (datex$,yr%,x$)
           convert str(x$,1%,4%) to yr%, data goto L62010
L62010:
           leap_yr% = 365%                          /* Standard Year   */
           if mod(yr%,4) = 0% then leap_yr% = 366%  /* Leap Year       */
                             /* Do for each Specified Day.             */
                             /* Windows Scanned from 7 AM of Specified */
                             /* Production Day, Until 6 59 AM of the   */
                             /* Next Production Day. 1st,2nd,3rd Shifts*/
            init(" ") jdate1$, jdate2$, bb_dte$, ee_dte$, dt_key$,       ~
                      ad_key$, ad_rec$
                                         /* Julian date Curr Prod Week */
            call "DATE" addr("GJ",tt_dte$, str(jdate1$,,5%), x%)
                                                         /* Convert to */
                                                         /* Julian Date*/
            call "DATJULCV" (jdate1$)
            convert str(jdate1$,5%,3%) to j1%, data goto L62140
L62140:
            j1% = j1%                            /* Day Julian Date    */
            j2% = j1% + 1%                       /* Tommorow's/Next Day*/
            jdate2$ = jdate1$                    /* Julian Date        */
            if j2% <= leap_yr% then goto L62240
               j2% = 001%
            convert str(jdate2$,1%,4%) to rhh%, data goto L62210
L62210:
            convert (rhh% + 1%) to str(jdate2$,1%,4%), pic(0000)

L62240:     convert j1% to str(jdate1$,5%,3%), pic(000)

            convert j2% to str(jdate2$,5%,3%), pic(000)
                                               /* Begin with Production*/
            call "DATJULCV" (jdate1$)
            call "DATJULCV" (jdate2$)   /* Date & Next Days Prod. Date */
            call "DATE" addr("JG", str(jdate1$,,5%), bb_dte$, x%)
            call "DATE" addr("JG", str(jdate2$,,5%), ee_dte$, x%)


        REM INIT(" ") DB$
        REM DB$ = "ToDay = " & BB_DTE$ & " Tommorow = " & EE_DTE$
        REM CALL "SHOSTAT" ( DB$ )
        REM STOP
                                          /* 1st Check Specified Prod. */
            p_scan% = 0%                     /* Production Not Scanned */
                                             /* Pruction Scanned       */
            if sc_sel% > 2% and sc_sel% < 5% then p_scan% = 1%
                                          /* 1st Check Specified Prod. */
           str(ad_key$,1%,6%) = bb_dte$   /* date from 7 AM until Mid. */
       /* AWD014 */
/*(CR456)IF SC_SEL$ = "1" AND SC_DEPT$ = "032" THEN                  ~
                                       GOTO CALC_SCANNED_FIRST_ALL */
/*SR65848 */
/* IF SC_SEL$ = "3" AND SC_DEPT$ = "032" THEN GOTO CALC_SCANNED_FIRST_ALL*/
           if str(sc_dept$,1%,3%) = "ALL" then goto calc_scanned_first_all
              str(ad_key$,7%,3%) = sc_dept$
        calc_scanned_first_all
           read #1,key > ad_key$, using L62450, ad_rec$,                  ~
                                             eod goto calc_scanned_done_all
           goto calc_read_first_all

        calc_scanned_nxt_all
/*(CR456) IF SC_DEPT$ = "031" AND SC_SEL$ = "3" THEN                 ~
                                       GOTO CALC_SCANNED_FIRST_ALL */

           read #1, using L62450, ad_rec$, eod goto calc_scanned_done_all
L62450:        FMT CH(64)
        calc_read_first_all
           ad_key$ = str(ad_rec$,19%,33%)
           if rpt% <> 0% then goto L62545
              count% = count% + 1%
              if mod(count%,100%) <> 0% then goto L62545
                 convert count% to str(count$,17%,8%), pic(########)

                 print at(02,28);hex(84);count$;

L62545: REM INIT(" ") DB$
        REM DB$ = "Audit Key = " & AD_KEY$ & "  Status = " &             ~
        REM                                           STR(AD_REC$,32%,2%)
        REM CALL "SHOSTAT" ( DB$ )
        REM STOP
/*PWW  IF STR(AD_REC$,24%, 18%) <> "070153130100020002" THEN GOTO PWW_CONT~
        STOP */
        pww_cont
                                          /* 1st Check Specified Prod. */
           ad_st$   = str(ad_rec$,32%,2%)          /* Scan Status Code */
           ad_dept$ = str(ad_rec$,25%,3%)          /* Scan Department  */
           ad_shft$ = str(ad_rec$,30%,2%)          /* Scan Shift Code  */
           ad_time$ = str(ad_rec$,52%,8%)          /* Scan Time        */
           ad_dte$  = str(ad_rec$,19%,6%)          /* Scan Date        */
           xx_dte$  = ad_dte$

REM           IF AD_DEPT$ <> "032" THEN GOTO TEST_CONT
REM           TEST_PWW% = 1%
REM           TEST_CONT

           if xx_dte$ > ee_dte$ then goto calc_scanned_done_all
        REM  CHECK DEPARTMENT
/*AWD014 (CR456) IF SC_SEL$ = "1" AND SC_DEPT$ = "032" THEN GOTO L62715 */
/*AWD014  IF SC_SEL$ = "3" AND SC_DEPT$ = "032" THEN GOTO L62715 SR65848 */

           if sc_dept$ = "ALL" then goto L62715
              if sc_dept$ <> ad_dept$ then goto calc_scanned_nxt_all

L62715: REM  CHECK SHIFT
           if sc_id$ = "AL" then goto L62720    /* Shift Not Specified */
              if sc_id$ <> ad_shft$ then goto calc_scanned_nxt_all
/* <AWD014> */
L62720:
/* (CR456) IF SC_SEL$ = "1" AND SC_DEPT$ = "032" THEN GOTO L62722 */
/*SR65848 IF SC_SEL$ = "3" AND SC_DEPT$ = "032" THEN GOTO L62722  */
          goto L62755
L62722:
            for i% = 1% to supp_max%                     /* Find Dept. */
                if ad_dept$ = supp$(i%) then goto calc_scanned_nxt_all
            next i%
/*(CR456) IF AD_ST$ = "15" THEN GOTO L62815                         */
/*(CR456) IF SC_SEL$ = "1" AND AD_ST$ < "12" AND AD_DEPT$ = "032" ~
                          THEN GOTO L62815 */
/*(CR456) IF SC_SEL$ = "3" AND AD_ST$ > "15" AND AD_DEPT$ = "032" ~
                          THEN GOTO L62815 */
          goto calc_scanned_nxt_all
/* </AWD014> */

L62755:    if ad_dept$ = "108" then goto L63105         /* CR2443 */
           if ad_dept$ = "106" then goto L63105         /* CR2508 */
           if p_scan% <> 0% then goto L62795
                                                        /* Not Scanned */
              if ad_st$ > "03" and ad_st$ < "12" and ad_st$ <> "11"    ~
                                    then goto L62815
                 goto calc_scanned_nxt_all
L62795:    if ad_st$ <> "12" then goto calc_scanned_nxt_all   /*Scanned  */

L62815:    init(" ") dt_key$, dt_rec$
           str(dt_key$,1%,18%) = str(ad_rec$,34%,18%)   /* Barcode     */
           str(dt_key$,19%,3%) = str(ad_rec$,25%,3%)    /* Dept Code   */
           str(dt_key$,22%,2%) = str(ad_rec$,28%,2%)    /* Process Code*/
           read #2,key = dt_key$, using L62870, dt_rec$,                  ~
                                              eod goto calc_scanned_nxt_all

/*PWW IF STR(DT_REC$,34%, 18%) <> "070153130100020002" THEN GOTO PWW_TEST2~
           STOP */
       pww_test2
           if str(sc_load$,1%,3%) = "ALL" then goto L62925
              if str(dt_rec$,1%,5%) < sc_load$ or                        ~
                 str(dt_rec$,1%,5%) > ed_load$ then                      ~
                             goto calc_scanned_nxt_all
/* <AWD014> */
L62925:
/*SR65848  IF SC_SEL$ = "3" AND SC_DEPT$ = "032" THEN GOTO L62945  */
/*(CR456)  IF SC_SEL$ <> "1" OR SC_DEPT$ <> "032" THEN GOTO L62927 */
           goto L62927        /* (CR456) */
            for i% = 1% to supp_max%                     /* Find Dept. */
               if str(dt_rec$,42,3) = supp$(i%) then               ~
                                            goto calc_scanned_nxt_all
            next i%
/*(CR456) IF STR(DT_REC$,42,3) = "032" AND STR(DT_REC$,64,2) < "12" THEN  ~
              GOTO L62945 */
/* <AWD015> */
/*(CR456) IF SC_DEPT$ <> "ALL" AND SC_DEPT$ <> "032" THEN             ~
                                              GOTO CALC_SCANNED_NXT_ALL */
/* </AWD015> */
/*(CR456)  IF STR(DT_REC$,64,2) = "15" THEN GOTO L62945 */
/*(CR456)  goto calc_scanned_nxt_all */
/* </AWD014> */

L62927:    if p_scan% <> 0% then goto L62945     /* Goto Scanned Product*/
/*SR65848  IF STR(DT_REC$,64%,2%) > "11" THEN GOTO CALC_SCANNED_NXT_ALL */
/*(CR456) if str(dt_rec$,64%,2%) > "11" and str(dt_rec$,64%,2%) <> "15" ~
                                     then goto calc_scanned_nxt_all */
              if str(dt_rec$,64%,2%) > "11" and sc_sel% <> 3% ~
                then goto calc_scanned_nxt_all
L62945: REM INIT(" ") DB$
        REM DB$ = "Hit - Detail Key = " & DT_KEY$ & " Status = "         ~
                                                      STR(DT_REC$,64%,2%)
        REM CALL "SHOSTAT" ( DB$ )
        REM STOP
L62955:      /* CR2443 */
                                           /* 1st Check Specified Prod. */
            if sc_sel% = 1% then goto L63105     /* Include Stock Prod  */
            if sc_sel% = 3% then goto L63105     /* Include Stock Prod  */
                                                /* Check for Stock Load*/
               stock$ = str(dt_rec$,1%, 1%)     /* Skip Stock Prod.    */
               if sc_sel% = 5% and stock$ = "S" then /* Skip Stock Prod*/~
                                                 goto calc_scanned_nxt_all
               if sc_sel% = 5% then goto L63105  /* NO STOCK INCLUDED   */
               if stock$ = "S" then goto L63105  /* Selection 2 and 4   */
                  goto calc_scanned_nxt_all      /* Stock Only          */

L63105:     hr% = 0%                             /* Scanned Product     */
            convert str(ad_time$,1%,2%) to hr%, data goto L63125
L63125:
           if xx_dte$ > ee_dte$ then goto calc_scanned_done_all
            ap$ = str(ad_time$,7%,2%)           /* Scanned AM or PM    */
            if xx_dte$ = ee_dte$ then goto L63235
                                                /*Check Specified First*/
                                                /* All of 1st and 2nd  */
                                                /* part of 3rd shift   */
/* CR2508 Change L63264 to L63263 */
               if ap$ = "PM" then goto L63263   
               if hr% < 7% or hr% = 12% then goto calc_scanned_nxt_all
                  goto L63263                    /*Check Next Day Second*/
                                                 /*Only last part of 3rd*/
                                                /*shift.Midnight to 7 AM*/
L63235:     if ap$ = "PM" then goto calc_scanned_nxt_all
            if hr% < 7% or hr% = 12% then goto L63263    /* Midnight to */
                  goto calc_scanned_nxt_all              /* 6 59AM      */
                  
L63263:    if ad_dept$ <> "108" then goto L63264         /* CR2443 CR2508 */
              ss_tot%(1%) = ss_tot%(1%) + 1%    /* Scanned Total       */
              ss%(r%,1%) = ss%(r%,1%) + 1%      /* R set in dept load  */        
              goto calc_scanned_nxt_all
              
L63264:    if ad_dept$ <> "106" then goto L63265         /* CR2508 */
           if ad_dept$ = "106" and ad_st$ <> "14" then calc_scanned_nxt_all
              ss_tot%(1%) = ss_tot%(1%) + 1%    /* Scanned Total       */
              ss%(u%,1%) = ss%(u%,1%) + 1%      /* U set in dept load  */        
              goto calc_scanned_nxt_all
              
L63265: REM PARTS AND SASH'S ARE IN SEPERATE BUCKETS
            gosub dataload
            if err% = 0% then gosub print_detail
            goto calc_scanned_nxt_all

        calc_scanned_done_all
            if rpt% = 0% then gosub load_screen_wrk                       ~
               else           gosub print_screen_wrk
            gosub print_totals
            sav_dte$ = ee_dte$
        return

        purge_audit
            init(" ") ad_key$, ad_rec$
            str(ad_key$,1%,8%) = sav_so$
        purge_audit_nxt
            read #1,hold,key 1% > ad_key$, using L63420, ad_key$,         ~
                                                eod goto purge_audit_done
L63420:         FMT CH(33)
            if str(ad_key$,1%,8%) <> sav_so$ then goto purge_audit_done
               delete #1
               aud% = aud% + 1%
               goto purge_audit_nxt

        purge_audit_done
        return

        purge_sched
            init(" ") sd_key$
            str(sd_key$,1%,8%) = sav_so$
        purge_sched_nxt
            read #6,hold,key > sd_key$, using L63570, sd_key$,            ~
                                                eod goto purge_sched_done
L63570:         FMT CH(23)
            if str(sd_key$,1%,8%) <> sav_so$ then goto purge_sched_done
               delete #6
               sched% = sched% + 1%
               goto purge_sched_nxt

        purge_sched_done
        return
                                                       /* (EWD004)      */
        check_samples
            ss% = 0%
            if len(dt_part$) < 20 then return        /* Quick Test      */
            if str(dt_part$,1%,1%) = "9" then return   /* Bay/Bow       */
            convert str(dt_part$,20%,3%) to ss%, data goto LS1

            if ss% < 1% or ss% > 80% then goto LS1   /* Not Samp/Disp   */
                                                     /*   (EWD007)      */
            if str(dt_part$,7%,2%) > "99" then goto LS1

            if ss% > 11% and ss% < 30% then tt% = 3%
        return                                       /* Code Found      */
LS1:        convert str(dt_part$,23%,3%) to ss%, data goto LS2

            if ss% > 11% and ss% < 30% then tt% = 3%

                                                      /* Code Found      */
LS2:    return
                                                       /* (EWD004)      */

                                                           /* (EWD003)  */
        open_error
           comp% = 2%
           hh$  = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = "(Open Error) - File = " & filename$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hh$, msg$(1%), msg$(2%), msg$(3%))
        return
                                                           /* (EWD003)  */

        open_work_file                                     /* (EWD010)  */
             if workfile% = 1% then call "FILEBGON" (#10)
             workfile% = 0%
             call "WORKOPEN" (#10, "IO", 500%, f2%(10))
                if f2%(10) = 1% then no_work_file  /*GetOutIfOpenFails*/
                workfile% = 1%
        no_work_file
        return                                             /* (EWD010)  */
        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            if workfile% = 1% then call "FILEBGON" (#10)
            call "SHOSTAT" ("One Moment Please")

            end


