        REM *************************************************************~
            *          NOTES - PF(20)- CLEAR ALL DATA FOR DEPARTMENT    *~
            *                  PF(12)- PURGE ALL DATA PRIOR TO AND      *~
            *                          INCLUDING PRODUCTION WEEK ENTERED*~
            *                  PF(13)- MOVE DATA PRV TO CURR,           *~
            *                                    HIST TO PRV            *~
            *                                                           *~
            *   AAA   PPPP    CCC   EEEEE  FFFFF  FFFFF  TTTTT  BBBB    *~
            *  A   A  P   P  C   C  E      F      F        T    B   B   *~
            *  AAAAA  PPPP   C      EEEE   FFFF   FFFF     T    BBBB    *~
            *  A   A  P      C   C  E      F      F        T    B   B   *~
            *  A   A  P       CCC   EEEEE  F      F        T    BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * APCEFFTB - Production Efficiency Reporting and Entry.     *~
            *                                                           *~
            *   NOTE - (1) - The Values for Indirect are in the Array   *~
            *                INDT(). Values Set at (9420)               *~
            *          (2) - The Daily Wages for SEQ% 43,46 Mgr Pay, and*~
            *                Incentive are set (51751 - 51780)          *~
            *                REM UNIT VALUES                            *~
            *          (3) - The 'SC_WK$' and 'SC_YR$' Must be set and  *~
            *                the program Compiled in Order to Purge Data*~
            *          (4) - Indirect Labor Standard Unit Per Manhour   *~
            *                Goals. (Wired in INDT() ) line 9500        *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/01/93 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 02/08/94 ! Mod to Calc Earned Hours and correct     ! RHH *~
            *          !   Weekly, Monthly, Yearly.               !     *~
            * 09/30/94 ! Mod to Correct Managers Pay and Incent.  ! RHH *~
            *          !   Changed at (51751 - 51780)             !     *~
            * 11/21/94 ! Mod to Add PURGE_DATA routine for        ! RHH *~
            *          !   Deleting History Data                  !     *~
            * 02/22/95 ! Mods (1) Coreect Scrap for IG            ! RHH *~
            *          !      (2) Change Indirect U.P.H Values    !     *~
            *          !      (3) Fix Total Calculations          !     *~
            *          !      (4) Swith to Orig Calc of EFF_UPMHA !     *~
            *          !      (5) Change Calc for EFF_EFF         !     *~
            *          !      (6) Add Table (APC EFF03) for Mgr   !     *~
            *          !          Pay and Incentives. ( GET_PAY ) !     *~
            *          !      (7) Do Not include Mgr and Incentive!     *~
            *          !          for day 6 and 7.                !     *~
            * 03/06/95 ! Mods (1) Combine Departments (01)-(05)   ! RHH *~
            *          !          into one Aluminum Dept. (01).   !     *~
            *          !      (2) Make Room for Three New Dept's  !     *~
            *          !          and allow for two Additional.   !     *~
            * 03/24/95 ! Mod to (APCEFFWG) for Shift?             ! RHH *~
            * 04/05/95 ! Mod to Divide Scrap entry for the glass  ! RHH *~
            *          !    Dept (37) by 15.7 .                   !     *~
            * 05/04/95 ! Mod to Eliminate Indirect Dept's from    ! RHH *~
            *          !     '35'= Samples, '38'= Training        !     *~
            * 07/25/95 ! Mod to Change INDT() Value for the       ! RHH *~
            *          !     Material Dept from 4.00 to 4.25      !     *~
            * 01/16/96 ! Mod to use new subroutine to calculate   ! RHH *~
            *          !   the current production year, week, day !     *~
            *          !   and validate the entered production    !     *~
            *          !   year, week, and day. (Sub-APCPLN0B)    !     *~
            * 01/23/96 ! Mod for EFF_UPMHA(), set equal to Earned ! RHH *~
            *          !   hours (60891). Also change calc for    !     *~
            *          !   Glass scrap EFF_SCRPA. (60970)         !     *~
            * 01/23/96 ! Mod for EFF_UPMHA() Indirect Departments ! RHH *~
            *          !   Earned hours.                          !     *~
            * 02/19/96 ! Mod for Efficiency Untit to be Pulled    ! RHH *~
            *          !   from the Scanning Database.            !     *~
            * 02/04/97 ! Mod for Efficiency Units to be Pulled    ! RHH *~
            *          !   from the New Scanning System and Audit !     *~
            *          !   File.                                  !     *~
            * 08/21/97 ! Mod to Add Arguemnt to (APCPLC40) for    ! RHH *~
            *          !   Load. SC_LOAD$                         !     *~
            * 11/12/97 ! Mod for Upgrade to R6.04.03              ! RHH *~
            * 01/21/98 ! Mod to allow for More Products per Dept. ! RHH *~
            *          !   MAX_PRODUCT% = 200%                    !     *~
            * 09/15/99 ! Mod to calculate glass buckets. Change   ! CMG *~
            *          !    OPENCHCK To EWDOPEN. (EWD0001)        !     *~
            * 09/16/99 ! Mod to recalc hours and wages from adjust! CMG *~
            *          !   in "APC EFFAJ"  (EWD0002)              !     *~
            * 09/27/99 ! Mod to Add "ASKUSER" Call to Clear Week, ! CMG *~
            *          !   Month, & Year Function Keys (EWD0003)  !     *~			
            * 10/04/99 ! Mod to Add RGA (dept 32) to auto_tab and ! CMG *~
            *          !   take out UPS (dept 11) (EWD0004)       !     *~
            * 02/15/00 ! Mod to changed remark reason codes that  ! CMG *~
            *          !   glass house doesn't get cedit for.     !     *~
            *          !                          (EWD0005)       !     *~
            *************************************************************

        dim                                                              ~
            aluminum$(5%)2,              /* ALUMINUM DEPT CODES        */~
            eff_k$16,                    /* PURGE DEPARTMENT KEY       */~
            prg_key$20, e_shift$2,       /* Purge Key                  */~
            auto_tab$(15%)2,             /* AUTO LOAD DEPT'S           */~
            sav_key$20,                  /* Copy Key                   */~
            eff_key$(297%)20,            /* Primary KEY                */~
            eff_key1$(297%)16,           /* Alt Key                    */~
            eff_proc$(297%)1,            /* EFF Process Flag           */~
            eff_wk$(297%)2,              /* Production Week            */~
            eff_day$(297%)1,             /* Production Day             */~
            eff_code$(297%)15,           /* Efficienct Code (APC EFF02)*/~
            eff_dte$(297%)6, eff_dte$6,  /* Production Date            */~
            eff_fil1$(297%)144,          /* Filler Area (1)            */~
            eff_rec$(3%)256,             /* Eff Record                 */~
            eff_tab_prc(297%),           /* Product Avg Price-EFF02    */~
            eff_tab_upmh(297%),          /* STD UPMH - EFF02           */~
            eff_tab_scrp(297%),          /* Prod Scrap Weight - EFF02  */~
            eff_untd(297%,4%),           /* Product Units D,W,M,Y      */~
            eff_hrsr(297%,4%),           /* Eff Hours Reg. D,W,M,Y     */~
            eff_hrso(297%,4%),           /* Eff Hours Ovr  D,W,M,Y     */~
            eff_hrst(297%,4%),           /* Eff Hours Tot  D,W,M,Y     */~
            eff_prc(297%,4%),            /* Prod Total Prc D,W,M,Y     */~
            eff_upmhg(297%,4%),          /* UPMH Goal D,W,M,Y          */~
            eff_wages(297%,4%),          /* Wages/Labor Dol D,W,M,Y    */~
            eff_upmha(297%,4%),          /* UPMH Actual                */~
            eff_labg(297%,4%),           /* Eff Labor Dol % D,W,M,Y    */~
            eff_laba(297%,4%),           /* Eff Actual Labor % D,W,M,Y */~
            eff_eff(297%,4%),            /* Eff Percent D,W,M,Y        */~
            eff_scrpa(297%,4%),          /* Eff Actual Scrap WT DWMY   */~
            eff_scrpp(297%,4%),          /* Eff Prod Scrap WT D,W,M,Y  */~
            eff_scrp(297%,4%),           /* Eff Scrap Percent D,W,M,Y  */~
            eff_hrse(297%,4%),           /* Eff HOURS EARNED  D,W,M,Y  */~
            sc_load$5, ed_load$,         /* Production Load or (ALL)   */~
            sc_wk$2, sc_wk_dte$8,        /* SCREEN WEEK                */~
            sc_day$1,                    /* SCREEN PRODUCTION DAY      */~
            sv_wk$2, sv_day$1,           /* CURRENT WEEK AND DAY       */~
            sc_dept$2, sc_dept_d$30,     /* Department Code/Descript   */~
            sc_yr$4, prv_yr$4,           /* PRODUCTION YEAR            */~
            prv_yr_bi$2,                 /* binary fmt                 */~
            seq$2,                       /* Prompt Sequence Number     */~
            d_txt$(297%)10,              /* Screen Text Prompts        */~
            prc(297),                    /* AVERAGE PRICE              */~
            upmh(297%),                  /* STD UPMH - EFF02           */~
            scrp(297%),                  /* PROD SCRAP WT              */~
            d_val$(297%)10,              /* Screen Text Values         */~
            d_val(297%),                 /* Screen Value               */~
            days$(7%)9,                  /* DAYS OF THE WEEK           */~
            days$9,                      /* DAY OF THE WEEK            */~
            date$8,                      /* SCREEN DATE                */~
            readkey$50,                  /* Generic Key                */~
            descr$32,                    /* Generic Description        */~
            rpt$1, rpt_d$25,             /* Report Type                */~
            rpt_sel$1, rpt_sel_d$8,      /* Report Selection           */~
            rpt_dte$8,                   /* Production Report Date     */~
            scr$(15%)40,                 /* Report Display             */~
            ef(7%),                      /* Efficiency Hours by Day    */~
            ef_ov(7%),                   /* Efficiency Overtime Hours  */~
            tot_ef(7%),                  /* Total Hours each Day       */~
            wg(7%),                      /* Total Wages Each Day       */~
            tot_unit$10,                 /* Screen Total Units         */~
            tot_hrsr$10,                 /* Screen Total Reg Hours     */~
            tot_hrso$10,                 /* Screen Total Overtime Hours*/~
            tot_wages$10,                /* Screen Total Wages         */~
            tot_key$(60)20,              /* DEPARTMENT TOTALS KEY      */~
            tot_wg(4%),                  /* PLANT TOTAL WAGES D,W,M,Y  */~
            indt(10%),                   /* Indirect Table Values 31,38*/~
            cursor%(2%),                 /* Cursor location for edit   */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(297%)1,                /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            hdr$40,                      /* ASKUSER HEADER             */~
            msg$(3%)79,                  /* ASKUSER TEXT               */~
            userid$3                     /* Current User Id            */

        dim                              /* Subroutine - Variables     */~
            cur_yr$4, cur_yr_bi$2,       /* Current Year               */~
            cur_wk$2,  cur_dy$1,         /* Current Prod. Week an Day  */~
            cur_dte$6, cur_date$8,       /* Prod Week Date Form/Unform */~
            ent_yr$4, ent_yr_bi$2,       /* Julian Year and Day YYDDD  */~
            ent_wk$2,  ent_dy$1,         /* Entry Prod. Week an Day    */~
            ent_dte$6, ent_date$8,       /* Prod Week Date Form/Unform */~
            temp1$10, temp2$8            /* work variables             */~

        dim f2%(8%),                     /* = 0 if the file is open    */~
            f1%(8%),                     /* = 1 if READ was successful */~
            fs%(8%), axd$4,              /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(8%)20                  /* Text from file opening     */


        dim dbg_msg$45,                  /* Header Message             */~
            dbg$(10%)10, type$7,         /* Convert and Store Debug Val*/~
            d_inp$79,                    /* Set Display Screen Prompt  */~
            dbkey$32                     /* Set Function Key Masks     */

        dim pd_dept$3, p_mod$(306%)3,    /* Planning Dept. Scan Time   */~
            jdate1$7, p_unt%(306%,3%),   /* Julian Dates Yes't, Today  */~
            p_unts%(306%,3%),            /* SAMPLES ONLY               */~
            p_untss%(306%,3%),           /* CHARGE SASHS ONLY          */~
            p_untpp%(306%,3%),           /* CHARGE PARTS ONLY          */~
            bg_dte$6, p_val(306%,3%),    /* BG_DTE$ Yesterday, Today   */~
            p_mrp(6%,3%),                /* Scanned Costing Values     */~
            p_shft$2                     /* Shift Code or 'AL'         */

       dim                               /* Glass Cnt  - Variables     */~
            rm_ky$33, rm_rec$256,        /* Remake Key and Record      */~
            rm_cnt$16, rm%(6%,2%),       /* Process Counter            */~
	    tot%(6%),                    /* Total For Buckets          */~
            rm_st$1,                     /* Gls stat '0'=Rmk,          */~          
                                         /*  '1'=Shed, '2'=Complete    */~
            rm_model$3,                  /* Model Code                 */~
            rm_gls$2,                    /* Glass Code Type            */~
            rm_bar$9,                    /* Glass Barcode              */~
            rm_lt$6,                     /* Liting Description Grid    */~
            rm_wd_d$8,                   /* Glass Calc Width- Decimal  */~
            rm_ht_d$8,                   /* Glass Calc Height- Decimal */~
            rm_temp$3,                   /* Glass Tempered flag '*'    */~
            lt_comp$(10%)6,              /* Liting Compare Values      */~
            gls_comp$(41%)2,             /* Glass  Compare Values      */~
            rm_num$3,                    /* Remake Number, Start at 00 */~
            rm_time$8,                   /* Time of Last Status Change */~
            rm_anal$1,                   /* Analysis Flag 0=Yes 1=No   */~
            ap$2,                        /* 'AM' or 'PM' From time     */~
            rm_reason$2,                 /* Remake Glass Reason Code   */~
            sc_dte$8                     /* Scan Date                  */
			
       dim                               /* Glass Cnt Rmk - Variables  */~
            rma_ky$12, rma_rec$64,       /* Remake Key and Record      */~
            rma_num$3,                   /* Remake Number              */~ 
            rma_sve$12,                  /* Remake Save Key            */~
            rma_dte$6,                   /* Date Glass Scanned for Rmk */~   
            rma_time$8,                  /* Time Glass Scanned for Rmk */~
            rma_comp$4,                  /* Total Hrs and Mins to Comp */~ 
	    rma_reason$2                 /* Remake Reason              */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(New) EWD Daily Production Efficiency   "
            pname$ = "APCEFFTB - Rev: R6.04"

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
            * #1  ! APCEFFTB ! Efficiency Master Table File             *~
            * #2  ! GENCODES ! Master Code Table File                   *~
            * #3  ! APCEMPMT ! Employee Master Detail File              *~
            * #4  ! APCEMPLY ! Employee Master File                     *~
            * #5  ! APCPLNDT ! Production Tracking File                 *~
            * #6  ! APCPLNAD ! Production Tracking Audit File           *~
            * #7  ! APCPLNDP ! Master Department File                   *~
            * #8  ! APCPLNGR ! Glass Sched/Remake File                  *~
            * #9  ! APCCSTLR ! Department Average Hourly Rate           *~
            * #10 ! APCPLNGA ! Glass Remake File/All Completed Remakes  *~			
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************


            select #1,   "APCEFFTB",                                     ~
                        varc,     indexed,  recsize =  674,              ~
                        keypos =    1, keylen =    20,                   ~
                        alt key  1, keypos =    5, keylen =  16, dup

            select #2,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #3,   "APCEMPMT",                                     ~
                        varc,     indexed,  recsize =   32,              ~
                        keypos =    1, keylen =   12,                    ~
                        alt key  1, keypos =    3, keylen =  10, dup

            select #4,   "APCEMPLY",                                     ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos =    6, keylen =    5,                    ~
                        alt key  1, keypos =    1, keylen =  10, dup,    ~
                            key  2, keypos =   11, keylen =  26, dup

            select #5,   "APCPLNDT",                                     ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =   24, keylen =   23,                    ~
                        alt key  1, keypos =   47, keylen =  57,         ~
                            key  2, keypos =   53, keylen =  51,         ~
                            key  3, keypos =    1, keylen =  23, dup,    ~
                            key  4, keypos =   96, keylen =   8, dup

            select #6,   "APCPLNAD",                                     ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =   19, keylen =   33,                    ~
                        alt key  1, keypos =    1, keylen =  33

            select #7,   "APCPLNDP",                                     ~
                        varc,     indexed,  recsize =   32,              ~
                        keypos =   11, keylen =   12,                    ~
                        alt key  1, keypos =    9, keylen =  14,         ~
                            key  2, keypos =    4, keylen =  12,         ~
                            key  3, keypos =    1, keylen =  15

            select #8,  "APCPLNGR",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos = 22,   keylen =  12,                     ~
                        alt key  1, keypos  =     7, keylen = 27,        ~
                            key  2, keypos  =     1, keylen = 33,        ~
                            key  3, keypos  =    13, keylen = 21
							
            select #9,   "APCCSTLR",                                     ~
                        varc,     indexed,  recsize =  102,              ~
                        keypos =    1, keylen =   3							

            select #10,  "APCPLNGA",                                     ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =  1,   keylen =  18,                     ~
                        alt key  1, keypos  =     7, keylen = 17           

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 500%, rslt$(1%))

            filename$ = "GENCODES" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCEMPMT" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCEMPLY" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNDT" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNAD" : call "EWDOPEN" (#6, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNDP" : call "EWDOPEN" (#7, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNGR" : call "EWDOPEN" (#8, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCCSTLR" : call "EWDOPEN" (#9, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNGA" : call "EWDOPEN" (#10, filename$, err%)
            if err% <> 0% then gosub open_error


            call "OPENOLIB" (#6, "SHARE", f2%(6%), rslt$(6%), axd$)

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            call "EXTRACT" addr("ID", userid$)
            days$(1%) = "MONDAY   " : days$(2%) = "TUESDAY  "
            days$(3%) = "WEDNESDAY" : days$(4%) = "THURSDAY "
            days$(5%) = "FRIDAY   " : days$(6%) = "SATURDAY "
            days$(7%) = "SUNDAY   "

            date$ = date
            call "DATEFMT" (date$)

            scr$( 1) = "****************************************"
            scr$( 2) = "*                  !                   *"
            scr$( 3) = "* Report Types     ! Report Selections *"
            scr$( 4) = "* ---------------- ! ----------------- *"
            scr$( 5) = "* (1) - Var Detail !  (1) - Daily      *"
            scr$( 6) = "*                  !                   *"
            scr$( 7) = "* (2) - Var Totals !  (2) - Weekly     *"
            scr$( 8) = "*                  !                   *"
            scr$( 9) = "* (3) - Current    !  (3) - Monthly    *"
            scr$(10) = "*                  !                   *"
            scr$(11) = "* (4) - Previous   !  (4) - Yearly     *"
            scr$(12) = "*                  !                   *"
            scr$(13) = "* (5) - History    !                   *"
            scr$(14) = "****************************************"
*         RHH - Mod - 01/15/96            /* Eff Indirect Values 31%-38%*/
            indt(1%)  = 2.00                              /* MAT HAND'L */
            indt(2%)  = 1.04                              /* Q.A        */
            indt(3%)  = 6.48                              /* MAINT      */
            indt(4%)  = 0.00                              /* AVAILABLE  */
/* (EWD0004) */
            indt(5%)  = 0.00                              /* AVAILABLE  */
/* (EWD0004) */
            indt(6%)  = 0.00                              /* Sort Team  */
            indt(7%)  = 0.00                              /* Glass Rec  */
            indt(8%)  = 0.00                              /* Training   */
            indt(9%)  = 0.00                              /* Lowes Reset*/
            indt(10%) = 0.00                              /* Inventory  */

            gosub check_access

            auto_tab$( 1%) = "15"   : auto_tab$( 8%) = "61"
            auto_tab$( 2%) = "24"   : auto_tab$( 9%) = "45"
            auto_tab$( 3%) = "13"   : auto_tab$(10%) = "  "
            auto_tab$( 4%) = "39"   : auto_tab$(11%) = "  "
            auto_tab$( 5%) = "31"   : auto_tab$(12%) = "  "  /* (EWD0004) */
            auto_tab$( 6%) = "61"   : auto_tab$(13%) = "  "
            auto_tab$( 7%) = "45"   : auto_tab$(14%) = "  "
            auto_current% = 0%

            aluminum$(1%) = "01" : aluminum$(2%) = "  "
            aluminum$(3%) = "  " : aluminum$(4%) = "  "

            max_product% = 297%
			

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 5%
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10280
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  =  6% then gosub complete_calc
                      if keyhit%  =  7% then gosub move_data
                      if keyhit%  =  8% then gosub delete_current
                      if keyhit%  =  9% then gosub clear_weekly
                      if keyhit%  = 10% then gosub clear_monthly
                      if keyhit%  = 11% then gosub clear_yearly
                      if keyhit%  = 14% then goto inputmode_a
                      if keyhit% <>  4% then       L10260
L10210:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10120
                         if fieldnr% = 1% then L10100
                         goto L10210
L10260:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10120
L10280:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
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
                  if keyhit%  = 14% then goto inputmode_a
                  if keyhit%  = 16% then gosub dataput
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
            *       I N P U T   M O D E   F O R   R E P O R T           *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode_a
            gosub initialize_variables
            rpt% = 1%
            for fieldnr% = 1% to 4%
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
            if fieldnr% < 1% or fieldnr% > 4% then editpg2
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
            call "APCEFF1B" ( sc_wk$,         /* Production Week       */~
                              sc_day$,        /* Production Day        */~
                              rpt_dte$,       /* Production Date       */~
                              rpt$,           /* Report Type (1,2,3,4) */~
                              rpt_sel$,       /* Report Sel (1,2,3,4)  */~
                              #1,             /* (APCEFFTB) Master File*/~
                              #2 )            /* (GENCODES) - File     */
        gosub display_debug
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
         "Enter a Valid Production Week (1-52), Blank = Current Week?  ",~
         "Enter a Valid Production Day (1-7), Blank = Current Day?     ",~
         "Enter a Valid Department Code?                               ",~
         "Enter the Department Total Scrap for Day?                    ",~
         "Enter the Applicable Units for the Associated Products       "

        deffn'060(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28320
                inpmessage$ = edtmessage$
                return

L28320
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn2_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn2_msg  :  data                                               ~
         "Enter a Valid Production Week (1-52), Blank = Current Week?  ",~
         "Enter a Valid Production Day (1-7), Blank = Current Day?     ",~
         "Enter a Valid Report Type ( 1 thru 5 )?                      ",~
         "Enter a Valid Report Selection ( 1 thru 4 )?                 "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, eff_proc$(), eff_wk$(),    ~
                      eff_day$(), eff_code$(), scrap$,                   ~
                      eff_dte$(), eff_dte$, sc_dept$,                    ~
                      sc_dept_d$, sc_wk$, days$, sc_wk_dte$, sc_day$,    ~
                      d_txt$(), d_val$(), seq$,                          ~
                      rpt$, rpt_d$, rpt_sel$, rpt_sel_d$, sc_yr$,        ~
                      eff_rec$(), eff_key$(), eff_key1$(),               ~
                      tot_unit$, tot_hrsr$, tot_hrso$, tot_wages$
             rpt% = 0%
             mat d_val = zer                  /* Val to be display on scr*/
             mat prc  = zer                   /* Price from APC EFF02    */
             mat upmh = zer                   /* Units per Man Hour      */
             mat scrp = zer                   /* Scrap from APC EFF02    */
             rm_anal$ = "0"                   /* Flag for Glass Analysis */
             gosub clear_rec
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
            call "SHOSTAT" ("Loading Department Data.")
            gosub clear_rec
            for i% = 1% to p_no%
                                                  /* 1-2 - LOCATION    */
                                                  /* 3-4 - DEPARTMENT  */
                                                  /* 5-11- SORT/DESC   */
                read #1,key 1% = eff_key1$(i%), eod goto L30135
                gosub data_get
                       eff_untd(i%,1%)  = 0.0
                       eff_hrsr(i%,1%)  = 0.0
                       eff_hrso(i%,1%)  = 0.0
                       eff_hrst(i%,1%)  = 0.0
                       eff_prc(i%,1%)   = 0.0
                       eff_upmhg(i%,1%) = 0.0
                       eff_wages(i%,1%) = 0.0
                       eff_upmha(i%,1%) = 0.0
                       eff_labg(i%,1%)  = 0.0
                       eff_laba(i%,1%)  = 0.0
                       eff_eff(i%,1%)   = 0.0
                       eff_scrpa(i%,1%) = 0.0
                       eff_scrpp(i%,1%) = 0.0
                       eff_scrp(i%,1%)  = 0.0
                       eff_hrse(i%,1%)  = 0.0
                goto L30140
L30135:           gosub load_ytd
L30140:     next i%
            eff_prc(p_no%,1%), eff_prc(p_no%,2%) = 0.0
            eff_prc(p_no%,3%), eff_prc(p_no%,4%) = 0.0
            eff_scrpp(p_no%,1%), eff_scrpp(p_no%,2%) = 0.0
            eff_scrpp(p_no%,3%), eff_scrpp(p_no%,4%) = 0.0
            eff_upmhg(p_no%,1%), eff_upmhg(p_no%,2%) = 0.0
            eff_upmhg(p_no%,3%), eff_upmhg(p_no%,4%) = 0.0
        return

        load_ytd
           readkey$ = " "
           str(readkey$,1%,9%) = "APC EFFTT"
           str(readkey$,10%,15%) = str(eff_key1$(i%),2%,15%)
           read #2,key = readkey$, using L30210, descr$, eod goto L30255
L30210:       FMT POS(25), CH(30)

           convert str(descr$,1%,9%) to eff_hrsr(i%,4%),data goto L30225
L30225:
           convert str(descr$,11%,9%) to eff_hrso(i%,4%),data goto L30235
L30235:
           convert str(descr$,21%,9%) to eff_hrst(i%,4%),data goto L30245
L30245:
           goto L30270
L30255:        call "SHOSTAT" ("(Error)-No (YTD) Data EFFTT ---> "       ~
                                                      & eff_key1$(i%) )
               stop

L30270:    readkey$ = " "
           str(readkey$,1%,9%) = "APC EFFT1"
           str(readkey$,10%,15%) = str(eff_key1$(i%),2%,15%)
           read #2,key = readkey$, using L30290, descr$, eod goto L30335
L30290:       FMT POS(25), CH(30)

           convert str(descr$,1%,9%) to eff_untd(i%,4%),data goto L30305
L30305:
           convert str(descr$,11%,9%) to eff_wages(i%,4%),data goto L30315
L30315:
           convert str(descr$,21%,9%) to eff_scrpa(i%,4%),data goto L30325
L30325:
           goto L30350
L30335:        call "SHOSTAT" ("(Error)-No (YTD) Data EFFT1 ---> "       ~
                                                        & eff_key1$(i%) )
               stop

L30350:    readkey$ = " "
           str(readkey$,1%,9%) = "APC EFFT4"       /* YTD EARNED HOURS */
           str(readkey$,10%,15%) = str(eff_key1$(i%),2%,15%)
           read #2,key = readkey$, using L30370, descr$, eod goto L30395
L30370:       FMT POS(25), CH(30)

           convert str(descr$,1%,9%) to eff_hrse(i%,4%),data goto L30385
L30385:
           return
L30395:        call "SHOSTAT" ("(Error)-No (YTD) Data EFFT4 ---> "       ~
                                                        & eff_key1$(i%) )
               stop
        return

        data_get
            get #1, using L35040,eff_proc$(i%), eff_wk$(i%), eff_day$(i%),~
                                eff_proc$(i%),eff_code$(i%),eff_dte$(i%),~
                                eff_tab_prc(i%), eff_tab_upmh(i%),       ~
                                eff_tab_scrp(i%), eff_untd(i%,1%),       ~
                                eff_untd(i%,2%), eff_untd(i%,3%),        ~
                                eff_untd(i%,4%), eff_hrsr(i%,1%),        ~
                                eff_hrsr(i%,2%), eff_hrsr(i%,3%),        ~
                                eff_hrsr(i%,4%), eff_hrso(i%,1%),        ~
                                eff_hrso(i%,2%), eff_hrso(i%,3%),        ~
                                eff_hrso(i%,4%), eff_hrst(i%,1%),        ~
                                eff_hrst(i%,2%), eff_hrst(i%,3%),        ~
                                eff_hrst(i%,4%), eff_prc(i%,1%),         ~
                                eff_prc(i%,2%), eff_prc(i%,3%),          ~
                                eff_prc(i%,4%), eff_upmhg(i%,1%),        ~
                                eff_upmhg(i%,2%), eff_upmhg(i%,3%),      ~
                                eff_upmhg(i%,4%), eff_wages(i%,1%),      ~
                                eff_wages(i%,2%), eff_wages(i%,3%),      ~
                                eff_wages(i%,4%), eff_upmha(i%,1%),      ~
                                eff_upmha(i%,2%), eff_upmha(i%,3%),      ~
                                eff_upmha(i%,4%), eff_labg(i%,1%),       ~
                                eff_labg(i%,2%), eff_labg(i%,3%),        ~
                                eff_labg(i%,4%), eff_laba(i%,1%),        ~
                                eff_laba(i%,2%), eff_laba(i%,3%),        ~
                                eff_laba(i%,4%), eff_eff(i%,1%),         ~
                                eff_eff(i%,2%), eff_eff(i%,3%),          ~
                                eff_eff(i%,4%), eff_scrpa(i%,1%),        ~
                                eff_scrpa(i%,2%), eff_scrpa(i%,3%),      ~
                                eff_scrpa(i%,4%), eff_scrpp(i%,1%),      ~
                                eff_scrpp(i%,2%), eff_scrpp(i%,3%),      ~
                                eff_scrpp(i%,4%), eff_scrp(i%,1%),       ~
                                eff_scrp(i%,2%), eff_scrp(i%,3%),        ~
                                eff_scrp(i%,4%), eff_hrse(i%,1%),        ~
                                eff_hrse(i%,2%), eff_hrse(i%,3%),        ~
                                eff_hrse(i%,4%),                         ~
                                eff_fil1$(i%)
        return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            call "SHOSTAT" ("Updating Department Data")
            for i% = 1% to p_no%
              str(eff_key$(i%),1%,1%)  = eff_proc$(i%)
              str(eff_key$(i%),2%,2%)  = eff_wk$(i%)
              str(eff_key$(i%),4%,1%)  = eff_day$(i%)
              str(eff_key$(i%),5%,1%)  = eff_proc$(i%)
              str(eff_key$(i%),6%,15%) = eff_code$(i%)

              read #1,hold,key = eff_key$(i%), eod goto L31160
                 delete #1
L31160:       gosub data_put

            write #1, eod goto L31260
L31190:     next i%
            if sc_dept$ <> "22" then goto L31230      /* CHECK FOR WOOL */
               gosub auto_current

L31230:     if auto_current% = 1% then return
        return clear all
        goto inputmode
L31260:     call "SHOSTAT" ("(Error) Updating ----> " & eff_key$(i%))
            stop
            goto L31190

        data_put

            put #1, using L35040,eff_proc$(i%), eff_wk$(i%), eff_day$(i%),~
                                eff_proc$(i%),eff_code$(i%),eff_dte$(i%),~
                                eff_tab_prc(i%), eff_tab_upmh(i%),       ~
                                eff_tab_scrp(i%), eff_untd(i%,1%),       ~
                                eff_untd(i%,2%), eff_untd(i%,3%),        ~
                                eff_untd(i%,4%), eff_hrsr(i%,1%),        ~
                                eff_hrsr(i%,2%), eff_hrsr(i%,3%),        ~
                                eff_hrsr(i%,4%), eff_hrso(i%,1%),        ~
                                eff_hrso(i%,2%), eff_hrso(i%,3%),        ~
                                eff_hrso(i%,4%), eff_hrst(i%,1%),        ~
                                eff_hrst(i%,2%), eff_hrst(i%,3%),        ~
                                eff_hrst(i%,4%), eff_prc(i%,1%),         ~
                                eff_prc(i%,2%), eff_prc(i%,3%),          ~
                                eff_prc(i%,4%), eff_upmhg(i%,1%),        ~
                                eff_upmhg(i%,2%), eff_upmhg(i%,3%),      ~
                                eff_upmhg(i%,4%), eff_wages(i%,1%),      ~
                                eff_wages(i%,2%), eff_wages(i%,3%),      ~
                                eff_wages(i%,4%), eff_upmha(i%,1%),      ~
                                eff_upmha(i%,2%), eff_upmha(i%,3%),      ~
                                eff_upmha(i%,4%), eff_labg(i%,1%),       ~
                                eff_labg(i%,2%), eff_labg(i%,3%),        ~
                                eff_labg(i%,4%), eff_laba(i%,1%),        ~
                                eff_laba(i%,2%), eff_laba(i%,3%),        ~
                                eff_laba(i%,4%), eff_eff(i%,1%),         ~
                                eff_eff(i%,2%), eff_eff(i%,3%),          ~
                                eff_eff(i%,4%), eff_scrpa(i%,1%),        ~
                                eff_scrpa(i%,2%), eff_scrpa(i%,3%),      ~
                                eff_scrpa(i%,4%), eff_scrpp(i%,1%),      ~
                                eff_scrpp(i%,2%), eff_scrpp(i%,3%),      ~
                                eff_scrpp(i%,4%), eff_scrp(i%,1%),       ~
                                eff_scrp(i%,2%), eff_scrp(i%,3%),        ~
                                eff_scrp(i%,4%), eff_hrse(i%,1%),        ~
                                eff_hrse(i%,2%), eff_hrse(i%,3%),        ~
                                eff_hrse(i%,4%),                         ~
                                eff_fil1$(i%)
        return

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

L35040: FMT                              /* File = (APCEFFTB)          */~
            CH(01),                      /* EFF Process Flag           */~
            CH(02),                      /* Production Week            */~
            CH(01),                      /* Production Day             */~
            CH(01),                      /* EFF Process Flag           */~
            CH(15),                      /* Efficienct Code (APC EFF02)*/~
            CH(06),                      /* Production Date            */~
            PD(14,4),                    /* Product Avg Price - EFF02  */~
            PD(14,4),                    /* Eff STD UPMH      - EFF02  */~
            PD(14,4),                    /* Product Scrap WT  - EFF02  */~
            4*PD(14,4),                  /* Production Units D,W,M,Y   */~
            4*PD(14,4),                  /* Eff Hours Reg. D,W,M,Y     */~
            4*PD(14,4),                  /* Eff Hours Over D,W,M,Y     */~
            4*PD(14,4),                  /* Eff Hours Tot  D,W,M,Y     */~
            4*PD(14,4),                  /* Prod Total Price D,W,M,Y   */~
            4*PD(14,4),                  /* UPMH Goal D,W,M,Y          */~
            4*PD(14,4),                  /* Wage/Labor Dollars D,W,M,Y */~
            4*PD(14,4),                  /* UPMH Actual D,W,M,H        */~
            4*PD(14,4),                  /* Eff Labor Dollars % D,W,M,Y*/~
            4*PD(14,4),                  /* Eff Actual Labor % S,W,M,Y */~
            4*PD(14,4),                  /* Eff Percent D,W,M,Y        */~
            4*PD(14,4),                  /* Eff Actual Scrap WT D,W,M,Y*/~
            4*PD(14,4),                  /* Eff Prod Scrap WT D,W,M,Y  */~
            4*PD(14,4),                  /* Eff Scrap Percent D,W,M,Y  */~
            4*PD(14,4),                  /* Eff HOURS EARNED  D,W,M,Y  */~
            CH(144)                      /* Filler Area                */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
        REM   GOSUB SET_PF1

              gosub'050(1%, fieldnr%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40105,         /* Production Week      */~
                                L40105,         /* Production Day       */~
                                L40105,         /* Department Code      */~
                                L40110,         /* Total Scrap (Day)    */~
                                L40120          /* Table Values (Day)   */

              goto L40140

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40105:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40110:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40120:           for k% = 1% to p_no%
                      lfac$(4%+k%) = hex(82)
                  next k%
                  return
L40140:     gosub set_pf1
            accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "Production Week, 1-52:",                     ~
               at (03,25), fac(lfac$(1%)), sc_wk$               , ch(02),~
               at (03,30), fac(hex(84)), sc_wk_dte$             , ch(08),~
               at (03,50), fac(lfac$(1%)), sc_yr$               , ch(04),~
                                                                         ~
               at (04,02), "Production Day(1 - 7):",                     ~
               at (04,25), fac(lfac$(2%)), sc_day$              , ch(01),~
               at (04,30), fac(hex(84)), days$                  , ch(09),~
                                                                         ~
               at (05,02), "Department Code      :",                     ~
               at (05,25), fac(lfac$(3%)), sc_dept$             , ch(02),~
               at (05,30), fac(hex(84)), sc_dept_d$             , ch(30),~
                                                                         ~
               at (06,02), "Daily Actual Scrap   :",                     ~
               at (06,25), fac(lfac$(4%)), scrap$               , ch(10),~
                                                                         ~
               at (07,02), fac(hex(84)), d_txt$(1%+kk%)         , ch(10),~
               at (07,15), fac(lfac$(5%)), d_val$(1%+kk%)       , ch(06),~
                                                                         ~
               at (07,27), fac(hex(84)), d_txt$(12%+kk%)        , ch(10),~
               at (07,40), fac(lfac$(16%)), d_val$(12%+kk%)     , ch(06),~
                                                                         ~
               at (07,52), fac(hex(84)), d_txt$(23%+kk%)        , ch(10),~
               at (07,65), fac(lfac$(27%)), d_val$(23%+kk%)     , ch(06),~
                                                                         ~
               at (08,02), fac(hex(84)), d_txt$(2%+kk%)         , ch(10),~
               at (08,15), fac(lfac$( 6%)), d_val$(2%+kk%)      , ch(06),~
                                                                         ~
               at (08,27), fac(hex(84)), d_txt$(13%+kk%)        , ch(10),~
               at (08,40), fac(lfac$(17%)), d_val$(13%+kk%)     , ch(06),~
                                                                         ~
               at (08,52), fac(hex(84)), d_txt$(24%+kk%)        , ch(10),~
               at (08,65), fac(lfac$(28%)), d_val$(24%+kk%)     , ch(06),~
                                                                         ~
               at (09,02), fac(hex(84)), d_txt$(3%+kk%)         , ch(10),~
               at (09,15), fac(lfac$( 7%)), d_val$(3%+kk%)      , ch(06),~
                                                                         ~
               at (09,27), fac(hex(84)), d_txt$(14%+kk%)        , ch(10),~
               at (09,40), fac(lfac$(18%)), d_val$(14%+kk%)     , ch(06),~
                                                                         ~
               at (09,52), fac(hex(84)), d_txt$(25%+kk%)        , ch(10),~
               at (09,65), fac(lfac$(29%)), d_val$(25%+kk%)     , ch(06),~
                                                                         ~
               at (10,02), fac(hex(84)), d_txt$(4%+kk%)         , ch(10),~
               at (10,15), fac(lfac$( 8%)), d_val$(4%+kk%)      , ch(06),~
                                                                         ~
               at (10,27), fac(hex(84)), d_txt$(15%+kk%)        , ch(10),~
               at (10,40), fac(lfac$(19%)), d_val$(15%+kk%)     , ch(06),~
                                                                         ~
               at (10,52), fac(hex(84)), d_txt$(26%+kk%)        , ch(10),~
               at (10,65), fac(lfac$(30%)), d_val$(26%+kk%)     , ch(06),~
                                                                         ~
               at (11,02), fac(hex(84)), d_txt$(5%+kk%)         , ch(10),~
               at (11,15), fac(lfac$( 9%)), d_val$(5%+kk%)      , ch(06),~
                                                                         ~
               at (11,27), fac(hex(84)), d_txt$(16%+kk%)        , ch(10),~
               at (11,40), fac(lfac$(20%)), d_val$(16%+kk%)     , ch(06),~
                                                                         ~
               at (11,52), fac(hex(84)), d_txt$(27%+kk%)        , ch(10),~
               at (11,65), fac(lfac$(31%)), d_val$(27%+kk%)     , ch(06),~
                                                                         ~
               at (12,02), fac(hex(84)), d_txt$(6%+kk%)         , ch(10),~
               at (12,15), fac(lfac$(10%)), d_val$(6%+kk%)      , ch(06),~
                                                                         ~
               at (12,27), fac(hex(84)), d_txt$(17%+kk%)        , ch(10),~
               at (12,40), fac(lfac$(21%)), d_val$(17%+kk%)     , ch(06),~
                                                                         ~
               at (12,52), fac(hex(84)), d_txt$(28%+kk%)        , ch(10),~
               at (12,65), fac(lfac$(32%)), d_val$(28%+kk%)     , ch(06),~
                                                                         ~
               at (13,02), fac(hex(84)), d_txt$(7%+kk%)         , ch(10),~
               at (13,15), fac(lfac$(11%)), d_val$(7%+kk%)      , ch(06),~
                                                                         ~
               at (13,27), fac(hex(84)), d_txt$(18%+kk%)        , ch(10),~
               at (13,40), fac(lfac$(22%)), d_val$(18%+kk%)     , ch(06),~
                                                                         ~
               at (13,52), fac(hex(84)), d_txt$(29%+kk%)        , ch(10),~
               at (13,65), fac(lfac$(33%)), d_val$(29%+kk%)     , ch(06),~
                                                                         ~
               at (14,02), fac(hex(84)), d_txt$(8%+kk%)         , ch(10),~
               at (14,15), fac(lfac$(12%)), d_val$(8%+kk%)      , ch(06),~
                                                                         ~
               at (14,27), fac(hex(84)), d_txt$(19%+kk%)        , ch(10),~
               at (14,40), fac(lfac$(23%)), d_val$(19%+kk%)     , ch(06),~
                                                                         ~
               at (14,52), fac(hex(84)), d_txt$(30%+kk%)        , ch(10),~
               at (14,65), fac(lfac$(34%)), d_val$(30%+kk%)     , ch(06),~
                                                                         ~
               at (15,02), fac(hex(84)), d_txt$(9%+kk%)         , ch(10),~
               at (15,15), fac(lfac$(13%)), d_val$(9%+kk%)      , ch(06),~
                                                                         ~
               at (15,27), fac(hex(84)), d_txt$(20%+kk%)        , ch(10),~
               at (15,40), fac(lfac$(24%)), d_val$(20%+kk%)     , ch(06),~
                                                                         ~
               at (15,52), fac(hex(84)), d_txt$(31%+kk%)        , ch(10),~
               at (15,65), fac(lfac$(35%)), d_val$(31%+kk%)     , ch(06),~
                                                                         ~
               at (16,02), fac(hex(84)), d_txt$(10%+kk%)        , ch(10),~
               at (16,15), fac(lfac$(14%)), d_val$(10%+kk%)     , ch(06),~
                                                                         ~
               at (16,27), fac(hex(84)), d_txt$(21%+kk%)        , ch(10),~
               at (16,40), fac(lfac$(25%)), d_val$(21%+kk%)     , ch(06),~
                                                                         ~
               at (16,52), fac(hex(84)), d_txt$(32%+kk%)        , ch(10),~
               at (16,65), fac(lfac$(36%)), d_val$(32%+kk%)     , ch(06),~
                                                                         ~
               at (17,02), fac(hex(84)), d_txt$(11%+kk%)        , ch(10),~
               at (17,15), fac(lfac$(15%)), d_val$(11%+kk%)     , ch(06),~
                                                                         ~
               at (17,27), fac(hex(84)), d_txt$(22%+kk%)        , ch(10),~
               at (17,40), fac(lfac$(26%)), d_val$(22%+kk%)     , ch(06),~
                                                                         ~
               at (17,52), fac(hex(84)), d_txt$(33%+kk%)        , ch(10),~
               at (17,65), fac(lfac$(37%)), d_val$(33%+kk%)     , ch(06),~
                                                                         ~
               at (18,02), "Total Units = ",                             ~
               at (18,20), fac(hex(84)), tot_unit$              , ch(10),~
                                                                         ~
               at (19,02), "Reg. Hrs = ",                                ~
               at (19,15), fac(hex(84)), tot_hrsr$              , ch(10),~
                                                                         ~
               at (19,30), "Ovr. Hrs = ",                                ~
               at (19,43), fac(hex(84)), tot_hrso$              , ch(10),~
               at (19,55), "Wages = ",                                   ~
               at (19,63), fac(hex(84)), tot_wages$             , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 2% then goto L40865            /* FIRST    */
L40850:           kk% = 0%
                  goto L40140

L40865:        if keyhit% <> 3% then goto L40890            /* LAST     */
L40870:           x% = int(p_no% / 33%)
                  kk% = (x% * 33%)
                  goto L40140

L40890:        if keyhit% <> 6% then goto L40925            /* PREVIOUS */
                  if fieldnr% = 1% then goto L40925       /* NOT APPLIC */
                  if kk% < 34% then goto L40850
                  kk% = kk% - 33%
                  if kk% <= 1% then goto L40850
                  goto L40140

L40925:        if keyhit% <> 5% then goto L40950            /* NEXT     */
                  kk% = kk% + 33%
                  if kk% < p_no% then goto L40140
                  goto L40870

L40950:        if keyhit% <> 15% then goto L40970
                  call "PRNTSCRN"
                  goto L40140

L40970:        if keyhit% <> 20% then goto L40985
                  gosub purge_department

L40985:        if keyhit% <> 12% then goto L41000
                  gosub purge_data

L41000:        if keyhit% <> 13% then goto L41015
                  gosub correct_data

L41015:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L41185     /*  Input Mode             */
            pf$(1%) = "(1)Start Over    (6)Complete Calc       " &       ~
                      "(9)Clear Weekly        (14)Print report"
            pf$(2%) = "                 (7)Move Data           " &       ~
                      "(10)Clear Monthly      (15)Print Screen"
            pf$(3%) = "(4)Previous Fld  (8)Delete Current      " &       ~
                      "(11)Clear Yearly       (16)Exit Program"
            pfkeys$ = hex(01ff030405060708090a0b0c0d0e0f10000000140000)

            if fieldnr% = 1% then L41135
            pf$(1%) = "(1)Start Over        (4)Previous Fld    " &       ~
                      "                                       "
            pf$(2%) = "(2)First Page        (5)Next Page       " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "(3)Last Page         (6)Previous Page   " &       ~
                      "                                       "

            pfkeys$ = hex(010203040506ffffffffff0c0dff0f10000000140000)

L41135:     if fieldnr% > 1% then L41150
                str(pf$(3%),1%,16%) = " " : str(pfkeys$, 4%,1%) = hex(ff)
        return
L41150:     if fieldnr% < 5% then goto L41170
               str(pfkeys$,20%,1%) = hex(ff)
               str(pfkeys$,12%,1%) = hex(ff)
               str(pfkeys$,13%,1%) = hex(ff)
L41170:     gosub check_screen
        return

L41185: if fieldnr% > 0% then L41235  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Update Data "
            pfkeys$ = hex(01ff03ffffffffffffffffffffff0f1000)
            return

L41235:
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                                       "
            pf$(3%) = "                                        " &       ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

        check_screen
            if p_no% > 33% then goto L41320
               gosub no_first
               gosub no_next
               gosub no_last
               gosub no_prev
               return
L41320:      if kk% >= 33% then goto L41335
                gosub no_first
                gosub no_prev
L41335:      if (kk% + 33%) <= p_no% then goto L41345
                gosub no_last
L41345:      if kk% <= (p_no% - 33%) then goto L41355
                gosub no_next
L41355: return
        no_first
            str(pf$(2%),1%,14%)  = " " : str(pfkeys$,2%,1%) = hex(ff)
        return
        no_next
            str(pf$(2%),22%,14%) = " " : str(pfkeys$,5%,1%) = hex(ff)
        return
        no_last
            str(pf$(3%),1%,14%)  = " " : str(pfkeys$,3%,1%) = hex(ff)
        return
        no_prev
            str(pf$(3%),22%,18%) = " " : str(pfkeys$,6%,1%) = hex(ff)
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
              on fieldnr% gosub L42190,         /* Production Week      */~
                                L42190,         /* Production Day       */~
                                L42190,         /* Report Type          */~
                                L42190          /* Report Selection     */
              goto L42220

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L42190:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L42220:     accept                                                       ~
               at (01,02),                                               ~
                  "Production Efficiency/Scrap Reports",                 ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "Production Week, 1-52:",                     ~
               at (03,25), fac(lfac$(1%)), sc_wk$               , ch(02),~
               at (03,30), fac(hex(84)), sc_wk_dte$             , ch(08),~
                                                                         ~
               at (03,40), fac(lfac$(1%)), sc_yr$               , ch(04),~
                                                                         ~
               at (04,02), "Production Day(1 - 7):",                     ~
               at (04,25), fac(lfac$(2%)), sc_day$              , ch(01),~
               at (04,30), fac(hex(84)), days$                  , ch(09),~
                                                                         ~
               at (05,02), "Report Type-1,2,3,4,5:",                     ~
               at (05,25), fac(lfac$(3%)), rpt$                 , ch(01),~
               at (05,30), fac(hex(84)), rpt_d$                 , ch(25),~
                                                                         ~
               at (06,02), "Report Selection     :",                     ~
               at (06,25), fac(lfac$(4%)), rpt_sel$             , ch(01),~
               at (06,30), fac(hex(84)), rpt_sel_d$             , ch(08),~
                                                                         ~
               at (07,21), fac(hex(84)), scr$(1%)               , ch(40),~
               at (08,21), fac(hex(84)), scr$(2%)               , ch(40),~
               at (09,21), fac(hex(84)), scr$(3%)               , ch(40),~
               at (10,21), fac(hex(84)), scr$(4%)               , ch(40),~
               at (11,21), fac(hex(84)), scr$(5%)               , ch(40),~
               at (12,21), fac(hex(84)), scr$(6%)               , ch(40),~
               at (13,21), fac(hex(84)), scr$(7%)               , ch(40),~
               at (14,21), fac(hex(84)), scr$(8%)               , ch(40),~
               at (15,21), fac(hex(84)), scr$(9%)               , ch(40),~
               at (16,21), fac(hex(84)), scr$(10%)              , ch(40),~
               at (17,21), fac(hex(84)), scr$(11%)              , ch(40),~
               at (18,21), fac(hex(84)), scr$(12%)              , ch(40),~
               at (19,21), fac(hex(84)), scr$(13%)              , ch(40),~
               at (20,21), fac(hex(84)), scr$(14%)              , ch(40),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L42700
                  call "PRNTSCRN"
                  goto L42220

L42700:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
        if edit% = 2% then L42890     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L42850
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L42850:     if fieldnr% > 1% then L42870
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L42870:     return

L42890: if fieldnr% > 0% then L42990  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return

L42990:
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
            on fieldnr% gosub L50150,         /* Production Week       */ ~
                              L50720,         /* Production Day        */ ~
                              L50980,         /* Department Code       */ ~
                              L51160,         /* Total Scrap           */ ~
                              L51310          /* Daily Unit Values     */
            return
L50140:
L50150: REM Production Week                       MT_WK$
           gosub check_date
           init(" ") cur_yr$, cur_wk$, cur_dy$, cur_dte$, cur_date$,     ~
                     ent_yr$, ent_wk$, ent_dy$, ent_dte$, ent_date$,     ~
                     cur_yr_bi$, ent_yr_bi$, prv_yr_bi$

           if sc_yr$ = " " then goto L50160
                ent_yr$ = sc_yr$
                convert ent_yr$ to temp%, data goto L50600
                ent_yr_bi$ = bin(temp%, 2)
L50160:    if sc_wk$  <> " " then ent_wk$ = sc_wk$
           if sc_day$ <> " " then ent_dy$ = sc_day$

           call "APCPLN0B" ( cur_yr_bi$, /* Current Production Year    */~
                             cur_wk$,    /* Current Production Week    */~
                             cur_dy$,    /* Current Production Day     */~
                             cur_dte$,   /* Current Production Date(6) */~
                             cur_date$,  /* Current Production Date(8) */~
                             ent_yr_bi$, /* Entry Production Year (IN) */~
                             ent_wk$,    /* Entry Prod Week       (IN) */~
                             ent_dy$,    /* Entry Production Day (OPT) */~
                             ent_dte$,   /* Entry Production Date (6)  */~
                             ent_date$,  /* Entry Production Date *8)  */~
                             prv_yr_bi$, /* Previous Year              */~
                             pl_e%    )  /* 0% = No, 1% = Found        */

            if pl_e% <> 0% then goto L50600

            temp% = val(cur_yr_bi$,2)
            convert temp% to cur_yr$, pic (####)
            temp% = val(ent_yr_bi$,2)
            convert temp% to ent_yr$, pic (####)            
            temp% = val(prv_yr_bi$,2)
            convert temp% to prv_yr$, pic (####)

            sc_wk_dte$ = ent_dte$
            sc_yr$     = ent_yr$
            sv_wk$     = ent_wk$
            sv_day$    = ent_dy$
            wk% = 0%
            convert sv_wk$ to wk%, data goto L50600

            convert sv_day$ to i%, data goto L50600

           eff_dte$ = sc_wk_dte$
           sc_wk$   = sv_wk$
           sc_day$  = sv_day$
           call "DATEFMT" (sc_wk_dte$)
           rpt_dte$ = sc_wk_dte$
           if sc_yr$ > cur_yr$ then goto L50670
           if sc_yr$ < cur_yr$ then goto L50550
           if sc_wk$ > cur_wk$ then goto L50630
L50550:    if rec% = 0% then goto L50590
              gosub L50720
              fieldnr% = 2%

L50590: return
L50600:    errormsg$ = "(Error) - Invalid Production Week (1 thru 52)?"
           init(" ") sc_wk_dte$, sc_wk$, sc_day$, days$, sc_yr$
        return
L50630:    errormsg$ = "(Error) - Future Production Week,Less than Equal ~
        ~to ("& cur_wk$ & ")"
           init(" ") sc_wk_dte$, sc_wk$, sc_day$, days$, sc_yr$
        return
L50670:    errormsg$ = "(Error) - Future Production Year,Less than Equal ~
        ~to ("& cur_yr$ & ")"
           init(" ") sc_wk_dte$, sc_wk$, sc_day$, days$, sc_yr$
        return
L50710:
L50720: REM Production Day
           if sc_day$ <> " " then goto L50760
              sc_day$ = sv_day$

L50760:    convert sc_day$ to x%, data goto L50900

           convert cur_dy$ to y%, data goto L50900

           if x% < 1% or x% > 7% then goto L50900

           if sc_yr$ < cur_yr$ then goto L50860
           if sc_wk$ <> cur_wk$ then goto L50860
           if x% > y% then goto L50930

L50860:    days$ = days$(x%)
           sc_day% = x%

        return
L50900:    errormsg$ = "(Error) - Invalid Production Day (1 thru 7)?"
           init(" ") sc_day$, days$
        return
L50930:    errormsg$ = "(Error) - Future Production Day,Less than Equal t~
        ~o (" & sv_day$ & ")"
           init(" ") sc_day$, days$
        return
L50970:
L50980: REM Department Code                       SC_DEPT$
            if sc_dept$ <> " " then goto L51010
               goto L51120
L51010:     convert sc_dept$ to x%, data goto L51120

            convert x% to sc_dept$, pic(00)

            init(" ") readkey$, sc_dept_d$
            str(readkey$,1%,9%)   = "EMP DEPT "
            str(readkey$,10%,15%) = sc_dept$
            read #2,key = readkey$, using L51100, sc_dept_d$,             ~
                                                           eod goto L51120
L51100:        FMT POS(25), CH(30)
        return
L51120:     errormsg$ = "(Error) - Invalid Department Code?"
            sc_dept$, sc_dept_d$ = " "
        return
L51150:
L51160: REM Total Scrap                                SCRAP
            scrap = 0.0
            convert scrap$ to scrap, data goto L51190
L51190:
            convert scrap to scrap$, pic(######.##-)

            gosub load_prompts
            gosub dataload               /* LOAD PREVIOUS "1"          */~

            gosub calc_scanned_units
           
            if sc_dept$ = "37" then gosub glass_analysis		   
            if sc_dept$ = "34" then gosub analysis_done3		   
        return
            scrap$ = " " : scrap = 0.0
            errormsg$ = "(Error) - Invalid Scrap Value? "
        return

L51310: REM Unit Values                                D_VAL$()
            tot_unit = 0.0
            for i% = 1% to p_no%
               eff_proc$(i%) = "0"
               eff_wk$(i%)   = sc_wk$
               eff_day$(i%)  = sc_day$
               eff_code$(i%) = str(eff_key1$(i%),2%,15%)
               eff_dte$(i%)  = eff_dte$

               eff_tab_prc(i%)  = prc(i%)
               eff_tab_upmh(i%) = upmh(i%)
               eff_tab_scrp(i%) = scrp(i%)

               d_val(i%) = 0.0
               convert d_val$(i%) to d_val(i%), data goto L51460
L51460:
               convert d_val(i%) to d_val$(i%), pic(######)

               tot_unit = tot_unit + d_val(i%)
               eff_untd(i%,1%) = d_val(i%)              /* MODEL UNITS */
            next i%

            if sc_dept$ <> "01" then goto L51570
               gosub get_aluminum
               goto L51800

L51570:     err% = 0%
            mat ef     = zer : mat ef_ov = zer         /* indt()     */
            mat tot_ef = zer : mat wg    = zer
            if seq% = 43% or seq% = 46% then goto L51730
            if seq% = 35% then goto L51730
            e_shift$ = "00"
            call "APCEFFWG" (sc_dept$, sc_yr$, sc_wk$, ef(), ef_ov(),    ~
                             tot_ef(), wg(), e_shift$, #4, #3, err% )
            if err% = 0% then goto L51730
               errormsg$ = "(Error) - Department Hours Must be Fixed ?"
               return

                                                 /* DAILY VALUES GIVEN */
                                         /* Daily Given and Items, Also*/
                                         /* the six (6) Required Start */
                                         /* YDT Values Needed.         */
L51730:      eff_untd(p_no%,1%)  = tot_unit          /* TOT UNITS      */
             eff_hrsr(p_no%,1%)  = ef(sc_day%)       /* EFF REG HOURS  */
             eff_hrso(p_no%,1%)  = ef_ov(sc_day%)    /* EFF OVER HOURS */
             eff_hrst(p_no%,1%)  = tot_ef(sc_day%)   /* TOTAL EFF HOURS*/
             eff_wages(p_no%,1%) = wg(sc_day%)       /* TOTAL WAGES    */
             eff_scrpa(p_no%,1%) = scrap             /* SCRAP          */


										    
L51800: REM - MANAGERS PAY, AND INCENTITIVES

             gosub get_pay
             eff_hrsr(p_no%,1%)  = eff_hrsr(p_no%,1%) + mgr_hrs
             eff_hrst(p_no%,1%)  = eff_hrst(p_no%,1%) + mgr_hrs
             eff_wages(p_no%,1%) = eff_wages(p_no%,1%) + mgr_pay         ~
                                                       + mgr_inct

/* (EWD0002) */
/* IF SCREEN DAY = 7 THEN CALCULATE ADJUSTMENTS FROM TABLE "APC EFFAJ" */
			 
             if sc_day$ <> "7" then goto L52175
		gosub get_adjust							  
		gosub adjust_time
/* (EWD0002) */							

L52175:      if seq% = 43% then eff_wages(p_no%,1%) = 0.0
             if seq% = 46% then eff_wages(p_no%,1%) = 0.0


             convert tot_unit to tot_unit$, pic(##########)
             convert eff_hrsr(p_no%,1%) to tot_hrsr$, pic(######.##-)
             convert eff_hrso(p_no%,1%) to tot_hrso$, pic(######.##-)
             convert eff_wages(p_no%,1%) to tot_wages$, pic(######.##-)

             gosub calc_direct

        return

        get_aluminum
          for i% = 1% to 1%             /* ALUMINUM$() = ALUMINUM DEPT */
            err% = 0%
            mat ef     = zer : mat ef_ov = zer
            mat tot_ef = zer : mat wg    = zer
            e_shift$ = "00"
            call "APCEFFWG" (aluminum$(i%),sc_yr$, sc_wk$, ef(), ef_ov(),~
                             tot_ef(), wg(), e_shift$, #4, #3, err% )
            if err% = 0% then goto L52170
               errormsg$ = "(Error)-Department Hrs Must be Fixed ("&     ~
                                                   aluminum$(i%) & ")"
               return

/* (EWD0002) */
/* IF SCREEN DAY = 7 THEN CALCULATE ADJUSTMENTS FROM TABLE "APC EFFAJ" */
			 
             if sc_day$ <> "7" then goto L52170
                gosub get_adjust
		gosub adjust_time
/* (EWD0002) */							
                                                 /* DAILY VALUES GIVEN */
                                         /* Daily Given and Items, Also*/
                                         /* the six (6) Required Start */
                                         /* YDT Values Needed.         */
L52170:                                              /* EFF REG HOURS  */
             eff_hrsr(p_no%,1%)  = eff_hrsr(p_no%,1%) + ef(sc_day%)
                                                     /* EFF OVER HOURS */
             eff_hrso(p_no%,1%)  = eff_hrso(p_no%,1%) + ef_ov(sc_day%)
                                                     /* TOTAL EFF HOURS*/
             eff_hrst(p_no%,1%)  = eff_hrst(p_no%,1%) + tot_ef(sc_day%)
                                                     /* TOTAL WAGES    */
             eff_wages(p_no%,1%) = eff_wages(p_no%,1%) + wg(sc_day%)
          next i%

             eff_untd(p_no%,1%)  = tot_unit          /* TOT UNITS      */
             eff_scrpa(p_no%,1%) = scrap             /* SCRAP          */
        return

        REM *************************************************************~
            *                 R E P O R T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on REPORT 1.                      *~
            *************************************************************

        deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50150,         /* Production Week       */ ~
                              L50720,         /* Production Day        */ ~
                              L52460,         /* Report Type ( 1 - 4 ) */ ~
                              L52640          /* Report Selection 1-4  */

            return

L52460: REM Report Type                           RPT$
            if rpt$ <> " " then goto L52490
               goto L52600
L52490:     convert rpt$ to x%, data goto L52600

            convert x% to rpt$, pic(0)

            if x% < 1% or x% > 5% then goto L52600
            if rpt$ = "1" then rpt_d$ = "STD Var Table Detail Rpt "
            if rpt$ = "2" then rpt_d$ = "STD Var Table Totals Rpt "
            if rpt$ = "3" then rpt_d$ = "Current Efficiency Report"
            if rpt$ = "4" then rpt_d$ = "Previous Eff. Report     "
            if rpt$ = "5" then rpt_d$ = "History Eff. Report      "
        return
L52600:     errormsg$ = "(Error) - Invalid Report Type?"
            rpt$, rpt_d$ = " "
        return

L52640: REM Report Selection                      RPT_SEL$
            if rpt_sel$ <> " " then goto L52670
               goto L52760
L52670:     convert rpt_sel$ to x%, data goto L52760

            convert x% to rpt_sel$, pic(0)

            if rpt_sel$ = "1" then rpt_sel_d$ = "Daily   "
            if rpt_sel$ = "2" then rpt_sel_d$ = "Weekly  "
            if rpt_sel$ = "3" then rpt_sel_d$ = "Monthly "
            if rpt_sel$ = "4" then rpt_sel_d$ = "Yearly  "
        return
L52760:     errormsg$ = "(Error) - Invalid Report Selection?"
            rpt_sel$, rpt_sel_d$ = " "
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        clear_rec
            init(" ") eff_proc$(),  eff_wk$(), eff_day$(), eff_code$(),  ~
                      eff_dte$(), eff_fil1$()
            mat eff_tab_prc  = zer
            mat eff_tab_upmh = zer
            mat eff_tab_scrp = zer
            mat eff_untd = zer
            mat eff_hrsr = zer
            mat eff_hrso = zer
            mat eff_hrst = zer
            mat eff_prc  = zer
            mat eff_upmhg= zer
            mat eff_wages= zer
            mat eff_upmha= zer
            mat eff_labg = zer
            mat eff_laba = zer
            mat eff_eff  = zer
            mat eff_scrpa= zer
            mat eff_scrpp= zer
            mat eff_scrp = zer
            mat eff_hrse = zer
        return

        load_prompts                   /* LOAD PROMPTS FOR DEPARTMENT */
                                       /* AND CREATE DATA LOAD KEY    */
           i% = 0% : p_no% = 0% : kk% = 0%
                                       /* UNIT VALUES                 */
           init(" ") readkey$, seq$, eff_key1$()
           str(readkey$,1%,9%) = "APC EFF01"
L60165:    read #2,key > readkey$, using L60175, readkey$,                ~
                                                          eod goto L60215
L60175:       FMT CH(24)
           if str(readkey$,1%,9%) <> "APC EFF01" then goto L60215
           if str(readkey$,12%,2%) <> sc_dept$ then goto L60165
              seq% = 0%
              seq$ = str(readkey$,10%,2%)
              convert seq$ to seq%, data goto L60205
L60205:

L60215:    if seq% = 0% then goto L60395

           readkey$ = " "
           str(readkey$,1%,9%) = "APC EFF02"
           str(readkey$,10%,4%) = seq$ & sc_dept$

L60245:    read #2,key > readkey$, using L60255, readkey$, descr$,        ~
                                                eod goto prompts_done
L60255:       FMT CH(24), CH(30)
           if str(readkey$,1%,9%) <> "APC EFF02" then goto prompts_done
           if str(readkey$,10%,2%) <> seq$ then goto prompts_done
           if str(readkey$,12%,2%) <> sc_dept$ then goto prompts_done
              i% = i% + 1%
              if i% > max_product% then  i% = max_product% /*MAX BUCKETS*/
              str(eff_key1$(i%),1%,1%) = "1"
              str(eff_key1$(i%),2%,15%) = str(readkey$,10%,15%)

              d_txt$(i%) = str(readkey$,15%,10%)
                                                 /* Average Unit Price */
              convert str(descr$,1%,9%) to prc(i%), data goto L60315
L60315:
                                                 /* Standard Unit/MHR  */
              convert str(descr$,11%,9%) to upmh(i%), data goto L60330
L60330:
                                                 /* SRD Scrap Weight   */
              convert str(descr$,21%,9%) to scrp(i%), data goto L60345
L60345:
              d_val$(i%) = "0"

           goto L60245
        prompts_done
           if i% = 0% then goto L60395
           p_no% = i%                      /* Number of Entries Needed */
                                           /* for Department           */
                                           /* P_NO% = Total Product    */
        return                             /*         Buckets for Dept */
L60395:    errormsg$ = "(Error) Loading Prompt Definitions"
           init(" ") d_txt$(), d_val$()
        return

        calc_direct                           /* CALC TOTALS - W, M, Y */
                                              /* For Departments       */
            call "SHOSTAT" ("Calculating Given Dept. (A)")
            for k% = 2% to 4%
                                               /* Product Units W,M,Y */
          eff_untd(p_no%,k%) =                                           ~
                     round( eff_untd(p_no%,k%) + eff_untd(p_no%,1%), 2)
                                               /* Reg Hours W,M,Y     */
          eff_hrsr(p_no%,k%) =                                           ~
                     round( eff_hrsr(p_no%,k%) + eff_hrsr(p_no%,1%), 2)
                                               /* Overtime Hours W,M,Y*/
          eff_hrso(p_no%,k%) =                                           ~
                     round( eff_hrso(p_no%,k%) + eff_hrso(p_no%,1%), 2)
                                               /* Total Eff Hours W,M,Y*/
          eff_hrst(p_no%,k%) =                                           ~
                     round( eff_hrst(p_no%,k%) + eff_hrst(p_no%,1%), 2)
                                               /* Total Eff Waged W,M,Y*/
          eff_wages(p_no%,k%) =                                          ~
                     round( eff_wages(p_no%,k%) + eff_wages(p_no%,1%), 2)
                                               /* Tot Actual Scrap     */
                                               /* Weight for W,M, Y    */
          eff_scrpa(p_no%,k%) =                                          ~
                     round( eff_scrpa(p_no%,k%) + eff_scrpa(p_no%,1%), 2)
            next k%
                                                /* Calc Actual UPMHA */
                                                /* Department Calc   */
                                          /* MOD - 01/23/96 DEACTIVATE */
        REM FOR K% = 1% TO 4%
        REM     IF EFF_HRST(P_NO%,K%) > 0.0 THEN GOTO 60565
        REM        EFF_UPMHA(P_NO%,K%) = 0.0
        REM        GOTO 60575
                                          /* MOD - 02/22/95 Activate */
        REM     EFF_UPMHA(P_NO%,K%) =                                    ~
        REM          ROUND( EFF_UNTD(P_NO%,K%) / EFF_HRST(P_NO%,K%), 4)
        REM NEXT K%

            call "SHOSTAT" ("Calculating Prod. Detail (B)")
                                        /* Calculations for Detail Line*/
                                        /* Items (Products)            */
           for i% = 1% to (p_no% - 1%)

*       RHH
              if eff_tab_upmh(i%) = 0 then goto L60635
               eff_hrse(i%,1%) = eff_untd(i%,1%) * (1.0/eff_tab_upmh(i%))
L60635:
             for k% = 1% to 4%
                                        /* Product Total Price (14)    */
                                        /* By Detail Line and For W,M,Y*/
              if k% = 1% then goto L60670
                 eff_untd(i%,k%) = eff_untd(i%,k%) + eff_untd(i%,1%)

L60670:       eff_hrse(p_no%,k%) = eff_hrse(p_no%,k%) + eff_hrse(i%,1%)

              eff_prc(i%,k%) = round(eff_untd(i%,k%) * prc(i%), 2)
              eff_prc(p_no%,k%) =                                        ~
                          round(eff_prc(p_no%,k%) + eff_prc(i%,k%), 2)
                                        /* Calc Units Per Manhour Goal */
                                        /* By Detail Line and For W,M,Y*/
              if eff_untd(p_no%,k%) > 0.0 then goto L60725
                 eff_upmhg(i%,k%) = 0.0
                 goto L60735

L60725:       eff_upmhg(i%,k%) =                                         ~
               round((eff_untd(i%,k%)/eff_untd(p_no%,k%)) * upmh(i%), 2)
L60735:       eff_upmhg(p_no%,k%) =                                      ~
                 round(eff_upmhg(p_no%,k%) + eff_upmhg(i%,k%), 2)

                                        /* Production Scrap Weight by */
                                      /* Product Item, and By D,W,M,Y */
              eff_scrpp(i%,k%) =                                         ~
                 round(eff_untd(i%,k%) * scrp(i%), 2)
              eff_scrpp(p_no%,k%) =                                      ~
                 round(eff_scrpp(p_no%,k%) + eff_scrpp(i%,k%), 2)

             next k%

           next i%

           call "SHOSTAT" ("Calculating Efficiency Percent (C)")
        REM STOP "CHECK CALC EFF "
        REM CLOSE WS

                                        /* Calc For Efficiency Percent */
                                        /* Columns.                    */
           for k% = 1% to 4%
                                        /* Calc Actual Labor Percent   */
                                        /* For Deptment D,W,M,Y        */
              if eff_prc(p_no%,k%) < .01 then goto L60870
              eff_laba(p_no%,k%) =                                       ~
                 round(eff_wages(p_no%,k%) / eff_prc(p_no%,k%), 4)

L60870:                                 /* Calc For Efficiency Percent */
                                        /* For Department D,W,M,Y      */
*       RHH                            /* Activated 01/05/96          */
              if eff_hrst(p_no%,k%) < .01 then goto L60900
              eff_eff(p_no%,k%) =                                        ~
                  round(eff_hrse(p_no%,k%) / eff_hrst(p_no%,k%), 4)
L60900:
              eff_upmha(p_no%,k%) = round(eff_hrse(p_no%,k%), 4)
                                       /* MOD - 01/23/96 - DEACTIVATED */
        REM   EFF_UPMHA(P_NO%,K%) =                                      ~
        REM      ROUND( EFF_EFF(P_NO%,K%) * EFF_UPMHG(P_NO%,K%), 4)
        REM   IF EFF_UPMHG(P_NO%,K%) > 0.0 THEN GOTO 60925
        REM      EFF_EFF(P_NO%,K%) = 0.0
        REM      GOTO 60940

        REM   EFF_EFF(P_NO%,K%) =                                        ~
        REM      ROUND( EFF_UPMHA(P_NO%,K%) / EFF_UPMHG(P_NO%,K%), 4)

*       RHH MOD TO SCRAP PCNT FOR IG - 02/15/95
                                        /* Efficiency Scrap Percent By */
                                        /* Department for D, W, M, Y   */
              if eff_scrpp(p_no%,k%) < .01 then goto L61020
              eff_scrp(p_no%,k%) =                                       ~
                 round(eff_scrpa(p_no%,k%) / eff_scrpp(p_no%,k%), 4)
                                        /* MOD - 01/23/96 - DEACTIVATE */
        REM   IF SC_DEPT$ <> "10" AND SC_DEPT$ <> "37" THEN GOTO 60995
        REM      EFF_SCRP(P_NO%,K%) = 0%
        REM      IF EFF_UNTD(P_NO%,K%) < .01 THEN GOTO 60995
        REM         EFF_SCRP(P_NO%,K%) =                                 ~
        REM            ROUND(EFF_SCRPA(P_NO%,K%) / EFF_UNTD(P_NO%,K%), 4)
L61020:    next k%

        return

                                        /* FROM SEQ NO. 30 AND GREATER*/
        calc_indirect
            seq% = 0%
            convert str(tot_key$(k%),6%,2%) to seq%, data goto L61060
L61060:
            convert str(tot_key$(k%),8%,2%) to dept%, data goto L61070
L61070:
            for j% = 1% to 4%
                in% = 0% : gosub display_debug
                if seq% <> 35% then goto L61100
                   eff_labg(1%,j%),eff_wages(1%,j%) = 0.0

L61100:         if tot_wg(j%) = 0 then goto L61120
            eff_labg(1%,j%) =                                            ~
                round( eff_wages(1%,j%) / tot_wg(j%), 4)

L61120:     if seq% < 31% or seq% > 39% then goto L61210

                                         /* By DEpartment UPMHG Goal   */
        REM  EFF_UPMHG(1%,J%)            /* Fixed Entry By Department  */
             eff_upmhg(1%,j%) = indt(seq% - 30%)  /*All Values From Tab*/
             if seq% <> 35% and seq% <> 38% then goto L61160
                eff_upmha(1%,j%), eff_upmhg(1%,j%), eff_eff(1%,j%) = 0.0

L61160:      if tot_wg(j%) = 0 then goto L61175
             eff_upmha(1%,j%) =                                          ~
                 round( (eff_wages(1%,j%) / tot_wg(j%)) * 100.0 , 4)
L61175:      if eff_upmha(1%,j%) = 0 then goto L61190
             eff_eff(1%,j%) =                                            ~
                 round( eff_upmhg(1%,j%) / eff_upmha(1%,j%), 4)
L61190: REM - 01/23/96 - EARNED HOURS INDIRECT
             eff_upmha(1%,j%) =                                          ~
                 round( eff_eff(1%,j%) * eff_hrst(1%,j%), 4)
             in% = 1% : gosub display_debug
L61210:      next j%

        return

        move_data
            clear% = 0%
            call "SHOSTAT" ("Moving Previous Data to History")
            readkey$ = all(hex(00))
            str(readkey$,1%,1%) = "0"
            read #1,key > readkey$, using L61330, readkey$, eod goto L61265
               if str(readkey$,1%,1%) = "0" then goto L61305
L61265:     errormsg$ = "(ERROR) - No Data to Move "
            call "SHOSTAT" ( errormsg$ ) : stop
            return
        move_clear
            clear% = 1%
            call "SHOSTAT" ("Moving Previous Data to History")
                                           /* 1st Move Previous Day To */
                                           /* History                  */
L61305:     readkey$ = all(hex(00))
            str(readkey$,1%,1%) = "1"
        move_prev
            read #1,hold,key > readkey$, using L61330, readkey$,          ~
                                               eod goto move_curr
L61330:        FMT CH(20)
            if str(readkey$,1%,1%) <> "1" then goto move_curr
               get #1, using L61345, eff_rec$()
L61345:          FMT 2*CH(256), CH(162)
               if clear% = 1% then goto L61360
                  delete #1
L61360:        str(eff_rec$(1),1%,1%) = "2"        /* Now History Data */
               str(eff_rec$(1),5%,1%) = "2"
L61370:        put #1, using L61345, eff_rec$()
               write #1, eod goto L61385
               goto move_prev
L61385:           read #1,hold,key = str(eff_rec$(1%),1%,20%),           ~
                                                       eod goto move_prev
                     delete #1
                     goto L61370

        move_curr
            if clear% = 1% then return

            call "SHOSTAT" ("Moving Current Data to Previous")
                                         /* 2nd Move Curr Day To Prev. */
            readkey$ = all(hex(00))
            str(readkey$,1%,1%) = "0"
        move_curr_nxt
            read #1,hold,key > readkey$, using L61460, readkey$,          ~
                                               eod goto move_data_done
L61460:        FMT CH(20)
            if str(readkey$,1%,1%) <> "0" then goto move_data_done
               get #1, using L61475, eff_rec$()
L61475:          FMT 2*CH(256), CH(162)
               delete #1
               str(eff_rec$(1),1%,1%) = "1"       /* Now Previous Data */
               str(eff_rec$(1),5%,1%) = "1"
               put #1, using L61475, eff_rec$()
               write #1, eod goto L61550
               goto move_curr_nxt
        move_data_done
        return clear all
        goto inputmode

L61530:     call "SHOSTAT" ("(Error)-Moving Prev. Data For ->"&readkey$)
            stop
            goto move_prev

L61550:     call "SHOSTAT" ("(Error)-Moving Curr. Data For ->"&readkey$)
            stop
            goto move_curr_nxt

        clear_weekly                     /* Clear All Week to Date     */
/* (EWD0003) */
            gosub ok_clear
            if comp% <> 0% then return
/* (EWD0003) */
            gosub move_clear
            clear% = 0%

            call "SHOSTAT" ("Clearing All Weekly Buckets")
            b% = 1% : e% = 2%            /* Daily and Weekly Buckets   */
            goto L61675
        clear_monthly                    /* Clear All Month To Date    */
/* (EWD0003) */
            gosub ok_clear
            if comp% <> 0% then return
/* (EWD0003) */
            gosub move_clear
            clear% = 0%

            call "SHOSTAT" ("Clearing All Monthly Buckets")
            b% = 1% : e% = 3%            /* Weekly and Monthly Buckets */
            goto L61675
        clear_yearly                     /* Clear All Year  To Date    */
/* (EWD0003) */
            gosub ok_clear
            if comp% <> 0% then return
/* (EWD0003) */
            gosub move_clear
            clear% = 0%

            call "SHOSTAT" ("Clearing All Yearly Buckets")
            b% = 1% : e% = 4%            /* Monthly and Yearly Buckets */

L61675:     readkey$ = all(hex(00))
            str(readkey$,1%,1%) = "1"
        clear_next
            read #1,hold,key > readkey$, using L61330, readkey$,          ~
                                               eod goto clear_done
               FMT CH(20)
            if str(readkey$,1%,1%) <> "1" then goto clear_done
               i% = 1%
               gosub data_get
               delete #1
               for k% = b% to e%
                   eff_untd(1%,k%)  = 0.0
                   eff_hrsr(1%,k%)  = 0.0
                   eff_hrso(1%,k%)  = 0.0
                   eff_hrst(1%,k%)  = 0.0
                   eff_prc(1%,k%)   = 0.0
                   eff_upmhg(1%,k%) = 0.0
                   eff_wages(1%,k%) = 0.0
                   eff_upmha(1%,k%) = 0.0
                   eff_labg(1%,k%)  = 0.0
                   eff_laba(1%,k%)  = 0.0
                   eff_eff(1%,k%)   = 0.0
                   eff_scrpa(1%,k%) = 0.0
                   eff_scrpp(1%,k%) = 0.0
                   eff_scrp(1%,k%)  = 0.0
                   eff_hrse(1%,k%)  = 0.0
               next k%
               gosub data_put
               write #1, eod goto L61530
               goto clear_next
        clear_done
        return clear all
        goto inputmode

          call "SHOSTAT" ("(Error)-While Clearing Data For -> "&readkey$)
          stop
          goto clear_next

        complete_calc
            call "SHOSTAT" ("Completing Calc's for Day")

            mat tot_wg = zer : mat eff_wages = zer
            init(" ") tot_key$()
            i% = 0%
            readkey$ = all(hex(00))
            str(readkey$,1%,1%) = "0"
        comp_nxt
            read #1,key > readkey$, using L61920, readkey$,               ~
                                                 eod goto comp_done
L61920:        FMT CH(20)
            if str(readkey$,1%,1%) <> "0" then goto comp_done
            if str(readkey$,11%,5%) <> "TOTAL" then goto comp_nxt

               get #1, using L61950, eff_wages(1%,1%), eff_wages(1%,2%),  ~
                                    eff_wages(1%,3%), eff_wages(1%,4%)
L61950:           FMT POS(243), 4*PD(14,4)

               for k% = 1% to 4%
                 tot_wg(k%) = round(tot_wg(k%) + eff_wages(1%,k%), 2)
               next k%
               i% = i% + 1%
               tot_key$(i%) = str(readkey$,1%,20%)
               goto comp_nxt
        comp_done
            p_no%  = i%
            for k% = 1% to p_no%
              read #1,hold,key = tot_key$(k%), eod goto L62035
                 i% = 1%
                 gosub data_get
                 gosub calc_indirect
                 gosub data_put
                 rewrite #1
L62035:     next k%
        return

        check_access
            init(" ") readkey$
            str(readkey$,1%,9%)   = "APC EFF00"
            str(readkey$,10%,15%) = userid$
            read #2,key = readkey$, eod goto L62085
               FMT POS(25), CH(30)
        return
L62085:    gosub access_denied
           goto exit_program

        access_denied
            comp% = 2%
            hdr$ = "* A c c e s s   D e n i e d *"
            msg$(1) = "You Do Not Have Access to the Efficiency System ?"
            msg$(2) = "                 S e c u r i t y                 "
            msg$(3) = "   Press <RETURN> or Any (PF) Key To Continue.   "
            call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
        return

        check_date                          /* Forces eff. screen to current in file */
            rec% = 0%
            if rpt% = 1% then return
            init(" ") readkey$, descr$
            str(readkey$,1%,1%) = "0"
            read #1,key > readkey$, using L62175, readkey$, descr$,       ~
                                                          eod goto L62200
L62175:        FMT CH(20), CH(6)
            if str(readkey$,1%,1%) <> "0" then return
            temp1$  = str(descr$,1%,6%)         /* production date */
            call "DATFMTC" (temp1$, temp1%, temp2$)
            sc_yr$  = str(temp2$,1%,4%)         /* YYYY            */
            sc_wk$  = str(readkey$,2%,2%)
            sc_day$ = str(readkey$,4%,1%)
            rec% = 1%
L62200: return

        delete_current
            call "SHOSTAT" ("Deleting Current Data")
            readkey$ = all(hex(00))
            str(readkey$,1%,1%) = "0"
L62230:     read #1,hold,key > readkey$, using L62240, readkey$,          ~
                                                           eod goto L62260
L62240:        FMT CH(20)
            if str(readkey$,1%,1%) <> "0" then return
               delete #1
               goto L62230
L62260: return

        auto_current
           call "SHOSTAT" ("AUTO LOADING INDIRECT DATA")          
           auto_current% = 1%
           for r% = 1% to 15%
             gosub initialize_variables        /* CLEAR VALUES         */
             gosub L50140                       /* PRODUCTION DATE      */
             gosub L50710                       /* PRODUCTION DAY       */
             sc_dept$ = auto_tab$(r%)          /* DEPARTMENT VALUE     */
             if sc_dept$ = "  " then goto L62345
             gosub L50970                       /* DEPARTMENT           */
             scrap$ = "0.0"
             gosub L51150                       /* SCRAP ROUTINE        */
             gosub L51310                       /* CHECK UNITS          */
             gosub dataput                     /* SAVE DATA            */
           next r%
L62345:    auto_current% = 0%
        return clear all
        goto inputmode

        correct_data
                                   /* Step (1) - Delete Previous       */
                                   /* Step (2) - Make Previous Curr    */
                                   /* Step (3) - Move History to Prev. */
                                   /* Note - History Used SC_WK$ and   */
                                   /*        SC_DAY$ for Move          */
          if userid$ <> "RHH" then return

          call "SHOSTAT" ("Correcting - Previous to Current")
          call "SHOSTAT" (                                               ~
               "Do You Really want to 'Correct Data', If Not Call Roy??")
          stop : close ws

                                           /* 1st Delete Previous Data */
            readkey$ = all(hex(00))
            str(readkey$,1%,1%) = "1"
        corr_nxt
            read #1,hold,key > readkey$, using L62460, readkey$,          ~
                                               eod goto corr_curr
L62460:        FMT CH(20)
            if str(readkey$,1%,1%) <> "1" then goto corr_curr
               get #1, using L62475, eff_rec$()
L62475:          FMT 2*CH(256), CH(162)
               delete #1                   /* Delete Previous Data     */
               str(eff_rec$(1),1%,1%) = "0"
               str(eff_rec$(1),5%,1%) = "0"
               put #1, using L62475, eff_rec$()
               write #1, eod goto L62670    /* Make Previous Data Curr  */
               goto corr_nxt
        corr_curr
            call "SHOSTAT" ("Correcting - History to Previous")
                                         /* 2nd Move History Data to */
                                         /* Previous Data.           */
            readkey$ = all(hex(00))
            str(readkey$,1%,1%) = "2"
            str(readkey$,2%,2%) = sc_wk$
            str(readkey$,4%,1%) = sc_day$
            str(readkey$,5%,1%) = "2"
            str(sav_key$,1%,5%) = str(readkey$,1%,5%)
        corr_curr_nxt
            read #1,hold,key > readkey$, using L62575, readkey$,          ~
                                               eod goto corr_data_done
L62575:        FMT CH(20)
            if str(readkey$,1%,5%) <> str(sav_key$,1%,5%) then           ~
                                                   goto corr_data_done
               get #1, using L62595, eff_rec$()
L62595:          FMT 2*CH(256), CH(162)
               delete #1                      /* Delete History for     */
               str(eff_rec$(1),1%,1%) = "1"   /* Specified Week and Day */
               str(eff_rec$(1),5%,1%) = "1"
               put #1, using L62595, eff_rec$()
               write #1, eod goto L62650       /* Create Previous for    */
               goto corr_curr_nxt             /* Specified Week and Day */
        corr_data_done
        return clear all
        goto inputmode

L62650:       call "SHOSTAT" ("WRITE (ERROR) CORRECTING PREVIOUS -- > ")
              stop
              goto corr_curr_nxt

L62670:       call "SHOSTAT" ("WRITE (ERROR) CORRECTING CURRENT --- > ")
              stop
              goto corr_nxt

        purge_data
        REM Set Production week and Production year for Purge
            sc_wk$ = "35"
            sc_yr$ = "1998"

            gosub ok_purge
            if comp% <> 0% then return
               cnt% = 0%
               call "SHOSTAT" ("Purging Old Efficiency Data")
               prg_key$ = all(hex(00))
               str(prg_key$,1%,1%) = "2"         /* Set History Status */
L62740: purge_next
               read #1,hold,key > prg_key$, using L62755, prg_key$,       ~
                                            eff_dte$, eod goto L62805
L62755:           FMT CH(20), CH(6)
               if mod(cnt%,25%) <> 0 then goto L62780
                  print at(03,38);hex(84);"[";cnt%;"]"
               temp1$ = eff_dte$
               call "DATFMTC" (temp1$, temp%, temp2$)
L62780:        if str(temp2$,1%,4%) = sc_yr$ then goto L62785
                  goto L62740  
L62785:        if str(prg_key$,2%,2%) > sc_wk$ then goto L62740
                  delete #1
                  cnt% = cnt% + 1%
                  goto purge_next

L62805: return clear all
        goto inputmode

        ok_purge
            init(" ") hdr$, msg$()
            comp% = 2%
            hdr$ = "*** Purge Utility Program ***"
            msg$(1) = "   All Data Prior-To and Including Production   "
            msg$(2) = "Week ("&sc_wk$&") for year ("&sc_yr$&") will "&~
                      "be purged??????"
            msg$(3) = "Press <RETURN> To Continue, Any PF() Key To Exit"
            call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
        return

/* (EWD0003) */
        ok_clear
            init(" ") hdr$, msg$()
            comp% = 2%
            hdr$ = "*** Clear Utility Program ***"
            msg$(1) = "       Data Will Be Cleared !!!!!               "
            msg$(2) = "  Are You Sure You Want to Delete Data??        "
            msg$(3) = "Press <RETURN> To Continue, Any PF() Key To Exit"
            call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
        return
/* (EWD0003) */

        get_pay
           mgr_hrs, mgr_pay, mgr_inct = 0.0
           if sc_day$ = "6" then return
           if sc_day$ = "7" then return
           readkey$ = " "
           str(readkey$,1%,9%)   = "APC EFF03"
           str(readkey$,10%,15%) = sc_dept$
           read #2,key = readkey$, using L62910, descr$, eod goto L62950
L62910:       FMT POS(25), CH(30)
           convert str(descr$,1%,6%) to mgr_hrs, data goto L62920
L62920:

           convert str(descr$,13%,6%) to mgr_pay, data goto L62935
L62935:
           convert str(descr$,20%,6%) to mgr_inct, data goto L62945
L62945:
L62950: return

        purge_department
            call "SHOSTAT" (" BEGINNING TO CLEAR DEPARTMENT ") : stop
            init(" ") readkey$
            str(readkey$,1%,9%) = "APC EFF01"
L62980:     read #2,key > readkey$, using L62985,readkey$,eod goto L63125
L62985:         FMT CH(24)
            if str(readkey$,1%,9%) <> "APC EFF01" then goto L63125
            if str(readkey$,12%,2%) <> sc_dept$ then goto L62980
            seq% = 0%
            seq$ = str(readkey$,10%,2%)
            call "SHOSTAT" ("PURGING PREVIOUS FOR DEPT ("& sc_dept$ &")")
            init(" ") readkey$, eff_k$
            str(eff_k$,1%,1%) = "1"               /* PURGE PREVIOUS */
            str(eff_k$,2%,2%) = seq$
            str(eff_k$,4%,2%) = sc_dept$
            str(readkey$,1%,5%)  = str(eff_k$,1%,5%)
L63040:     read #1,hold,key 1% > eff_k$,using L63050,eff_k$,             ~
                                                           eod goto L63070
L63050:         FMT POS(5), CH(16)
            if str(readkey$,1%,5%) <> str(eff_k$,1%,5%) then goto L63070
               delete #1
               goto L63040
L63070:     init(" ") readkey$, eff_k$
            call "SHOSTAT" ("PURGING HISTORY FOR DEPT ("& sc_dept$ &")")
            str(eff_k$,1%,1%) = "2"                  /* PURGE HISTORY  */
            str(eff_k$,2%,2%) = seq$
            str(eff_k$,4%,2%) = sc_dept$
            str(readkey$,1%,5%)  = str(eff_k$,1%,5%)
L63100:     read #1,hold,key 1% > eff_k$,using L63050,eff_k$,             ~
                                                           eod goto L63125
            if str(readkey$,1%,5%) <> str(eff_k$,1%,5%) then goto L63125
               delete #1
               goto L63100
L63125: return clear all
        goto inputmode

        display_debug
                                         /* IN% = 0% - Set Before Calc */
                                         /* IN% = 1% - Set After Calc  */
            if userid$ <> "RRH" then return
            gosub set_debug
L63165:     accept                                                       ~
               at (01,02), fac(hex(94)), dbg_msg$               , ch(45),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,31),                                               ~
                  "Debug Display Screen for ",                           ~
               at (02,56), fac(hex(84)), type$                  , ch(07),~
                                                                         ~
               at (05,10), "Sequence No.  :",                            ~
               at (05,30), fac(hex(84)), dbg$( 1%)              , ch(10),~
                                                                         ~
               at (06,10), "Dept. Code    :",                            ~
               at (06,30), fac(hex(84)), dbg$( 2%)              , ch(10),~
                                                                         ~
               at (08,10), "Dept. Wages   :",                            ~
               at (08,30), fac(hex(84)), dbg$( 3%)              , ch(10),~
                                                                         ~
               at (10,10), "Plant Wages   :",                            ~
               at (10,30), fac(hex(84)), dbg$( 4%)              , ch(10),~
                                                                         ~
               at (12,10), "Labor Dollars :",                            ~
               at (12,30), fac(hex(84)), dbg$( 5%)              , ch(10),~
                                                                         ~
               at (14,10), "UPMH Goal     :",                            ~
               at (14,30), fac(hex(84)), dbg$( 6%)              , ch(10),~
                                                                         ~
               at (16,10), "UPMH Actual   :",                            ~
               at (16,30), fac(hex(84)), dbg$( 7%)              , ch(10),~
                                                                         ~
               at (18,10), "Efficiency Pct:",                            ~
               at (18,30), fac(hex(84)), dbg$( 8%)              , ch(10),~
                                                                         ~
               at (20,10), "Total Wages I%:",                            ~
               at (20,30), fac(hex(84)), dbg$( 9%)              , ch(10),~
                                                                         ~
               at (21,10), "Total Hours   :",                            ~
               at (21,30), fac(hex(84)), dbg$(10%)              , ch(10),~
                                                                         ~
               at (24,02), fac(hex(a4)), d_inp$                 , ch(79),~
                                                                         ~
               keys(dbkey$), key(keyhit%)

               if keyhit% <> 15% then goto L63380
                  call "PRNTSCRN"
                  goto L63165

L63380:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_debug
            if in% = 0% then                                             ~
               dbg_msg$ = "** Before Indirect Labor Calc - Efficiency **"~
                        else                                             ~
               dbg_msg$ = "** After Indirect Labor Calc  - Efficiency **"

          d_inp$ = "Press <Return> To Continue, PF(15) to Print the Scree~
        ~n?"
            dbkey$ = hex(01ffffffffffffffffffffffffff0f1000)
            init(" ") dbg$(), type$
                if j% = 1% then type$ = "Daily  "
                if j% = 2% then type$ = "Weekly "
                if j% = 3% then type$ = "Monthly"
                if j% = 4% then type$ = "Yearly "

            convert seq%  to dbg$( 1%), pic(##########)

            convert dept% to dbg$( 2%), pic(##########)

            convert eff_wages(1%,j%) to dbg$( 3%),pic(####.####-)

            convert tot_wg(j%) to dbg$( 4%),pic(####.####-)

            convert eff_labg(1%,j%) to dbg$( 5%),pic(####.####-)

            convert eff_upmhg(1%,j%) to dbg$( 6%),pic(####.####-)

            convert eff_upmha(1%,j%) to dbg$( 7%),pic(####.####-)

            convert eff_eff(1%,j%) to dbg$( 8%),pic(####.####-)

            convert tot_wg(i%) to dbg$( 9%),pic(####.####-)

            convert eff_hrst(1%,j%) to dbg$(10%),pic(####.####-)

        return

        calc_scanned_units
            yr% = 0% : leap_yr% = 365%
            temp1$ = eff_dte$
            call "DATFMTC" (temp1$, temp%, temp2$)
            convert str(temp2$,1%,4%) to yr%, data goto L63590
L63590:
            if mod(yr%,4%) = 0% then leap_yr% = 366%
                             /* Windows Scanned from 7 AM of Specified */
                             /* Production Day, Until 6 59 AM of the   */
                             /* Next Production Day. 1st,2nd,3rd Shifts*/
            pd_dept$ = "0" & sc_dept$          /* Applicable Department*/

            call "SHOSTAT" ("Calc. New Scanning Units")
            call "DATE" addr("GJ", eff_dte$, jdate1$, x%)
                                               /* Convert to Julian DTE*/
            get jdate1$ using L63600, temp%
L63600:         FMT PD(9,0)
            convert temp% to jdate1$, pic(#######)
            convert str(jdate1$,5%,3%) to j1%, data goto L63650
L63650:
            convert sc_day$ to jj%, data goto L63660
L63660:                                        /* Current Production */
            j1% = j1% + (jj% - 1%)             /* Day Julian Date    */
            if j1% <= leap_yr% then goto L63690
               convert (yr%+1%) to str(jdate1$,1%,4%), pic(####)
               j1% = j1% - leap_yr%

L63690:     convert j1% to str(jdate1$,5%,3%), pic(000)
                                               /* Begin with Production*/
            call "DATJULCV" (jdate1$)
            call "DATE" addr("JG", jdate1$, bg_dte$, x%) /* Date and   */

/* (EWD0001)      */
/* MOVE CHECK DEPT DOWN SO THAT BG_DTE$ WILL HAVE VALUE BEFORE RETURN   */

           gosub check_dept
           if check% = 0% then return         /* Not a Scanning Dept. */
/* (EWD0001)      */

            p_max% = p_no%                     /* No. of Models Defined*/
            p_flg% = 1%                        /* Do Not Load Planning */
            p_scan% = 1%                       /* Only Scanned Product */
            p_shft$ = "AL"                     /* All Shifts           */
            p_screen% = 0%                     /* Display Screen       */
            init(" ") p_mod$()
            mat p_unt% = zer
            mat p_unts%  = zer
            mat p_untss% = zer
            mat p_untpp% = zer
            for i% = 1% to p_no%               /* Pre Load Models      */
              p_mod$(i%) = str(d_txt$(i%),1%,3%)
            next i%
           sc_load$ = "ALL  "            /* Default equal All Loads    */
           ed_load$ = "ALL  "            /* Default equal All Loads    */

           call "APCPLC40" ( pd_dept$,   /* Specified Department Code  */~
                             bg_dte$,    /* Specified Production Date  */~
                                         /* for a Production Day       */~
                             p_shft$,    /* Dept Shift Code or (AL)    */~
                             sc_load$,   /* Production Load or (ALL)   */~
                             ed_load$,   /* Ending Load Number         */~
                             p_mod$(),   /* Department Products        */~
                             p_unt%(),   /* Product Units              */~
                             p_unts%(),  /* SAMPLES ONLY               */~
                             p_untss%(), /* CHARGE SASHS ONLY          */~
                             p_untpp%(), /* CHARGE PARTS ONLY          */~
                             p_val(),    /* Product Dollar Value       */~
                             p_mrp(),    /* (5) Costing Buckets        */~
                             p_max%,     /* Max Number of Products     */~
                             p_flg%,     /* 0% = Load, 1% = No Load    */~
                             p_scan%,    /* 0%=Not Scanned, 1%=Scanned */~
                             p_screen%,  /* 0% = Yes, 1% = No - Display*/~
                             #7,         /* (APCPLNDP) NOT USED        */~
                             #5,         /* (APCPLNDT) Prod. Tracking  */~
                             #6,         /* (APCPLNAD) Audit File      */~
                             #2 )        /* (GENCDSIN) Master Code Tab */
            yy = 0.0                     /* Set Units Scanned Values   */
            for i% = 1% to p_no%         /* from Scanning Data.        */
                xx% = 0%
                for k% = 1% to 3%
                    xx% = xx% + p_unt%(i%,k%)
                    yy  = yy + p_val(i%,k%)
                next k%
                convert  xx% to d_val$(i%), pic(######)
            next i%

        return

        check_dept
            check% = 0%
            init(" ") readkey$
            str(readkey$,1%,9%)   = "PLAN DEPT"
            str(readkey$,10%,15%) = pd_dept$
            read #2,key = readkey$, eod goto L63980
               check% = 1%
L63980: return

/* (EWD0001)      */
/* ADD GLASS ANALYSIS TO COUNT THE COMPLETED GLASS FOR BG_DTE$ AND ED_DTE$ */


                                          /* ONLY DO ONCE                */
        glass_analysis
	    if rm_anal$ <> "0" then return           /* Flag - To perform    */
                                                     /* Glass Ana. or Not    */ 
            lt_comp$( 1%) = "GCOL  "   : lt_comp$( 5%) = "LEAD  "
            lt_comp$( 2%) = "GDIAM "   : lt_comp$( 6%) = "PRAIR "
            lt_comp$( 3%) = "GDPR  "   : lt_comp$( 7%) = "DIAGRD"
            lt_comp$( 4%) = "GFLO  "   : lt_comp$( 8%) = "DIAFLK"
            lt_comp$( 9%) = "SPCLIT"   : lt_comp$(10%) = "CG    "

          gls_comp$( 1%) = "TP" : gls_comp$(15%) = "PT" : gls_comp$(29%) = "EI"
          gls_comp$( 2%) = "TA" : gls_comp$(16%) = "PU" : gls_comp$(30%) = "EJ"
          gls_comp$( 3%) = "TE" : gls_comp$(17%) = "PW" : gls_comp$(31%) = "EK"
          gls_comp$( 4%) = "TB" : gls_comp$(18%) = "PX" : gls_comp$(32%) = "EL"
          gls_comp$( 5%) = "TL" : gls_comp$(19%) = "PY" : gls_comp$(33%) = "EM"
          gls_comp$( 6%) = "AT" : gls_comp$(20%) = "PZ" : gls_comp$(34%) = "EN"
          gls_comp$( 7%) = "AE" : gls_comp$(21%) = "EA" : gls_comp$(35%) = "EP"
          gls_comp$( 8%) = "TZ" : gls_comp$(22%) = "EB" : gls_comp$(36%) = "EQ"
          gls_comp$( 9%) = "SL" : gls_comp$(23%) = "EC" : gls_comp$(37%) = "ER"
          gls_comp$(10%) = "OT" : gls_comp$(24%) = "ED" : gls_comp$(38%) = "ES"
          gls_comp$(11%) = "TX" : gls_comp$(25%) = "EE" : gls_comp$(39%) = "ET"
          gls_comp$(12%) = "SP" : gls_comp$(26%) = "EF" : gls_comp$(40%) = "EU" 
          gls_comp$(13%) = "PR" : gls_comp$(27%) = "EG" : gls_comp$(41%) = "EV"
          gls_comp$(14%) = "PS" : gls_comp$(28%) = "EH"  
		  
     	    sc_dte$ = bg_dte$                                                   /* (Y2K, LDJ) */             
            testdate2$ = sc_dte$
            call "DATFMTC" (testdate2$,yr%,testdate$)                           /* (Y2K, LDJ) */
            convert str(testdate$,1%,4%) to yr%, data goto L64100               /* (Y2K, LDJ) */
L64100:
            leap_yr% = 365%
            if mod(yr%,4%) = 0% then leap_yr% = 366%
                             /* Windows Scanned from 7 AM of Specified */
                             /* Production Day, Until 6 59 AM of the   */
                             /* Next Production Day. 1st,2nd,3rd Shifts*/
            init(" ") jdate1$, jdate2$, bg_dte$, ed_dte$          
     	    init(" ") rm_ky$, rm_rec$, rm_model$, rm_gls$, rm_lt$, rm_num$
            init(" ") rm_wd_d$, rm_ht_d$, rm_temp$, rm_time$, rm_st$
            init(" ") rm_reason$

                                         /* Julian date Curr Prod Week */
            call "DATE" addr("GJ", str(sc_dte$,,6%), str(jdate1$,,5%), x%)      /* (Y2K, LDJ) */
                                                         /* CONVERT TO */
                                                         /* JULIAN DATE*/
            call "DATJULCV" (jdate1$)                                           /* (Y2K, LDJ) */
            convert str(jdate1$,5%,3%) to j1%, data goto L64110                 /* (Y2K, LDJ) */
L64110:
                                                 /* Current Production */
                                                 /* Day Julian Date    */
            j2% = j1% + 1%                       /* Tommorow's/Next Day*/
            jdate2$ = jdate1$                    /* Julian Date        */
            if j2% < leap_yr% then goto L64130
               j2% = 001%
            convert str(jdate2$,1%,4%) to rhh%, data goto L64120                /* (Y2K, LDJ) */
L64120:
            convert (rhh% + 1%) to str(jdate2$,1%,4%), pic(0000)                /* (Y2K, LDJ) */

L64130:     convert j1% to str(jdate1$,5%,3%), pic(000)                         /* (Y2K, LDJ) */

            convert j2% to str(jdate2$,5%,3%), pic(000)                         /* (Y2K, LDJ) */
                                               /* Begin with Production*/
            call "DATJULCV" (jdate1$)                                           /* (Y2K, LDJ) */
            call "DATJULCV" (jdate2$)                                           /* (Y2K, LDJ) */
			
            call "DATE" addr("JG", str(jdate1$,,5%), bg_dte$, x%)               /* (Y2K, LDJ) */
                                                         /* Date and   */
            call "DATE" addr("JG", str(jdate2$,,5%), ed_dte$, x%)               /* (Y2K, LDJ) */
                                                         /*Next Days   */
                                                         /*Prod. Date  */

            call "SHOSTAT" ("Analyzing Completed Glass for "& testdate2$)
            rm_cnt$  = "Checked [xxxxxx]"
            cnt%     = 0%
            mat rm%  = zer
            mat tot% = zer
                                          /* 1st Check Specified Prod. */
                                          /* date from 7 AM until Mid. */
            str(rm_ky$,1%,6%) = bg_dte$

            read #8,key 1% > rm_ky$, using L64000  , rm_rec$,            ~
                                                     eod goto analysis_done2            
            goto L64010
        analysis_done
            init(" ") rm_ky$, rm_rec$, rm_model$, rm_gls$, rm_lt$, rm_num$
            init(" ") rm_wd_d$, rm_ht_d$, rm_temp$, rm_time$, rm_st$
            init(" ") rm_reason$
            rma_flag% = 0%	               /* Flag for Org or Rmk     */
                                               /* 0=Org, 1=Rmk, & 2=Extra */  
            hr%       = 0%                     /* Hour Completed          */
	    rm_wd_d%  = 0%                     /* Glass Width Decimal     */
	    rm_ht_d%  = 0%                     /* Glass Height Decimal    */   
            k%        = 1%	               /* Array for Org or Rmk    */
                                               /* 1 = Org  2= Rmk         */			
            
                read #8,  using L64000  , rm_rec$, eod goto analysis_done2
             
L64000:        FMT CH(256)

L64010: 
               cnt% = cnt% + 1%
               if mod(cnt%,100%) <> 0 then goto L64020
                  convert cnt% to str(rm_cnt$,10%,6%), pic(######)
                  print at(02,33%);hex(84);rm_cnt$;

L64020:     rm_ky$     = str(rm_rec$,7%,27%)
            rm_bar$    = str(rm_rec$,22%,9%)
            rm_st$     = str(rm_rec$,13%,1%)
            rm_num$    = str(rm_rec$,31%,3%)			
            rm_reason$ = str(rm_rec$,34%,2%)
	    rm_time$   = str(rm_rec$,14%,8%)			
            rm_model$  = str(rm_rec$,72%,3%)
            rm_gls$    = str(rm_rec$,77%,2%)
            rm_lt$     = str(rm_rec$,79%,6%)
            rm_wd_d$   = str(rm_rec$,191%,8%)
            rm_ht_d$   = str(rm_rec$,199%,8%)
            rm_temp$   = str(rm_rec$,219%,1%)


            str(rma_ky$,1%,9%) = rm_bar$               /* Set Rmk Barcode & Rmk Num */
            str(rma_ky$,10%,3%) = "000"                /* for reading on Rmks.      */	

            rma_sve$ = str(rma_ky$,1%,9%)		/* Save BARCODE to copare if */
	                                                /* finished reading.         */	
												                                               
            if str(rm_ky$,1%,6%) > ed_dte$ then goto analysis_done2
            if rm_st$ <> "2" then goto analysis_done

            			
	   convert str(rm_wd_d$,1%,3%) to rm_wd_d%, data goto L64140

L64140:
	   convert str(rm_ht_d$,1%,3%) to rm_ht_d%, data goto L64141
		   
L64141:
           convert str(rm_time$,1%,2%) to hr%, data goto L64142

L64142:
           if str(rm_num$,3%,1%) <> "0" then goto analysis_done1
			
           ap$ = str(rm_time$,7%,2%)            /* Scanned 'AM' or 'PM'  */
           if str(rm_ky$,1%,6%) = ed_dte$ then goto L64150
                                                /* Check Current Scan Day*/
                                                /* First. 1st, 2nd and   */
                                                /* part of 3rd shift     */
                                                /* 7 AM to 12 Midnight   */
               if ap$ = "PM" then goto L64250   /* Part of Prev Day Prod.*/
                  if hr% < 7% or hr% = 12% then goto analysis_done
                  goto L64250
                                                /* Second Check Next Day */
                                                /* Scanned Data. Last    */
                                                /* part of 3rd Shift.    */
                                                /* Midnight to 6 59 'AM' */
L64150:    if ap$ = "PM" then goto analysis_done  
           if hr% < 7% or hr% = 12% then goto L64250 /* Midnight to 6 59A*/
              goto analysis_done                     /* Current Day      */

L64250:     init(" ") readkey$
            str(readkey$,1%,9%)   = "PLAN SHAP"
            str(readkey$,10%,13%) = rm_model$   
            read #2,key = readkey$, eod goto L64030
                              
                goto L64050   
L64030:     if rm_model$ < "311" or rm_model$ > "314" then goto L64040

              if rm_temp$ = "*" then goto L64080  
                                                           
L64050:        rm%(5%,k%) = rm%(5%,k%) + 1%
            goto L64300
                
L64040:     for i% = 1% to 41%
                if gls_comp$(i%) = rm_gls$ then goto L64050
            next i%


            for i% = 1% to 10%
                if lt_comp$(i%) = rm_lt$ then goto L64050
            next i%
            
            rm_lt1$ = str(rm_lt$,1%,1%) & str(rm_lt$,4%,1%)

               if rm_wd_d% < 40% and rm_ht_d% < 40% then goto L64080
           	  if rm_lt1$ = "00" then rm%(1%,k%) = rm%(1%,k%) + 1%   ~
                                    else rm%(2%,k%) = rm%(2%,k%) + 1%
                   goto L64300

L64080:     if rm_lt1$ = "00" then rm%(3%,k%) = rm%(3%,k%) + 1%            ~ 
                              else rm%(4%,k%) = rm%(4%,k%) + 1%

            
L64300:     if rma_flag% = 0% then goto analysis_done                      
               if rma_flag% = 1% then goto analysis_done1                      
                  if rma_flag% = 2% then goto L64265                      
           
        analysis_done1
	    init(" ") rma_rec$, rma_time$, rma_dte$, rma_comp$ 
            init(" ") rm_reason$, jdate3$, xx_dte$, rma_num$
	     	
            rma_flag% = 1%                        /* Remake Flag          */
                                /* If Flag = 0 then goto analysis_done    */
                                /* else goto analysis_done1               */  
            sc_hr%  = 0%                          /* Scan Hour            */
            sc_mn%  = 0%                          /* Scan Minute          */
            co_hr%  = 0%                          /* Complete Hour        */
            co_mn%  = 0%                          /* Complete Minute      */ 
            to_hr%  = 0%                          /* Total Hour           */
            to_mn%  = 0%                          /* Total Minute         */ 
            ad_mn%  = 0%                          /* Adjust Minute        */
            ad_mn1% = 0%                          /* Adjust Minute 1      */
            j3%     = 0%                          /* Julian Rmk Date      */			 
            k%      = 2%                          /* Array for Org or Rmk */
                                                  /* 1 = Org  2= Rmk      */  
			            			 
            read #10,key 1% > rma_ky$, using L64005  , rma_rec$,           ~
                                                 eod goto analysis_done
L64005:		FMT CH(64)
            
            cnt% = cnt% + 1%
            if mod(cnt%,100%) <> 0 then goto L64006
              convert cnt% to str(rm_cnt$,10%,6%), pic(######)
              print at(02,33%);hex(84);rm_cnt$;
L64006:						  
            rma_ky$     = str(rma_rec$,7%,12%)
            rma_num$    = str(rma_rec$,16%,3%)
            rma_dte$    = str(rma_rec$,1%,6%)
            rma_reason$ = str(rma_rec$,37%,2%)
	    rma_time$   = str(rma_rec$,19%,8%)
	    rma_comp$   = str(rma_rec$,30%,4%)			
             
			                   /* if <> then done reading.     */
            if str(rma_ky$,1%,9%) <> rma_sve$ then goto analysis_done

                                                  /* sc_hr & sc_mn = scan */
                                                  /* hour and minute      */
            convert str(rma_time$,1%,2%) to sc_hr%, data goto L64261

L64261:
            convert str(rma_time$,3%,2%) to sc_mn%, data goto L64262

L64262:                                           /* co_hr & co_mn = complete*/
                                                  /* hour and minute         */
            convert str(rma_comp$,1%,2%) to co_hr%, data goto L64263

L64263:
            convert str(rma_comp$,3%,2%) to co_mn%, data goto L64264

L64264:     if str(rma_num$,3%,1%) = "1" then goto add_extra
L64265:        rma_flag% = 1%                        /* Remake Flag          */
               k%      = 2%                          /* Array for Org or Rmk */
                                                     /* 1 = Org  2= Rmk      */  

             			            /* Dont count these reason codes */
                                            /* (EWD0005)                     */
            if rm_reason$ > "25" and rm_reason$ < "50" then goto analysis_done1
           
                                                   /* Total hours & minutes */
            to_hr%  = sc_hr% + co_hr%
	    to_mn%  = sc_mn% + co_mn%
			
    		                  /* Convert to Julian date so can add to day   */
				  /* if hour > 24.  */
            call "DATE" addr("GJ", str(rma_dte$,,6%), str(jdate3$,,5%), x%)
            call "DATJULCV" (jdate3$) 
  		
            convert str(jdate3$,5%,3%) to j3%, data goto L64260

L64260:   if to_mn% < 60% then goto L64280
		ad_mn%  = to_mn% / 60%
		ad_mn1% = mod(to_mn%,60%)
	
		to_hr%  = to_hr% + ad_mn%
		to_mn%  = ad_mn1%
			
L64280:   if to_hr% < 24% then goto L64270   /* if < 24 then dont add to day */
     	        to_hr% = to_hr% - 24%
                j3%    = j3% + 1%
          if to_hr% > 24% then goto L64280   /* if > 24 add to day again     */
			
	                                  /* Convert date back, so can    */
					  /* compare beg & end date.      */
L64270:     convert j3% to str(jdate3$,5%,3%), pic(000)
              
	    call "DATJULCV" (jdate3$)
            call "DATE" addr("JG", str(jdate3$,,5%), xx_dte$, x%)

	                             /* if xx_dte$ < or > than beg or ed*/
                                     /* date values then dont count.    */			  
            if xx_dte$ < bg_dte$ then goto analysis_done1
            if xx_dte$ > ed_dte$ then goto analysis_done1
	    if xx_dte$ = bg_dte$ then goto L64275
	    if xx_dte$ = ed_dte$ then goto L64290
                                              /* Check Current Scan Day*/
                                              /* First. 1st, 2nd and   */
                                              /* part of 3rd shift     */
                                              /* 7 AM to 12 Midnight   */
                                              /* Part of Prev Day Prod.*/
L64275:    if to_hr% < 7% then goto analysis_done1   
               goto L64250
                                              /* Second Check Next Day */
                                              /* Scanned Data. Last    */
                                              /* part of 3rd Shift.    */
L64290:    if to_hr% < 7% then goto L64250    /* time to 6 59A         */
              goto analysis_done1             /* Current Day           */
 
                                       
        analysis_done2
            for j% = 1% to 6%
                                  /* rm%(j%,2%) is the remake values    */
		                  /* rm%(j%,1%) is the original values  */
       		convert rm%(j%,1%) to d_val$(j%), pic(######)

            next j%
        return
                                  /* analysis_done3 is used for convert */
		                  /* remake counts to dummy dept        */         
        analysis_done3             
          for j% = 1% to 6%
                                  /* rm%(j%,2%) is the remake values    */
		                  /* rm%(j%,1%) is the original values  */
       		convert rm%(j%,2%) to d_val$(j%), pic(######)

            next j%
        return

        add_extra
           rma_flag% = 2%                        /* Remake Flag          */
           k%      = 1%                          /* Array for Org or Rmk */
                                                 /* 1 = Org  2= Rmk      */
            if rma_dte$ < bg_dte$ then goto L64265
            if rma_dte$ > ed_dte$ then goto L64265

	    if rma_dte$ = bg_dte$ then goto L64285
	    if rma_dte$ = ed_dte$ then goto L64295
                                              /* Check Current Scan Day*/
                                              /* First. 1st, 2nd and   */
                                              /* part of 3rd shift     */
                                              /* 7 AM to 12 Midnight   */
                                              /* Part of Prev Day Prod.*/
L64285:    if sc_hr% < 7% then goto L64265  
               goto L64250
                                              /* Second Check Next Day */
                                              /* Scanned Data. Last    */
                                              /* part of 3rd Shift.    */
L64295:    if sc_hr% < 7% then goto L64250    /* time to 6 59A         */
              goto L64265                     /* Current Day           */

        error_prompt
           comp% = 2%
           hh$  = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hh$, msg$(1%), msg$(2%), msg$(3%))
        return
                                                                            
        open_error                                                            
            errormsg$ = "(Open Error) - File = " & filename$
            gosub error_prompt
        return

        rewrite_error                                                            
            errormsg$ = "(Rewrite Error) - Dept = " & sc_dept$
            gosub error_prompt
        return
/* (EWD0001)      */

/* (EWD0002)      */

        get_adjust
           adj_hrsr, adj_hrso, adj_hrst = 0.0
		   adj_rate, adj_rate_ov, adj_pay, adj_pay_ov, adj_wages = 0.0

REM - Begin Calculations
           call "SHOSTAT" ("Calc Efficiency Adjustments for Dept ("         ~ 
		                       & sc_dept$ & ")" )
           readkey$ = " "
           str(readkey$,1%,9%)   = "APC EFFAJ"
           str(readkey$,10%,15%) = "0" & sc_dept$
           read #2,hold,key = readkey$, using L64180, descr$, eod goto L64240
L64180:       FMT POS(25), CH(30)
             
           convert str(descr$,1%,8%) to adj_hrsr, data goto L64200

L64200: 
           convert str(descr$,12%,8%) to adj_hrso, data goto L64225

L64225:
           convert str(descr$,23%,5%) to adj_rate, data goto L64230

L64230:
           adj_hrst    =  adj_hrst + ((adj_hrsr) + (adj_hrso))
	   adj_rate_ov =  round((adj_rate) * 1.5, 2)
	   adj_pay     =  (adj_hrsr) * adj_rate
	   adj_pay_ov  =  (adj_hrso) * adj_rate_ov
	   adj_wages   =  adj_wages + ((adj_pay) + (adj_pay_ov))
		   		   
      	   str(descr$,1%,19%) = " 0000.00 -  0000.00"
        rewrite #2, using L64180, descr$, data gosub rewrite_error              	   	   
L64240: return
                           /* Add adjustment pay to week, month, & year */
                           /* Not day b/c weekly hours in adjust table. */
        adjust_time							  
          for k% = 2% to 4%
                                                /* Reg Hours W,M,Y     */
              eff_hrsr(p_no%,k%) = eff_hrsr(p_no%,k%) + (adj_hrsr)
                                               /* Overtime Hours W,M,Y*/
              eff_hrso(p_no%,k%) = eff_hrso(p_no%,k%) + (adj_hrso)
                                               /* Total Eff Hours W,M,Y*/
              eff_hrst(p_no%,k%) = eff_hrst(p_no%,k%) + (adj_hrst)
                                               /* Total Eff Waged W,M,Y*/
              eff_wages(p_no%,k%) = eff_wages(p_no%,k%) + (adj_wages)
        next k%
       return	

/* (EWD0002)      */

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *************************************************************

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
			
			
